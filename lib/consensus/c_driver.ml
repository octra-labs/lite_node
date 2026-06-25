(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


module Frame = Octra_net.P2p_frame

type resource_committee_config = {
  activation_delay : int;
  committee_size : int;
  minimum_weight : int64;
  fraud_window : int64;
  future_window : int64;
  pubkey_of_node : string -> string option;
  source_seed_for_epoch : int64 -> Resource_attestation_flow.epoch_seed option;
  on_committee_selected : Resource_attestation_flow.committee_snapshot -> unit Lwt.t;
}

type scheduled_validator_set_config = {
  activate_epoch : int64;
  validator_set : C_types.validator_set;
  fingerprint : string;
}

type config = {
  chain_id : string;
  my_addr : string;
  sign_fn : string -> string;
  verify_fn : string -> string -> string -> bool;
  role_can_vote : unit -> bool;
  can_vote : unit -> bool;
  execute_fn : C_types.propose -> bool;
  verify_proposal : C_types.propose -> bool Lwt.t;
  on_finalized : C_types.finalize -> unit Lwt.t;
  make_proposal : int64 -> (C_types.epoch_header * string list) option Lwt.t;
  before_precommit_broadcast :
    epoch_id:int64 -> round:int -> proposal_id:string ->
    proposed_state_root:string -> txid_hi:int64 -> unit Lwt.t;
  lookup_epoch_root : int64 -> string option;
  local_head_epoch : unit -> int64;
  lookup_bundle : string -> (string list * string list * string list) option;
  lookup_catchup_range :
    from_epoch:int64 -> max_epochs:int ->
    [ `Ok of C_codec.catchup_epoch_record list * int64 option
    | `NotFound
    | `Internal of string ];
  on_resource_attestation : Resource_attestation_flow.gossip -> unit Lwt.t;
  scheduled_validator_set_config : scheduled_validator_set_config option;
  load_scheduled_validator_set_config :
    unit -> scheduled_validator_set_config option Lwt.t;
  resource_committee_config : resource_committee_config option;
}

type epoch_root_response_record = {
  responder_addr : string;
  responder_head_epoch : int64;
  state_root : string option;
}

type bundle_response_record = {
  responder_addr : string;
  tx_hashes : string list;
  txs_json : string list;
  receipts_json : string list;
}

type catchup_range_response_record = {
  responder_addr : string;
  request_id : string;
  status : string;
  records : C_codec.catchup_epoch_record list;
  next_epoch : int64 option;
}

type peer_state_record = {
  responder_addr : string;
  mutable head_epoch : int64;
  mutable checked_epoch : int64;
  mutable state_root : string option;
  mutable last_seen : float;
  mutable source : string;
}

type proposal_build = {
  gen : int;
  height : int64;
  round : int;
  step : C_types.round_step;
  started_at : float;
}

type proposal_height_status =
  | Proposal_current
  | Proposal_stale
  | Proposal_future

type t = {
  mutable n_validators : int;
  config : config;
  engine : C_engine.t;
  swarm : Octra_net.P2p_swarm.t;
  seen : (string, bool) Hashtbl.t;
  mutable running : bool;
  mutable epoch_start_mono : int64;
  epoch_root_responses : (int64, epoch_root_response_record list) Hashtbl.t;
  bundle_responses : (string, bundle_response_record list) Hashtbl.t;
  catchup_responses : (string, catchup_range_response_record list) Hashtbl.t;
  peer_states : (string, peer_state_record) Hashtbl.t;
  resource_attestations : (string, Resource_attestations.attestation) Hashtbl.t;
  resource_admission : Resource_attestation_admission.pool;
  activated_validator_set_fingerprints : (string, bool) Hashtbl.t;
  pending_votes : (string, C_types.vote) Hashtbl.t;
  pending_finalizes : (int64, C_types.finalize) Hashtbl.t;
  catchup_query_from_epoch : (string, int64) Hashtbl.t;
  mutable proposal_build : proposal_build option;
  mutable proposal_verify : proposal_build option;
  mutable output_loop_active : bool;
  mutable output_loop_requested : bool;
}

let epoch_slot_seconds = 10.0

let raw_to_hex s =
  String.concat "" (List.init (String.length s)
    (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let catchup_response_key (rec_ : catchup_range_response_record) =
  let records_root = C_hash.catchup_records_root rec_.records in
  let next_epoch_s =
    match rec_.next_epoch with
    | Some e -> Int64.to_string e
    | None -> "-" in
  rec_.status ^ "|" ^ next_epoch_s ^ "|" ^ records_root

let catchup_agreement_threshold t =
  if t.n_validators <= 1 then 1
  else min (max 1 (t.n_validators - 1)) (t.engine.vs.f + 1)

let create ~config ~validator_set ~swarm ~start_height =
  let engine = C_engine.create
    ~chain_id:config.chain_id
    ~my_addr:config.my_addr
    ~validator_set
    ~start_height
    ~can_vote:config.can_vote in
  { n_validators = validator_set.C_types.n; config; engine; swarm;
    seen = Hashtbl.create 256; running = false;
    epoch_start_mono = Mtime_clock.elapsed_ns ();
    epoch_root_responses = Hashtbl.create 16;
    bundle_responses = Hashtbl.create 16;
    catchup_responses = Hashtbl.create 8;
    peer_states = Hashtbl.create 16;
    resource_attestations = Hashtbl.create 256;
    resource_admission = Resource_attestation_admission.create_pool ();
    activated_validator_set_fingerprints = Hashtbl.create 8;
    pending_votes = Hashtbl.create 16;
    pending_finalizes = Hashtbl.create 16;
    catchup_query_from_epoch = Hashtbl.create 8;
    proposal_build = None;
    proposal_verify = None;
    output_loop_active = false;
    output_loop_requested = false }

let proposal_build_grace_ms () =
  match Sys.getenv_opt "OCTRA_BFT_PROPOSAL_BUILD_GRACE_MS" with
  | Some raw ->
      (try max 0 (int_of_string raw) with _ -> 15_000)
  | None -> 15_000

let proposal_verify_grace_ms () =
  match Sys.getenv_opt "OCTRA_BFT_PROPOSAL_VERIFY_GRACE_MS" with
  | Some raw ->
      (try max 0 (int_of_string raw) with _ -> 60_000)
  | None -> 60_000

let same_proposal_build b ~gen ~height ~round ~step =
  b.gen = gen
  && b.height = height
  && b.round = round
  && b.step = step

let local_validator t =
  C_types.is_validator t.engine.vs t.config.my_addr

let query_frame msg_type =
  msg_type = Frame.msg_query_epoch_root
  || msg_type = Frame.msg_epoch_root_response
  || msg_type = Frame.msg_query_bundle
  || msg_type = Frame.msg_bundle_response
  || msg_type = Frame.msg_query_catchup_range
  || msg_type = Frame.msg_catchup_range_response
  || msg_type = Frame.msg_query_catchup_range_v2
  || msg_type = Frame.msg_catchup_range_response_v2

let engine_output_frame msg_type =
  msg_type = Frame.msg_cons_propose
  || msg_type = Frame.msg_cons_vote
  || msg_type = Frame.msg_cons_finalize

let clear_proposal_build t ~gen ~height ~round ~step =
  match t.proposal_build with
  | Some b when same_proposal_build b ~gen ~height ~round ~step ->
      t.proposal_build <- None
  | _ -> ()

let proposal_build_active t ~generation ~round ~step =
  match t.proposal_build with
  | Some b ->
      b.gen = generation
      && b.height = t.engine.state.height
      && b.round = round
      && b.step = step
  | None -> false

let proposal_build_grace_left t ~generation ~round ~step =
  match t.proposal_build with
  | Some b when
      b.gen = generation
      && b.height = t.engine.state.height
      && b.round = round
      && b.step = step ->
      let elapsed = int_of_float ((Unix.gettimeofday () -. b.started_at) *. 1000.0) in
      elapsed < proposal_build_grace_ms ()
  | _ -> false

let mark_proposal_verify t ~gen ~height ~round ~step =
  t.proposal_verify <- Some { gen; height; round; step; started_at = Unix.gettimeofday () }

let clear_proposal_verify t ~gen ~height ~round ~step =
  match t.proposal_verify with
  | Some b when same_proposal_build b ~gen ~height ~round ~step ->
      t.proposal_verify <- None
  | _ -> ()

let proposal_verify_grace_left t ~generation ~round ~step =
  match t.proposal_verify with
  | Some b when
      b.gen = generation
      && b.height = t.engine.state.height
      && b.round = round
      && b.step = step ->
      let elapsed = int_of_float ((Unix.gettimeofday () -. b.started_at) *. 1000.0) in
      elapsed < proposal_verify_grace_ms ()
  | _ -> false

let vote_step_label = function
  | C_types.Prevote -> "PREVOTE"
  | C_types.Precommit -> "PRECOMMIT"

let proposal_height_status ~current ~proposal =
  match Int64.compare proposal current with
  | 0 -> Proposal_current
  | n when n < 0 -> Proposal_stale
  | _ -> Proposal_future

let proposal_local_status ~engine_head ~local_head ~proposal =
  if Int64.compare proposal local_head <= 0 then Proposal_stale
  else proposal_height_status ~current:engine_head ~proposal

let vote_key (v : C_types.vote) =
  Printf.sprintf "%s|%Ld|%d|%s|%s"
    (vote_step_label v.vote_type)
    v.epoch_id
    v.round
    v.validator
    (raw_to_hex v.proposal_id)

let pending_vote_count t = Hashtbl.length t.pending_votes

let pending_finalize_count t = Hashtbl.length t.pending_finalizes

let queue_vote t (v : C_types.vote) =
  Hashtbl.replace t.pending_votes (vote_key v) v

let queue_future_finalize t (f : C_types.finalize) =
  let keep =
    match Hashtbl.find_opt t.pending_finalizes f.epoch_id with
    | None -> true
    | Some old -> f.commit_round < old.commit_round
  in
  if keep then Hashtbl.replace t.pending_finalizes f.epoch_id f

let vote_still_relevant t (v : C_types.vote) =
  Int64.compare v.epoch_id t.engine.state.height = 0
  && Int64.compare v.epoch_id t.engine.finalized_height > 0

let broadcast_vote t (v : C_types.vote) =
  let open Lwt.Syntax in
  Octra_log.stdout "CONSENSUS [%s]: → SendVote %s epoch = %Ld round = %d pid = %s\n%!"
    t.config.my_addr
    (vote_step_label v.vote_type)
    v.epoch_id v.round
    (let h = Digestif.SHA256.to_hex (Digestif.SHA256.of_raw_string v.proposal_id) in
     if String.length h >= 8 then String.sub h 0 8 else h);
  let nil_hash = String.make 32 '\x00' in
  let* () =
    if v.vote_type = C_types.Precommit && v.proposal_id <> nil_hash then
      match C_engine.find_header t.engine v.proposal_id with
      | Some header ->
        t.config.before_precommit_broadcast
          ~epoch_id:v.epoch_id ~round:v.round
          ~proposal_id:v.proposal_id
          ~proposed_state_root:header.proposed_state_root
          ~txid_hi:header.txid_hi
      | None -> Lwt.return_unit
    else Lwt.return_unit
  in
  let payload = C_codec.encode_vote v in
  Octra_net.P2p_swarm.broadcast t.swarm { msg_type = Frame.msg_cons_vote; payload }

let flush_pending_votes t =
  if not (t.config.can_vote ()) then Lwt.return_unit
  else begin
    let queued =
      Hashtbl.fold (fun key vote acc -> (key, vote) :: acc) t.pending_votes [] in
    List.iter (fun (key, _) -> Hashtbl.remove t.pending_votes key) queued;
    let votes =
      queued
      |> List.filter_map (fun (_, vote) ->
        if vote_still_relevant t vote then Some vote else None)
      |> List.sort (fun (a : C_types.vote) b ->
        let c = Int64.compare a.epoch_id b.epoch_id in
        if c <> 0 then c else
        let c = compare a.round b.round in
        if c <> 0 then c else
        compare (vote_step_label a.vote_type) (vote_step_label b.vote_type))
    in
    match votes with
    | [] -> Lwt.return_unit
    | _ ->
      Octra_log.stdout "CONSENSUS [%s]: flushing %d pending votes after voting-ready\n%!"
        t.config.my_addr (List.length votes);
      Lwt_list.iter_s (broadcast_vote t) votes
  end

let verify_engine_signature t addr msg signature =
  match C_types.pubkey_of_addr t.engine.vs addr with
  | Some pk -> C_hash.verify_ed25519 ~pubkey_raw:pk ~msg ~signature
  | None -> false

let remember_peer_state t ~source ~responder_addr ~head_epoch ~checked_epoch ~state_root =
  match Hashtbl.find_opt t.peer_states responder_addr with
  | Some rec_ ->
      rec_.head_epoch <- head_epoch;
      rec_.checked_epoch <- checked_epoch;
      rec_.state_root <- state_root;
      rec_.last_seen <- Unix.gettimeofday ();
      rec_.source <- source
  | None ->
      Hashtbl.replace t.peer_states responder_addr {
        responder_addr;
        head_epoch;
        checked_epoch;
        state_root;
        last_seen = Unix.gettimeofday ();
        source;
      }

let peer_state_snapshot t =
  Hashtbl.fold (fun _ rec_ acc -> rec_ :: acc) t.peer_states []

let msg_id msg_type payload =
  Octra_net.Hash_domain.hash
    "octra:seen:consensus:v1"
    (String.make 1 (Char.chr msg_type) ^ payload)

let is_seen t msg_type payload =
  let id = msg_id msg_type payload in
  if Hashtbl.mem t.seen id then true
  else begin
    Hashtbl.replace t.seen id true;

    if Hashtbl.length t.seen > 10000 then Hashtbl.reset t.seen;
    false
  end

let resource_attestation_pool t =
  Hashtbl.fold (fun _ attestation acc -> attestation :: acc) t.resource_attestations []

let maybe_activate_scheduled_validator_set t ~target_epoch =
  let open Lwt.Syntax in
  let* dynamic_cfg = t.config.load_scheduled_validator_set_config () in
  let cfg_opt =
    match dynamic_cfg with
    | Some cfg -> Some cfg
    | None -> t.config.scheduled_validator_set_config
  in
  match cfg_opt with
  | None -> Lwt.return_unit
  | Some cfg ->
    if Hashtbl.mem t.activated_validator_set_fingerprints cfg.fingerprint
       || Int64.compare target_epoch cfg.activate_epoch < 0 then
      Lwt.return_unit
    else begin
      C_engine.replace_validator_set t.engine cfg.validator_set;
      t.n_validators <- cfg.validator_set.C_types.n;
      Hashtbl.replace t.activated_validator_set_fingerprints cfg.fingerprint true;
      Octra_log.stdout
        "CONSENSUS [%s]: validator_set activated target_epoch = %Ld n = %d quorum = %d fingerprint = %s\n%!"
        t.config.my_addr target_epoch cfg.validator_set.n cfg.validator_set.quorum
        cfg.fingerprint;
      if C_types.is_validator cfg.validator_set t.config.my_addr then
        Lwt.return_unit
      else begin
        Octra_log.stdout
          "CONSENSUS [%s]: validator_set activated as non-voting observer target_epoch = %Ld\n%!"
          t.config.my_addr target_epoch;
        Lwt.return_unit
      end
    end

let resource_committee_snapshot t ~activation_delay ~committee_size ~target_epoch ~source_seed =
  Resource_attestation_flow.select_snapshot
    ~activation_delay
    ~committee_size
    ~target_epoch
    ~source_seed
    (resource_attestation_pool t)

let maybe_activate_resource_committee t ~target_epoch =
  match t.config.resource_committee_config with
  | None -> Lwt.return_unit
  | Some cfg ->
      match Resource_attestation_flow.activated_source_epoch
        ~activation_delay:cfg.activation_delay ~target_epoch with
      | None -> Lwt.return_unit
      | Some source_epoch ->
          match cfg.source_seed_for_epoch source_epoch with
          | None -> Lwt.return_unit
          | Some source_seed ->
              match resource_committee_snapshot t
                ~activation_delay:cfg.activation_delay
                ~committee_size:cfg.committee_size
                ~target_epoch
                ~source_seed with
              | None -> Lwt.return_unit
              | Some snapshot
                when not (Resource_attestation_flow.ready_for_voting
                  ~minimum_weight:cfg.minimum_weight snapshot) ->
                  Octra_log.stdout
                    "CONSENSUS [%s]: resource_committee pending target_epoch = %Ld source_epoch = %Ld weight = %Ld minimum = %Ld\n%!"
                    t.config.my_addr target_epoch snapshot.source_epoch snapshot.total_weight cfg.minimum_weight;
                  Lwt.return_unit
              | Some snapshot ->
                  match Resource_attestation_flow.validator_set_of_committee
                    ~pubkey_of_node:cfg.pubkey_of_node snapshot.committee with
                  | None ->
                      Octra_log.stdout
                        "CONSENSUS [%s]: resource_committee rejected target_epoch = %Ld reason = missing_pubkey\n%!"
                        t.config.my_addr target_epoch;
                      Lwt.return_unit
                  | Some validator_set ->
                      C_engine.replace_validator_set t.engine validator_set;
                      t.n_validators <- validator_set.C_types.n;
                      let root_hex = raw_to_hex snapshot.committee_root in
                      Octra_log.stdout
                        "CONSENSUS [%s]: resource_committee activated target_epoch = %Ld source_epoch = %Ld n = %d quorum = %d weight = %Ld root = %s\n%!"
                        t.config.my_addr target_epoch snapshot.source_epoch
                        validator_set.n validator_set.quorum snapshot.total_weight
                        (String.sub root_hex 0 (min 16 (String.length root_hex)));
                      cfg.on_committee_selected snapshot

let try_current_leader_proposal t =
  let open Lwt.Syntax in
  if t.running
     && C_engine.am_i_leader t.engine
     && t.engine.state.step = C_types.ProposeStep then begin
    let gen = t.engine.generation in
    let height = t.engine.state.height in
    let round = t.engine.state.round in
    let step = t.engine.state.step in
    match t.proposal_build with
    | Some b when same_proposal_build b ~gen ~height ~round ~step ->
        Lwt.return_false
    | _ ->
    t.proposal_build <- Some { gen; height; round; step; started_at = Unix.gettimeofday () };
    let* proposal_opt =
      Lwt.finalize
        (fun () -> t.config.make_proposal height)
        (fun () ->
          clear_proposal_build t ~gen ~height ~round ~step;
          Lwt.return_unit)
    in
    if t.engine.generation = gen
       && t.engine.state.height = height
       && t.engine.state.round = round
       && t.engine.state.step = step then
      match proposal_opt with
      | Some (hdr, txs) ->
        C_engine.do_propose t.engine hdr txs ~sign_fn:t.config.sign_fn;
        Lwt.return (t.engine.state.step <> step)
      | None ->
        Octra_log.stdout "CONSENSUS [%s]: make_proposal returned None at h = %Ld r = %d\n%!"
          t.config.my_addr height round;
        Lwt.return_false
    else begin
      Octra_log.stdout "CONSENSUS [%s]: stale make_proposal result dropped (state moved h = %Ld->%Ld r = %d->%d)\n%!"
        t.config.my_addr height t.engine.state.height round t.engine.state.round;
      Lwt.return_false
    end
  end else
    Lwt.return_false

let admit_resource_attestation t attestation =
  match t.config.resource_committee_config with
  | None -> Resource_attestation_admission.Accept
  | Some cfg ->
      match cfg.source_seed_for_epoch attestation.Resource_attestations.epoch_id with
      | None -> Resource_attestation_admission.Quarantine Resource_attestation_admission.MissingChallenge
      | Some source_seed ->
          let challenge = Resource_attestation_flow.challenge source_seed in
          let window = Resource_attestation_admission.{
            current_epoch = t.config.local_head_epoch ();
            fraud_window = cfg.fraud_window;
            future_window = cfg.future_window;
          } in
          Resource_attestation_admission.ingest
            ~chain_id:t.config.chain_id
            ~challenge
            ~window
            ~pubkey_of_node:cfg.pubkey_of_node
            t.resource_admission
            attestation

let rec process_outputs_once t =
  let open Lwt.Syntax in
  let current_height = t.engine.state.height in
  Hashtbl.filter_map_inplace (fun epoch finalize ->
    if Int64.compare epoch t.engine.C_engine.finalized_height <= 0 then None
    else if Int64.compare epoch current_height = 0 then begin
      let accepted = C_engine.accept_finalize_batch t.engine finalize in
      Octra_log.stdout
        "CONSENSUS [%s]: pending finalize for current height epoch = %Ld accepted = %b\n%!"
        t.config.my_addr epoch accepted;
      None
    end else
      Some finalize
  ) t.pending_finalizes;
  let outputs = C_engine.drain_outputs t.engine in
  let has_finalized =
    List.exists (function
      | C_engine.Finalized _ -> true
      | _ -> false
    ) outputs
  in
  List.iter (fun o ->
    let name = match o with
      | C_engine.SendPropose _ -> "SendPropose"
      | C_engine.SendVote _ -> "SendVote"
      | C_engine.SendFinalize _ -> "SendFinalize"
      | C_engine.ScheduleTimeout _ -> "ScheduleTimeout"
      | C_engine.Finalized _ -> "Finalized" in
    Octra_log.stdout "CONSENSUS [%s]: output → %s\n%!" t.config.my_addr name
  ) outputs;
  let* () = flush_pending_votes t in
  let* () = Lwt_list.iter_s (fun output ->
    match output with
    | C_engine.SendPropose p ->
      Octra_log.stdout "CONSENSUS [%s]: → SendPropose epoch = %Ld round = %d\n%!" t.config.my_addr p.epoch_id p.round;
      let payload = C_codec.encode_propose p in
      Octra_net.P2p_swarm.broadcast t.swarm { msg_type = Frame.msg_cons_propose; payload }
    | C_engine.SendFinalize f ->
      Octra_log.stdout "CONSENSUS [%s]: → SendFinalize epoch = %Ld round = %d pid = %s creator = %s precommits = %d\n%!"
        t.config.my_addr f.epoch_id f.commit_round
        (let h = Digestif.SHA256.to_hex (Digestif.SHA256.of_raw_string f.proposal_id) in
         if String.length h >= 16 then String.sub h 0 16 else h)
        (String.sub f.header.creator_addr 0 (min 14 (String.length f.header.creator_addr)))
        (List.length f.precommits);
      let payload = C_codec.encode_finalize f in
      Octra_net.P2p_swarm.broadcast t.swarm { msg_type = Frame.msg_cons_finalize; payload }
    | C_engine.ScheduleTimeout { step; round; delay_ms; generation } ->
      Lwt.async (fun () ->
        let* () = Lwt_unix.sleep (float_of_int delay_ms /. 1000.0) in
        let rec fire () =
          if not t.running then Lwt.return_unit
          else if step = C_types.ProposeStep
                  && proposal_build_active t ~generation ~round ~step
                  && proposal_build_grace_left t ~generation ~round ~step then begin
            Octra_log.stdout
              "CONSENSUS [%s]: defer propose timeout h = %Ld r = %d while proposal build is active\n%!"
              t.config.my_addr t.engine.state.height round;
            let* () = Lwt_unix.sleep 0.5 in
            fire ()
          end else if step = C_types.ProposeStep
                  && proposal_verify_grace_left t ~generation ~round ~step then begin
            Octra_log.stdout
              "CONSENSUS [%s]: defer propose timeout h = %Ld r = %d while proposal verify is active\n%!"
              t.config.my_addr t.engine.state.height round;
            let* () = Lwt_unix.sleep 0.5 in
            fire ()
          end else begin
            C_engine.on_timeout t.engine ~step ~round ~generation ~sign_fn:t.config.sign_fn;
            let* _ = try_current_leader_proposal t in
            process_outputs t
          end in
        fire ());
      Lwt.return_unit
    | C_engine.SendVote v ->
      if not (t.config.can_vote ()) then begin
        queue_vote t v;
        Octra_log.stdout "CONSENSUS [%s]: queued vote while local state is not voting-ready %s epoch = %Ld round = %d pending = %d\n%!"
          t.config.my_addr
          (vote_step_label v.vote_type)
          v.epoch_id v.round
          (pending_vote_count t);
        Lwt.return_unit
      end else
        broadcast_vote t v
    | C_engine.Finalized { epoch_id; finalize } ->
      let header = finalize.header in
      let round = finalize.commit_round in
      Octra_log.stdout "CONSENSUS [%s]: FINALIZED epoch = %Ld round = %d creator = %s root = %s\n%!"
        t.config.my_addr epoch_id round
        (String.sub header.creator_addr 0 (min 14 (String.length header.creator_addr)))
        (let h = Digestif.SHA256.to_hex (Digestif.SHA256.of_raw_string header.proposed_state_root) in
         if String.length h >= 16 then String.sub h 0 16 else h);
      let* () = t.config.on_finalized finalize in
      let* () = maybe_activate_scheduled_validator_set t ~target_epoch:(Int64.add epoch_id 1L) in
      let* () = maybe_activate_resource_committee t ~target_epoch:(Int64.add epoch_id 1L) in
      let next = Int64.add epoch_id 1L in
      C_engine.start_height t.engine next;
      let now_ns = Mtime_clock.elapsed_ns () in
      let slot_ns = Int64.of_float (epoch_slot_seconds *. 1e9) in
      let target_ns = Int64.add t.epoch_start_mono slot_ns in
      let remaining_ns = Int64.sub target_ns now_ns in
      let* () =
        if remaining_ns > 100_000_000L then
          Lwt_unix.sleep (Int64.to_float remaining_ns /. 1e9)
        else Lwt.return_unit in
      t.epoch_start_mono <- Mtime_clock.elapsed_ns ();
      let* _ = try_current_leader_proposal t in
      process_outputs t
  ) outputs in
  if has_finalized then Lwt.return_unit
  else
    let* proposed = try_current_leader_proposal t in
    if proposed then process_outputs t else Lwt.return_unit
and process_outputs t =
  let open Lwt.Syntax in
  if t.output_loop_active then begin
    t.output_loop_requested <- true;
    Lwt.return_unit
  end else begin
    t.output_loop_active <- true;
    let rec loop () =
      t.output_loop_requested <- false;
      let* () = process_outputs_once t in
      if t.output_loop_requested then loop () else Lwt.return_unit
    in
    Lwt.finalize loop (fun () ->
      t.output_loop_active <- false;
      Lwt.return_unit)
  end

let on_p2p_message t _conn (frame : Frame.frame) =
  let open Lwt.Syntax in

  let skip_dedup = query_frame frame.msg_type in
  if (not skip_dedup) && is_seen t frame.msg_type frame.payload then Lwt.return_unit
  else begin
    (match frame.msg_type with
    | t' when t' = Frame.msg_cons_propose ->
      Lwt.catch (fun () ->
        let p = C_codec.decode_propose frame.payload in
        if p.chain_id <> t.config.chain_id then begin
          Octra_log.stdout "CONSENSUS [%s]: REJECTED propose chain_id mismatch (got = %s ours = %s)\n%!"
            t.config.my_addr p.chain_id t.config.chain_id;
          Lwt.return_unit
        end else
        let valid = match C_types.pubkey_of_addr t.engine.vs p.proposer with
          | Some pk -> C_hash.verify_propose ~pubkey_raw:pk p
          | None -> false
        in
        if not valid then begin
          Octra_log.stdout "CONSENSUS [%s]: REJECTED propose from %s (bad sig or unknown)\n%!"
            t.config.my_addr (String.sub p.proposer 0 (min 12 (String.length p.proposer)));
          Octra_net.P2p_swarm.report_bad_peer t.swarm _conn ~reason:"bad_signature_propose";
          Lwt.return_unit
        end else
        match proposal_local_status
                ~engine_head:t.engine.state.height
                ~local_head:(t.config.local_head_epoch ())
                ~proposal:p.epoch_id with
        | Proposal_stale
        | Proposal_future ->
          Lwt.return_unit
        | Proposal_current ->
          let gen = t.engine.generation in
          let height = t.engine.state.height in
          let round = t.engine.state.round in
          let step = t.engine.state.step in
          let verify_current_round =
            p.round = round && step = C_types.ProposeStep
          in
          if verify_current_round then
            mark_proposal_verify t ~gen ~height ~round ~step;
          let* preview_ok =
            Lwt.finalize
              (fun () -> t.config.verify_proposal p)
              (fun () ->
                if verify_current_round then
                  clear_proposal_verify t ~gen ~height ~round ~step;
                Lwt.return_unit)
          in
          let* () =
            if preview_ok then
              Octra_net.P2p_swarm.broadcast t.swarm
                { msg_type = Frame.msg_cons_propose; payload = frame.payload }
            else Lwt.return_unit
          in
          ignore (C_engine.on_propose t.engine p
            ~verify_fn:(verify_engine_signature t)
            ~execute_fn:(fun _ -> preview_ok)
            ~sign_fn:t.config.sign_fn);
          process_outputs t
        )
      (fun exn ->
        Octra_log.stdout "CONSENSUS [%s]: bad propose: %s\n%!" t.config.my_addr (Printexc.to_string exn);
        Octra_net.P2p_swarm.report_bad_peer t.swarm _conn ~reason:"invalid_frame_propose";
        Lwt.return_unit)
    | t' when t' = Frame.msg_cons_vote ->
      Lwt.catch (fun () ->
        let v = C_codec.decode_vote frame.payload in
        if v.chain_id <> t.config.chain_id then begin
          Octra_log.stdout "CONSENSUS [%s]: REJECTED vote chain_id mismatch (got = %s ours = %s)\n%!"
            t.config.my_addr v.chain_id t.config.chain_id;
          Lwt.return_unit
        end else
        let valid = match C_types.pubkey_of_addr t.engine.vs v.validator with
          | Some pk -> C_hash.verify_vote ~pubkey_raw:pk v
          | None -> false
        in
        if valid then
          C_engine.on_vote t.engine v ~sign_fn:t.config.sign_fn;
        if not valid then
          Octra_log.stdout "CONSENSUS [%s]: REJECTED vote from %s (bad sig or unknown validator)\n%!"
            t.config.my_addr (String.sub v.validator 0 (min 12 (String.length v.validator)));
        if not valid then
          Octra_net.P2p_swarm.report_bad_peer t.swarm _conn ~reason:"bad_signature_vote";
        Lwt.return_unit)
      (fun exn ->
        Octra_log.stdout "CONSENSUS [%s]: bad vote: %s\n%!" t.config.my_addr (Printexc.to_string exn);
        Octra_net.P2p_swarm.report_bad_peer t.swarm _conn ~reason:"invalid_frame_vote";
        Lwt.return_unit)
    | t' when t' = Frame.msg_cons_finalize ->
      Lwt.catch (fun () ->
        let f = C_codec.decode_finalize frame.payload in
        if f.chain_id <> t.config.chain_id then begin
          Octra_log.stdout "CONSENSUS [%s]: REJECTED finalize chain_id mismatch (got = %s ours = %s)\n%!"
            t.config.my_addr f.chain_id t.config.chain_id;
          Lwt.return_unit
        end else
        if f.epoch_id <= t.engine.finalized_height then Lwt.return_unit
        else begin
          let verify_vote (vote : C_types.vote) =
            match C_types.pubkey_of_addr t.engine.vs vote.C_types.validator with
            | Some pk -> C_hash.verify_vote ~pubkey_raw:pk vote
            | None -> false
          in
          match C_qc.validate_finalize
            ~chain_id:t.config.chain_id
            ~validator_set:t.engine.vs
            ~verify_vote
            f
          with
          | C_qc.Invalid reason ->
            Octra_log.stdout "CONSENSUS [%s]: REJECTED finalize — qc_%s\n%!"
              t.config.my_addr reason;
            let peer_reason =
              if reason = "signature" then "bad_signature_finalize"
              else "invalid_frame_finalize_qc_" ^ reason
            in
            Octra_net.P2p_swarm.report_bad_peer t.swarm _conn ~reason:peer_reason;
            Lwt.return_unit
          | C_qc.Valid ->
            let expected_pid = C_hash.proposal_id f.header in
          Octra_log.stdout
            "CONSENSUS [%s]: RECV finalize epoch = %Ld commit_r = %d pid = %s creator = %s precommits = %d\n%!"
            t.config.my_addr f.epoch_id f.commit_round
            (let h = Digestif.SHA256.to_hex (Digestif.SHA256.of_raw_string f.proposal_id) in
             if String.length h >= 16 then String.sub h 0 16 else h)
            (String.sub f.header.creator_addr 0 (min 14 (String.length f.header.creator_addr)))
            (List.length f.precommits);
              let accepted = C_engine.accept_finalize_batch t.engine f in
              if accepted then begin
                Octra_log.stdout "CONSENSUS [%s]: FINALIZE via batch: epoch = %Ld round = %d (%d/%d valid precommits)\n%!"
                  t.config.my_addr f.epoch_id f.commit_round
                  (List.length f.precommits)
                  (List.length f.precommits);
                let* () = Octra_net.P2p_swarm.broadcast t.swarm
                  { msg_type = Frame.msg_cons_finalize; payload = frame.payload } in
                process_outputs t
              end else begin
                Octra_log.stdout "CONSENSUS [%s]: finalize rejected by engine state epoch = %Ld round = %d pid = %s\n%!"
                  t.config.my_addr f.epoch_id f.commit_round
                  (String.sub expected_pid 0 (min 16 (String.length expected_pid)));
                if Int64.compare f.epoch_id t.engine.state.height > 0 then begin
                  queue_future_finalize t f;
                  Octra_log.stdout
                    "CONSENSUS [%s]: queued future finalize epoch = %Ld current = %Ld pending = %d\n%!"
                    t.config.my_addr f.epoch_id t.engine.state.height
                    (pending_finalize_count t)
                end;
                Lwt.return_unit
              end
        end)
      (fun exn ->
        Octra_log.stdout "CONSENSUS [%s]: bad finalize: %s\n%!" t.config.my_addr (Printexc.to_string exn);
        Octra_net.P2p_swarm.report_bad_peer t.swarm _conn ~reason:"invalid_frame_finalize";
        Lwt.return_unit)
    | t' when t' = Frame.msg_query_epoch_root ->
      Lwt.catch (fun () ->
        let q = C_codec.decode_epoch_root_query frame.payload in
        if q.chain_id <> t.config.chain_id then Lwt.return_unit
        else if not (local_validator t) then Lwt.return_unit
        else begin
          let state_root = t.config.lookup_epoch_root q.epoch_id in
          let responder_addr = t.config.my_addr in
          let responder_head_epoch = t.config.local_head_epoch () in
          let sign_bytes = C_hash.epoch_root_response_sign_bytes
            ~chain_id:t.config.chain_id ~epoch_id:q.epoch_id
            ~state_root ~responder_addr ~responder_head_epoch in
          let signature = t.config.sign_fn sign_bytes in
          let response = C_codec.{
            chain_id = t.config.chain_id;
            epoch_id = q.epoch_id;
            state_root;
            responder_addr;
            responder_head_epoch;
            signature;
          } in
          let payload = C_codec.encode_epoch_root_response response in
          Octra_net.P2p_swarm.broadcast t.swarm
            { msg_type = Frame.msg_epoch_root_response; payload }
        end)
        (fun exn ->
          Octra_log.stdout "CONSENSUS [%s]: bad epoch_root_query: %s\n%!"
            t.config.my_addr (Printexc.to_string exn);
          Lwt.return_unit)
    | t' when t' = Frame.msg_epoch_root_response ->
      Lwt.catch (fun () ->
        let r = C_codec.decode_epoch_root_response frame.payload in
        if r.chain_id <> t.config.chain_id then Lwt.return_unit
        else if not (C_types.is_validator t.engine.vs r.responder_addr) then begin
          Octra_log.stdout "CONSENSUS [%s]: ignored epoch_root_response from non-validator %s\n%!"
            t.config.my_addr (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
          Lwt.return_unit
        end else begin
          let sign_bytes = C_hash.epoch_root_response_sign_bytes
            ~chain_id:r.chain_id ~epoch_id:r.epoch_id
            ~state_root:r.state_root ~responder_addr:r.responder_addr
            ~responder_head_epoch:r.responder_head_epoch in
          if not (verify_engine_signature t r.responder_addr sign_bytes r.signature) then begin
            Octra_log.stdout "CONSENSUS [%s]: ignored epoch_root_response with BAD SIGNATURE from %s\n%!"
              t.config.my_addr (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
            Octra_net.P2p_swarm.report_bad_peer t.swarm _conn ~reason:"bad_signature_epoch_root";
            Lwt.return_unit
          end else begin
            remember_peer_state t
              ~source:"epoch_root"
              ~responder_addr:r.responder_addr
              ~head_epoch:r.responder_head_epoch
              ~checked_epoch:r.epoch_id
              ~state_root:r.state_root;
            let record = {
              responder_addr = r.responder_addr;
              responder_head_epoch = r.responder_head_epoch;
              state_root = r.state_root;
            } in
            let prior = try Hashtbl.find t.epoch_root_responses r.epoch_id with Not_found -> [] in
            let already_seen = List.exists (fun (rec_ : epoch_root_response_record) ->
              rec_.responder_addr = r.responder_addr) prior in
            if not already_seen then
              Hashtbl.replace t.epoch_root_responses r.epoch_id (record :: prior);
            Lwt.return_unit
          end
        end)
        (fun exn ->
          Octra_log.stdout "CONSENSUS [%s]: bad epoch_root_response: %s\n%!"
            t.config.my_addr (Printexc.to_string exn);
          Lwt.return_unit)
    | t' when t' = Frame.msg_query_bundle ->
      Lwt.catch (fun () ->
        let q = C_codec.decode_bundle_query frame.payload in
        if q.chain_id <> t.config.chain_id then Lwt.return_unit
        else if not (local_validator t) then Lwt.return_unit
        else begin
          match t.config.lookup_bundle q.proposal_id with
          | None -> Lwt.return_unit
          | Some (tx_hashes, txs_json, receipts_json) ->
            let r = C_codec.{
              chain_id = t.config.chain_id;
              epoch_id = q.epoch_id;
              proposal_id = q.proposal_id;
              responder_addr = t.config.my_addr;
              tx_hashes;
              txs_json;
              receipts_json;
            } in
            let payload = C_codec.encode_bundle_response r in
            Octra_net.P2p_swarm.broadcast t.swarm
              { msg_type = Frame.msg_bundle_response; payload }
        end)
        (fun exn ->
          Octra_log.stdout "CONSENSUS [%s]: bad bundle_query: %s\n%!"
            t.config.my_addr (Printexc.to_string exn);
          Lwt.return_unit)
    | t' when t' = Frame.msg_bundle_response ->
      Lwt.catch (fun () ->
        let r = C_codec.decode_bundle_response frame.payload in
        if r.chain_id <> t.config.chain_id then Lwt.return_unit
        else if not (C_types.is_validator t.engine.vs r.responder_addr) then begin
          Octra_log.stdout "CONSENSUS [%s]: ignored bundle_response from non-validator %s\n%!"
            t.config.my_addr (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
          Lwt.return_unit
        end else if List.length r.tx_hashes <> List.length r.txs_json then begin
          Octra_log.stdout "CONSENSUS [%s]: ignored bundle_response with mismatched lengths from %s\n%!"
            t.config.my_addr (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
          Lwt.return_unit
        end else begin
          let record = {
            responder_addr = r.responder_addr;
            tx_hashes = r.tx_hashes;
            txs_json = r.txs_json;
            receipts_json = r.receipts_json;
          } in
          let prior = try Hashtbl.find t.bundle_responses r.proposal_id with Not_found -> [] in
          let already_seen = List.exists (fun (rec_ : bundle_response_record) ->
            rec_.responder_addr = r.responder_addr) prior in
          if not already_seen then
            Hashtbl.replace t.bundle_responses r.proposal_id (record :: prior);
          Lwt.return_unit
        end)
        (fun exn ->
          Octra_log.stdout "CONSENSUS [%s]: bad bundle_response: %s\n%!"
            t.config.my_addr (Printexc.to_string exn);
          Lwt.return_unit)
    | t' when t' = Frame.msg_query_catchup_range
              || t' = Frame.msg_query_catchup_range_v2 ->
      let is_v2 = (t' = Frame.msg_query_catchup_range_v2) in
      Lwt.catch (fun () ->
        let q = C_codec.decode_catchup_query_range frame.payload in
        if q.chain_id <> t.config.chain_id then Lwt.return_unit
        else if not (local_validator t) then Lwt.return_unit
        else begin
          let outcome = t.config.lookup_catchup_range
            ~from_epoch:q.from_epoch ~max_epochs:q.max_epochs in
          let make_response status error_code records next_epoch =
            let records_root =
              if is_v2 then C_hash.catchup_records_root records
              else C_hash.catchup_records_root_v1_wire records in
            let sign_bytes = C_hash.catchup_range_response_sign_bytes
              ~chain_id:t.config.chain_id
              ~request_id:q.request_id
              ~responder_addr:t.config.my_addr
              ~from_epoch:q.from_epoch
              ~records_root in
            let signature = t.config.sign_fn sign_bytes in
            C_codec.{
              chain_id = t.config.chain_id;
              request_id = q.request_id;
              status;
              error_code;
              records;
              next_epoch;
              responder_addr = t.config.my_addr;
              signature;
            }
          in
          let response = match outcome with
            | `Ok (records, next_epoch) -> make_response "ok" None records next_epoch
            | `NotFound -> make_response "not_found" (Some "no_data") [] None
            | `Internal err -> make_response "internal_error" (Some err) [] None
          in
          let (payload, response_msg_type) =
            if is_v2 then
              (C_codec.encode_catchup_range_response_v2 response,
               Frame.msg_catchup_range_response_v2)
            else
              (C_codec.encode_catchup_range_response_v1 response,
               Frame.msg_catchup_range_response)
          in
          Octra_net.P2p_swarm.broadcast t.swarm
            { msg_type = response_msg_type; payload }
        end)
        (fun exn ->
          Octra_log.stdout "CONSENSUS [%s]: bad catchup_query_range (v2 = %b): %s\n%!"
            t.config.my_addr is_v2 (Printexc.to_string exn);
          Lwt.return_unit)
    | t' when t' = Frame.msg_catchup_range_response
              || t' = Frame.msg_catchup_range_response_v2 ->
      let is_v2 = (t' = Frame.msg_catchup_range_response_v2) in
      Lwt.catch (fun () ->
        let r =
          if is_v2 then C_codec.decode_catchup_range_response_v2 frame.payload
          else C_codec.decode_catchup_range_response_v1 frame.payload
        in
        if r.chain_id <> t.config.chain_id then Lwt.return_unit
        else if not (C_types.is_validator t.engine.vs r.responder_addr) then begin
          Octra_log.stdout "CONSENSUS [%s]: ignored catchup_range_response (v2 = %b) from non-validator %s\n%!"
            t.config.my_addr is_v2 (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
          Lwt.return_unit
        end else begin

          let from_epoch_opt =
            match Hashtbl.find_opt t.catchup_query_from_epoch r.request_id with
            | Some fe -> Some fe
            | None ->
              (match r.records with
               | first :: _ -> Some first.C_codec.epoch_id
               | [] -> None)
          in
          match from_epoch_opt with
          | None ->
            Octra_log.stdout "CONSENSUS [%s]: ignored catchup_range_response (v2 = %b) request_id unknown + empty records (cannot determine from_epoch for sig verify)\n%!"
              t.config.my_addr is_v2;
            Lwt.return_unit
          | Some from_epoch ->
            let records_root =
              if is_v2 then C_hash.catchup_records_root r.records
              else C_hash.catchup_records_root_v1_wire r.records in
            let sign_bytes = C_hash.catchup_range_response_sign_bytes
              ~chain_id:r.chain_id
              ~request_id:r.request_id
              ~responder_addr:r.responder_addr
              ~from_epoch
              ~records_root in
            if not (C_types.is_validator t.engine.vs r.responder_addr) then begin
              Octra_log.stdout "CONSENSUS [%s]: ignored catchup_range_response (v2 = %b) from non-validator %s\n%!"
                t.config.my_addr is_v2 (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
              Lwt.return_unit
            end else if not (verify_engine_signature t r.responder_addr sign_bytes r.signature) then begin
              Octra_log.stdout "CONSENSUS [%s]: ignored catchup_range_response (v2 = %b) BAD SIGNATURE from %s\n%!"
                t.config.my_addr is_v2 (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
              Octra_net.P2p_swarm.report_bad_peer t.swarm _conn ~reason:"bad_signature_catchup_range";
              Lwt.return_unit
            end else begin
              let checked_epoch, state_root =
                match List.rev r.records with
                | last :: _ -> last.C_codec.epoch_id, Some last.C_codec.state_root
                | [] -> from_epoch, None in
              let head_epoch =
                match r.next_epoch with
                | Some e -> Int64.sub e 1L
                | None -> checked_epoch in
              remember_peer_state t
                ~source:"catchup_range"
                ~responder_addr:r.responder_addr
                ~head_epoch
                ~checked_epoch
                ~state_root;
              let record = {
                responder_addr = r.responder_addr;
                request_id = r.request_id;
                status = r.status;
                records = r.records;
                next_epoch = r.next_epoch;
              } in
              let prior = try Hashtbl.find t.catchup_responses r.request_id with Not_found -> [] in
              let already_seen = List.exists (fun (rec_ : catchup_range_response_record) ->
                rec_.responder_addr = r.responder_addr) prior in
              if not already_seen then
                Hashtbl.replace t.catchup_responses r.request_id (record :: prior);
              Lwt.return_unit
            end
        end)
        (fun exn ->
          Octra_log.stdout "CONSENSUS [%s]: bad catchup_range_response (v2 = %b): %s\n%!"
            t.config.my_addr is_v2 (Printexc.to_string exn);
          Lwt.return_unit)
    | t' when t' = Frame.msg_resource_attestation ->
      Lwt.catch (fun () ->
        let gossip = Resource_attestation_flow.decode_gossip frame.payload in
        if gossip.chain_id <> t.config.chain_id then Lwt.return_unit
        else begin
          let decision = admit_resource_attestation t gossip.attestation in
          Octra_log.stdout
            "CONSENSUS [%s]: resource_attestation decision = %s seen_epoch = %Ld node = %s\n%!"
            t.config.my_addr
            (Resource_attestation_admission.decision_to_string decision)
            gossip.seen_epoch
            (String.sub gossip.attestation.Resource_attestations.node_id 0
              (min 14 (String.length gossip.attestation.Resource_attestations.node_id)));
          match decision with
          | Resource_attestation_admission.Accept ->
              Hashtbl.replace
                t.resource_attestations
                (Resource_attestations.attestation_id gossip.attestation)
                gossip.attestation;
              t.config.on_resource_attestation gossip
          | Resource_attestation_admission.Reject _
          | Resource_attestation_admission.Quarantine _ ->
              Lwt.return_unit
        end)
        (fun exn ->
          Octra_log.stdout "CONSENSUS [%s]: bad resource_attestation: %s\n%!"
            t.config.my_addr (Printexc.to_string exn);
          Lwt.return_unit)
    | _ -> Lwt.return_unit)
  end |> fun p ->
  if engine_output_frame frame.msg_type then
    Lwt.bind p (fun () -> process_outputs t)
  else
    p

let broadcast_resource_attestation t attestation =
  let gossip = Resource_attestation_flow.{
    chain_id = t.config.chain_id;
    seen_epoch = t.config.local_head_epoch ();
    attestation;
  } in
  match admit_resource_attestation t attestation with
  | Resource_attestation_admission.Accept ->
      Hashtbl.replace
        t.resource_attestations
        (Resource_attestations.attestation_id attestation)
        attestation;
      let payload = Resource_attestation_flow.encode_gossip gossip in
      Octra_net.P2p_swarm.broadcast t.swarm { msg_type = Frame.msg_resource_attestation; payload }
  | Resource_attestation_admission.Reject _
  | Resource_attestation_admission.Quarantine _ ->
      Lwt.return_unit

let query_bundle t ~epoch_id ~proposal_id ~timeout_seconds
    ~(validate : bundle_response_record -> bool) =
  let open Lwt.Syntax in
  Hashtbl.remove t.bundle_responses proposal_id;
  let q = C_codec.{
    chain_id = t.config.chain_id;
    epoch_id;
    proposal_id;
  } in
  let payload = C_codec.encode_bundle_query q in
  let* () = Octra_net.P2p_swarm.broadcast t.swarm
    { msg_type = Frame.msg_query_bundle; payload } in
  let rejected : (string, unit) Hashtbl.t = Hashtbl.create 4 in
  let pick_valid responses =
    List.find_opt (fun (rec_ : bundle_response_record) ->
      if Hashtbl.mem rejected rec_.responder_addr then false
      else if validate rec_ then true
      else begin
        Hashtbl.replace rejected rec_.responder_addr ();
        Octra_log.stdout
          "CONSENSUS [%s]: bundle_response from %s rejected by validate\n%!"
          t.config.my_addr
          (String.sub rec_.responder_addr 0 (min 14 (String.length rec_.responder_addr)));
        false
      end
    ) responses
  in
  let rec wait_valid deadline =
    let now = Unix.gettimeofday () in
    if now >= deadline then Lwt.return_none
    else
      let responses =
        try Hashtbl.find t.bundle_responses proposal_id with Not_found -> []
      in
      match pick_valid responses with
      | Some r -> Lwt.return_some r
      | None ->
        let* () = Lwt_unix.sleep 0.1 in
        wait_valid deadline
  in
  let deadline = Unix.gettimeofday () +. timeout_seconds in
  let* result = wait_valid deadline in
  Hashtbl.remove t.bundle_responses proposal_id;
  Lwt.return result

let query_catchup_range t ~from_epoch ~max_epochs ~timeout_seconds
    ~(validate : catchup_range_response_record -> bool) =
  let open Lwt.Syntax in
  let take_prefix n xs =
    let rec loop acc i = function
      | _ when i <= 0 -> List.rev acc
      | [] -> List.rev acc
      | x :: tl -> loop (x :: acc) (i - 1) tl
    in
    loop [] n xs
  in
  let prefix_next_epoch (records : C_codec.catchup_epoch_record list) =
    match List.rev records with
    | last :: _ -> Some (Int64.add last.C_codec.epoch_id 1L)
    | [] -> None
  in
  let request_id =
    Octra_net.Hash_domain.hash "octra:catchup_range_request_id:v1"
      (Printf.sprintf "%s:%Ld:%d:%f"
        t.config.my_addr from_epoch max_epochs (Unix.gettimeofday ()))
  in
  Hashtbl.remove t.catchup_responses request_id;

  Hashtbl.replace t.catchup_query_from_epoch request_id from_epoch;
  let q = C_codec.{
    chain_id = t.config.chain_id;
    request_id;
    from_epoch;
    max_epochs;
  } in
  let payload = C_codec.encode_catchup_query_range q in
  let pick_valid_with rejected responses =
    let required = catchup_agreement_threshold t in
    let groups : (string, int * catchup_range_response_record) Hashtbl.t =
      Hashtbl.create 8 in
    let prefix_groups
      : (string, int * int * catchup_range_response_record) Hashtbl.t =
      Hashtbl.create 16
    in
    List.iter (fun (rec_ : catchup_range_response_record) ->
      if not (Hashtbl.mem rejected rec_.responder_addr) then begin
        if validate rec_ then begin
          let key = catchup_response_key rec_ in
          let count, repr =
            match Hashtbl.find_opt groups key with
            | Some (n, repr) -> (n + 1, repr)
            | None -> (1, rec_)
          in
          Hashtbl.replace groups key (count, repr);
          if rec_.status = "ok" then begin
            let len = List.length rec_.records in
            for prefix_len = 1 to len do
              let prefix_records = take_prefix prefix_len rec_.records in
              let prefix_root = C_hash.catchup_records_root prefix_records in
              let prefix_key =
                "ok|prefix:" ^ string_of_int prefix_len ^ "|" ^ prefix_root
              in
              let prefix_repr = {
                rec_ with
                records = prefix_records;
                next_epoch = prefix_next_epoch prefix_records;
              } in
              let prefix_count, _prefix_best_len, prefix_best_repr =
                match Hashtbl.find_opt prefix_groups prefix_key with
                | Some (n, best_len, best_repr) ->
                  (n + 1, best_len, best_repr)
                | None ->
                  (1, prefix_len, prefix_repr)
              in
              Hashtbl.replace prefix_groups prefix_key
                (prefix_count, prefix_len, prefix_best_repr)
            done
          end
        end else begin
          Hashtbl.replace rejected rec_.responder_addr ();
          Octra_log.stdout
            "CONSENSUS [%s]: catchup_range_response from %s rejected by validate\n%!"
            t.config.my_addr
            (String.sub rec_.responder_addr 0 (min 14 (String.length rec_.responder_addr)));
        end
      end
    ) responses;
    let best_full =
      Hashtbl.fold (fun _ (count, repr) acc ->
        match acc with
        | None -> Some (count, repr)
        | Some (best_count, _best_repr) when count > best_count -> Some (count, repr)
        | _ -> acc
      ) groups None
    in
    let best_prefix =
      Hashtbl.fold (fun _ (count, prefix_len, repr) acc ->
        match acc with
        | None -> Some (count, prefix_len, repr)
        | Some (best_count, best_prefix_len, _best_repr)
          when prefix_len > best_prefix_len
               || (prefix_len = best_prefix_len && count > best_count) ->
          Some (count, prefix_len, repr)
        | _ -> acc
      ) prefix_groups None
    in
    match best_full, best_prefix with
    | Some (count, repr), _ when count >= required ->
      let records_root_hex =
        raw_to_hex (C_hash.catchup_records_root repr.records) in
      Octra_log.stdout
        "CONSENSUS [%s]: catchup_range agreed responders = %d required = %d status = %s root = %s records = %d\n%!"
        t.config.my_addr count required repr.status
        (String.sub records_root_hex 0 (min 16 (String.length records_root_hex)))
        (List.length repr.records);
      Some repr
    | _, Some (count, prefix_len, repr) when count >= required ->
      let records_root_hex =
        raw_to_hex (C_hash.catchup_records_root repr.records) in
      Octra_log.stdout
        "CONSENSUS [%s]: catchup_range agreed PREFIX responders = %d required = %d prefix_records = %d root = %s\n%!"
        t.config.my_addr count required prefix_len
        (String.sub records_root_hex 0 (min 16 (String.length records_root_hex)));
      Some repr
    | _ -> None
  in
  let rec wait_valid_with rejected deadline =
    let now = Unix.gettimeofday () in
    if now >= deadline then Lwt.return_none
    else
      let responses =
        try Hashtbl.find t.catchup_responses request_id with Not_found -> []
      in
      match pick_valid_with rejected responses with
      | Some r -> Lwt.return_some r
      | None ->
        let* () = Lwt_unix.sleep 0.1 in
        wait_valid_with rejected deadline
  in
  let cleanup () =
    Hashtbl.remove t.catchup_responses request_id;
    Hashtbl.remove t.catchup_query_from_epoch request_id
  in

  let v2_budget = max 1.5 (timeout_seconds /. 3.0) in
  let rejected_v2 : (string, unit) Hashtbl.t = Hashtbl.create 4 in
  let* () = Octra_net.P2p_swarm.broadcast t.swarm
    { msg_type = Frame.msg_query_catchup_range_v2; payload } in
  let* v2_result = wait_valid_with rejected_v2 (Unix.gettimeofday () +. v2_budget) in
  match v2_result with
  | Some _ as r ->
    cleanup ();
    Lwt.return r
  | None ->
    Octra_log.stdout "CONSENSUS [%s]: catchup v2 (0x3a) no response within %.1fs, falling back to v1 (0x38)\n%!"
      t.config.my_addr v2_budget;

    let rejected_v1 : (string, unit) Hashtbl.t = Hashtbl.create 4 in
    let* () = Octra_net.P2p_swarm.broadcast t.swarm
      { msg_type = Frame.msg_query_catchup_range; payload } in
    let* v1_result = wait_valid_with rejected_v1 (Unix.gettimeofday () +. timeout_seconds -. v2_budget) in
    cleanup ();
    Lwt.return v1_result

let query_epoch_root t ~epoch_id ~timeout_seconds =
  let open Lwt.Syntax in
  Hashtbl.remove t.epoch_root_responses epoch_id;
  let has_root_quorum records =
    let required = catchup_agreement_threshold t in
    let counts = Hashtbl.create 8 in
    List.exists (fun (r : epoch_root_response_record) ->
      match r.state_root with
      | None -> false
      | Some root ->
        let n = match Hashtbl.find_opt counts root with
          | Some n -> n + 1
          | None -> 1
        in
        Hashtbl.replace counts root n;
        n >= required
    ) records
  in
  let q = C_codec.{
    chain_id = t.config.chain_id;
    epoch_id;
  } in
  let payload = C_codec.encode_epoch_root_query q in
  let* () = Octra_net.P2p_swarm.broadcast t.swarm
    { msg_type = Frame.msg_query_epoch_root; payload } in
  let deadline = Unix.gettimeofday () +. timeout_seconds in
  let rec wait () =
    let collected = match Hashtbl.find_opt t.epoch_root_responses epoch_id with
      | Some records -> records
      | None -> []
    in
    if Unix.gettimeofday () >= deadline || has_root_quorum collected then
      Lwt.return collected
    else
      let* () = Lwt_unix.sleep 0.1 in
      wait ()
  in
  let* collected = wait () in
  Hashtbl.remove t.epoch_root_responses epoch_id;
  Lwt.return collected

let try_propose t ~header ~tx_hashes =
  C_engine.do_propose t.engine header tx_hashes ~sign_fn:t.config.sign_fn;
  process_outputs t

let wake_ready t =
  let open Lwt.Syntax in
  let* _ = try_current_leader_proposal t in
  process_outputs t

let clear_height_local_state t =
  t.proposal_build <- None;
  t.proposal_verify <- None;
  Hashtbl.clear t.pending_votes;
  t.output_loop_requested <- false;
  t.epoch_start_mono <- Mtime_clock.elapsed_ns ()

let start_height t height =
  clear_height_local_state t;
  C_engine.start_height t.engine height;
  process_outputs t

let realign_height t height =
  Octra_log.stdout "realign height = %Ld\n%!"
    t.config.my_addr height;
  start_height t height

let start t =
  t.running <- true;
  let open Lwt.Syntax in
  Octra_log.stdout "driver started addr = %s height = %Ld n = %d quorum = %d\n%!"
    t.config.my_addr t.engine.state.height t.engine.vs.n t.engine.vs.quorum;
  let min_peers =
    if t.n_validators <= 1 then 0
    else max 1 (t.engine.vs.quorum - 1) in
  let rec wait_peers tries =
    let pc = Octra_net.P2p_swarm.connected_count t.swarm in
    if pc >= min_peers || tries <= 0 then Lwt.return_unit
    else begin
      Octra_log.stdout "waiting for peers (%d/%d, need %d)...\n%!" t.config.my_addr pc t.n_validators min_peers;
      let* () = Lwt_unix.sleep 1.0 in
      wait_peers (tries - 1)
    end
  in
  let* () = wait_peers 60 in
  Octra_log.stdout "peers = %d/%d (min = %d), starting consensus\n%!" t.config.my_addr (Octra_net.P2p_swarm.connected_count t.swarm) t.n_validators min_peers;

  let* () = Lwt_unix.sleep 3.0 in
  if C_engine.is_pristine t.engine then
    C_engine.start_height t.engine t.engine.state.height;

  let* () = if C_engine.am_i_leader t.engine then begin
    Octra_log.stdout "proposing height = %Ld\n%!" t.config.my_addr t.engine.state.height;
    let* proposal_opt = t.config.make_proposal t.engine.state.height in
    (match proposal_opt with
    | Some (hdr, txs) ->
      C_engine.do_propose t.engine hdr txs ~sign_fn:t.config.sign_fn;
      Lwt.return_unit
    | None -> Lwt.return_unit)
  end else Lwt.return_unit in
  process_outputs t

let stop t =
  t.running <- false;
  Lwt.return_unit

let current_height t = t.engine.state.height
let current_round t = t.engine.state.round
let current_step t = t.engine.state.step
let am_i_leader t = C_engine.am_i_leader t.engine