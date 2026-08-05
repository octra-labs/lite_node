(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

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

type proposal_plan = {
  header : C_types.epoch_header;
  tx_hashes : string list;
  parent_commit : C_types.parent_commit option;
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
  verify_parent_commit :
    epoch_id:int64 ->
    C_types.parent_commit option ->
    (unit, string) result;
  on_finalized : C_types.finalize -> unit Lwt.t;
  make_proposal : int64 -> proposal_plan option Lwt.t;
  before_precommit_broadcast :
    epoch_id:int64 -> round:int -> proposal_id:string ->
    proposed_state_root:string -> txid_hi:int64 ->
    proposal_wire:string -> vote_wire:string -> bool Lwt.t;
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
  started_at : int64;
}

type pending_finalize = {
  finalize : C_types.finalize;
  validator_set_hash : string;
}

type proposal_height_status =
  | Proposal_current
  | Proposal_stale
  | Proposal_future

type proposal_frame_error =
  | Proposal_signature_or_unknown
  | Proposal_envelope
  | Proposal_parent_commit_hash

type t = {
  mutable n_validators : int;
  config : config;
  engine : C_engine.t;
  swarm : Octra_net.P2p_swarm.t;
  seen : C_seen.t;
  mutable running : bool;
  mutable epoch_start_mono : int64;
  epoch_root_responses : (int64, epoch_root_response_record list) Hashtbl.t;
  bundle_responses : (string, bundle_response_record list) Hashtbl.t;
  catchup_responses : (string, catchup_range_response_record list) Hashtbl.t;
  peer_states : (string, peer_state_record) Hashtbl.t;
  resource_attestations : (string, Resource_attestations.attestation) Hashtbl.t;
  resource_admission : Resource_attestation_admission.pool;
  vote_evidence : (string, C_evidence.vote_conflict) Hashtbl.t;
  activated_validator_set_fingerprints : (string, bool) Hashtbl.t;
  pending_votes : (string, C_types.vote) Hashtbl.t;
  future_votes : (string, C_types.vote) Hashtbl.t;
  durable_votes : (string, unit) Hashtbl.t;
  pending_finalizes : (int64, pending_finalize) Hashtbl.t;
  pending_proposals : (string, C_types.propose) Hashtbl.t;
  deferred_proposals : (string, C_types.propose) Hashtbl.t;
  round_sync_replies : (string, int64) Hashtbl.t;
  catchup_query_from_epoch : (string, int64) Hashtbl.t;
  mutable proposal_build : proposal_build option;
  mutable proposal_retry : proposal_build option;
  mutable proposal_verify : proposal_build option;
  proposal_work_gate : C_proposal_work_gate.t;
  mutable on_validator_set_activated :
    C_types.validator_set -> string -> unit Lwt.t;
  mutable output_loop_active : bool;
  mutable output_loop_requested : bool;
}

let raw_to_hex s =
  String.concat "" (List.init (String.length s)
    (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let log_node addr fmt =
  Printf.ksprintf
    (fun msg ->
      Octra_log.info "consensus" "node = %s module = driver %s" addr msg)
    fmt

let trace_node addr fmt =
  Printf.ksprintf
    (fun msg ->
      Octra_log.trace "consensus" "node = %s module = driver %s" addr msg)
    fmt

let warn_node addr fmt =
  Printf.ksprintf
    (fun msg ->
      Octra_log.warn "consensus" "node = %s module = driver %s" addr msg)
    fmt

let error_node addr fmt =
  Printf.ksprintf
    (fun msg ->
      Octra_log.error "consensus" "node = %s module = driver %s" addr msg)
    fmt

let log fmt =
  Printf.ksprintf
    (fun msg -> Octra_log.info "consensus" "module = driver %s" msg)
    fmt

let catchup_response_key (rec_ : catchup_range_response_record) =
  let records_root = C_hash.catchup_records_root rec_.records in
  let next_epoch_s =
    match rec_.next_epoch with
    | Some e -> Int64.to_string e
    | None -> "-" in
  rec_.status ^ "|" ^ next_epoch_s ^ "|" ^ records_root

let catchup_source_validator_set = C_types.resilient_validator_set

let catchup_agreement_validator_set t ~epoch_id:_ =
  catchup_source_validator_set t.engine.vs

let catchup_agreement_weight t ~epoch_id =
  catchup_agreement_validator_set t ~epoch_id
  |> C_types.round_skip_weight

let catchup_source_agreement_reached validator_set
    ~signer_count ~signed_weight =
  signer_count >= validator_set.C_types.f + 1
  && Z.geq signed_weight (C_types.round_skip_weight validator_set)

let catchup_agreement_reached t ~epoch_id ~count ~weight =
  let validator_set = catchup_agreement_validator_set t ~epoch_id in
  catchup_source_agreement_reached
    validator_set
    ~signer_count:count
    ~signed_weight:weight

let catchup_responder_weight t ~epoch_id responder_addr =
  catchup_agreement_validator_set t ~epoch_id
  |> fun validator_set ->
     match C_types.weight_of_addr validator_set responder_addr with
     | Some weight -> weight
     | None -> Z.zero

let catchup_agreement_epoch ~from_epoch records =
  match List.rev records with
  | last :: _ -> last.C_codec.epoch_id
  | [] -> from_epoch

let create ~config ~validator_set ~swarm ~start_height =
  let validator_set =
    C_types.validator_set_for_epoch
      ~chain_id:config.chain_id
      ~epoch_id:start_height
      validator_set
  in
  let engine = C_engine.create
    ~chain_id:config.chain_id
    ~my_addr:config.my_addr
    ~validator_set
    ~start_height
    ~can_vote:config.can_vote in
  { n_validators = validator_set.C_types.n; config; engine; swarm;
    seen = C_seen.create ~capacity:10_000; running = false;
    epoch_start_mono = Mtime_clock.elapsed_ns ();
    epoch_root_responses = Hashtbl.create 16;
    bundle_responses = Hashtbl.create 16;
    catchup_responses = Hashtbl.create 8;
    peer_states = Hashtbl.create 16;
    resource_attestations = Hashtbl.create 256;
    resource_admission = Resource_attestation_admission.create_pool ();
    vote_evidence = Hashtbl.create 16;
    activated_validator_set_fingerprints = Hashtbl.create 8;
    pending_votes = Hashtbl.create 16;
    future_votes = Hashtbl.create 32;
    durable_votes = Hashtbl.create 16;
    pending_finalizes = Hashtbl.create 16;
    pending_proposals = Hashtbl.create 8;
    deferred_proposals = Hashtbl.create 16;
    round_sync_replies = Hashtbl.create 16;
    catchup_query_from_epoch = Hashtbl.create 8;
    proposal_build = None;
    proposal_retry = None;
    proposal_verify = None;
    proposal_work_gate = C_proposal_work_gate.create ();
    on_validator_set_activated =
      (fun _ _ -> Lwt.return_unit);
    output_loop_active = false;
    output_loop_requested = false }

let set_validator_set_activation_handler t handler =
  t.on_validator_set_activated <- handler

let grace_ms name ~default ~limit =
  match Sys.getenv_opt name with
  | None -> default
  | Some raw ->
    try
      let value = int_of_string raw in
      if value < 0 || value > limit then default else value
    with _ -> default

let proposal_build_grace_ms () =
  grace_ms "OCTRA_BFT_PROPOSAL_BUILD_GRACE_MS"
    ~default:180_000
    ~limit:300_000

let proposal_retry_ms = 500

let proposal_verify_grace_ms () =
  grace_ms "OCTRA_BFT_PROPOSAL_VERIFY_GRACE_MS"
    ~default:180_000
    ~limit:300_000

let within_grace started_at grace_ms =
  let elapsed = Int64.sub (Mtime_clock.elapsed_ns ()) started_at in
  let limit = Int64.mul (Int64.of_int grace_ms) 1_000_000L in
  Int64.compare elapsed 0L >= 0 && Int64.compare elapsed limit < 0

let bounded_timeout value =
  match classify_float value with
  | FP_nan | FP_infinite -> 0.
  | FP_normal | FP_subnormal | FP_zero -> min 300. (max 0. value)

let deadline_after seconds =
  let ns = Int64.of_float (bounded_timeout seconds *. 1e9) in
  Int64.add (Mtime_clock.elapsed_ns ()) ns

let deadline_reached deadline =
  Int64.compare (Mtime_clock.elapsed_ns ()) deadline >= 0

let same_proposal_build b ~gen ~height ~round ~step =
  b.gen = gen
  && b.height = height
  && b.round = round
  && b.step = step

let local_validator t =
  C_types.is_validator t.engine.vs t.config.my_addr

let query_frame msg_type =
  msg_type = Frame.msg_cons_round_sync
  || msg_type = Frame.msg_query_epoch_root
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
  || msg_type = Frame.msg_cons_round_sync

let frame_allowed ~running msg_type =
  running || not (engine_output_frame msg_type)

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
      within_grace b.started_at (proposal_build_grace_ms ())
  | _ -> false

let proposal_retry_pending t ~gen ~height ~round ~step =
  match t.proposal_retry with
  | Some retry
    when same_proposal_build retry ~gen ~height ~round ~step ->
      within_grace retry.started_at proposal_retry_ms
  | _ -> false

let mark_proposal_verify t ~gen ~height ~round ~step =
  t.proposal_verify <- Some {
    gen;
    height;
    round;
    step;
    started_at = Mtime_clock.elapsed_ns ();
  }

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
      within_grace b.started_at (proposal_verify_grace_ms ())
  | _ -> false

let proposal_work_current t work =
  same_proposal_build work
    ~gen:t.engine.generation
    ~height:t.engine.state.height
    ~round:t.engine.state.round
    ~step:t.engine.state.step

let proposal_work_active t =
  let active grace = function
    | Some work ->
      proposal_work_current t work
      && within_grace work.started_at grace
    | None -> false
  in
  active (proposal_build_grace_ms ()) t.proposal_build
  || active (proposal_verify_grace_ms ()) t.proposal_verify

let vote_step_label = function
  | C_types.Prevote -> "PREVOTE"
  | C_types.Precommit -> "PRECOMMIT"

let round_step_label = function
  | C_types.ProposeStep -> "propose"
  | C_types.PrevoteStep -> "prevote"
  | C_types.PrecommitStep -> "precommit"

let proposal_height_status ~current ~proposal =
  match Int64.compare proposal current with
  | 0 -> Proposal_current
  | n when n < 0 -> Proposal_stale
  | _ -> Proposal_future

let proposal_local_status ~engine_head ~local_head ~proposal =
  if Int64.compare proposal local_head <= 0 then Proposal_stale
  else proposal_height_status ~current:engine_head ~proposal

let proposal_frame_error_label = function
  | Proposal_signature_or_unknown -> "signature_or_unknown_validator"
  | Proposal_envelope -> "envelope"
  | Proposal_parent_commit_hash -> "parent_commit_hash"

let proposal_frame_peer_reason = function
  | Proposal_signature_or_unknown -> "bad_signature_propose"
  | Proposal_envelope -> "invalid_frame_propose_envelope"
  | Proposal_parent_commit_hash -> "invalid_frame_propose_parent_commit"

let validate_proposal_frame ~chain_id ~validator_set (p : C_types.propose) =
  match C_types.pubkey_of_addr validator_set p.proposer with
  | None -> Error Proposal_signature_or_unknown
  | Some pubkey when not (C_hash.verify_propose ~pubkey_raw:pubkey p) ->
    Error Proposal_signature_or_unknown
  | Some _
    when not
      (C_types.proposal_is_well_formed
         ~chain_id
         ~validator_set
         p) ->
    Error Proposal_envelope
  | Some _
    when C_hash.parent_commit_hash_opt p.parent_commit
         <> p.header.parent_commit_hash ->
    Error Proposal_parent_commit_hash
  | Some _ -> Ok ()

let proposal_verify_relevant ~current_round ~current_step ~proposal_round =
  proposal_round >= current_round
  && proposal_round - current_round <= C_engine.max_round_ahead
  && (proposal_round > current_round
      || current_step = C_types.ProposeStep)

let proposal_verify_current t (p : C_types.propose) =
  p.epoch_id = t.engine.state.height
  && proposal_verify_relevant
       ~current_round:t.engine.state.round
       ~current_step:t.engine.state.step
       ~proposal_round:p.round

let vote_key (v : C_types.vote) =
  Printf.sprintf "%s|%Ld|%d|%s|%s"
    (vote_step_label v.vote_type)
    v.epoch_id
    v.round
    v.validator
    (raw_to_hex v.proposal_id)

let future_vote_key (v : C_types.vote) =
  Printf.sprintf "%s|%Ld|%d|%s"
    (vote_step_label v.vote_type)
    v.epoch_id
    v.round
    v.validator

type future_vote_result =
  | Future_vote_not_applicable
  | Future_vote_deferred
  | Future_vote_same
  | Future_vote_conflict of C_types.vote

let pending_vote_count t = Hashtbl.length t.pending_votes

let pending_finalize_count t = Hashtbl.length t.pending_finalizes

let max_pending_finalizes = 128

let pending_finalize_mem t epoch =
  Hashtbl.mem t.pending_finalizes epoch

let farthest_pending_finalize t =
  Hashtbl.fold
    (fun epoch _ farthest ->
      match farthest with
      | None -> Some epoch
      | Some old -> Some (Int64.max epoch old))
    t.pending_finalizes None

let queue_vote t (v : C_types.vote) =
  Hashtbl.replace t.pending_votes (vote_key v) v

let defer_future_vote t (v : C_types.vote) =
  let height = t.engine.state.height in
  if height = Int64.max_int
     || v.epoch_id <> Int64.succ height
     || v.round < 0
     || v.round > C_engine.max_round_ahead
  then
    Future_vote_not_applicable
  else
    let key = future_vote_key v in
    match Hashtbl.find_opt t.future_votes key with
    | Some prior when prior.proposal_id = v.proposal_id ->
      Future_vote_same
    | Some prior ->
      Future_vote_conflict prior
    | None ->
      Hashtbl.add t.future_votes key v;
      Future_vote_deferred

let queue_future_finalize t (f : C_types.finalize) =
  if Int64.compare f.epoch_id t.engine.state.height <= 0 then ()
  else begin
    let pending = {
      finalize = f;
      validator_set_hash = C_config.validator_set_hash t.engine.vs;
    } in
    match Hashtbl.find_opt t.pending_finalizes f.epoch_id with
    | Some old when f.commit_round < old.finalize.commit_round ->
      Hashtbl.replace t.pending_finalizes f.epoch_id pending
    | Some _ -> ()
    | None when Hashtbl.length t.pending_finalizes < max_pending_finalizes ->
      Hashtbl.add t.pending_finalizes f.epoch_id pending
    | None ->
      match farthest_pending_finalize t with
      | Some epoch when Int64.compare f.epoch_id epoch < 0 ->
        Hashtbl.remove t.pending_finalizes epoch;
        Hashtbl.add t.pending_finalizes f.epoch_id pending
      | _ -> ()
  end

let vote_still_relevant t (v : C_types.vote) =
  Int64.compare v.epoch_id t.engine.state.height = 0
  && Int64.compare v.epoch_id t.engine.finalized_height > 0

let vote_durable t (v : C_types.vote) =
  let nil_hash = String.make 32 '\x00' in
  if v.vote_type = C_types.Precommit && v.proposal_id <> nil_hash then
    let key = vote_key v in
    if Hashtbl.mem t.durable_votes key then Lwt.return_true
    else
      match
        C_engine.find_proposal_message
          t.engine
          ~proposal_id:v.proposal_id
          ~round:v.round
      with
      | Some proposal ->
        let open Lwt.Syntax in
        let* durable =
          t.config.before_precommit_broadcast
            ~epoch_id:v.epoch_id ~round:v.round
            ~proposal_id:v.proposal_id
            ~proposed_state_root:proposal.header.proposed_state_root
            ~txid_hi:proposal.header.txid_hi
            ~proposal_wire:(C_codec.encode_propose proposal)
            ~vote_wire:(C_codec.encode_vote v)
        in
        if durable then Hashtbl.replace t.durable_votes key ();
        Lwt.return durable
      | None ->
        error_node t.config.my_addr
          "event = refuse_precommit_broadcast reason = proposal_missing epoch = %Ld round = %d"
          v.epoch_id
          v.round;
        Lwt.return_false
  else
    Lwt.return_true

let broadcast_vote t (v : C_types.vote) =
  let open Lwt.Syntax in
  let* allowed = vote_durable t v in
  if not allowed then begin
    error_node t.config.my_addr
      "event = refuse_vote_broadcast type = %s epoch = %Ld round = %d"
      (vote_step_label v.vote_type)
      v.epoch_id
      v.round;
    Lwt.return_false
  end else begin
    trace_node t.config.my_addr
      "event = send_vote type = %s epoch = %Ld round = %d pid = %s"
      (vote_step_label v.vote_type)
      v.epoch_id
      v.round
      (let h =
         Digestif.SHA256.to_hex
           (Digestif.SHA256.of_raw_string v.proposal_id)
       in
       if String.length h >= 8 then String.sub h 0 8 else h);
    let payload = C_codec.encode_vote v in
    let* () =
      Octra_net.P2p_swarm.broadcast t.swarm
        { msg_type = Frame.msg_cons_vote; payload }
    in
    Lwt.return_true
  end

let make_round_sync t ~request =
  let unsigned =
    C_codec.{
      chain_id = t.config.chain_id;
      epoch_id = t.engine.state.height;
      round = t.engine.state.round;
      step = t.engine.state.step;
      request;
      validator = t.config.my_addr;
      signature = String.make 64 '\x00';
    }
  in
  {
    unsigned with
    signature = t.config.sign_fn (C_hash.round_sync_sign_bytes unsigned);
  }

let round_sync_frame sync =
  {
    Frame.msg_type = Frame.msg_cons_round_sync;
    payload = C_codec.encode_round_sync sync;
  }

let broadcast_round_sync t ~request =
  if not (local_validator t)
     || (not request && not (t.config.can_vote ())) then
    Lwt.return_unit
  else
    Octra_net.P2p_swarm.broadcast
      t.swarm
      (round_sync_frame (make_round_sync t ~request))

let local_round_votes t =
  let local_vote set =
    Hashtbl.find_opt set.C_engine.votes t.config.my_addr
  in
  [local_vote t.engine.prevotes; local_vote t.engine.precommits]
  |> List.filter_map Fun.id

let local_round_proposal t =
  match t.engine.current_proposal with
  | Some proposal
    when proposal.epoch_id = t.engine.state.height
      && proposal.round = t.engine.state.round ->
    Some proposal
  | _ -> None

let send_vote_to t conn vote =
  let open Lwt.Syntax in
  let* durable = vote_durable t vote in
  if not durable then begin
    error_node t.config.my_addr
      "event = refuse_round_sync_vote type = %s epoch = %Ld round = %d"
      (vote_step_label vote.C_types.vote_type)
      vote.epoch_id
      vote.round;
    Lwt.return_unit
  end else
    Octra_net.P2p_conn.send
      conn
      {
        Frame.msg_type = Frame.msg_cons_vote;
        payload = C_codec.encode_vote vote;
      }

let send_current_round t conn =
  let open Lwt.Syntax in
  if not (local_validator t) || not (t.config.can_vote ()) then
    Lwt.return_unit
  else begin
    let* () =
      Octra_net.P2p_conn.send
        conn
        (round_sync_frame (make_round_sync t ~request:false))
    in
    let* () =
      match local_round_proposal t with
      | None -> Lwt.return_unit
      | Some proposal ->
        Octra_net.P2p_conn.send
          conn
          {
            Frame.msg_type = Frame.msg_cons_propose;
            payload = C_codec.encode_propose proposal;
          }
    in
    Lwt_list.iter_s (send_vote_to t conn) (local_round_votes t)
  end

let round_step_rank = function
  | C_types.ProposeStep -> 1
  | C_types.PrevoteStep -> 2
  | C_types.PrecommitStep -> 3

let round_sync_reply_needed t (sync : C_codec.round_sync) =
  sync.request
  || sync.round < t.engine.state.round
  || (sync.round = t.engine.state.round
      && round_step_rank sync.step < round_step_rank t.engine.state.step)

let round_sync_response_due ~last ~now =
  match last with
  | None -> true
  | Some prior ->
    Int64.compare now prior >= 0
    && Int64.compare (Int64.sub now prior) 1_000_000_000L >= 0

let round_sync_response_allowed t validator =
  let now = Mtime_clock.elapsed_ns () in
  let last = Hashtbl.find_opt t.round_sync_replies validator in
  if round_sync_response_due ~last ~now then begin
    Hashtbl.replace t.round_sync_replies validator now;
    true
  end else
    false

let flush_pending_votes t =
  if not (t.config.can_vote ()) then Lwt.return_true
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
    | [] -> Lwt.return_true
    | _ ->
      trace_node t.config.my_addr "event = flush_pending_votes count = %d"
        (List.length votes);
      Lwt_list.fold_left_s
        (fun allowed vote ->
          if allowed then broadcast_vote t vote
          else Lwt.return_false)
        true
        votes
  end

let verify_engine_signature t addr msg signature =
  match C_types.pubkey_of_addr t.engine.vs addr with
  | Some pk -> C_hash.verify_ed25519 ~pubkey_raw:pk ~msg ~signature
  | None -> false

let proposal_round_key epoch round =
  Printf.sprintf "%Ld|%d" epoch round

let defer_verified_proposal t (p : C_types.propose) =
  let leader =
    C_engine.leader_of t.engine.vs ~epoch_id:p.epoch_id ~round:p.round
  in
  if leader.address <> p.proposer then ()
  else
    let key = proposal_round_key p.epoch_id p.round in
    if not (Hashtbl.mem t.deferred_proposals key) then
      Hashtbl.add t.deferred_proposals key p

let defer_pending_proposal t (p : C_types.propose) =
  let height = t.engine.state.height in
  if height = Int64.max_int
     || p.epoch_id <> Int64.succ height
     || p.round < 0
     || p.round > C_engine.max_round_ahead
  then
    false
  else
    let key = proposal_round_key p.epoch_id p.round in
    if Hashtbl.mem t.pending_proposals key then
      false
    else begin
      Hashtbl.add t.pending_proposals key p;
      true
    end

let replay_deferred_proposal t =
  let height = t.engine.state.height in
  let round = t.engine.state.round in
  let key = proposal_round_key height round in
  match Hashtbl.find_opt t.deferred_proposals key with
  | None -> ()
  | Some p ->
    Hashtbl.remove t.deferred_proposals key;
    log_node t.config.my_addr
      "event = replay_deferred_proposal epoch = %Ld round = %d"
      height round;
    C_engine.on_propose
      t.engine
      p
      ~verify_fn:(verify_engine_signature t)
      ~execute_fn:(fun _ -> true)
      ~sign_fn:t.config.sign_fn

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
  not (C_seen.remember t.seen id)

let resource_attestation_pool t =
  Hashtbl.fold (fun _ attestation acc -> attestation :: acc) t.resource_attestations []

let vote_evidence_window = 128L
let max_vote_evidence = 4_096

let prune_vote_evidence t epoch =
  let oldest = Int64.sub epoch vote_evidence_window in
  Hashtbl.filter_map_inplace
    (fun _ evidence ->
      if evidence.C_evidence.second.epoch_id < oldest then None
      else Some evidence)
    t.vote_evidence

let remember_vote_evidence t evidence =
  prune_vote_evidence t evidence.C_evidence.second.epoch_id;
  let slot = C_evidence.vote_slot_id evidence.C_evidence.second in
  if Hashtbl.mem t.vote_evidence slot then false
  else if Hashtbl.length t.vote_evidence >= max_vote_evidence then false
  else begin
    Hashtbl.replace t.vote_evidence slot evidence;
    true
  end

let vote_evidence t =
  Hashtbl.fold (fun _ evidence acc -> evidence :: acc) t.vote_evidence []
  |> List.sort (fun left right ->
    String.compare
      (C_evidence.vote_conflict_id left)
      (C_evidence.vote_conflict_id right))

let vote_evidence_root t =
  C_evidence.vote_conflict_root (vote_evidence t)

let vote_evidence_in_window t evidence =
  let epoch = evidence.C_evidence.second.epoch_id in
  let current = t.engine.state.height in
  epoch <= current && epoch >= Int64.sub current vote_evidence_window

let verify_vote_evidence t evidence =
  let vote = evidence.C_evidence.second in
  vote.chain_id = t.config.chain_id
  && vote_evidence_in_window t evidence
  && match C_types.pubkey_of_addr t.engine.vs vote.validator with
     | None -> false
     | Some pubkey -> C_evidence.verify_vote_conflict ~pubkey_raw:pubkey evidence

let vote_evidence_frame evidence =
  {
    Frame.msg_type = Frame.msg_vote_evidence;
    payload = C_evidence.encode_vote_conflict evidence;
  }

let record_vote_conflict ?conn t prior vote =
  match C_evidence.vote_conflict prior vote with
  | None -> None
  | Some evidence ->
    let evidence_id = C_evidence.vote_conflict_id evidence in
    let remembered = remember_vote_evidence t evidence in
    error_node t.config.my_addr
      "event = vote_equivocation epoch = %Ld round = %d validator = %s evidence = %s stored = %b"
      vote.epoch_id
      vote.round
      (String.sub vote.validator 0 (min 12 (String.length vote.validator)))
      (Digestif.SHA256.to_hex
        (Digestif.SHA256.of_raw_string evidence_id)
       |> fun hex -> String.sub hex 0 16)
      remembered;
    (match conn with
     | Some value ->
       Octra_net.P2p_swarm.report_bad_peer t.swarm value
         ~reason:"vote_equivocation"
     | None -> ());
    if remembered then Some evidence else None

let maybe_activate_scheduled_validator_set_raw t ~target_epoch =
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
      let validator_set =
        C_types.validator_set_for_epoch
          ~chain_id:t.config.chain_id
          ~epoch_id:target_epoch
          cfg.validator_set
      in
      let* () =
        t.on_validator_set_activated
          validator_set
          cfg.fingerprint
      in
      C_engine.replace_validator_set t.engine validator_set;
      t.n_validators <- validator_set.C_types.n;
      Hashtbl.replace t.activated_validator_set_fingerprints cfg.fingerprint true;
      log_node t.config.my_addr
        "event = validator_set_activated target_epoch = %Ld n = %d quorum = %d fingerprint = %s"
        target_epoch validator_set.n validator_set.quorum cfg.fingerprint;
      if C_types.is_validator validator_set t.config.my_addr then
        Lwt.return_unit
      else begin
        log_node t.config.my_addr
          "event = validator_set_observer target_epoch = %Ld"
          target_epoch;
        Lwt.return_unit
      end
    end

let maybe_activate_quorum_policy t ~target_epoch =
  let open Lwt.Syntax in
  let current = t.engine.vs in
  let effective =
    C_types.validator_set_for_epoch
      ~chain_id:t.config.chain_id
      ~epoch_id:target_epoch
      current
  in
  if String.equal
       (C_config.validator_set_hash current)
       (C_config.validator_set_hash effective)
  then
    Lwt.return_unit
  else
    let fingerprint =
      C_config.validator_set_hash effective
      |> raw_to_hex
    in
    let* () = t.on_validator_set_activated effective fingerprint in
    C_engine.replace_validator_set t.engine effective;
    t.n_validators <- effective.n;
    log_node t.config.my_addr
      "event = validator_quorum_policy_activated target_epoch = %Ld n = %d quorum = %d total_weight = %s quorum_weight = %s fingerprint = %s"
      target_epoch
      effective.n
      effective.quorum
      (Z.to_string effective.total_weight)
      (Z.to_string effective.quorum_weight)
      fingerprint;
    Lwt.return_unit

let maybe_activate_scheduled_validator_set t ~target_epoch =
  let open Lwt.Syntax in
  let* () = maybe_activate_scheduled_validator_set_raw t ~target_epoch in
  maybe_activate_quorum_policy t ~target_epoch

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
                  log_node t.config.my_addr
                    "event = resource_committee_pending target_epoch = %Ld source_epoch = %Ld weight = %Ld minimum = %Ld"
                    target_epoch snapshot.source_epoch snapshot.total_weight cfg.minimum_weight;
                  Lwt.return_unit
              | Some snapshot ->
                  match Resource_attestation_flow.validator_set_of_committee
                    ~pubkey_of_node:cfg.pubkey_of_node snapshot.committee with
                  | None ->
                      log_node t.config.my_addr
                        "event = resource_committee_rejected target_epoch = %Ld reason = missing_pubkey"
                        target_epoch;
                      Lwt.return_unit
                  | Some validator_set ->
                      C_engine.replace_validator_set t.engine validator_set;
                      t.n_validators <- validator_set.C_types.n;
                      let root_hex = raw_to_hex snapshot.committee_root in
                      log_node t.config.my_addr
                        "event = resource_committee_activated target_epoch = %Ld source_epoch = %Ld n = %d quorum = %d weight = %Ld root = %s"
                        target_epoch snapshot.source_epoch
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
    if proposal_retry_pending t ~gen ~height ~round ~step then
      Lwt.return_false
    else begin
      t.proposal_retry <- None;
      match t.proposal_build with
      | Some _ ->
        Lwt.return_false
      | None ->
        let work = {
          gen;
          height;
          round;
          step;
          started_at = Mtime_clock.elapsed_ns ();
        } in
        t.proposal_build <- Some work;
        let* proposal_opt =
          Lwt.finalize
            (fun () ->
              C_proposal_work_gate.run
                t.proposal_work_gate
                ~relevant:(fun () ->
                  t.running
                  && C_engine.am_i_leader t.engine
                  && proposal_work_current t work)
                (fun () -> t.config.make_proposal height))
            (fun () ->
              clear_proposal_build t ~gen ~height ~round ~step;
              Lwt.return_unit)
        in
        if t.engine.generation = gen
           && t.engine.state.height = height
           && t.engine.state.round = round
           && t.engine.state.step = step then
          match proposal_opt with
          | Some plan ->
            C_engine.do_propose
              ?parent_commit:plan.parent_commit
              t.engine
              plan.header
              plan.tx_hashes
              ~sign_fn:t.config.sign_fn;
            Lwt.return (t.engine.state.step <> step)
          | None ->
            t.proposal_retry <- Some {
              work with
              started_at = Mtime_clock.elapsed_ns ();
            };
            log_node t.config.my_addr
              "event = make_proposal_none height = %Ld round = %d"
              height
              round;
            Lwt.return_false
        else begin
          log_node t.config.my_addr
            "event = make_proposal_stale old_generation = %d new_generation = %d old_height = %Ld new_height = %Ld old_round = %d new_round = %d old_step = %s new_step = %s"
            gen
            t.engine.generation
            height
            t.engine.state.height
            round
            t.engine.state.round
            (round_step_label step)
            (round_step_label t.engine.state.step);
          Lwt.return_false
        end
    end
  end else
    Lwt.return_false

let admit_resource_attestation t attestation =
  match t.config.resource_committee_config with
  | None ->
      Resource_attestation_admission.Reject
        Resource_attestation_admission.Disabled
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

let admit_current_proposal t (p : C_types.propose) =
  let open Lwt.Syntax in
  let signature_valid =
    match C_types.pubkey_of_addr t.engine.vs p.proposer with
    | None -> false
    | Some pubkey -> C_hash.verify_propose ~pubkey_raw:pubkey p
  in
  let envelope_valid =
    C_types.proposal_is_well_formed
      ~chain_id:t.config.chain_id
      ~validator_set:t.engine.vs
      p
    && C_hash.parent_commit_hash_opt p.parent_commit
       = p.header.parent_commit_hash
  in
  if not signature_valid
     || not envelope_valid
     || not
          (proposal_verify_relevant
             ~current_round:t.engine.state.round
             ~current_step:t.engine.state.step
             ~proposal_round:p.round)
  then
    Lwt.return_unit
  else
    let* preview =
      C_proposal_work_gate.run
        t.proposal_work_gate
        ~relevant:(fun () -> proposal_verify_current t p)
        (fun () ->
          let gen = t.engine.generation in
          let height = t.engine.state.height in
          let round = t.engine.state.round in
          let step = t.engine.state.step in
          mark_proposal_verify t ~gen ~height ~round ~step;
          let* valid =
            Lwt.finalize
              (fun () -> t.config.verify_proposal p)
              (fun () ->
                clear_proposal_verify t ~gen ~height ~round ~step;
                Lwt.return_unit)
          in
          Lwt.return_some valid)
    in
    match preview with
    | None ->
      Lwt.return_unit
    | Some _ when not (proposal_verify_current t p) ->
      Lwt.return_unit
    | Some preview_ok ->
      let* () =
        if preview_ok then
          let payload = C_codec.encode_propose p in
          Octra_net.P2p_swarm.broadcast t.swarm
            { msg_type = Frame.msg_cons_propose; payload }
        else
          Lwt.return_unit
      in
      if preview_ok && p.round > t.engine.state.round then
        defer_verified_proposal t p;
      ignore
        (C_engine.on_propose
           t.engine
           p
           ~verify_fn:(verify_engine_signature t)
           ~execute_fn:(fun _ -> preview_ok)
           ~sign_fn:t.config.sign_fn);
      Lwt.return_unit

let replay_pending_proposal t =
  let height = t.engine.state.height in
  let round = t.engine.state.round in
  Hashtbl.filter_map_inplace
    (fun _ (p : C_types.propose) ->
      if p.epoch_id < height then None else Some p)
    t.pending_proposals;
  let key = proposal_round_key height round in
  match Hashtbl.find_opt t.pending_proposals key with
  | None -> Lwt.return_unit
  | Some p ->
    Hashtbl.remove t.pending_proposals key;
    log_node t.config.my_addr
      "event = replay_pending_proposal epoch = %Ld round = %d"
      height
      round;
    admit_current_proposal t p

let vote_type_rank = function
  | C_types.Prevote -> 0
  | C_types.Precommit -> 1

let compare_future_vote (left : C_types.vote) (right : C_types.vote) =
  match
    Int.compare
      (vote_type_rank left.vote_type)
      (vote_type_rank right.vote_type)
  with
  | 0 ->
    (match Int.compare left.round right.round with
     | 0 -> String.compare left.validator right.validator
     | value -> value)
  | value -> value

let replay_future_votes t =
  let height = t.engine.state.height in
  let round = t.engine.state.round in
  let ready = ref [] in
  Hashtbl.filter_map_inplace
    (fun _ (vote : C_types.vote) ->
      if vote.epoch_id < height then
        None
      else if vote.epoch_id = height && vote.round = round then begin
        ready := vote :: !ready;
        None
      end else
        Some vote)
    t.future_votes;
  let evidence =
    List.sort compare_future_vote !ready
    |> List.filter_map (fun (vote : C_types.vote) ->
    match C_types.pubkey_of_addr t.engine.vs vote.validator with
    | Some pubkey when C_hash.verify_vote ~pubkey_raw:pubkey vote ->
      (match C_engine.conflicting_vote t.engine vote with
       | Some prior -> record_vote_conflict t prior vote
       | None ->
         C_engine.on_vote t.engine vote ~sign_fn:t.config.sign_fn;
         None)
    | _ -> None)
  in
  (match !ready with
   | [] -> ()
   | _ ->
     log_node t.config.my_addr
       "event = replay_future_votes epoch = %Ld round = %d count = %d"
       height
       round
       (List.length !ready));
  evidence

let rec process_outputs_once t =
  let open Lwt.Syntax in
  C_engine.on_ready t.engine ~sign_fn:t.config.sign_fn;
  let* () = replay_pending_proposal t in
  let replayed_evidence = replay_future_votes t in
  let* () =
    Lwt_list.iter_s
      (fun evidence ->
        Octra_net.P2p_swarm.broadcast t.swarm
          (vote_evidence_frame evidence))
      replayed_evidence
  in
  replay_deferred_proposal t;
  let current_height = t.engine.state.height in
  let validator_set_hash = C_config.validator_set_hash t.engine.vs in
  Hashtbl.filter_map_inplace (fun epoch pending ->
    if Int64.compare epoch t.engine.C_engine.finalized_height <= 0 then None
    else if pending.validator_set_hash <> validator_set_hash then begin
      warn_node t.config.my_addr
        "event = drop_pending_finalize epoch = %Ld reason = validator_set_changed"
        epoch;
      None
    end
    else if Int64.compare epoch current_height = 0 then begin
      let accepted =
        C_engine.accept_finalize_batch t.engine pending.finalize
      in
      log_node t.config.my_addr
        "event = pending_finalize_current epoch = %Ld accepted = %b"
        epoch accepted;
      None
    end else
      Some pending
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
    trace_node t.config.my_addr "event = engine_output output = %s" name
  ) outputs;
  let proceed task =
    let* () = task in
    Lwt.return_true
  in
  let* pending_allowed = flush_pending_votes t in
  let* output_allowed =
    if not pending_allowed then Lwt.return_false
    else
      Lwt_list.fold_left_s
        (fun allowed output ->
          if not allowed then Lwt.return_false
          else
            match output with
            | C_engine.SendPropose p ->
              trace_node t.config.my_addr
                "event = send_propose epoch = %Ld round = %d"
                p.epoch_id
                p.round;
              let payload = C_codec.encode_propose p in
              proceed
                (Octra_net.P2p_swarm.broadcast t.swarm
                  { msg_type = Frame.msg_cons_propose; payload })
            | C_engine.SendFinalize f ->
              trace_node t.config.my_addr
                "event = send_finalize epoch = %Ld round = %d pid = %s creator = %s precommits = %d"
                f.epoch_id
                f.commit_round
                (let h =
                   Digestif.SHA256.to_hex
                     (Digestif.SHA256.of_raw_string f.proposal_id)
                 in
                 if String.length h >= 16 then String.sub h 0 16 else h)
                (String.sub
                  f.header.creator_addr
                  0
                  (min 14 (String.length f.header.creator_addr)))
                (List.length f.precommits);
              let payload = C_codec.encode_finalize f in
              proceed
                (Octra_net.P2p_swarm.broadcast t.swarm
                  { msg_type = Frame.msg_cons_finalize; payload })
            | C_engine.ScheduleTimeout { step; round; delay_ms; generation } ->
              let* () =
                if step = C_types.ProposeStep then
                  broadcast_round_sync t ~request:false
                else
                  Lwt.return_unit
              in
              Lwt.async (fun () ->
                let* () =
                  Lwt_unix.sleep (float_of_int delay_ms /. 1000.0)
                in
                let rec fire () =
                  if not t.running then Lwt.return_unit
                  else if
                    step = C_types.ProposeStep
                    && proposal_build_active t ~generation ~round ~step
                    && proposal_build_grace_left t ~generation ~round ~step
                  then begin
                    log_node t.config.my_addr
                      "event = defer_propose_timeout reason = proposal_build_active height = %Ld round = %d"
                      t.engine.state.height
                      round;
                    let* () = Lwt_unix.sleep 0.5 in
                    fire ()
                  end else if
                    step = C_types.ProposeStep
                    && proposal_verify_grace_left t ~generation ~round ~step
                  then begin
                    log_node t.config.my_addr
                      "event = defer_propose_timeout reason = proposal_verify_active height = %Ld round = %d"
                      t.engine.state.height
                      round;
                    let* () = Lwt_unix.sleep 0.5 in
                    fire ()
                  end else begin
                    C_engine.on_timeout
                      t.engine
                      ~step
                      ~round
                      ~generation
                      ~sign_fn:t.config.sign_fn;
                    let* _ = try_current_leader_proposal t in
                    process_outputs t
                  end
                in
                fire ());
              Lwt.return_true
            | C_engine.SendVote v ->
              if not (t.config.can_vote ()) then begin
                let* durable = vote_durable t v in
                if not durable then begin
                  error_node t.config.my_addr
                    "event = refuse_vote_queue type = %s epoch = %Ld round = %d"
                    (vote_step_label v.vote_type)
                    v.epoch_id
                    v.round;
                  Lwt.return_false
                end else begin
                  queue_vote t v;
                  log_node t.config.my_addr
                    "event = queue_vote_not_ready type = %s epoch = %Ld round = %d pending = %d"
                    (vote_step_label v.vote_type)
                    v.epoch_id
                    v.round
                    (pending_vote_count t);
                  Lwt.return_true
                end
              end else
                broadcast_vote t v
            | C_engine.Finalized { epoch_id; finalize } ->
              let header = finalize.header in
              let round = finalize.commit_round in
              log_node t.config.my_addr
                "event = finalized epoch = %Ld round = %d creator = %s root = %s"
                epoch_id
                round
                (String.sub
                  header.creator_addr
                  0
                  (min 14 (String.length header.creator_addr)))
                (let h =
                   Digestif.SHA256.to_hex
                     (Digestif.SHA256.of_raw_string
                       header.proposed_state_root)
                 in
                 if String.length h >= 16 then String.sub h 0 16 else h);
              let* () = t.config.on_finalized finalize in
              let* () =
                maybe_activate_scheduled_validator_set
                  t
                  ~target_epoch:(Int64.add epoch_id 1L)
              in
              let* () =
                maybe_activate_resource_committee
                  t
                  ~target_epoch:(Int64.add epoch_id 1L)
              in
              let next = Int64.add epoch_id 1L in
              Hashtbl.clear t.durable_votes;
              C_engine.start_height t.engine next;
              let now_ns = Mtime_clock.elapsed_ns () in
              let slot_ns =
                Int64.of_float (Epoch_time.interval_seconds *. 1e9)
              in
              let target_ns = Int64.add t.epoch_start_mono slot_ns in
              let monotonic_remaining_ns = Int64.sub target_ns now_ns in
              let epoch_time_remaining_ns =
                Int64.mul
                  (Epoch_time.next_delay_ms
                     ~now:(Unix.gettimeofday ())
                     ~previous:header.ts)
                  1_000_000L
              in
              let remaining_ns =
                if epoch_time_remaining_ns > monotonic_remaining_ns then
                  epoch_time_remaining_ns
                else
                  monotonic_remaining_ns
              in
              let carry_ms =
                if now_ns > target_ns then
                  Int64.div
                    (Int64.sub now_ns target_ns)
                    1_000_000L
                else
                  0L
              in
              let wait_ms =
                if remaining_ns > 0L then
                  Int64.div remaining_ns 1_000_000L
                else
                  0L
              in
              log_node t.config.my_addr
                "event = epoch_pacer height = %Ld carry_ms = %Ld wait_ms = %Ld"
                next
                carry_ms
                wait_ms;
              let* () =
                if remaining_ns > 100_000_000L then
                  Lwt_unix.sleep (Int64.to_float remaining_ns /. 1e9)
                else Lwt.return_unit
              in
              t.epoch_start_mono <- Mtime_clock.elapsed_ns ();
              let* _ = try_current_leader_proposal t in
              let* () = process_outputs t in
              Lwt.return_true)
        true
        outputs
  in
  if not output_allowed then begin
    t.running <- false;
    error_node t.config.my_addr
      "event = consensus_driver_stop reason = vote_not_durable height = %Ld round = %d"
      t.engine.state.height
      t.engine.state.round;
    Lwt.return_unit
  end else if has_finalized then Lwt.return_unit
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
  if not (frame_allowed ~running:t.running frame.msg_type) then begin
    trace_node t.config.my_addr
      "event = defer_consensus_frame reason = driver_not_started type = %d"
      frame.msg_type;
    Lwt.return_unit
  end
  else if (not skip_dedup) && is_seen t frame.msg_type frame.payload then
    Lwt.return_unit
  else begin
    (match frame.msg_type with
    | t' when t' = Frame.msg_cons_round_sync ->
      Lwt.catch
        (fun () ->
          let sync = C_codec.decode_round_sync frame.payload in
          if sync.chain_id <> t.config.chain_id then
            Lwt.return_unit
          else
            match
              proposal_height_status
                ~current:t.engine.state.height
                ~proposal:sync.epoch_id
            with
            | Proposal_stale
            | Proposal_future ->
              Lwt.return_unit
            | Proposal_current ->
              match C_types.pubkey_of_addr t.engine.vs sync.validator with
              | None ->
                warn_node t.config.my_addr
                  "event = reject_round_sync reason = unknown_validator from = %s"
                  (String.sub sync.validator 0
                    (min 12 (String.length sync.validator)));
                Octra_net.P2p_swarm.report_bad_peer
                  t.swarm
                  _conn
                  ~reason:"unknown_round_sync_validator";
                Lwt.return_unit
              | Some pubkey
                when not (C_hash.verify_round_sync ~pubkey_raw:pubkey sync) ->
                warn_node t.config.my_addr
                  "event = reject_round_sync reason = bad_signature from = %s"
                  (String.sub sync.validator 0
                    (min 12 (String.length sync.validator)));
                Octra_net.P2p_swarm.report_bad_peer
                  t.swarm
                  _conn
                  ~reason:"bad_signature_round_sync";
                Lwt.return_unit
              | Some pubkey
                when Octra_net.P2p_handshake.node_id_of_pubkey pubkey
                     <> _conn.Octra_net.P2p_conn.peer_id ->
                warn_node t.config.my_addr
                  "event = reject_round_sync reason = peer_identity_mismatch from = %s"
                  (String.sub sync.validator 0
                    (min 12 (String.length sync.validator)));
                Octra_net.P2p_swarm.report_bad_peer
                  t.swarm
                  _conn
                  ~reason:"round_sync_peer_identity_mismatch";
                Lwt.return_unit
              | Some _ ->
                let current_round = t.engine.state.round in
                if sync.round > current_round + C_engine.max_round_ahead then
                  Lwt.return_unit
                else begin
                  C_engine.on_round_sync
                    t.engine
                    ~round:sync.round
                    ~validator:sync.validator;
                  let* () = process_outputs t in
                  if round_sync_reply_needed t sync
                     && round_sync_response_allowed t sync.validator then
                    send_current_round t _conn
                  else
                    Lwt.return_unit
                end)
        (fun exn ->
          warn_node t.config.my_addr
            "event = bad_round_sync error = %s"
            (Printexc.to_string exn);
          Octra_net.P2p_swarm.report_bad_peer
            t.swarm
            _conn
            ~reason:"invalid_frame_round_sync";
          Lwt.return_unit)
    | t' when t' = Frame.msg_cons_propose ->
      Lwt.catch (fun () ->
        let p = C_codec.decode_propose frame.payload in
        if p.chain_id <> t.config.chain_id then begin
          warn_node t.config.my_addr
            "event = reject_propose reason = chain_id_mismatch got = %s ours = %s"
            p.chain_id t.config.chain_id;
          Lwt.return_unit
        end else
        match proposal_local_status
                ~engine_head:t.engine.state.height
                ~local_head:(t.config.local_head_epoch ())
                ~proposal:p.epoch_id with
        | Proposal_stale ->
          Lwt.return_unit
        | Proposal_future ->
          (match
             validate_proposal_frame
               ~chain_id:t.config.chain_id
               ~validator_set:t.engine.vs
               p
           with
           | Error error ->
             log_node t.config.my_addr
               "event = ignore_future_propose reason = unresolved_validator_set predicate = %s epoch = %Ld local_height = %Ld"
               (proposal_frame_error_label error)
               p.epoch_id
               t.engine.state.height
           | Ok () ->
             if defer_pending_proposal t p then
               log_node t.config.my_addr
                 "event = defer_pending_proposal epoch = %Ld round = %d local_height = %Ld"
                 p.epoch_id
                 p.round
                 t.engine.state.height);
          Lwt.return_unit
        | Proposal_current ->
          (match
             validate_proposal_frame
               ~chain_id:t.config.chain_id
               ~validator_set:t.engine.vs
               p
           with
           | Error error ->
             warn_node t.config.my_addr
               "event = reject_propose reason = %s epoch = %Ld round = %d from = %s"
               (proposal_frame_error_label error)
               p.epoch_id
               p.round
               (String.sub p.proposer 0
                  (min 12 (String.length p.proposer)));
             Octra_net.P2p_swarm.report_bad_peer
               t.swarm
               _conn
               ~reason:(proposal_frame_peer_reason error);
             Lwt.return_unit
           | Ok () ->
             let* () = admit_current_proposal t p in
             process_outputs t)
        )
      (fun exn ->
        warn_node t.config.my_addr "event = bad_propose error = %s"
          (Printexc.to_string exn);
        Octra_net.P2p_swarm.report_bad_peer t.swarm _conn ~reason:"invalid_frame_propose";
        Lwt.return_unit)
    | t' when t' = Frame.msg_cons_vote ->
      Lwt.catch (fun () ->
        let v = C_codec.decode_vote frame.payload in
        if v.chain_id <> t.config.chain_id then begin
          warn_node t.config.my_addr
            "event = reject_vote reason = chain_id_mismatch got = %s ours = %s"
            v.chain_id t.config.chain_id;
          Lwt.return_unit
        end else
        match proposal_local_status
                ~engine_head:t.engine.state.height
                ~local_head:(t.config.local_head_epoch ())
                ~proposal:v.epoch_id with
        | Proposal_stale ->
          Lwt.return_unit
        | status ->
          let valid =
            match C_types.pubkey_of_addr t.engine.vs v.validator with
            | Some pk -> C_hash.verify_vote ~pubkey_raw:pk v
            | None -> false
          in
          if not valid then begin
            (match status with
             | Proposal_current ->
               warn_node t.config.my_addr
                 "event = reject_vote reason = bad_signature_or_unknown_validator from = %s"
                 (String.sub v.validator 0
                    (min 12 (String.length v.validator)));
               Octra_net.P2p_swarm.report_bad_peer
                 t.swarm
                 _conn
                 ~reason:"bad_signature_vote"
             | Proposal_future ->
               log_node t.config.my_addr
                 "event = ignore_future_vote reason = unresolved_validator_set epoch = %Ld local_height = %Ld"
                 v.epoch_id
                 t.engine.state.height
             | Proposal_stale -> ());
            Lwt.return_unit
          end else
            let future =
              match status with
              | Proposal_future -> defer_future_vote t v
              | Proposal_current
              | Proposal_stale -> Future_vote_not_applicable
            in
            (match future with
             | Future_vote_deferred ->
               log_node t.config.my_addr
                 "event = defer_future_vote type = %s epoch = %Ld round = %d validator = %s local_height = %Ld"
                 (vote_step_label v.vote_type)
                 v.epoch_id
                 v.round
                 v.validator
                 t.engine.state.height
             | _ -> ());
            let evidence =
              match status, future with
              | Proposal_future, Future_vote_conflict prior ->
                record_vote_conflict ~conn:_conn t prior v
              | Proposal_future, _ ->
                None
              | Proposal_current, _ ->
                (match C_engine.conflicting_vote t.engine v with
                 | Some prior -> record_vote_conflict ~conn:_conn t prior v
                 | None ->
                   C_engine.on_vote t.engine v ~sign_fn:t.config.sign_fn;
                   None)
              | Proposal_stale, _ ->
                None
            in
            let* () =
              match evidence with
              | None -> Lwt.return_unit
              | Some value ->
                Octra_net.P2p_swarm.broadcast
                  t.swarm
                  (vote_evidence_frame value)
            in
            Lwt.return_unit)
      (fun exn ->
        warn_node t.config.my_addr "event = bad_vote error = %s"
          (Printexc.to_string exn);
        Octra_net.P2p_swarm.report_bad_peer t.swarm _conn ~reason:"invalid_frame_vote";
        Lwt.return_unit)
    | t' when t' = Frame.msg_cons_finalize ->
      Lwt.catch (fun () ->
        let f = C_codec.decode_finalize frame.payload in
        if f.chain_id <> t.config.chain_id then begin
          warn_node t.config.my_addr
            "event = reject_finalize reason = chain_id_mismatch got = %s ours = %s"
            f.chain_id t.config.chain_id;
          Lwt.return_unit
        end else
        match proposal_local_status
                ~engine_head:t.engine.state.height
                ~local_head:(t.config.local_head_epoch ())
                ~proposal:f.epoch_id with
        | Proposal_stale ->
          Lwt.return_unit
        | Proposal_future ->
          log_node t.config.my_addr
            "event = ignore_future_finalize reason = unresolved_validator_set epoch = %Ld local_height = %Ld"
            f.epoch_id
            t.engine.state.height;
          Lwt.return_unit
        | Proposal_current ->
          match
            t.config.verify_parent_commit
              ~epoch_id:f.epoch_id
              f.parent_commit
          with
          | Error reason ->
            warn_node t.config.my_addr
              "event = reject_finalize reason = parent_commit detail = %s"
              reason;
            Lwt.return_unit
          | Ok () ->
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
            warn_node t.config.my_addr
              "event = reject_finalize reason = qc_%s" reason;
            let peer_reason =
              if reason = "signature" then "bad_signature_finalize"
              else "invalid_frame_finalize_qc_" ^ reason
            in
            Octra_net.P2p_swarm.report_bad_peer t.swarm _conn ~reason:peer_reason;
            Lwt.return_unit
          | C_qc.Valid ->
            let expected_pid = C_hash.proposal_id f.header in
          trace_node t.config.my_addr
            "event = recv_finalize epoch = %Ld commit_round = %d pid = %s creator = %s precommits = %d"
            f.epoch_id f.commit_round
            (let h = Digestif.SHA256.to_hex (Digestif.SHA256.of_raw_string f.proposal_id) in
             if String.length h >= 16 then String.sub h 0 16 else h)
            (String.sub f.header.creator_addr 0 (min 14 (String.length f.header.creator_addr)))
            (List.length f.precommits);
              let accepted = C_engine.accept_finalize_batch t.engine f in
              if accepted then begin
                trace_node t.config.my_addr
                  "event = finalize_batch epoch = %Ld round = %d valid_precommits = %d total_precommits = %d"
                  f.epoch_id f.commit_round
                  (List.length f.precommits)
                  (List.length f.precommits);
                let* () = Octra_net.P2p_swarm.broadcast t.swarm
                  { msg_type = Frame.msg_cons_finalize; payload = frame.payload } in
                process_outputs t
              end else begin
                warn_node t.config.my_addr
                  "event = reject_finalize reason = engine_state epoch = %Ld round = %d pid = %s"
                  f.epoch_id f.commit_round
                  (String.sub expected_pid 0 (min 16 (String.length expected_pid)));
                Lwt.return_unit
              end
        )
      (fun exn ->
        warn_node t.config.my_addr "event = bad_finalize error = %s"
          (Printexc.to_string exn);
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
          log_node t.config.my_addr "event = bad_epoch_root_query error = %s"
            (Printexc.to_string exn);
          Lwt.return_unit)
    | t' when t' = Frame.msg_epoch_root_response ->
      Lwt.catch (fun () ->
        let r = C_codec.decode_epoch_root_response frame.payload in
        if r.chain_id <> t.config.chain_id then Lwt.return_unit
        else if not (Hashtbl.mem t.epoch_root_responses r.epoch_id) then
          Lwt.return_unit
        else if not (C_types.is_validator t.engine.vs r.responder_addr) then begin
          log_node t.config.my_addr
            "event = ignore_epoch_root_response reason = non_validator from = %s"
            (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
          Lwt.return_unit
        end else begin
          let sign_bytes = C_hash.epoch_root_response_sign_bytes
            ~chain_id:r.chain_id ~epoch_id:r.epoch_id
            ~state_root:r.state_root ~responder_addr:r.responder_addr
            ~responder_head_epoch:r.responder_head_epoch in
          if not (verify_engine_signature t r.responder_addr sign_bytes r.signature) then begin
            log_node t.config.my_addr
              "event = ignore_epoch_root_response reason = bad_signature from = %s"
              (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
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
          log_node t.config.my_addr "event = bad_epoch_root_response error = %s"
            (Printexc.to_string exn);
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
              signature = String.make 64 '\x00';
            } in
            let r = {
              r with
              signature = t.config.sign_fn (C_hash.bundle_response_sign_bytes r);
            } in
            let payload = C_codec.encode_bundle_response r in
            Octra_net.P2p_swarm.broadcast t.swarm
              { msg_type = Frame.msg_bundle_response; payload }
        end)
        (fun exn ->
          log_node t.config.my_addr "event = bad_bundle_query error = %s"
            (Printexc.to_string exn);
          Lwt.return_unit)
    | t' when t' = Frame.msg_bundle_response ->
      Lwt.catch (fun () ->
        let r = C_codec.decode_bundle_response frame.payload in
        if r.chain_id <> t.config.chain_id then Lwt.return_unit
        else if not (Hashtbl.mem t.bundle_responses r.proposal_id) then
          Lwt.return_unit
        else if not (C_types.is_validator t.engine.vs r.responder_addr) then begin
          log_node t.config.my_addr
            "event = ignore_bundle_response reason = non_validator from = %s"
            (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
          Lwt.return_unit
        end else if not (verify_engine_signature t r.responder_addr
          (C_hash.bundle_response_sign_bytes r) r.signature) then begin
          log_node t.config.my_addr
            "event = ignore_bundle_response reason = bad_signature from = %s"
            (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
          Octra_net.P2p_swarm.report_bad_peer t.swarm _conn
            ~reason:"bad_signature_bundle";
          Lwt.return_unit
        end else if List.length r.tx_hashes <> List.length r.txs_json then begin
          log_node t.config.my_addr
            "event = ignore_bundle_response reason = mismatched_lengths from = %s"
            (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
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
          log_node t.config.my_addr "event = bad_bundle_response error = %s"
            (Printexc.to_string exn);
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
          log_node t.config.my_addr
            "event = bad_catchup_query_range v2 = %b error = %s"
            is_v2 (Printexc.to_string exn);
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
        else if not (Hashtbl.mem t.catchup_query_from_epoch r.request_id) then
          Lwt.return_unit
        else if not (C_types.is_validator t.engine.vs r.responder_addr) then begin
          log_node t.config.my_addr
            "event = ignore_catchup_range_response v2 = %b reason = non_validator from = %s"
            is_v2 (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
          Lwt.return_unit
        end else begin

          let from_epoch_opt =
            Hashtbl.find_opt t.catchup_query_from_epoch r.request_id
          in
          match from_epoch_opt with
          | None ->
            log_node t.config.my_addr
              "event = ignore_catchup_range_response v2 = %b reason = unknown_request_empty_records"
              is_v2;
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
              log_node t.config.my_addr
                "event = ignore_catchup_range_response v2 = %b reason = non_validator from = %s"
                is_v2 (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
              Lwt.return_unit
            end else if not (verify_engine_signature t r.responder_addr sign_bytes r.signature) then begin
              log_node t.config.my_addr
                "event = ignore_catchup_range_response v2 = %b reason = bad_signature from = %s"
                is_v2 (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
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
          log_node t.config.my_addr
            "event = bad_catchup_range_response v2 = %b error = %s"
            is_v2 (Printexc.to_string exn);
          Lwt.return_unit)
    | t' when t' = Frame.msg_vote_evidence ->
      Lwt.catch (fun () ->
        let evidence = C_evidence.decode_vote_conflict frame.payload in
        if not (verify_vote_evidence t evidence) then begin
          Octra_net.P2p_swarm.report_bad_peer t.swarm _conn
            ~reason:"invalid_vote_evidence";
          Lwt.return_unit
        end else if not (remember_vote_evidence t evidence) then
          Lwt.return_unit
        else
          Octra_net.P2p_swarm.broadcast_except t.swarm
            ~except:_conn.Octra_net.P2p_conn.peer_id
            (vote_evidence_frame evidence))
      (fun exn ->
        log_node t.config.my_addr "event = bad_vote_evidence error = %s"
          (Printexc.to_string exn);
        Octra_net.P2p_swarm.report_bad_peer t.swarm _conn
          ~reason:"invalid_frame_vote_evidence";
        Lwt.return_unit)
    | t' when t' = Frame.msg_resource_attestation ->
      Lwt.catch (fun () ->
        let gossip = Resource_attestation_flow.decode_gossip frame.payload in
        if gossip.chain_id <> t.config.chain_id then begin
          Octra_net.P2p_swarm.report_bad_peer t.swarm _conn
            ~reason:"resource_attestation_chain_mismatch";
          Lwt.return_unit
        end
        else begin
          let decision = admit_resource_attestation t gossip.attestation in
          log_node t.config.my_addr
            "event = resource_attestation decision = %s seen_epoch = %Ld resource_node = %s"
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
          | Resource_attestation_admission.Reject _ ->
              Octra_net.P2p_swarm.report_bad_peer t.swarm _conn
                ~reason:"invalid_resource_attestation";
              Lwt.return_unit
          | Resource_attestation_admission.Quarantine _ ->
              Lwt.return_unit
        end)
        (fun exn ->
          log_node t.config.my_addr "event = bad_resource_attestation error = %s"
            (Printexc.to_string exn);
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
  Hashtbl.replace t.bundle_responses proposal_id [];
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
        log_node t.config.my_addr
          "event = reject_bundle_response reason = validate from = %s"
          (String.sub rec_.responder_addr 0 (min 14 (String.length rec_.responder_addr)));
        false
      end
    ) responses
  in
  let rec wait_valid deadline =
    if deadline_reached deadline then Lwt.return_none
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
  let deadline = deadline_after timeout_seconds in
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
  let request_nonce = Unix.gettimeofday () in
  let request_id phase =
    Octra_net.Hash_domain.hash "octra:catchup_range_request_id:v1"
      (Printf.sprintf "%s:%Ld:%d:%f:%s"
        t.config.my_addr from_epoch max_epochs request_nonce phase)
  in
  let pick_valid_with rejected responses =
    let groups
      : (string, Z.t * int * int64 * catchup_range_response_record) Hashtbl.t =
      Hashtbl.create 8 in
    let prefix_groups
      : (string, Z.t * int * int * int64 * catchup_range_response_record) Hashtbl.t =
      Hashtbl.create 16
    in
    List.iter (fun (rec_ : catchup_range_response_record) ->
      if not (Hashtbl.mem rejected rec_.responder_addr) then begin
        if validate rec_ then begin
          let agreement_epoch =
            catchup_agreement_epoch ~from_epoch rec_.records
          in
          let responder_weight =
            catchup_responder_weight
              t
              ~epoch_id:agreement_epoch
              rec_.responder_addr
          in
          let key = catchup_response_key rec_ in
          let weight, count, agreement_epoch, repr =
            match Hashtbl.find_opt groups key with
            | Some (weight, count, agreement_epoch, repr) ->
              Z.add weight responder_weight, count + 1, agreement_epoch, repr
            | None -> responder_weight, 1, agreement_epoch, rec_
          in
          Hashtbl.replace groups key
            (weight, count, agreement_epoch, repr);
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
              let prefix_epoch =
                catchup_agreement_epoch ~from_epoch prefix_records
              in
              let prefix_responder_weight =
                catchup_responder_weight
                  t
                  ~epoch_id:prefix_epoch
                  rec_.responder_addr
              in
              let
                prefix_weight,
                prefix_count,
                _prefix_best_len,
                prefix_epoch,
                prefix_best_repr
              =
                match Hashtbl.find_opt prefix_groups prefix_key with
                | Some (weight, count, best_len, prefix_epoch, best_repr) ->
                  Z.add weight prefix_responder_weight,
                  count + 1,
                  best_len,
                  prefix_epoch,
                  best_repr
                | None ->
                  prefix_responder_weight,
                  1,
                  prefix_len,
                  prefix_epoch,
                  prefix_repr
              in
              Hashtbl.replace prefix_groups prefix_key
                (prefix_weight, prefix_count, prefix_len, prefix_epoch,
                 prefix_best_repr)
            done
          end
        end else begin
          Hashtbl.replace rejected rec_.responder_addr ();
          log_node t.config.my_addr
            "event = reject_catchup_range_response reason = validate from = %s"
            (String.sub rec_.responder_addr 0 (min 14 (String.length rec_.responder_addr)));
        end
      end
    ) responses;
    let best_full =
      Hashtbl.fold (fun _ (weight, count, epoch_id, repr) acc ->
        match acc with
        | None -> Some (weight, count, epoch_id, repr)
        | Some (best_weight, _, _, _) when Z.gt weight best_weight ->
          Some (weight, count, epoch_id, repr)
        | _ -> acc
      ) groups None
    in
    let best_prefix =
      Hashtbl.fold (fun _ (weight, count, prefix_len, epoch_id, repr) acc ->
        match acc with
        | None -> Some (weight, count, prefix_len, epoch_id, repr)
        | Some (best_weight, _, best_prefix_len, _, _)
          when prefix_len > best_prefix_len
               || (prefix_len = best_prefix_len
                   && Z.gt weight best_weight) ->
          Some (weight, count, prefix_len, epoch_id, repr)
        | _ -> acc
      ) prefix_groups None
    in
    match best_full, best_prefix with
    | Some (weight, count, epoch_id, repr), _
      when catchup_agreement_reached t ~epoch_id ~count ~weight ->
      let required_weight = catchup_agreement_weight t ~epoch_id in
      let records_root_hex =
        raw_to_hex (C_hash.catchup_records_root repr.records) in
      log_node t.config.my_addr
        "event = catchup_range_agreed responders = %d weight = %s required_weight = %s status = %s root = %s records = %d"
        count
        (Z.to_string weight)
        (Z.to_string required_weight)
        repr.status
        (String.sub records_root_hex 0 (min 16 (String.length records_root_hex)))
        (List.length repr.records);
      Some repr
    | _, Some (weight, count, prefix_len, epoch_id, repr)
      when catchup_agreement_reached t ~epoch_id ~count ~weight ->
      let required_weight = catchup_agreement_weight t ~epoch_id in
      let records_root_hex =
        raw_to_hex (C_hash.catchup_records_root repr.records) in
      log_node t.config.my_addr
        "event = catchup_range_prefix_agreed responders = %d weight = %s required_weight = %s prefix_records = %d root = %s"
        count
        (Z.to_string weight)
        (Z.to_string required_weight)
        prefix_len
        (String.sub records_root_hex 0 (min 16 (String.length records_root_hex)));
      Some repr
    | _ -> None
  in
  let rec wait_valid_with request_id rejected deadline =
    if deadline_reached deadline then Lwt.return_none
    else
      let responses =
        try Hashtbl.find t.catchup_responses request_id with Not_found -> []
      in
      match pick_valid_with rejected responses with
      | Some r -> Lwt.return_some r
      | None ->
        let* () = Lwt_unix.sleep 0.1 in
        wait_valid_with request_id rejected deadline
  in
  let cleanup request_id =
    Hashtbl.remove t.catchup_responses request_id;
    Hashtbl.remove t.catchup_query_from_epoch request_id
  in
  let run_phase ~phase ~msg_type ~budget =
    let request_id = request_id phase in
    let query = C_codec.{
      chain_id = t.config.chain_id;
      request_id;
      from_epoch;
      max_epochs;
    } in
    let payload = C_codec.encode_catchup_query_range query in
    Hashtbl.remove t.catchup_responses request_id;
    Hashtbl.replace t.catchup_query_from_epoch request_id from_epoch;
    let rejected : (string, unit) Hashtbl.t = Hashtbl.create 4 in
    Lwt.finalize
      (fun () ->
        let* () =
          Octra_net.P2p_swarm.broadcast t.swarm { msg_type; payload }
        in
        wait_valid_with request_id rejected (deadline_after budget))
      (fun () ->
        cleanup request_id;
        Lwt.return_unit)
  in
  let total_budget = bounded_timeout timeout_seconds in
  let first_budget =
    min total_budget (max 1.5 (total_budget /. 2.0))
  in
  let* first =
    run_phase
      ~phase:"canonical"
      ~msg_type:Frame.msg_query_catchup_range_v2
      ~budget:first_budget
  in
  match first with
  | Some _ as result ->
    Lwt.return result
  | None ->
    log_node t.config.my_addr
      "event = catchup_canonical_retry timeout_s = %.1f"
      first_budget;
    run_phase
      ~phase:"canonical_retry"
      ~msg_type:Frame.msg_query_catchup_range_v2
      ~budget:(total_budget -. first_budget)

let query_epoch_root t ~epoch_id ~timeout_seconds =
  let open Lwt.Syntax in
  Hashtbl.replace t.epoch_root_responses epoch_id [];
  let has_root_quorum records =
    let counts = Hashtbl.create 8 in
    List.exists (fun (r : epoch_root_response_record) ->
      match r.state_root with
      | None -> false
      | Some root ->
        let responder_weight =
          catchup_responder_weight t ~epoch_id r.responder_addr
        in
        let weight, count =
          match Hashtbl.find_opt counts root with
          | Some (weight, count) -> Z.add weight responder_weight, count + 1
          | None -> responder_weight, 1
        in
        Hashtbl.replace counts root (weight, count);
        catchup_agreement_reached t ~epoch_id ~count ~weight
    ) records
  in
  let q = C_codec.{
    chain_id = t.config.chain_id;
    epoch_id;
  } in
  let payload = C_codec.encode_epoch_root_query q in
  let* () = Octra_net.P2p_swarm.broadcast t.swarm
    { msg_type = Frame.msg_query_epoch_root; payload } in
  let deadline = deadline_after timeout_seconds in
  let rec wait () =
    let collected = match Hashtbl.find_opt t.epoch_root_responses epoch_id with
      | Some records -> records
      | None -> []
    in
    if deadline_reached deadline || has_root_quorum collected then
      Lwt.return collected
    else
      let* () = Lwt_unix.sleep 0.1 in
      wait ()
  in
  let* collected = wait () in
  Hashtbl.remove t.epoch_root_responses epoch_id;
  Lwt.return collected

let try_propose ?parent_commit t ~header ~tx_hashes =
  C_engine.do_propose
    ?parent_commit
    t.engine
    header
    tx_hashes
    ~sign_fn:t.config.sign_fn;
  process_outputs t

let wake_ready t =
  let open Lwt.Syntax in
  let* () = broadcast_round_sync t ~request:true in
  let* _ = try_current_leader_proposal t in
  process_outputs t

let clear_local_transients t =
  t.proposal_build <- None;
  t.proposal_verify <- None;
  Hashtbl.clear t.pending_votes;
  t.output_loop_requested <- false;
  t.epoch_start_mono <- Mtime_clock.elapsed_ns ()

let clear_height_local_state t =
  clear_local_transients t;
  Hashtbl.clear t.durable_votes;
  Hashtbl.clear t.deferred_proposals

let clear_round_local_state t ~height ~round =
  clear_local_transients t;
  Hashtbl.filter_map_inplace
    (fun _ (proposal : C_types.propose) ->
      if proposal.epoch_id = height && proposal.round >= round then
        Some proposal
      else
        None)
    t.deferred_proposals

let start_height t height =
  let open Lwt.Syntax in
  let* () = maybe_activate_scheduled_validator_set t ~target_epoch:height in
  clear_height_local_state t;
  C_engine.start_height t.engine height;
  process_outputs t

let restore_precommit_lock t proposal =
  if t.running then Error "consensus driver is already running"
  else C_engine.restore_precommit_lock t.engine proposal

let realign_progress t ~height ~round =
  let current_height = t.engine.state.height in
  if height < current_height then
    Lwt.return_unit
  else if height > current_height then begin
    log_node t.config.my_addr
      "event = realign_progress old_height = %Ld new_height = %Ld"
      current_height height;
    start_height t height
  end else begin
    let current_round = t.engine.state.round in
    let target_round = max (current_round + 1) (round + 1) in
    log_node t.config.my_addr
      "event = realign_progress height = %Ld old_round = %d new_round = %d"
      height current_round target_round;
    clear_round_local_state t ~height ~round:target_round;
    C_engine.realign_round t.engine target_round;
    process_outputs t
  end

let start t =
  t.running <- true;
  let open Lwt.Syntax in
  log "event = driver_start node = %s height = %Ld n = %d quorum = %d"
    t.config.my_addr t.engine.state.height t.engine.vs.n t.engine.vs.quorum;
  let min_peers =
    if t.n_validators <= 1 then 0
    else max 1 (t.engine.vs.quorum - 1) in
  let rec wait_peers tries =
    let pc = Octra_net.P2p_swarm.connected_count t.swarm in
    if pc >= min_peers || tries <= 0 then Lwt.return_unit
    else begin
      log_node t.config.my_addr
        "event = wait_peers connected = %d validators = %d min = %d"
        pc t.n_validators min_peers;
      let* () = Lwt_unix.sleep 1.0 in
      wait_peers (tries - 1)
    end
  in
  let* () = wait_peers 60 in
  log_node t.config.my_addr
    "event = start_consensus connected = %d validators = %d min = %d"
    (Octra_net.P2p_swarm.connected_count t.swarm) t.n_validators min_peers;

  let* () = Lwt_unix.sleep 3.0 in
  let* () =
    if C_engine.is_pristine t.engine then
      start_height t t.engine.state.height
    else
      maybe_activate_scheduled_validator_set
        t
        ~target_epoch:t.engine.state.height
  in
  let* () = broadcast_round_sync t ~request:true in
  let* _ = try_current_leader_proposal t in
  process_outputs t

let stop t =
  t.running <- false;
  Lwt.return_unit

let current_height t = t.engine.state.height
let current_round t = t.engine.state.round
let current_step t = t.engine.state.step
let active_validator_set t = t.engine.vs
let am_i_leader t = C_engine.am_i_leader t.engine