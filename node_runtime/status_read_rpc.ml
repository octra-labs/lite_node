(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Ledger = Octra_core.Ledger
module Metrics = Octra_core.Metrics
module Store_chaindata = Octra_core.Store_chaindata
module Store_irmin = Octra_core.Store_irmin
module Staging = Octra_core.Tx_staging
module Tree = Octra_core.Tree

type rpc_result = (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result Lwt.t

type enrollment_snapshot = {
  head_epoch : int;
  head_proposal_id : string option;
  state_root : string;
  chain_id : string;
  config_hash : string;
  entry : Octra_core.Validator_entry.entry option;
  duty : Octra_core.Set_fold.receipt option;
  sets : string option * string option;
}

type read_ctx = {
  data_dir : string;
  ledger : Ledger.t;
  store : Store_irmin.t;
  chaindata : Store_chaindata.t;
  tree_ref : Tree.t ref;
  validator_address : string;
  validator_pubkey : string;
  validator_priv_b64 : string;
  chain_id : string;
  program_trust_hash : string option;
  runtime_profile_hash : string option;
  validator_view_pub : string;
  validator_set_ref : Octra_consensus.C_types.validator_set ref;
  scheduled_validator_set_ref : Octra_consensus.C_config.scheduled option ref;
  current_epoch : int ref;
  total_tx_count : int ref;
  encrypted : unit -> Z.t;
  swarm : unit -> Octra_net.P2p_swarm.t option;
  driver_ref : Octra_consensus.C_driver.t option ref;
  validator_enrollment : unit -> (enrollment_snapshot, string) result;
}

type 'handler dispatch_adapters = {
  status_read : (Yojson.Safe.t -> read_ctx -> rpc_result) -> 'handler;
}

let ok value =
  Lwt.return (Ok value)

let signer ~chain_id ~validator_addr ~validator_pubkey ~validator_priv_b64
    ~config_hash =
  Light_rpc.{
    chain_id;
    validator_addr;
    validator_pubkey;
    validator_priv_b64;
    config_hash;
  }

let node_version () =
  ok (Status_rpc.node_version ())

let release_hash name length =
  match Sys.getenv_opt name with
  | None -> None
  | Some raw ->
    let value = String.trim raw in
    if String.length value = length
       && String.for_all
            (function
              | '0' .. '9' | 'a' .. 'f' -> true
              | _ -> false)
            value
    then Some value
    else None

let validator_view_pubkey ~validator_view_pub ~validator_address =
  ok (Status_rpc.validator_view_pubkey ~validator_view_pub ~validator_address)

let epoch_tags store =
  let open Lwt.Syntax in
  let* count, min_epoch, max_epoch = Store_irmin.epoch_tag_stats store in
  let* split_epoch, gc_enabled, gc_running, gc_need =
    Store_irmin.pack_gc_status store
  in
  ok
    (Status_rpc.epoch_tags
       ~count
       ~min_epoch
       ~max_epoch
       ~keep_epochs:Store_irmin.gc_keep_epochs
       ~split_epoch
       ~gc_enabled
       ~gc_running
       ~gc_need)

let signed_root signer head =
  match head with
  | None ->
    Lwt.return (Error (Octra_core.Rpc.not_found "HEAD not available"))
  | Some h ->
    match Light_rpc.signed_root signer h with
    | Error e ->
      Lwt.return (Error (Octra_core.Rpc.err (-32000) e None))
    | Ok root ->
      ok root

let current_validator_config ~chain_id ~program_trust_hash
    ~runtime_profile_hash ~validator_set_ref ~scheduled_validator_set_ref =
  let validator_set = !validator_set_ref in
  let scheduled = !scheduled_validator_set_ref in
  let config_hash =
    Octra_consensus.C_config.hash
      ~chain_id
      ~validator_set
      ?scheduled
      ?program_trust_hash
      ?runtime_profile_hash
      ()
  in
  validator_set, scheduled, config_hash

let validator_set_proof ~chain_id ~program_trust_hash
    ~runtime_profile_hash ~validator_set_ref ~scheduled_validator_set_ref =
  let validator_set, scheduled, config_hash =
    current_validator_config
      ~chain_id
      ~program_trust_hash
      ~runtime_profile_hash
      ~validator_set_ref
      ~scheduled_validator_set_ref
  in
  Lwt.return
    (Status_rpc.validator_set_proof
       ~chain_id
       ~config_hash
       ?program_trust_hash
       ?runtime_profile_hash
       ?scheduled
       validator_set)

let head_proposal_id ~source ~chain_id ~(head : Octra_core.Head_manifest.t) =
  if head.epoch_id < 0 || head.epoch_id = max_int then
    Error "committed head epoch is invalid"
  else
    Result.bind source (fun source ->
      Result.bind
        (Consensus_parent_commit.load_head source ~epoch_id:(Int64.of_int head.epoch_id))
        (function
        | None -> Error "committed head certificate is unavailable"
        | Some parent ->
          let certificate = parent.Octra_consensus.C_types.certificate in
          if certificate.epoch_id <> Int64.of_int head.epoch_id
             || certificate.chain_id <> chain_id
             || not (Text.is_hex_len 64 head.state_root || Text.is_hex_len 128 head.state_root)
             || Consensus_epoch_apply_guard.raw32_of_pre_root head.state_root
                <> certificate.header.proposed_state_root
             || String.length certificate.proposal_id <> 32 then
            Error "committed head certificate does not match head"
          else Ok (Text.raw_to_hex certificate.proposal_id)))

let duty_state ~store ~(head : Octra_core.Head_manifest.t) =
  let open Lwt.Syntax in
  match head.ledger_state_root with
  | None -> Lwt.return_error "committed ledger root unavailable"
  | Some state_root ->
    let* snapshot = Store_irmin.capture_read_snapshot_at store
      ~epoch_id:(Int64.of_int head.epoch_id) ~state_root in
    match snapshot with
    | Error error -> Lwt.return_error ("committed duty snapshot unavailable: " ^ error)
    | Ok snapshot ->
      let* raw = Store_irmin.read_snapshot snapshot ["meta"; Octra_core.Set_fold.meta_key] in
      Lwt.return (match raw with
        | None -> Ok Octra_core.Set_fold.empty
        | Some raw -> Octra_core.Set_fold.of_string raw
          |> Result.map_error (fun error -> "committed duty state invalid: " ^ error))

let load_validator_enrollment ~store ~head ~validator_address ~chain_id ~config_hash ~automatic =
  let open Lwt.Syntax in
  match head with
  | None -> Lwt.return_error "committed head unavailable"
  | Some head ->
    begin
      match head.Octra_core.Head_manifest.ledger_state_root with
      | None -> Lwt.return_error "committed ledger root unavailable"
      | Some state_root ->
        let* snapshot =
          Store_irmin.capture_read_snapshot_at
            store
            ~epoch_id:(Int64.of_int head.epoch_id)
            ~state_root
        in
        begin
          match snapshot with
          | Error error ->
            Lwt.return_error
              ("committed validator state unavailable: " ^ error)
          | Ok snapshot ->
            let* raw =
              Store_irmin.read_snapshot
                snapshot
                ["meta"; Octra_core.Validator_registry.meta_key]
            in
            let registry =
              match raw with
              | None -> Ok Octra_core.Validator_registry.empty
              | Some raw -> Octra_core.Validator_registry.of_string raw
            in
            begin
              match registry with
              | Error error ->
                Lwt.return_error
                  ("committed validator registry invalid: " ^ error)
              | Ok registry ->
                let* duty =
                  if not automatic then Lwt.return_ok None
                  else
                    let* raw = Store_irmin.read_snapshot snapshot
                      ["meta"; Octra_core.Set_fold.meta_key] in
                    match raw with
                    | None -> Lwt.return_error "committed validator duty is unavailable"
                    | Some raw ->
                      Octra_core.Set_fold.of_string raw
                      |> Result.map (fun state -> Some
                        (Octra_core.Set_fold.receipt ~address:validator_address state))
                      |> Lwt.return
                in
                let* active = Store_irmin.read_snapshot snapshot
                  ["meta"; Octra_core.Validator_set_update.active_meta_key] in
                let* pending = Store_irmin.read_snapshot snapshot
                  ["meta"; Octra_core.Validator_set_update.pending_meta_key] in
                Lwt.return (Result.map (fun duty -> {
                  head_epoch = head.epoch_id;
                  head_proposal_id = None;
                  state_root = head.state_root;
                  chain_id;
                  config_hash;
                  entry =
                    Octra_core.Validator_registry.find
                      validator_address
                      registry;
                  duty;
                  sets = active, pending;
                }) duty)
            end
        end
    end

let enrollment_membership = function
  | None -> `Null
  | Some (member : Enroll_members.t) ->
    `Assoc [
      "epoch", `String (Int64.to_string member.epoch);
      "active", `Bool member.active;
      "scheduled", `Bool member.scheduled;
      "activate_epoch", Status_rpc.enrollment_epoch member.activate_epoch;
      "next_set_epoch", Status_rpc.enrollment_epoch member.next_set_epoch;
      "validator_set_hash", `String (Text.raw_to_hex member.set_hash);
    ]

let validator_enrollment ~snapshot ~validator_address ~validator_pubkey =
  match snapshot with
  | Error error ->
    Lwt.return
      (Error
         (Octra_core.Rpc.err
            (-32000)
            error
            None))
  | Ok snapshot ->
    let duty = match snapshot.duty with
      | None -> `Null
      | Some receipt ->
        let cfg = Octra_core.Set_fold.standard in
        `Assoc [
          "automatic", `Bool true;
          "head_epoch", `String (string_of_int snapshot.head_epoch);
          "last_pulse", (match receipt.pulse with
            | None -> `Null
            | Some epoch -> `String (Int64.to_string epoch));
          "required_span", `String (Int64.to_string cfg.rejoin_span);
          "max_gap", `String (Int64.to_string cfg.pulse_gap);
        ]
    in
    let proposal = match snapshot.head_proposal_id with
      | Some proposal -> ["head_proposal_id", `String proposal]
      | None when snapshot.head_epoch < max_int
                  && Octra_core.Rule_graph.ready_exec_at
                    ~chain_id:snapshot.chain_id ~epoch:(snapshot.head_epoch + 1)
                    = Octra_core.Rule_graph.Active ->
        ["head_proposal_id", `Null]
      | None -> []
    in
    let ready = `Assoc ([
      "consensus_pubkey", `String validator_pubkey;
      "head_epoch", `String (string_of_int snapshot.head_epoch);
      "state_root", `String snapshot.state_root;
      "chain_id", `String snapshot.chain_id;
      "config_hash", `String snapshot.config_hash;
      "catchup_head_epoch", `String (string_of_int snapshot.head_epoch);
    ] @ proposal) in
    Lwt.return
      (Result.bind (Status_rpc.validator_enrollment
         ~chain_id:snapshot.chain_id
         ~head_epoch:snapshot.head_epoch
         ~address:validator_address
         ~pubkey:validator_pubkey
         snapshot.entry)
         (function
           | `Assoc fields ->
             Enroll_members.of_values ~head_epoch:(Int64.of_int snapshot.head_epoch)
               ~address:validator_address ~pubkey:validator_pubkey snapshot.sets
             |> Result.map_error (fun reason -> Octra_core.Rpc.err (-32000) reason None)
             |> Result.map (fun membership -> `Assoc (fields @ [
               "state_root", `String snapshot.state_root;
               "chain_id", `String snapshot.chain_id;
               "membership", enrollment_membership membership;
               "ready", ready;
               "duty", duty;
             ]))
           | _ -> Error (Octra_core.Rpc.err (-32000) "invalid enrollment result" None)))

let runtime_version ~chain_id ~epoch ~validator_address ~program_trust_hash
    ~runtime_profile_hash ~validator_set_ref ~scheduled_validator_set_ref =
  let _, _, config_hash =
    current_validator_config
      ~chain_id
      ~program_trust_hash
      ~runtime_profile_hash
      ~validator_set_ref
      ~scheduled_validator_set_ref
  in
  let json = Status_rpc.runtime_version
       ~source_commit:(release_hash "OCTRA_SOURCE_COMMIT" 40)
       ~binary_hash:(release_hash "OCTRA_BINARY_HASH" 64)
       ~consensus_standard:Consensus_profile.standard
       ~consensus_standard_hash:
         (Consensus_profile.standard_hash ~chain_id ~epoch Sys.getenv_opt)
       ~activation_graph_hash:
         (Consensus_profile.activation_graph_hash ~chain_id)
       ~compat_wire_profile:Consensus_profile.compat_wire_profile
       ~compat_wire_rules_id:Consensus_profile.compat_wire_rules_id
       ~runtime_profile_hash
       ~config_hash
       ~chain_id
       ~validator:validator_address in
  let profiles = 0 :: Consensus_profile.epochs chain_id
    |> List.sort_uniq Int.compare
    |> List.map (fun start -> `Assoc [
      "from_epoch", `Int start;
      "hash", `String (Consensus_profile.hash ~chain_id ~epoch:start Sys.getenv_opt
        |> Text.raw_to_hex);
    ]) in
  match json with
  | `Assoc fields -> ok (`Assoc (fields @ [
      "profile_epoch", `Int epoch;
      "runtime_profiles", `List profiles;
    ]))
  | _ -> assert false

let consensus_peer_states ~swarm ~driver_ref =
  let now = Unix.gettimeofday () in
  let diag =
    match swarm with
    | None ->
      None
    | Some swarm ->
      Some (Octra_net.P2p_swarm.peer_diagnostics swarm)
  in
  let peer_records, voting, voting_reason, round_state, round_peers,
      round_votes, round_agreed =
    match !driver_ref with
    | None ->
      None, false, Some "driver_unavailable", None, [], None, false
    | Some driver ->
      let voting, voting_reason = Octra_consensus.C_driver.vote_state driver in
      let round_peers =
        Octra_consensus.C_driver.round_peer_snapshot driver
        |> List.sort (fun left right ->
          String.compare left.Octra_consensus.C_driver.validator_addr
            right.Octra_consensus.C_driver.validator_addr)
        |> List.map (Rpc_view.consensus_round_peer ~now)
      in
      Some (Octra_consensus.C_driver.peer_state_snapshot driver),
      voting,
      voting_reason,
      Some (Octra_consensus.C_driver.round_state driver),
      round_peers,
      Some (Octra_consensus.C_driver.round_vote_snapshot driver),
      Octra_consensus.C_driver.round_agreed driver ~now
  in
  ok
    (Status_rpc.consensus_peer_states
       ~now ~diag ~peer_records ~voting ~voting_reason ~round_state ~round_peers
       ~round_votes ~round_agreed)

let round_witness params ~driver_ref =
  match Octra_core.Rpc.require_address params 0 "validator" with
  | Error error -> Lwt.return (Error error)
  | Ok validator ->
    begin
      match !driver_ref with
      | None ->
        Lwt.return (Error (Octra_core.Rpc.not_found "round witness unavailable"))
      | Some driver ->
        begin
          match Octra_consensus.C_driver.round_witness driver ~validator with
          | None ->
            Lwt.return (Error (Octra_core.Rpc.not_found "round witness unavailable"))
          | Some witness ->
            ok
              (`Assoc [
                "validator", `String validator;
                "seen_at", `Float witness.seen_at;
                "round_sync", `String (Base64.encode_exn witness.wire);
              ])
        end
    end

let node_status ~tree_ref ~validator =
  let t = !tree_ref in
  ok
    (Status_rpc.node_status
       ~epoch:t.Tree.epoch_id
       ~validator
       ~roots:(Tree.root_count t)
       ~timestamp:(Unix.gettimeofday ())
       ~head:(Octra_core.Head_manifest.get_cached ()))

let sync_result chain_id head stamp status target =
  ok (`Assoc [
    "chainId", `String chain_id;
    "status", `String status;
    "localEpoch", (match head with None -> `Null
      | Some (head : Octra_core.Head_manifest.t) -> `String (string_of_int head.epoch_id));
    "headRoot", (match head with None -> `Null | Some head -> `String head.state_root);
    "setStamp", (match stamp with None -> `Null | Some value -> `String value);
    "targetEpoch", (match target with None -> `Null | Some value -> `String (Int64.to_string value));
  ])

let sync_head_data ~chain_id ~data_dir ~base_dir ~hint () =
  let module Index = Octra_core.Chaindata_index in
  let module Epoch = Octra_core.Epochlog in
  let ( let* ) = Result.bind in
  let* head = match Octra_core.Head_manifest.load_result data_dir with
    | Present head when head.epoch_id >= 0 && head.epoch_id < max_int
        && (Text.is_hex_len 64 head.state_root || Text.is_hex_len 128 head.state_root) -> Ok head
    | _ -> Error "sync head is unavailable" in
  let* offset = match hint with
    | Some (epoch, offset) when epoch = head.epoch_id -> Ok offset
    | None | Some _ -> Error "sync head location is unavailable" in
  let* _ = Octra_consensus.C_protocol.activation_epoch_of Sys.getenv_opt in
  let index = Index.open_index ~readonly:true ~reader_lock:true (Filename.concat base_dir "index") in
  let* raw, floor = match Fun.protect ~finally:(fun () -> Index.close index)
    (fun () -> Index.read_head index head.epoch_id) with
    | None -> Error "sync head index read aborted" | Some value -> Ok value in
  let* raw = raw in
  let* indexed = match Option.bind raw Epoch.epoch_of_json with
    | None -> Error "sync head index is invalid" | Some value -> Ok value in
  let* header, stop = Epoch.read_location ~path:(Filename.concat base_dir "epochlog/epochs.dat")
    ~epoch_id:head.epoch_id ~offset in
  let* () = if indexed <> header then Error "sync head index differs from journal"
    else if Option.fold ~none:false ~some:((<>) stop) head.epochlog_off then
      Error "sync head journal offset differs" else Ok () in
  let read_floor () = let* floor = floor in match floor with
    | None -> Ok None
    | Some raw -> Result.map Option.some (Octra_core.History_floor.of_string raw) in
  let* validators = Consensus_parent_commit.set_at_head_read ~chain_id ~data_dir ~read_floor
    ~epoch_id:(Int64.of_int head.epoch_id) head in
  if header.id <> head.epoch_id || header.state_root <> head.state_root then
    Error "sync head time differs" else
  Ok (head, validators, header.finalized_at)

let sync_head_packet ~chain_id ~data_dir ~base_dir ~hint ~keep () =
  Fun.protect ~finally:(fun () -> ignore (Sys.opaque_identity keep)) (fun () ->
    let packet = match sync_head_data ~chain_id ~data_dir ~base_dir ~hint () with
      | Error _ -> `Null
      | Ok (head, validators, time) -> `Assoc [
          "head", `String (Octra_core.Head_manifest.to_json head);
          "validators", `String (Base64.encode_exn
            (Octra_consensus.C_codec.encode_validator_set validators));
          "time", `Float time;
        ] in
    Yojson.Safe.to_string packet)

let sync_head_read ~chain_id ~data_dir ~chaindata ~epoch_id () =
  let open Lwt.Syntax in
  let base_dir = Filename.dirname chaindata.Store_chaindata.txlog.dir in
  let hint = if epoch_id < 0L || epoch_id >= Int64.of_int max_int then None else
    let epoch = Int64.to_int epoch_id in
    Option.map (fun offset -> epoch, offset) (Hashtbl.find_opt chaindata.epochlog.offsets epoch) in
  let+ raw = Sync_probe.read_child (sync_head_packet ~chain_id ~data_dir ~base_dir ~hint ~keep:chaindata) in
  match Yojson.Safe.from_string raw with
  | `Assoc ["head", `String head; "validators", `String validators; "time", time] ->
    let head = Octra_core.Head_manifest.of_json head in
    let validators = Base64.decode_exn validators |> Octra_consensus.C_codec.decode_validator_set in
    let time = Yojson.Safe.Util.to_number time in
    if Float.is_finite time && time >= 0. then Ok (head, validators, time)
    else Error "sync head time is invalid"
  | _ -> Error "sync head is unavailable"

let sync_driver_current driver_ref driver head generation height =
  match !driver_ref with
  | Some current when current == driver ->
    driver.Octra_consensus.C_driver.running
    && driver.config.local_head_epoch () = Int64.of_int head.Octra_core.Head_manifest.epoch_id
    && driver.engine.generation = generation && driver.engine.state.height = height
  | None | Some _ -> false

type sync_key = {
  driver : Octra_consensus.C_driver.t;
  head : Octra_core.Head_manifest.t;
  generation : int;
  height : int64;
  validators : Octra_consensus.C_types.validator_set;
  stamp : string;
  file : int * int * int * float * float;
}

type sync_sample = {
  value : (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result;
  key : sync_key option;
}

type sync_target = {
  chain : string;
  current : sync_sample -> bool;
  probe : sync_sample Sync_probe.t;
}

let sync_head_file data_dir =
  let file = Unix.lstat (Octra_core.Head_manifest.path data_dir) in
  if file.Unix.st_kind <> Unix.S_REG then failwith "sync head is not a regular file";
  file.st_dev, file.st_ino, file.st_size, file.st_mtime, file.st_ctime

let sync_key_current ~data_dir ~driver_ref key =
  sync_driver_current driver_ref key.driver key.head key.generation key.height
  && (try sync_head_file data_dir = key.file with _ -> false)
  && match Octra_consensus.C_driver.epoch_root_weights ~validator_set:key.validators
      key.driver ~epoch_id:(Int64.of_int key.head.epoch_id) with
    | Some weights -> weights.stamp = key.stamp
    | None -> false

let probe_sync_target ~chain_id ~data_dir ~chaindata ~driver_ref driver =
  let module D = Octra_consensus.C_driver in
  let module H = Consensus_health_probe in
  let open Lwt.Syntax in
  let read () = sync_head_read ~chain_id ~data_dir ~chaindata
    ~epoch_id:(driver.D.config.local_head_epoch ()) () in
  let deadline = Sync_probe.monotonic () +. 2.5 in
  let reply head stamp status target =
    let+ value = sync_result chain_id head stamp status target in {value; key = None} in
  let file = sync_head_file data_dir in
  let* sample = read () in
  match sample with
  | Error _ -> reply None None "unavailable" None
  | Ok (head, validators, head_time) ->
    let epoch_id = Int64.of_int head.epoch_id in
    let generation = driver.D.engine.generation and height = driver.engine.state.height in
    match D.epoch_root_weights ~validator_set:validators driver ~epoch_id with
    | None -> reply (Some head) None "unavailable" None
    | Some weights when not (sync_driver_current driver_ref driver head generation height) ->
      reply (Some head) (Some weights.stamp) "changed" None
    | Some weights when Sync_probe.monotonic () >= deadline ->
      reply (Some head) (Some weights.stamp) "unavailable" None
    | Some weights ->
      let remaining = deadline -. Sync_probe.monotonic () in
      let* replies = Lwt.catch
        (fun () -> Lwt_unix.with_timeout remaining (fun () ->
          let+ replies = D.query_epoch_root ~fresh:true ~request_next:false
            ~wait_for:D.Collection_window driver ~epoch_id
            ~timeout_seconds:(min 2.0 remaining) in Some replies))
        (function Lwt_unix.Timeout -> Lwt.return_none | error -> Lwt.fail error) in
      match replies with
      | None -> reply (Some head) (Some weights.stamp) "unavailable" None
      | Some replies ->
      let* latest = read () in
      match latest with
      | Ok (current, validators, current_time) when current = head && current_time = head_time
          && Sync_probe.monotonic () < deadline
          && sync_driver_current driver_ref driver head generation height ->
        begin match D.epoch_root_weights ~validator_set:validators driver ~epoch_id with
        | Some current when current.epoch_id = weights.epoch_id && current.stamp = weights.stamp ->
          let ceiling = H.target_ceiling ~head:epoch_id ~head_time:(Some head_time)
            ~now:(Unix.gettimeofday ()) ~allowance:2 in
          let target = Option.bind ceiling (fun ceiling ->
            match H.peer_target ~weights ~ceiling replies with
            | Some target when target >= epoch_id -> Some target
            | None | Some _ -> None) in
          let key = {driver; head; generation; height; validators; stamp = weights.stamp; file} in
          if not (sync_key_current ~data_dir ~driver_ref key) then
            reply (Some head) (Some weights.stamp) "changed" None
          else
            let+ value = sync_result chain_id (Some head) (Some weights.stamp)
              (if Option.is_some target then "agreed" else "no_agreement") target in
            {value; key = Some key}
        | None | Some _ -> reply (Some head) (Some weights.stamp) "changed" None
        end
      | Error _ | Ok _ -> reply (Some head) (Some weights.stamp) "changed" None

let create_sync_target ~chain_id ~data_dir ~chaindata ~driver_ref =
  let valid sample = match sample.key with
    | Some key -> sync_key_current ~data_dir ~driver_ref key
    | None -> false in
  let run () = match !driver_ref with
    | Some driver when driver.Octra_consensus.C_driver.running
        && driver.config.chain_id = chain_id ->
      probe_sync_target ~chain_id ~data_dir ~chaindata ~driver_ref driver
    | None | Some _ ->
      let open Lwt.Syntax in
      let+ value = sync_result chain_id None None "unavailable" None in
      {value; key = None} in
  {chain = chain_id; current = valid; probe = Sync_probe.create ~valid run}

let sync_target target =
  let open Lwt.Syntax in
  let* sample = Sync_probe.query target.probe in
  match sample with
  | Some ({key = Some key; _} as sample) when not (target.current sample) ->
    sync_result target.chain (Some key.head) (Some key.stamp) "changed" None
  | Some sample -> Lwt.return sample.value
  | None -> sync_result target.chain None None "unavailable" None

let close_sync_target target = Sync_probe.close target.probe

let node_stats ~ledger ~chaindata ~current_epoch ~total_confirmed ~encrypted =
  let true_total = Ledger.get_total_supply ledger in
  let total_accounts = Ledger.length ledger in
  let active_accounts = Ledger.active_count ledger in
  let recent_epochs =
    List.init (min 10 current_epoch) (fun i -> current_epoch - 1 - i)
  in
  let recent_tx_count =
    Store_chaindata.recent_tx_count
      chaindata
      ~n_epochs:10
      ~current_epoch
  in
  ok
    (Status_rpc.node_stats
       ~current_epoch
       ~total_accounts
       ~active_accounts
       ~true_total
       ~encrypted
       ~max_supply:(Ledger.get_max_supply ())
       ~total_confirmed
       ~staging:(Staging.staging_size ())
       ~recent_tx_count
       ~latest_epochs:recent_epochs
       ~head:(Octra_core.Head_manifest.get_cached ()))

let signer_of_ctx (ctx : read_ctx) =
  let _, _, config_hash =
    current_validator_config
      ~chain_id:ctx.chain_id
      ~program_trust_hash:ctx.program_trust_hash
      ~runtime_profile_hash:ctx.runtime_profile_hash
      ~validator_set_ref:ctx.validator_set_ref
      ~scheduled_validator_set_ref:ctx.scheduled_validator_set_ref
  in
  signer
    ~chain_id:ctx.chain_id
    ~validator_addr:ctx.validator_address
    ~validator_pubkey:ctx.validator_pubkey
    ~validator_priv_b64:ctx.validator_priv_b64
    ~config_hash

let node_version_params _params _ctx =
  node_version ()

let runtime_version_params _params (ctx : read_ctx) =
  runtime_version
    ~chain_id:ctx.chain_id
    ~epoch:!(ctx.current_epoch)
    ~validator_address:ctx.validator_address
    ~program_trust_hash:ctx.program_trust_hash
    ~runtime_profile_hash:ctx.runtime_profile_hash
    ~validator_set_ref:ctx.validator_set_ref
    ~scheduled_validator_set_ref:ctx.scheduled_validator_set_ref

let validator_view_pubkey_params _params (ctx : read_ctx) =
  validator_view_pubkey
    ~validator_view_pub:ctx.validator_view_pub
    ~validator_address:ctx.validator_address

let epoch_tags_params _params (ctx : read_ctx) =
  epoch_tags ctx.store

let signed_root_params _params (ctx : read_ctx) =
  signed_root
    (signer_of_ctx ctx)
    (Octra_core.Head_manifest.get_cached ())

let account_proof_params params (ctx : read_ctx) =
  Light_rpc.account_proof_params
    (signer_of_ctx ctx)
    ~store:ctx.store
    ~head:(Octra_core.Head_manifest.get_cached ())
    params

let validator_set_proof_params _params (ctx : read_ctx) =
  validator_set_proof
    ~chain_id:ctx.chain_id
    ~program_trust_hash:ctx.program_trust_hash
    ~runtime_profile_hash:ctx.runtime_profile_hash
    ~validator_set_ref:ctx.validator_set_ref
    ~scheduled_validator_set_ref:ctx.scheduled_validator_set_ref

let local_control ~data_dir ~(snapshot : enrollment_snapshot) ~address ~pubkey =
  let state = match snapshot.entry with
    | None -> Ok None
    | Some entry ->
      Octra_core.Validator_control.status
        (Octra_core.Validator_control.create ~data_dir)
        Octra_core.Validator_intent.{
          chain_id = snapshot.chain_id;
          address;
          pubkey;
          bonded_epoch = entry.bonded_epoch;
        }
  in
  match state with
  | Ok id -> `Assoc [
      "exit_intent", `Bool true;
      "exit_requested", `Bool (Option.is_some id);
      "intent_id", (match id with None -> `Null | Some id -> `String id);
    ]
  | Error reason -> `Assoc [
      "exit_intent", `Bool true;
      "error", `String reason;
    ]

let validator_enrollment_params _params (ctx : read_ctx) =
  let open Lwt.Syntax in
  let snapshot = ctx.validator_enrollment () in
  let* response = validator_enrollment
    ~snapshot
    ~validator_address:ctx.validator_address
    ~validator_pubkey:ctx.validator_pubkey in
  match response, snapshot with
  | Ok (`Assoc fields), Ok snapshot ->
    let control = local_control ~data_dir:ctx.data_dir ~snapshot
      ~address:ctx.validator_address ~pubkey:ctx.validator_pubkey in
    ok (`Assoc (fields @ ["local_control", control]))
  | _ -> Lwt.return response

let epoch_proof_params params (ctx : read_ctx) =
  Light_rpc.epoch_proof_params ~chain_id:ctx.chain_id ctx.chaindata params

let tx_inclusion_proof_params params (ctx : read_ctx) =
  Light_rpc.tx_inclusion_proof_params ~chain_id:ctx.chain_id ctx.chaindata params

let consensus_peer_states_params _params (ctx : read_ctx) =
  consensus_peer_states
    ~swarm:(ctx.swarm ())
    ~driver_ref:ctx.driver_ref

let round_witness_params params (ctx : read_ctx) =
  round_witness params ~driver_ref:ctx.driver_ref

let node_status_params _params (ctx : read_ctx) =
  node_status
    ~tree_ref:ctx.tree_ref
    ~validator:ctx.validator_address

let node_stats_params _params (ctx : read_ctx) =
  node_stats
    ~ledger:ctx.ledger
    ~chaindata:ctx.chaindata
    ~current_epoch:!(ctx.current_epoch)
    ~total_confirmed:!(ctx.total_tx_count)
    ~encrypted:(ctx.encrypted ())

let node_metrics_params _params _ctx =
  ok (Metrics.get_metrics ())

let epoch_page_params params (ctx : read_ctx) =
  match Epoch_page.parse params with
  | Error error -> Lwt.return (Error error)
  | Ok request ->
    let head = Option.map
      (fun (head : Octra_core.Head_manifest.t) -> Epoch_page.{
        chain = ctx.chain_id; epoch = head.epoch_id; root = head.state_root;
      }) (Octra_core.Head_manifest.get_cached ())
    in
    let load ~max_bytes epoch =
      Octra_core.Epochlog.read_entry ctx.chaindata.Store_chaindata.epochlog ~max_bytes epoch
      |> Result.map_error (function
        | `Limit -> Octra_core.Rpc.err 107 "epoch record exceeds read limit" None
        | `Invalid -> Octra_core.Rpc.err (-32012) "epoch journal is inconsistent" None
        | `Io -> Octra_core.Rpc.err (-32012) "epoch journal is unavailable" None)
    in
    let open Lwt.Syntax in
    let+ page = Epoch_page.read ~head ~load request in
    Result.map Epoch_page.json page

let core_dispatch adapters =
  [
    "node_version", adapters.status_read node_version_params;
    "octra_runtimeVersion", adapters.status_read runtime_version_params;
    "octra_validatorEnrollment", adapters.status_read validator_enrollment_params;
    "node_status", adapters.status_read node_status_params;
    "node_stats", adapters.status_read node_stats_params;
    "node_metrics", adapters.status_read node_metrics_params;
    "octra_epochPage", adapters.status_read epoch_page_params;
  ]

let proof_dispatch adapters =
  [
    "octra_validatorViewPubkey", adapters.status_read validator_view_pubkey_params;
    "octra_epochTags", adapters.status_read epoch_tags_params;
    "octra_signedRoot", adapters.status_read signed_root_params;
    "octra_accountProof", adapters.status_read account_proof_params;
    "octra_validatorSetProof", adapters.status_read validator_set_proof_params;
    "octra_epochProof", adapters.status_read epoch_proof_params;
    "octra_txInclusionProof", adapters.status_read tx_inclusion_proof_params;
    "octra_consensusPeerStates", adapters.status_read consensus_peer_states_params;
    "octra_roundWitness", adapters.status_read round_witness_params;
  ]

let dispatch adapters =
  core_dispatch adapters @ proof_dispatch adapters