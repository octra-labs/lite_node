(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Log = Octra_log
module C_catchup = Octra_consensus.C_catchup
module C_driver = Octra_consensus.C_driver

type queued = {
  target_epoch : int64;
  reason : string;
}

type finish = {
  tag : string;
  root_verified : bool;
}

let finish_unverified tag =
  { tag; root_verified = false }

let finish_verified tag =
  { tag; root_verified = true }

type queue_event = {
  queued_target_epoch : int64;
  queued_reason : string;
  queued_active : bool;
  queued_label : string;
}

type node_queue_runtime = {
  queue : Consensus_catchup_queue.t;
  catchup_active : bool ref;
  clear_state_attested : unit -> unit;
}

type node_queue = {
  queue_catchup_target : target_epoch:int64 -> reason:string -> unit;
  queue_finalized_gap : target_epoch:int64 -> reason:string -> unit;
}

type deps = {
  catchup_active : unit -> bool;
  set_catchup_active : bool -> unit;
  queue_target : target_epoch:int64 -> reason:string -> string;
  committed_head_epoch : unit -> int;
  start_height : int64 -> unit Lwt.t;
  take_queued_after : head:int64 -> queued option;
  clear_queue : unit -> unit;
  read_local_root : unit -> string Lwt.t;
  set_state_attested : head:int -> root:string -> unit;
  clear_quarantine : string -> unit;
  mark_quarantine : string -> unit;
  observer : bool;
  drain_pending_finalized : unit -> unit Lwt.t;
  wake_ready : unit -> unit Lwt.t;
}

type run_one =
  target_epoch:int64 ->
  reason:string ->
  finish_success:(finish -> unit Lwt.t) ->
  fail_catchup:(string -> unit Lwt.t) ->
  unit Lwt.t

type range_plan = {
  remain : int;
  max_epochs : int;
  timeout_seconds : float;
  log_progress : bool;
}

type query_deps = {
  sleep : float -> unit Lwt.t;
  query_range :
    from_epoch:int64 ->
    max_epochs:int ->
    timeout_seconds:float ->
    validate:(C_driver.catchup_range_response_record -> bool) ->
    C_driver.catchup_range_response_record option Lwt.t;
  http_range :
    from_epoch:int64 ->
    max_epochs:int ->
    C_driver.catchup_range_response_record option Lwt.t;
}

type chunk_query_deps = {
  env_timeout : unit -> string option;
  read_query_root : unit -> string Lwt.t;
  range_query : query_deps;
}

type gate_action =
  | Gate_continue
  | Gate_finish of string
  | Gate_retry
  | Gate_fail of string

type apply_gate =
  | Apply_chunk of C_driver.catchup_range_response_record
  | Apply_finish of string
  | Apply_retry
  | Apply_fail of string

type chunk_query_step =
  | Query_chunk of C_driver.catchup_range_response_record
  | Query_failed of string

type query_progress =
  | Await_range
  | Restart_from_head

type chunk_apply_step =
  | Chunk_continue of C_driver.catchup_range_response_record
  | Chunk_finish of string
  | Chunk_retry
  | Chunk_fail of string

type record_apply_action =
  | Record_apply
  | Record_skip_applied
  | Record_retry_moved_head of string

type validated_record = {
  record : Octra_consensus.C_codec.catchup_epoch_record;
  parsed_txs : Octra_core.Transaction.t list;
  parsed_tx_hashes : string list;
  expected_eic : string;
  expected_txid : int64;
  proposer : Octra_core.Epochlog.proposer_info option;
  reward : Consensus_reward_attribution.t;
  expected_root : string option;
  apply_action : record_apply_action;
}

type cached_apply_head = {
  cached_root : string;
  cached_eic : string option;
}

type apply_point_source = {
  head_epoch : unit -> int;
  read_root : unit -> string Lwt.t;
  cached_head : unit -> cached_apply_head;
  next_txid : unit -> int64;
}

type record_apply_deps = {
  chain_id : string;
  expected_validator_set_hash : int64 -> (string, string) result;
  head_before_record : unit -> int;
  find_finalized : int -> Octra_consensus.C_types.finalize option;
  put_proposer : int -> Octra_core.Epochlog.proposer_info -> unit;
  put_expected_root : int -> string -> unit;
  activate_gap : unit -> unit;
  point_source : apply_point_source;
  write_finality : validated_record -> unit;
  promote_finality : validated_record -> unit;
  apply_record : validated_record -> unit Lwt.t;
  advance_height : int64 -> unit Lwt.t;
}

type chunk_apply_deps = {
  read_local_root : unit -> string Lwt.t;
  base_eic : unit -> string;
  next_txid : unit -> int64;
  current_head : unit -> int64;
  gap_active : unit -> bool;
  record_apply : record_apply_deps;
}

type target_deps = {
  normalize : source:string -> unit;
  head_epoch : unit -> int;
  query : chunk_query_deps;
  apply : chunk_apply_deps;
}

type target_wiring = {
  chain_id : string;
  expected_validator_set_hash : int64 -> (string, string) result;
  normalize : source:string -> unit;
  head_epoch : unit -> int;
  env_timeout : unit -> string option;
  read_query_root : unit -> string Lwt.t;
  range_query : query_deps;
  read_apply_root : unit -> string Lwt.t;
  cached_head : unit -> cached_apply_head;
  next_txid : unit -> int64;
  find_finalized : int -> Octra_consensus.C_types.finalize option;
  put_proposer : int -> Octra_core.Epochlog.proposer_info -> unit;
  put_expected_root : int -> string -> unit;
  activate_gap : unit -> unit;
  write_finality : validated_record -> unit;
  promote_finality : validated_record -> unit;
  apply_record : validated_record -> unit Lwt.t;
  advance_height : int64 -> unit Lwt.t;
  read_local_root : unit -> string Lwt.t;
  base_eic : unit -> string;
  current_head : unit -> int64;
  gap_active : unit -> bool;
}

type node_deps_wiring = {
  catchup_active : bool ref;
  queue : Consensus_catchup_queue.t;
  committed_head_epoch : unit -> int;
  start_height : int64 -> unit Lwt.t;
  read_local_root : unit -> string Lwt.t;
  set_state_attested : head:int -> root:string -> unit;
  clear_quarantine : string -> unit;
  mark_quarantine : string -> unit;
  observer : bool;
  drain_pending_finalized : unit -> unit Lwt.t;
  wake_ready : unit -> unit Lwt.t;
}

type node_target_wiring = {
  chain_id : string;
  expected_validator_set_hash : int64 -> (string, string) result;
  normalize : source:string -> unit;
  head_epoch : unit -> int;
  env_timeout : unit -> string option;
  read_local_root : unit -> string Lwt.t;
  range_query : query_deps;
  cached_head : unit -> cached_apply_head;
  next_txid : unit -> int64;
  finality : Consensus_finality_state.callbacks;
  queue : Consensus_catchup_queue.t;
  write_finality : validated_record -> unit;
  promote_finality : validated_record -> unit;
  apply_record : validated_record -> unit Lwt.t;
  advance_height : int64 -> unit Lwt.t;
  base_eic : unit -> string;
  current_head : unit -> int64;
}

type driver_io = {
  start_height : int64 -> unit Lwt.t;
  advance_height : int64 -> unit Lwt.t;
  wake_ready : unit -> unit Lwt.t;
  range_query : query_deps;
}

type driver_runner_wiring = {
  chain_id : string;
  expected_validator_set_hash : int64 -> (string, string) result;
  catchup_active : bool ref;
  queue : Consensus_catchup_queue.t;
  committed_head_epoch : unit -> int;
  normalize : source:string -> unit;
  env_timeout : unit -> string option;
  read_local_root : unit -> string Lwt.t;
  cached_head : unit -> cached_apply_head;
  next_txid : unit -> int64;
  finality : Consensus_finality_state.callbacks;
  write_finality : validated_record -> unit;
  promote_finality : validated_record -> unit;
  apply_record : validated_record -> unit Lwt.t;
  base_eic : unit -> string;
  current_head : unit -> int64;
  set_state_attested : head:int -> root:string -> unit;
  clear_quarantine : string -> unit;
  mark_quarantine : string -> unit;
  observer : bool;
  drain_pending_finalized : unit -> unit Lwt.t;
  http_range :
    from_epoch:int64 ->
    max_epochs:int ->
    C_driver.catchup_range_response_record option Lwt.t;
}

type driver_runner_node_wiring = {
  chain_id : string;
  expected_validator_set_hash : int64 -> (string, string) result;
  catchup_active : bool ref;
  queue : Consensus_catchup_queue.t;
  committed_head_epoch : unit -> int;
  normalize : source:string -> unit;
  env_timeout : unit -> string option;
  read_local_root : unit -> string Lwt.t;
  cached_root : unit -> Consensus_driver_read.cached_root;
  next_txid : unit -> int64;
  finality : Consensus_finality_state.callbacks;
  write_finality : validated_record -> unit;
  promote_finality : validated_record -> unit;
  apply_record : validated_record -> unit Lwt.t;
  base_eic : unit -> string;
  set_state_attested : head:int -> root:string -> unit;
  clear_quarantine : string -> unit;
  mark_quarantine : string -> unit;
  observer : bool;
  drain_pending_finalized : unit -> unit Lwt.t;
  http_range :
    from_epoch:int64 ->
    max_epochs:int ->
    C_driver.catchup_range_response_record option Lwt.t;
}

let range_cap = 16

let short_hex8 s =
  String.concat ""
    (List.init
       (min 8 (String.length s))
       (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let timeout_of_env ~remain = function
  | Some s ->
    begin
      try max 0.5 (float_of_string s) with _ -> 8.0
    end
  | None ->
    if remain <= 4 then 2.0 else 8.0

let range_plan ~env_timeout ~from_epoch ~target_epoch =
  let remain = Int64.to_int (Int64.sub target_epoch from_epoch) + 1 in
  {
    remain;
    max_epochs = min range_cap remain;
    timeout_seconds = timeout_of_env ~remain env_timeout;
    log_progress = Int64.to_int from_epoch mod 100 = 0;
  }

let query_range deps ~attempts ~retry_delay ~from_epoch ~max_epochs
    ~timeout_seconds ~reason ~validate =
  let open Lwt.Syntax in
  let rec loop attempts =
    let* result =
      deps.query_range ~from_epoch ~max_epochs ~timeout_seconds ~validate
    in
    match result with
    | Some _ ->
      Lwt.return result
    | None when attempts > 1 ->
      Log.warn "catchup"
        "event = query_retry from = %Ld retries_left = %d reason = %s"
        from_epoch (attempts - 1) reason;
      let* () = deps.sleep retry_delay in
      loop (attempts - 1)
    | None ->
      Lwt.return_none
  in
  loop attempts

let parse_record_txs record =
  try
    Ok (
      List.map
        (fun tx_json ->
          match Yojson.Safe.from_string tx_json
                |> Octra_core.Tx_payload.decode_final with
          | Ok tx -> tx
          | Error e -> failwith ("bad tx_json: " ^ e))
        record.Octra_consensus.C_codec.txs_json
    )
  with exn ->
    Error ("tx_json parse failed: " ^ Printexc.to_string exn)

let record_payload_error record =
  match parse_record_txs record with
  | Error error -> Some error
  | Ok parsed_txs ->
    let parsed_tx_hashes =
      List.map Octra_core.Transaction.hash parsed_txs
    in
    begin
      match C_catchup.verify_record_hashes ~record ~parsed_tx_hashes with
      | Error error -> Some error
      | Ok () ->
        match C_catchup.verify_record_receipts ~record with
        | Error error -> Some error
        | Ok () -> None
    end

let response_payload_error records =
  let rec first = function
    | [] -> None
    | record :: rest ->
      match record_payload_error record with
      | Some error ->
        Some
          (Printf.sprintf
             "epoch = %Ld %s"
             record.Octra_consensus.C_codec.epoch_id
             error)
      | None -> first rest
  in
  match records with
  | [] -> Some "empty records"
  | _ -> first records

let response_payload_valid records =
  Option.is_none (response_payload_error records)

let query_chunk (deps : chunk_query_deps) ~target_epoch ~from_epoch ~reason =
  let open Lwt.Syntax in
  let range_plan =
    range_plan
      ~env_timeout:(deps.env_timeout ())
      ~from_epoch
      ~target_epoch
  in
  if range_plan.log_progress then
    Log.info "catchup"
      "event = progress epoch = %d remain = %d target = %Ld reason = %s"
      (Int64.to_int from_epoch) range_plan.remain target_epoch reason;
  let* local_root = deps.read_query_root () in
  let reject (r : C_driver.catchup_range_response_record) reason =
    Log.info "catchup"
      "event = range_validate_failed from = %Ld peer = %s reason = %s"
      from_epoch
      (Text.addr_short r.C_driver.responder_addr)
      reason;
    false
  in
  let validate r =
    if not
        (C_catchup.is_valid_chunk_first_epoch
           ~status:r.C_driver.status
           ~records:r.records
           ~from_epoch)
    then reject r "first epoch or status mismatch"
    else
      match response_payload_error r.records with
      | Some error -> reject r ("payload: " ^ error)
      | None ->
        match C_catchup.verify_chain_continuity
                ~records:r.records
                ~from_epoch
                ~prev_root:local_root with
        | Ok () -> true
        | Error error -> reject r ("continuity: " ^ error)
  in
  let* result =
    query_range
      deps.range_query
      ~attempts:3
      ~retry_delay:2.0
      ~from_epoch
      ~max_epochs:range_plan.max_epochs
      ~timeout_seconds:range_plan.timeout_seconds
      ~reason
      ~validate
  in
  let* result =
    match result with
    | Some _ -> Lwt.return result
    | None ->
      let* response =
        deps.range_query.http_range
          ~from_epoch
          ~max_epochs:range_cap
      in
      begin
        match response with
        | Some response when validate response ->
          Log.warn "catchup"
            "event = range_http_secondary from = %Ld records = %d reason = %s"
            from_epoch
            (List.length response.C_driver.records)
            reason;
          Lwt.return_some response
        | Some response ->
          Log.warn "catchup"
            "event = range_http_rejected from = %Ld records = %d reason = %s"
            from_epoch
            (List.length response.C_driver.records)
            reason;
          Lwt.return_none
        | None -> Lwt.return_none
      end
  in
  match result with
  | Some chunk ->
    Lwt.return (Query_chunk chunk)
  | None ->
    Log.error "catchup"
      "event = query_failed attempts = 3 from = %Ld reason = %s"
      from_epoch reason;
    Lwt.return (Query_failed ("catchup_failed:" ^ reason))

let query_progress ~head ~from_epoch =
  if Int64.compare (Int64.of_int head) from_epoch >= 0 then
    Restart_from_head
  else
    Await_range

let base_gate ~target_epoch ~start_head ~current_head ~from_epoch ~reason
    ~head =
  match C_catchup.verify_apply_base ~from_epoch ~head with
  | Ok () ->
    Gate_continue
  | Error e ->
    match C_catchup.base_gate_recovery
            ~target:target_epoch
            ~start_head
            ~current_head with
    | C_catchup.Base_gate_already_applied ->
      Log.info "catchup"
        "event = base_gate_skip from = %Ld target = %Ld reason = %s error = %s"
        from_epoch target_epoch reason e;
      Gate_finish ("already_advanced:" ^ reason)
    | C_catchup.Base_gate_retry ->
      Log.warn "catchup"
        "event = base_gate_retry from = %Ld current = %Ld target = %Ld reason = %s error = %s"
        start_head current_head target_epoch reason e;
      Gate_retry
    | C_catchup.Base_gate_quarantine ->
      Log.error "catchup"
        "event = base_gate_fail from = %Ld reason = %s error = %s"
        from_epoch reason e;
      Gate_fail ("catchup_base_gate_failed:" ^ reason)

let continuity_gate ~records ~from_epoch ~prev_root ~reason =
  match C_catchup.verify_chain_continuity ~records ~from_epoch ~prev_root with
  | Ok () ->
    Gate_continue
  | Error e ->
    Log.error "catchup"
      "event = continuity_fail from = %Ld reason = %s local_root = %s error = %s"
      from_epoch reason (short_hex8 prev_root) e;
    Gate_fail ("catchup_base_mismatch:" ^ reason)

let final_apply_gate ~last_epoch ~expected_root ~expected_eic ~expected_txid
    ~head chunk =
  match C_catchup.verify_apply_final
          ~last_epoch
          ~expected_root
          ~expected_eic
          ~expected_txid
          ~head with
  | Ok () -> Ok chunk
  | Error e -> Error e

let proposer_of_record record =
  if String.length record.Octra_consensus.C_codec.creator_addr > 3 then
    Some {
      Octra_core.Epochlog.creator_addr = record.creator_addr;
      commit_round = record.commit_round;
    }
  else
    None

let expected_root_of_record record =
  if String.length record.Octra_consensus.C_codec.state_root = 32
     && record.state_root <> String.make 32 '\x00' then
    Some record.state_root
  else
    None

let apply_action_of_record ~head_before_record record =
  let record_epoch = Int64.to_int record.Octra_consensus.C_codec.epoch_id in
  match C_catchup.record_apply_decision ~head:head_before_record ~record_epoch with
  | C_catchup.Apply_record ->
    Record_apply
  | C_catchup.Skip_applied_record ->
    Record_skip_applied
  | C_catchup.Retry_moved_head ->
    Record_retry_moved_head
      (Printf.sprintf
         "catchup local head advanced during apply epoch = %Ld head = %d"
         record.epoch_id
         head_before_record)

let apply_point_of_source (source : apply_point_source) ~root ~eic =
  C_catchup.{
    epoch = Int64.of_int (source.head_epoch ());
    root;
    eic;
    txid = source.next_txid ();
  }

let read_apply_point (source : apply_point_source) =
  let open Lwt.Syntax in
  let* root = source.read_root () in
  let cached = source.cached_head () in
  Lwt.return (apply_point_of_source source ~root ~eic:cached.cached_eic)

let cached_apply_point (source : apply_point_source) =
  let cached = source.cached_head () in
  apply_point_of_source
    source
    ~root:cached.cached_root
    ~eic:cached.cached_eic

let validate_record ~chain_id ~expected_validator_set_hash ~prev_eic
    ~start_txid ~head_before_record record =
  match parse_record_txs record with
  | Error e -> Error e
  | Ok parsed_txs ->
    let parsed_tx_hashes = List.map Octra_core.Transaction.hash parsed_txs in
    match C_catchup.verify_record_hashes ~record ~parsed_tx_hashes with
    | Error e -> Error e
    | Ok () ->
      match C_catchup.verify_record_receipts ~record with
      | Error e -> Error e
      | Ok () ->
        match
          Octra_core.Tx_outcome.decode_final
            ~confirmed:parsed_txs
            record.receipts_json
        with
        | Error e ->
          Error (
            Printf.sprintf
              "catchup outcome mismatch epoch = %Ld reason = %s"
              record.epoch_id
              e
          )
        | Ok partition ->
        match Octra_core.Preverify_receipt_policy.check
                ~epoch_id:(Int64.to_int record.epoch_id)
                ~receipts:partition.preverify
                parsed_txs with
        | Error e ->
          Error (
            Printf.sprintf
              "catchup preverify mismatch epoch = %Ld reason = %s"
              record.epoch_id
              e
          )
        | Ok () ->
          let reward =
            match record.Octra_consensus.C_codec.reward_source with
            | None -> Error "catchup reward source is missing"
            | Some source ->
              begin
                match Consensus_reward_attribution.of_source source with
                | Error error -> Error error
                | Ok reward -> Ok reward
              end
          in
          match reward with
          | Error _ as error -> error
          | Ok reward ->
          let _, expected_eic =
            Octra_core.Epoch_index_commitment.next_root_from_hashes
              ~prev:prev_eic
              ~epoch_id:(Int64.to_int record.epoch_id)
              ~start_txid
              record.tx_hashes
          in
          let expected_txid =
            Int64.add start_txid
              (Int64.of_int (List.length record.tx_hashes))
          in
          begin
            match
              C_catchup.verify_record_finality
                ~chain_id
                ~expected_validator_set_hash
                ~expected_txid
                ~record
            with
            | Error _ as error -> error
            | Ok _ ->
              Ok {
                record;
                parsed_txs;
                parsed_tx_hashes;
                expected_eic;
                expected_txid;
                proposer = proposer_of_record record;
                reward;
                expected_root = expected_root_of_record record;
                apply_action =
                  apply_action_of_record ~head_before_record record;
              }
          end

let bind_finality validated finalized =
  let record = validated.record in
  let header = finalized.Octra_consensus.C_types.header in
  let txid_hi = Int64.pred validated.expected_txid in
  let same_commitment =
    finalized.epoch_id = record.epoch_id
    && header.epoch_id = record.epoch_id
    && header.prev_state_root = record.prev_state_root
    && header.proposed_state_root = record.state_root
    && header.tx_list_hash = record.tx_list_hash
    && header.receipt_root = record.receipt_root
    && header.txid_hi = txid_hi
  in
  if not same_commitment then
    Error "catchup finality commitment mismatch"
  else
    match
      Consensus_epoch_apply_proposer.proposer_from_finalized finalized
    with
    | None ->
      Error "catchup finality proposer is missing"
    | Some proposer ->
      let reward =
        match record.Octra_consensus.C_codec.finality with
        | None -> Error "catchup finality metadata is missing"
        | Some finality ->
          begin
            match
              Consensus_reward_attribution.bind_finality
                ~validator_set:finality.validator_set
                finalized
                validated.reward
            with
            | Ok _ as bound -> bound
            | Error error -> Error ("catchup " ^ error)
          end
      in
      Result.map
        (fun reward ->
          {
            validated with
            record = {
              record with
              epoch_ts = header.ts;
              creator_addr = proposer.creator_addr;
              commit_round = proposer.commit_round;
            };
            proposer = Some proposer;
            reward;
          })
        reward

let complete_finality (deps : record_apply_deps) validated =
  let epoch = Int64.to_int validated.record.epoch_id in
  match validated.record.finality with
  | None ->
    Error "catchup finality metadata is missing"
  | Some finality ->
    begin
      match deps.find_finalized epoch with
      | Some local
        when local.Octra_consensus.C_types.proposal_id
             <> finality.finalize.proposal_id
             || local.header <> finality.finalize.header ->
        Error "catchup finality conflicts with local finalize"
      | Some _
      | None ->
        bind_finality validated finality.finalize
    end

let store_record_metadata (deps : record_apply_deps) validated =
  let record = validated.record in
  Option.iter
    (fun proposer ->
      deps.put_proposer
        (Int64.to_int record.Octra_consensus.C_codec.epoch_id)
        proposer)
    validated.proposer;
  Option.iter
    (fun root ->
      deps.put_expected_root
        (Int64.to_int record.Octra_consensus.C_codec.epoch_id)
        root)
    validated.expected_root

let assert_already_applied ~head_before_record validated point =
  let record = validated.record in
  if point.C_catchup.root <> record.Octra_consensus.C_codec.state_root then
    failwith
      (Printf.sprintf
         "catchup already-applied root mismatch epoch = %Ld"
         record.epoch_id);
  if point.eic <> Some validated.expected_eic then
    failwith
      (Printf.sprintf
         "catchup already-applied EIC mismatch epoch = %Ld"
         record.epoch_id);
  if Int64.compare point.txid validated.expected_txid <> 0 then
    failwith
      (Printf.sprintf
         "catchup already-applied txid mismatch epoch = %Ld"
         record.epoch_id);
  Log.warn "catchup"
    "event = record_skip_applied epoch = %Ld head = %d"
    record.epoch_id head_before_record

let assert_post_apply validated point =
  let record = validated.record in
  if point.C_catchup.root <> record.Octra_consensus.C_codec.state_root then
    failwith
      (Printf.sprintf
         "catchup post-apply root mismatch epoch = %Ld"
         record.epoch_id);
  if point.eic <> Some validated.expected_eic then
    failwith
      (Printf.sprintf
         "catchup post-apply EIC mismatch epoch = %Ld"
         record.epoch_id)

let apply_validated_record (deps : record_apply_deps) ~head_before_record
    validated =
  let open Lwt.Syntax in
  let validated =
    match complete_finality deps validated with
    | Ok completed ->
      completed
    | Error error ->
      failwith error
  in
  store_record_metadata deps validated;
  let advance () =
    deps.advance_height
      (Int64.succ validated.record.Octra_consensus.C_codec.epoch_id)
  in
  match validated.apply_action with
  | Record_retry_moved_head err ->
    deps.activate_gap ();
    failwith err
  | Record_skip_applied ->
    let* point = read_apply_point deps.point_source in
    assert_already_applied ~head_before_record validated point;
    deps.write_finality validated;
    deps.promote_finality validated;
    advance ()
  | Record_apply ->
    deps.write_finality validated;
    let* () = deps.apply_record validated in
    assert_post_apply validated (cached_apply_point deps.point_source);
    deps.promote_finality validated;
    advance ()

let apply_chunk_records (deps : record_apply_deps) ~prev_eic ~start_txid chunk =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      let expected_eic = ref prev_eic in
      let expected_txid = ref start_txid in
      let* () =
        Lwt_list.iter_s
          (fun record ->
            let head_before_record = deps.head_before_record () in
            let validated =
              let expected_validator_set_hash =
                match
                  deps.expected_validator_set_hash
                    record.Octra_consensus.C_codec.epoch_id
                with
                | Ok hash -> hash
                | Error error -> failwith error
              in
              match validate_record
                      ~chain_id:deps.chain_id
                      ~expected_validator_set_hash
                      ~prev_eic:!expected_eic
                      ~start_txid:!expected_txid
                      ~head_before_record
                      record with
              | Ok validated -> validated
              | Error e -> failwith e
            in
            expected_eic := validated.expected_eic;
            expected_txid := validated.expected_txid;
            apply_validated_record deps ~head_before_record validated)
          chunk.C_driver.records
      in
      let last_record =
        match List.rev chunk.records with
        | last :: _ -> last
        | [] -> failwith "catchup empty chunk after validation"
      in
      let* final_point = read_apply_point deps.point_source in
      Lwt.return
        (final_apply_gate
           ~last_epoch:last_record.epoch_id
           ~expected_root:last_record.state_root
           ~expected_eic:!expected_eic
           ~expected_txid:!expected_txid
           ~head:final_point
           chunk))
    (fun exn -> Lwt.return (Error (Printexc.to_string exn)))

let apply_result_gate ~gap_active ~target_epoch ~start_head ~current_head
    ~from_epoch ~reason = function
  | Ok chunk ->
    Apply_chunk chunk
  | Error e ->
    match C_catchup.apply_recovery
            ~gap_active
            ~target:target_epoch
            ~start_head
            ~current_head with
    | C_catchup.Apply_already_applied ->
      Log.info "catchup"
        "event = apply_gap_skip from = %Ld target = %Ld reason = %s error = %s"
        from_epoch target_epoch reason e;
      Apply_finish ("apply_already_advanced:" ^ reason)
    | C_catchup.Apply_retry ->
      Log.warn "catchup"
        "event = apply_gap_retry from = %Ld current = %Ld target = %Ld reason = %s error = %s"
        start_head current_head target_epoch reason e;
      Apply_retry
    | C_catchup.Apply_quarantine ->
      Log.error "catchup"
        "event = apply_fail from = %Ld target = %Ld reason = %s error = %s"
        from_epoch target_epoch reason e;
      Apply_fail ("catchup_apply_failed:" ^ reason)

let base_apply_point (deps : chunk_apply_deps) ~root =
  C_catchup.{
    epoch = deps.current_head ();
    root;
    eic = Some (deps.base_eic ());
    txid = deps.next_txid ();
  }

let apply_chunk_gate (deps : chunk_apply_deps) ~target_epoch ~start_head
    ~from_epoch ~reason chunk =
  let open Lwt.Syntax in
  let* local_root_before_apply = deps.read_local_root () in
  let current_head = deps.current_head () in
  match base_gate
          ~target_epoch
          ~start_head
          ~current_head
          ~from_epoch
          ~reason
          ~head:(base_apply_point deps ~root:local_root_before_apply) with
  | Gate_finish tag ->
    Lwt.return (Chunk_finish tag)
  | Gate_retry ->
    Lwt.return Chunk_retry
  | Gate_fail tag ->
    Lwt.return (Chunk_fail tag)
  | Gate_continue ->
    match continuity_gate
            ~records:chunk.C_driver.records
            ~from_epoch
            ~prev_root:local_root_before_apply
            ~reason with
    | Gate_finish tag ->
      Lwt.return (Chunk_finish tag)
    | Gate_retry ->
      Lwt.return Chunk_retry
    | Gate_fail tag ->
      Lwt.return (Chunk_fail tag)
    | Gate_continue ->
      let* apply_result =
        apply_chunk_records
          deps.record_apply
          ~prev_eic:(deps.base_eic ())
          ~start_txid:(deps.next_txid ())
          chunk
      in
      match apply_result_gate
              ~gap_active:(deps.gap_active ())
              ~target_epoch
              ~start_head
              ~current_head:(deps.current_head ())
              ~from_epoch
              ~reason
              apply_result with
      | Apply_finish tag ->
        Lwt.return (Chunk_finish tag)
      | Apply_retry ->
        Lwt.return Chunk_retry
      | Apply_fail tag ->
        Lwt.return (Chunk_fail tag)
      | Apply_chunk applied_chunk ->
        Lwt.return (Chunk_continue applied_chunk)

let last_applied_epoch chunk =
  match List.rev chunk.C_driver.records with
  | last :: _ ->
    last.Octra_consensus.C_codec.epoch_id
  | [] ->
    failwith "catchup empty chunk after application"

type target_query_step =
  | Range_step of chunk_query_step
  | Local_apply

let rec wait_for_local_apply (deps : target_deps) ~from_epoch =
  let open Lwt.Syntax in
  match query_progress ~head:(deps.head_epoch ()) ~from_epoch with
  | Restart_from_head ->
    Lwt.return_unit
  | Await_range ->
    let* () = deps.query.range_query.sleep 0.1 in
    let* () = Lwt.pause () in
    wait_for_local_apply deps ~from_epoch

let cancel_and_wait ~sleep task =
  let open Lwt.Syntax in
  Lwt.cancel task;
  let settled =
    Lwt.catch
      (fun () ->
        let+ _ = task in
        true)
      (fun _ -> Lwt.return_true)
  in
  let expired =
    let* () = Lwt.pause () in
    let+ () = sleep 0.5 in
    false
  in
  Lwt.pick [settled; expired]

let query_or_local_apply deps ~target_epoch ~from_epoch ~reason =
  let open Lwt.Syntax in
  let range_step =
    let+ step = query_chunk deps.query ~target_epoch ~from_epoch ~reason in
    Range_step step
  in
  let local_step =
    let+ () = wait_for_local_apply deps ~from_epoch in
    Local_apply
  in
  let cancel task =
    cancel_and_wait ~sleep:deps.query.range_query.sleep task
  in
  Lwt.catch
    (fun () ->
      let* step = Lwt.choose [range_step; local_step] in
      let* settled =
        match step with
        | Range_step _ -> cancel local_step
        | Local_apply -> cancel range_step
      in
      if not settled then
        Log.warn "catchup"
          "event = query_cancel_pending from = %Ld reason = %s"
          from_epoch reason;
      Lwt.return step)
    (fun exn ->
      let* _ = cancel range_step in
      let* _ = cancel local_step in
      Lwt.fail exn)

let run_target (deps : target_deps) ~target_epoch ~reason ~finish_success
    ~fail_catchup =
  let open Lwt.Syntax in
  let rec run () =
    deps.normalize ~source:("catchup:" ^ reason);
    let our_head_int = deps.head_epoch () in
    let our_head = Int64.of_int our_head_int in
    let rec loop ~from_epoch ~root_verified =
      let* () = deps.apply.record_apply.advance_height from_epoch in
      if Int64.compare from_epoch target_epoch > 0 then begin
        Log.info "catchup"
          "event = complete epoch = %Ld reason = %s"
          target_epoch
          reason;
        finish_success
          (if root_verified then finish_verified reason
           else finish_unverified reason)
      end else
        let* query_step =
          query_or_local_apply deps ~target_epoch ~from_epoch ~reason
        in
        match query_step with
        | Local_apply ->
          Log.info "catchup"
            "event = query_obsolete from = %Ld head = %d reason = %s"
            from_epoch (deps.head_epoch ()) reason;
          run ()
        | Range_step (Query_failed tag) ->
          fail_catchup tag
        | Range_step (Query_chunk chunk) ->
          let* step =
            apply_chunk_gate
              deps.apply
              ~target_epoch
              ~start_head:our_head
              ~from_epoch
              ~reason
              chunk
          in
          match step with
          | Chunk_finish tag ->
            finish_success (finish_unverified tag)
          | Chunk_retry ->
            run ()
          | Chunk_fail tag ->
            fail_catchup tag
          | Chunk_continue applied_chunk ->
            loop
              ~from_epoch:(Int64.succ (last_applied_epoch applied_chunk))
              ~root_verified:true
    in
    if Int64.compare target_epoch our_head <= 0 then begin
      Log.info "catchup"
        "event = complete status = already_in_sync target = %Ld reason = %s"
        target_epoch
        reason;
      finish_success (finish_unverified ("already_in_sync:" ^ reason))
    end else begin
      Log.warn "catchup"
        "event = start local = %d target = %Ld lag = %d reason = %s"
        our_head_int
        target_epoch
        (Int64.to_int (Int64.sub target_epoch our_head))
        reason;
      loop ~from_epoch:(Int64.succ our_head) ~root_verified:false
    end
  in
  run ()

let queue_active (deps : deps) ~target_epoch ~reason =
  let queued = deps.queue_target ~target_epoch ~reason in
  Log.warn "catchup"
    "event = queue_target target = %Ld reason = %s active = %b queued = %s"
    target_epoch reason true queued;
  Lwt.return_unit

let run (deps : deps) ~run_one ~target_epoch ~reason =
  let open Lwt.Syntax in
  let rec finish_success finish =
    let active_reason = finish.tag in
    let next_height = Int64.succ (Int64.of_int (deps.committed_head_epoch ())) in
    let* () = deps.start_height next_height in
    deps.set_catchup_active false;
    let head = Int64.of_int (deps.committed_head_epoch ()) in
    match deps.take_queued_after ~head with
    | Some queued ->
      deps.set_catchup_active true;
      Log.warn "catchup"
        "event = continue active_reason = %s next_target = %Ld next_reason = %s head = %Ld"
        active_reason queued.target_epoch queued.reason head;
      run_queued queued
    | None ->
      deps.clear_queue ();
      let head = deps.committed_head_epoch () in
      let* () =
        if finish.root_verified then begin
          let* root = deps.read_local_root () in
          deps.set_state_attested ~head ~root;
          deps.clear_quarantine
            ("catchup_complete:root_verified:" ^ active_reason);
          Lwt.return_unit
        end else begin
          Log.warn "catchup"
            "event = complete action = hold_attestation reason = unverified_root tag = %s head = %d"
            active_reason head;
          Lwt.return_unit
        end
      in
      if deps.observer then Lwt.return_unit
      else
        let* () = deps.drain_pending_finalized () in
        deps.wake_ready ()
  and fail_catchup quarantine_tag =
    deps.clear_queue ();
    deps.set_catchup_active false;
    deps.mark_quarantine quarantine_tag;
    Lwt.return_unit
  and run_queued queued =
    Lwt.catch
      (fun () ->
        let* () = deps.drain_pending_finalized () in
        run_one
          ~target_epoch:queued.target_epoch
          ~reason:queued.reason
          ~finish_success
          ~fail_catchup)
      (fun exn ->
        Log.error "catchup"
          "event = actor_failed target = %Ld reason = %s error = %s"
          queued.target_epoch
          queued.reason
          (Printexc.to_string exn);
        fail_catchup "catchup_failed:unexpected")
  in
  if deps.catchup_active () then
    queue_active deps ~target_epoch ~reason
  else begin
    deps.set_catchup_active true;
    run_queued { target_epoch; reason }
  end

let run_with_target deps ~target ~target_epoch ~reason =
  run
    deps
    ~run_one:(fun ~target_epoch ~reason ~finish_success ~fail_catchup ->
      run_target target ~target_epoch ~reason ~finish_success ~fail_catchup)
    ~target_epoch
    ~reason

let target_of_wiring (wiring : target_wiring) =
  let point_source = {
    head_epoch = wiring.head_epoch;
    read_root = wiring.read_apply_root;
    cached_head = wiring.cached_head;
    next_txid = wiring.next_txid;
  } in
  let record_apply = {
    chain_id = wiring.chain_id;
    expected_validator_set_hash = wiring.expected_validator_set_hash;
    head_before_record = wiring.head_epoch;
    find_finalized = wiring.find_finalized;
    put_proposer = wiring.put_proposer;
    put_expected_root = wiring.put_expected_root;
    activate_gap = wiring.activate_gap;
    point_source;
    write_finality = wiring.write_finality;
    promote_finality = wiring.promote_finality;
    apply_record = wiring.apply_record;
    advance_height = wiring.advance_height;
  } in
  {
    normalize = wiring.normalize;
    head_epoch = wiring.head_epoch;
    query = {
      env_timeout = wiring.env_timeout;
      read_query_root = wiring.read_query_root;
      range_query = wiring.range_query;
    };
    apply = {
      read_local_root = wiring.read_local_root;
      base_eic = wiring.base_eic;
      next_txid = wiring.next_txid;
      current_head = wiring.current_head;
      gap_active = wiring.gap_active;
      record_apply;
    };
  }

let node_deps (wiring : node_deps_wiring) =
  {
    catchup_active = (fun () -> !(wiring.catchup_active));
    set_catchup_active = (fun active ->
      wiring.catchup_active := active);
    queue_target = (fun ~target_epoch ~reason ->
      Consensus_catchup_queue.queue wiring.queue ~target_epoch ~reason
      |> Consensus_catchup_queue.target_label);
    committed_head_epoch = wiring.committed_head_epoch;
    start_height = wiring.start_height;
    take_queued_after = (fun ~head ->
      match Consensus_catchup_queue.take_if_after wiring.queue ~head with
      | Some queued ->
        Some {
          target_epoch = queued.target_epoch;
          reason = queued.reason;
        }
      | None ->
        None);
    clear_queue = (fun () ->
      Consensus_catchup_queue.clear_all wiring.queue);
    read_local_root = wiring.read_local_root;
    set_state_attested = wiring.set_state_attested;
    clear_quarantine = wiring.clear_quarantine;
    mark_quarantine = wiring.mark_quarantine;
    observer = wiring.observer;
    drain_pending_finalized = wiring.drain_pending_finalized;
    wake_ready = wiring.wake_ready;
  }

let target_wiring_of_node (wiring : node_target_wiring) =
  {
    chain_id = wiring.chain_id;
    expected_validator_set_hash = wiring.expected_validator_set_hash;
    normalize = wiring.normalize;
    head_epoch = wiring.head_epoch;
    env_timeout = wiring.env_timeout;
    read_query_root = wiring.read_local_root;
    range_query = wiring.range_query;
    read_apply_root = wiring.read_local_root;
    cached_head = wiring.cached_head;
    next_txid = wiring.next_txid;
    find_finalized = wiring.finality.find_finalized;
    put_proposer = wiring.finality.store_proposer;
    put_expected_root = (fun epoch root ->
      wiring.finality.store_expected_root ~epoch ~root);
    activate_gap = (fun () ->
      Consensus_catchup_queue.activate_gap wiring.queue);
    write_finality = wiring.write_finality;
    promote_finality = wiring.promote_finality;
    apply_record = wiring.apply_record;
    advance_height = wiring.advance_height;
    read_local_root = wiring.read_local_root;
    base_eic = wiring.base_eic;
    current_head = wiring.current_head;
    gap_active = (fun () ->
      Consensus_catchup_queue.gap_active wiring.queue);
  }

let driver_io_of_driver driver =
  {
    start_height = C_driver.start_height driver;
    advance_height = C_driver.catchup_height driver;
    wake_ready = (fun () ->
      C_driver.wake_ready driver);
    range_query = {
      sleep = Lwt_unix.sleep;
      query_range = (fun ~from_epoch ~max_epochs ~timeout_seconds ~validate ->
        C_driver.query_catchup_range
          driver
          ~from_epoch
          ~max_epochs
          ~timeout_seconds
          ~validate);
      http_range = (fun ~from_epoch:_ ~max_epochs:_ -> Lwt.return_none);
    };
  }

let cached_apply_head_of_driver_root root =
  {
    cached_root = root.Consensus_driver_read.root;
    cached_eic = root.Consensus_driver_read.eic;
  }

let driver_runner_wiring_of_node wiring =
  {
    chain_id = wiring.chain_id;
    expected_validator_set_hash = wiring.expected_validator_set_hash;
    catchup_active = wiring.catchup_active;
    queue = wiring.queue;
    committed_head_epoch = wiring.committed_head_epoch;
    normalize = wiring.normalize;
    env_timeout = wiring.env_timeout;
    read_local_root = wiring.read_local_root;
    cached_head = (fun () ->
      cached_apply_head_of_driver_root (wiring.cached_root ()));
    next_txid = wiring.next_txid;
    finality = wiring.finality;
    write_finality = wiring.write_finality;
    promote_finality = wiring.promote_finality;
    apply_record = wiring.apply_record;
    base_eic = wiring.base_eic;
    current_head = (fun () ->
      Int64.of_int (wiring.committed_head_epoch ()));
    set_state_attested = wiring.set_state_attested;
    clear_quarantine = wiring.clear_quarantine;
    mark_quarantine = wiring.mark_quarantine;
    observer = wiring.observer;
    drain_pending_finalized = wiring.drain_pending_finalized;
    http_range = wiring.http_range;
  }

let node_deps_of_driver_runner (wiring : driver_runner_wiring) io =
  node_deps
    {
      catchup_active = wiring.catchup_active;
      queue = wiring.queue;
      committed_head_epoch = wiring.committed_head_epoch;
      start_height = io.start_height;
      read_local_root = wiring.read_local_root;
      set_state_attested = wiring.set_state_attested;
      clear_quarantine = wiring.clear_quarantine;
      mark_quarantine = wiring.mark_quarantine;
      observer = wiring.observer;
      drain_pending_finalized = wiring.drain_pending_finalized;
      wake_ready = io.wake_ready;
    }

let target_wiring_of_driver_runner (wiring : driver_runner_wiring) io =
  target_wiring_of_node
    {
      chain_id = wiring.chain_id;
      expected_validator_set_hash = wiring.expected_validator_set_hash;
      normalize = wiring.normalize;
      head_epoch = wiring.committed_head_epoch;
      env_timeout = wiring.env_timeout;
      read_local_root = wiring.read_local_root;
      range_query = { io.range_query with http_range = wiring.http_range };
      cached_head = wiring.cached_head;
      next_txid = wiring.next_txid;
      finality = wiring.finality;
      queue = wiring.queue;
      write_finality = wiring.write_finality;
      promote_finality = wiring.promote_finality;
      apply_record = wiring.apply_record;
      advance_height = io.advance_height;
      base_eic = wiring.base_eic;
      current_head = wiring.current_head;
    }

let run_driver_wired (wiring : driver_runner_wiring) io ~target_epoch ~reason =
  run_with_target
    (node_deps_of_driver_runner wiring io)
    ~target:(target_wiring_of_driver_runner wiring io |> target_of_wiring)
    ~target_epoch
    ~reason

let node_driver_runner wiring driver ~target_epoch ~reason =
  run_driver_wired
    (driver_runner_wiring_of_node wiring)
    (driver_io_of_driver driver)
    ~target_epoch
    ~reason

let queue_event_of_snapshot ~active ~target_epoch ~reason snapshot =
  {
    queued_target_epoch = target_epoch;
    queued_reason = reason;
    queued_active = active;
    queued_label = Consensus_catchup_queue.target_label snapshot;
  }

let queue_target_event queue ~active ~target_epoch ~reason =
  Consensus_catchup_queue.queue queue ~target_epoch ~reason
  |> queue_event_of_snapshot ~active ~target_epoch ~reason

let queue_gap_event queue ~active ~target_epoch ~reason =
  Consensus_catchup_queue.queue_gap queue ~target_epoch ~reason
  |> queue_event_of_snapshot ~active ~target_epoch ~reason

let log_queue_event event =
  Log.warn "catchup"
    "event = queue_target target = %Ld reason = %s active = %b queued = %s"
    event.queued_target_epoch
    event.queued_reason
    event.queued_active
    event.queued_label

let queue_target_and_log queue ~active ~target_epoch ~reason =
  queue_target_event queue ~active:(active ()) ~target_epoch ~reason
  |> log_queue_event

let queue_gap_and_log queue ~active ~clear_state_attested ~target_epoch
    ~reason =
  let event = queue_gap_event queue ~active:(active ()) ~target_epoch ~reason in
  clear_state_attested ();
  log_queue_event event

let node_queue (runtime : node_queue_runtime) =
  {
    queue_catchup_target =
      (fun ~target_epoch ~reason ->
        queue_target_and_log
          runtime.queue
          ~active:(fun () -> !(runtime.catchup_active))
          ~target_epoch
          ~reason);
    queue_finalized_gap =
      (fun ~target_epoch ~reason ->
        queue_gap_and_log
          runtime.queue
          ~active:(fun () -> !(runtime.catchup_active))
          ~clear_state_attested:runtime.clear_state_attested
          ~target_epoch
          ~reason);
  }

let run_wired deps ~target ~target_epoch ~reason =
  run_with_target
    deps
    ~target:(target_of_wiring target)
    ~target_epoch
    ~reason