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


module Log = Octra_log
module C_catchup = Octra_consensus.C_catchup
module C_driver = Octra_consensus.C_driver

type queued = {
  target_epoch : int64;
  reason : string;
}

type queue_event = {
  queued_target_epoch : int64;
  queued_reason : string;
  queued_active : bool;
  queued_label : string;
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
  finish_success:(string -> unit Lwt.t) ->
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
  head_before_record : unit -> int;
  put_proposer : int -> Octra_core.Epochlog.proposer_info -> unit;
  put_expected_root : int -> string -> unit;
  activate_gap : unit -> unit;
  point_source : apply_point_source;
  write_finality : Octra_consensus.C_codec.catchup_epoch_record -> unit;
  apply_record : validated_record -> unit Lwt.t;
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
  normalize : source:string -> unit;
  head_epoch : unit -> int;
  env_timeout : unit -> string option;
  read_query_root : unit -> string Lwt.t;
  range_query : query_deps;
  read_apply_root : unit -> string Lwt.t;
  cached_head : unit -> cached_apply_head;
  next_txid : unit -> int64;
  put_proposer : int -> Octra_core.Epochlog.proposer_info -> unit;
  put_expected_root : int -> string -> unit;
  activate_gap : unit -> unit;
  write_finality : Octra_consensus.C_codec.catchup_epoch_record -> unit;
  apply_record : validated_record -> unit Lwt.t;
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
  normalize : source:string -> unit;
  head_epoch : unit -> int;
  env_timeout : unit -> string option;
  read_local_root : unit -> string Lwt.t;
  range_query : query_deps;
  cached_head : unit -> cached_apply_head;
  next_txid : unit -> int64;
  finality : Consensus_finality_state.callbacks;
  queue : Consensus_catchup_queue.t;
  write_finality : Octra_consensus.C_codec.catchup_epoch_record -> unit;
  apply_record : validated_record -> unit Lwt.t;
  base_eic : unit -> string;
  current_head : unit -> int64;
}

type driver_io = {
  start_height : int64 -> unit Lwt.t;
  wake_ready : unit -> unit Lwt.t;
  range_query : query_deps;
}

type driver_runner_wiring = {
  catchup_active : bool ref;
  queue : Consensus_catchup_queue.t;
  committed_head_epoch : unit -> int;
  normalize : source:string -> unit;
  env_timeout : unit -> string option;
  read_local_root : unit -> string Lwt.t;
  cached_head : unit -> cached_apply_head;
  next_txid : unit -> int64;
  finality : Consensus_finality_state.callbacks;
  write_finality : Octra_consensus.C_codec.catchup_epoch_record -> unit;
  apply_record : validated_record -> unit Lwt.t;
  base_eic : unit -> string;
  current_head : unit -> int64;
  set_state_attested : head:int -> root:string -> unit;
  clear_quarantine : string -> unit;
  mark_quarantine : string -> unit;
  observer : bool;
  drain_pending_finalized : unit -> unit Lwt.t;
}

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
    max_epochs = min 16 remain;
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
        "query_catchup_range failed from = %Ld retries_left = %d reason = %s"
        from_epoch (attempts - 1) reason;
      let* () = deps.sleep retry_delay in
      loop (attempts - 1)
    | None ->
      Lwt.return_none
  in
  loop attempts

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
      "CATCHUP_PROGRESS at epoch = %d remain = %d target = %Ld reason = %s"
      (Int64.to_int from_epoch) range_plan.remain target_epoch reason;
  let* local_root = deps.read_query_root () in
  let validate r =
    C_catchup.is_valid_chunk_first_epoch
      ~status:r.C_driver.status
      ~records:r.records
      ~from_epoch
    &&
    match C_catchup.verify_chain_continuity
            ~records:r.records
            ~from_epoch
            ~prev_root:local_root with
    | Ok () -> true
    | Error _ -> false
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
  match result with
  | Some chunk ->
    Lwt.return (Query_chunk chunk)
  | None ->
    Log.error "catchup"
      "no valid response after 3 attempts for from = %Ld reason = %s"
      from_epoch reason;
    Lwt.return (Query_failed ("catchup_failed:" ^ reason))

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
        "chunk apply base gate skipped after local progress from = %Ld target = %Ld reason = %s err = %s"
        from_epoch target_epoch reason e;
      Gate_finish ("already_advanced:" ^ reason)
    | C_catchup.Base_gate_retry ->
      Log.warn "catchup"
        "chunk apply base gate moved head from = %Ld current = %Ld target = %Ld reason = %s err = %s"
        start_head current_head target_epoch reason e;
      Gate_retry
    | C_catchup.Base_gate_quarantine ->
      Log.error "catchup"
        "chunk apply base gate failed from = %Ld reason = %s err = %s"
        from_epoch reason e;
      Gate_fail ("catchup_base_gate_failed:" ^ reason)

let continuity_gate ~records ~from_epoch ~prev_root ~reason =
  match C_catchup.verify_chain_continuity ~records ~from_epoch ~prev_root with
  | Ok () ->
    Gate_continue
  | Error e ->
    Log.error "catchup"
      "chunk continuity/base mismatch before apply from = %Ld reason = %s local_root = %s err = %s"
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

let parse_record_txs record =
  try
    Ok (
      List.map
        (fun tx_json ->
          match Yojson.Safe.from_string tx_json
                |> Octra_core.Transaction.of_yojson with
          | Ok tx -> tx
          | Error e -> failwith ("bad tx_json: " ^ e))
        record.Octra_consensus.C_codec.txs_json
    )
  with exn ->
    Error ("tx_json parse failed: " ^ Printexc.to_string exn)

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

let validate_record ~prev_eic ~start_txid ~head_before_record record =
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
        match Octra_core.Preverify_receipt_policy.check
                ~epoch_id:(Int64.to_int record.epoch_id)
                ~receipts:record.receipts_json
                parsed_txs with
        | Error e ->
          Error (
            Printf.sprintf
              "catchup preverify mismatch epoch = %Ld reason = %s"
              record.epoch_id
              e
          )
        | Ok () ->
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
          Ok {
            record;
            parsed_txs;
            parsed_tx_hashes;
            expected_eic;
            expected_txid;
            proposer = proposer_of_record record;
            expected_root = expected_root_of_record record;
            apply_action = apply_action_of_record ~head_before_record record;
          }

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
    "record already applied during catchup epoch = %Ld head = %d"
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
  let record = validated.record in
  store_record_metadata deps validated;
  match validated.apply_action with
  | Record_retry_moved_head err ->
    deps.activate_gap ();
    failwith err
  | Record_skip_applied ->
    let* point = read_apply_point deps.point_source in
    assert_already_applied ~head_before_record validated point;
    Lwt.return_unit
  | Record_apply ->
    if validated.proposer = None then
      Log.warn "catchup"
        "record epoch = %Ld missing proposer metadata; apply will rely on finalized_header/disk fallback"
        record.epoch_id;
    deps.write_finality record;
    let* () = deps.apply_record validated in
    assert_post_apply validated (cached_apply_point deps.point_source);
    Lwt.return_unit

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
              match validate_record
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
        "apply gap skipped after local progress from = %Ld target = %Ld reason = %s err = %s"
        from_epoch target_epoch reason e;
      Apply_finish ("apply_already_advanced:" ^ reason)
    | C_catchup.Apply_retry ->
      Log.warn "catchup"
        "apply gap moved head from = %Ld current = %Ld target = %Ld reason = %s err = %s"
        start_head current_head target_epoch reason e;
      Apply_retry
    | C_catchup.Apply_quarantine ->
      Log.error "catchup"
        "apply failed during catchup from = %Ld target = %Ld reason = %s err = %s"
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

let run_target (deps : target_deps) ~target_epoch ~reason ~finish_success
    ~fail_catchup =
  let open Lwt.Syntax in
  let rec run () =
    deps.normalize ~source:("catchup:" ^ reason);
    let our_head_int = deps.head_epoch () in
    let our_head = Int64.of_int our_head_int in
    let rec loop ~from_epoch =
      if Int64.compare from_epoch target_epoch > 0 then begin
        Log.info "catchup"
          "CATCHUP_COMPLETE at epoch = %Ld reason = %s"
          target_epoch
          reason;
        finish_success reason
      end else
        let* query_step =
          query_chunk deps.query ~target_epoch ~from_epoch ~reason
        in
        match query_step with
        | Query_failed tag ->
          fail_catchup tag
        | Query_chunk chunk ->
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
            finish_success tag
          | Chunk_retry ->
            run ()
          | Chunk_fail tag ->
            fail_catchup tag
          | Chunk_continue applied_chunk ->
            loop ~from_epoch:(Int64.succ (last_applied_epoch applied_chunk))
    in
    if Int64.compare target_epoch our_head <= 0 then begin
      Log.info "catchup"
        "CATCHUP_COMPLETE already in sync target = %Ld reason = %s"
        target_epoch
        reason;
      finish_success ("already_in_sync:" ^ reason)
    end else begin
      Log.warn "catchup"
        "CATCHUP_START local = %d target = %Ld lag = %d reason = %s"
        our_head_int
        target_epoch
        (Int64.to_int (Int64.sub target_epoch our_head))
        reason;
      loop ~from_epoch:(Int64.succ our_head)
    end
  in
  run ()

let queue_active (deps : deps) ~target_epoch ~reason =
  let queued = deps.queue_target ~target_epoch ~reason in
  Log.warn "catchup"
    "queued catchup target = %Ld reason = %s active = %b queued = %s"
    target_epoch reason true queued;
  Lwt.return_unit

let run (deps : deps) ~run_one ~target_epoch ~reason =
  let open Lwt.Syntax in
  let rec finish_success active_reason =
    let next_height = Int64.succ (Int64.of_int (deps.committed_head_epoch ())) in
    let* () = deps.start_height next_height in
    deps.set_catchup_active false;
    let head = Int64.of_int (deps.committed_head_epoch ()) in
    match deps.take_queued_after ~head with
    | Some queued ->
      deps.set_catchup_active true;
      Log.warn "catchup"
        "CATCHUP_CONTINUE active_reason = %s next_target = %Ld next_reason = %s head = %Ld"
        active_reason queued.target_epoch queued.reason head;
      run_queued queued
    | None ->
      deps.clear_queue ();
      let head = deps.committed_head_epoch () in
      let* root = deps.read_local_root () in
      deps.set_state_attested ~head ~root;
      deps.clear_quarantine ("catchup_complete:" ^ active_reason);
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
    run_one
      ~target_epoch:queued.target_epoch
      ~reason:queued.reason
      ~finish_success
      ~fail_catchup
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
    head_before_record = wiring.head_epoch;
    put_proposer = wiring.put_proposer;
    put_expected_root = wiring.put_expected_root;
    activate_gap = wiring.activate_gap;
    point_source;
    write_finality = wiring.write_finality;
    apply_record = wiring.apply_record;
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
    normalize = wiring.normalize;
    head_epoch = wiring.head_epoch;
    env_timeout = wiring.env_timeout;
    read_query_root = wiring.read_local_root;
    range_query = wiring.range_query;
    read_apply_root = wiring.read_local_root;
    cached_head = wiring.cached_head;
    next_txid = wiring.next_txid;
    put_proposer = wiring.finality.store_proposer;
    put_expected_root = (fun epoch root ->
      wiring.finality.store_expected_root ~epoch ~root);
    activate_gap = (fun () ->
      Consensus_catchup_queue.activate_gap wiring.queue);
    write_finality = wiring.write_finality;
    apply_record = wiring.apply_record;
    read_local_root = wiring.read_local_root;
    base_eic = wiring.base_eic;
    current_head = wiring.current_head;
    gap_active = (fun () ->
      Consensus_catchup_queue.gap_active wiring.queue);
  }

let driver_io_of_driver driver =
  {
    start_height = C_driver.start_height driver;
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
    };
  }

let node_deps_of_driver_runner wiring io =
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

let target_wiring_of_driver_runner wiring io =
  target_wiring_of_node
    {
      normalize = wiring.normalize;
      head_epoch = wiring.committed_head_epoch;
      env_timeout = wiring.env_timeout;
      read_local_root = wiring.read_local_root;
      range_query = io.range_query;
      cached_head = wiring.cached_head;
      next_txid = wiring.next_txid;
      finality = wiring.finality;
      queue = wiring.queue;
      write_finality = wiring.write_finality;
      apply_record = wiring.apply_record;
      base_eic = wiring.base_eic;
      current_head = wiring.current_head;
    }

let run_driver_wired wiring io ~target_epoch ~reason =
  run_with_target
    (node_deps_of_driver_runner wiring io)
    ~target:(target_wiring_of_driver_runner wiring io |> target_of_wiring)
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
    "queued catchup target = %Ld reason = %s active = %b queued = %s"
    event.queued_target_epoch
    event.queued_reason
    event.queued_active
    event.queued_label

let run_wired deps ~target ~target_epoch ~reason =
  run_with_target
    deps
    ~target:(target_of_wiring target)
    ~target_epoch
    ~reason