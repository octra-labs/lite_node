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


module C_driver = Octra_consensus.C_driver
module Head_manifest = Octra_core.Head_manifest
module Wal = Octra_core.Wal

type decision =
  | Confirmed_elsewhere of {
      agreed : int;
      quorum : int;
    }
  | All_peers_missing of {
      responses : int;
    }
  | Inconclusive of {
      agreed : int;
      peers : int;
      responses : int;
      quorum : int;
    }

let raw32_of_hex value =
  try
    let len = String.length value / 2 in
    let raw = String.init len (fun i ->
      Char.chr (int_of_string ("0x" ^ String.sub value (i * 2) 2))) in
    if String.length raw = 32 then Some raw
    else if String.length raw > 32 then Some (String.sub raw 0 32)
    else Some (raw ^ String.make (32 - String.length raw) '\x00')
  with _ ->
    None

let root_matches promised_root_raw (r : C_driver.epoch_root_response_record) =
  match r.state_root, promised_root_raw with
  | Some local, Some promised -> local = promised
  | _ -> false

let all_missing responses =
  List.for_all
    (fun (r : C_driver.epoch_root_response_record) -> r.state_root = None)
    responses

let decide ~validator_count ~peer_quorum ~promised_root_raw responses =
  let agreed =
    List.filter (root_matches promised_root_raw) responses
    |> List.length
  in
  let response_count = List.length responses in
  let peers = validator_count - 1 in
  if agreed >= peer_quorum then
    Confirmed_elsewhere { agreed; quorum = peer_quorum }
  else if response_count >= peers && all_missing responses then
    All_peers_missing { responses = response_count }
  else
    Inconclusive {
      agreed;
      peers;
      responses = response_count;
      quorum = peer_quorum;
    }

type deps = {
  read_pending_commits : unit -> Wal.pending_commit list;
  head_epoch : unit -> int;
  query_epoch_root :
    epoch_id:int64 ->
    C_driver.epoch_root_response_record list Lwt.t;
  run_catchup_to_target : target_epoch:int64 -> reason:string -> unit Lwt.t;
  delete_pending_commit : epoch_id:int -> round:int -> unit;
}

type 'driver driver_runtime = {
  read_pending_commits : unit -> Wal.pending_commit list;
  head : unit -> Head_manifest.t option;
  query_epoch_root :
    'driver ->
    epoch_id:int64 ->
    C_driver.epoch_root_response_record list Lwt.t;
  run_catchup_to_target :
    'driver ->
    target_epoch:int64 ->
    reason:string ->
    unit Lwt.t;
  delete_pending_commit : epoch_id:int -> round:int -> unit;
  validator_count : int;
  peer_quorum : int;
}

let head_epoch_of_manifest = function
  | Some head -> head.Head_manifest.epoch_id
  | None -> -1

let deps_of_driver_runtime (runtime : 'driver driver_runtime) driver =
  {
    read_pending_commits = runtime.read_pending_commits;
    head_epoch = (fun () -> head_epoch_of_manifest (runtime.head ()));
    query_epoch_root = (fun ~epoch_id ->
      runtime.query_epoch_root driver ~epoch_id);
    run_catchup_to_target = (fun ~target_epoch ~reason ->
      runtime.run_catchup_to_target driver ~target_epoch ~reason);
    delete_pending_commit = runtime.delete_pending_commit;
  }

let unresolved (deps : deps) =
  let head = deps.head_epoch () in
  let pending =
    deps.read_pending_commits ()
    |> List.filter (fun p -> p.Wal.epoch_id > head)
  in
  head, pending

let delete (deps : deps) p =
  deps.delete_pending_commit ~epoch_id:p.Wal.epoch_id ~round:p.round

let run_confirmed (deps : deps) p ~agreed ~quorum =
  let open Lwt.Syntax in
  let epoch = Int64.of_int p.Wal.epoch_id in
  Octra_log.warn "consensus"
    "event = pending_commit action = catchup_recovery epoch = %d peer_root_match = %d quorum = %d"
    p.epoch_id agreed quorum;
  let* () = deps.run_catchup_to_target
    ~target_epoch:epoch
    ~reason:"pending_commit_recovery" in
  let head_after = deps.head_epoch () in
  if head_after >= p.epoch_id then begin
    delete deps p;
    Octra_log.info "consensus"
      "event = pending_commit action = recovery_applied epoch = %d head = %d"
      p.epoch_id head_after;
    Lwt.return_unit
  end else begin
    Octra_log.warn "consensus"
      "event = pending_commit action = keep_breadcrumb epoch = %d head = %d reason = catchup_no_advance"
      p.epoch_id head_after;
    Lwt.return_unit
  end

let run_all_missing deps p ~responses =
  Octra_log.info "consensus"
    "event = pending_commit action = drop_breadcrumb epoch = %d responses = %d reason = peers_missing_epoch"
    p.Wal.epoch_id responses;
  delete deps p;
  Lwt.return_unit

let run_inconclusive p ~agreed ~peers ~responses ~quorum =
  Octra_log.warn "consensus"
    "event = pending_commit action = keep_breadcrumb epoch = %d agreed = %d peers = %d responses = %d quorum = %d reason = inconclusive"
    p.Wal.epoch_id agreed peers responses quorum;
  Lwt.return_unit

let run_pending (deps : deps) ~validator_count ~peer_quorum p =
  let open Lwt.Syntax in
  let epoch = Int64.of_int p.Wal.epoch_id in
  let* responses = deps.query_epoch_root ~epoch_id:epoch in
  let promised_root_raw = raw32_of_hex p.Wal.proposed_state_root in
  match decide ~validator_count ~peer_quorum ~promised_root_raw responses with
  | Confirmed_elsewhere { agreed; quorum } ->
    run_confirmed deps p ~agreed ~quorum
  | All_peers_missing { responses } ->
    run_all_missing deps p ~responses
  | Inconclusive { agreed; peers; responses; quorum } ->
    run_inconclusive p ~agreed ~peers ~responses ~quorum

let run_once (deps : deps) ~validator_count ~peer_quorum =
  let head, pending = unresolved deps in
  if pending = [] then Lwt.return_unit
  else begin
    Octra_log.warn "consensus"
      "event = pending_commit_recovery action = query_peers records = %d head = %d"
      (List.length pending) head;
    Lwt_list.iter_s
      (run_pending deps ~validator_count ~peer_quorum)
      pending
  end

let run_with_driver runtime driver =
  run_once
    (deps_of_driver_runtime runtime driver)
    ~validator_count:runtime.validator_count
    ~peer_quorum:runtime.peer_quorum