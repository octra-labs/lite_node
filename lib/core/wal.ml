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


type entry = {
  epoch_id : int;
  pre_state_root : string;
  post_state_root : string;
  parent_commit : string;
  start_txid : int64;
  tx_count : int;
  finalized_by : string;
  finalized_at : float;
  irmin_last_epoch_before : int;
}

let to_json e =
  `Assoc [
    "epoch_id", `Int e.epoch_id;
    "pre_state_root", `String e.pre_state_root;
    "post_state_root", `String e.post_state_root;
    "parent_commit", `String e.parent_commit;
    "start_txid", `String (Int64.to_string e.start_txid);
    "tx_count", `Int e.tx_count;
    "finalized_by", `String e.finalized_by;
    "finalized_at", `Float e.finalized_at;
    "irmin_last_epoch_before", `Int e.irmin_last_epoch_before;
  ] |> Yojson.Safe.to_string

let of_json s =
  let j = Yojson.Safe.from_string s in
  let open Yojson.Safe.Util in
  {
    epoch_id = j |> member "epoch_id" |> to_int;
    pre_state_root = j |> member "pre_state_root" |> to_string;
    post_state_root = j |> member "post_state_root" |> to_string;
    parent_commit = j |> member "parent_commit" |> to_string;
    start_txid = Int64.of_string (j |> member "start_txid" |> to_string);
    tx_count = j |> member "tx_count" |> to_int;
    finalized_by = j |> member "finalized_by" |> to_string;
    finalized_at = j |> member "finalized_at" |> to_number;
    irmin_last_epoch_before = j |> member "irmin_last_epoch_before" |> to_int;
  }

let wal_dir data_dir = Filename.concat data_dir "wal"

let entry_path data_dir epoch_id =
  Filename.concat (wal_dir data_dir) (Printf.sprintf "%010d.wal" epoch_id)

let ensure_dir data_dir =
  let dir = wal_dir data_dir in
  if not (Sys.file_exists dir) then
    (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let write data_dir entry =
  ensure_dir data_dir;
  let path = entry_path data_dir entry.epoch_id in
  let tmp = path ^ ".tmp" in
  let json = to_json entry in
  let oc = open_out_bin tmp in
  output_string oc json;
  flush oc;
  (try Unix.fsync (Unix.descr_of_out_channel oc) with _ -> ());
  close_out oc;
  Unix.rename tmp path;

  (try
    let dfd = Unix.openfile (wal_dir data_dir) [Unix.O_RDONLY] 0 in
    (try Unix.fsync dfd with _ -> ());
    Unix.close dfd
   with _ -> ())

let delete data_dir epoch_id =
  let path = entry_path data_dir epoch_id in
  if Sys.file_exists path then
    (try Sys.remove path with _ -> ())

let read_pending data_dir =
  let dir = wal_dir data_dir in
  if not (Sys.file_exists dir) then []
  else
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".wal")
    |> List.filter_map (fun f ->
        let path = Filename.concat dir f in
        try
          let ic = open_in path in
          let n = in_channel_length ic in
          let buf = Bytes.create n in
          really_input ic buf 0 n;
          close_in ic;
          Some (of_json (Bytes.to_string buf))
        with _ -> None)
    |> List.sort (fun a b -> compare a.epoch_id b.epoch_id)

type recovery_action =
  | Skip
  | ForwardTagOnly
  | ForwardReplayIrmin
  | ForwardReplayChaindataAndIrmin
  | InconsistentState of string

let decide_action ~entry ~chaindata_last_epoch ~irmin_last_epoch =
  if chaindata_last_epoch >= entry.epoch_id
     && irmin_last_epoch >= entry.epoch_id then Skip
  else if chaindata_last_epoch >= entry.epoch_id
          && irmin_last_epoch = entry.irmin_last_epoch_before then
    ForwardReplayIrmin
  else if chaindata_last_epoch >= entry.epoch_id
          && irmin_last_epoch >= entry.epoch_id then
    ForwardTagOnly
  else if chaindata_last_epoch = entry.epoch_id - 1
          && irmin_last_epoch = entry.irmin_last_epoch_before then
    ForwardReplayChaindataAndIrmin
  else
    InconsistentState (Printf.sprintf
      "WAL epoch=%d expected_irmin_before=%d, actual chaindata=%d irmin=%d"
      entry.epoch_id entry.irmin_last_epoch_before
      chaindata_last_epoch irmin_last_epoch)

let action_to_string = function
  | Skip -> "Skip"
  | ForwardTagOnly -> "ForwardTagOnly"
  | ForwardReplayIrmin -> "ForwardReplayIrmin"
  | ForwardReplayChaindataAndIrmin -> "ForwardReplayChaindataAndIrmin"
  | InconsistentState s -> Printf.sprintf "InconsistentState(%s)" s

type pending_commit = {
  epoch_id : int;
  round : int;
  proposal_id : string;
  proposed_state_root : string;
  txid_hi : int64;
  ts : float;
  validator_addr : string;
}

let pending_commit_to_json p =
  `Assoc [
    "epoch_id", `Int p.epoch_id;
    "round", `Int p.round;
    "proposal_id", `String p.proposal_id;
    "proposed_state_root", `String p.proposed_state_root;
    "txid_hi", `String (Int64.to_string p.txid_hi);
    "ts", `Float p.ts;
    "validator_addr", `String p.validator_addr;
  ] |> Yojson.Safe.to_string

let pending_commit_of_json s =
  let j = Yojson.Safe.from_string s in
  let open Yojson.Safe.Util in
  {
    epoch_id = j |> member "epoch_id" |> to_int;
    round = j |> member "round" |> to_int;
    proposal_id = j |> member "proposal_id" |> to_string;
    proposed_state_root = j |> member "proposed_state_root" |> to_string;
    txid_hi = Int64.of_string (j |> member "txid_hi" |> to_string);
    ts = j |> member "ts" |> to_number;
    validator_addr = j |> member "validator_addr" |> to_string;
  }

let pending_commit_path data_dir epoch_id round =
  Filename.concat (wal_dir data_dir)
    (Printf.sprintf "%010d_%04d.pending" epoch_id round)

let write_pending_commit data_dir p =
  ensure_dir data_dir;
  let path = pending_commit_path data_dir p.epoch_id p.round in
  let tmp = path ^ ".tmp" in
  let json = pending_commit_to_json p in
  let oc = open_out_bin tmp in
  output_string oc json;
  flush oc;
  (try Unix.fsync (Unix.descr_of_out_channel oc) with _ -> ());
  close_out oc;
  Unix.rename tmp path;
  (try
    let dfd = Unix.openfile (wal_dir data_dir) [Unix.O_RDONLY] 0 in
    (try Unix.fsync dfd with _ -> ());
    Unix.close dfd
   with _ -> ())

let delete_pending_commit data_dir epoch_id round =
  let path = pending_commit_path data_dir epoch_id round in
  if Sys.file_exists path then
    (try Sys.remove path with _ -> ())

let delete_pending_commits_for_epoch data_dir epoch_id =
  let dir = wal_dir data_dir in
  if Sys.file_exists dir then begin
    let prefix = Printf.sprintf "%010d_" epoch_id in
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f ->
        Filename.check_suffix f ".pending"
        && String.length f >= String.length prefix
        && String.sub f 0 (String.length prefix) = prefix)
    |> List.iter (fun f ->
        try Sys.remove (Filename.concat dir f) with _ -> ())
  end

let read_pending_commits data_dir =
  let dir = wal_dir data_dir in
  if not (Sys.file_exists dir) then []
  else
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".pending")
    |> List.filter_map (fun f ->
        let path = Filename.concat dir f in
        try
          let ic = open_in path in
          let n = in_channel_length ic in
          let buf = Bytes.create n in
          really_input ic buf 0 n;
          close_in ic;
          Some (pending_commit_of_json (Bytes.to_string buf))
        with _ -> None)
    |> List.sort (fun a b ->
        let by_epoch = compare a.epoch_id b.epoch_id in
        if by_epoch <> 0 then by_epoch else compare a.round b.round)