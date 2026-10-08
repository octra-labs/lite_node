(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type entry = {
  epoch_id : int;
  pre_state_root : string;
  post_state_root : string;
  parent_commit : string;
  irmin_parent : string option;
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
    "irmin_parent", (match e.irmin_parent with Some hash -> `String hash | None -> `Null);
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
    irmin_parent = (match j |> member "irmin_parent" with
      | `Null -> None
      | `String hash when hash <> "" -> Some hash
      | _ -> failwith "WAL Irmin parent is invalid");
    start_txid = Int64.of_string (j |> member "start_txid" |> to_string);
    tx_count = j |> member "tx_count" |> to_int;
    finalized_by = j |> member "finalized_by" |> to_string;
    finalized_at = j |> member "finalized_at" |> to_number;
    irmin_last_epoch_before = j |> member "irmin_last_epoch_before" |> to_int;
  }

let wal_dir data_dir = Filename.concat data_dir "wal"

let entry_path data_dir epoch_id =
  Filename.concat (wal_dir data_dir) (Printf.sprintf "%010d.wal" epoch_id)

let sync_dir ?(sync = Unix.fsync) dir =
  let fd = Unix.openfile dir [Unix.O_RDONLY] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> sync fd)

let dir_exists dir =
  match Unix.lstat dir with
  | stat when stat.Unix.st_kind = Unix.S_DIR -> true
  | _ -> failwith ("WAL directory is not a directory: " ^ dir)
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false

let ensure_dir ?sync data_dir =
  let dir = wal_dir data_dir in
  if not (dir_exists dir) then begin
    (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    if not (dir_exists dir) then failwith "WAL directory disappeared"
  end;
  sync_dir ?sync data_dir

let write data_dir entry =
  ensure_dir data_dir;
  let path = entry_path data_dir entry.epoch_id in
  let staged = path ^ ".staged" in
  let json = to_json entry in
  let oc = open_out_bin staged in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () ->
    output_string oc json;
    flush oc;
    Unix.fsync (Unix.descr_of_out_channel oc));
  Unix.rename staged path;
  sync_dir (wal_dir data_dir)

let delete_file ?sync path =
  (try Unix.unlink path with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  sync_dir ?sync (Filename.dirname path)

let delete data_dir epoch_id =
  delete_file (entry_path data_dir epoch_id)

exception Read_error of string * string

let read_file path action =
  try action () with
  | (Failure _ | Invalid_argument _ | Sys_error _ | End_of_file
    | Unix.Unix_error _ | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _) as exn ->
    raise (Read_error (path, Printexc.to_string exn))

let read_entry path =
  read_file path (fun () ->
  let stat = Unix.lstat path in
  if stat.Unix.st_kind <> Unix.S_REG then
    failwith ("WAL entry is not a regular file: " ^ path);
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
    let entry = of_json (really_input_string channel (in_channel_length channel)) in
    if entry.epoch_id < 0
       || Filename.basename path <> Printf.sprintf "%010d.wal" entry.epoch_id then
      failwith ("WAL entry epoch differs from filename: " ^ path);
    entry))

let read_pending data_dir =
  let dir = wal_dir data_dir in
  read_file dir (fun () ->
  if not (dir_exists dir) then []
  else
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".wal")
    |> List.map (fun f -> read_entry (Filename.concat dir f))
    |> List.sort (fun a b -> compare a.epoch_id b.epoch_id))

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
      "WAL epoch = %d expected_irmin_before = %d, actual chaindata = %d irmin = %d"
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
  proposal_b64 : string option;
  vote_b64 : string option;
  tx_hashes : string list;
  txs_json : string list;
  receipts_json : string list;
}

let max_pending_commit_bytes = 128 * 1024 * 1024

let string_list_to_json values =
  `List (List.map (fun value -> `String value) values)

let pending_commit_to_json p =
  `Assoc [
    "epoch_id", `Int p.epoch_id;
    "round", `Int p.round;
    "proposal_id", `String p.proposal_id;
    "proposed_state_root", `String p.proposed_state_root;
    "txid_hi", `String (Int64.to_string p.txid_hi);
    "ts", `Float p.ts;
    "validator_addr", `String p.validator_addr;
    "proposal_b64",
      (match p.proposal_b64 with Some value -> `String value | None -> `Null);
    "vote_b64",
      (match p.vote_b64 with Some value -> `String value | None -> `Null);
    "tx_hashes", string_list_to_json p.tx_hashes;
    "txs_json", string_list_to_json p.txs_json;
    "receipts_json", string_list_to_json p.receipts_json;
  ] |> Yojson.Safe.to_string

let pending_commit_of_json s =
  let j = Yojson.Safe.from_string s in
  let open Yojson.Safe.Util in
  let optional_string name =
    match j |> member name with
    | `String value -> Some value
    | `Null -> None
    | _ -> failwith ("pending commit field is invalid: " ^ name)
  in
  let string_list name =
    match j |> member name with
    | `List values -> List.map to_string values
    | `Null -> []
    | _ -> failwith ("pending commit field is invalid: " ^ name)
  in
  {
    epoch_id = j |> member "epoch_id" |> to_int;
    round = j |> member "round" |> to_int;
    proposal_id = j |> member "proposal_id" |> to_string;
    proposed_state_root = j |> member "proposed_state_root" |> to_string;
    txid_hi = Int64.of_string (j |> member "txid_hi" |> to_string);
    ts = j |> member "ts" |> to_number;
    validator_addr = j |> member "validator_addr" |> to_string;
    proposal_b64 = optional_string "proposal_b64";
    vote_b64 = optional_string "vote_b64";
    tx_hashes = string_list "tx_hashes";
    txs_json = string_list "txs_json";
    receipts_json = string_list "receipts_json";
  }

let pending_commit_path data_dir epoch_id round =
  Filename.concat (wal_dir data_dir)
    (Printf.sprintf "%010d_%04d.pending" epoch_id round)

let read_pending_commit_file path =
  read_file path (fun () ->
  let stat = Unix.lstat path in
  if stat.Unix.st_kind <> Unix.S_REG then
    failwith "pending commit is not a regular file";
  if stat.Unix.st_size <= 0 || stat.Unix.st_size > max_pending_commit_bytes then
    failwith "pending commit size is invalid";
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let record = pending_commit_of_json
        (really_input_string ic stat.Unix.st_size) in
      if record.epoch_id < 0 || record.round < 0
         || Filename.basename path <>
           Printf.sprintf "%010d_%04d.pending" record.epoch_id record.round then
        failwith ("pending commit position differs from filename: " ^ path);
      record))

let same_pending_commit left right =
  left.epoch_id = right.epoch_id
  && left.round = right.round
  && left.proposal_id = right.proposal_id
  && left.proposed_state_root = right.proposed_state_root
  && left.txid_hi = right.txid_hi
  && left.validator_addr = right.validator_addr
  && left.proposal_b64 = right.proposal_b64
  && left.vote_b64 = right.vote_b64
  && left.tx_hashes = right.tx_hashes
  && left.txs_json = right.txs_json
  && left.receipts_json = right.receipts_json

let write_pending_commit ?(sync = Unix.fsync) data_dir p =
  ensure_dir ~sync data_dir;
  let path = pending_commit_path data_dir p.epoch_id p.round in
  let write () =
    let staged = path ^ ".staged" in
    let json = pending_commit_to_json p in
    if String.length json > max_pending_commit_bytes then
      failwith "pending commit exceeds size limit";
    let oc = open_out_bin staged in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () ->
        output_string oc json;
        flush oc;
        sync (Unix.descr_of_out_channel oc));
    Unix.rename staged path;
    sync_dir ~sync (wal_dir data_dir)
  in
  if Sys.file_exists path then begin
    let existing = read_pending_commit_file path in
    if not (same_pending_commit existing p) then
      failwith "conflicting pending commit record";
    let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> sync fd);
    sync_dir ~sync (wal_dir data_dir)
  end else
    write ()

let delete_pending_commit data_dir epoch_id round =
  delete_file (pending_commit_path data_dir epoch_id round)

let delete_pending_commits_for_epoch data_dir epoch_id =
  let dir = wal_dir data_dir in
  if dir_exists dir then begin
    let prefix = Printf.sprintf "%010d_" epoch_id in
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f ->
        Filename.check_suffix f ".pending"
        && String.length f >= String.length prefix
        && String.sub f 0 (String.length prefix) = prefix)
    |> List.iter (fun f -> delete_file (Filename.concat dir f))
  end

let read_pending_commits data_dir =
  let dir = wal_dir data_dir in
  read_file dir (fun () ->
  if not (dir_exists dir) then []
  else
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".pending")
    |> List.map (fun f ->
        let path = Filename.concat dir f in
        read_pending_commit_file path)
    |> List.sort (fun a b ->
        let by_epoch = compare a.epoch_id b.epoch_id in
        if by_epoch <> 0 then by_epoch else compare a.round b.round))