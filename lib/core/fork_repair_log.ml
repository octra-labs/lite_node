(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type plan = {
  source : Head_manifest.t;
  target_root : string;
  next_txid : int64;
  head : Head_manifest.t;
}

type load =
  | Missing
  | Ready of plan
  | Invalid of string

let schema = 1

let path data_dir =
  Filename.concat data_dir "fork_repair.json"

let staged_path data_dir =
  path data_dir ^ ".staged"

let hex64 value =
  String.length value = 64
  && String.for_all
       (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
       value

let validate plan =
  let head = plan.head in
  let target = head.Head_manifest.epoch_id in
  let source = plan.source in
  if source.schema_version <> Head_manifest.schema_version then
    Error "fork repair source HEAD schema mismatch"
  else if source.generation <> source.epoch_id then
    Error "fork repair source HEAD generation mismatch"
  else if source.epoch_id <= target then
    Error "fork repair source is not ahead of target"
  else if head.schema_version <> Head_manifest.schema_version then
    Error "fork repair HEAD schema mismatch"
  else if head.generation <> target then
    Error "fork repair HEAD generation mismatch"
  else if not (hex64 plan.target_root) then
    Error "fork repair target root is invalid"
  else if not (String.equal head.state_root plan.target_root) then
    Error "fork repair HEAD root mismatch"
  else if Option.is_none head.ledger_state_root then
    Error "fork repair ledger root is missing"
  else if Option.is_none head.irmin_commit then
    Error "fork repair Irmin commit is missing"
  else if Option.is_none head.txlog_seg || Option.is_none head.txlog_off then
    Error "fork repair txlog boundary is missing"
  else if Option.is_none head.epochlog_off then
    Error "fork repair epochlog boundary is missing"
  else if Option.is_none head.epoch_index_hash
          || Option.is_none head.epoch_index_root then
    Error "fork repair epoch commitment is missing"
  else if Int64.compare plan.next_txid 0L < 0 then
    Error "fork repair next txid is invalid"
  else if Int64.equal head.txid_hi Int64.max_int
          || not (Int64.equal plan.next_txid (Int64.succ head.txid_hi)) then
    Error "fork repair transaction boundary mismatch"
  else
    Ok plan

let to_json plan =
  `Assoc [
    "schema", `Int schema;
    "source", Yojson.Safe.from_string (Head_manifest.to_json plan.source);
    "target_root", `String plan.target_root;
    "next_txid", `String (Int64.to_string plan.next_txid);
    "head", Yojson.Safe.from_string (Head_manifest.to_json plan.head);
  ]

let of_json json =
  let open Yojson.Safe.Util in
  let got_schema = json |> member "schema" |> to_int in
  if got_schema <> schema then
    Error "fork repair log schema mismatch"
  else
    let plan = {
      source =
        json
        |> member "source"
        |> Yojson.Safe.to_string
        |> Head_manifest.of_json;
      target_root = json |> member "target_root" |> to_string;
      next_txid = json |> member "next_txid" |> to_string |> Int64.of_string;
      head = json |> member "head" |> Yojson.Safe.to_string |> Head_manifest.of_json;
    } in
    validate plan

let rec write_all fd value offset =
  if offset < String.length value then begin
    let wrote =
      Unix.write_substring fd value offset (String.length value - offset)
    in
    if wrote <= 0 then failwith "fork repair log write returned zero";
    write_all fd value (offset + wrote)
  end

let sync_dir data_dir =
  let fd = Unix.openfile data_dir [Unix.O_RDONLY] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () -> Unix.fsync fd)

let write data_dir plan =
  match validate plan with
  | Error reason -> invalid_arg reason
  | Ok plan ->
    let target = path data_dir in
    let staged = staged_path data_dir in
    let bytes = Yojson.Safe.to_string (to_json plan) ^ "\n" in
    let fd =
      Unix.openfile staged [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o640
    in
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () ->
        write_all fd bytes 0;
        Unix.fsync fd);
    Unix.rename staged target;
    sync_dir data_dir

let read data_dir =
  let target = path data_dir in
  if not (Sys.file_exists target) then Missing
  else
    try
      match Yojson.Safe.from_file target |> of_json with
      | Ok plan -> Ready plan
      | Error reason -> Invalid reason
    with exn ->
      Invalid (Printexc.to_string exn)

let clear data_dir =
  let target = path data_dir in
  if Sys.file_exists target then begin
    Unix.unlink target;
    sync_dir data_dir
  end