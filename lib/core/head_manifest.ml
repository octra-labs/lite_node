(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let schema_version = 3

type t = {
  schema_version : int;
  generation : int;
  epoch_id : int;
  state_root : string;
  ledger_state_root : string option;
  irmin_commit : string option;
  txid_hi : int64;
  txlog_seg : int option;
  txlog_off : int option;
  epochlog_off : int option;
  commit_id : string;
  ts : float;
  quorum_cert_hash : string option;
  epoch_index_hash : string option;
  epoch_index_root : string option;
}

let opt_int_to_json = function Some i -> `Int i | None -> `Null

let opt_str_to_json = function Some s -> `String s | None -> `Null

let opt_int_of_json j =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ -> (try Some (to_int j) with _ -> None)

let opt_str_of_json j =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ -> (try Some (to_string j) with _ -> None)

let ledger_state_root t =
  match t.ledger_state_root with
  | Some root -> root
  | None -> t.state_root

let to_json t =
  `Assoc [
    "schema_version", `Int t.schema_version;
    "generation",  `Int t.generation;
    "epoch_id",    `Int t.epoch_id;
    "state_root",  `String t.state_root;
    "ledger_state_root", opt_str_to_json t.ledger_state_root;
    "irmin_commit",opt_str_to_json t.irmin_commit;
    "txid_hi",     `String (Int64.to_string t.txid_hi);
    "txlog_seg",   opt_int_to_json t.txlog_seg;
    "txlog_off",   opt_int_to_json t.txlog_off;
    "epochlog_off",opt_int_to_json t.epochlog_off;
    "commit_id",   `String t.commit_id;
    "ts",          `Float t.ts;
    "quorum_cert_hash", opt_str_to_json t.quorum_cert_hash;
    "epoch_index_hash", opt_str_to_json t.epoch_index_hash;
    "epoch_index_root", opt_str_to_json t.epoch_index_root;
  ] |> Yojson.Safe.to_string

let of_json s =
  let j = Yojson.Safe.from_string s in
  let open Yojson.Safe.Util in
  let sv = try j |> member "schema_version" |> to_int with _ -> 1 in
  {
    schema_version = sv;
    generation   = j |> member "generation" |> to_int;
    epoch_id     = j |> member "epoch_id" |> to_int;
    state_root   = j |> member "state_root" |> to_string;
    ledger_state_root =
      (try opt_str_of_json (j |> member "ledger_state_root") with _ -> None);
    irmin_commit = (try opt_str_of_json (j |> member "irmin_commit") with _ -> None);
    txid_hi      = Int64.of_string (j |> member "txid_hi" |> to_string);
    txlog_seg    = opt_int_of_json (j |> member "txlog_seg");
    txlog_off    = opt_int_of_json (j |> member "txlog_off");
    epochlog_off = opt_int_of_json (j |> member "epochlog_off");
    commit_id    = j |> member "commit_id" |> to_string;
    ts           = j |> member "ts" |> to_number;
    quorum_cert_hash =
      (try opt_str_of_json (j |> member "quorum_cert_hash") with _ -> None);
    epoch_index_hash =
      (try opt_str_of_json (j |> member "epoch_index_hash") with _ -> None);
    epoch_index_root =
      (try opt_str_of_json (j |> member "epoch_index_root") with _ -> None);
  }

let path data_dir = Filename.concat data_dir "HEAD.json"

let atomic_write data_dir t =
  let p = path data_dir in
  let tmp = p ^ ".tmp" in
  let json = to_json t in
  let oc = open_out_bin tmp in
  output_string oc json;
  flush oc;
  (try Unix.fsync (Unix.descr_of_out_channel oc) with _ -> ());
  close_out oc;
  Unix.rename tmp p;
  (try
    let dfd = Unix.openfile data_dir [Unix.O_RDONLY] 0 in
    (try Unix.fsync dfd with _ -> ());
    Unix.close dfd
   with _ -> ())

let load data_dir =
  let p = path data_dir in
  if not (Sys.file_exists p) then None
  else
    try
      let ic = open_in p in
      let n = in_channel_length ic in
      let buf = Bytes.create n in
      really_input ic buf 0 n;
      close_in ic;
      Some (of_json (Bytes.to_string buf))
    with _ -> None

type load_result =
  | Missing
  | Present of t
  | Corrupt of string

let load_result data_dir =
  let p = path data_dir in
  if not (Sys.file_exists p) then Missing
  else
    try
      let ic = open_in p in
      let n = in_channel_length ic in
      let buf = Bytes.create n in
      really_input ic buf 0 n;
      close_in ic;
      Present (of_json (Bytes.to_string buf))
    with e -> Corrupt (Printexc.to_string e)

let cached : t option ref = ref None

let set_cached h = cached := Some h

let get_cached () = !cached

let load_to_cache data_dir =
  cached := load data_dir;
  !cached

let is_epoch_visible head epoch_id =
  match head with
  | None -> true
  | Some h -> epoch_id <= h.epoch_id

let is_txid_visible head txid =
  match head with
  | None -> true
  | Some h -> Int64.compare txid h.txid_hi <= 0

let is_txlog_pos_visible head ~seg ~off =
  match head with
  | None -> true
  | Some h ->
    (match h.txlog_seg, h.txlog_off with
     | None, _ | _, None -> true
     | Some hs, Some ho ->
       if seg < hs then true
       else if seg > hs then false
       else off <= ho)