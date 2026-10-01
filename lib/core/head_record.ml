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

let opt_int_to_json = function Some value -> `Int value | None -> `Null
let opt_str_to_json = function Some value -> `String value | None -> `Null

let opt_int_of_json = function
  | `Null -> None
  | value -> Some (Yojson.Safe.Util.to_int value)

let opt_str_of_json = function
  | `Null -> None
  | value -> Some (Yojson.Safe.Util.to_string value)

let ledger_state_root head =
  match head.ledger_state_root with Some root -> root | None -> head.state_root

let to_json head =
  `Assoc [
    "schema_version", `Int head.schema_version;
    "generation", `Int head.generation;
    "epoch_id", `Int head.epoch_id;
    "state_root", `String head.state_root;
    "ledger_state_root", opt_str_to_json head.ledger_state_root;
    "irmin_commit", opt_str_to_json head.irmin_commit;
    "txid_hi", `String (Int64.to_string head.txid_hi);
    "txlog_seg", opt_int_to_json head.txlog_seg;
    "txlog_off", opt_int_to_json head.txlog_off;
    "epochlog_off", opt_int_to_json head.epochlog_off;
    "commit_id", `String head.commit_id;
    "ts", `Float head.ts;
    "quorum_cert_hash", opt_str_to_json head.quorum_cert_hash;
    "epoch_index_hash", opt_str_to_json head.epoch_index_hash;
    "epoch_index_root", opt_str_to_json head.epoch_index_root;
  ] |> Yojson.Safe.to_string

let require reason valid = if not valid then failwith ("HEAD " ^ reason)

let fields version has_version rows =
  let base = ["generation"; "epoch_id"; "state_root"; "txid_hi";
    "txlog_seg"; "txlog_off"; "epochlog_off"; "commit_id"; "ts"] in
  let prefix = if has_version then "schema_version" :: base else base in
  let known = match version with
    | 1 -> prefix
    | 2 -> "irmin_commit" :: prefix
    | 3 -> "quorum_cert_hash" :: "irmin_commit" :: prefix
    | _ -> failwith "HEAD schema is unsupported" in
  let full = ["schema_version"; "irmin_commit"; "quorum_cert_hash";
    "ledger_state_root"; "epoch_index_hash"; "epoch_index_root"] @ base in
  let actual = List.map fst rows |> List.sort String.compare in
  require "fields are missing, repeated or unknown"
    (actual = List.sort String.compare known
     || (has_version && actual = List.sort String.compare full))

let validate head =
  require "schema is unsupported" (head.schema_version >= 1 && head.schema_version <= schema_version);
  require "generation is negative" (head.generation >= 0);
  require "epoch is negative" (head.epoch_id >= 0);
  require "transaction high-water is invalid" (head.txid_hi >= -1L);
  require "state root is empty" (head.state_root <> "");
  require "commit identity is empty" (head.commit_id <> "");
  require "timestamp is invalid" (Float.is_finite head.ts && head.ts >= 0.);
  List.iter (fun (name, value) ->
    require (name ^ " is empty") (Option.fold ~none:true ~some:((<>) "") value))
    ["ledger root", head.ledger_state_root; "Irmin commit", head.irmin_commit;
     "quorum hash", head.quorum_cert_hash; "epoch hash", head.epoch_index_hash;
     "epoch root", head.epoch_index_root];
  List.iter (fun (name, value) ->
    require (name ^ " is negative") (Option.fold ~none:true ~some:(fun value -> value >= 0) value))
    ["segment", head.txlog_seg; "transaction offset", head.txlog_off; "epoch offset", head.epochlog_off];
  require "transaction position is incomplete" (Option.is_some head.txlog_seg = Option.is_some head.txlog_off);
  require "epoch commitment is incomplete" (Option.is_some head.epoch_index_hash = Option.is_some head.epoch_index_root);
  head

let of_json bytes =
  let json = Yojson.Safe.from_string bytes in
  let rows = match json with `Assoc rows -> rows | _ -> failwith "HEAD is not an object" in
  let open Yojson.Safe.Util in
  let version = match List.assoc_opt "schema_version" rows with
    | None -> 1
    | Some value -> to_int value in
  fields version (List.mem_assoc "schema_version" rows) rows;
  let field name = member name json in
  validate {
    schema_version = version;
    generation = field "generation" |> to_int;
    epoch_id = field "epoch_id" |> to_int;
    state_root = field "state_root" |> to_string;
    ledger_state_root = field "ledger_state_root" |> opt_str_of_json;
    irmin_commit = field "irmin_commit" |> opt_str_of_json;
    txid_hi = field "txid_hi" |> to_string |> Int64.of_string;
    txlog_seg = field "txlog_seg" |> opt_int_of_json;
    txlog_off = field "txlog_off" |> opt_int_of_json;
    epochlog_off = field "epochlog_off" |> opt_int_of_json;
    commit_id = field "commit_id" |> to_string;
    ts = field "ts" |> to_number;
    quorum_cert_hash = field "quorum_cert_hash" |> opt_str_of_json;
    epoch_index_hash = field "epoch_index_hash" |> opt_str_of_json;
    epoch_index_root = field "epoch_index_root" |> opt_str_of_json;
  }