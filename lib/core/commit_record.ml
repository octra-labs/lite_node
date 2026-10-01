(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type record =
  | Prepare of {
      commit_id : string;
      prev_generation : int;
      epoch_id : int;
      planned_txid_hi : int64;
      planned_state_root : string;
      ts : float;
    }
  | Commit of {
      commit_id : string;
      generation : int;
      ts : float;
    }
  | Abort of {
      commit_id : string;
      reason : string;
      ts : float;
    }

let record_to_json = function
  | Prepare p ->
    `Assoc [
      "type", `String "PREPARE";
      "commit_id", `String p.commit_id;
      "prev_generation", `Int p.prev_generation;
      "epoch_id", `Int p.epoch_id;
      "planned_txid_hi", `String (Int64.to_string p.planned_txid_hi);
      "planned_state_root", `String p.planned_state_root;
      "ts", `Float p.ts;
    ]
  | Commit c ->
    `Assoc [
      "type", `String "COMMIT";
      "commit_id", `String c.commit_id;
      "generation", `Int c.generation;
      "ts", `Float c.ts;
    ]
  | Abort a ->
    `Assoc [
      "type", `String "ABORT";
      "commit_id", `String a.commit_id;
      "reason", `String a.reason;
      "ts", `Float a.ts;
    ]

let invalid reason = failwith ("commit record " ^ reason)

let object_rows = function
  | `Assoc rows -> rows
  | _ -> invalid "is not an object"

let schema rows expected =
  let actual = List.map fst rows |> List.sort String.compare in
  if actual <> List.sort String.compare expected then
    invalid "fields are missing, repeated or unknown"

let parse json =
  let open Yojson.Safe.Util in
  let rows = object_rows json in
  let field name = member name json in
  let text name = field name |> to_string in
  let integer name = field name |> to_int in
  let commit_id = text "commit_id" in
  if commit_id = "" then invalid "identity is empty";
  let ts = field "ts" |> to_number in
  if not (Float.is_finite ts) || ts < 0. then invalid "timestamp is invalid";
  match text "type" with
  | "PREPARE" ->
    schema rows ["type"; "commit_id"; "prev_generation"; "epoch_id";
      "planned_txid_hi"; "planned_state_root"; "ts"];
    let prev_generation = integer "prev_generation" in
    let epoch_id = integer "epoch_id" in
    let planned_txid_hi = text "planned_txid_hi" |> Int64.of_string in
    let planned_state_root = text "planned_state_root" in
    if prev_generation < -1 || epoch_id < 0 || planned_txid_hi < -1L
       || planned_state_root = "" then invalid "prepare values are invalid";
    Prepare {commit_id; prev_generation; epoch_id; planned_txid_hi; planned_state_root; ts}
  | "COMMIT" ->
    schema rows ["type"; "commit_id"; "generation"; "ts"];
    let generation = integer "generation" in
    if generation < 0 then invalid "generation is invalid";
    Commit {commit_id; generation; ts}
  | "ABORT" ->
    schema rows ["type"; "commit_id"; "reason"; "ts"];
    Abort {commit_id; reason = text "reason"; ts}
  | _ -> invalid "type is unknown"

let decode json =
  try Ok (parse json) with
  | (Failure _ | Invalid_argument _ | Yojson.Safe.Util.Type_error _) as error ->
    Error (Printexc.to_string error)

let record_of_json json =
  match decode json with
  | Ok row -> Some row
  | Error reason -> failwith reason