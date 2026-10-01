(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type key =
  | Receipt of string
  | Rejected of string
  | Address of string * string
  | Epoch of int * string
  | Metadata of string

type cell = Value of string option | Member of bool
type anchor = { epoch : int; root : string; commit_id : string }
type row = { key : key; prior : cell; next : cell }
type t = { previous : anchor option; target : anchor; rows : row list }
type decision = Restore | Retire

let ( let* ) = Result.bind
let require reason check = if check then Ok () else Error reason

let valid_anchor value =
  value.epoch >= 0 && value.root <> "" && value.commit_id <> ""

let valid_cell = function
  | (Receipt _ | Rejected _ | Metadata _), Value _ -> true
  | (Address _ | Epoch _), Member _ -> true
  | _ -> false

let valid_key = function
  | Receipt key | Rejected key | Metadata key -> key <> ""
  | Address (key, value) -> key <> "" && value <> ""
  | Epoch (epoch, key) -> epoch >= 0 && Int64.of_int epoch <= Int64.of_int32 Int32.max_int && key <> ""

let ordered rows = List.sort (fun (left, _) (right, _) -> compare left right) rows

let unique rows =
  let rec loop = function
    | (left, _) :: ((right, _) :: _ as rest) -> left <> right && loop rest
    | _ -> true in
  loop rows

let seal ~previous ~target ~before ~after =
  let* () = require "auxiliary target anchor is invalid" (valid_anchor target) in
  let* () = require "auxiliary predecessor anchor is invalid"
    (match previous with None -> target.epoch = 0
     | Some head -> valid_anchor head && head.epoch < max_int && target.epoch = head.epoch + 1) in
  let before, after = ordered before, ordered after in
  let* () = require "auxiliary write keys repeat" (unique before && unique after) in
  let* () = require "auxiliary write keys differ" (List.map fst before = List.map fst after) in
  let* () = require "auxiliary write key is invalid" (List.for_all (fun (key, _) -> valid_key key) before) in
  let* () = require "auxiliary cell type differs"
    (List.for_all valid_cell before && List.for_all valid_cell after) in
  let rows = List.map2 (fun (key, prior) (_, next) -> {key; prior; next}) before after
    |> List.filter (fun row -> row.prior <> row.next) in
  Ok {previous; target; rows}

let decide ~head journal =
  if head = Some journal.target then Ok Retire
  else if head = journal.previous then Ok Restore
  else Error "auxiliary journal does not match selected HEAD"

let restore ~head ~current journal =
  let* decision = decide ~head journal in
  let* () = require "committed auxiliary journal cannot be reversed" (decision = Restore) in
  let expected = List.map (fun row -> row.key, row.next) journal.rows in
  let* () = require "auxiliary values differ from journal" (ordered current = expected) in
  Ok (List.map (fun row -> row.key, row.prior) journal.rows)

let anchor_json value = `List [`Int value.epoch; `String value.root; `String value.commit_id]

let key_json = function
  | Receipt hash -> `List [`String "receipt"; `String hash]
  | Rejected hash -> `List [`String "rejected"; `String hash]
  | Address (address, value) -> `List [`String "address"; `String address; `String value]
  | Epoch (epoch, hash) -> `List [`String "epoch"; `Int epoch; `String hash]
  | Metadata key -> `List [`String "metadata"; `String key]

let cell_json = function
  | Value None -> `Null
  | Value (Some value) -> `String (Base64.encode_string value)
  | Member value -> `Bool value

let payload_json value = `List [
  `String "octra_aux_delta";
  (match value.previous with None -> `Null | Some head -> anchor_json head);
  anchor_json value.target;
  `List (List.map (fun row -> `List [key_json row.key; cell_json row.prior; cell_json row.next]) value.rows)]

let checksum payload =
  Digestif.SHA256.(digest_string ("octra_aux_delta\000" ^ Yojson.Safe.to_string payload) |> to_hex)

let encode value =
  let payload = payload_json value in
  Yojson.Safe.to_string (`List [payload; `String (checksum payload)])

let anchor_of_json = function
  | `List [`Int epoch; `String root; `String commit_id] -> Ok {epoch; root; commit_id}
  | _ -> Error "auxiliary anchor encoding is invalid"

let key_of_json = function
  | `List [`String "receipt"; `String hash] -> Ok (Receipt hash)
  | `List [`String "rejected"; `String hash] -> Ok (Rejected hash)
  | `List [`String "address"; `String address; `String value] -> Ok (Address (address, value))
  | `List [`String "epoch"; `Int epoch; `String hash] when epoch >= 0 -> Ok (Epoch (epoch, hash))
  | `List [`String "metadata"; `String key] -> Ok (Metadata key)
  | _ -> Error "auxiliary key encoding is invalid"

let cell_of_json = function
  | `Null -> Ok (Value None)
  | `String bytes -> (match Base64.decode bytes with
    | Ok value when Base64.encode_string value = bytes -> Ok (Value (Some value))
    | _ -> Error "auxiliary value encoding is invalid")
  | `Bool value -> Ok (Member value)
  | _ -> Error "auxiliary cell encoding is invalid"

let row_of_json = function
  | `List [key; prior; next] ->
    let* key = key_of_json key in
    let* prior = cell_of_json prior in
    let* next = cell_of_json next in
    Ok {key; prior; next}
  | _ -> Error "auxiliary row encoding is invalid"

let decode bytes =
  try match Yojson.Safe.from_string bytes with
  | `List [(`List [`String "octra_aux_delta"; previous; target; `List values] as payload); `String digest] ->
    let* () = require "auxiliary journal checksum differs" (checksum payload = digest) in
    let* previous = match previous with
      | `Null -> Ok None | value -> Result.map Option.some (anchor_of_json value) in
    let* target = anchor_of_json target in
    let* rows = List.fold_left (fun result value ->
      let* rows = result in let* row = row_of_json value in Ok (row :: rows)) (Ok []) values
      |> Result.map List.rev in
    let* journal = seal ~previous ~target
      ~before:(List.map (fun row -> row.key, row.prior) rows)
      ~after:(List.map (fun row -> row.key, row.next) rows) in
    let* () = require "auxiliary rows are not strictly ordered changes" (journal.rows = rows) in
    let* () = require "auxiliary journal bytes differ from writer" (encode journal = bytes) in
    Ok journal
  | _ -> Error "auxiliary journal encoding is invalid"
  with Yojson.Json_error _ -> Error "auxiliary journal JSON is invalid"