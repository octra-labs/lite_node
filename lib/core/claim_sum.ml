(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Ids = Set.Make (Int)
module Hashes = Set.Make (String)
module P = Pvac_ffi

type change =
  | Keep
  | Deposit of Z.t
  | Withdraw of Z.t
  | Send of string
  | Claim of Claim_history.checked

type t = {
  address : string;
  point : bytes;
  ids : Ids.t;
  hashes : Hashes.t;
  records : int;
  changes : int;
}

type summary = {
  commitment : string;
  records : int;
  changes : int;
  received : int;
}

let ( let* ) = Result.bind
let require valid reason = if valid then Ok () else Error reason

let create ~address = {
  address; point = P.pedersen_identity (); ids = Ids.empty;
  hashes = Hashes.empty; records = 0; changes = 0;
}

let public_point amount =
  let* () = require
    (Z.sign amount >= 0 && Z.leq amount Denomination.max_supply
     && Z.leq amount (Z.of_int64 Int64.max_int))
    "history public amount exceeds supply range" in
  Ok (P.pedersen_commit_amount (Z.to_int64 amount) (Bytes.make 32 '\000'))

let private_point raw =
  let bytes = Bytes.of_string (Base64.decode_exn raw) in
  let* () = require (Bytes.length bytes = 32) "history amount point length invalid" in
  Ok (P.pedersen_add (P.pedersen_identity ()) bytes)

let apply value ~hash change =
  try
    let* () = require (not (Hashes.mem hash value.hashes))
      "history transaction repeated" in
    let* () = require (value.records < max_int) "history record count overflows" in
    let* point, ids = match change with
      | Keep -> Ok (value.point, value.ids)
      | Deposit amount ->
        let* point = public_point amount in
        Ok (P.pedersen_add value.point point, value.ids)
      | Withdraw amount ->
        let* point = public_point amount in
        Ok (P.pedersen_sub value.point point, value.ids)
      | Send raw ->
        let* point = private_point raw in
        Ok (P.pedersen_sub value.point point, value.ids)
      | Claim receipt ->
        let* () = require (receipt.Claim_history.receiver = value.address)
          "history claim belongs to another account" in
        let* () = require (not (Ids.mem receipt.id value.ids))
          "history output credited twice" in
        let* () = require (hash = receipt.claim_hash)
          "history claim transaction differs" in
        let* point = private_point receipt.commitment in
        Ok (P.pedersen_add value.point point, Ids.add receipt.id value.ids) in
    Ok {value with point; ids; hashes = Hashes.add hash value.hashes;
      records = value.records + 1;
      changes = value.changes + (match change with Keep -> 0 | _ -> 1)}
  with exn -> Error ("history accounting failed: " ^ Printexc.to_string exn)

let finish (value : t) = {
  commitment = Base64.encode_exn (Bytes.to_string value.point);
  records = value.records; changes = value.changes; received = Ids.cardinal value.ids;
}