(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type part = {
  hash : string;
  index : int;
  count : int;
  data : string;
}

type view =
  | Full of Yojson.Safe.t
  | Part of part

let version = "octra-range-part-v1"
let body_max = 4_500_000
let raw_max = 3_000_000
let full_max = 4 * Octra_net.P2p_frame.catchup_payload_max

let hash raw =
  raw
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex

let valid_hash value =
  String.length value = 64
  && String.for_all
       (function
         | '0' .. '9'
         | 'a' .. 'f' -> true
         | _ -> false)
       value

let count length =
  (length + raw_max - 1) / raw_max

let slice raw index =
  let offset = index * raw_max in
  let length = min raw_max (String.length raw - offset) in
  String.sub raw offset length

let reply ?index json =
  let raw = Yojson.Safe.to_string json in
  let length = String.length raw in
  if length > full_max then Error "range response exceeds full limit"
  else if length <= body_max then
    match index with
    | None -> Ok json
    | Some _ -> Error "range response has no parts"
  else
    let count = count length in
    let index = Option.value ~default:0 index in
    if index < 0 || index >= count then Error "range part index is invalid"
    else
      Ok
        (`Assoc [
           "version", `String version;
           "status", `String "part";
           "sha256", `String (hash raw);
           "index", `Int index;
           "count", `Int count;
           "data", `String (Base64.encode_exn (slice raw index));
         ])

let exact_fields fields =
  List.length fields = 6
  && List.sort String.compare (List.map fst fields)
     = ["count"; "data"; "index"; "sha256"; "status"; "version"]

let exact_data data =
  match Base64.decode data with
  | Error _ -> Error "range part data is not base64"
  | Ok raw when Base64.encode_exn raw <> data ->
    Error "range part data is not exact"
  | Ok raw -> Ok raw

let view json =
  match json with
  | `Assoc fields when List.assoc_opt "status" fields = Some (`String "part") ->
    if not (exact_fields fields) then Error "range part fields are invalid"
    else
      begin
        match List.assoc_opt "version" fields,
              List.assoc_opt "sha256" fields,
              List.assoc_opt "index" fields,
              List.assoc_opt "count" fields,
              List.assoc_opt "data" fields with
        | Some (`String wire), Some (`String hash), Some (`Int index),
          Some (`Int count), Some (`String data)
          when String.equal wire version
               && valid_hash hash
               && count > 1
               && index >= 0
               && index < count ->
          exact_data data
          |> Result.map (fun _ -> Part { hash; index; count; data })
        | _ -> Error "range part values are invalid"
      end
  | _ -> Ok (Full json)

let join parts =
  match parts with
  | [] -> Error "range parts are empty"
  | first :: _ when List.length parts <> first.count ->
    Error "range part count does not match"
  | first :: _ ->
    let buffer = Buffer.create (min full_max (first.count * raw_max)) in
    let rec add index = function
      | [] -> Ok ()
      | part :: rest ->
        if part.index <> index
           || part.count <> first.count
           || not (String.equal part.hash first.hash) then
          Error "range part sequence does not match"
        else
          Result.bind (exact_data part.data) (fun raw ->
            let last = index = first.count - 1 in
            let length = String.length raw in
            if length <= 0
               || length > raw_max
               || (not last && length <> raw_max)
               || length > full_max - Buffer.length buffer then
              Error "range part size is invalid"
            else begin
              Buffer.add_string buffer raw;
              add (index + 1) rest
            end)
    in
    Result.bind (add 0 parts) (fun () ->
      let raw = Buffer.contents buffer in
      if String.length raw <= body_max then
        Error "range parts encode an inline response"
      else if not (String.equal (hash raw) first.hash) then
        Error "range part hash does not match"
      else
        try
          let json = Yojson.Safe.from_string raw in
          match view json with
          | Ok (Full _) -> Ok json
          | Ok (Part _) -> Error "range parts contain a nested part"
          | Error _ as error -> error
        with _ -> Error "range parts do not contain JSON")