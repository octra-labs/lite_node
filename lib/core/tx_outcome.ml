(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type rejection = {
  position : int;
  tx : Transaction.t;
  error_type : string;
  reason : string;
}

type partition = {
  preverify : string list;
  rejections : rejection list;
}

type item =
  | Preverify of string
  | Rejection of rejection

let schema = "octra_epoch_rejection"
let max_rejections = 2_048
let max_error_type_bytes = 128
let max_reason_bytes = 512

let text_ok max_bytes value =
  let length = String.length value in
  length > 0
  && length <= max_bytes
  && String.for_all
       (fun char ->
          let code = Char.code char in
          code >= 0x20 && code <= 0x7e)
       value

let digest value =
  Digestif.SHA256.(digest_string value |> to_hex)

let printable value =
  String.map
    (fun char ->
       let code = Char.code char in
       if code >= 0x20 && code <= 0x7e then char else '?')
    value

let normalize_text ~default ~max_bytes value =
  let safe = printable value in
  if value <> "" && value = safe && String.length value <= max_bytes then
    value
  else
    let suffix = " [sha256=" ^ digest value ^ "]" in
    let source = if safe = "" then default else safe in
    let available = max 0 (max_bytes - String.length suffix) in
    let length = min available (String.length source) in
    String.sub source 0 length ^ suffix

let normalize_error_type value =
  normalize_text
    ~default:"execution_failed"
    ~max_bytes:max_error_type_bytes
    value

let normalize_reason ~error_type value =
  if String.ends_with ~suffix:"_exception" error_type then
    "execution exception"
  else
    normalize_text
      ~default:"execution rejected"
      ~max_bytes:max_reason_bytes
      value

let encode_rejection rejection =
  `Assoc [
    "schema", `String schema;
    "position", `Int rejection.position;
    "hash", `String (Transaction.hash rejection.tx);
    "tx", Transaction.to_yojson rejection.tx;
    "error_type", `String rejection.error_type;
    "reason", `String rejection.reason;
  ]
  |> Yojson.Safe.to_string

let assoc name fields =
  List.assoc_opt name fields

let parse_rejection raw fields =
  match
    assoc "position" fields,
    assoc "hash" fields,
    assoc "tx" fields,
    assoc "error_type" fields,
    assoc "reason" fields
  with
  | Some (`Int position), Some (`String hash), Some tx_json,
    Some (`String error_type), Some (`String reason) ->
    begin
      match Tx_payload.decode tx_json with
      | Error error -> Error ("rejection_tx_invalid:" ^ error)
      | Ok tx ->
        let rejection = { position; tx; error_type; reason } in
        if position < 0 || position >= max_rejections then
          Error "rejection_position_invalid"
        else if Transaction.hash tx <> hash then
          Error "rejection_hash_mismatch"
        else if not (text_ok max_error_type_bytes error_type) then
          Error "rejection_error_type_invalid"
        else if not (text_ok max_reason_bytes reason) then
          Error "rejection_reason_invalid"
        else if encode_rejection rejection <> raw then
          Error "rejection_encoding_mismatch"
        else
          Ok (Rejection rejection)
    end
  | _ -> Error "rejection_fields_invalid"

let parse raw =
  try
    match Yojson.Safe.from_string raw with
    | `Assoc fields ->
      begin
        match assoc "schema" fields with
        | Some (`String value) when value = schema ->
          parse_rejection raw fields
        | _ -> Ok (Preverify raw)
      end
    | _ -> Ok (Preverify raw)
  with exn ->
    Error ("outcome_json_invalid:" ^ Printexc.to_string exn)

let strictly_ordered rejections =
  let rec loop prior = function
    | [] -> true
    | rejection :: rest ->
      rejection.position > prior
      && loop rejection.position rest
  in
  loop (-1) rejections

let distinct_hashes rejections =
  let hashes = List.map (fun rejection -> Transaction.hash rejection.tx) rejections in
  List.length hashes = List.length (List.sort_uniq String.compare hashes)

let split raws =
  let rec loop seen_rejection preverify rejections = function
    | [] ->
      let rejections = List.rev rejections in
      if List.length rejections > max_rejections then
        Error "rejection_count_exceeded"
      else if not (strictly_ordered rejections) then
        Error "rejection_positions_not_ordered"
      else if not (distinct_hashes rejections) then
        Error "rejection_hash_duplicate"
      else
        Ok {
          preverify = List.rev preverify;
          rejections;
        }
    | raw :: rest ->
      begin
        match parse raw with
        | Error _ as error -> error
        | Ok (Preverify _) when seen_rejection ->
          Error "preverify_after_rejection"
        | Ok (Preverify value) ->
          loop false (value :: preverify) rejections rest
        | Ok (Rejection rejection) ->
          loop true preverify (rejection :: rejections) rest
      end
  in
  loop false [] [] raws

let find_position hash txs =
  let rec loop position = function
    | [] -> None
    | tx :: rest ->
      if Transaction.hash tx = hash then Some position
      else loop (position + 1) rest
  in
  loop 0 txs

let build ~candidates rejected =
  let candidates = Transaction.consensus_order candidates in
  let candidate_hashes = List.map Transaction.hash candidates in
  if
    List.length candidate_hashes
    <> List.length (List.sort_uniq String.compare candidate_hashes)
  then
    Error "candidate_hash_duplicate"
  else
    let rec loop acc = function
      | [] ->
        let rejections =
          List.sort
            (fun left right -> compare left.position right.position)
            acc
        in
        if distinct_hashes rejections then Ok rejections
        else Error "rejection_hash_duplicate"
      | (tx, error_type, reason) :: rest ->
        let hash = Transaction.hash tx in
        begin
          match find_position hash candidates with
          | None -> Error "rejection_not_in_candidates"
          | Some position ->
            let reason = normalize_reason ~error_type reason in
            let error_type = normalize_error_type error_type in
            let rejection = { position; tx; error_type; reason } in
            loop (rejection :: acc) rest
        end
    in
    loop [] rejected

let merge ~confirmed ~rejections =
  let total = List.length confirmed + List.length rejections in
  if total > max_rejections then
    Error "outcome_count_exceeded"
  else
    let rec loop position confirmed rejections acc =
      if position = total then
        match confirmed, rejections with
        | [], [] ->
          let candidates = List.rev acc in
          if Transaction.consensus_order candidates <> candidates then
            Error "outcome_order_invalid"
          else
            let hashes = List.map Transaction.hash candidates in
            if List.length hashes <> List.length (List.sort_uniq String.compare hashes) then
              Error "outcome_hash_duplicate"
            else
              Ok candidates
        | _ -> Error "outcome_partition_incomplete"
      else
        match rejections with
        | rejection :: rest when rejection.position = position ->
          loop (position + 1) confirmed rest (rejection.tx :: acc)
        | rejection :: _ when rejection.position < position ->
          Error "rejection_position_duplicate"
        | _ ->
          begin
            match confirmed with
            | [] -> Error "rejection_position_missing"
            | tx :: rest ->
              loop (position + 1) rest rejections (tx :: acc)
          end
    in
    if not (strictly_ordered rejections) then
      Error "rejection_positions_not_ordered"
    else
      loop 0 confirmed rejections []

let decode ~confirmed raws =
  match split raws with
  | Error _ as error -> error
  | Ok partition ->
    match merge ~confirmed ~rejections:partition.rejections with
    | Error _ as error -> error
    | Ok _ -> Ok partition

let encode preverify rejections =
  preverify @ List.map encode_rejection rejections

let equal left right =
  List.map encode_rejection left = List.map encode_rejection right