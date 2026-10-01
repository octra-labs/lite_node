(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Delta = Octra_core.Aux_delta

let need = function Ok value -> value | Error reason -> failwith reason
let check name valid =
  if not valid then failwith name;
  Printf.printf "event = delta_frame test = %s status = pass\n%!" name

let stamp payload = Digestif.SHA256.(digest_string
  ("octra_aux_delta\000" ^ Yojson.Safe.to_string payload) |> to_hex)

let pack payload = Yojson.Safe.to_string (`List [payload; `String (stamp payload)])

let () =
  let previous = Delta.{epoch = 0; root = "old-root"; commit_id = "zero"} in
  let target = Delta.{epoch = 1; root = "new-root"; commit_id = "one"} in
  let journal = need (Delta.seal ~previous:(Some previous) ~target
    ~before:[Delta.Metadata "note", Delta.Value (Some "old")]
    ~after:[Delta.Metadata "note", Delta.Value (Some "new")]) in
  let expected = {|[["octra_aux_delta",[0,"old-root","zero"],[1,"new-root","one"],[[["metadata","note"],"b2xk","bmV3"]]],"1f15ffe3a0534f2068ecddc20c9e0f3320c3641bb29157ba267cee4b4a713f86"]|} in
  let encoded = Delta.encode journal in
  check "independent_digest" (encoded = expected);
  check "roundtrip" (Delta.decode encoded = Ok journal);
  let payload, digest = match Yojson.Safe.from_string encoded with
    | `List [payload; `String digest] -> payload, digest
    | _ -> failwith "frame shape differs" in
  let refuse name bytes = check name (Result.is_error (Delta.decode bytes)) in
  refuse "no_checksum" (Yojson.Safe.to_string payload);
  refuse "wrong_checksum" (Yojson.Safe.to_string (`List [payload; `String (String.make 64 '0')]));
  refuse "uppercase_checksum" (Yojson.Safe.to_string (`List [payload; `String (String.uppercase_ascii digest)]));
  refuse "empty_checksum" (Yojson.Safe.to_string (`List [payload; `String ""]));
  refuse "null_checksum" (Yojson.Safe.to_string (`List [payload; `Null]));
  refuse "extra_field" (Yojson.Safe.to_string (`List [payload; `String digest; `Null]));
  refuse "leading_space" (" " ^ encoded);
  refuse "trailing_space" (encoded ^ " ");
  refuse "trailing_record" (encoded ^ encoded);
  for length = 0 to String.length encoded - 1 do
    if Result.is_ok (Delta.decode (String.sub encoded 0 length)) then
      failwith "truncated frame accepted"
  done;
  check "all_truncations" true;
  for index = 0 to String.length encoded - 1 do
    for bit = 0 to 7 do
      let changed = Bytes.of_string encoded in
      Bytes.set changed index (Char.chr (Char.code (Bytes.get changed index) lxor (1 lsl bit)));
      if Result.is_ok (Delta.decode (Bytes.to_string changed)) then
        failwith "single bit change accepted"
    done
  done;
  check "every_single_bit" true;
  Printf.printf "event = frame_mutations bytes = %d bit_changes = %d truncations = %d\n%!"
    (String.length encoded) (8 * String.length encoded) (String.length encoded);
  let alter_rows action = match payload with
    | `List [schema; previous; target; `List rows] ->
      pack (`List [schema; previous; target; `List (action rows)])
    | _ -> failwith "payload shape differs" in
  refuse "valid_stamp_duplicate_rows" (alter_rows (fun rows -> rows @ rows));
  refuse "valid_stamp_wrong_cell" (alter_rows (List.map (function
    | `List [key; _; next] -> `List [key; `Bool true; next]
    | _ -> failwith "row shape differs")));
  refuse "valid_stamp_equal_values" (alter_rows (List.map (function
    | `List [key; prior; _] -> `List [key; prior; prior]
    | _ -> failwith "row shape differs")));
  refuse "valid_stamp_bad_base64" (alter_rows (List.map (function
    | `List [key; _; next] -> `List [key; `String "!!!!"; next]
    | _ -> failwith "row shape differs")));
  (match payload with
  | `List [_; previous; target; rows] ->
    refuse "valid_stamp_unknown_schema" (pack (`List [`String "unknown"; previous; target; rows]))
  | _ -> failwith "payload shape differs");
  List.iter (fun length ->
    let bytes = String.init length (fun index -> Char.chr (index mod 256)) in
    let row = need (Delta.seal ~previous:(Some previous) ~target
      ~before:[Delta.Receipt "bytes", Delta.Value None]
      ~after:[Delta.Receipt "bytes", Delta.Value (Some bytes)]) in
    if Delta.decode (Delta.encode row) <> Ok row then
      failwith "arbitrary bytes roundtrip differs") [0; 1; 2; 3; 255; 256; 4096; 65536];
  check "arbitrary_byte_values" true