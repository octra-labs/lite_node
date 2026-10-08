(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type request =
  | Read_key of bool * string
  | Read_cipher of bool * bool * string
  | Add of string * string * string
  | Sub of string * string * string
  | Mul of bool * int * string * string * string * string
  | Scale of bool * string * string * int64
  | Divide of string * string * int64
  | Add_int of bool * string * string * int64
  | Sub_int of bool * string * string * int64
  | Commit of string * string

type value = Key of Fhe_image.key | Cipher of Fhe_image.cipher | Digest of string
type error = Invalid | Memory | Stack

let magic = "octra_fhe_v1\000"
let max_bytes = 536_870_912

let encode fields =
  if List.length fields > 8 then invalid_arg "fhe field count";
  let size = List.fold_left (fun size field ->
    let length = String.length field in
    if length > max_bytes - size - 8 then raise Out_of_memory;
    size + 8 + length) (String.length magic + 1) fields in
  let bytes = Bytes.create size in
  Bytes.blit_string magic 0 bytes 0 (String.length magic);
  Bytes.set bytes (String.length magic) (Char.chr (List.length fields));
  let _ = List.fold_left (fun offset field ->
    let length = String.length field in
    Bytes.set_int64_be bytes offset (Int64.of_int length);
    Bytes.blit_string field 0 bytes (offset + 8) length;
    offset + 8 + length) (String.length magic + 1) fields in
  Bytes.unsafe_to_string bytes

let decode raw =
  let size = String.length raw in
  let start = String.length magic in
  if size > max_bytes || size <= start || not (String.starts_with ~prefix:magic raw) then
    Error "fhe message header"
  else
    let count = Char.code raw.[start] in
    let rec fields count offset values =
      if count = 0 then
        if offset = size then Ok (List.rev values) else Error "fhe extra bytes"
      else if size - offset < 8 then Error "fhe field length"
      else
        let length = String.get_int64_be raw offset in
        let offset = offset + 8 in
        if length < 0L || length > Int64.of_int (size - offset) then
          Error "fhe field size"
        else
          let length = Int64.to_int length in
          fields (count - 1) (offset + length) (String.sub raw offset length :: values)
    in
    if count > 8 then Error "fhe field count" else fields count (start + 1) []

let flag value = if value then "1" else "0"

let request_bytes = function
  | Read_key (registry, raw) -> encode ["read_key"; flag registry; raw]
  | Read_cipher (strict, cap, raw) -> encode ["read_cipher"; flag strict; flag cap; raw]
  | Add (key, left, right) -> encode ["add"; key; left; right]
  | Sub (key, left, right) -> encode ["sub"; key; left; right]
  | Mul (math, draws, key, left, right, seed) ->
    encode ["mul"; flag math; string_of_int draws; key; left; right; seed]
  | Scale (math, key, cipher, value) ->
    encode ["scale"; flag math; key; cipher; Int64.to_string value]
  | Divide (key, cipher, value) -> encode ["divide"; key; cipher; Int64.to_string value]
  | Add_int (math, key, cipher, value) ->
    encode ["add_int"; flag math; key; cipher; Int64.to_string value]
  | Sub_int (math, key, cipher, value) ->
    encode ["sub_int"; flag math; key; cipher; Int64.to_string value]
  | Commit (key, cipher) -> encode ["commit"; key; cipher]

let request_of_bytes raw =
  let boolean = function "0" -> false | "1" -> true | _ -> invalid_arg "fhe flag" in
  Result.bind (decode raw) (fun fields ->
    try
      let value = match fields with
        | ["read_key"; registry; raw] -> Read_key (boolean registry, raw)
        | ["read_cipher"; strict; cap; raw] -> Read_cipher (boolean strict, boolean cap, raw)
        | ["add"; key; left; right] -> Add (key, left, right)
        | ["sub"; key; left; right] -> Sub (key, left, right)
        | ["mul"; math; draws; key; left; right; seed] ->
          let draws = int_of_string draws in
          if draws < 0 || draws > 1024 then invalid_arg "fhe sample count";
          Mul (boolean math, draws, key, left, right, seed)
        | ["scale"; math; key; cipher; value] ->
          Scale (boolean math, key, cipher, Int64.of_string value)
        | ["divide"; key; cipher; value] -> Divide (key, cipher, Int64.of_string value)
        | ["add_int"; math; key; cipher; value] ->
          Add_int (boolean math, key, cipher, Int64.of_string value)
        | ["sub_int"; math; key; cipher; value] ->
          Sub_int (boolean math, key, cipher, Int64.of_string value)
        | ["commit"; key; cipher] -> Commit (key, cipher)
        | _ -> invalid_arg "fhe request" in
      Ok value
    with Invalid_argument _ | Failure _ -> Error "fhe request")

let hash raw = Digestif.SHA256.(digest_string raw |> to_hex)

let map_key apply = function
  | Add (key, left, right) -> Add (apply key, left, right)
  | Sub (key, left, right) -> Sub (apply key, left, right)
  | Mul (math, draws, key, left, right, seed) -> Mul (math, draws, apply key, left, right, seed)
  | Scale (math, key, cipher, value) -> Scale (math, apply key, cipher, value)
  | Divide (key, cipher, value) -> Divide (apply key, cipher, value)
  | Add_int (math, key, cipher, value) -> Add_int (math, apply key, cipher, value)
  | Sub_int (math, key, cipher, value) -> Sub_int (math, apply key, cipher, value)
  | Commit (key, cipher) -> Commit (apply key, cipher)
  | (Read_key _ | Read_cipher _) as request -> request

let key_ref raw = "\000octra_key:" ^ hash raw

let response_bytes id = function
  | Ok (Key key) -> encode [id; "key"; key.data; Fhe_image.key_meta key]
  | Ok (Cipher cipher) -> encode [id; "cipher"; cipher.data; Fhe_image.cipher_meta cipher]
  | Ok (Digest bytes) -> encode [id; "digest"; bytes]
  | Error Invalid -> encode [id; "invalid"]
  | Error Memory -> encode [id; "memory"]
  | Error Stack -> encode [id; "stack"]

let response_of_bytes id raw =
  Result.bind (decode raw) (function
    | [found; "key"; bytes; meta] when found = id ->
      Result.map (fun key -> Ok (Key key)) (Fhe_image.key_of_meta bytes meta)
    | [found; "cipher"; bytes; meta] when found = id ->
      Result.map (fun cipher -> Ok (Cipher cipher)) (Fhe_image.cipher_of_meta bytes meta)
    | [found; "digest"; bytes] when found = id -> Ok (Ok (Digest bytes))
    | [found; "invalid"] when found = id -> Ok (Error Invalid)
    | [found; "memory"] when found = id -> Ok (Error Memory)
    | [found; "stack"] when found = id -> Ok (Error Stack)
    | _ -> Error "fhe response")

let eval ?(key = fun bytes -> Pvac_ffi.deserialize_pubkey (Bytes.of_string bytes))
    ?(cipher = fun bytes -> Pvac_ffi.deserialize_cipher ~strict:false ~cap:false (Bytes.of_string bytes))
    ?(of_key = Fhe_image.of_key) ?(of_cipher = Fhe_image.of_cipher) request =
  let module P = Pvac_ffi in
  let compute = match request with
    | Read_key (registry, bytes) ->
      fun () -> `Key (if registry then
        match Pvac_registry.load_pubkey bytes with
        | Ok key -> key
        | Error reason -> failwith reason
      else P.deserialize_pubkey (Bytes.of_string bytes))
    | Read_cipher (strict, cap, bytes) ->
      fun () -> `Cipher (P.deserialize_cipher ~strict ~cap (Bytes.of_string bytes))
    | Add (pk, left, right) ->
      let pk, left, right = key pk, cipher left, cipher right in
      fun () -> `Cipher (P.ct_add pk left right)
    | Sub (pk, left, right) ->
      let pk, left, right = key pk, cipher left, cipher right in
      fun () -> `Cipher (P.ct_sub pk left right)
    | Mul (math, draws, pk, left, right, seed) ->
      let pk, left, right = key pk, cipher left, cipher right in
      let seed = Bytes.of_string seed in
      fun () -> `Cipher (if draws = 0 then P.ct_mul_seeded ~math pk left right seed
        else P.ct_mul_work (math, draws) pk left right seed)
    | Scale (math, pk, ct, value) ->
      let pk, ct = key pk, cipher ct in
      fun () -> `Cipher (P.ct_scale ~math pk ct value)
    | Divide (pk, ct, value) ->
      let pk, ct = key pk, cipher ct in
      fun () -> `Cipher (P.ct_div_const pk ct value 0L)
    | Add_int (math, pk, ct, value) ->
      let pk, ct = key pk, cipher ct in
      let lo, hi = if math && value < 0L then Int64.pred value, Int64.max_int else value, 0L in
      fun () -> `Cipher (P.ct_add_const ~math pk ct lo hi)
    | Sub_int (math, pk, ct, value) ->
      let pk, ct = key pk, cipher ct in
      fun () -> `Cipher (if math && value < 0L then
        P.ct_add_const ~math:true pk ct (Int64.neg value) 0L
        else P.ct_sub_const ~math pk ct value)
    | Commit (pk, ct) ->
      let pk, ct = key pk, cipher ct in
      fun () -> `Digest (P.commit_ct pk ct)
  in
  let result = try Ok (compute ()) with
    | Out_of_memory -> Error Memory
    | Stack_overflow -> Error Stack
    | Invalid_argument _ | Failure _ -> Error Invalid in
  Result.map (function
    | `Key key -> Key (of_key key)
    | `Cipher cipher -> Cipher (of_cipher cipher)
    | `Digest bytes -> Digest (Bytes.to_string bytes)) result

let read_request () =
  let bytes = Bytes.create 65_536 in
  let buffer = Buffer.create 65_536 in
  let rec read () =
    let count = input stdin bytes 0 (Bytes.length bytes) in
    if count > max_bytes - Buffer.length buffer then failwith "fhe input size";
    if count > 0 then begin
      Buffer.add_subbytes buffer bytes 0 count;
      read ()
    end in
  read ();
  Buffer.contents buffer

let serve () =
  let raw = read_request () in
  match request_of_bytes raw with
  | Error reason -> failwith reason
  | Ok request ->
    let response = response_bytes (hash raw) (eval request) in
    output_string stdout response;
    flush stdout

let read_frame () =
  let header = really_input_string stdin 8 in
  let length = String.get_int64_be header 0 in
  if length <= 0L || length > Int64.of_int max_bytes then failwith "fhe frame size";
  really_input_string stdin (Int64.to_int length)

let write_frame raw =
  let header = Bytes.create 8 in
  Bytes.set_int64_be header 0 (Int64.of_int (String.length raw));
  output_bytes stdout header;
  output_string stdout raw;
  flush stdout

let serve_session () =
  let saved = ref None in
  let key bytes =
    match !saved with
    | Some (raw, reference, key) when raw = bytes || reference = bytes -> key
    | _ ->
      saved := None;
      Gc.full_major ();
      let key = Pvac_ffi.deserialize_pubkey (Bytes.of_string bytes) in
      if Pvac_ffi.pubkey_image_size key <= 67_108_864 then
        saved := Some (bytes, key_ref bytes, key);
      key in
  let rec loop () =
    let raw = read_frame () in
    match request_of_bytes raw with
    | Error reason -> failwith reason
    | Ok request ->
      let result = try eval ~key request with
        | Out_of_memory -> Error Memory
        | Stack_overflow -> Error Stack
        | Invalid_argument _ | Failure _ -> Error Invalid in
      write_frame (response_bytes (hash raw) result);
      loop () in
  try loop () with End_of_file -> ()