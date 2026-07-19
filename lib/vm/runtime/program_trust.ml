(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


type t = Program_attestation.key list

type error =
  | Invalid_entry
  | Invalid_key_id
  | Invalid_public_key
  | Duplicate_key_id

let error_message = function
  | Invalid_entry -> "invalid program trust anchor entry"
  | Invalid_key_id -> "invalid program trust anchor key id"
  | Invalid_public_key -> "invalid program trust anchor public key"
  | Duplicate_key_id -> "duplicate program trust anchor key id"

let key_id_valid value =
  let length = String.length value in
  length > 0
  && length <= 64
  && String.for_all
       (fun ch -> Char.code ch >= 33 && Char.code ch <= 126)
       value

let entry raw =
  match String.index_opt raw '=' with
  | None -> Error Invalid_entry
  | Some pos ->
    let id = String.trim (String.sub raw 0 pos) in
    let encoded = String.trim (String.sub raw (pos + 1) (String.length raw - pos - 1)) in
    if not (key_id_valid id) then Error Invalid_key_id
    else
      match Base64.decode encoded with
      | Error _ -> Error Invalid_public_key
      | Ok public_key when String.length public_key <> 32 -> Error Invalid_public_key
      | Ok public_key ->
        (match Mirage_crypto_ec.Ed25519.pub_of_octets public_key with
         | Ok _ -> Ok { Program_attestation.id; public_key }
         | Error _ -> Error Invalid_public_key)

let of_env getenv =
  match getenv "OCTRA_PROGRAM_TRUST_KEYS" with
  | None -> Ok []
  | Some raw when String.trim raw = "" -> Ok []
  | Some raw ->
    let rec read seen acc = function
      | [] -> Ok (List.rev acc)
      | item :: rest ->
        (match entry item with
         | Error error -> Error error
         | Ok key when List.mem key.Program_attestation.id seen ->
           Error Duplicate_key_id
         | Ok key -> read (key.id :: seen) (key :: acc) rest)
    in
    read [] [] (String.split_on_char ',' raw)

let keys value = value

let empty = []