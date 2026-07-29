(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module FB = Crypto.FheBalance

type status = {
  has_pvac_pubkey : bool;
  pubkey_size : int option;
  canonical_binding : bool;
  pubkey_format : string option;
}

type register_decision =
  | Already_registered
  | Existing_key_requires_key_switch
  | Canonical_key_switch_required

type kat_admission =
  | Kat_ok
  | Kat_mismatch of {
      got : string;
      expected : string;
    }

let max_pubkey_bytes = 5_000_000

let expected_kat () = FB.aes_kat_hex ()

let key_hash blob =
  let hex = Digestif.SHA256.(digest_string blob |> to_hex) in
  String.sub hex 0 16

let full_key_hash blob =
  Digestif.SHA256.digest_string blob
  |> Digestif.SHA256.to_hex

let decode_b64 s =
  try Ok (Base64.decode_exn s)
  with e -> Error (Printexc.to_string e)

let validate_size raw =
  if String.length raw > max_pubkey_bytes then
    Error "pvac pubkey too large (max 5MB)"
  else
    Ok raw

let register_blob_of_b64 s =
  match decode_b64 s with
  | Error e -> Error e
  | Ok raw -> validate_size raw

let kat_admission = function
  | Some got ->
    let expected = expected_kat () in
    if got <> expected then Kat_mismatch { got; expected } else Kat_ok
  | None -> Kat_ok

let byte raw i =
  Char.code (String.get raw i)

let raw_header_ok raw =
  String.length raw >= 6
  && String.sub raw 0 4 = "PVAC"
  && byte raw 4 >= 0x01
  && byte raw 4 <= 0x05
  && byte raw 5 = 0x01

let packed_header_ok raw =
  String.length raw >= 5 && byte raw 0 = 0xEC

let validate_shape raw =
  match validate_size raw with
  | Error e -> Error e
  | Ok raw when raw_header_ok raw || packed_header_ok raw -> Ok raw
  | Ok _ -> Error "pvac pubkey has invalid envelope"

let load_pubkey raw =
  match validate_shape raw with
  | Error e -> Error e
  | Ok raw ->
    match FB.load_pubkey_result raw with
    | Error e -> Error e
    | Ok pk -> Ok pk

let canonicalize_blob raw =
  match load_pubkey raw with
  | Error e -> Error e
  | Ok pk -> Ok (Bytes.to_string (Pvac_ffi.serialize_pubkey pk))

let status_of_blob ~canonical_binding = function
  | None ->
    {
      has_pvac_pubkey = false;
      pubkey_size = None;
      canonical_binding = false;
      pubkey_format = None;
    }
  | Some blob ->
    let pubkey_format =
      if String.length blob > 0 && Char.code (String.get blob 0) = 0xEC then
        Some "compressed"
      else
        Some "raw"
    in
    {
      has_pvac_pubkey = true;
      pubkey_size = Some (String.length blob);
      canonical_binding;
      pubkey_format;
    }

let register_decision ~existing ~incoming =
  match existing with
  | Some blob when blob = incoming -> Already_registered
  | Some _ -> Existing_key_requires_key_switch
  | None -> Canonical_key_switch_required