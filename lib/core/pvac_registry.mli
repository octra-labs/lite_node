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


type status = {
  has_pvac_pubkey : bool;
  pubkey_size : int option;
  deserializable : bool;
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

val max_pubkey_bytes : int

val expected_kat : unit -> string

val key_hash : string -> string

val decode_b64 : string -> (string, string) result

val validate_size : string -> (string, string) result

val register_blob_of_b64 : string -> (string, string) result

val kat_admission : string option -> kat_admission

val validate_shape : string -> (string, string) result

val load_pubkey : string -> (Pvac_ffi.pubkey, string) result

val canonicalize_blob : string -> (string, string) result

val status_of_blob : string option -> status

val register_decision : existing:string option -> incoming:string -> register_decision