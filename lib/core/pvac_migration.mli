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


type cipher_class =
  | Empty
  | V3
  | Legacy_hfhe
  | Foreign
  | Malformed of string

type status = {
  cipher_class : cipher_class;
  can_key_switch : bool;
  can_v3_migrate : bool;
  needs_legacy_public_replay : bool;
  reason : string;
}

val classify_cipher : string -> cipher_class

val status_of_cipher : string -> status

val cipher_class_to_string : cipher_class -> string