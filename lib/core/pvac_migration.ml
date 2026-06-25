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

let cipher_class_to_string = function
  | Empty -> "empty"
  | V3 -> "v3"
  | Legacy_hfhe -> "legacy_hfhe"
  | Foreign -> "foreign"
  | Malformed _ -> "malformed"

let classify_cipher = function
  | "" | "0" -> Empty
  | cipher when not (Crypto.FheBalance.is_fhe_cipher cipher) -> Foreign
  | cipher ->
    match Crypto.FheBalance.cipher_has_key_bound_material cipher with
    | Ok true -> V3
    | Ok false -> Legacy_hfhe
    | Error e -> Malformed e

let status_of_class = function
  | Empty ->
    {
      cipher_class = Empty;
      can_key_switch = true;
      can_v3_migrate = false;
      needs_legacy_public_replay = false;
      reason = "encrypted balance is empty";
    }
  | V3 ->
    {
      cipher_class = V3;
      can_key_switch = false;
      can_v3_migrate = true;
      needs_legacy_public_replay = false;
      reason = "v3 encrypted balance can migrate with bound equality proofs";
    }
  | Legacy_hfhe ->
    {
      cipher_class = Legacy_hfhe;
      can_key_switch = false;
      can_v3_migrate = false;
      needs_legacy_public_replay = true;
      reason = "legacy hfhe balance needs public-history replay or ciphertext migration proof";
    }
  | Foreign ->
    {
      cipher_class = Foreign;
      can_key_switch = false;
      can_v3_migrate = false;
      needs_legacy_public_replay = false;
      reason = "encrypted balance is not hfhe";
    }
  | Malformed e ->
    {
      cipher_class = Malformed e;
      can_key_switch = false;
      can_v3_migrate = false;
      needs_legacy_public_replay = false;
      reason = "encrypted balance is malformed: " ^ e;
    }

let status_of_cipher cipher =
  status_of_class (classify_cipher cipher)