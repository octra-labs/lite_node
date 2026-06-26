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


type pubkey
type seckey
type evalkey
type cipher
type params
type zero_proof
type range_proof

val default_params : unit -> params
val keygen : params -> pubkey * seckey
val keygen_from_seed : params -> bytes -> pubkey * seckey
val make_evalkey : pubkey -> seckey -> int -> int -> evalkey

val enc_value_seeded : pubkey -> seckey -> int64 -> bytes -> cipher
val enc_values_seeded : pubkey -> seckey -> int64 array -> bytes -> cipher
val enc_zero_seeded : pubkey -> seckey -> bytes -> cipher

val dec_value : pubkey -> seckey -> cipher -> int64
val dec_values : pubkey -> seckey -> cipher -> int64 array

val ct_add : pubkey -> cipher -> cipher -> cipher
val ct_sub : pubkey -> cipher -> cipher -> cipher
val ct_mul_seeded : pubkey -> cipher -> cipher -> bytes -> cipher
val ct_scale : pubkey -> cipher -> int64 -> cipher
val ct_add_const : pubkey -> cipher -> int64 -> int64 -> cipher
val ct_sub_const : pubkey -> cipher -> int64 -> cipher
val ct_div_const : pubkey -> cipher -> int64 -> int64 -> cipher
val ct_square_seeded : pubkey -> cipher -> bytes -> cipher
val ct_recrypt_seeded : pubkey -> evalkey -> cipher -> bytes -> cipher

val commit_ct : pubkey -> cipher -> bytes
val cipher_has_key_bound_material : cipher -> bool
val pubkey_is_key_bound_extension : pubkey -> pubkey -> bool
val cipher_is_key_bound_extension : cipher -> cipher -> bool
val bind_legacy_cipher_material : pubkey -> seckey -> cipher -> cipher

val make_zero_proof : pubkey -> seckey -> cipher -> zero_proof

val verify_zero : pubkey -> cipher -> zero_proof -> bool

val make_zero_proof_bound : pubkey -> seckey -> cipher -> int64 -> bytes -> zero_proof

val verify_zero_bound : pubkey -> cipher -> zero_proof -> bytes -> bool

val make_zero_proof_bound_range : pubkey -> seckey -> cipher -> int64 -> bytes -> zero_proof

val pedersen_commit_amount : int64 -> bytes -> bytes
val pedersen_identity : unit -> bytes
val pedersen_add : bytes -> bytes -> bytes
val pedersen_sub : bytes -> bytes -> bytes

val make_range_proof : pubkey -> seckey -> cipher -> int64 -> range_proof

val verify_range : pubkey -> cipher -> range_proof -> bool

type agg_range_proof
val make_aggregated_range_proof : pubkey -> seckey -> cipher -> int64 -> agg_range_proof
val serialize_agg_range_proof : agg_range_proof -> bytes

val verify_range_any : pubkey -> cipher -> bytes -> bool

val serialize_cipher : cipher -> bytes
val deserialize_cipher : bytes -> cipher
val serialize_pubkey : pubkey -> bytes
val serialize_pubkey_legacy_v2 : pubkey -> bytes
val deserialize_pubkey : bytes -> pubkey
val serialize_seckey : seckey -> bytes
val deserialize_seckey : bytes -> seckey
val serialize_zero_proof : zero_proof -> bytes
val serialize_bound_range_proof : zero_proof -> bytes
val deserialize_zero_proof : bytes -> zero_proof
val serialize_range_proof : range_proof -> bytes
val deserialize_range_proof : bytes -> range_proof

val aes_kat : unit -> bytes