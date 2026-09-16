(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type pubkey
type seckey
type evalkey
type cipher
type params
type zero_proof
type range_proof
type cipher_shape = {
  slots : int;
  layers : int;
  edges : int;
  c0 : int;
  base_layers : int;
}

external isolate_worker : unit -> unit = "caml_pvac_worker_isolate"
external default_params : unit -> params = "caml_pvac_default_params"
external keygen : params -> pubkey * seckey = "caml_pvac_keygen"
external keygen_from_seed : params -> bytes -> pubkey * seckey = "caml_pvac_keygen_from_seed"
external make_evalkey : pubkey -> seckey -> int -> int -> evalkey = "caml_pvac_make_evalkey"

external enc_value_seeded : pubkey -> seckey -> int64 -> bytes -> cipher = "caml_pvac_enc_value_seeded"
external enc_values_seeded : pubkey -> seckey -> int64 array -> bytes -> cipher = "caml_pvac_enc_values_seeded"
external enc_zero_seeded : pubkey -> seckey -> bytes -> cipher = "caml_pvac_enc_zero_seeded"

external dec_value : pubkey -> seckey -> cipher -> int64 = "caml_pvac_dec_value"
external dec_values : pubkey -> seckey -> cipher -> int64 array = "caml_pvac_dec_values"

external ct_add : pubkey -> cipher -> cipher -> cipher = "caml_pvac_ct_add"
external ct_sub : pubkey -> cipher -> cipher -> cipher = "caml_pvac_ct_sub"
external ct_mul_seeded_raw : bool -> pubkey -> cipher -> cipher -> bytes -> cipher
  = "caml_pvac_ct_mul_seeded_math"

let ct_mul_seeded ?(math = false) key left right seed =
  ct_mul_seeded_raw math key left right seed

external ct_scale_raw : bool -> pubkey -> cipher -> int64 -> cipher
  = "caml_pvac_ct_scale_math"

let ct_scale ?(math = false) key cipher scalar =
  ct_scale_raw math key cipher scalar

external ct_add_const_raw : bool -> pubkey -> cipher -> int64 -> int64 -> cipher
  = "caml_pvac_ct_add_const_math"

let ct_add_const ?(math = false) key cipher lo hi =
  ct_add_const_raw math key cipher lo hi

external ct_sub_const_raw : bool -> pubkey -> cipher -> int64 -> cipher
  = "caml_pvac_ct_sub_const_math"

let ct_sub_const ?(math = false) key cipher scalar =
  ct_sub_const_raw math key cipher scalar

external ct_div_const : pubkey -> cipher -> int64 -> int64 -> cipher = "caml_pvac_ct_div_const"
external ct_square_seeded_raw : bool -> pubkey -> cipher -> bytes -> cipher
  = "caml_pvac_ct_square_seeded_math"

let ct_square_seeded ?(math = false) key cipher seed =
  ct_square_seeded_raw math key cipher seed

external ct_recrypt_seeded : pubkey -> evalkey -> cipher -> bytes -> cipher = "caml_pvac_ct_recrypt_seeded"

external commit_ct : pubkey -> cipher -> bytes = "caml_pvac_commit_ct"
external cipher_has_key_bound_material : cipher -> bool = "caml_pvac_cipher_has_key_bound_material"
external cipher_base_layers : cipher -> int = "caml_pvac_cipher_base_layers"
external cipher_shape : cipher -> cipher_shape = "caml_pvac_cipher_shape"
external cipher_mul_depth : cipher -> int = "caml_pvac_cipher_mul_depth"
external cipher_is_wrapped_scalar : cipher -> bool = "caml_pvac_cipher_is_wrapped_scalar"
external pubkey_is_key_bound_extension : pubkey -> pubkey -> bool = "caml_pvac_pubkey_is_key_bound_extension"
external pubkey_supports_alias_rejection : pubkey -> bool = "caml_pvac_pubkey_supports_alias_rejection"
external cipher_is_key_bound_extension : cipher -> cipher -> bool = "caml_pvac_cipher_is_key_bound_extension"

external make_zero_proof_raw : bool -> pubkey -> seckey -> cipher -> zero_proof
  = "caml_pvac_make_zero_proof_math"

let make_zero_proof ?(math = false) key secret cipher =
  make_zero_proof_raw math key secret cipher

external verify_zero_raw : bool -> pubkey -> cipher -> zero_proof -> bool
  = "caml_pvac_verify_zero_math"

let verify_zero ?(math = false) key cipher proof =
  verify_zero_raw math key cipher proof

external make_zero_proof_bound_raw : bool -> pubkey -> seckey -> cipher -> int64 -> bytes -> zero_proof
  = "caml_pvac_make_zero_proof_bound_bytecode" "caml_pvac_make_zero_proof_bound_math"

let make_zero_proof_bound ?(math = false) key secret cipher amount blind =
  make_zero_proof_bound_raw math key secret cipher amount blind

external verify_zero_bound_raw : bool -> pubkey -> cipher -> zero_proof -> bytes -> bool
  = "caml_pvac_verify_zero_bound_math"

let verify_zero_bound ?(math = false) key cipher proof commitment =
  verify_zero_bound_raw math key cipher proof commitment

external verify_zero_amount_prior_raw : bool -> pubkey -> cipher -> zero_proof -> bytes -> bool
  = "caml_pvac_verify_zero_amount_prior_math"

let verify_zero_amount_prior ?(math = false) key cipher proof commitment =
  verify_zero_amount_prior_raw math key cipher proof commitment

external verify_zero_bound_key_switch_raw : bool -> pubkey -> cipher -> zero_proof -> bytes -> bool
  = "caml_pvac_verify_zero_bound_key_switch_math"

let verify_zero_bound_key_switch ?(math = false) key cipher proof commitment =
  verify_zero_bound_key_switch_raw math key cipher proof commitment

external verify_zero_amount_key_switch_prior_raw : bool -> pubkey -> cipher -> zero_proof -> bytes -> bool
  = "caml_pvac_verify_zero_amount_key_switch_prior_math"

let verify_zero_amount_key_switch_prior ?(math = false) key cipher proof commitment =
  verify_zero_amount_key_switch_prior_raw math key cipher proof commitment

external make_zero_proof_bound_historical_migration_raw : bool -> pubkey -> seckey -> cipher -> int64 -> bytes -> zero_proof
  = "caml_pvac_make_zero_proof_bound_historical_migration_bytecode" "caml_pvac_make_zero_proof_bound_historical_migration_math"

let make_zero_proof_bound_historical_migration ?(math = false) key secret cipher amount blind =
  make_zero_proof_bound_historical_migration_raw math key secret cipher amount blind

external verify_zero_bound_historical_migration_raw : bool -> pubkey -> cipher -> zero_proof -> bytes -> bool
  = "caml_pvac_verify_zero_bound_historical_migration_math"

let verify_zero_bound_historical_migration ?(math = false) key cipher proof commitment =
  verify_zero_bound_historical_migration_raw math key cipher proof commitment

external verify_zero_amount_historical_prior_raw : bool -> pubkey -> cipher -> zero_proof -> bytes -> bool
  = "caml_pvac_verify_zero_amount_historical_prior_math"

let verify_zero_amount_historical_prior ?(math = false) key cipher proof commitment =
  verify_zero_amount_historical_prior_raw math key cipher proof commitment

external make_zero_proof_bound_range_raw : bool -> pubkey -> seckey -> cipher -> int64 -> bytes -> zero_proof
  = "caml_pvac_make_zero_proof_bound_range_bytecode" "caml_pvac_make_zero_proof_bound_range_math"

let make_zero_proof_bound_range ?(math = false) key secret cipher amount blind =
  make_zero_proof_bound_range_raw math key secret cipher amount blind

external pedersen_commit_amount : int64 -> bytes -> bytes
  = "caml_pvac_pedersen_commit_amount"
external pedersen_identity : unit -> bytes = "caml_pvac_pedersen_identity"
external pedersen_add : bytes -> bytes -> bytes = "caml_pvac_pedersen_add"
external pedersen_sub : bytes -> bytes -> bytes = "caml_pvac_pedersen_sub"

external make_range_proof_raw : bool -> pubkey -> seckey -> cipher -> int64 -> range_proof
  = "caml_pvac_make_range_proof_math"

let make_range_proof ?(math = false) key secret cipher amount =
  make_range_proof_raw math key secret cipher amount

external verify_range_raw : bool -> pubkey -> cipher -> range_proof -> bool
  = "caml_pvac_verify_range_math"

let verify_range ?(math = false) key cipher proof =
  verify_range_raw math key cipher proof

type agg_range_proof
external make_aggregated_range_proof_raw : bool -> pubkey -> seckey -> cipher -> int64 -> agg_range_proof
  = "caml_pvac_make_aggregated_range_proof_math"

let make_aggregated_range_proof ?(math = false) key secret cipher amount =
  make_aggregated_range_proof_raw math key secret cipher amount

external serialize_agg_range_proof : agg_range_proof -> bytes
  = "caml_pvac_serialize_agg_range_proof"

external verify_range_any_raw : bool -> pubkey -> cipher -> bytes -> bool -> bool
  = "caml_pvac_verify_range_any_math"

let verify_range_any ?(math = false) key cipher proof strict =
  verify_range_any_raw math key cipher proof strict

external verify_range_bound_raw : bool -> pubkey -> cipher -> bytes -> bytes -> bool
  = "caml_pvac_verify_range_bound_math"

let verify_range_bound ?(math = false) key cipher proof commitment =
  verify_range_bound_raw math key cipher proof commitment

external verify_range_amount_prior_raw : bool -> pubkey -> cipher -> bytes -> bytes -> bool
  = "caml_pvac_verify_range_amount_prior_math"

let verify_range_amount_prior ?(math = false) key cipher proof commitment =
  verify_range_amount_prior_raw math key cipher proof commitment

external serialize_cipher : cipher -> bytes = "caml_pvac_serialize_cipher"
external serialize_cipher_public : cipher -> bytes = "caml_pvac_serialize_cipher_public"
external deserialize_cipher_result : bytes -> (cipher, string) result
  = "caml_pvac_deserialize_cipher_result"
external deserialize_cipher_prior_result : bytes -> (cipher, string) result
  = "caml_pvac_deserialize_cipher_prior_result"
external deserialize_cipher_cap_result : bytes -> (cipher, string) result
  = "caml_pvac_deserialize_cipher_cap_result"

let deserialize_cipher ?(strict = true) ?(cap = true) bytes =
  let result =
    if strict then deserialize_cipher_result bytes
    else if cap then deserialize_cipher_cap_result bytes
    else deserialize_cipher_prior_result bytes
  in
  match result with
  | Ok cipher -> cipher
  | Error reason -> failwith reason
external serialize_pubkey : pubkey -> bytes = "caml_pvac_serialize_pubkey"
external serialize_pubkey_legacy_v2 : pubkey -> bytes = "caml_pvac_serialize_pubkey_legacy_v2"
external deserialize_pubkey_result : bytes -> (pubkey, string) result
  = "caml_pvac_deserialize_pubkey_result"

let deserialize_pubkey bytes =
  match deserialize_pubkey_result bytes with
  | Ok pubkey -> pubkey
  | Error reason -> failwith reason
external serialize_seckey : seckey -> bytes = "caml_pvac_serialize_seckey"
external deserialize_seckey : bytes -> seckey = "caml_pvac_deserialize_seckey"
external serialize_zero_proof : zero_proof -> bytes = "caml_pvac_serialize_zero_proof"
external serialize_bound_range_proof : zero_proof -> bytes = "caml_pvac_serialize_bound_range_proof"
external deserialize_zero_proof : bytes -> zero_proof = "caml_pvac_deserialize_zero_proof"
external serialize_range_proof : range_proof -> bytes = "caml_pvac_serialize_range_proof"
external deserialize_range_proof : bytes -> range_proof = "caml_pvac_deserialize_range_proof"

external aes_kat : unit -> bytes = "caml_pvac_aes_kat"