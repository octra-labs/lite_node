(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type cipher_class =
  | Empty
  | V3
  | Legacy_hfhe
  | Foreign
  | Malformed of string

type key_class =
  | Current
  | Historical
  | Missing
  | Malformed_key of string

type route =
  | Direct_key_switch
  | Bound_migration
  | History_migration
  | Blocked

type status = {
  cipher_class : cipher_class;
  key_class : key_class;
  route : route;
  reason : string;
}

let cipher_class_to_string = function
  | Empty -> "empty"
  | V3 -> "v3"
  | Legacy_hfhe -> "legacy_hfhe"
  | Foreign -> "foreign"
  | Malformed _ -> "malformed"

let key_class_to_string = function
  | Current -> "current"
  | Historical -> "historical"
  | Missing -> "missing"
  | Malformed_key _ -> "malformed"

let route_to_string = function
  | Direct_key_switch -> "direct_key_switch"
  | Bound_migration -> "bound_migration"
  | History_migration -> "history_migration"
  | Blocked -> "blocked"

let can_key_switch status =
  status.route = Direct_key_switch

let can_bound_migrate status =
  status.route = Bound_migration

let needs_history_migration status =
  status.route = History_migration

let classify_cipher = function
  | "" | "0" -> Empty
  | cipher when not (Crypto.FheBalance.is_fhe_cipher cipher) -> Foreign
  | cipher ->
    match Crypto.FheBalance.cipher_has_key_bound_material cipher with
    | Ok true -> V3
    | Ok false -> Legacy_hfhe
    | Error e -> Malformed e

let classify_key = function
  | None -> Missing
  | Some blob ->
    begin
      match Crypto.FheBalance.blob_supports_alias_rejection blob with
      | Ok true -> Current
      | Ok false -> Historical
      | Error error -> Malformed_key error
    end

let status_of_classes cipher_class key_class =
  match cipher_class, key_class with
  | Empty, _ ->
    {
      cipher_class = Empty;
      key_class;
      route = Direct_key_switch;
      reason = "encrypted balance is empty";
    }
  | V3, Current ->
    {
      cipher_class = V3;
      key_class;
      route = Bound_migration;
      reason = "key-bound encrypted balance can migrate with equality proofs";
    }
  | V3, Historical ->
    {
      cipher_class = V3;
      key_class;
      route = History_migration;
      reason = "pvac pubkey proof circuit requires public or commitment history migration";
    }
  | V3, Missing ->
    {
      cipher_class = V3;
      key_class;
      route = Blocked;
      reason = "encrypted balance has no pvac pubkey";
    }
  | V3, Malformed_key error ->
    {
      cipher_class = V3;
      key_class;
      route = Blocked;
      reason = error;
    }
  | Legacy_hfhe, _ ->
    {
      cipher_class = Legacy_hfhe;
      key_class;
      route = History_migration;
      reason = "legacy hfhe balance needs public-history replay or ciphertext migration proof";
    }
  | Foreign, _ ->
    {
      cipher_class = Foreign;
      key_class;
      route = Blocked;
      reason = "encrypted balance is not hfhe";
    }
  | Malformed error, _ ->
    {
      cipher_class = Malformed error;
      key_class;
      route = Blocked;
      reason = "encrypted balance is malformed: " ^ error;
    }

let status_of_state ~cipher ~pubkey =
  status_of_classes (classify_cipher cipher) (classify_key pubkey)