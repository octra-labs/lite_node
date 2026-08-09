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

val classify_cipher : string -> cipher_class

val classify_key : string option -> key_class

val status_of_state :
  cipher:string ->
  pubkey:string option ->
  status

val cipher_class_to_string : cipher_class -> string

val key_class_to_string : key_class -> string

val route_to_string : route -> string

val can_key_switch : status -> bool

val can_bound_migrate : status -> bool

val needs_history_migration : status -> bool