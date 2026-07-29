(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

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