(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type key = {
  id : string;
  public_key : string;
}

type error

val error_message : error -> string
val attach : key_id:string -> private_key:string -> string -> (string, error) result
val verify : trusted:key list -> string -> (unit, error) result