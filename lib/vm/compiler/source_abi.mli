(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val type_name : Oct_lang.typ -> string
val to_json : Oct_lang.contract -> Yojson.Safe.t
val encode : Oct_lang.contract -> string