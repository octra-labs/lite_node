(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val create : data_dir:string -> t
val status : t -> Validator_intent.identity -> (string option, string) result
val request : t -> Validator_intent.identity -> privkey:string -> (string, string) result
val cancel : t -> Validator_intent.identity -> privkey:string -> (unit, string) result
val guard : t -> Validator_intent.identity -> (unit -> ('a, string) result) -> ('a, string) result