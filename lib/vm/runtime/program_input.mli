(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type error

val error_message : error -> string
val parse : Program_type_flow.kind list -> Yojson.Safe.t list -> (Contract_vm.v list, error) result
val validate : Program_type_flow.kind list -> Contract_vm.v list -> (unit, error) result