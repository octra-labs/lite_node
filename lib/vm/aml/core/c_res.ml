(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let typ ok err = C_type.Sum (ok, err)
let ok ~err value = C_term.Inl (value, err)
let err ~ok value = C_term.Inr (ok, value)

let fold value ok_bind yes err_bind no =
  C_term.Case (value, ok_bind, yes, err_bind, no)