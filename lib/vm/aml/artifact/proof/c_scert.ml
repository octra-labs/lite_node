(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bits = C_cert.bits

type error =
  | Lower of C_low.error
  | Cert of C_cert.error

let issue rules ~epoch items body =
  match C_low.prog items body with
  | Error error -> Error (Lower error)
  | Ok core ->
    begin
      match C_cert.issue rules ~epoch core with
      | Ok bits -> Ok bits
      | Error error -> Error (Cert error)
    end

let verify rules ~epoch items body input =
  match issue rules ~epoch items body with
  | Ok exact -> List.equal Bool.equal exact input
  | Error _ -> false

let text = function
  | Lower error -> C_low.text error
  | Cert error -> C_cert.text error