(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Circle_program = Octra_circle_runtime.Circle_program
module Circles = Octra_core.Circles
module Epoch_exec = Octra_core.Epoch_exec
module Program_trust = Octra_vm.Program_trust
module Store = Octra_core.Store_irmin
module Transaction = Octra_core.Transaction

let reject reason =
  Error ("circle_program_invalid", reason)

let admit_octb program_trust code_b64 =
  match
    Circle_program.decode_bytecode
      ~trusted:(Program_trust.keys program_trust)
      code_b64
  with
  | Ok _ -> Ok ()
  | Error reason -> reject reason

let admit_deploy program_trust tx =
  match Epoch_exec.parse_circle_deploy_payload tx with
  | Error error -> Lwt.return_error error
  | Ok { Circles.runtime = Circles.Wasm_v1; _ }
  | Ok { code_b64 = None; _ } ->
    Lwt.return_ok ()
  | Ok { runtime = Circles.Octb; code_b64 = Some code_b64; _ } ->
    Lwt.return (admit_octb program_trust code_b64)

let admit_update store program_trust tx =
  match Epoch_exec.parse_circle_program_update_payload tx with
  | Error error -> Lwt.return_error error
  | Ok payload ->
    let open Lwt.Syntax in
    let* info = Store.get_circle_info store tx.Transaction.to_ in
    begin
      match info with
      | None
      | Some { Circles.runtime = Circles.Wasm_v1; _ } ->
        Lwt.return_ok ()
      | Some { runtime = Circles.Octb; _ } ->
        Lwt.return
          (admit_octb program_trust payload.Circles.code_b64)
    end

let admit ~store ~program_trust tx =
  match tx.Transaction.op_type with
  | Transaction.CircleDeploy ->
    admit_deploy program_trust tx
  | Transaction.CircleProgramUpdate ->
    admit_update store program_trust tx
  | _ ->
    Lwt.return_ok ()