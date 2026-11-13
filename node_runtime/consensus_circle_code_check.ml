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

let code_work ~scalar_work ~limit ~octb code_b64 =
  if not scalar_work then true else
  let load = Octra_vm.Scalar_work.load (String.length code_b64) in
  if Z.gt load limit then false
  else if not octb then true
  else match Base64.decode code_b64 with
    | Error _ -> false
    | Ok raw -> Octra_vm.Program_envelope.is_program raw
      && Option.fold ~none:false ~some:(fun cost -> Z.leq (Z.add load cost) limit)
        (Octra_vm.Code_check.scalar_cost raw)

let admit_octb ~scalar_work ~limit ~point_ops program_trust code_b64 =
  if not (code_work ~scalar_work ~limit ~octb:true code_b64)
  then reject "circle creation work refused"
  else
  match
    Circle_program.decode_bytecode
      ~trusted:(Program_trust.keys program_trust)
      ~point_ops
      code_b64
  with
  | Ok _ -> Ok ()
  | Error reason -> reject reason

let admit_deploy ~scalar_work ~resource_errors ~point_ops program_trust tx =
  match Epoch_exec.parse_circle_deploy_payload ~resource_errors tx with
  | Error error -> Lwt.return_error error
  | Ok { Circles.runtime = Circles.Wasm_v1; code_b64 = Some code; _ } ->
    Lwt.return (if code_work ~scalar_work ~limit:tx.Transaction.ou ~octb:false code
      then Ok () else reject "circle creation work refused")
  | Ok { code_b64 = None; _ } ->
    Lwt.return_ok ()
  | Ok { runtime = Circles.Octb; code_b64 = Some code_b64; _ } ->
    Lwt.return (admit_octb ~scalar_work ~limit:tx.Transaction.ou ~point_ops program_trust code_b64)

let admit_update ~scalar_work ~resource_errors ~point_ops store program_trust tx =
  match Epoch_exec.parse_circle_program_update_payload ~resource_errors tx with
  | Error error -> Lwt.return_error error
  | Ok payload ->
    let open Lwt.Syntax in
    let* info = Store.get_circle_info store tx.Transaction.to_ in
    begin
      match info with
      | None ->
        Lwt.return_ok ()
      | Some { Circles.runtime = Circles.Wasm_v1; _ } ->
        Lwt.return (if code_work ~scalar_work ~limit:tx.Transaction.ou ~octb:false payload.Circles.code_b64
          then Ok () else reject "circle creation work refused")
      | Some { runtime = Circles.Octb; _ } ->
        Lwt.return
          (admit_octb ~scalar_work ~limit:tx.Transaction.ou ~point_ops program_trust payload.Circles.code_b64)
    end

let admit ?(scalar_work = false) ?(resource_errors = false) ~store ~program_trust ~point_ops tx =
  match tx.Transaction.op_type with
  | Transaction.CircleDeploy ->
    admit_deploy ~scalar_work ~resource_errors ~point_ops program_trust tx
  | Transaction.CircleProgramUpdate ->
    admit_update ~scalar_work ~resource_errors ~point_ops store program_trust tx
  | _ ->
    Lwt.return_ok ()