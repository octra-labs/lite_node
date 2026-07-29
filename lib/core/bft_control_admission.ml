(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let validate_message op_type message =
  match op_type with
  | Transaction.ValidatorSetUpdate ->
    begin
      match Validator_set_update.of_message message with
      | Ok _ -> Ok ()
      | Error e -> Error e
    end
  | Transaction.ValidatorReady ->
    begin
      match
        Validator_set_update.ready_ext_of_message message,
        Validator_registry.ready_payload_of_message message
      with
      | Ok _, _
      | _, Ok _ -> Ok ()
      | Error legacy, Error bonded ->
        Error (legacy ^ "; " ^ bonded)
    end
  | Transaction.ValidatorBond ->
    begin
      match Validator_registry.bond_payload_of_message message with
      | Ok _ -> Ok ()
      | Error e -> Error e
    end
  | Transaction.ValidatorEvidence ->
    begin
      match Validator_evidence.proof_of_message message with
      | Ok _ -> Ok ()
      | Error e -> Error e
    end
  | _ -> Ok ()