(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type limits = {
  encrypted_data_len : int;
  message_len : int;
  message_zkp_len : int;
  message_validator_len : int;
  message_program_len : int;
}

let standard = {
  encrypted_data_len = 50_000_000;
  message_len = 256;
  message_zkp_len = 50_000;
  message_validator_len = 16_384;
  message_program_len = 10_000_000;
}

let message_limit limits tx =
  match tx.Transaction.op_type with
  | Transaction.RecryptOp
  | Transaction.ClaimOp -> limits.message_zkp_len
  | Transaction.CircleAssetPut
  | Transaction.CircleAssetPutEncrypted
  | Transaction.CircleSealedSlotPut -> Transaction.circle_asset_message_len
  | Transaction.ContractCall
  | Transaction.ProgramExec
  | Transaction.MultiExec
  | Transaction.CircleCall
  | Transaction.CircleSlotPolicyPut
  | Transaction.CircleStateDescriptorPut
  | Transaction.CircleBalanceCellPut
  | Transaction.CircleRegisterCellPut
  | Transaction.CircleTransportPolicyPut
  | Transaction.CircleHfhePolicyPut
  | Transaction.CircleKeyPolicyPut
  | Transaction.CircleKeyGrant
  | Transaction.CircleKeyExtend
  | Transaction.CircleKeyRevoke
  | Transaction.CircleKeyErase
  | Transaction.CircleOutboxOpen
  | Transaction.CircleRelayClaim
  | Transaction.CircleRelayCancel
  | Transaction.CircleIngressCommit
  | Transaction.ContractDeploy
  | Transaction.ProgramDeploy
  | Transaction.CircleDeploy
  | Transaction.CircleProgramUpdate -> limits.message_program_len
  | Transaction.ValidatorSetUpdate
  | Transaction.ValidatorReady
  | Transaction.ValidatorBond
  | Transaction.ValidatorEvidence -> limits.message_validator_len
  | _ -> limits.message_len

let encrypted_data_limit limits tx =
  match tx.Transaction.op_type with
  | Transaction.CircleAssetPut
  | Transaction.CircleAssetPutEncrypted
  | Transaction.CircleSealedSlotPut
  | Transaction.CircleBalanceCellPut
  | Transaction.CircleRegisterCellPut ->
    Transaction.circle_asset_max_encrypted_data_len
  | _ -> limits.encrypted_data_len

let size_ok ~limits tx =
  let encrypted_limit = encrypted_data_limit limits tx in
  let message_limit = message_limit limits tx in
  Option.fold
    ~none:true
    ~some:(fun value -> String.length value <= encrypted_limit)
    tx.Transaction.encrypted_data
  && Option.fold
       ~none:true
       ~some:(fun value -> String.length value <= message_limit)
       tx.Transaction.message

let admit ?(limits=standard) tx =
  if not (size_ok ~limits tx) then
    Error "encrypted_data or message exceeds size limit"
  else
    Bft_control_admission.validate_message
      tx.Transaction.op_type
      tx.Transaction.message

let decode ?(limits=standard) json =
  match Transaction.of_yojson json with
  | Error _ as error -> error
  | Ok tx -> Result.map (fun () -> tx) (admit ~limits tx)