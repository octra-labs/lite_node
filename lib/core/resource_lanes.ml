(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type lane =
  | Standard
  | Program_deploy
  | Program
  | Circle_compute
  | Circle_metadata
  | Circle_assets
  | Pvac
  | Fhe

type budget = {
  max_txs : int;
  max_bytes : int;
  max_ou : Z.t;
  max_proof : int;
}

type used = {
  txs : int;
  bytes : int;
  ou : Z.t;
  proof : int;
}

type verdict =
  | Accept of used
  | Reject of string

let zero = {
  txs = 0;
  bytes = 0;
  ou = Z.zero;
  proof = 0;
}

let to_string = function
  | Standard -> "standard"
  | Program_deploy -> "program_deploy"
  | Program -> "program"
  | Circle_compute -> "circle_compute"
  | Circle_metadata -> "circle_metadata"
  | Circle_assets -> "circle_assets"
  | Pvac -> "pvac"
  | Fhe -> "fhe"

let all = [
  Standard;
  Program_deploy;
  Program;
  Circle_compute;
  Circle_metadata;
  Circle_assets;
  Pvac;
  Fhe;
]

let of_op = function
  | Transaction.Standard
  | Transaction.ValidatorSetUpdate
  | Transaction.ValidatorReady
  | Transaction.ValidatorBond
  | Transaction.ValidatorExit
  | Transaction.ValidatorWithdraw
  | Transaction.ValidatorEvidence
  | Transaction.Op01Burn -> Standard
  | Transaction.ContractDeploy
  | Transaction.ContractCall
  | Transaction.ProgramExec
  | Transaction.MultiExec
  | Transaction.ContractUpgrade -> Program
  | Transaction.ProgramDeploy -> Program_deploy
  | Transaction.CircleCall -> Circle_compute
  | Transaction.CircleAssetPut
  | Transaction.CircleAssetPutEncrypted
  | Transaction.CircleSealedSlotPut -> Circle_assets
  | Transaction.CircleBalanceCellPut
  | Transaction.CircleRegisterCellPut
  | Transaction.RecryptOp
  | Transaction.KeySwitch -> Fhe
  | Transaction.EncryptOp
  | Transaction.DecryptOp
  | Transaction.PrivateOp
  | Transaction.StealthOp
  | Transaction.ClaimOp -> Pvac
  | Transaction.CircleDeploy
  | Transaction.CircleProgramUpdate
  | Transaction.CircleSlotPolicyPut
  | Transaction.CircleStateDescriptorPut
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
  | Transaction.CircleIngressCommit -> Circle_metadata

let default_budget = function
  | Standard -> {
    max_txs = 2_048;
    max_bytes = 8 * 1_024 * 1_024;
    max_ou = Z.of_int 2_000_000;
    max_proof = 0;
  }
  | Program_deploy -> {
    max_txs = 1;
    max_bytes = 4 * 1_024 * 1_024;
    max_ou = Z.of_int 5_000_000;
    max_proof = 0;
  }
  | Program -> {
    max_txs = 128;
    max_bytes = 4 * 1_024 * 1_024;
    max_ou = Z.of_int 10_000_000;
    max_proof = 16;
  }
  | Circle_compute -> {
    max_txs = 1;
    max_bytes = 4 * 1_024 * 1_024;
    max_ou = Z.of_int 20_000_000;
    max_proof = 1;
  }
  | Circle_metadata -> {
    max_txs = 256;
    max_bytes = 2 * 1_024 * 1_024;
    max_ou = Z.of_int 5_000_000;
    max_proof = 0;
  }
  | Circle_assets -> {
    max_txs = 16;
    max_bytes = 64 * 1_024 * 1_024;
    max_ou = Z.of_int 10_000_000;
    max_proof = 0;
  }
  | Pvac -> {
    max_txs = 64;
    max_bytes = 16 * 1_024 * 1_024;
    max_ou = Z.of_int 20_000_000;
    max_proof = 64;
  }
  | Fhe -> {
    max_txs = 32;
    max_bytes = 16 * 1_024 * 1_024;
    max_ou = Z.of_int 20_000_000;
    max_proof = 32;
  }

let proof_cost op =
  match op with
  | Transaction.CircleCall -> 1
  | _ ->
    match of_op op with
  | Pvac | Fhe -> 1
  | Standard
  | Program_deploy
  | Program
  | Circle_compute
  | Circle_metadata
  | Circle_assets -> 0

let tx_bytes tx =
  tx
  |> Transaction.to_yojson
  |> Yojson.Safe.to_string
  |> String.length

let compute_ou tx =
  match of_op tx.Transaction.op_type with
  | Program_deploy -> Transaction.ou_cost tx
  | Program | Circle_compute -> tx.Transaction.ou
  | Standard | Circle_metadata | Circle_assets | Pvac | Fhe ->
    Transaction.ou_cost tx

let cost tx = {
  txs = 1;
  bytes = tx_bytes tx;
  ou = compute_ou tx;
  proof = proof_cost tx.Transaction.op_type;
}

let add a b = {
  txs = a.txs + b.txs;
  bytes = a.bytes + b.bytes;
  ou = Z.add a.ou b.ou;
  proof = a.proof + b.proof;
}

let over b u =
  if u.txs > b.max_txs then Some "tx_count"
  else if u.bytes > b.max_bytes then Some "bytes"
  else if Z.gt u.ou b.max_ou then Some "ou"
  else if u.proof > b.max_proof then Some "proof"
  else None

let within b u =
  over b u = None

let admit b u tx =
  let next = add u (cost tx) in
  match over b next with
  | None -> Accept next
  | Some reason -> Reject reason