(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module T = Octra_consensus.C_types
module R = Octra_core.Rule_graph
module A = Octra_core.Claim_archive
module C = Octra_core.Store_chaindata
module M = State_sync_manifest
module Tx = Octra_core.Transaction
module H = Octra_core.Claim_history
module Sum = Octra_core.Claim_sum
module Epochs = Map.Make (Int)

type t = {
  headers : T.epoch_header Epochs.t;
  rules : R.t;
}

type total = {
  address : string;
  first_epoch : int;
  last_epoch : int;
  state_root : string;
  source_cipher_hash : string;
  sum : Sum.summary;
}

let ( let* ) = Result.bind
let require valid reason = if valid then Ok () else Error reason

let position (value : T.finalize) =
  let* () = require
    (value.epoch_id >= 0L && value.epoch_id <= Int64.of_int max_int
     && value.header.txid_hi >= -1L && value.header.txid_hi < Int64.max_int)
    "history finality position invalid" in
  Ok (Int64.to_int value.epoch_id)

let read ~chain_id ~config_hash ~validator_set ~exporter_set ~certificate
    ~first_epoch ~max_epochs ~read_finality =
  try
    let checkpoint = certificate.M.checkpoint in
    let* () = require
      (checkpoint.chain_id = chain_id && checkpoint.config_hash = config_hash)
      "history certificate network differs" in
    let* () = require
      (first_epoch >= 0 && max_epochs > 0
       && checkpoint.epoch >= Int64.of_int first_epoch
       && Int64.sub checkpoint.epoch (Int64.of_int first_epoch)
          < Int64.of_int max_epochs)
      "history range exceeds epoch limit" in
    let* certificate = M.verify_reference_certificate
      ~validator_set ~exporter_set certificate in
    let* encoded = match M.finality certificate with
      | Some value -> Ok value
      | None -> Error "history finality missing" in
    let* anchor = Sync_anchor.decode encoded in
    let rec collect current headers =
      let* epoch = position current in
      let headers = Epochs.add epoch current.T.header headers in
      if epoch = first_epoch then Ok headers
      else
        let* () = require (current.header.proto_version = T.proto_version_current)
          "history parent link is not authenticated by header version" in
        let* parent = match current.parent_commit with
          | Some value -> Ok value
          | None -> Error "history parent commit missing" in
        let* prior = read_finality (Int64.pred current.epoch_id) in
        let* prior = Root_win.bind ~current ~validator_set:parent.validator_set prior in
        let* _ = Root_win.verify ~anchor:current [prior] in
        let* () = require (prior.header.txid_hi <= current.header.txid_hi)
          "history transaction position decreases" in
        collect prior headers
    in
    let* headers = collect (Sync_anchor.finality anchor) Epochs.empty in
    let root_at epoch = match Epochs.find_opt epoch headers with
      | None -> R.Missing
      | Some header -> R.Root
          (State_sync_checkpoint.raw_to_hex header.T.proposed_state_root) in
    Ok {headers; rules = R.create ~chain_id ~root_at}
  with exn -> Error ("history finality read failed: " ^ Printexc.to_string exn)

let math value ~epoch =
  let* () = require (Epochs.mem epoch value.headers)
    "history epoch is not authenticated" in
  match R.math value.rules ~epoch with
  | Ok mode -> Ok (mode = R.Active)
  | Error fault -> Error (R.fault_message fault)

let pin value archive epoch =
  let* header = match Epochs.find_opt epoch value.headers with
    | Some header -> Ok header
    | None -> Error "history checkpoint is not authenticated" in
  let* index_root = match C.get_epoch_index_commitment archive epoch with
    | _, Some root -> Ok root
    | _ when epoch = 0 -> Ok Octra_core.Epoch_index_commitment.genesis_root
    | _ -> Error "history transaction root missing" in
  Ok A.{epoch; index_root; next_txid = Int64.succ header.txid_hi;
    state_root = State_sync_checkpoint.raw_to_hex header.proposed_state_root}

let verify value store archive ~send_epoch ~claim_epoch ~send_index ~claim_index
    ~max_txs =
  try
    let* () = require (send_epoch > 0 && claim_epoch >= send_epoch)
      "history operation epochs invalid" in
    let* sender_math = math value ~epoch:send_epoch in
    let* receiver_math = math value ~epoch:claim_epoch in
    let read epoch =
      let* before = pin value archive (epoch - 1) in
      let* after = pin value archive epoch in
      A.read store archive ~before ~after ~max_txs in
    let* send = read send_epoch in
    let* claim = if send_epoch = claim_epoch then Ok send else read claim_epoch in
    A.verify ~send ~claim ~send_index ~claim_index ~sender_math ~receiver_math
  with exn -> Error ("history receipt read failed: " ^ Printexc.to_string exn)

let total value store archive ~address ~first_epoch ~last_epoch ~max_txs ~max_records =
  try
    let* () = require
      (Octra_core.Crypto.Address.is_valid_address address
       && first_epoch >= 0 && last_epoch >= first_epoch
       && max_txs >= 0 && max_records >= 0)
      "history account range invalid" in
    let* initial = pin value archive first_epoch in
    let* final = pin value archive last_epoch in
    let* cipher = A.cipher_at store initial address in
    let* () = require
      (match cipher with None | Some "" | Some "0" -> true | _ -> false)
      "history initial encrypted balance is not empty" in
    let* () = require
      (final.next_txid >= initial.next_txid
       && Int64.sub final.next_txid initial.next_txid <= Int64.of_int max_records)
      "history interval exceeds record limit" in
    let read epoch limit =
      let* before = pin value archive (epoch - 1) in
      let* after = pin value archive epoch in
      A.read store archive ~before ~after ~max_txs:limit in
    let credit current epoch index =
      let* source_epoch, hash = A.source current index in
      let* () = require (source_epoch > 0 && source_epoch <= epoch)
        "history source epoch invalid" in
      let* source = if source_epoch = epoch then Ok current else read source_epoch max_txs in
      let* send_index = A.find source hash in
      let* sender_math = math value ~epoch:source_epoch in
      let* receiver_math = math value ~epoch in
      A.verify ~send:source ~claim:current ~send_index ~claim_index:index
        ~sender_math ~receiver_math in
    let change current epoch (entry : H.entry) (tx : Tx.t) =
      if tx.from <> address && tx.to_ <> address then Ok Sum.Keep
      else match tx.op_type with
      | Tx.EncryptOp | Tx.DecryptOp ->
        let* math = math value ~epoch in
        let* key = A.key current ~address ~index:entry.index ~math in
        let* amount = H.amount ~op:tx.op_type entry key in
        Ok (if tx.op_type = Tx.EncryptOp then Sum.Deposit amount else Sum.Withdraw amount)
      | Tx.StealthOp ->
        let* () = require (tx.from = address) "history stealth sender differs" in
        let* math = math value ~epoch in
        let* point = A.sent current ~index:entry.index ~math in
        Ok (Sum.Send point)
      | Tx.ClaimOp ->
        let* receipt = credit current epoch entry.index in
        Ok (Sum.Claim receipt)
      | Tx.KeySwitch | Tx.RecryptOp | Tx.PrivateOp ->
        Error ("history account effect unresolved: " ^ Tx.op_type_to_string tx.op_type)
      | Tx.Standard | Tx.Op01Burn
      | Tx.ContractDeploy | Tx.ProgramDeploy | Tx.ContractCall | Tx.ProgramExec
      | Tx.MultiExec | Tx.ContractUpgrade | Tx.CircleDeploy | Tx.CircleProgramUpdate
      | Tx.CircleAssetPut | Tx.CircleAssetPutEncrypted | Tx.CircleSealedSlotPut
      | Tx.CircleSlotPolicyPut | Tx.CircleStateDescriptorPut | Tx.CircleBalanceCellPut
      | Tx.CircleRegisterCellPut | Tx.CircleTransportPolicyPut | Tx.CircleHfhePolicyPut
      | Tx.CircleKeyPolicyPut | Tx.CircleKeyGrant | Tx.CircleKeyExtend | Tx.CircleKeyRevoke
      | Tx.CircleKeyErase | Tx.CircleOutboxOpen | Tx.CircleRelayClaim | Tx.CircleRelayCancel
      | Tx.CircleIngressCommit | Tx.CircleCall | Tx.ValidatorSetUpdate | Tx.ValidatorReady
      | Tx.ValidatorBond | Tx.ValidatorExit | Tx.ValidatorWithdraw | Tx.ValidatorEvidence ->
        Ok Sum.Keep in
    let rec scan epoch sum =
      if epoch = last_epoch then Ok sum
      else
        let next = epoch + 1 in
        let* current = read next max_txs in
        let* sum = A.fold current ~init:sum ~f:(fun sum entry tx ->
          let* change = change current next entry tx in
          Sum.apply sum ~hash:entry.H.hash change) in
        scan next sum in
    let* sum = scan first_epoch (Sum.create ~address) in
    let sum = Sum.finish sum in
    let* () = require
      (Int64.of_int sum.records = Int64.sub final.next_txid initial.next_txid)
      "history interval record count differs" in
    let* cipher = A.cipher_at store final address in
    let source_cipher_hash = Octra_core.Pvac_migration_admission.source_cipher_hash
      (Option.value ~default:"0" cipher) in
    Ok {address; first_epoch; last_epoch; state_root = final.state_root;
      source_cipher_hash; sum}
  with exn -> Error ("history interval read failed: " ^ Printexc.to_string exn)