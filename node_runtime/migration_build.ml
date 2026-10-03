(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Chain = Octra_bootstrap.Claim_chain
module Store = Octra_core.Store_irmin
module Archive = Octra_core.Store_chaindata
module Journal = Consensus_finality_journal

type request = {
  data_dir : string;
  chain_id : string;
  config_hash : string;
  certificate : Octra_bootstrap.State_sync_manifest.certificate;
  validator_set : Octra_consensus.C_types.validator_set;
  exporter_set : Octra_consensus.C_types.validator_set;
  first_epoch : int;
  activation_epoch : int;
  addresses : string list;
  max_epochs : int;
  max_txs : int;
  max_records : int;
}

let ( let* ) = Result.bind
let require value reason = if value then Ok () else Error reason

let read request =
  try
    let checkpoint = request.certificate.Octra_bootstrap.State_sync_manifest.checkpoint in
    let* () = require
      (checkpoint.epoch >= 0L && checkpoint.epoch < Int64.of_int request.activation_epoch)
      "migration activation must follow snapshot" in
    let addresses = List.sort_uniq String.compare request.addresses in
    let* () = require
      (addresses <> [] && List.length addresses = List.length request.addresses
       && List.length addresses <= 1_000_000
       && List.for_all Octra_core.Crypto.Address.is_valid_address addresses)
      "migration addresses invalid" in
    let* () = require (request.max_txs >= 0 && request.max_records >= 0)
      "migration record limits invalid" in
    let read_finality epoch =
      match Journal.read_committed_epoch ~chain_id:request.chain_id ~epoch request.data_dir with
      | Journal.Valid record -> Ok record.finalize
      | Journal.Missing -> Error "migration committed finality missing"
      | Journal.Invalid reason -> Error reason in
    let* chain = Chain.read ~chain_id:request.chain_id ~config_hash:request.config_hash
      ~validator_set:request.validator_set ~exporter_set:request.exporter_set
      ~certificate:request.certificate ~first_epoch:request.first_epoch
      ~max_epochs:request.max_epochs ~read_finality in
    let path name =
      let path = Filename.concat request.data_dir name in
      if (Unix.stat path).Unix.st_kind <> Unix.S_DIR then
        failwith "migration store directory missing";
      path in
    let store = Lwt_main.run (Store.open_store ~readonly:true (path "irmin_store")) in
    Fun.protect ~finally:(fun () -> Lwt_main.run (Store.close store)) (fun () ->
      let archive = Archive.open_chaindata ~readonly:true (path "chaindata") in
      Fun.protect ~finally:(fun () -> Archive.close archive) (fun () ->
        let rec collect totals = function
          | [] -> Chain.admission chain ~activation_epoch:request.activation_epoch (List.rev totals)
          | address :: rest ->
            let* total = Chain.total chain store archive ~address
              ~first_epoch:request.first_epoch ~last_epoch:(Int64.to_int checkpoint.epoch)
              ~max_txs:request.max_txs ~max_records:request.max_records in
            collect (total :: totals) rest in
        collect [] addresses))
  with exn -> Error ("migration history read failed: " ^ Printexc.to_string exn)