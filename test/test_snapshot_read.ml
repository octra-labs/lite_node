(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module SC = Octra_core.Store_chaindata
module EL = Octra_core.Epochlog
module Eic = Octra_core.Epoch_index_commitment
module Floor = Octra_core.History_floor
module Head = Octra_core.Head_manifest
module S = Octra_node_runtime.Startup_history_shell
module C = Octra_consensus.C_types
module Hash = Octra_consensus.C_hash
module Config = Octra_consensus.C_config
module Text = Octra_node_runtime.Text
module Ed = Mirage_crypto_ec.Ed25519

let expect label condition = if not condition then failwith label
let hash text = Digestif.SHA256.(digest_string text |> to_hex)

let floor () =
  let signers = List.init 4 (fun index ->
    let key, pub = Ed.generate () in
    key, C.{address = "validator-" ^ string_of_int index; pubkey = Ed.pub_to_octets pub}) in
  let validators = C.make_validator_set (List.map snd signers) in
  let ledger_root = hash "ledger" and epoch_index_root = hash "epoch-root" in
  let state_root = Eic.folded_state_root ~ledger_state_root:ledger_root ~epoch_index_root in
  let header = C.{
    proto_version = proto_version_current; chain_id = "octra-devnet-9871-cluster";
    epoch_id = 100L; prev_state_root = String.make 32 'a'; tx_list_hash = Hash.tx_list_hash [];
    receipt_root = Hash.receipt_root [];
    proposed_state_root = Octra_node_runtime.Consensus_epoch_apply_guard.raw32_of_pre_root state_root;
    parent_commit_hash = Octra_net.Hash_domain.nil_hash; creator_addr = "validator-0";
    txid_hi = 0L; ts = 1.;
  } in
  let proposal_id = Hash.proposal_id header in
  let votes = List.map (fun (key, validator) ->
    let vote = C.{chain_id = header.chain_id; epoch_id = header.epoch_id; round = 0;
      vote_type = Precommit; proposal_id; validator = validator.address; signature = ""} in
    {vote with signature = Ed.sign ~key (Hash.vote_sign_bytes vote)}) signers in
  let parent_commit = C.{validator_set = validators; certificate = {
    chain_id = header.chain_id; epoch_id = header.epoch_id; commit_round = 0;
    header; proposal_id; precommits = votes}} in
  Floor.create ~chain_id:header.chain_id ~epoch:100 ~state_root
    ~ledger_state_root:ledger_root ~txid_hi:0L ~config_hash:(hash "config")
    ~validator_set_hash:(Text.raw_to_hex (Config.validator_set_hash validators))
    ~epoch_index_hash:(hash "epoch") ~epoch_index_root ~parent_commit
  |> Result.get_ok

let run store visits =
  S.run_startup_checks {
    int_value = (fun _ default -> default);
    first_epoch = (fun () -> SC.first_history_epoch store |> Result.get_ok);
    last_epoch = (fun () -> SC.last_epoch_id store |> Result.get_ok);
    status_at = (fun epoch -> visits := epoch :: !visits; SC.get_epoch_index_status store epoch);
    marker_path = "marker"; marker_exists = (fun _ -> false);
    irmin_stealth_counter = (fun () -> 0L);
    chaindata_next_txid = (fun () -> SC.next_txid store);
    exit_fatal = (fun () -> failwith "snapshot history refused");
  }

let test_floor () =
  let floor = floor () in
  Test_workspace.with_dir "snapshot_read" (fun dir ->
    Head.cached := None;
    let store = SC.open_chaindata (Filename.concat dir "chaindata") in
    Fun.protect ~finally:(fun () -> SC.close store; Head.cached := None) (fun () ->
      SC.seed_history_floor store floor |> Result.get_ok;
      expect "first local epoch differs" (SC.first_history_epoch store = Ok 101);
      let visits = ref [] in
      run store visits;
      expect "snapshot tried to read omitted history" (!visits = []);
      SC.begin_batch store;
      SC.set_epoch store {EL.empty_epoch_header with id = 101; start_txid = 1L;
        tx_count = 0; state_root = hash "next-state"};
      SC.fsync store;
      SC.commit_batch store;
      run store visits;
      expect "snapshot did not verify local epoch" (!visits = [101])))

let test_bad_floor () =
  Test_workspace.with_dir "snapshot_bad" (fun dir ->
    let store = SC.open_chaindata (Filename.concat dir "chaindata") in
    Fun.protect ~finally:(fun () -> SC.close store) (fun () ->
      Octra_core.Chaindata_index.set_meta_direct (SC.index store) "history_floor" "invalid";
      expect "invalid floor accepted" (Result.is_error (SC.first_history_epoch store))))

let () =
  Mirage_crypto_rng_unix.use_default ();
  test_floor ();
  test_bad_floor ();
  print_endline "event = snapshot_read status = passed"