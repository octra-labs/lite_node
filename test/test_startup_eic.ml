(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module SC = Octra_core.Store_chaindata
module SI = Octra_core.Store_irmin
module HM = Octra_core.Head_manifest
module EL = Octra_core.Epochlog
module Eic = Octra_core.Epoch_index_commitment

let assert_msg ok msg =
  if not ok then failwith msg

let hash c =
  String.make 64 c

let work_dir () =
  Test_workspace.unique_dir "octra_startup_eic"

let cleanup dir =
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir)))

let epoch_header ~epoch_id ~start_txid ~state_root =
  {
    EL.empty_epoch_header with
    id = epoch_id;
    state_root;
    prev_state_root = hash '8';
    parent_commit = hash '7';
    start_txid;
    tx_count = 1;
    finalized_by = "tester";
    finalized_at = Unix.gettimeofday ();
  }

let prepare_node data_dir =
  let chaindata = SC.open_chaindata (Filename.concat data_dir "chaindata") in
  let store = Lwt_main.run (SI.open_store (Filename.concat data_dir "irmin_store")) in
  Lwt_main.run (SI.begin_epoch_batch store);
  Lwt_main.run (SI.set_meta store "last_epoch" "0");
  Lwt_main.run (SI.set_meta store "current_epoch" "1");
  Lwt_main.run (SI.commit_epoch_batch store "test epoch zero");
  let ledger_state_root = Option.get (Lwt_main.run (SI.get_head_hash store)) in
  let irmin_commit = Lwt_main.run (SI.get_commit_hash store) in
  SC.begin_batch chaindata;
  let epoch_id = 0 in
  let start_txid = SC.next_txid chaindata in
  let tx_hash = hash 'a' in
  SC.save_tx chaindata
    ~hash:tx_hash
    ~epoch_id
    ~from_addr:"octFROM"
    ~to_addr:"octTO"
    ~tx_json:{|{"from":"octFROM","to_":"octTO"}|}
    ~op_type:"standard"
    ~encrypted_data:""
    ~message:"";
  let epoch_hash, root = Eic.next_root
    ~prev:Eic.genesis_root
    ~epoch_id
    [Eic.item ~txid:start_txid ~hash:tx_hash]
  in
  let state_root = Eic.folded_state_root ~ledger_state_root ~epoch_index_root:root in
  SC.set_epoch chaindata (epoch_header ~epoch_id ~start_txid ~state_root);
  SC.set_epoch_index_commitment chaindata ~epoch_id ~epoch_hash ~root;
  SC.commit_batch chaindata;
  SC.fsync chaindata;
  let txlog_seg, txlog_off = SC.txlog_position chaindata in
  let epochlog_off = SC.epochlog_offset chaindata in
  HM.atomic_write data_dir {
    schema_version = HM.schema_version;
    generation = epoch_id;
    epoch_id;
    state_root;
    ledger_state_root = Some ledger_state_root;
    irmin_commit;
    txid_hi = start_txid;
    txlog_seg = Some txlog_seg;
    txlog_off = Some txlog_off;
    epochlog_off = Some epochlog_off;
    commit_id = "startup-eic";
    ts = Unix.gettimeofday ();
    quorum_cert_hash = None;
    epoch_index_hash = Some epoch_hash;
    epoch_index_root = Some root;
  };
  chaindata, store

let test_recovery_checks_eic () =
  let data_dir = work_dir () in
  let chaindata, store = prepare_node data_dir in
  Fun.protect
    ~finally:(fun () ->
      SC.close chaindata;
      Lwt_main.run (SI.close store);
      cleanup data_dir)
    (fun () ->
      let result = Lwt_main.run (Octra_core.Startup_recovery.recover
        ~data_dir
        ~chaindata
        ~store)
      in
      assert_msg result.boundary_ok "boundary must converge";
      assert_msg result.eic_checked "EIC must be checked";
      assert_msg result.eic_ok "EIC must pass")

let rule_store dir mode offset =
  let module C = Octra_consensus.C_types in
  let module H = Octra_consensus.C_hash in
  let module G = Octra_core.Rule_graph in
  let await = Lwt_main.run in
  let get = function Ok value -> value | Error reason -> failwith reason in
  let hex raw = String.concat "" (List.init (String.length raw)
    (fun index -> Printf.sprintf "%02x" (Char.code raw.[index]))) in
  let raw hex = String.init 32 (fun index ->
    Char.chr (int_of_string ("0x" ^ String.sub hex (index * 2) 2))) in
  let chain_id = "octra-devnet-9871-cluster" in
  let graph = G.create ~chain_id ~root_at:(fun _ -> G.Missing) in
  let plan = Option.get (G.circle_activation graph) in
  assert_msg (List.mem mode ["before"; "missing"; "wrong"] && List.mem offset [0; 1])
    "invalid rule store mode";
  let epoch = plan.activation_epoch + (if mode = "before" then -1 else offset) in
  let floor_epoch = if mode = "missing" then plan.activation_epoch - 2
    else plan.anchor_epoch - 1 in
  let private_raw = String.make 32 '\017' in
  let key = Mirage_crypto_ec.Ed25519.priv_of_octets private_raw
    |> function Ok key -> key | Error _ -> failwith "test key refused" in
  let pubkey = Mirage_crypto_ec.Ed25519.pub_of_priv key
    |> Mirage_crypto_ec.Ed25519.pub_to_octets in
  let pub = Base64.encode_exn pubkey in
  let address = Octra_core.Crypto.Address.address_from_pubkey pub in
  let wallet = Filename.concat dir "wallet.json" in
  Yojson.Safe.to_file wallet (`Assoc ["priv", `String (Base64.encode_exn private_raw);
    "pub", `String pub; "address", `String address]);
  Unix.chmod wallet 0o600;
  let validators = C.make_validator_set [C.{address; pubkey}] in
  let config_hash = Octra_consensus.C_config.network_hash ~chain_id () |> hex in
  let data = SC.open_chaindata (Filename.concat dir "chaindata") in
  let store = await (SI.open_store (Filename.concat dir "irmin_store")) in
  Fun.protect ~finally:(fun () -> SC.close data; await (SI.close store)) (fun () ->
    await (SI.begin_epoch_batch store);
    let ledger = Octra_core.Ledger.create store in
    get (Octra_core.Ledger.add_account_with_pubkey ledger address Z.one pub);
    await (Octra_core.Ledger.flush_dirty_lwt ledger);
    List.iter (fun (key, value) -> await (SI.set_meta store key value))
      ["last_epoch", string_of_int (epoch - 1); "current_epoch", string_of_int epoch;
       "total_supply", "1"; "emission_remaining", "0"];
    await (SI.commit_epoch_batch store "rule startup sample");
    let ledger_state_root = Option.get (await (SI.get_head_hash store)) in
    let irmin_commit = await (SI.get_commit_hash store) in
    let index_root = hash 'd' and index_hash = hash 'e' in
    let floor_root = Eic.folded_state_root ~ledger_state_root ~epoch_index_root:index_root in
    let header = C.{proto_version = proto_version_current; chain_id;
      epoch_id = Int64.of_int floor_epoch; prev_state_root = String.make 32 '\000';
      tx_list_hash = H.tx_list_hash []; receipt_root = H.receipt_root [];
      proposed_state_root = raw floor_root; parent_commit_hash = Octra_net.Hash_domain.nil_hash;
      creator_addr = address; txid_hi = 0L; ts = 1.} in
    let proposal_id = H.proposal_id header in
    let vote = C.{chain_id; epoch_id = header.epoch_id; round = 0; vote_type = Precommit;
      proposal_id; validator = address; signature = String.make 64 '\000'} in
    let vote = {vote with C.signature = H.sign_ed25519 ~priv_raw:private_raw ~msg:(H.vote_sign_bytes vote)} in
    let certificate = C.{chain_id; epoch_id = header.epoch_id; commit_round = 0;
      header; proposal_id; precommits = [vote]} in
    let floor = Octra_core.History_floor.create ~chain_id ~epoch:floor_epoch
      ~state_root:floor_root ~ledger_state_root ~txid_hi:0L ~config_hash
      ~validator_set_hash:(Octra_consensus.C_config.validator_set_hash validators |> hex)
      ~epoch_index_hash:index_hash ~epoch_index_root:index_root
      ~parent_commit:C.{certificate; validator_set = validators} |> get in
    get (SC.seed_history_floor data floor);
    let rec append id previous index last_hash anchor =
      if id >= epoch then previous, index, last_hash, anchor
      else
        let epoch_hash, root = Eic.next_root ~prev:index ~epoch_id:id [] in
        let computed = Eic.folded_state_root ~ledger_state_root ~epoch_index_root:root in
        let state_root = if id = plan.anchor_epoch && mode = "before"
          then plan.anchor_state_root else computed in
        let anchor = if id = plan.anchor_epoch then Some state_root else anchor in
        SC.set_epoch data {EL.empty_epoch_header with id; state_root;
          prev_state_root = previous; start_txid = 1L; tx_count = 0;
          finalized_by = address; finalized_at = 1.};
        SC.set_epoch_index_commitment data ~epoch_id:id ~epoch_hash ~root;
        append (id + 1) state_root root epoch_hash anchor in
    SC.begin_batch data;
    let state_root, root, epoch_hash, anchor =
      append (floor_epoch + 1) floor_root index_root index_hash None in
    SC.commit_batch data;
    SC.fsync data;
    let txlog_seg, txlog_off = SC.txlog_position data in
    HM.atomic_write dir {HM.schema_version = HM.schema_version; generation = epoch - 1;
      epoch_id = epoch - 1; state_root; ledger_state_root = Some ledger_state_root;
      irmin_commit; txid_hi = 0L; txlog_seg = Some txlog_seg; txlog_off = Some txlog_off;
      epochlog_off = Some (SC.epochlog_offset data); commit_id = "rule-start"; ts = 1.;
      quorum_cert_hash = None; epoch_index_hash = Some epoch_hash; epoch_index_root = Some root};
    let checked = await (Octra_core.Startup_recovery.recover ~data_dir:dir ~chaindata:data ~store) in
    assert_msg (checked.boundary_ok && checked.eic_checked && checked.eic_ok)
      "rule store recovery refused";
    assert_msg (mode <> "wrong" || anchor <> Some plan.anchor_state_root)
      "wrong anchor unexpectedly matches";
    Yojson.Safe.to_string (`Assoc ["epoch", `Int epoch; "activation", `Int plan.activation_epoch;
      "anchor_epoch", `Int plan.anchor_epoch; "expected", `String plan.anchor_state_root;
      "actual", (match anchor with None -> `Null | Some value -> `String value);
      "validators", `String (address ^ ":" ^ pub); "config_hash", `String config_hash])
    |> print_endline)

let () =
  Random.init 11;
  match Array.to_list Sys.argv with
  | [_] ->
    test_recovery_checks_eic ();
    print_endline "status = pass test = startup_eic"
  | [_; "--rule-store"; dir; mode; offset] -> rule_store dir mode (int_of_string offset)
  | _ -> failwith "usage: test_startup_eic [--rule-store data mode offset]"