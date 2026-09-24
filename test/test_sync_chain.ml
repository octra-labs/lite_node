(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Chain = Octra_node_runtime.Sync_chain
module Journal = Octra_node_runtime.Consensus_finality_journal
module Anchor = Octra_bootstrap.Sync_anchor
module Checkpoint = Octra_bootstrap.State_sync_checkpoint
module Manifest = Octra_bootstrap.State_sync_manifest
module Irmin = Octra_core.Store_irmin
module Store = Octra_core.Store_chaindata
module Update = Octra_core.Validator_set_update
module C = Octra_consensus.C_types
module H = Octra_consensus.C_hash

let get = function
  | Ok value -> value
  | Error reason -> failwith reason

let check label value =
  if not value then failwith label

let sha value = Digestif.SHA256.(digest_string value |> to_hex)
let raw value = Digestif.SHA256.(digest_string value |> to_raw_string)

let private_key = String.make 32 '\123'

let wallet =
  let key = Mirage_crypto_ec.Ed25519.priv_of_octets private_key
    |> Result.map_error (fun _ -> "invalid test key") |> get in
  let pub = Mirage_crypto_ec.Ed25519.(pub_of_priv key |> pub_to_octets)
    |> Base64.encode_exn in
  Octra_core.Crypto.Wallet.{
    address = Octra_core.Crypto.Address.address_from_pubkey pub;
    pub;
    priv = Base64.encode_exn private_key;
  }

let trusted = C.make_validator_set [C.{ address = wallet.address; pubkey = wallet.pub }]
let initial = get (Anchor.raw_validator_set trusted)
let chain_id = "octra-sync-test"

let with_store path action =
  let store = Lwt_main.run (Irmin.open_store ~fresh:true path) in
  Fun.protect ~finally:(fun () -> Lwt_main.run (Irmin.close store))
    (fun () -> action store)

let set store key value =
  Lwt_main.run (Irmin.set_meta store key value)

let proof store epoch key =
  Lwt_main.run (Irmin.tag_epoch store epoch);
  get (Lwt_main.run (Irmin.merkle_proof_at_epoch store epoch ["meta"; key]))

let update index =
  let epoch = 3 * index in
  let member = Octra_core.Validator_admission.{
    address = wallet.address;
    pubkey = Base64.decode_exn wallet.pub;
    weight = Z.of_int (index + 1);
  } in
  get (Update.make_weighted ~source_epoch:(Int64.of_int (epoch - 1))
    ~activate_epoch:(Int64.of_int (epoch + 1)) [member])

let finality epoch validator_set (proof : Irmin.merkle_proof) =
  let epoch_id = Int64.of_int epoch in
  let index_root = sha ("index-" ^ string_of_int epoch) in
  let state_root = Octra_core.Epoch_index_commitment.folded_state_root
    ~ledger_state_root:proof.ledger_state_root ~epoch_index_root:index_root in
  let header = C.{
    proto_version = proto_version_current;
    chain_id;
    epoch_id;
    prev_state_root = raw "parent";
    tx_list_hash = H.tx_list_hash [];
    receipt_root = H.receipt_root [];
    proposed_state_root = Digestif.SHA256.(of_hex state_root |> to_raw_string);
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = wallet.address;
    txid_hi = 0L;
    ts = float_of_int epoch;
  } in
  let proposal_id = H.proposal_id header in
  let vote = C.{
    chain_id; epoch_id; round = 0; vote_type = Precommit; proposal_id;
    validator = wallet.address; signature = String.make 64 '\000';
  } in
  let vote = C.{ vote with signature = H.sign_ed25519 ~priv_raw:private_key
    ~msg:(H.vote_sign_bytes vote) } in
  let finalize = C.{
    chain_id; epoch_id; commit_round = 0; header; proposal_id;
    precommits = [vote]; parent_commit = None;
  } in
  get (Anchor.verify_finalize ~chain_id ~validator_set finalize);
  Journal.{ finalize; validator_set; bundle = None }, index_root

let certificate steps record index_root ledger_root =
  let finalize = record.Journal.finalize in
  let checkpoint = Checkpoint.{
    chain_id; epoch = finalize.epoch_id;
    state_root = Octra_node_runtime.Text.raw_to_hex finalize.header.proposed_state_root;
    ledger_state_root = ledger_root;
    txid_hi = 0L; config_hash = sha "config";
    validator_set_hash = Octra_consensus.C_config.validator_set_hash record.validator_set
      |> Octra_node_runtime.Text.raw_to_hex;
    quorum_cert_hash = Some (Anchor.finalize_hash finalize);
    epoch_index_hash = Some (sha "index");
    epoch_index_root = Some index_root;
    created_at = finalize.epoch_id;
    valid_until = Int64.add finalize.epoch_id 100_000L;
  } in
  let anchor = Anchor.make ~steps ~finalize ~validator_set:record.validator_set in
  let encoded = Anchor.encode anchor in
  ignore (get (Anchor.verify ~validator_set:trusted checkpoint encoded));
  let checkpoint_hash = get (Checkpoint.hash checkpoint) in
  let files = ["HEAD.json"; "ledger.dat"; Octra_bootstrap.Root_win.name; "state_root"]
    |> List.sort String.compare
    |> List.map (fun path -> Manifest.{
      path; size = 1L; sha256 = sha "x";
      chunks = [{ index = 0; offset = 0L; size = 1; sha256 = sha "x" }];
    }) in
  let manifest = Manifest.{
    checkpoint_hash; snapshot_id = checkpoint_hash; irmin_commit = None;
    chunk_size = chunk_size_min; total_size = 4L; file_count = 4;
    chunk_count = 4; chunks_root = chunks_root files; files;
  } in
  let certificate = Manifest.{
    checkpoint; checkpoint_hash; authority = Finalized encoded; manifest;
    manifest_hash = get (manifest_hash manifest);
    exporter_signatures = [get (make_exporter_signature ~wallet manifest)];
  } in
  get (Manifest.verify_certificate ~validator_set:trusted ~exporter_set:trusted certificate)

let saved path =
  with_store path (fun store ->
    let rec loop index validator_set acc =
      if index > Anchor.max_steps then List.rev acc, validator_set
      else
        let value = update index in
        let encoded = Update.to_string value in
        set store Update.pending_meta_key encoded;
        let proof = proof store (3 * index) Update.pending_meta_key in
        let record, index_root = finality (3 * index) validator_set proof in
        let step = Anchor.{
          source = Pending; finalize = record.finalize;
          ledger_state_root = proof.ledger_state_root;
          epoch_index_root = index_root; update = encoded; proof = proof.proof;
        } in
        set store Update.active_meta_key encoded;
        loop (index + 1) (get (Update.validator_set value)) (step :: acc)
    in
    let steps, validator_set = loop 1 initial [] in
    let epoch = 3 * Anchor.max_steps + 2 in
    let proof = proof store epoch Update.active_meta_key in
    let record, index_root = finality epoch validator_set proof in
    certificate steps record index_root proof.ledger_state_root)

let test_limit () =
  Test_workspace.with_dir "sync_chain" (fun root ->
    let saved = saved (Filename.concat root "history") in
    let old_epoch = Int64.to_int saved.Manifest.checkpoint.epoch in
    let path = Filename.concat root "certificate.json" in
    Manifest.write_json path (Manifest.certificate_json saved);
    with_store (Filename.concat root "current") (fun store ->
      let chaindata = Store.open_chaindata (Filename.concat root "chaindata") in
      Fun.protect ~finally:(fun () -> Store.close chaindata) (fun () ->
        let old = update Anchor.max_steps in
        let old_raw = Update.to_string old in
        let old_set = get (Update.validator_set old) in
        set store Update.active_meta_key old_raw;
        set store Update.pending_meta_key old_raw;
        let old_proof = proof store old_epoch Update.active_meta_key in
        let old_record, old_index = finality old_epoch old_set old_proof in
        check "restored ledger root differs"
          (old_proof.ledger_state_root = saved.checkpoint.ledger_state_root);
        let entries = ref [old_record, old_index] in
        let deps = Chain.{
          data_dir = root; chain_id; store; chaindata;
          certificate_path = (fun () -> path);
          read_finality = (fun epoch ->
            match List.find_opt (fun (record, _) -> record.Journal.finalize.epoch_id = epoch) !entries with
            | Some (record, _) -> Journal.Valid record
            | None -> Journal.Missing);
        } in
        let build epoch validator_set = Lwt_main.run
          (Chain.build deps ~head_epoch:(Int64.of_int epoch) trusted validator_set) in
        check "stored chain at limit refused"
          (List.length (get (build old_epoch old_set)) = Anchor.max_steps);
        let next = update (Anchor.max_steps + 1) in
        let next_raw = Update.to_string next in
        let next_set = get (Update.validator_set next) in
        let epoch = old_epoch + 1 in
        set store Update.pending_meta_key next_raw;
        let pending = proof store epoch Update.pending_meta_key in
        let next_record, next_index = finality epoch old_set pending in
        entries := (next_record, next_index) :: !entries;
        List.iter (fun (record, root) ->
          Store.set_epoch_index_commitment_direct chaindata
            ~epoch_id:(Int64.to_int record.Journal.finalize.epoch_id)
            ~epoch_hash:(sha "index") ~root) !entries;
        set store Update.active_meta_key next_raw;
        let target = epoch + 2 in
        let current = proof store target Update.active_meta_key in
        let record, index_root = finality target next_set current in
        let steps = get (build target next_set) in
        check "stored extension exceeds limit instead of retrying trusted base"
          (List.length steps = 2);
        let compressed = certificate steps record index_root current.ledger_state_root in
        Manifest.write_json path (Manifest.certificate_json compressed);
        let reloaded = get (Manifest.load_certificate path) in
        ignore (get (Manifest.verify_certificate ~validator_set:trusted
          ~exporter_set:trusted reloaded));
        check "restarted chain changed" (get (build target next_set) = steps);
        Manifest.write_json path (Manifest.certificate_json saved);
        let invalid = Journal.{ old_record with
          finalize = C.{ old_record.finalize with
            precommits = List.map (fun (vote : C.vote) ->
              C.{ vote with signature = String.make 64 '\000' })
              old_record.finalize.precommits;
          };
        } in
        entries := [next_record, next_index; invalid, old_index];
        begin match build target next_set with
        | Error _ -> ()
        | Ok _ -> failwith "invalid bridge signature was accepted"
        end;
        entries := [next_record, next_index];
        begin match build target next_set with
        | Error _ -> ()
        | Ok _ -> failwith "missing short proof was accepted"
        end;
        let unchanged = get (Manifest.load_certificate path) in
        check "failed build replaced saved certificate"
          (Manifest.certificate_json unchanged = Manifest.certificate_json saved))))

let () =
  test_limit ();
  print_endline "status = pass test = sync_chain"