(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Checkpoint = Octra_bootstrap.State_sync_checkpoint
module Manifest = Octra_bootstrap.State_sync_manifest
module Journal = Octra_bootstrap.State_sync_journal
module Sync_anchor = Octra_bootstrap.Sync_anchor
module Verify = Octra_bootstrap.State_sync_verify
module C_config = Octra_consensus.C_config
module C_hash = Octra_consensus.C_hash
module C_types = Octra_consensus.C_types

let fail message =
  failwith ("test_state_sync_manifest: " ^ message)

let expect_ok = function
  | Ok value -> value
  | Error reason -> fail reason

let expect_error = function
  | Error _ -> ()
  | Ok _ -> fail "expected error"

let sha value =
  Digestif.SHA256.(digest_string value |> to_hex)

let raw_sha value =
  Digestif.SHA256.(digest_string value |> to_raw_string)

let raw_hex value =
  String.init 32 (fun index ->
    Char.chr (int_of_string ("0x" ^ String.sub value (index * 2) 2)))

let wallet seed =
  let private_raw = String.make 32 (Char.chr seed) in
  let private_key =
    match Mirage_crypto_ec.Ed25519.priv_of_octets private_raw with
    | Ok key -> key
    | Error _ -> fail "private key creation failed"
  in
  let public_key =
    Mirage_crypto_ec.Ed25519.pub_of_priv private_key
    |> Mirage_crypto_ec.Ed25519.pub_to_octets
    |> Base64.encode_exn
  in
  let address = Octra_core.Crypto.Address.address_from_pubkey public_key in
  Octra_core.Crypto.Wallet.{
    priv = Base64.encode_exn private_raw;
    pub = public_key;
    address;
  }

let signer_set wallets =
  wallets
  |> List.map (fun wallet ->
    Octra_consensus.C_types.{
      address = wallet.Octra_core.Crypto.Wallet.address;
      pubkey = wallet.pub;
    })
  |> Octra_consensus.C_types.make_validator_set

let raw_signer_set wallets =
  wallets
  |> List.map (fun wallet ->
    C_types.{
      address = wallet.Octra_core.Crypto.Wallet.address;
      pubkey = Base64.decode_exn wallet.pub;
    })
  |> C_types.make_validator_set

let sample_checkpoint validators =
  Checkpoint.{
    chain_id = "octra-devnet-bft";
    epoch = 123L;
    state_root = sha "state";
    ledger_state_root =
      "ffb10d4fe86ad4254806e180b11835cfee1f31376f0b0548ff715b329941fb9377435fc8998ec8d01547bfd0313accfb0f670d5a9efaba30edd4e521bafb0c2f";
    txid_hi = 99L;
    config_hash = sha "config";
    validator_set_hash = Manifest.set_hash validators;
    quorum_cert_hash = Some (sha "qc");
    epoch_index_hash = Some (sha "index");
    epoch_index_root = Some (sha "index-root");
    created_at = 1_000L;
    valid_until = 2_000L;
  }

let manifest_of_paths checkpoint_hash paths =
  let files =
    List.map (fun path ->
      let payload = "state-sync-payload:" ^ path in
      let chunk = Manifest.{
        index = 0;
        offset = 0L;
        size = String.length payload;
        sha256 = sha payload;
      } in
      Manifest.{
        path;
        size = Int64.of_int (String.length payload);
        sha256 = sha payload;
        chunks = [chunk];
      }) paths
  in
  let total_size =
    List.fold_left (fun total file -> Int64.add total file.Manifest.size) 0L files
  in
  let initial = Manifest.{
    checkpoint_hash;
    snapshot_id = checkpoint_hash;
    irmin_commit = Some (String.make 128 'a');
    chunk_size = Manifest.chunk_size_min;
    total_size;
    file_count = List.length files;
    chunk_count = List.length files;
    chunks_root = String.make 64 '0';
    files;
  } in
  { initial with chunks_root = Manifest.chunks_root initial.files }

let sample_manifest checkpoint_hash =
  manifest_of_paths checkpoint_hash [
    "HEAD.json";
    "chaindata/epochlog/epochs.dat";
    "chaindata/txlog/seg000000.dat";
    "irmin_store/index/log";
    "irmin_store/store.0.suffix";
    "irmin_store/store.branches";
    "irmin_store/store.control";
    "irmin_store/store.dict";
    "state_root";
  ]

let sample_reference_manifest checkpoint_hash =
  manifest_of_paths checkpoint_hash [
    "HEAD.json";
    "ledger.dat";
    "ready_roots";
    "state_root";
  ]

let signed_vote wallet (header : C_types.epoch_header) proposal_id =
  let unsigned = C_types.{
    chain_id = header.chain_id;
    epoch_id = header.epoch_id;
    round = 2;
    vote_type = Precommit;
    proposal_id;
    validator = wallet.Octra_core.Crypto.Wallet.address;
    signature = String.make 64 '\000';
  } in
  let private_raw = Base64.decode_exn wallet.priv in
  C_types.{
    unsigned with
    signature =
      C_hash.sign_ed25519
        ~priv_raw:private_raw
        ~msg:(C_hash.vote_sign_bytes unsigned);
  }

let finalized_certificate wallets =
  let validators = signer_set wallets in
  let active = raw_signer_set wallets in
  let exporters = signer_set [List.hd wallets] in
  let ledger_root =
    "ffb10d4fe86ad4254806e180b11835cfee1f31376f0b0548ff715b329941fb9377435fc8998ec8d01547bfd0313accfb0f670d5a9efaba30edd4e521bafb0c2f"
  in
  let index_root = sha "index-root" in
  let state_root =
    Octra_core.Epoch_index_commitment.folded_state_root
      ~ledger_state_root:ledger_root
      ~epoch_index_root:index_root
  in
  let header = C_types.{
    proto_version = proto_version_current;
    chain_id = "octra-devnet-bft";
    epoch_id = 123L;
    prev_state_root = raw_sha "finalized-parent";
    tx_list_hash = C_hash.tx_list_hash [];
    receipt_root = C_hash.receipt_root [];
    proposed_state_root = raw_hex state_root;
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = (List.hd wallets).Octra_core.Crypto.Wallet.address;
    txid_hi = 99L;
    ts = 1_000.;
  } in
  let proposal_id = C_hash.proposal_id header in
  let finalize = C_types.{
    chain_id = header.chain_id;
    epoch_id = header.epoch_id;
    commit_round = 2;
    header;
    proposal_id;
    precommits =
      wallets
      |> List.filteri (fun index _ -> index < 4)
      |> List.map (fun wallet -> signed_vote wallet header proposal_id);
    parent_commit = None;
  } in
  let checkpoint = Checkpoint.{
    chain_id = header.chain_id;
    epoch = header.epoch_id;
    state_root;
    ledger_state_root = ledger_root;
    txid_hi = header.txid_hi;
    config_hash = sha "config";
    validator_set_hash =
      C_config.validator_set_hash active
      |> Checkpoint.raw_to_hex;
    quorum_cert_hash = Some (Sync_anchor.finalize_hash finalize);
    epoch_index_hash = Some (sha "index");
    epoch_index_root = Some index_root;
    created_at = 1_000L;
    valid_until = 2_000L;
  } in
  let checkpoint_hash = expect_ok (Checkpoint.hash checkpoint) in
  let manifest = sample_reference_manifest checkpoint_hash in
  let manifest_digest = expect_ok (Manifest.manifest_hash manifest) in
  let finality_blob =
    Sync_anchor.make ~steps:[] ~finalize ~validator_set:active
    |> Sync_anchor.encode
  in
  let exporter_signature =
    expect_ok (Manifest.make_exporter_signature ~wallet:(List.hd wallets) manifest)
  in
  validators, active, finalize, exporters, Manifest.{
    checkpoint;
    checkpoint_hash;
    authority = Finalized finality_blob;
    manifest;
    manifest_hash = manifest_digest;
    exporter_signatures = [exporter_signature];
  }

let certificate wallets exporter_count =
  let validators = signer_set wallets in
  let exporters = signer_set [List.hd wallets] in
  let checkpoint = sample_checkpoint validators in
  let checkpoint_hash = expect_ok (Checkpoint.hash checkpoint) in
  let manifest = sample_manifest checkpoint_hash in
  let manifest_digest = expect_ok (Manifest.manifest_hash manifest) in
  let quorum_signatures =
    wallets
    |> List.filteri (fun index _ -> index < 4)
    |> List.map (fun wallet ->
      expect_ok (Manifest.make_checkpoint_signature ~wallet checkpoint))
    |> List.sort (fun left right ->
      String.compare left.Checkpoint.signer right.signer)
  in
  let exporter_signature =
    if exporter_count = 1 then
      expect_ok (Manifest.make_exporter_signature ~wallet:(List.hd wallets) manifest)
    else
      expect_ok (Manifest.make_exporter_signature ~wallet:(List.nth wallets 1) manifest)
  in
  validators, exporters, Manifest.{
    checkpoint;
    checkpoint_hash;
    authority = Checkpoint_quorum quorum_signatures;
    manifest;
    manifest_hash = manifest_digest;
    exporter_signatures = [exporter_signature];
  }

let test_certificate () =
  let wallets = List.init 5 (fun index -> wallet (index + 1)) in
  let validators, exporters, valid = certificate wallets 1 in
  ignore (expect_ok (Manifest.verify_certificate
    ~validator_set:validators
    ~exporter_set:exporters
    valid));
  expect_error (Manifest.verify_reference_certificate
    ~validator_set:validators
    ~exporter_set:exporters
    valid);
  let checkpoint_signatures = Manifest.checkpoint_signatures valid in
  let short = {
    valid with
    authority = Checkpoint_quorum (List.filteri
      (fun index _ -> index < 3)
      checkpoint_signatures);
  } in
  expect_error (Manifest.verify_certificate
    ~validator_set:validators
    ~exporter_set:exporters
    short);
  let duplicate =
    match checkpoint_signatures with
    | first :: rest -> {
        valid with
        authority = Checkpoint_quorum (first :: first :: rest);
      }
    | [] -> fail "checkpoint signatures missing"
  in
  expect_error (Manifest.verify_certificate
    ~validator_set:validators
    ~exporter_set:exporters
    duplicate);
  let _, _, wrong_exporter = certificate wallets 0 in
  expect_error (Manifest.verify_certificate
    ~validator_set:validators
    ~exporter_set:exporters
    wrong_exporter);
  let mutated = {
    valid with
    checkpoint = { valid.checkpoint with epoch = 124L };
  } in
  expect_error (Manifest.verify_certificate
    ~validator_set:validators
    ~exporter_set:exporters
    mutated)

let test_reference_format () =
  let wallets = List.init 5 (fun index -> wallet (index + 1)) in
  let _, _, certificate = certificate wallets 1 in
  let encoded = Manifest.certificate_json certificate in
  begin
    match encoded with
    | `Assoc fields ->
        begin
          match List.assoc_opt "version" fields with
          | Some (`String "octra-state-sync") -> ()
          | _ -> fail "state sync format changed"
        end
    | _ -> fail "certificate JSON is not an object"
  end;
  ignore (expect_ok (Manifest.parse_certificate_json encoded));
  let renamed =
    match encoded with
    | `Assoc fields ->
        `Assoc (
          ("version", `String "unsupported-format")
          :: List.remove_assoc "version" fields)
    | _ -> fail "certificate JSON is not an object"
  in
  expect_error (Manifest.parse_certificate_json renamed)

let test_finalized_anchor () =
  let wallets = List.init 5 (fun index -> wallet (index + 1)) in
  let validators, active, finalize, exporters, certificate =
    finalized_certificate wallets
  in
  ignore (expect_ok (Manifest.verify_certificate
    ~validator_set:validators
    ~exporter_set:exporters
    certificate));
  ignore (expect_ok (Manifest.verify_reference_certificate
    ~validator_set:validators
    ~exporter_set:exporters
    certificate));
  let encoded_set = expect_ok (Sync_anchor.encoded_validator_set active) in
  if C_config.validator_set_hash encoded_set = C_config.validator_set_hash active then
    fail "finality validator encoding did not change representation";
  let encoded = Option.get (Manifest.finality certificate) in
  ignore (expect_ok (Sync_anchor.verify
    ~validator_set:validators
    certificate.checkpoint
    encoded));
  let other_validators =
    List.init 5 (fun index -> wallet (index + 11))
    |> signer_set
  in
  expect_error (Sync_anchor.verify
    ~validator_set:other_validators
    certificate.checkpoint
    encoded);
  expect_error (Sync_anchor.verify
    ~validator_set:validators
    { certificate.checkpoint with state_root = sha "other-state" }
    encoded);
  expect_error (Sync_anchor.verify
    ~validator_set:validators
    { certificate.checkpoint with ledger_state_root = String.make 128 'b' }
    encoded);
  expect_error (Sync_anchor.verify
    ~validator_set:validators
    { certificate.checkpoint with epoch_index_root = Some (sha "other-index") }
    encoded);
  expect_error (Sync_anchor.verify
    ~validator_set:validators
    { certificate.checkpoint with txid_hi = 100L }
    encoded);
  expect_error (Sync_anchor.verify
    ~validator_set:validators
    { certificate.checkpoint with validator_set_hash = sha "other-set" }
    encoded);
  expect_error (Sync_anchor.verify
    ~validator_set:validators
    { certificate.checkpoint with quorum_cert_hash = Some (sha "other-qc") }
    encoded);
  ignore (expect_ok (Sync_anchor.decode encoded));
  let first_vote = List.hd finalize.precommits in
  let duplicate =
    Sync_anchor.make
      ~steps:[]
      ~finalize:{
        finalize with
        precommits = first_vote :: finalize.precommits;
      }
      ~validator_set:active
  in
  expect_error (Sync_anchor.verify
    ~validator_set:validators
    certificate.checkpoint
    (Sync_anchor.encode duplicate));
  let invalid_vote = C_types.{ first_vote with signature = String.make 64 '\000' } in
  let invalid =
    Sync_anchor.make
      ~steps:[]
      ~finalize:{
        finalize with
        precommits = invalid_vote :: List.tl finalize.precommits;
      }
      ~validator_set:active
  in
  expect_error (Sync_anchor.verify
    ~validator_set:validators
    certificate.checkpoint
    (Sync_anchor.encode invalid));
  let different_active = raw_signer_set (List.init 5 (fun index -> wallet (index + 11))) in
  let foreign =
    Sync_anchor.make ~steps:[] ~finalize ~validator_set:different_active
  in
  expect_error (Sync_anchor.verify
    ~validator_set:validators
    certificate.checkpoint
    (Sync_anchor.encode foreign));
  let trailing =
    Base64.decode_exn encoded ^ "x"
    |> Base64.encode_exn
  in
  expect_error (Sync_anchor.decode trailing)

let test_manifest_shape () =
  let wallets = List.init 5 (fun index -> wallet (index + 1)) in
  let checkpoint = sample_checkpoint (signer_set wallets) in
  ignore (expect_ok (Checkpoint.validate checkpoint));
  let legacy = { checkpoint with ledger_state_root = checkpoint.state_root } in
  ignore (expect_ok (Checkpoint.validate legacy));
  let invalid_root = { checkpoint with ledger_state_root = "" } in
  expect_error (Checkpoint.validate invalid_root);
  let missing_index = { checkpoint with epoch_index_hash = None; epoch_index_root = None } in
  expect_error (Checkpoint.validate missing_index);
  let checkpoint_hash = expect_ok (Checkpoint.hash checkpoint) in
  let manifest = sample_reference_manifest checkpoint_hash in
  ignore (expect_ok (Manifest.validate_reference_body manifest));
  ignore
    (expect_ok
       (Manifest.validate_reference_body
          (manifest_of_paths checkpoint_hash [
             "HEAD.json";
             "ledger.dat";
             "pvac/migration_state.json";
             "ready_roots";
             "state_root";
           ])));
  ignore
    (expect_ok
       (Manifest.validate_reference_body { manifest with irmin_commit = None }));
  let replace_path source target =
    let files =
      manifest.files
      |> List.map (fun file ->
        if file.Manifest.path = source then { file with path = target }
        else file)
      |> List.sort (fun left right -> String.compare left.Manifest.path right.path)
    in
    { manifest with files; chunks_root = Manifest.chunks_root files }
  in
  expect_error
    (Manifest.validate_reference_body
       (replace_path "ledger.dat" "chaindata/index/data.mdb"));
  expect_error
    (Manifest.validate_reference_body
       (replace_path "ledger.dat" "chaindata/txlog/seg000000.dat"));
  expect_error
    (Manifest.validate_reference_body
       (replace_path "ledger.dat" "chaindata/epochlog/epochs.dat"));
  expect_error
    (Manifest.validate_reference_body
       (replace_path "ledger.dat" "preverify_receipts/epoch_1.json"));
  expect_error
    (Manifest.validate_reference_body
       (replace_path "ledger.dat" "pvac/noise.kat"));
  expect_error (Manifest.validate_body { manifest with snapshot_id = "operator-name" });
  let file = List.hd manifest.files in
  expect_error (Manifest.validate_body {
    manifest with
    files = [{ file with path = "../HEAD.json" }];
  });
  let chunk = List.hd file.chunks in
  expect_error (Manifest.validate_body {
    manifest with
    files = [{ file with chunks = [{ chunk with offset = 1L }] }];
  });
  let json =
    match Manifest.body_json manifest with
    | `Assoc fields -> `Assoc (("extra", `Bool true) :: fields)
    | _ -> fail "manifest JSON is not an object"
  in
  expect_error (Manifest.parse_body json)

let test_hash32_encoder () =
  let buffer = Buffer.create 64 in
  Buffer.add_string buffer "prefix";
  begin
    match Octra_net.Oce1.put_hash32_checked buffer (String.make 64 'g') with
    | Error _ -> ()
    | Ok () -> fail "non-hex hash32 was accepted"
  end;
  if Buffer.contents buffer <> "prefix" then
    fail "rejected hash32 changed output buffer";
  begin
    match Octra_net.Oce1.hash32_of_hex (String.make 63 'a') with
    | Error _ -> ()
    | Ok _ -> fail "short hash32 was accepted"
  end;
  expect_error (Octra_net.Oce1.hash32_of_hex (String.make 64 'A'));
  let valid = String.make 64 'a' in
  ignore (expect_ok (Octra_net.Oce1.put_hash32_checked buffer valid));
  if Buffer.length buffer <> 38 then fail "valid hash32 encoded with wrong length";
  begin
    match Octra_net.Oce1.put_sig64_checked buffer (String.make 63 's') with
    | Error _ -> ()
    | Ok () -> fail "short sig64 was accepted"
  end;
  if Buffer.length buffer <> 38 then fail "rejected sig64 changed output buffer";
  ignore (expect_ok (Octra_net.Oce1.put_sig64_checked buffer (String.make 64 's')));
  if Buffer.length buffer <> 102 then fail "valid sig64 encoded with wrong length"

let test_historical_anchor () =
  let validators = signer_set (List.init 5 (fun index -> wallet (index + 1))) in
  let index_hash = sha "historical-index" in
  let index_root = sha "historical-root" in
  let ledger_root =
    "ffb10d4fe86ad4254806e180b11835cfee1f31376f0b0548ff715b329941fb9377435fc8998ec8d01547bfd0313accfb0f670d5a9efaba30edd4e521bafb0c2f"
  in
  let state_root =
    Octra_core.Epoch_index_commitment.folded_state_root
      ~ledger_state_root:ledger_root
      ~epoch_index_root:index_root
  in
  let checkpoint = Checkpoint.{
    (sample_checkpoint validators) with
    state_root;
    ledger_state_root = ledger_root;
    epoch_index_hash = Some index_hash;
    epoch_index_root = Some index_root;
  } in
  let proposer = Octra_core.Epochlog.{
    creator_addr = "oct6wMiXWiH5SKkfpXC9RvGEBTyPPMN9NgQxVWPDwjhmtcZ";
    commit_round = 2;
  } in
  let header = Octra_core.Epochlog.{
    empty_epoch_header with
    id = 123;
    state_root;
    start_txid = 97L;
    tx_count = 3;
    proposer;
  } in
  let finality =
    Octra_consensus.Finality_log.make
      ~height:123
      ~round:2
      ~proposal_id:(sha "proposal")
      ~tx_list_hash:(sha "transactions")
      ~state_root
      ~creator_addr:proposer.creator_addr
      ~txid_hi:99L
      ~qc_hash:(sha "qc")
      ~ts:1_000.0
      ()
  in
  let status = Octra_core.Store_chaindata.{
    eic_epoch_id = 123;
    eic_stored_hash = Some index_hash;
    eic_stored_root = Some index_root;
    eic_actual_hash = Some index_hash;
    eic_actual_root = Some index_root;
    eic_ok = true;
    eic_errors = [];
  } in
  ignore (expect_ok (Verify.verify_historical_anchor
    checkpoint header finality status));
  expect_error (Verify.verify_historical_anchor
    { checkpoint with txid_hi = 100L }
    header
    finality
    status);
  expect_error (Verify.verify_historical_anchor
    checkpoint
    header
    { finality with qc_hash = Some (sha "other-qc") }
    status)

let rec remove_tree path =
  try
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
        Sys.readdir path
        |> Array.iter (fun name ->
          if name <> "." && name <> ".." then
            remove_tree (Filename.concat path name));
        Unix.rmdir path
    | _ -> Unix.unlink path
  with Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let rec mkdir_p path =
  if path = "" || path = "." || Sys.file_exists path then ()
  else begin
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755
  end

let test_journal () =
  let root =
    Filename.concat
      "runtime_data"
      (Printf.sprintf "test_state_sync_manifest_%d" (Unix.getpid ()))
  in
  remove_tree root;
  mkdir_p root;
  let wallets = List.init 5 (fun index -> wallet (index + 1)) in
  let checkpoint = sample_checkpoint (signer_set wallets) in
  let manifest = sample_manifest (expect_ok (Checkpoint.hash checkpoint)) in
  let file = List.hd manifest.files in
  let chunk = List.hd file.chunks in
  let manifest_hash = expect_ok (Manifest.manifest_hash manifest) in
  let journal_path = Filename.concat root "journal.jsonl" in
  let journal =
    expect_ok (Journal.open_journal ~path:journal_path ~manifest_hash)
  in
  Journal.record_completed journal file.path chunk;
  if not (Journal.is_completed journal file.path chunk) then fail "journal completion missing";
  Journal.record_invalid journal file.path chunk;
  let reopened =
    expect_ok (Journal.open_journal ~path:journal_path ~manifest_hash)
  in
  if Journal.is_completed reopened file.path chunk then fail "journal drop missing";
  expect_error (Journal.open_journal ~path:journal_path ~manifest_hash:(sha "other"));
  let torn_path = Filename.concat root "torn.jsonl" in
  let torn = expect_ok (Journal.open_journal ~path:torn_path ~manifest_hash) in
  Journal.record_completed torn file.path chunk;
  let output = open_out_gen [Open_wronly; Open_append; Open_binary] 0o600 torn_path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr output)
    (fun () ->
      output_string output "{\"version\":";
      flush output;
      Unix.fsync (Unix.descr_of_out_channel output));
  let recovered = expect_ok (Journal.open_journal ~path:torn_path ~manifest_hash) in
  if not (Journal.is_completed recovered file.path chunk) then
    fail "journal did not ignore torn final entry";
  remove_tree root

let test_snapshot_roots () =
  let root =
    Filename.concat
      "runtime_data"
      (Printf.sprintf "test_state_sync_roots_%d" (Unix.getpid ()))
  in
  remove_tree root;
  let write path value =
    let parent = Filename.dirname path in
    let rec mkdir current =
      if current = "" || current = "." || Sys.file_exists current then ()
      else begin
        mkdir (Filename.dirname current);
        Unix.mkdir current 0o755
      end
    in
    mkdir parent;
    let output = open_out_bin path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr output)
      (fun () -> output_string output value)
  in
  write (Filename.concat root "HEAD.json") "{}";
  write (Filename.concat root "preverify_receipts/epoch_7.json") "{}";
  write (Filename.concat root "chaindata/index/lock.mdb") "lock";
  write (Filename.concat root "chaindata/index.bak_pre_cutover/data.mdb") "backup";
  write (Filename.concat root "irmin_store/store.control") "control";
  write (Filename.concat root "commit_journal.log") "journal";
  write (Filename.concat root "wallet.json") "secret";
  Unix.symlink
    "../wallet.json"
    (Filename.concat root "chaindata/wallet.json");
  let files = Octra_bootstrap.State_sync.list_files root in
  if not (List.mem "preverify_receipts/epoch_7.json" files) then
    fail "preverify receipts are missing from snapshot";
  if List.mem "chaindata/index/lock.mdb" files then fail "LMDB lock entered snapshot";
  if List.mem "chaindata/index.bak_pre_cutover/data.mdb" files then
    fail "backup index entered snapshot";
  if List.mem "irmin_store/store.control" files then fail "Irmin control entered snapshot";
  if List.mem "commit_journal.log" files then fail "commit journal entered snapshot";
  if List.mem "wallet.json" files then fail "wallet entered snapshot";
  if List.mem "chaindata/wallet.json" files then
    fail "wallet symlink entered snapshot";
  write (Filename.concat root ".ready.json") "{}";
  let sealed_files = Octra_bootstrap.State_sync.list_files root in
  if not (List.mem "irmin_store/store.control" sealed_files) then
    fail "sealed Irmin control is missing from snapshot";
  Unix.unlink (Filename.concat root ".ready.json");
  write (Filename.concat root ".compact-ready.json") "{}";
  let compact_files = Octra_bootstrap.State_sync.list_files root in
  if not (List.mem "irmin_store/store.control" compact_files) then
    fail "compact Irmin control is missing from snapshot";
  remove_tree root

let test_chunks () =
  let root =
    Filename.concat
      "runtime_data"
      (Printf.sprintf "test_state_sync_chunks_%d" (Unix.getpid ()))
  in
  remove_tree root;
  mkdir_p root;
  let chunk_size = Manifest.chunk_size_min in
  let path = Filename.concat root "payload.bin" in
  let output = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr output)
    (fun () ->
      output_string output (String.make chunk_size 'a');
      output_string output (String.make chunk_size 'b');
      output_string output (String.make 17 'c'));
  let _, chunks = Manifest.hash_file_chunks ~chunk_size path in
  begin
    match chunks with
    | [first; second; final] ->
        if first.index <> 0 || first.offset <> 0L || first.size <> chunk_size then
          fail "first chunk offset mismatch";
        if second.index <> 1
           || second.offset <> Int64.of_int chunk_size
           || second.size <> chunk_size then
          fail "second chunk offset mismatch";
        if final.index <> 2
           || final.offset <> Int64.of_int (2 * chunk_size)
           || final.size <> 17 then
          fail "final chunk offset mismatch"
    | _ -> fail "chunk count mismatch"
  end;
  remove_tree root

let test_hash_slices () =
  let root = Filename.concat "runtime_data"
      (Printf.sprintf "sync_hash_%d" (Unix.getpid ())) in
  mkdir_p root;
  let path = Filename.concat root "payload.bin" in
  let quantum = Manifest.hash_slice in
  let check chunk_size size =
    let body = String.init size (fun index -> Char.chr ((index * 17 + 31) land 255)) in
    let output = open_out_bin path in
    Fun.protect ~finally:(fun () -> close_out_noerr output)
      (fun () -> output_string output body);
    let slices = ref [] in
    let yield count = slices := count :: !slices in
    let digest, chunks = Manifest.hash_file_chunks ~yield ~chunk_size path in
    if digest <> sha body then fail "sliced file digest differs";
    let expected = List.init ((size + chunk_size - 1) / chunk_size) (fun index ->
      let offset = index * chunk_size in
      let size = min chunk_size (size - offset) in
      Manifest.{ index; offset = Int64.of_int offset; size;
        sha256 = sha (String.sub body offset size) }) in
    if chunks <> expected then fail "sliced chunk records differ";
    if List.fold_left ( + ) 0 !slices <> size
       || List.exists (fun count -> count <= 0 || count > quantum) !slices then
      fail "hash slice budget differs";
    let direct = Manifest.hash_file_chunks ~chunk_size path in
    if direct <> (digest, chunks) then fail "scheduler changes hash output"
  in
  Fun.protect ~finally:(fun () -> remove_tree root) (fun () ->
    List.iter (fun chunk_size ->
      List.iter (check chunk_size)
        [0; 1; quantum - 1; quantum; quantum + 1;
         chunk_size - 1; chunk_size; chunk_size + 1; 2 * chunk_size + 17])
      [quantum - 1; quantum + 1; Manifest.chunk_size_min; 16 * 1024 * 1024])

let test_large_manifest () =
  let root =
    Filename.concat
      "runtime_data"
      (Printf.sprintf "test_state_sync_large_%d" (Unix.getpid ()))
  in
  remove_tree root;
  mkdir_p root;
  let wallets = List.init 5 (fun index -> wallet (index + 1)) in
  let checkpoint = sample_checkpoint (signer_set wallets) in
  let checkpoint_hash = expect_ok (Checkpoint.hash checkpoint) in
  let count = 55_000 in
  let rec make_files index files =
    if index = count then List.rev files
    else
      let path = Printf.sprintf "chaindata/objects/%06d" index in
      let digest = sha path in
      let chunk = Manifest.{
        index = 0;
        offset = 0L;
        size = 1;
        sha256 = digest;
      } in
      let file = Manifest.{
        path;
        size = 1L;
        sha256 = digest;
        chunks = [chunk];
      } in
      make_files (index + 1) (file :: files)
  in
  let files = make_files 0 [] in
  let initial = Manifest.{
    checkpoint_hash;
    snapshot_id = checkpoint_hash;
    irmin_commit = Some (String.make 128 'a');
    chunk_size = Manifest.chunk_size_min;
    total_size = Int64.of_int count;
    file_count = count;
    chunk_count = count;
    chunks_root = String.make 64 '0';
    files;
  } in
  let manifest = {
    initial with
    chunks_root = Manifest.chunks_root files;
  } in
  let draft = Manifest.{
    checkpoint;
    checkpoint_hash;
    manifest;
    manifest_hash = expect_ok (Manifest.manifest_hash manifest);
  } in
  let path = Filename.concat root "draft.json" in
  Manifest.write_json path (Manifest.draft_json draft);
  ignore (expect_ok (Manifest.load_draft path) |> Manifest.verify_draft |> expect_ok);
  remove_tree root

let () =
  if not (Octra_bootstrap.State_sync.path_allowed "pvac/migration_state.json") then
    fail "migration state is excluded from state sync";
  test_certificate ();
  test_reference_format ();
  test_finalized_anchor ();
  test_manifest_shape ();
  test_hash32_encoder ();
  test_historical_anchor ();
  test_journal ();
  test_snapshot_roots ();
  test_chunks ();
  test_hash_slices ();
  test_large_manifest ();
  print_endline "test_state_sync_manifest: ok"