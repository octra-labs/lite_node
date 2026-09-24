(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module H = Octra_core.Claim_history
module T = Octra_core.Transaction
module FB = Octra_core.Crypto.FheBalance
module PT = Octra_core.Crypto.PrivateTransferV4
module SC = Octra_core.Crypto.StealthClaimV5
module SA = Octra_core.Crypto.StealthAddress
module P = Pvac_ffi
module R = Octra_core.Claim_archive
module S = Octra_core.Store_irmin
module C = Octra_core.Store_chaindata
module E = Octra_core.Epoch_index_commitment
module Chain = Octra_bootstrap.Claim_chain
module M = Octra_bootstrap.State_sync_manifest
module CP = Octra_bootstrap.State_sync_checkpoint
module Anchor = Octra_bootstrap.Sync_anchor
module F = Octra_consensus.C_types
module Hash = Octra_consensus.C_hash
module Sum = Octra_core.Claim_sum

let check name valid =
  if not valid then failwith ("test_claim_history: " ^ name)

let unwrap = function
  | Ok value -> value
  | Error reason -> failwith reason

let refused name = function
  | Error _ -> ()
  | Ok _ -> failwith ("accepted: " ^ name)

let rejected name reason = function
  | Error actual when actual = reason -> ()
  | Error actual -> failwith (Printf.sprintf "%s: expected %S, received %S" name reason actual)
  | Ok _ -> failwith ("accepted: " ^ name)

let bytes ch = Bytes.make 32 ch
let b64 value = Base64.encode_exn (Bytes.to_string value)
let hex value = SA.stealth_tag_to_hex (Bytes.to_string value)

let set name value = function
  | `Assoc fields -> `Assoc ((name, value) :: List.remove_assoc name fields)
  | _ -> failwith "object required"

let key pk =
  let blob = Bytes.to_string (P.serialize_pubkey pk) in
  H.{blob; hash = Octra_core.Pvac_registry.full_key_hash blob; math = false}

let identity seed =
  let secret = Bytes.to_string (bytes seed) in
  let sk = match Mirage_crypto_ec.Ed25519.priv_of_octets secret with
    | Ok value -> value
    | Error _ -> failwith "test signing key invalid" in
  let public = Mirage_crypto_ec.Ed25519.pub_of_priv sk
    |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn in
  Base64.encode_exn secret, public, Octra_core.Crypto.Address.address_from_pubkey public

let sender_secret, sender_public, sender = identity '\051'
let receiver_secret, receiver_public, receiver = identity '\052'

let sender_wallet = Octra_core.Crypto.Wallet.{
  priv = sender_secret; pub = sender_public; address = sender;
}

let receiver_wallet = Octra_core.Crypto.Wallet.{
  priv = receiver_secret; pub = receiver_public; address = receiver;
}

let chain_id = "octra-devnet-9871-cluster"
let digest value = Digestif.SHA256.(digest_string value |> to_hex)
let raw value = Digestif.SHA256.(of_hex value |> to_raw_string)

let members ?(encoded = false) (wallet : Octra_core.Crypto.Wallet.t) =
  let pubkey = if encoded then wallet.pub else Base64.decode_exn wallet.pub in
  F.make_validator_set [F.{address = wallet.address; pubkey}]

let finalized ?(version = F.proto_version_current) wallet parent (pin : R.pin) =
  let prev = match parent with
    | None -> String.make 32 '\000'
    | Some value -> value.F.certificate.header.proposed_state_root in
  let header = F.{
    proto_version = version; chain_id; epoch_id = Int64.of_int pin.epoch;
    prev_state_root = prev; tx_list_hash = Hash.tx_list_hash [];
    receipt_root = Hash.receipt_root []; proposed_state_root = raw pin.state_root;
    parent_commit_hash = Hash.parent_commit_hash_opt parent;
    creator_addr = wallet.Octra_core.Crypto.Wallet.address;
    txid_hi = Int64.pred pin.next_txid; ts = float_of_int pin.epoch;
  } in
  let proposal_id = Hash.proposal_id header in
  let vote = F.{chain_id; epoch_id = header.epoch_id; round = 0;
    vote_type = Precommit; proposal_id; validator = wallet.address; signature = ""} in
  let vote = {vote with signature = Hash.sign_ed25519
    ~priv_raw:(Base64.decode_exn wallet.priv) ~msg:(Hash.vote_sign_bytes vote)} in
  F.{chain_id; epoch_id = header.epoch_id; commit_round = 0; header;
    proposal_id; precommits = [vote]; parent_commit = parent}

let parent wallet finalize = F.{
  certificate = F.certificate_of_finalize finalize; validator_set = members wallet;
}

let certificate wallet finalize ledger_root index_root =
  let set = members wallet in
  let checkpoint = CP.{
    chain_id; epoch = finalize.F.epoch_id;
    state_root = CP.raw_to_hex finalize.header.proposed_state_root;
    ledger_state_root = ledger_root; txid_hi = finalize.header.txid_hi;
    config_hash = digest "claim-history-config";
    validator_set_hash = CP.raw_to_hex (Octra_consensus.C_config.validator_set_hash set);
    quorum_cert_hash = Some (Anchor.finalize_hash finalize);
    epoch_index_hash = Some (digest "index"); epoch_index_root = Some index_root;
    created_at = 1000L; valid_until = 2000L;
  } in
  let checkpoint_hash = CP.hash checkpoint |> unwrap in
  let files = ["HEAD.json"; "ledger.dat"; "ready_roots"; "state_root"]
    |> List.map (fun path ->
      let sha256 = digest path in
      M.{path; size = 1L; sha256;
        chunks = [{index = 0; offset = 0L; size = 1; sha256}]}) in
  let manifest = M.{checkpoint_hash; snapshot_id = checkpoint_hash;
    irmin_commit = Some ledger_root; chunk_size = chunk_size_min; total_size = 4L;
    file_count = 4; chunk_count = 4; chunks_root = chunks_root files; files} in
  let manifest_digest = M.manifest_hash manifest |> unwrap in
  let signature = M.make_exporter_signature ~wallet manifest |> unwrap in
  M.{checkpoint; checkpoint_hash; manifest; manifest_hash = manifest_digest;
    exporter_signatures = [signature];
    authority = Finalized (Anchor.make ~steps:[] ~finalize ~validator_set:set |> Anchor.encode)}

let chain_read ?(first_epoch = 0) ?(max_epochs = 2) wallet certificate read_finality =
  Chain.read ~chain_id ~config_hash:(digest "claim-history-config")
    ~validator_set:(members ~encoded:true wallet) ~exporter_set:(members ~encoded:true wallet)
    ~certificate ~first_epoch ~max_epochs ~read_finality

let chain_rules () =
  let rules = Octra_core.Rule_graph.create ~chain_id
    ~root_at:(fun _ -> Octra_core.Rule_graph.Missing) in
  let plan = Octra_core.Rule_graph.math_activation rules |> Option.get in
  let first_epoch = plan.anchor_epoch in
  let count = plan.activation_epoch - first_epoch + 2 in
  let ledger = String.make 128 'a' in
  let index = digest "math-index" in
  let state_root = E.folded_state_root ~ledger_state_root:ledger ~epoch_index_root:index in
  let build anchor_root =
    let initial = finalized sender_wallet None R.{epoch = first_epoch;
      state_root = anchor_root; index_root = index; next_txid = 1L} in
    let records = Array.make count initial in
    for offset = 1 to count - 1 do
      let prior_wallet = if offset = 1 then sender_wallet else receiver_wallet in
      records.(offset) <- finalized receiver_wallet
        (Some (parent prior_wallet records.(offset - 1)))
        R.{epoch = first_epoch + offset; state_root; index_root = index; next_txid = 1L}
    done;
    let cert = certificate receiver_wallet records.(count - 1) ledger index in
    chain_read ~first_epoch ~max_epochs:count receiver_wallet cert (fun epoch ->
      let offset = Int64.to_int epoch - first_epoch in
      if offset < 0 || offset >= count then Error "history gap"
      else Ok records.(offset)) |> unwrap
  in
  let chain = build plan.anchor_state_root in
  check "math before activation"
    (Chain.math chain ~epoch:(plan.activation_epoch - 1) = Ok false);
  check "math at activation"
    (Chain.math chain ~epoch:plan.activation_epoch = Ok true);
  check "math after activation"
    (Chain.math chain ~epoch:(plan.activation_epoch + 1) = Ok true);
  let altered = build (digest "different-anchor") in
  refused "math anchor differs" (Chain.math altered ~epoch:plan.activation_epoch);
  Printf.printf "event = claim_history phase = chain_rules status = pass\n%!"

let tx ?(nonce = 1) ?(amount = Z.zero) from to_ op_type payload =
  let secret, public = if from = sender then sender_secret, sender_public
    else if from = receiver then receiver_secret, receiver_public
    else failwith "unknown test signer" in
  let value = T.{
    from;
    to_;
    amount;
    nonce;
    ou = Z.of_int 5_000;
    timestamp = 1000.0;
    signature = "";
    public_key = Some public;
    message = None;
    op_type;
    encrypted_data = Some (Yojson.Safe.to_string payload);
  } in
  let signed = T.sign_with_privkey value secret in
  check "transaction signature" (T.verify signed public);
  signed

let entry epoch index tx =
  H.{epoch; index; hash = T.hash tx; json = Yojson.Safe.to_string (T.to_yojson tx)}

let send_entry payload = entry 10 100L (tx sender "stealth" T.StealthOp payload)
let claim_entry payload = entry 12 150L (tx receiver receiver T.ClaimOp payload)

let material () =
  let pk, sk = P.keygen_from_seed (P.default_params ()) (bytes '\001') in
  let rpk, rsk = P.keygen_from_seed (P.default_params ()) (bytes '\002') in
  let blind = bytes '\003' in
  let secret = bytes '\004' in
  let delta = P.enc_value_seeded pk sk 7L (bytes '\005') in
  let cipher = P.enc_value_seeded rpk rsk 7L (bytes '\006') in
  let balance = P.enc_value_seeded pk sk 93L (bytes '\014') in
  let transfer = PT.{
    version = 5;
    delta_cipher = FB.encode_cipher delta;
    commitment = b64 (P.commit_ct pk delta);
    range_proof_delta = FB.encode_bound_range_proof
      (P.make_zero_proof_bound_range pk sk delta 7L (bytes '\007'));
    range_proof_balance = FB.encode_bound_range_proof
      (P.make_zero_proof_bound_range pk sk balance 93L (bytes '\015'));
    eph_pub = b64 (bytes '\010');
    stealth_tag = String.make 32 'a';
    enc_amount = b64 (Bytes.make 68 '\011');
    claim_pub = SA.stealth_tag_to_hex
      (SA.compute_claim_pub (Bytes.to_string secret) receiver);
    amount_commitment = b64 (P.pedersen_commit_amount 7L blind);
    send_zero_proof = FB.encode_zero_proof
      (P.make_zero_proof_bound pk sk delta 7L blind);
  } in
  let receipt = SC.{
    version = 5;
    output_id = 3;
    claim_cipher = FB.encode_cipher cipher;
    commitment = b64 (P.commit_ct rpk cipher);
    claim_secret = hex secret;
    zero_proof = FB.encode_zero_proof
      (P.make_zero_proof_bound rpk rsk cipher 7L blind);
  } in
  let public pk sk amount seed =
    let cipher = P.enc_value_seeded pk sk amount (bytes seed) in
    let blinding = bytes '\024' in
    cipher, `Assoc [
      "cipher", `String (FB.encode_cipher cipher);
      "amount_commitment", `String (b64 (P.pedersen_commit_amount amount blinding));
      "zero_proof", `String (FB.encode_zero_proof
        (P.make_zero_proof_bound pk sk cipher amount blinding));
      "blinding", `String (b64 blinding);
    ] in
  let _, deposit = public pk sk 100L '\022' in
  let delta, withdraw = public rpk rsk 4L '\023' in
  let remaining = P.ct_sub rpk cipher delta in
  let withdraw = set "range_proof_balance" (`String (FB.encode_bound_range_proof
    (P.make_zero_proof_bound_range rpk rsk remaining 3L (bytes '\025')))) withdraw in
  key pk, key rpk, transfer, receipt, deposit, withdraw

let amount_case sender_key receiver_key deposit withdraw =
  let make from op amount payload =
    entry 1 0L (tx ~amount from from op payload) in
  let encrypt = make sender T.EncryptOp (Z.of_int 100) deposit in
  let decrypt = make receiver T.DecryptOp (Z.of_int 4) withdraw in
  check "public deposit proof" (H.amount ~op:T.EncryptOp encrypt sender_key = Ok (Z.of_int 100));
  check "public withdrawal proof" (H.amount ~op:T.DecryptOp decrypt receiver_key = Ok (Z.of_int 4));
  rejected "public amount substitution"
    "amount commitment mismatch: pedersen_commit(amount, blinding) != amount_commitment"
    (H.amount ~op:T.EncryptOp (make sender T.EncryptOp (Z.of_int 101) deposit) sender_key);
  rejected "public amount operation" "history operation has no public FHE amount"
    (H.amount ~op:T.Standard encrypt sender_key);
  rejected "public operation differs" "history operation differs"
    (H.amount ~op:T.DecryptOp encrypt sender_key);
  rejected "negative amount rejected by decoder" "Malformed JSON: missing or invalid fields"
    (H.amount ~op:T.EncryptOp (make sender T.EncryptOp Z.minus_one deposit) sender_key);
  rejected "public amount supply cap" "history public amount exceeds supply range"
    (H.amount ~op:T.EncryptOp
      (make sender T.EncryptOp (Z.succ Octra_core.Denomination.max_supply) deposit) sender_key);
  rejected "public payload missing field" "history string missing: cipher"
    (H.amount ~op:T.EncryptOp (make sender T.EncryptOp (Z.of_int 100) (`Assoc [])) sender_key);
  let duplicate = match deposit with
    | `Assoc fields -> `Assoc (("cipher", `String "other") :: fields)
    | _ -> failwith "public payload object required" in
  rejected "public payload duplicate" "duplicate history field"
    (H.amount ~op:T.EncryptOp (make sender T.EncryptOp (Z.of_int 100) duplicate) sender_key);
  rejected "public key digest" "history key hash differs"
    (H.amount ~op:T.EncryptOp encrypt {sender_key with H.hash = String.make 64 '0'});
  rejected "public target differs" "history public FHE operation is not self-directed"
    (H.amount ~op:T.EncryptOp
      (entry 1 0L (tx ~amount:(Z.of_int 100) sender receiver T.EncryptOp deposit)) sender_key)

let output (send : H.entry) (claim : H.entry) (transfer : PT.t) =
  `Assoc [
    "id", `Int 3;
    "epoch_id", `Int send.H.epoch;
    "claimed", `Int 1;
    "tx_hash", `String send.hash;
    "claim_tx_hash", `String claim.H.hash;
    "sender_addr", `String sender;
    "delta_cipher_stored", `String transfer.delta_cipher;
    "amount_commitment", `String transfer.amount_commitment;
    "claim_pub", `String transfer.claim_pub;
    "stealth_tag", `String transfer.stealth_tag;
    "eph_pub", `String transfer.eph_pub;
    "enc_amount", `String transfer.enc_amount;
  ]

type archive_mode = Valid | Bad_key | Bad_payload | Prior_output | Bad_amount

let unspent value = value
  |> set "claimed" (`Int 0)
  |> set "claim_tx_hash" (`String "")

let archive_write dir mode sender_key receiver_key transfer receipt =
    let path = Filename.concat dir "irmin_store" in
    let history = Filename.concat dir "chaindata" in
    let store = Lwt_main.run (S.open_store path) in
    let archive = C.open_chaindata history in
    let commit epoch =
      Lwt_main.run (S.set_meta store "last_epoch" (string_of_int epoch));
      Lwt_main.run (S.tag_epoch store epoch);
      let view = Lwt_main.run (S.capture_read_snapshot store) |> unwrap in
      view.S.state_root
    in
    let set_key addr key = Lwt_main.run (S.set_pvac_pubkey store addr key.H.blob) in
    Fun.protect
      ~finally:(fun () -> C.close archive; Lwt_main.run (S.close store)) (fun () ->
        set_key sender receiver_key;
        set_key receiver sender_key;
        if mode = Prior_output then
          Lwt_main.run (S.write store ["stealth"; "3"]
            (Yojson.Safe.to_string (`Assoc ["id", `Int 3; "claimed", `Int 1])));
        let root = commit 0 in
        let before = R.{epoch = 0; index_root = E.genesis_root; next_txid = 0L;
          state_root = E.folded_state_root ~ledger_state_root:root
            ~epoch_index_root:E.genesis_root} in
        let switch owner nonce key =
          let payload = `Assoc [
            "new_pubkey", `String (Base64.encode_exn key.H.blob);
            "aes_kat", `String (Octra_core.Pvac_registry.expected_kat ());
          ] in
          tx ~nonce owner owner T.KeySwitch payload
        in
        let send_tx = tx ~nonce:2 sender "stealth" T.StealthOp (PT.to_json transfer) in
        let claim_tx = tx ~nonce:2 receiver receiver T.ClaimOp (SC.to_json receipt) in
        let txs = [
          switch sender 1 sender_key;
          send_tx;
          switch sender 3 receiver_key;
          switch receiver 1 receiver_key;
          claim_tx;
          switch receiver 3 sender_key;
        ] in
        C.begin_batch archive;
        List.iter (fun tx ->
          let json = T.to_yojson tx in
          let json = if mode = Bad_payload && tx.T.op_type = T.ClaimOp then
            set "encrypted_data" (`String "{}") json else json in
          C.save_tx archive ~hash:(T.hash tx) ~epoch_id:1
          ~from_addr:tx.T.from ~to_addr:tx.to_
          ~tx_json:(Yojson.Safe.to_string json)
          ~op_type:(T.op_type_to_string tx.op_type)
          ~encrypted_data:(Option.value ~default:"" tx.encrypted_data) ~message:"") txs;
        let items = List.mapi (fun index tx ->
          E.item ~txid:(Int64.of_int index) ~hash:(T.hash tx)) txs in
        let epoch_hash, index_root = E.next_root ~prev:E.genesis_root ~epoch_id:1 items in
        let send = entry 1 1L send_tx in
        let claim = entry 1 4L claim_tx in
        Lwt_main.run (S.write store ["stealth"; "3"]
          (Yojson.Safe.to_string (output send claim transfer)));
        if mode = Bad_key then set_key sender sender_key;
        let root = commit 1 in
        let after = R.{epoch = 1; index_root; next_txid = 6L;
          state_root = E.folded_state_root ~ledger_state_root:root ~epoch_index_root:index_root} in
        C.set_epoch archive {Octra_core.Epochlog.empty_epoch_header with
          id = 1; state_root = after.state_root; prev_state_root = before.state_root;
          start_txid = 0L; tx_count = 6};
        C.set_epoch_index_commitment archive ~epoch_id:1 ~epoch_hash ~root:index_root;
        C.commit_batch archive;
        set_key sender receiver_key;
        set_key receiver sender_key;
        ignore (commit 2);
        before, after)

let chain_case store archive before after transfer =
  let view = Lwt_main.run (S.capture_read_snapshot_epoch store ~epoch_id:1L) |> unwrap in
  let initial = finalized sender_wallet None before in
  let top = finalized receiver_wallet (Some (parent sender_wallet initial)) after in
  let cert = certificate receiver_wallet top view.state_root after.R.index_root in
  let calls = ref 0 in
  let record epoch =
    incr calls;
    if epoch = 0L then Ok initial else Error "missing finality" in
  let chain = chain_read receiver_wallet cert record |> unwrap in
  check "one parent read" (!calls = 1);
  check "historical math prior" (Chain.math chain ~epoch:1 = Ok false);
  let verify ?(send_epoch = 1) ?(claim_epoch = 1) value =
    Chain.verify value store archive ~send_epoch ~claim_epoch
      ~send_index:1L ~claim_index:4L ~max_txs:6 in
  let result = verify chain |> unwrap in
  check "authenticated receipt point" (result.H.commitment = transfer.PT.amount_commitment);
  refused "absent authenticated epoch" (Chain.math chain ~epoch:2);
  refused "receipt before history" (verify ~send_epoch:0 chain);
  refused "receipt after history" (verify ~claim_epoch:2 chain);
  let no_reads name action =
    calls := 0;
    refused name (action ());
    check "refused before archive reads" (!calls = 0) in
  no_reads "self signed validator set" (fun () -> chain_read sender_wallet cert record);
  no_reads "epoch cap" (fun () -> chain_read ~max_epochs:1 receiver_wallet cert record);
  no_reads "zero cap" (fun () -> chain_read ~max_epochs:0 receiver_wallet cert record);
  no_reads "negative cap" (fun () -> chain_read ~max_epochs:(-1) receiver_wallet cert record);
  ignore (chain_read ~max_epochs:500_001 receiver_wallet cert record |> unwrap);
  no_reads "negative first epoch" (fun () -> chain_read ~first_epoch:(-1) receiver_wallet cert record);
  no_reads "network chain" (fun () -> chain_read receiver_wallet
    {cert with checkpoint = {cert.checkpoint with chain_id = "another-chain"}} record);
  no_reads "network config" (fun () -> chain_read receiver_wallet
    {cert with checkpoint = {cert.checkpoint with config_hash = digest "another-config"}} record);
  no_reads "exporter signature" (fun () -> chain_read receiver_wallet
    {cert with exporter_signatures = []} record);
  refused "missing finality" (chain_read receiver_wallet cert (fun _ -> Error "missing"));
  refused "parent root substitution" (chain_read receiver_wallet cert (fun _ ->
    Ok {initial with header = {initial.header with proposed_state_root = raw after.state_root}}));
  refused "parent epoch substitution" (chain_read receiver_wallet cert (fun _ ->
    Ok {initial with epoch_id = 1L}));
  refused "parent proof substitution" (chain_read receiver_wallet cert (fun _ ->
    Ok {initial with precommits = []}));
  let read_pair prior head =
    let cert = certificate receiver_wallet head view.state_root after.index_root in
    chain_read receiver_wallet cert (fun _ -> Ok prior) in
  let legacy = finalized ~version:F.proto_version_parent_legacy sender_wallet None before in
  let linked = finalized receiver_wallet (Some (parent sender_wallet legacy)) after in
  ignore (read_pair legacy linked |> unwrap);
  let legacy_top = finalized ~version:F.proto_version_parent_legacy
    receiver_wallet None after in
  refused "unsigned parent traversal" (read_pair initial legacy_top);
  let empty = finalized sender_wallet None {before with next_txid = after.next_txid} in
  let linked = finalized receiver_wallet (Some (parent sender_wallet empty)) after in
  let empty_chain = read_pair empty linked |> unwrap in
  refused "authenticated transaction count differs" (verify empty_chain);
  let ahead = finalized sender_wallet None {before with next_txid = Int64.succ after.next_txid} in
  let linked = finalized receiver_wallet (Some (parent sender_wallet ahead)) after in
  refused "transaction position decreases" (read_pair ahead linked);
  let overflow = finalized sender_wallet None {before with next_txid = Int64.min_int} in
  let linked = finalized receiver_wallet (Some (parent sender_wallet overflow)) after in
  refused "transaction position overflow" (read_pair overflow linked);
  let late = {after with epoch = 1_510_000} in
  let head = finalized receiver_wallet None late in
  let cert = certificate receiver_wallet head view.state_root after.index_root in
  let chain = chain_read ~first_epoch:late.epoch ~max_epochs:1 receiver_wallet cert
    (fun _ -> failwith "unexpected read") |> unwrap in
  refused "math anchor missing" (Chain.math chain ~epoch:late.epoch);
  let later = {after with epoch = 2_004_440} in
  let head = finalized receiver_wallet (Some (parent sender_wallet initial)) later in
  let cert = certificate receiver_wallet head view.state_root after.index_root in
  let probe = chain_read ~first_epoch:1_504_440 ~max_epochs:500_001
    receiver_wallet cert (fun _ -> Error "archive read reached") in
  check "history budget is explicit" (probe = Error "archive read reached")

let archive_case mode sender_key receiver_key transfer receipt =
  Test_workspace.with_dir "claim_history" (fun dir ->
    let before, after = archive_write dir mode sender_key receiver_key transfer receipt in
    let path = Filename.concat dir "irmin_store" in
    let history = Filename.concat dir "chaindata" in
    let store = Lwt_main.run (S.open_store ~readonly:true path) in
    let archive = C.open_chaindata ~readonly:true history in
    Fun.protect ~finally:(fun () -> C.close archive; Lwt_main.run (S.close store)) (fun () ->
      let read ?(before = before) ?(after = after) ?(max_txs = 6) () =
        R.read store archive ~before ~after ~max_txs
      in
      if mode = Bad_payload then refused "disk payload hash" (read ())
      else
      let epoch = unwrap (read ()) in
      if mode = Bad_key then
        refused "key endpoint differs" (R.key epoch ~address:sender ~index:1L ~math:false)
      else if mode = Prior_output then
        rejected "pre-existing output id" "history output existed before creation epoch"
          (R.verify ~send:epoch ~claim:epoch ~send_index:1L ~claim_index:4L
            ~sender_math:false ~receiver_math:false)
      else begin
      check "current key differs from send"
        (Lwt_main.run (S.get_pvac_pubkey store sender) = Some receiver_key.H.blob);
      let at address index = unwrap (R.key epoch ~address ~index ~math:false) in
      check "send key before rotation" ((at sender 0L).hash = receiver_key.H.hash);
      check "send key at operation" ((at sender 1L).hash = sender_key.H.hash);
      check "send key restored" ((at sender 3L).hash = receiver_key.hash);
      check "receiver key at operation" ((at receiver 4L).hash = receiver_key.hash);
      check "receiver key before switch" ((at receiver 3L).hash = sender_key.hash);
      refused "index outside epoch" (R.key epoch ~address:sender ~index:6L ~math:false);
      refused "missing registration" (R.key epoch ~address:"octMissing" ~index:1L ~math:false);
      let result = R.verify ~send:epoch ~claim:epoch ~send_index:1L ~claim_index:4L
        ~sender_math:false ~receiver_math:false |> unwrap in
      check "disk receipt point" (result.commitment = transfer.PT.amount_commitment);
      chain_case store archive before after transfer;
      refused "history cap" (read ~max_txs:5 ());
      refused "history height gap" (read ~before:{before with epoch = 2} ());
      refused "history root" (read ~after:{after with state_root = before.state_root} ());
      refused "history index root" (read ~after:{after with index_root = E.genesis_root} ());
      refused "history txid start" (read ~before:{before with next_txid = 1L} ());
      refused "history txid end" (read ~after:{after with next_txid = 5L} ());
      refused "history key endpoint" (read ~after:{after with epoch = 2} ());
      let path = S.pvac_blob_path store receiver_key.hash in
      let held = path ^ ".held" in
      Unix.rename path held;
      Fun.protect ~finally:(fun () ->
        if Sys.file_exists path then Unix.unlink path;
        Unix.rename held path) (fun () ->
          refused "missing key blob" (R.key epoch ~address:sender ~index:1L ~math:false);
          let channel = open_out_bin path in
          Fun.protect ~finally:(fun () -> close_out_noerr channel)
            (fun () -> output_string channel "PVAC altered key");
          refused "corrupt key blob" (R.key epoch ~address:sender ~index:1L ~math:false));
      check "key read recovery" ((at sender 1L).hash = sender_key.hash)
      end
    ))

let accounting receipt extra =
  let zero = Sum.create ~address:receiver in
  let step value hash change = Sum.apply value ~hash change |> unwrap in
  let point value = (Sum.finish value).Sum.commitment in
  let received = step zero receipt.H.claim_hash (Sum.Claim receipt) in
  let paid = step received "withdraw" (Sum.Withdraw (Z.of_int 4)) in
  check "claim-funded public withdrawal"
    (point paid = b64 (P.pedersen_commit_amount 3L (bytes '\003')));
  let mixed = step paid "deposit" (Sum.Deposit (Z.of_int 10)) in
  check "mixed total"
    (point mixed = b64 (P.pedersen_commit_amount 13L (bytes '\003')));
  let sent = step zero receipt.send_hash (Sum.Send receipt.commitment) in
  check "unclaimed send debited" (point sent = b64 (P.pedersen_sub
    (P.pedersen_identity ()) (Bytes.of_string (Base64.decode_exn receipt.commitment))));
  let returned = step sent receipt.claim_hash (Sum.Claim receipt) in
  check "send and receive cancel" (point returned = point zero);
  let summary = Sum.finish (step returned "neutral" Sum.Keep) in
  check "accounting counts"
    (summary.records = 3 && summary.changes = 2 && summary.received = 1);
  rejected "claim hash repeated" "history transaction repeated"
    (Sum.apply received ~hash:receipt.claim_hash (Sum.Claim receipt));
  rejected "output id repeated" "history output credited twice"
    (Sum.apply received ~hash:extra.H.claim_hash (Sum.Claim extra));
  rejected "receipt hash differs" "history claim transaction differs"
    (Sum.apply zero ~hash:"other" (Sum.Claim receipt));
  rejected "receipt owner differs" "history claim belongs to another account"
    (Sum.apply (Sum.create ~address:sender)
    ~hash:receipt.claim_hash (Sum.Claim receipt));
  rejected "public amount negative" "history public amount exceeds supply range"
    (Sum.apply zero ~hash:"negative" (Sum.Deposit Z.minus_one));
  rejected "public amount too large" "history public amount exceeds supply range"
    (Sum.apply zero ~hash:"large"
    (Sum.Withdraw (Z.succ Octra_core.Denomination.max_supply)));
  rejected "amount point length" "history amount point length invalid"
    (Sum.apply zero ~hash:"short-point" (Sum.Send (b64 (Bytes.make 31 '\000'))));
  refused "amount point malformed" (Sum.apply zero ~hash:"point" (Sum.Send "wrong"));
  check "refusal keeps original" (point received = receipt.commitment)

let self_case key transfer receipt =
  let send = entry 10 100L (tx receiver "stealth" T.StealthOp (PT.to_json transfer)) in
  let receipt = {receipt with SC.claim_cipher = transfer.PT.delta_cipher;
    commitment = transfer.commitment; zero_proof = transfer.send_zero_proof} in
  let claim = entry 12 150L (tx ~nonce:2 receiver receiver T.ClaimOp (SC.to_json receipt)) in
  let output = output send claim transfer |> set "sender_addr" (`String receiver) in
  let checked = H.verify ~send ~claim ~before_send:None
    ~before_claim:(Some (unspent output)) ~output ~sender_key:key ~receiver_key:key |> unwrap in
  check "self receipt sender" (checked.sender = receiver && checked.receiver = receiver);
  let sent = Sum.apply (Sum.create ~address:receiver) ~hash:send.hash
    (Sum.Send checked.commitment) |> unwrap in
  let received = Sum.apply sent ~hash:claim.hash (Sum.Claim checked) |> unwrap |> Sum.finish in
  check "verified self send cancels"
    (received.commitment = b64 (P.pedersen_identity ()) && received.received = 1)

let interval_write dir mode sender_key receiver_key transfer receipt deposit withdraw =
  let store = Lwt_main.run (S.open_store (Filename.concat dir "irmin_store")) in
  let archive = C.open_chaindata (Filename.concat dir "chaindata") in
  let commit epoch =
    Lwt_main.run (S.set_meta store "last_epoch" (string_of_int epoch));
    Lwt_main.run (S.tag_epoch store epoch);
    (Lwt_main.run (S.capture_read_snapshot store) |> unwrap).S.state_root in
  let account address cipher =
    Lwt_main.run (S.begin_epoch_batch ~mode:Octra_core.Rule_graph.Active store);
    Lwt_main.run (S.set_account store address
      {Octra_core.Ledger_types.empty_account with encrypted_balance = cipher});
    Lwt_main.run (S.commit_epoch_batch store "history account") in
  Fun.protect ~finally:(fun () -> C.close archive; Lwt_main.run (S.close store)) (fun () ->
    Lwt_main.run (S.set_pvac_pubkey store sender sender_key.H.blob);
    Lwt_main.run (S.set_pvac_pubkey store receiver receiver_key.H.blob);
    account sender (Some "0");
    if mode = Prior_output then
      Lwt_main.run (S.write store ["stealth"; "3"]
        (Yojson.Safe.to_string (`Assoc ["id", `Int 3; "claimed", `Int 1])));
    let root = commit 0 in
    let initial = R.{epoch = 0; index_root = E.genesis_root; next_txid = 0L;
      state_root = E.folded_state_root ~ledger_state_root:root ~epoch_index_root:E.genesis_root} in
    let public_cipher payload = payload |> Yojson.Safe.Util.member "cipher"
      |> Yojson.Safe.Util.to_string |> FB.decode_cipher |> unwrap in
    let deposit_cipher = public_cipher deposit in
    let withdraw_cipher = public_cipher withdraw in
    let deposit = tx ~amount:(Z.of_int 100) sender sender T.EncryptOp deposit in
    let send_tx = tx ~nonce:2 sender "stealth" T.StealthOp (PT.to_json transfer) in
    let claim_tx = tx receiver receiver T.ClaimOp (SC.to_json receipt) in
    let amount = Z.of_int (if mode = Bad_amount then 5 else 4) in
    let withdraw = tx ~nonce:2 ~amount receiver receiver T.DecryptOp withdraw in
    let send = entry 1 1L send_tx in
    let claim = entry 2 2L claim_tx in
    let spent = output send claim transfer in
    let save prior epoch txs effect =
      C.begin_batch archive;
      List.iter (fun tx ->
        C.save_tx archive ~hash:(T.hash tx) ~epoch_id:epoch
          ~from_addr:tx.T.from ~to_addr:tx.to_
          ~tx_json:(Yojson.Safe.to_string (T.to_yojson tx))
          ~op_type:(T.op_type_to_string tx.op_type)
          ~encrypted_data:(Option.value ~default:"" tx.encrypted_data) ~message:"") txs;
      let items = List.mapi (fun index tx -> E.item
        ~txid:(Int64.add prior.R.next_txid (Int64.of_int index)) ~hash:(T.hash tx)) txs in
      let epoch_hash, index_root = E.next_root ~prev:prior.index_root ~epoch_id:epoch items in
      effect ();
      let root = commit epoch in
      let after = R.{epoch; index_root;
        next_txid = Int64.add prior.next_txid (Int64.of_int (List.length txs));
        state_root = E.folded_state_root ~ledger_state_root:root ~epoch_index_root:index_root} in
      C.set_epoch archive {Octra_core.Epochlog.empty_epoch_header with
        id = epoch; state_root = after.state_root; prev_state_root = prior.state_root;
        start_txid = prior.next_txid; tx_count = List.length txs};
      C.set_epoch_index_commitment archive ~epoch_id:epoch ~epoch_hash ~root:index_root;
      C.commit_batch archive;
      after in
    let first = save initial 1 [deposit; send_tx] (fun () ->
      Lwt_main.run (S.write store ["stealth"; "3"] (Yojson.Safe.to_string (unspent spent)));
      let delta = FB.decode_cipher transfer.PT.delta_cipher |> unwrap in
      let pk = P.deserialize_pubkey (Bytes.of_string sender_key.blob) in
      account sender (Some (FB.encode_cipher (P.ct_sub pk deposit_cipher delta)))) in
    let cipher = FB.decode_cipher receipt.SC.claim_cipher |> unwrap in
    let pk = P.deserialize_pubkey (Bytes.of_string receiver_key.blob) in
    let final_cipher = FB.encode_cipher (P.ct_sub pk cipher withdraw_cipher) in
    let second = save first 2 [claim_tx; withdraw] (fun () ->
      Lwt_main.run (S.write store ["stealth"; "3"] (Yojson.Safe.to_string spent));
      account receiver (Some final_cipher);
      C.save_rejected archive ~hash:(digest "refused-deposit") ~from_addr:receiver
        ~to_addr:receiver ~amount:"100000" ~nonce:3 ~error_type:"refused"
        ~reason:"test rejection" ~epoch_id:2 ~ts:1000.0) in
    let third = save second 3 [tx ~nonce:3 receiver receiver T.KeySwitch (`Assoc [])]
      (fun () -> ()) in
    let last = save third 4 [] (fun () -> ()) in
    [initial; first; second; third; last], final_cipher)

let interval_case mode sender_key receiver_key transfer receipt deposit withdraw =
  Test_workspace.with_dir "claim_interval" (fun dir ->
    let pins, cipher = interval_write dir mode sender_key receiver_key transfer receipt deposit withdraw in
    let store = Lwt_main.run (S.open_store ~readonly:true (Filename.concat dir "irmin_store")) in
    let archive = C.open_chaindata ~readonly:true (Filename.concat dir "chaindata") in
    Fun.protect ~finally:(fun () -> C.close archive; Lwt_main.run (S.close store)) (fun () ->
      let records = List.fold_left (fun values pin ->
        let prior = match values with [] -> None | head :: _ -> Some (parent sender_wallet head) in
        finalized sender_wallet prior pin :: values) [] pins |> List.rev |> Array.of_list in
      let view = Lwt_main.run (S.capture_read_snapshot_epoch store ~epoch_id:4L) |> unwrap in
      let cert = certificate sender_wallet records.(4) view.state_root (List.nth pins 4).R.index_root in
      let chain = chain_read ~max_epochs:5 sender_wallet cert
        (fun epoch -> Ok records.(Int64.to_int epoch)) |> unwrap in
      let total ?(address = receiver) ?(first_epoch = 0) ?(last_epoch = 2)
          ?(max_txs = 2) ?(max_records = 4) () =
        Chain.total chain store archive ~address ~first_epoch ~last_epoch ~max_txs ~max_records in
      if mode = Prior_output then begin
        let verified = Chain.verify chain store archive ~send_epoch:1 ~claim_epoch:2
          ~send_index:1L ~claim_index:2L ~max_txs:2 in
        let summed = total () in
        rejected "cross-epoch pre-existing receipt"
          "history output existed before creation epoch" verified;
        rejected "cross-epoch pre-existing total"
          "history output existed before creation epoch" summed
      end else if mode = Bad_amount then
        rejected "authenticated history amount differs"
          "amount commitment mismatch: pedersen_commit(amount, blinding) != amount_commitment"
          (total ())
      else begin
      let result = total () |> unwrap in
      check "interval amount point"
        (result.sum.commitment = b64 (P.pedersen_commit_amount 3L (bytes '\003')));
      check "interval rejected records excluded"
        (result.sum.records = 4 && result.sum.changes = 2 && result.sum.received = 1);
      check "interval final cipher"
        (result.source_cipher_hash = Octra_core.Pvac_migration_admission.source_cipher_hash cipher);
      let tail = total ~first_epoch:1 ~max_records:2 () |> unwrap in
      check "source before empty starting state"
        (tail.sum.commitment = result.sum.commitment && tail.sum.records = 2);
      let sender_total = total ~address:sender ~last_epoch:1 ~max_records:2 () |> unwrap in
      check "interval unclaimed sender debit" (sender_total.sum.commitment =
        b64 (P.pedersen_sub (P.pedersen_commit_amount 100L (bytes '\000'))
          (Bytes.of_string (Base64.decode_exn transfer.PT.amount_commitment))));
      let empty = total ~last_epoch:0 ~max_txs:0 ~max_records:0 () |> unwrap in
      check "empty interval"
        (empty.sum.records = 0 && empty.sum.commitment = b64 (P.pedersen_identity ()));
      rejected "total record cap" "history interval exceeds record limit"
        (total ~max_records:3 ());
      rejected "epoch record cap" "history epoch exceeds transaction limit"
        (total ~max_txs:1 ());
      rejected "negative record cap" "history account range invalid"
        (total ~max_records:(-1) ());
      rejected "backwards interval" "history account range invalid"
        (total ~first_epoch:2 ~last_epoch:1 ());
      rejected "unknown authenticated epoch" "history checkpoint is not authenticated"
        (total ~last_epoch:5 ());
      rejected "unresolved target operation" "history account effect unresolved: key_switch"
        (total ~last_epoch:3 ~max_records:5 ());
      let _, _, absent = identity '\053' in
      let no_effect = total ~address:absent ~first_epoch:3 ~last_epoch:4 ~max_records:0 () |> unwrap in
      check "empty epoch traversed" (no_effect.sum.records = 0);
      let view = Lwt_main.run (S.capture_read_snapshot_epoch store ~epoch_id:2L) |> unwrap in
      let packed = Lwt_main.run (S.read_snapshot view ["accounts"; receiver; "data"])
        |> Option.get |> Octra_core.Account_pack.data |> unwrap in
      check "packed cipher is not empty" (match packed with
        | Octra_core.Account_pack.Parts (account, _) -> account.encrypted_balance = None
        | _ -> false);
      check "packed cipher reconstructed"
        (R.cipher_at store (List.nth pins 2) receiver = Ok (Some cipher));
      rejected "nonempty packed start" "history initial encrypted balance is not empty"
        (total ~first_epoch:2 ~last_epoch:2 ());
      Printf.printf "event = claim_history phase = interval status = pass\n%!"
      end))

let run () =
  chain_rules ();
  let sender_key, receiver_key, transfer, receipt, deposit, withdraw = material () in
  amount_case sender_key receiver_key deposit withdraw;
  let send_json = PT.to_json transfer in
  let claim_json = SC.to_json receipt in
  let send = send_entry send_json in
  let claim = claim_entry claim_json in
  let output = output send claim transfer in
  let before = Some (unspent output) in
  let verify ?(send = send) ?(claim = claim) ?(output = output)
      ?(before = before) ?(before_send = None)
      ?(sender_key = sender_key) ?(receiver_key = receiver_key) () =
    H.verify ~send ~claim ~before_send ~before_claim:before ~output ~sender_key ~receiver_key
  in
  let valid = unwrap (verify ()) in
  check "receipt point" (valid.commitment = transfer.amount_commitment);
  check "receipt identity"
    (valid.id = 3 && valid.sender = sender && valid.receiver = receiver
     && valid.send_hash = send.hash && valid.claim_hash = claim.hash);
  rejected "missing unspent output" "history unspent output missing" (verify ~before:None ());
  rejected "output predates send" "history output existed before creation epoch"
    (verify ~before_send:(Some output) ());
  rejected "already spent output" "history output was already spent" (verify ~before:(Some output) ());
  rejected "unspent with claim hash" "history unspent output has a spending hash" (verify
    ~before:(Some (set "claimed" (`Int 0) output)) ());
  rejected "origin substituted" "history output origin changed: tx_hash" (verify
    ~before:(Some (set "tx_hash" (`String claim.hash) (unspent output))) ());
  ignore (verify ~before:(Some (unspent output |> set "id" (`Intlit "3"))) () |> unwrap);
  List.iter (fun (name, value) ->
    refused name (verify ~output:(set name value output) ())) [
    "id", `Int 4;
    "epoch_id", `Int 9;
    "claimed", `Int 0;
    "claimed", `Int 2;
    "tx_hash", `String claim.hash;
    "claim_tx_hash", `String send.hash;
    "sender_addr", `String receiver;
    "delta_cipher_stored", `String receipt.claim_cipher;
    "amount_commitment", `String (b64 (P.pedersen_identity ()));
    "claim_pub", `String (String.make 64 '0');
    "stealth_tag", `String (String.make 32 'b');
    "eph_pub", `String (b64 (bytes '\012'));
    "enc_amount", `String "other";
  ];
  refused "missing receipt" (verify ~output:(`Assoc []) ());
  let repeated = match output with
    | `Assoc fields -> `Assoc (("id", `Int 3) :: fields)
    | _ -> assert false
  in
  refused "duplicate output field" (verify ~output:repeated ());
  refused "transaction bytes" (verify ~claim:{claim with json = claim.json ^ " "} ());
  refused "payload hash" (verify ~claim:{claim with json = send.json} ());
  refused "transaction hash" (verify ~send:{send with hash = claim.hash} ());
  List.iter (fun index ->
    refused "transaction order" (verify ~claim:{claim with index} ())) [-1L; 99L; 100L];
  refused "epoch order" (verify ~claim:{claim with epoch = 9} ());
  refused "key hash" (verify ~sender_key:{sender_key with hash = receiver_key.hash} ());
  refused "sender key" (verify ~sender_key:receiver_key ());
  refused "receiver key" (verify ~receiver_key:sender_key ());
  let change_claim json =
    let claim = claim_entry json in
    verify ~claim ~output:(set "claim_tx_hash" (`String claim.hash) output) ()
  in
  List.iter (fun (name, value) ->
    refused name (change_claim (set name value claim_json))) [
    "version", `Int 4;
    "output_id", `Int 4;
    "claim_secret", `String (hex (bytes '\020'));
    "claim_secret", `String (String.make 64 'z');
    "commitment", `String (b64 (bytes '\021'));
    "zero_proof", `String transfer.send_zero_proof;
    "claim_cipher", `String transfer.delta_cipher;
  ];
  let extra = change_claim
    (set "amount_commitment" (`String (b64 (P.pedersen_identity ()))) claim_json)
    |> unwrap in
  check "carried point ignored" (extra.commitment = transfer.amount_commitment);
  accounting valid extra;
  self_case sender_key transfer receipt;
  check "outgoing verified point" (H.sent send sender_key = Ok transfer.amount_commitment);
  let duplicate = match claim_json with
    | `Assoc fields -> `Assoc (("version", `Int 5) :: fields)
    | _ -> assert false
  in
  refused "duplicate payload field" (change_claim duplicate);
  let operation = `Assoc [
    "from", `String receiver;
    "to_", `String receiver;
    "amount", `String "0";
    "nonce", `Int 1;
    "ou", `String "5000";
    "timestamp", `Float 1000.0;
    "op_type", `String "claim";
  ] in
  let hash = operation |> Yojson.Safe.to_string
    |> Digestif.SHA256.digest_string |> Digestif.SHA256.to_hex in
  let claim = {claim with hash} in
  refused "operation hash" (verify ~claim
    ~output:(set "claim_tx_hash" (`String claim.hash) output) ());
  let change_send transfer =
    let send = send_entry (PT.to_json transfer) in
    let output = output
      |> set "tx_hash" (`String send.hash)
      |> set "amount_commitment" (`String transfer.PT.amount_commitment) in
    verify ~send ~output ~before:(Some (unspent output)) ()
  in
  refused "range proof" (change_send {transfer with range_proof_delta = "invalid"});
  refused "cipher commitment" (change_send {transfer with commitment = receipt.commitment});
  refused "send proof" (change_send {transfer with send_zero_proof = receipt.zero_proof});
  refused "amount proof" (change_send {transfer with
    amount_commitment = b64 (P.pedersen_commit_amount 8L (bytes '\003'))});
  Printf.printf "event = claim_history phase = proofs status = pass\n%!";
  List.iter (fun mode -> archive_case mode sender_key receiver_key transfer receipt)
    [Valid; Bad_key; Bad_payload; Prior_output];
  List.iter (fun mode -> interval_case mode sender_key receiver_key transfer receipt deposit withdraw)
    [Valid; Prior_output; Bad_amount];
  Printf.printf "status = pass test = claim_history\n%!"

let () = run ()