(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module E = Octra_core.Epoch_exec
module FB = Octra_core.Crypto.FheBalance
module C = Octra_consensus.C_types
module CE = Octra_consensus.C_engine
module CH = Octra_consensus.C_hash
module J = Octra_node_runtime.Consensus_finality_journal
module JR = Octra_node_runtime.Consensus_finality_journal_recovery
module P = Pvac_ffi
module PL = Octra_core.Private_ledger
module R = Octra_core.Preverify_receipt
module T = Octra_core.Transaction
module VP = Octra_core.Pvac_verify_protocol
module VW = Octra_core.Pvac_verify_worker
module W = Octra_core.Preverify_worker

let () =
  Mirage_crypto_rng_unix.use_default ()

let fail reason =
  failwith ("test_private_transition_receipt: " ^ reason)

let expect value reason =
  if not value then fail reason

let bytes value =
  Bytes.make 32 value

let zero_proof_cache = ref None

let zero_proof pk sk cipher amount blind =
  match !zero_proof_cache with
  | Some proof -> proof
  | None ->
    let proof =
      P.make_zero_proof_bound pk sk cipher amount blind
      |> FB.encode_zero_proof
    in
    zero_proof_cache := Some proof;
    proof

let rec remove_tree path =
  if Sys.file_exists path then
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
      Sys.readdir path
      |> Array.iter (fun name ->
        remove_tree (Filename.concat path name));
      Unix.rmdir path
    | _ ->
      Unix.unlink path

let clear_case path =
  remove_tree path;
  remove_tree (path ^ "_pvac")

let addr =
  "oct" ^ String.make 44 '1'

let tx encrypted_data =
  T.{
    from = addr;
    to_ = addr;
    amount = Z.of_int 10;
    nonce = 1;
    ou = Z.one;
    timestamp = 1.;
    signature = "sig";
    public_key = Some "pub";
    message = None;
    op_type = EncryptOp;
    encrypted_data = Some encrypted_data;
  }

let pending_case () =
  let first = { (tx "first") with T.op_type = DecryptOp } in
  let second = {
    first with
    T.nonce = 2;
    timestamp = 2.;
    signature = "sig2";
    op_type = StealthOp;
    encrypted_data = Some "second";
  } in
  let check txs =
    match W.isolate_private_transitions txs with
    | [ready], [skip] ->
      expect (T.hash ready = T.hash first) "pending first";
      expect (T.hash skip.W.tx = T.hash second) "pending second";
      expect
        (skip.reason = "private_transition_dependency_deferred")
        "pending reason";
      expect (skip.kind = W.Deferred) "pending kind"
    | _ -> fail "pending shape"
  in
  check [first; second];
  check [second; first];
  let other = {
    second with
    T.from = "oct" ^ String.make 44 '2';
    to_ = "oct" ^ String.make 44 '2';
    nonce = 1;
  } in
  match W.isolate_private_transitions [first; other] with
  | ready, [] -> expect (List.length ready = 2) "pending independent"
  | _ -> fail "pending independent shape"

let payload pk sk proof_ok =
  let amount = 10L in
  let blind = bytes '\003' in
  let cipher = P.enc_value_seeded pk sk amount (bytes '\002') in
  let zero_proof =
    if proof_ok then
      zero_proof pk sk cipher amount blind
    else
      "invalid"
  in
  Yojson.Safe.to_string
    (`Assoc [
      "cipher", `String (FB.encode_cipher cipher);
      "amount_commitment",
      `String
        (P.pedersen_commit_amount amount blind
         |> Bytes.to_string
         |> Base64.encode_exn);
      "zero_proof", `String zero_proof;
      "blinding", `String (Base64.encode_exn (Bytes.to_string blind));
    ])

let legacy_replay ~epoch:_ ~address:_ ~cipher:_ =
  {
    Octra_core.Pvac_legacy_public_replay.audit_class =
      Octra_core.Pvac_legacy_public_replay.Poisoned;
    can_public_migrate = false;
    public_net = None;
    commitment_net = None;
    blockers = ["unused"];
    effects = [];
    reason = "unused";
  }

let setup name =
  let path =
    Filename.concat
      "runtime_data/private_transition_receipt"
      (Printf.sprintf
         "private_transition_receipt_%s_%d"
         name
         (Unix.getpid ()))
  in
  clear_case path;
  let store =
    Lwt_main.run (Octra_core.Store_irmin.open_store path)
  in
  let ledger = Octra_core.Ledger.create store in
  let pk, sk =
    P.keygen_from_seed (P.default_params ()) (bytes '\001')
  in
  begin
    match Octra_core.Ledger.add_account ledger addr (Z.of_int 1_000_000) with
    | Ok () -> ()
    | Error e -> fail e
  end;
  Lwt_main.run
    (Octra_core.Ledger.set_pvac_pubkey
       ledger
       addr
       (P.serialize_pubkey pk |> Bytes.to_string));
  Lwt_main.run (Octra_core.Ledger.flush_dirty_lwt ledger);
  path, store, ledger, pk, sk

let receipt ledger transaction transition_hash =
  let root = Lwt_main.run (Octra_core.Ledger.hash ledger) in
  let pre_state_hash = W.state_hash root in
  let state =
    match
      Lwt_main.run
        (W.source_binding ledger pre_state_hash transaction)
    with
    | Ok state ->
      {
        state with
        R.transition_hash = Some transition_hash;
      }
    | Error e -> fail e
  in
  match
    R.for_tx_bound
      ~input_hash:(W.bound_input_hash transaction state)
      ~output_hash:(W.bound_output_hash transaction state "ok")
      ~state
      ~ok:true
      ~reason:""
      transaction
  with
  | Ok value -> value
  | Error e -> fail e

let env =
  E.{
    chain_id = "octra-transition-receipt";
    epoch_id = 1;
    proposer_addr = addr;
    validator_addrs = [addr];
    validator_pubkeys = [];
    prev_state_root = String.make 64 '0';
    epoch_ts = 1.;
    ready_state_root_at = None;
    ready_max_lag = 0;
  }

let process proof_mode store ledger transaction receipt =
  let gate =
    Octra_core.Preverify_commit.create [receipt]
  in
  begin
    match
      Lwt_main.run
        (Octra_core.Preverify_commit.check_bound
           ledger
           gate
           [transaction])
    with
    | Ok () -> ()
    | Error e -> fail e
  end;
  let transition =
    Octra_core.Private_transition.create
      ~preverify:(Some gate)
      ~ledger
      ~epoch_id:env.epoch_id
      ~owner_migration_mode:Octra_core.Rule_graph.Active
      ~proof_mode
      ~field_policy:Octra_core.Private_ledger.Unique_fields
      ~result_policy:Octra_core.Private_result_policy.Recoverable
      ~legacy_replay
      ~limits:Octra_core.Private_transition.{
        max_fhe = 1;
        max_stealth = 1;
      }
  in
  Lwt_main.run
    (Octra_core.Private_transition.process
       transition
       ~backend:(E.make_live_backend store ledger)
       ~env
       transaction)

let valid_case () =
  let path, store, ledger, pk, sk = setup "valid" in
  Fun.protect
    ~finally:(fun () ->
      Lwt_main.run (Octra_core.Store_irmin.close store);
      clear_case path)
    (fun () ->
      let transaction = tx (payload pk sk true) in
      let plan =
        match
          Lwt_main.run
            (PL.prepare_encrypt_plan
               ~field_policy:PL.Unique_fields
               ledger
               transaction)
        with
        | Ok plan -> plan
        | Error e -> fail e.PL.reason
      in
      let transition_hash =
        PL.hash_prepared (PL.Prepared_encrypt plan)
      in
      let receipt = receipt ledger transaction transition_hash in
      begin
        match
          process
            Octra_core.Rule_graph.Active
            store
            ledger
            transaction
            receipt
        with
        | Ok fee -> expect (Z.equal fee Z.one) "certified fee"
        | Error (_, reason) -> fail reason
      end;
      match Octra_core.Ledger.find_opt ledger addr with
      | Some account ->
        expect (account.Octra_core.Ledger_types.nonce = 1) "certified nonce";
        expect
          (account.encrypted_balance = Some plan.PL.next_cipher)
          "certified cipher"
      | None -> fail "certified account missing")

let mismatch_case () =
  let path, store, ledger, pk, sk = setup "mismatch" in
  Fun.protect
    ~finally:(fun () ->
      Lwt_main.run (Octra_core.Store_irmin.close store);
      clear_case path)
    (fun () ->
      let transaction = tx (payload pk sk true) in
      let receipt =
        receipt ledger transaction (String.make 64 'f')
      in
      begin
        match
          process
            Octra_core.Rule_graph.Active
            store
            ledger
            transaction
            receipt
        with
        | Error ("preverify_transition_mismatch", _) -> ()
        | Error (_, reason) -> fail ("unexpected mismatch: " ^ reason)
        | Ok _ -> fail "mismatched transition accepted"
      end;
      match Octra_core.Ledger.find_opt ledger addr with
      | Some account ->
        expect (account.Octra_core.Ledger_types.nonce = 0) "mismatch nonce";
        expect
          (account.encrypted_balance = None)
          "mismatch cipher"
      | None -> fail "mismatch account missing")

let forged_case () =
  let path, store, ledger, pk, sk = setup "forged" in
  Fun.protect
    ~finally:(fun () ->
      Lwt_main.run (Octra_core.Store_irmin.close store);
      clear_case path)
    (fun () ->
      let transaction = tx (payload pk sk false) in
      let plan =
        match
          Lwt_main.run
            (PL.prepare_encrypt_plan
               ~field_policy:PL.Unique_fields
               ledger
               transaction)
        with
        | Ok plan -> plan
        | Error e -> fail e.PL.reason
      in
      let transition_hash =
        PL.hash_prepared (PL.Prepared_encrypt plan)
      in
      let receipt = receipt ledger transaction transition_hash in
      begin
        match
          process
            Octra_core.Rule_graph.Active
            store
            ledger
            transaction
            receipt
        with
        | Error ("bad_zero_proof", _) -> ()
        | Error (tag, _) -> fail ("unexpected forged result: " ^ tag)
        | Ok _ -> fail "forged proof accepted"
      end;
      match Octra_core.Ledger.find_opt ledger addr with
      | Some account ->
        expect (account.Octra_core.Ledger_types.nonce = 0) "forged nonce";
        expect (account.encrypted_balance = None) "forged cipher"
      | None -> fail "forged account missing")

let prior_receipt_case () =
  let path, store, ledger, pk, sk = setup "prior_receipt" in
  Fun.protect
    ~finally:(fun () ->
      Lwt_main.run (Octra_core.Store_irmin.close store);
      clear_case path)
    (fun () ->
      let transaction = tx (payload pk sk false) in
      let plan =
        match
          Lwt_main.run
            (PL.prepare_encrypt_plan
               ~field_policy:PL.Unique_fields
               ledger
               transaction)
        with
        | Ok plan -> plan
        | Error e -> fail e.PL.reason
      in
      let receipt =
        receipt
          ledger
          transaction
          (PL.hash_prepared (PL.Prepared_encrypt plan))
      in
      match
        process
          Octra_core.Rule_graph.Prior
          store
          ledger
          transaction
          receipt
      with
      | Ok fee -> expect (Z.equal fee Z.one) "prior receipt fee"
      | Error (tag, _) -> fail ("prior receipt rejected: " ^ tag))

let worker_retry_case () =
  let path, store, ledger, pk, sk = setup "worker_retry" in
  Fun.protect
    ~finally:(fun () ->
      Lwt_main.run (Octra_core.Store_irmin.close store);
      clear_case path)
    (fun () ->
      let transaction = tx (payload pk sk true) in
      let plan =
        match
          Lwt_main.run
            (PL.prepare_encrypt_plan
               ~field_policy:PL.Unique_fields
               ledger
               transaction)
        with
        | Ok plan -> plan
        | Error e -> fail e.PL.reason
      in
      let transition_hash =
        PL.hash_prepared (PL.Prepared_encrypt plan)
      in
      let receipt = receipt ledger transaction transition_hash in
      let worker =
        Octra_core.Pvac_verify_worker.worker_path ()
        |> Option.get
      in
      let retried =
        Fun.protect
          ~finally:(fun () ->
            Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" worker)
          (fun () ->
            Unix.putenv
              "OCTRA_PVAC_VERIFY_WORKER"
              "runtime_data/private_transition_receipt/absent_worker";
            try
              ignore
                (process
                   Octra_core.Rule_graph.Active
                   store
                   ledger
                   transaction
                   receipt);
              false
            with
            | PL.Worker_retry _ -> true)
      in
      expect retried "worker failure became a transaction rejection";
      match Octra_core.Ledger.find_opt ledger addr with
      | Some account ->
        expect (account.Octra_core.Ledger_types.nonce = 0) "retry nonce";
        expect (account.encrypted_balance = None) "retry cipher"
      | None -> fail "retry account missing")

let circle_reject_case () =
  let request =
    VP.Circle_cell {
      pubkey = "invalid";
      cipher = "invalid";
      ciphertext_commitment = "invalid";
      proof_kind = VP.Circle_bound_zero;
      proof = "invalid";
      amount_commitment = "invalid";
    }
  in
  match Lwt_main.run (VW.classified_result request) with
  | Error (VW.Proof_rejected _) -> ()
  | Error failure -> fail (VW.verification_failure_message failure)
  | Ok () -> fail "forged circle proof accepted"

let validator_keys =
  ["octA"; "octB"; "octC"; "octD"]
  |> List.map (fun address ->
    let private_key, public_key = Mirage_crypto_ec.Ed25519.generate () in
    C.{
      address;
      pubkey = Mirage_crypto_ec.Ed25519.pub_to_octets public_key;
    },
    private_key)

let validator_set =
  validator_keys
  |> List.map fst
  |> C.make_validator_set

let sign address value =
  let _, private_key =
    List.find
      (fun (validator, _) ->
        String.equal validator.C.address address)
      validator_keys
  in
  Mirage_crypto_ec.Ed25519.sign ~key:private_key value

let finalized transaction receipt =
  let receipt_json = R.canonical receipt in
  let tx_hashes = [T.hash transaction] in
  let header =
    C.{
      proto_version = proto_version_current;
      chain_id = "octra-transition-recovery";
      epoch_id = 1L;
      prev_state_root = String.make 32 '\x11';
      tx_list_hash = CE.tx_list_hash_for_header tx_hashes;
      receipt_root = CH.receipt_root [receipt_json];
      proposed_state_root = String.make 32 '\x22';
      parent_commit_hash = Octra_net.Hash_domain.nil_hash;
      creator_addr = "octA";
      txid_hi = 1L;
      ts = 1.;
    }
  in
  let proposal_id = CH.proposal_id header in
  let vote address =
    let unsigned =
      C.{
        chain_id = header.chain_id;
        epoch_id = header.epoch_id;
        round = 0;
        vote_type = Precommit;
        proposal_id;
        validator = address;
        signature = String.make 64 '\x00';
      }
    in
    {
      unsigned with
      signature = sign address (CH.vote_sign_bytes unsigned);
    }
  in
  C.{
    chain_id = header.chain_id;
    epoch_id = header.epoch_id;
    commit_round = 0;
    header;
    proposal_id;
    precommits = List.map vote ["octA"; "octB"; "octC"];
    parent_commit = None;
  },
  J.{
    tx_hashes;
    txs = [transaction];
    receipts_json = [receipt_json];
  }

let recovered_bundle result =
  let stored = ref None in
  let recovery =
    JR.{
      read_journal = (fun () -> result);
      read_pending_epoch = (fun () -> Ok None);
      drop_invalid_unapplied = (fun ~head_epoch:_ -> Ok 0);
      head_epoch = (fun () -> 0);
      root_at_epoch = (fun _ -> None);
      current_root = (fun () -> Some (String.make 32 '\x11'));
      write_finality = (fun _ -> ());
      store_finalized = (fun ~epoch:_ ~validator_set:_ _ -> ());
      store_proposer = (fun _ -> ());
      store_expected_root = (fun ~epoch:_ ~root:_ -> ());
      store_bundle =
        (fun ~proposal_id:_ ~tx_hashes ~txs ~receipts_json ->
          stored := Some J.{ tx_hashes; txs; receipts_json });
      set_proposal = (fun _ _ -> ());
      reset_proposal_state = (fun () -> ());
      set_consensus_finalized = (fun _ -> ());
      clear_state_attested = (fun () -> ());
      commit_journal = (fun ~epoch:_ ~state_root:_ -> ());
      mark_quarantine = (fun reason -> fail reason);
      require_sync = (fun _ -> ());
    }
  in
  expect (JR.run recovery = JR.Armed) "journal recovery armed";
  match !stored with
  | Some bundle -> bundle
  | None -> fail "journal recovery bundle missing"

let recovery_case () =
  let path, store, ledger, pk, sk = setup "recovery" in
  let transaction = tx (payload pk sk true) in
  let plan =
    match
      Lwt_main.run
        (PL.prepare_encrypt_plan
           ~field_policy:PL.Unique_fields
           ledger
           transaction)
    with
    | Ok plan -> plan
    | Error e -> fail e.PL.reason
  in
  let transition_hash =
    PL.hash_prepared (PL.Prepared_encrypt plan)
  in
  let receipt = receipt ledger transaction transition_hash in
  let finalize, bundle = finalized transaction receipt in
  J.persist_certificate path ~validator_set finalize;
  J.persist_bundle path finalize bundle;
  Lwt_main.run (Octra_core.Store_irmin.close store);
  let reopened =
    Lwt_main.run (Octra_core.Store_irmin.open_store path)
  in
  Fun.protect
    ~finally:(fun () ->
      Lwt_main.run (Octra_core.Store_irmin.close reopened);
      clear_case path)
    (fun () ->
      let recovered =
        J.read_validated
          ~chain_id:"octra-transition-recovery"
          ~validator_set
          path
        |> recovered_bundle
      in
      let recovered_receipt =
        match
          Octra_core.Preverify_commit.receipts_of_strings
            recovered.J.receipts_json
        with
        | Ok [value] -> value
        | Ok _ -> fail "journal receipt count"
        | Error e -> fail e
      in
      let recovered_tx =
        match recovered.J.txs with
        | [value] -> value
        | _ -> fail "journal transaction count"
      in
      let recovered_ledger = Octra_core.Ledger.create reopened in
      begin
        match
          process
            Octra_core.Rule_graph.Active
            reopened
            recovered_ledger
            recovered_tx
            recovered_receipt
        with
        | Ok fee -> expect (Z.equal fee Z.one) "recovered fee"
        | Error (_, reason) -> fail reason
      end;
      Lwt_main.run
        (Octra_core.Ledger.flush_dirty_lwt recovered_ledger);
      let post_root =
        Lwt_main.run (Octra_core.Ledger.hash recovered_ledger)
      in
      let gate =
        Octra_core.Preverify_commit.create [recovered_receipt]
      in
      begin
        match
          Lwt_main.run
            (Octra_core.Preverify_commit.check_bound
               recovered_ledger
               gate
               [recovered_tx])
        with
        | Error _ -> ()
        | Ok () -> fail "duplicate receipt remained applicable"
      end;
      expect
        (Lwt_main.run (Octra_core.Ledger.hash recovered_ledger) = post_root)
        "duplicate recovery changed root";
      match Octra_core.Ledger.find_opt recovered_ledger addr with
      | Some account ->
        expect (account.Octra_core.Ledger_types.nonce = 1) "recovered nonce";
        expect
          (account.encrypted_balance = Some plan.PL.next_cipher)
          "recovered cipher"
      | None -> fail "recovered account missing")

let () =
  if not (Sys.file_exists "runtime_data") then Unix.mkdir "runtime_data" 0o755;
  if not (Sys.file_exists "runtime_data/private_transition_receipt") then
    Unix.mkdir "runtime_data/private_transition_receipt" 0o755;
  Unix.putenv "OCTRA_BFT_CRYPTO_PROFILE" "private_v1";
  pending_case ();
  prior_receipt_case ();
  forged_case ();
  valid_case ();
  mismatch_case ();
  worker_retry_case ();
  circle_reject_case ();
  recovery_case ();
  print_endline "status = pass test = private_transition_receipt"