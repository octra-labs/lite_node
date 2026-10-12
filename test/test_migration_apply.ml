(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module A = Octra_core.Pvac_migration_admission
module E = Octra_core.Epoch_exec
module FB = Octra_core.Crypto.FheBalance
module G = Octra_core.Rule_graph
module I = Octra_core.Epoch_index_commitment
module L = Octra_core.Ledger
module M = Octra_core.Pvac_migration
module P = Pvac_ffi
module PL = Octra_core.Private_ledger
module PT = Octra_core.Private_transition
module R = Octra_core.Pvac_legacy_public_replay
module S = Octra_core.Store_irmin
module T = Octra_core.Transaction
module W = Octra_core.Preverify_worker

external historical : P.pubkey -> P.seckey -> unit
  = "caml_pvac_test_set_historical_profile"

let check name value = if not value then failwith ("migration_apply: " ^ name)
let get = function Ok value -> value | Error reason -> failwith reason
let run = Lwt_main.run
let bytes value = Bytes.make 32 value
let b64 value = Base64.encode_exn (Bytes.to_string value)
let raw value = Bytes.to_string value
let digest value = Digestif.SHA256.(digest_string value |> to_hex)
let chain_id = "octra-devnet-9871-cluster"
let funds = Z.of_int 1_000_000
let secret = b64 (bytes '\031')
let public =
  let key = match Mirage_crypto_ec.Ed25519.priv_of_octets (raw (bytes '\031')) with
    | Ok key -> key
    | Error _ -> failwith "migration_apply: signing key invalid" in
  Mirage_crypto_ec.Ed25519.pub_of_priv key
  |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn
let address = Octra_core.Crypto.Address.address_from_pubkey public
let proposer = Octra_core.Crypto.Address.address_from_pubkey (b64 (bytes '\032'))

let transaction ?(nonce = 1) fields =
  let tx = T.{from = address; to_ = address; amount = Z.zero; nonce;
    ou = Z.of_int 1000; timestamp = 1.; signature = ""; public_key = Some public;
    message = None; op_type = KeySwitch;
    encrypted_data = Some (Yojson.Safe.to_string (`Assoc fields))} in
  let tx = T.sign_with_privkey tx secret in
  check "signed transaction" (T.verify tx public);
  tx

let replace name value fields = (name, value) :: List.remove_assoc name fields

let admission ~root ~cipher commitment =
  let audit = R.{audit_class = Hidden_witness; can_public_migrate = false;
    public_net = None; commitment_net = Some commitment; blockers = []; effects = [];
    reason = "authenticated history requires commitment proof"} in
  let entry = A.{address; source_cipher_hash = source_cipher_hash cipher;
    total = 2; decision = audit} in
  A.create ~classifier:A.Receipt_v1 ~chain_id ~snapshot_epoch:0 ~state_root:root
    ~activation_epoch:1 [entry] |> get

let capture store ledger =
  L.find_opt ledger address, run (L.get_pvac_pubkey ledger address),
  L.get_pvac_kat ledger address, run (S.get_account store address),
  L.get_total_supply ledger

let execute ?preverify ?(owner_mode = G.Active) ?(fail_flush = false)
    ?(cipher_mode = G.Prior)
    ~math ~epoch store ledger admitted tx =
  let backend = E.make_live_backend ~proof_mode:G.Active ~math
    ~emission_policy:Octra_core.Emission_policy.Guard
    ~emission_schedule:(Octra_core.Emission_schedule.of_env_exn (fun _ -> None))
    ~validator_policy:(Octra_core.Validator_policy.of_env_exn (fun _ -> None)) store ledger in
  let backend = if fail_flush then
    {backend with E.flush_dirty = (fun () -> Lwt.fail_with "migration flush interrupted")}
    else backend in
  let transition = PT.create ~math ~preverify ~ledger ~epoch_id:epoch
    ~owner_migration_mode:owner_mode ~proof_mode:G.Active ~field_policy:PL.Unique_fields
    ~cipher_mode
    ~result_policy:Octra_core.Private_result_policy.Recoverable
    ~legacy_replay:(A.decision admitted) ~limits:PT.{max_fhe = 1; max_stealth = 1} in
  let env : E.env = {chain_id; epoch_id = epoch; proposer_addr = proposer;
    validator_addrs = [proposer]; validator_pubkeys = [];
    prev_state_root = run (L.hash ledger); epoch_ts = float_of_int epoch;
    ready_state_root_at = None; ready_max_lag = 0} in
  let process_tx ~backend ~env tx = PT.process transition ~backend ~env tx in
  match preverify with
  | None -> run (E.run ~backend ~env ~txs:[tx] ~process_tx)
  | Some preverify -> run (E.run_checked ~preverify ~backend ~env ~txs:[tx] ~process_tx)

let preverify ?(cipher_mode = G.Prior) ~math ~epoch ledger admitted tx =
  run (W.run ~math ~cipher_mode ~field_policy:PL.Unique_fields ~strict:true ~ledger
    ~legacy_replay:(A.decision admitted ~epoch) tx)

let refused ?reason ~math ~epoch store ledger admitted name tx =
  let before = capture store ledger in
  begin match preverify ~math ~epoch ledger admitted tx with
  | W.Skip actual ->
    Option.iter (fun expected -> check (name ^ " preverify reason")
      (String.starts_with ~prefix:expected actual)) reason
  | W.Defer reason -> failwith (name ^ ": " ^ reason)
  | W.Ready _ -> failwith ("migration_apply: " ^ name ^ " preverify accepted")
  end;
  let result = execute ~math ~epoch store ledger admitted tx |> get in
  check (name ^ " apply accepted")
    (result.E.artifacts.confirmed = [] && List.length result.artifacts.rejected = 1);
  Option.iter (fun expected -> check (name ^ " apply reason")
    (List.for_all (fun item -> String.starts_with ~prefix:expected item.E.reason)
      result.artifacts.rejected)) reason;
  check (name ^ " changed state") (capture store ledger = before)

let test_apply math =
  let old_pk, old_sk = P.keygen_from_seed (P.default_params ()) (bytes '\001') in
  historical old_pk old_sk;
  let source = P.enc_value_seeded old_pk old_sk 3L (bytes '\002') |> FB.encode_cipher in
  let old_key = P.serialize_pubkey old_pk |> raw in
  let old_status = M.status_of_state ~cap:true ~cipher:source ~pubkey:(Some old_key) in
  check "historical profile selected"
    (old_status.cipher_class = M.V3 && old_status.key_class = M.Historical
     && M.needs_history_migration old_status);
  check "historical amount" (FB.get_balance old_pk old_sk source = Ok (Z.of_int 3));
  let pk, sk = P.keygen_from_seed (P.default_params ()) (bytes '\003') in
  let key = P.serialize_pubkey pk |> raw in
  let blind = bytes '\004' in
  let point amount = P.pedersen_commit_amount amount blind |> b64 in
  let payload amount =
    let ct = P.enc_value_seeded pk sk amount (bytes '\005') in
    let proof = P.make_zero_proof_bound ~math pk sk ct amount blind in
    ["migration_mode", `String "historical_owner_proof";
     "new_pubkey", `String (Base64.encode_exn key);
     "aes_kat", `String (Octra_core.Pvac_registry.expected_kat ());
     "source_cipher_hash", `String (A.source_cipher_hash source);
     "new_cipher", `String (FB.encode_cipher ct);
     "new_zero_proof", `String (FB.encode_zero_proof proof);
     "amount_commitment", `String (point amount)] in
  let good = payload 3L in
  let extra = payload 4L in
  Test_workspace.with_dir "migration_apply" (fun dir ->
    let path = Filename.concat dir "irmin_store" in
    let store = run (S.open_store path) in
    let expected = Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
      let ledger = L.create store in
      L.add_account_with_pubkey ledger address funds public |> get;
      L.update_enc_balance ledger address source |> get;
      run (L.set_pvac_pubkey ledger address old_key);
      L.set_pvac_kat ledger address (Octra_core.Pvac_registry.expected_kat ());
      run (L.flush_dirty_lwt ledger);
      run (S.set_meta store "total_supply" (Z.to_string (Z.add funds (Z.of_int 3))));
      run (S.set_meta store "emission_remaining" "0");
      let root = run (S.get_head_hash store) |> Option.get in
      let state_root = I.folded_state_root ~ledger_state_root:root
        ~epoch_index_root:I.genesis_root in
      let admitted = admission ~root:state_root ~cipher:source (point 3L) in
      let check_bad name reason fields =
        refused ~reason ~math ~epoch:1 store ledger admitted name (transaction fields) in
      check_bad "history point substitution"
        "historical owner commitment differs from finalized history" extra;
      check_bad "proof amount substitution" "new encrypted balance proof failed: "
        (replace "amount_commitment" (`String (point 3L)) extra);
      check_bad "source substitution" "encrypted balance changed before historical owner verification"
        (replace "source_cipher_hash" (`String (digest "other")) good);
      check_bad "amount disclosure" "historical owner migration must not disclose amount blinding"
        (replace "amount_blinding" (`String (b64 blind)) good);
      check_bad "standard historical switch" old_status.reason
        (replace "migration_mode" (`String "standard") good);
      check_bad "cipher rebinding" "legacy ciphertext rebinding is not statement preserving"
        (replace "migration_mode" (`String "rejected_rebinding") good);
      let tx = transaction good in
      refused ~reason:"migration entitlement artifact unavailable"
        ~math ~epoch:1 store ledger (A.disabled ~chain_id) "missing admission" tx;
      let wrong = admission ~root:state_root ~cipher:(source ^ "other") (point 3L) in
      refused ~reason:"migration entitlement source ciphertext mismatch"
        ~math ~epoch:1 store ledger wrong "admission source" tx;
      let before = capture store ledger in
      let inactive = execute ~owner_mode:G.Prior ~math ~epoch:1 store ledger admitted tx |> get in
      check "owner activation required" (inactive.artifacts.confirmed = []);
      check "inactive refusal preserves state" (capture store ledger = before);
      let receipt = match preverify ~math ~epoch:1 ledger admitted tx with
        | W.Ready value -> value
        | W.Skip reason | W.Defer reason -> failwith reason in
      let gate = Octra_core.Preverify_commit.create [receipt] in
      let root = run (S.get_head_hash store) in
      let interrupted = execute ~preverify:gate ~fail_flush:true ~math ~epoch:1
        store ledger admitted tx in
      check "flush interruption returned"
        (interrupted = Error "Failure(\"migration flush interrupted\")");
      check "flush interruption preserves state" (capture store ledger = before);
      check "flush interruption preserves root" (run (S.get_head_hash store) = root);
      let result = execute ~preverify:gate ~math ~epoch:1 store ledger admitted tx |> get in
      check "migration confirmed" (List.map (fun (item, _) -> T.hash item)
        result.artifacts.confirmed = [T.hash tx] && result.artifacts.rejected = []);
      let account = L.find_opt ledger address |> Option.get in
      check "migration fee" (Z.equal account.balance (Z.sub funds tx.ou));
      check "migration nonce" (account.nonce = 1);
      let cipher = Option.get account.encrypted_balance in
      check "migration amount" (FB.get_balance pk sk cipher = Ok (Z.of_int 3));
      check "migration key" (run (L.get_pvac_pubkey ledger address) = Some key);
      check "migration public supply" (Z.equal (L.get_total_supply ledger) funds);
      check "migration recorded supply"
        (run (S.get_meta store "total_supply") = Some (Z.to_string (Z.add funds (Z.of_int 3))));
      let accepted = capture store ledger in
      let replay = execute ~math ~epoch:2 store ledger admitted tx |> get in
      check "migration replay refused" (replay.artifacts.confirmed = []);
      check "migration replay unchanged" (capture store ledger = accepted);
      account, cipher) in
    let store = run (S.open_store ~readonly:true path) in
    Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
      let account, cipher = expected in
      let ledger = L.create store in
      check "migration restart account" (L.find_opt ledger address = Some account);
      check "migration restart key" (run (L.get_pvac_pubkey ledger address) = Some key);
      check "migration restart amount" (FB.get_balance pk sk cipher = Ok (Z.of_int 3))));
  Printf.printf "event = migration_apply math = %b status = pass\n%!" math

let test_recovery tool =
  let channel = Unix.open_process_args_in tool [|tool; "--history"|] in
  let images = Fun.protect ~finally:(fun () ->
    check "image generator exit" (Unix.close_process_in channel = Unix.WEXITED 0))
    (fun () -> [input_line channel; input_line channel]) in
  let decode hex =
    let bytes = String.init (String.length hex / 2) (fun index ->
      Char.chr (int_of_string ("0x" ^ String.sub hex (2 * index) 2))) in
    "hfhe_v1|" ^ Base64.encode_exn bytes in
  let pk, sk = P.keygen_from_seed (P.default_params ()) (bytes '\065') in
  let key = P.serialize_pubkey pk |> raw in
  let blind = Bytes.make 32 '\000' in
  let payload amount =
    let ct = P.enc_value_seeded pk sk amount (bytes '\066') in
    let proof = P.make_zero_proof_bound ~math:true pk sk ct amount blind in
    ["migration_mode", `String "public_history";
     "new_pubkey", `String (Base64.encode_exn key);
     "aes_kat", `String (Octra_core.Pvac_registry.expected_kat ());
     "new_cipher", `String (FB.encode_cipher ct);
     "new_zero_proof", `String (FB.encode_zero_proof proof);
     "amount_commitment", `String (P.pedersen_commit_amount amount blind |> b64);
     "amount_blinding", `String (b64 blind)] in
  let good = payload 3L in
  let inflated = payload 4L in
  let zero = replace "migration_mode" (`String "verified_zero_reset") (payload 0L) in
  let proven = FB.encode_cipher (P.enc_value_seeded pk sk 3L (bytes '\067')) in
  List.iteri (fun index source ->
    let fields payload =
      if index = 3 then
        payload |> List.remove_assoc "amount_blinding"
        |> replace "migration_mode" (`String "commitment_history")
      else payload in
    let good = fields good in
    let inflated = fields inflated in
    List.iter (fun present ->
      Test_workspace.with_dir "history_recovery" (fun dir ->
        let store = run (S.open_store (Filename.concat dir "store")) in
        Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
          let ledger = L.create store in
          L.add_account_with_pubkey ledger address funds public |> get;
          L.update_enc_balance ledger address source |> get;
          if present then run (L.set_pvac_pubkey ledger address key);
          run (L.flush_dirty_lwt ledger);
          run (S.set_meta store "total_supply" "1000003");
          run (S.set_meta store "emission_remaining" "0");
          let head = run (S.get_head_hash store) |> Option.get in
          let root = I.folded_state_root ~ledger_state_root:head
            ~epoch_index_root:I.genesis_root in
          let verdict =
            if index = 3 then R.{audit_class = Hidden_witness;
              can_public_migrate = false; public_net = None;
              commitment_net = Some (P.pedersen_commit_amount 3L blind |> b64);
              blockers = []; effects = []; reason = "verified commitment history"}
            else R.replay_history ~addr:address [`Assoc [
              "from", `String address; "to_", `String address;
              "op_type", `String "encrypt"; "amount", `String "3"]] in
          let record = A.{address; source_cipher_hash = source_cipher_hash source;
            total = 1; decision = verdict} in
          let records = A.create ~chain_id ~snapshot_epoch:0 ~state_root:root
            ~activation_epoch:1 [record] |> get in
          let state () =
            capture store ledger,
            run (S.get_tree_hash_at_path store ["accounts"; address]),
            run (S.get_meta store "total_supply") in
          let before = state () in
          let check_bad label records fields =
            let tx = transaction fields in
            check (label ^ " preverify") (match preverify ~cipher_mode:G.Active
                ~math:true ~epoch:1 ledger records tx with
              | W.Skip _ -> true | _ -> false);
            let result = execute ~cipher_mode:G.Active ~math:true ~epoch:1
              store ledger records tx |> get in
            check (label ^ " rejected") (result.artifacts.confirmed = []);
            check (label ^ " unchanged")
              (state () = before) in
          check_bad "missing record" (A.disabled ~chain_id) good;
          check_bad "inflated amount" records inflated;
          check_bad "zero replacement" records zero;
          let wrong = A.create ~chain_id ~snapshot_epoch:0 ~state_root:root
            ~activation_epoch:1 [{record with source_cipher_hash = digest "other"}] |> get in
          check_bad "wrong source" wrong good;
          if index = 2 && not present then begin
            let result = execute ~cipher_mode:G.Prior ~math:true ~epoch:1
              store ledger records (transaction good) |> get in
            check "prior missing key refused" (result.artifacts.confirmed = []);
            check "prior state unchanged"
              (state () = before)
          end;
          let tx = transaction good in
          if index = 2 then begin
            let head = run (S.get_head_hash store) in
            let interrupted = execute ~cipher_mode:G.Active ~math:true ~epoch:1
              ~fail_flush:true store ledger records tx in
            check "record write interrupted"
              (interrupted = Error "Failure(\"migration flush interrupted\")");
            check "record interruption keeps funds"
              (state () = before);
            check "record interruption keeps root" (run (S.get_head_hash store) = head)
          end;
          let result = execute ~cipher_mode:G.Active ~math:true ~epoch:1
            store ledger records tx |> get in
          check "record recovery confirmed" (List.length result.artifacts.confirmed = 1);
          let account = L.find_opt ledger address |> Option.get in
          let cipher = Option.get account.encrypted_balance in
          check "record amount" (FB.get_balance pk sk cipher = Ok (Z.of_int 3));
          check "record key" (run (L.get_pvac_pubkey ledger address) = Some key);
          check "record nonce" (account.nonce = 1);
          check "record fee" (account.balance = Z.sub funds tx.ou);
          check "record supply" (run (S.get_meta store "total_supply") = Some "1000003");
          let after = capture store ledger in
          let repeated = execute ~cipher_mode:G.Active ~math:true ~epoch:2
            store ledger records (transaction ~nonce:2 good) |> get in
          check "record cannot apply twice" (repeated.artifacts.confirmed = []);
          check "repeat keeps funds" (capture store ledger = after))))
      (if index >= 2 then [false] else [false; true]))
    (List.map decode images @ [proven; proven]);
  Printf.printf "event = history_recovery status = pass\n%!"

let test_zero () =
  let old_pk, old_sk = P.keygen_from_seed (P.default_params ()) (bytes '\101') in
  historical old_pk old_sk;
  let old_key = P.serialize_pubkey old_pk |> raw in
  let source = P.enc_value_seeded old_pk old_sk 0L (bytes '\102') |> FB.encode_cipher in
  let pk, sk = P.keygen_from_seed (P.default_params ()) (bytes '\103') in
  let key = P.serialize_pubkey pk |> raw in
  let blind = bytes '\104' in
  let cipher = P.enc_value_seeded pk sk 0L (bytes '\105') in
  let proof = P.make_zero_proof_bound ~math:true pk sk cipher 0L blind in
  let fields = [
    "migration_mode", `String "verified_zero_reset";
    "new_pubkey", `String (Base64.encode_exn key);
    "aes_kat", `String (Octra_core.Pvac_registry.expected_kat ());
    "new_cipher", `String (FB.encode_cipher cipher);
    "new_zero_proof", `String (FB.encode_zero_proof proof);
    "amount_commitment", `String (P.pedersen_commit_amount 0L blind |> b64);
    "amount_blinding", `String (b64 blind)] in
  let tx = transaction fields in
  let prior_epoch = (Option.get (G.cipher_activation_for_chain chain_id)).activation_epoch - 1 in
  let active_epoch = prior_epoch + 1 in
  let prior = G.Prior in
  let active = G.Active in
  check "zero rule schedule differs"
    (G.cipher_migration_at ~chain_id ~epoch:prior_epoch = G.Prior
     && G.cipher_migration_at ~chain_id ~epoch:active_epoch = G.Active);
  Test_workspace.with_dir "zero_history" (fun dir ->
    let store = run (S.open_store (Filename.concat dir "store")) in
    Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
      let ledger = L.create store in
      L.add_account_with_pubkey ledger address funds public |> get;
      L.update_enc_balance ledger address source |> get;
      run (L.set_pvac_pubkey ledger address old_key);
      run (L.flush_dirty_lwt ledger);
      run (S.set_meta store "total_supply" (Z.to_string funds));
      run (S.set_meta store "emission_remaining" "0");
      let head = run (S.get_head_hash store) |> Option.get in
      let root = I.folded_state_root ~ledger_state_root:head
        ~epoch_index_root:I.genesis_root in
      let clean = R.replay_history ~addr:address [] in
      check "zero public history" (clean.can_public_migrate && clean.public_net = Some Z.zero);
      let records ?(hash = A.source_cipher_hash source) verdict =
        let entry = A.{address; source_cipher_hash = hash; total = 0; decision = verdict} in
        A.create ~chain_id ~snapshot_epoch:0 ~state_root:root
          ~activation_epoch:1 [entry] |> get in
      let ready = records clean in
      let before = capture store ledger in
      let checked = run (PL.key_switch_plan ~math:true ~cipher_mode:prior
        ~field_policy:PL.Unique_fields ~strict:true ledger tx) in
      check "prior zero check" (Result.is_ok checked);
      let checked = run (PL.key_switch_plan ~math:true ~cipher_mode:active
        ~field_policy:PL.Unique_fields ~strict:true ledger tx) in
      check "prior cache cannot authorize zero reset" (match checked with
        | Error failure -> failure.PL.reason = "zero reset requires verified zero public history"
        | Ok _ -> false);
      let denied name records =
        check (name ^ " preverify") (match preverify ~cipher_mode:active
            ~math:true ~epoch:active_epoch ledger records tx with
          | W.Skip _ -> true | _ -> false);
        let result = execute ~cipher_mode:active ~math:true ~epoch:active_epoch
          store ledger records tx |> get in
        check (name ^ " apply") (result.artifacts.confirmed = []);
        check (name ^ " funds") (capture store ledger = before) in
      denied "zero missing record" (A.disabled ~chain_id);
      denied "zero nonzero record" (records {clean with public_net = Some (Z.of_int 3)});
      denied "zero private record" (records {clean with can_public_migrate = false;
        public_net = None; audit_class = R.Hidden_witness});
      denied "zero wrong source" (records ~hash:(digest "other") clean);
      let receipt = match preverify ~cipher_mode:active ~math:true ~epoch:active_epoch
          ledger ready tx with
        | W.Ready value -> value
        | W.Skip reason | W.Defer reason -> failwith reason in
      let gate = Octra_core.Preverify_commit.create [receipt] in
      let head = run (S.get_head_hash store) in
      let interrupted = execute ~preverify:gate ~cipher_mode:active ~fail_flush:true
        ~math:true ~epoch:active_epoch store ledger ready tx in
      check "zero interruption" (interrupted = Error "Failure(\"migration flush interrupted\")");
      check "zero interruption funds" (capture store ledger = before);
      check "zero interruption root" (run (S.get_head_hash store) = head);
      let result = execute ~preverify:gate ~cipher_mode:active ~math:true
        ~epoch:active_epoch store ledger ready tx |> get in
      check "zero confirmed" (List.length result.artifacts.confirmed = 1);
      let account = L.find_opt ledger address |> Option.get in
      check "zero fee and nonce" (account.balance = Z.sub funds tx.ou && account.nonce = 1);
      check "zero key" (run (L.get_pvac_pubkey ledger address) = Some key);
      check "zero amount" (FB.get_balance pk sk (Option.get account.encrypted_balance) = Ok Z.zero);
      check "zero supply" (run (S.get_meta store "total_supply") = Some (Z.to_string funds))));
  Printf.printf "event = zero_history status = pass\n%!"

let () =
  Mirage_crypto_rng_unix.use_default ();
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--zero" then test_zero ()
  else begin
    if Array.length Sys.argv = 2 then List.iter test_apply [false; true]
    else if Array.length Sys.argv <> 3 || Sys.argv.(2) <> "--history" then
      failwith "migration_apply: expected image tool and optional --history";
    test_recovery Sys.argv.(1);
    test_zero ()
  end