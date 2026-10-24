(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module F = Octra_core.Crypto.FheBalance
module P = Pvac_ffi
module V = Octra_core.Pvac_verify_protocol
module W = Octra_core.Pvac_verify_worker
module E = Octra_core.Epoch_exec
module G = Octra_core.Rule_graph
module L = Octra_core.Ledger
module S = Octra_core.Store_irmin
module T = Octra_core.Transaction
module A = Octra_core.Private_transition
module PL = Octra_core.Private_ledger
module K = Octra_node_runtime.Consensus_key_switch_preverify

let run = Lwt_main.run

external historical : P.pubkey -> P.seckey -> unit
  = "caml_pvac_test_set_historical_profile"

let check name condition =
  if not condition then failwith ("cipher_migration: " ^ name)

let seed value = Bytes.make 32 value
let key pk = P.serialize_pubkey pk |> Bytes.to_string
let point amount blind =
  P.pedersen_commit_amount amount blind |> Bytes.to_string |> Base64.encode_exn

let verify math request =
  Lwt_main.run (W.classified_result ~keyed:false ~math ~priority:W.Required request)

let rejected name = function
  | Error (W.Proof_rejected _) -> ()
  | Error _ -> failwith ("cipher_migration: " ^ name ^ " failed technically")
  | Ok () -> failwith ("cipher_migration: " ^ name ^ " accepted")

let accepted name = function
  | Ok () -> ()
  | Error (W.Proof_rejected reason) -> failwith (name ^ ": " ^ reason)
  | Error _ -> failwith ("cipher_migration: " ^ name ^ " failed technically")

let timed stage run =
  let start = Unix.gettimeofday () in
  let value = run () in
  Printf.printf "event = cipher_migration stage = %s seconds = %.3f\n%!"
    stage (Unix.gettimeofday () -. start);
  value

let state_check math old_key source new_key target old_proof new_proof commitment =
  let signing = seed '\071' in
  let private_key = Base64.encode_exn (Bytes.to_string signing) in
  let public_key = match Mirage_crypto_ec.Ed25519.priv_of_octets (Bytes.to_string signing) with
    | Error _ -> failwith "signing key rejected"
    | Ok key -> Mirage_crypto_ec.Ed25519.pub_of_priv key
      |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn in
  let address = Octra_core.Crypto.Address.address_from_pubkey public_key in
  let proposer = Octra_core.Crypto.Address.address_from_pubkey
    (Base64.encode_exn (Bytes.to_string (seed '\072'))) in
  let fields = ["migration_mode", `String "cipher_proof";
    "new_pubkey", `String (Base64.encode_exn new_key);
    "aes_kat", `String (Octra_core.Pvac_registry.expected_kat ());
    "source_cipher_hash", `String (Digestif.SHA256.(digest_string source |> to_hex));
    "new_cipher", `String target; "old_zero_proof", `String old_proof;
    "new_range_proof", `String new_proof; "amount_commitment", `String commitment] in
  let tx ?(nonce = 1) fields = T.sign_with_privkey T.{from = address; to_ = address;
    amount = Z.zero; nonce; ou = Z.of_int 3000; timestamp = 1.;
    signature = ""; public_key = Some public_key; message = None; op_type = KeySwitch;
    encrypted_data = Some (Yojson.Safe.to_string (`Assoc fields))} private_key in
  check "signed migration" (T.verify (tx fields) public_key);
  check "migration fits payload limit" (Octra_core.Tx_payload.admit (tx fields) = Ok ());
  Test_workspace.with_dir "cipher_migration" (fun path ->
    let store = run (S.open_store (Filename.concat path "store")) in
    Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
      let ledger = L.create store in
      L.add_account_with_pubkey ledger address (Z.of_int 1_000_000) public_key
      |> Result.get_ok;
      L.update_enc_balance ledger address source |> Result.get_ok;
      run (L.set_pvac_pubkey ledger address old_key);
      L.set_pvac_kat ledger address (Octra_core.Pvac_registry.expected_kat ());
      run (L.flush_dirty_lwt ledger);
      run (S.set_meta store "total_supply" "1000003");
      run (S.set_meta store "emission_remaining" "0");
      let account_state () = L.find_opt ledger address,
        run (L.get_pvac_pubkey ledger address), L.get_pvac_kat ledger address,
        run (S.get_account store address), run (S.get_meta store "total_supply") in
      let capture () = account_state (), run (S.get_head_hash store) in
      let initial = account_state () in
      List.iter (fun op_type ->
        let operation = {(tx fields) with T.op_type} in
        check "cipher selection only applies to key switch"
          (not (PL.key_switch_requests_cipher_proof
            ~field_policy:PL.Unique_fields operation)))
        [T.EncryptOp; T.DecryptOp; T.StealthOp; T.ClaimOp];
      let blocked () =
        List.iter (fun (cipher_mode, strict) ->
          let pool = K.create ~registered:(fun () -> false) ~keyed:(fun () -> false) ~math:(fun () -> math)
            ~field_policy:(fun () -> PL.Unique_fields) ~strict:(fun () -> strict)
            ~cipher_mode:(fun () -> cipher_mode) ledger in
          check "inactive pool does not queue proof"
            (K.admit pool (tx fields) = Octra_core.Preverify_availability.Unmanaged);
          check "inactive pool has no work" ((K.stats pool).pending = 0);
          let result = run (PL.preverify_key_switch_artifact ~registered:false ~keyed:false ~math ~cipher_mode
            ~field_policy:PL.Unique_fields ~strict ledger (tx fields)) in
          check "inactive proof refused" (match result with
            | Error failure -> failure.PL.reason = "unknown key_switch migration_mode = cipher_proof"
            | Ok _ -> false)) [G.Prior, true; G.Active, false] in
      blocked ();
      let apply_txs ?(interrupt = false) cipher_mode txs =
        let backend = E.make_live_backend ~proof_mode:G.Active ~math
          ~emission_policy:Octra_core.Emission_policy.Guard
          ~emission_schedule:(Octra_core.Emission_schedule.of_env_exn (fun _ -> None))
          ~validator_policy:(Octra_core.Validator_policy.of_env_exn (fun _ -> None))
          store ledger in
        let backend = if interrupt then
          {backend with E.flush_dirty = (fun () -> Lwt.fail_with "migration write interrupted")}
          else backend in
        let transition = A.create ~registered:false ~keyed:false ~math ~preverify:None ~ledger ~epoch_id:1
          ~owner_migration_mode:G.Active ~cipher_mode ~proof_mode:G.Active
          ~field_policy:Octra_core.Private_ledger.Unique_fields
          ~result_policy:Octra_core.Private_result_policy.Recoverable
          ~legacy_replay:(fun ~epoch:_ ~address:_ ~cipher:_ ->
            failwith "cipher proof read migration records")
          ~limits:A.{max_fhe = 1; max_stealth = 1} in
        let env : E.env = {chain_id = "octra-devnet-9871-cluster"; epoch_id = 1;
          proposer_addr = proposer; validator_addrs = [proposer]; validator_pubkeys = [];
          prev_state_root = run (L.hash ledger); epoch_ts = 1.;
          ready_state_root_at = None; ready_max_lag = 0} in
        let process_tx ~backend ~env tx = A.process transition ~backend ~env tx in
        run (E.run ~backend ~env ~txs ~process_tx) in
      let apply ?interrupt mode fields = apply_txs ?interrupt mode [tx fields] in
      let empty = apply_txs G.Prior [] |> Result.get_ok in
      check "empty epoch preserves account" (account_state () = initial);
      check "empty epoch has no transactions"
        (empty.artifacts.confirmed = [] && empty.artifacts.rejected = []);
      let before = capture () in
      let inactive = apply G.Prior fields |> Result.get_ok in
      check "inactive refused" (inactive.artifacts.confirmed = []);
      check "inactive reason preserves compatibility"
        (List.exists (fun rejected -> rejected.E.reason =
          "unknown key_switch migration_mode = cipher_proof") inactive.artifacts.rejected);
      check "inactive preserves state" (capture () = before);
      let refused name fields =
        let result = apply G.Active fields |> Result.get_ok in
        check (name ^ " refused") (result.artifacts.confirmed = []);
        check (name ^ " preserves state") (capture () = before) in
      refused "source replacement"
        (("source_cipher_hash", `String (String.make 64 '0')) ::
          List.remove_assoc "source_cipher_hash" fields);
      refused "old key supplied"
        (("old_bound_pubkey", `String (Base64.encode_exn new_key)) :: fields);
      refused "blinding supplied" (("amount_blinding", `String commitment) :: fields);
      let interrupted = apply ~interrupt:true G.Active fields in
      check "write interruption refused"
        (interrupted = Error "Failure(\"migration write interrupted\")");
      check "write interruption preserves state" (capture () = before);
      let inactive = apply G.Prior fields |> Result.get_ok in
      check "cached proof cannot bypass activation" (inactive.artifacts.confirmed = []);
      check "cached refusal preserves state" (capture () = before);
      blocked ();
      let prepared = run (PL.key_switch_plan ~registered:false ~keyed:false ~math ~cipher_mode:G.Active
        ~field_policy:PL.Unique_fields ~strict:true ledger (tx fields)) |> Result.get_ok in
      let blocked_batch = run (Octra_core.Preverify_worker.run_many ~registered:false ~keyed:false ~math
        ~field_policy:PL.Unique_fields ~strict:true ~ledger
        ~prepared:(fun _ -> Lwt.return (Octra_core.Preverify_availability.Ready
          (PL.Prepared_key_switch prepared))) [tx fields]) in
      check "prepared proof cannot bypass inactive precheck" (blocked_batch.ready = []);
      List.iter (fun mode ->
        let result = run (Octra_core.State_preview.with_state
          ~base_store:store ~base_ledger:ledger ~epoch_id:1
          ~proposal_id:"cipher_sender"
          (fun _ fork ->
            let confirmed = ref false in
            let rejected = ref None in
            let counted = ref 0 in
            let open Lwt.Syntax in
            let* () = Octra_node_runtime.Consensus_epoch_key_switch_shell.run_live_ledger_tx
              {registered = false; ledger = fork; field_policy = PL.Unique_fields; strict = true; math; keyed = false;
                cipher_mode = mode; legacy_replay = (fun _ -> failwith "unexpected history read");
                gate = (fun () -> None);
                reject_gate = (fun _ -> failwith "unexpected transaction gate");
                record_rejected = (fun _ _ reason -> rejected := Some reason);
                continue_after_reject = (fun ~consume_nonce ->
                  check "refused sender keeps nonce" (not consume_nonce); Lwt.return_unit);
                short_addr = Fun.id; incr_fhe = (fun () -> incr counted);
                confirm = (fun () -> confirmed := true; Lwt.return_unit)} (tx fields) in
            let active = mode = G.Active in
            check "sender activation" (!confirmed = active && !counted = (if active then 1 else 0));
            check "sender refusal" (if active then !rejected = None else
              !rejected = Some "unknown key_switch migration_mode = cipher_proof");
            let* key = L.get_pvac_pubkey fork address in
            check "sender key" (key = Some (if active then new_key else old_key));
            let account = L.find_opt fork address |> Option.get in
            check "sender ciphertext" (account.encrypted_balance =
              Some (if active then target else source));
            Lwt.return_ok ())) in
        check "sender preview returned" (result = Ok ());
        check "sender preview isolated" (capture () = before)) [G.Prior; G.Active];
      let result = apply G.Active fields |> Result.get_ok in
      check "migration confirmed without records"
        (List.length result.artifacts.confirmed = 1 && result.artifacts.rejected = []);
      let account = L.find_opt ledger address |> Option.get in
      check "source replaced once" (account.nonce = 1
        && account.encrypted_balance = Some target);
      check "only fee deducted" (account.balance = Z.of_int 997_000);
      check "registered key replaced" (run (L.get_pvac_pubkey ledger address) = Some new_key);
      check "supply preserved" (run (S.get_meta store "total_supply") = Some "1000003");
      let after = capture () in
      let replay = apply G.Active fields |> Result.get_ok in
      check "replay refused" (replay.artifacts.confirmed = []);
      check "replay preserves state" (capture () = after);
      let replay = apply_txs G.Active [tx ~nonce:2 fields] |> Result.get_ok in
      check "fresh nonce proof replay refused" (replay.artifacts.confirmed = []);
      check "fresh nonce replay preserves state" (capture () = after));
    let store = run (S.open_store ~readonly:true (Filename.concat path "store")) in
    Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
      let ledger = L.create store in
      let account = L.find_opt ledger address |> Option.get in
      check "restart retains balance" (account.balance = Z.of_int 997_000
        && account.nonce = 1 && account.encrypted_balance = Some target);
      check "restart retains key" (run (L.get_pvac_pubkey ledger address) = Some new_key);
      check "restart retains supply" (run (S.get_meta store "total_supply") = Some "1000003")))

let test_pair ?(deep = false) math =
  let old_pk, old_sk = P.keygen_from_seed (P.default_params ()) (seed '\051') in
  historical old_pk old_sk;
  let old_ct = P.enc_value_seeded old_pk old_sk 3L (seed '\052') in
  let old_ct = if not deep then old_ct else
    let rec extend count cipher =
      let layers = (P.cipher_shape cipher).P.base_layers in
      if layers = 8 then cipher
      else if layers > 8 || count = 8 then failwith "deep cipher shape rejected"
      else
        let zero = P.enc_value_seeded old_pk old_sk 0L
          (seed (Char.chr (80 + count))) in
        extend (count + 1) (P.ct_add old_pk cipher zero) in
    extend 0 old_ct in
  let new_pk, new_sk = P.keygen_from_seed (P.default_params ()) (seed '\053') in
  let new_ct = P.enc_value_seeded new_pk new_sk 3L (seed '\054') in
  check "old amount" (P.dec_value old_pk old_sk old_ct = 3L);
  check "new amount" (P.dec_value new_pk new_sk new_ct = 3L);
  let blind = seed '\055' in
  let commitment = point 3L blind in
  let old_proof = timed "old_proof" (fun () ->
    P.make_zero_proof_bound_historical_migration ~keyed:false
      ~math old_pk old_sk old_ct 3L blind |> F.encode_zero_proof) in
  let new_proof = timed "new_proof" (fun () ->
    P.make_zero_proof_bound_range ~keyed:false
      ~math new_pk new_sk new_ct 3L blind |> F.encode_bound_range_proof) in
  Printf.printf "event = cipher_migration old_bytes = %d new_bytes = %d\n%!"
    (String.length old_proof) (String.length new_proof);
  let old_request ?(pubkey = key old_pk) ?(cipher = F.encode_cipher old_ct)
      ?(proof = old_proof) ?(commitment = commitment) () =
    V.Historical_migration_claim { pubkey; cipher; proof; commitment; strict = true } in
  let new_request ?(pubkey = key new_pk) ?(cipher = F.encode_cipher new_ct)
      ?(proof = new_proof) ?(commitment = commitment) () =
    V.Range_bound { pubkey; cipher; proof; commitment; strict = true } in
  accepted "old ciphertext" (verify math (old_request ()));
  accepted "new ciphertext" (verify math (new_request ()));
  rejected "old amount" (verify math (old_request ~commitment:(point 4L blind) ()));
  rejected "new amount" (verify math (new_request ~commitment:(point 4L blind) ()));
  let other_ct = P.enc_value_seeded old_pk old_sk 3L (seed '\056') in
  rejected "old ciphertext replacement"
    (verify math (old_request ~cipher:(F.encode_cipher other_ct) ()));
  rejected "old key replacement" (verify math (old_request ~pubkey:(key new_pk) ()));
  rejected "new key replacement" (verify math (new_request ~pubkey:(key old_pk) ()));
  let plain = P.make_zero_proof_bound ~keyed:false ~math new_pk new_sk new_ct 3L blind in
  rejected "missing range proof"
    (verify math (new_request ~proof:(F.encode_zero_proof plain) ()));
  if math then timed "state" (fun () -> state_check math (key old_pk) (F.encode_cipher old_ct)
    (key new_pk) (F.encode_cipher new_ct) old_proof new_proof commitment);
  Printf.printf "event = cipher_migration math = %b bases = %d status = pass\n%!"
    math (P.cipher_shape old_ct).P.base_layers

let wallet_server ~keyed path =
  let output = Unix.dup Unix.stdout |> Unix.out_channel_of_descr in
  Unix.dup2 Unix.stderr Unix.stdout;
  let json = Yojson.Safe.from_file path in
  let field name = Yojson.Safe.Util.(member name json |> to_string) in
  let source = field "source_cipher" in
  let old_key = Base64.decode_exn (field "source_key") in
  let _, sk = P.keygen_from_seed (P.default_params ()) (seed '\051') in
  let signing = Bytes.to_string (seed '\051') in
  let public = match Mirage_crypto_ec.Ed25519.priv_of_octets signing with
    | Error _ -> failwith "wallet signing key invalid"
    | Ok key -> Mirage_crypto_ec.Ed25519.pub_of_priv key
      |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn in
  let address = Octra_core.Crypto.Address.address_from_pubkey public in
  let chain_id = "octra-devnet-9871-cluster" in
  let proposer = Octra_core.Crypto.Address.address_from_pubkey
    (Base64.encode_exn (Bytes.to_string (seed '\072'))) in
  Test_workspace.with_dir "wallet_cycle" (fun path ->
    let store = run (S.open_store (Filename.concat path "store")) in
    Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
      let ledger = L.create store in
      L.add_account_with_pubkey ledger address (Z.of_int 1_000_000) public |> Result.get_ok;
      L.update_enc_balance ledger address source |> Result.get_ok;
      run (L.set_pvac_pubkey ledger address old_key);
      L.set_pvac_kat ledger address (Octra_core.Pvac_registry.expected_kat ());
      run (L.flush_dirty_lwt ledger);
      run (S.set_meta store "total_supply" "1000003");
      run (S.set_meta store "emission_remaining" "0");
      let rule = if keyed then Octra_core.Rule_graph.proof_key_activation_for_chain
        else Octra_core.Rule_graph.cipher_activation_for_chain in
      let epoch = ref (Option.get (rule chain_id)).activation_epoch in
      let cipher () = (L.find_opt ledger address |> Option.get).encrypted_balance
        |> Option.value ~default:"0" in
      let response method_ params =
        let account = L.find_opt ledger address |> Option.get in
        match method_ with
        | "octra_balance" -> `Assoc ["balance_raw", `String (Z.to_string account.balance);
            "nonce", `Int account.nonce; "pending_nonce", `Int account.nonce]
        | "octra_pvacPubkey" -> `Assoc ["pvac_pubkey",
            (run (L.get_pvac_pubkey ledger address) |> Option.map Base64.encode_exn
             |> Option.fold ~none:`Null ~some:(fun value -> `String value))]
        | "octra_encryptedBalance" -> `Assoc ["cipher", `String (cipher ())]
        | "octra_recommendedFee" -> `Assoc ["recommended", `String "3000"]
        | "octra_pvacMigrationStatus" ->
          let status = Octra_core.Pvac_migration.status_of_state ~cap:true
            ~cipher:(cipher ()) ~pubkey:(run (L.get_pvac_pubkey ledger address)) in
          Octra_node_runtime.Rpc_view.pvac_migration_status ~keyed ~addr:address
            ~cipher:(cipher ()) ~epoch:!epoch ~math:true ~owner_migration_mode:G.Active
            ~cipher_mode:G.Active ~proof_mode:G.Active status
            (Octra_core.Pvac_migration_check.disabled ~chain_id)
        | "inspect" ->
          let wire = run (L.get_pvac_pubkey ledger address) |> Option.get in
          let registered = wire |> Bytes.of_string |> P.deserialize_pubkey in
          let amount = F.get_balance registered sk (cipher ()) |> Result.get_ok in
          `Assoc ["amount", `String (Z.to_string amount);
            "key_hash", `String Digestif.SHA256.(digest_string wire |> to_hex);
            "balance", `String (Z.to_string account.balance); "nonce", `Int account.nonce;
            "supply", `String (run (S.get_meta store "total_supply") |> Option.get)]
        | "octra_submit" ->
          let tx = match params with
            | `List [value] -> T.of_yojson value |> Result.get_ok
            | _ -> failwith "wallet transaction parameters invalid" in
          check "wallet address" (tx.from = address && tx.to_ = address);
          check "wallet signature" (T.verify tx public);
          check "wallet payload size" (Octra_core.Tx_payload.admit tx = Ok ());
          let backend = E.make_live_backend ~proof_mode:G.Active ~math:true
            ~emission_policy:Octra_core.Emission_policy.Guard
            ~emission_schedule:(Octra_core.Emission_schedule.of_env_exn (fun _ -> None))
            ~validator_policy:(Octra_core.Validator_policy.of_env_exn (fun _ -> None)) store ledger in
          let transition = A.create ~registered:false ~keyed ~math:true ~preverify:None ~ledger ~epoch_id:!epoch
            ~owner_migration_mode:G.Active ~cipher_mode:G.Active ~proof_mode:G.Active
            ~field_policy:PL.Unique_fields ~result_policy:Octra_core.Private_result_policy.Recoverable
            ~legacy_replay:(fun ~epoch:_ ~address:_ ~cipher:_ -> failwith "unexpected history read")
            ~limits:A.{max_fhe = 1; max_stealth = 1} in
          let env : E.env = {chain_id; epoch_id = !epoch; proposer_addr = proposer;
            validator_addrs = [proposer]; validator_pubkeys = [];
            prev_state_root = run (L.hash ledger); epoch_ts = tx.timestamp;
            ready_state_root_at = None; ready_max_lag = 0} in
          let result = run (E.run ~backend ~env ~txs:[tx]
            ~process_tx:(fun ~backend ~env tx -> A.process transition ~backend ~env tx))
            |> Result.get_ok in
          begin match result.artifacts.confirmed, result.artifacts.rejected with
          | [_], [] -> incr epoch; `Assoc ["tx_hash", `String (T.hash tx)]
          | [], [rejected] -> failwith rejected.E.reason
          | _ -> failwith "wallet transaction result differs"
          end
        | "staging_view" -> `Assoc ["transactions", `List []]
        | "node_status" -> `Assoc ["epoch", `Int !epoch; "chain_id", `String chain_id]
        | _ -> `Assoc [] in
      let rec loop () = match input_line stdin with
        | line ->
          let reply = try
            let request = Yojson.Safe.from_string line in
            let method_ = Yojson.Safe.Util.(member "method" request |> to_string) in
            let params = Yojson.Safe.Util.member "params" request in
            `Assoc ["result", response method_ params]
          with error -> `Assoc ["error", `String (Printexc.to_string error)] in
          output_string output (Yojson.Safe.to_string reply ^ "\n");
          flush output;
          loop ()
        | exception End_of_file -> () in
      Fun.protect ~finally:(fun () -> close_out_noerr output) loop))

let () =
  Mirage_crypto_rng_unix.use_default ();
  match Array.to_list Sys.argv with
  | [_; "--serve-wallet"; path] -> wallet_server ~keyed:false path
  | [_; "--serve-keyed-wallet"; path] -> wallet_server ~keyed:true path
  | [_; "--wallet"; path] ->
    let input = open_in_bin path in
    let text = Fun.protect ~finally:(fun () -> close_in_noerr input) (fun () ->
      let size = in_channel_length input in
      if size > 80 * 1024 * 1024 then failwith "wallet sample too large";
      really_input_string input size) in
    let json = Yojson.Safe.from_string text in
    let field name = Yojson.Safe.Util.(member name json |> to_string) in
    let old_key = Base64.decode_exn (field "source_key") in
    let new_key = Base64.decode_exn (field "new_pubkey") in
    let source = field "source_cipher" in
    check "wallet source hash"
      (field "source_cipher_hash" = Digestif.SHA256.(digest_string source |> to_hex));
    timed "wallet_state" (fun () -> state_check true old_key source new_key
      (field "new_cipher") (field "old_zero_proof") (field "new_range_proof")
      (field "amount_commitment"));
    Printf.printf "event = wallet_interop status = pass\n%!"
  | [_; "--wide"] -> test_pair true
  | [_; "--deep"] -> test_pair ~deep:true true
  | _ -> List.iter test_pair [false; true]