(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_core
module E = C.Epoch_exec
module G = C.Rule_graph
module L = C.Ledger
module S = C.Store_irmin
module T = C.Transaction
module A = Octra_node_runtime.Consensus_epoch_apply_shared

let run = Lwt_main.run
let check reason value = if not value then failwith reason
let b64 = Base64.encode_exn
let chain_id = "octra-devnet-9871-cluster"
let plan = Option.get (G.proof_key_activation_for_chain chain_id)
let signing = String.make 32 '\091' |> b64
let public_key =
  Mirage_crypto_ec.Ed25519.priv_of_octets (Base64.decode_exn signing)
  |> Result.get_ok |> Mirage_crypto_ec.Ed25519.pub_of_priv
  |> Mirage_crypto_ec.Ed25519.pub_to_octets |> b64
let address = C.Crypto.Address.address_from_pubkey public_key

let read path =
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in input) (fun () ->
    let values = Array.init 14 (fun _ -> really_input_string input (input_binary_int input)) in
    check "extra key bytes" (pos_in input = in_channel_length input);
    values)

let transaction blob owner =
  let fields = ["new_pubkey", `String (b64 blob);
    "aes_kat", `String (C.Pvac_registry.expected_kat ())] @ owner in
  T.sign_with_privkey T.{from = address; to_ = address; amount = Z.zero;
    nonce = 1; ou = Z.one; timestamp = 1.; signature = "";
    public_key = Some public_key; message = None; op_type = KeySwitch;
    encrypted_data = Some (Yojson.Safe.to_string (`Assoc fields))} signing

let apply ?(reason = "pvac public key powers are not cyclic")
    epoch blob owner accepted =
  Test_workspace.with_dir "key_epoch" (fun path ->
    let store = run (S.open_store (Filename.concat path "store")) in
    Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
      let ledger = L.create store in
      L.add_account_with_pubkey ledger address (Z.of_int 1000) public_key
      |> Result.get_ok;
      run (L.flush_dirty_lwt ledger);
      let graph = G.create ~chain_id ~root_at:(fun height ->
        check "key anchor differs" (height = plan.anchor_epoch);
        G.Root plan.anchor_state_root) in
      let fold epoch = match G.proof_key graph ~epoch with
        | Error error -> Error (G.fault_message error)
        | Ok proof_key -> E.prior_fold epoch |> Result.map (fun value ->
          {value with E.proof_key}) in
      let backend = E.make_live_backend ~fold ~proof_mode:G.Active ~math:true store ledger in
      run (backend.begin_batch G.Prior);
      let env = E.{chain_id; epoch_id = epoch; proposer_addr = address;
        validator_addrs = []; validator_pubkeys = []; epoch_ts = 1.;
        prev_state_root = String.make 64 'a'; ready_state_root_at = None;
        ready_max_lag = -1} in
      let unexpected () = failwith "unexpected key epoch callback" in
      let runtime = A.{consensus_mode = true; current_epoch = (fun () -> epoch);
        log_shared = (fun ~tx_count:_ -> unexpected ()); short = Fun.id;
        fatal = (fun _ -> unexpected ()); exit = unexpected;
        backend = (fun () -> backend); env = (fun () -> env);
        max_fhe_per_epoch = 1; max_stealth_per_epoch = 1;
        process_sender = (fun _ -> unexpected ());
        confirm_tx = (fun _ -> unexpected ());
        reject_tx = (fun _ _ _ -> unexpected ());
        notify_confirmed = (fun _ _ -> unexpected ());
        notify_rejected = (fun _ _ -> unexpected ());
        program_trust = Octra_vm.Program_trust.empty;
        circle_mode = G.Prior; wasm_compute_mode = G.Prior; object_cost = false;
        owner_migration_mode = G.Prior; cipher_mode = G.Active;
        proof_mode = G.Active; math = true;
        private_field_policy = C.Private_ledger.Unique_fields;
        legacy_replay = (fun ~epoch:_ ~address:_ ~cipher:_ -> unexpected ());
        private_result_policy = (fun _ -> C.Private_result_policy.Recoverable);
        add_rejected_fee = (fun _ -> unexpected ());
        advance_validator_set = (fun () -> unexpected ())} in
      let before = run (L.hash ledger), L.find ledger address,
        run (L.get_pvac_pubkey ledger address), L.get_pvac_kat ledger address in
      let operation = transaction blob owner in
      check "key epoch signature" (T.verify operation public_key);
      let result = run ((A.runtime_deps runtime).shared.process operation) in
      let account = L.find ledger address in
      if accepted then begin
        check "key epoch registration refused" (result = Ok (E.Confirmed Z.one));
        check "key epoch debit differs" (account.balance = Z.of_int 999 && account.nonce = 1);
        check "key epoch stored wrong key"
          (run (L.get_pvac_pubkey ledger address) =
            Some (C.Pvac_registry.key_blob ~keyed:false blob |> Result.get_ok))
      end else begin
        check "invalid key epoch accepted"
          (result = Error ("key_switch_rejected", reason));
        check "key epoch refusal changed state"
          (before = (run (L.hash ledger), account,
            run (L.get_pvac_pubkey ledger address), L.get_pvac_kat ledger address))
      end))

let () =
  check "sample path missing" (Array.length Sys.argv = 2);
  let previous = Sys.getenv_opt "OCTRA_BFT_CRYPTO_PROFILE" in
  Unix.putenv "OCTRA_BFT_CRYPTO_PROFILE" "private_v1";
  Fun.protect ~finally:(fun () ->
    Unix.putenv "OCTRA_BFT_CRYPTO_PROFILE" (Option.value ~default:"" previous)) (fun () ->
      let samples = read Sys.argv.(1) in
      let owner index secret =
        let pk = Pvac_ffi.deserialize_pubkey (Bytes.of_string samples.(index)) in
        let sk = Pvac_ffi.deserialize_seckey (Bytes.of_string samples.(secret)) in
        let blind = C.Pvac_registry.owner_blinding address in
        let cipher = Pvac_ffi.enc_value_seeded pk sk 0L (Bytes.make 32 '\109') in
        let proof = Pvac_ffi.make_zero_proof_bound ~keyed:true ~math:true
          pk sk cipher 0L blind in
        ["owner_cipher", `String (C.Crypto.FheBalance.encode_cipher cipher);
         "owner_proof", `String (C.Crypto.FheBalance.encode_zero_proof proof)] in
      let owners = [0, owner 0 1; 2, owner 2 3; 6, owner 6 3] in
      List.iter (fun epoch ->
        List.iter (fun index ->
          apply epoch samples.(index) (List.assoc index owners) true) [0; 2; 6];
        List.iter (fun index ->
          apply epoch samples.(index) [] (epoch < plan.activation_epoch))
          [4; 5; 7; 8; 9; 10; 11; 12; 13])
        [plan.activation_epoch - 1; plan.activation_epoch; plan.activation_epoch + 1];
      apply (plan.activation_epoch - 1) samples.(0) [] true;
      List.iter (fun name ->
        let malformed = [name, `Int 1] in
        let repeated = [name, `String "a"; name, `String "b"] in
        List.iter (fun fields ->
          apply (plan.activation_epoch - 1) samples.(0) fields true)
          [malformed; repeated];
        List.iter (fun epoch ->
          apply ~reason:(name ^ " must be a string") epoch samples.(0) malformed false;
          apply ~reason:("duplicate " ^ name) epoch samples.(0) repeated false)
          [plan.activation_epoch; plan.activation_epoch + 1])
        ["owner_cipher"; "owner_proof"];
      apply ~reason:"key ownership proof is required"
        plan.activation_epoch samples.(0) [] false;
      apply ~reason:"key ownership proof is required"
        (plan.activation_epoch + 1) samples.(0) [] false);
  Printf.printf "status = pass test = key_epoch\n%!"