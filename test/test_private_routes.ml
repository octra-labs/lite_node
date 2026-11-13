(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_core
module N = Octra_node_runtime
module V = Octra_vm
module S = C.Store_irmin
module L = C.Ledger
module E = C.Epoch_exec
module T = C.Transaction
module P = Pvac_ffi
module F = C.Crypto.FheBalance
module G = C.Rule_graph
module Sender = N.Consensus_epoch_apply_sender

let run = Lwt_main.run
let check name value = if not value then failwith name
let get = function Ok value -> value | Error error -> failwith error
let seed n = Bytes.make 32 (Char.chr n)
let b64 bytes = Base64.encode_exn (Bytes.to_string bytes)
let chain = "octra-devnet-9871-cluster"
let epoch = Octra_consensus.C_quorum_policy.round_epoch ~chain_id:chain |> Option.get |> Int64.to_int
let signing = b64 (seed 91)
let public = Mirage_crypto_ec.Ed25519.priv_of_octets (Bytes.to_string (seed 91)) |> Result.get_ok
  |> Mirage_crypto_ec.Ed25519.pub_of_priv |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn
let owner = C.Crypto.Address.address_from_pubkey public
let env = E.{chain_id = chain; epoch_id = epoch; proposer_addr = owner;
  validator_addrs = []; validator_pubkeys = []; prev_state_root = String.make 64 'a';
  epoch_ts = 1.; ready_state_root_at = None; ready_max_lag = -1}
let fold height = E.prior_fold height |> Result.map (fun value ->
  {value with E.standard_mode = G.Active; proof_key = G.Active; math = true})
let transaction nonce amount op_type fields =
  T.sign_with_privkey T.{from = owner; to_ = owner; amount = Z.of_int amount;
    nonce; ou = Z.one; timestamp = 1.; signature = ""; public_key = Some public;
    message = None; op_type; encrypted_data = Some (Yojson.Safe.to_string (`Assoc fields))} signing

let execute ?(mode = G.Active) ~(effects : Sender.tx_sink_refs) ~notices single store ledger chaindata =
  let fold height = fold height |> Result.map (fun value -> {value with E.standard_mode = mode}) in
  if single then fun tx ->
    let sink = Sender.live_tx_sink Sender.{chaindata; current_epoch = (fun () -> epoch);
      pending_tx_saves = effects.pending_tx_saves; total_tx_count = effects.total_tx_count;
      confirmed_fees = effects.confirmed_fees; processed_hashes = effects.processed_hashes} in
    let module Sender = N.Consensus_epoch_apply_sender_live in
    let accepted = ref false and refused = ref None in
    run (Sender.run Sender.{ledger; store; chaindata; program_trust = V.Program_trust.empty;
      object_cost = false; cipher_mode = G.Active; wallet_addr = owner;
      pre_state_hash = env.prev_state_root; fold; standard_env = (fun () -> env);
      current_epoch = (fun () -> epoch); max_fhe_per_epoch = 4; max_stealth_per_epoch = 4;
      max_stealth_defer = 1; stealth_inline_verify_allowed = true;
      fhe_in_epoch_counter = ref 0; stealth_in_epoch_counter = ref 0;
      stealth_defer_count = Hashtbl.create 1; deferred_stealth_txs = ref [];
      confirmed_fees = ref Z.zero; short_addr = Fun.id;
      log_rejected = (fun tx tag reason -> refused := Some (tag ^ ": " ^ reason); sink.reject tx tag reason);
      confirm_tx = (fun tx -> accepted := true; sink.confirm tx); notify_new_account = ignore;
      notify_confirmed = (fun tx _ -> notices := (T.hash tx, "confirmed") :: !notices);
      notify_rejected = (fun tx reason -> notices := (T.hash tx, reason) :: !notices);
      legacy_replay = (fun ~epoch:_ ~address:_ ~cipher:_ -> failwith "unexpected migration");
      private_field_policy = C.Private_ledger.Unique_fields;
      private_result_policy = (fun _ -> C.Private_result_policy.Recoverable)} [tx]);
    if !accepted then Ok () else Error (Option.value !refused ~default:"missing result")
  else
    let module Shared = N.Consensus_epoch_apply_shared in
    let shared = Shared.runtime_deps Shared.{
      consensus_mode = true; current_epoch = (fun () -> epoch); log_shared = (fun ~tx_count:_ -> ());
      short = Fun.id; fatal = failwith; exit = (fun () -> failwith "unexpected exit");
      backend = (fun () -> E.make_live_backend ~fold store ledger); env = (fun () -> env);
      max_fhe_per_epoch = 4; max_stealth_per_epoch = 4;
      process_sender = (fun _ -> failwith "unexpected sender"); confirm_tx = ignore;
      reject_tx = (fun _ _ _ -> ()); notify_confirmed = (fun _ _ -> ());
      notify_rejected = (fun _ _ -> ()); program_trust = V.Program_trust.empty;
      circle_mode = G.Prior; wasm_compute_mode = G.Prior; object_cost = false;
      owner_migration_mode = G.Active; cipher_mode = G.Active; proof_mode = mode; math = true;
      private_field_policy = C.Private_ledger.Unique_fields;
      private_result_policy = (fun _ -> C.Private_result_policy.Recoverable);
      legacy_replay = (fun ~epoch:_ ~address:_ ~cipher:_ -> failwith "unexpected migration");
      add_rejected_fee = ignore; advance_validator_set = (fun () -> Lwt.return_unit)} in
    fun tx -> match run (shared.shared.process tx) with
      | Ok (E.Confirmed _) -> Ok ()
      | Ok (E.Rejected_after_fee error) -> Error error.reason
      | Error (tag, reason) -> Error (tag ^ ": " ^ reason)

let () =
  Unix.putenv "OCTRA_BFT_CRYPTO_PROFILE" "private_v1";
  let pk, sk = P.keygen_from_seed (P.default_params ()) (seed 11) in
  let blob = P.serialize_pubkey pk |> Bytes.to_string in
  let owner_cipher = P.enc_value_seeded pk sk 0L (seed 12) in
  let owner_proof = P.make_zero_proof_bound ~keyed:true ~math:true pk sk owner_cipher 0L
    (C.Pvac_registry.owner_blinding owner) |> F.encode_zero_proof in
  let register = transaction 1 0 T.KeySwitch ["new_pubkey", `String (Base64.encode_exn blob);
    "aes_kat", `String (C.Pvac_registry.expected_kat ());
    "owner_cipher", `String (F.encode_cipher owner_cipher); "owner_proof", `String owner_proof] in
  let cipher = P.enc_value_seeded pk sk 10L (seed 13) in
  let blind = seed 14 in
  let fields cipher = ["cipher", `String (F.encode_cipher cipher);
    "zero_proof", `String (P.make_zero_proof_bound ~keyed:true ~math:true pk sk cipher 10L blind |> F.encode_zero_proof);
    "amount_commitment", `String (P.pedersen_commit_amount 10L blind |> b64);
    "blinding", `String (b64 blind)] in
  let encrypt = transaction 2 10 T.EncryptOp (fields cipher) in
  let delta = P.enc_value_seeded pk sk 10L (seed 16) in
  let zero = P.ct_sub pk cipher delta in
  let decrypt = transaction 3 10 T.DecryptOp
    (("range_proof_balance", `String (P.make_zero_proof_bound_range ~keyed:true ~math:true
      pk sk zero 0L (seed 15) |> F.encode_bound_range_proof)) :: fields delta) in
  let results = List.map (fun single -> Test_workspace.with_dir "private-routes" (fun dir ->
    let scoped tx = T.sign_with_privkey
      {tx with T.timestamp = (if single then 2. else 1.); signature = ""} signing in
    let register = scoped register and encrypt = scoped encrypt and decrypt = scoped decrypt in
    let store = run (S.open_store (Filename.concat dir "state")) in
    let chaindata = C.Store_chaindata.open_chaindata (Filename.concat dir "chain") in
    Fun.protect ~finally:(fun () -> S.abort_epoch_batch store; run (S.close store);
      C.Store_chaindata.abort_batch chaindata; C.Store_chaindata.close chaindata) (fun () ->
      let ledger = L.create store in
      get (L.add_account_with_pubkey ledger owner (Z.of_int 1000) public);
      run (S.begin_epoch_batch ~mode:G.Active store);
      C.Store_chaindata.begin_batch chaindata;
      let effects : Sender.tx_sink_refs = {pending_tx_saves = ref []; total_tx_count = ref 0;
        confirmed_fees = ref Z.zero; processed_hashes = ref []} in
      let notices = ref [] in
      let prior_execute = execute ~mode:G.Prior ~effects ~notices single store ledger chaindata in
      let execute = execute ~effects ~notices single store ledger chaindata in
      let apply tx =
        check "private transaction signature" (T.verify tx public);
        let state () =
          let nonces = Hashtbl.fold (fun nonce () entries -> nonce :: entries)
            ledger.L.spent_nonces [] |> List.sort compare in
          let history = Option.map (fun (batch : C.Chaindata_index.batch) ->
            batch.txs, batch.epochs, batch.rejected, batch.receipts, batch.meta_writes,
            batch.clear_rebuild_tables) chaindata.C.Store_chaindata.index.batch in
          L.find ledger owner, run (S.get_batch_tree_hash store),
          run (S.get_pvac_pubkey ~registered:true store owner), L.get_pvac_kat ledger owner,
          nonces, history, chaindata.next_txid, chaindata.tx_writes,
          !(effects.pending_tx_saves), !(effects.total_tx_count), !(effects.confirmed_fees),
          !(effects.processed_hashes), !notices in
        let before = state () in
        let module Memory = C.Compute_memory in
        let usage = Memory.stats Memory.shared in
        let held = Memory.reserve Memory.shared ~speculative:false
          (Memory.limit_of_getenv Sys.getenv_opt) |> Option.get in
        let released = Lwt_unix.sleep 15. in
        Lwt.on_success released (fun () -> Memory.release held);
        let refused = Fun.protect ~finally:(fun () ->
          Lwt.cancel released; Memory.release held) (fun () ->
          try ignore (execute tx); false with
          | C.Exec_resource.Unavailable Host -> single
          | C.Private_ledger.Worker_stopped _ -> true
          | _ -> false) in
        check "memory refusal became transaction outcome" refused;
        check "memory refusal changed private state" (state () = before);
        check "private route retained memory" (Memory.stats Memory.shared = usage);
        execute tx |> get in
      apply register;
      check "registration not in state" (run (S.get_pvac_pubkey ~registered:true store owner) = Some blob);
      let prior_hash = if not single then None else begin
        let tx = T.sign_with_privkey {encrypt with T.timestamp = 3.; signature = ""} signing in
        let worker = Sys.getenv "OCTRA_PVAC_VERIFY_WORKER" in
        let before = L.find ledger owner, run (S.get_batch_tree_hash store) in
        let result = Fun.protect ~finally:(fun () -> Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" worker)
          (fun () -> Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" "/no-worker";
            try Some (prior_execute tx) with _ -> None) in
        check "prior private refusal changed"
          (result = Some (Error "private_worker_retry: encrypt: proof worker unavailable: worker_missing"));
        check "prior refusal changed balance" ((L.find ledger owner, run (S.get_batch_tree_hash store)) = before);
        Some (T.hash tx)
      end in
      L.set_pvac_kat ledger owner "different local file";
      let before = L.find ledger owner in
      let path = S.pvac_blob_path store (S.pvac_hash blob) in
      Unix.unlink path;
      check "missing blob became a private outcome"
        (try ignore (execute encrypt); false
         with
         | C.Exec_resource.Unavailable Host -> single
         | C.Private_ledger.Worker_stopped "host unavailable" -> not single);
      check "host failure changed account" (L.find ledger owner = before);
      S.write_blob store (S.pvac_hash blob) blob;
      apply encrypt;
      let account = L.find ledger owner in
      check "encrypted value differs" (F.get_balance pk sk (Option.get account.encrypted_balance) = Ok (Z.of_int 10));
      apply decrypt;
      let account = L.find ledger owner in
      check "private accounting differs" (account.balance = Z.of_int 997 && account.nonce = 3);
      check "decrypted value differs" (F.get_balance pk sk (Option.get account.encrypted_balance) = Ok Z.zero);
      C.Store_chaindata.commit_batch chaindata;
      List.iter (fun tx -> check "memory refusal persisted in history"
        (C.Store_chaindata.get_rejected_tx chaindata (T.hash tx) = None)) [register; encrypt; decrypt];
      Option.iter (fun hash -> check "prior refusal absent from history"
        (Option.is_some (C.Store_chaindata.get_rejected_tx chaindata hash))) prior_hash;
      run (L.flush_dirty_lwt ledger);
      account, run (S.get_batch_tree_hash store)))) [false; true] in
  check "Single and BFT private state differs" (List.hd results = List.nth results 1);
  print_endline "status = pass test = private_routes"