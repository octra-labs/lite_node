(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_core
module V = Octra_vm
module R = C.Rule_graph
module E = C.Epoch_exec
module T = C.Transaction
module S = C.Store_irmin

let run = Lwt_main.run
let need reason value = if not value then failwith reason
let unwrap = function Ok value -> value | Error _ -> failwith "expected accepted value"
let sha value = Digestif.SHA256.(digest_string value |> to_hex)
let chain = "octra-devnet-9871-cluster"
let epoch = 2_000_000
let initial = Z.of_int 1_000_000_000
let save path value = Out_channel.with_open_gen [Open_creat; Open_excl; Open_wronly; Open_binary] 0o600 path
  (fun channel -> output_string channel (Yojson.Safe.pretty_to_string value ^ "\n"))
let compile body = V.Program_package.compile_sources_at ~metered:true ~loops:true
  ~compiler:V.Program_package.Preview ~point_ops:true ~main:"main.aml"
  ~sources:[V.Program_package.{path = "main.aml"; body}] |> unwrap

let factory_source =
  let names = List.init 18 (fun index -> "p" ^ string_of_int index) in
  let declarations = List.mapi (fun index name -> name ^ ": " ^
    (if index >= 10 then "bytes32" else "string")) names in
  "program PoolFactory { state { operator: address created: bool } " ^
  "constructor(operator: address) { require(caller == operator, \"operator required\") " ^
  "self.operator = operator self.created = false } public nonreentrant fn create(code: string, " ^
  String.concat ", " declarations ^ "): address { require(caller == self.operator, \"operator required\") " ^
  "require(!self.created, \"already created\") self.created = true return deploy(code, " ^
  String.concat ", " names ^ ") } }"

let identity index =
  let raw = String.make 31 '\009' ^ String.make 1 (Char.chr index) in
  let key = Mirage_crypto_ec.Ed25519.priv_of_octets raw |> unwrap in
  let public = Mirage_crypto_ec.Ed25519.pub_of_priv key |> Mirage_crypto_ec.Ed25519.pub_to_octets
    |> Base64.encode_exn in
  raw |> Base64.encode_exn, public, C.Crypto.Address.address_from_pubkey public

let identity_for envelope =
  let rec select index =
    need "synthetic key search exhausted" (index < 128);
    let _, _, sender = identity index in
    if Program_spawn.non_key (V.Contract.addr_from_code envelope sender 1)
    then identity index else select (index + 1) in
  select 1

let roots at = match R.root_after_floor ~chain_id:chain ~floor_epoch:epoch ~epoch:at with
  | Some value -> R.Root value
  | None -> R.Missing
let rules () = R.create ~chain_id:chain ~root_at:roots

let fold at =
  let ( let* ) = Result.bind in
  let* policy = Set_rule.policy (rules ()) at in
  let* prior = E.prior_fold at in
  Ok {prior with E.standard_mode = policy.standard_mode; program_mode = policy.program_mode;
    program_overlap = policy.program_overlap; fhe_work = policy.fhe_work; proof_exec = policy.proof_exec;
    proof_key = policy.proof_key; vm_bytes = policy.vm_bytes; program_records = policy.program_records;
    wasm_float = policy.wasm_float; math = policy.math}

let environment timestamp sender : E.env = {
  chain_id = chain; epoch_id = epoch; proposer_addr = sender; validator_addrs = [];
  validator_pubkeys = []; prev_state_root = String.make 64 'a'; epoch_ts = timestamp;
  ready_state_root_at = None; ready_max_lag = -1;
}

let sign ?(at = epoch) key tx = C.Tx_signature.sign (C.Tx_signature.at ~chain_id:chain ~epoch:(Int64.of_int at)) tx key

let transaction ~timestamp ~public ~sender ~target ~nonce ~fee ~operation ~payload ~params : T.t = {
  from = sender; to_ = target; amount = Z.zero; nonce; ou = Z.of_int fee; timestamp;
  signature = ""; public_key = Some public; op_type = operation;
  encrypted_data = Some payload; message = Some (C.Json_tree.write params);
}

let ingress ?(at = epoch) ?preverify_admit ledger tx =
  let runtime : Node_rest_facade.runtime = {
    signing = (fun () -> C.Tx_signature.at ~chain_id:chain ~epoch:(Int64.of_int at));
    swarm_ref = ref None; duty_head = (fun () -> None); proof_mode = (fun () -> R.Active);
    compute_mode = (fun () -> R.Active); settle_mode = (fun () -> R.Prior);
    marks_mode = (fun () -> R.Active); queue_head = (fun () -> Some (Int64.of_int at));
    preverify_admit = Option.value preverify_admit ~default:(fun value ->
      need "unexpected proof operation" (not (C.Preverify_queue.needs_preverify value)); Ok ());
    save_drops = (fun drops -> need "unexpected dropped transactions" (drops = []));
    find_drop = (fun _ -> None); drops_by_addr = (fun _ ~limit:_ ~offset:_ -> []);
  } in
  run (Submit_rpc.submit ~validate:(Node_rest_facade.validate_and_submit_tx runtime ledger)
    (`List [T.to_yojson tx]))

let limits = Consensus_proposal.limits ~max_txs:128 ~max_bytes:4_194_304 ~max_ou:(Z.of_int 10_000_000)

let bundle ledger env txs =
  let header : Octra_consensus.C_types.epoch_header = {
    proto_version = Octra_consensus.C_types.proto_version_current;
    chain_id = chain; epoch_id = Int64.of_int epoch; prev_state_root = env.E.prev_state_root;
    tx_list_hash = ""; receipt_root = Octra_consensus.C_hash.receipt_root [];
    proposed_state_root = ""; parent_commit_hash = ""; creator_addr = env.proposer_addr;
    txid_hi = 0L; ts = env.epoch_ts;
  } in
  Consensus_proposal.verify_bundle {
    public_key_for_tx = Tx_sender_key.resolve ~find_account:(C.Ledger.find_opt ledger);
    verify_address_pubkey = (fun ~addr ~pubkey -> C.Crypto.Address.verify_address_pubkey addr pubkey);
    verify_tx_signature = (fun ~signing tx ~pubkey -> C.Tx_signature.verify signing tx pubkey);
  } ~limits ~header ~expected_tx_count:(List.length txs) txs []

let select ledger tx =
  (match ingress ledger tx with
  | Ok _ -> ()
  | Error error -> failwith ("signed RPC rejected: " ^ error.C.Rpc.message));
  let queued = C.Tx_staging.ready_epoch_txs ~capacity:C.Tx_staging.max_ou_per_epoch
    ~accept:(fun _ -> true) ~confirmed_nonce:(fun address ->
      Option.map (fun (account : C.Ledger_types.account) -> account.nonce) (C.Ledger.find_opt ledger address)) in
  need "staging selection differs" (List.map T.hash queued = [T.hash tx]);
  let picked = Consensus_proposal.cap ~limits queued in
  need "proposal resource selection differs" (List.map T.hash picked.txs = [T.hash tx]);
  need "queue resource policy refused" (C.Resource_lanes.queue_check ~mode:R.Active tx = Ok ());
  picked.txs

let fields store address = run (S.load_contract_storage store address) |> S.sorted_storage_pairs

let sender path backend env tx =
  let chaindata = C.Store_chaindata.open_chaindata (Filename.concat path "receipts") in
  Fun.protect ~finally:(fun () -> C.Store_chaindata.close chaindata) (fun () ->
    let outcome = ref None in
    C.Store_chaindata.begin_batch chaindata;
    let deps : Consensus_epoch_apply_sender_live.deps = {
      ledger = backend.E.ledger; store = backend.store; chaindata; program_trust = V.Program_trust.empty;
      object_cost = false; cipher_mode = R.Active; wallet_addr = env.E.proposer_addr;
      pre_state_hash = env.prev_state_root; fold; standard_env = (fun () -> env);
      current_epoch = (fun () -> epoch); max_fhe_per_epoch = 8; max_stealth_per_epoch = 8;
      max_stealth_defer = 0; stealth_inline_verify_allowed = false; fhe_in_epoch_counter = ref 0;
      stealth_in_epoch_counter = ref 0; stealth_defer_count = Hashtbl.create 1;
      deferred_stealth_txs = ref []; confirmed_fees = ref Z.zero; short_addr = Fun.id;
      log_rejected = (fun _ _ reason -> outcome := Some (false, reason));
      confirm_tx = (fun _ -> outcome := Some (true, "confirmed"));
      notify_new_account = ignore; notify_confirmed = (fun _ _ -> ()); notify_rejected = (fun _ _ -> ());
      legacy_replay = (fun ~epoch:_ ~address:_ ~cipher:_ -> failwith "unexpected private replay");
      private_field_policy = C.Private_ledger.Unique_fields;
      private_result_policy = (fun _ -> C.Private_result_policy.Recoverable);
    } in
    run (Consensus_epoch_apply_sender_live.run deps [tx]);
    C.Store_chaindata.commit_batch chaindata;
    Option.get !outcome, C.Store_chaindata.get_contract_receipt chaindata ~tx_hash:(T.hash tx))

let validator backend env tx =
  let receipt = ref None in
  let result = run (C.Tx_savepoint.run ~ledger:backend.E.ledger ~store:backend.store (fun () ->
    Consensus_vm_transition.process_tx ~save_receipt_raw:(fun ~tx_hash ~json ->
      need "receipt transaction differs" (tx_hash = T.hash tx); receipt := Some (Yojson.Safe.from_string json))
      ~backend ~env ~circle_mode:R.Prior ~wasm_compute_mode:R.Active
      ~program_trust:V.Program_trust.empty ~object_cost:false tx)) in
  let outcome = match result with
    | Ok (E.Confirmed _) -> true, "confirmed"
    | Ok (E.Rejected_after_fee value) -> false, value.reason
    | Error (_, reason) -> false, reason in
  outcome, !receipt