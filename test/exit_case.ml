(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_consensus.C_types
module H = Octra_consensus.C_hash
module L = Octra_core.Ledger
module P = Octra_core.Validator_policy
module R = Octra_core.Validator_registry
module S = Octra_core.Store_irmin
module T = Octra_core.Transaction
module X = Octra_core.Epoch_exec
module G = Octra_core.Rule_graph

let expect label condition =
  if not condition then failwith label

let get label = function
  | Ok value -> value
  | Error error -> failwith (label ^ ": " ^ error)

type key = {
  address : string;
  public : string;
  secret : string;
}

let key index =
  let secret = String.init 32 (fun byte -> Char.chr (index + byte)) in
  let private_key =
    match Mirage_crypto_ec.Ed25519.priv_of_octets secret with
    | Ok value -> value
    | Error _ -> failwith "test key rejected"
  in
  let public =
    Mirage_crypto_ec.Ed25519.pub_of_priv private_key
    |> Mirage_crypto_ec.Ed25519.pub_to_octets
  in
  let address =
    Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn public)
  in
  { address; public; secret }

let owner = key 1
let peers = List.init 4 (fun index -> key (index + 40))
let chain_id = "octra-devnet-9871-cluster"
let first_epoch = 1_600_001
let emission_epoch = 1_500_000
let exit_epoch = first_epoch + 1
let mature = exit_epoch + Int64.to_int P.exit_wait
let fee = Z.of_int 1_000
let balance = Z.of_int 2_000_000

let policy =
  P.of_env_exn (fun name ->
    if name = P.env_name then Some "1266000" else None)

let rules =
  let initial = G.create ~chain_id ~root_at:(fun _ -> G.Missing) in
  let anchors =
    [G.circle_activation; G.wasm_compute_activation;
     G.validator_quorum_activation; G.epoch_time_activation;
     G.owner_migration_activation; G.private_payload_activation;
     G.set_fold_activation; G.validator_ready_activation;
     G.ready_ref_activation; G.set_live_activation; G.set_fold_cap_activation;
     G.set_open_activation; G.object_cost_activation; G.account_pack_activation;
     G.standard_activation; G.set_plan_activation; G.math_activation; G.exit_activation]
    |> List.filter_map (fun activation -> activation initial)
  in
  G.create ~chain_id ~root_at:(fun epoch ->
    match List.find_opt (fun item -> item.G.anchor_epoch = epoch) anchors with
    | None -> G.Missing
    | Some item -> G.Root item.anchor_state_root)

let root_bytes root =
  expect ("state root length = " ^ string_of_int (String.length root))
    (String.length root >= 64);
  Octra_node_runtime.Consensus_epoch_apply_guard.raw32_of_pre_root root

let parent ~epoch ~root ~active =
  let keys = if active then owner :: peers else peers in
  let validator_set =
    List.map
      (fun (key : key) -> C.{ address = key.address; pubkey = key.public }, Z.one)
      keys
    |> C.make_weighted_validator_set
    |> get "parent set"
  in
  let header = C.{
    proto_version = proto_version_current;
    chain_id;
    epoch_id = Int64.of_int (epoch - 1);
    prev_state_root = root_bytes root;
    tx_list_hash = H.receipt_root [];
    receipt_root = H.receipt_root [];
    proposed_state_root = root_bytes root;
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = (List.hd peers).address;
    txid_hi = 0L;
    ts = float_of_int (epoch - 1) *. 10.;
  } in
  let proposal_id = H.proposal_id header in
  let precommits =
    List.map
      (fun (key : key) ->
        let vote = C.{
          chain_id;
          epoch_id = header.epoch_id;
          round = 0;
          vote_type = Precommit;
          proposal_id;
          validator = key.address;
          signature = String.make 64 '\000';
        } in
        { vote with signature =
          H.sign_ed25519 ~priv_raw:key.secret ~msg:(H.vote_sign_bytes vote) })
      keys
  in
  C.{ validator_set; certificate = {
    chain_id; epoch_id = header.epoch_id; commit_round = 0;
    header; proposal_id; precommits;
  } }

let transaction ?(cost = fee) ?(amount = Z.zero) ?(message = "{}")
    ?(target = owner.address) ~epoch ~nonce op_type =
  let unsigned = T.{
    from = owner.address;
    to_ = target;
    amount;
    nonce;
    ou = cost;
    timestamp = float_of_int epoch *. 10.;
    signature = "";
    public_key = Some (Base64.encode_exn owner.public);
    message = Some message;
    op_type;
    encrypted_data = None;
  } in
  let signed = T.sign_with_privkey unsigned (Base64.encode_exn owner.secret) in
  expect "transaction signature" (T.verify signed (Base64.encode_exn owner.public));
  let decoded = T.of_yojson (T.to_yojson signed) |> get "transaction codec" in
  expect "signed transaction round trip" (decoded = signed);
  signed

let bond ?(epoch = first_epoch) ?(nonce = 1) () =
  let payload =
    R.bond_message ~chain_id ~address:owner.address ~amount:P.min_bond
      ~nonce ~pubkey:owner.public
  in
  let proof = H.sign_ed25519 ~priv_raw:owner.secret ~msg:payload in
  let message = Yojson.Safe.to_string (`Assoc [
    "consensus_pubkey", `String (Base64.encode_exn owner.public);
    "proof", `String (Base64.encode_exn proof);
  ]) in
  transaction ~epoch ~nonce ~amount:P.min_bond
    ~target:R.escrow_address ~message T.ValidatorBond

type step = {
  epoch : int;
  active : bool;
  txs : T.t list;
}

let step ?(active = false) epoch txs = { epoch; active; txs }

let with_store path action =
  let store = Lwt_main.run (S.open_store path) in
  Fun.protect
    ~finally:(fun () -> Lwt_main.run (S.close store))
    (fun () -> action store (L.create store))

let seed path =
  with_store path (fun store ledger ->
    List.iter
      (fun key ->
        let amount = if key = owner then balance else Z.zero in
        L.add_account_with_pubkey ledger key.address amount
          (Base64.encode_exn key.public)
        |> get "seed account")
      (owner :: peers);
    Lwt_main.run (L.flush_dirty_lwt ledger);
    let module E = Octra_core.Emission_schedule in
    let retired = Z.sub Octra_core.Denomination.max_supply balance in
    ["emission_remaining", "0";
     "total_supply", Z.to_string balance;
     E.standard_key, E.standard_name;
     E.activation_key, string_of_int emission_epoch;
     E.initial_key, "0";
     E.retired_key, Z.to_string retired]
    |> List.iter (fun (name, value) ->
      Lwt_main.run (S.set_meta store name value)))

let backend_at root store ledger step =
  let parent = parent ~epoch:step.epoch ~root ~active:step.active in
  let fold =
    Octra_node_runtime.Set_rule.bind rules ~chain_id ~parent:(Some parent)
      ~epoch:step.epoch
    |> get "current rules"
  in
  let ctx = fold step.epoch |> get "current context" in
  expect "active standard rules" (ctx.X.standard_mode = G.Active);
  expect "active account rules" (ctx.X.account_mode = G.Active);
  let backend =
    X.make_live_backend ~emission_policy:Octra_core.Emission_policy.Guard
      ~emission_schedule:(Octra_core.Emission_schedule.Security_curve {
        activation_epoch = emission_epoch;
      })
      ~legacy_total_supply:(Z.to_string balance)
      ~sender_key_activation_epoch:0 ~validator_policy:policy ~fold store ledger
  in
  let env = X.{
    chain_id; epoch_id = step.epoch; proposer_addr = (List.hd peers).address;
    validator_addrs =
      List.map (fun (key : key) -> key.address)
        (if step.active then peers else owner :: peers);
    validator_pubkeys =
      List.map
        (fun (key : key) -> key.address, Base64.encode_exn key.public)
        (owner :: peers);
    prev_state_root = root; epoch_ts = float_of_int step.epoch *. 10.;
    ready_state_root_at = None; ready_max_lag = 0;
  } in
  let reward =
    Octra_node_runtime.Consensus_reward_attribution.of_parent_commit parent
    |> get "parent rewards"
  in
  backend, env, reward

let backend store ledger step =
  backend_at (Lwt_main.run (S.state_hash store)) store ledger step

let execute_lwt backend env reward txs =
  X.run_core ~reward:(Some reward) ~preverify:None ~backend ~env ~txs
    ~process_tx:(X.confirmed_process X.process_standard_tx)

let execute backend env reward txs =
  Lwt_main.run (execute_lwt backend env reward txs)

type view = {
  root : string;
  account : Octra_core.Ledger_types.account;
  escrow : Z.t;
  registry : string option;
  total : Z.t;
  retired : Z.t;
}

let view_lwt store ledger =
  let open Lwt.Syntax in
  let* root = S.state_hash store in
  let* registry = S.get_meta store R.meta_key in
  let* retired = S.get_meta store Octra_core.Emission_schedule.retired_key in
  Lwt.return {
    root;
    account = L.find ledger owner.address;
    escrow =
      Option.fold ~none:Z.zero ~some:(fun account -> account.L.balance)
        (L.find_opt ledger R.escrow_address);
    registry;
    total = L.get_total_supply ledger;
    retired = retired |> Option.get |> Z.of_string;
  }

let view store ledger = Lwt_main.run (view_lwt store ledger)

let apply_lwt store ledger step =
  let open Lwt.Syntax in
  let* root = S.state_hash store in
  let backend, env, reward = backend_at root store ledger step in
  let* result = execute_lwt backend env reward step.txs in
  let result = get "epoch execution" result in
  let* state = view_lwt store ledger in
  expect "committed root" (result.X.post_state_root = state.root);
  expect "value conserved"
    Z.(equal (add state.total state.retired) Octra_core.Denomination.max_supply);
  Lwt.return (result.artifacts, state)

let apply store ledger step = Lwt_main.run (apply_lwt store ledger step)

let run path steps =
  with_store path (fun store ledger -> List.map (apply store ledger) steps)

let path name =
  Filename.concat (Test_workspace.unique_dir name) "irmin"