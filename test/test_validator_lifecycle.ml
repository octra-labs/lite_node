(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module E = Octra_core.Emission_policy
module X = Octra_core.Epoch_exec
module L = Octra_core.Ledger
module Policy = Octra_core.Validator_policy
module Registry = Octra_core.Validator_registry
module S = Octra_core.Store_irmin
module Fold = Octra_core.Set_fold
module T = Octra_core.Transaction
module Update = Octra_core.Validator_set_update

type identity = {
  address : string;
  consensus_private : Mirage_crypto_ec.Ed25519.priv;
  consensus_public : string;
}

let expect label value =
  if not value then failwith label

let expect_ok label = function
  | Ok value -> value
  | Error error -> failwith (label ^ ": " ^ error)

let rec remove_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
    end else
      Unix.unlink path

let rec make_directory path =
  if not (Sys.file_exists path) then begin
    let parent = Filename.dirname path in
    if not (String.equal parent path) then make_directory parent;
    Unix.mkdir path 0o755
  end

let with_store f =
  let root =
    Filename.concat
      (Sys.getcwd ())
      ("runtime_data/validator_lifecycle_" ^ string_of_int (Unix.getpid ()))
  in
  remove_tree root;
  make_directory root;
  let store = Lwt_main.run (S.open_store (Filename.concat root "irmin")) in
  Fun.protect
    ~finally:(fun () ->
      Lwt_main.run (S.close store);
      remove_tree root)
    (fun () -> f store)

let identity () =
  let consensus_private, consensus_public =
    Mirage_crypto_ec.Ed25519.generate ()
  in
  let consensus_public =
    Mirage_crypto_ec.Ed25519.pub_to_octets consensus_public
  in
  let address =
    consensus_public
    |> Base64.encode_exn
    |> Octra_core.Crypto.Address.address_from_pubkey
  in
  {
    address;
    consensus_private;
    consensus_public;
  }

let parent chain_id epoch identities =
  let module C = Octra_consensus.C_types in
  let module H = Octra_consensus.C_hash in
  let validator_set = C.make_validator_set (List.map (fun (item : identity) ->
    C.{address = item.address; pubkey = item.consensus_public}) identities) in
  let header = C.{
    proto_version = proto_version_current; chain_id; epoch_id = epoch;
    prev_state_root = String.make 32 'a'; tx_list_hash = H.receipt_root [];
    receipt_root = H.receipt_root []; proposed_state_root = String.make 32 'b';
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = (List.hd identities).address; txid_hi = 0L; ts = 1.;
  } in
  let proposal_id = H.proposal_id header in
  let precommits = List.map (fun (item : identity) ->
    let vote = C.{chain_id; epoch_id = epoch; round = 0; vote_type = Precommit;
      proposal_id; validator = item.address; signature = ""} in
    {vote with signature = Mirage_crypto_ec.Ed25519.sign
      ~key:item.consensus_private (H.vote_sign_bytes vote)}) identities in
  C.{validator_set; certificate = {chain_id; epoch_id = epoch; commit_round = 0;
    header; proposal_id; precommits}}

let proposal_hex commit =
  let value = commit.Octra_consensus.C_types.certificate.proposal_id in
  String.concat "" (List.init (String.length value)
    (fun index -> Printf.sprintf "%02x" (Char.code value.[index])))

let bond_message ~chain_id ~amount ~nonce identity =
  let message =
    Registry.bond_message
      ~chain_id
      ~address:identity.address
      ~amount
      ~nonce
      ~pubkey:identity.consensus_public
  in
  let proof =
    Mirage_crypto_ec.Ed25519.sign
      ~key:identity.consensus_private
      message
  in
  `Assoc [
    "consensus_pubkey", `String (Base64.encode_exn identity.consensus_public);
    "proof", `String (Base64.encode_exn proof);
  ]
  |> Yojson.Safe.to_string

let ready_message ?proposal_id ~head_epoch ~state_root identity =
  `Assoc ([
    "consensus_pubkey", `String (Base64.encode_exn identity.consensus_public);
    "head_epoch", `String (Int64.to_string head_epoch);
    "state_root", `String state_root;
  ] @ match proposal_id with
      | None -> []
      | Some value -> ["head_proposal_id", `String value])
  |> Yojson.Safe.to_string

let transaction ~ou ~from ~public_key ~to_ ~amount ~nonce ~message ~op_type =
  {
    T.from;
    to_;
    amount;
    nonce;
    ou;
    timestamp = 360.;
    signature = "signature";
    public_key = Some public_key;
    message = Some message;
    op_type;
    encrypted_data = None;
  }

let conflict ~chain_id ~epoch (identity : identity) =
  let signed proposal =
    let vote = Octra_consensus.C_types.{
      chain_id;
      epoch_id = epoch;
      round = 1;
      vote_type = Precommit;
      proposal_id = proposal;
      validator = identity.address;
      signature = String.make 64 '\x00';
    } in
    {
      vote with
      signature =
        Mirage_crypto_ec.Ed25519.sign
          ~key:identity.consensus_private
          (Octra_consensus.C_hash.vote_sign_bytes vote);
    }
  in
  match
    Octra_consensus.C_evidence.vote_conflict
      (signed (String.make 32 '\x01'))
      (signed (String.make 32 '\x02'))
  with
  | Some value -> value
  | None -> failwith "conflict construction failed"

let policy =
  Policy.of_env_exn
    (fun name ->
      if String.equal name Policy.env_name then Some "100"
      else None)

let run_lifecycle ?(ready_exec_mode = Octra_core.Rule_graph.Prior) ?(delay = 0) store =
  let chain_id = "validator-lifecycle-test" in
  let source_epoch = 36 in
  let state_root = String.make 64 'a' in
  let amount = Policy.min_bond in
  let identities = List.init 5 (fun _ -> identity ()) in
  let commit = parent chain_id 35L identities in
  let heads = List.map (fun epoch -> parent chain_id epoch identities) [33L; 34L] in
  let state = List.fold_left (fun state commit ->
    let final, signers, active = Fold.read_parent ~chain_id commit
      |> expect_ok "verified ready history" in
    Fold.note_final Fold.standard ~at:(Int64.succ final.epoch) ~active ~final ~signers state
    |> expect_ok "ready history") Fold.empty heads in
  if ready_exec_mode = Octra_core.Rule_graph.Active then
    Lwt_main.run (S.set_meta store Fold.meta_key (Fold.to_string state));
  let reference = Int64.of_int (35 - delay) in
  let proposal_id = parent chain_id reference identities |> proposal_hex in
  let ledger = L.create store in
  List.iter
    (fun validator ->
      L.add_account ledger validator.address (Z.of_int 2_000_000)
      |> expect_ok "validator account")
    identities;
  Lwt_main.run (L.flush_dirty_lwt ledger);
  let first = List.hd identities in
  let second = List.nth identities 1 in
  let mismatched_bond =
    transaction
      ~ou:(Z.of_int 1_000)
      ~from:first.address
      ~public_key:(Base64.encode_exn second.consensus_public)
      ~to_:Registry.escrow_address
      ~amount
      ~nonce:1
      ~message:(bond_message ~chain_id ~amount ~nonce:1 first)
      ~op_type:T.ValidatorBond
  in
  let transactions =
    identities
    |> List.concat_map
      (fun validator ->
        [
          transaction
            ~ou:(Z.of_int 1_000)
            ~from:validator.address
            ~public_key:(Base64.encode_exn validator.consensus_public)
            ~to_:Registry.escrow_address
            ~amount
            ~nonce:1
            ~message:
              (bond_message
                 ~chain_id
                 ~amount
                 ~nonce:1
                 validator)
            ~op_type:T.ValidatorBond;
          transaction
            ~ou:(Z.of_int 1_000)
            ~from:validator.address
            ~public_key:(Base64.encode_exn validator.consensus_public)
            ~to_:validator.address
            ~amount:Z.zero
            ~nonce:2
            ~message:
              (ready_message
                 ~proposal_id
                 ~head_epoch:reference
                 ~state_root
                 validator)
            ~op_type:T.ValidatorReady;
        ])
    |> List.cons mismatched_bond
  in
  let env =
    {
      X.chain_id;
      epoch_id = source_epoch;
      proposer_addr = (List.hd identities).address;
      validator_addrs = List.map (fun validator -> validator.address) identities;
      validator_pubkeys =
        List.map
          (fun validator ->
            validator.address, validator.consensus_public)
          identities;
      prev_state_root = state_root;
      epoch_ts = 360.;
      ready_state_root_at = (match ready_exec_mode with
        | Octra_core.Rule_graph.Prior -> None
        | Octra_core.Rule_graph.Active -> Some (fun _ -> failwith "local root lookup"));
      ready_max_lag = 0;
    }
  in
  let fold epoch = X.prior_fold epoch
    |> Result.map (fun ctx -> X.{ctx with ready_exec_mode; parent = Some commit}) in
  let build store ledger =
    X.make_live_backend
      ~emission_policy:E.Guard
      ~legacy_total_supply:"10000000"
      ~validator_policy:policy
      ~fold
      store
      ledger
  in
  let backend = build store ledger in
  let execute backend env = X.run ~backend ~env ~txs:transactions
    ~process_tx:X.process_standard_tx in
  let before = Lwt_main.run (S.state_hash store) in
  let preview = Lwt_main.run (Octra_core.State_preview.with_state
    ~base_store:store ~base_ledger:ledger ~epoch_id:source_epoch
    ~proposal_id:"ready-preview"
    (fun store ledger -> execute (build store ledger) env))
    |> expect_ok "ready preview" in
  expect "ready preview isolated" (Lwt_main.run (S.state_hash store) = before);
  let copy = store.S.store_path ^ "_copy" in
  remove_tree copy;
  let commit = Lwt_main.run (S.get_commit_hash store) |> Option.get in
  ignore (Lwt_main.run (S.create_compact_store store ~expected_commit:commit ~target:copy)
    |> expect_ok "ready state export");
  let restored = Lwt_main.run (S.open_store copy) in
  let replay = Fun.protect
    ~finally:(fun () -> Lwt_main.run (S.close restored); remove_tree copy)
    (fun () ->
      expect "ready snapshot root" (Lwt_main.run (S.state_hash restored) = before);
      let env = match ready_exec_mode with
        | Octra_core.Rule_graph.Prior -> env
        | Octra_core.Rule_graph.Active -> {env with ready_max_lag = max_int;
            ready_state_root_at = Some (fun _ -> failwith "restored local history read")}
      in
      execute (build restored (L.create restored)) env
      |> Lwt_main.run |> expect_ok "ready restored execution") in
  expect "ready preview snapshot partition" (preview = replay);
  let result =
    X.run
      ~backend
      ~env
      ~txs:transactions
      ~process_tx:X.process_standard_tx
    |> Lwt_main.run
    |> expect_ok "lifecycle epoch"
  in
  expect "ready apply snapshot equality" (result = replay);
  expect "all lifecycle transactions confirmed"
    (List.length result.X.artifacts.confirmed = 10);
  expect "mismatched consensus key rejected"
    (List.length result.X.artifacts.rejected = 1);
  expect "prior rule wrote no set fold state"
    (Lwt_main.run (S.get_meta store Fold.meta_key)
     = (if ready_exec_mode = Octra_core.Rule_graph.Prior then None
        else Some (Fold.to_string state)));
  let registry =
    S.get_meta store Registry.meta_key
    |> Lwt_main.run
    |> Option.get
    |> Registry.of_string
    |> expect_ok "stored registry"
  in
  expect "five registered validators"
    (List.length (Registry.entries registry) = 5);
  let update =
    S.get_meta store Update.pending_meta_key
    |> Lwt_main.run
    |> Option.get
    |> Update.of_string
    |> expect_ok "scheduled validator set"
  in
  expect "weighted update" update.Update.weighted;
  expect "snapshot source epoch"
    (update.Update.source_epoch = Some 36L);
  expect "delayed activation"
    (update.Update.activate_epoch = 100L);
  expect "five scheduled validators"
    (List.length update.Update.validators = 5);
  let total_weight =
    List.fold_left
      (fun total validator -> Z.add total validator.Update.weight)
      Z.zero
      update.Update.validators
  in
  expect "scheduled weight preserves bonds"
    Z.(equal total_weight (mul amount (of_int 5)));
  expect "escrow balance preserves bonds"
    Z.(
      equal
        (L.find ledger Registry.escrow_address).L.balance
        (mul amount (of_int 5)));
  let activation_env = { env with X.epoch_id = 100; epoch_ts = 1_000. } in
  X.run
    ~backend
    ~env:activation_env
    ~txs:[]
    ~process_tx:X.process_standard_tx
  |> Lwt_main.run
  |> expect_ok "activation epoch"
  |> ignore;
  let active =
    S.get_meta store Update.active_meta_key
    |> Lwt_main.run
    |> Option.get
    |> Update.of_string
    |> expect_ok "active validator set"
  in
  expect "promoted fingerprint"
    (String.equal active.Update.fingerprint update.Update.fingerprint);
  let next =
    S.get_meta store Update.pending_meta_key
    |> Lwt_main.run
    |> Option.get
    |> Update.of_string
    |> expect_ok "next validator set"
  in
  expect "next snapshot source epoch"
    (next.Update.source_epoch = Some 100L);
  expect "next delayed activation"
    (next.Update.activate_epoch = 164L);
  expect "next snapshot differs from active marker"
    (not (String.equal next.Update.fingerprint active.Update.fingerprint));
  let fee = Z.of_int 1_000 in
  let exit_tx =
    transaction
      ~ou:fee
      ~from:first.address
      ~public_key:(Base64.encode_exn first.consensus_public)
      ~to_:first.address
      ~amount:Z.zero
      ~nonce:3
      ~message:"{}"
      ~op_type:T.ValidatorExit
  in
  let exit_env = { env with X.epoch_id = 101; epoch_ts = 1_010. } in
  let exit_result =
    X.run
      ~backend
      ~env:exit_env
      ~txs:[exit_tx]
      ~process_tx:X.process_standard_tx
    |> Lwt_main.run
    |> expect_ok "validator exit"
  in
  expect "validator exit confirmed"
    (List.length exit_result.X.artifacts.confirmed = 1);
  let registry =
    S.get_meta store Registry.meta_key
    |> Lwt_main.run
    |> Option.get
    |> Registry.of_string
    |> expect_ok "exiting registry"
  in
  let exiting = Registry.find first.address registry |> Option.get in
  expect "validator exit epoch recorded"
    (exiting.exit_epoch = Some 101L);
  let source_env = { env with X.epoch_id = 164; epoch_ts = 1_640. } in
  X.run
    ~backend
    ~env:source_env
    ~txs:[]
    ~process_tx:X.process_standard_tx
  |> Lwt_main.run
  |> expect_ok "exit snapshot source"
  |> ignore;
  let replacement =
    S.get_meta store Update.pending_meta_key
    |> Lwt_main.run
    |> Option.get
    |> Update.of_string
    |> expect_ok "exit replacement set"
  in
  expect "exit replacement activates after delay"
    (replacement.Update.activate_epoch = 228L);
  expect "exit replacement retains four validators"
    (List.length replacement.Update.validators = 4);
  expect "exit replacement omits exiting validator"
    (not
       (List.exists
          (fun validator ->
            String.equal validator.Update.address first.address)
          replacement.Update.validators));
  let replacement_env = { env with X.epoch_id = 228; epoch_ts = 2_280. } in
  X.run
    ~backend
    ~env:replacement_env
    ~txs:[]
    ~process_tx:X.process_standard_tx
  |> Lwt_main.run
  |> expect_ok "exit replacement activation"
  |> ignore;
  let active =
    S.get_meta store Update.active_meta_key
    |> Lwt_main.run
    |> Option.get
    |> Update.of_string
    |> expect_ok "active replacement set"
  in
  let active_addresses =
    List.map (fun validator -> validator.Update.address) active.Update.validators
  in
  expect "exiting validator left active set"
    (not (List.mem first.address active_addresses));
  let withdraw_tx =
    transaction
      ~ou:fee
      ~from:first.address
      ~public_key:(Base64.encode_exn first.consensus_public)
      ~to_:first.address
      ~amount:Z.zero
      ~nonce:4
      ~message:"{}"
      ~op_type:T.ValidatorWithdraw
  in
  let early_env = {
    env with
    X.epoch_id = 229;
    proposer_addr = second.address;
    validator_addrs = active_addresses;
    epoch_ts = 2_290.;
  } in
  let early =
    X.run
      ~backend
      ~env:early_env
      ~txs:[withdraw_tx]
      ~process_tx:X.process_standard_tx
    |> Lwt_main.run
    |> expect_ok "early validator withdrawal"
  in
  expect "early validator withdrawal rejected"
    (match early.X.artifacts.rejected with
     | [{ X.error_type = "validator_withdraw_rejected"; _ }] -> true
     | _ -> false);
  let mature_epoch = Int64.add 101L Policy.unbonding_epochs in
  let mature_env = {
    early_env with
    X.epoch_id = Int64.to_int mature_epoch;
    epoch_ts = Int64.to_float mature_epoch *. 10.;
  } in
  let balance_before = (L.find ledger first.address).L.balance in
  let escrow_before = (L.find ledger Registry.escrow_address).L.balance in
  let mature =
    X.run
      ~backend
      ~env:mature_env
      ~txs:[withdraw_tx]
      ~process_tx:X.process_standard_tx
    |> Lwt_main.run
    |> expect_ok "mature validator withdrawal"
  in
  expect "mature validator withdrawal confirmed"
    (List.length mature.X.artifacts.confirmed = 1);
  let registry =
    S.get_meta store Registry.meta_key
    |> Lwt_main.run
    |> Option.get
    |> Registry.of_string
    |> expect_ok "withdrawn registry"
  in
  expect "withdrawal removes validator"
    (Registry.find first.address registry = None);
  expect "withdrawal returns full bond from escrow"
    Z.(equal
         (L.find ledger Registry.escrow_address).L.balance
         (sub escrow_before amount));
  expect "withdrawal returns bond after fee"
    Z.(equal
         (L.find ledger first.address).L.balance
         (add balance_before (sub amount fee)))

let run_manual_takeover_rejected ~validator_policy ~epoch_id store =
  let operator = identity () in
  let attacker = identity () in
  let balance = Z.of_int 100_000 in
  let ledger = L.create store in
  L.add_account ledger operator.address balance
  |> expect_ok "manual update operator account";
  Lwt_main.run (L.flush_dirty_lwt ledger);
  let validators = [
    Update.{
      address = attacker.address;
      pubkey_b64 = Base64.encode_exn attacker.consensus_public;
      weight = Z.one;
    };
  ] in
  let update = Update.{
    activate_epoch = Int64.of_int (epoch_id + 10);
    source_epoch = None;
    validators;
    weighted = false;
    fingerprint = Update.fingerprint validators;
    eviction = None;
  } in
  let tx =
    transaction
      ~ou:(Z.of_int 1_000)
      ~from:operator.address
      ~public_key:(Base64.encode_exn operator.consensus_public)
      ~to_:operator.address
      ~amount:Z.zero
      ~nonce:1
      ~message:(Update.to_string update)
      ~op_type:T.ValidatorSetUpdate
  in
  let env = {
    X.chain_id = "manual-takeover-test";
    epoch_id;
    proposer_addr = operator.address;
    validator_addrs = [operator.address];
    validator_pubkeys = [operator.address, operator.consensus_public];
    prev_state_root = String.make 64 'd';
    epoch_ts = 100.;
    ready_state_root_at = None;
    ready_max_lag = 0;
  } in
  let backend =
    X.make_live_backend
      ~emission_policy:E.Guard
      ~legacy_total_supply:(Z.to_string balance)
      ~validator_policy
      store
      ledger
  in
  let result =
    X.run
      ~backend
      ~env
      ~txs:[tx]
      ~process_tx:X.process_standard_tx
    |> Lwt_main.run
    |> expect_ok "manual takeover epoch"
  in
  expect "manual takeover rejected"
    (match result.X.artifacts.rejected with
     | [{ X.error_type = "validator_set_update_rejected";
          reason = "manual validator updates are disabled"; _ }] -> true
     | _ -> false);
  expect "manual takeover did not write pending set"
    (Lwt_main.run (S.get_meta store Update.pending_meta_key) = None);
  let account = L.find ledger operator.address in
  expect "manual takeover did not debit balance"
    Z.(equal account.L.balance balance);
  expect "manual takeover did not consume nonce" (account.L.nonce = 0)

let run_slashing store =
  let chain_id = "validator-slash-test" in
  let epoch_id = 101 in
  let offender = identity () in
  let reporter = identity () in
  let amount = Policy.min_bond in
  let reporter_balance = Z.of_int 100_000 in
  let fee = Z.of_int 5_000 in
  let ledger = L.create store in
  L.add_account ledger reporter.address reporter_balance
  |> expect_ok "reporter account";
  L.add_account ledger offender.address Z.zero
  |> expect_ok "offender account";
  L.add_account ledger Registry.escrow_address amount
  |> expect_ok "escrow account";
  Lwt_main.run (L.flush_dirty_lwt ledger);
  let payload =
    Registry.{
      consensus_pubkey_b64 =
        Base64.encode_exn offender.consensus_public;
      proof_b64 =
        Registry.bond_message
          ~chain_id
          ~address:offender.address
          ~amount
          ~nonce:1
          ~pubkey:offender.consensus_public
        |> Mirage_crypto_ec.Ed25519.sign
             ~key:offender.consensus_private
        |> Base64.encode_exn;
    }
  in
  let registry =
    Registry.apply_bond
      Policy.parameters
      ~chain_id
      ~epoch:100L
      ~address:offender.address
      ~sender_pubkey_b64:(Base64.encode_exn offender.consensus_public)
      ~amount
      ~nonce:1
      payload
      Registry.empty
    |> expect_ok "offender bond"
  in
  let total_before = L.get_total_supply ledger in
  let initial = Z.sub Octra_core.Denomination.max_supply total_before in
  let set key value = Lwt_main.run (S.set_meta store key value) in
  set Registry.meta_key (Registry.to_string registry);
  set "total_supply" (Z.to_string total_before);
  set "emission_remaining" (Z.to_string initial);
  set Octra_core.Emission_schedule.standard_key
    Octra_core.Emission_schedule.standard_name;
  set Octra_core.Emission_schedule.activation_key (string_of_int epoch_id);
  set Octra_core.Emission_schedule.initial_key (Z.to_string initial);
  set Octra_core.Emission_schedule.retired_key "0";
  let evidence =
    conflict
      ~chain_id
      ~epoch:100L
      offender
  in
  let tx =
    transaction
      ~ou:fee
      ~from:reporter.address
      ~public_key:(Base64.encode_exn reporter.consensus_public)
      ~to_:offender.address
      ~amount:Z.zero
      ~nonce:1
      ~message:(Octra_core.Validator_evidence.message evidence)
      ~op_type:T.ValidatorEvidence
  in
  let env = {
    X.chain_id;
    epoch_id;
    proposer_addr = reporter.address;
    validator_addrs = [reporter.address];
    validator_pubkeys = [
      reporter.address,
      Base64.encode_exn reporter.consensus_public;
    ];
    prev_state_root = String.make 64 'b';
    epoch_ts = 1_010.;
    ready_state_root_at = None;
    ready_max_lag = 0;
  } in
  let backend =
    X.make_live_backend
      ~emission_policy:E.Allow
      ~emission_schedule:
        (Octra_core.Emission_schedule.Security_curve {
          activation_epoch = epoch_id;
        })
      ~validator_policy:policy
      store
      ledger
  in
  let result =
    X.run
      ~backend
      ~env
      ~txs:[tx]
      ~process_tx:X.process_standard_tx
    |> Lwt_main.run
    |> expect_ok "slash epoch"
  in
  expect "slash transaction confirmed"
    (List.length result.X.artifacts.confirmed = 1);
  let registry =
    S.get_meta store Registry.meta_key
    |> Lwt_main.run
    |> Option.get
    |> Registry.of_string
    |> expect_ok "slashed registry"
  in
  expect "slashed validator removed"
    (Registry.find offender.address registry = None);
  expect "slash evidence persisted"
    (List.length (Registry.slashes registry) = 1);
  expect "escrow bond retired"
    Z.(equal (L.find ledger Registry.escrow_address).L.balance zero);
  let total =
    S.get_meta store "total_supply"
    |> Lwt_main.run
    |> Option.get
    |> Z.of_string
  in
  let remaining =
    S.get_meta store "emission_remaining"
    |> Lwt_main.run
    |> Option.get
    |> Z.of_string
  in
  let retired =
    S.get_meta store Octra_core.Emission_schedule.retired_key
    |> Lwt_main.run
    |> Option.get
    |> Z.of_string
  in
  expect "slash ledger and metadata agree"
    Z.(equal total (L.get_total_supply ledger));
  expect "slash and fee burn retired"
    Z.(equal retired (add amount (div fee (of_int 5))));
  expect "slash supply envelope"
    Z.(
      equal
        (add total (add remaining retired))
        Octra_core.Denomination.max_supply)

let run_slash_rollback store =
  let chain_id = "validator-slash-rollback-test" in
  let offender = identity () in
  let reporter = identity () in
  let amount = Policy.min_bond in
  let fee = Z.of_int 5_000 in
  let ledger = L.create store in
  L.add_account ledger reporter.address (Z.of_int 10_000)
  |> expect_ok "rollback reporter";
  L.add_account ledger offender.address Z.zero
  |> expect_ok "rollback offender";
  L.add_account ledger Registry.escrow_address (Z.pred amount)
  |> expect_ok "rollback escrow";
  Lwt_main.run (L.flush_dirty_lwt ledger);
  let bond_proof =
    Registry.bond_message
      ~chain_id
      ~address:offender.address
      ~amount
      ~nonce:1
      ~pubkey:offender.consensus_public
    |> Mirage_crypto_ec.Ed25519.sign
         ~key:offender.consensus_private
  in
  let registry =
    Registry.apply_bond
      Policy.parameters
      ~chain_id
      ~epoch:100L
      ~address:offender.address
      ~sender_pubkey_b64:(Base64.encode_exn offender.consensus_public)
      ~amount
      ~nonce:1
      Registry.{
        consensus_pubkey_b64 =
          Base64.encode_exn offender.consensus_public;
        proof_b64 = Base64.encode_exn bond_proof;
      }
      Registry.empty
    |> expect_ok "rollback bond"
  in
  let total = L.get_total_supply ledger in
  let remaining = Z.sub Octra_core.Denomination.max_supply total in
  let set key value = Lwt_main.run (S.set_meta store key value) in
  let registry_raw = Registry.to_string registry in
  set Registry.meta_key registry_raw;
  set "total_supply" (Z.to_string total);
  set "emission_remaining" (Z.to_string remaining);
  set Octra_core.Emission_schedule.retired_key "0";
  let evidence =
    conflict
      ~chain_id
      ~epoch:100L
      offender
  in
  let tx =
    transaction
      ~ou:fee
      ~from:reporter.address
      ~public_key:(Base64.encode_exn reporter.consensus_public)
      ~to_:offender.address
      ~amount:Z.zero
      ~nonce:1
      ~message:(Octra_core.Validator_evidence.message evidence)
      ~op_type:T.ValidatorEvidence
  in
  let env = {
    X.chain_id;
    epoch_id = 101;
    proposer_addr = reporter.address;
    validator_addrs = [reporter.address];
    validator_pubkeys = [];
    prev_state_root = String.make 64 'c';
    epoch_ts = 1_010.;
    ready_state_root_at = None;
    ready_max_lag = 0;
  } in
  let backend =
    X.make_live_backend
      ~emission_policy:E.Allow
      ~emission_schedule:Octra_core.Emission_schedule.Inactive
      ~validator_policy:policy
      store
      ledger
  in
  let reporter_before = (L.find ledger reporter.address).L.balance in
  let escrow_before =
    (L.find ledger Registry.escrow_address).L.balance
  in
  Lwt_main.run (S.begin_epoch_batch store);
  let result =
    Octra_core.Tx_savepoint.run
      ~ledger
      ~store
      (fun () -> X.process_validator_evidence_tx ~backend ~env tx)
    |> Lwt_main.run
  in
  S.abort_epoch_batch store;
  expect "insufficient escrow rejects slash" (Result.is_error result);
  expect "rollback restores reporter"
    Z.(equal reporter_before (L.find ledger reporter.address).L.balance);
  expect "rollback restores escrow"
    Z.(
      equal
        escrow_before
        (L.find ledger Registry.escrow_address).L.balance);
  expect "rollback restores registry"
    (S.get_meta store Registry.meta_key
     |> Lwt_main.run
     |> Option.value ~default:""
     |> String.equal registry_raw);
  expect "rollback restores total supply"
    (S.get_meta store "total_supply"
     |> Lwt_main.run
     = Some (Z.to_string total));
  expect "rollback restores retired supply"
    (S.get_meta store Octra_core.Emission_schedule.retired_key
     |> Lwt_main.run
     = Some "0")

let run_ready_active ~delay store =
  let module G = Octra_core.Rule_graph in
  let chain_id = "ready-active-test" in
  let config = String.make 64 'c' in
  let cfg = Fold.participating in
  let validators = List.init 4 (fun _ -> identity ()) in
  let returning = identity () in
  let fresh = identity () in
  let senders = [returning; fresh] in
  let active = List.map (fun item -> item.address) validators in
  expect "ready senders are outside the certified set"
    (List.for_all (fun item -> not (List.mem item.address active)) senders);
  let ledger = L.create store in
  let funds = Z.of_int 2_000_000 in
  let fee = Z.of_int 1_000 in
  List.iter (fun item ->
    L.add_account ledger item.address funds |> expect_ok "active ready account")
    (validators @ senders);
  L.add_account ledger Registry.escrow_address (Z.mul Policy.min_bond (Z.of_int 2))
  |> expect_ok "active ready escrow";
  Lwt_main.run (L.flush_dirty_lwt ledger);
  let registry = `Assoc [
    "standard", `String Policy.standard_name;
    "candidates", `List (List.map (fun item -> `Assoc [
      "address", `String item.address;
      "consensus_pubkey", `String (Base64.encode_exn item.consensus_public);
      "bond", `String (Z.to_string Policy.min_bond);
      "bonded_epoch", `String "100";
      "ready_epoch", `Null;
      "exit_epoch", `Null;
    ]) senders);
    "slashes", `List [];
  ] |> Registry.of_yojson |> expect_ok "active ready registry" in
  let series = List.init 16 (fun index -> Int64.of_int (137 + 4 * index))
    |> List.fold_left (fun state epoch ->
      Fold.note_pulse cfg ~epoch ~active:false ~address:returning.address state
      |> expect_ok "active ready prior pulse") Fold.empty in
  let state = List.fold_left (fun state at ->
    let commit = parent chain_id (Int64.pred at) validators in
    let state, reason, _ = Fold.advance cfg ~chain_id ~start:198L ~at
      ~parent:(Some commit) state |> expect_ok "active ready history" in
    expect "active ready history has no safety delay" (reason = None);
    state) series [198L; 199L; 200L] in
  Lwt_main.run (S.set_meta store Registry.meta_key (Registry.to_string registry));
  Lwt_main.run (S.set_meta store Fold.meta_key (Fold.to_string state));
  let fold epoch =
    let commit = parent chain_id (Int64.of_int (epoch - 1)) validators in
    match Fold.read_parent ~chain_id commit with
    | Error _ as error -> error
    | Ok (final, _, members) ->
      expect "active ready parent is the execution parent"
        (final.epoch = Int64.of_int (epoch - 1));
      X.prior_fold epoch |> Result.map (fun ctx -> X.{ctx with
        mode = G.Active;
        ready_mode = G.Active;
        ready_ref_mode = G.Active;
        ready_exec_mode = G.Active;
        standard_mode = G.Active;
        live_mode = G.Active;
        cap_mode = Fold.Prune;
        ready_config_hash = Some config;
        start = 100L;
        profile_start = 100L;
        parent = Some commit;
        members;
      })
  in
  let accounts ledger = List.map (fun item ->
    let account = L.find ledger item.address in
    account.L.nonce, account.L.balance) senders in
  let execute store ledger env txs =
    let open Lwt.Syntax in
    let backend = X.make_live_backend ~emission_policy:E.Guard
      ~emission_schedule:Octra_core.Emission_schedule.Inactive
      ~legacy_total_supply:(Z.to_string (L.get_total_supply ledger))
      ~validator_policy:policy ~fold store ledger in
    let* result = X.run ~backend ~env ~txs ~process_tx:X.process_standard_tx in
    match result with
    | Error _ as error -> Lwt.return error
    | Ok result ->
      let* folded = S.get_meta store Fold.meta_key in
      let* registered = S.get_meta store Registry.meta_key in
      Lwt.return_ok (result, Option.get folded, Option.get registered, accounts ledger)
  in
  let ready ?(chain = chain_id) ?(hash = config) ?catchup epoch nonce item =
    let head = Int64.of_int (epoch - 1 - delay) in
    let proposal = parent chain_id head validators |> proposal_hex in
    let message = `Assoc [
      "consensus_pubkey", `String (Base64.encode_exn item.consensus_public);
      "head_epoch", `String (Int64.to_string head);
      "head_proposal_id", `String proposal;
      "state_root", `String "informational";
      "chain_id", `String chain;
      "config_hash", `String hash;
      "catchup_head_epoch", `String (Int64.to_string (Option.value catchup ~default:head));
    ] |> Yojson.Safe.to_string in
    let tx = transaction ~ou:fee ~from:item.address
      ~public_key:(Base64.encode_exn item.consensus_public) ~to_:item.address
      ~amount:Z.zero ~nonce ~message ~op_type:T.ValidatorReady in
    let secret = Mirage_crypto_ec.Ed25519.priv_to_octets item.consensus_private
      |> Base64.encode_exn in
    let tx = T.sign_with_privkey tx secret in
    expect "active ready payload is signed"
      (T.verify tx (Base64.encode_exn item.consensus_public));
    tx
  in
  let env epoch = X.{
    chain_id;
    epoch_id = epoch;
    proposer_addr = List.hd active;
    validator_addrs = active;
    validator_pubkeys = List.map (fun (item : identity) -> item.address, item.consensus_public) validators;
    prev_state_root = String.make 64 'a';
    epoch_ts = float_of_int epoch;
    ready_state_root_at = Some (fun _ -> failwith "active ready consulted local history");
    ready_max_lag = 0;
  } in
  let preview env txs =
    let root = Lwt_main.run (S.state_hash store) in
    let balances = accounts ledger in
    let result = Octra_core.State_preview.with_state ~base_store:store ~base_ledger:ledger
      ~epoch_id:env.X.epoch_id ~proposal_id:"active-ready-preview"
      (fun store ledger -> execute store ledger env txs)
      |> Lwt_main.run |> expect_ok "active ready preview" in
    expect "active ready preview leaves live state untouched"
      (Lwt_main.run (S.state_hash store) = root && accounts ledger = balances);
    result
  in
  if delay = 2 then
    List.iter (fun (tx, reason) ->
      let result, _, registered, balances = preview (env 201) [tx] in
      expect "active policy rejects a mismatched signed claim"
        (result.X.artifacts.confirmed = []
         && match result.artifacts.rejected with
            | [row] -> row.error_type = "validator_ready_rejected" && row.reason = reason
            | _ -> false);
      expect "active policy rejection preserves registry and sender accounts"
        (registered = Registry.to_string registry && balances = accounts ledger))
      [ready ~chain:"other" 201 1 returning, "chain_id mismatch";
       ready ~hash:(String.make 64 'd') 201 1 returning, "config_hash mismatch";
       ready ~catchup:200L 201 1 returning, "catchup_head_epoch mismatch"];
  let pulse raw address =
    let open Yojson.Safe.Util in
    let entries = Yojson.Safe.from_string raw |> member "members" |> to_list in
    match List.find_opt (fun item -> member "address" item = `String address) entries with
    | None -> None
    | Some item ->
      let value = item |> member "phase" |> member "pulse" in
      if value = `Null then None else Some (
        (member "first" value |> to_string |> Int64.of_string),
        (member "last" value |> to_string |> Int64.of_string),
        (member "count" value |> to_int))
  in
  List.iter (fun epoch ->
    let env = env epoch in
    let nonce = epoch - 200 in
    let txs = List.map (ready epoch nonce) senders in
    let predicted = preview env txs in
    let before = Lwt_main.run (S.state_hash store) in
    let copy = store.S.store_path ^ "_active_copy" in
    remove_tree copy;
    let commit = Lwt_main.run (S.get_commit_hash store) |> Option.get in
    S.create_compact_store store ~expected_commit:commit ~target:copy
    |> Lwt_main.run |> expect_ok "active ready compact export" |> ignore;
    let restored = Lwt_main.run (S.open_store copy) in
    let replay = Fun.protect
      ~finally:(fun () -> Lwt_main.run (S.close restored); remove_tree copy)
      (fun () ->
        expect "active ready restored root matches" (Lwt_main.run (S.state_hash restored) = before);
        let env = {env with X.ready_max_lag = max_int;
          ready_state_root_at = Some (fun _ -> failwith "active restore consulted local history")} in
        execute restored (L.create restored) env txs
        |> Lwt_main.run |> expect_ok "active ready restored execution") in
    let applied = execute store ledger env txs
      |> Lwt_main.run |> expect_ok "active ready apply" in
    expect "active preview restore and apply agree exactly"
      (predicted = replay && replay = applied);
    let result, folded, registered, balances = applied in
    let state = Fold.of_string folded |> expect_ok "active ready stored fold" in
    let registry = Registry.of_string registered |> expect_ok "active ready stored registry" in
    let source = Int64.of_int epoch in
    if delay = 3 then begin
      expect "lag three rejects both signed ready transactions"
        (result.X.artifacts.confirmed = [] && List.length result.artifacts.rejected = 2
         && List.for_all (fun row -> row.X.error_type = "validator_ready_rejected"
           && row.reason = "head_epoch outside delivery window") result.artifacts.rejected);
      expect "lag three preserves pulse progress"
        (pulse folded returning.address = Some (137L, 197L, 16)
         && pulse folded fresh.address = None);
      expect "lag three consumes neither fees nor nonces"
        (Z.equal result.artifacts.confirmed_fees Z.zero
         && balances = [0, funds; 0, funds])
    end else begin
      expect "lag two confirms both signed ready transactions"
        (List.length result.X.artifacts.confirmed = 2 && result.artifacts.rejected = []);
      expect "execution credits the signed head successor"
        (pulse folded returning.address = Some (137L, Int64.of_int (epoch - 2), 16 + nonce));
      expect "delayed first and repeated credit cannot backdate the series"
        (pulse folded fresh.address = Some (201L, 201L, 1));
      expect "returning eligibility needs the full credited span"
        (Fold.allows cfg ~start:100L ~source ~address:returning.address state = (epoch = 203));
      expect "fresh delivery cannot accelerate rejoin"
        (not (Fold.allows cfg ~start:100L ~source ~address:fresh.address state));
      expect "accepted delivery charges one fee and nonce per transaction"
        (Z.equal result.artifacts.confirmed_fees (Z.mul fee (Z.of_int 2))
         && balances = List.map (fun _ -> nonce, Z.sub funds (Z.mul fee (Z.of_int nonce))) senders)
    end;
    let returning = Registry.find returning.address registry |> Option.get in
    let fresh = Registry.find fresh.address registry |> Option.get in
    expect "active ready marking follows credited eligibility"
      (returning.Octra_core.Validator_entry.ready_epoch =
         (if delay = 2 && epoch = 203 then Some source else None)
       && fresh.Octra_core.Validator_entry.ready_epoch = None))
    (if delay = 2 then [201; 202; 203] else [201])

let run_slots ~transition store =
  let pending = Update.{
    activate_epoch = 136L;
    source_epoch = Some 128L;
    validators = [];
    weighted = true;
    fingerprint = "";
    eviction = None;
  } in
  expect "empty slot available" (Update.snapshot_slot ~epoch:128L None);
  expect "future slot retained" (not (Update.snapshot_slot ~epoch:128L (Some pending)));
  expect "due slot available"
    (Update.snapshot_slot ~epoch:pending.activate_epoch (Some pending));
  expect "past slot available"
    (Update.snapshot_slot ~epoch:(Int64.succ pending.activate_epoch) (Some pending));
  let validators = List.init 5 (fun _ -> identity ()) in
  let addresses = List.map (fun item -> item.address) validators in
  let registry =
    `Assoc [
      "standard", `String Policy.standard_name;
      "candidates", `List (List.map (fun item ->
        `Assoc [
          "address", `String item.address;
          "consensus_pubkey", `String (Base64.encode_exn item.consensus_public);
          "bond", `String (Z.to_string Policy.min_bond);
          "bonded_epoch", `String "0";
          "ready_epoch", `String "0";
          "exit_epoch", `Null;
        ]) validators);
      "slashes", `List [];
    ]
    |> Registry.of_yojson
    |> expect_ok "schedule registry"
  in
  Lwt_main.run (S.set_meta store Registry.meta_key (Registry.to_string registry));
  let fold epoch =
    X.prior_fold epoch
    |> Result.map (fun ctx -> X.{ ctx with
      mode = Octra_core.Rule_graph.Active;
      live_mode = Octra_core.Rule_graph.Active;
      standard_mode = Octra_core.Rule_graph.Active;
      plan_mode =
        if epoch >= transition then Octra_core.Rule_graph.Active
        else Octra_core.Rule_graph.Prior;
      start = 92L;
      profile_start = 92L;
    })
  in
  let backend =
    X.make_live_backend ~validator_policy:policy ~fold store (L.create store)
  in
  let read key = Lwt_main.run (S.get_meta store key) in
  let decode raw = Update.of_string raw |> expect_ok "schedule decode" in
  let rec epochs epoch pending active =
    if epoch > 128 then ()
    else
      let env = X.{
        chain_id = "set-plan-test";
        epoch_id = epoch;
        proposer_addr = List.hd addresses;
        validator_addrs = addresses;
        validator_pubkeys = List.map (fun (item : identity) ->
          item.address, item.consensus_public) validators;
        prev_state_root = "";
        epoch_ts = float_of_int epoch;
        ready_state_root_at = None;
        ready_max_lag = 0;
      } in
      let source = Int64.of_int epoch in
      let due = Option.bind pending (fun raw ->
        if (decode raw).Update.activate_epoch <= source then Some raw
        else None) in
      let promoted = Lwt_main.run (X.promote_active_validator_set ~backend ~env) in
      let active = match due with Some _ -> due | None -> active in
      Lwt_main.run (X.schedule_validator_snapshot ?active:promoted ~backend ~env ());
      let next = read Update.pending_meta_key in
      expect "promotion preserves exact bytes" (read Update.active_meta_key = active);
      let held = epoch >= transition && Option.fold ~none:false
        ~some:(fun raw -> (decode raw).Update.activate_epoch > source) pending in
      if epoch mod 4 <> 0 || held then
        expect "pending bytes retained" (next = pending)
      else begin
        let update = Option.get next |> decode in
        expect "snapshot source retained" (update.Update.source_epoch = Some source);
        expect "snapshot delay retained" (update.Update.activate_epoch = Int64.add source 8L);
        expect "member keys retained" (List.for_all (fun item ->
          List.exists (fun member ->
            member.Update.address = item.address
            && member.pubkey_b64 = Base64.encode_exn item.consensus_public)
            update.validators) validators)
      end;
      let first = if transition <= 92 then 100 else 104 in
      expect "first activation occurs on time"
        (Option.is_some active = (transition <= 100 && epoch >= first));
      epochs (epoch + 1) next active
  in
  epochs 92 None None;
  let env = X.{
    chain_id = "set-plan-test";
    epoch_id = 128;
    proposer_addr = List.hd addresses;
    validator_addrs = addresses;
    validator_pubkeys = [];
    prev_state_root = "";
    epoch_ts = 128.;
    ready_state_root_at = None;
    ready_max_lag = 0;
  } in
  if transition <= 100 then begin
    let before = read Update.pending_meta_key in
    Lwt_main.run (S.set_meta store Registry.meta_key "unreadable");
    Lwt_main.run (X.schedule_validator_snapshot ~backend ~env ());
    expect "held plan avoids registry reads" (read Update.pending_meta_key = before)
  end

let run_weight_guard store =
  let module Q = Octra_consensus.C_quorum_policy in
  let chain_id = "octra-devnet-9871-cluster" in
  let at = (Option.get (Q.seatless_activation_for_chain chain_id)).activation_epoch in
  let identities = List.init 7 (fun _ -> identity ()) in
  let parameters = {Policy.parameters with activation_delay = 1L} in
  let policy = Policy.Bonded {activation_epoch = 64; parameters;
    snapshot_interval = 1L; evidence_epochs = Policy.evidence_epochs} in
  let backend = X.make_live_backend ~validator_policy:policy ~fold:X.prior_fold store (L.create store) in
  let env epoch_id = X.{chain_id; epoch_id; proposer_addr = (List.hd identities).address;
    validator_addrs = List.map (fun (item : identity) -> item.address) identities;
    validator_pubkeys = List.map (fun (item : identity) ->
      item.address, Base64.encode_exn item.consensus_public) identities;
    prev_state_root = ""; epoch_ts = float_of_int epoch_id;
    ready_state_root_at = None; ready_max_lag = 0} in
  let update rows source activate =
    List.map2 (fun (item : identity) weight -> Octra_core.Validator_entry.{address = item.address;
      pubkey = item.consensus_public; weight = Z.mul Policy.min_bond (Z.of_int weight)}) identities rows
    |> Update.make_weighted ~source_epoch:(Int64.of_int source) ~activate_epoch:(Int64.of_int activate)
    |> expect_ok "weight update" |> Update.to_string in
  let safe = update [1; 1; 1; 1; 1; 1; 1] (at - 2) (at - 1) in
  let heavy = update [4; 1; 1; 1; 1; 1; 1] (at - 2) at in
  let voting raw = Update.of_string raw |> expect_ok "promotion source"
    |> Update.validator_set |> expect_ok "promotion voting set" in
  let read key = Lwt_main.run (S.get_meta store key) in
  let write key value = Lwt_main.run (S.set_meta store key value) in
  let registry = `Assoc ["standard", `String Policy.standard_name;
    "candidates", `List (List.mapi (fun index (item : identity) -> `Assoc [
      "address", `String item.address;
      "consensus_pubkey", `String (Base64.encode_exn item.consensus_public);
      "bond", `String (Z.to_string (Z.mul Policy.min_bond (Z.of_int (if index = 0 then 4 else 1))));
      "bonded_epoch", `String "0"; "ready_epoch", `String "0"; "exit_epoch", `Null]) identities);
    "slashes", `List []] |> Registry.of_yojson |> expect_ok "weight registry" |> Registry.to_string in
  write Registry.meta_key registry;
  List.iter (fun epoch ->
    write Update.active_meta_key safe;
    write Update.pending_meta_key safe;
    Lwt_main.run (X.schedule_validator_snapshot ~backend ~env:(env epoch) ());
    expect "snapshot changed registry" (read Registry.meta_key = Some registry);
    expect "snapshot changed active set" (read Update.active_meta_key = Some safe);
    if epoch < at then expect "historical snapshot refused"
      (read Update.pending_meta_key = Some (update [4; 1; 1; 1; 1; 1; 1] epoch (epoch + 1)))
    else expect "heavy snapshot published" (read Update.pending_meta_key = Some safe))
    [at - 1; at; at + 1];
  List.iter (fun epoch ->
    write Update.active_meta_key safe;
    let pending = if epoch < at then update [4; 1; 1; 1; 1; 1; 1] (at - 2) (at - 1) else heavy in
    write Update.pending_meta_key pending;
    let backend = {backend with X.voting_set = Some (epoch, voting safe)} in
    ignore (Lwt_main.run (X.promote_active_validator_set ~backend ~env:(env epoch)));
    expect "promotion pending state differs"
      (read Update.pending_meta_key = if epoch < at then Some pending else None);
    expect "promotion changed wrong active set"
      (read Update.active_meta_key = Some (if epoch < at then pending else safe))) [at - 1; at; at + 1];
  write Update.active_meta_key heavy;
  write Update.pending_meta_key (update [1; 1; 1; 1; 1; 1; 1] (at - 1) at);
  let backend = {backend with X.voting_set = Some (at, voting heavy)} in
  ignore (Lwt_main.run (X.promote_active_validator_set ~backend ~env:(env at)));
  expect "valid replacement refused" (read Update.active_meta_key = read Update.pending_meta_key)

let check_pending_choice () =
  let module C = Octra_consensus.C_types in
  let module Q = Octra_consensus.C_quorum_policy in
  let chain_id = "pending-choice" in
  let policy = Q.make ~count_epoch:0L ~stake_epoch:(Some 20L) ~round_epoch:None
    |> fun value -> Q.with_seatless_epoch value (Some 30L) in
  let members = List.init 7 (fun index -> C.{address = "member" ^ string_of_int index;
    pubkey = String.make 32 (Char.chr (index + 1))}) in
  let set first = List.mapi (fun index member -> member, Z.of_int (if index = 0 then first else 1)) members
    |> C.make_weighted_validator_set |> expect_ok "pending choice set" in
  let safe = set 1 and heavy = set 4 and next = set 2 in
  let validate epoch set = C.validate_validator_set_for_epoch ~policy ~chain_id ~epoch_id:epoch set in
  let choose epoch active pending = C.pending_set_for_epoch ~policy ~chain_id ~epoch_id:epoch ~active pending in
  let shape = function Error (C.Set_shape _) -> true | _ -> false in
  let malformed = {heavy with C.n = heavy.n + 1} in
  let author = List.hd members, Z.one in
  let bad_authors = {heavy with C.authors = Some (30L, [author])} in
  expect "pending equality cap differs" (validate 30L heavy = Error C.Seat_cap);
  expect "pending shape hidden by cap" (shape (validate 30L malformed));
  expect "pending authors hidden by cap" (shape (validate 30L bad_authors));
  List.iter (fun epoch ->
    expect "pending cap retained invalid set"
      (choose epoch (Some safe) heavy = Ok (C.Active_set safe));
    expect "pending valid replacement refused"
      (choose epoch (Some heavy) next = Ok (C.Pending_set next));
    expect "pending cap lost trusted active"
      (choose epoch (Some heavy) heavy = Ok (C.Active_set heavy));
    expect "pending missing active accepted" (shape (choose epoch None heavy));
    expect "pending malformed plan ignored" (shape (choose epoch (Some safe) malformed));
    expect "pending malformed active ignored" (shape (choose epoch (Some malformed) next))) [30L; 31L];
  expect "pending malformed authors ignored" (shape (choose 30L (Some safe) bad_authors));
  expect "pending historical replacement changed"
    (choose 29L (Some safe) heavy = Ok (C.Pending_set heavy));
  expect "pending historical shape changed"
    (validate 29L malformed = Ok (C.validator_set_for_epoch ~policy ~chain_id ~epoch_id:29L malformed));
  expect "pending historical authors changed"
    (validate 29L bad_authors = Ok (C.validator_set_for_epoch ~policy ~chain_id ~epoch_id:29L bad_authors));
  expect "pending legacy adapter differs"
    (C.checked_validator_set_for_epoch ~policy ~chain_id ~epoch_id:30L heavy
      = Result.map_error C.set_error_message (validate 30L heavy))

let eviction_inputs () =
  let module C = Octra_consensus.C_types in
  let members = List.mapi (fun index weight ->
    let pubkey = String.make 32 (Char.chr (60 + index)) in
    let address = Base64.encode_exn pubkey |> Octra_core.Crypto.Address.address_from_pubkey in
    C.{address; pubkey}, Z.of_int weight) [3; 3; 2; 1; 1] in
  let current = C.make_weighted_validator_set members |> expect_ok "eviction old set" in
  let offender = (fst (List.nth members 4)).C.address in
  current, offender

let check_eviction_update () =
  let module C = Octra_consensus.C_types in
  let module Q = Octra_consensus.C_quorum_policy in
  let chain_id = "octra-devnet-9871-cluster" in
  let source_epoch = Int64.of_int
    (Option.get (Q.seatless_activation_for_chain chain_id)).activation_epoch in
  let activate_epoch = Int64.succ source_epoch in
  let current, offender = eviction_inputs () in
  let update, weight = Update.make_eviction ~source_epoch ~activate_epoch
    ~offenders:[offender] current |> expect_ok "eviction update" in
  let next = Update.validator_set update |> expect_ok "eviction next set" in
  expect "eviction update old weight differs" (Z.equal weight Z.one);
  expect "eviction update source differs" (update.source_epoch = Some source_epoch);
  expect "eviction update activation differs" (update.activate_epoch = activate_epoch);
  expect "eviction update changed retained entries"
    (C.weights_of_set next = List.filter (fun (member, _) -> member.C.address <> offender)
      (C.weights_of_set current));
  expect "eviction update is not exact"
    (Update.check_eviction ~source_epoch ~offenders:[offender] ~current update = Ok (next, weight));
  expect "eviction update bytes differ" (Update.of_string (Update.to_string update) = Ok update);
  expect "exact eviction shape refused"
    (Update.check_set_at ~chain_id ~epoch_id:activate_epoch update = Ok ());
  expect "ordinary update bypassed strict entry cap"
    (Result.is_error (Update.check_set_at ~chain_id ~epoch_id:activate_epoch
      {update with eviction = None}))

let check_eviction_update_faults () =
  let module C = Octra_consensus.C_types in
  let current, offender = eviction_inputs () in
  let update, _ = Update.make_eviction ~source_epoch:30L ~activate_epoch:31L
    ~offenders:[offender] current |> expect_ok "eviction update faults" in
  let first = List.hd update.validators in
  let changed validators = {update with Update.validators;
    fingerprint = Update.weighted_fingerprint ~source_epoch:30L ~activate_epoch:31L validators} in
  let removed = List.find (fun member -> member.C.address = offender) current.validators in
  let removed = Update.{address = removed.address; pubkey_b64 = Base64.encode_exn removed.pubkey;
    weight = List.assoc offender current.weights} in
  List.iter (fun (name, value) -> expect ("invalid eviction update accepted: " ^ name)
    (Result.is_error (Update.check_eviction ~source_epoch:30L ~offenders:[offender] ~current value)))
    ["undeclared", {update with weighted = false};
     "missing source", {update with source_epoch = None};
     "other source", {update with source_epoch = Some 29L};
     "same epoch", {update with activate_epoch = 30L};
     "delayed epoch", {update with activate_epoch = 32L};
     "fingerprint", {update with fingerprint = String.make 64 '0'};
     "ordering", changed (List.rev update.validators);
     "extra exit", changed (List.tl update.validators);
     "duplicate", changed (first :: update.validators);
     "offender retained", changed (removed :: update.validators);
     "weight", changed ({first with weight = Z.succ first.weight} :: List.tl update.validators);
     "key", changed ({first with pubkey_b64 = Base64.encode_exn (String.make 32 'z')}
       :: List.tl update.validators)];
  expect "eviction update accepted another offender"
    (Result.is_error (Update.check_eviction ~source_epoch:30L
      ~offenders:[first.address] ~current update))

let check_eviction_epochs () =
  let current, offender = eviction_inputs () in
  List.iter (fun (source_epoch, activate_epoch) ->
    expect "eviction accepted invalid epoch step"
      (Result.is_error (Update.make_eviction ~source_epoch ~activate_epoch
        ~offenders:[offender] current)))
    [-1L, 0L; 30L, 30L; 30L, 29L; 30L, 32L; Int64.max_int, Int64.min_int];
  List.iter (fun source_epoch ->
    let activate_epoch = Int64.succ source_epoch in
    expect "eviction refused valid epoch step"
      (Result.is_ok (Update.make_eviction ~source_epoch ~activate_epoch
        ~offenders:[offender] current))) [0L; 30L; Int64.pred Int64.max_int]

let eviction_registry ?(bonded_epoch = 0L) members events =
  let module C = Octra_consensus.C_types in
  `Assoc ["standard", `String Policy.standard_name;
    "candidates", `List (List.map (fun (member, weight) -> `Assoc [
      "address", `String member.C.address;
      "consensus_pubkey", `String (Base64.encode_exn member.pubkey);
      "bond", `String (Z.to_string (Z.mul Policy.min_bond weight));
      "bonded_epoch", `String (Int64.to_string bonded_epoch);
      "ready_epoch", `String (Int64.to_string bonded_epoch); "exit_epoch", `Null;
    ]) members);
    "slashes", `List (List.mapi (fun index (offender, applied) -> `Assoc [
      "id", `String (Base64.encode_exn (String.make 32 (Char.chr (index + 1))));
      "offender", `String offender; "evidence_epoch", `String (Int64.to_string (Int64.pred applied));
      "applied_epoch", `String (Int64.to_string applied);
      "amount", `String (Z.to_string Policy.min_bond);
    ]) events)] |> Registry.of_yojson |> expect_ok "eviction registry"

let check_eviction_registry () =
  let module C = Octra_consensus.C_types in
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch = (Option.get
    (Octra_core.Rule_graph.seatless_activation_for_chain chain_id)).activation_epoch in
  let current, offender = eviction_inputs () in
  let members = C.weights_of_set current in
  let absent = List.filter (fun (member, _) -> member.C.address <> offender) members in
  let choose epoch registry = Update.eviction_offenders ~chain_id ~epoch ~registry ~current in
  let fresh = eviction_registry absent [offender, Int64.of_int epoch] in
  expect "eviction fresh record ignored" (choose epoch fresh = Ok [offender]);
  expect "eviction historical epoch changed" (choose (epoch - 1) fresh = Ok []);
  expect "eviction future record applied"
    (choose epoch (eviction_registry absent [offender, Int64.of_int (epoch + 1)]) = Ok []);
  let prior = [offender, Int64.of_int (epoch - 1)] in
  expect "eviction activation lost historical slash"
    (choose epoch (eviction_registry absent prior) = Ok [offender]);
  expect "eviction lost unresolved historical slash"
    (choose (epoch + 1) (eviction_registry absent prior) = Ok [offender]);
  expect "eviction retained historical rebond"
    (choose epoch (eviction_registry ~bonded_epoch:(Int64.of_int epoch) members prior) = Ok [offender]);
  let reward = X.{proposer_addr = offender; proposer_public_key = None;
    validators = List.map (fun (member, weight) ->
      {address = member.C.address; public_key = None; weight}) members} in
  let old = Registry.to_string (eviction_registry ~bonded_epoch:(Int64.of_int epoch)
    members [offender, Int64.of_int (epoch - 5)]) in
  List.iter (fun (at, expected) ->
    expect "activation reward exclusion differs"
      (X.reward_exclusions ~chain_id ~epoch_id:at ~registry:(Some old) ~reward () = Ok expected))
    [epoch - 1, []; epoch, [offender]; epoch + 1, [offender]; epoch + 2, [offender]];
  expect "eviction inferred evidence from absence" (choose epoch (eviction_registry absent []) = Ok []);
  let pending = C.make_weighted_validator_set (List.filter
    (fun (member, _) -> member.C.address = offender || Z.equal (List.assoc member.address current.weights)
      (Z.of_int 3)) members) |> expect_ok "eviction pending set" in
  expect "eviction pending-only offender ignored"
    (Update.eviction_offenders ~chain_id ~epoch ~registry:fresh ~current:pending = Ok [offender])

let check_eviction_state () =
  let module C = Octra_consensus.C_types in
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch = (Option.get
    (Octra_core.Rule_graph.seatless_activation_for_chain chain_id)).activation_epoch in
  let current, offender = eviction_inputs () in
  let members = C.weights_of_set current in
  let offenders = List.filter_map (fun (member, weight) ->
    if Z.equal weight Z.one then Some member.C.address else None) members in
  let registry = eviction_registry [] (List.map (fun address -> address, Int64.of_int epoch) offenders) in
  let make offenders = Update.make_eviction ~source_epoch:(Int64.of_int epoch)
    ~activate_epoch:(Int64.of_int (epoch + 1)) ~offenders current |> expect_ok "eviction state update" |> fst in
  let update = make offenders in
  let check current registry update = Update.check_eviction_state ~chain_id ~epoch ~registry ~current update in
  expect "eviction exact registry refused" (Result.is_ok (check current registry update));
  expect "eviction incomplete registry accepted"
    (Result.is_error (check current registry (make [offender])));
  expect "eviction self-asserted witness authorized"
    (Result.is_error (check current Registry.empty update));
  expect "eviction malformed source accepted"
    (Result.is_error (check {current with C.n = current.n + 1} registry update));
  let first, weight = List.hd members in
  let changed = C.make_weighted_validator_set ((first, Z.succ weight) :: List.tl members)
    |> expect_ok "eviction changed voting weights" in
  expect "eviction different old weights accepted" (Result.is_error (check changed registry update));
  expect "eviction authors entered voting identity"
    (C.same_voting_set current {current with authors = Some (0L, [])});
  let plain = {update with Update.eviction = None} in
  expect "ordinary update serialization gained witness"
    (not (List.mem_assoc "eviction" (match Update.to_yojson plain with `Assoc fields -> fields | _ -> [])));
  expect "ordinary update bytes changed" (Update.of_string (Update.to_string plain) = Ok plain)

let check_eviction_codec () =
  let current, offender = eviction_inputs () in
  let update = Update.make_eviction ~source_epoch:30L ~activate_epoch:31L ~offenders:[offender] current
    |> expect_ok "eviction codec update" |> fst in
  let fields = match Update.to_yojson update with `Assoc fields -> fields | _ -> assert false in
  let witness = match List.assoc "eviction" fields with `Assoc fields -> fields | _ -> assert false in
  let altered name value = `Assoc ((name, value) :: List.remove_assoc name witness) in
  List.iter (fun value -> expect "invalid eviction codec accepted"
    (Result.is_error (Update.of_yojson (`Assoc (("eviction", value) :: List.remove_assoc "eviction" fields)))))
    [`Null; altered "epoch" (`String "030"); altered "epoch" (`String "-1");
     altered "offenders" (`List []); altered "offenders" (`List [`String offender; `String offender]);
     altered "source" (`String "AA==");
     altered "source" (`String (String.make (4 * ((Update.eviction_source_limit + 2) / 3) + 1) 'A'));
     `Assoc (("extra", `Null) :: witness); `Assoc (("epoch", `String "30") :: witness)];
  expect "duplicate eviction field accepted"
    (Result.is_error (Update.of_yojson (`Assoc (("eviction", `Assoc witness) :: fields))));
  expect "eviction non-exact string accepted"
    (Result.is_error (Update.of_string (" " ^ Update.to_string update)));
  expect "eviction missing fingerprint accepted"
    (Result.is_error (Update.of_yojson (`Assoc (List.remove_assoc "fingerprint" fields))));
  expect "eviction witness was not preserved" (Update.of_string (Update.to_string update) = Ok update)

let run_pending_guard ?(at_start = false) ~expect_rejection store =
  let module Q = Octra_consensus.C_quorum_policy in
  let chain_id = "octra-devnet-9871-cluster" in
  let at = (Option.get (Q.seatless_activation_for_chain chain_id)).activation_epoch in
  let source_for phase = at - 1 - ((at - 1 - phase) mod 4 + 4) mod 4 in
  List.iter (fun phase ->
    let source = source_for phase in
    expect "pending source phase differs" ((source - phase) mod 4 = 0);
    expect "pending source misses activation" (source < at && source + 8 >= at))
    [0; 1; 2; 3];
  let activation = 1_266_000 in
  let source = if at_start then at - 8 else source_for activation in
  let due = source + 8 in
  if at_start then expect "pending release phase differs" (due = 1_842_000 && due mod 4 = 0);
  let identities = List.init 7 (fun _ -> identity ()) in
  let addresses = List.map (fun (item : identity) -> item.address) identities in
  let weights rows = List.map (fun value -> Z.mul Policy.min_bond (Z.of_int value)) rows in
  let safe = weights [1; 1; 1; 1; 1; 1; 1] in
  let heavy = weights [4; 1; 1; 1; 1; 1; 1] in
  let repaired = weights [4; 2; 2; 2; 2; 2; 2] in
  let update source activate rows =
    List.map2 (fun (item : identity) weight -> Octra_core.Validator_entry.{
      address = item.address; pubkey = item.consensus_public; weight}) identities rows
    |> Update.make_weighted ~source_epoch:(Int64.of_int source)
      ~activate_epoch:(Int64.of_int activate)
    |> expect_ok "pending update" |> Update.to_string in
  let registry rows =
    `Assoc ["standard", `String Policy.standard_name;
      "candidates", `List (List.map2 (fun (item : identity) bond -> `Assoc [
        "address", `String item.address;
        "consensus_pubkey", `String (Base64.encode_exn item.consensus_public);
        "bond", `String (Z.to_string bond);
        "bonded_epoch", `String "0";
        "ready_epoch", `String "0";
        "exit_epoch", `Null]) identities rows);
      "slashes", `List []]
    |> Yojson.Safe.to_string |> Registry.of_string
    |> expect_ok "pending registry" |> Registry.to_string in
  let read key = Lwt_main.run (S.get_meta store key) in
  let write key value = Lwt_main.run (S.set_meta store key value) in
  let active = update (source - 16) (source - 8) safe in
  let blocked = update source due heavy in
  let next = update due (due + 8) repaired in
  let parameters = {Policy.parameters with activation_delay = 8L} in
  let policy = Policy.Bonded {activation_epoch = activation; parameters;
    snapshot_interval = 4L; evidence_epochs = Policy.evidence_epochs} in
  let current = Update.of_string active |> expect_ok "pending active update"
    |> Update.validator_set |> expect_ok "pending active set" in
  let fold epoch = X.prior_fold epoch |> Result.map (fun ctx -> X.{ctx with
    standard_mode = Octra_core.Rule_graph.Active;
    plan_mode = Octra_core.Rule_graph.Active}) in
  let env epoch_id = X.{chain_id; epoch_id; proposer_addr = List.hd addresses;
    validator_addrs = List.map (fun item -> item.Octra_consensus.C_types.address) current.validators;
    validator_pubkeys = List.map (fun item -> item.Octra_consensus.C_types.address,
      Base64.encode_exn item.Octra_consensus.C_types.pubkey) current.validators; prev_state_root = "";
    epoch_ts = float_of_int epoch_id; ready_state_root_at = None; ready_max_lag = 0} in
  let ledger = L.create store in
  let backend ?(epoch = due) store ledger =
    let value = X.make_live_backend ~validator_policy:policy ~fold
      ~emission_policy:E.Guard ~emission_schedule:Octra_core.Emission_schedule.Inactive store ledger in
    {value with X.voting_set = Some (epoch, current)} in
  let execute store ledger = X.run_core ~reward:None ~preverify:None
    ~backend:(backend store ledger) ~env:(env due) ~txs:[]
    ~process_tx:(X.confirmed_process X.process_standard_tx) in
  write "emission_remaining" "0";
  write "total_supply" "16000000";
  L.add_account ledger Registry.escrow_address (Z.of_int 16_000_000)
    |> expect_ok "pending escrow";
  Lwt_main.run (L.flush_dirty_lwt ledger);
  write Update.active_meta_key active;
  write Update.pending_meta_key active;
  write Registry.meta_key (registry heavy);
  Lwt_main.run (X.schedule_validator_snapshot ~backend:(backend ~epoch:source store ledger) ~env:(env source) ());
  expect "historical pending changed" (read Update.pending_meta_key = Some blocked);
  write Registry.meta_key (registry repaired);
  let before = Lwt_main.run (S.state_hash store) in
  let preview = Lwt_main.run (Octra_core.State_preview.with_state
    ~base_store:store ~base_ledger:ledger ~epoch_id:due ~proposal_id:"pending-guard"
    (fun store ledger -> execute store ledger)) in
  expect "pending preview changed state" (Lwt_main.run (S.state_hash store) = before);
  let applied = Lwt_main.run (execute store ledger) in
  expect "pending preview differs" (preview = applied);
  expect "pending replaced active" (read Update.active_meta_key = Some active);
  expect "pending changed repaired registry" (read Registry.meta_key = Some (registry repaired));
  if expect_rejection then begin
    expect "pending wedge not reproduced" (Result.is_error applied);
    expect "pending rejection changed bytes" (read Update.pending_meta_key = Some blocked);
    expect "pending rejection changed root" (Lwt_main.run (S.state_hash store) = before)
  end else begin
    ignore (expect_ok "pending progress" applied);
    expect "pending fresh snapshot differs" (read Update.pending_meta_key = Some next)
  end;
  write Update.pending_meta_key active;
  ignore (Lwt_main.run (execute store ledger) |> expect_ok "pending valid control");
  expect "pending control snapshot differs" (read Update.pending_meta_key = Some next);
  expect "pending control changed active" (read Update.active_meta_key = Some active)

let run_signed_set store =
  let module Plan = struct
    type action = Bond of int * Z.t | Ready of int | Exit of int | Pay of int * int * Z.t
    type state = {
      accounts : (Z.t * int) list;
      escrow : Z.t;
      supply : Z.t;
      remaining : Z.t;
      retired : Z.t;
    }

    let unit = Z.of_int 1_000_000
    let funds = Z.of_int 100_000_000
    let fee = Z.of_int 1000
    let emission = Z.of_int 1700
    let reserve = Z.of_string "47304000000"
    let maximum = Z.of_string "1000000000000000"
    let initial =
      let supply = Z.mul funds (Z.of_int 7) in
      let remaining = Z.sub reserve (Z.of_int 1699) in
      {accounts = List.init 7 (fun _ -> funds, 0); escrow = Z.zero;
       supply; remaining; retired = Z.sub maximum (Z.add supply remaining)}

    let checkpoints = [10; 11; 12; 50; 51; 52; 74; 75; 76]

    let start_at ~seatless ~policy_activation =
      let floor = max (seatless + 16) policy_activation in
      let phase = (floor - policy_activation) mod 4 in
      floor + ((5 - phase) mod 4)

    let check_start () =
      List.iter (fun policy_activation ->
        List.iteri (fun phase shift ->
          let seatless = policy_activation + phase in
          let floor = seatless + 16 in
          let start = start_at ~seatless ~policy_activation in
          expect "signed start phase differs" ((start - policy_activation) mod 4 = 1);
          expect "signed start is not the first matching epoch" (start = floor + shift);
          expect "signed start moved outside cadence" (start >= floor && start - floor < 4))
          [1; 0; 3; 2];
        expect "signed start precedes validator policy"
          (start_at ~seatless:0 ~policy_activation = policy_activation + 1))
        [100; 101; 102; 103]

    let weights offset =
      if offset < 12 then [1; 1; 1; 1; 1; 1; 1]
      else if offset < 52 then [1; 2; 3; 4; 5; 6; 7]
      else if offset < 76 then [2; 2; 3; 4; 5; 6; 7]
      else [2; 2; 3; 4; 5; 6; 0]

    let registry_weights source =
      if source < 40 then [1; 2; 3; 4; 5; 6; 7]
      else if source < 64 then [2; 2; 3; 4; 5; 6; 7]
      else [2; 2; 3; 4; 5; 6; 0]

    let actions offset =
      if offset = 0 then List.init 7 (fun index -> Bond (index, Z.mul unit (Z.of_int (index + 1))))
      else if offset = 1 then List.init 4 (fun index -> Ready (index + 3))
      else if offset = 2 then List.init 3 (fun index -> Ready index)
      else if offset = 40 then [Bond (0, unit)]
      else if offset = 64 then [Exit 6]
      else if List.mem offset checkpoints then [Pay (1, 2, Z.one)]
      else []

    let fees offset = Z.mul fee (Z.of_int (List.length (actions offset)))

    let credits offset fees =
      let burned = Z.div fees (Z.of_int 5) in
      let reward = Z.sub (Z.add emission fees) burned in
      let proposer = Z.div (Z.mul reward (Z.of_int 7)) (Z.of_int 10) in
      let pool = Z.sub reward proposer in
      let weights = List.map Z.of_int (weights offset) in
      let total = List.fold_left Z.add Z.zero weights in
      let shares = List.map (fun weight -> Z.div (Z.mul pool weight) total) weights in
      let dust = Z.sub pool (List.fold_left Z.add Z.zero shares) in
      List.mapi (fun index share -> if index = 0 then Z.add share (Z.add proposer dust) else share) shares

    let change index amount nonce accounts =
      List.mapi (fun at (balance, count) ->
        if at = index then Z.add balance amount, count + nonce else balance, count) accounts

    let apply state = function
      | Bond (index, amount) ->
        {state with accounts = change index (Z.neg (Z.add amount fee)) 1 state.accounts;
          escrow = Z.add state.escrow amount}
      | Ready index | Exit index ->
        {state with accounts = change index (Z.neg fee) 1 state.accounts}
      | Pay (sender, recipient, amount) ->
        let accounts = change sender (Z.neg (Z.add amount fee)) 1 state.accounts in
        {state with accounts = change recipient amount 0 accounts}

    let advance offset state =
      let state = List.fold_left apply state (actions offset) in
      let fees = fees offset in
      let burned = Z.div fees (Z.of_int 5) in
      {state with
        accounts = List.map2 (fun (balance, nonce) credit -> Z.add balance credit, nonce)
          state.accounts (credits offset fees);
        supply = Z.sub (Z.add state.supply emission) burned;
        remaining = Z.sub state.remaining emission;
        retired = Z.add state.retired burned}

    let metadata start count state = [
      "total_supply", Z.to_string state.supply;
      "emission_standard", "security_curve";
      "emission_activation_epoch", string_of_int (start - 1);
      "emission_initial", Z.to_string reserve;
      "emission_remaining", Z.to_string state.remaining;
      "supply_retired", Z.to_string state.retired;
      "last_epoch", string_of_int (start + count - 1);
      "current_epoch", string_of_int (start + count);
    ]

    let check_curve () =
      let duration = Z.of_int 47_304_000 in
      expect "signed reserve scale differs" (Z.equal reserve (Z.mul (Z.of_int 1000) duration));
      List.iter (fun elapsed ->
        let k = Z.of_int elapsed in
        let correction = Z.mul (Z.of_int 700) (Z.mul k k) in
        expect "signed emission interval differs" (Z.sign correction > 0 && Z.lt correction duration);
        let numerator = Z.mul reserve
          (Z.sub (Z.mul (Z.of_int 17) (Z.mul duration k)) (Z.mul (Z.of_int 7) (Z.mul k k))) in
        let issued = Z.div numerator (Z.mul (Z.of_int 10) (Z.mul duration duration)) in
        expect "signed cumulative emission differs" (Z.equal issued (Z.pred (Z.mul emission k))))
        (List.init 98 (fun index -> index + 1))

    let check_shares () =
      List.iter (fun (offset, amounts) ->
        expect "signed reward arithmetic differs" (credits offset fee = List.map Z.of_int amounts)) [
        10, [1858; 107; 107; 107; 107; 107; 107];
        12, [1780; 53; 80; 107; 133; 160; 187];
        52, [1804; 51; 77; 103; 129; 155; 181];
        76, [1820; 68; 102; 136; 170; 204; 0];
      ];
      expect "signed first cohort fees differ"
        (credits 1 (fees 1) = List.map Z.of_int [3640; 210; 210; 210; 210; 210; 210]);
      expect "signed second cohort fees differ"
        (credits 2 (fees 2) = List.map Z.of_int [3050; 175; 175; 175; 175; 175; 175])
  end in
  let module C = Octra_consensus.C_types in
  let module H = Octra_consensus.C_hash in
  let module G = Octra_core.Rule_graph in
  let module N = Octra_node_runtime in
  let module Sig = Octra_core.Tx_signature in
  let chain_id = "octra-devnet-9871-cluster" in
  let policy_activation = 1_266_000 in
  let seatless = (Option.get (Octra_consensus.C_quorum_policy.seatless_activation_for_chain chain_id)).activation_epoch in
  let start = Plan.start_at ~seatless ~policy_activation in
  Plan.check_start ();
  Plan.check_curve ();
  Plan.check_shares ();
  expect "signed snapshot cadence changed" ((start - policy_activation) mod 4 = 1);
  expect "signed bond unit changed" (Z.equal Policy.min_bond Plan.unit);
  expect "signed supply maximum changed" (Z.equal Octra_core.Denomination.max_supply Plan.maximum);
  let config = String.make 64 'c' in
  let rules = G.create_ready ~chain_id ~ready_config_hash:config ~root_at:(fun epoch ->
    match G.root_after_floor ~chain_id ~floor_epoch:start ~epoch with
    | Some root -> G.Root root | None -> G.Missing) in
  let validators = List.init 7 (fun _ -> identity ()) in
  let first = List.hd validators in
  let ledger = L.create store in
  List.iter (fun item -> expect_ok "signed account" (L.add_account_with_pubkey ledger
    item.address Plan.funds (Base64.encode_exn item.consensus_public))) validators;
  Lwt_main.run (L.flush_dirty_lwt ledger);
  expect "signed initial supply differs" (Z.equal (L.get_total_supply ledger) Plan.initial.supply);
  List.iter (fun (key, value) -> Lwt_main.run (S.set_meta store key value))
    (Plan.metadata start 0 Plan.initial);
  let policy = Policy.of_env_exn (fun name ->
    if name = Policy.env_name then Some (string_of_int policy_activation) else None) in
  let read store key = Lwt_main.run (S.get_meta store key) in
  let members entries = C.make_weighted_validator_set (List.map (fun ((item : identity), weight) ->
    C.{address = item.address; pubkey = item.consensus_public}, weight) entries)
    |> expect_ok "signed members" in
  let initial = members (List.map (fun item -> item, Policy.min_bond) validators) in
  let rows weights = List.map2 (fun item weight -> item.address, Z.mul Plan.unit (Z.of_int weight)) validators weights
    |> List.filter (fun (_, weight) -> Z.sign weight > 0)
    |> List.sort (fun (left, _) (right, _) -> String.compare left right) in
  let chosen epoch current = match read store Update.active_meta_key with
    | None -> current
    | Some raw ->
      let update = expect_ok "signed plan" (Update.of_string raw) in
      if update.activate_epoch > Int64.of_int epoch then current else
      members (List.map (fun item ->
        List.find (fun key -> key.address = item.Update.address) validators,
        item.weight) update.validators) in
  let verify = N.Consensus_proposal.{
    public_key_for_tx = (fun tx -> tx.T.public_key);
    verify_address_pubkey = (fun ~addr ~pubkey -> Octra_core.Crypto.Address.verify_address_pubkey addr pubkey);
    verify_tx_signature = (fun ~signing tx ~pubkey -> Sig.verify signing tx pubkey);
  } in
  let snapshot store ledger =
    let open Lwt.Syntax in
    let* root = S.state_hash store in
    let* registry = S.get_meta store Registry.meta_key in
    let* pending = S.get_meta store Update.pending_meta_key in
    let* active = S.get_meta store Update.active_meta_key in
    let* metadata = Lwt_list.map_s (fun (key, _) ->
      let* value = S.get_meta store key in
      Lwt.return (key, value)) (Plan.metadata start 0 Plan.initial) in
    Lwt.return (root, registry, pending, active,
      List.map (fun item -> let account = L.find ledger item.address in account.L.balance, account.nonce) validators,
      Option.map (fun account -> account.L.balance) (L.find_opt ledger Registry.escrow_address),
      metadata, L.get_total_supply ledger) in
  let check_money label count planned (_, _, _, _, accounts, escrow, metadata, supply) =
    List.iteri (fun index (actual, expected) ->
      expect (Printf.sprintf "signed %s account differs at %d index %d" label count index) (actual = expected))
      (List.combine accounts planned.Plan.accounts);
    expect ("signed " ^ label ^ " escrow differs")
      (escrow = if count = 0 then None else Some planned.escrow);
    expect ("signed " ^ label ^ " metadata differs")
      (metadata = List.map (fun (key, value) -> key, Some value) (Plan.metadata start count planned));
    expect ("signed " ^ label ^ " public supply differs") (Z.equal supply planned.supply);
    expect "signed planned supply differs"
      (Z.equal planned.supply (List.fold_left (fun total (balance, _) -> Z.add total balance)
        planned.escrow planned.accounts));
    expect "signed planned supply sum differs"
      (Z.equal Plan.maximum (Z.add planned.supply (Z.add planned.remaining planned.retired))) in
  let check_sets offset (_, _, pending, active, _, _, _, _) =
    List.iter (fun (label, raw, first) ->
      if offset < first then expect ("signed early " ^ label) (raw = None)
      else
        let source = 3 + 8 * ((offset - first) / 8) in
        let update = Option.get raw |> Update.of_string |> expect_ok ("signed " ^ label) in
        expect ("signed " ^ label ^ " epochs differ")
          (update.source_epoch = Some (Int64.of_int (start + source))
            && update.activate_epoch = Int64.of_int (start + source + 8));
        expect ("signed " ^ label ^ " weights differ")
          (update.weighted && List.map (fun item -> item.Update.address, item.weight) update.validators
            = rows (Plan.registry_weights source)))
      ["pending", pending, 3; "active", active, 11] in
  let signers_for offset current =
    let voting = C.weights_of_set current |> List.map (fun (item, weight) -> item.C.address, weight) in
    expect "signed parent weights differ" (voting = rows (Plan.weights offset));
    let omitted = fst (List.nth voting (offset mod List.length voting)) in
    let signers = List.filter (fun (member : C.validator_info) -> member.address <> omitted) current.validators in
    let signed_rows = List.filter (fun (address, _) -> address <> omitted) voting in
    let total = List.fold_left (fun total (_, weight) -> Z.add total weight) Z.zero voting in
    let signed_weight = List.fold_left (fun total (_, weight) -> Z.add total weight) Z.zero signed_rows in
    let quota = Z.succ (Z.div (Z.mul total (Z.of_int 2)) (Z.of_int 3)) in
    expect "signed parent is not a proper quorum subset"
      (List.length signers = List.length voting - 1 && Z.geq signed_weight quota);
    omitted, signers in
  let check_parent (commit : C.parent_commit) =
    let precommits = commit.certificate.precommits in
    let vote = List.hd precommits in
    let altered = {vote with C.signature = String.make 64 '\000'} :: List.tl precommits in
    expect "signed invalid parent signature accepted" (Result.is_error (Fold.read_parent ~chain_id
      {commit with certificate = {commit.certificate with precommits = altered}}));
    expect "signed parent below quorum accepted" (Result.is_error (Fold.read_parent ~chain_id
      {commit with certificate = {commit.certificate with precommits = [vote]}})) in
  let make_parent offset current previous root =
    let omitted, signers = signers_for offset current in
    let old = parent chain_id (Int64.of_int (start + offset - 1)) validators in
    let header = {old.certificate.header with C.proposed_state_root =
      N.Consensus_epoch_apply_guard.raw32_of_pre_root root;
      parent_commit_hash = previous} in
    let proposal_id = H.proposal_id header in
    let precommits = List.map (fun (member : C.validator_info) ->
      let key = List.find (fun item -> item.address = member.address) validators in
      let vote = C.{chain_id; epoch_id = header.epoch_id; round = 0; vote_type = Precommit;
        proposal_id; validator = member.address; signature = ""} in
      {vote with signature = Mirage_crypto_ec.Ed25519.sign ~key:key.consensus_private (H.vote_sign_bytes vote)}) signers in
    let commit = C.{validator_set = current; certificate = {old.certificate with header; proposal_id; precommits}} in
    ignore (Fold.read_parent ~chain_id commit |> expect_ok "signed parent");
    if offset = 0 then check_parent commit;
    commit, omitted in
  let sign_tx epoch item nonce op_type target amount message =
    let mode = Sig.at ~chain_id ~epoch:(Int64.of_int epoch) in
    let tx = transaction ~ou:Plan.fee ~from:item.address
      ~public_key:(Base64.encode_exn item.consensus_public) ~to_:target ~amount ~nonce ~message ~op_type in
    Sig.sign mode tx (Base64.encode_exn (Mirage_crypto_ec.Ed25519.priv_to_octets item.consensus_private)) in
  let bond_tx epoch item nonce amount =
    sign_tx epoch item nonce T.ValidatorBond Registry.escrow_address amount
      (bond_message ~chain_id ~amount ~nonce item) in
  let ready_tx epoch root (commit : C.parent_commit) item nonce =
    let header = commit.certificate.header in
    sign_tx epoch item nonce T.ValidatorReady item.address Z.zero
      (Yojson.Safe.to_string (`Assoc [
        "consensus_pubkey", `String (Base64.encode_exn item.consensus_public);
        "head_epoch", `String (Int64.to_string header.epoch_id);
        "head_proposal_id", `String (proposal_hex commit);
        "state_root", `String root; "chain_id", `String chain_id;
        "config_hash", `String config;
        "catchup_head_epoch", `String (Int64.to_string header.epoch_id)])) in
  let make_inputs offset (planned : Plan.state) root commit =
    let epoch = start + offset in
    let nonces = List.map2 (fun item (_, nonce) -> item.address, nonce + 1) validators planned.accounts in
    List.map (function
      | Plan.Bond (index, amount) ->
        let item = List.nth validators index in
        bond_tx epoch item (List.assoc item.address nonces) amount
      | Plan.Ready index ->
        let item = List.nth validators index in
        ready_tx epoch root commit item (List.assoc item.address nonces)
      | Plan.Exit index ->
        let item = List.nth validators index in
        sign_tx epoch item (List.assoc item.address nonces) T.ValidatorExit item.address Z.zero "{}"
      | Plan.Pay (sender, recipient, amount) ->
        let item = List.nth validators sender in
        sign_tx epoch item (List.assoc item.address nonces) T.Standard (List.nth validators recipient).address amount "{}")
      (Plan.actions offset)
    |> T.consensus_order in
  let checked_inputs epoch (commit : C.parent_commit) txs =
    N.Consensus_proposal.verify_bundle verify
      ~limits:(N.Consensus_proposal.limits ~max_txs:100 ~max_bytes:1_000_000 ~max_ou:(Z.of_int 1_000_000))
      ~header:{commit.certificate.header with C.epoch_id = Int64.of_int epoch}
      ~expected_tx_count:(List.length txs) txs [] in
  let check_inputs epoch commit txs =
    (match checked_inputs epoch commit txs with Ok _ -> () | Error error ->
      N.Consensus_proposal.log_reject ~epoch_id:(Int64.of_int epoch) error;
      failwith "signed input refused");
    List.iter (fun tx -> List.iter (fun changed ->
      expect "signed alteration accepted" (match checked_inputs epoch commit [changed] with
        | Error (N.Consensus_proposal.Invalid_tx_signature _ | Invalid_tx_envelope _) -> true
        | _ -> false))
      [{tx with T.signature = Base64.encode_exn (String.make 64 '\000')};
       {tx with T.nonce = tx.nonce + 1}; {tx with T.amount = Z.succ tx.amount}]) txs in
  let bind_epoch epoch commit =
    let fold = N.Set_rule.bind rules ~chain_id ~parent:(Some commit) ~epoch |> expect_ok "signed rules" in
    let ctx = fold epoch |> expect_ok "signed context" in
    expect "signed active rules absent" (ctx.X.ready_exec_mode = G.Active && ctx.plan_mode = G.Active
      && ctx.duty_marks = G.Active && ctx.standard_mode = G.Active);
    fold in
  let reward_for offset omitted commit =
    let reward = N.Consensus_reward_attribution.resolve_for_epoch ~chain_id ~epoch_id:(Int64.of_int (start + offset))
      ~proposer_addr:first.address ~validator_pubkeys:[] (Some commit) |> expect_ok "signed reward" in
    expect "signed reward source differs" (reward.proposer_addr = first.address
      && List.map (fun (item : X.reward_validator) -> item.address, item.weight) reward.validators
        = rows (Plan.weights offset));
    expect "signed reward omitted nonsigner"
      (List.exists (fun (item : X.reward_validator) -> item.address = omitted) reward.validators);
    reward in
  let epoch_runner offset current omitted (commit : C.parent_commit) =
    let epoch = start + offset in
    let fold = bind_epoch epoch commit in
    let reward = reward_for offset omitted commit in
    let env = X.{chain_id; epoch_id = epoch; proposer_addr = first.address;
      validator_addrs = List.map (fun item -> item.C.address) current.C.validators;
      validator_pubkeys = List.map (fun item -> item.C.address, Base64.encode_exn item.pubkey) current.validators;
      prev_state_root = N.Text.raw_to_hex commit.certificate.header.proposed_state_root; epoch_ts = float_of_int epoch *. 10.;
      ready_state_root_at = Some (fun _ -> failwith "signed local history"); ready_max_lag = 0} in
    fun store ledger txs ->
      let open Lwt.Syntax in
      let backend = X.make_live_backend ~emission_policy:E.Allow
        ~emission_schedule:(Octra_core.Emission_schedule.Security_curve {activation_epoch = start - 1})
        ~validator_policy:policy ~fold ~voting_set:(epoch, current) store ledger in
      let* value = X.run_core ~reward:(Some reward) ~preverify:None ~backend ~env ~txs
        ~process_tx:(X.confirmed_process X.process_standard_tx) in
      let result = expect_ok "signed execution" value in
      let* state = snapshot store ledger in
      Lwt.return_ok (result, state) in
  let check_replay epoch commit preview before next =
    let account = L.find ledger first.address in
    let replay = bond_tx epoch first account.nonce Policy.min_bond in
    expect "signed repeated nonce has invalid proof" (Result.is_ok (checked_inputs epoch commit [replay]));
    let refused, state = preview [replay] in
    expect "signed repeated nonce accepted" (refused.X.artifacts.confirmed = []
      && match refused.artifacts.rejected with
        | [row] -> row.reason = "Invalid nonce" && not row.charged
        | _ -> false);
    expect "signed repeated nonce charged fees" (Z.equal refused.artifacts.confirmed_fees Z.zero);
    check_money "repeated nonce" (epoch - start + 1) next state;
    expect "signed repeated nonce mutated store" (Lwt_main.run (snapshot store ledger) = before) in
  let check_escrow offset inputs =
    if offset = 0 || offset = 40 || offset = 64 then begin
      let registry = read store Registry.meta_key |> Option.get |> Registry.of_string |> expect_ok "signed registry" in
      let bonded = List.fold_left (fun total item -> Z.add total item.Octra_core.Validator_entry.bond)
        Z.zero (Registry.entries registry) in
      expect "signed escrow differs" (Z.equal (L.find ledger Registry.escrow_address).L.balance bonded);
      Printf.printf "event = signed_set offset = %d inputs = %d escrow = %s\n%!" offset inputs (Z.to_string bonded)
    end in
  let apply_step offset current omitted commit txs planned =
    let epoch = start + offset in
    let execute = epoch_runner offset current omitted commit in
    let before = Lwt_main.run (snapshot store ledger) in
    let preview txs = Lwt_main.run (Octra_core.State_preview.with_state
      ~base_store:store ~base_ledger:ledger ~epoch_id:epoch ~proposal_id:"signed-set"
      (fun store ledger -> execute store ledger txs)) |> expect_ok "signed preview" in
    let next = Plan.advance offset planned in
    let previewed, state = preview txs in
    check_money "preview" (offset + 1) next state;
    check_sets offset state;
    expect "signed preview mutated store" (Lwt_main.run (snapshot store ledger) = before);
    expect "signed transition rejected" (previewed.X.artifacts.rejected = []
      && List.map fst previewed.artifacts.confirmed = T.consensus_order txs);
    expect "signed preview fees differ" (Z.equal previewed.artifacts.confirmed_fees (Plan.fees offset));
    if offset = 41 then check_replay epoch commit preview before next;
    let applied, applied_state = Lwt_main.run (execute store ledger txs) |> expect_ok "signed apply" in
    check_money "apply" (offset + 1) next applied_state;
    check_sets offset applied_state;
    expect "signed apply fees differ" (Z.equal applied.artifacts.confirmed_fees (Plan.fees offset));
    expect "signed preview apply differs" ((applied, applied_state) = (previewed, state));
    check_escrow offset (List.length txs);
    next in
  let rec advance offset current previous planned =
    if offset = 97 then planned else
    let current = chosen (start + offset - 1) current in
    let root = Lwt_main.run (S.state_hash store) in
    let commit, omitted = make_parent offset current previous root in
    let txs = make_inputs offset planned root commit in
    check_inputs (start + offset) commit txs;
    let next = apply_step offset current omitted commit txs planned in
    advance (offset + 1) current commit.certificate.proposal_id next in
  check_money "initial" 0 Plan.initial (Lwt_main.run (snapshot store ledger));
  let final = advance 0 initial Octra_net.Hash_domain.nil_hash Plan.initial in
  expect "signed final supply differs" (Z.equal final.supply (Z.of_int 700_159_900));
  expect "signed final reserve differs" (Z.equal final.remaining (Z.of_string "47303833401"));
  expect "signed final escrow differs" (Z.equal final.escrow (Z.of_int 29_000_000));
  expect "signed final nonce counts differ" (List.map snd final.accounts = [3; 11; 2; 2; 2; 2; 3]);
  expect "signed final balances differ" (List.map fst final.accounts = List.map Z.of_int
    [98_130_802; 97_993_806; 97_004_518; 96_006_223; 95_007_949; 94_009_683; 93_006_919]);
  let expected = Lwt_main.run (snapshot store ledger) in
  let copy = store.S.store_path ^ "_signed" in
  let commit = Lwt_main.run (S.get_commit_hash store) |> Option.get in
  S.create_compact_store store ~expected_commit:commit ~target:copy
  |> Lwt_main.run |> expect_ok "signed export" |> ignore;
  let reopened = Lwt_main.run (S.open_store copy) in
  Fun.protect ~finally:(fun () -> Lwt_main.run (S.close reopened); remove_tree copy) (fun () ->
    let state = Lwt_main.run (snapshot reopened (L.create reopened)) in
    check_money "reopen" 97 final state;
    check_sets 96 state;
    expect "signed reopen differs" (state = expected))

let eviction_env ~chain_id ~epoch_id ~proposer (set : Octra_consensus.C_types.validator_set) =
  let module C = Octra_consensus.C_types in
  X.{chain_id; epoch_id; proposer_addr = proposer;
    validator_addrs = List.map (fun member -> member.C.address) set.validators;
    validator_pubkeys = List.map (fun member ->
      member.C.address, Base64.encode_exn member.pubkey) set.validators;
    prev_state_root = String.make 64 'a'; epoch_ts = float_of_int epoch_id;
    ready_state_root_at = None; ready_max_lag = 0}

let write_eviction_set store source_epoch activate_epoch set =
  let module C = Octra_consensus.C_types in
  let members = C.weights_of_set set |> List.map (fun (member, weight) ->
    Octra_core.Validator_entry.{address = member.C.address; pubkey = member.pubkey; weight}) in
  let update = Update.make_weighted ~source_epoch ~activate_epoch members
    |> expect_ok "eviction durable set" in
  Lwt_main.run (S.set_meta store Update.pending_meta_key (Update.to_string update));
  Update.to_string update

let check_eviction_progress ~backend ~env ~current ~pending =
  let module C = Octra_consensus.C_types in
  let read key = Lwt_main.run (S.get_meta backend.X.store key) in
  let env = eviction_env ~chain_id:env.X.chain_id ~epoch_id:(env.epoch_id + 1)
    ~proposer:env.proposer_addr current in
  let backend = {backend with X.voting_set = Some (env.epoch_id, current)} in
  Lwt_main.run (X.advance_validator_set ~backend ~env);
  expect "eviction promotion did not install exact set" (read Update.active_meta_key = Some pending);
  expect "eviction promotion kept pending" (read Update.pending_meta_key = None);
  let snapshot_epoch = env.epoch_id + ((100 - env.epoch_id) mod 4 + 4) mod 4 in
  let env = {env with X.epoch_id = snapshot_epoch} in
  let backend = {backend with X.voting_set = Some (snapshot_epoch, current)} in
  Lwt_main.run (X.schedule_validator_snapshot ~backend ~env ());
  expect "ordinary heavy snapshot was scheduled" (read Update.pending_meta_key = None);
  expect "ordinary heavy snapshot replaced active" (read Update.active_meta_key = Some pending);
  expect "eviction heavy set failed raw quorum"
    (C.has_quorum_at ~chain_id:env.chain_id ~epoch_id:(Int64.of_int env.epoch_id) current
      (List.map (fun member -> member.C.address) current.validators))

let check_pending_ejection ~backend ~env ~current ~future ~retained =
  let env = eviction_env ~chain_id:env.X.chain_id ~epoch_id:env.epoch_id
    ~proposer:env.proposer_addr current in
  let backend = {backend with X.voting_set = Some (env.epoch_id, current)} in
  Lwt_main.run (S.set_meta backend.store Update.pending_meta_key future);
  expect "pending-only offender plan was not canceled"
    (Lwt_main.run (X.schedule_validator_eviction ~backend ~env));
  expect "pending-only offender plan remained"
    (Lwt_main.run (S.get_meta backend.store Update.pending_meta_key) = None);
  Lwt_main.run (S.set_meta backend.store Update.pending_meta_key retained)

let run_certified_eviction ~historical store =
  let module C = Octra_consensus.C_types in
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch = (Option.get
    (Octra_core.Rule_graph.seatless_activation_for_chain chain_id)).activation_epoch in
  let people = List.init 5 (fun _ -> identity ()) in
  let reporter = List.hd people and ledger = L.create store in
  let offender = List.nth people 4 in
  let rows = List.map2 (fun (person : identity) weight ->
    C.{address = person.address; pubkey = person.consensus_public}, Z.of_int weight)
    people [3; 3; 2; 1; 1] in
  let current = C.make_weighted_validator_set rows |> expect_ok "eviction live set" in
  let registry = eviction_registry rows [] in
  List.iter (fun (person : identity) -> L.add_account ledger person.address Z.zero
    |> expect_ok "eviction member account") people;
  L.credit ledger reporter.address (Z.of_int 10_000) |> expect_ok "eviction reporter";
  L.add_account ledger Registry.escrow_address (Z.mul Policy.min_bond (Z.of_int 10))
    |> expect_ok "eviction escrow";
  Lwt_main.run (L.flush_dirty_lwt ledger);
  let write key value = Lwt_main.run (S.set_meta store key value) in
  write Registry.meta_key (Registry.to_string registry);
  write "total_supply" (Z.to_string (L.get_total_supply ledger));
  write "emission_remaining" "0";
  write Octra_core.Emission_schedule.retired_key
    (Z.to_string (Z.sub Octra_core.Denomination.max_supply (L.get_total_supply ledger)));
  let future = write_eviction_set store (Int64.of_int (epoch - 1))
    (Int64.of_int (epoch + 1000)) current in
  let env = eviction_env ~chain_id ~epoch_id:epoch ~proposer:reporter.address current in
  let backend = X.make_live_backend ~emission_policy:E.Allow
    ~emission_schedule:Octra_core.Emission_schedule.Inactive ~validator_policy:policy
    ~voting_set:(epoch, current) store ledger in
  let applied = if historical then epoch - 1 else epoch in
  let evidence = conflict ~chain_id ~epoch:(Int64.of_int (applied - 1)) offender in
  let tx = transaction ~ou:(Z.of_int 5000) ~from:reporter.address
    ~public_key:(Base64.encode_exn reporter.consensus_public) ~to_:offender.address
    ~amount:Z.zero ~nonce:1 ~message:(Octra_core.Validator_evidence.message evidence)
    ~op_type:T.ValidatorEvidence in
  let tx = T.sign_with_privkey tx
    (Mirage_crypto_ec.Ed25519.priv_to_octets reporter.consensus_private |> Base64.encode_exn) in
  expect "eviction report signature differs" (T.verify tx (Base64.encode_exn reporter.consensus_public));
  let evidence_env = {env with X.epoch_id = applied} in
  let evidence_backend = {backend with X.voting_set = Some (applied, current)} in
  Lwt_main.run (X.process_validator_evidence_tx ~backend:evidence_backend ~env:evidence_env tx)
    |> (function Ok _ -> () | Error (_, reason) -> failwith reason);
  if historical then begin
    expect "preactivation scheduled eviction"
      (not (Lwt_main.run (X.schedule_validator_eviction ~backend:evidence_backend ~env:evidence_env)));
    expect "preactivation changed future plan"
      (Lwt_main.run (S.get_meta store Update.pending_meta_key) = Some future)
  end;
  List.iter (fun (voting_set, expected) ->
    let backend = {backend with X.voting_set} in
    let failed = try ignore (Lwt_main.run (X.schedule_validator_eviction ~backend ~env)); false
      with Failure reason -> reason = expected in
    expect "eviction accepted missing or wrong typed source" failed;
    expect "eviction source refusal changed plan"
      (Lwt_main.run (S.get_meta store Update.pending_meta_key) = Some future))
    [None, "epoch voting set is missing";
     Some (epoch - 1, current), "epoch voting set height differs"];
  Lwt_main.run (X.advance_validator_set ~backend ~env);
  let raw = Lwt_main.run (S.get_meta store Update.pending_meta_key) |> Option.get in
  let update = Update.of_string raw |> expect_ok "eviction pending parse" in
  let registry = Lwt_main.run (S.get_meta store Registry.meta_key) |> Option.get
    |> Registry.of_string |> expect_ok "eviction applied registry" in
  let next, removed = Update.check_eviction_state ~chain_id ~epoch ~registry ~current update
    |> expect_ok "eviction live state" in
  expect "eviction old voting weight used bond" (removed = Z.one);
  expect "eviction did not replace future plan"
    (raw <> future && update.activate_epoch = Int64.of_int (epoch + 1));
  expect "eviction retained slashed voting key" (C.weight_of_addr next offender.address = None);
  expect "eviction slashed vote counted" (C.signed_weight next [offender.address] = None);
  expect "eviction remaining heavy set was clipped"
    (next.total_weight = Z.of_int 9 && Result.is_error
      (C.checked_validator_set_for_epoch ~chain_id ~epoch_id:(Int64.of_int (epoch + 1)) next));
  let reward = X.{proposer_addr = reporter.address;
    proposer_public_key = Some (Base64.encode_exn reporter.consensus_public);
    validators = List.map (fun (member, weight) ->
      {address = member.C.address; public_key = Some (Base64.encode_exn member.pubkey); weight}) rows} in
  let total = Lwt_main.run (S.get_meta store "total_supply") |> Option.get |> Z.of_string in
  let retired = Z.sub Octra_core.Denomination.max_supply total in
  let plan = X.build_reward_plan_with_base ~fee_burn_active:true ~supply_retired:retired
    ~base_reward:Z.zero ~validator_count:5 ~emission_remaining:Z.zero
    ~confirmed_fees:(Z.of_int 5000) ~prev_supply:total |> expect_ok "eviction reward plan"
    |> X.prepare_reward_plan ~chain_id ~epoch_id:epoch ~registry:(Some (Registry.to_string registry)) ~reward
    |> expect_ok "eviction reward prepare" in
  Lwt_main.run (X.apply_epoch_footer_with_reward ~backend ~env ~reward ~plan);
  expect "eviction slashed member was rewarded" ((L.find ledger offender.address).L.balance = Z.zero);
  expect "eviction reward burn differs" (plan.reward_burned = Z.of_int 120);
  check_pending_ejection ~backend ~env ~current:next ~future ~retained:raw;
  check_eviction_progress ~backend ~env ~current:next ~pending:raw

let run_evidence_capacity weights selected accepted store =
  let module C = Octra_consensus.C_types in
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch = (Option.get
    (Octra_core.Rule_graph.seatless_activation_for_chain chain_id)).activation_epoch in
  let people = List.map (fun _ -> identity ()) weights in
  let reporter = List.nth people 2 and sender = List.nth people 3 in
  let rows = List.map2 (fun (person : identity) weight ->
    C.{address = person.address; pubkey = person.consensus_public}, Z.of_int weight) people weights in
  let current = C.make_weighted_validator_set rows |> expect_ok "evidence capacity set" in
  let ledger = L.create store in
  List.iter (fun (person : identity) ->
    L.add_account ledger person.address (Z.of_int 20_000) |> expect_ok "evidence capacity account") people;
  let escrow = Z.mul Policy.min_bond (List.fold_left (fun sum weight -> Z.add sum (Z.of_int weight)) Z.zero weights) in
  L.add_account ledger Registry.escrow_address escrow |> expect_ok "evidence capacity escrow";
  Lwt_main.run (L.flush_dirty_lwt ledger);
  let total = L.get_total_supply ledger in
  let retired = Z.sub Octra_core.Denomination.max_supply total in
  List.iter (fun (key, value) -> Lwt_main.run (S.set_meta store key value))
    [Registry.meta_key, Registry.to_string (eviction_registry rows []);
     "total_supply", Z.to_string total; "emission_remaining", "0";
     Octra_core.Emission_schedule.retired_key, Z.to_string retired;
     Octra_core.Emission_schedule.standard_key, Octra_core.Emission_schedule.standard_name;
     Octra_core.Emission_schedule.activation_key, string_of_int epoch;
     Octra_core.Emission_schedule.initial_key, "0"];
  let env = eviction_env ~chain_id ~epoch_id:epoch ~proposer:reporter.address current in
  let sign person tx = T.sign_with_privkey tx
    (Mirage_crypto_ec.Ed25519.priv_to_octets person.consensus_private |> Base64.encode_exn) in
  let reports = List.mapi (fun index member ->
    let person = List.nth people member in
    transaction ~ou:(Z.of_int 5000) ~from:reporter.address
      ~public_key:(Base64.encode_exn reporter.consensus_public) ~to_:person.address
      ~amount:Z.zero ~nonce:(index + 1)
      ~message:(Octra_core.Validator_evidence.message
        (conflict ~chain_id ~epoch:(Int64.of_int (epoch - 1)) person))
      ~op_type:T.ValidatorEvidence |> sign reporter) selected in
  let ordinary = transaction ~ou:Z.one ~from:sender.address
    ~public_key:(Base64.encode_exn sender.consensus_public) ~to_:(List.hd people).address
    ~amount:(Z.of_int 7) ~nonce:1 ~message:"" ~op_type:T.Standard |> sign sender in
  let execute store ledger =
    let backend = X.make_live_backend ~emission_policy:E.Allow
      ~emission_schedule:(Octra_core.Emission_schedule.Security_curve {activation_epoch = epoch})
      ~validator_policy:policy
      ~voting_set:(epoch, current) store ledger in
    let reward = X.{proposer_addr = reporter.address;
      proposer_public_key = Some (Base64.encode_exn reporter.consensus_public);
      validators = List.map (fun (member, weight) ->
        {address = member.C.address; public_key = Some (Base64.encode_exn member.pubkey); weight}) rows} in
    X.run_core ~reward:(Some reward) ~preverify:None ~backend ~env ~txs:(reports @ [ordinary])
      ~process_tx:(X.confirmed_process X.process_standard_tx) in
  let before = Lwt_main.run (S.state_hash store) in
  let preview = Lwt_main.run (Octra_core.State_preview.with_state
    ~base_store:store ~base_ledger:ledger ~epoch_id:epoch ~proposal_id:"evidence-capacity"
    execute) |> expect_ok "evidence capacity preview" in
  expect "evidence capacity preview changed store" (Lwt_main.run (S.state_hash store) = before);
  let result = Lwt_main.run (execute store ledger) |> expect_ok "evidence capacity apply" in
  expect "evidence capacity preview differs" (result = preview);
  let refused = List.map2 (fun tx allowed -> if allowed then None else Some (T.hash tx)) reports accepted
    |> List.filter_map Fun.id in
  expect "evidence capacity rejected another transaction"
    (List.map (fun (row : X.tx_reject) -> T.hash row.tx) result.artifacts.rejected = refused);
  List.iter (fun (row : X.tx_reject) ->
    expect "evidence capacity refusal changed" (row.error_type = "validator_evidence_rejected"
      && row.reason = "evicted weight reaches blocking threshold" && not row.charged)) result.artifacts.rejected;
  expect "evidence capacity lost ordinary transfer"
    (List.exists (fun (tx, _) -> T.hash tx = T.hash ordinary) result.artifacts.confirmed);
  let removed = List.fold_left2 (fun sum member allowed ->
    if allowed then Z.add sum (Z.of_int (List.nth weights member)) else sum) Z.zero selected accepted in
  let burned = Z.mul Policy.min_bond removed in
  let fees = Z.of_int (1 + (5000 * List.length (List.filter Fun.id accepted))) in
  let fee_burned = Z.div fees (Z.of_int 5) in
  let rewarded = Z.sub fees fee_burned in
  let pool = Z.sub rewarded Z.(div (mul rewarded (of_int 7)) (of_int 10)) in
  let reward_burned = List.fold_left2 (fun sum member allowed ->
    if allowed then Z.add sum (Z.div (Z.mul pool (Z.of_int (List.nth weights member))) current.total_weight)
    else sum) Z.zero selected accepted in
  let retired_delta = Z.add fee_burned (Z.add burned reward_burned) in
  expect "evidence capacity changed escrow" ((L.find ledger Registry.escrow_address).L.balance = Z.sub escrow burned);
  expect "evidence capacity changed reporter nonce"
    ((L.find ledger reporter.address).L.nonce = List.length (List.filter Fun.id accepted));
  let read key = Lwt_main.run (S.get_meta store key) |> Option.get in
  expect "evidence capacity changed supply" (Z.of_string (read "total_supply") = Z.sub total retired_delta);
  expect "evidence capacity changed retired"
    (Z.of_string (read Octra_core.Emission_schedule.retired_key) = Z.add retired retired_delta);
  let registry = Registry.of_string (read Registry.meta_key) |> expect_ok "evidence capacity registry" in
  List.iter2 (fun member allowed ->
    expect "evidence capacity changed rejected offender"
      (Option.is_none (Registry.find (List.nth people member).address registry) = allowed)) selected accepted;
  let delayed = not (Octra_consensus.C_measure.live ~total:current.total_weight ~lost:removed) in
  expect "evidence capacity changed eviction decision"
    (Option.is_some (Lwt_main.run (S.get_meta store X.eviction_wait_key)) = delayed);
  if delayed then begin
    expect "unsafe eviction changed pending set"
      (Lwt_main.run (S.get_meta store Update.pending_meta_key) = None);
    let later = epoch + Int64.to_int Policy.evidence_epochs + 10 in
    let env = eviction_env ~chain_id ~epoch_id:later ~proposer:reporter.address current in
    let backend = X.make_live_backend ~emission_policy:E.Allow ~validator_policy:policy
      ~voting_set:(later, current) store ledger in
    Lwt_main.run (X.advance_validator_set ~backend ~env);
    let fresh = List.mapi (fun index person -> index, person) people |> List.find (fun (index, _) ->
      not (List.mem index selected) && index <> 2) |> snd in
    let report = transaction ~ou:Z.one ~from:reporter.address
      ~public_key:(Base64.encode_exn reporter.consensus_public) ~to_:fresh.address
      ~amount:Z.zero ~nonce:(List.length reports + 1)
      ~message:(Octra_core.Validator_evidence.message
        (conflict ~chain_id ~epoch:(Int64.of_int (later - 1)) fresh))
      ~op_type:T.ValidatorEvidence |> sign reporter in
    Lwt_main.run (X.process_validator_evidence_tx ~backend ~env report)
      |> (function Ok _ -> () | Error (_, reason) -> failwith reason);
    let reward = X.{proposer_addr = reporter.address; proposer_public_key = None;
      validators = List.map (fun (member, weight) ->
        {address = member.C.address; public_key = None; weight}) rows} in
    let excluded = X.reward_exclusions ~chain_id ~epoch_id:later
      ~registry:(Some (read Registry.meta_key)) ~reward () |> expect_ok "delayed reward exclusions" in
    List.iter (fun member ->
      let person = List.nth people member in
      expect "delayed offender recovered reward" (List.mem person.address excluded);
      expect "delayed offender penalty missing"
        (Option.is_some (Lwt_main.run (S.get_meta store (X.validator_penalty_key person.address))));
      let bond = transaction ~ou:Z.one ~from:person.address
        ~public_key:(Base64.encode_exn person.consensus_public) ~to_:Registry.escrow_address
        ~amount:Policy.min_bond ~nonce:1
        ~message:(bond_message ~chain_id ~amount:Policy.min_bond ~nonce:1 person) ~op_type:T.ValidatorBond in
      let root = Lwt_main.run (S.state_hash store) in
      expect "disqualified offender re-entered"
        (Lwt_main.run (X.process_validator_bond_tx ~backend ~env bond)
          = Error ("validator_bond_rejected", "validator was disqualified by evidence"));
      expect "rejected re-entry changed state" (Lwt_main.run (S.state_hash store) = root)) selected
  end

let run_old_slash_wait count store =
  let module C = Octra_consensus.C_types in
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch = (Option.get (Octra_core.Rule_graph.seatless_activation_for_chain chain_id)).activation_epoch in
  let people = List.init 7 (fun _ -> identity ()) in
  let rows = List.map (fun (person : identity) ->
    C.{address = person.address; pubkey = person.consensus_public}, Z.one) people in
  let current = C.make_weighted_validator_set rows |> expect_ok "old slash set" in
  let offenders = List.filteri (fun index _ -> index < count) people in
  let entries = List.filter (fun (member, _) ->
    not (List.exists (fun (person : identity) -> person.address = member.C.address) offenders)) rows in
  let outsider = identity () in
  let registry = eviction_registry
    (entries @ [C.{address = outsider.address; pubkey = outsider.consensus_public}, Z.one])
    ((outsider.address, Int64.of_int (epoch - 100_000)) :: List.map (fun (person : identity) ->
    person.address, Int64.of_int (epoch - 1)) offenders) in
  let ledger = L.create store and reporter = List.nth people 6 in
  List.iter (fun (person : identity) -> L.add_account ledger person.address (Z.of_int 1000)
    |> expect_ok "old slash account") (outsider :: people);
  L.add_account ledger Registry.escrow_address (Z.mul Policy.min_bond (Z.of_int (8 - count)))
    |> expect_ok "old slash escrow";
  Lwt_main.run (L.flush_dirty_lwt ledger);
  Lwt_main.run (S.set_meta store Registry.meta_key (Registry.to_string registry));
  let total = L.get_total_supply ledger in
  List.iter (fun (key, value) -> Lwt_main.run (S.set_meta store key value))
    ["total_supply", Z.to_string total; "emission_remaining", "0";
     Octra_core.Emission_schedule.retired_key,
     Z.to_string (Z.sub Octra_core.Denomination.max_supply total)];
  let source = Int64.of_int (epoch - 1) in
  let future = write_eviction_set store source (Int64.of_int (epoch + 100)) current in
  let run at store ledger =
    let open Lwt.Syntax in
    let backend = X.make_live_backend ~emission_policy:E.Allow ~validator_policy:policy
      ~voting_set:(at, current) store ledger in
    let env = eviction_env ~chain_id ~epoch_id:at ~proposer:reporter.address current in
    let* () = if at <> epoch then Lwt.return_unit else
      let fresh = List.nth people 5 in
      let tx = transaction ~ou:Z.one ~from:reporter.address
        ~public_key:(Base64.encode_exn reporter.consensus_public) ~to_:fresh.address
        ~amount:Z.zero ~nonce:1
        ~message:(Octra_core.Validator_evidence.message
          (conflict ~chain_id ~epoch:(Int64.of_int (epoch - 1)) fresh)) ~op_type:T.ValidatorEvidence in
      let* result = X.process_validator_evidence_tx ~backend ~env tx in
      match result with Ok _ -> Lwt.return_unit | Error (_, reason) -> Lwt.fail_with reason in
    let* () = X.advance_validator_set ~backend ~env in
    let* root = S.state_hash store in
    let* pending = S.get_meta store Update.pending_meta_key in
    let* delayed = S.get_meta store X.eviction_wait_key in
    let* bans = Lwt_list.map_s (fun (person : identity) ->
      S.get_meta store (X.validator_penalty_key person.address)) (outsider :: offenders) in
    Lwt.return_ok (root, pending, delayed, bans) in
  ignore (Lwt_main.run (run (epoch - 1) store ledger) |> expect_ok "old slash prior epoch");
  expect "old slash changed preactivation plan"
    (Lwt_main.run (S.get_meta store Update.pending_meta_key) = Some future);
  let before = Lwt_main.run (S.state_hash store) in
  let preview = Octra_core.State_preview.with_state ~base_store:store ~base_ledger:ledger
    ~epoch_id:epoch ~proposal_id:"old-slash-wait" (run epoch) |> Lwt_main.run |> expect_ok "old slash preview" in
  expect "old slash preview changed store" (Lwt_main.run (S.state_hash store) = before);
  let applied = Lwt_main.run (run epoch store ledger) |> expect_ok "old slash apply" in
  expect "old slash preview differs" (preview = applied);
  let _, pending, delayed, bans = applied in
  expect "unsafe historical eviction was scheduled" (pending = None);
  expect "historical eviction lacks durable decision" (Option.is_some delayed);
  expect "historical offenders can re-enter" (List.for_all Option.is_some bans);
  let repeated = Lwt_main.run (run (epoch + 1) store ledger) |> expect_ok "old slash repeated epoch" in
  expect "old slash decision is not stable" (applied = repeated);
  let backend = X.make_live_backend ~emission_policy:E.Allow ~validator_policy:policy
    ~voting_set:(epoch + 1, current) store ledger in
  let env = eviction_env ~chain_id ~epoch_id:(epoch + 1) ~proposer:reporter.address current in
  let ready = `Assoc ["consensus_pubkey", `String (Base64.encode_exn outsider.consensus_public);
    "head_epoch", `String (string_of_int epoch); "state_root", `String (String.make 64 '0')]
    |> Yojson.Safe.to_string in
  let ready_tx = transaction ~ou:Z.one ~from:outsider.address
    ~public_key:(Base64.encode_exn outsider.consensus_public) ~to_:outsider.address
    ~amount:Z.zero ~nonce:1 ~message:ready ~op_type:T.ValidatorReady in
  expect "old rebond regained readiness"
    (Lwt_main.run (X.process_validator_ready_tx ~backend ~env ready_tx)
      = Error ("validator_ready_rejected", "validator was disqualified by evidence"));
  let current_registry = S.get_meta store Registry.meta_key |> Lwt_main.run |> Option.get
    |> Registry.of_string |> expect_ok "old slash pruned registry" in
  expect "old nonmember record did not exercise pruning"
    (not (List.exists (fun (row : Registry.slash_record) -> row.offender = outsider.address)
      (Registry.slashes current_registry)));
  let parameters = match policy with Policy.Bonded p -> p | _ -> assert false in
  let _, selected, _ = Lwt_main.run (X.validator_snapshot_input ~backend ~env parameters current_registry) in
  expect "old rebond re-entered snapshot"
    (not (List.exists (fun (entry : Octra_core.Validator_entry.entry) -> entry.address = outsider.address) selected));
  let reward = X.{proposer_addr = outsider.address; proposer_public_key = None;
    validators = [{address = outsider.address; public_key = None; weight = Z.one}]} in
  let penalized = Lwt_main.run (X.validator_penalties ~read:(S.get_meta backend.store) ~chain_id ~epoch_id:env.epoch_id
    (X.reward_addresses reward)) in
  expect "old parent offender regained reward"
    (X.reward_exclusions ~penalized ~chain_id ~epoch_id:env.epoch_id
      ~registry:(Some (Registry.to_string current_registry)) ~reward () = Ok [outsider.address])

let test_slash_reward_retention () =
  let module Evidence = Octra_core.Validator_evidence in
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch = (Option.get (Octra_core.Rule_graph.seatless_activation_for_chain chain_id)).activation_epoch in
  let at = Int64.of_int epoch in
  let window = Policy.evidence_age Octra_core.Rule_graph.Active Policy.evidence_epochs in
  let bonded = Int64.sub at (Int64.add window 10L) in
  let first = identity () and second = identity () and third = identity () in
  let registry = List.fold_left (fun registry person ->
    let amount = Policy.min_bond in
    let payload = Registry.bond_payload_of_message
      (Some (bond_message ~chain_id ~amount ~nonce:1 person)) |> expect_ok "retention bond payload" in
    Registry.apply_bond Policy.parameters ~chain_id ~epoch:bonded ~address:person.address
      ~sender_pubkey_b64:(Base64.encode_exn person.consensus_public) ~amount ~nonce:1 payload registry
    |> expect_ok "retention bond") Registry.empty [first; second; third] in
  let verified current evidence_epoch person =
    let proof = conflict ~chain_id ~epoch:evidence_epoch person |> Evidence.message
      |> Option.some |> Evidence.proof_of_message |> expect_ok "retention evidence payload" in
    Evidence.verify ~chain_id ~current_epoch:current ~evidence_epochs:window ~bonded_epoch:bonded
      ~address:person.address ~pubkey:person.consensus_public proof |> expect_ok "retention evidence" in
  let first_proof = verified at (Int64.sub at (Int64.pred window)) first in
  let first_state, _ = Registry.apply_slash ~retain_rewards:true ~current_epoch:at
    ~evidence_epochs:window first_proof registry |> expect_ok "retention first slash" in
  let next = Int64.succ at in
  let second_proof = verified next at second in
  let slash retain_rewards = Registry.apply_slash ~retain_rewards ~current_epoch:next
    ~evidence_epochs:window second_proof first_state |> expect_ok "retention second slash" |> fst in
  let prior = slash false and active = slash true in
  let contains person state = List.exists (fun (record : Registry.slash_record) ->
    record.offender = person.address) (Registry.slashes state) in
  expect "legacy retention changed" (not (contains first prior));
  expect "reward witness was pruned" (contains first active && contains second active);
  let reward = X.{proposer_addr = first.address;
    proposer_public_key = Some (Base64.encode_exn first.consensus_public);
    validators = List.map (fun (person : identity) -> X.{address = person.address;
      public_key = Some (Base64.encode_exn person.consensus_public); weight = Z.one}) [first; second]} in
  let raw = X.build_reward_plan ~fee_burn_active:false ~supply_retired:Z.zero ~validator_count:2
    ~emission_remaining:Z.zero ~confirmed_fees:(Z.of_int 37) ~prev_supply:(Z.of_int 1000)
    |> expect_ok "retention reward plan" in
  let credits registry = X.prepare_reward_plan ~chain_id ~epoch_id:(epoch + 1)
    ~registry:(Some (Registry.to_string registry)) ~reward raw
    |> expect_ok "retention reward prepare" |> X.reward_credits reward |> expect_ok "retention reward credits" in
  expect "legacy pruning reproduction absent"
    (List.exists (fun (credit : X.reward_credit) -> credit.address = first.address) (credits prior));
  expect "pruned offender received reward" (credits active = []);
  let live = with_store (fun store ->
    let ledger = L.create store and reporter = identity () in
    L.add_account ledger reporter.address (Z.of_int 100) |> expect_ok "retention reporter";
    L.add_account ledger Registry.escrow_address (Z.mul (Z.of_int 2) Policy.min_bond)
      |> expect_ok "retention escrow";
    Lwt_main.run (L.flush_dirty_lwt ledger);
    let total = L.get_total_supply ledger in
    let values = [Registry.meta_key, Registry.to_string first_state;
      "total_supply", Z.to_string total;
      "emission_remaining", Z.to_string Z.(sub (sub Octra_core.Denomination.max_supply total) Policy.min_bond);
      Octra_core.Emission_schedule.retired_key, Z.to_string Policy.min_bond] in
    List.iter (fun (key, value) -> Lwt_main.run (S.set_meta store key value)) values;
    let fold epoch = X.prior_fold epoch |> Result.map (fun ctx ->
      {ctx with X.exit_mode = Octra_core.Rule_graph.Active}) in
    let voters = reporter :: second :: third :: List.init 4 (fun _ -> identity ()) in
    let current = Octra_consensus.C_types.make_weighted_validator_set
      (List.map (fun (person : identity) ->
        Octra_consensus.C_types.{address = person.address; pubkey = person.consensus_public}, Z.one) voters)
      |> expect_ok "retention voting set" in
    let backend = X.make_live_backend ~emission_policy:E.Allow ~fold
      ~voting_set:(epoch + 1, current)
      ~validator_policy:(Policy.Bonded {activation_epoch = 0; parameters = Policy.parameters;
        snapshot_interval = 64L; evidence_epochs = window}) store ledger in
    let env = eviction_env ~chain_id ~epoch_id:(epoch + 1) ~proposer:reporter.address current in
    let tx = transaction ~ou:Z.one ~from:reporter.address
      ~public_key:(Base64.encode_exn reporter.consensus_public) ~to_:second.address
      ~amount:Z.zero ~nonce:1 ~op_type:T.ValidatorEvidence
      ~message:(Evidence.message (conflict ~chain_id ~epoch:at second)) in
    begin match Lwt_main.run (X.process_validator_evidence_tx ~backend ~env tx) with
    | Ok fee -> expect "retention live fee differs" (fee = Z.one)
    | Error (_, reason) -> failwith ("retention live slash refused: " ^ reason)
    end;
    S.get_meta store Registry.meta_key |> Lwt_main.run |> Option.get
      |> Registry.of_string |> expect_ok "retention live registry") in
  expect "live slash lost reward witness" (contains first live && contains second live);
  expect "live slash paid removed offender" (credits live = []);
  let final_epoch = Int64.succ next in
  let third_proof = verified final_epoch next third in
  let final, _ = Registry.apply_slash ~retain_rewards:true ~current_epoch:final_epoch
    ~evidence_epochs:window third_proof active |> expect_ok "retention third slash" in
  expect "reward witness retained indefinitely" (not (contains first final))

let run_registry_change ?(historical = false) ?(expiry = false) ?(promotion = false) ?(density = false)
    label weights actor op_type units accepted store =
  let module C = Octra_consensus.C_types in
  let module G = Octra_core.Rule_graph in
  let module V = Octra_core.Validator_entry in
  let module Sig = Octra_core.Tx_signature in
  let chain_id = "octra-devnet-9871-cluster" in
  let activation = (Option.get (G.seatless_activation_for_chain chain_id)).activation_epoch in
  let first = activation + 16 in
  let epoch = if historical then activation - 1 else first + ((1 - first mod 4 + 4) mod 4) in
  let check name value = expect (label ^ ": " ^ name) value in
  let people = List.map (fun _ -> identity ()) weights in
  let sender = List.nth people actor in
  let payer = identity () and payee = identity () in
  let voting = if expiry || promotion then List.filteri (fun index _ -> index < 4) people else people in
  let parent_at epoch =
    let commit = parent chain_id epoch voting in
    let validator_set = C.make_weighted_validator_set
      (List.map (fun member -> member, Z.one) commit.C.validator_set.validators)
      |> expect_ok (label ^ ": weighted parent") in
    {commit with C.validator_set} in
  let commit = parent_at (Int64.of_int (epoch - 1)) in
  let current = commit.C.validator_set in
  let config = String.make 64 'c' in
  let rules = G.create_ready ~chain_id ~ready_config_hash:config ~root_at:(fun at ->
    match G.root_after_floor ~chain_id ~floor_epoch:epoch ~epoch:at with
    | Some root -> G.Root root | None -> G.Missing) in
  let fold = Octra_node_runtime.Set_rule.bind rules ~chain_id ~parent:(Some commit) ~epoch
    |> expect_ok (label ^ ": rules") in
  let ctx = fold epoch |> expect_ok (label ^ ": context") in
  check "execution modes are not active"
    (ctx.X.mode = G.Active && ctx.live_mode = G.Active && ctx.standard_mode = G.Active);
  let cfg = Fold.participating in
  let initial = if promotion || density then
    List.init 100 (fun index -> epoch - 100 + index)
    |> List.fold_left (fun state at ->
      let commit = parent_at (Int64.of_int (at - 1)) in
      let keep_d = at - 1 >= epoch - 31 && at - 1 <= epoch - 16 in
      let commit = if density then
        let sparse = List.filteri (fun index _ -> index >= 4 && index mod 3 = at mod 3) voting in
        {commit with C.certificate = {commit.certificate with precommits =
          List.filter (fun (vote : C.vote) ->
            not (List.exists (fun (person : identity) -> person.address = vote.validator) sparse))
            commit.certificate.precommits}}
        else if keep_d then commit else
        let excluded = (List.nth voting 3).address in
        {commit with C.certificate = {commit.certificate with precommits =
          List.filter (fun (vote : C.vote) -> vote.validator <> excluded) commit.certificate.precommits}} in
      Fold.advance ~cap_mode:ctx.cap_mode ~marks:(ctx.duty_marks = G.Active)
        cfg ~chain_id ~start:ctx.profile_start ~at:(Int64.of_int at)
        ~parent:(Some commit) state
      |> expect_ok (label ^ ": promotion history") |> fun (state, _, _) -> state) Fold.empty
    else
      Fold.advance ~cap_mode:ctx.cap_mode ~marks:(ctx.duty_marks = G.Active)
        cfg ~chain_id ~start:ctx.profile_start ~at:(Int64.of_int (epoch - 1))
        ~parent:(Some (parent_at (Int64.of_int (epoch - 2)))) Fold.empty
      |> expect_ok (label ^ ": initial fold") |> fun (state, _, _) -> state in
  let initial = if not (expiry || promotion) then initial else
    List.filteri (fun index _ -> index >= 4) people
    |> List.fold_left (fun state person ->
      List.init 17 (fun index -> Int64.of_int (epoch - 65 + 4 * index))
      |> List.fold_left (fun state at ->
        Fold.note_pulse cfg ~epoch:at ~active:false ~address:person.address state
        |> expect_ok (label ^ ": shadow pulse")) state) initial in
  let entries = List.mapi (fun index (person : identity) ->
    let weight = List.nth weights index in
    if weight = 0 then None else Some V.{address = person.address;
      pubkey = person.consensus_public; bond = Z.mul Policy.min_bond (Z.of_int weight);
      bonded_epoch = 0L; ready_epoch = (if op_type = T.ValidatorReady && index = actor
        then None else Some 0L); exit_epoch = None}) people |> List.filter_map Fun.id in
  if promotion then begin
    let at = Int64.of_int epoch in
    let prepared, _, _ = Fold.advance ~cap_mode:ctx.cap_mode ~marks:(ctx.duty_marks = G.Active)
      cfg ~chain_id ~start:ctx.profile_start ~at ~parent:(Some commit) initial
      |> expect_ok (label ^ ": current fold") in
    let ready = List.map (fun (entry : V.entry) ->
      if entry.address = sender.address then {entry with ready_epoch = Some at} else entry) entries in
    let select source state = Fold.filter cfg ~start:ctx.profile_start ~source ready state |> fst
      |> V.select_members Policy.parameters ~source_epoch:source in
    let initial_members = select at prepared in
    check "initial promoted snapshot differs"
      (List.length initial_members = 5
        && List.fold_left (fun total (member : V.member) -> Z.add total member.weight)
          Z.zero initial_members = Z.mul Policy.min_bond (Z.of_int 11));
    check "initial promoted snapshot is invalid"
      (Result.is_ok (V.check_weight_change ~before:[] ~after:initial_members));
    let expires = Int64.add at 8L in
    check "shadow pulse did not expire at head plus nine"
      (not (Fold.allows cfg ~start:ctx.profile_start ~source:expires ~address:sender.address prepared));
    let promoted = Fold.note_set cfg ~epoch:expires
      ~active:(List.map (fun (member : V.member) -> member.address) initial_members) prepared
      |> expect_ok (label ^ ": membership promotion") in
    let later = select (Int64.add at 20L) promoted in
    check "promoted member disappeared with its old pulse"
      (List.exists (fun (member : V.member) -> member.address = sender.address) later);
    check "old member marks did not expire"
      (not (List.exists (fun (member : V.member) -> member.address = (List.nth voting 3).address) later));
    check "post-promotion weights differ"
      (List.map (fun (member : V.member) -> Z.div member.weight Policy.min_bond) later
        |> List.sort Z.compare = List.map Z.of_int [2; 2; 2; 3]);
    check "post-promotion blocking weight accepted"
      (Result.is_error (V.check_weight_change ~before:[] ~after:later))
  end;
  let registry = `Assoc ["standard", `String Policy.standard_name;
    "candidates", `List (List.map (fun (entry : V.entry) -> `Assoc [
      "address", `String entry.address;
      "consensus_pubkey", `String (Base64.encode_exn entry.pubkey);
      "bond", `String (Z.to_string entry.bond); "bonded_epoch", `String "0";
      "ready_epoch", (match entry.ready_epoch with None -> `Null | Some _ -> `String "0");
      "exit_epoch", `Null]) entries); "slashes", `List []]
    |> Registry.of_yojson |> expect_ok (label ^ ": registry") in
  let ledger = L.create store in
  let all = people @ [payer; payee] in
  List.iter (fun person -> L.add_account_with_pubkey ledger person.address
    (Z.of_int 100_000_000) (Base64.encode_exn person.consensus_public)
    |> expect_ok (label ^ ": account")) all;
  let escrow = List.fold_left (fun total (entry : V.entry) -> Z.add total entry.bond) Z.zero entries in
  L.add_account ledger Registry.escrow_address escrow |> expect_ok (label ^ ": escrow");
  Lwt_main.run (L.flush_dirty_lwt ledger);
  List.iter (fun (key, value) -> Lwt_main.run (S.set_meta store key value))
    [Registry.meta_key, Registry.to_string registry; Fold.meta_key, Fold.to_string initial;
     "last_epoch", string_of_int (epoch - 1); "current_epoch", string_of_int (epoch - 1);
     "total_supply", Z.to_string (L.get_total_supply ledger); "emission_remaining", "0"];
  let env = eviction_env ~chain_id ~epoch_id:epoch ~proposer:(List.hd voting).address current in
  let env = {env with X.prev_state_root = Octra_node_runtime.Text.raw_to_hex
      commit.certificate.header.proposed_state_root;
    ready_state_root_at = Some (fun _ -> failwith "registry test read local history")} in
  let amount = Z.mul Policy.min_bond (Z.of_int units) in
  let message = match op_type with
    | T.ValidatorBond -> bond_message ~chain_id ~amount ~nonce:1 sender
    | T.ValidatorReady -> `Assoc [
        "consensus_pubkey", `String (Base64.encode_exn sender.consensus_public);
        "head_epoch", `String (string_of_int (epoch - 1));
        "head_proposal_id", `String (proposal_hex commit);
        "state_root", `String env.prev_state_root; "chain_id", `String chain_id;
        "config_hash", `String config;
        "catchup_head_epoch", `String (string_of_int (epoch - 1))]
      |> Yojson.Safe.to_string
    | _ -> "{}" in
  let sign person tx = Sig.sign (Sig.at ~chain_id ~epoch:(Int64.of_int epoch)) tx
    (Mirage_crypto_ec.Ed25519.priv_to_octets person.consensus_private |> Base64.encode_exn) in
  let operation = transaction ~ou:(Z.of_int 5000) ~from:sender.address
    ~public_key:(Base64.encode_exn sender.consensus_public)
    ~to_:(if op_type = T.ValidatorBond then Registry.escrow_address else sender.address)
    ~amount ~nonce:1 ~message ~op_type |> sign sender in
  let ordinary = transaction ~ou:Z.one ~from:payer.address
    ~public_key:(Base64.encode_exn payer.consensus_public) ~to_:payee.address
    ~amount:(Z.of_int 7) ~nonce:1 ~message:"" ~op_type:T.Standard |> sign payer in
  check "operation signature differs"
    (Sig.verify (Sig.at ~chain_id ~epoch:(Int64.of_int epoch)) operation
      (Base64.encode_exn sender.consensus_public));
  let validator_policy = Policy.of_env_exn (fun name ->
    if name = Policy.env_name then Some "65" else None) in
  check "snapshot phase is not offset"
    (epoch mod 4 <> 0 || historical);
  let snapshot store ledger =
    let open Lwt.Syntax in
    let* root = S.state_hash store in
    let* registry = S.get_meta store Registry.meta_key in
    let* folded = S.get_meta store Fold.meta_key in
    let* pending = S.get_meta store Update.pending_meta_key in
    let accounts = List.map (fun person -> let account = L.find ledger person.address in
      account.L.balance, account.nonce, account.public_key) all in
    Lwt.return (root, registry, folded, pending, accounts,
      (L.find ledger Registry.escrow_address).L.balance) in
  let execute txs store ledger =
    let open Lwt.Syntax in
    let backend = X.make_live_backend ~validator_policy ~fold ~voting_set:(epoch, current)
      ~emission_policy:E.Guard ~emission_schedule:Octra_core.Emission_schedule.Inactive store ledger in
    let* result = X.run_core ~reward:None ~preverify:None ~backend ~env ~txs
      ~process_tx:(X.confirmed_process X.process_standard_tx) in
    match result with
    | Error _ as error -> Lwt.return error
    | Ok result -> let* state = snapshot store ledger in Lwt.return_ok (result, state) in
  let before = Lwt_main.run (snapshot store ledger) in
  let preview txs = Octra_core.State_preview.with_state ~base_store:store ~base_ledger:ledger
    ~epoch_id:epoch ~proposal_id:("registry-" ^ label) (execute txs)
    |> Lwt_main.run |> expect_ok (label ^ ": preview") in
  let control = preview [ordinary] in
  let result, projected = preview [operation; ordinary] in
  check "preview changed original state" (Lwt_main.run (snapshot store ledger) = before);
  check "ordinary transfer disappeared"
    (List.exists (fun (tx, _) -> T.hash tx = T.hash ordinary) result.X.artifacts.confirmed);
  check "confirmed fee differs"
    (Z.equal result.artifacts.confirmed_fees (Z.of_int (if accepted then 5001 else 1)));
  if accepted then begin
    check "operation was refused" (result.artifacts.rejected = []
      && List.length result.artifacts.confirmed = 2);
    let _, raw, _, _, accounts, balance = projected in
    let registered = Registry.of_string (Option.get raw) |> expect_ok (label ^ ": result registry") in
    let entry = Registry.find sender.address registered |> Option.get in
    check "accepted operation nonce differs" (let _, nonce, _ = List.nth accounts actor in nonce = 1);
    check "accepted escrow differs" (Z.equal balance
      (if op_type = T.ValidatorBond then Z.add escrow amount else escrow));
    check "accepted registry transition differs" (match op_type with
      | T.ValidatorBond -> Z.equal entry.bond
          (Z.add (Z.mul Policy.min_bond (Z.of_int (List.nth weights actor))) amount)
      | T.ValidatorReady -> entry.ready_epoch = Some (Int64.of_int epoch)
      | T.ValidatorExit -> entry.exit_epoch = Some (Int64.of_int epoch)
      | _ -> false)
  end else begin
    check "operation refusal differs" (match result.artifacts.rejected with
      | [row] -> T.hash row.tx = T.hash operation && not row.charged
        && row.reason = "validator change increases blocking weight"
        && row.error_type = (match op_type with T.ValidatorBond -> "validator_bond_rejected"
          | T.ValidatorReady -> "validator_ready_rejected" | _ -> "validator_exit_rejected")
      | _ -> false);
    check "refusal differs from ordinary-only state" (projected = snd control)
  end;
  let applied = Lwt_main.run (execute [operation; ordinary] store ledger)
    |> expect_ok (label ^ ": apply") in
  check "preview and apply differ" (applied = (result, projected))

let run_live_penalty store =
  let module C = Octra_consensus.C_types in
  let module G = Octra_core.Rule_graph in
  let module Shared = Octra_node_runtime.Consensus_epoch_apply_shared in
  let module Data = Octra_core.Store_chaindata in
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch = (Option.get (G.seatless_activation_for_chain chain_id)).activation_epoch + 16 in
  let people = List.init 7 (fun _ -> identity ()) in
  let offender = List.hd people in
  let rows = List.map (fun (person : identity) ->
    C.{address = person.address; pubkey = person.consensus_public}, Policy.min_bond) people in
  let current = C.make_weighted_validator_set rows |> expect_ok "live penalty set" in
  let registry = eviction_registry rows [offender.address, Int64.of_int (epoch - 100_000)] in
  let ledger = L.create store in
  List.iter (fun (person : identity) -> L.add_account_with_pubkey ledger person.address
    (Z.of_int 10_000_000) (Base64.encode_exn person.consensus_public)
    |> expect_ok "live penalty account") people;
  L.add_account ledger Registry.escrow_address (Z.mul Policy.min_bond (Z.of_int 7))
    |> expect_ok "live penalty escrow";
  Lwt_main.run (L.flush_dirty_lwt ledger);
  Lwt_main.run (S.set_meta store Registry.meta_key (Registry.to_string registry));
  Lwt_main.run (S.set_meta store "last_epoch" (string_of_int (epoch - 1)));
  Lwt_main.run (S.set_meta store "current_epoch" (string_of_int (epoch - 1)));
  let original = L.find ledger offender.address in
  let env = eviction_env ~chain_id ~epoch_id:epoch ~proposer:offender.address current in
  let commit = { (parent chain_id (Int64.of_int (epoch - 1)) people) with C.validator_set = current } in
  let env = {env with X.prev_state_root = Octra_node_runtime.Text.raw_to_hex
    commit.certificate.header.proposed_state_root} in
  let rules = G.create_ready ~chain_id ~ready_config_hash:(String.make 64 'c')
    ~root_at:(fun at -> match G.root_after_floor ~chain_id ~floor_epoch:epoch ~epoch:at with
      | Some root -> G.Root root | None -> G.Missing) in
  let tx = transaction ~ou:Z.one ~from:offender.address ~public_key:(Base64.encode_exn offender.consensus_public)
    ~to_:Registry.escrow_address ~amount:Policy.min_bond ~nonce:1
    ~message:(bond_message ~chain_id ~amount:Policy.min_bond ~nonce:1 offender) ~op_type:T.ValidatorBond in
  let tx = Octra_core.Tx_signature.sign (Octra_core.Tx_signature.at ~chain_id ~epoch:(Int64.of_int epoch)) tx
    (Base64.encode_exn (Mirage_crypto_ec.Ed25519.priv_to_octets offender.consensus_private)) in
  let chaindata = Data.open_chaindata (Filename.concat (Filename.dirname store.S.store_path) "live") in
  let previous = Sys.getenv_opt Policy.env_name in
  Unix.putenv Policy.env_name "65";
  Fun.protect ~finally:(fun () ->
    Unix.putenv Policy.env_name (Option.value previous ~default:"");
    Data.abort_batch chaindata; Data.close chaindata)
    (fun () ->
      let fees = ref Z.zero and confirmed = ref 0 and rejected = ref [] in
      let runtime = Shared.{ledger; store; chaindata; rules;
        program_trust = Octra_vm.Program_trust.empty; wallet_addr = offender.address;
        pre_state_hash = String.make 64 'b'; standard_env = (fun () -> env); current_epoch = (fun () -> epoch);
        consensus_mode = true; max_fhe_per_epoch = 1; max_stealth_per_epoch = 1;
        max_stealth_defer = 0; stealth_inline_verify_allowed = false;
        fhe_in_epoch_counter = ref 0; stealth_in_epoch_counter = ref 0; stealth_defer_count = Hashtbl.create 1;
        pending_tx_saves = ref []; total_tx_count = confirmed; confirmed_fees = fees; processed_hashes = ref [];
        short = Fun.id; log_shared = (fun ~tx_count:_ -> ()); fatal = failwith;
        exit = (fun () -> failwith "live penalty exited"); notify_new_account = ignore;
        notify_confirmed = (fun _ _ -> ()); notify_rejected = (fun tx reason -> rejected := (T.hash tx, reason) :: !rejected);
        legacy_replay = (fun ~epoch:_ ~address:_ ~cipher:_ -> failwith "live penalty private read");
        private_result_policy = (fun _ -> Octra_core.Private_result_policy.Recoverable)} in
      expect_ok "live penalty journal" (L.begin_journal ledger);
      Lwt_main.run (S.begin_epoch_batch store);
      Data.begin_batch chaindata;
      ignore (Lwt_main.run (Shared.run_node ~parent_commit:commit ~voting_set:(epoch, current) runtime [tx]));
      expect "live penalty debit occurred" (L.find ledger offender.address = original);
      expect "live penalty transaction confirmed" (!fees = Z.zero && !confirmed = 0);
      expect "live penalty rejection missing"
        (!rejected = [T.hash tx, "validator was disqualified by evidence"]);
      expect "live penalty migration missing"
        (Option.is_some (Lwt_main.run (S.get_meta store (X.validator_penalty_key offender.address))));
      Lwt_main.run (S.commit_epoch_batch store "live-penalty");
      expect_ok "live penalty journal commit" (L.commit_journal ledger))

let () =
  Mirage_crypto_rng_unix.use_default ();
  with_store (run_registry_change "new-bond" [0; 1; 1; 1; 1; 1; 1] 0 T.ValidatorBond 1 true);
  with_store (run_registry_change "topup" [1; 1; 1; 1; 1; 1; 1] 0 T.ValidatorBond 1 true);
  with_store (run_registry_change "topup-cap" [1; 1; 1; 1; 1; 1; 1] 0 T.ValidatorBond 3 false);
  with_store (run_registry_change "ready" [2; 1; 1; 1; 1; 1; 1] 0 T.ValidatorReady 0 true);
  with_store (run_registry_change "ready-cap" [4; 1; 1; 1; 1; 1; 1] 0 T.ValidatorReady 0 false);
  with_store (run_registry_change "three-four" [100; 1; 1; 1] 3 T.ValidatorReady 0 false);
  with_store (run_registry_change "exit" [1; 1; 1; 1; 1; 1; 1] 0 T.ValidatorExit 0 true);
  with_store (run_registry_change "exit-cap" [3; 1; 1; 1; 1; 1; 1; 1] 7 T.ValidatorExit 0 false);
  with_store (run_registry_change "repair" [4; 1; 1; 1; 1; 1; 1] 1 T.ValidatorBond 1 true);
  with_store (run_registry_change ~historical:true "before-seatless"
    [1; 1; 1; 1; 1; 1; 1] 0 T.ValidatorBond 3 true);
  with_store (run_registry_change ~expiry:true "pulse-promotes-support"
    [2; 2; 2; 2; 1; 1; 1] 0 T.ValidatorBond 1 true);
  with_store (run_registry_change ~promotion:true "pulse-promoted"
    [2; 2; 2; 2; 3] 4 T.ValidatorReady 0 true);
  with_store (run_registry_change ~density:true "unequal-duty"
    [1; 1; 1; 1; 1; 1; 1] 0 T.ValidatorBond 1 true);
  with_store (run_evidence_capacity [4; 2; 2; 1; 1] [0] [true]);
  with_store (run_evidence_capacity [5; 2; 1; 1; 1] [0] [true]);
  with_store (run_evidence_capacity [3; 3; 2; 1; 1] [1; 4] [true; true]);
  with_store (run_old_slash_wait 3);
  with_store (run_old_slash_wait 4);
  with_store (run_old_slash_wait 2);
  with_store run_live_penalty;
  test_slash_reward_retention ();
  check_pending_choice ();
  check_eviction_update ();
  check_eviction_update_faults ();
  check_eviction_epochs ();
  check_eviction_registry ();
  check_eviction_state ();
  check_eviction_codec ();
  let args = Array.to_list Sys.argv in
  if args = [Sys.argv.(0); "--pending-repro"]
     || args = [Sys.argv.(0); "--pending-progress"] then begin
    let expect_rejection = args = [Sys.argv.(0); "--pending-repro"] in
    with_store (run_pending_guard ~expect_rejection);
    with_store (run_pending_guard ~at_start:true ~expect_rejection);
    print_endline (if expect_rejection then "status = reproduced test = pending_guard"
      else "status = pass test = pending_guard")
  end else begin
  with_store (run_pending_guard ~at_start:true ~expect_rejection:false);
  with_store (run_pending_guard ~expect_rejection:false);
  with_store run_weight_guard;
  if Array.to_list Sys.argv = [Sys.argv.(0); "--signed-set"] then begin
    with_store run_signed_set;
    print_endline "status = pass test = signed_set"
  end else begin
  with_store run_signed_set;
  with_store (run_slots ~transition:max_int);
  with_store (run_slots ~transition:92);
  with_store (run_slots ~transition:100);
  with_store (run_lifecycle ~ready_exec_mode:Octra_core.Rule_graph.Prior);
  with_store (run_lifecycle ~ready_exec_mode:Octra_core.Rule_graph.Active);
  with_store (run_lifecycle ~ready_exec_mode:Octra_core.Rule_graph.Active ~delay:1);
  with_store (run_lifecycle ~ready_exec_mode:Octra_core.Rule_graph.Active ~delay:2);
  with_store (run_ready_active ~delay:2);
  with_store (run_ready_active ~delay:3);
  with_store
    (run_manual_takeover_rejected
       ~validator_policy:Policy.Inactive
       ~epoch_id:10);
  let bonded_policy =
    Policy.of_env_exn (fun name ->
      if String.equal name Policy.env_name then Some "64" else None)
  in
  with_store
    (run_manual_takeover_rejected
       ~validator_policy:bonded_policy
       ~epoch_id:65);
  with_store run_slashing;
  with_store run_slash_rollback;
  with_store (run_certified_eviction ~historical:false);
  with_store (run_certified_eviction ~historical:true);
  print_endline "status = pass test = validator_lifecycle"
  end
  end