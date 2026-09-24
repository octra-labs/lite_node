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
    (List.length (Registry.candidates registry) = 5);
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
  expect "slashed candidate removed"
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
      (returning.Octra_core.Validator_admission.ready_epoch =
         (if delay = 2 && epoch = 203 then Some source else None)
       && fresh.Octra_core.Validator_admission.ready_epoch = None))
    (if delay = 2 then [201; 202; 203] else [201])

let run_slots ~transition store =
  let pending = Update.{
    activate_epoch = 136L;
    source_epoch = Some 128L;
    validators = [];
    weighted = true;
    fingerprint = "";
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

let () =
  Mirage_crypto_rng_unix.use_default ();
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
  print_endline "validator lifecycle tests passed"