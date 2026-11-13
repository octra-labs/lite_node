(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module E = Octra_core.Emission_policy
module ES = Octra_core.Emission_schedule
module X = Octra_core.Epoch_exec
module L = Octra_core.Ledger
module S = Octra_core.Store_irmin
module K = Octra_core.Sender_key_policy
module T = Octra_core.Transaction
module C = Octra_consensus.C_types
module H = Octra_consensus.C_hash
module R = Octra_node_runtime.Consensus_reward_attribution
module P = Octra_node_runtime.Consensus_profile
module F = Octra_node_runtime.Consensus_epoch_apply_finish_shell
module PC = Octra_node_runtime.P2p_config
module A = Octra_node_runtime.Consensus_epoch_apply_shared
module AC = Octra_node_runtime.Consensus_epoch_apply_checked
module V = Octra_node_runtime.Epoch_visibility
module VM = Octra_node_runtime.Consensus_epoch_vm_shell

let fail msg =
  failwith ("test_epoch_exec_reward_atomic: " ^ msg)

let expect label condition =
  if not condition then fail label

let raw_hex raw =
  let hex = "0123456789abcdef" in
  let out = Bytes.create (String.length raw * 2) in
  String.iteri
    (fun i value ->
      let code = Char.code value in
      Bytes.set out (i * 2) hex.[code lsr 4];
      Bytes.set out ((i * 2) + 1) hex.[code land 15])
    raw;
  Bytes.unsafe_to_string out

let expect_ok label = function
  | Ok value -> value
  | Error error -> fail (label ^ ": " ^ error)

let with_env name value f =
  let previous = Sys.getenv_opt name in
  Unix.putenv name value;
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv name (Option.value ~default:"" previous))
    f

let rec rm_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Sys.readdir path
      |> Array.iter (fun name -> rm_tree (Filename.concat path name));
      Unix.rmdir path
    end else
      Sys.remove path

let rec mkdir_p path =
  if not (Sys.file_exists path) then begin
    let parent = Filename.dirname path in
    if not (String.equal parent path) then mkdir_p parent;
    Unix.mkdir path 0o755
  end

let with_store f =
  let root =
    Filename.concat
      (Sys.getcwd ())
      ("runtime_data/reward_atomic/" ^ string_of_int (Unix.getpid ()))
  in
  rm_tree root;
  mkdir_p root;
  let store = Lwt_main.run (S.open_store (Filename.concat root "irmin")) in
  Fun.protect
    ~finally:(fun () ->
      ignore (Lwt_main.run (S.close store));
      rm_tree root)
    (fun () -> f store)

let env =
  {
    X.chain_id = "reward-atomic-test";
    epoch_id = 4;
    proposer_addr = "octRewardProposer";
    validator_addrs = [];
    validator_pubkeys = [];
    prev_state_root = "";
    epoch_ts = 40.;
    ready_state_root_at = None;
    ready_max_lag = -1;
  }

let fold standard_mode =
  X.{
    mode = Octra_core.Rule_graph.Active;
    ready_mode = Octra_core.Rule_graph.Active;
    ready_ref_mode = Octra_core.Rule_graph.Active;
    live_mode = Octra_core.Rule_graph.Active;
    seat_mode = Octra_core.Rule_graph.Active;
    open_mode = Octra_core.Rule_graph.Active;
    account_mode = Octra_core.Rule_graph.Active;
    standard_mode;
    plan_mode = Octra_core.Rule_graph.Prior;
    exit_mode = Octra_core.Rule_graph.Prior;
    ready_exec_mode = Octra_core.Rule_graph.Prior;
    program_mode = Octra_core.Rule_graph.Prior;
    program_overlap = false;
    fhe_work = Octra_core.Rule_graph.Prior;
    proof_exec = Octra_core.Rule_graph.Prior;
    proof_key = Octra_core.Rule_graph.Prior;
    vm_bytes = Octra_core.Rule_graph.Prior;
    program_records = Octra_core.Rule_graph.Prior;
    duty_marks = Octra_core.Rule_graph.Prior;
    wasm_float = Octra_core.Rule_graph.Prior;
    math = false;
    cap_mode = Octra_core.Set_fold.Prune;
    ready_config_hash = Some "ready";
    start = 1_334_000L;
    profile_start = 1_500_000L;
    parent = None;
    members = ["octNew"];
  }

let test_standard_gate () =
  let prior = fold Octra_core.Rule_graph.Prior in
  let active = fold Octra_core.Rule_graph.Active in
  expect "prior fold config"
    (X.set_fold_cfg prior = Octra_core.Set_fold.standard);
  expect "active fold config"
    (X.set_fold_cfg active = Octra_core.Set_fold.participating);
  expect "prior fold start" (X.set_fold_start prior = 1_334_000L);
  expect "active fold start" (X.set_fold_start active = 1_500_000L);
  expect "prior member source"
    (X.std_value prior ["octOld"] ["octNew"] = ["octOld"]);
  expect "active member source"
    (X.std_value active ["octOld"] ["octNew"] = ["octNew"])

let plan () =
  X.build_reward_plan
    ~fee_burn_active:false
    ~supply_retired:Z.zero
    ~validator_count:1
    ~emission_remaining:Z.zero
    ~confirmed_fees:(Z.of_int 10)
    ~prev_supply:Z.zero
  |> expect_ok "reward plan"

let test_reward_plan () =
  let proposer, validators =
    Octra_core.Reward_policy.split (Z.of_int 101)
    |> expect_ok "reward policy split"
  in
  expect "reward proposer share is seventy percent"
    (Z.equal proposer (Z.of_int 70));
  expect "reward validator share receives rounding"
    (Z.equal validators (Z.of_int 31));
  expect "reward policy identity binds divisor"
    (String.equal
       Octra_core.Reward_policy.consensus_id
       "reward:18198732:10000:7:10");
  let max = Octra_core.Denomination.max_supply in
  let final =
    X.build_reward_plan
      ~fee_burn_active:false
      ~supply_retired:Z.zero
      ~validator_count:3
      ~emission_remaining:(Z.of_int 7)
      ~confirmed_fees:(Z.of_int 5)
      ~prev_supply:(Z.sub max (Z.of_int 7))
    |> expect_ok "final reward"
  in
  expect "final mint exact" (Z.equal final.X.base_reward (Z.of_int 7));
  expect "final pool empty" (Z.equal final.new_emission_remaining Z.zero);
  expect "hard cap exact" (Z.equal final.new_total_supply max);
  let suffix =
    X.build_reward_plan
      ~fee_burn_active:false
      ~supply_retired:Z.zero
      ~validator_count:3
      ~emission_remaining:Z.zero
      ~confirmed_fees:(Z.of_int 5)
      ~prev_supply:max
    |> expect_ok "fee suffix"
  in
  expect "fee suffix has no mint" (Z.equal suffix.base_reward Z.zero);
  expect "fee suffix preserves supply" (Z.equal suffix.new_total_supply max);
  expect "fee suffix redistributes fees" (Z.equal suffix.total_reward (Z.of_int 5));
  expect "distribution exact"
    (Z.equal
       (Z.add suffix.proposer_total
          (Z.mul suffix.each_validator (Z.of_int 3)))
       suffix.total_reward);
  expect "pool above headroom rejected"
    (match X.build_reward_plan
       ~fee_burn_active:false
       ~supply_retired:Z.zero
       ~validator_count:1
       ~emission_remaining:(Z.of_int 8)
       ~confirmed_fees:Z.zero
       ~prev_supply:(Z.sub max (Z.of_int 7)) with
     | Error _ -> true
     | Ok _ -> false);
  expect "negative fees rejected"
    (match X.build_reward_plan
       ~fee_burn_active:false
       ~supply_retired:Z.zero
       ~validator_count:1
       ~emission_remaining:Z.zero
       ~confirmed_fees:(Z.neg Z.one)
       ~prev_supply:Z.zero with
     | Error _ -> true
     | Ok _ -> false);
  expect "empty validator set rejected"
    (match X.build_reward_plan
       ~fee_burn_active:false
       ~supply_retired:Z.zero
       ~validator_count:0
       ~emission_remaining:Z.zero
       ~confirmed_fees:Z.zero
       ~prev_supply:Z.zero with
     | Error _ -> true
     | Ok _ -> false)

let test_reward_env () =
  let implicit = X.default_reward env in
  expect "single validator reserve"
    (List.map
       (fun (validator : X.reward_validator) -> validator.address)
       implicit.X.validators
     = [env.X.proposer_addr]);
  let explicit =
    X.default_reward { env with X.validator_addrs = ["octA"; "octB"] } in
  expect "explicit validators preserved"
    (List.map
       (fun (validator : X.reward_validator) -> validator.address)
       explicit.X.validators
     = ["octA"; "octB"])

let test_weighted_reward_credits () =
  let plan =
    X.build_reward_plan_with_base
      ~fee_burn_active:false
      ~supply_retired:Z.zero
      ~base_reward:(Z.of_int 100)
      ~validator_count:2
      ~emission_remaining:(Z.of_int 100)
      ~confirmed_fees:Z.zero
      ~prev_supply:Z.zero
    |> expect_ok "weighted reward plan"
  in
  let reward =
    X.{
      proposer_addr = "octA";
      proposer_public_key = None;
      validators = [
        { address = "octB"; public_key = None; weight = Z.one };
        { address = "octA"; public_key = None; weight = Z.of_int 3 };
      ];
    }
  in
  let credits =
    X.reward_credits reward plan
    |> expect_ok "weighted reward credits"
  in
  expect "weighted reward total"
    (List.fold_left
       (fun total (credit : X.reward_credit) -> Z.add total credit.amount)
       Z.zero
       credits
     = Z.of_int 100);
  expect "weighted reward addresses"
    (List.map (fun (credit : X.reward_credit) -> credit.address) credits
     = ["octA"; "octB"]);
  let first = List.nth credits 0 in
  let second = List.nth credits 1 in
  expect "weighted proposer validator" (first.proposer && first.validator);
  expect "weighted proposer amount" (first.amount = Z.of_int 93);
  expect "weighted validator amount"
    (not second.proposer && second.validator && second.amount = Z.of_int 7)

let test_reward_finality_binding () =
  let standard_epoch = 1_500_000L in
  let validator address byte =
    C.{ address; pubkey = String.make 32 byte }
  in
  let validators = [validator "octA" '\x01'; validator "octB" '\x02'] in
  let validator_set = C.make_validator_set validators in
  let header = C.{
    proto_version = proto_version_current;
    chain_id = "octra-devnet-9871-cluster";
    epoch_id = Int64.pred standard_epoch;
    prev_state_root = String.make 32 '\x03';
    tx_list_hash = String.make 32 '\x04';
    receipt_root = H.receipt_root [];
    proposed_state_root = String.make 32 '\x05';
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = "octA";
    txid_hi = 0L;
    ts = 80.;
  } in
  let proposal_id = H.proposal_id header in
  let vote = C.{
    chain_id = header.chain_id;
    epoch_id = header.epoch_id;
    round = 1;
    vote_type = Precommit;
    proposal_id;
    validator = "octB";
    signature = String.make 64 '\x06';
  } in
  let parent = C.{
    validator_set;
    certificate = {
      chain_id = header.chain_id;
      epoch_id = header.epoch_id;
      commit_round = 1;
      header;
      proposal_id;
      precommits = [vote];
    };
  } in
  let supplied = R.of_parent_commit parent |> expect_ok "reward source" in
  let finalize = C.{
    chain_id = header.chain_id;
    epoch_id = standard_epoch;
    commit_round = 1;
    header = { header with epoch_id = standard_epoch };
    proposal_id;
    precommits = [];
    parent_commit = Some parent;
  } in
  expect "linked reward source accepted"
    (R.bind_finality ~validator_set finalize supplied = Ok supplied);
  expect "prior missing reward parent accepted"
    (R.bind_finality
       ~validator_set
       { finalize with epoch_id = header.epoch_id; header; parent_commit = None }
       supplied
     = Ok supplied);
  expect "missing reward parent rejected"
    (Result.is_error
       (R.bind_finality
          ~validator_set
          { finalize with parent_commit = None }
          supplied));
  expect "forged reward source rejected"
    (Result.is_error
       (R.bind_finality
          ~validator_set
          finalize
          { supplied with proposer_addr = "octB" }))

let test_set_reward () =
  let module Policy = Octra_consensus.C_quorum_policy in
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch_id = 1_900_000L in
  let policy = Policy.with_seatless_epoch (Policy.for_chain chain_id) (Some epoch_id) in
  let entries = List.mapi (fun index weight ->
    C.{address = "oct" ^ String.make 1 (Char.chr (65 + index));
      pubkey = String.make 32 (Char.chr (index + 1))}, Z.of_int weight) [4; 3; 2; 1] in
  let validator_set = C.make_weighted_validator_set entries |> expect_ok "reward weighted set" in
  let make_parent at signers =
    let header = C.{proto_version = proto_version_current; chain_id; epoch_id = Int64.pred at;
      prev_state_root = String.make 32 '\001'; tx_list_hash = H.tx_list_hash [];
      receipt_root = H.receipt_root []; proposed_state_root = String.make 32 '\002';
      parent_commit_hash = Octra_net.Hash_domain.nil_hash; creator_addr = "octA";
      txid_hi = 0L; ts = 80.} in
    let proposal_id = H.proposal_id header in
    C.{validator_set; certificate = {
      chain_id; epoch_id = header.epoch_id; commit_round = 1; header; proposal_id;
      precommits = List.map (fun validator -> C.{chain_id; epoch_id = header.epoch_id;
        round = 1; vote_type = Precommit; proposal_id; validator;
        signature = String.make 64 '\003'}) signers;
    }} in
  let resolve at parent = R.resolve_for_epoch ~policy ~chain_id ~epoch_id:at
    ~proposer_addr:"ignored" ~validator_pubkeys:[] (Some parent) in
  let short = make_parent epoch_id ["octA"; "octB"] in
  let long = make_parent epoch_id ["octA"; "octB"; "octC"] in
  let reward = resolve epoch_id short |> expect_ok "set reward" in
  expect "set reward depended on certificate signers" (resolve epoch_id long = Ok reward);
  let author = C.{address = "octZ"; pubkey = String.make 32 '\009'} in
  let authors = [author, Z.one] in
  let with_authors authors = {short with validator_set =
    {validator_set with authors = Some (Int64.pred epoch_id, authors)}} in
  let relief = with_authors authors in
  expect "set reward dropped relief metadata" (resolve epoch_id relief = Ok reward);
  let relief = {relief with certificate = {relief.certificate with
    header = {relief.certificate.header with creator_addr = author.address}}} in
  let outside = resolve epoch_id relief |> expect_ok "set reward prior author" in
  expect "set reward added prior author to voters"
    (outside.validators = reward.validators && outside.proposer_addr = author.address
      && outside.proposer_public_key = Some (Base64.encode_exn author.pubkey));
  List.iter (fun altered ->
    expect "set reward accepted malformed relief authors" (Result.is_error (resolve epoch_id altered))) [
    with_authors [];
    with_authors [author, Z.zero];
    with_authors (List.hd entries :: authors);
    with_authors [{author with pubkey = (fst (List.hd entries)).pubkey}, Z.one];
    {short with validator_set = {validator_set with authors = Some (epoch_id, authors)}};
  ];
  expect "set reward omitted member weight"
    (List.map (fun (member : X.reward_validator) -> member.address, member.weight) reward.validators
      = List.map (fun (member, weight) -> member.C.address, weight) entries);
  let previous = Int64.pred epoch_id in
  let before = make_parent previous ["octA"; "octB"] in
  expect "set reward changed historical source" (resolve previous before = R.of_parent_commit before);
  expect "set reward accepted another parent epoch" (Result.is_error (resolve epoch_id before));
  expect "set reward accepted undeclared weights"
    (Result.is_error (resolve epoch_id {short with validator_set = C.make_validator_set validator_set.validators}));
  List.iter (fun altered ->
    expect "set reward accepted malformed parent set"
      (Result.is_error (resolve epoch_id {short with validator_set = altered}))) [
    {validator_set with n = validator_set.n + 1};
    {validator_set with quorum = validator_set.quorum + 1};
    {validator_set with total_weight = Z.succ validator_set.total_weight};
    {validator_set with weights = ("octA", Z.zero) :: List.remove_assoc "octA" validator_set.weights};
    {validator_set with weights = ("octZ", Z.one) :: validator_set.weights};
    {validator_set with validators = List.hd validator_set.validators :: validator_set.validators};
  ];
  let finalize = C.{chain_id; epoch_id; commit_round = 1;
    header = {short.certificate.header with epoch_id}; proposal_id = short.certificate.proposal_id;
    precommits = []; parent_commit = Some short} in
  expect "set reward finality did not bind full set"
    (R.bind_finality ~policy ~validator_set finalize reward = Ok reward);
  expect "set reward accepted certificate-only attribution"
    (Result.is_error (R.bind_finality ~policy ~validator_set finalize
      (R.of_parent_commit short |> expect_ok "old reward")));
  expect "set reward accepted missing parent before standard"
    (Result.is_error (R.bind_finality ~policy ~validator_set
      {finalize with chain_id = "reward-unconfigured"; parent_commit = None} reward));
  expect "set reward resolved missing parent before standard"
    (Result.is_error (R.resolve_for_epoch ~policy ~chain_id:"reward-unconfigured"
      ~epoch_id ~proposer_addr:"octA" ~validator_pubkeys:[] None));
  expect "set reward accepted old finality version"
    (Result.is_error (R.bind_finality ~policy ~validator_set
      {finalize with header = {finalize.header with proto_version = C.proto_version_parent_legacy}} reward));
  List.iter (fun base -> List.iter (fun fees ->
    let base = Z.of_string base and fees = Z.of_int fees in
    let plan = X.build_reward_plan_with_base ~fee_burn_active:true ~supply_retired:Z.zero
      ~base_reward:base ~validator_count:4 ~emission_remaining:base
      ~confirmed_fees:fees ~prev_supply:(Z.sub Octra_core.Denomination.max_supply base)
      |> expect_ok "set reward plan" in
    let credits = X.reward_credits reward plan |> expect_ok "set reward credits" in
    let total = List.fold_left (fun sum (credit : X.reward_credit) -> Z.add sum credit.amount) Z.zero credits in
    expect "set reward did not conserve issued units"
      (Z.equal total (Z.sub (Z.add base fees) plan.fees_burned));
    let pool = Z.add (Z.mul (Z.of_int 4) plan.each_validator) plan.remainder in
    let shares = List.map (fun (_, weight) -> Z.div (Z.mul pool weight) (Z.of_int 10)) entries in
    let dust = Z.sub pool (List.fold_left Z.add Z.zero shares) in
    expect "set reward rounding exceeds member count" (Z.geq dust Z.zero && Z.lt dust (Z.of_int 4));
    List.iter (fun (member, weight) ->
      let amount = List.find_opt (fun (credit : X.reward_credit) -> credit.address = member.C.address) credits
        |> Option.fold ~none:Z.zero ~some:(fun (credit : X.reward_credit) -> credit.amount) in
      let share = Z.div (Z.mul pool weight) (Z.of_int 10) in
      expect "set reward weight division differs"
        (Z.equal amount
          (if member.address = reward.proposer_addr then
            Z.add share (Z.add (Z.sub plan.proposer_total plan.remainder) dust)
           else share))) entries;
    expect "set reward changed with member order"
      (X.reward_credits {reward with validators = List.rev reward.validators} plan = Ok credits);
    let scaled = List.map (fun (member : X.reward_validator) ->
      {member with weight = Z.mul member.weight (Z.shift_left Z.one 70)}) reward.validators in
    expect "set reward lost large weight precision"
      (X.reward_credits {reward with validators = scaled} plan = Ok credits))
    [0; 1; 4; 5; 37]) ["0"; "1"; "3"; "100"; "999999999999000"];
  let apply reward = with_store (fun store ->
    let ledger = L.create store in
    expect "set reward payer setup" (L.add_account ledger "octA" (Z.of_int 1000) = Ok ());
    expect "set reward fee debit" (L.debit ledger "octA" (Z.of_int 37) 1 = Ok ());
    let plan = X.build_reward_plan_with_base ~fee_burn_active:true
      ~supply_retired:(Z.sub Octra_core.Denomination.max_supply (Z.of_int 1100))
      ~base_reward:(Z.of_int 100) ~validator_count:4 ~emission_remaining:(Z.of_int 100)
      ~confirmed_fees:(Z.of_int 37) ~prev_supply:(Z.of_int 1000) |> expect_ok "set reward apply plan" in
    let backend = X.make_live_backend ~emission_policy:E.Allow store ledger in
    Lwt_main.run (X.apply_epoch_footer_with_reward ~reward ~backend ~env ~plan);
    List.iter (fun (address, balance) ->
      expect "set reward applied balance differs"
        (Z.equal (L.find ledger address).L.balance (Z.of_int balance)))
      ["octA", 1072; "octB", 11; "octC", 7; "octD", 3];
    expect "set reward applied supply differs" (Z.equal (L.get_total_supply ledger) (Z.of_int 1093));
    expect "set reward retired supply differs"
      (Lwt_main.run (S.get_meta store ES.retired_key)
        = Some (Z.to_string (Z.sub Octra_core.Denomination.max_supply (Z.of_int 1093))));
    Lwt_main.run (L.flush_dirty_lwt ledger);
    List.iter (fun (member : X.reward_validator) ->
      let account = Lwt_main.run (S.get_account store member.X.address) |> Option.get in
      expect "set reward saved key differs" (account.public_key = member.public_key)) reward.validators;
    Lwt_main.run (S.state_hash store)) in
  expect "set reward applied root depends on certificate"
    (apply reward = apply (resolve epoch_id long |> expect_ok "set reward long source"))

let test_validator_unbond () =
  let address = "oct4ZKCyAUbLCrHEZzNkqysPokEtqQeC1y5x4YK7Jm4vXSC" in
  let registry =
    Octra_core.Validator_registry.of_yojson
      (`Assoc [
        "standard", `String Octra_core.Validator_policy.standard_name;
        "candidates", `List [
          `Assoc [
            "address", `String address;
            "consensus_pubkey",
              `String (Base64.encode_exn (String.make 32 '\x07'));
            "bond", `String "5000000";
            "bonded_epoch", `String "2";
            "ready_epoch", `String "3";
            "exit_epoch", `Null;
          ];
        ];
        "slashes", `List [];
      ])
    |> expect_ok "validator registry"
  in
  let exiting =
    Octra_core.Validator_registry.request_exit
      ~epoch:10L
      ~address
      registry
    |> expect_ok "validator exit"
  in
  let parameters = {
    Octra_core.Validator_policy.parameters with
    Octra_core.Validator_entry.unbonding_epochs = 8L;
  } in
  expect "early validator withdrawal rejected"
    (Result.is_error
       (Octra_core.Validator_registry.withdraw
          parameters
          ~current_epoch:17L
          ~active_addresses:[]
          ~address
          exiting));
  expect "active validator withdrawal rejected"
    (Result.is_error
       (Octra_core.Validator_registry.withdraw
          parameters
          ~current_epoch:18L
          ~active_addresses:[address]
          ~address
          exiting));
  let withdrawn, amount =
    Octra_core.Validator_registry.withdraw
      parameters
      ~current_epoch:18L
      ~active_addresses:[]
      ~address
      exiting
    |> expect_ok "validator withdrawal"
  in
  expect "validator bond returned" (Z.equal amount (Z.of_int 5_000_000));
  expect "validator removed after withdrawal"
    (Octra_core.Validator_registry.find address withdrawn = None)

let test_consensus_standard () =
  let getenv _ = None in
  let compat = P.compat_hash getenv in
  let first = P.standard_hash ~chain_id:"standard-a" ~epoch:0 getenv in
  let repeated = P.standard_hash ~chain_id:"standard-a" ~epoch:0 getenv in
  let other = P.standard_hash ~chain_id:"standard-b" ~epoch:0 getenv in
  expect "consensus standard name"
    (String.equal P.standard "octra_consensus");
  expect "compat hash golden"
    (String.equal
       (raw_hex compat)
       "cf3813dc7d75a9df5b696817677f7aafc94cbbb737cef9f9a64edb1b2d04006b");
  expect "consensus standard hash is stable" (String.equal first repeated);
  expect "consensus standard binds chain" (not (String.equal first other));
  expect "activation graph binds chain"
    (not
       (String.equal
          (P.activation_graph_hash ~chain_id:"standard-a")
          (P.activation_graph_hash ~chain_id:"standard-b")))

let test_consensus_cutover () =
  let chain_id = "octra-devnet-9871-cluster" in
  let getenv _ = None in
  let compat = P.compat_hash getenv in
  let full = P.standard_hash ~chain_id ~epoch:1_500_000 getenv in
  expect "wire hashes differ" (not (String.equal compat full));
  expect "wire hash stays compatible before activation"
    (String.equal
       compat
       (P.hash ~chain_id ~epoch:1_499_999 getenv));
  expect "wire hash changes at activation"
    (String.equal
       full
       (P.hash ~chain_id ~epoch:1_500_000 getenv));
  expect "wire hash stays full after activation"
    (String.equal
       full
       (P.hash ~chain_id ~epoch:1_500_001 getenv));
  expect "wire switch waits for final prior epoch"
    (not (P.switch_after ~chain_id ~applied_epoch:1_499_998));
  expect "wire switch follows final prior epoch"
    (P.switch_after ~chain_id ~applied_epoch:1_499_999);
  expect "wire switch runs once"
    (not (P.switch_after ~chain_id ~applied_epoch:1_500_000));
  let no_swarm =
    F.switch_profile
      ~swarm:None
      ~chain_id
      ~epoch:1_500_000
      ~env:getenv
    |> Lwt_main.run
    |> Result.get_ok
  in
  expect "wire switch without swarm installs full profile"
    (String.equal no_swarm full);
  expect "unrelated chain stays compatible"
    (String.equal
       compat
       (P.hash ~chain_id:"octra-mainnet" ~epoch:max_int getenv));
  let switched = ref [] in
  let run applied_epoch =
    F.cutover
      ~switch:(fun epoch ->
        switched := epoch :: !switched;
        Lwt.return_unit)
      ~chain_id
      ~applied_epoch
    |> Lwt_main.run
  in
  run 1_499_998;
  run 1_499_999;
  run 1_500_000;
  expect "cutover switches only the activation epoch"
    (!switched = [1_500_000])

let test_integer_work_gate () =
  with_store (fun store ->
    let make proof_mode =
      VM.make_live_contract_ctx
        {
          value_journal =
            Octra_vm.Value_journal.create ~balance:(fun _ -> Z.zero);
          program_journal = Octra_vm.Program_journal.create ();
          trusted_program_keys = Octra_vm.Program_trust.empty;
          store;
          get_fhe_pubkey = (fun _ -> None);
          proof_mode;
          fhe_work = Octra_core.Rule_graph.Prior;
          proof_exec = Octra_core.Rule_graph.Prior;
          proof_key = Octra_core.Rule_graph.Prior;
          vm_bytes = Octra_core.Rule_graph.Prior;
          wasm_float = Octra_core.Rule_graph.Prior;
          math = false;
          object_cost = false;
          current_epoch = 0;
          epoch_time_ms = 0L;
          tree_hash = "";
          node_id = "";
          tx_hash = "";
        }
    in
    let prior = make Octra_core.Rule_graph.Prior in
    let active = make Octra_core.Rule_graph.Active in
    expect "prior integer work mode"
      (prior.Octra_vm.Contract_vm.int_work = Octra_vm.Int_work.Prior);
    expect "active integer work mode"
      (active.Octra_vm.Contract_vm.int_work = Octra_vm.Int_work.Active))

let test_epoch_serialization () =
  let visibility = V.create () in
  let release, wake = Lwt.wait () in
  let events = ref [] in
  let add event = events := event :: !events in
  let first =
    V.try_apply visibility (fun () ->
      let open Lwt.Syntax in
      add "first-start";
      let* () = release in
      add "first-end";
      Lwt.return_unit)
  in
  let second =
    V.try_apply visibility (fun () ->
      add "second";
      Lwt.return_unit)
  in
  expect "second epoch apply waits" (List.rev !events = ["first-start"]);
  Lwt.wakeup_later wake ();
  let first_result, second_result = Lwt_main.run (Lwt.both first second) in
  expect "first epoch apply completes"
    (match first_result with V.Applied () -> true | V.Busy -> false);
  expect "waiting epoch apply is superseded"
    (match second_result with V.Busy -> true | V.Applied () -> false);
  expect "superseded epoch payload is discarded"
    (List.rev !events = ["first-start"; "first-end"]);
  expect "epoch visibility reopens" (not (V.is_applying visibility));
  let third =
    V.try_apply visibility (fun () ->
      add "third";
      Lwt.return_unit)
    |> Lwt_main.run
  in
  expect "fresh epoch apply completes"
    (match third with V.Applied () -> true | V.Busy -> false);
  expect "fresh epoch payload runs"
    (List.rev !events = ["first-start"; "first-end"; "third"])

let busy_case next =
  let reads = ref 0 in
  let retried = ref false in
  let deps = AC.{
    head = (fun () ->
      incr reads;
      if !reads = 1 then 10 else next);
    set_current_epoch = (fun _ -> ());
    catchup_active = (fun () -> false);
    queue_gap = (fun ~active:_ ~target_epoch:_ ~reason:_ -> fail "busy gap");
    clear_state_attested = (fun () -> ());
    log_already = (fun ~current_epoch:_ ~head:_ -> ());
    log_defer = (fun _ _ -> ());
    preflight = (fun () -> Ok ());
    defer = (fun _ -> ());
    apply = (fun () -> Lwt.return AC.Apply_busy);
    retry = (fun () ->
      retried := true;
      Lwt.return_unit);
  } in
  AC.run deps ~consensus_mode:true ~current_epoch:11 |> Lwt_main.run;
  !retried

let test_epoch_busy_gate () =
  expect "applied epoch needs no retry" (not (busy_case 11));
  expect "unchanged head retries" (busy_case 10);
  expect "advanced head retries" (busy_case 12)

let test_upgrade_ready_refresh () =
  let old_hash = String.make 32 '\x01' in
  let new_hash = String.make 32 '\x02' in
  let binary_hash = String.make 32 '\x03' in
  let config_hash = ref old_hash in
  let plan = Octra_net.P2p_upgrade_plan.{
    activate_epoch = 20L;
    target_binary_hash = binary_hash;
    target_config_hash = new_hash;
    rollback = None;
  } in
  let config = PC.{
    config_hash = old_hash;
    binary_hash;
    require_binary_hash = true;
    upgrade_plan = Some plan;
    profile_plan = [];
    handshake_allowed_pubkeys = [];
    validator_pubkeys = [];
  } in
  let ready =
    PC.upgrade_ready_checker
      ~log_blocked:(fun _ -> ())
      ~epoch:(fun () -> 20L)
      ~consensus_config_hash:(fun () -> !config_hash)
      config
  in
  expect "upgrade readiness rejects the startup hash" (not (ready ()));
  config_hash := new_hash;
  expect "upgrade readiness reads the switched hash" (ready ())

let test_reward_properties () =
  let max = Octra_core.Denomination.max_supply in
  let divisor = X.emission_divisor in
  let floor = X.emission_floor in
  let threshold = Z.mul divisor floor in
  let remaining_values = [
    Z.zero;
    Z.one;
    Z.pred floor;
    floor;
    Z.succ floor;
    Z.pred divisor;
    divisor;
    Z.succ divisor;
    Z.pred threshold;
    threshold;
    Z.succ threshold;
    Z.div max (Z.of_int 2);
    Z.mul (Z.of_int 900_000_000) Octra_core.Denomination.units_per_oct;
  ] in
  let validator_counts = [1; 2; 3; 4; 5; 7; 10] in
  List.iter
    (fun remaining ->
       let supply = Z.sub max remaining in
       List.iter
         (fun validator_count ->
            let fees = Z.of_int (validator_count * 17 + 3) in
            let plan =
              X.build_reward_plan
                ~fee_burn_active:false
                ~supply_retired:Z.zero
                ~validator_count
                ~emission_remaining:remaining
                ~confirmed_fees:fees
                ~prev_supply:supply
              |> expect_ok "reward property plan"
            in
            expect "base reward nonnegative" (Z.sign plan.base_reward >= 0);
            expect "base reward within limit"
              (Z.leq plan.base_reward remaining);
            expect "positive pool mints"
              (Z.equal remaining Z.zero || Z.gt plan.base_reward Z.zero);
            expect "empty pool does not mint"
              (not (Z.equal remaining Z.zero) || Z.equal plan.base_reward Z.zero);
            expect "reserve conserved"
              (Z.equal
                 (Z.add plan.new_total_supply plan.new_emission_remaining)
                 (Z.add supply remaining));
            expect "reward total exact"
              (Z.equal plan.total_reward (Z.add plan.base_reward fees));
            expect "reward credits exact"
              (Z.equal
                 (Z.add
                    plan.proposer_total
                    (Z.mul plan.each_validator (Z.of_int validator_count)))
                 plan.total_reward))
         validator_counts)
    remaining_values

let active_plan ~fees =
  let max = Octra_core.Denomination.max_supply in
  let prev_supply = Z.of_int 100 in
  X.build_reward_plan_with_base
    ~fee_burn_active:true
    ~supply_retired:(Z.sub max prev_supply)
    ~base_reward:Z.zero
    ~validator_count:1
    ~emission_remaining:Z.zero
    ~confirmed_fees:(Z.of_int fees)
    ~prev_supply
  |> expect_ok "active reward plan"

let test_fee_burn_parity () =
  let plans = List.init 5 (fun _ -> active_plan ~fees:25) in
  let first = List.hd plans in
  expect "fee burn exact" (Z.equal first.X.fees_burned (Z.of_int 5));
  expect "fee reward exact" (Z.equal first.fees_rewarded (Z.of_int 20));
  expect "net supply exact" (Z.equal first.new_total_supply (Z.of_int 95));
  expect "retired supply exact"
    (Z.equal
       first.new_supply_retired
       (Z.sub Octra_core.Denomination.max_supply (Z.of_int 95)));
  expect "validator parity"
    (List.for_all (fun plan -> plan = first) plans)

let test_fee_burn_epoch_accounting () =
  with_store (fun store ->
    let ledger = L.create store in
    expect "burn account added"
      (L.add_account ledger env.X.proposer_addr (Z.of_int 100) = Ok ());
    expect "fee debit"
      (L.debit ledger env.X.proposer_addr (Z.of_int 25) 1 = Ok ());
    let backend = X.make_live_backend ~emission_policy:E.Allow store ledger in
    let plan = active_plan ~fees:25 in
    Lwt_main.run (X.apply_epoch_footer ~backend ~env ~plan);
    expect "public ledger accounting"
      (Z.equal
         (L.find ledger env.X.proposer_addr).L.balance
         (Z.of_int 95));
    expect "public supply accounting"
      (Z.equal (L.get_total_supply ledger) (Z.of_int 95));
    expect "exact supply accounting"
      (Lwt_main.run (S.get_meta store "total_supply") = Some "95");
    expect "retired supply accounting"
      (Lwt_main.run (S.get_meta store ES.retired_key)
       = Some
           (Z.to_string
              (Z.sub Octra_core.Denomination.max_supply (Z.of_int 95)))))

let test_policy () =
  expect "allow ignores absent metadata"
    (E.check_remaining E.Allow None = Ok ());
  expect "allow ignores malformed metadata"
    (E.check_remaining E.Allow (Some "bad") = Ok ());
  expect "guard accepts zero"
    (E.check_remaining E.Guard (Some "0") = Ok ());
  expect "guard accepts absent reserve as zero"
    (E.check_remaining E.Guard None = Ok ());
  expect "guard rejects positive pool"
    (match E.check_remaining E.Guard (Some "1") with Error _ -> true | Ok () -> false);
  expect "guard rejects malformed metadata"
    (match E.check_remaining E.Guard (Some "bad") with Error _ -> true | Ok () -> false);
  expect "guard accepts zero reward"
    (E.check_reward E.Guard Z.zero = Ok ());
  expect "guard rejects base reward"
    (match E.check_reward E.Guard Z.one with Error _ -> true | Ok () -> false);
  let state =
    E.state_of_meta
      ~emission_remaining:(Some "30")
      ~total_supply:(Some "70")
    |> expect_ok "supply state"
  in
  expect "supply state values"
    (Z.equal state.emission_remaining (Z.of_int 30)
     && Z.equal state.total_supply (Z.of_int 70));
  let legacy_state =
    E.state_of_meta
      ~emission_remaining:None
      ~total_supply:(Some "70")
    |> expect_ok "legacy supply state"
  in
  expect "legacy reserve is zero"
    (Z.equal legacy_state.emission_remaining Z.zero
     && Z.equal legacy_state.total_supply (Z.of_int 70));
  expect "legacy total accepted under guard"
    (E.resolve_total
       ~policy:E.Guard
       ~stored:None
       ~legacy:(Some "70")
       ~public_supply:(Z.of_int 60)
     = Ok (Z.of_int 70));
  expect "legacy total rejected without guard"
    (match
       E.resolve_total
         ~policy:E.Allow
         ~stored:None
         ~legacy:(Some "70")
         ~public_supply:(Z.of_int 60)
     with
     | Error _ -> true
     | Ok _ -> false);
  expect "legacy total below public rejected"
    (match
       E.resolve_total
         ~policy:E.Guard
         ~stored:None
         ~legacy:(Some "59")
         ~public_supply:(Z.of_int 60)
     with
     | Error _ -> true
     | Ok _ -> false);
  expect "hidden supply exact"
    (E.hidden_supply
       ~total_supply:(Z.of_int 70)
       ~public_supply:(Z.of_int 45)
     = Ok (Z.of_int 25));
  expect "public supply overflow rejected"
    (match
       E.hidden_supply
         ~total_supply:(Z.of_int 70)
         ~public_supply:(Z.of_int 71)
     with
     | Error _ -> true
     | Ok _ -> false);
  expect "missing supply metadata rejected"
    (match
       E.state_of_meta
         ~emission_remaining:(Some "0")
         ~total_supply:None
     with
     | Error _ -> true
     | Ok _ -> false)

let test_credit_failure () =
  with_store (fun store ->
    let ledger = L.create store in
    begin match L.add_account ledger env.X.proposer_addr (Z.of_int 100) with
    | Ok () -> ()
    | Error error -> fail error
    end;
    Lwt_main.run (L.flush_dirty_lwt ledger);
    let base = X.make_live_backend ~emission_policy:E.Allow store ledger in
    let ops = { base.X.ops with credit = (fun _ _ -> Error "injected credit failure") } in
    let backend = { base with X.ops } in
    let before = L.find_opt ledger env.X.proposer_addr in
    let failed =
      try
        ignore (Lwt_main.run (X.apply_epoch_footer ~backend ~env ~plan:(plan ())));
        false
      with Failure _ -> true
    in
    expect "credit failure propagated" failed;
    expect "balance unchanged"
      (L.find_opt ledger env.X.proposer_addr = before);
    expect "current epoch not written"
      (Lwt_main.run (S.get_meta store "current_epoch") = None))

let run_empty_epoch backend =
  X.run
    ~backend
    ~env
    ~txs:[]
    ~process_tx:(fun ~backend:_ ~env:_ _ -> Lwt.return (Ok Z.zero))
  |> Lwt_main.run

let test_legacy_zero_epoch () =
  with_store (fun store ->
    let ledger = L.create store in
    expect "legacy account added"
      (L.add_account ledger env.X.proposer_addr (Z.of_int 70) = Ok ());
    Lwt_main.run (L.flush_dirty_lwt ledger);
    let backend =
      X.make_live_backend
        ~emission_policy:E.Guard
        ~legacy_total_supply:"70"
        store
        ledger
    in
    expect "legacy epoch accepted"
      (match run_empty_epoch backend with Ok _ -> true | Error _ -> false);
    expect "legacy balance unchanged"
      ((L.find ledger env.X.proposer_addr).L.balance = Z.of_int 70);
    expect "legacy supply unchanged"
      (Lwt_main.run (S.get_meta store "total_supply") = Some "70");
    expect "legacy reserve normalized"
      (Lwt_main.run (S.get_meta store "emission_remaining") = Some "0");
    expect "legacy supply normalized"
      (Lwt_main.run (S.get_meta store "total_supply") = Some "70");
    expect "legacy epoch advanced"
      (Lwt_main.run (S.get_meta store "current_epoch") = Some "5"))

let test_guard_allows_inactive_epoch () =
  with_store (fun store ->
    let ledger = L.create store in
    expect "guard account added"
      (L.add_account ledger env.X.proposer_addr (Z.of_int 69) = Ok ());
    Lwt_main.run (L.flush_dirty_lwt ledger);
    Lwt_main.run (S.set_meta store "total_supply" "69");
    Lwt_main.run (S.set_meta store "emission_remaining" "1");
    let backend = X.make_live_backend ~emission_policy:E.Guard store ledger in
    expect "inactive epoch accepted"
      (match run_empty_epoch backend with Ok _ -> true | Error _ -> false);
    expect "guard balance unchanged"
      ((L.find ledger env.X.proposer_addr).L.balance = Z.of_int 69);
    expect "guard reserve unchanged"
      (Lwt_main.run (S.get_meta store "emission_remaining") = Some "1");
    expect "guard supply unchanged"
      (Lwt_main.run (S.get_meta store "total_supply") = Some "69");
    expect "guard epoch advanced"
      (Lwt_main.run (S.get_meta store "current_epoch") = Some "5"))

let run_security_curve_epoch () =
  with_store (fun store ->
    let ledger = L.create store in
    let reserve = Z.of_int ES.duration_epochs in
    expect "curve account added"
      (L.add_account ledger env.X.proposer_addr (Z.of_int 100) = Ok ());
    Lwt_main.run (L.flush_dirty_lwt ledger);
    Lwt_main.run (S.set_meta store "total_supply" "100");
    Lwt_main.run
      (S.set_meta store "emission_remaining" (Z.to_string reserve));
    let backend =
      X.make_live_backend
        ~emission_policy:E.Allow
        ~emission_schedule:(ES.Security_curve { activation_epoch = env.epoch_id })
        store
        ledger
    in
    let applied =
      match run_empty_epoch backend with
      | Ok value -> value
      | Error error -> fail error
    in
    let meta key = Lwt_main.run (S.get_meta store key) in
    applied.X.post_state_root,
    (L.find ledger env.X.proposer_addr).L.balance,
    meta ES.standard_key,
    meta ES.activation_key,
    meta ES.initial_key,
    meta "emission_remaining")

let test_security_curve_epoch_parity () =
  let left = run_security_curve_epoch () in
  let right = run_security_curve_epoch () in
  let left_root, left_balance, profile, activation, initial, remaining = left in
  let right_root, right_balance, _, _, _, _ = right in
  expect "curve root parity" (String.equal left_root right_root);
  expect "curve balance parity" (Z.equal left_balance right_balance);
  expect "curve first reward" (Z.equal left_balance (Z.of_int 101));
  expect "curve standard persisted" (profile = Some "security_curve");
  expect "curve activation persisted"
    (activation = Some (string_of_int env.epoch_id));
  expect "curve initial persisted"
    (initial = Some (string_of_int ES.duration_epochs));
  expect "curve reserve decremented"
    (remaining = Some (string_of_int (ES.duration_epochs - 1)))

let run_zero_reserve_cutover () =
  with_store (fun store ->
    let ledger = L.create store in
    let reserve =
      Z.add
        (Z.mul (Z.of_int ES.duration_epochs) (Z.of_int 3))
        (Z.of_int 7)
    in
    let total =
      Z.sub Octra_core.Denomination.max_supply reserve
    in
    expect "cutover account added"
      (L.add_account ledger env.X.proposer_addr (Z.of_int 100) = Ok ());
    Lwt_main.run (L.flush_dirty_lwt ledger);
    Lwt_main.run (S.set_meta store "total_supply" (Z.to_string total));
    Lwt_main.run (S.set_meta store "emission_remaining" "0");
    let backend =
      X.make_live_backend
        ~emission_policy:E.Allow
        ~emission_schedule:(ES.Security_curve { activation_epoch = env.epoch_id })
        store
        ledger
    in
    let applied =
      match run_empty_epoch backend with
      | Ok value -> value
      | Error error -> fail error
    in
    let meta key = Lwt_main.run (S.get_meta store key) in
    applied.X.post_state_root,
    reserve,
    total,
    (L.find ledger env.X.proposer_addr).L.balance,
    meta ES.standard_key,
    meta ES.activation_key,
    meta ES.initial_key,
    meta "emission_remaining",
    meta "total_supply")

let test_zero_reserve_cutover () =
  let left = run_zero_reserve_cutover () in
  let right = run_zero_reserve_cutover () in
  let left_root, reserve, total, balance, profile, activation, initial,
      remaining, supply = left
  in
  let right_root, _, _, _, _, _, _, _, _ = right in
  expect "cutover root parity" (String.equal left_root right_root);
  expect "cutover first reward" (Z.equal balance (Z.of_int 105));
  expect "cutover standard persisted" (profile = Some "security_curve");
  expect "cutover activation persisted"
    (activation = Some (string_of_int env.epoch_id));
  expect "cutover initial equals headroom"
    (initial = Some (Z.to_string reserve));
  expect "cutover reserve decremented"
    (remaining = Some (Z.to_string (Z.sub reserve (Z.of_int 5))));
  expect "cutover supply incremented"
    (supply = Some (Z.to_string (Z.add total (Z.of_int 5))))

let test_ledger_journal () =
  with_store (fun store ->
    let ledger = L.create store in
    begin
      match L.add_account ledger "octJournal" (Z.of_int 100) with
      | Ok () -> ()
      | Error error -> fail error
    end;
    expect "journal begins" (L.begin_journal ledger = Ok ());
    expect "journal active" (L.journal_active ledger);
    expect "journal debit"
      (L.debit ledger "octJournal" (Z.of_int 40) 1 = Ok ());
    expect "journal credit"
      (L.credit ledger "octNew" (Z.of_int 7) = Ok ());
    expect "nested journal begins" (L.begin_journal ledger = Ok ());
    expect "nested journal debit"
      (L.debit ledger "octJournal" (Z.of_int 10) 2 = Ok ());
    expect "nested journal abort" (L.abort_journal ledger = Ok ());
    expect "nested journal restored account"
      ((L.find ledger "octJournal").L.balance = Z.of_int 60);
    expect "nested journal restored nonce"
      ((L.find ledger "octJournal").L.nonce = 1);
    expect "journal abort" (L.abort_journal ledger = Ok ());
    expect "journal closed after abort" (not (L.journal_active ledger));
    expect "journal restored account"
      ((L.find ledger "octJournal").L.balance = Z.of_int 100);
    expect "journal restored nonce"
      ((L.find ledger "octJournal").L.nonce = 0);
    expect "journal removed new account" (L.find_opt ledger "octNew" = None);
    expect "journal restored supply"
      (L.get_total_supply ledger = Z.of_int 100);
    expect "second journal begins" (L.begin_journal ledger = Ok ());
    expect "second journal debit"
      (L.debit ledger "octJournal" (Z.of_int 10) 1 = Ok ());
    expect "journal commit" (L.commit_journal ledger = Ok ());
    expect "journal commit retained account"
      ((L.find ledger "octJournal").L.balance = Z.of_int 90);
    expect "journal commit retained supply"
      (L.get_total_supply ledger = Z.of_int 90))

let test_kat_journal () =
  with_store (fun store ->
    let ledger = L.create store in
    expect "kat starts absent" (L.get_pvac_kat ledger "octKat" = None);
    expect "kat journal begins" (L.begin_journal ledger = Ok ());
    L.set_pvac_kat ledger "octKat" "outer";
    expect "kat nested begins" (L.begin_journal ledger = Ok ());
    L.set_pvac_kat ledger "octKat" "inner";
    expect "kat nested abort" (L.abort_journal ledger = Ok ());
    expect "kat nested restored" (L.get_pvac_kat ledger "octKat" = Some "outer");
    expect "kat outer abort" (L.abort_journal ledger = Ok ());
    expect "kat absence restored" (L.get_pvac_kat ledger "octKat" = None);
    expect "kat commit begins" (L.begin_journal ledger = Ok ());
    L.set_pvac_kat ledger "octKat" "kept";
    expect "kat commit" (L.commit_journal ledger = Ok ());
    expect "kat retained" (S.get_pvac_kat store "octKat" = Some "kept");
    expect "kat replacement begins" (L.begin_journal ledger = Ok ());
    L.set_pvac_kat ledger "octKat" "changed";
    expect "kat replacement abort" (L.abort_journal ledger = Ok ());
    expect "kat replacement restored" (L.get_pvac_kat ledger "octKat" = Some "kept"))

let atomic_tx nonce =
  {
    T.from = env.X.proposer_addr;
    to_ = "octAtomicTarget";
    amount = Z.one;
    nonce;
    ou = Z.one;
    timestamp = 40.;
    signature = "sig";
    public_key = Some "pub";
    message = None;
    op_type = T.Standard;
    encrypted_data = None;
  }

let atomic_backend ?sender_key_activation_epoch store ledger =
  X.make_live_backend
    ~emission_policy:E.Guard
    ~legacy_total_supply:"100"
    ?sender_key_activation_epoch
    store
    ledger

let profile_at ~chain_id ~epoch =
  if chain_id <> env.X.chain_id || epoch < 4 then None
  else Some (String.make 32 (if epoch = 4 then '\001' else '\002'))

let execution_marker epoch byte =
  X.execution_marker_to_string X.{ epoch; profile = String.make 32 byte }
  |> expect_ok "execution marker"

let execution_base store epoch =
  Lwt_main.run (Lwt_list.iter_s (fun (key, value) -> S.set_meta store key value) [
    "last_epoch", string_of_int (epoch - 1);
    "current_epoch", string_of_int epoch;
    "total_supply", "0";
    "emission_remaining", "0";
  ])

let execution_backend store ledger =
  X.make_live_backend ~emission_policy:E.Guard ~emission_schedule:ES.Inactive
    ~execution_profile:profile_at store ledger

let test_execution_codec () =
  let profile = String.init 32 Char.chr in
  let hex = "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f" in
  let raw = "{\"epoch\":4,\"profile\":\"" ^ hex ^ "\"}" in
  expect "execution encoding differs"
    (X.execution_marker_to_string X.{ epoch = 4; profile } = Ok raw);
  expect "execution decoding differs"
    (X.execution_marker_of_string raw = Ok X.{ epoch = 4; profile });
  let fields = ["epoch", `Int 4; "profile", `String hex] in
  List.iter (fun fields ->
    expect "execution invalid fields accepted"
      (Result.is_error (X.execution_marker_of_string (Yojson.Safe.to_string (`Assoc fields))))) [
    List.remove_assoc "epoch" fields;
    List.remove_assoc "profile" fields;
    ("epoch", `Int 4) :: fields;
    ("profile", `String hex) :: fields;
    ("extra", `Null) :: fields;
    ["epoch", `Int (-1); "profile", `String hex];
    ["epoch", `Int max_int; "profile", `String hex];
    ["epoch", `String "4"; "profile", `String hex];
    ["epoch", `Float 4.; "profile", `String hex];
    ["epoch", `Int 4; "profile", `String (String.uppercase_ascii hex)];
    ["epoch", `Int 4; "profile", `String profile];
    ["epoch", `Int 4; "profile", `String (String.make 64 'g')];
  ];
  List.iter (fun raw ->
    expect "execution malformed marker accepted"
      (Result.is_error (X.execution_marker_of_string raw)))
    ["null"; "[]"; "{"; raw ^ "{}"; String.make 257 ' '];
  List.iter (fun marker ->
    expect "execution invalid marker encoded"
      (Result.is_error (X.execution_marker_to_string marker)))
    [X.{epoch = -1; profile}; X.{epoch = 4; profile = hex}];
  let check epoch marker =
    X.check_execution ~profile:profile_at ~chain_id:env.X.chain_id ~epoch marker in
  expect "execution preactivation rejected" (check 3 None = Ok ());
  expect "execution premature marker accepted"
    (check 3 (Some (execution_marker 3 '\001'))
      = Error "execution profile marker is unexpected");
  expect "execution missing marker accepted"
    (check 4 None = Error "execution profile marker is missing");
  expect "execution valid marker rejected" (check 4 (Some (execution_marker 4 '\001')) = Ok ());
  expect "execution wrong epoch accepted"
    (check 5 (Some (execution_marker 4 '\002')) = Error "execution profile epoch differs");
  expect "execution wrong hash accepted"
    (check 5 (Some (execution_marker 5 '\001')) = Error "execution profile hash differs");
  expect "execution text callback accepted"
    (X.check_execution ~profile:(fun ~chain_id:_ ~epoch:_ -> Some hex)
      ~chain_id:env.X.chain_id ~epoch:4 (Some raw)
     = Error "execution profile hash is invalid")

let test_execution_state () =
  with_store (fun store ->
    execution_base store 4;
    let check epoch = Lwt_main.run (X.check_execution_state ~profile:profile_at
      ~chain_id:env.X.chain_id ~epoch store) in
    expect "execution legacy state rejected" (check 3 = Ok ());
    expect "execution unmarked state accepted"
      (check 4 = Error "execution profile marker is missing");
    execution_base store 5;
    Lwt_main.run (S.set_meta store X.execution_meta_key (execution_marker 4 '\001'));
    expect "execution applied state rejected" (check 4 = Ok ());
    List.iter (fun (key, value, error) ->
      execution_base store 5;
      Lwt_main.run (match value with
        | Some value -> S.set_meta store key value
        | None -> S.remove_path store ["meta"; key]);
      expect "execution inconsistent epoch accepted" (check 4 = Error error)) [
      "last_epoch", None, "execution profile last epoch differs";
      "last_epoch", Some "3", "execution profile last epoch differs";
      "last_epoch", Some "04", "execution profile last epoch differs";
      "current_epoch", None, "execution profile current epoch differs";
      "current_epoch", Some "6", "execution profile current epoch differs";
      "current_epoch", Some "+5", "execution profile current epoch differs";
    ])

let test_execution_refusal () =
  List.iter (fun (epoch, last, current, marker, error) -> with_store (fun store ->
    execution_base store epoch;
    let ledger = L.create store in
    expect "execution refusal account" (L.add_account ledger env.X.proposer_addr (Z.of_int 100) = Ok ());
    Lwt_main.run (L.flush_dirty_lwt ledger);
    Lwt_main.run (S.set_meta store "total_supply" "100");
    Lwt_main.run (S.set_meta store "last_epoch" last);
    Lwt_main.run (S.set_meta store "current_epoch" current);
    Option.iter (fun raw -> Lwt_main.run (S.set_meta store X.execution_meta_key raw)) marker;
    let backend = execution_backend store ledger in
    let env = { env with X.epoch_id = epoch } in
    let before = Lwt_main.run (S.state_hash store) in
    let account = L.find ledger env.X.proposer_addr in
    expect "execution parent refusal differs"
      (Lwt_main.run (X.check_execution_parent ~backend ~env) = Error error);
    let calls = ref 0 in
    let result = Lwt_main.run (X.run_core ~reward:None ~preverify:None ~backend ~env
      ~txs:[atomic_tx 1] ~process_tx:(fun ~backend:_ ~env:_ _ ->
        incr calls; Lwt.return_ok (X.Confirmed Z.zero))) in
    expect "execution refused after transaction" (result = Error error && !calls = 0);
    let plan = X.build_reward_plan ~fee_burn_active:false ~supply_retired:Z.zero
      ~validator_count:1 ~emission_remaining:Z.zero ~confirmed_fees:(Z.of_int 10)
      ~prev_supply:(Z.of_int 100) |> expect_ok "execution refusal reward" in
    let refused = Lwt_main.run (Lwt.catch (fun () ->
      Lwt.map (fun () -> None) (X.apply_epoch_footer ~backend ~env ~plan))
      (function Failure reason -> Lwt.return_some reason | error -> Lwt.fail error)) in
    expect "execution footer refusal differs" (refused = Some error);
    expect "execution refusal changed reward" (L.find ledger env.X.proposer_addr = account);
    expect "execution refusal changed state" (Lwt_main.run (S.state_hash store) = before);
    expect "execution refusal opened batch" (store.S.batch_tree = None && not (L.journal_active ledger)))) [
    4, "2", "4", None, "execution profile last epoch differs";
    4, "3", "04", None, "execution profile current epoch differs";
    4, "3", "4", Some (execution_marker 3 '\001'), "execution profile marker is unexpected";
    5, "4", "5", None, "execution profile marker is missing";
    5, "4", "5", Some (execution_marker 3 '\001'), "execution profile epoch differs";
    5, "4", "5", Some (execution_marker 4 '\002'), "execution profile hash differs";
    5, "4", "5", Some "{}", "execution profile marker is invalid";
  ]

let test_execution_chain () =
  with_store (fun store ->
    execution_base store 4;
    let ledger = L.create store in
    let backend = execution_backend store ledger in
    let run backend epoch = X.run ~backend ~env:{env with X.epoch_id = epoch}
      ~txs:[] ~process_tx:(fun ~backend:_ ~env:_ _ -> Lwt.return_ok Z.zero)
      |> Lwt_main.run in
    let check epoch = Lwt_main.run (X.check_execution_state ~profile:profile_at
      ~chain_id:env.X.chain_id ~epoch store) in
    let first = run backend 4 |> expect_ok "execution activation" in
    expect "execution activation marker differs"
      (Lwt_main.run (S.get_meta store X.execution_meta_key) = Some (execution_marker 4 '\001'));
    expect "execution activation state rejected" (check 4 = Ok ());
    expect "execution root omitted marker" (Lwt_main.run (S.state_hash store) = first.X.post_state_root);
    let head = Lwt_main.run (S.Store.Head.find store.S.store) |> Option.get in
    let zero = X.build_reward_plan ~fee_burn_active:false ~supply_retired:Z.zero
      ~validator_count:1 ~emission_remaining:Z.zero ~confirmed_fees:Z.zero
      ~prev_supply:Z.zero |> expect_ok "execution zero reward" in
    Lwt_main.run (S.begin_epoch_batch store);
    Lwt_main.run (X.apply_epoch_footer ~backend ~env:{env with X.epoch_id = 5} ~plan:zero);
    expect "execution footer omitted batch marker"
      (Lwt_main.run (S.get_meta store X.execution_meta_key) = Some (execution_marker 5 '\002'));
    expect "execution footer omitted batch root"
      (Lwt_main.run (S.get_batch_tree_hash store) <> Some first.post_state_root);
    expect "execution uncommitted footer changed root"
      (Lwt_main.run (S.state_hash store) = first.post_state_root);
    S.abort_epoch_batch store;
    expect "execution abort kept marker" (check 4 = Ok ());
    let old = X.make_live_backend ~emission_policy:E.Guard ~emission_schedule:ES.Inactive store ledger in
    ignore (run old 5 |> expect_ok "execution old binary advance");
    expect "execution old binary stamped marker"
      (Lwt_main.run (S.get_meta store X.execution_meta_key) = Some (execution_marker 4 '\001'));
    expect "execution old binary state accepted" (check 5 = Error "execution profile epoch differs");
    let before = Lwt_main.run (S.state_hash store) in
    expect "execution old suffix restamped"
      (run backend 6 = Error "execution profile epoch differs");
    expect "execution old suffix changed root" (Lwt_main.run (S.state_hash store) = before);
    Lwt_main.run (S.Store.Head.set store.S.store head);
    expect "execution recovered state rejected" (check 4 = Ok ());
    ignore (run backend 5 |> expect_ok "execution successor");
    expect "execution successor profile differs"
      (Lwt_main.run (S.get_meta store X.execution_meta_key) = Some (execution_marker 5 '\002'));
    expect "execution successor state rejected" (check 5 = Ok ()))

let test_execution_parity () =
  let run profile = with_store (fun store ->
    execution_base store 4;
    let ledger = L.create store in
    let base = Lwt_main.run (S.state_hash store) in
    let result = Octra_core.State_preview.with_preview ~base_store:store ~base_ledger:ledger
      ~proof_mode:Octra_core.Rule_graph.Prior ~fold:X.prior_fold
      ~epoch_id:4 ~proposal_id:"execution-preview"
      (fun backend -> X.run ~backend:{backend with X.execution_profile = profile;
        emission_policy = E.Guard; emission_schedule = ES.Inactive}
        ~env ~txs:[] ~process_tx:(fun ~backend:_ ~env:_ _ -> Lwt.return_ok Z.zero))
      |> Lwt_main.run |> expect_ok "execution preview" in
    expect "execution preview changed live root" (Lwt_main.run (S.state_hash store) = base);
    expect "execution preview leaked marker" (Lwt_main.run (S.get_meta store X.execution_meta_key) = None);
    let backend = X.make_live_backend ~emission_policy:E.Guard ~emission_schedule:ES.Inactive
      ~execution_profile:profile store ledger in
    let applied = run_empty_epoch backend |> expect_ok "execution live" in
    expect "execution preview root differs" (result.X.post_state_root = applied.X.post_state_root);
    applied.post_state_root) in
  expect "execution marker absent from root"
    (run profile_at <> run (fun ~chain_id:_ ~epoch:_ -> None));
  let legacy profile = with_store (fun store ->
    execution_base store 3;
    let backend = X.make_live_backend ~execution_profile:profile store (L.create store) in
    let result = X.run ~backend ~env:{env with X.epoch_id = 3} ~txs:[]
      ~process_tx:(fun ~backend:_ ~env:_ _ -> Lwt.return_ok Z.zero)
      |> Lwt_main.run |> expect_ok "execution legacy" in
    expect "execution legacy stamped marker" (Lwt_main.run (S.get_meta store X.execution_meta_key) = None);
    result.X.post_state_root) in
  expect "execution legacy root changed"
    (legacy profile_at = legacy (fun ~chain_id:_ ~epoch:_ -> None))

let test_execution_capture () =
  with_store (fun store ->
    execution_base store 5;
    Lwt_main.run (S.set_meta store X.execution_meta_key (execution_marker 4 '\001'));
    let ledger = L.create store in
    expect "execution capture account" (L.add_account ledger env.X.proposer_addr (Z.of_int 100) = Ok ());
    Lwt_main.run (L.flush_dirty_lwt ledger);
    Lwt_main.run (S.set_meta store "total_supply" "100");
    let hash = ref (String.make 32 '\002') in
    let profile ~chain_id ~epoch =
      if epoch = 5 then Some !hash else profile_at ~chain_id ~epoch in
    let backend = X.make_live_backend ~emission_policy:E.Guard ~emission_schedule:ES.Inactive
      ~sender_key_activation_epoch:max_int ~execution_profile:profile store ledger in
    let result = X.run_core ~reward:None ~preverify:None ~backend
      ~env:{env with X.epoch_id = 5} ~txs:[atomic_tx 1]
      ~process_tx:(fun ~backend:_ ~env:_ _ ->
        hash := String.make 32 '\003'; Lwt.return_ok (X.Confirmed Z.zero))
      |> Lwt_main.run |> expect_ok "execution capture" in
    expect "execution profile captured after transaction"
      (Lwt_main.run (S.get_meta store X.execution_meta_key) = Some (execution_marker 5 '\002'));
    expect "execution captured root differs" (Lwt_main.run (S.state_hash store) = result.X.post_state_root);
    expect "execution captured state rejected"
      (Lwt_main.run (X.check_execution_state ~profile:profile_at ~chain_id:env.X.chain_id ~epoch:5 store) = Ok ()))

let test_sender_key_activation () =
  let run epoch activation =
    with_store (fun store ->
      let ledger = L.create store in
      expect "sender account added"
        (L.add_account ledger env.X.proposer_addr (Z.of_int 100) = Ok ());
      Lwt_main.run (L.flush_dirty_lwt ledger);
      let backend =
        atomic_backend ~sender_key_activation_epoch:activation store ledger in
      let result =
        X.run
          ~backend
          ~env:{ env with X.epoch_id = epoch }
          ~txs:[atomic_tx 1]
          ~process_tx:X.process_standard_tx
        |> Lwt_main.run
      in
      expect "sender key epoch applied" (Result.is_ok result);
      (L.find ledger env.X.proposer_addr).L.public_key)
  in
  expect "sender key inactive" (run 3 4 = None);
  expect "sender key active" (run 4 4 = Some "pub")

let test_sender_key_preview_parity () =
  with_env "OCTRA_EMISSION_GUARD" "1" (fun () ->
    with_env "OCTRA_LEGACY_TOTAL_SUPPLY_RAW" "100" (fun () ->
      with_env K.env_name "4" (fun () ->
        with_store (fun store ->
          let ledger = L.create store in
          expect "preview sender account added"
            (L.add_account ledger env.X.proposer_addr (Z.of_int 100) = Ok ());
          Lwt_main.run (L.flush_dirty_lwt ledger);
          let tx = atomic_tx 1 in
          let preview =
            Octra_core.State_preview.with_preview
              ~base_store:store
              ~base_ledger:ledger
              ~proof_mode:Octra_core.Rule_graph.Prior
              ~fold:X.prior_fold
              ~epoch_id:env.X.epoch_id
              ~proposal_id:"sender-key-preview"
              (fun backend ->
                expect "preview proof mode"
                  (backend.X.proof_mode = Octra_core.Rule_graph.Prior);
                X.run
                  ~backend
                  ~env
                  ~txs:[tx]
                  ~process_tx:X.process_standard_tx)
            |> Lwt_main.run
            |> expect_ok "sender key preview"
          in
          expect "preview sender key isolated"
            ((L.find ledger env.X.proposer_addr).L.public_key = None);
          let applied =
            X.run
              ~backend:(atomic_backend ~sender_key_activation_epoch:4 store ledger)
              ~env
              ~txs:[tx]
              ~process_tx:X.process_standard_tx
            |> Lwt_main.run
            |> expect_ok "sender key live apply"
          in
          expect "sender key preview root parity"
            (preview.X.post_state_root = applied.X.post_state_root);
          expect "live sender key persisted"
            ((L.find ledger env.X.proposer_addr).L.public_key = Some "pub")))))

let test_preview_clean_ledger () =
  with_store (fun store ->
    let ledger = L.create store in
    expect "dirty preview account added"
      (L.add_account ledger env.X.proposer_addr (Z.of_int 100) = Ok ());
    Lwt_main.run (L.flush_dirty_lwt ledger);
    expect "dirty preview account changed"
      (L.credit ledger env.X.proposer_addr Z.one = Ok ());
    let result =
      Octra_core.State_preview.with_preview
        ~base_store:store
        ~base_ledger:ledger
        ~proof_mode:Octra_core.Rule_graph.Prior
        ~fold:X.prior_fold
        ~epoch_id:env.X.epoch_id
        ~proposal_id:"dirty-preview"
        (fun backend ->
          expect "dirty preview proof mode"
            (backend.X.proof_mode = Octra_core.Rule_graph.Prior);
          Lwt.return_ok ())
      |> Lwt_main.run
    in
    expect "dirty preview rejected"
      (result = Error "ledger clone requires flushed accounts"))

let test_sender_key_policy () =
  expect "sender key activation absent"
    (K.activation_epoch_of (fun _ -> None) = Ok (Some 0));
  expect "sender key activation valid"
    (K.activation_epoch_of (fun _ -> Some "1266000") = Ok (Some 1266000));
  expect "sender key activation zero"
    (K.activation_epoch_of (fun _ -> Some "0") = Ok (Some 0));
  expect "sender key activation malformed rejected"
    (Result.is_error (K.activation_epoch_of (fun _ -> Some "bad")));
  expect "sender key inactive decision"
    (K.decide
       ~activation_epoch:(Some 4)
       ~epoch:3
       ~stored:None
       ~carried:(Some "new") = K.Keep);
  expect "sender key missing decision"
    (K.decide
       ~activation_epoch:(Some 4)
       ~epoch:4
       ~stored:None
       ~carried:None = K.Keep);
  expect "sender key immutable decision"
    (K.decide
       ~activation_epoch:(Some 4)
       ~epoch:4
       ~stored:(Some "old")
       ~carried:(Some "new") = K.Keep)

let test_tx_reject_rollback () =
  with_store (fun store ->
    let ledger = L.create store in
    expect "reject account added"
      (L.add_account ledger env.X.proposer_addr (Z.of_int 100) = Ok ());
    Lwt_main.run (L.flush_dirty_lwt ledger);
    let backend =
      atomic_backend ~sender_key_activation_epoch:env.X.epoch_id store ledger in
    let tx = atomic_tx 1 in
    let result =
      X.run
        ~backend
        ~env
        ~txs:[tx]
        ~process_tx:(fun ~backend:_ ~env:_ _ ->
          let open Lwt.Syntax in
          match L.debit ledger tx.from (Z.of_int 40) tx.nonce with
          | Error error -> Lwt.return_error ("debit_failed", error)
          | Ok () ->
            let* () = S.set_meta store "partial_tx" "bad" in
            Lwt.return_error ("injected_reject", "reject after debit"))
      |> Lwt_main.run
    in
    expect "rejected epoch completed"
      (match result with
       | Ok applied -> List.length applied.X.artifacts.rejected = 1
       | Error _ -> false);
    expect "rejected debit rolled back"
      (Z.equal (L.find ledger env.X.proposer_addr).L.balance (Z.of_int 100));
    expect "rejected nonce rolled back"
      ((L.find ledger env.X.proposer_addr).L.nonce = 0);
    expect "rejected sender key rolled back"
      ((L.find ledger env.X.proposer_addr).L.public_key = None);
    expect "rejected store write rolled back"
      (Lwt_main.run (S.get_meta store "partial_tx") = None))

let test_sender_key_after_fee () =
  with_store (fun store ->
    let ledger = L.create store in
    expect "fee rejection account added"
      (L.add_account ledger env.X.proposer_addr (Z.of_int 100) = Ok ());
    Lwt_main.run (L.flush_dirty_lwt ledger);
    let backend =
      atomic_backend ~sender_key_activation_epoch:env.X.epoch_id store ledger in
    let tx = atomic_tx 1 in
    let result =
      X.run_core
        ~reward:None
        ~preverify:None
        ~backend
        ~env
        ~txs:[tx]
        ~process_tx:(fun ~backend:_ ~env:_ _ ->
          match L.debit ledger tx.from tx.ou tx.nonce with
          | Error error -> Lwt.return_error ("debit_failed", error)
          | Ok () ->
            Lwt.return_ok
              (X.Rejected_after_fee {
                fee = tx.ou;
                error_type = "injected_reject";
                reason = "reject after fee";
              }))
      |> Lwt_main.run
    in
    expect "fee rejection epoch completed"
      (match result with
       | Ok applied ->
         List.length applied.X.artifacts.rejected = 1
         && Z.equal applied.X.artifacts.confirmed_fees tx.ou
       | Error _ -> false);
    expect "fee rejection nonce committed"
      ((L.find ledger env.X.proposer_addr).L.nonce = tx.nonce);
    expect "fee rejection sender key absent"
      ((L.find ledger env.X.proposer_addr).L.public_key = None))

let test_preview_prefix () =
  let module Work = Octra_core.Exec_resource in
  let execute partial = with_store (fun store ->
    let ledger = L.create store in
    expect "prefix account added" (L.add_account ledger env.X.proposer_addr (Z.of_int 100) = Ok ());
    Lwt_main.run (L.flush_dirty_lwt ledger);
    let backend = atomic_backend ~sender_key_activation_epoch:env.X.epoch_id store ledger in
    let first = atomic_tx 1 in
    let second = atomic_tx 2 in
    let deferred = ref [] in
    let run () = X.run_core ~reward:None ~preverify:None ~backend ~env
      ~txs:(if partial then [first; second] else [first])
      ~process_tx:(fun ~backend:_ ~env:_ tx -> Work.run ~hash:(T.hash tx) (fun () ->
        let open Lwt.Syntax in
        expect "prefix debit" (L.debit ledger tx.from (Z.of_int 10) tx.nonce = Ok ());
        let* () = S.set_meta store "prefix_last" (string_of_int tx.nonce) in
        if tx.nonce = 2 then Lwt.fail (Work.Unavailable Time)
        else Lwt.return_ok (X.Confirmed Z.zero))) in
    let result, stopped = Lwt_main.run (if partial then
      Work.preview ~accept:(fun () -> true) ~defer:(fun hash resource ->
        deferred := (hash, resource) :: !deferred) run
      else Lwt.map (fun result -> result, false) (run ())) in
    let result = expect_ok "prefix execution" result in
    expect "prefix selection state" (stopped = partial);
    expect "prefix resource identity" (!deferred = if partial then [T.hash second, Work.Time] else []);
    expect "prefix transaction count" (result.X.artifacts.tx_count = 1);
    expect "prefix has no rejected record" (result.X.artifacts.rejected = []);
    expect "prefix storage rollback" (Lwt_main.run (S.get_meta store "prefix_last") = Some "1");
    expect "prefix ledger rollback" ((L.find ledger first.from).L.nonce = 1);
    result.post_state_root) in
  let complete = execute false in
  expect "prefix root differs from full replay" (execute true = complete)

let test_preview_senders () =
  let module Work = Octra_core.Exec_resource in
  List.iter (fun resource -> List.iter (fun failed ->
    let execute partial = with_store (fun store ->
      let ledger = L.create store in
      let healthy = {(atomic_tx 1) with T.from = "octZHealthy"} in
      List.iter (fun address ->
        expect "sender account added" (L.add_account ledger address (Z.of_int 50) = Ok ()))
        [env.X.proposer_addr; healthy.from];
      Lwt_main.run (L.flush_dirty_lwt ledger);
      let backend = atomic_backend ~sender_key_activation_epoch:env.X.epoch_id store ledger in
      let chain = List.init 3 (fun i -> atomic_tx (i + 1)) in
      let kept = List.filter (fun tx -> tx.T.nonce < failed) chain @ [healthy] in
      let deferred = ref [] in
      let visited = ref [] in
      let run () = X.run_core ~reward:None ~preverify:None ~backend ~env
        ~txs:(if partial then healthy :: chain else kept)
        ~process_tx:(fun ~backend:_ ~env:_ tx -> Work.run ~hash:(T.hash tx) (fun () ->
          let open Lwt.Syntax in
          visited := tx :: !visited;
          expect "sender successor executed" (tx.from <> env.X.proposer_addr || tx.nonce <= failed);
          expect "sender debit" (L.debit ledger tx.from (Z.of_int 10) tx.nonce = Ok ());
          let* () = S.set_meta store tx.from (string_of_int tx.nonce) in
          if tx.from = env.X.proposer_addr && tx.nonce = failed then
            Lwt.fail (Work.Unavailable resource)
          else Lwt.return_ok (X.Confirmed Z.zero))) in
      let result, partial_result = Lwt_main.run (Work.preview ~accept:(fun () -> true)
        ~defer:(fun hash resource -> deferred := (hash, resource) :: !deferred) run) in
      let result = expect_ok "sender execution" result in
      expect "sender partial result" (partial_result = partial);
      expect "sender resource identity" (!deferred =
        if partial then [T.hash (atomic_tx failed), resource] else []);
      expect "other sender omitted" (List.map fst result.X.artifacts.confirmed = T.consensus_order kept);
      expect "sender failure charged" (result.X.artifacts.rejected = [] && Z.equal result.X.artifacts.confirmed_fees Z.zero);
      expect "sender successor executed" (not (List.exists (fun tx ->
        tx.T.from = env.X.proposer_addr && tx.nonce > failed) !visited));
      expect "sender nonce rollback" ((L.find ledger env.X.proposer_addr).L.nonce = failed - 1);
      expect "sender balance rollback" (Z.equal (L.find ledger env.X.proposer_addr).L.balance
        (Z.of_int (50 - 10 * (failed - 1))));
      expect "sender storage rollback" (Lwt_main.run (S.get_meta store env.X.proposer_addr) =
        if failed = 1 then None else Some "1");
      expect "other sender storage" (Lwt_main.run (S.get_meta store healthy.from) = Some "1");
      result.post_state_root) in
    let complete = execute false in
    expect "sender replay root differs" (execute true = complete)) [1; 2])
    Work.[Time; Memory; Stack; Host]

let test_tx_exception_rollback () =
  with_store (fun store ->
    let ledger = L.create store in
    expect "exception account added"
      (L.add_account ledger env.X.proposer_addr (Z.of_int 100) = Ok ());
    Lwt_main.run (L.flush_dirty_lwt ledger);
    let backend = atomic_backend store ledger in
    let tx = atomic_tx 1 in
    let result =
      X.run
        ~backend
        ~env
        ~txs:[tx]
        ~process_tx:(fun ~backend:_ ~env:_ _ ->
          match L.debit ledger tx.from (Z.of_int 40) tx.nonce with
          | Error error -> Lwt.fail_with error
          | Ok () -> Lwt.fail_with "injected tx failure")
      |> Lwt_main.run
    in
    expect "exception rejected epoch"
      (match result with Error _ -> true | Ok _ -> false);
    expect "exception debit rolled back"
      (Z.equal (L.find ledger env.X.proposer_addr).L.balance (Z.of_int 100));
    expect "exception nonce rolled back"
      ((L.find ledger env.X.proposer_addr).L.nonce = 0);
    expect "exception journal closed" (not (L.journal_active ledger)))

let test_worker_retry_rollback () =
  with_store (fun store ->
    let ledger = L.create store in
    expect "retry account added"
      (L.add_account ledger env.X.proposer_addr (Z.of_int 100) = Ok ());
    Lwt_main.run (L.flush_dirty_lwt ledger);
    let backend = atomic_backend store ledger in
    let tx = atomic_tx 1 in
    let first =
      X.run
        ~backend
        ~env
        ~txs:[tx]
        ~process_tx:(fun ~backend:_ ~env:_ _ ->
          match L.debit ledger tx.from (Z.of_int 40) tx.nonce with
          | Error error -> Lwt.fail_with error
          | Ok () ->
            Lwt.fail
              (Octra_core.Private_ledger.Worker_retry "worker unavailable"))
      |> Lwt_main.run
    in
    expect "retry rejected epoch" (Result.is_error first);
    expect "retry debit rolled back"
      (Z.equal (L.find ledger env.X.proposer_addr).L.balance (Z.of_int 100));
    expect "retry nonce rolled back"
      ((L.find ledger env.X.proposer_addr).L.nonce = 0);
    expect "retry journal closed" (not (L.journal_active ledger));
    let second =
      X.run
        ~backend
        ~env
        ~txs:[tx]
        ~process_tx:X.process_standard_tx
      |> Lwt_main.run
    in
    expect "retry epoch reapplied" (Result.is_ok second);
    expect "retry transaction committed"
      ((L.find ledger env.X.proposer_addr).L.nonce = 1))

let test_worker_retry_loop () =
  let calls = ref 0 in
  let waits = ref [] in
  let wait delay =
    waits := delay :: !waits;
    Lwt.return_unit
  in
  let apply () =
    incr calls;
    if !calls < 3 then
      Lwt.fail (Octra_core.Private_ledger.Worker_retry "worker unavailable")
    else
      Lwt.return 7
  in
  let value = A.worker_retry ~wait apply |> Lwt_main.run in
  expect "worker retry result" (value = 7);
  expect "worker retry count" (!calls = 3);
  expect "worker retry waits" (List.rev !waits = [0.25; 0.5]);
  calls := 0;
  waits := [];
  let stopped = try
    A.worker_retry ~wait (fun () ->
      incr calls;
      if !calls > 4 then failwith "worker retry budget exceeded";
      Lwt.fail (Octra_core.Private_ledger.Worker_retry "permanent"))
    |> Lwt_main.run |> ignore;
    false
  with Octra_core.Private_ledger.Worker_stopped reason -> reason = "permanent" in
  expect "worker failure did not stop" (stopped && !calls = 4);
  expect "worker stop delay changed" (List.rev !waits = [0.25; 0.5; 1.]);
  List.iter (fun (resource, attempts) ->
    calls := 0;
    waits := [];
    let stopped = try
      A.worker_retry ~wait (fun () ->
        incr calls;
        if !calls > 4 then failwith "worker retry budget exceeded";
        Lwt.fail (Octra_core.Exec_resource.Unavailable resource))
      |> Lwt_main.run |> ignore;
      false
    with
    | Octra_core.Private_ledger.Worker_stopped _ -> true
    | Octra_core.Exec_resource.Unavailable _ -> false in
    expect "worker resource bypassed retry" (stopped && !calls = attempts);
    let delays = if attempts = 4 then [0.25; 0.5; 1.] else [] in
    expect "worker resource delay changed" (List.rev !waits = delays))
    Octra_core.Exec_resource.[Host, 4; Memory, 4; Stack, 1];
  calls := 0;
  waits := [];
  let value = A.worker_retry ~wait (fun () ->
    incr calls;
    if !calls < 3 then Lwt.fail (Octra_core.Exec_resource.Unavailable Memory)
    else Lwt.return 9) |> Lwt_main.run in
  expect "memory pressure stopped apply" (value = 9 && !calls = 3);
  expect "memory pressure busy loop" (List.rev !waits = [0.25; 0.5]);
  let sleeping, _ = Lwt.task () in
  let retry = A.worker_retry ~wait:(fun _ -> sleeping) (fun () ->
    Lwt.fail (Octra_core.Exec_resource.Unavailable Memory)) in
  Lwt.cancel retry;
  expect "memory wait ignored cancellation" (Lwt.state retry = Lwt.Fail Lwt.Canceled);
  let failed =
    try
      A.worker_retry ~wait (fun () -> Lwt.fail (Failure "fatal"))
      |> Lwt_main.run
      |> ignore;
      false
    with
    | Failure reason -> String.equal reason "fatal"
    | _ -> false
  in
  expect "worker retry preserves failure" failed;
  let sleeping, _ = Lwt.task () in
  let retry = A.worker_retry ~wait:(fun _ -> sleeping) (fun () ->
    Lwt.fail (Octra_core.Private_ledger.Worker_retry "worker unavailable")) in
  Lwt.cancel retry;
  expect "worker retry ignored cancellation" (Lwt.state retry = Lwt.Fail Lwt.Canceled)

let test_worker_stop_rollback () =
  let module Private = Octra_core.Private_ledger in
  let module Sender = Octra_node_runtime.Consensus_epoch_apply_sender in
  List.iter (fun (resource, attempts) ->
    with_store (fun store ->
      let ledger = L.create store in
      let tx = atomic_tx 1 in
      expect "worker account added" (L.add_account ledger tx.from (Z.of_int 100) = Ok ());
      Lwt_main.run (L.flush_dirty_lwt ledger);
      Lwt_main.run (S.begin_epoch_batch store);
      let calls = ref 0 in
      let stopped = try
        Sender.run Sender.{
          process = (fun _ ->
            Private.worker_retry ~wait:(fun _ -> Lwt.return_unit) (fun () ->
              incr calls;
              if !calls > 4 then failwith "worker retry budget exceeded";
              Octra_core.Tx_savepoint.run ~ledger ~store (fun () ->
                expect "worker attempt debit refused"
                  (L.debit ledger tx.from (Z.of_int 40) tx.nonce = Ok ());
                Lwt.fail (Octra_core.Exec_resource.Unavailable resource))) |> Lwt.map ignore);
          fatal = (fun ~sender:_ error ->
            failwith ("worker stop became fatal restart: " ^ Printexc.to_string error));
        } [tx]
        |> Lwt_main.run |> ignore;
        false
      with Private.Worker_stopped reason ->
        reason = Octra_core.Exec_resource.name resource ^ " unavailable" in
      expect "worker stop lost reason" (stopped && !calls = attempts);
      expect "worker stop changed balance" (Z.equal (L.find ledger tx.from).L.balance (Z.of_int 100));
      expect "worker stop changed nonce" ((L.find ledger tx.from).L.nonce = 0);
      expect "worker stop retained journal" (not (L.journal_active ledger));
      S.abort_epoch_batch store))
    Octra_core.Exec_resource.[Host, 4; Memory, 4; Stack, 1]

let test_checkpoint_outcomes () =
  List.iter (fun outcome -> with_store (fun store ->
    let ledger = L.create store in
    let tx = atomic_tx 1 in
    expect "checkpoint account" (L.add_account ledger tx.from (Z.of_int 100) = Ok ());
    Lwt_main.run (L.flush_dirty_lwt ledger);
    Lwt_main.run (S.begin_epoch_batch store);
    let used = ref 4 and restored = ref 0 in
    let checkpoint () =
      let prior = !used in
      fun () -> incr restored; used := prior in
    let result = Lwt_main.run (Lwt.catch (fun () ->
      Lwt.map (fun result -> Ok result) (Octra_core.Tx_savepoint.run ~checkpoint
        ~ledger ~store (fun () ->
          expect "checkpoint debit" (L.debit ledger tx.from (Z.of_int 40) tx.nonce = Ok ());
          used := 9;
          match outcome with
          | 0 -> Lwt.return_error "refused"
          | 1 -> Lwt.fail (Failure "checkpoint fault")
          | 2 -> Lwt.return_ok (X.Confirmed (Z.of_int 40))
          | _ -> Lwt.return_ok (X.Rejected_after_fee
              {fee = Z.of_int 40; error_type = "charged"; reason = "refused"}))))
      (fun error -> Lwt.return_error (Printexc.to_string error))) in
    expect "checkpoint result class" (match outcome, result with
      | 0, Ok (Error "refused") -> true
      | 1, Error reason -> reason = "Failure(\"checkpoint fault\")"
      | (2 | 3), Ok (Ok _) -> true
      | _ -> false);
    let committed = outcome >= 2 in
    expect "checkpoint outcome restored incorrectly"
      (!used = (if committed then 9 else 4) && !restored = (if committed then 0 else 1));
    let account = L.find ledger tx.from in
    expect "checkpoint account outcome"
      (Z.equal account.balance (Z.of_int (if committed then 60 else 100))
       && account.nonce = (if committed then 1 else 0));
    expect "checkpoint journal remains" (not (L.journal_active ledger));
    S.abort_epoch_batch store)) [0; 1; 2; 3];
  with_store (fun store ->
    let ledger = L.create store in
    Lwt_main.run (S.begin_epoch_batch store);
    ignore (L.begin_journal ledger |> expect_ok "checkpoint outer journal");
    let used = ref 4 and restored = ref 0 in
    let checkpoint () = let prior = !used in fun () -> incr restored; used := prior in
    let failed = try ignore (Lwt_main.run (Octra_core.Tx_savepoint.run ~checkpoint
      ~ledger ~store (fun () -> used := 9; S.abort_epoch_batch store;
        Lwt.return_error "refused"))); false
      with Failure reason -> reason = "store batch is not active" in
    expect "checkpoint lost restoration failure" failed;
    expect "checkpoint skipped participant restoration" (!used = 4 && !restored = 1);
    expect "checkpoint aborted outer journal twice" (L.journal_active ledger);
    ignore (L.abort_journal ledger |> expect_ok "checkpoint outer restore"))

let test_checkpoint_journal () =
  with_store (fun store ->
    let ledger = L.create store in
    Lwt_main.run (S.begin_epoch_batch store);
    let before = Lwt_main.run (S.get_batch_tree_hash store) in
    let used = ref 4 and restored = ref 0 in
    let checkpoint () = let prior = !used in fun () -> incr restored; used := prior in
    let failed = try ignore (Lwt_main.run (Octra_core.Tx_savepoint.run ~checkpoint
      ~ledger ~store (fun () ->
        used := 9;
        Lwt.bind (S.set_meta store "journal_probe" "1") (fun () ->
          ignore (L.commit_journal ledger |> expect_ok "journal inner commit");
          Lwt.return_ok (X.Confirmed Z.zero))))); false
      with Failure _ -> true in
    expect "journal commit failure was accepted" failed;
    expect "journal commit failure kept participant state" (!used = 4 && !restored = 1);
    expect "journal commit failure kept store write"
      (before <> None && Lwt_main.run (S.get_batch_tree_hash store) = before);
    S.abort_epoch_batch store)

let test_checkpoint_cancel () =
  with_store (fun store ->
    let module Work = Octra_core.Exec_resource in
    let ledger = L.create store in
    let tx = atomic_tx 1 in
    expect "cancel account" (L.add_account ledger tx.from (Z.of_int 100) = Ok ());
    Lwt_main.run (L.flush_dirty_lwt ledger);
    Lwt_main.run (S.begin_epoch_batch store);
    let used = ref 4 and events = ref [] in
    let pending, _ = Lwt.task () in
    let cleanup, release = Lwt.wait () in
    let checkpoint () = let prior = !used in fun () ->
      events := "restore" :: !events; used := prior in
    let task = Octra_core.Tx_savepoint.run ~checkpoint ~ledger ~store (fun () ->
      Work.interrupt (fun () ->
        expect "cancel debit" (L.debit ledger tx.from (Z.of_int 40) tx.nonce = Ok ());
        used := 9;
        Lwt.finalize (fun () -> pending) (fun () ->
          let open Lwt.Syntax in
          let* () = Lwt.no_cancel cleanup in
          events := "cleanup" :: !events;
          Lwt.return_unit))) in
    Lwt.cancel task;
    expect "checkpoint restored before cleanup" (!events = [] && !used = 9 && Lwt.is_sleeping task);
    Lwt.wakeup release ();
    let canceled = Lwt_main.run (Lwt.catch (fun () -> Lwt.map (fun _ -> false) task)
      (function Lwt.Canceled -> Lwt.return_true | error -> Lwt.fail error)) in
    expect "checkpoint cancellation differs" canceled;
    expect "checkpoint cleanup order" (List.rev !events = ["cleanup"; "restore"] && !used = 4);
    expect "checkpoint cancellation changed account"
      ((L.find ledger tx.from).nonce = 0 && Z.equal (L.find ledger tx.from).balance (Z.of_int 100));
    expect "checkpoint cancellation retained journal" (not (L.journal_active ledger));
    S.abort_epoch_batch store)

let test_checkpoint_commit () =
  List.iter (fun (expires, yields) -> with_store (fun store ->
    let module Work = Octra_core.Exec_resource in
    let module Save = Octra_core.Tx_savepoint in
    let ledger = L.create store in
    let tx = atomic_tx 1 in
    expect "commit account" (L.add_account ledger tx.from (Z.of_int 100) = Ok ());
    Lwt_main.run (L.flush_dirty_lwt ledger);
    Lwt_main.run (S.begin_epoch_batch store);
    ignore (L.begin_journal ledger |> expect_ok "commit epoch journal");
    expect "commit prior credit" (L.credit ledger tx.from (Z.of_int 7) = Ok ());
    let entered = ref false and used = ref 4 in
    let checkpoint () = let prior = !used in fun () -> used := prior in
    let clock () =
      if expires && !entered && List.length ledger.L.epoch_journals < 3 then 2. else 0. in
    let result = Lwt_main.run (Lwt.catch (fun () -> Lwt.map (fun value -> Ok value)
      (Save.run ~checkpoint ~ledger ~store (fun () ->
        Work.select ~limit:1. ~clock ~running:(fun () -> true) (fun () ->
          Work.run ~hash:"commit" (fun () ->
            Save.run ~ledger ~store (fun () ->
              let open Lwt.Syntax in
              let* () = if yields then Lwt.pause () else Lwt.return_unit in
              entered := true;
              expect "commit debit" (L.debit ledger tx.from (Z.of_int 40) tx.nonce = Ok ());
              used := 9;
              let* () = S.set_meta store "commit-write" "accepted" in
              Lwt.return_ok ())))))) (fun error -> Lwt.return_error error)) in
    let committed = match result with
      | Ok (Ok ()) -> true
      | Error (Work.Exhausted ("commit", Work.Time)) when expires -> false
      | _ -> failwith "commit result differs" in
    expect "commit cut erased prior operation"
      (Z.equal (L.find ledger tx.from).balance (Z.of_int (if committed then 67 else 107)));
    expect "commit nonce differs" ((L.find ledger tx.from).nonce = (if committed then 1 else 0));
    expect "commit participant differs" (!used = (if committed then 9 else 4));
    expect "commit store differs"
      (Lwt_main.run (S.get_meta store "commit-write") = (if committed then Some "accepted" else None));
    expect "commit cut closed epoch journal" (List.length ledger.L.epoch_journals = 1);
    ignore (L.abort_journal ledger |> expect_ok "commit epoch restore");
    expect "commit epoch restore differs" (Z.equal (L.find ledger tx.from).balance (Z.of_int 100));
    S.abort_epoch_batch store)) [false, false; false, true; true, false; true, true]

let test_memory_rollback () =
  let execute retries = with_store (fun store ->
    let ledger = L.create store in
    let tx = atomic_tx 1 in
    expect "memory account added" (L.add_account ledger tx.from (Z.of_int 100) = Ok ());
    Lwt_main.run (L.flush_dirty_lwt ledger);
    Lwt_main.run (S.begin_epoch_batch store);
    let calls = ref 0 in
    let result = Lwt_main.run (A.worker_retry ~wait:(fun _ -> Lwt.pause ()) (fun () ->
      Octra_core.Tx_savepoint.run ~ledger ~store (fun () ->
        incr calls;
        expect "memory retry debit" (L.debit ledger tx.from (Z.of_int 40) tx.nonce = Ok ());
        let open Lwt.Syntax in
        let* () = S.set_meta store "memory-write" "accepted" in
        if !calls <= retries then Lwt.fail (Octra_core.Exec_resource.Unavailable Memory)
        else Lwt.return_ok ()))) in
    expect "memory retry result" (result = Ok ());
    expect "memory retry attempts" (!calls = retries + 1);
    expect "memory retry balance" (Z.equal (L.find ledger tx.from).L.balance (Z.of_int 60));
    expect "memory retry nonce" ((L.find ledger tx.from).L.nonce = 1);
    expect "memory retry journal" (not (L.journal_active ledger));
    let state = Lwt_main.run (L.hash ledger) in
    S.abort_epoch_batch store;
    state) in
  expect "memory retry state differs" (execute 2 = execute 0)

let test_key_retry () =
  let module Shell = Octra_node_runtime.Consensus_epoch_key_switch_shell in
  let module Private = Octra_core.Private_ledger in
  let calls = ref 0 in
  let confirmed = ref 0 in
  let refused = ref 0 in
  let failure = Private.{tag = "key_switch_worker_retry"; reason = "worker unavailable";
    user_reason = "worker unavailable"} in
  let unavailable = Private.Key_switch_rejected {failure; consume_nonce = false} in
  let deps = Shell.{
    gate = (fun () -> None);
    apply = (fun () ->
      incr calls;
      Lwt.return (if !calls = 1 then unavailable
        else Private.Key_switch_applied {old_key_hash = "old"; new_key_hash = "new";
          migrated_cipher = true}));
    reject_gate = (fun _ -> failwith "unexpected private gate");
    reject_key_switch = (fun ~event:_ _ -> incr refused; Lwt.return_unit);
    log_applied = (fun _ -> ());
    incr_fhe = (fun () -> ());
    confirm = (fun () -> incr confirmed; Lwt.return_unit);
  } in
  Lwt_main.run (Shell.run deps);
  expect "key switch worker became rejection" (!calls = 2 && !confirmed = 1 && !refused = 0);
  let retry = Shell.run {deps with apply = (fun () -> Lwt.return unavailable)} in
  Lwt.cancel retry;
  expect "key switch retry ignored cancellation" (Lwt.state retry = Lwt.Fail Lwt.Canceled);
  expect "key switch cancellation wrote receipt" (!confirmed = 1 && !refused = 0)

let burn_inputs () =
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch_id = (Option.get
    (Octra_core.Rule_graph.seatless_activation_for_chain chain_id)).activation_epoch in
  let validators = List.mapi (fun index weight ->
    let public_key = Base64.encode_exn (String.make 32 (Char.chr (80 + index))) in
    X.{address = Octra_core.Crypto.Address.address_from_pubkey public_key;
      public_key = Some public_key; weight = Z.of_int weight}) [4; 3; 2; 1] in
  let first = List.hd validators in
  chain_id, epoch_id, X.{proposer_addr = first.address;
    proposer_public_key = first.public_key; validators}

let burn_registry epoch addresses =
  let slashes = List.mapi (fun index offender -> `Assoc [
    "id", `String (Base64.encode_exn (String.make 32 (Char.chr (index + 1))));
    "offender", `String offender;
    "evidence_epoch", `String (string_of_int (epoch - 1));
    "applied_epoch", `String (string_of_int epoch);
    "amount", `String "999999";
  ]) addresses in
  `Assoc ["standard", `String Octra_core.Validator_policy.standard_name;
    "candidates", `List []; "slashes", `List slashes]
  |> Yojson.Safe.to_string |> Octra_core.Validator_registry.of_string
  |> expect_ok "burn registry" |> Octra_core.Validator_registry.to_string |> Option.some

let burn_plan ~base ~fees =
  X.build_reward_plan_with_base ~fee_burn_active:true
    ~supply_retired:(Z.sub Octra_core.Denomination.max_supply (Z.of_int (1000 + base)))
    ~base_reward:(Z.of_int base) ~validator_count:4 ~emission_remaining:(Z.of_int base)
    ~confirmed_fees:(Z.of_int fees) ~prev_supply:(Z.of_int 1000)
  |> expect_ok "burn plan"

let check_burn_values reward plan expected =
  let credits = X.reward_credits reward plan |> expect_ok "burn credits" in
  List.iter2 (fun (member : X.reward_validator) amount ->
    let actual = List.find_opt (fun (credit : X.reward_credit) ->
      credit.address = member.address) credits
      |> Option.fold ~none:Z.zero ~some:(fun (credit : X.reward_credit) -> credit.amount) in
    expect "burn changed individual share" (Z.equal actual (Z.of_int amount)))
    reward.X.validators expected;
  expect "burn net conservation" (X.sum_reward_credits credits = plan.X.total_reward);
  expect "burn envelope differs"
    (Z.add plan.new_total_supply (Z.add plan.new_emission_remaining plan.new_supply_retired)
      = Octra_core.Denomination.max_supply)

let test_burn_shares () =
  let chain_id, epoch_id, reward = burn_inputs () in
  let raw = burn_plan ~base:100 ~fees:37 in
  List.iter (fun (indices, burned, expected) ->
    let addresses = List.map (fun index ->
      (List.nth reward.X.validators index).X.address) indices in
    let registry = burn_registry epoch_id addresses in
    let plan = X.prepare_reward_plan ~chain_id ~epoch_id ~registry ~reward raw
      |> expect_ok "burn prepare" in
    expect "burn amount differs" (plan.reward_burned = Z.of_int burned);
    expect "burn gross splits changed"
      (plan.each_validator = raw.each_validator && plan.remainder = raw.remainder
       && plan.proposer_total = raw.proposer_total);
    expect "burn reserve changed" (plan.new_emission_remaining = raw.new_emission_remaining);
    expect "burn supply differs" (plan.new_total_supply = Z.of_int (1093 - burned));
    check_burn_values reward plan expected;
    expect "burn checked credits differ"
      (X.reward_credits_at ~chain_id ~epoch_id ~registry ~reward plan = X.reward_credits reward plan))
    [[], 0, [109; 11; 7; 3]; [1], 11, [109; 0; 7; 3];
     [0], 109, [0; 11; 7; 3]; [0; 1; 2; 3], 130, [0; 0; 0; 0]]

let test_burn_guards () =
  let chain_id, epoch_id, reward = burn_inputs () in
  let raw = burn_plan ~base:100 ~fees:37 in
  let registry = burn_registry epoch_id [reward.X.proposer_addr] in
  let prepare epoch_id registry plan = X.prepare_reward_plan ~chain_id ~epoch_id ~registry ~reward plan in
  let plan = prepare epoch_id registry raw |> expect_ok "burn guard prepare" in
  expect "burn prepared twice" (Result.is_error (prepare epoch_id registry plan));
  expect "burn raw footer plan accepted"
    (Result.is_error (X.reward_credits_at ~chain_id ~epoch_id ~registry ~reward raw));
  expect "burn forged amount accepted"
    (Result.is_error (X.reward_credits reward {plan with reward_burned = Z.succ plan.reward_burned}));
  expect "burn future slash applied"
    (prepare epoch_id (burn_registry (epoch_id + 1) [reward.proposer_addr]) raw = Ok raw);
  expect "burn past slash ignored"
    (prepare (epoch_id + 1) registry raw = Ok plan);
  expect "burn unresolved offender recovered later rewards"
    (prepare (epoch_id + 2) registry raw = Ok plan);
  expect "burn historical reward rewritten" (prepare (epoch_id - 1) registry raw = Ok raw);
  expect "burn historical malformed registry changed behavior"
    (prepare (epoch_id - 1) (Some "bad") raw = Ok raw);
  expect "burn malformed registry ignored" (Result.is_error (prepare epoch_id (Some "bad") raw));
  expect "burn duplicate exclusions accepted"
    (Result.is_error (X.reward_credits reward {plan with excluded = plan.excluded @ plan.excluded}))

let test_burn_dust () =
  let chain_id, epoch_id, reward = burn_inputs () in
  let registry = burn_registry epoch_id [reward.X.proposer_addr] in
  let plan = X.prepare_reward_plan ~chain_id ~epoch_id ~registry ~reward
    (burn_plan ~base:1 ~fees:0) |> expect_ok "burn dust prepare" in
  expect "burn proposer dust paid" (plan.reward_burned = Z.one && plan.total_reward = Z.zero);
  check_burn_values reward plan [0; 0; 0; 0];
  let addresses = List.map (fun (member : X.reward_validator) -> member.address) reward.validators in
  let plan = X.prepare_reward_plan ~chain_id ~epoch_id ~registry:(burn_registry epoch_id addresses)
    ~reward (burn_plan ~base:0 ~fees:37) |> expect_ok "burn fee only" in
  expect "burn fee only amount differs" (plan.reward_burned = Z.of_int 30);
  expect "burn fee only supply differs" (plan.new_total_supply = Z.of_int 963);
  check_burn_values reward plan [0; 0; 0; 0]

let burn_signer () =
  let raw = String.make 32 '\090' in
  let secret = Mirage_crypto_ec.Ed25519.priv_of_octets raw |> function
    | Ok secret -> secret | Error _ -> fail "burn signing key refused" in
  let public = Mirage_crypto_ec.Ed25519.pub_of_priv secret
    |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn in
  Base64.encode_exn raw, public, Octra_core.Crypto.Address.address_from_pubkey public

let seed_burn ?(durable = false) store ledger registry =
  let set key value = Lwt_main.run (S.set_meta store key value) in
  let _, public, payer = burn_signer () in
  expect "burn payer added" (L.add_account_with_pubkey ledger payer (Z.of_int 1000) public = Ok ());
  Lwt_main.run (L.flush_dirty_lwt ledger);
  set "total_supply" "1000";
  set "emission_remaining" (string_of_int ES.duration_epochs);
  Option.iter (set Octra_core.Validator_registry.meta_key) registry;
  if durable then begin
    let _, _, reward = burn_inputs () in
    set (X.validator_penalty_key reward.proposer_addr) (Base64.encode_exn (String.make 32 'p'))
  end

let burn_backend store ledger epoch_id =
  X.make_live_backend ~emission_policy:E.Allow
    ~emission_schedule:(ES.Security_curve {activation_epoch = epoch_id})
    ~validator_policy:Octra_core.Validator_policy.Inactive store ledger

let apply_burn_node backend (reward : X.reward_attribution) epoch_id =
  let module Footer = Octra_node_runtime.Consensus_epoch_apply_footer in
  let open Lwt.Syntax in
  let values = Hashtbl.create 12 in
  let* () = Lwt_list.iter_s (fun key ->
    let* value = S.get_meta backend.X.store key in
    Option.iter (Hashtbl.replace values key) value; Lwt.return_unit)
    (["total_supply"; "emission_remaining"; Octra_core.Validator_registry.meta_key]
      @ List.map X.validator_penalty_key (X.reward_addresses reward)) in
  let set_meta key value =
    let* () = backend.set_meta key value in
    Hashtbl.replace values key value; Lwt.return_unit in
  let backend = {backend with X.set_meta} in
  let deps = Footer.{get_meta = Hashtbl.find_opt values; set_meta; policy = E.Allow;
    schedule = backend.emission_schedule; legacy_total = None;
    public_supply = backend.ops.total_supply;
    apply_footer = (fun env reward plan -> X.apply_epoch_footer_with_reward ~backend ~env ~reward ~plan);
    log = (fun _ -> ())} in
  let request = Footer.{chain_id = "octra-devnet-9871-cluster"; epoch_id; epoch_ts = 40.;
    proposer_addr = reward.X.proposer_addr; validator_pubkeys = []; reward;
    ready_state_root_at = (fun _ -> Lwt.return_none); ready_max_lag = -1;
    confirmed_fees = Z.of_int 37; short = Fun.id} in
  let* result = Footer.run_node deps request in
  expect "burn node net plan differs"
    (result.plan.reward_burned = Z.of_int 25 && result.plan.total_reward = Z.of_int 6);
  expect "burn node exposed excluded recipient"
    (List.for_all (fun (recipient : Octra_core.Epochlog.reward_recipient) ->
      recipient.reward_addr <> reward.proposer_addr) result.reward_recipients);
  Lwt.return_unit

let run_burn_path node store ledger (reward : X.reward_attribution) epoch_id =
  let module C = Octra_consensus.C_types in
  let voting = List.map2 (fun (member : X.reward_validator) weight ->
    C.{address = member.address; pubkey = Base64.decode_exn (Option.get member.public_key)}, Z.of_int weight)
    reward.validators [3; 3; 2; 2]
    |> C.make_weighted_validator_set |> expect_ok "burn voting set" in
  let backend = { (burn_backend store ledger epoch_id) with X.voting_set = Some (epoch_id, voting) } in
  let env = {env with X.chain_id = "octra-devnet-9871-cluster"; epoch_id;
    proposer_addr = reward.X.proposer_addr;
    validator_addrs = List.map (fun member -> member.C.address) voting.validators;
    validator_pubkeys = List.map (fun member ->
      member.C.address, Base64.encode_exn member.pubkey) voting.validators} in
  let secret, public, payer = burn_signer () in
  let tx = {(atomic_tx 1) with T.from = payer; to_ = reward.proposer_addr;
    public_key = Some public; amount = Z.zero; ou = Z.of_int 37} in
  let tx = Octra_core.Tx_signature.sign
    (Octra_core.Tx_signature.at ~chain_id:env.chain_id ~epoch:(Int64.of_int epoch_id)) tx secret in
  let charge () = L.debit ledger payer tx.ou tx.nonce |> expect_ok "burn fee debit" in
  let open Lwt.Syntax in
  let* () = if node then begin
    let* () = S.begin_epoch_batch store in
    let* () = X.prepare_validator_penalties ~backend ~env in
    charge ();
    let* () = X.advance_validator_set ~backend ~env in
    let* () = apply_burn_node backend reward epoch_id in
    let* () = L.flush_dirty_lwt ledger in
    S.commit_epoch_batch store "burn-node"
  end else
    let* result = X.run_core ~reward:(Some reward) ~preverify:None ~backend ~env
      ~txs:[tx] ~process_tx:(fun ~backend:_ ~env:_ _ ->
        charge (); Lwt.return_ok (X.Confirmed (Z.of_int 37))) in
    ignore (expect_ok "burn core apply" result); Lwt.return_unit in
  List.iter2 (fun (member : X.reward_validator) expected ->
    let balance = L.find_opt ledger member.address
      |> Option.fold ~none:Z.zero ~some:(fun account -> account.L.balance) in
    expect "burn applied share differs" (balance = Z.of_int expected)) reward.validators [0; 3; 2; 1];
  expect "burn public total differs" (L.get_total_supply ledger = Z.of_int 969);
  let* total = S.get_meta store "total_supply" in
  let* remaining = S.get_meta store "emission_remaining" in
  let* retired = S.get_meta store ES.retired_key in
  expect "burn metadata total differs" (total = Some "969");
  expect "burn reserve metadata differs" (remaining = Some (string_of_int (ES.duration_epochs - 1)));
  expect "burn schedule overwrote retired"
    (retired = Some (Z.to_string (Z.sub Octra_core.Denomination.max_supply
      (Z.of_int (ES.duration_epochs + 968)))));
  S.state_hash store

let test_burn_zero_footer () =
  let chain_id, epoch_id, reward = burn_inputs () in
  let addresses = List.map (fun (member : X.reward_validator) -> member.address) reward.X.validators in
  let registry = burn_registry epoch_id addresses in
  let plan = X.prepare_reward_plan ~chain_id ~epoch_id ~registry ~reward
    (burn_plan ~base:100 ~fees:37) |> expect_ok "burn zero prepare" in
  with_store (fun store ->
    let ledger = L.create store in
    expect "burn zero payer" (L.add_account ledger env.X.proposer_addr (Z.of_int 1000) = Ok ());
    expect "burn zero fee" (L.debit ledger env.X.proposer_addr (Z.of_int 37) 1 = Ok ());
    Option.iter (fun raw -> Lwt_main.run (S.set_meta store
      Octra_core.Validator_registry.meta_key raw)) registry;
    let backend = burn_backend store ledger epoch_id in
    Lwt_main.run (X.apply_epoch_footer_with_reward ~backend ~reward ~plan
      ~env:{env with X.chain_id; epoch_id; proposer_addr = reward.proposer_addr});
    expect "burn zero created recipients" (List.for_all (fun address ->
      L.find_opt ledger address = None) addresses);
    expect "burn zero public supply" (L.get_total_supply ledger = Z.of_int 963);
    expect "burn zero supply metadata" (Lwt_main.run (S.get_meta store "total_supply") = Some "963");
    expect "burn zero emission metadata" (Lwt_main.run (S.get_meta store "emission_remaining") = Some "0");
    expect "burn zero retired metadata" (Lwt_main.run (S.get_meta store ES.retired_key)
      = Some (Z.to_string (Z.sub Octra_core.Denomination.max_supply (Z.of_int 963)))))

let test_burn_paths () =
  let _, epoch_id, reward = burn_inputs () in
  let registry = burn_registry epoch_id [reward.X.proposer_addr] in
  let run node = with_store (fun store ->
    let ledger = L.create store in seed_burn store ledger registry;
    Lwt_main.run (run_burn_path node store ledger reward epoch_id)) in
  let core = run false in
  expect "burn node and core roots differ" (run true = core);
  with_store (fun store ->
    let ledger = L.create store in seed_burn store ledger registry;
    let before = Lwt_main.run (S.state_hash store) in
    let result = Octra_core.State_preview.with_state ~base_store:store ~base_ledger:ledger
      ~epoch_id ~proposal_id:"reward-burn" (fun store ledger ->
        Lwt.map Result.ok (run_burn_path false store ledger reward epoch_id))
      |> Lwt_main.run |> expect_ok "burn preview" in
    expect "burn preview and apply differ" (result = core);
    expect "burn preview changed live root" (Lwt_main.run (S.state_hash store) = before);
    expect "burn preview changed live balance"
      ((L.find ledger (let _, _, payer = burn_signer () in payer)).L.balance = Z.of_int 1000))

let test_burn_pruned_node () =
  let _, epoch_id, reward = burn_inputs () in
  let run node = with_store (fun store ->
    let ledger = L.create store in
    seed_burn ~durable:true store ledger None;
    Lwt_main.run (run_burn_path node store ledger reward epoch_id)) in
  expect "durable penalty node and core roots differ" (run true = run false)

let test_voting_source () =
  let module C = Octra_consensus.C_types in
  let chain_id, epoch_id, reward = burn_inputs () in
  let source = List.map2 (fun (member : X.reward_validator) weight ->
    let pubkey = Base64.decode_exn (Option.get member.public_key) in
    C.{address = member.address; pubkey}, Z.of_int weight)
    reward.validators [3; 3; 2; 2]
    |> C.make_weighted_validator_set |> expect_ok "voting source" in
  let offender = (List.nth source.validators 3).C.address in
  let next, _ = C.evict_validator_set ~offenders:[offender] source
    |> expect_ok "voting removal" in
  expect "voting entry cap missing"
    (Result.is_error (C.checked_validator_set_for_epoch ~chain_id
      ~epoch_id:(Int64.of_int epoch_id) next));
  with_store (fun store ->
    let env = {env with X.chain_id; epoch_id;
      validator_addrs = List.map (fun member -> member.C.address) next.validators;
      validator_pubkeys = List.map (fun member ->
        member.C.address, Base64.encode_exn member.pubkey) next.validators} in
    let backend = X.make_live_backend ~voting_set:(epoch_id, next) store (L.create store) in
    expect "voting source rejected" (X.current_voting_set ~backend ~env = Ok next);
    List.iter (fun (name, backend, env) ->
      expect name (Result.is_error (X.current_voting_set ~backend ~env))) [
      "voting missing accepted", {backend with voting_set = None}, env;
      "voting epoch accepted", {backend with voting_set = Some (epoch_id - 1, next)}, env;
      "voting members accepted", backend, {env with validator_addrs = []};
      "voting keys accepted", backend, {env with validator_pubkeys = []};
    ])

let () =
  test_voting_source ();
  test_burn_shares ();
  test_burn_guards ();
  test_burn_dust ();
  test_burn_zero_footer ();
  test_burn_paths ();
  test_burn_pruned_node ();
  test_execution_codec ();
  test_execution_state ();
  test_execution_refusal ();
  test_execution_chain ();
  test_execution_parity ();
  test_execution_capture ();
  test_checkpoint_outcomes ();
  test_checkpoint_journal ();
  test_checkpoint_cancel ();
  test_checkpoint_commit ();
  test_preview_prefix ();
  test_preview_senders ();
  test_standard_gate ();
  test_policy ();
  test_reward_plan ();
  test_reward_env ();
  test_weighted_reward_credits ();
  test_reward_finality_binding ();
  test_set_reward ();
  test_validator_unbond ();
  test_consensus_standard ();
  test_consensus_cutover ();
  test_integer_work_gate ();
  test_epoch_serialization ();
  test_epoch_busy_gate ();
  test_upgrade_ready_refresh ();
  test_reward_properties ();
  test_fee_burn_parity ();
  test_fee_burn_epoch_accounting ();
  test_credit_failure ();
  test_legacy_zero_epoch ();
  test_guard_allows_inactive_epoch ();
  test_security_curve_epoch_parity ();
  test_zero_reserve_cutover ();
  test_ledger_journal ();
  test_kat_journal ();
  test_sender_key_policy ();
  test_sender_key_activation ();
  test_sender_key_preview_parity ();
  test_preview_clean_ledger ();
  test_tx_reject_rollback ();
  test_sender_key_after_fee ();
  test_tx_exception_rollback ();
  test_worker_retry_rollback ();
  test_worker_retry_loop ();
  test_worker_stop_rollback ();
  test_memory_rollback ();
  test_key_retry ();
  print_endline "status = pass test = epoch_exec_reward_atomic"