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
  let tail =
    X.build_reward_plan
      ~fee_burn_active:false
      ~supply_retired:Z.zero
      ~validator_count:3
      ~emission_remaining:Z.zero
      ~confirmed_fees:(Z.of_int 5)
      ~prev_supply:max
    |> expect_ok "fee tail"
  in
  expect "fee tail has no mint" (Z.equal tail.base_reward Z.zero);
  expect "fee tail preserves supply" (Z.equal tail.new_total_supply max);
  expect "fee tail redistributes fees" (Z.equal tail.total_reward (Z.of_int 5));
  expect "distribution exact"
    (Z.equal
       (Z.add tail.proposer_total
          (Z.mul tail.each_validator (Z.of_int 3)))
       tail.total_reward);
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
    Octra_core.Validator_admission.unbonding_epochs = 8L;
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
  let first = P.standard_hash ~chain_id:"standard-a" getenv in
  let repeated = P.standard_hash ~chain_id:"standard-a" getenv in
  let other = P.standard_hash ~chain_id:"standard-b" getenv in
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
  let full = P.standard_hash ~chain_id getenv in
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
  expect "waiting epoch apply is stale"
    (match second_result with V.Busy -> true | V.Applied () -> false);
  expect "stale epoch payload is discarded"
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
    profile = None;
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
  let tail = X.emission_tail in
  let threshold = Z.mul divisor tail in
  let remaining_values = [
    Z.zero;
    Z.one;
    Z.pred tail;
    tail;
    Z.succ tail;
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
              ~fold:X.prior_fold
              ~epoch_id:env.X.epoch_id
              ~proposal_id:"sender-key-preview"
              (fun backend ->
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

let test_preview_clone_requires_clean_ledger () =
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
        ~fold:X.prior_fold
        ~epoch_id:env.X.epoch_id
        ~proposal_id:"dirty-preview"
        (fun _ -> Lwt.return_ok ())
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

let test_sender_key_rejected_after_fee () =
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
  expect "worker retry preserves failure" failed

let () =
  test_standard_gate ();
  test_policy ();
  test_reward_plan ();
  test_reward_env ();
  test_weighted_reward_credits ();
  test_reward_finality_binding ();
  test_validator_unbond ();
  test_consensus_standard ();
  test_consensus_cutover ();
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
  test_sender_key_policy ();
  test_sender_key_activation ();
  test_sender_key_preview_parity ();
  test_preview_clone_requires_clean_ledger ();
  test_tx_reject_rollback ();
  test_sender_key_rejected_after_fee ();
  test_tx_exception_rollback ();
  test_worker_retry_rollback ();
  test_worker_retry_loop ();
  print_endline "status = pass test = epoch_exec_reward_atomic"