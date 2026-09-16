(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Graph = Octra_core.Rule_graph

let fail name = failwith name

let require condition name =
  if not condition then fail name

let activation graph =
  match Graph.circle_activation graph with
  | Some value -> value
  | None -> fail "devnet activation missing"

let validator_quorum_activation graph =
  match Graph.validator_quorum_activation graph with
  | Some value -> value
  | None -> fail "devnet validator quorum activation missing"

let epoch_time_activation graph =
  match Graph.epoch_time_activation graph with
  | Some value -> value
  | None -> fail "devnet epoch time activation missing"

let owner_migration_activation graph =
  match Graph.owner_migration_activation graph with
  | Some value -> value
  | None -> fail "devnet owner migration activation missing"

let wasm_compute_activation graph =
  match Graph.wasm_compute_activation graph with
  | Some value -> value
  | None -> fail "devnet wasm compute activation missing"

let private_payload_activation graph =
  match Graph.private_payload_activation graph with
  | Some value -> value
  | None -> fail "devnet private payload activation missing"

let set_fold_activation graph =
  match Graph.set_fold_activation graph with
  | Some value -> value
  | None -> fail "devnet set fold activation missing"

let set_live_activation graph =
  match Graph.set_live_activation graph with
  | Some value -> value
  | None -> fail "devnet set live activation missing"

let object_cost_activation graph =
  match Graph.object_cost_activation graph with
  | Some value -> value
  | None -> fail "devnet object cost activation missing"

let account_pack_activation graph =
  match Graph.account_pack_activation graph with
  | Some value -> value
  | None -> fail "devnet account pack activation missing"

let standard_activation graph =
  match Graph.standard_activation graph with
  | Some value -> value
  | None -> fail "devnet standard activation missing"

let graph root_at =
  Graph.create ~chain_id:"octra-devnet-9871-cluster" ~root_at

let test_set_plan () =
  let chain_id = "octra-devnet-9871-cluster" in
  let root = "8e7f0e5a6e582070c040a07e7439caf532973fa09cddc79357a5ed964468065d" in
  let seed = graph (fun _ -> fail "set plan read before activation") in
  let plan = Option.get (Graph.set_plan_activation seed) in
  require
    (plan.anchor_epoch = 1_504_440
     && plan.anchor_state_root = root
     && plan.activation_epoch = 1_510_000)
    "set plan activation changed";
  require
    (Graph.profile_epochs ~chain_id = [1_500_000; 1_510_000])
    "profile epochs changed";
  List.iter (fun epoch ->
    require (Graph.set_plan seed ~epoch = Ok Graph.Prior)
      "set plan active before activation";
    require (Graph.set_plan_at ~chain_id ~epoch = Graph.Prior)
      "set plan profile active before activation")
    [0; 1_500_000; 1_507_999; 1_508_000; 1_508_001; 1_509_999];
  let matching = graph (fun epoch ->
    require (epoch = 1_504_440) "set plan read wrong anchor";
    Graph.Root root) in
  List.iter (fun epoch ->
    require (Graph.set_plan matching ~epoch = Ok Graph.Active)
      "set plan inactive after activation";
    require (Graph.set_plan_at ~chain_id ~epoch = Graph.Active)
      "set plan profile inactive after activation";
    require
      (Graph.set_plan (graph (fun _ -> Graph.Missing)) ~epoch
       = Error (Graph.Anchor_missing 1_504_440))
      "set plan accepted missing anchor";
    require
      (match Graph.set_plan (graph (fun _ -> Graph.Root "other")) ~epoch with
       | Error (Graph.Anchor_mismatch fault) ->
         fault.epoch = 1_504_440 && fault.expected = root && fault.actual = "other"
       | _ -> false)
      "set plan accepted different anchor";
    require
      (match Graph.set_plan (graph (fun _ -> Graph.Unreadable "read")) ~epoch with
       | Error (Graph.Anchor_unreadable fault) ->
         fault.epoch = 1_504_440 && fault.reason = "read"
       | _ -> false)
      "set plan accepted unreadable anchor")
    [1_510_000; 1_510_001; max_int];
  List.iter (fun (floor_epoch, expected) ->
    require
      (Graph.root_after_floor ~chain_id ~floor_epoch ~epoch:1_504_440 = expected)
      "set plan floor root differs")
    [1_504_439, None; 1_504_440, Some root; 1_510_001, Some root];
  let pruned = graph (fun epoch ->
    match Graph.root_after_floor ~chain_id ~floor_epoch:1_510_001 ~epoch with
    | Some root -> Graph.Root root
    | None -> Graph.Missing) in
  require (Graph.set_plan pruned ~epoch:1_510_001 = Ok Graph.Active)
    "set plan rejected verified floor";
  let prior = Graph.consensus_id ~chain_id ~epoch:1_500_000 in
  require (Graph.consensus_id ~chain_id ~epoch:1_509_999 = prior)
    "set plan changed prior graph";
  let entry = "|1504440:" ^ root ^ ":1510000" in
  let active = prior ^ entry ^ entry in
  List.iter (fun epoch ->
    require (Graph.consensus_id ~chain_id ~epoch = active)
      "set plan graph differs") [1_510_000; 1_510_001; max_int];
  let chain_id = "other" in
  let other = Graph.create ~chain_id ~root_at:(fun _ -> fail "other anchor read") in
  require (Graph.set_plan_activation other = None) "other set plan present";
  require (Graph.profile_epochs ~chain_id = []) "other profile epochs present";
  require
    (Graph.root_after_floor ~chain_id ~floor_epoch:max_int ~epoch:1_504_440 = None)
    "other floor accepted devnet anchor";
  List.iter (fun epoch ->
    require (Graph.set_plan other ~epoch = Ok Graph.Prior)
      "other set plan active";
    require (Graph.set_plan_at ~chain_id ~epoch = Graph.Prior)
      "other set plan profile active") [0; 1_510_000; 1_510_001; max_int]

let test_math () =
  let chain_id = "octra-devnet-9871-cluster" in
  let missing = graph (fun _ -> Graph.Missing) in
  let plan = Option.get (Graph.math_activation missing) in
  require (plan.activation_epoch = 1_510_000) "math activation differs";
  let matching = graph (fun epoch ->
    require (epoch = plan.anchor_epoch) "math anchor epoch differs";
    Graph.Root plan.anchor_state_root) in
  List.iter (fun epoch ->
    let expected = if epoch < plan.activation_epoch then Graph.Prior else Graph.Active in
    require (Graph.math_at ~chain_id ~epoch = expected) "math profile differs";
    require (Graph.math matching ~epoch = Ok expected) "math rule differs";
    if expected = Graph.Active then begin
      require (Graph.math missing ~epoch = Error (Graph.Anchor_missing plan.anchor_epoch))
        "math missing anchor accepted";
      List.iter (fun root ->
        require (Result.is_error (Graph.math (graph (fun _ -> root)) ~epoch))
          "math invalid anchor accepted")
        [Graph.Root "other"; Graph.Unreadable "read"]
    end else
      require (Graph.math missing ~epoch = Ok Graph.Prior) "math read before activation")
    [0; 1_500_000; 1_507_999; 1_508_000; 1_508_001;
     1_509_999; 1_510_000; 1_510_001; max_int];
  let other = Graph.create ~chain_id:"other" ~root_at:(fun _ -> fail "other math read") in
  require (Graph.math other ~epoch:max_int = Ok Graph.Prior) "other math enabled"

let () =
  test_math ();
  test_set_plan ();
  let seed = graph (fun _ -> Graph.Unreadable "unused") in
  let plan = activation seed in
  let matching =
    graph (fun epoch ->
      if epoch = plan.anchor_epoch then Graph.Root plan.anchor_state_root
      else Graph.Missing)
  in
  require
    (Graph.circle matching ~epoch:(plan.activation_epoch - 1) = Ok Graph.Prior)
    "new rule differs from prior rule before activation";
  require
    (Graph.circle matching ~epoch:plan.activation_epoch = Ok Graph.Active)
    "new rule inactive at activation";
  require
    (Graph.circle matching ~epoch:(plan.activation_epoch + 1) = Ok Graph.Active)
    "new rule inactive after activation";
  require
    (Graph.root_after_floor
       ~chain_id:"octra-devnet-9871-cluster"
       ~floor_epoch:plan.activation_epoch
       ~epoch:plan.anchor_epoch
     = Some plan.anchor_state_root)
    "verified floor did not recover exact activation root";
  require
    (Graph.root_after_floor
       ~chain_id:"octra-devnet-9871-cluster"
       ~floor_epoch:(plan.activation_epoch - 1)
       ~epoch:plan.anchor_epoch
     = None)
    "pre-activation floor recovered activation root";
  require
    (Graph.root_after_floor
       ~chain_id:"octra-devnet-9871-cluster"
       ~floor_epoch:plan.activation_epoch
       ~epoch:(plan.anchor_epoch + 1)
     = None)
    "floor recovered an unbound epoch root";
  let missing = graph (fun _ -> Graph.Missing) in
  require
    (match Graph.circle missing ~epoch:plan.activation_epoch with
     | Error (Graph.Anchor_missing epoch) -> epoch = plan.anchor_epoch
     | _ -> false)
    "missing activation anchor accepted";
  let mismatched = graph (fun _ -> Graph.Root (String.make 64 '0')) in
  require
    (match Graph.circle mismatched ~epoch:plan.activation_epoch with
     | Error (Graph.Anchor_mismatch value) ->
       value.epoch = plan.anchor_epoch
       && String.equal value.expected plan.anchor_state_root
     | _ -> false)
    "mismatched activation anchor accepted";
  let unrelated =
    Graph.create
      ~chain_id:"unrelated-chain"
      ~root_at:(fun _ -> Graph.Unreadable "unused")
  in
  require
    (Graph.circle unrelated ~epoch:max_int = Ok Graph.Prior)
    "unrelated network activated devnet rule";
  let quorum_plan = validator_quorum_activation seed in
  let quorum_graph =
    graph (fun epoch ->
      if epoch = quorum_plan.anchor_epoch then
        Graph.Root quorum_plan.anchor_state_root
      else
        Graph.Missing)
  in
  require
    (Graph.validator_quorum
       quorum_graph
       ~epoch:(quorum_plan.activation_epoch - 1)
     = Ok Graph.Prior)
    "validator quorum rule differs before activation";
  require
    (Graph.validator_quorum
       quorum_graph
       ~epoch:quorum_plan.activation_epoch
     = Ok Graph.Active)
    "validator quorum rule inactive at activation";
  require
    (Graph.validator_quorum
       quorum_graph
       ~epoch:(quorum_plan.activation_epoch + 1)
     = Ok Graph.Active)
    "validator quorum rule inactive after activation";
  require
    (Graph.root_after_floor
       ~chain_id:"octra-devnet-9871-cluster"
       ~floor_epoch:quorum_plan.anchor_epoch
       ~epoch:quorum_plan.anchor_epoch
     = Some quorum_plan.anchor_state_root)
    "verified floor did not recover validator quorum anchor";
  require
    (Graph.validator_quorum unrelated ~epoch:0 = Ok Graph.Active)
    "new network did not activate protected quorum at genesis";
  require
    (Graph.validator_quorum unrelated ~epoch:max_int = Ok Graph.Active)
    "new network lost protected quorum after genesis";
  let time_plan = epoch_time_activation seed in
  let time_graph =
    graph (fun epoch ->
      if epoch = time_plan.anchor_epoch then
        Graph.Root time_plan.anchor_state_root
      else
        Graph.Missing)
  in
  require
    (Graph.epoch_time
       time_graph
       ~epoch:(time_plan.activation_epoch - 1)
     = Ok Graph.Prior)
    "epoch time rule differs before activation";
  require
    (Graph.epoch_time
       time_graph
       ~epoch:time_plan.activation_epoch
     = Ok Graph.Active)
    "epoch time rule inactive at activation";
  require
    (Graph.epoch_time
       time_graph
       ~epoch:(time_plan.activation_epoch + 1)
     = Ok Graph.Active)
    "epoch time rule inactive after activation";
  require
    (Graph.root_after_floor
       ~chain_id:"octra-devnet-9871-cluster"
       ~floor_epoch:time_plan.anchor_epoch
       ~epoch:time_plan.anchor_epoch
     = Some time_plan.anchor_state_root)
    "verified floor did not recover epoch time anchor";
  require
    (Graph.epoch_time unrelated ~epoch:0 = Ok Graph.Active)
    "new network did not activate uniform epoch time at genesis";
  require
    (Graph.epoch_time unrelated ~epoch:max_int = Ok Graph.Active)
    "new network lost uniform epoch time after genesis";
  let owner_plan = owner_migration_activation seed in
  let owner_graph =
    graph (fun epoch ->
      if epoch = owner_plan.anchor_epoch then
        Graph.Root owner_plan.anchor_state_root
      else
        Graph.Missing)
  in
  require
    (Graph.owner_migration
       owner_graph
       ~epoch:(owner_plan.activation_epoch - 1)
     = Ok Graph.Prior)
    "owner migration rule differs before activation";
  require
    (Graph.owner_migration
       owner_graph
       ~epoch:owner_plan.activation_epoch
     = Ok Graph.Active)
    "owner migration rule inactive at activation";
  require
    (Graph.owner_migration
       owner_graph
       ~epoch:(owner_plan.activation_epoch + 1)
     = Ok Graph.Active)
    "owner migration rule inactive after activation";
  let owner_missing = graph (fun _ -> Graph.Missing) in
  require
    (match
       Graph.owner_migration
         owner_missing
         ~epoch:owner_plan.activation_epoch
     with
     | Error (Graph.Anchor_missing epoch) -> epoch = owner_plan.anchor_epoch
     | _ -> false)
    "missing owner migration anchor accepted";
  let owner_mismatched = graph (fun _ -> Graph.Root (String.make 64 '0')) in
  require
    (match
       Graph.owner_migration
         owner_mismatched
         ~epoch:owner_plan.activation_epoch
     with
     | Error (Graph.Anchor_mismatch value) ->
       value.epoch = owner_plan.anchor_epoch
       && String.equal value.expected owner_plan.anchor_state_root
     | _ -> false)
    "mismatched owner migration anchor accepted";
  require
    (Graph.root_after_floor
       ~chain_id:"octra-devnet-9871-cluster"
       ~floor_epoch:owner_plan.anchor_epoch
       ~epoch:owner_plan.anchor_epoch
     = Some owner_plan.anchor_state_root)
    "verified floor did not recover owner migration anchor";
  require
    (Graph.owner_migration unrelated ~epoch:max_int = Ok Graph.Prior)
    "unrelated network activated devnet owner migration";
  let compute_plan = wasm_compute_activation seed in
  let compute_graph =
    graph (fun epoch ->
      if epoch = compute_plan.anchor_epoch then
        Graph.Root compute_plan.anchor_state_root
      else
        Graph.Missing)
  in
  require
    (Graph.wasm_compute
       compute_graph
       ~epoch:(compute_plan.activation_epoch - 1)
     = Ok Graph.Prior)
    "wasm compute rule differs before activation";
  require
    (Graph.wasm_compute
       compute_graph
       ~epoch:compute_plan.activation_epoch
     = Ok Graph.Active)
    "wasm compute rule inactive at activation";
  require
    (Graph.wasm_compute
       compute_graph
       ~epoch:(compute_plan.activation_epoch + 1)
     = Ok Graph.Active)
    "wasm compute rule inactive after activation";
  let compute_missing = graph (fun _ -> Graph.Missing) in
  require
    (match
       Graph.wasm_compute
         compute_missing
         ~epoch:compute_plan.activation_epoch
     with
     | Error (Graph.Anchor_missing epoch) -> epoch = compute_plan.anchor_epoch
     | _ -> false)
    "missing wasm compute anchor accepted";
  let compute_mismatched =
    graph (fun _ -> Graph.Root (String.make 64 '0'))
  in
  require
    (match
       Graph.wasm_compute
         compute_mismatched
         ~epoch:compute_plan.activation_epoch
     with
     | Error (Graph.Anchor_mismatch value) ->
       value.epoch = compute_plan.anchor_epoch
       && String.equal value.expected compute_plan.anchor_state_root
     | _ -> false)
    "mismatched wasm compute anchor accepted";
  require
    (Graph.root_after_floor
       ~chain_id:"octra-devnet-9871-cluster"
       ~floor_epoch:compute_plan.anchor_epoch
       ~epoch:compute_plan.anchor_epoch
     = Some compute_plan.anchor_state_root)
    "verified floor did not recover wasm compute anchor";
  require
    (Graph.wasm_compute unrelated ~epoch:0 = Ok Graph.Active)
    "new network did not activate wasm compute at genesis";
  require
    (Graph.wasm_compute unrelated ~epoch:max_int = Ok Graph.Active)
    "new network lost wasm compute after genesis";
  let payload_plan = private_payload_activation seed in
  let payload_graph =
    graph (fun epoch ->
      if epoch = payload_plan.anchor_epoch then
        Graph.Root payload_plan.anchor_state_root
      else
        Graph.Missing)
  in
  require
    (Graph.private_payload
       payload_graph
       ~epoch:(payload_plan.activation_epoch - 1)
     = Ok Graph.Prior)
    "private payload rule differs before activation";
  require
    (Graph.private_payload
       payload_graph
       ~epoch:payload_plan.activation_epoch
     = Ok Graph.Active)
    "private payload rule inactive at activation";
  require
    (Graph.private_payload
       payload_graph
       ~epoch:(payload_plan.activation_epoch + 1)
     = Ok Graph.Active)
    "private payload rule inactive after activation";
  let payload_missing = graph (fun _ -> Graph.Missing) in
  require
    (match
       Graph.private_payload
         payload_missing
         ~epoch:payload_plan.activation_epoch
     with
     | Error (Graph.Anchor_missing epoch) -> epoch = payload_plan.anchor_epoch
     | _ -> false)
    "missing private payload anchor accepted";
  require
    (Graph.private_payload unrelated ~epoch:0 = Ok Graph.Active)
    "new network did not require strict private payloads at genesis";
  let fold_plan = set_fold_activation seed in
  require (fold_plan.activation_epoch = 1_334_000)
    "set fold activation epoch changed";
  let live_plan = set_live_activation seed in
  require (live_plan.activation_epoch = 1_450_000)
    "set live activation epoch changed";
  let validator_policy =
    Octra_core.Validator_policy.Bonded {
      activation_epoch = 100;
      parameters = Octra_core.Validator_policy.parameters;
      snapshot_interval = 64L;
      evidence_epochs = Octra_core.Validator_policy.evidence_epochs;
    }
  in
  require
    (Octra_core.Validator_policy.snapshot_at
       validator_policy
       ~source_epoch:91L
       ~cadence:4L
       ~delay:8L
     = None)
    "validator snapshot started before evidence boundary";
  require
    (Octra_core.Validator_policy.snapshot_at
       validator_policy
       ~source_epoch:92L
       ~cadence:4L
       ~delay:8L
     = Some 100L)
    "validator snapshot missed activation boundary";
  require
    (Octra_core.Validator_policy.snapshot_at
       validator_policy
       ~source_epoch:94L
       ~cadence:4L
       ~delay:8L
     = None)
    "validator snapshot ignored cadence";
  require
    (Octra_core.Validator_policy.snapshot_at
       validator_policy
       ~source_epoch:96L
       ~cadence:4L
       ~delay:8L
     = Some 104L)
    "validator snapshot delay changed";
  let object_cost_plan = object_cost_activation seed in
  let standard_plan = standard_activation seed in
  let object_cost_graph =
    graph (fun epoch ->
      if epoch = object_cost_plan.anchor_epoch then
        Graph.Root object_cost_plan.anchor_state_root
      else
        Graph.Missing)
  in
  require
    (Graph.object_cost
       object_cost_graph
       ~epoch:(object_cost_plan.activation_epoch - 1)
     = Ok Graph.Prior)
    "object cost rule differs before activation";
  require
    (Graph.object_cost
       object_cost_graph
       ~epoch:object_cost_plan.activation_epoch
     = Ok Graph.Active)
    "object cost inactive at activation";
  require
    (Graph.object_cost
       object_cost_graph
       ~epoch:(standard_plan.activation_epoch - 1)
     = Ok Graph.Active)
    "object cost inactive before standard activation";
  require
    (Graph.object_cost unrelated ~epoch:max_int = Ok Graph.Prior)
    "unrelated network activated devnet object cost";
  let pack_plan = account_pack_activation seed in
  require (pack_plan.activation_epoch = 1_480_000)
    "account pack activation epoch changed";
  let pack_graph =
    graph (fun epoch ->
      if epoch = pack_plan.anchor_epoch then
        Graph.Root pack_plan.anchor_state_root
      else
        Graph.Missing)
  in
  require
    (Graph.account_pack
       pack_graph
       ~epoch:(pack_plan.activation_epoch - 1)
     = Ok Graph.Prior)
    "account pack changed before activation";
  require
    (Graph.account_pack
       pack_graph
       ~epoch:pack_plan.activation_epoch
     = Ok Graph.Active)
    "account pack inactive at activation";
  require
    (match Graph.account_pack missing ~epoch:pack_plan.activation_epoch with
     | Error (Graph.Anchor_missing epoch) -> epoch = pack_plan.anchor_epoch
     | _ -> false)
    "account pack accepted without anchor";
  require
    (Graph.account_pack unrelated ~epoch:max_int = Ok Graph.Prior)
    "unrelated network activated devnet account pack";
  require (standard_plan.activation_epoch = 1_500_000)
    "standard activation epoch changed";
  let standard_graph =
    graph (fun epoch ->
      if epoch = standard_plan.anchor_epoch then
        Graph.Root standard_plan.anchor_state_root
      else if epoch = object_cost_plan.anchor_epoch then
        Graph.Root object_cost_plan.anchor_state_root
      else
        Graph.Missing)
  in
  require
    (Graph.standard
       standard_graph
       ~epoch:(standard_plan.activation_epoch - 1)
     = Ok Graph.Prior)
    "standard changed before activation";
  require
    (Graph.standard
       standard_graph
       ~epoch:standard_plan.activation_epoch
     = Ok Graph.Active)
    "standard inactive at activation";
  require
    (Graph.object_cost
       standard_graph
       ~epoch:standard_plan.activation_epoch
     = Ok Graph.Active)
    "object cost inactive with standard";
  require
    (match Graph.standard missing ~epoch:standard_plan.activation_epoch with
     | Error (Graph.Anchor_missing epoch) -> epoch = standard_plan.anchor_epoch
     | _ -> false)
    "standard accepted without anchor";
  require
    (Graph.standard unrelated ~epoch:max_int = Ok Graph.Prior)
    "unrelated network activated devnet standard";
  require
    (Graph.root_after_floor
       ~chain_id:"octra-devnet-9871-cluster"
       ~floor_epoch:standard_plan.activation_epoch
       ~epoch:standard_plan.anchor_epoch
     = Some standard_plan.anchor_state_root)
    "standard anchor is absent from floor graph";
  let fold_graph =
    graph (fun epoch ->
      if epoch = fold_plan.anchor_epoch then
        Graph.Root fold_plan.anchor_state_root
      else
        Graph.Missing)
  in
  require
    (Graph.set_fold
       fold_graph
       ~epoch:(fold_plan.activation_epoch - 1)
     = Ok Graph.Prior)
    "set fold rule differs before activation";
  require
    (Graph.set_fold
       fold_graph
       ~epoch:fold_plan.activation_epoch
     = Ok Graph.Active)
    "set fold rule inactive at activation";
  require
    (Graph.set_fold
       fold_graph
       ~epoch:(fold_plan.activation_epoch + 1)
     = Ok Graph.Active)
    "set fold rule inactive after activation";
  let fold_missing = graph (fun _ -> Graph.Missing) in
  require
    (match
       Graph.set_fold
         fold_missing
         ~epoch:fold_plan.activation_epoch
     with
     | Error (Graph.Anchor_missing epoch) -> epoch = fold_plan.anchor_epoch
     | _ -> false)
    "missing set fold anchor accepted";
  require
    (Graph.root_after_floor
       ~chain_id:"octra-devnet-9871-cluster"
       ~floor_epoch:fold_plan.activation_epoch
       ~epoch:fold_plan.anchor_epoch
     = Some fold_plan.anchor_state_root)
    "set fold anchor is absent from floor graph";
  require
    (Graph.set_fold unrelated ~epoch:0 = Ok Graph.Prior)
    "unrelated network activated set fold without an anchor";
  require
    (Graph.set_fold unrelated ~epoch:max_int = Ok Graph.Prior)
    "unrelated network activated set fold after genesis";
  let mainnet =
    Graph.create ~chain_id:"octra-mainnet" ~root_at:(fun _ -> Graph.Missing)
  in
  require
    (Graph.epoch_time mainnet ~epoch:max_int = Ok Graph.Prior)
    "mainnet epoch time changed without anchored activation";
  require
    (Graph.owner_migration mainnet ~epoch:max_int = Ok Graph.Prior)
    "unrelated owner migration changed without anchored activation";
  require
    (Graph.set_fold mainnet ~epoch:max_int = Ok Graph.Prior)
    "mainnet set fold changed without anchored activation";
  require
    (Graph.wasm_compute mainnet ~epoch:max_int = Ok Graph.Active)
    "new mainnet did not activate wasm compute at genesis";
  require
    (Graph.private_payload mainnet ~epoch:max_int = Ok Graph.Active)
    "new mainnet did not require strict private payloads at genesis";
  Printf.printf
    "rule_graph_before = 1\nrule_graph_boundary = 1\nrule_graph_after = 1\nrule_graph_mixed = 1\nPASS\n%!"