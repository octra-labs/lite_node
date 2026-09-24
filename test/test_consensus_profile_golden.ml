(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module P = Octra_node_runtime.Consensus_profile

let fail name = failwith ("test_consensus_profile_golden: " ^ name)

let expect name ok = if not ok then fail name

let raw_hex raw =
  String.to_seq raw
  |> Seq.map (fun c -> Printf.sprintf "%02x" (Char.code c))
  |> List.of_seq
  |> String.concat ""

let chain_id = "octra-devnet-9871-cluster"

let standard_golden =
  "3dc39c75172ff6ea95a5ba8eeb9b3a95cb24061709e516cc59d0f3fc4976c89c"

let compat_golden =
  "be146e7537bd9220f610cbf1bed813ce7520a090fe3e5f96038df72b44d43050"

let empty_compat_golden =
  "cf3813dc7d75a9df5b696817677f7aafc94cbbb737cef9f9a64edb1b2d04006b"

let plan_golden =
  "20cb24dc201d8d065e22915ca4692c06fe9d863d13fea6c0ea88c6631502e636"

let exit_golden =
  "4fcb797044feff89af203d505acd3762f624fae315fe515cfcc991a1319d13a7"

let sample = [
  "OCTRA_BFT_PROPOSAL_MAX_TXS", "800";
  "OCTRA_BFT_PROPOSAL_MAX_BYTES", "4000000";
  "OCTRA_BFT_PROPOSAL_MAX_OU", "9000000000";
  "OCTRA_STEALTH_MAX_DEFER", "20";
  "OCTRA_STEALTH_MAX_PER_EPOCH", "3";
  "OCTRA_FHE_MAX_PER_EPOCH", "1";
  "OCTRA_STEALTH_INLINE_VERIFY", "1";
  "OCTRA_STEALTH_MIN_OU", "6000";
  "OCTRA_PREVERIFY_RECEIPT_ACTIVATION_EPOCH", "1266000";
  "OCTRA_PRIVATE_RESULT_ACTIVATION_EPOCH", "1266000";
  "OCTRA_MULTI_EXEC_MAX_CALLS", "6";
  "OCTRA_BFT_RELEASE_PROFILE", "devnet_full_v1";
  "OCTRA_EMISSION_ACTIVATION_EPOCH", "1266000";
  "OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH", "1266000";
  "OCTRA_FEE_RECIPIENT", "oct" ^ String.make 44 '1';
  "OCTRA_PROPOSAL_PROTOCOL_ACTIVATION_EPOCH", "1266000";
  "OCTRA_PVAC_MIGRATION_ROOT", String.make 64 'a';
]

let getenv name = List.assoc_opt name sample

let devnet = [
  "OCTRA_BFT_PROPOSAL_MAX_TXS", "1000";
  "OCTRA_BFT_PROPOSAL_MAX_BYTES", "5000000";
  "OCTRA_BFT_PROPOSAL_MAX_OU", "10000000000";
  "OCTRA_STEALTH_MAX_DEFER", "30";
  "OCTRA_STEALTH_MAX_PER_EPOCH", "1";
  "OCTRA_FHE_MAX_PER_EPOCH", "1";
  "OCTRA_STEALTH_MIN_OU", "5000";
  "OCTRA_PREVERIFY_RECEIPT_ACTIVATION_EPOCH", "1260767";
  "OCTRA_PRIVATE_RESULT_ACTIVATION_EPOCH", "1266000";
  "OCTRA_MULTI_EXEC_MAX_CALLS", "8";
  "OCTRA_BFT_RELEASE_PROFILE", "devnet_full_v1";
  "OCTRA_EMISSION_ACTIVATION_EPOCH", "1260767";
  "OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH", "1266000";
  "OCTRA_PROPOSAL_PROTOCOL_ACTIVATION_EPOCH", "1265416";
  "OCTRA_PVAC_MIGRATION_ROOT",
    "35a5ff289c799e701f315d31355d1007f8ab9c9825a8df67de380ddcf85640a8";
]

let components ~epoch getenv =
  let validator_policy = Octra_core.Validator_policy.of_env_exn getenv in
  let schedule =
    match Octra_core.Emission_schedule.of_env getenv with
    | Ok value -> value
    | Error reason -> fail ("emission schedule: " ^ reason)
  in
  [
    "runtime_compatibility", P.compat_hash getenv;
    "activation_graph", Octra_core.Rule_graph.consensus_id ~chain_id ~epoch;
    "quorum", Octra_consensus.C_quorum_policy.consensus_id ~chain_id;
    "set_fold_bootstrap",
      Octra_core.Set_fold.consensus_id Octra_core.Set_fold.standard;
    "set_fold_participating",
      Octra_core.Set_fold.consensus_id Octra_core.Set_fold.participating;
    "set_fold_rule", Octra_node_runtime.Set_rule.consensus_id;
    "validator_admission",
      Octra_core.Validator_policy.consensus_id validator_policy;
    "emission", Octra_core.Emission_schedule.consensus_id schedule;
    "reward", Octra_core.Reward_policy.consensus_id;
    "reward_source",
      Octra_node_runtime.Consensus_reward_attribution.consensus_id;
    "fee", Octra_core.Fee_policy.consensus_id;
    "private_receipt", Octra_core.Private_transition.consensus_id;
    "circle_receipt", Octra_core.Circle_cell_transition.consensus_id;
    "circle_storage",
      Octra_circle_runtime.Circle_runtime_storage.consensus_id;
    "circle_hfhe", Octra_core.Circle_hfhe_transcript.consensus_id;
    "integer_work", Octra_vm.Int_work.consensus_id;
    "program_compiler", Octra_vm.Program_package.standard_id;
    "vm_undo", "nested_commit";
    "proposal_protocol", Octra_consensus.C_protocol.consensus_id getenv;
  ]
  @ (match Octra_core.Rule_graph.set_plan_at ~chain_id ~epoch with
     | Octra_core.Rule_graph.Prior -> []
     | Octra_core.Rule_graph.Active -> ["set_plan", "retain_until_activation"])
  @ (match Octra_core.Rule_graph.math_at ~chain_id ~epoch with
     | Octra_core.Rule_graph.Prior -> []
     | Octra_core.Rule_graph.Active -> ["math", "scalar65_field_signed_cipher_zero_q16"])
  @ (match Octra_core.Rule_graph.exit_at ~chain_id ~epoch with
     | Octra_core.Rule_graph.Prior -> []
     | Octra_core.Rule_graph.Active -> ["validator_exit", Octra_core.Validator_policy.exit_id])
  @ (match Octra_core.Rule_graph.ready_exec_at ~chain_id ~epoch with
     | Octra_core.Rule_graph.Prior -> []
     | Octra_core.Rule_graph.Active -> ["ready_reference", "proposal_delay2_inclusion_first"])
  @ (match Octra_core.Rule_graph.program_source_at ~chain_id ~epoch with
     | Octra_core.Rule_graph.Prior -> []
     | Octra_core.Rule_graph.Active -> ["program_source", "aml_source:oct_gen_count:source_abi:checked_address:option_values:scalar_equality:shape_limits:overlap_64"])

let derived ~epoch getenv =
  Octra_net.Hash_domain.hash_encoded "octra:consensus_standard" (fun buf ->
    Octra_net.Oce1.put_string buf P.standard;
    Octra_net.Oce1.put_string buf chain_id;
    List.iter
      (fun (name, value) ->
        Octra_net.Oce1.put_string buf name;
        Octra_net.Oce1.put_string buf value)
      (components ~epoch getenv))

let () =
  expect "profile validates" (P.validate getenv = Ok ());
  let standard = P.standard_hash ~chain_id ~epoch:1_500_000 getenv in
  let compat = P.compat_hash getenv in
  Printf.printf
    "event = consensus_profile_golden standard = %s compat = %s\n"
    (raw_hex standard)
    (raw_hex compat);
  expect "standard hash golden" (String.equal (raw_hex standard) standard_golden);
  expect "compat hash golden" (String.equal (raw_hex compat) compat_golden);
  expect "empty env compat hash golden"
    (String.equal (raw_hex (P.compat_hash (fun _ -> None))) empty_compat_golden);
  expect "standard hash binds every component"
    (String.equal standard (derived ~epoch:1_500_000 getenv));
  expect "component count" (List.length (components ~epoch:1_500_000 getenv) = 19);
  List.iter
    (fun epoch ->
      expect "prior epoch selects compat"
        (String.equal (P.hash ~chain_id ~epoch getenv) compat))
    [0; 1_499_998; 1_499_999];
  List.iter
    (fun epoch ->
      expect "released active profile preserved"
        (String.equal (P.hash ~chain_id ~epoch getenv) standard);
      expect "no additional profile switch"
        (not (P.switch_after ~chain_id ~applied_epoch:epoch)))
    [1_500_000; 1_500_001; 1_507_999; 1_508_000; 1_508_001; 1_509_998];
  expect "announced profile switch retained"
    (P.switch_after ~chain_id ~applied_epoch:1_499_999);
  expect "set plan prior sample preserved"
    (P.hash ~chain_id ~epoch:1_509_999 getenv = standard);
  let devnet_env name = List.assoc_opt name devnet in
  let prior_hash =
    "14bac4b24aa67795bcaecf618b6c10a6ef0f3b2a113ded7f5f9117362375cee6"
  in
  let active_hash =
    "adb84cb6daf4e71f58e2ce9830136a8854b2e705bad09d866b0221059c5565c9"
  in
  List.iter
    (fun (epoch, expected) ->
      let actual = raw_hex (P.hash ~chain_id ~epoch devnet_env) in
      expect "marker 12 profile" (String.equal actual expected);
      Printf.printf "event = devnet_profile epoch = %d hash = %s\n" epoch actual)
    [1_499_999, prior_hash; 1_500_000, active_hash;
     1_500_001, active_hash; 1_507_999, active_hash;
     1_508_000, active_hash; 1_508_001, active_hash; 1_509_999, active_hash];
  expect "set plan profile switches at the agreed epoch"
    (P.switch_after ~chain_id ~applied_epoch:1_509_999);
  List.iter (fun epoch ->
    let value = P.hash ~chain_id ~epoch getenv in
    expect "math component count" (List.length (components ~epoch getenv) = 21);
    expect "set plan hash binds components" (value = derived ~epoch getenv);
    expect "set plan hash differs" (value <> standard);
    expect "exit switch follows last prior epoch"
      (P.switch_after ~chain_id ~applied_epoch:epoch = (epoch = 1_566_999));
    let actual = raw_hex (P.hash ~chain_id ~epoch devnet_env) in
    expect "devnet profile binds components" (actual = raw_hex (derived ~epoch devnet_env));
    Printf.printf "event = set_plan_profile epoch = %d hash = %s\n"
      epoch actual;
    expect "set plan hash golden" (actual = plan_golden))
    [1_510_000; 1_510_001; 1_541_998; 1_541_999; 1_542_000;
     1_542_001; 1_548_999; 1_549_000; 1_549_001; 1_552_203;
     1_562_999; 1_563_000; 1_563_001; 1_566_998; 1_566_999];
  let epoch = 1_567_000 in
  expect "exit activation switches profile" (P.switch_after ~chain_id ~applied_epoch:(epoch - 1));
  let current = P.hash ~chain_id ~epoch devnet_env in
  expect "exit profile differs" (raw_hex current <> plan_golden);
  Printf.printf "event = ready_profile epoch = %d hash = %s\n%!" epoch (raw_hex current);
  expect "exit profile golden" (raw_hex current = exit_golden);
  expect "exit profile commits parameters" (current = derived ~epoch devnet_env);
  expect "exit profile stable after activation"
    (current = P.hash ~chain_id ~epoch:max_int devnet_env);
  Printf.printf "event = exit_profile epoch = %d hash = %s\n" epoch (raw_hex current);
  List.iter (fun (applied_epoch, expected) ->
    expect "profile switch epoch"
      (P.switch_after ~chain_id ~applied_epoch = expected))
    [0, false; 1_499_998, false; 1_499_999, true;
     1_500_000, false; 1_500_001, false; 1_507_999, false;
     1_508_000, false; 1_508_001, false; 1_509_998, false;
     1_509_999, true; 1_510_000, false; 1_510_001, false; max_int, false];
  List.iter (fun epoch ->
    expect "other chain profile preserved"
      (P.hash ~chain_id:"octra-mainnet" ~epoch getenv = compat);
    expect "other chain does not switch"
      (not (P.switch_after ~chain_id:"octra-mainnet" ~applied_epoch:epoch)))
    [0; 1_499_999; 1_500_000; 1_509_999; 1_510_000; 1_510_001;
     1_566_999; 1_567_000; 1_567_001; max_int];
  expect "standard binds chain"
    (not (String.equal standard (P.standard_hash ~chain_id:"octra-mainnet" ~epoch:1_500_000 getenv)));
  print_endline "status = pass test = consensus_profile_golden"