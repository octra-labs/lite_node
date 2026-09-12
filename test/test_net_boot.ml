(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module S = Octra_node_runtime.Startup_network_boot_shell

let fail msg =
  failwith ("test_node_runtime_startup_network_boot_shell: " ^ msg)

let expect label cond =
  if not cond then fail label

let test_create_refs () =
  let refs = S.create_refs () in
  expect "config hash size" (String.length !(refs.consensus_config_hash) = 32);
  expect
    "config hash zero"
    (!(refs.consensus_config_hash) = String.make 32 '\x00');
  expect
    "validator set empty"
    ((!(refs.consensus_validator_set)).Octra_consensus.C_types.n = 0);
  expect "scheduled empty" (!(refs.scheduled_validator_set) = None)

let test_profile_during_sync () =
  let refs = S.create_refs () in
  let runtime_profile_hash = String.make 32 '\x41' in
  let actual = ref None in
  S.sync ~recovery:false (fun () ->
    actual :=
      Some
        (S.bind_profile
           refs
           ~chain_id:"octra-test"
           ~program_trust_hash:None
           ~runtime_profile_hash);
    Lwt.return_unit);
  let expected =
    Octra_consensus.C_config.hash
      ~chain_id:"octra-test"
      ~validator_set:!(refs.consensus_validator_set)
      ~runtime_profile_hash
      ()
  in
  expect "profile hash" (Option.equal String.equal !actual (Some expected));
  expect "profile stored" (String.equal !(refs.consensus_config_hash) expected)

let () =
  test_create_refs ();
  test_profile_during_sync ();
  print_endline "status = pass test = node_runtime_startup_network_boot_shell"