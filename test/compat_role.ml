(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let require ok reason = if not ok then failwith reason

let chain = "octra-test"

let role directory =
  let need = Sync_mark.need (Sync_mark.read ~data_dir:directory ~chain) |> Result.get_ok in
  Consensus_mode.with_need need (Consensus_mode.of_inputs ~cli_observer:false ~env_mode:(Some "bft"))

let check directory expected head removed =
  let need = match Sync_mark.read ~data_dir:directory ~chain with
    | Sync_mark.Ready need -> need | _ -> failwith "old reader rejected marker" in
  require (need.cause = expected) "old reader cause differs";
  let before = role directory in
  require (before.observer_enabled && not before.voting_enabled) "old marker allowed voting";
  if expected = Sync_need.Range then
    require (Boot_check.sync_plan ~head:100 (Sync_mark.Ready need) = Ok None)
      "old Range unexpectedly holds boot";
  let calls = ref 0 and exits = ref [] in
  let attest = Attest_check.create ~data_dir:directory ~chain
    ~set_state_attested:(fun ~head:_ ~root:_ -> incr calls)
    ~exit:(fun code -> exits := code :: !exits)
    ~exit_error:(fun () -> failwith "old attestation failed") (Some need) in
  attest ~head ~root:"checked";
  require (!calls = 1) "old attestation skipped state check";
  require ((Sync_mark.read ~data_dir:directory ~chain = Sync_mark.Missing) = removed)
    "old attestation marker result differs";
  require (!exits = if removed then [Sync_mark.restart_code] else []) "old restart result differs";
  let after = role directory in
  require (after.voting_enabled = removed && after.observer_enabled = not removed)
    "old restart role differs"

let () =
  let range = Sys.argv.(1) and root = Sys.argv.(2) in
  check range Sync_need.Range 5000 false;
  require (Sync_mark.write ~data_dir:root ~chain (Sync_need.root ~head:100 ~epoch:101) = Ok Sync_mark.Stored)
    "old root control write failed";
  check root Sync_need.Root 101 true;
  print_endline "status = pass test = compat_role reader = f7637a7e range = observer root = validator"