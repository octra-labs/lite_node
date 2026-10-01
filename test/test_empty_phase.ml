(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Recovery_case

module J = Octra_core.Commit_journal
module A = Octra_core.Commit_attempt

let first = J.Prepare {commit_id = "first"; prev_generation = -1;
  epoch_id = 0; planned_txid_hi = -1L; planned_state_root = hash 'a'; ts = 0.}
let done_row = J.Commit {commit_id = "first"; generation = 0; ts = 0.}
let aborted = J.Abort {commit_id = "first"; reason = "recovery_before_head"; ts = 0.}
let second = J.Prepare {commit_id = "second"; prev_generation = -1;
  epoch_id = 0; planned_txid_hi = -1L; planned_state_root = hash 'a'; ts = 0.}

let run root =
  let cases = [
    "empty", [], true;
    "first_prepare", [first], true;
    "first_aborted", [first; aborted], true;
    "first_retry", [first; aborted; second], true;
    "first_overlap", [first; second], false;
    "abort_only", [aborted], false;
    "completed", [first; done_row], false;
    "completion_only", [done_row], false;
    "later_prepare", [J.Prepare {commit_id = "later"; prev_generation = 5;
      epoch_id = 6; planned_txid_hi = 99L; planned_state_root = hash 'b'; ts = 0.}], false;
  ] in
  let failed = ref false in
  List.iter (fun (name, rows, accepted) ->
    let dir = Filename.concat root name in
    Unix.mkdir dir 0o700;
    with_stores dir (fun _ _ -> ());
    List.iter (J.append dir) rows;
    let evidence () = files (Filename.concat dir "chaindata") @
      (let path = J.path dir in if Sys.file_exists path then [path, read path] else []) in
    let before = evidence () in
    let result = recover dir in
    let after = evidence () in
    let actual = result = Unix.WEXITED 0 in
    let closed = match A.read ~epoch:0 ~generation:(-1) (J.read_all dir) with
      | Ok attempts -> A.active attempts = Ok None
      | Error _ -> false in
    let retained = List.for_all (fun (path, bytes) -> List.assoc_opt path after = Some bytes) before in
    Printf.printf "event = empty_phase case = %s accepted = %b expected = %b closed = %b retained = %b head_missing = %b\n%!"
      name actual accepted closed retained (HM.load_result dir = HM.Missing);
    if actual <> accepted || (accepted && not closed) || (not accepted && not retained)
       || not (Marker.recovery_required dir) || HM.load_result dir <> HM.Missing
    then failed := true) cases;
  if !failed then exit 1

let () = Test_workspace.with_dir "empty_phase" run