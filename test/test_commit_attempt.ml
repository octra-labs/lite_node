(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module A = Octra_core.Commit_attempt
module J = Octra_core.Commit_journal

let expect reason valid = if not valid then failwith reason
let ok = function Ok value -> value | Error reason -> failwith reason
let prepare ?(epoch = 8) ?(generation = 7) id = J.Prepare {
  commit_id = id; prev_generation = generation; epoch_id = epoch;
  planned_txid_hi = 9L; planned_state_root = "root"; ts = 0.}
let abort id = J.Abort {commit_id = id; reason = "recovery_to_head"; ts = 0.}
let commit ?(generation = 8) id = J.Commit {commit_id = id; generation; ts = 0.}
let read = A.read ~epoch:8 ~generation:7

let run () =
  let cases = [
    "duplicate", [prepare "a"; prepare "a"];
    "overlap", [prepare "a"; prepare "b"];
    "reused", [prepare "a"; abort "a"; prepare "a"];
    "abort_first", [abort "a"; prepare "a"];
    "commit_first", [commit "a"; prepare "a"];
    "commit_missing", [commit "a"];
    "abort_after_commit", [prepare "a"; commit "a"; abort "a"];
    "commit_after_abort", [prepare "a"; abort "a"; commit "a"];
    "after_commit", [prepare "a"; commit "a"; prepare "b"];
    "empty_id", [prepare ""];
    "prepare_generation", [prepare ~generation:6 "a"];
    "commit_generation", [prepare "a"; commit ~generation:9 "a"];
    "other_epoch_id", [prepare ~epoch:6 ~generation:5 "a"; prepare "a"];
  ] in
  List.iter (fun (name, records) ->
    expect ("accepted invalid journal: " ^ name) (Result.is_error (read records));
    Printf.printf "event = passed case = %s\n%!" name) cases;
  let selected = ok (read [prepare "a"; abort "a"; abort "a"; prepare "b"]) in
  expect "wrong active attempt"
    (ok (A.active selected) = Some A.{commit_id = "b"; planned_txid_hi = 9L;
      planned_state_root = "root"});
  expect "retirement skipped an earlier visible abort" (ok (A.retire selected) = ["a"; "b"]);
  let published = ok (read [prepare "a"; commit "a"; commit "a"]) in
  expect "published successor accepted as waiting" (Result.is_error (A.active published));
  expect "published successor allowed retirement" (Result.is_error (A.retire published));
  let retired = ok (read [prepare "a"; abort "a"]) in
  expect "retired attempt became waiting" (ok (A.active retired) = None);
  expect "retired attempt lost its sync requirement" (ok (A.retire retired) = ["a"]);
  let other = ok (read [prepare ~epoch:3 ~generation:2 "other"; commit ~generation:3 "other";
    prepare "a"]) in
  expect "unrelated history changed selected epoch" (ok (A.retire other) = ["a"]);
  let empty = ok (read []) in
  expect "empty journal fabricated an attempt" (ok (A.active empty) = None && ok (A.retire empty) = []);
  Printf.printf "event = passed scope = commit_attempt negatives = %d\n%!" (List.length cases + 2)

let () = run ()