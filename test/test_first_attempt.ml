(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module A = Octra_core.Commit_attempt
module J = Octra_core.Commit_journal

let prepare ?(epoch = 0) ?(generation = -1) id = J.Prepare {
  commit_id = id; prev_generation = generation; epoch_id = epoch;
  planned_txid_hi = -1L; planned_state_root = "root"; ts = 0.}
let abort id = J.Abort {commit_id = id; reason = "recovery_before_head"; ts = 0.}
let commit ?(generation = 0) id = J.Commit {commit_id = id; generation; ts = 0.}
let expect reason value = if not value then failwith reason

let run () =
  let accepted = [
    "empty", [], [];
    "prepared", [prepare "a"], ["a"];
    "retired", [prepare "a"; abort "a"], ["a"];
    "sync_retry", [prepare "a"; abort "a"; abort "a"], ["a"];
    "next_attempt", [prepare "a"; abort "a"; prepare "b"], ["a"; "b"];
  ] in
  List.iter (fun (name, records, ids) ->
    expect ("valid first attempts refused: " ^ name) (A.initial records = Ok ids);
    Printf.printf "event = passed case = %s\n%!" name) accepted;
  let refused = [
    "published", [prepare "a"; commit "a"];
    "commit_only", [commit "a"];
    "commit_later", [commit ~generation:8 "a"];
    "abort_only", [abort "a"];
    "abort_unknown", [prepare "a"; abort "other"];
    "abort_first", [abort "a"; prepare "a"];
    "overlap", [prepare "a"; prepare "b"];
    "duplicate", [prepare "a"; prepare "a"];
    "reused", [prepare "a"; abort "a"; prepare "a"];
    "empty_id", [prepare ""];
    "generation", [prepare ~generation:0 "a"];
    "epoch", [prepare ~epoch:1 "a"];
    "later_history", [prepare ~epoch:6 ~generation:5 "a"; abort "a"];
    "mixed_epoch", [prepare "a"; abort "a"; prepare ~epoch:1 "b"];
  ] in
  List.iter (fun (name, records) ->
    expect ("invalid first attempts accepted: " ^ name) (Result.is_error (A.initial records));
    Printf.printf "event = passed case = %s\n%!" name) refused;
  Printf.printf "event = first_attempt valid = %d invalid = %d\n%!"
    (List.length accepted) (List.length refused)

let () = run ()