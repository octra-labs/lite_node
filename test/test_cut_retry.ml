(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Case = Recovery_case
module Journal = Octra_core.Commit_journal

let first_cut dir head =
  Case.advance dir head;
  Case.change_prepare dir (function
    | Journal.Prepare row -> Journal.Prepare {row with commit_id = "first-attempt"}
    | _ -> failwith "unexpected journal record");
  Case.with_stores dir (fun _ store ->
    let prior = Option.get (Lwt_main.run
      (Case.SI.Store.Branch.find store.Case.SI.repo "epoch_0")) in
    Lwt_main.run (Case.SI.Store.Head.set store.store prior));
  Case.Marker.write_marker dir 1 "wal_written";
  Case.expect "initial cut failed" (Case.recover dir = Unix.WEXITED 0);
  Case.expect "initial cut changed HEAD" (Case.HM.load dir = Some head)

let run root =
  let prepared_only dir head =
    Journal.append dir (Journal.Prepare {
      commit_id = "first-attempt"; prev_generation = head.Case.HM.generation;
      epoch_id = 1; planned_txid_hi = 1L; planned_state_root = "uncommitted"; ts = 0.});
    Case.expect "prepared-only recovery refused" (Case.recover dir = Unix.WEXITED 0);
    Case.expect "prepared-only recovery changed HEAD" (Case.HM.load dir = Some head) in
  let cases = ["first_attempt", (fun _ _ -> ()); "after_cut", first_cut;
    "prepared_only", prepared_only] in
  let failed = List.filter_map (fun (name, setup) ->
    let dir = Filename.concat root name in
    Unix.mkdir dir 0o700;
    let head, _ = Case.prepare dir in
    setup dir head;
    Case.advance dir head;
    let before = Case.evidence dir in
    let journal = Journal.read_all dir in
    let prepares = List.filter_map (function
      | Journal.Prepare row -> Some row.commit_id | _ -> None) journal in
    Printf.printf "event = attempt case = %s prepares = %s\n%!" name (String.concat "," prepares);
    let outcome = Case.recover dir in
    Printf.printf "event = recovery case = %s exit = %s preserved = %b\n%!" name
      (match outcome with Unix.WEXITED value -> string_of_int value | _ -> "signal")
      (before = Case.evidence dir);
    let ok = outcome = Unix.WEXITED 0 in
    if ok then begin
      Case.expect "recovery did not publish successor" ((Option.get (Case.HM.load dir)).epoch_id = 1);
      Case.expect "successful recovery retained WAL" (Case.Wal.read_pending dir = [])
    end;
    if ok then None else Some name) cases in
  if failed <> [] then exit 1

let () = Test_workspace.with_dir "cut_retry" run