(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Recovery_case

let run root =
  let cases = [
    "forward", (fun _ -> ()), true;
    "cached_head", (fun dir -> HM.set_cached (Option.get (HM.load dir))), true;
    "wal_post", (fun dir -> change_wal dir (fun entry ->
      {entry with Wal.post_state_root = hash '6'})), false;
    "wal_pre", (fun dir -> change_wal dir (fun entry ->
      {entry with Wal.pre_state_root = hash '6'})), false;
    "wal_range", (fun dir -> change_wal dir (fun entry ->
      {entry with Wal.start_txid = 42L})), false;
    "wal_parent", (fun dir -> change_wal dir (fun entry ->
      {entry with Wal.parent_commit = hash '6'})), false;
    "prepare_root", (fun dir -> change_prepare dir (function
      | Octra_core.Commit_journal.Prepare entry ->
        Octra_core.Commit_journal.Prepare {entry with planned_state_root = hash '6'}
      | _ -> assert false)), false;
    "prepare_range", (fun dir -> change_prepare dir (function
      | Octra_core.Commit_journal.Prepare entry ->
        Octra_core.Commit_journal.Prepare {entry with planned_txid_hi = 42L}
      | _ -> assert false)), false;
    "prepare_missing", (fun dir -> Unix.unlink (Octra_core.Commit_journal.path dir)), false;
    "suffix_after_commit", (fun dir -> with_stores dir (fun chain _ ->
      ignore (Octra_core.Txlog.append chain.SC.txlog ~epoch_id:2 ~payload:(hash 'c' ^ "{}")))), false;
    "marker_epoch", (fun dir -> Marker.write_marker dir 99 "irmin_committed"), false] in
  let failed = List.fold_left (fun failed (name, alter, succeeds) ->
    try
      HM.cached := None;
      let dir = Filename.concat root name in
      Unix.mkdir dir 0o700;
      let head, _ = prepare dir in
      advance dir head;
      alter dir;
      let before = evidence dir in
      let outcome = recover dir in
      HM.cached := None;
      let after = evidence dir in
      let old_head, old_records, old_tags, old_commit = before in
      let new_head, new_records, new_tags, new_commit = after in
      Printf.printf "event = result case = %s exit = %s head_same = %b records_same = %b tags_same = %b commit_same = %b\n%!"
        name (match outcome with Unix.WEXITED n -> string_of_int n | _ -> "signal")
        (old_head = new_head) (old_records = new_records) (old_tags = new_tags)
        (old_commit = new_commit);
      if succeeds then begin
        expect "valid recovery refused" (outcome = Unix.WEXITED 0);
        expect "valid recovery kept WAL" (Wal.read_pending dir = []);
        with_stores dir (fun _ store ->
          let head = match HM.load_result dir with
            | HM.Present head -> head | _ -> failwith "recovered HEAD absent" in
          expect "forward HEAD epoch differs" (head.epoch_id = 1);
          expect "forward HEAD root differs"
            (Some (HM.ledger_state_root head) = Lwt_main.run (SI.get_head_hash store));
          expect "forward HEAD commit differs"
            (head.irmin_commit = Lwt_main.run (SI.get_commit_hash store));
          expect "forward tag absent" (List.mem 1 (Lwt_main.run (SI.list_epoch_tags store))))
      end
      else begin
        expect "invalid recovery changed evidence" (before = after);
        expect "invalid recovery accepted" (outcome = Unix.WEXITED 1 || outcome = Unix.WEXITED 2)
      end;
      failed
    with exn ->
      Printf.eprintf "event = failed case = %s reason = %s\n%!" name (Printexc.to_string exn);
      true) false cases in
  if failed then exit 1

let () = Test_workspace.with_dir "forward_phase" run