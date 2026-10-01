(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Case = Recovery_case
module SI = Octra_core.Store_irmin
module S = Octra_node_runtime.Startup_store_shell

external root_count : unit -> int = "octra_root_count"
external root_syncs : unit -> int = "octra_root_syncs"
external root_writes : unit -> int = "octra_root_writes"
external root_renames : unit -> int = "octra_root_renames"

let phase value =
  Unix.putenv "OCTRA_TEST_ROOT_COUNT" "0";
  Unix.putenv "OCTRA_TEST_ROOT_SYNCS" "0";
  Unix.putenv "OCTRA_TEST_ROOT_WRITES" "0";
  Unix.putenv "OCTRA_TEST_ROOT_RENAMES" "0";
  Unix.putenv "OCTRA_TEST_ROOT_PHASE" value

let save store = Lwt_main.run (SI.save_state_root store)

let child action =
  flush_all ();
  match Unix.fork () with
  | 0 ->
    (try action (); exit 0 with error ->
      Printf.eprintf "event = refused reason = %s\n%!" (Printexc.to_string error);
      let injected = match error with
        | Unix.Unix_error (Unix.EIO, _, _) -> true
        | Sys_error _ ->
          (Sys.getenv_opt "OCTRA_TEST_ROOT_PHASE" = Some "error"
           && root_count () = 1 && root_syncs () = 0)
          || (Sys.getenv_opt "OCTRA_TEST_ROOT_PHASE" = Some "write_error"
              && root_writes () > 0 && root_syncs () = 0)
        | _ -> false in
      exit (if injected then 12 else 2))
  | pid -> Case.wait pid

let check_integrity dir store =
  S.run_integrity {
    head_state = (fun () -> match Case.HM.load_result dir with
      | Case.HM.Present head -> S.Head_ready {
          epoch = head.epoch_id; root = Case.HM.ledger_state_root head}
      | Case.HM.Missing -> S.Head_missing
      | Case.HM.Corrupt reason -> S.Head_corrupt reason);
    store_root = (fun () -> Lwt_main.run (SI.get_head_hash store));
    epoch_root = (fun epoch -> match Lwt_main.run (SI.epoch_binding store epoch) with
      | Ok entry -> Some entry.SI.root | Error _ -> None);
    rollback_epoch = (fun epoch -> Lwt_main.run (SI.rollback_to_epoch store epoch));
    verify_integrity = (fun () -> Lwt_main.run (SI.verify_integrity store));
    save_state_root = (fun () -> save store);
    exit_fatal = (fun () -> failwith "integrity refused");
  }

let run_case root mode =
  let dir = Filename.concat root mode in
  Unix.mkdir dir 0o700;
  let previous, _ = Case.prepare dir in
  Case.with_stores dir (fun _ store ->
    Lwt_main.run (SI.save_state_root store);
    check_integrity dir store);
  let path = Filename.concat dir "state_root" in
  let prior = Case.read path in
  Case.advance dir previous;
  Case.expect "successor setup recovery failed" (Case.recover dir = Unix.WEXITED 0);
  let head = match Case.HM.load_result dir with
    | Case.HM.Present head -> head | _ -> failwith "successor HEAD is missing" in
  let current = Case.HM.ledger_state_root head in
  Case.expect "root update has no change" (current <> prior);
  let evidence = Case.evidence dir in
  let stopped = child (fun () -> Case.with_stores dir (fun _ store ->
    Unix.putenv "OCTRA_TEST_ROOT_DIR" dir;
    phase mode;
    save store;
    Case.expect "root syscall interception missed writer" (root_count () = 1);
    Case.expect "root writer did not synchronize file and directory" (root_syncs () = 2);
    Case.expect "root write interception missed writer" (root_writes () = 1);
    Case.expect "root writer did not replace file" (root_renames () = 1))) in
  phase "";
  let expected = match mode with
    | "control" -> Unix.WEXITED 0
    | "kill" | "file_kill" | "dir_kill" | "file_kill_after" | "dir_kill_after" ->
      Unix.WSIGNALED Sys.sigkill
    | "write_kill" | "rename_kill" | "rename_kill_after" -> Unix.WSIGNALED Sys.sigkill
    | _ -> Unix.WEXITED 12 in
  Case.expect "unexpected writer status" (stopped = expected);
  let after = Case.read path in
  Case.expect "root hint failure changed HEAD" (Case.HM.load_result dir = Case.HM.Present head);
  Case.expect "root hint failure changed history or store" (Case.evidence dir = evidence);
  let usable = try
    Case.with_stores dir (fun _ store -> check_integrity dir store);
    true
  with error ->
    Printf.printf "event = root_integrity mode = %s error = %s\n%!" mode (Printexc.to_string error);
    false in
  Printf.printf "event = root_cut mode = %s bytes_before = %d bytes_after = %d integrity = %b evidence = preserved\n%!"
    mode (String.length prior) (String.length after) usable;
  Case.expect "interrupted hint is neither old nor new complete root" (after = prior || after = current);
  Case.expect "interrupted hint prevented integrity check" usable;
  Case.expect "integrity did not keep current root" (Case.read path = current);
  Case.expect "integrity repair changed evidence" (Case.evidence dir = evidence)

let run root =
  let failed = ref false in
  List.iter (fun mode -> try run_case root mode with error ->
    phase "";
    failed := true;
    Printf.eprintf "event = root_cut mode = %s status = fail reason = %s\n%!"
      mode (Printexc.to_string error))
    ["control"; "error"; "kill"; "file_error"; "file_kill";
      "file_error_after"; "file_kill_after"; "dir_error"; "dir_kill";
      "dir_error_after"; "dir_kill_after"; "write_error"; "write_kill";
      "rename_error"; "rename_kill"; "rename_error_after"; "rename_kill_after"];
  if !failed then exit 1

let () = Test_workspace.with_dir "root_write" run