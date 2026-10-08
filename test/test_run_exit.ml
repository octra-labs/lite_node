(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module S = Octra_node_runtime.Startup_run_shell
module Lock = Octra_core.Store_lock
module Store = Octra_core.Store_irmin

external halt_gc : bool -> unit = "octra_test_gc_halt"

let gc_child path mode =
  let open Lwt.Syntax in
  let owner = Lock.acquire path in
  let* store = Store.open_store ~fresh:true (Filename.concat path "irmin") in
  let write epoch =
    let* () = Store.write store ["value"] (string_of_int epoch) in
    Store.tag_epoch store epoch in
  let* () = write 1 in
  let* () = write 2 in
  let* split = Store.collect_pack_at store ~keep:1 2 in
  if split <> Store.Gc_split 2 then failwith "GC split missing";
  let* () = write 3 in
  halt_gc true;
  let* started = Store.collect_pack_at ~free:Int64.max_int store ~keep:1 3 in
  halt_gc false;
  (match started with Store.Gc_started _ -> () | _ -> failwith "GC not started");
  let pid, status = Unix.waitpid [Unix.WUNTRACED] (-1) in
  if status <> Unix.WSTOPPED Sys.sigstop then failwith "GC child not stopped";
  let channel = open_out (Filename.concat path "gc.pid") in
  output_string channel (string_of_int pid);
  close_out channel;
  (match Lock.acquire path with
  | other -> Lock.release other; failwith "GC setup lost ownership"
  | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), "flock", _) -> ());
  ignore (Sys.opaque_identity owner);
  let exit_fatal = S.exit_store store in
  let exit_refused = S.exit_store ~code:78 store in
  if mode = "worker-apply" || mode = "worker-apply-log" || mode = "apply" then begin
    let module Ledger = Octra_core.Ledger in
    let module History = Octra_core.Store_chaindata in
    let ledger = Ledger.create store in
    let history = History.open_chaindata (Filename.concat path "history") in
    let fatal text =
      let clean = not (Ledger.journal_active ledger) && History.next_txid history = 0L in
      let output = open_out (Filename.concat path "abort.status") in
      Fun.protect ~finally:(fun () -> close_out output) (fun () ->
        output_string output (if clean then "clean" else "pending"));
      if mode = "worker-apply-log" then raise (Sys_error "log unavailable");
      print_endline text in
    Octra_node_runtime.Epoch_atomic.run_store ~fatal ~store ~ledger ~chaindata:history
      (fun () ->
        ignore (Result.get_ok (Ledger.begin_journal ledger));
        let* () = Store.begin_epoch_batch store in
        History.begin_batch history;
        History.save_tx history ~hash:(String.make 64 'a') ~epoch_id:4
          ~from_addr:"sender" ~to_addr:"recipient" ~tx_json:"{}"
          ~op_type:"transfer" ~encrypted_data:"" ~message:"";
        let* () = Store.write store ["value"] "4" in
        if mode = "apply" then Lwt.fail Out_of_memory
        else Octra_core.Private_ledger.worker_retry
          ~wait:(fun _ -> Lwt.pause ())
          (fun () -> Lwt.fail (Octra_core.Exec_resource.Unavailable Host)))
  end else if mode = "sync" || mode = "sync-error" || mode = "sync-present" then begin
    let module Need = Octra_node_runtime.Sync_need in
    let module Mark = Octra_node_runtime.Sync_mark in
    let chain = "octra-test" in
    let need = Need.root ~epoch:4 ~head:3 in
    if mode = "sync-error" then begin
      let channel = open_out (Filename.dirname (Mark.path path)) in
      output_string channel "retained";
      close_out channel
    end else if mode = "sync-present" then
      ignore (Result.get_ok (Mark.write ~data_dir:path ~chain need));
    S.require_sync ~data_dir:path ~chain ~store need
  end else if mode = "async" || mode = "worker-async" then begin
    Octra_node_runtime.Startup_process_shell.configure_lwt ~exit_fatal ~exit_refused;
    !Lwt.async_exception_hook (if mode = "async" then Out_of_memory
      else Octra_core.Private_ledger.Worker_stopped "test");
    failwith "async memory failure returned"
  end else
    let log = match mode with
      | "worker-log" -> {S.default_join_log with
          fatal = (fun _ -> raise (Sys_error "log unavailable"))}
      | "worker-warn" -> {S.default_join_log with
          warn = (fun _ -> raise (Sys_error "log unavailable"))}
      | _ -> S.default_join_log in
    S.run_join ~log
      ~tasks:[Lwt.fail (if String.starts_with ~prefix:"worker" mode then
        Octra_core.Private_ledger.Worker_stopped "test" else Failure "GC owner task")]
      ~exit_fatal ~exit_refused

let gc_process mode = Test_workspace.with_dir "fatal_gc" (fun path ->
  let pid = Unix.create_process Sys.executable_name
    [|Sys.executable_name; "--gc-child"; path; mode|] Unix.stdin Unix.stdout Unix.stderr in
  let rec reap () =
    try snd (Unix.waitpid [] pid)
    with Unix.Unix_error (Unix.EINTR, _, _) -> reap () in
  let code = if String.starts_with ~prefix:"worker" mode then 78 else 1 in
  let status = reap () in
  let channel = open_in (Filename.concat path "gc.pid") in
  let worker = Fun.protect ~finally:(fun () -> close_in channel)
    (fun () -> int_of_string (input_line channel)) in
  if status <> Unix.WEXITED code then begin
    (try Unix.kill worker Sys.sigkill
     with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
    failwith "GC owner exit differs"
  end;
  let clock = Mtime_clock.counter () in
  let rec acquire () =
    match Lock.acquire path with
    | owner -> owner
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), "flock", _) ->
      if Mtime.Span.to_float_ns (Mtime_clock.count clock) > 3e9 then begin
        Unix.kill worker Sys.sigkill;
        failwith "fatal exit retained GC ownership"
      end;
      Unix.sleepf 0.01;
      acquire () in
  let owner = acquire () in
  Fun.protect ~finally:(fun () -> Lock.release owner) (fun () ->
    let module Need = Octra_node_runtime.Sync_need in
    let module Mark = Octra_node_runtime.Sync_mark in
    if mode = "worker-apply" || mode = "worker-apply-log" || mode = "apply" then begin
      let input = open_in (Filename.concat path "abort.status") in
      let value = Fun.protect ~finally:(fun () -> close_in input) (fun () -> input_line input) in
      if value <> "clean" then failwith "epoch abort left pending writes";
      let module History = Octra_core.Store_chaindata in
      let history = History.open_chaindata (Filename.concat path "history") in
      Fun.protect ~finally:(fun () -> History.close history) (fun () ->
        if History.next_txid history <> 0L then failwith "failed epoch published history");
      if Mark.read ~data_dir:path ~chain:"octra-test" <> Mark.Missing then
        failwith "worker fault requested snapshot"
    end;
    if mode = "sync" || mode = "sync-present" then
      if Mark.read ~data_dir:path ~chain:"octra-test" <> Mark.Ready (Need.root ~epoch:4 ~head:3)
      then failwith "recovery marker was not preserved";
    if mode = "sync-error" then begin
      let input = open_in (Filename.dirname (Mark.path path)) in
      let value = Fun.protect ~finally:(fun () -> close_in input) (fun () -> input_line input) in
      if value <> "retained" then failwith "invalid recovery path was modified"
    end;
    Lwt_main.run (let open Lwt.Syntax in
      let* store = Store.open_store (Filename.concat path "irmin") in
      Lwt.finalize (fun () ->
        let* value = Store.read store ["value"] in
        if value <> Some "3" then failwith "GC exit changed committed contents";
        Lwt.return_unit) (fun () -> Store.close store)));
  print_endline "event = fatal_gc status = passed")

let ownership () = Test_workspace.with_dir "fatal_owner" (fun path ->
  let owner = Lock.acquire path in
  Fun.protect ~finally:(fun () -> Lock.release owner) (fun () ->
    let exited = ref false in
    Lwt_main.run (S.run_join ~log:S.default_join_log
      ~tasks:[Lwt.fail (Failure "runtime task")]
      ~exit_refused:(fun () -> failwith "ordinary fault became worker stop")
      ~exit_fatal:(fun () ->
        exited := true;
        match Lock.acquire path with
        | other -> Lock.release other; failwith "fatal exit released ownership"
        | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), "flock", _) -> ()));
    if not !exited then failwith "fatal task did not exit";
    print_endline "event = fatal_owner status = passed"))

let fatal_process () =
  let pid = Unix.create_process Sys.executable_name
    [|Sys.executable_name; "--fatal-child"|] Unix.stdin Unix.stdout Unix.stderr in
  let clock = Mtime_clock.counter () in
  let rec reap () =
    try ignore (Unix.waitpid [] pid)
    with Unix.Unix_error (Unix.EINTR, _, _) -> reap () in
  let rec wait () = match Unix.waitpid [Unix.WNOHANG] pid with
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
    | 0, _ ->
      if Mtime.Span.to_float_ns (Mtime_clock.count clock) > 3e9 then begin
        Unix.kill pid Sys.sigkill;
        reap ();
        failwith "fatal exit waited for diagnostic hook"
      end;
      Unix.sleepf 0.01;
      wait ()
    | _, Unix.WEXITED 1 -> ()
    | _ -> failwith "fatal exit status differs" in
  wait ();
  print_endline "event = fatal_process status = passed"

let () =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--fatal-child" then begin
    Stdlib.at_exit (fun () -> Unix.sleep 30);
    Lwt_main.run (S.run_join ~log:S.default_join_log
      ~tasks:[Lwt.fail (Failure "fatal child")]
      ~exit_refused:(fun () -> failwith "ordinary fault became worker stop")
      ~exit_fatal:S.exit_fatal)
  end else if Array.length Sys.argv = 4 && Sys.argv.(1) = "--gc-child" then
    Lwt_main.run (gc_child Sys.argv.(2) Sys.argv.(3))
  else begin ownership (); fatal_process ();
    List.iter gc_process ["task"; "async"; "worker"; "worker-async"; "worker-log"; "worker-warn";
      "sync"; "sync-error"; "sync-present"; "worker-apply"; "worker-apply-log"; "apply"] end