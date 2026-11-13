(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module S = Octra_node_runtime.Startup_run_shell
module Lock = Octra_core.Store_lock
module Store = Octra_core.Store_irmin

external halt_gc : bool -> unit = "octra_test_gc_halt"
external limit_gc : bool -> unit = "octra_test_gc_limit"
external gc_limits : unit -> int64 * int64 = "octra_test_gc_limits"
external gc_high_fd : Unix.file_descr -> bool = "octra_test_gc_high_fd"

let checked_exit pid code =
  match Unix.waitpid [Unix.WNOHANG] pid with
  | exception Unix.Unix_error (Unix.ECHILD, _, _) -> Unix._exit code
  | _ -> failwith "fatal exit did not reap owned child"

let file_hashes root =
  let rec walk path =
    if Sys.is_directory path then
      Sys.readdir path |> Array.to_list |> List.sort String.compare
      |> List.concat_map (fun name -> walk (Filename.concat path name))
    else [path, Digest.file path] in
  walk root

let measure_limits () =
  let limits = gc_limits () in
  let files = ref [] in
  Fun.protect ~finally:(fun () ->
    limit_gc false;
    List.iter (fun file -> try Unix.close file
      with Unix.Unix_error (Unix.EBADF, _, _) -> ()) !files) (fun () ->
    for _ = 1 to 48 do
      files := Unix.openfile "/dev/null" [Unix.O_RDONLY] 0 :: !files
    done;
    let inherited = List.find gc_high_fd !files in
    limit_gc true;
    if gc_limits () <> limits then failwith "GC hook changed parent limits";
    let size = Store.{records = 1L; bytes = 33L} in
    let actual = Lwt_main.run (Store.gc_pack_child (fun () ->
      if gc_limits () <> (16L, 16L) then failwith "GC child limits differ";
      match Unix.fstat inherited with
      | exception Unix.Unix_error (Unix.EBADF, _, _) -> size
      | _ -> failwith "GC child retained high descriptor")) in
    if actual <> size then failwith "GC limited child result differs";
    if gc_limits () <> limits then failwith "GC child changed parent limits";
    List.iter (fun file -> ignore (Unix.fstat file)) !files);
  if gc_limits () <> limits then failwith "GC disarm changed parent limits";
  print_endline "event = gc_fd_limits status = passed"

let mark_measure path =
  let file = Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o600 in
  Unix.close file

let measure_child path =
  let open Lwt.Syntax in
  let owner = Lock.acquire path in
  let* store = Store.open_store ~fresh:true (Filename.concat path "irmin") in
  let* () = Store.write store ["value"] "measure-committed" in
  let ready = Filename.concat path "measure.ready" in
  let release = Filename.concat path "measure.release" in
  let work = Store.run_gc_measure store (fun () ->
    let output = open_out ready in
    output_string output (string_of_int (Unix.getpid ()));
    close_out output;
    let clock = Mtime_clock.counter () in
    while Sys.file_exists path && not (Sys.file_exists release) do
      if Mtime.Span.to_float_ns (Mtime_clock.count clock) > 60e9 then
        failwith "GC measure barrier expired";
      Unix.sleepf 0.01
    done;
    Store.{records = 1L; bytes = 33L}) in
  let clock = Mtime_clock.counter () in
  let rec wait_ready () = match Lwt.state work with
    | Lwt.Fail error -> Lwt.fail error
    | Lwt.Return _ -> Lwt.fail_with "GC measure completed before exit"
    | Lwt.Sleep ->
      if Sys.file_exists ready then Lwt.return_unit
      else if Mtime.Span.to_float_ns (Mtime_clock.count clock) > 10e9 then
        Lwt.fail_with "GC measure readiness expired"
      else let* () = Lwt_unix.sleep 0.01 in wait_ready () in
  let* () = wait_ready () in
  if store.gc_job = None then failwith "GC measure job not owned";
  (match Lock.acquire path with
  | other -> Lock.release other; failwith "GC measure setup lost ownership"
  | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), "flock", _) -> ());
  ignore (Sys.opaque_identity owner);
  let input = open_in ready in
  let pid = Fun.protect ~finally:(fun () -> close_in input)
    (fun () -> int_of_string (input_line input)) in
  Store.Store.Backend.Repo.batch store.repo (fun _ _ _ ->
    S.exit_store ~exit:(checked_exit pid) store ())

let measure_process () = Test_workspace.with_dir "fatal_measure" (fun path ->
  let pid = Unix.create_process Sys.executable_name
    [|Sys.executable_name; "--gc-child"; path; "measure"|] Unix.stdin Unix.stdout Unix.stderr in
  let owned = ref true in
  let wait seconds =
    let clock = Mtime_clock.counter () in
    let rec poll () =
      if Mtime.Span.to_float_ns (Mtime_clock.count clock) > seconds *. 1e9 then None
      else match Unix.waitpid [Unix.WNOHANG] pid with
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> poll ()
      | exception Unix.Unix_error (Unix.ECHILD, _, _) ->
        owned := false;
        failwith "GC measure owner reaped elsewhere"
      | 0, _ -> Unix.sleepf 0.01; poll ()
      | _, status -> owned := false; Some status in
    poll () in
  Fun.protect ~finally:(fun () ->
    Fun.protect ~finally:(fun () ->
      if !owned then begin
        (try Unix.kill pid Sys.sigkill with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
        if wait 5. = None then failwith "GC measure owner was not reaped"
      end) (fun () -> mark_measure (Filename.concat path "measure.release"))) (fun () ->
    let status = match wait 30. with
      | Some status -> status
      | None -> failwith "GC measure owner exit expired" in
    let owner = match Lock.acquire path with
      | owner -> owner
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), "flock", _) ->
        failwith "fatal measure retained store ownership" in
    Fun.protect ~finally:(fun () -> Lock.release owner) (fun () ->
      if status <> Unix.WEXITED 1 then failwith "GC measure owner exit differs";
      if not (Sys.file_exists (Filename.concat path "measure.ready")) then
        failwith "GC measure isolation was not reached"));
  print_endline "event = fatal_measure status = passed")

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
  let pid = Option.get (Store.Store.Internal.gc_pid store.repo) in
  let _, status = Unix.waitpid [Unix.WUNTRACED] pid in
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
  if mode = "refused" then begin
    store.gc_job <- Some {task = Lwt.return_unit; finished = Lwt.return_unit;
      stop = (fun () -> failwith "owned worker refusal")};
    S.exit_store store ()
  end else if mode = "signal" then begin
    Sys.set_signal Sys.sigusr1 (Sys.Signal_handle (fun _ ->
      S.exit_store ~exit:(checked_exit pid) store ()));
    Unix.kill (Unix.getpid ()) Sys.sigusr1;
    Lwt.fail_with "fatal signal returned"
  end else if mode = "batch" then begin
    let before = file_hashes store.store_path in
    Store.Store.Backend.Repo.batch store.repo (fun contents _ _ ->
      let* _ = Store.Store.Backend.Contents.add contents "unpublished-batch" in
      let exit code =
        if file_hashes store.store_path <> before then
          failwith "fatal batch flushed store files";
        checked_exit pid code in
      S.exit_store ~exit store ())
  end else if mode = "worker-apply" || mode = "worker-apply-log" || mode = "apply" then begin
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
  end else if mode = "async" || mode = "worker-async" || mode = "store-async" then begin
    Octra_node_runtime.Startup_process_shell.configure_lwt ~exit_fatal ~exit_refused;
    !Lwt.async_exception_hook (if mode = "async" then Out_of_memory
      else if mode = "store-async" then
        Octra_node_runtime.Consensus_pending_commit_recovery.Store_fault "journal"
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
        Octra_core.Private_ledger.Worker_stopped "test"
        else if mode = "store" then
          Octra_node_runtime.Consensus_pending_commit_recovery.Store_fault "journal"
        else Failure "GC owner task")]
      ~exit_fatal ~exit_refused

let gc_process mode = Test_workspace.with_dir "fatal_gc" (fun path ->
  let pid = Unix.create_process Sys.executable_name
    [|Sys.executable_name; "--gc-child"; path; mode|] Unix.stdin Unix.stdout Unix.stderr in
  let rec reap () =
    try snd (Unix.waitpid [] pid)
    with Unix.Unix_error (Unix.EINTR, _, _) -> reap () in
  let code = if String.starts_with ~prefix:"worker" mode
    || String.starts_with ~prefix:"store" mode then 78 else 1 in
  let deadline = Mtime_clock.counter () in
  let rec wait () = match Unix.waitpid [Unix.WNOHANG; Unix.WUNTRACED] pid with
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
    | 0, _ ->
      if Mtime.Span.to_float_ns (Mtime_clock.count deadline) > 30e9 then begin
        Unix.kill (-pid) Sys.sigkill;
        ignore (reap ());
        failwith "fatal GC exit expired"
      end;
      Unix.sleepf 0.01; wait ()
    | _, status -> status in
  let status = wait () in
  let retained = mode = "refused" && status = Unix.WSTOPPED Sys.sigstop in
  if retained then begin
    (match Lock.acquire path with
    | other -> Lock.release other; failwith "uncertain exit released store lock"
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), "flock", _) -> ());
    Unix.kill (-pid) Sys.sigkill;
    ignore (reap ())
  end;
  if (mode = "refused" && not retained)
    || (mode <> "refused" && status <> Unix.WEXITED code) then begin
    (try Unix.kill (-pid) Sys.sigkill
     with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
    failwith "GC owner exit differs"
  end;
  let clock = Mtime_clock.counter () in
  let rec acquire () =
    match Lock.acquire path with
    | owner -> owner
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), "flock", _) ->
      if Mtime.Span.to_float_ns (Mtime_clock.count clock) > 2e9 then begin
        Unix.kill (-pid) Sys.sigkill;
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
        let* () = Store.write store ["value"] "4" in
        let* () = Store.tag_epoch store 5 in
        let* result = Store.collect_pack_at ~free:Int64.max_int store ~keep:1 5 in
        (match result with Store.Gc_started _ -> () | _ -> failwith "GC exit prevented next collection");
        let* result = Store.Store.Gc.wait store.repo in
        (match result with Ok (Some _) -> () | _ -> failwith "GC after exit failed");
        let* value = Store.read store ["value"] in
        if value <> Some "4" then failwith "GC after exit changed committed contents";
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
      if Mtime.Span.to_float_ns (Mtime_clock.count clock) > 120e9 then begin
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
  end else if Array.length Sys.argv = 4 && Sys.argv.(1) = "--gc-child" then begin
    ignore (Unix.setsid ());
    Lwt_main.run (if Sys.argv.(3) = "measure" then measure_child Sys.argv.(2)
      else gc_child Sys.argv.(2) Sys.argv.(3))
  end
  else begin ownership (); fatal_process (); measure_process (); measure_limits ();
    List.iter gc_process ["task"; "async"; "worker"; "worker-async"; "worker-log"; "worker-warn";
      "sync"; "sync-error"; "sync-present"; "worker-apply"; "worker-apply-log"; "apply";
      "store"; "store-async"; "batch"; "signal"; "refused"] end