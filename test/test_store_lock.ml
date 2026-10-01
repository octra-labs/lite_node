(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Store_lock = Octra_core.Store_lock

let expect message value = if not value then failwith message

let busy action =
  match action () with
  | owner -> Store_lock.release owner; failwith "second writer was accepted"
  | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), "flock", _) -> ()

let rec wait child =
  match Unix.waitpid [] child with
  | _, status -> status
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait child

let execute args =
  let command = Array.of_list (Sys.executable_name :: args) in
  wait (Unix.create_process Sys.executable_name command Unix.stdin Unix.stdout Unix.stderr)

let child mode dir =
  match mode with
  | "probe" ->
    (match Store_lock.acquire dir with
    | owner -> Store_lock.release owner; exit 0
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), "flock", _) -> exit 2)
  | "hold" ->
    let owner = Store_lock.acquire dir in
    Fun.protect ~finally:(fun () -> Store_lock.release owner) (fun () ->
      print_endline "ready";
      flush stdout;
      ignore (input_line stdin))
  | "exec" ->
    let owner = Store_lock.acquire dir in
    Fun.protect ~finally:(fun () -> Store_lock.release owner) (fun () ->
      Unix.execv Sys.executable_name [|Sys.executable_name; "child"; "probe"; dir|])
  | _ -> failwith "unknown child mode"

let with_owner dir action =
  let owner = Store_lock.acquire dir in
  Fun.protect ~finally:(fun () -> Store_lock.release owner) (fun () -> action owner)

let test_kill dir =
  let input, input_write = Unix.pipe ~cloexec:true () in
  let output_read, output = Unix.pipe ~cloexec:true () in
  let process = Unix.create_process Sys.executable_name
    [|Sys.executable_name; "child"; "hold"; dir|] input output Unix.stderr in
  Unix.close input;
  Unix.close output;
  let channel = Unix.in_channel_of_descr output_read in
  Fun.protect ~finally:(fun () ->
    close_in_noerr channel;
    Unix.close input_write;
    (try Unix.kill process Sys.sigkill with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
    (try ignore (wait process) with Unix.Unix_error (Unix.ECHILD, _, _) -> ())) (fun () ->
    let ready, _, _ = Unix.select [output_read] [] [] 5. in
    expect "child did not signal readiness" (ready <> []);
    expect "child readiness differs" (input_line channel = "ready");
    busy (fun () -> Store_lock.acquire dir);
    Unix.kill process Sys.sigkill;
    expect "owner was not killed" (wait process = Unix.WSIGNALED Sys.sigkill);
    with_owner dir (fun _ -> ()))

let run root =
  let dir name =
    let path = Filename.concat root name in
    Unix.mkdir path 0o700;
    path in
  let cases = [
    "orphan_wait", (fun () ->
      let path = dir "orphan_wait" in
      let input, output = Unix.pipe ~cloexec:true () in
      let creator = match Unix.fork () with
        | 0 ->
          Unix.close input;
          let owner = Store_lock.acquire path in
          (match Unix.fork () with
          | 0 -> Unix.sleepf 0.3; Store_lock.release owner; Unix._exit 0
          | _ -> ignore (Unix.write_substring output "r" 0 1); Unix._exit 0)
        | pid -> pid in
      Unix.close output;
      let byte = Bytes.create 1 in
      expect "owner readiness missing" (Unix.read input byte 0 1 = 1);
      expect "owner exit differs" (wait creator = Unix.WEXITED 0);
      busy (fun () -> Store_lock.acquire path);
      let owner = Store_lock.acquire ~wait_seconds:2. path in
      Store_lock.release owner;
      expect "orphan still running" (Unix.read input byte 0 1 = 0);
      Unix.close input;
      expect "lock wait mutated store" (Sys.readdir path = [||]));
    "wait_deadline", (fun () ->
      let path = dir "wait_deadline" in
      with_owner path (fun _ ->
        let clock = Mtime_clock.counter () in
        busy (fun () -> Store_lock.acquire ~wait_seconds:0.05 path);
        expect "lock deadline did not wait"
          (Mtime.Span.to_float_ns (Mtime_clock.count clock) >= 40_000_000.)));
    "same_process", (fun () ->
      let path = dir "same_process" in
      with_owner path (fun _ -> busy (fun () -> Store_lock.acquire path)));
    "other_process", (fun () ->
      let path = dir "other_process" in
      with_owner path (fun _ ->
        expect "child writer was not refused" (execute ["child"; "probe"; path] = Unix.WEXITED 2)));
    "reopen", (fun () ->
      let path = dir "reopen" in
      with_owner path (fun _ -> ());
      with_owner path (fun _ -> ()));
    "alias", (fun () ->
      let path = dir "alias" in
      let alias = Filename.concat root "link" in
      Unix.symlink path alias;
      with_owner path (fun _ -> busy (fun () -> Store_lock.acquire alias)));
    "other_directory", (fun () ->
      let path = dir "first" and other = dir "second" in
      with_owner path (fun _ -> with_owner other (fun _ -> ())));
    "close_other", (fun () ->
      let path = dir "close_other" in
      with_owner path (fun _ ->
        let descriptor = Unix.openfile path [Unix.O_RDONLY] 0 in
        Unix.close descriptor;
        busy (fun () -> Store_lock.acquire path)));
    "double_release", (fun () ->
      let path = dir "double_release" in
      let owner = Store_lock.acquire path in
      Store_lock.release owner;
      with_owner path (fun _ ->
        Store_lock.release owner;
        busy (fun () -> Store_lock.acquire path)));
    "no_mutation", (fun () ->
      let path = dir "no_mutation" in
      with_owner path (fun _ -> expect "ownership wrote directory content" (Sys.readdir path = [||])));
    "file_refused", (fun () ->
      let path = Filename.concat root "file" in
      let channel = open_out_bin path in
      close_out channel;
      (match Store_lock.acquire path with
      | owner -> Store_lock.release owner; failwith "regular file was accepted"
      | exception Invalid_argument _ -> ()));
    "exec_release", (fun () ->
      expect "exec retained ownership" (execute ["child"; "exec"; dir "exec"] = Unix.WEXITED 0));
    "kill_release", (fun () -> test_kill (dir "kill"))] in
  let failed = List.filter_map (fun (name, action) ->
    match action () with
    | () -> Printf.printf "event = passed case = %s\n%!" name; None
    | exception error ->
      Printf.eprintf "event = failed case = %s reason = %s\n%!" name (Printexc.to_string error);
      Some name) cases in
  if failed <> [] then exit 1

let () =
  match Array.to_list Sys.argv with
  | [_; "child"; mode; dir] -> child mode dir
  | [_] -> Test_workspace.with_dir "store_lock" run
  | [_; root] -> run root
  | _ -> failwith "test arguments are invalid"