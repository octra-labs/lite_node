(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module S = Octra_node_runtime.Startup_store_shell
module W = Octra_node_runtime.Wal_start
module Lock = Octra_core.Store_lock

let expect reason value = if not value then failwith reason

let close (chain, store) =
  Fun.protect ~finally:(fun () -> Octra_core.Store_chaindata.close chain)
    (fun () -> Lwt_main.run (Octra_core.Store_irmin.close store))

let rec wait pid =
  match Unix.waitpid [] pid with
  | _, status -> status
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait pid

let held data =
  match W.recover ~data_dir:data (fun () -> S.open_stores data) with
  | Ok stores -> close stores; failwith "busy startup acquired stores"
  | Error error ->
    expect "busy startup exit policy changed" (W.exit_code = 78);
    expect "busy startup lost its path" (error.path = data);
    expect "busy startup reason changed"
      (error.reason = "store ownership is held by another process");
    expect "busy startup opened Irmin" (not (Sys.file_exists (S.irmin_path data)));
    expect "busy startup changed chaindata"
      (Sys.readdir (Filename.concat data "chaindata") = [||])

let inherited data =
  let chain = Filename.concat data "chaindata" in
  Unix.mkdir chain 0o700;
  let owner = Lock.acquire chain in
  let input, output = Unix.pipe ~cloexec:true () in
  flush_all ();
  match Unix.fork () with
  | 0 ->
    Unix.close output;
    let byte = Bytes.create 1 in
    ignore (Unix.read input byte 0 1);
    Unix.close input;
    Lock.release owner;
    Unix._exit 0
  | pid ->
    Unix.close input;
    Lock.release owner;
    Fun.protect ~finally:(fun () ->
      Unix.close output;
      expect "inherited owner did not exit" (wait pid = Unix.WEXITED 0))
      (fun () -> held data);
    close (S.open_stores data)

let open_failure data =
  let path = S.irmin_path data in
  let output = open_out_bin path in
  output_string output "retained";
  close_out output;
  begin match S.open_stores data with
  | stores -> close stores; failwith "invalid Irmin path accepted"
  | exception _ -> ()
  end;
  let owner = Lock.acquire (Filename.concat data "chaindata") in
  Lock.release owner;
  let input = open_in_bin path in
  let value = Fun.protect ~finally:(fun () -> close_in input)
    (fun () -> really_input_string input (in_channel_length input)) in
  expect "failed opening changed Irmin evidence" (value = "retained")

let root_error data =
  let module I = Octra_core.Store_irmin in
  let stores = S.open_stores data in
  Fun.protect ~finally:(fun () -> close stores) (fun () ->
    let _, store = stores in
    Lwt_main.run (I.set_meta store "last_epoch" "0");
    Lwt_main.run (I.set_account store "octTEST" Octra_core.Ledger_types.empty_account);
    let output = open_out_bin store.I.state_root_file in
    Fun.protect ~finally:(fun () -> close_out output)
      (fun () -> output_string output "different\n");
    let verify () = Lwt_main.run (I.verify_integrity store) in
    let fault = verify () in
    expect "saved root mismatch was not reported" (not fault.ok && List.length fault.errors = 1);
    expect "saved root event overlaps apply failure"
      (List.for_all (String.starts_with ~prefix:"event = saved_root_mismatch ") fault.errors);
    let writes = ref 0 and exits = ref 0 in
    let root () = Lwt_main.run (I.get_head_hash store) in
    let deps = S.{
      head_state = (fun () -> Head_ready {epoch = 0; root = Option.get (root ())});
      store_root = root;
      epoch_root = (fun _ -> root ());
      rollback_epoch = (fun _ -> failwith "unexpected rollback");
      verify_integrity = verify;
      save_state_root = (fun () -> incr writes; Lwt_main.run (I.save_state_root store));
      exit_fatal = (fun () -> incr exits);
    } in
    S.run_integrity deps;
    expect "root repair did not reverify" (!writes = 1 && !exits = 0 && (verify ()).ok);
    Lwt_main.run (I.write store ["accounts"; "octTEST"; "data"] "invalid");
    let fault = verify () in
    expect "invalid account was not reported" (not fault.ok);
    S.run_integrity deps;
    expect "invalid account was repaired or accepted" (!writes = 1 && !exits = 1))

let () =
  Test_workspace.with_dir "store_boot" (fun root ->
    List.iter (fun (name, run) ->
      let data = Filename.concat root name in
      Unix.mkdir data 0o700;
      run data;
      Printf.printf "event = passed case = %s\n%!" name)
      ["fresh", (fun data -> close (S.open_stores data));
       "inherited", inherited; "open_failure", open_failure; "root_error", root_error];
    match W.recover ~data_dir:root (fun () ->
      raise (Unix.Unix_error (Unix.EACCES, "open", root))) with
    | _ -> failwith "unrelated startup error was hidden"
    | exception Unix.Unix_error (Unix.EACCES, "open", _) -> ());
  Printf.printf "status = pass test = store_boot\n%!"