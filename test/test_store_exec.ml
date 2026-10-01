(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Case = Recovery_case
module Chain = Case.SC
module Txlog = Octra_core.Txlog
module Epochlog = Octra_core.Epochlog

external fd_number : Unix.file_descr -> int = "octra_test_fd"
external fd_value : int -> Unix.file_descr = "octra_test_fd"

let identity name descriptor =
  let value = Unix.fstat descriptor in
  [name; string_of_int (fd_number descriptor); string_of_int value.Unix.st_dev;
    string_of_int value.Unix.st_ino]

let rec inspect = function
  | [] -> true
  | name :: number :: device :: inode :: rest ->
    let retained = match Unix.fstat (fd_value (int_of_string number)) with
      | value -> value.Unix.st_dev = int_of_string device && value.Unix.st_ino = int_of_string inode
      | exception Unix.Unix_error (Unix.EBADF, _, _) -> false in
    Printf.printf "event = descriptor name = %s retained = %b\n%!" name retained;
    let remaining = inspect rest in
    not retained && remaining
  | _ -> failwith "descriptor arguments are incomplete"

let owner mode dir =
  let chain = Chain.open_chaindata (Filename.concat dir "chaindata") in
  Fun.protect ~finally:(fun () -> Chain.close chain) (fun () ->
    if mode = "cut" then begin
      let segment, offset = Chain.txlog_position chain in
      Txlog.truncate_to chain.Chain.txlog ~seg_id:segment ~offset
    end;
    if mode = "rotation" then Txlog.rotate chain.Chain.txlog;
    if mode = "explicit" then begin
      Unix.set_close_on_exec chain.Chain.txlog.Txlog.current_fd;
      Unix.set_close_on_exec chain.epochlog.Epochlog.fd
    end;
    let args = identity "txlog" chain.Chain.txlog.Txlog.current_fd
      @ identity "epochlog" chain.epochlog.Epochlog.fd in
    Unix.execv Sys.executable_name (Array.of_list (Sys.executable_name :: "inspect" :: args)))

let run root =
  let failed = List.filter_map (fun mode ->
    let dir = Filename.concat root mode in
    Unix.mkdir dir 0o700;
    if mode <> "fresh" then ignore (Case.prepare dir);
    let process = Unix.create_process Sys.executable_name
      [|Sys.executable_name; "owner"; mode; dir|] Unix.stdin Unix.stdout Unix.stderr in
    let result = Case.wait process in
    Printf.printf "event = exec case = %s exit = %s\n%!" mode
      (match result with Unix.WEXITED value -> string_of_int value | _ -> "signal");
    if result = Unix.WEXITED 0 then None else Some mode)
    ["fresh"; "existing"; "cut"; "rotation"; "explicit"] in
  if failed <> [] then exit 1

let () =
  match Array.to_list Sys.argv with
  | _ :: "inspect" :: args -> if not (inspect args) then exit 1
  | [_; "owner"; mode; dir] -> owner mode dir
  | _ :: root :: _ -> run root
  | [_] -> Test_workspace.with_dir "store_exec" run
  | _ -> failwith "test arguments are invalid"