(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Tx = Octra_core.Txlog
module Epoch = Octra_core.Epochlog

let expect message ok = if not ok then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
    really_input_string channel (in_channel_length channel))

let contents dir =
  Sys.readdir dir |> Array.to_list |> List.sort String.compare
  |> List.map (fun name -> name, read (Filename.concat dir name))

let test_tx dir choose =
  let log = Tx.open_log dir in
  Fun.protect ~finally:(fun () -> try Tx.close log with _ -> ()) (fun () ->
    ignore (Tx.append log ~epoch_id:0 ~payload:"committed");
    Tx.fsync log;
    let position = Tx.current_position log in
    let segment, offset = choose position in
    let before = contents dir in
    let refused = match Tx.truncate_to log ~seg_id:segment ~offset with
      | () -> false
      | exception _ -> true in
    expect "invalid transaction cut accepted" refused;
    expect "refused transaction cut changed files" (before = contents dir);
    expect "refused transaction cut changed position" (position = Tx.current_position log);
    ignore (Unix.fstat log.current_fd))

let test_epoch dir choose =
  let path = Filename.concat dir "epochs.dat" in
  let log = Epoch.open_log path in
  Fun.protect ~finally:(fun () -> Epoch.close log) (fun () ->
    Epoch.append log {Epoch.empty_epoch_header with id = 0};
    Epoch.fsync log;
    let finish = Epoch.current_offset log in
    let before = contents dir in
    let refused = match Epoch.truncate_to log ~offset:(choose finish) with
      | () -> false
      | exception _ -> true in
    expect "invalid epoch cut accepted" refused;
    expect "refused epoch cut changed files" (before = contents dir);
    expect "refused epoch cut changed position" (finish = Epoch.current_offset log))

let run root =
  let cases = [
    "tx_inside", (fun dir -> test_tx dir (fun (segment, offset) -> segment, offset - 1));
    "tx_past_end", (fun dir -> test_tx dir (fun (segment, offset) -> segment, offset + 1));
    "tx_missing", (fun dir -> test_tx dir (fun (segment, _) -> segment + 1, Tx.header_size));
    "tx_header", (fun dir -> test_tx dir (fun (segment, _) -> segment, Tx.header_size - 1));
    "epoch_inside", (fun dir -> test_epoch dir (fun offset -> offset - 1));
    "epoch_past_end", (fun dir -> test_epoch dir (fun offset -> offset + 1));
    "epoch_header", (fun dir -> test_epoch dir (fun _ -> Epoch.header_size - 1))] in
  let failed = List.fold_left (fun failed (name, action) ->
    let dir = Filename.concat root name in
    Unix.mkdir dir 0o700;
    try action dir;
      Printf.printf "case = %s status = pass\n%!" name;
      failed
    with exn ->
      Printf.eprintf "case = %s status = fail reason = %s\n%!" name (Printexc.to_string exn);
      true) false cases in
  if failed then exit 1

let () = Test_workspace.with_dir "log_cut" run