(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module Core = Octra_core

let store_dir data_dir name =
  let path = Filename.concat data_dir name in
  if (Unix.stat path).Unix.st_kind <> Unix.S_DIR then
    invalid_arg ("store directory is missing: " ^ path);
  path

let repair data_dir =
  let irmin_path = store_dir data_dir "irmin_store" in
  let chain_path = store_dir data_dir "chaindata" in
  let chain = Core.Store_chaindata.open_chaindata chain_path in
  Lwt.finalize (fun () ->
    let* store = Core.Store_irmin.open_store irmin_path in
    Lwt.finalize
      (fun () -> Core.Startup_recovery.recover_cut ~data_dir ~chaindata:chain ~store)
      (fun () -> Core.Store_irmin.close store))
    (fun () -> Core.Store_chaindata.close chain; Lwt.return_unit)

let () =
  let data_dir = match Array.to_list Sys.argv with
    | [_; "--help"] ->
      Printf.printf "command = repair_chaindata_to_head usage = \"--offline data_dir\" node = stopped ownership = exclusive action = cut_uncommitted_suffix\n%!";
      exit 0
    | [_; "--offline"; path] -> path
    | _ ->
      Printf.eprintf "status = refused reason = offline_confirmation_required usage = \"--offline data_dir\"\n%!";
      exit 2 in
  match Lwt_main.run (repair data_dir) with
  | result ->
    Printf.printf "event = rollback status = completed epoch = %d recovery = required\n%!"
      result.Core.Startup_recovery.irmin_last_epoch_after
  | exception error ->
    Printf.eprintf "event = rollback status = refused reason = %S\n%!" (Printexc.to_string error);
    exit 2