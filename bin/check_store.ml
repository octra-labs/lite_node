(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module Core = Octra_core

let store_dir data_dir name =
  let path = Filename.concat data_dir name in
  if (Unix.stat path).Unix.st_kind <> Unix.S_DIR then
    invalid_arg ("store directory is missing: " ^ path);
  path

let check data_dir =
  let irmin_path = store_dir data_dir "irmin_store" in
  let chain_path = store_dir data_dir "chaindata" in
  let* store = Core.Store_irmin.open_store ~readonly:true irmin_path in
  Lwt.finalize (fun () ->
    let chain = Core.Store_chaindata.open_chaindata ~readonly:true chain_path in
    Lwt.finalize
      (fun () -> Core.Startup_recovery.inspect ~data_dir ~chaindata:chain ~store)
      (fun () -> Core.Store_chaindata.close chain; Lwt.return_unit))
    (fun () -> Core.Store_irmin.close store)

let () =
  if Array.length Sys.argv <> 2 then begin
    Printf.eprintf "event = usage command = check_store argument = offline_data_dir\n%!";
    exit 1
  end;
  match Lwt_main.run (check Sys.argv.(1)) with
  | _, plan, _, _ ->
    let action, epoch = match plan with
      | Core.Recovery_phase.Empty _ -> "empty", -1
      | Stay (head, _) -> "stay", head.epoch_id
      | Trim (head, _) -> "trim_txlog", head.epoch_id
      | Cut (head, _, _) -> "cut_to_head", head.epoch_id
      | Publish head -> "publish_head", head.epoch_id in
    Printf.printf "event = store_check status = verified action = %s epoch = %d readonly = true\n%!"
      action epoch
  | exception error ->
    Printf.eprintf "event = store_check status = refused reason = %S readonly = true\n%!"
      (Printexc.to_string error);
    exit 2