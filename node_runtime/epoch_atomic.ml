(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type effects = {
  abort_ledger : unit -> unit;
  abort_store : unit -> unit;
  abort_history : unit -> unit;
  fatal : string -> unit;
  exit : exn -> unit;
}

let attempt name f =
  try
    f ();
    None
  with exn ->
    Some (name ^ ": " ^ Printexc.to_string exn)

let run effects apply =
  Lwt.catch
    apply
    (fun exn ->
      Fun.protect ~finally:(fun () -> effects.exit exn) (fun () ->
        [
          "ledger", effects.abort_ledger;
          "store", effects.abort_store;
          "history", effects.abort_history;
        ]
        |> List.map (fun (name, abort) -> attempt name abort)
        |> List.filter_map Fun.id
        |> List.iter (fun error ->
          effects.fatal ("event = epoch_abort_failed reason = " ^ error));
        effects.fatal
          ("event = epoch_apply_failed reason = " ^ Printexc.to_string exn));
      Lwt.fail exn)

let exit_store ?(code = 1) store () =
  match Octra_core.Store_irmin.Store.Gc.cancel store.Octra_core.Store_irmin.repo with
  | _ -> Unix._exit code
  | exception _ -> Unix._exit code

let run_store ?(fatal = Octra_log.fatal "epoch" "%s") ~store ~ledger ~chaindata apply =
  run {
    abort_ledger = (fun () ->
      match Octra_core.Ledger.abort_journal ledger with
      | Ok () -> ()
      | Error error -> failwith error);
    abort_store = (fun () -> Octra_core.Store_irmin.abort_epoch_batch store);
    abort_history = (fun () -> Octra_core.Store_chaindata.abort_batch chaindata);
    fatal;
    exit = (fun error ->
      let code = match error with
        | Octra_core.Private_ledger.Worker_stopped _ -> 78
        | _ -> 1 in
      exit_store ~code store ());
  } apply