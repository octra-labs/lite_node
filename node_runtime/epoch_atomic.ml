(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type effects = {
  abort_ledger : unit -> unit;
  abort_store : unit -> unit;
  abort_history : unit -> unit;
  fatal : string -> unit;
  exit : unit -> unit;
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
        ("event = epoch_apply_failed reason = " ^ Printexc.to_string exn);
      effects.exit ();
      Lwt.fail exn)