(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

exception Close_failed of string

type phase = Open of (unit -> unit) list | Closed | Failed of string
type t = { mutable phase : phase; release : unit -> unit }

let create ~release = {phase = Open []; release}

let acquire scope action close =
  match scope.phase with
  | Closed | Failed _ -> invalid_arg "store resource scope is not open"
  | Open pending ->
    let value = action () in
    scope.phase <- Open ((fun () -> close value) :: pending);
    value

let finish scope known_error =
  match scope.phase with
  | Closed -> ()
  | Failed reason -> raise (Close_failed reason)
  | Open pending ->
    scope.phase <- Closed;
    let error = List.fold_left (fun prior close ->
      match close () with
      | () -> prior
      | exception error ->
        (match prior with None -> Some (Printexc.to_string error) | Some _ -> prior))
      known_error pending in
    let error = match error with
      | Some _ -> error
      | None ->
        (match scope.release () with
        | () -> None
        | exception error -> Some (Printexc.to_string error)) in
    match error with
    | None -> ()
    | Some reason -> scope.phase <- Failed reason; raise (Close_failed reason)

let close scope = finish scope None

let guard scope action =
  match action () with
  | value -> value
  | exception error ->
    let trace = Printexc.get_raw_backtrace () in
    let known_error = match error with
      | Close_failed reason -> Some reason
      | _ -> None in
    finish scope known_error;
    Printexc.raise_with_backtrace error trace

let protect ~close action =
  match action () with
  | value -> value
  | exception error ->
    let trace = Printexc.get_raw_backtrace () in
    (match close () with
    | () -> ()
    | exception reason -> raise (Close_failed (Printexc.to_string reason)));
    Printexc.raise_with_backtrace error trace