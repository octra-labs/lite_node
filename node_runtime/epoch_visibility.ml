(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type phase =
  | Stable of int
  | Applying of int

type t = {
  mutable phase : phase;
  changed : unit Lwt_condition.t;
}

let create () = {
  phase = Stable 0;
  changed = Lwt_condition.create ();
}

let begin_apply t =
  match t.phase with
  | Applying _ -> Error "epoch visibility transition is already active"
  | Stable generation ->
    t.phase <- Applying (generation + 1);
    Ok ()

let finish_apply t =
  match t.phase with
  | Stable _ -> Error "epoch visibility transition is not active"
  | Applying generation ->
    t.phase <- Stable (generation + 1);
    Lwt_condition.broadcast t.changed ();
    Ok ()

let with_apply t apply =
  match begin_apply t with
  | Error error -> Lwt.fail_with error
  | Ok () ->
    Lwt.finalize
      apply
      (fun () ->
        match finish_apply t with
        | Ok () -> Lwt.return_unit
        | Error error -> Lwt.fail_with error)

let rec await_stable t =
  match t.phase with
  | Stable generation -> Lwt.return generation
  | Applying _ ->
    let open Lwt.Syntax in
    let* () = Lwt_condition.wait t.changed in
    await_stable t

let same_generation t generation =
  match t.phase with
  | Stable current -> current = generation
  | Applying _ -> false

let read ?(retries=2) t read_value =
  let rec attempt remaining =
    let open Lwt.Syntax in
    let* generation = await_stable t in
    let* value = read_value () in
    if same_generation t generation then
      Lwt.return_some value
    else if remaining <= 0 then
      Lwt.return_none
    else
      attempt (remaining - 1)
  in
  attempt retries

let is_applying t =
  match t.phase with
  | Stable _ -> false
  | Applying _ -> true