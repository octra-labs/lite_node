(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type deps = {
  now : unit -> float;
  wait : float -> unit Lwt.t;
  staged : string -> bool;
  post : Octra_core.Transaction.t -> (unit, string) result Lwt.t;
  warn : string -> unit;
}

type item = {
  hash : string;
  tx : Octra_core.Transaction.t;
}

type t = {
  deps : deps;
  mutable item : item option;
  mutable due : float;
  mutable busy : bool;
  mutable timer : bool;
  mutable open_ : bool;
}

let period = 30.0

let create deps = {
  deps;
  item = None;
  due = 0.0;
  busy = false;
  timer = false;
  open_ = true;
}

let clear t =
  t.item <- None;
  t.due <- 0.0

let rec arm t =
  if t.open_ && Option.is_some t.item && not t.timer then begin
    t.timer <- true;
    let delay = max 0.0 (t.due -. t.deps.now ()) in
    Lwt.async (fun () ->
      let open Lwt.Syntax in
      let* () = t.deps.wait delay in
      t.timer <- false;
      run t;
      Lwt.return_unit)
  end

and run t =
  match t.item with
  | None -> ()
  | Some _ when not t.open_ -> clear t
  | Some item when not (t.deps.staged item.hash) -> clear t
  | Some _ when t.busy -> ()
  | Some _ when t.deps.now () < t.due -> arm t
  | Some item ->
    t.busy <- true;
    t.due <- t.deps.now () +. period;
    Lwt.async (fun () ->
      let open Lwt.Syntax in
      let* result =
        Lwt.catch
          (fun () -> t.deps.post item.tx)
          (fun exn -> Lwt.return_error (Printexc.to_string exn))
      in
      t.busy <- false;
      begin
        match result with
        | Ok () -> ()
        | Error reason -> t.deps.warn reason
      end;
      arm t;
      Lwt.return_unit)

let put t ~hash tx =
  t.item <- Some { hash; tx };
  t.due <- t.deps.now ();
  run t

let tick t = run t

let stop t =
  t.open_ <- false;
  clear t