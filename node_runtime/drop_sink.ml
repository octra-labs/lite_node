(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Rows = Map.Make (String)
module Row = Octra_core.Drop_record

type entry = { row : Row.t; bytes : int; until : float }
type phase = Open | Closing | Failed of string | Closed
type state = { rows : entry Rows.t; bytes : int; phase : phase }
type t = {
  mutable state : state;
  write : Row.t list -> (unit, string) result Lwt.t;
  now : unit -> float;
  wake : unit Lwt_condition.t;
  done_ : unit Lwt.t;
}

let capacity = 4096
let byte_limit = 4 * 1024 * 1024
let batch_limit = 256
let lifetime = 30.

let insert state now rows =
  let put state row =
    let bytes = String.length (Row.encode row) in
    let old = match Rows.find_opt row.Row.hash state.rows with
      | None -> 0 | Some entry -> entry.bytes in
    let rows = Rows.add row.hash {row; bytes; until = now +. lifetime} state.rows in
    let bytes = state.bytes - old + bytes in
    if Rows.cardinal rows > capacity || bytes > byte_limit then
      invalid_arg "local drop queue is full";
    {state with rows; bytes} in
  match state.phase with
  | Closing | Closed -> Error "local drop writer is closing"
  | Failed reason -> Error reason
  | Open ->
    try Ok (List.fold_left (fun state row -> put state row) state rows)
    with error -> Error (Printexc.to_string error)

let take state now =
  let live, expired = Rows.partition (fun _ entry -> entry.until >= now) state.rows in
  let ordered = Rows.bindings live |> List.map snd
    |> List.sort (fun a b -> String.compare (Row.order_key a.row) (Row.order_key b.row)) in
  let rec select count selected rows = function
    | [] -> List.rev selected, rows
    | _ when count = batch_limit -> List.rev selected, rows
    | entry :: rest -> select (count + 1) (entry.row :: selected)
        (Rows.remove entry.row.hash rows) rest in
  let batch, rows = select 0 [] live ordered in
  let bytes = Rows.fold (fun _ (entry : entry) sum -> sum + entry.bytes) rows 0 in
  batch, {state with rows; bytes}, Rows.cardinal expired

let rec loop t =
  let open Lwt.Syntax in
  match t.state.phase with
  | Closed | Failed _ -> Lwt.return_unit
  | Closing when Rows.is_empty t.state.rows ->
    t.state <- {t.state with phase = Closed};
    Lwt.return_unit
  | Open when Rows.is_empty t.state.rows ->
    let* () = Lwt_condition.wait t.wake in
    loop t
  | Open | Closing ->
    let* () = Lwt_unix.sleep 0.02 in
    let batch, state, expired = take t.state (t.now ()) in
    t.state <- state;
    if expired > 0 then Log.warn "staging" "event = drop_queue_expired count = %d" expired;
    let* result = if batch = [] then Lwt.return_ok () else
      Lwt.catch (fun () -> t.write batch)
        (fun error -> Lwt.return_error (Printexc.to_string error)) in
    begin match result with
    | Ok () -> loop t
    | Error reason ->
      let lost = List.length batch + Rows.cardinal t.state.rows in
      t.state <- {rows = Rows.empty; bytes = 0; phase = Failed reason};
      Log.warn "staging" "event = drop_persist_failed count = %d reason = %s" lost reason;
      Lwt.return_unit
    end

let create ~now ~write =
  let done_, resolver = Lwt.wait () in
  let t = {state = {rows = Rows.empty; bytes = 0; phase = Open};
    now; write; wake = Lwt_condition.create (); done_} in
  Lwt.async (fun () ->
    let open Lwt.Syntax in
    let* () = loop t in
    Lwt.wakeup_later resolver ();
    Lwt.return_unit);
  t

let submit t rows =
  match insert t.state (t.now ()) rows with
  | Error _ as error -> error
  | Ok state ->
    t.state <- state;
    Lwt_condition.signal t.wake ();
    Ok ()

let shutdown t =
  begin match t.state.phase with
  | Open -> t.state <- {t.state with phase = Closing}
  | Closing | Failed _ | Closed -> ()
  end;
  Lwt_condition.signal t.wake ();
  Lwt.protected t.done_

let finish ~close t =
  let open Lwt.Syntax in
  let* drained = Lwt.pick [
    Lwt.map (fun () -> true) (shutdown t);
    Lwt.map (fun () -> false) (Lwt_unix.sleep 2.)
  ] in
  if drained then close ()
  else Log.warn "staging" "event = drop_shutdown_timeout seconds = 2";
  Lwt.return_unit