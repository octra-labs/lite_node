(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type failure = Retry of string | Wait of string | Refused of string

let http_failure status =
  let reason = Printf.sprintf "validator duty RPC returned HTTP %d" status in
  if status = 408 || status = 429 || status >= 500 && status < 600 then
    Retry reason
  else
    Refused reason

let rpc_failure error =
  let reason =
    "validator duty RPC refused transaction: " ^ Yojson.Safe.to_string error
  in
  let code, data =
    match error with
    | `Assoc fields -> List.assoc_opt "code" fields, List.assoc_opt "data" fields
    | _ -> None, None
  in
  match code, data with
  | Some (`Int (104 | 107 | 110 | 113 | -32005)), _ -> Retry reason
  | Some (`Int (100 | 103 | 106)), _ -> Wait reason
  | Some (`Int 105), Some (`String "duplicate nonce (fee rate bump < 10%)") ->
    Wait reason
  | Some (`Int 105), Some (`String data)
    when List.exists
      (fun prefix -> data = prefix || String.starts_with ~prefix:(prefix ^ " ") data)
      ["pre_verify_busy"; "pre_verify_unavailable"] ->
    Retry reason
  | _ -> Refused reason

type deps = {
  now : unit -> float;
  wait : float -> unit Lwt.t;
  staged : string -> bool;
  landed : Octra_core.Transaction.t -> bool;
  post : Octra_core.Transaction.t -> (unit, failure) result Lwt.t;
  warn : string -> unit;
}

type eligibility = Eligible | Paused | Expired

type retry = {
  eligible : Octra_core.Transaction.t -> eligibility;
  post : current:(unit -> bool) -> Octra_core.Transaction.t -> (unit, failure) result Lwt.t;
  retain : bool;
}

type delivery = Pending | Rejected

type item = {
  hash : string;
  tx : Octra_core.Transaction.t;
  attempts : int;
  retry : retry option;
  delivery : delivery;
  generation : int;
}

type t = {
  deps : deps;
  mutable item : item option;
  mutable due : float;
  mutable busy : bool;
  mutable timer : unit Lwt.t option;
  mutable open_ : bool;
  mutable generation : int;
}

let period = 30.0

let retry_delay = function
  | 1 -> 1.0
  | 2 -> 2.0
  | _ -> period

let create deps = {
  deps;
  item = None;
  due = 0.0;
  busy = false;
  timer = None;
  open_ = true;
  generation = 0;
}

let cancel_timer t =
  let timer = t.timer in
  t.timer <- None;
  Option.iter Lwt.cancel timer

let clear t =
  cancel_timer t;
  t.item <- None;
  t.due <- 0.0

let retained item =
  Option.fold ~none:false ~some:(fun retry -> retry.retain) item.retry

let eligibility t item =
  if t.deps.landed item.tx then Expired
  else if not (retained item) && not (t.deps.staged item.hash) then Expired
  else match item.retry with
    | None -> Eligible
    | Some retry ->
      try retry.eligible item.tx
      with exn ->
        t.deps.warn (Printexc.to_string exn);
        Paused

let pending t =
  begin
    match t.item with
    | Some item when eligibility t item = Expired -> clear t
    | _ -> ()
  end;
  t.busy || Option.is_some t.item

let rec arm t =
  if t.open_ && Option.is_some t.item && Option.is_none t.timer then begin
    let delay = max 0.0 (t.due -. t.deps.now ()) in
    let timer = t.deps.wait delay in
    t.timer <- Some timer;
    let owns_timer () =
      Option.fold ~none:false ~some:(fun current -> current == timer) t.timer
    in
    Lwt.async (fun () ->
      Lwt.try_bind
        (fun () -> timer)
        (fun () ->
          if owns_timer () then begin
            t.timer <- None;
            run t
          end;
          Lwt.return_unit)
        (fun exn ->
          if owns_timer () then begin
            t.timer <- None;
            match exn with
            | Lwt.Canceled -> ()
            | _ -> t.deps.warn (Printexc.to_string exn)
          end;
          Lwt.return_unit))
  end

and run t =
  match t.item with
  | None -> ()
  | Some _ when not t.open_ -> clear t
  | Some _ when t.busy -> ()
  | Some item ->
    match eligibility t item with
    | Expired -> clear t
    | Paused ->
      cancel_timer t;
      t.due <- t.deps.now () +. period;
      arm t
    | Eligible when t.deps.now () < t.due -> arm t
    | Eligible when item.delivery = Rejected ->
      t.due <- t.deps.now () +. period;
      arm t
    | Eligible ->
    cancel_timer t;
    let attempts = min 3 (item.attempts + 1) in
    t.item <- Some { item with attempts };
    t.busy <- true;
    t.due <- t.deps.now () +. retry_delay attempts;
    let current () =
      t.open_ && Option.fold ~none:false
        ~some:(fun (active : item) -> active.generation = item.generation) t.item
    in
    Lwt.async (fun () ->
      let open Lwt.Syntax in
      let* result =
        Lwt.catch
          (fun () ->
            match item.retry with
            | None -> t.deps.post item.tx
            | Some retry -> retry.post ~current item.tx)
          (fun exn -> Lwt.return_error (Retry (Printexc.to_string exn)))
      in
      t.busy <- false;
      begin
        match t.item with
        | Some current when current.generation = item.generation ->
          begin
            match result with
            | Ok () when retained current ->
              if eligibility t current = Expired then clear t
              else begin
                t.due <- t.deps.now () +. period;
                arm t
              end
            | Ok () -> clear t
            | Error failure ->
              if eligibility t current = Expired then
                clear t
              else
                match failure with
                | Refused reason ->
                  if retained current then begin
                    t.item <- Some { current with delivery = Rejected };
                    t.due <- t.deps.now () +. period;
                    arm t
                  end else clear t;
                  t.deps.warn reason
                | Wait reason ->
                  t.item <- Some { current with attempts = 3 };
                  t.due <- t.deps.now () +. period;
                  t.deps.warn reason;
                  arm t
                | Retry reason ->
                  t.deps.warn reason;
                  arm t
          end
        | _ -> run t
      end;
      Lwt.return_unit)

let put ?retry t ~hash tx =
  ignore (pending t);
  begin
    match t.item with
    | Some item when item.hash = hash -> ()
    | Some item when retained item -> ()
    | _ ->
      cancel_timer t;
      t.generation <- t.generation + 1;
      t.item <- Some {
        hash; tx; attempts = 0; retry; delivery = Pending; generation = t.generation;
      };
      t.due <- t.deps.now ()
  end;
  run t

let tick t = run t

let stop t =
  t.open_ <- false;
  clear t