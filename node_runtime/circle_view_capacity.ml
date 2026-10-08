(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  limit : int;
  mutable active : int;
}

let create ~limit =
  if limit <= 0 then invalid_arg "circle view capacity must be positive";
  { limit; active = 0 }

let active t =
  t.active

let with_slot ?timeout ?(stop = Fun.id) t ~busy run =
  if t.active >= t.limit then busy ()
  else begin
    t.active <- t.active + 1;
    let work =
      Lwt.finalize
        run
        (fun () ->
           t.active <- t.active - 1;
           Lwt.return_unit)
    in
    let response, wake = Lwt.task () in
    let finished = ref false in
    let timer = ref None in
    let deliver result =
      if Lwt.is_sleeping response then
        match result with
        | Ok value -> Lwt.wakeup_later wake value
        | Error error -> Lwt.wakeup_later_exn wake error in
    let complete result =
      if not !finished then begin
        finished := true;
        Option.iter Lwt.cancel !timer;
        deliver result
      end in
    let stopped = ref false in
    let stop () =
      if not !stopped then begin
        stopped := true;
        stop ();
        Lwt.cancel work
      end in
    Lwt.on_any work (fun value -> complete (Ok value)) (fun error -> complete (Error error));
    Lwt.on_cancel response (fun () ->
      finished := true;
      Option.iter Lwt.cancel !timer;
      stop ());
    begin match timeout with
    | Some (seconds, expired) when not !finished ->
      let clock = Lwt_unix.sleep seconds in
      timer := Some clock;
      Lwt.on_success clock (fun () ->
        if not !finished then begin
          finished := true;
          stop ();
          Lwt.on_any (Lwt.apply expired ())
            (fun value -> deliver (Ok value)) (fun error -> deliver (Error error))
        end)
    | _ -> ()
    end;
    response
  end