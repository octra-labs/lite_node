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
    let response =
      match timeout with
      | None -> Lwt.protected work
      | Some (seconds, expired) ->
        let timer =
          let open Lwt.Syntax in
          let* () = Lwt_unix.sleep seconds in
          stop ();
          expired ()
        in
        Lwt.pick [Lwt.protected work; timer]
    in
    Lwt.on_cancel response stop;
    response
  end