(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = { mutable descriptor : Unix.file_descr option }

external lock_exclusive : Unix.file_descr -> unit = "octra_store_lock"

let release owner =
  match owner.descriptor with
  | None -> ()
  | Some descriptor ->
    owner.descriptor <- None;
    Unix.close descriptor

let acquire ?(wait_seconds = 0.) path =
  if not (Float.is_finite wait_seconds) || wait_seconds < 0. then
    invalid_arg "store ownership wait is invalid";
  let descriptor = Unix.openfile path [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
  let owner = {descriptor = Some descriptor} in
  let clock = Mtime_clock.counter () in
  let rec attempt () =
    match lock_exclusive descriptor with
    | () -> ()
    | exception (Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), "flock", _) as error) ->
      let elapsed = Mtime.Span.to_float_ns (Mtime_clock.count clock) /. 1e9 in
      let left = wait_seconds -. elapsed in
      if left <= 0. then raise error;
      Unix.sleepf (min 0.1 left);
      attempt () in
  match
    if (Unix.fstat descriptor).Unix.st_kind <> Unix.S_DIR then
      invalid_arg "store ownership requires a directory";
    attempt ()
  with
  | () -> owner
  | exception error ->
    let trace = Printexc.get_raw_backtrace () in
    (try release owner with _ -> ());
    Printexc.raise_with_backtrace error trace