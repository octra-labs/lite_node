(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type deps = {
  trace : string -> unit;
  log_head : string -> unit;
  log_gc : int -> unit;
  save_state_root : unit -> unit Lwt.t;
  get_head_hash : unit -> string option Lwt.t;
  cleanup_old_tags : int -> unit Lwt.t;
  collect_pack : int -> unit Lwt.t;
}

type ctx = {
  current_epoch : int;
}

val should_cleanup_old_tags : int -> bool

val should_collect_pack : int -> bool

val run : deps -> ctx -> unit Lwt.t

val run_node :
  store:Octra_core.Store_irmin.t ->
  current_epoch:int ->
  unit Lwt.t