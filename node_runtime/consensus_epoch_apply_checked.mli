(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type apply_result =
  | Apply_done
  | Apply_busy

type deps = {
  head : unit -> int;
  set_current_epoch : int -> unit;
  catchup_active : unit -> bool;
  queue_gap :
    active:bool ->
    target_epoch:int64 ->
    reason:string ->
    Consensus_catchup_shell.queue_event;
  clear_state_attested : unit -> unit;
  log_already : current_epoch:int -> head:int -> unit;
  log_defer :
    Consensus_epoch_apply_admission.catchup ->
    Consensus_catchup_shell.queue_event ->
    unit;
  preflight : unit -> (unit, string) result;
  defer : string -> unit;
  apply : unit -> apply_result Lwt.t;
  retry : unit -> unit Lwt.t;
}

val run :
  deps ->
  consensus_mode:bool ->
  current_epoch:int ->
  unit Lwt.t

val run_node :
  consensus_mode:bool ->
  current_epoch:int ref ->
  head:(unit -> int) ->
  catchup_queue:Consensus_catchup_queue.t ->
  catchup_in_progress:bool ref ->
  clear_state_attested:(unit -> unit) ->
  preflight:(unit -> (unit, string) result) ->
  defer:(string -> unit) ->
  apply:(unit -> apply_result Lwt.t) ->
  retry:(unit -> unit Lwt.t) ->
  unit Lwt.t