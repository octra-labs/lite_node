(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Store_irmin = Octra_core.Store_irmin

type deps = {
  trace : string -> unit;
  log_head : string -> unit;
  log_gc : int -> unit;
  save_state_root : unit -> unit Lwt.t;
  get_head_hash : unit -> string option Lwt.t;
  cleanup_old_tags : int -> unit Lwt.t;
}

type ctx = {
  current_epoch : int;
}

let should_cleanup_old_tags epoch =
  epoch > 0 && epoch mod 100 = 0

let run deps ctx =
  let open Lwt.Syntax in
  deps.trace "step:save_state_root";
  let* () = deps.save_state_root () in
  deps.trace "step:get_head_hash";
  let* head = deps.get_head_hash () in
  Option.iter deps.log_head head;
  if should_cleanup_old_tags ctx.current_epoch then begin
    deps.log_gc ctx.current_epoch;
    deps.cleanup_old_tags ctx.current_epoch
  end else
    Lwt.return_unit

let node_deps store =
  {
    trace = (fun event -> Log.trace "epoch" "%s" event);
    log_head = (fun root -> Log.trace "integrity" "post_epoch_state_root = %s" root);
    log_gc = (fun epoch -> Log.info "gc" "cleanup_old_tags epoch = %d" epoch);
    save_state_root = (fun () -> Store_irmin.save_state_root store);
    get_head_hash = (fun () -> Store_irmin.get_head_hash store);
    cleanup_old_tags = Store_irmin.cleanup_old_tags store;
  }

let run_node ~store ~current_epoch =
  run (node_deps store) { current_epoch }