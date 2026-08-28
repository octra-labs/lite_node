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
  collect_pack : int -> unit Lwt.t;
}

type ctx = {
  current_epoch : int;
}

let should_cleanup_old_tags epoch =
  epoch > 0 && epoch mod 100 = 0

let should_collect_pack epoch =
  should_cleanup_old_tags epoch

let run deps ctx =
  let open Lwt.Syntax in
  deps.trace "step:save_state_root";
  let* () = deps.save_state_root () in
  deps.trace "step:get_head_hash";
  let* head = deps.get_head_hash () in
  Option.iter deps.log_head head;
  let* () =
    if should_collect_pack ctx.current_epoch then
      deps.collect_pack ctx.current_epoch
    else
      Lwt.return_unit
  in
  let* () =
    if should_cleanup_old_tags ctx.current_epoch then begin
      deps.log_gc ctx.current_epoch;
      deps.cleanup_old_tags ctx.current_epoch
    end else
      Lwt.return_unit
  in
  Lwt.return_unit

let collect_pack store epoch =
  let open Lwt.Syntax in
  let* result = Store_irmin.collect_pack store epoch in
  match result with
  | Store_irmin.Gc_started { floor; removed } ->
    Log.info "gc"
      "event = pack_gc status = started epoch = %d floor = %d tags_removed = %d"
      epoch floor removed;
    Lwt.return_unit
  | Store_irmin.Gc_split epoch ->
    Log.info "gc" "event = pack_gc status = split epoch = %d" epoch;
    Lwt.return_unit
  | Store_irmin.Gc_wait _ -> Lwt.return_unit
  | Store_irmin.Gc_space { free; need } ->
    Log.warn "gc"
      "event = pack_gc status = delayed epoch = %d free_bytes = %Ld need_bytes = %Ld"
      epoch free need;
    Lwt.return_unit
  | Store_irmin.Gc_busy ->
    Log.info "gc" "event = pack_gc status = busy epoch = %d" epoch;
    Lwt.return_unit
  | Store_irmin.Gc_off ->
    Log.warn "gc" "event = pack_gc status = unavailable epoch = %d" epoch;
    Lwt.return_unit
  | Store_irmin.Gc_missing floor ->
    Log.warn "gc"
      "event = pack_gc status = skipped epoch = %d floor = %d reason = epoch_tag_missing"
      epoch floor;
    Lwt.return_unit
  | Store_irmin.Gc_error reason ->
    Log.error "gc"
      "event = pack_gc status = failed epoch = %d reason = %s"
      epoch reason;
    Lwt.return_unit

let node_deps store =
  {
    trace = (fun event -> Log.trace "epoch" "%s" event);
    log_head = (fun root -> Log.trace "integrity" "post_epoch_state_root = %s" root);
    log_gc = (fun epoch -> Log.info "gc" "cleanup_old_tags epoch = %d" epoch);
    save_state_root = (fun () -> Store_irmin.save_state_root store);
    get_head_hash = (fun () -> Store_irmin.get_head_hash store);
    cleanup_old_tags = Store_irmin.cleanup_old_tags store;
    collect_pack = collect_pack store;
  }

let run_node ~store ~current_epoch =
  run (node_deps store) { current_epoch }