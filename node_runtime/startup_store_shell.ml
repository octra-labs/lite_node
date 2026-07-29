(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type integrity_plan =
  | Fresh_store
  | Integrity_ok of Octra_core.Store_irmin.integrity_result
  | Recover_state_root of Octra_core.Store_irmin.integrity_result
  | Integrity_fatal of Octra_core.Store_irmin.integrity_result

type integrity_deps = {
  is_fresh_store : unit -> bool;
  verify_integrity : unit -> Octra_core.Store_irmin.integrity_result;
  save_state_root : unit -> unit;
  exit_fatal : unit -> unit;
}

type epoch_tag_plan =
  | Tags_present of {
      count : int;
      oldest : int;
      newest : int;
    }
  | Create_initial_tag of int
  | Skip_initial_tag

type epoch_tag_deps = {
  list_epoch_tags : unit -> int list;
  last_epoch : unit -> string option;
  tag_epoch : int -> unit;
}

let state_root_mismatch_error error =
  String.length error >= 16 && String.sub error 0 16 = "STATE ROOT MISMA"

let recoverable_state_root_mismatch r =
  List.for_all state_root_mismatch_error r.Octra_core.Store_irmin.errors
  && r.accounts_ok = r.accounts_sampled

let integrity_plan ~is_fresh_store result =
  match is_fresh_store, result with
  | true, _ -> Fresh_store
  | false, Some r when r.Octra_core.Store_irmin.ok -> Integrity_ok r
  | false, Some r when recoverable_state_root_mismatch r ->
    Recover_state_root r
  | false, Some r -> Integrity_fatal r
  | false, None ->
    invalid_arg "startup integrity result is required for non-fresh store"

let log_integrity_ok r =
  Octra_log.info "init" "integrity OK state_root = %s accounts = %d/%d"
    r.Octra_core.Store_irmin.head_hash
    r.accounts_ok
    r.accounts_sampled

let log_integrity_fatal r =
  Octra_log.fatal "init"
    "INTEGRITY CHECK FAILED state_root = %s accounts = %d/%d"
    r.Octra_core.Store_irmin.head_hash
    r.accounts_ok
    r.accounts_sampled;
  List.iter
    (fun e -> Octra_log.fatal "init" "  error: %s" e)
    r.errors;
  Octra_log.fatal "init" "REFUSING TO START - data store may be corrupted"

let run_integrity deps =
  let is_fresh_store = deps.is_fresh_store () in
  match integrity_plan
          ~is_fresh_store
          (if is_fresh_store then None else Some (deps.verify_integrity ())) with
  | Fresh_store ->
    Octra_log.info "init" "fresh store - skipping integrity check"
  | Integrity_ok r ->
    log_integrity_ok r
  | Recover_state_root r ->
    Octra_log.warn "init"
      "state_root mismatch likely_unclean_shutdown = true accounts = %d/%d"
      r.Octra_core.Store_irmin.accounts_ok
      r.accounts_sampled;
    deps.save_state_root ();
    Octra_log.info "init" "state_root updated = %s" r.head_hash
  | Integrity_fatal r ->
    log_integrity_fatal r;
    deps.exit_fatal ()

let epoch_tag_plan ~existing_tags ~last_epoch =
  match existing_tags, last_epoch with
  | [], Some s -> Create_initial_tag (int_of_string s)
  | [], None -> Skip_initial_tag
  | tags, _ ->
    Tags_present {
      count = List.length tags;
      oldest = List.hd tags;
      newest = List.nth tags (List.length tags - 1);
    }

let run_epoch_tags deps =
  match epoch_tag_plan
          ~existing_tags:(deps.list_epoch_tags ())
          ~last_epoch:(deps.last_epoch ()) with
  | Create_initial_tag epoch ->
    deps.tag_epoch epoch;
    Octra_log.info "init"
      "created initial epoch tag = epoch_%d reason = migration_checkpoint"
      epoch
  | Skip_initial_tag ->
    Octra_log.warn "init"
      "no epoch tags and no last_epoch meta - skipping initial tag"
  | Tags_present { count; oldest; newest } ->
    Octra_log.info "init" "epoch tags = %d oldest = %d newest = %d"
      count oldest newest

let irmin_path data_dir =
  data_dir ^ "/irmin_store"