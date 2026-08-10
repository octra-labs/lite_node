(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type integrity_plan =
  | Fresh_store
  | Verify_store
  | Restore_epoch of {
      epoch : int;
      root : string;
    }
  | Integrity_fatal of string

type head_state =
  | Head_missing
  | Head_corrupt of string
  | Head_ready of {
      epoch : int;
      root : string;
    }

type integrity_deps = {
  head_state : unit -> head_state;
  store_root : unit -> string option;
  epoch_root : int -> string option;
  rollback_epoch : int -> (unit, string) result;
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

let integrity_plan ~head_state ~store_root ~epoch_root =
  match head_state, store_root with
  | Head_missing, None -> Fresh_store
  | Head_missing, Some root ->
    Integrity_fatal
      ("HEAD.json is missing for an existing store root " ^ root)
  | Head_corrupt reason, _ ->
    Integrity_fatal ("HEAD.json is unreadable: " ^ reason)
  | Head_ready { epoch; _ }, None ->
    Integrity_fatal
      (Printf.sprintf "Irmin HEAD is missing for finalized epoch %d" epoch)
  | Head_ready { root; _ }, Some store_root
    when String.equal root store_root -> Verify_store
  | Head_ready { epoch; root }, Some store_root ->
    begin
      match epoch_root with
      | Some tagged_root when String.equal tagged_root root ->
        Restore_epoch { epoch; root }
      | Some tagged_root ->
        Integrity_fatal
          (Printf.sprintf
             "store root %s differs from finalized root %s and epoch %d tag has root %s"
             store_root
             root
             epoch
             tagged_root)
      | None ->
        Integrity_fatal
          (Printf.sprintf
             "store root %s differs from finalized root %s and epoch %d tag is unavailable"
             store_root
             root
             epoch)
    end

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

let verify_store deps =
  let result = deps.verify_integrity () in
  if result.Octra_core.Store_irmin.ok then
    log_integrity_ok result
  else if recoverable_state_root_mismatch result then begin
    deps.save_state_root ();
    let verified = deps.verify_integrity () in
    if verified.Octra_core.Store_irmin.ok then
      log_integrity_ok verified
    else begin
      log_integrity_fatal verified;
      deps.exit_fatal ()
    end
  end else begin
    log_integrity_fatal result;
    deps.exit_fatal ()
  end

let run_integrity deps =
  let head_state = deps.head_state () in
  let store_root = deps.store_root () in
  let epoch_root =
    match head_state with
    | Head_ready { epoch; _ } -> deps.epoch_root epoch
    | Head_missing
    | Head_corrupt _ -> None
  in
  match integrity_plan ~head_state ~store_root ~epoch_root with
  | Fresh_store ->
    Octra_log.info "init" "fresh store - skipping integrity check"
  | Verify_store -> verify_store deps
  | Restore_epoch { epoch; root } ->
    begin
      match deps.rollback_epoch epoch with
      | Error reason ->
        Octra_log.fatal "init"
          "event = store_restore status = rejected epoch = %d reason = %s"
          epoch
          reason;
        deps.exit_fatal ()
      | Ok () ->
        begin
          match deps.store_root () with
          | Some restored when String.equal restored root ->
            deps.save_state_root ();
            Octra_log.warn "init"
              "event = store_restore status = applied epoch = %d root = %s"
              epoch
              root;
            verify_store deps
          | Some restored ->
            Octra_log.fatal "init"
              "event = store_restore status = rejected epoch = %d expected = %s actual = %s"
              epoch
              root
              restored;
            deps.exit_fatal ()
          | None ->
            Octra_log.fatal "init"
              "event = store_restore status = rejected epoch = %d reason = missing_head"
              epoch;
            deps.exit_fatal ()
        end
    end
  | Integrity_fatal reason ->
    Octra_log.fatal "init" "event = store_integrity status = rejected reason = %s"
      reason;
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