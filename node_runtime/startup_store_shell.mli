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

val integrity_plan :
  head_state:head_state ->
  store_root:string option ->
  epoch_root:string option ->
  integrity_plan

val epoch_tag_plan :
  existing_tags:int list ->
  last_epoch:string option ->
  epoch_tag_plan

val run_integrity :
  integrity_deps ->
  unit

val run_epoch_tags :
  epoch_tag_deps ->
  unit

val irmin_path :
  string ->
  string