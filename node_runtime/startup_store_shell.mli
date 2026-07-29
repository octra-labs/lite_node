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

val integrity_plan :
  is_fresh_store:bool ->
  Octra_core.Store_irmin.integrity_result option ->
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