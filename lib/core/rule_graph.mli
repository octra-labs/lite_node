(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type mode = Prior | Active

type root_read = Missing | Root of string | Unreadable of string

type fault =
  | Anchor_missing of int
  | Anchor_mismatch of {
      epoch : int;
      expected : string;
      actual : string;
    }
  | Anchor_unreadable of {
      epoch : int;
      reason : string;
    }

type activation = {
  anchor_epoch : int;
  anchor_state_root : string;
  activation_epoch : int;
}

type t

val create :
  chain_id:string ->
  root_at:(int -> root_read) ->
  t

val circle_activation : t -> activation option

val root_after_floor :
  chain_id:string ->
  floor_epoch:int ->
  epoch:int ->
  string option

val circle :
  t ->
  epoch:int ->
  (mode, fault) result

val fault_message : fault -> string