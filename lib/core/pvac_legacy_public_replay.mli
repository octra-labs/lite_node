(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type effect =
  | Neutral
  | Public_encrypt of Z.t
  | Public_decrypt of Z.t
  | Hidden_commitment of string * int * string
  | Hidden_flow of string
  | Poisoned_flow of string

type audit_class =
  | Public_clean
  | Hidden_witness
  | Poisoned

type decision = {
  audit_class : audit_class;
  can_public_migrate : bool;
  public_net : Z.t option;
  commitment_net : string option;
  blockers : string list;
  effects : effect list;
  reason : string;
}

val classify_tx : addr:string -> Yojson.Safe.t -> effect

val string_of_audit_class : audit_class -> string

val replay_history : addr:string -> Yojson.Safe.t list -> decision