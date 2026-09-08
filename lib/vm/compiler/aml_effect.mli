(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type target =
  | State of string
  | Event of string
  | Fault of string

type bind = private {
  atom : C_eff.atom;
  target : target;
}

type rollback =
  | Preserve
  | Journal
  | Record
  | Abort

type host =
  | Load of string * Contract_vm.v
  | Store of string * Contract_vm.v
  | Log of string * Contract_vm.v list
  | Reject of string * Contract_vm.v list

type item = private {
  action : C_eval.action;
  host : host;
  rollback : rollback;
  ops : Contract_vm.instr list;
  cost : Z.t;
}

type entry =
  | State_entry of string * Oct_lang.typ
  | Event_entry of string * Oct_lang.typ list
  | Fault_entry of string * int * string

type t
type sealed

type charge =
  | Exact
  | Storage

type block = private {
  index : int;
  site : C_check.site;
  rollback : rollback;
  ops : Contract_vm.instr list;
  cost : Z.t;
  charge : charge;
}

type plan = private {
  items : item list;
  cost : Z.t;
}

type prepared = private {
  out : C_eval.out;
  plan : plan;
}

type error =
  | Program
  | Static of C_check.error
  | Run of C_eval.error
  | Input_count of int * int
  | Bind_limit of int
  | Atom_invalid of C_eff.atom
  | Atom_repeated of C_eff.atom
  | Target_kind of C_eff.atom * target
  | Target_absent of target
  | Target_repeated of target
  | Target_type of target * Oct_lang.typ
  | Binding_absent of C_eff.atom
  | Site_type of C_eff.atom * C_type.t * C_type.t
  | Site_payload of C_eff.atom * C_type.t
  | Site_origin of C_eff.atom * C_check.origin
  | Site_absent of C_eff.atom
  | Site_limit of C_eff.atom
  | Trace
  | Site_index of int
  | Register of int
  | Payload_regs of C_eff.atom * int * int
  | Scratch_regs of C_eff.atom * int * int
  | Register_alias of C_eff.atom * int
  | Payload of C_eff.atom
  | Origin of C_eff.atom * C_eval.origin
  | Host_limit of C_eff.atom * int
  | After_fail of C_eff.atom
  | Close_unavailable of C_nat.t * C_nat.t

val bind : C_eff.atom -> target -> bind
val make : Oct_lang.program -> bind list -> (t, error) result
val seal : t -> C_term.bind list -> C_term.t -> (sealed, error) result
val entries : t -> (C_eff.atom * entry) list
val registry : sealed -> t
val prog : sealed -> C_low.prog
val typ : sealed -> C_type.t
val flow : sealed -> C_check.flow
val locations : sealed -> C_check.location array
val lower :
  sealed -> index:int -> payload:int list -> scratch:int list ->
  (block, error) result
val trace : sealed -> C_eval.action list -> bool
val prepare : sealed -> C_eval.value list -> (prepared, error) result
val text : error -> string