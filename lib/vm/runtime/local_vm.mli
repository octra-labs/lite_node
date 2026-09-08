(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type config

type stop =
  | Returned
  | Reverted
  | Step_cap
  | Host_operation of int * string

type frame = {
  index : int;
  pc : int;
  next_pc : int;
  effort_before : int;
  effort_after : int;
  op : Contract_vm.instr;
  result : Contract_vm.v;
}

type outcome = {
  stop : stop;
  result : Contract_vm.v;
  regs : Contract_vm.v array;
  effort : int;
  steps : int;
  storage : (string * string) list;
  events : Contract_vm.event_record list;
  closes : Contract_vm.cap list;
  frames : frame list;
}

type error =
  | Dispatcher_absent
  | Duplicate_storage of string
  | Invalid_step_cap
  | Program_counter of int
  | Grant_count of int * int
  | Grant_repeat
  | Grant of Z.t * Z.t * Z.t
  | Session of C_sess.error

val config :
  ?storage:(string * string) list ->
  ?storage_kinds:(string * Contract_vm.storage_kind) list ->
  ?strict_values:bool ->
  ?caller:string ->
  ?origin:string ->
  ?address:string ->
  ?value:Z.t ->
  ?limit:int ->
  ?step_cap:int ->
  ?epoch:int ->
  ?epoch_time:int64 ->
  ?tree_hash:string ->
  ?node_id:string ->
  ?tx_hash:string ->
  ?view:bool ->
  ?byte_result:Contract_vm.byte_result ->
  ?grants:Contract_vm.cap list ->
  method_name:string ->
  args:Contract_vm.v list ->
  unit ->
  config

val run :
  trace:bool ->
  config ->
  Contract_vm.instr array ->
  (outcome, error) result

val run_at :
  trace:bool ->
  config ->
  entry:int ->
  Contract_vm.instr array ->
  (outcome, error) result

val grants : C_sess.state -> C_sess.token list -> Contract_vm.v list option
val grant : C_sess.state -> C_sess.token -> Contract_vm.v option
val settle :
  C_sess.state ->
  C_sess.token list ->
  outcome ->
  (C_sess.state * C_sess.token list, error) result

val value_text : Contract_vm.v -> string
val stop_text : stop -> string
val error_text : error -> string