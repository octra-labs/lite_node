(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val event_json :
  Contract_vm.event_record ->
  Yojson.Safe.t

val call_arg_json :
  Contract_vm.v ->
  Yojson.Safe.t

val nested_call_arg_json :
  Contract_vm.v ->
  Yojson.Safe.t

val result_json :
  Contract_vm.v ->
  Yojson.Safe.t

val return_json :
  Contract_vm.v option ->
  Yojson.Safe.t

val view_error :
  string option ->
  string

type view_call_outcome =
  | View_call_ok of Yojson.Safe.t
  | View_call_failed of string

val view_call_outcome :
  Contract.exec_result ->
  view_call_outcome

val events_json :
  Contract_vm.event_record list ->
  Yojson.Safe.t

val program_events_json :
  Contract_vm.event_record list ->
  Yojson.Safe.t

val revert_reason :
  Contract_vm.event_record list ->
  string option

val constructor_revert_message :
  Contract_vm.event_record list ->
  string option ->
  string

val execution_revert_message :
  Contract_vm.event_record list ->
  string option ->
  string

type call_outcome =
  | Call_ok
  | Call_failed of string

type direct_call_domain =
  | Circle_call
  | Program_call

type direct_call_meta = {
  scope : string;
  target_key : string;
  failure_type : string;
  exception_type : string;
}

type direct_call_decision =
  | Direct_call_ok of direct_call_meta
  | Direct_call_failed of {
      meta : direct_call_meta;
      error : string;
    }

val call_outcome :
  success:bool ->
  events:Contract_vm.event_record list ->
  error:string option ->
  call_outcome

val receipt_outcome :
  Contract.exec_result ->
  call_outcome

val direct_call_meta :
  direct_call_domain ->
  direct_call_meta

val direct_call_decision :
  direct_call_domain ->
  Contract.exec_result ->
  direct_call_decision

val receipt_events_json :
  ?program:bool ->
  Contract.exec_result ->
  Yojson.Safe.t

type receipt_save = {
  success : bool;
  effort_used : int;
  events_json : Yojson.Safe.t;
  error : string option;
}

val receipt_save :
  ?program:bool ->
  Contract.exec_result ->
  receipt_save

val direct_receipt_json :
  ?program:bool ->
  epoch:int ->
  now:float ->
  target:string ->
  method_name:string ->
  Contract.exec_result ->
  string

val multi_exec_call_report :
  index:int ->
  program:string ->
  method_name:string ->
  amount:Z.t ->
  success:bool ->
  effort:int ->
  error:string option ->
  return_value:Contract_vm.v option ->
  Yojson.Safe.t

type multi_exec_trace = {
  effort : int;
  calls : Yojson.Safe.t list;
  events : Contract_vm.event_record list;
}

val empty_multi_exec_trace :
  multi_exec_trace

val add_multi_exec_trace :
  multi_exec_trace ->
  index:int ->
  program:string ->
  method_name:string ->
  amount:Z.t ->
  success:bool ->
  effort:int ->
  error:string option ->
  return_value:Contract_vm.v option ->
  events:Contract_vm.event_record list ->
  multi_exec_trace

val multi_exec_step_error :
  index:int ->
  success:bool ->
  events:Contract_vm.event_record list ->
  error:string option ->
  string option

val multi_exec_receipt_json :
  epoch:int ->
  now:float ->
  effort:int ->
  calls:Yojson.Safe.t list ->
  events:Contract_vm.event_record list ->
  success:bool ->
  error:string option ->
  string