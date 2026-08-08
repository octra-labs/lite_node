(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type prepare = {
  model_epoch : int64;
  model_state_root : string;
  circle_id : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
}

type execute = {
  request_id : string;
  session_id : string;
  epoch_id : int64;
  state_root : string;
  model_epoch : int64;
  model_state_root : string;
  circle_id : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
  caller : string;
  method_name : string;
  params_json : string;
  program_b64 : string;
  program_runtime : string;
  storage : (string * string) list;
  max_output_bytes : int;
}

type self_test = {
  executor_root : string;
  evidence_root : string;
  profile : string;
}

type prepared = {
  cache_key : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
  entries : int;
  bytes : int64;
}

type executed = {
  output_json : string;
  output_hash : string;
  trace_root : string;
  steps : int64;
  operations : int64;
  effort_used : int;
}

val max_http_body_bytes : int
val prepare_to_json : prepare -> Yojson.Safe.t
val prepare_of_json : Yojson.Safe.t -> (prepare, string) result
val execute_to_json : execute -> Yojson.Safe.t
val execute_of_json : Yojson.Safe.t -> (execute, string) result
val self_test_to_json : self_test -> Yojson.Safe.t
val self_test_of_json : Yojson.Safe.t -> (self_test, string) result
val prepared_to_json : prepared -> Yojson.Safe.t
val prepared_of_json : Yojson.Safe.t -> (prepared, string) result
val executed_to_json : executed -> Yojson.Safe.t
val executed_of_json : Yojson.Safe.t -> (executed, string) result