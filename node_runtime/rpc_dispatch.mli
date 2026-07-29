(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type 'handler route = string * 'handler

type 'handler circle_handlers = {
  circle_info : 'handler;
  circle_info_auth : 'handler;
  circle_program_info : 'handler;
  circle_program_info_auth : 'handler;
  circle_asset : 'handler;
  circle_view : 'handler;
  circle_view_auth : 'handler;
  circle_slot_policy : 'handler;
  circle_slot_policy_auth : 'handler;
  circle_state_policy : 'handler;
  circle_state_policy_auth : 'handler;
  circle_state_descriptor : 'handler;
  circle_state_descriptor_auth : 'handler;
  circle_balance_cell : 'handler;
  circle_balance_cell_auth : 'handler;
  circle_register_cell : 'handler;
  circle_register_cell_auth : 'handler;
  circle_balance_binding : 'handler;
  circle_balance_binding_auth : 'handler;
  circle_register_binding : 'handler;
  circle_register_binding_auth : 'handler;
  circle_balance_workflow : 'handler;
  circle_balance_workflow_auth : 'handler;
  circle_register_workflow : 'handler;
  circle_register_workflow_auth : 'handler;
  circle_object_summary : 'handler;
  circle_object_summary_auth : 'handler;
  circle_object_members : 'handler;
  circle_object_members_auth : 'handler;
  circle_object_detail : 'handler;
  circle_object_detail_auth : 'handler;
  circle_object_member : 'handler;
  circle_object_member_auth : 'handler;
  circle_object_refs : 'handler;
  circle_object_refs_auth : 'handler;
  circle_object_list : 'handler;
  circle_object_list_auth : 'handler;
  circle_transport_policy : 'handler;
  circle_transport_policy_auth : 'handler;
  circle_hfhe_policy : 'handler;
  circle_hfhe_policy_auth : 'handler;
  circle_key_policy : 'handler;
  circle_key_policy_auth : 'handler;
  circle_storage : 'handler;
  circle_storage_auth : 'handler;
  circle_storage_dump : 'handler;
  circle_storage_dump_auth : 'handler;
  circle_outbox_claim : 'handler;
  circle_outbox_claim_auth : 'handler;
  circle_outbox_intent : 'handler;
  circle_outbox_intent_auth : 'handler;
  circle_outbox_status : 'handler;
  circle_outbox_status_auth : 'handler;
  circle_ingress_packet : 'handler;
  circle_ingress_packet_auth : 'handler;
  circle_asset_ciphertext : 'handler;
  circle_asset_ciphertext_by_resource_key : 'handler;
  circle_asset_ciphertext_by_slot_ref : 'handler;
  circle_asset_ciphertext_by_state_ref : 'handler;
  circle_asset_ciphertext_auth : 'handler;
  circle_asset_ciphertext_by_resource_key_auth : 'handler;
  circle_asset_ciphertext_by_slot_ref_auth : 'handler;
  circle_asset_ciphertext_by_state_ref_auth : 'handler;
  circle_program : 'handler;
  circle_program_auth : 'handler;
}

type 'handler program_handlers = {
  program_info : 'handler;
  program_receipt : 'handler;
  program_call : 'handler;
  program_compute_address : 'handler;
  program_list : 'handler;
  program_storage : 'handler;
  program_storage_dump : 'handler;
  program_abi : 'handler;
  program_verify : 'handler;
  program_save_abi : 'handler;
  program_source : 'handler;
  program_bytecode : 'handler;
  program_compile_assembly : 'handler;
  program_compile_aml : 'handler;
  program_compile_aml_multi : 'handler;
  program_tokens_by_address : 'handler;
}

type 'handler node_route_groups = {
  status_core : 'handler route list;
  account_public : 'handler route list;
  submission : 'handler route list;
  history : 'handler route list;
  rest : 'handler route list;
  staging : 'handler route list;
  circle : 'handler route list;
  program : 'handler route list;
  mutation : 'handler route list;
  account_pvac : 'handler route list;
  status_proof : 'handler route list;
}

val route :
  string ->
  'handler ->
  'handler route list

val route_aliases :
  string ->
  string ->
  'handler ->
  'handler route list

val circle_routes :
  'handler circle_handlers ->
  'handler route list

val program_routes :
  'handler program_handlers ->
  'handler route list

val node_routes :
  'handler node_route_groups ->
  'handler route list

val find :
  string ->
  'handler route list ->
  'handler option

val handle_request :
  Rpc_http.meta ->
  Octra_core.Rpc.request ->
  'ctx ->
  (Yojson.Safe.t ->
   'ctx ->
   (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result Lwt.t) route list ->
  Octra_core.Rpc.response Lwt.t

val process_body :
  Rpc_http.meta ->
  string ->
  'ctx ->
  (Yojson.Safe.t ->
   'ctx ->
   (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result Lwt.t) route list ->
  Yojson.Safe.t Lwt.t