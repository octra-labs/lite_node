(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


module Rpc = Octra_core.Rpc

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

let route name handler =
  [name, handler]

let route_aliases primary alias handler =
  [primary, handler; alias, handler]

let circle_routes h =
  List.concat [
    route_aliases "circle_info" "octra_circleInfo" h.circle_info;
    route_aliases "circle_info_auth" "octra_circleInfoAuth" h.circle_info_auth;
    route_aliases "circle_program_info" "octra_circleProgramInfo" h.circle_program_info;
    route_aliases "circle_program_info_auth" "octra_circleProgramInfoAuth" h.circle_program_info_auth;
    route_aliases "circle_asset" "octra_circleAsset" h.circle_asset;
    route_aliases "circle_view" "octra_circleView" h.circle_view;
    route_aliases "circle_view_auth" "octra_circleViewAuth" h.circle_view_auth;
    route_aliases "circle_slot_policy" "octra_circleSlotPolicy" h.circle_slot_policy;
    route_aliases "circle_slot_policy_auth" "octra_circleSlotPolicyAuth" h.circle_slot_policy_auth;
    route_aliases "circle_state_policy" "octra_circleStatePolicy" h.circle_state_policy;
    route_aliases "circle_state_policy_auth" "octra_circleStatePolicyAuth" h.circle_state_policy_auth;
    route_aliases "circle_state_descriptor" "octra_circleStateDescriptor" h.circle_state_descriptor;
    route_aliases "circle_state_descriptor_auth" "octra_circleStateDescriptorAuth" h.circle_state_descriptor_auth;
    route_aliases "circle_balance_cell" "octra_circleBalanceCell" h.circle_balance_cell;
    route_aliases "circle_balance_cell_auth" "octra_circleBalanceCellAuth" h.circle_balance_cell_auth;
    route_aliases "circle_register_cell" "octra_circleRegisterCell" h.circle_register_cell;
    route_aliases "circle_register_cell_auth" "octra_circleRegisterCellAuth" h.circle_register_cell_auth;
    route_aliases "circle_balance_binding" "octra_circleBalanceBinding" h.circle_balance_binding;
    route_aliases "circle_balance_binding_auth" "octra_circleBalanceBindingAuth" h.circle_balance_binding_auth;
    route_aliases "circle_register_binding" "octra_circleRegisterBinding" h.circle_register_binding;
    route_aliases "circle_register_binding_auth" "octra_circleRegisterBindingAuth" h.circle_register_binding_auth;
    route_aliases "circle_balance_workflow" "octra_circleBalanceWorkflow" h.circle_balance_workflow;
    route_aliases "circle_balance_workflow_auth" "octra_circleBalanceWorkflowAuth" h.circle_balance_workflow_auth;
    route_aliases "circle_register_workflow" "octra_circleRegisterWorkflow" h.circle_register_workflow;
    route_aliases "circle_register_workflow_auth" "octra_circleRegisterWorkflowAuth" h.circle_register_workflow_auth;
    route_aliases "circle_object_summary" "octra_circleObjectSummary" h.circle_object_summary;
    route_aliases "circle_object_summary_auth" "octra_circleObjectSummaryAuth" h.circle_object_summary_auth;
    route_aliases "circle_object_members" "octra_circleObjectMembers" h.circle_object_members;
    route_aliases "circle_object_members_auth" "octra_circleObjectMembersAuth" h.circle_object_members_auth;
    route_aliases "circle_object_detail" "octra_circleObjectDetail" h.circle_object_detail;
    route_aliases "circle_object_detail_auth" "octra_circleObjectDetailAuth" h.circle_object_detail_auth;
    route_aliases "circle_object_member" "octra_circleObjectMember" h.circle_object_member;
    route_aliases "circle_object_member_auth" "octra_circleObjectMemberAuth" h.circle_object_member_auth;
    route_aliases "circle_object_refs" "octra_circleObjectRefs" h.circle_object_refs;
    route_aliases "circle_object_refs_auth" "octra_circleObjectRefsAuth" h.circle_object_refs_auth;
    route_aliases "circle_object_list" "octra_circleObjectList" h.circle_object_list;
    route_aliases "circle_object_list_auth" "octra_circleObjectListAuth" h.circle_object_list_auth;
    route_aliases "circle_transport_policy" "octra_circleTransportPolicy" h.circle_transport_policy;
    route_aliases "circle_transport_policy_auth" "octra_circleTransportPolicyAuth" h.circle_transport_policy_auth;
    route_aliases "circle_hfhe_policy" "octra_circleHfhePolicy" h.circle_hfhe_policy;
    route_aliases "circle_hfhe_policy_auth" "octra_circleHfhePolicyAuth" h.circle_hfhe_policy_auth;
    route_aliases "circle_key_policy" "octra_circleKeyPolicy" h.circle_key_policy;
    route_aliases "circle_key_policy_auth" "octra_circleKeyPolicyAuth" h.circle_key_policy_auth;
    route_aliases "circle_storage" "octra_circleStorage" h.circle_storage;
    route_aliases "circle_storage_auth" "octra_circleStorageAuth" h.circle_storage_auth;
    route_aliases "circle_storage_dump" "octra_circleStorageDump" h.circle_storage_dump;
    route_aliases "circle_storage_dump_auth" "octra_circleStorageDumpAuth" h.circle_storage_dump_auth;
    route_aliases "circle_outbox_claim" "octra_circleOutboxClaim" h.circle_outbox_claim;
    route_aliases "circle_outbox_claim_auth" "octra_circleOutboxClaimAuth" h.circle_outbox_claim_auth;
    route_aliases "circle_outbox_intent" "octra_circleOutboxIntent" h.circle_outbox_intent;
    route_aliases "circle_outbox_intent_auth" "octra_circleOutboxIntentAuth" h.circle_outbox_intent_auth;
    route_aliases "circle_outbox_status" "octra_circleOutboxStatus" h.circle_outbox_status;
    route_aliases "circle_outbox_status_auth" "octra_circleOutboxStatusAuth" h.circle_outbox_status_auth;
    route_aliases "circle_ingress_packet" "octra_circleIngressPacket" h.circle_ingress_packet;
    route_aliases "circle_ingress_packet_auth" "octra_circleIngressPacketAuth" h.circle_ingress_packet_auth;
    route_aliases "circle_asset_ciphertext" "octra_circleAssetCiphertext" h.circle_asset_ciphertext;
    route_aliases "circle_asset_ciphertext_by_resource_key" "octra_circleAssetCiphertextByResourceKey" h.circle_asset_ciphertext_by_resource_key;
    route_aliases "circle_asset_ciphertext_by_slot_ref" "octra_circleAssetCiphertextBySlotRef" h.circle_asset_ciphertext_by_slot_ref;
    route_aliases "circle_asset_ciphertext_by_state_ref" "octra_circleAssetCiphertextByStateRef" h.circle_asset_ciphertext_by_state_ref;
    route_aliases "circle_asset_ciphertext_auth" "octra_circleAssetCiphertextAuth" h.circle_asset_ciphertext_auth;
    route_aliases "circle_asset_ciphertext_by_resource_key_auth" "octra_circleAssetCiphertextByResourceKeyAuth" h.circle_asset_ciphertext_by_resource_key_auth;
    route_aliases "circle_asset_ciphertext_by_slot_ref_auth" "octra_circleAssetCiphertextBySlotRefAuth" h.circle_asset_ciphertext_by_slot_ref_auth;
    route_aliases "circle_asset_ciphertext_by_state_ref_auth" "octra_circleAssetCiphertextByStateRefAuth" h.circle_asset_ciphertext_by_state_ref_auth;
    route "octra_circleProgram" h.circle_program;
  ]

let program_routes h =
  List.concat [
    route_aliases "vm_contract" "octra_programInfo" h.program_info;
    route_aliases "contract_receipt" "octra_programReceipt" h.program_receipt;
    route_aliases "contract_call" "octra_programCall" h.program_call;
    route_aliases "octra_computeContractAddress" "octra_computeProgramAddress" h.program_compute_address;
    route_aliases "octra_listContracts" "octra_listPrograms" h.program_list;
    route_aliases "octra_contractStorage" "octra_programStorage" h.program_storage;
    route_aliases "octra_contractStorageDump" "octra_programStorageDump" h.program_storage_dump;
    route_aliases "octra_contractAbi" "octra_programAbi" h.program_abi;
    route_aliases "contract_verify" "octra_programVerify" h.program_verify;
    route_aliases "contract_saveAbi" "octra_programSaveAbi" h.program_save_abi;
    route_aliases "contract_source" "octra_programSource" h.program_source;
    route_aliases "octra_contractBytecode" "octra_programBytecode" h.program_bytecode;
    route "octra_compileAssembly" h.program_compile_assembly;
    route "octra_compileAml" h.program_compile_aml;
    route "octra_compileAmlMulti" h.program_compile_aml_multi;
    route "octra_tokensByAddress" h.program_tokens_by_address;
  ]

let node_routes h =
  h.status_core
  @ h.account_public
  @ h.submission
  @ h.history
  @ h.rest
  @ h.staging
  @ h.circle
  @ h.program
  @ h.mutation
  @ h.account_pvac
  @ h.status_proof

let find method_name routes =
  List.assoc_opt method_name routes

let env_seconds name fallback =
  match Sys.getenv_opt name with
  | Some s ->
    begin
      try (float_of_int (int_of_string s)) /. 1000.0 with _ -> fallback
    end
  | None ->
    fallback

let handle_request meta req ctx routes =
  match find req.Rpc.method_ routes with
  | None ->
    Lwt.return (Rpc.Error_ (Rpc.method_not_found req.method_, req.id))
  | Some handler ->
    let open Lwt.Syntax in
    let t0 = Unix.gettimeofday () in
    let* result = handler req.params ctx in
    let elapsed = Unix.gettimeofday () -. t0 in
    let slow_warn_s = env_seconds "OCTRA_RPC_SLOW_WARN_MS" 1.0 in
    let info_s = env_seconds "OCTRA_RPC_INFO_MS" 0.5 in
    if elapsed > slow_warn_s then
      Log.warn
        "rpc"
        "SLOW method=%s took=%.2fs from=%s id=%s params=%s body_bytes=%d ua=%s"
        req.method_
        elapsed
        meta.Rpc_http.rpc_peer
        (Rpc_view.json_short 80 req.id)
        (Rpc_view.json_short 240 req.params)
        meta.rpc_body_bytes
        meta.rpc_user_agent
    else if elapsed > info_s then
      Log.info
        "rpc"
        "method=%s took=%.0fms from=%s params=%s"
        req.method_
        (elapsed *. 1000.0)
        meta.rpc_peer
        (Rpc_view.json_short 160 req.params)
    else
      Log.trace "rpc" "%s %.0fms" req.method_ (elapsed *. 1000.0);
    Lwt.return
      (match result with
       | Ok v -> Rpc.Result (v, req.id)
       | Error rpc_err -> Rpc.Error_ (rpc_err, req.id))

let process_body meta body ctx routes =
  match Rpc.parse_body body with
  | Error e ->
    Lwt.return (Rpc.response_json (Rpc.Error_ (e, `Null)))
  | Ok (`Single req) ->
    let open Lwt.Syntax in
    let* resp = handle_request meta req ctx routes in
    Lwt.return (Rpc.response_json resp)
  | Ok (`Batch reqs) ->
    let open Lwt.Syntax in
    let* results =
      Lwt_list.map_s
        (function
         | Ok req ->
           let* resp = handle_request meta req ctx routes in
           Lwt.return (Rpc.response_json resp)
         | Error e ->
           Lwt.return (Rpc.response_json (Rpc.Error_ (e, `Null))))
        reqs
    in
    Lwt.return (`List results)