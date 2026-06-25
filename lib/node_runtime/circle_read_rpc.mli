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


type rpc_result = (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result

type 'handler dispatch_adapters = {
  store_read : (Octra_core.Store_irmin.t -> Yojson.Safe.t -> rpc_result Lwt.t) -> 'handler;
  epoch_read :
    (Octra_core.Store_irmin.t ->
     Yojson.Safe.t ->
     current_epoch:int ->
     rpc_result Lwt.t) ->
    'handler;
  public_asset_read :
    (Octra_core.Store_irmin.t ->
     Yojson.Safe.t ->
     current_epoch:int ->
     reveal_sensitive_fields:bool ->
     rpc_result Lwt.t) ->
    'handler;
  circle_view : 'handler;
  circle_view_auth : 'handler;
}

val query_result :
  ('a, string) result ->
  ('a -> Yojson.Safe.t) ->
  rpc_result

val option_result :
  'a option ->
  not_found:string ->
  ('a -> Yojson.Safe.t) ->
  rpc_result

val program_descriptor :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  rpc_result Lwt.t

val program_descriptor_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val info_public :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  rpc_result Lwt.t

val info_public_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val info_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val with_auth :
  Yojson.Safe.t ->
  Octra_core.Circles.circle_info ->
  int ->
  int ->
  int ->
  gate:Circle_auth.gate ->
  op:string ->
  circle_id:string ->
  subject:string ->
  (string -> rpc_result Lwt.t) ->
  rpc_result Lwt.t

val auth_required0 :
  Yojson.Safe.t ->
  string ->
  rpc_result Lwt.t

val auth_required1 :
  Yojson.Safe.t ->
  string ->
  string ->
  rpc_result Lwt.t

val auth_required2 :
  Yojson.Safe.t ->
  string ->
  string ->
  string ->
  rpc_result Lwt.t

val slot_policy_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val state_policy_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val state_descriptor_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val balance_cell_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val register_cell_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val balance_binding_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val register_binding_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val balance_workflow_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val register_workflow_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val object_summary_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val object_members_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val object_detail_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val object_member_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val object_refs_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val object_list_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val transport_policy_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val hfhe_policy_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val key_policy_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val storage_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val storage_dump_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val outbox_intent_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val outbox_claim_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val outbox_status_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val ingress_packet_required_route :
  Yojson.Safe.t ->
  'ctx ->
  rpc_result Lwt.t

val with_scope_auth :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  gate:Circle_auth.gate ->
  op:string ->
  (string -> Octra_core.Circles.circle_info -> rpc_result Lwt.t) ->
  rpc_result Lwt.t

val with_owner_scope_auth :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  op:string ->
  (string -> rpc_result Lwt.t) ->
  rpc_result Lwt.t

val with_owner_subject_auth :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  field:string ->
  op:string ->
  (string -> string -> rpc_result Lwt.t) ->
  rpc_result Lwt.t

val with_any_auth :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  circle_id:string ->
  op:string ->
  subject:string ->
  (unit -> rpc_result Lwt.t) ->
  rpc_result Lwt.t

val with_owner_validated_subject_auth :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  field:string ->
  op:string ->
  validate:(string -> (string, string) result) ->
  (string -> string -> rpc_result Lwt.t) ->
  rpc_result Lwt.t

val with_owner_locator_auth :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  field:string ->
  op:string ->
  parse_locator:(string -> (Circle_view.locator, string) result) ->
  (string -> Circle_view.locator -> rpc_result Lwt.t) ->
  rpc_result Lwt.t

val with_object_member_auth :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  (string -> string -> string -> rpc_result Lwt.t) ->
  rpc_result Lwt.t

val program_info_public :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  rpc_result Lwt.t

val program_info_public_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val program :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  rpc_result Lwt.t

val program_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val view_call :
  Octra_core.Store_irmin.t ->
  view_ctx:Octra_vm.Contract_vm.exec_ctx ->
  circle_id:string ->
  method_name:string ->
  call_params:Yojson.Safe.t list ->
  caller_addr:string ->
  include_storage:bool ->
  rpc_result Lwt.t

val view_call_public :
  Octra_core.Store_irmin.t ->
  view_ctx:Octra_vm.Contract_vm.exec_ctx ->
  circle_id:string ->
  method_name:string ->
  call_params:Yojson.Safe.t list ->
  caller_addr:string ->
  rpc_result Lwt.t

val view_call_public_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  view_ctx:Octra_vm.Contract_vm.exec_ctx ->
  rpc_result Lwt.t

val view_call_auth :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  view_ctx:Octra_vm.Contract_vm.exec_ctx ->
  rpc_result Lwt.t

val object_read :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  object_ref:string ->
  load:(Octra_core.Store_irmin.t -> string -> string -> ('a, string) result Lwt.t) ->
  view:(string -> 'a -> Yojson.Safe.t) ->
  rpc_result Lwt.t

val object_summary_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val object_members_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val object_detail_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val object_member_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val object_discovery :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  load:(Octra_core.Store_irmin.t -> string -> ('a, string) result Lwt.t) ->
  view:('a -> Yojson.Safe.t) ->
  rpc_result Lwt.t

val object_refs_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val object_list_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val transport_policy :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  rpc_result Lwt.t

val hfhe_policy :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  rpc_result Lwt.t

val key_policy :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  key_id:string ->
  current_epoch:int ->
  rpc_result Lwt.t

val slot_locator_policy :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  locator:Circle_view.locator ->
  current_epoch:int ->
  rpc_result Lwt.t

val slot_locator_policy_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  current_epoch:int ->
  rpc_result Lwt.t

val state_locator_policy :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  locator:Circle_view.locator ->
  current_epoch:int ->
  rpc_result Lwt.t

val state_locator_policy_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  current_epoch:int ->
  rpc_result Lwt.t

val state_descriptor :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  locator:Circle_view.locator ->
  rpc_result Lwt.t

val state_descriptor_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val asset_plaintext :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  path_key:string ->
  canonical_path:string ->
  rpc_result Lwt.t

val asset_plaintext_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val asset_ciphertext :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  path_key:string ->
  canonical_path:string ->
  current_epoch:int ->
  reveal_sensitive_fields:bool ->
  rpc_result Lwt.t

val asset_ciphertext_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  current_epoch:int ->
  reveal_sensitive_fields:bool ->
  rpc_result Lwt.t

val asset_ciphertext_by_resource_key :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  resource_key:string ->
  current_epoch:int ->
  reveal_sensitive_fields:bool ->
  rpc_result Lwt.t

val asset_ciphertext_by_resource_key_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  current_epoch:int ->
  reveal_sensitive_fields:bool ->
  rpc_result Lwt.t

val asset_ciphertext_by_locator_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  field:string ->
  parse_locator:(string -> (Circle_view.locator, string) result) ->
  current_epoch:int ->
  reveal_sensitive_fields:bool ->
  rpc_result Lwt.t

val asset_ciphertext_by_slot_ref_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  current_epoch:int ->
  reveal_sensitive_fields:bool ->
  rpc_result Lwt.t

val asset_ciphertext_by_state_ref_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  current_epoch:int ->
  reveal_sensitive_fields:bool ->
  rpc_result Lwt.t

val asset_ciphertext_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  current_epoch:int ->
  rpc_result Lwt.t

val asset_ciphertext_by_resource_key_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  current_epoch:int ->
  rpc_result Lwt.t

val asset_ciphertext_by_locator_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  op:string ->
  field:string ->
  parse_locator:(string -> (Circle_view.locator, string) result) ->
  current_epoch:int ->
  rpc_result Lwt.t

val asset_ciphertext_by_slot_ref_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  current_epoch:int ->
  rpc_result Lwt.t

val asset_ciphertext_by_state_ref_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  current_epoch:int ->
  rpc_result Lwt.t

val private_state_cell :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  locator:Circle_view.locator ->
  current_epoch:int ->
  not_found:string ->
  cell_field:string ->
  empty_cell:'a ->
  load_cell:(Octra_core.Store_irmin.t -> string -> string -> 'a option Lwt.t) ->
  cell_json:('a -> Yojson.Safe.t) ->
  rpc_result Lwt.t

val balance_cell_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  current_epoch:int ->
  rpc_result Lwt.t

val register_cell_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  current_epoch:int ->
  rpc_result Lwt.t

val binding :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  binding_field:string ->
  binding_key_field:string ->
  binding_key_value:string ->
  empty_binding:'a ->
  load_binding:(Octra_core.Store_irmin.t -> string -> string -> 'a option Lwt.t) ->
  binding_json:('a -> Yojson.Safe.t) ->
  current_state_ref:('a -> string option) ->
  rpc_result Lwt.t

val balance_binding_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val register_binding_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val workflow :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  workflow_field:string ->
  workflow_ref:string ->
  empty_workflow:'a ->
  load_workflow:(Octra_core.Store_irmin.t -> string -> string -> 'a option Lwt.t) ->
  workflow_json:('a -> Yojson.Safe.t) ->
  rpc_result Lwt.t

val balance_workflow_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val register_workflow_auth_params :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val storage :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  key:string ->
  rpc_result Lwt.t

val storage_dump :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  rpc_result Lwt.t

val outbox_claim :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  intent_id:string ->
  current_epoch:int ->
  rpc_result Lwt.t

val outbox_intent :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  intent_id:string ->
  rpc_result Lwt.t

val outbox_status :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  intent_id:string ->
  current_epoch:int ->
  rpc_result Lwt.t

val ingress_packet :
  Octra_core.Store_irmin.t ->
  circle_id:string ->
  intent_id:string ->
  rpc_result Lwt.t

val transport_policy_auth :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val hfhe_policy_auth :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val key_policy_auth :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  current_epoch:int ->
  rpc_result Lwt.t

val storage_auth :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val storage_dump_auth :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val outbox_claim_auth :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  current_epoch:int ->
  rpc_result Lwt.t

val outbox_intent_auth :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val outbox_status_auth :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  current_epoch:int ->
  rpc_result Lwt.t

val ingress_packet_auth :
  Octra_core.Store_irmin.t ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val dispatch :
  (Yojson.Safe.t -> 'ctx -> rpc_result Lwt.t) dispatch_adapters ->
  (Yojson.Safe.t -> 'ctx -> rpc_result Lwt.t) Rpc_dispatch.route list