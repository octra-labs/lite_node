(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type policy = {
  delivery_key_id : string option;
  activate_after_epoch : int64 option;
  expire_after_epoch : int64 option;
  tombstone : bool;
  revoked : bool;
}

type delivery_key = (string * Octra_core.Circle_key_state.t) option

type locator = {
  field : string;
  value : string;
  canonical_path : string;
  path_key : string;
}

type view_call = {
  circle_id : string;
  method_name : string;
  call_params : Yojson.Safe.t list;
}

type asset_path = {
  circle_id : string;
  canonical_path : string;
  path_key : string;
}

type asset_locator = {
  circle_id : string;
  locator : locator;
}

type asset_resource = {
  circle_id : string;
  resource_key : string;
}

type object_member_request = {
  circle_id : string;
  object_ref : string;
  member_ref : string;
}

val policy :
  delivery_key_id:string option ->
  activate_after_epoch:int64 option ->
  expire_after_epoch:int64 option ->
  tombstone:bool ->
  revoked:bool ->
  policy

val view_subject :
  method_name:string ->
  call_params:Yojson.Safe.t list ->
  include_storage:bool ->
  string

val asset_subject :
  kind:string ->
  subject:string ->
  string

val view_call_params :
  Yojson.Safe.t ->
  (view_call, Octra_core.Rpc.rpc_error) result

val asset_path_params :
  Yojson.Safe.t ->
  (asset_path, Octra_core.Rpc.rpc_error) result

val asset_locator_params :
  Yojson.Safe.t ->
  string ->
  (string -> (locator, string) result) ->
  (asset_locator, Octra_core.Rpc.rpc_error) result

val asset_resource_params :
  Yojson.Safe.t ->
  (asset_resource, Octra_core.Rpc.rpc_error) result

val asset_resource_subject :
  asset_resource ->
  string

val asset_locator_subject :
  string ->
  locator ->
  string

val object_member_params :
  Yojson.Safe.t ->
  (object_member_request, Octra_core.Rpc.rpc_error) result

val object_member_subject :
  object_member_request ->
  string

val state_policy :
  policy ->
  delivery_key ->
  Yojson.Safe.t

val slot_locator :
  string ->
  (locator, string) result

val state_locator :
  string ->
  (locator, string) result

val locator_policy :
  circle_id:string ->
  canonical_path:string ->
  path_key:string ->
  locator_field:string ->
  locator_value:string ->
  policy ->
  delivery_key ->
  Yojson.Safe.t

val locator_policy_of :
  circle_id:string ->
  locator ->
  policy ->
  delivery_key ->
  Yojson.Safe.t

val state_descriptor :
  circle_id:string ->
  canonical_path:string ->
  path_key:string ->
  state_ref:string ->
  Octra_core.Circle_state_descriptor.t option ->
  Yojson.Safe.t

val binding :
  circle_id:string ->
  binding_field:string ->
  binding_key_field:string ->
  binding_key_value:string ->
  binding_json:Yojson.Safe.t ->
  binding_materialized:bool ->
  current_state_ref:string option ->
  descriptor_opt:Octra_core.Circle_state_descriptor.t option ->
  Yojson.Safe.t

val workflow :
  circle_id:string ->
  workflow_field:string ->
  workflow_ref:string ->
  workflow_json:Yojson.Safe.t ->
  workflow_materialized:bool ->
  Yojson.Safe.t

val transport_policy :
  circle_id:string ->
  Octra_core.Circle_transport_policy.t ->
  Yojson.Safe.t

val hfhe_policy :
  circle_id:string ->
  Octra_core.Circle_hfhe_policy.t ->
  Yojson.Safe.t

val key_policy :
  circle_id:string ->
  key_id:string ->
  current_epoch:int64 ->
  key_state:Octra_core.Circle_key_state.t ->
  Octra_core.Circle_key_policy.t ->
  Yojson.Safe.t

val storage :
  circle_id:string ->
  key:string ->
  value:string option ->
  Yojson.Safe.t

val storage_dump :
  circle_id:string ->
  total:int ->
  (string * string) list ->
  Yojson.Safe.t

val outbox_claim :
  circle_id:string ->
  primary_claim:Octra_core.Circles.relay_claim option ->
  claims:Octra_core.Circles.relay_claim list ->
  active_claims:Octra_core.Circles.relay_claim list ->
  Yojson.Safe.t

val outbox_claim_of_state :
  circle_id:string ->
  claims:Octra_core.Circles.relay_claim list ->
  active_claims:Octra_core.Circles.relay_claim list ->
  Yojson.Safe.t

val outbox_intent :
  circle_id:string ->
  Octra_core.Circles.outbox_intent ->
  Yojson.Safe.t

val ingress_packet :
  circle_id:string ->
  Octra_core.Circles.ingress_packet ->
  Yojson.Safe.t

val program :
  circle_id:string ->
  code_b64:string option ->
  code_hash:string ->
  runtime:Octra_core.Circles.runtime ->
  Yojson.Safe.t

val view_result :
  ?storage_pairs:(string * string) list ->
  Yojson.Safe.t ->
  Yojson.Safe.t

val object_summary :
  circle_id:string ->
  Octra_core.Circle_object_query.summary ->
  Yojson.Safe.t

val object_members :
  circle_id:string ->
  object_ref:string ->
  Octra_core.Circle_object_query.member_summary list ->
  Yojson.Safe.t

val object_detail :
  circle_id:string ->
  Octra_core.Circle_object_detail_query.detail ->
  Yojson.Safe.t

val object_member_detail :
  circle_id:string ->
  object_ref:string ->
  Octra_core.Circle_object_detail_query.member_detail ->
  Yojson.Safe.t

val object_refs :
  circle_id:string ->
  string list ->
  Yojson.Safe.t

val object_list :
  circle_id:string ->
  Octra_core.Circle_object_query.summary list ->
  Yojson.Safe.t

val outbox_status :
  circle_id:string ->
  intent_id:string ->
  stored_status:Octra_core.Circles.outbox_status ->
  delivery_key_status:Octra_core.Circle_transport_state.delivery_key_status ->
  transport_policy:Octra_core.Circle_transport_policy.t ->
  evaluation:Octra_core.Circle_transport_resolution.evaluation ->
  active_claims:Octra_core.Circles.relay_claim list ->
  claims:Octra_core.Circles.relay_claim list ->
  ingress_eligible_relays:string list ->
  selected_ingress_relay:string option ->
  Yojson.Safe.t

val outbox_status_of_state :
  circle_id:string ->
  intent_id:string ->
  current_epoch:int ->
  intent:Octra_core.Circles.outbox_intent ->
  stored_status:Octra_core.Circles.outbox_status ->
  stored_resolution:Octra_core.Circles.outbox_resolution option ->
  active_claims:Octra_core.Circles.relay_claim list ->
  claims:Octra_core.Circles.relay_claim list ->
  transport_policy:Octra_core.Circle_transport_policy.t ->
  delivery_key_status:Octra_core.Circle_transport_state.delivery_key_status ->
  Yojson.Safe.t

val private_state :
  circle_id:string ->
  state_ref:string ->
  canonical_path:string ->
  path_key:string ->
  ciphertext_b64:string ->
  meta:Octra_core.Circles.asset_meta ->
  descriptor_opt:Octra_core.Circle_state_descriptor.t option ->
  policy:policy ->
  delivery_key:delivery_key ->
  cell_field:string ->
  cell_materialized:bool ->
  cell_json:Yojson.Safe.t ->
  Yojson.Safe.t

val asset_delivery_key_fields :
  delivery_key ->
  (string * Yojson.Safe.t) list

val asset_descriptor_fields :
  Octra_core.Circle_state_descriptor.t option option ->
  (string * Yojson.Safe.t) list

val asset_effective_key_id :
  policy ->
  Octra_core.Circles.asset_meta ->
  string option

val asset_state_descriptor_path_key :
  Octra_core.Circles.asset_meta ->
  string option

val asset_plaintext :
  circle_id:string ->
  canonical_path:string ->
  path_key:string ->
  browser_mode:Octra_core.Circles.browser_mode ->
  resource_mode:Octra_core.Circles.resource_mode ->
  meta:Octra_core.Circles.asset_meta ->
  body_b64:string ->
  Yojson.Safe.t

val asset_ciphertext :
  circle_id:string ->
  ciphertext_b64:string ->
  canonical_path:string ->
  path_key:string ->
  browser_mode:Octra_core.Circles.browser_mode ->
  resource_mode:Octra_core.Circles.resource_mode ->
  meta:Octra_core.Circles.asset_meta ->
  delivery_key_fields:(string * Yojson.Safe.t) list ->
  state_descriptor_fields:(string * Yojson.Safe.t) list ->
  reveal_sensitive_fields:bool ->
  Yojson.Safe.t