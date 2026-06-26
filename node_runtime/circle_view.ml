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

module Rpc = Octra_core.Rpc

let policy ~delivery_key_id ~activate_after_epoch ~expire_after_epoch ~tombstone ~revoked =
  {
    delivery_key_id;
    activate_after_epoch;
    expire_after_epoch;
    tombstone;
    revoked;
  }

let params_json = function
  | Some (`List values) -> values
  | _ -> []

let view_subject =
  Circle_auth.view_subject

let asset_subject =
  Circle_auth.asset_subject

let view_call_params params =
  match Rpc.require_string params 0 "circle_id",
        Rpc.require_string params 1 "method" with
  | Error e, _ | _, Error e -> Error e
  | Ok circle_id, Ok method_name ->
    Ok {
      circle_id;
      method_name;
      call_params = params_json (Rpc.param_json params 2);
    }

let asset_path_params params =
  match Rpc.require_string params 0 "circle_id", Rpc.require_string params 1 "path" with
  | Error e, _ | _, Error e -> Error e
  | Ok circle_id, Ok raw_path ->
    match Octra_core.Circles.path_key_of_raw_path raw_path with
    | Error e -> Error (Rpc.invalid_params e)
    | Ok (canonical_path, path_key) ->
      Ok {
        circle_id;
        canonical_path;
        path_key;
      }

let asset_locator_params params field parse_locator =
  match Rpc.require_string params 0 "circle_id", Rpc.require_string params 1 field with
  | Error e, _ | _, Error e -> Error e
  | Ok circle_id, Ok value ->
    match parse_locator value with
    | Error e -> Error (Rpc.invalid_params e)
    | Ok locator -> Ok { circle_id; locator }

let asset_resource_params params =
  match Rpc.require_string params 0 "circle_id", Rpc.require_string params 1 "resource_key" with
  | Error e, _ | _, Error e -> Error e
  | Ok circle_id, Ok resource_key ->
    Ok { circle_id; resource_key }

let asset_resource_subject req =
  asset_subject ~kind:"resource_key" ~subject:req.resource_key

let asset_locator_subject field locator =
  asset_subject ~kind:field ~subject:locator.value

let object_member_params params =
  match Rpc.require_string params 0 "circle_id",
        Rpc.require_string params 1 "object_ref",
        Rpc.require_string params 2 "member_ref" with
  | Error e, _, _ | _, Error e, _ | _, _, Error e ->
    Error e
  | Ok circle_id, Ok object_ref, Ok member_ref ->
    match
      Octra_core.Circle_private_common.normalize_hex64 "object_ref" object_ref,
      Octra_core.Circle_object_member.validate_member_ref member_ref
    with
    | Error e, _
    | _, Error e ->
      Error (Rpc.invalid_params e)
    | Ok object_ref, Ok member_ref ->
      Ok {
        circle_id;
        object_ref;
        member_ref;
      }

let object_member_subject req =
  req.object_ref ^ "|" ^ req.member_ref

let opt_string = function
  | Some value -> `String value
  | None -> `Null

let opt_epoch = function
  | Some epoch -> `String (Int64.to_string epoch)
  | None -> `Null

let descriptor_json descriptor_opt =
  Octra_core.Circle_state_descriptor.yojson_of_t
    (Option.value
       ~default:Octra_core.Circle_state_descriptor.default
       descriptor_opt)

let delivery_key_fields = function
  | Some (key_id, key_state) ->
    [
      "delivery_key_id", `String key_id;
      "delivery_key_state",
      `String (Octra_core.Circle_key_state.string_of_t key_state);
      "delivery_key_live", `Bool (Octra_core.Circle_key_state.live key_state);
    ]
  | None ->
    [
      "delivery_key_id", `Null;
      "delivery_key_state", `Null;
      "delivery_key_live", `Null;
    ]

let policy_fields policy delivery_key =
  delivery_key_fields delivery_key
  @ [
    "activate_after_epoch", opt_epoch policy.activate_after_epoch;
    "expire_after_epoch", opt_epoch policy.expire_after_epoch;
    "tombstone", `Bool policy.tombstone;
    "revoked", `Bool policy.revoked;
  ]

let state_policy policy delivery_key =
  `Assoc (policy_fields policy delivery_key)

let slot_locator slot_ref =
  match Octra_core.Circles.path_key_of_slot_ref slot_ref with
  | Error e ->
    Error e
  | Ok (_, canonical_path, path_key) ->
    Ok {
      field = "slot_ref";
      value = slot_ref;
      canonical_path;
      path_key;
    }

let state_locator state_ref =
  match Octra_core.Circles.path_key_of_state_ref state_ref with
  | Error e ->
    Error e
  | Ok (_, canonical_path, path_key) ->
    Ok {
      field = "state_ref";
      value = state_ref;
      canonical_path;
      path_key;
    }

let locator_policy
    ~circle_id
    ~canonical_path
    ~path_key
    ~locator_field
    ~locator_value
    policy
    delivery_key =
  `Assoc
    ([
      "circle_id", `String circle_id;
      locator_field, `String locator_value;
      "canonical_path", `String canonical_path;
      "path_key", `String path_key;
    ] @ policy_fields policy delivery_key)

let locator_policy_of ~circle_id (locator : locator) policy delivery_key =
  locator_policy
    ~circle_id
    ~canonical_path:locator.canonical_path
    ~path_key:locator.path_key
    ~locator_field:locator.field
    ~locator_value:locator.value
    policy
    delivery_key

let state_descriptor
    ~circle_id
    ~canonical_path
    ~path_key
    ~state_ref
    descriptor_opt =
  `Assoc [
    "circle_id", `String circle_id;
    "state_ref", `String state_ref;
    "canonical_path", `String canonical_path;
    "path_key", `String path_key;
    "materialized", `Bool (Option.is_some descriptor_opt);
    "descriptor", descriptor_json descriptor_opt;
  ]

let binding
    ~circle_id
    ~binding_field
    ~binding_key_field
    ~binding_key_value
    ~binding_json
    ~binding_materialized
    ~current_state_ref
    ~descriptor_opt =
  `Assoc [
    "circle_id", `String circle_id;
    binding_key_field, `String binding_key_value;
    "materialized", `Bool binding_materialized;
    "current_state_ref", opt_string current_state_ref;
    "descriptor_materialized", `Bool (Option.is_some descriptor_opt);
    "descriptor", descriptor_json descriptor_opt;
    binding_field, binding_json;
  ]

let workflow ~circle_id ~workflow_field ~workflow_ref ~workflow_json ~workflow_materialized =
  `Assoc [
    "circle_id", `String circle_id;
    "workflow_ref", `String workflow_ref;
    "materialized", `Bool workflow_materialized;
    workflow_field, workflow_json;
  ]

let transport_policy ~circle_id policy =
  `Assoc [
    "circle_id", `String circle_id;
    "policy", Octra_core.Circle_transport_policy.yojson_of_t policy;
  ]

let hfhe_policy ~circle_id policy =
  `Assoc [
    "circle_id", `String circle_id;
    "policy", Octra_core.Circle_hfhe_policy.yojson_of_t policy;
  ]

let key_policy ~circle_id ~key_id ~current_epoch ~key_state policy =
  `Assoc [
    "circle_id", `String circle_id;
    "key_id", `String key_id;
    "current_epoch", `String (Int64.to_string current_epoch);
    "live", `Bool (Octra_core.Circle_key_state.live key_state);
    "state", `String (Octra_core.Circle_key_state.string_of_t key_state);
    "policy", Octra_core.Circle_key_policy.yojson_of_t key_id policy;
  ]

let storage ~circle_id ~key ~value =
  `Assoc [
    "circle_id", `String circle_id;
    "key", `String key;
    "value", opt_string value;
  ]

let storage_dump ~circle_id storage_pairs =
  `Assoc [
    "circle_id", `String circle_id;
    "storage", Rpc_view.storage_assoc storage_pairs;
    "storage_sizes", Rpc_view.storage_sizes storage_pairs;
    "count", `Int (List.length storage_pairs);
  ]

let outbox_claim ~circle_id ~primary_claim ~claims ~active_claims =
  `Assoc [
    "circle_id", `String circle_id;
    "claim",
    begin
      match primary_claim with
      | Some claim -> Octra_core.Circles.yojson_of_relay_claim claim
      | None -> `Null
    end;
    "claims", `List (List.map Octra_core.Circles.yojson_of_relay_claim claims);
    "active_claims", `List (List.map Octra_core.Circles.yojson_of_relay_claim active_claims);
  ]

let outbox_claim_of_state ~circle_id ~claims ~active_claims =
  let primary_claim =
    match active_claims with
    | claim :: _ -> Some claim
    | [] -> Octra_core.Circle_transport_claim_set.latest_claim claims in
  outbox_claim
    ~circle_id
    ~primary_claim
    ~claims
    ~active_claims

let outbox_intent ~circle_id intent =
  `Assoc [
    "circle_id", `String circle_id;
    "intent", Octra_core.Circles.yojson_of_outbox_intent intent;
  ]

let ingress_packet ~circle_id packet =
  `Assoc [
    "circle_id", `String circle_id;
    "packet", Octra_core.Circles.yojson_of_ingress_packet packet;
  ]

let code_bytes_of_b64 = function
  | None ->
    0
  | Some code_b64 ->
    let clean = String.concat "" (String.split_on_char '\n' code_b64) in
    let len = String.length clean in
    let pad =
      if len >= 2 && String.equal (String.sub clean (len - 2) 2) "==" then 2
      else if len >= 1 && String.equal (String.sub clean (len - 1) 1) "=" then 1
      else 0
    in
    max 0 (((len * 3) / 4) - pad)

let program ~circle_id ~code_b64 ~code_hash ~runtime =
  `Assoc [
    "circle_id", `String circle_id;
    "has_program", `Bool (Option.is_some code_b64);
    "code_b64", opt_string code_b64;
    "code_bytes", `Int (code_bytes_of_b64 code_b64);
    "code_hash", `String code_hash;
    "runtime", `String (Octra_core.Circles.string_of_runtime runtime);
  ]

let view_result ?storage_pairs result =
  match storage_pairs with
  | Some storage_pairs ->
    `Assoc [
      "result", result;
      "storage", Rpc_view.storage_assoc storage_pairs;
    ]
  | None ->
    `Assoc [
      "result", result;
    ]

let object_summary ~circle_id (summary : Octra_core.Circle_object_query.summary) =
  `Assoc [
    "circle_id", `String circle_id;
    "object_ref", `String summary.object_ref;
    "binding_materialized",
    `Bool (Octra_core.Circle_object_binding.materialized summary.binding);
    "binding", Octra_core.Circle_object_binding.yojson_of_t summary.binding;
    "policy_materialized", `Bool (Option.is_some summary.policy);
    "policy",
    begin
      match summary.policy with
      | Some policy -> Octra_core.Circle_object_policy.yojson_of_t policy
      | None -> `Null
    end;
    "active_member_count", `Int summary.active_member_count;
    "member_refs", `List (List.map (fun member_ref -> `String member_ref) summary.member_refs);
  ]

let object_members ~circle_id ~object_ref members =
  `Assoc [
    "circle_id", `String circle_id;
    "object_ref", `String object_ref;
    "member_count", `Int (List.length members);
    "members",
    `List
      (List.map
         (fun (member : Octra_core.Circle_object_query.member_summary) ->
           `Assoc [
             "member_ref", `String member.member_ref;
             "member_state",
             Octra_core.Circle_object_member.yojson_of_t member.member_state;
           ])
         members);
  ]

let object_detail ~circle_id (detail : Octra_core.Circle_object_detail_query.detail) =
  `Assoc [
    "circle_id", `String circle_id;
    "object_ref", `String detail.summary.object_ref;
    "summary", object_summary ~circle_id detail.summary;
    "current_state_descriptor_materialized",
    `Bool (Option.is_some detail.current_descriptor);
    "current_state_descriptor",
    begin
      match detail.current_descriptor with
      | Some descriptor -> Octra_core.Circle_state_descriptor.yojson_of_t descriptor
      | None -> `Null
    end;
    "last_transition_materialized", `Bool (Option.is_some detail.last_transition);
    "last_transition",
    begin
      match detail.last_transition with
      | Some transition -> Octra_core.Circle_object_transition.yojson_of_t transition
      | None -> `Null
    end;
  ]

let object_member_detail
    ~circle_id
    ~object_ref
    (detail : Octra_core.Circle_object_detail_query.member_detail) =
  `Assoc [
    "circle_id", `String circle_id;
    "object_ref", `String object_ref;
    "member_ref", `String detail.member_ref;
    "materialized",
    `Bool (Octra_core.Circle_object_member.materialized detail.member_state);
    "member_state", Octra_core.Circle_object_member.yojson_of_t detail.member_state;
    "descriptor_materialized", `Bool (Option.is_some detail.descriptor);
    "descriptor",
    begin
      match detail.descriptor with
      | Some descriptor -> Octra_core.Circle_state_descriptor.yojson_of_t descriptor
      | None -> `Null
    end;
  ]

let object_refs ~circle_id object_refs =
  `Assoc [
    "circle_id", `String circle_id;
    "object_count", `Int (List.length object_refs);
    "object_refs", `List (List.map (fun object_ref -> `String object_ref) object_refs);
  ]

let object_list ~circle_id summaries =
  `Assoc [
    "circle_id", `String circle_id;
    "object_count", `Int (List.length summaries);
    "objects", `List (List.map (object_summary ~circle_id) summaries);
  ]

let outbox_status
    ~circle_id
    ~intent_id
    ~stored_status
    ~(delivery_key_status : Octra_core.Circle_transport_state.delivery_key_status)
    ~(transport_policy : Octra_core.Circle_transport_policy.t)
    ~(evaluation : Octra_core.Circle_transport_resolution.evaluation)
    ~active_claims
    ~claims
    ~ingress_eligible_relays
    ~selected_ingress_relay =
  `Assoc [
    "circle_id", `String circle_id;
    "intent_id", `String intent_id;
    "status", `String (Octra_core.Circles.string_of_outbox_status evaluation.effective_status);
    "stored_status", `String (Octra_core.Circles.string_of_outbox_status stored_status);
    "delivery_key_live", `Bool (Octra_core.Circle_key_state.live delivery_key_status.state);
    "delivery_key_state", `String (Octra_core.Circle_key_state.string_of_t delivery_key_status.state);
    "delivery_key_id", opt_string delivery_key_status.key_id;
    "claim_topology",
    `String
      (Octra_core.Circle_transport_policy.string_of_claim_topology
         transport_policy.claim_topology);
    "quorum_mode",
    `String
      (Octra_core.Circle_transport_policy.string_of_quorum_mode
         transport_policy.quorum_mode);
    "ingress_strategy",
    `String
      (Octra_core.Circle_transport_policy.string_of_ingress_strategy
         transport_policy.ingress_strategy);
    "lease_class",
    `String
      (Octra_core.Circle_transport_policy.string_of_lease_class
         transport_policy.lease_class);
    "quorum_threshold",
    `Int (Octra_core.Circle_transport_policy.quorum_threshold_value transport_policy);
    "quorum_weight_threshold",
    begin
      match transport_policy.quorum_mode with
      | Octra_core.Circle_transport_policy.Weighted_quorum ->
        `Int (Octra_core.Circle_transport_policy.quorum_weight_threshold_value transport_policy)
      | Octra_core.Circle_transport_policy.Count_quorum ->
        `Null
    end;
    "max_active_claims",
    begin
      match transport_policy.max_active_claims, transport_policy.claim_topology with
      | Some max_active_claims, _ -> `Int max_active_claims
      | None, Octra_core.Circle_transport_policy.Single_relay -> `Int 1
      | None, _ -> `Null
    end;
    "active_claim_count", `Int (List.length active_claims);
    "active_claim_weight",
    `Int
      (Octra_core.Circle_transport_topology.active_claim_weight
         transport_policy
         active_claims);
    "relay_weights",
    `Assoc
      (List.map
         (fun (relay_id, weight) -> relay_id, `Int weight)
         transport_policy.relay_weights);
    "claim_ready",
    `Bool
      (Octra_core.Circle_transport_state.claim_ready
         ~transport_policy
         active_claims);
    "selected_ingress_relay", opt_string selected_ingress_relay;
    "ingress_eligible_relays",
    `List (List.map (fun relay_id -> `String relay_id) ingress_eligible_relays);
    "claims",
    `List (List.map Octra_core.Circles.yojson_of_relay_claim claims);
    "active_claims",
    `List (List.map Octra_core.Circles.yojson_of_relay_claim active_claims);
    "resolution",
    begin
      match evaluation.effective_resolution with
      | Some resolution -> Octra_core.Circles.yojson_of_outbox_resolution resolution
      | None -> `Null
    end;
  ]

let outbox_status_of_state
    ~circle_id
    ~intent_id
    ~current_epoch
    ~intent
    ~stored_status
    ~stored_resolution
    ~active_claims
    ~claims
    ~transport_policy
    ~delivery_key_status =
  let evaluation =
    Octra_core.Circle_transport_state.evaluate_outbox
      ~current_epoch
      ~transport_policy
      ~intent
      ~stored_status
      ~stored_resolution
      ~active_claims
      ~delivery_key_status in
  let ingress_eligible_relays =
    Octra_core.Circle_transport_quorum.eligible_relays
      transport_policy
      active_claims in
  let selected_ingress_relay =
    match ingress_eligible_relays with
    | relay_id :: _ -> Some relay_id
    | [] -> None in
  outbox_status
    ~circle_id
    ~intent_id
    ~stored_status
    ~delivery_key_status
    ~transport_policy
    ~evaluation
    ~active_claims
    ~claims
    ~ingress_eligible_relays
    ~selected_ingress_relay

let private_state
    ~circle_id
    ~state_ref
    ~canonical_path
    ~path_key
    ~ciphertext_b64
    ~(meta : Octra_core.Circles.asset_meta)
    ~descriptor_opt
    ~policy
    ~delivery_key
    ~cell_field
    ~cell_materialized
    ~cell_json =
  `Assoc [
    "circle_id", `String circle_id;
    "state_ref", `String state_ref;
    "canonical_path", `String canonical_path;
    "path_key", `String path_key;
    "ciphertext_b64", `String ciphertext_b64;
    "content_type", `String meta.content_type;
    "encoding", `String meta.encoding;
    "size_bytes", `String (Int64.to_string meta.size_bytes);
    "metadata_mode",
    `String (Octra_core.Circles.string_of_metadata_mode meta.metadata_mode);
    "key_id", opt_string meta.key_id;
    "plaintext_hash", opt_string meta.plaintext_hash;
    "padding_class", opt_string meta.padding_class;
    "descriptor_materialized", `Bool (Option.is_some descriptor_opt);
    "descriptor", descriptor_json descriptor_opt;
    "state_policy", state_policy policy delivery_key;
    cell_field, cell_json;
    cell_field ^ "_materialized", `Bool cell_materialized;
  ]

let asset_delivery_key_fields = function
  | Some (key_id, key_state) ->
    [
      "delivery_key_id", `String key_id;
      "delivery_key_state",
      `String (Octra_core.Circle_key_state.string_of_t key_state);
      "delivery_key_live", `Bool (Octra_core.Circle_key_state.live key_state);
    ]
  | None ->
    []

let asset_descriptor_fields = function
  | Some descriptor_opt ->
    [
      "state_descriptor_materialized", `Bool (Option.is_some descriptor_opt);
      "state_descriptor", descriptor_json descriptor_opt;
    ]
  | None ->
    []

let asset_effective_key_id policy (meta : Octra_core.Circles.asset_meta) =
  match policy.delivery_key_id, meta.key_id with
  | Some key_id, _ -> Some key_id
  | None, Some key_id -> Some key_id
  | None, None -> None

let asset_state_descriptor_path_key (meta : Octra_core.Circles.asset_meta) =
  match Octra_core.Circles.state_ref_of_canonical_path meta.canonical_path with
  | Some _ -> Some meta.path_key
  | None -> None

let asset_meta_sensitive_fields meta =
  [
    "slot_ref", opt_string meta.Octra_core.Circles.slot_ref;
    "state_ref",
    begin
      match Octra_core.Circles.state_ref_of_canonical_path meta.canonical_path with
      | Some state_ref -> `String state_ref
      | None -> `Null
    end;
    "activate_after_epoch", opt_epoch meta.activate_after_epoch;
    "expire_after_epoch", opt_epoch meta.expire_after_epoch;
    "plaintext_hash", opt_string meta.plaintext_hash;
    "key_id", opt_string meta.key_id;
    "padding_class", opt_string meta.padding_class;
  ]

let asset_sensitive_fields meta delivery_key_fields state_descriptor_fields =
  asset_meta_sensitive_fields meta @ delivery_key_fields @ state_descriptor_fields

let asset_plaintext
    ~circle_id
    ~canonical_path
    ~path_key
    ~browser_mode
    ~resource_mode
    ~(meta : Octra_core.Circles.asset_meta)
    ~body_b64 =
  `Assoc [
    "circle_id", `String circle_id;
    "canonical_path", `String canonical_path;
    "path_key", `String path_key;
    "content_type", `String meta.content_type;
    "encoding", `String meta.encoding;
    "size_bytes", `String (Int64.to_string meta.size_bytes);
    "blob_hash", `String meta.blob_hash;
    "resource_key", `String meta.resource_key;
    "browser_mode", `String (Octra_core.Circles.string_of_browser_mode browser_mode);
    "resource_mode", `String (Octra_core.Circles.string_of_resource_mode resource_mode);
    "body_b64", `String body_b64;
  ]

let asset_ciphertext
    ~circle_id
    ~ciphertext_b64
    ~canonical_path
    ~path_key
    ~browser_mode
    ~resource_mode
    ~(meta : Octra_core.Circles.asset_meta)
    ~delivery_key_fields
    ~state_descriptor_fields
    ~reveal_sensitive_fields =
  let address_fields, response_fields =
    match meta.metadata_mode with
    | Octra_core.Circles.Metadata_reveal ->
      ([
         "canonical_path", `String canonical_path;
         "path_key", `String path_key;
         "blob_hash", `String meta.blob_hash;
         "resource_key", `String meta.resource_key;
       ],
       [
         "content_type", `String meta.content_type;
         "encoding", `String meta.encoding;
         "size_bytes", `String (Int64.to_string meta.size_bytes);
         "body_mode", `String (Octra_core.Circles.string_of_resource_mode meta.body_mode);
         "locator_mode", `String (Octra_core.Circles.string_of_locator_mode meta.locator_mode);
         "metadata_mode", `String (Octra_core.Circles.string_of_metadata_mode meta.metadata_mode);
       ] @ asset_meta_sensitive_fields meta @ [
         "browser_mode", `String (Octra_core.Circles.string_of_browser_mode browser_mode);
         "resource_mode", `String (Octra_core.Circles.string_of_resource_mode resource_mode);
       ] @ delivery_key_fields @ state_descriptor_fields)
    | Octra_core.Circles.Metadata_opaque ->
      let opaque_sensitive_fields =
        if reveal_sensitive_fields then
          asset_sensitive_fields meta delivery_key_fields state_descriptor_fields
        else
          [] in
      ([],
       [
         "content_type", `String meta.content_type;
         "encoding", `String meta.encoding;
         "metadata_mode", `String (Octra_core.Circles.string_of_metadata_mode meta.metadata_mode);
       ] @ opaque_sensitive_fields)
  in
  `Assoc
    ([
      "circle_id", `String circle_id;
      "ciphertext_b64", `String ciphertext_b64;
    ] @ response_fields @ address_fields)