(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type runtime =
  | Octb
  | Wasm_v1

type privacy_class =
  | Public
  | Scoped
  | Sealed
  | Circle

type browser_mode =
  | Gateway_allowed
  | Native_sealed

type resource_mode =
  | Public_resources
  | Sealed_read

type locator_mode =
  | Path_locator
  | Slot_locator
  | State_locator

type metadata_mode =
  | Metadata_reveal
  | Metadata_opaque

type limits = {
  max_stable_bytes : int64;
  max_assets_bytes : int64;
  max_inline_value : int64;
  max_wasm_bytes : int64;
}

type circle_info = {
  circle_id : string;
  runtime : runtime;
  version : int64;
  owner : string;
  code_hash : string;
  stable_root : string;
  assets_root : string;
  privacy_class : privacy_class;
  browser_mode : browser_mode;
  resource_mode : resource_mode;
  policy_hash : string option;
  members_root : string option;
  export_policy : string option;
  limits : limits;
}

type deploy_payload = {
  runtime : runtime;
  privacy_class : privacy_class;
  browser_mode : browser_mode;
  resource_mode : resource_mode;
  code_b64 : string option;
  policy_hash : string option;
  members_root : string option;
  export_policy : string option;
  limits : limits;
}

type spawn_owner =
  | Spawn_owner_caller
  | Spawn_owner_parent

type program_update_payload = {
  code_b64 : string;
}

type asset_put_payload = {
  path : string;
  content_type : string;
  encoding : string option;
}

type encrypted_asset_put_payload = {
  path : string option;
  slot_ref : string option;
  state_ref : string option;
  content_type : string;
  encoding : string option;
  key_id : string;
  plaintext_hash : string;
  padding_class : string option;
  activate_after_epoch : int64 option;
  expire_after_epoch : int64 option;
  metadata_mode : metadata_mode option;
}

type sealed_slot_put_payload = {
  slot_ref : string option;
  state_ref : string option;
  content_type : string;
  encoding : string option;
  key_id : string;
  plaintext_hash : string;
  padding_class : string option;
  activate_after_epoch : int64 option;
  expire_after_epoch : int64 option;
  metadata_mode : metadata_mode option;
}

type slot_policy_put_payload = {
  slot_ref : string option;
  state_ref : string option;
  delivery_key_id : string option;
  activate_after_epoch : int64 option;
  expire_after_epoch : int64 option;
  tombstone : bool;
  revoked : bool;
}

type state_descriptor_put_payload = Circle_state_descriptor.put_payload

type transport_policy_put_payload = Circle_transport_policy.put_payload

type hfhe_policy_put_payload = Circle_hfhe_policy.put_payload

type key_policy_put_payload = Circle_key_policy.put_payload

type key_grant_payload = {
  key_id : string;
  activate_after_epoch : int64 option;
  expire_after_epoch : int64 option;
}

type key_extend_payload = {
  key_id : string;
  expire_after_epoch : int64;
}

type key_revoke_payload = {
  key_id : string;
}

type key_erase_payload = {
  key_id : string;
}

type stable_value =
  | Inline of string
  | Blob_ref of {
      blob_hash : string;
      value_size : int64;
    }

type stable_entry = {
  raw_key : string;
  key_hash : string;
  value : stable_value;
}

type asset_meta = {
  path_key : string;
  canonical_path : string;
  content_type : string;
  encoding : string;
  size_bytes : int64;
  blob_hash : string;
  body_mode : resource_mode;
  plaintext_hash : string option;
  key_id : string option;
  padding_class : string option;
  resource_key : string;
  locator_mode : locator_mode;
  slot_ref : string option;
  activate_after_epoch : int64 option;
  expire_after_epoch : int64 option;
  metadata_mode : metadata_mode;
}

type outbox_status =
  | Open
  | Claimed
  | Fulfilled
  | Expired
  | Cancelled

type outbox_intent = {
  intent_id : string;
  created_epoch : int64;
  expiry_epoch : int64;
  relay_policy_hash : string;
  payload_hash : string;
  ciphertext_blob_hash : string option;
  delivery_key_id : string option;
  max_response_bytes : int64;
  fee_budget : int64;
  route_hint : string option;
  callback_policy_hash : string option;
}

type outbox_open_payload = {
  intent_id : string;
  expiry_epoch : int64;
  relay_policy_hash : string;
  payload_hash : string;
  ciphertext_blob_hash : string option;
  delivery_key_id : string option;
  max_response_bytes : int64;
  fee_budget : int64;
  route_hint : string option;
  callback_policy_hash : string option;
}

type relay_claim = {
  intent_id : string;
  relay_id : string;
  claim_epoch : int64;
  claim_expiry_epoch : int64;
  signature : string;
}

type outbox_resolution_reason =
  | Fulfilled_delivery
  | Relay_cancelled
  | Owner_cancelled
  | Intent_expired
  | Claim_expired
  | Claim_set_exhausted
  | Delivery_key_inactive
  | Delivery_key_expired
  | Delivery_key_revoked
  | Delivery_key_erased

type relay_cancel = {
  intent_id : string;
  relay_id : string;
  cancel_epoch : int64;
  reason : outbox_resolution_reason;
  related_key_id : string option;
  signature : string;
}

type outbox_resolution = {
  intent_id : string;
  status : outbox_status;
  reason : outbox_resolution_reason;
  resolved_epoch : int64;
  actor_id : string option;
  related_key_id : string option;
}

type ingress_packet = {
  intent_id : string;
  relay_id : string;
  ingress_nonce : int64;
  result_code : int;
  response_payload_hash : string;
  response_size_bytes : int64;
  response_ciphertext_blob_hash : string option;
  external_receipt_hash : string option;
  signature : string;
}

type ingress_commit_payload = {
  intent_id : string;
  relay_id : string;
  ingress_nonce : int64;
  result_code : int;
  response_payload_hash : string;
  response_size_bytes : int64;
  response_ciphertext_blob_hash : string option;
  external_receipt_hash : string option;
  signature : string;
}

type relay_cancel_payload = relay_cancel

type resource = {
  circle_id : string;
  path : string;
}

let default_limits = {
  max_stable_bytes = 33_554_432L;
  max_assets_bytes = 33_554_432L;
  max_inline_value = 65_536L;
  max_wasm_bytes = 33_554_432L;
}

let validate_limits limits =
  let fields = [
    "max_stable_bytes", limits.max_stable_bytes, default_limits.max_stable_bytes;
    "max_assets_bytes", limits.max_assets_bytes, default_limits.max_assets_bytes;
    "max_inline_value", limits.max_inline_value, default_limits.max_inline_value;
    "max_wasm_bytes", limits.max_wasm_bytes, default_limits.max_wasm_bytes;
  ] in
  match
    List.find_opt
      (fun (_, value, cap) ->
        Int64.compare value 0L < 0 || Int64.compare value cap > 0)
      fields
  with
  | Some (name, _, _) -> Error ("circle limit out of range: " ^ name)
  | None when Int64.compare limits.max_inline_value limits.max_stable_bytes > 0 ->
    Error "circle max_inline_value exceeds max_stable_bytes"
  | None -> Ok ()

let zero_hash_hex = String.make 64 '0'

let validate_stable_storage limits storage_tbl =
  let total_bytes =
    Hashtbl.fold (fun raw_key value acc ->
      let entry_bytes =
        Int64.add
          (Int64.of_int (String.length raw_key))
          (Int64.of_int (String.length value)) in
      Int64.add acc entry_bytes
    ) storage_tbl 0L in
  let oversize_entry =
    Hashtbl.fold (fun raw_key value acc ->
      match acc with
      | Some _ -> acc
      | None ->
        let value_bytes = Int64.of_int (String.length value) in
        if Int64.compare value_bytes limits.max_inline_value > 0 then
          Some (raw_key, value_bytes)
        else
          None
    ) storage_tbl None in
  match oversize_entry with
  | Some (raw_key, value_bytes) ->
    Error
      (Printf.sprintf
         "circle stable value exceeds max_inline_value key=%s bytes=%Ld limit=%Ld"
         raw_key
         value_bytes
         limits.max_inline_value)
  | None ->
    if Int64.compare total_bytes limits.max_stable_bytes > 0 then
      Error
        (Printf.sprintf
           "circle stable storage exceeds max_stable_bytes bytes=%Ld limit=%Ld"
           total_bytes
           limits.max_stable_bytes)
    else
      Ok total_bytes

let string_of_runtime = function
  | Octb -> "octb"
  | Wasm_v1 -> "wasm_v1"

let runtime_of_string = function
  | "octb" -> Ok Octb
  | "wasm_v1" -> Ok Wasm_v1
  | s -> Error ("unknown runtime: " ^ s)

let string_of_privacy_class = function
  | Public -> "public"
  | Scoped -> "scoped"
  | Sealed -> "sealed"
  | Circle -> "circle"

let privacy_class_of_string = function
  | "public" -> Ok Public
  | "scoped" -> Ok Scoped
  | "sealed" -> Ok Sealed
  | "circle" -> Ok Circle
  | s -> Error ("unknown privacy class: " ^ s)

let string_of_browser_mode = function
  | Gateway_allowed -> "gateway_allowed"
  | Native_sealed -> "native_sealed"

let browser_mode_of_string = function
  | "gateway_allowed" -> Ok Gateway_allowed
  | "native_sealed" -> Ok Native_sealed
  | s -> Error ("unknown browser mode: " ^ s)

let string_of_resource_mode = function
  | Public_resources -> "public_resources"
  | Sealed_read -> "sealed_read"

let resource_mode_of_string = function
  | "public_resources" -> Ok Public_resources
  | "sealed_read" -> Ok Sealed_read
  | s -> Error ("unknown resource mode: " ^ s)

let string_of_spawn_owner = function
  | Spawn_owner_caller -> "caller"
  | Spawn_owner_parent -> "parent"

let spawn_owner_of_string = function
  | "caller" -> Ok Spawn_owner_caller
  | "parent" -> Ok Spawn_owner_parent
  | s -> Error ("unknown spawn owner: " ^ s)

let string_of_locator_mode = function
  | Path_locator -> "path"
  | Slot_locator -> "opaque_slot"
  | State_locator -> "private_state"

let locator_mode_of_string = function
  | "path" -> Ok Path_locator
  | "opaque_slot" -> Ok Slot_locator
  | "private_state" -> Ok State_locator
  | s -> Error ("unknown locator mode: " ^ s)

let string_of_metadata_mode = function
  | Metadata_reveal -> "reveal"
  | Metadata_opaque -> "opaque"

let metadata_mode_of_string = function
  | "reveal" -> Ok Metadata_reveal
  | "opaque" -> Ok Metadata_opaque
  | s -> Error ("unknown metadata mode: " ^ s)

let string_of_outbox_status = function
  | Open -> "open"
  | Claimed -> "claimed"
  | Fulfilled -> "fulfilled"
  | Expired -> "expired"
  | Cancelled -> "cancelled"

let outbox_status_of_string = function
  | "open" -> Ok Open
  | "claimed" -> Ok Claimed
  | "fulfilled" -> Ok Fulfilled
  | "expired" -> Ok Expired
  | "cancelled" -> Ok Cancelled
  | s -> Error ("unknown outbox status: " ^ s)

let string_of_outbox_resolution_reason = function
  | Fulfilled_delivery -> "fulfilled_delivery"
  | Relay_cancelled -> "relay_cancelled"
  | Owner_cancelled -> "owner_cancelled"
  | Intent_expired -> "intent_expired"
  | Claim_expired -> "claim_expired"
  | Claim_set_exhausted -> "claim_set_exhausted"
  | Delivery_key_inactive -> "delivery_key_inactive"
  | Delivery_key_expired -> "delivery_key_expired"
  | Delivery_key_revoked -> "delivery_key_revoked"
  | Delivery_key_erased -> "delivery_key_erased"

let outbox_resolution_reason_of_string = function
  | "fulfilled_delivery" -> Ok Fulfilled_delivery
  | "relay_cancelled" -> Ok Relay_cancelled
  | "owner_cancelled" -> Ok Owner_cancelled
  | "intent_expired" -> Ok Intent_expired
  | "claim_expired" -> Ok Claim_expired
  | "delivery_key_inactive" -> Ok Delivery_key_inactive
  | "claim_set_exhausted" -> Ok Claim_set_exhausted
  | "delivery_key_expired" -> Ok Delivery_key_expired
  | "delivery_key_revoked" -> Ok Delivery_key_revoked
  | "delivery_key_erased" -> Ok Delivery_key_erased
  | s -> Error ("unknown outbox resolution reason: " ^ s)

let yojson_of_limits (t : limits) =
  `Assoc [
    "max_stable_bytes", `String (Int64.to_string t.max_stable_bytes);
    "max_assets_bytes", `String (Int64.to_string t.max_assets_bytes);
    "max_inline_value", `String (Int64.to_string t.max_inline_value);
    "max_wasm_bytes", `String (Int64.to_string t.max_wasm_bytes);
  ]

let limits_of_yojson = function
  | `Assoc fields ->
    let read_i64 key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok (Int64.of_string s)
      | _ -> Error ("missing limits field: " ^ key)
    in
    begin
      match read_i64 "max_stable_bytes", read_i64 "max_assets_bytes",
        read_i64 "max_inline_value", read_i64 "max_wasm_bytes" with
      | Ok max_stable_bytes, Ok max_assets_bytes, Ok max_inline_value, Ok max_wasm_bytes ->
        Ok { max_stable_bytes; max_assets_bytes; max_inline_value; max_wasm_bytes }
      | Error e, _, _, _ | _, Error e, _, _ | _, _, Error e, _ | _, _, _, Error e ->
        Error e
    end
  | _ -> Error "limits must be a json object"

let yojson_of_stable_entry (t : stable_entry) =
  let value_json =
    match t.value with
    | Inline value_inline ->
      `Assoc [
        "value_kind", `String "inline";
        "value_inline", `String value_inline;
      ]
    | Blob_ref { blob_hash; value_size } ->
      `Assoc [
        "value_kind", `String "blob_ref";
        "value_blob_hash", `String blob_hash;
        "value_size", `String (Int64.to_string value_size);
      ]
  in
  `Assoc [
    "raw_key", `String t.raw_key;
    "key_hash", `String t.key_hash;
    "value", value_json;
  ]

let stable_entry_of_yojson = function
  | `Assoc fields ->
    let read_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok s
      | _ -> Error ("missing stable entry field: " ^ key)
    in
    let read_value = function
      | `Assoc value_fields -> begin
          match List.assoc_opt "value_kind" value_fields with
          | Some (`String "inline") ->
            begin
              match List.assoc_opt "value_inline" value_fields with
              | Some (`String value_inline) -> Ok (Inline value_inline)
              | _ -> Error "missing stable inline value"
            end
          | Some (`String "blob_ref") ->
            begin
              match List.assoc_opt "value_blob_hash" value_fields, List.assoc_opt "value_size" value_fields with
              | Some (`String blob_hash), Some (`String value_size) ->
                Ok (Blob_ref {
                  blob_hash;
                  value_size = Int64.of_string value_size;
                })
              | _ -> Error "missing stable blob ref fields"
            end
          | Some (`String other) -> Error ("unknown stable value kind: " ^ other)
          | _ -> Error "missing stable value kind"
        end
      | _ -> Error "stable value must be a json object"
    in
    begin
      match read_string "raw_key", read_string "key_hash", List.assoc_opt "value" fields with
      | Ok raw_key, Ok key_hash, Some value_json ->
        begin
          match read_value value_json with
          | Ok value -> Ok { raw_key; key_hash; value }
          | Error e -> Error e
        end
      | Error e, _, _ | _, Error e, _ -> Error e
      | _, _, None -> Error "missing stable value object"
    end
  | _ -> Error "stable entry must be a json object"

let yojson_of_asset_meta (t : asset_meta) =
  `Assoc [
    "path_key", `String t.path_key;
    "canonical_path", `String t.canonical_path;
    "content_type", `String t.content_type;
    "encoding", `String t.encoding;
    "size_bytes", `String (Int64.to_string t.size_bytes);
    "blob_hash", `String t.blob_hash;
    "body_mode", `String (string_of_resource_mode t.body_mode);
    "plaintext_hash",
    begin
      match t.plaintext_hash with
      | Some s -> `String s
      | None -> `Null
    end;
    "key_id",
    begin
      match t.key_id with
      | Some s -> `String s
      | None -> `Null
    end;
    "padding_class",
    begin
      match t.padding_class with
      | Some s -> `String s
      | None -> `Null
    end;
    "resource_key", `String t.resource_key;
    "locator_mode", `String (string_of_locator_mode t.locator_mode);
    "slot_ref",
    begin
      match t.slot_ref with
      | Some s -> `String s
      | None -> `Null
    end;
    "activate_after_epoch",
    begin
      match t.activate_after_epoch with
      | Some n -> `String (Int64.to_string n)
      | None -> `Null
    end;
    "expire_after_epoch",
    begin
      match t.expire_after_epoch with
      | Some n -> `String (Int64.to_string n)
      | None -> `Null
    end;
    "metadata_mode", `String (string_of_metadata_mode t.metadata_mode);
  ]

let asset_meta_of_yojson = function
  | `Assoc fields ->
    let read_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok s
      | _ -> Error ("missing asset meta field: " ^ key)
    in
    let read_optional_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok (Some s)
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid asset meta field: " ^ key)
    in
    let read_optional_i64 key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok (Some (Int64.of_string s))
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid asset meta field: " ^ key)
    in
    begin
      match read_string "path_key", read_string "canonical_path", read_string "content_type",
        read_string "encoding", read_string "size_bytes", read_string "blob_hash",
        read_string "body_mode", read_optional_string "plaintext_hash",
        read_optional_string "key_id", read_optional_string "padding_class",
        read_string "resource_key", read_optional_string "locator_mode",
        read_optional_string "slot_ref", read_optional_i64 "activate_after_epoch",
        read_optional_i64 "expire_after_epoch", read_optional_string "metadata_mode" with
      | Ok path_key, Ok canonical_path, Ok content_type, Ok encoding, Ok size_bytes, Ok blob_hash,
        Ok body_mode_s, Ok plaintext_hash, Ok key_id, Ok padding_class, Ok resource_key,
        Ok locator_mode_s, Ok slot_ref, Ok activate_after_epoch, Ok expire_after_epoch,
        Ok metadata_mode_s ->
        begin
          let locator_mode =
            match locator_mode_s with
            | Some raw_mode -> locator_mode_of_string raw_mode
            | None -> Ok Path_locator
          in
          let metadata_mode =
            match metadata_mode_s with
            | Some raw_mode -> metadata_mode_of_string raw_mode
            | None -> Ok Metadata_reveal
          in
          match resource_mode_of_string body_mode_s, locator_mode, metadata_mode with
          | Ok body_mode, Ok locator_mode, Ok metadata_mode ->
            Ok {
              path_key;
              canonical_path;
              content_type;
              encoding;
              size_bytes = Int64.of_string size_bytes;
              blob_hash;
              body_mode;
              plaintext_hash;
              key_id;
              padding_class;
              resource_key;
              locator_mode;
              slot_ref;
              activate_after_epoch;
              expire_after_epoch;
              metadata_mode;
            }
          | Error e, _, _
          | _, Error e, _
          | _, _, Error e ->
            Error e
        end
      | Error e, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _
      | _, Error e, _, _, _, _, _, _, _, _, _, _, _, _, _, _
      | _, _, Error e, _, _, _, _, _, _, _, _, _, _, _, _, _
      | _, _, _, Error e, _, _, _, _, _, _, _, _, _, _, _, _
      | _, _, _, _, Error e, _, _, _, _, _, _, _, _, _, _, _
      | _, _, _, _, _, Error e, _, _, _, _, _, _, _, _, _, _
      | _, _, _, _, _, _, Error e, _, _, _, _, _, _, _, _, _
      | _, _, _, _, _, _, _, Error e, _, _, _, _, _, _, _, _
      | _, _, _, _, _, _, _, _, Error e, _, _, _, _, _, _, _
      | _, _, _, _, _, _, _, _, _, Error e, _, _, _, _, _, _
      | _, _, _, _, _, _, _, _, _, _, Error e, _, _, _, _, _
      | _, _, _, _, _, _, _, _, _, _, _, Error e, _, _, _, _
      | _, _, _, _, _, _, _, _, _, _, _, _, Error e, _, _, _
      | _, _, _, _, _, _, _, _, _, _, _, _, _, Error e, _, _
      | _, _, _, _, _, _, _, _, _, _, _, _, _, _, Error e, _
      | _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Error e ->
        Error e
    end
  | _ -> Error "asset meta must be a json object"

let yojson_of_outbox_intent (t : outbox_intent) =
  `Assoc [
    "intent_id", `String t.intent_id;
    "created_epoch", `String (Int64.to_string t.created_epoch);
    "expiry_epoch", `String (Int64.to_string t.expiry_epoch);
    "relay_policy_hash", `String t.relay_policy_hash;
    "payload_hash", `String t.payload_hash;
    "ciphertext_blob_hash",
    begin
      match t.ciphertext_blob_hash with
      | Some s -> `String s
      | None -> `Null
    end;
    "delivery_key_id",
    begin
      match t.delivery_key_id with
      | Some s -> `String s
      | None -> `Null
    end;
    "max_response_bytes", `String (Int64.to_string t.max_response_bytes);
    "fee_budget", `String (Int64.to_string t.fee_budget);
    "route_hint",
    begin
      match t.route_hint with
      | Some s -> `String s
      | None -> `Null
    end;
    "callback_policy_hash",
    begin
      match t.callback_policy_hash with
      | Some s -> `String s
      | None -> `Null
    end;
  ]

let yojson_of_outbox_open_payload (t : outbox_open_payload) =
  `Assoc [
    "intent_id", `String t.intent_id;
    "expiry_epoch", `String (Int64.to_string t.expiry_epoch);
    "relay_policy_hash", `String t.relay_policy_hash;
    "payload_hash", `String t.payload_hash;
    "ciphertext_blob_hash",
    begin
      match t.ciphertext_blob_hash with
      | Some s -> `String s
      | None -> `Null
    end;
    "delivery_key_id",
    begin
      match t.delivery_key_id with
      | Some s -> `String s
      | None -> `Null
    end;
    "max_response_bytes", `String (Int64.to_string t.max_response_bytes);
    "fee_budget", `String (Int64.to_string t.fee_budget);
    "route_hint",
    begin
      match t.route_hint with
      | Some s -> `String s
      | None -> `Null
    end;
    "callback_policy_hash",
    begin
      match t.callback_policy_hash with
      | Some s -> `String s
      | None -> `Null
    end;
  ]

let yojson_of_relay_claim (t : relay_claim) =
  `Assoc [
    "intent_id", `String t.intent_id;
    "relay_id", `String t.relay_id;
    "claim_epoch", `String (Int64.to_string t.claim_epoch);
    "claim_expiry_epoch", `String (Int64.to_string t.claim_expiry_epoch);
    "signature", `String t.signature;
  ]

let yojson_of_relay_cancel (t : relay_cancel) =
  `Assoc [
    "intent_id", `String t.intent_id;
    "relay_id", `String t.relay_id;
    "cancel_epoch", `String (Int64.to_string t.cancel_epoch);
    "reason", `String (string_of_outbox_resolution_reason t.reason);
    "related_key_id",
    begin
      match t.related_key_id with
      | Some s -> `String s
      | None -> `Null
    end;
    "signature", `String t.signature;
  ]

let yojson_of_relay_cancel_payload (t : relay_cancel_payload) =
  yojson_of_relay_cancel t

let yojson_of_outbox_resolution (t : outbox_resolution) =
  `Assoc [
    "intent_id", `String t.intent_id;
    "status", `String (string_of_outbox_status t.status);
    "reason", `String (string_of_outbox_resolution_reason t.reason);
    "resolved_epoch", `String (Int64.to_string t.resolved_epoch);
    "actor_id",
    begin
      match t.actor_id with
      | Some s -> `String s
      | None -> `Null
    end;
    "related_key_id",
    begin
      match t.related_key_id with
      | Some s -> `String s
      | None -> `Null
    end;
  ]

let outbox_intent_of_yojson = function
  | `Assoc fields ->
    let read_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok s
      | _ -> Error ("missing outbox intent field: " ^ key)
    in
    let read_optional_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok (Some s)
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid optional outbox field: " ^ key)
    in
    begin
      match read_string "intent_id", read_string "created_epoch", read_string "expiry_epoch",
        read_string "relay_policy_hash", read_string "payload_hash",
        read_optional_string "ciphertext_blob_hash", read_optional_string "delivery_key_id",
        read_string "max_response_bytes",
        read_string "fee_budget", read_optional_string "route_hint",
        read_optional_string "callback_policy_hash" with
      | Ok intent_id, Ok created_epoch, Ok expiry_epoch, Ok relay_policy_hash, Ok payload_hash,
        Ok ciphertext_blob_hash, Ok delivery_key_id, Ok max_response_bytes, Ok fee_budget, Ok route_hint,
        Ok callback_policy_hash ->
        Ok {
          intent_id;
          created_epoch = Int64.of_string created_epoch;
          expiry_epoch = Int64.of_string expiry_epoch;
          relay_policy_hash;
          payload_hash;
          ciphertext_blob_hash;
          delivery_key_id;
          max_response_bytes = Int64.of_string max_response_bytes;
          fee_budget = Int64.of_string fee_budget;
          route_hint;
          callback_policy_hash;
        }
      | Error e, _, _, _, _, _, _, _, _, _, _
      | _, Error e, _, _, _, _, _, _, _, _, _
      | _, _, Error e, _, _, _, _, _, _, _, _
      | _, _, _, Error e, _, _, _, _, _, _, _
      | _, _, _, _, Error e, _, _, _, _, _, _
      | _, _, _, _, _, Error e, _, _, _, _, _
      | _, _, _, _, _, _, Error e, _, _, _, _
      | _, _, _, _, _, _, _, Error e, _, _, _
      | _, _, _, _, _, _, _, _, Error e, _, _
      | _, _, _, _, _, _, _, _, _, Error e, _
      | _, _, _, _, _, _, _, _, _, _, Error e ->
        Error e
    end
  | _ -> Error "outbox intent must be a json object"

let outbox_open_payload_of_yojson = function
  | `Assoc fields ->
    let read_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok s
      | _ -> Error ("missing outbox open payload field: " ^ key)
    in
    let read_optional_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok (Some s)
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid outbox open payload field: " ^ key)
    in
    begin
      match read_string "intent_id", read_string "expiry_epoch", read_string "relay_policy_hash",
        read_string "payload_hash", read_optional_string "ciphertext_blob_hash",
        read_optional_string "delivery_key_id",
        read_string "max_response_bytes", read_string "fee_budget",
        read_optional_string "route_hint", read_optional_string "callback_policy_hash" with
      | Ok intent_id, Ok expiry_epoch, Ok relay_policy_hash, Ok payload_hash,
        Ok ciphertext_blob_hash, Ok delivery_key_id, Ok max_response_bytes, Ok fee_budget,
        Ok route_hint, Ok callback_policy_hash ->
        Ok {
          intent_id;
          expiry_epoch = Int64.of_string expiry_epoch;
          relay_policy_hash;
          payload_hash;
          ciphertext_blob_hash;
          delivery_key_id;
          max_response_bytes = Int64.of_string max_response_bytes;
          fee_budget = Int64.of_string fee_budget;
          route_hint;
          callback_policy_hash;
        }
      | Error e, _, _, _, _, _, _, _, _, _
      | _, Error e, _, _, _, _, _, _, _, _
      | _, _, Error e, _, _, _, _, _, _, _
      | _, _, _, Error e, _, _, _, _, _, _
      | _, _, _, _, Error e, _, _, _, _, _
      | _, _, _, _, _, Error e, _, _, _, _
      | _, _, _, _, _, _, Error e, _, _, _
      | _, _, _, _, _, _, _, Error e, _, _
      | _, _, _, _, _, _, _, _, Error e, _
      | _, _, _, _, _, _, _, _, _, Error e ->
        Error e
    end
  | _ -> Error "outbox open payload must be a json object"

let relay_claim_of_yojson = function
  | `Assoc fields ->
    let read_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok s
      | _ -> Error ("missing relay claim field: " ^ key)
    in
    begin
      match read_string "intent_id", read_string "relay_id", read_string "claim_epoch",
        read_string "claim_expiry_epoch", read_string "signature" with
      | Ok intent_id, Ok relay_id, Ok claim_epoch, Ok claim_expiry_epoch, Ok signature ->
        Ok {
          intent_id;
          relay_id;
          claim_epoch = Int64.of_string claim_epoch;
          claim_expiry_epoch = Int64.of_string claim_expiry_epoch;
          signature;
        }
      | Error e, _, _, _, _
      | _, Error e, _, _, _
      | _, _, Error e, _, _
      | _, _, _, Error e, _
      | _, _, _, _, Error e ->
        Error e
    end
  | _ -> Error "relay claim must be a json object"

let relay_cancel_of_yojson = function
  | `Assoc fields ->
    let read_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok s
      | _ -> Error ("missing relay cancel field: " ^ key)
    in
    let read_optional_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok (Some s)
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid relay cancel field: " ^ key)
    in
    begin
      match read_string "intent_id", read_string "relay_id", read_string "cancel_epoch",
            read_string "reason", read_optional_string "related_key_id", read_string "signature" with
      | Ok intent_id, Ok relay_id, Ok cancel_epoch, Ok reason_raw, Ok related_key_id, Ok signature ->
        begin
          match outbox_resolution_reason_of_string reason_raw with
          | Ok reason ->
            Ok {
              intent_id;
              relay_id;
              cancel_epoch = Int64.of_string cancel_epoch;
              reason;
              related_key_id;
              signature;
            }
          | Error _ as e ->
            e
        end
      | Error e, _, _, _, _, _
      | _, Error e, _, _, _, _
      | _, _, Error e, _, _, _
      | _, _, _, Error e, _, _
      | _, _, _, _, Error e, _
      | _, _, _, _, _, Error e ->
        Error e
    end
  | _ -> Error "relay cancel must be a json object"

let relay_cancel_payload_of_yojson =
  relay_cancel_of_yojson

let outbox_resolution_of_yojson = function
  | `Assoc fields ->
    let read_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok s
      | _ -> Error ("missing outbox resolution field: " ^ key)
    in
    let read_optional_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok (Some s)
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid outbox resolution field: " ^ key)
    in
    begin
      match read_string "intent_id", read_string "status", read_string "reason",
            read_string "resolved_epoch", read_optional_string "actor_id", read_optional_string "related_key_id" with
      | Ok intent_id, Ok status_raw, Ok reason_raw, Ok resolved_epoch, Ok actor_id, Ok related_key_id ->
        begin
          match outbox_status_of_string status_raw, outbox_resolution_reason_of_string reason_raw with
          | Ok status, Ok reason ->
            Ok {
              intent_id;
              status;
              reason;
              resolved_epoch = Int64.of_string resolved_epoch;
              actor_id;
              related_key_id;
            }
          | Error e, _
          | _, Error e ->
            Error e
        end
      | Error e, _, _, _, _, _
      | _, Error e, _, _, _, _
      | _, _, Error e, _, _, _
      | _, _, _, Error e, _, _
      | _, _, _, _, Error e, _
      | _, _, _, _, _, Error e ->
        Error e
    end
  | _ -> Error "outbox resolution must be a json object"

let yojson_of_ingress_packet (t : ingress_packet) =
  `Assoc [
    "intent_id", `String t.intent_id;
    "relay_id", `String t.relay_id;
    "ingress_nonce", `String (Int64.to_string t.ingress_nonce);
    "result_code", `Int t.result_code;
    "response_payload_hash", `String t.response_payload_hash;
    "response_size_bytes", `String (Int64.to_string t.response_size_bytes);
    "response_ciphertext_blob_hash",
    begin
      match t.response_ciphertext_blob_hash with
      | Some s -> `String s
      | None -> `Null
    end;
    "external_receipt_hash",
    begin
      match t.external_receipt_hash with
      | Some s -> `String s
      | None -> `Null
    end;
    "signature", `String t.signature;
  ]

let yojson_of_ingress_commit_payload (t : ingress_commit_payload) =
  yojson_of_ingress_packet {
    intent_id = t.intent_id;
    relay_id = t.relay_id;
    ingress_nonce = t.ingress_nonce;
    result_code = t.result_code;
    response_payload_hash = t.response_payload_hash;
    response_size_bytes = t.response_size_bytes;
    response_ciphertext_blob_hash = t.response_ciphertext_blob_hash;
    external_receipt_hash = t.external_receipt_hash;
    signature = t.signature;
  }

let ingress_signature_subject ~circle_id ~(packet : ingress_packet) =
  String.concat "|"
    [
      "octra_circle_ingress_commit";
      circle_id;
      packet.intent_id;
      packet.relay_id;
      Int64.to_string packet.ingress_nonce;
      string_of_int packet.result_code;
      packet.response_payload_hash;
      Int64.to_string packet.response_size_bytes;
      Option.value ~default:"" packet.response_ciphertext_blob_hash;
      Option.value ~default:"" packet.external_receipt_hash;
    ]

let ingress_commit_signature_subject ~circle_id (payload : ingress_commit_payload) =
  ingress_signature_subject
    ~circle_id
    ~packet:{
      intent_id = payload.intent_id;
      relay_id = payload.relay_id;
      ingress_nonce = payload.ingress_nonce;
      result_code = payload.result_code;
      response_payload_hash = payload.response_payload_hash;
      response_size_bytes = payload.response_size_bytes;
      response_ciphertext_blob_hash = payload.response_ciphertext_blob_hash;
      external_receipt_hash = payload.external_receipt_hash;
      signature = payload.signature;
    }

let ingress_packet_of_yojson json : (ingress_packet, string) result =
  match json with
  | `Assoc fields ->
    let read_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok s
      | _ -> Error ("missing ingress packet field: " ^ key)
    in
    let read_optional_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok (Some s)
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid ingress packet field: " ^ key)
    in
    begin
      match read_string "intent_id", read_string "relay_id", read_string "ingress_nonce",
        List.assoc_opt "result_code" fields, read_string "response_payload_hash", read_string "response_size_bytes",
        read_optional_string "response_ciphertext_blob_hash",
        read_optional_string "external_receipt_hash", read_string "signature" with
      | Ok intent_id, Ok relay_id, Ok ingress_nonce, Some (`Int result_code),
        Ok response_payload_hash, Ok response_size_bytes, Ok response_ciphertext_blob_hash,
        Ok external_receipt_hash, Ok signature ->
        Ok {
          intent_id;
          relay_id;
          ingress_nonce = Int64.of_string ingress_nonce;
          result_code;
          response_payload_hash;
          response_size_bytes = Int64.of_string response_size_bytes;
          response_ciphertext_blob_hash;
          external_receipt_hash;
          signature;
        }
      | Ok _, Ok _, Ok _, _, Ok _, Ok _, Ok _, Ok _, Ok _ ->
        Error "missing ingress packet field: result_code"
      | Error e, _, _, _, _, _, _, _, _
      | _, Error e, _, _, _, _, _, _, _
      | _, _, Error e, _, _, _, _, _, _
      | _, _, _, _, Error e, _, _, _, _
      | _, _, _, _, _, Error e, _, _, _
      | _, _, _, _, _, _, Error e, _, _
      | _, _, _, _, _, _, _, Error e, _
      | _, _, _, _, _, _, _, _, Error e ->
        Error e
    end
  | _ -> Error "ingress packet must be a json object"

let ingress_commit_payload_of_yojson json =
  match ingress_packet_of_yojson json with
  | Error e -> Error e
  | Ok packet ->
    Ok {
      intent_id = packet.intent_id;
      relay_id = packet.relay_id;
      ingress_nonce = packet.ingress_nonce;
      result_code = packet.result_code;
      response_payload_hash = packet.response_payload_hash;
      response_size_bytes = packet.response_size_bytes;
      response_ciphertext_blob_hash = packet.response_ciphertext_blob_hash;
      external_receipt_hash = packet.external_receipt_hash;
      signature = packet.signature;
    }

let yojson_of_circle_info (t : circle_info) =
  `Assoc [
    "circle_id", `String t.circle_id;
    "runtime", `String (string_of_runtime t.runtime);
    "version", `String (Int64.to_string t.version);
    "owner", `String t.owner;
    "code_hash", `String t.code_hash;
    "stable_root", `String t.stable_root;
    "assets_root", `String t.assets_root;
    "privacy_class", `String (string_of_privacy_class t.privacy_class);
    "browser_mode", `String (string_of_browser_mode t.browser_mode);
    "resource_mode", `String (string_of_resource_mode t.resource_mode);
    "policy_hash",
    begin
      match t.policy_hash with
      | Some s -> `String s
      | None -> `Null
    end;
    "members_root",
    begin
      match t.members_root with
      | Some s -> `String s
      | None -> `Null
    end;
    "export_policy",
    begin
      match t.export_policy with
      | Some s -> `String s
      | None -> `Null
    end;
    "limits", yojson_of_limits t.limits;
  ]

let yojson_of_deploy_payload (t : deploy_payload) =
  `Assoc [
    "runtime", `String (string_of_runtime t.runtime);
    "privacy_class", `String (string_of_privacy_class t.privacy_class);
    "browser_mode", `String (string_of_browser_mode t.browser_mode);
    "resource_mode", `String (string_of_resource_mode t.resource_mode);
    "code_b64",
    begin
      match t.code_b64 with
      | Some s -> `String s
      | None -> `Null
    end;
    "policy_hash",
    begin
      match t.policy_hash with
      | Some s -> `String s
      | None -> `Null
    end;
    "members_root",
    begin
      match t.members_root with
      | Some s -> `String s
      | None -> `Null
    end;
    "export_policy",
    begin
      match t.export_policy with
      | Some s -> `String s
      | None -> `Null
    end;
    "limits", yojson_of_limits t.limits;
  ]

let yojson_of_program_update_payload (t : program_update_payload) =
  `Assoc [
    "code_b64", `String t.code_b64;
  ]

let deploy_payload_of_yojson = function
  | `Assoc fields ->
    let read_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok s
      | _ -> Error ("missing deploy payload field: " ^ key)
    in
    let read_optional_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok (Some s)
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid deploy payload field: " ^ key)
    in
    let limits_json =
      match List.assoc_opt "limits" fields with
      | Some json -> Ok json
      | None -> Error "missing deploy payload field: limits"
    in
    begin
      match read_string "runtime", read_string "privacy_class", read_string "browser_mode",
        read_string "resource_mode",
        read_optional_string "code_b64",
        read_optional_string "policy_hash", read_optional_string "members_root",
        read_optional_string "export_policy", limits_json with
      | Ok runtime_s, Ok privacy_class_s, Ok browser_mode_s, Ok resource_mode_s, Ok code_b64, Ok policy_hash,
        Ok members_root, Ok export_policy, Ok limits_json ->
        begin
          match runtime_of_string runtime_s, privacy_class_of_string privacy_class_s,
            browser_mode_of_string browser_mode_s, resource_mode_of_string resource_mode_s,
            limits_of_yojson limits_json with
          | Ok runtime, Ok privacy_class, Ok browser_mode, Ok resource_mode, Ok limits ->
            Ok {
              runtime;
              privacy_class;
              browser_mode;
              resource_mode;
              code_b64;
              policy_hash;
              members_root;
              export_policy;
              limits;
            }
          | Error e, _, _, _, _
          | _, Error e, _, _, _
          | _, _, Error e, _, _
          | _, _, _, Error e, _
          | _, _, _, _, Error e ->
            Error e
        end
      | Error e, _, _, _, _, _, _, _, _
      | _, Error e, _, _, _, _, _, _, _
      | _, _, Error e, _, _, _, _, _, _
      | _, _, _, Error e, _, _, _, _, _
      | _, _, _, _, Error e, _, _, _, _
      | _, _, _, _, _, Error e, _, _, _
      | _, _, _, _, _, _, Error e, _, _
      | _, _, _, _, _, _, _, Error e, _
      | _, _, _, _, _, _, _, _, Error e ->
        Error e
    end
  | _ ->
    Error "deploy payload must be a json object"

let program_update_payload_of_yojson = function
  | `Assoc fields ->
    begin
      match List.assoc_opt "code_b64" fields with
      | Some (`String code_b64) when String.length code_b64 > 0 ->
        Ok { code_b64 }
      | Some (`String _) ->
        Error "invalid program update payload field: code_b64"
      | _ ->
        Error "missing program update payload field: code_b64"
    end
  | _ ->
    Error "program update payload must be a json object"

let yojson_of_asset_put_payload (t : asset_put_payload) =
  `Assoc [
    "path", `String t.path;
    "content_type", `String t.content_type;
    "encoding",
    begin
      match t.encoding with
      | Some s -> `String s
      | None -> `Null
    end;
  ]

let yojson_of_encrypted_asset_put_payload (t : encrypted_asset_put_payload) =
  `Assoc [
    "path",
    begin
      match t.path with
      | Some s -> `String s
      | None -> `Null
    end;
    "slot_ref",
    begin
      match t.slot_ref with
      | Some s -> `String s
      | None -> `Null
    end;
    "state_ref",
    begin
      match t.state_ref with
      | Some s -> `String s
      | None -> `Null
    end;
    "content_type", `String t.content_type;
    "encoding",
    begin
      match t.encoding with
      | Some s -> `String s
      | None -> `Null
    end;
    "key_id", `String t.key_id;
    "plaintext_hash", `String t.plaintext_hash;
    "padding_class",
    begin
      match t.padding_class with
      | Some s -> `String s
      | None -> `Null
    end;
    "activate_after_epoch",
    begin
      match t.activate_after_epoch with
      | Some n -> `String (Int64.to_string n)
      | None -> `Null
    end;
    "expire_after_epoch",
    begin
      match t.expire_after_epoch with
      | Some n -> `String (Int64.to_string n)
      | None -> `Null
    end;
    "metadata_mode",
    begin
      match t.metadata_mode with
      | Some mode -> `String (string_of_metadata_mode mode)
      | None -> `Null
    end;
  ]

let encrypted_asset_put_payload_of_sealed_slot_put_payload (t : sealed_slot_put_payload) = {
  path = None;
  slot_ref = t.slot_ref;
  state_ref = t.state_ref;
  content_type = t.content_type;
  encoding = t.encoding;
  key_id = t.key_id;
  plaintext_hash = t.plaintext_hash;
  padding_class = t.padding_class;
  activate_after_epoch = t.activate_after_epoch;
  expire_after_epoch = t.expire_after_epoch;
  metadata_mode = t.metadata_mode;
}

let yojson_of_sealed_slot_put_payload (t : sealed_slot_put_payload) =
  yojson_of_encrypted_asset_put_payload (encrypted_asset_put_payload_of_sealed_slot_put_payload t)

let yojson_of_slot_policy_put_payload (t : slot_policy_put_payload) =
  `Assoc [
    "slot_ref",
    begin
      match t.slot_ref with
      | Some value -> `String value
      | None -> `Null
    end;
    "state_ref",
    begin
      match t.state_ref with
      | Some value -> `String value
      | None -> `Null
    end;
    "delivery_key_id",
    begin
      match t.delivery_key_id with
      | Some value -> `String value
      | None -> `Null
    end;
    "activate_after_epoch",
    begin
      match t.activate_after_epoch with
      | Some n -> `String (Int64.to_string n)
      | None -> `Null
    end;
    "expire_after_epoch",
    begin
      match t.expire_after_epoch with
      | Some n -> `String (Int64.to_string n)
      | None -> `Null
    end;
    "tombstone", `Bool t.tombstone;
    "revoked", `Bool t.revoked;
  ]

let yojson_of_state_descriptor_put_payload =
  Circle_state_descriptor.yojson_of_put_payload

let yojson_of_transport_policy_put_payload (t : transport_policy_put_payload) =
  Circle_transport_policy.yojson_of_put_payload t

let yojson_of_hfhe_policy_put_payload (t : hfhe_policy_put_payload) =
  Circle_hfhe_policy.yojson_of_put_payload t

let yojson_of_key_policy_put_payload (t : key_policy_put_payload) =
  Circle_key_policy.yojson_of_put_payload t

let yojson_of_key_grant_payload (t : key_grant_payload) =
  `Assoc [
    "key_id", `String t.key_id;
    "activate_after_epoch",
    begin
      match t.activate_after_epoch with
      | Some value -> `String (Int64.to_string value)
      | None -> `Null
    end;
    "expire_after_epoch",
    begin
      match t.expire_after_epoch with
      | Some value -> `String (Int64.to_string value)
      | None -> `Null
    end;
  ]

let yojson_of_key_extend_payload (t : key_extend_payload) =
  `Assoc [
    "key_id", `String t.key_id;
    "expire_after_epoch", `String (Int64.to_string t.expire_after_epoch);
  ]

let yojson_of_key_revoke_payload (t : key_revoke_payload) =
  `Assoc [
    "key_id", `String t.key_id;
  ]

let yojson_of_key_erase_payload (t : key_erase_payload) =
  `Assoc [
    "key_id", `String t.key_id;
  ]

let asset_put_payload_of_yojson = function
  | `Assoc fields ->
    let read_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok s
      | _ -> Error ("missing asset put payload field: " ^ key)
    in
    let read_optional_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok (Some s)
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid asset put payload field: " ^ key)
    in
    begin
      match read_string "path", read_string "content_type", read_optional_string "encoding" with
      | Ok path, Ok content_type, Ok encoding ->
        Ok { path; content_type; encoding }
      | Error e, _, _
      | _, Error e, _
      | _, _, Error e ->
        Error e
    end
  | _ -> Error "asset put payload must be a json object"

let encrypted_asset_put_payload_of_yojson = function
  | `Assoc fields ->
    let read_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok s
      | _ -> Error ("missing encrypted asset put payload field: " ^ key)
    in
    let read_optional_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok (Some s)
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid encrypted asset put payload field: " ^ key)
    in
    let read_optional_i64 key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok (Some (Int64.of_string s))
      | Some (`Int n) -> Ok (Some (Int64.of_int n))
      | Some (`Intlit s) -> Ok (Some (Int64.of_string s))
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid encrypted asset put payload field: " ^ key)
    in
    begin
      match read_optional_string "path", read_optional_string "slot_ref", read_optional_string "state_ref",
        read_string "content_type", read_optional_string "encoding",
        read_string "key_id", read_string "plaintext_hash", read_optional_string "padding_class",
        read_optional_i64 "activate_after_epoch", read_optional_i64 "expire_after_epoch",
        read_optional_string "metadata_mode" with
      | Ok path, Ok slot_ref, Ok state_ref, Ok content_type, Ok encoding, Ok key_id, Ok plaintext_hash,
        Ok padding_class, Ok activate_after_epoch, Ok expire_after_epoch, Ok metadata_mode_s ->
        begin
          let metadata_mode =
            match metadata_mode_s with
            | Some raw_mode ->
              begin
                match metadata_mode_of_string raw_mode with
                | Ok mode -> Ok (Some mode)
                | Error e -> Error e
              end
            | None -> Ok None
          in
          match metadata_mode with
          | Ok metadata_mode ->
            Ok {
              path;
              slot_ref;
              state_ref;
              content_type;
              encoding;
              key_id;
              plaintext_hash;
              padding_class;
              activate_after_epoch;
              expire_after_epoch;
              metadata_mode;
            }
          | Error e ->
            Error e
        end
      | Error e, _, _, _, _, _, _, _, _, _, _
      | _, Error e, _, _, _, _, _, _, _, _, _
      | _, _, Error e, _, _, _, _, _, _, _, _
      | _, _, _, Error e, _, _, _, _, _, _, _
      | _, _, _, _, Error e, _, _, _, _, _, _
      | _, _, _, _, _, Error e, _, _, _, _, _
      | _, _, _, _, _, _, Error e, _, _, _, _
      | _, _, _, _, _, _, _, Error e, _, _, _
      | _, _, _, _, _, _, _, _, Error e, _, _
      | _, _, _, _, _, _, _, _, _, Error e, _
      | _, _, _, _, _, _, _, _, _, _, Error e ->
        Error e
    end
  | _ -> Error "encrypted asset put payload must be a json object"

let sealed_slot_put_payload_of_yojson json =
  match encrypted_asset_put_payload_of_yojson json with
  | Error e -> Error e
  | Ok payload ->
    begin
      match payload.path, payload.slot_ref, payload.state_ref with
      | Some _, _, _ ->
        Error "sealed slot put payload must not include path"
      | None, None, None ->
        Error "sealed slot put payload requires slot_ref or state_ref"
      | None, Some _, Some _ ->
        Error "sealed slot put payload requires exactly one of slot_ref or state_ref"
      | None, Some slot_ref, None ->
        Ok {
          slot_ref = Some slot_ref;
          state_ref = None;
          content_type = payload.content_type;
          encoding = payload.encoding;
          key_id = payload.key_id;
          plaintext_hash = payload.plaintext_hash;
          padding_class = payload.padding_class;
          activate_after_epoch = payload.activate_after_epoch;
          expire_after_epoch = payload.expire_after_epoch;
          metadata_mode = payload.metadata_mode;
        }
      | None, None, Some state_ref ->
        Ok {
          slot_ref = None;
          state_ref = Some state_ref;
          content_type = payload.content_type;
          encoding = payload.encoding;
          key_id = payload.key_id;
          plaintext_hash = payload.plaintext_hash;
          padding_class = payload.padding_class;
          activate_after_epoch = payload.activate_after_epoch;
          expire_after_epoch = payload.expire_after_epoch;
          metadata_mode = payload.metadata_mode;
        }
    end

let slot_policy_put_payload_of_yojson = function
  | `Assoc fields ->
    let read_optional_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok (Some s)
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid slot policy payload field: " ^ key)
    in
    let read_optional_i64 key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok (Some (Int64.of_string s))
      | Some (`Int n) -> Ok (Some (Int64.of_int n))
      | Some (`Intlit s) -> Ok (Some (Int64.of_string s))
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid slot policy payload field: " ^ key)
    in
    let read_bool_default key =
      match List.assoc_opt key fields with
      | Some (`Bool b) -> Ok b
      | None -> Ok false
      | _ -> Error ("invalid slot policy payload field: " ^ key)
    in
    begin
      match read_optional_string "slot_ref",
        read_optional_string "state_ref",
        read_optional_string "delivery_key_id",
        read_optional_i64 "activate_after_epoch",
        read_optional_i64 "expire_after_epoch",
        read_bool_default "tombstone",
        read_bool_default "revoked" with
      | Ok slot_ref, Ok state_ref, Ok delivery_key_id, Ok activate_after_epoch, Ok expire_after_epoch, Ok tombstone, Ok revoked ->
        begin
          match slot_ref, state_ref with
          | None, None ->
            Error "slot policy payload requires slot_ref or state_ref"
          | Some _, Some _ ->
            Error "slot policy payload requires exactly one of slot_ref or state_ref"
          | _ ->
            Ok {
              slot_ref;
              state_ref;
              delivery_key_id;
              activate_after_epoch;
              expire_after_epoch;
              tombstone;
              revoked;
            }
        end
      | Error e, _, _, _, _, _, _
      | _, Error e, _, _, _, _, _
      | _, _, Error e, _, _, _, _
      | _, _, _, Error e, _, _, _
      | _, _, _, _, Error e, _, _
      | _, _, _, _, _, Error e, _
      | _, _, _, _, _, _, Error e ->
        Error e
    end
  | _ -> Error "slot policy payload must be a json object"

let state_descriptor_put_payload_of_yojson =
  Circle_state_descriptor.put_payload_of_yojson

let transport_policy_put_payload_of_yojson =
  Circle_transport_policy.put_payload_of_yojson

let hfhe_policy_put_payload_of_yojson =
  Circle_hfhe_policy.put_payload_of_yojson

let key_policy_put_payload_of_yojson =
  Circle_key_policy.put_payload_of_yojson

let key_grant_payload_of_yojson = function
  | `Assoc fields ->
    let read_string key =
      match List.assoc_opt key fields with
      | Some (`String value) -> Ok value
      | _ -> Error ("missing key grant field: " ^ key)
    in
    let read_optional_i64 key =
      match List.assoc_opt key fields with
      | Some (`String value) -> Ok (Some (Int64.of_string value))
      | Some (`Int value) -> Ok (Some (Int64.of_int value))
      | Some (`Intlit value) -> Ok (Some (Int64.of_string value))
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid key grant field: " ^ key)
    in
    begin
      match read_string "key_id",
            read_optional_i64 "activate_after_epoch",
            read_optional_i64 "expire_after_epoch" with
      | Ok key_id, Ok activate_after_epoch, Ok expire_after_epoch ->
        Ok {
          key_id;
          activate_after_epoch;
          expire_after_epoch;
        }
      | Error e, _, _
      | _, Error e, _
      | _, _, Error e ->
        Error e
    end
  | _ ->
    Error "key grant payload must be a json object"

let key_extend_payload_of_yojson = function
  | `Assoc fields ->
    let read_string key =
      match List.assoc_opt key fields with
      | Some (`String value) -> Ok value
      | _ -> Error ("missing key extend field: " ^ key)
    in
    begin
      match read_string "key_id", read_string "expire_after_epoch" with
      | Ok key_id, Ok expire_after_epoch ->
        Ok {
          key_id;
          expire_after_epoch = Int64.of_string expire_after_epoch;
        }
      | Error e, _
      | _, Error e ->
        Error e
    end
  | _ ->
    Error "key extend payload must be a json object"

let key_revoke_payload_of_yojson = function
  | `Assoc fields ->
    begin
      match List.assoc_opt "key_id" fields with
      | Some (`String key_id) ->
        Ok { key_id }
      | _ ->
        Error "missing key revoke field: key_id"
    end
  | _ ->
    Error "key revoke payload must be a json object"

let key_erase_payload_of_yojson = function
  | `Assoc fields ->
    begin
      match List.assoc_opt "key_id" fields with
      | Some (`String key_id) ->
        Ok { key_id }
      | _ ->
        Error "missing key erase field: key_id"
    end
  | _ ->
    Error "key erase payload must be a json object"

let circle_info_of_yojson = function
  | `Assoc fields ->
    let read_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok s
      | _ -> Error ("missing circle info field: " ^ key)
    in
    let read_optional_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Ok (Some s)
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid circle info field: " ^ key)
    in
    let limits_json =
      match List.assoc_opt "limits" fields with
      | Some json -> Ok json
      | None -> Error "missing circle info field: limits"
    in
    begin
      match read_string "circle_id", read_string "runtime", read_string "version",
        read_string "owner", read_string "code_hash", read_string "stable_root",
        read_string "assets_root", read_string "privacy_class", read_string "browser_mode",
        read_string "resource_mode",
        read_optional_string "policy_hash", read_optional_string "members_root",
        read_optional_string "export_policy", limits_json with
      | Ok circle_id, Ok runtime_s, Ok version, Ok owner, Ok code_hash, Ok stable_root,
        Ok assets_root, Ok privacy_class_s, Ok browser_mode_s, Ok resource_mode_s, Ok policy_hash, Ok members_root,
        Ok export_policy, Ok limits_json ->
        begin
          match runtime_of_string runtime_s, privacy_class_of_string privacy_class_s,
            browser_mode_of_string browser_mode_s, resource_mode_of_string resource_mode_s,
            limits_of_yojson limits_json with
          | Ok runtime, Ok privacy_class, Ok browser_mode, Ok resource_mode, Ok limits ->
            Ok {
              circle_id;
              runtime;
              version = Int64.of_string version;
              owner;
              code_hash;
              stable_root;
              assets_root;
              privacy_class;
              browser_mode;
              resource_mode;
              policy_hash;
              members_root;
              export_policy;
              limits;
            }
          | Error e, _, _, _, _
          | _, Error e, _, _, _
          | _, _, Error e, _, _
          | _, _, _, Error e, _
          | _, _, _, _, Error e ->
            Error e
        end
      | Error e, _, _, _, _, _, _, _, _, _, _, _, _, _
      | _, Error e, _, _, _, _, _, _, _, _, _, _, _, _
      | _, _, Error e, _, _, _, _, _, _, _, _, _, _, _
      | _, _, _, Error e, _, _, _, _, _, _, _, _, _, _
      | _, _, _, _, Error e, _, _, _, _, _, _, _, _, _
      | _, _, _, _, _, Error e, _, _, _, _, _, _, _, _
      | _, _, _, _, _, _, Error e, _, _, _, _, _, _, _
      | _, _, _, _, _, _, _, Error e, _, _, _, _, _, _
      | _, _, _, _, _, _, _, _, Error e, _, _, _, _, _
      | _, _, _, _, _, _, _, _, _, Error e, _, _, _, _
      | _, _, _, _, _, _, _, _, _, _, Error e, _, _, _
      | _, _, _, _, _, _, _, _, _, _, _, Error e, _, _
      | _, _, _, _, _, _, _, _, _, _, _, _, Error e, _
      | _, _, _, _, _, _, _, _, _, _, _, _, _, Error e ->
        Error e
    end
  | _ -> Error "circle info must be a json object"

let encode_u32be n =
  let buf = Bytes.create 4 in
  Bytes.set buf 0 (Char.chr ((n lsr 24) land 0xff));
  Bytes.set buf 1 (Char.chr ((n lsr 16) land 0xff));
  Bytes.set buf 2 (Char.chr ((n lsr 8) land 0xff));
  Bytes.set buf 3 (Char.chr (n land 0xff));
  Bytes.unsafe_to_string buf

let encode_u64be n =
  let buf = Bytes.create 8 in
  Bytes.set buf 0 (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical n 56) 0xffL)));
  Bytes.set buf 1 (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical n 48) 0xffL)));
  Bytes.set buf 2 (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical n 40) 0xffL)));
  Bytes.set buf 3 (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical n 32) 0xffL)));
  Bytes.set buf 4 (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical n 24) 0xffL)));
  Bytes.set buf 5 (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical n 16) 0xffL)));
  Bytes.set buf 6 (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical n 8) 0xffL)));
  Bytes.set buf 7 (Char.chr (Int64.to_int (Int64.logand n 0xffL)));
  Bytes.unsafe_to_string buf

let h256_raw tag parts =
  let h = Digestif.SHA256.init () in
  let h = Digestif.SHA256.feed_string h tag in
  let h = Digestif.SHA256.feed_string h "\x00" in
  let h =
    List.fold_left
      (fun acc part ->
        let acc = Digestif.SHA256.feed_string acc (encode_u32be (String.length part)) in
        Digestif.SHA256.feed_string acc part)
      h
      parts
  in
  Digestif.SHA256.get h |> Digestif.SHA256.to_raw_string

let h256_hex tag parts =
  h256_raw tag parts |> Digestif.SHA256.of_raw_string |> Digestif.SHA256.to_hex

let sha256_hex data =
  Digestif.SHA256.digest_string data |> Digestif.SHA256.to_hex

let stable_key_hash key =
  h256_hex "octra:stable_key:v1" [key]

let make_stable_entry raw_key value =
  {
    raw_key;
    key_hash = stable_key_hash raw_key;
    value;
  }

let deploy_payload_hash payload =
  h256_hex "octra:circle_deploy_payload:v1"
    [Yojson.Safe.to_string (yojson_of_deploy_payload payload)]

let circle_id_of_deploy ~deployer ~nonce payload =
  let seed =
    h256_raw "octra:circle_deploy_id:v1"
      [deployer; encode_u64be (Int64.of_int nonce); deploy_payload_hash payload]
  in
  let base58_hash = Crypto.Base58.encode seed in
  let base58_part =
    if String.length base58_hash >= 44 then String.sub base58_hash 0 44
    else
      let rec extend acc i =
        if String.length acc >= 44 then String.sub acc 0 44
        else extend (acc ^ String.make 1 base58_hash.[i mod String.length base58_hash]) (i + 1)
      in
      if String.length base58_hash = 0 then String.make 44 '1'
      else extend base58_hash 0
  in
  "oct" ^ base58_part

let circle_id_of_spawn ~parent ~caller ~tx_hash ~spawn_nonce ~owner_mode ~payload_json =
  let seed =
    h256_raw "octra:circle_spawn_id:v1"
      [
        parent;
        caller;
        tx_hash;
        encode_u64be (Int64.of_int spawn_nonce);
        string_of_spawn_owner owner_mode;
        payload_json;
      ]
  in
  let base58_hash = Crypto.Base58.encode seed in
  let base58_part =
    if String.length base58_hash >= 44 then String.sub base58_hash 0 44
    else
      let rec extend acc i =
        if String.length acc >= 44 then String.sub acc 0 44
        else extend (acc ^ String.make 1 base58_hash.[i mod String.length base58_hash]) (i + 1)
      in
      if String.length base58_hash = 0 then String.make 44 '1'
      else extend base58_hash 0
  in
  "oct" ^ base58_part

let resource_key_of_path ~circle_id ~canonical_path =
  h256_hex "octra:circle_resource_key:v1" [circle_id; canonical_path]

let is_hex_char = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let normalize_slot_ref raw_slot_ref =
  let slot_ref = String.lowercase_ascii (String.trim raw_slot_ref) in
  if String.length slot_ref <> 64 then
    Error "slot_ref must be a 64-char hex string"
  else if String.exists (fun c -> not (is_hex_char c)) slot_ref then
    Error "slot_ref must be a 64-char hex string"
  else
    Ok slot_ref

let slot_canonical_path slot_ref =
  "/_slots/" ^ slot_ref

let path_key_of_slot_ref raw_slot_ref =
  match normalize_slot_ref raw_slot_ref with
  | Error e -> Error e
  | Ok slot_ref ->
    let canonical_path = slot_canonical_path slot_ref in
    Ok (slot_ref, canonical_path, h256_hex "octra:asset_path:v1" [canonical_path])

let resource_key_of_slot_ref ~circle_id ~slot_ref =
  h256_hex "octra:circle_resource_key:slot:v1" [circle_id; slot_ref]

let normalize_state_ref raw_state_ref =
  let state_ref = String.lowercase_ascii (String.trim raw_state_ref) in
  if String.length state_ref <> 64 then
    Error "state_ref must be a 64-char hex string"
  else if String.exists (fun c -> not (is_hex_char c)) state_ref then
    Error "state_ref must be a 64-char hex string"
  else
    Ok state_ref

let state_canonical_path state_ref =
  "/_state/" ^ state_ref

let state_ref_of_canonical_path canonical_path =
  let prefix = "/_state/" in
  if not (String.starts_with ~prefix canonical_path) then
    None
  else
    let state_ref =
      String.sub canonical_path (String.length prefix) (String.length canonical_path - String.length prefix) in
    match normalize_state_ref state_ref with
    | Ok normalized ->
      Some normalized
    | Error _ ->
      None

let path_key_of_state_ref raw_state_ref =
  match normalize_state_ref raw_state_ref with
  | Error e -> Error e
  | Ok state_ref ->
    let canonical_path = state_canonical_path state_ref in
    Ok (state_ref, canonical_path, h256_hex "octra:asset_state_path:v1" [canonical_path])

let resource_key_of_state_ref ~circle_id ~state_ref =
  h256_hex "octra:circle_resource_key:state:v1" [circle_id; state_ref]

let slot_policy_key path_key suffix =
  "slot_policy:" ^ path_key ^ ":" ^ suffix

let slot_policy_activate_after_key path_key =
  slot_policy_key path_key "activate_after_epoch"

let slot_policy_delivery_key_key path_key =
  slot_policy_key path_key "delivery_key_id"

let slot_policy_expire_after_key path_key =
  slot_policy_key path_key "expire_after_epoch"

let slot_policy_tombstone_key path_key =
  slot_policy_key path_key "tombstone"

let slot_policy_revoked_key path_key =
  slot_policy_key path_key "revoked"

let state_policy_key path_key suffix =
  "state_policy:" ^ path_key ^ ":" ^ suffix

let state_policy_activate_after_key path_key =
  state_policy_key path_key "activate_after_epoch"

let state_policy_delivery_key_key path_key =
  state_policy_key path_key "delivery_key_id"

let state_policy_expire_after_key path_key =
  state_policy_key path_key "expire_after_epoch"

let state_policy_tombstone_key path_key =
  state_policy_key path_key "tombstone"

let state_policy_revoked_key path_key =
  state_policy_key path_key "revoked"

let legacy_mailbox_policy_key path_key suffix =
  "mailbox:" ^ path_key ^ ":" ^ suffix

let mailbox_activate_after_key path_key =
  legacy_mailbox_policy_key path_key "activate_after_epoch"

let mailbox_expire_after_key path_key =
  legacy_mailbox_policy_key path_key "expire_after_epoch"

let mailbox_tombstone_key path_key =
  legacy_mailbox_policy_key path_key "tombstone"

let mailbox_revoked_key path_key =
  legacy_mailbox_policy_key path_key "revoked"

let asset_blob_hash raw_bytes =
  sha256_hex raw_bytes

let is_valid_utf8 s =
  let byte i = Char.code s.[i] in
  let is_cont i =
    i < String.length s && let b = byte i in b land 0xC0 = 0x80
  in
  let rec loop i =
    if i >= String.length s then true
    else
      let b0 = byte i in
      if b0 <= 0x7F then
        loop (i + 1)
      else if b0 >= 0xC2 && b0 <= 0xDF then
        is_cont (i + 1) && loop (i + 2)
      else if b0 = 0xE0 then
        i + 2 < String.length s
        && (let b1 = byte (i + 1) in b1 >= 0xA0 && b1 <= 0xBF)
        && is_cont (i + 2)
        && loop (i + 3)
      else if b0 >= 0xE1 && b0 <= 0xEC then
        is_cont (i + 1) && is_cont (i + 2) && loop (i + 3)
      else if b0 = 0xED then
        i + 2 < String.length s
        && (let b1 = byte (i + 1) in b1 >= 0x80 && b1 <= 0x9F)
        && is_cont (i + 2)
        && loop (i + 3)
      else if b0 >= 0xEE && b0 <= 0xEF then
        is_cont (i + 1) && is_cont (i + 2) && loop (i + 3)
      else if b0 = 0xF0 then
        i + 3 < String.length s
        && (let b1 = byte (i + 1) in b1 >= 0x90 && b1 <= 0xBF)
        && is_cont (i + 2)
        && is_cont (i + 3)
        && loop (i + 4)
      else if b0 >= 0xF1 && b0 <= 0xF3 then
        is_cont (i + 1) && is_cont (i + 2) && is_cont (i + 3) && loop (i + 4)
      else if b0 = 0xF4 then
        i + 3 < String.length s
        && (let b1 = byte (i + 1) in b1 >= 0x80 && b1 <= 0x8F)
        && is_cont (i + 2)
        && is_cont (i + 3)
        && loop (i + 4)
      else
        false
  in
  loop 0

let hex_nibble c =
  match c with
  | '0' .. '9' -> Ok (Char.code c - Char.code '0')
  | 'a' .. 'f' -> Ok (10 + Char.code c - Char.code 'a')
  | 'A' .. 'F' -> Ok (10 + Char.code c - Char.code 'A')
  | _ -> Error ("invalid hex nibble: " ^ String.make 1 c)

let percent_decode s =
  let buf = Buffer.create (String.length s) in
  let rec loop i =
    if i >= String.length s then
      Ok (Buffer.contents buf)
    else
      match s.[i] with
      | '%' ->
        if i + 2 >= String.length s then
          Error "truncated percent escape"
        else
          begin
            match hex_nibble s.[i + 1], hex_nibble s.[i + 2] with
            | Ok hi, Ok lo ->
              Buffer.add_char buf (Char.chr ((hi lsl 4) lor lo));
              loop (i + 3)
            | Error e, _ | _, Error e ->
              Error e
          end
      | c ->
        Buffer.add_char buf c;
        loop (i + 1)
  in
  loop 0

let normalize_path raw_path =
  let initial =
    if String.length raw_path = 0 then "/"
    else raw_path
  in
  match percent_decode initial with
  | Error e -> Error e
  | Ok decoded ->
    if not (is_valid_utf8 decoded) then
      Error "path is not valid utf-8"
    else
      let with_slash =
        if String.length decoded = 0 then "/"
        else if decoded.[0] = '/' then decoded
        else "/" ^ decoded
      in
      let raw_segments = String.split_on_char '/' with_slash in
      let rec normalize acc = function
        | [] -> Ok (List.rev acc)
        | "" :: rest -> normalize acc rest
        | "." :: rest -> normalize acc rest
        | ".." :: _ -> Error "path traversal is not allowed"
        | seg :: rest -> normalize (seg :: acc) rest
      in
      begin
        match normalize [] raw_segments with
        | Error e -> Error e
        | Ok segments ->
          let normalized =
            match segments with
            | [] -> "/"
            | _ -> "/" ^ String.concat "/" segments
          in
          if String.length normalized > 1024 then
            Error "path exceeds max length"
          else
            Ok normalized
      end

let asset_path_key canonical_path =
  h256_hex "octra:asset_path:v1" [canonical_path]

let path_key_of_raw_path raw_path =
  match normalize_path raw_path with
  | Ok canonical_path -> Ok (canonical_path, asset_path_key canonical_path)
  | Error e -> Error e

let circle_intent_id ~circle_id ~version ~monotonic_nonce ~payload_hash =
  h256_hex "octra:circle_intent:v1"
    [circle_id; encode_u64be version; encode_u64be monotonic_nonce; payload_hash]

let parse_oct_resource uri =
  let prefix = "oct://" in
  let prefix_len = String.length prefix in
  if String.length uri < prefix_len || String.sub uri 0 prefix_len <> prefix then
    Error "resource must start with oct://"
  else
    let body = String.sub uri prefix_len (String.length uri - prefix_len) in
    match String.index_opt body '/' with
    | None ->
      if String.length body = 0 then Error "missing circle id"
      else Ok { circle_id = body; path = "/" }
    | Some slash_index ->
      let circle_id = String.sub body 0 slash_index in
      let raw_path = String.sub body slash_index (String.length body - slash_index) in
      if String.length circle_id = 0 then
        Error "missing circle id"
      else
        match normalize_path raw_path with
        | Ok path -> Ok { circle_id; path }
        | Error e -> Error e