(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let reserved_prefixes = [
  "object_policy:";
  "object_binding:";
  "object_member:";
  "object_transition:";
  "balance_binding:";
  "register_binding:";
  "balance_cell:";
  "balance_workflow:";
  "register_cell:";
  "register_workflow:";
  "state_descriptor:";
  "transport_policy:";
  "hfhe_policy:";
  "key_policy:";
  "mailbox:";
  "outbox:";
  "ingress:";
]

let reserved_prefix key =
  List.find_opt (fun prefix -> String.starts_with ~prefix key) reserved_prefixes

let object_policy_suffix_allowed = function
  | "delivery_key_id"
  | "activate_after_epoch"
  | "expire_after_epoch"
  | "tombstone"
  | "revoked" ->
    true
  | _ ->
    false

let string_is_hex64 value =
  String.length value = 64
  && String.for_all Octra_core.Circles.is_hex_char value

let string_is_bool_literal value =
  let normalized =
    String.lowercase_ascii (String.trim value) in
  normalized = "0"
  || normalized = "1"
  || normalized = "false"
  || normalized = "true"
  || normalized = "no"
  || normalized = "yes"

let validate_object_policy_runtime_key prefix raw_key value =
  match String.split_on_char ':' raw_key with
  | [raw_prefix; path_key; suffix] when raw_prefix = prefix ->
    if not (string_is_hex64 path_key) then
      Error ("circle_runtime_invalid_object_policy_key", raw_key, "path_key")
    else if not (object_policy_suffix_allowed suffix) then
      Error ("circle_runtime_invalid_object_policy_key", raw_key, suffix)
    else
      begin
        match suffix with
        | "activate_after_epoch"
        | "expire_after_epoch" ->
          begin
            try
              ignore (Int64.of_string (String.trim value));
              Ok ()
            with _ ->
              Error ("circle_runtime_invalid_object_policy_value", raw_key, suffix)
          end
        | "tombstone"
        | "revoked" ->
          if string_is_bool_literal value then
            Ok ()
          else
            Error ("circle_runtime_invalid_object_policy_value", raw_key, suffix)
        | "delivery_key_id" ->
          if String.trim value = "" then
            Error ("circle_runtime_invalid_object_policy_value", raw_key, suffix)
          else
            Ok ()
        | _ ->
          Error ("circle_runtime_invalid_object_policy_key", raw_key, suffix)
      end
  | _ ->
    Error ("circle_runtime_invalid_object_policy_key", raw_key, "shape")

let validate_runtime_key_write raw_key value =
  if String.starts_with ~prefix:"slot_policy:" raw_key then
    validate_object_policy_runtime_key "slot_policy" raw_key value
  else if String.starts_with ~prefix:"state_policy:" raw_key then
    validate_object_policy_runtime_key "state_policy" raw_key value
  else if String.starts_with ~prefix:"state_descriptor:" raw_key then
    Octra_core.Circle_state_descriptor.validate_runtime_key raw_key value
  else if String.starts_with ~prefix:"object_policy:" raw_key then
    Octra_core.Circle_object_policy.validate_runtime_key raw_key value
  else if String.starts_with ~prefix:"object_binding:" raw_key then
    Octra_core.Circle_object_binding.validate_runtime_key raw_key value
  else if String.starts_with ~prefix:"object_member:" raw_key then
    Octra_core.Circle_object_member.validate_runtime_key raw_key value
  else if String.starts_with ~prefix:"object_transition:" raw_key then
    Octra_core.Circle_object_transition.validate_runtime_key raw_key value
  else if String.starts_with ~prefix:"balance_binding:" raw_key then
    Octra_core.Circle_balance_binding.validate_runtime_key raw_key value
  else if String.starts_with ~prefix:"register_binding:" raw_key then
    Octra_core.Circle_register_binding.validate_runtime_key raw_key value
  else if String.starts_with ~prefix:"balance_cell:" raw_key then
    Octra_core.Circle_balance_cell.validate_runtime_key raw_key value
  else if String.starts_with ~prefix:"balance_workflow:" raw_key then
    Octra_core.Circle_balance_workflow.validate_runtime_key raw_key value
  else if String.starts_with ~prefix:"register_cell:" raw_key then
    Octra_core.Circle_register_cell.validate_runtime_key raw_key value
  else if String.starts_with ~prefix:"register_workflow:" raw_key then
    Octra_core.Circle_register_workflow.validate_runtime_key raw_key value
  else
    begin
      match reserved_prefix raw_key with
      | Some prefix ->
        Error ("circle_runtime_reserved_storage_key", raw_key, prefix)
      | None ->
        Ok ()
    end

let validate_runtime_key_delete raw_key =
  if String.starts_with ~prefix:"slot_policy:" raw_key
     || String.starts_with ~prefix:"state_policy:" raw_key
     || String.starts_with ~prefix:"object_policy:" raw_key
     || String.starts_with ~prefix:"object_binding:" raw_key
     || String.starts_with ~prefix:"object_member:" raw_key
     || String.starts_with ~prefix:"object_transition:" raw_key
     || String.starts_with ~prefix:"balance_binding:" raw_key
     || String.starts_with ~prefix:"register_binding:" raw_key
     || String.starts_with ~prefix:"balance_cell:" raw_key
     || String.starts_with ~prefix:"balance_workflow:" raw_key
     || String.starts_with ~prefix:"register_cell:" raw_key
     || String.starts_with ~prefix:"register_workflow:" raw_key
  then
    Ok ()
  else
    begin
      match reserved_prefix raw_key with
      | Some prefix ->
        Error ("circle_runtime_reserved_storage_key", raw_key, prefix)
      | None ->
        Ok ()
    end

let validate_runtime_storage_keys storage_tbl =
  Hashtbl.fold
    (fun raw_key value acc ->
      match acc with
      | Error _ as e -> e
      | Ok () -> validate_runtime_key_write raw_key value)
    storage_tbl
    (Ok ())

let validate_runtime_storage_delta before_tbl after_tbl =
  let seen = Hashtbl.create 64 in
  let validate_key raw_key =
    if Hashtbl.mem seen raw_key then
      Ok ()
    else begin
      Hashtbl.replace seen raw_key ();
      let before_value = Hashtbl.find_opt before_tbl raw_key in
      let after_value = Hashtbl.find_opt after_tbl raw_key in
      match before_value, after_value with
      | Some before_value, Some after_value when String.equal before_value after_value ->
        Ok ()
      | _, Some after_value ->
        validate_runtime_key_write raw_key after_value
      | Some _, None ->
        validate_runtime_key_delete raw_key
      | None, None ->
        Ok ()
    end
  in
  let result = ref (Ok ()) in
  Hashtbl.iter
    (fun raw_key _ ->
      match !result with
      | Error _ -> ()
      | Ok () -> result := validate_key raw_key)
    before_tbl;
  Hashtbl.iter
    (fun raw_key _ ->
      match !result with
      | Error _ -> ()
      | Ok () -> result := validate_key raw_key)
    after_tbl;
  !result