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

let consensus_id = "circle_storage:cell_owner:standard"

let reserved_prefix key =
  List.find_opt (fun prefix -> String.starts_with ~prefix key) reserved_prefixes

let cell_prefix key =
  if String.starts_with ~prefix:"balance_cell:" key then
    Some "balance_cell:"
  else if String.starts_with ~prefix:"register_cell:" key then
    Some "register_cell:"
  else
    None

let validate_cell_change proof_mode raw_key =
  match proof_mode, cell_prefix raw_key with
  | Octra_core.Rule_graph.Active, Some prefix ->
    Error ("circle_runtime_cell_write_denied", raw_key, prefix)
  | Octra_core.Rule_graph.Active, None
  | Octra_core.Rule_graph.Prior, _ ->
    Ok ()

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

let validate_runtime_storage_delta ~proof_mode before_tbl after_tbl =
  let validate_key raw_key =
    let before_value = Hashtbl.find_opt before_tbl raw_key in
    let after_value = Hashtbl.find_opt after_tbl raw_key in
    match before_value, after_value with
    | Some before_value, Some after_value when String.equal before_value after_value ->
      Ok ()
    | _, Some after_value ->
      begin
        match validate_cell_change proof_mode raw_key with
        | Error _ as e -> e
        | Ok () -> validate_runtime_key_write raw_key after_value
      end
    | Some _, None ->
      begin
        match validate_cell_change proof_mode raw_key with
        | Error _ as e -> e
        | Ok () -> validate_runtime_key_delete raw_key
      end
    | None, None ->
      Ok ()
  in
  let keys () =
    List.of_seq (Hashtbl.to_seq_keys before_tbl)
    @ List.of_seq (Hashtbl.to_seq_keys after_tbl)
    |> List.sort_uniq String.compare
  in
  let rec walk = function
    | [] -> Ok ()
    | raw_key :: rest ->
      begin
        match validate_key raw_key with
        | Error _ as e -> e
        | Ok () -> walk rest
      end
  in
  match proof_mode with
  | Octra_core.Rule_graph.Active -> walk (keys ())
  | Octra_core.Rule_graph.Prior ->
    let seen = Hashtbl.create 64 in
    let result = ref (Ok ()) in
    let visit raw_key _ =
      match !result with
      | Error _ -> ()
      | Ok () when Hashtbl.mem seen raw_key -> ()
      | Ok () ->
        Hashtbl.replace seen raw_key ();
        result := validate_key raw_key
    in
    Hashtbl.iter visit before_tbl;
    Hashtbl.iter visit after_tbl;
    !result