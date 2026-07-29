(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type relay_mode =
  | Any_registered
  | Allowlist

type lease_class =
  | Reopen_on_expiry
  | Terminal_on_expiry
  | Cancel_on_expiry

type claim_strategy =
  | Any_replacement
  | Same_relay_only
  | No_replacement

type claim_topology =
  | Single_relay
  | Parallel_relays
  | Quorum_relays

type quorum_mode =
  | Count_quorum
  | Weighted_quorum

type ingress_strategy =
  | Any_ready_relay
  | Primary_oldest
  | Primary_newest
  | Primary_heaviest
  | Primary_heaviest_oldest
  | Primary_heaviest_newest

type t = {
  relay_mode : relay_mode;
  lease_class : lease_class;
  claim_strategy : claim_strategy;
  claim_topology : claim_topology;
  quorum_mode : quorum_mode;
  ingress_strategy : ingress_strategy;
  quorum_threshold : int option;
  quorum_weight_threshold : int option;
  max_active_claims : int option;
  relay_allowlist : string list;
  relay_weights : (string * int) list;
  max_claim_window_epochs : int64 option;
  max_response_bytes : int64 option;
  require_response_ciphertext : bool;
  require_external_receipt : bool;
  accepted_result_codes : int list option;
}

type put_payload = {
  relay_mode : relay_mode option;
  lease_class : lease_class option;
  claim_strategy : claim_strategy option;
  claim_topology : claim_topology option;
  quorum_mode : quorum_mode option;
  ingress_strategy : ingress_strategy option;
  quorum_threshold : int option;
  quorum_weight_threshold : int option;
  max_active_claims : int option;
  relay_allowlist : string list option;
  relay_weights : (string * int) list option;
  max_claim_window_epochs : int64 option;
  max_response_bytes : int64 option;
  require_response_ciphertext : bool option;
  require_external_receipt : bool option;
  accepted_result_codes : int list option;
}

let default : t = {
  relay_mode = Any_registered;
  lease_class = Reopen_on_expiry;
  claim_strategy = Any_replacement;
  claim_topology = Single_relay;
  quorum_mode = Count_quorum;
  ingress_strategy = Any_ready_relay;
  quorum_threshold = None;
  quorum_weight_threshold = None;
  max_active_claims = None;
  relay_allowlist = [];
  relay_weights = [];
  max_claim_window_epochs = None;
  max_response_bytes = None;
  require_response_ciphertext = false;
  require_external_receipt = false;
  accepted_result_codes = None;
}

let string_of_relay_mode = function
  | Any_registered -> "any_registered"
  | Allowlist -> "allowlist"

let relay_mode_of_string = function
  | "any_registered" -> Ok Any_registered
  | "allowlist" -> Ok Allowlist
  | other -> Error ("unknown relay mode: " ^ other)

let string_of_lease_class = function
  | Reopen_on_expiry -> "reopen_on_expiry"
  | Terminal_on_expiry -> "terminal_on_expiry"
  | Cancel_on_expiry -> "cancel_on_expiry"

let lease_class_of_string = function
  | "reopen_on_expiry" -> Ok Reopen_on_expiry
  | "terminal_on_expiry" -> Ok Terminal_on_expiry
  | "cancel_on_expiry" -> Ok Cancel_on_expiry
  | other -> Error ("unknown lease class: " ^ other)

let string_of_claim_strategy = function
  | Any_replacement -> "any_replacement"
  | Same_relay_only -> "same_relay_only"
  | No_replacement -> "no_replacement"

let claim_strategy_of_string = function
  | "any_replacement" -> Ok Any_replacement
  | "same_relay_only" -> Ok Same_relay_only
  | "no_replacement" -> Ok No_replacement
  | other -> Error ("unknown claim strategy: " ^ other)

let string_of_claim_topology = function
  | Single_relay -> "single_relay"
  | Parallel_relays -> "parallel_relays"
  | Quorum_relays -> "quorum_relays"

let claim_topology_of_string = function
  | "single_relay" -> Ok Single_relay
  | "parallel_relays" -> Ok Parallel_relays
  | "quorum_relays" -> Ok Quorum_relays
  | other -> Error ("unknown claim topology: " ^ other)

let string_of_quorum_mode = function
  | Count_quorum -> "count_quorum"
  | Weighted_quorum -> "weighted_quorum"

let quorum_mode_of_string = function
  | "count_quorum" -> Ok Count_quorum
  | "weighted_quorum" -> Ok Weighted_quorum
  | other -> Error ("unknown quorum mode: " ^ other)

let string_of_ingress_strategy = function
  | Any_ready_relay -> "any_ready_relay"
  | Primary_oldest -> "primary_oldest"
  | Primary_newest -> "primary_newest"
  | Primary_heaviest -> "primary_heaviest"
  | Primary_heaviest_oldest -> "primary_heaviest_oldest"
  | Primary_heaviest_newest -> "primary_heaviest_newest"

let ingress_strategy_of_string = function
  | "any_ready_relay" -> Ok Any_ready_relay
  | "primary_oldest" -> Ok Primary_oldest
  | "primary_newest" -> Ok Primary_newest
  | "primary_heaviest" -> Ok Primary_heaviest
  | "primary_heaviest_oldest" -> Ok Primary_heaviest_oldest
  | "primary_heaviest_newest" -> Ok Primary_heaviest_newest
  | other -> Error ("unknown ingress strategy: " ^ other)

let base_key = "transport_policy"

let key suffix =
  base_key ^ ":" ^ suffix

let relay_mode_key = key "relay_mode"

let lease_class_key = key "lease_class"

let claim_strategy_key = key "claim_strategy"

let claim_topology_key = key "claim_topology"

let quorum_mode_key = key "quorum_mode"

let ingress_strategy_key = key "ingress_strategy"

let quorum_threshold_key = key "quorum_threshold"

let quorum_weight_threshold_key = key "quorum_weight_threshold"

let max_active_claims_key = key "max_active_claims"

let relay_allowlist_key = key "relay_allowlist"

let relay_weights_key = key "relay_weights"

let max_claim_window_key = key "max_claim_window_epochs"

let max_response_bytes_key = key "max_response_bytes"

let require_response_ciphertext_key = key "require_response_ciphertext"

let require_external_receipt_key = key "require_external_receipt"

let accepted_result_codes_key = key "accepted_result_codes"

let yojson_of_t (policy : t) =
  `Assoc [
    "relay_mode", `String (string_of_relay_mode policy.relay_mode);
    "lease_class", `String (string_of_lease_class policy.lease_class);
    "claim_strategy", `String (string_of_claim_strategy policy.claim_strategy);
    "claim_topology", `String (string_of_claim_topology policy.claim_topology);
    "quorum_mode", `String (string_of_quorum_mode policy.quorum_mode);
    "ingress_strategy", `String (string_of_ingress_strategy policy.ingress_strategy);
    "quorum_threshold",
    begin
      match policy.quorum_threshold with
      | Some value -> `Int value
      | None -> `Null
    end;
    "quorum_weight_threshold",
    begin
      match policy.quorum_weight_threshold with
      | Some value -> `Int value
      | None -> `Null
    end;
    "max_active_claims",
    begin
      match policy.max_active_claims with
      | Some value -> `Int value
      | None -> `Null
    end;
    "relay_allowlist", `List (List.map (fun value -> `String value) policy.relay_allowlist);
    "relay_weights",
    `Assoc (List.map (fun (relay_id, weight) -> relay_id, `Int weight) policy.relay_weights);
    "max_claim_window_epochs",
    begin
      match policy.max_claim_window_epochs with
      | Some value -> `String (Int64.to_string value)
      | None -> `Null
    end;
    "max_response_bytes",
    begin
      match policy.max_response_bytes with
      | Some value -> `String (Int64.to_string value)
      | None -> `Null
    end;
    "require_response_ciphertext", `Bool policy.require_response_ciphertext;
    "require_external_receipt", `Bool policy.require_external_receipt;
    "accepted_result_codes",
    begin
      match policy.accepted_result_codes with
      | Some values -> `List (List.map (fun value -> `Int value) values)
      | None -> `Null
    end;
  ]

let yojson_of_put_payload (payload : put_payload) =
  `Assoc [
    "relay_mode",
    begin
      match payload.relay_mode with
      | Some value -> `String (string_of_relay_mode value)
      | None -> `Null
    end;
    "lease_class",
    begin
      match payload.lease_class with
      | Some value -> `String (string_of_lease_class value)
      | None -> `Null
    end;
    "claim_strategy",
    begin
      match payload.claim_strategy with
      | Some value -> `String (string_of_claim_strategy value)
      | None -> `Null
    end;
    "claim_topology",
    begin
      match payload.claim_topology with
      | Some value -> `String (string_of_claim_topology value)
      | None -> `Null
    end;
    "quorum_mode",
    begin
      match payload.quorum_mode with
      | Some value -> `String (string_of_quorum_mode value)
      | None -> `Null
    end;
    "ingress_strategy",
    begin
      match payload.ingress_strategy with
      | Some value -> `String (string_of_ingress_strategy value)
      | None -> `Null
    end;
    "quorum_threshold",
    begin
      match payload.quorum_threshold with
      | Some value -> `Int value
      | None -> `Null
    end;
    "quorum_weight_threshold",
    begin
      match payload.quorum_weight_threshold with
      | Some value -> `Int value
      | None -> `Null
    end;
    "max_active_claims",
    begin
      match payload.max_active_claims with
      | Some value -> `Int value
      | None -> `Null
    end;
    "relay_allowlist",
    begin
      match payload.relay_allowlist with
      | Some values -> `List (List.map (fun value -> `String value) values)
      | None -> `Null
    end;
    "relay_weights",
    begin
      match payload.relay_weights with
      | Some values -> `Assoc (List.map (fun (relay_id, weight) -> relay_id, `Int weight) values)
      | None -> `Null
    end;
    "max_claim_window_epochs",
    begin
      match payload.max_claim_window_epochs with
      | Some value -> `String (Int64.to_string value)
      | None -> `Null
    end;
    "max_response_bytes",
    begin
      match payload.max_response_bytes with
      | Some value -> `String (Int64.to_string value)
      | None -> `Null
    end;
    "require_response_ciphertext",
    begin
      match payload.require_response_ciphertext with
      | Some value -> `Bool value
      | None -> `Null
    end;
    "require_external_receipt",
    begin
      match payload.require_external_receipt with
      | Some value -> `Bool value
      | None -> `Null
    end;
    "accepted_result_codes",
    begin
      match payload.accepted_result_codes with
      | Some values -> `List (List.map (fun value -> `Int value) values)
      | None -> `Null
    end;
  ]

let put_payload_of_yojson = function
  | `Assoc fields ->
    let read_optional_string key =
      match List.assoc_opt key fields with
      | Some (`String value) -> Ok (Some value)
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid transport policy field: " ^ key)
    in
    let read_optional_i64 key =
      match List.assoc_opt key fields with
      | Some (`String value) -> Ok (Some (Int64.of_string value))
      | Some (`Int value) -> Ok (Some (Int64.of_int value))
      | Some (`Intlit value) -> Ok (Some (Int64.of_string value))
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid transport policy field: " ^ key)
    in
    let read_optional_int key =
      match List.assoc_opt key fields with
      | Some (`Int value) -> Ok (Some value)
      | Some (`Intlit value) -> Ok (Some (int_of_string value))
      | Some (`String value) -> Ok (Some (int_of_string value))
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid transport policy field: " ^ key)
    in
    let read_optional_bool key =
      match List.assoc_opt key fields with
      | Some (`Bool value) -> Ok (Some value)
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid transport policy field: " ^ key)
    in
    let read_optional_string_list key =
      match List.assoc_opt key fields with
      | Some (`List values) ->
        Ok (Some (List.filter_map (function `String value -> Some value | _ -> None) values))
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid transport policy field: " ^ key)
    in
    let read_optional_string_int_assoc key =
      match List.assoc_opt key fields with
      | Some (`Assoc values) ->
        Ok
          (Some
             (List.filter_map
                (fun (relay_id, value) ->
                  match value with
                  | `Int weight -> Some (relay_id, weight)
                  | `Intlit weight ->
                    begin
                      try Some (relay_id, int_of_string weight)
                      with _ -> None
                    end
                  | `String weight ->
                    begin
                      try Some (relay_id, int_of_string weight)
                      with _ -> None
                    end
                  | _ -> None)
                values))
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid transport policy field: " ^ key)
    in
    let read_optional_int_list key =
      match List.assoc_opt key fields with
      | Some (`List values) ->
        Ok
          (Some
             (List.filter_map
                (function
                  | `Int value -> Some value
                  | `Intlit value ->
                    begin
                      try Some (int_of_string value)
                      with _ -> None
                    end
                  | `String value ->
                    begin
                      try Some (int_of_string value)
                      with _ -> None
                    end
                  | _ -> None)
                values))
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid transport policy field: " ^ key)
    in
    begin
      match read_optional_string "relay_mode",
            read_optional_string "lease_class",
            read_optional_string "claim_strategy",
            read_optional_string "claim_topology",
            read_optional_string "quorum_mode",
            read_optional_string "ingress_strategy",
            read_optional_int "quorum_threshold",
            read_optional_int "quorum_weight_threshold",
            read_optional_int "max_active_claims",
            read_optional_string_list "relay_allowlist",
            read_optional_string_int_assoc "relay_weights",
            read_optional_i64 "max_claim_window_epochs",
            read_optional_i64 "max_response_bytes",
            read_optional_bool "require_response_ciphertext",
            read_optional_bool "require_external_receipt",
            read_optional_int_list "accepted_result_codes" with
      | Ok relay_mode_raw, Ok lease_class_raw, Ok claim_strategy_raw, Ok claim_topology_raw,
        Ok quorum_mode_raw, Ok ingress_strategy_raw, Ok quorum_threshold, Ok quorum_weight_threshold,
        Ok max_active_claims, Ok relay_allowlist, Ok relay_weights, Ok max_claim_window_epochs, Ok max_response_bytes,
        Ok require_response_ciphertext, Ok require_external_receipt, Ok accepted_result_codes ->
        let parse_optional raw parse =
          match raw with
          | Some raw ->
            begin
              match parse raw with
              | Ok value -> Ok (Some value)
              | Error _ as e -> e
            end
          | None ->
            Ok None
        in
        begin
          match parse_optional relay_mode_raw relay_mode_of_string,
                parse_optional lease_class_raw lease_class_of_string,
                parse_optional claim_strategy_raw claim_strategy_of_string,
                parse_optional claim_topology_raw claim_topology_of_string,
                parse_optional quorum_mode_raw quorum_mode_of_string,
                parse_optional ingress_strategy_raw ingress_strategy_of_string with
          | Ok relay_mode, Ok lease_class, Ok claim_strategy, Ok claim_topology, Ok quorum_mode, Ok ingress_strategy ->
            Ok {
              relay_mode;
              lease_class;
              claim_strategy;
              claim_topology;
              quorum_mode;
              ingress_strategy;
              quorum_threshold;
              quorum_weight_threshold;
              max_active_claims;
              relay_allowlist;
              relay_weights;
              max_claim_window_epochs;
              max_response_bytes;
              require_response_ciphertext;
              require_external_receipt;
              accepted_result_codes;
            }
          | Error e, _, _, _, _, _
          | _, Error e, _, _, _, _
          | _, _, Error e, _, _, _
          | _, _, _, Error e, _, _
          | _, _, _, _, Error e, _
          | _, _, _, _, _, Error e ->
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
  | _ ->
    Error "transport policy payload must be a json object"

let int_of_stored_value raw =
  match Circle_policy_value.int64_value raw with
  | Some value ->
    begin
      try Some (Int64.to_int value)
      with _ -> None
    end
  | None ->
    None

let of_stored_values
    ~relay_mode_raw
    ~lease_class_raw
    ~claim_strategy_raw
    ~claim_topology_raw
    ~quorum_mode_raw
    ~ingress_strategy_raw
    ~quorum_threshold_raw
    ~quorum_weight_threshold_raw
    ~max_active_claims_raw
    ~relay_allowlist_raw
    ~relay_weights_raw
    ~max_claim_window_raw
    ~max_response_bytes_raw
    ~require_response_ciphertext_raw
    ~require_external_receipt_raw
    ~accepted_result_codes_raw : t =
  let relay_mode =
    match relay_mode_raw with
    | Some relay_mode_raw ->
      begin
        match relay_mode_of_string relay_mode_raw with
        | Ok value -> value
        | Error _ -> default.relay_mode
      end
    | None ->
      default.relay_mode
  in
  let lease_class =
    match lease_class_raw with
    | Some lease_class_raw ->
      begin
        match lease_class_of_string lease_class_raw with
        | Ok value -> value
        | Error _ -> default.lease_class
      end
    | None ->
      default.lease_class
  in
  let claim_strategy =
    match claim_strategy_raw with
    | Some claim_strategy_raw ->
      begin
        match claim_strategy_of_string claim_strategy_raw with
        | Ok value -> value
        | Error _ -> default.claim_strategy
      end
    | None ->
      default.claim_strategy
  in
  let claim_topology =
    match claim_topology_raw with
    | Some claim_topology_raw ->
      begin
        match claim_topology_of_string claim_topology_raw with
        | Ok value -> value
        | Error _ -> default.claim_topology
      end
    | None ->
      default.claim_topology
  in
  let quorum_mode =
    match quorum_mode_raw with
    | Some quorum_mode_raw ->
      begin
        match quorum_mode_of_string quorum_mode_raw with
        | Ok value -> value
        | Error _ -> default.quorum_mode
      end
    | None ->
      default.quorum_mode
  in
  let ingress_strategy =
    match ingress_strategy_raw with
    | Some ingress_strategy_raw ->
      begin
        match ingress_strategy_of_string ingress_strategy_raw with
        | Ok value -> value
        | Error _ -> default.ingress_strategy
      end
    | None ->
      default.ingress_strategy
  in
  {
    relay_mode;
    lease_class;
    claim_strategy;
    claim_topology;
    quorum_mode;
    ingress_strategy;
    quorum_threshold = int_of_stored_value quorum_threshold_raw;
    quorum_weight_threshold = int_of_stored_value quorum_weight_threshold_raw;
    max_active_claims = int_of_stored_value max_active_claims_raw;
    relay_allowlist = Circle_policy_value.string_list_value relay_allowlist_raw;
    relay_weights = Circle_policy_value.string_int_assoc_value relay_weights_raw;
    max_claim_window_epochs = Circle_policy_value.int64_value max_claim_window_raw;
    max_response_bytes = Circle_policy_value.int64_value max_response_bytes_raw;
    require_response_ciphertext = Circle_policy_value.bool_value require_response_ciphertext_raw;
    require_external_receipt = Circle_policy_value.bool_value require_external_receipt_raw;
    accepted_result_codes = Circle_policy_value.int_list_value accepted_result_codes_raw;
  }

let resolve_put_payload (payload : put_payload) : t = {
  relay_mode = Option.value ~default:default.relay_mode payload.relay_mode;
  lease_class = Option.value ~default:default.lease_class payload.lease_class;
  claim_strategy = Option.value ~default:default.claim_strategy payload.claim_strategy;
  claim_topology = Option.value ~default:default.claim_topology payload.claim_topology;
  quorum_mode = Option.value ~default:default.quorum_mode payload.quorum_mode;
  ingress_strategy = Option.value ~default:default.ingress_strategy payload.ingress_strategy;
  quorum_threshold = payload.quorum_threshold;
  quorum_weight_threshold = payload.quorum_weight_threshold;
  max_active_claims = payload.max_active_claims;
  relay_allowlist = Option.value ~default:default.relay_allowlist payload.relay_allowlist;
  relay_weights = Option.value ~default:default.relay_weights payload.relay_weights;
  max_claim_window_epochs = payload.max_claim_window_epochs;
  max_response_bytes = payload.max_response_bytes;
  require_response_ciphertext =
    Option.value ~default:default.require_response_ciphertext payload.require_response_ciphertext;
  require_external_receipt =
    Option.value ~default:default.require_external_receipt payload.require_external_receipt;
  accepted_result_codes = payload.accepted_result_codes;
}

let validate (policy : t) =
  let invalid field message =
    Error (field, message)
  in
  if List.exists (fun (_relay_id, weight) -> weight <= 0) policy.relay_weights then
    invalid "invalid_circle_transport_relay_weight" "relay weights must be positive"
  else
  match policy.quorum_threshold, policy.max_active_claims with
  | Some value, _ when value <= 0 ->
    invalid "invalid_circle_transport_quorum_threshold" "quorum_threshold must be positive"
  | _, Some value when value <= 0 ->
    invalid "invalid_circle_transport_max_active_claims" "max_active_claims must be positive"
  | _, _ ->
    begin
      match policy.quorum_weight_threshold with
      | Some value when value <= 0 ->
        invalid "invalid_circle_transport_quorum_weight_threshold" "quorum_weight_threshold must be positive"
      | _ ->
        begin
          match policy.claim_topology with
          | Single_relay ->
            begin
              match policy.quorum_threshold, policy.quorum_weight_threshold, policy.max_active_claims with
              | Some value, _, _ when value <> 1 ->
                invalid "invalid_circle_transport_quorum_threshold" "single_relay topology requires quorum_threshold = 1"
              | _, Some value, _ when value <> 1 ->
                invalid "invalid_circle_transport_quorum_weight_threshold" "single_relay topology requires quorum_weight_threshold = 1 when provided"
              | _, _, Some value when value <> 1 ->
                invalid "invalid_circle_transport_max_active_claims" "single_relay topology requires max_active_claims = 1"
              | _ ->
                Ok ()
            end
          | Parallel_relays ->
            begin
              match policy.quorum_threshold, policy.quorum_weight_threshold with
              | Some value, _ when value <> 1 ->
                invalid "invalid_circle_transport_quorum_threshold" "parallel_relays topology requires quorum_threshold = 1 when provided"
              | _, Some value when value <> 1 ->
                invalid "invalid_circle_transport_quorum_weight_threshold" "parallel_relays topology requires quorum_weight_threshold = 1 when provided"
              | _ ->
                Ok ()
            end
          | Quorum_relays ->
            let threshold =
              match policy.quorum_threshold with
              | Some value -> value
              | None -> 2
            in
            let weight_threshold =
              match policy.quorum_weight_threshold with
              | Some value -> value
              | None -> 2
            in
            if threshold < 2 then
              invalid "invalid_circle_transport_quorum_threshold" "quorum_relays topology requires quorum_threshold >= 2"
            else if policy.quorum_mode = Weighted_quorum && weight_threshold < 2 then
              invalid "invalid_circle_transport_quorum_weight_threshold" "weighted quorum requires quorum_weight_threshold >= 2"
            else
              match policy.max_active_claims with
              | Some max_active_claims when threshold > max_active_claims ->
                invalid "invalid_circle_transport_quorum_threshold" "quorum_threshold must not exceed max_active_claims"
              | _ ->
                Ok ()
        end
    end

let write_snapshot storage_tbl payload =
  let resolved = resolve_put_payload payload in
  Circle_policy_value.set_or_clear
    storage_tbl
    relay_mode_key
    (if resolved.relay_mode = default.relay_mode then None else Some (string_of_relay_mode resolved.relay_mode));
  Circle_policy_value.set_or_clear
    storage_tbl
    lease_class_key
    (if resolved.lease_class = default.lease_class then None else Some (string_of_lease_class resolved.lease_class));
  Circle_policy_value.set_or_clear
    storage_tbl
    claim_strategy_key
    (if resolved.claim_strategy = default.claim_strategy then None else Some (string_of_claim_strategy resolved.claim_strategy));
  Circle_policy_value.set_or_clear
    storage_tbl
    claim_topology_key
    (if resolved.claim_topology = default.claim_topology then None else Some (string_of_claim_topology resolved.claim_topology));
  Circle_policy_value.set_or_clear
    storage_tbl
    quorum_mode_key
    (if resolved.quorum_mode = default.quorum_mode then None else Some (string_of_quorum_mode resolved.quorum_mode));
  Circle_policy_value.set_or_clear
    storage_tbl
    ingress_strategy_key
    (if resolved.ingress_strategy = default.ingress_strategy then None else Some (string_of_ingress_strategy resolved.ingress_strategy));
  Circle_policy_value.set_or_clear
    storage_tbl
    quorum_threshold_key
    (Option.map string_of_int resolved.quorum_threshold);
  Circle_policy_value.set_or_clear
    storage_tbl
    quorum_weight_threshold_key
    (Option.map string_of_int resolved.quorum_weight_threshold);
  Circle_policy_value.set_or_clear
    storage_tbl
    max_active_claims_key
    (Option.map string_of_int resolved.max_active_claims);
  Circle_policy_value.set_or_clear
    storage_tbl
    relay_allowlist_key
    (if resolved.relay_allowlist = [] then None else Some (Circle_policy_value.json_string_list resolved.relay_allowlist));
  Circle_policy_value.set_or_clear
    storage_tbl
    relay_weights_key
    (if resolved.relay_weights = [] then None else Some (Circle_policy_value.json_string_int_assoc resolved.relay_weights));
  Circle_policy_value.set_or_clear
    storage_tbl
    max_claim_window_key
    (Option.map Int64.to_string resolved.max_claim_window_epochs);
  Circle_policy_value.set_or_clear
    storage_tbl
    max_response_bytes_key
    (Option.map Int64.to_string resolved.max_response_bytes);
  Circle_policy_value.set_or_clear
    storage_tbl
    require_response_ciphertext_key
    (if resolved.require_response_ciphertext then Some "true" else None);
  Circle_policy_value.set_or_clear
    storage_tbl
    require_external_receipt_key
    (if resolved.require_external_receipt then Some "true" else None);
  Circle_policy_value.set_or_clear
    storage_tbl
    accepted_result_codes_key
    (Option.map Circle_policy_value.json_int_list resolved.accepted_result_codes)

let relay_allowed (policy : t) relay_id =
  match policy.relay_mode with
  | Any_registered -> true
  | Allowlist -> List.mem relay_id policy.relay_allowlist

let result_code_allowed (policy : t) result_code =
  match policy.accepted_result_codes with
  | Some codes -> List.mem result_code codes
  | None -> true

let response_size_allowed (policy : t) size_bytes =
  match policy.max_response_bytes with
  | Some limit -> Int64.compare size_bytes limit <= 0
  | None -> true

let active_claim_limit (policy : t) =
  match policy.claim_topology, policy.max_active_claims with
  | Single_relay, _ -> 1
  | _, Some limit -> limit
  | _ -> max_int

let quorum_threshold_value (policy : t) =
  match policy.claim_topology, policy.quorum_threshold with
  | Single_relay, _ -> 1
  | Parallel_relays, _ -> 1
  | Quorum_relays, Some value -> value
  | Quorum_relays, None -> 2

let quorum_weight_threshold_value (policy : t) =
  match policy.claim_topology, policy.quorum_weight_threshold with
  | Single_relay, _ -> 1
  | Parallel_relays, _ -> 1
  | Quorum_relays, Some value -> value
  | Quorum_relays, None -> 2