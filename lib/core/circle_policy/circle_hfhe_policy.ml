(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type access_mode =
  | Deny
  | Owner_only
  | Caller_self
  | Owner_or_caller
  | Any_registered
  | Active_relay
  | Owner_or_active_relay

type t = {
  load_pk_mode : access_mode;
  encrypt_mode : access_mode;
  decrypt_mode : access_mode;
  cipher_arithmetic_mode : access_mode;
  commit_mode : access_mode;
  pedersen_mode : access_mode;
  cipher_serde_mode : access_mode;
  pubkey_serde_mode : access_mode;
  verify_zero_mode : access_mode;
  verify_range_mode : access_mode;
  verify_bound_mode : access_mode;
  proof_receipt_signer_mode : access_mode;
  proof_receipt_class : Circle_hfhe_receipt_class.t;
  pk_allowlist : string list option;
  require_live_key_policy : bool;
  require_receipt_transport_binding : bool;
  encrypt_proof : Circle_hfhe_proof.t;
  decrypt_proof : Circle_hfhe_proof.t;
}

type put_payload = {
  load_pk_mode : access_mode option;
  encrypt_mode : access_mode option;
  decrypt_mode : access_mode option;
  cipher_arithmetic_mode : access_mode option;
  commit_mode : access_mode option;
  pedersen_mode : access_mode option;
  cipher_serde_mode : access_mode option;
  pubkey_serde_mode : access_mode option;
  verify_zero_mode : access_mode option;
  verify_range_mode : access_mode option;
  verify_bound_mode : access_mode option;
  proof_receipt_signer_mode : access_mode option;
  proof_receipt_class : Circle_hfhe_receipt_class.t option;
  pk_allowlist : string list option;
  require_live_key_policy : bool option;
  require_receipt_transport_binding : bool option;
  encrypt_proof : Circle_hfhe_proof.t option;
  decrypt_proof : Circle_hfhe_proof.t option;
}

let default : t = {
  load_pk_mode = Caller_self;
  encrypt_mode = Owner_only;
  decrypt_mode = Owner_only;
  cipher_arithmetic_mode = Owner_only;
  commit_mode = Owner_only;
  pedersen_mode = Owner_only;
  cipher_serde_mode = Owner_only;
  pubkey_serde_mode = Owner_only;
  verify_zero_mode = Owner_only;
  verify_range_mode = Owner_only;
  verify_bound_mode = Owner_only;
  proof_receipt_signer_mode = Caller_self;
  proof_receipt_class = Circle_hfhe_receipt_class.Detached;
  pk_allowlist = None;
  require_live_key_policy = true;
  require_receipt_transport_binding = false;
  encrypt_proof = Circle_hfhe_proof.Bound_zero_v1;
  decrypt_proof = Circle_hfhe_proof.No_proof;
}

let string_of_access_mode = function
  | Deny -> "deny"
  | Owner_only -> "owner_only"
  | Caller_self -> "caller_self"
  | Owner_or_caller -> "owner_or_caller"
  | Any_registered -> "any_registered"
  | Active_relay -> "active_relay"
  | Owner_or_active_relay -> "owner_or_active_relay"

let access_mode_of_string = function
  | "deny" -> Ok Deny
  | "owner_only" -> Ok Owner_only
  | "caller_self" -> Ok Caller_self
  | "owner_or_caller" -> Ok Owner_or_caller
  | "any_registered" -> Ok Any_registered
  | "active_relay" -> Ok Active_relay
  | "owner_or_active_relay" -> Ok Owner_or_active_relay
  | other -> Error ("unknown hfhe access mode: " ^ other)

let base_key = "hfhe_policy"

let key suffix =
  base_key ^ ":" ^ suffix

let load_pk_mode_key = key "load_pk_mode"

let encrypt_mode_key = key "encrypt_mode"

let decrypt_mode_key = key "decrypt_mode"

let cipher_arithmetic_mode_key = key "cipher_arithmetic_mode"

let commit_mode_key = key "commit_mode"

let pedersen_mode_key = key "pedersen_mode"

let cipher_serde_mode_key = key "cipher_serde_mode"

let pubkey_serde_mode_key = key "pubkey_serde_mode"

let verify_zero_mode_key = key "verify_zero_mode"

let verify_range_mode_key = key "verify_range_mode"

let verify_bound_mode_key = key "verify_bound_mode"

let proof_receipt_signer_mode_key = key "proof_receipt_signer_mode"

let proof_receipt_class_key = key "proof_receipt_class"

let pk_allowlist_key = key "pk_allowlist"

let require_live_key_policy_key = key "require_live_key_policy"

let require_receipt_transport_binding_key = key "require_receipt_transport_binding"

let encrypt_proof_key = key "encrypt_proof"

let decrypt_proof_key = key "decrypt_proof"

let yojson_of_t (policy : t) =
  `Assoc [
    "load_pk_mode", `String (string_of_access_mode policy.load_pk_mode);
    "encrypt_mode", `String (string_of_access_mode policy.encrypt_mode);
    "decrypt_mode", `String (string_of_access_mode policy.decrypt_mode);
    "cipher_arithmetic_mode", `String (string_of_access_mode policy.cipher_arithmetic_mode);
    "commit_mode", `String (string_of_access_mode policy.commit_mode);
    "pedersen_mode", `String (string_of_access_mode policy.pedersen_mode);
    "cipher_serde_mode", `String (string_of_access_mode policy.cipher_serde_mode);
    "pubkey_serde_mode", `String (string_of_access_mode policy.pubkey_serde_mode);
    "verify_zero_mode", `String (string_of_access_mode policy.verify_zero_mode);
    "verify_range_mode", `String (string_of_access_mode policy.verify_range_mode);
    "verify_bound_mode", `String (string_of_access_mode policy.verify_bound_mode);
    "proof_receipt_signer_mode", `String (string_of_access_mode policy.proof_receipt_signer_mode);
    "proof_receipt_class", `String (Circle_hfhe_receipt_class.string_of_t policy.proof_receipt_class);
    "pk_allowlist",
    begin
      match policy.pk_allowlist with
      | Some values -> `List (List.map (fun value -> `String value) values)
      | None -> `Null
    end;
    "require_live_key_policy", `Bool policy.require_live_key_policy;
    "require_receipt_transport_binding", `Bool policy.require_receipt_transport_binding;
    "encrypt_proof", `String (Circle_hfhe_proof.string_of_t policy.encrypt_proof);
    "decrypt_proof", `String (Circle_hfhe_proof.string_of_t policy.decrypt_proof);
  ]

let yojson_of_put_payload (payload : put_payload) =
  `Assoc [
    "load_pk_mode",
    begin
      match payload.load_pk_mode with
      | Some value -> `String (string_of_access_mode value)
      | None -> `Null
    end;
    "encrypt_mode",
    begin
      match payload.encrypt_mode with
      | Some value -> `String (string_of_access_mode value)
      | None -> `Null
    end;
    "decrypt_mode",
    begin
      match payload.decrypt_mode with
      | Some value -> `String (string_of_access_mode value)
      | None -> `Null
    end;
    "cipher_arithmetic_mode",
    begin
      match payload.cipher_arithmetic_mode with
      | Some value -> `String (string_of_access_mode value)
      | None -> `Null
    end;
    "commit_mode",
    begin
      match payload.commit_mode with
      | Some value -> `String (string_of_access_mode value)
      | None -> `Null
    end;
    "pedersen_mode",
    begin
      match payload.pedersen_mode with
      | Some value -> `String (string_of_access_mode value)
      | None -> `Null
    end;
    "cipher_serde_mode",
    begin
      match payload.cipher_serde_mode with
      | Some value -> `String (string_of_access_mode value)
      | None -> `Null
    end;
    "pubkey_serde_mode",
    begin
      match payload.pubkey_serde_mode with
      | Some value -> `String (string_of_access_mode value)
      | None -> `Null
    end;
    "verify_zero_mode",
    begin
      match payload.verify_zero_mode with
      | Some value -> `String (string_of_access_mode value)
      | None -> `Null
    end;
    "verify_range_mode",
    begin
      match payload.verify_range_mode with
      | Some value -> `String (string_of_access_mode value)
      | None -> `Null
    end;
    "verify_bound_mode",
    begin
      match payload.verify_bound_mode with
      | Some value -> `String (string_of_access_mode value)
      | None -> `Null
    end;
    "proof_receipt_signer_mode",
    begin
      match payload.proof_receipt_signer_mode with
      | Some value -> `String (string_of_access_mode value)
      | None -> `Null
    end;
    "proof_receipt_class",
    begin
      match payload.proof_receipt_class with
      | Some value -> `String (Circle_hfhe_receipt_class.string_of_t value)
      | None -> `Null
    end;
    "pk_allowlist",
    begin
      match payload.pk_allowlist with
      | Some values -> `List (List.map (fun value -> `String value) values)
      | None -> `Null
    end;
    "require_live_key_policy",
    begin
      match payload.require_live_key_policy with
      | Some value -> `Bool value
      | None -> `Null
    end;
    "require_receipt_transport_binding",
    begin
      match payload.require_receipt_transport_binding with
      | Some value -> `Bool value
      | None -> `Null
    end;
    "encrypt_proof",
    begin
      match payload.encrypt_proof with
      | Some value -> `String (Circle_hfhe_proof.string_of_t value)
      | None -> `Null
    end;
    "decrypt_proof",
    begin
      match payload.decrypt_proof with
      | Some value -> `String (Circle_hfhe_proof.string_of_t value)
      | None -> `Null
    end;
  ]

let put_payload_of_yojson = function
  | `Assoc fields ->
    let read_optional_string key =
      match List.assoc_opt key fields with
      | Some (`String value) -> Ok (Some value)
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid hfhe policy field: " ^ key)
    in
    let read_optional_bool key =
      match List.assoc_opt key fields with
      | Some (`Bool value) -> Ok (Some value)
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid hfhe policy field: " ^ key)
    in
    let read_optional_string_list key =
      match List.assoc_opt key fields with
      | Some (`List values) ->
        Ok (Some (List.filter_map (function `String value -> Some value | _ -> None) values))
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid hfhe policy field: " ^ key)
    in
    begin
      match read_optional_string "load_pk_mode",
            read_optional_string "encrypt_mode",
            read_optional_string "decrypt_mode",
            read_optional_string "cipher_arithmetic_mode",
            read_optional_string "commit_mode",
            read_optional_string "pedersen_mode",
            read_optional_string "cipher_serde_mode",
            read_optional_string "pubkey_serde_mode",
            read_optional_string "verify_zero_mode",
            read_optional_string "verify_range_mode",
            read_optional_string "verify_bound_mode",
            read_optional_string "proof_receipt_signer_mode",
            read_optional_string "proof_receipt_class",
            read_optional_string_list "pk_allowlist",
            read_optional_bool "require_live_key_policy",
            read_optional_bool "require_receipt_transport_binding",
            read_optional_string "encrypt_proof",
            read_optional_string "decrypt_proof" with
      | Ok load_pk_mode_raw, Ok encrypt_mode_raw, Ok decrypt_mode_raw,
        Ok cipher_arithmetic_mode_raw, Ok commit_mode_raw, Ok pedersen_mode_raw,
        Ok cipher_serde_mode_raw, Ok pubkey_serde_mode_raw,
        Ok verify_zero_mode_raw, Ok verify_range_mode_raw, Ok verify_bound_mode_raw, Ok proof_receipt_signer_mode_raw,
        Ok proof_receipt_class_raw, Ok pk_allowlist, Ok require_live_key_policy, Ok require_receipt_transport_binding,
        Ok encrypt_proof_raw, Ok decrypt_proof_raw ->
        let parse_mode = function
          | Some raw ->
            begin
              match access_mode_of_string raw with
              | Ok value -> Ok (Some value)
              | Error _ as e -> e
            end
          | None -> Ok None
        in
        let parse_proof = function
          | Some raw ->
            begin
              match Circle_hfhe_proof.of_string raw with
              | Ok value -> Ok (Some value)
              | Error _ as e -> e
            end
          | None -> Ok None
        in
        let parse_receipt_class = function
          | Some raw ->
            begin
              match Circle_hfhe_receipt_class.of_string raw with
              | Ok value -> Ok (Some value)
              | Error _ as e -> e
            end
          | None -> Ok None
        in
        begin
          match parse_mode load_pk_mode_raw,
                parse_mode encrypt_mode_raw,
                parse_mode decrypt_mode_raw,
                parse_mode cipher_arithmetic_mode_raw,
                parse_mode commit_mode_raw,
                parse_mode pedersen_mode_raw,
                parse_mode cipher_serde_mode_raw,
                parse_mode pubkey_serde_mode_raw,
                parse_mode verify_zero_mode_raw,
                parse_mode verify_range_mode_raw,
                parse_mode verify_bound_mode_raw,
                parse_mode proof_receipt_signer_mode_raw,
                parse_receipt_class proof_receipt_class_raw,
                parse_proof encrypt_proof_raw,
                parse_proof decrypt_proof_raw with
          | Ok load_pk_mode, Ok encrypt_mode, Ok decrypt_mode,
            Ok cipher_arithmetic_mode, Ok commit_mode, Ok pedersen_mode,
            Ok cipher_serde_mode, Ok pubkey_serde_mode,
            Ok verify_zero_mode, Ok verify_range_mode, Ok verify_bound_mode, Ok proof_receipt_signer_mode, Ok proof_receipt_class,
            Ok encrypt_proof, Ok decrypt_proof ->
            Ok {
              load_pk_mode;
              encrypt_mode;
              decrypt_mode;
              cipher_arithmetic_mode;
              commit_mode;
              pedersen_mode;
              cipher_serde_mode;
              pubkey_serde_mode;
              verify_zero_mode;
              verify_range_mode;
              verify_bound_mode;
              proof_receipt_signer_mode;
              proof_receipt_class;
              pk_allowlist;
              require_live_key_policy;
              require_receipt_transport_binding;
              encrypt_proof;
              decrypt_proof;
            }
          | Error e, _, _, _, _, _, _, _, _, _, _, _, _, _, _
          | _, Error e, _, _, _, _, _, _, _, _, _, _, _, _, _
          | _, _, Error e, _, _, _, _, _, _, _, _, _, _, _, _
          | _, _, _, Error e, _, _, _, _, _, _, _, _, _, _, _
          | _, _, _, _, Error e, _, _, _, _, _, _, _, _, _, _
          | _, _, _, _, _, Error e, _, _, _, _, _, _, _, _, _
          | _, _, _, _, _, _, Error e, _, _, _, _, _, _, _, _
          | _, _, _, _, _, _, _, Error e, _, _, _, _, _, _, _
          | _, _, _, _, _, _, _, _, Error e, _, _, _, _, _, _
          | _, _, _, _, _, _, _, _, _, Error e, _, _, _, _, _
          | _, _, _, _, _, _, _, _, _, _, Error e, _, _, _, _
          | _, _, _, _, _, _, _, _, _, _, _, Error e, _, _, _
          | _, _, _, _, _, _, _, _, _, _, _, _, Error e, _, _
          | _, _, _, _, _, _, _, _, _, _, _, _, _, Error e, _
          | _, _, _, _, _, _, _, _, _, _, _, _, _, _, Error e ->
            Error e
        end
      | Error e, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _
      | _, Error e, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _
      | _, _, Error e, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _
      | _, _, _, Error e, _, _, _, _, _, _, _, _, _, _, _, _, _, _
      | _, _, _, _, Error e, _, _, _, _, _, _, _, _, _, _, _, _, _
      | _, _, _, _, _, Error e, _, _, _, _, _, _, _, _, _, _, _, _
      | _, _, _, _, _, _, Error e, _, _, _, _, _, _, _, _, _, _, _
      | _, _, _, _, _, _, _, Error e, _, _, _, _, _, _, _, _, _, _
      | _, _, _, _, _, _, _, _, Error e, _, _, _, _, _, _, _, _, _
      | _, _, _, _, _, _, _, _, _, Error e, _, _, _, _, _, _, _, _
      | _, _, _, _, _, _, _, _, _, _, Error e, _, _, _, _, _, _, _
      | _, _, _, _, _, _, _, _, _, _, _, Error e, _, _, _, _, _, _
      | _, _, _, _, _, _, _, _, _, _, _, _, Error e, _, _, _, _, _
      | _, _, _, _, _, _, _, _, _, _, _, _, _, Error e, _, _, _, _
      | _, _, _, _, _, _, _, _, _, _, _, _, _, _, Error e, _, _, _
      | _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Error e, _, _
      | _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Error e, _
      | _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Error e ->
        Error e
    end
  | _ ->
    Error "hfhe policy payload must be a json object"

let of_stored_values
    ~load_pk_mode_raw
    ~encrypt_mode_raw
    ~decrypt_mode_raw
    ~cipher_arithmetic_mode_raw
    ~commit_mode_raw
    ~pedersen_mode_raw
    ~cipher_serde_mode_raw
    ~pubkey_serde_mode_raw
    ~verify_zero_mode_raw
    ~verify_range_mode_raw
    ~verify_bound_mode_raw
    ~proof_receipt_signer_mode_raw
    ~proof_receipt_class_raw
    ~pk_allowlist_raw
    ~require_live_key_policy_raw
    ~require_receipt_transport_binding_raw
    ~encrypt_proof_raw
    ~decrypt_proof_raw : t =
  let parse_mode raw fallback =
    match raw with
    | Some raw ->
      begin
        match access_mode_of_string raw with
        | Ok value -> value
        | Error _ -> Deny
      end
    | None -> fallback
  in
  let parse_bool raw ~default ~invalid =
    match raw with
    | None -> default
    | Some raw ->
      let normalized =
        String.lowercase_ascii (String.trim raw) in
      begin
        match normalized with
        | "1"
        | "true"
        | "yes" ->
          true
        | "0"
        | "false"
        | "no" ->
          false
        | _ ->
          invalid
      end
  in
  let require_live_key_policy =
    parse_bool
      require_live_key_policy_raw
      ~default:default.require_live_key_policy
      ~invalid:true in
  let require_receipt_transport_binding =
    parse_bool
      require_receipt_transport_binding_raw
      ~default:default.require_receipt_transport_binding
      ~invalid:true in
  let strict_receipt_proof =
    if require_receipt_transport_binding then
      Circle_hfhe_proof.Bound_zero_receipt_v1
    else
      Circle_hfhe_proof.Bound_zero_v1 in
  let parse_proof raw fallback =
    match Circle_policy_value.string_value raw with
    | Some raw ->
      begin
        match Circle_hfhe_proof.of_string raw with
        | Ok value -> value
        | Error _ -> strict_receipt_proof
      end
    | None ->
      fallback
  in
  let parse_allowlist = function
    | None ->
      None
    | Some raw ->
      let trimmed = String.trim raw in
      begin
        try
          match Yojson.Safe.from_string trimmed with
          | `List items ->
            Some
              (List.filter_map
                 (function
                   | `String value -> Some (String.trim value)
                   | _ -> None)
                 items)
          | _ ->
            Some []
        with _ ->
          Some []
      end
  in
  {
    load_pk_mode = parse_mode load_pk_mode_raw default.load_pk_mode;
    encrypt_mode = parse_mode encrypt_mode_raw default.encrypt_mode;
    decrypt_mode = parse_mode decrypt_mode_raw default.decrypt_mode;
    cipher_arithmetic_mode =
      parse_mode cipher_arithmetic_mode_raw default.cipher_arithmetic_mode;
    commit_mode = parse_mode commit_mode_raw default.commit_mode;
    pedersen_mode = parse_mode pedersen_mode_raw default.pedersen_mode;
    cipher_serde_mode = parse_mode cipher_serde_mode_raw default.cipher_serde_mode;
    pubkey_serde_mode = parse_mode pubkey_serde_mode_raw default.pubkey_serde_mode;
    verify_zero_mode = parse_mode verify_zero_mode_raw default.verify_zero_mode;
    verify_range_mode = parse_mode verify_range_mode_raw default.verify_range_mode;
    verify_bound_mode = parse_mode verify_bound_mode_raw default.verify_bound_mode;
    proof_receipt_signer_mode =
      parse_mode proof_receipt_signer_mode_raw default.proof_receipt_signer_mode;
    proof_receipt_class =
      begin
        match Circle_policy_value.string_value proof_receipt_class_raw with
        | Some raw ->
          begin
            match Circle_hfhe_receipt_class.of_string raw with
            | Ok value -> value
            | Error _ ->
              if require_receipt_transport_binding then
                Circle_hfhe_receipt_class.Transport_bound
              else
                Circle_hfhe_receipt_class.Transport_bound
          end
        | None ->
          if require_receipt_transport_binding then
            Circle_hfhe_receipt_class.Transport_bound
          else
            default.proof_receipt_class
      end;
    pk_allowlist = parse_allowlist pk_allowlist_raw;
    require_live_key_policy;
    require_receipt_transport_binding;
    encrypt_proof = parse_proof encrypt_proof_raw default.encrypt_proof;
    decrypt_proof = parse_proof decrypt_proof_raw default.decrypt_proof
  }

let write_snapshot storage_tbl payload =
  let resolved : t = {
    load_pk_mode = Option.value ~default:default.load_pk_mode payload.load_pk_mode;
    encrypt_mode = Option.value ~default:default.encrypt_mode payload.encrypt_mode;
    decrypt_mode = Option.value ~default:default.decrypt_mode payload.decrypt_mode;
    cipher_arithmetic_mode =
      Option.value ~default:default.cipher_arithmetic_mode payload.cipher_arithmetic_mode;
    commit_mode = Option.value ~default:default.commit_mode payload.commit_mode;
    pedersen_mode = Option.value ~default:default.pedersen_mode payload.pedersen_mode;
    cipher_serde_mode =
      Option.value ~default:default.cipher_serde_mode payload.cipher_serde_mode;
    pubkey_serde_mode =
      Option.value ~default:default.pubkey_serde_mode payload.pubkey_serde_mode;
    verify_zero_mode = Option.value ~default:default.verify_zero_mode payload.verify_zero_mode;
    verify_range_mode = Option.value ~default:default.verify_range_mode payload.verify_range_mode;
    verify_bound_mode = Option.value ~default:default.verify_bound_mode payload.verify_bound_mode;
    proof_receipt_signer_mode =
      Option.value ~default:default.proof_receipt_signer_mode payload.proof_receipt_signer_mode;
    proof_receipt_class =
      Option.value ~default:default.proof_receipt_class payload.proof_receipt_class;
    pk_allowlist = payload.pk_allowlist;
    require_live_key_policy =
      Option.value ~default:default.require_live_key_policy payload.require_live_key_policy;
    require_receipt_transport_binding =
      Option.value
        ~default:default.require_receipt_transport_binding
        payload.require_receipt_transport_binding;
    encrypt_proof = Option.value ~default:default.encrypt_proof payload.encrypt_proof;
    decrypt_proof = Option.value ~default:default.decrypt_proof payload.decrypt_proof;
  } in
  Circle_policy_value.set_or_clear
    storage_tbl
    load_pk_mode_key
    (if resolved.load_pk_mode = default.load_pk_mode then None else Some (string_of_access_mode resolved.load_pk_mode));
  Circle_policy_value.set_or_clear
    storage_tbl
    encrypt_mode_key
    (if resolved.encrypt_mode = default.encrypt_mode then None else Some (string_of_access_mode resolved.encrypt_mode));
  Circle_policy_value.set_or_clear
    storage_tbl
    decrypt_mode_key
    (if resolved.decrypt_mode = default.decrypt_mode then None else Some (string_of_access_mode resolved.decrypt_mode));
  Circle_policy_value.set_or_clear
    storage_tbl
    cipher_arithmetic_mode_key
    (if resolved.cipher_arithmetic_mode = default.cipher_arithmetic_mode then None else Some (string_of_access_mode resolved.cipher_arithmetic_mode));
  Circle_policy_value.set_or_clear
    storage_tbl
    commit_mode_key
    (if resolved.commit_mode = default.commit_mode then None else Some (string_of_access_mode resolved.commit_mode));
  Circle_policy_value.set_or_clear
    storage_tbl
    pedersen_mode_key
    (if resolved.pedersen_mode = default.pedersen_mode then None else Some (string_of_access_mode resolved.pedersen_mode));
  Circle_policy_value.set_or_clear
    storage_tbl
    cipher_serde_mode_key
    (if resolved.cipher_serde_mode = default.cipher_serde_mode then None else Some (string_of_access_mode resolved.cipher_serde_mode));
  Circle_policy_value.set_or_clear
    storage_tbl
    pubkey_serde_mode_key
    (if resolved.pubkey_serde_mode = default.pubkey_serde_mode then None else Some (string_of_access_mode resolved.pubkey_serde_mode));
  Circle_policy_value.set_or_clear
    storage_tbl
    verify_zero_mode_key
    (if resolved.verify_zero_mode = default.verify_zero_mode then None else Some (string_of_access_mode resolved.verify_zero_mode));
  Circle_policy_value.set_or_clear
    storage_tbl
    verify_range_mode_key
    (if resolved.verify_range_mode = default.verify_range_mode then None else Some (string_of_access_mode resolved.verify_range_mode));
  Circle_policy_value.set_or_clear
    storage_tbl
    verify_bound_mode_key
    (if resolved.verify_bound_mode = default.verify_bound_mode then None else Some (string_of_access_mode resolved.verify_bound_mode));
  Circle_policy_value.set_or_clear
    storage_tbl
    proof_receipt_signer_mode_key
    (if resolved.proof_receipt_signer_mode = default.proof_receipt_signer_mode then None else Some (string_of_access_mode resolved.proof_receipt_signer_mode));
  Circle_policy_value.set_or_clear
    storage_tbl
    proof_receipt_class_key
    (if resolved.proof_receipt_class = default.proof_receipt_class then None else Some (Circle_hfhe_receipt_class.string_of_t resolved.proof_receipt_class));
  Circle_policy_value.set_or_clear
    storage_tbl
    pk_allowlist_key
    (match resolved.pk_allowlist with Some values when values <> [] -> Some (Circle_policy_value.json_string_list values) | _ -> None);
  Circle_policy_value.set_or_clear
    storage_tbl
    require_live_key_policy_key
    (if resolved.require_live_key_policy = default.require_live_key_policy then None else Some "false")
  ;
  Circle_policy_value.set_or_clear
    storage_tbl
    require_receipt_transport_binding_key
    (if resolved.require_receipt_transport_binding = default.require_receipt_transport_binding then None else Some "true")
  ;
  Circle_policy_value.set_or_clear
    storage_tbl
    encrypt_proof_key
    (if resolved.encrypt_proof = default.encrypt_proof then None else Some (Circle_hfhe_proof.string_of_t resolved.encrypt_proof));
  Circle_policy_value.set_or_clear
    storage_tbl
    decrypt_proof_key
    (if resolved.decrypt_proof = default.decrypt_proof then None else Some (Circle_hfhe_proof.string_of_t resolved.decrypt_proof))

let any_registered addr =
  Crypto.Address.is_valid_address addr

let mode_allows mode ~owner ~caller ~subject ~active_relay =
  match mode with
  | Deny -> false
  | Owner_only -> caller = owner
  | Caller_self -> caller = subject
  | Owner_or_caller -> caller = owner || caller = subject
  | Any_registered -> any_registered caller
  | Active_relay ->
    begin
      match active_relay with
      | Some relay_id -> caller = relay_id
      | None -> false
    end
  | Owner_or_active_relay ->
    caller = owner
    ||
    begin
      match active_relay with
      | Some relay_id -> caller = relay_id
      | None -> false
    end

let pk_allowed (policy : t) requested_addr =
  match policy.pk_allowlist with
  | Some allowlist -> List.mem requested_addr allowlist
  | None -> true

let load_pk_allowed (policy : t) ~owner ~caller ~requested_addr =
  pk_allowed policy requested_addr
  && mode_allows policy.load_pk_mode ~owner ~caller ~subject:requested_addr ~active_relay:None

let load_pk_allowed_with_transport (policy : t) ~owner ~caller ~requested_addr ~active_relay =
  pk_allowed policy requested_addr
  && mode_allows policy.load_pk_mode ~owner ~caller ~subject:requested_addr ~active_relay

let encrypt_allowed (policy : t) ~owner ~caller ~active_relay =
  mode_allows policy.encrypt_mode ~owner ~caller ~subject:caller ~active_relay

let decrypt_allowed (policy : t) ~owner ~caller ~active_relay =
  mode_allows policy.decrypt_mode ~owner ~caller ~subject:caller ~active_relay

let cipher_arithmetic_allowed (policy : t) ~owner ~caller ~active_relay =
  mode_allows policy.cipher_arithmetic_mode ~owner ~caller ~subject:caller ~active_relay

let commit_allowed (policy : t) ~owner ~caller ~active_relay =
  mode_allows policy.commit_mode ~owner ~caller ~subject:caller ~active_relay

let pedersen_allowed (policy : t) ~owner ~caller ~active_relay =
  mode_allows policy.pedersen_mode ~owner ~caller ~subject:caller ~active_relay

let cipher_serde_allowed (policy : t) ~owner ~caller ~active_relay =
  mode_allows policy.cipher_serde_mode ~owner ~caller ~subject:caller ~active_relay

let pubkey_serde_allowed (policy : t) ~owner ~caller ~active_relay =
  mode_allows policy.pubkey_serde_mode ~owner ~caller ~subject:caller ~active_relay

let verify_zero_allowed (policy : t) ~owner ~caller ~active_relay =
  mode_allows policy.verify_zero_mode ~owner ~caller ~subject:caller ~active_relay

let verify_range_allowed (policy : t) ~owner ~caller ~active_relay =
  mode_allows policy.verify_range_mode ~owner ~caller ~subject:caller ~active_relay

let verify_bound_allowed (policy : t) ~owner ~caller ~active_relay =
  mode_allows policy.verify_bound_mode ~owner ~caller ~subject:caller ~active_relay