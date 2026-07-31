(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Replay = Pvac_legacy_public_replay

type entry = {
  address : string;
  source_cipher_hash : string;
  total : int;
  decision : Replay.decision;
}

type enabled = {
  chain_id : string;
  snapshot_epoch : int;
  state_root : string;
  activation_epoch : int;
  root : string;
  entries : (string, entry) Hashtbl.t;
}

type t =
  | Disabled of string
  | Enabled of enabled

let schema = "octra_pvac_migration_entitlements_v2"
let max_artifact_bytes = 64 * 1024 * 1024
let max_entries = 1_000_000
let state_name = "migration_state.json"

let source_cipher_hash cipher =
  Digestif.SHA256.(digest_string cipher |> to_hex)

let disabled ~chain_id =
  Disabled chain_id

let hex_char = function
  | '0' .. '9' | 'a' .. 'f' -> true
  | _ -> false

let valid_hash value =
  String.length value = 64 && String.for_all hex_char value

let blocked reason =
  {
    Replay.audit_class = Replay.Poisoned;
    can_public_migrate = false;
    public_net = None;
    commitment_net = None;
    blockers = [reason];
    effects = [];
    reason;
  }

let audit_class_name = Replay.string_of_audit_class

let audit_class_of_string = function
  | "public_clean" -> Ok Replay.Public_clean
  | "hidden_witness" -> Ok Replay.Hidden_witness
  | "poisoned" -> Ok Replay.Poisoned
  | value -> Error ("invalid migration audit class = " ^ value)

let put_string buf value =
  Buffer.add_string buf (string_of_int (String.length value));
  Buffer.add_char buf ':';
  Buffer.add_string buf value

let put_option put buf = function
  | None -> Buffer.add_char buf '0'
  | Some value ->
    Buffer.add_char buf '1';
    put buf value

let put_bool buf value =
  Buffer.add_char buf (if value then '1' else '0')

let put_int buf value =
  put_string buf (string_of_int value)

let put_z buf value =
  put_string buf (Z.to_string value)

let encode_entry buf entry =
  let decision = entry.decision in
  put_string buf entry.address;
  put_string buf entry.source_cipher_hash;
  put_int buf entry.total;
  put_string buf (audit_class_name decision.audit_class);
  put_bool buf decision.can_public_migrate;
  put_option put_z buf decision.public_net;
  put_option put_string buf decision.commitment_net;
  put_int buf (List.length decision.blockers);
  List.iter (put_string buf) decision.blockers;
  put_string buf decision.reason

let encoded_payload ~chain_id ~snapshot_epoch ~state_root ~activation_epoch entries =
  let buf = Buffer.create 4096 in
  put_string buf schema;
  put_string buf chain_id;
  put_int buf snapshot_epoch;
  put_string buf state_root;
  put_int buf activation_epoch;
  put_int buf (List.length entries);
  List.iter (encode_entry buf) entries;
  Buffer.contents buf

let root_of ~chain_id ~snapshot_epoch ~state_root ~activation_epoch entries =
  encoded_payload
    ~chain_id
    ~snapshot_epoch
    ~state_root
    ~activation_epoch
    entries
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex

let valid_commitment value =
  try
    let point = Bytes.of_string (Base64.decode_exn value) in
    if Bytes.length point <> 32 then
      false
    else begin
      ignore (Pvac_ffi.pedersen_add (Pvac_ffi.pedersen_identity ()) point);
      true
    end
  with _ ->
    false

let validate_decision decision =
  match decision.Replay.commitment_net with
  | Some value when not (valid_commitment value) ->
    Error "migration commitment has invalid encoding"
  | _ when
      (match decision.public_net with
       | Some amount ->
         Z.sign amount < 0
         || Z.gt amount Denomination.max_supply
       | None -> false) ->
    Error "migration public_net is outside the supply envelope"
  | _ when decision.can_public_migrate
      && decision.audit_class <> Replay.Public_clean ->
    Error "public migration entitlement requires public_clean audit"
  | _ when decision.can_public_migrate
      && Option.is_none decision.public_net ->
    Error "public migration entitlement requires public_net"
  | _ when decision.can_public_migrate
      && decision.blockers <> [] ->
    Error "public migration entitlement must not contain blockers"
  | _ ->
    Ok ()

let validate_entry entry =
  if not (Crypto.Address.is_valid_address entry.address) then
    Error ("invalid migration address = " ^ entry.address)
  else if not (valid_hash entry.source_cipher_hash) then
    Error ("invalid migration source hash for address = " ^ entry.address)
  else if entry.total < 0 then
    Error ("invalid migration history count for address = " ^ entry.address)
  else
    Result.map_error
      (fun reason -> reason ^ " address = " ^ entry.address)
      (validate_decision entry.decision)

let rec validate_entries seen = function
  | [] -> Ok ()
  | entry :: rest ->
    if Hashtbl.mem seen entry.address then
      Error ("duplicate migration address = " ^ entry.address)
    else
      match validate_entry entry with
      | Error error -> Error error
      | Ok () ->
        Hashtbl.add seen entry.address ();
        validate_entries seen rest

let create ~chain_id ~snapshot_epoch ~state_root ~activation_epoch entries =
  if chain_id = "" then
    Error "migration chain_id is empty"
  else if snapshot_epoch < 0 then
    Error "migration snapshot_epoch is negative"
  else if not (valid_hash state_root) then
    Error "migration state_root must be lowercase sha256 hex"
  else if activation_epoch < 0 then
    Error "migration activation_epoch is negative"
  else if activation_epoch <= snapshot_epoch then
    Error "migration activation_epoch must follow snapshot_epoch"
  else if List.length entries > max_entries then
    Error "migration entitlement entry cap exceeded"
  else
    let entries =
      List.sort (fun left right -> String.compare left.address right.address) entries
    in
    match validate_entries (Hashtbl.create (List.length entries)) entries with
    | Error error -> Error error
    | Ok () ->
      let table = Hashtbl.create (List.length entries) in
      List.iter (fun entry -> Hashtbl.add table entry.address entry) entries;
      Ok
        (Enabled {
          chain_id;
          snapshot_epoch;
          state_root;
          activation_epoch;
          root =
            root_of
              ~chain_id
              ~snapshot_epoch
              ~state_root
              ~activation_epoch
              entries;
          entries = table;
        })

let root = function
  | Disabled _ -> None
  | Enabled value -> Some value.root

let activation_epoch = function
  | Disabled _ -> None
  | Enabled value -> Some value.activation_epoch

let snapshot_epoch = function
  | Disabled _ -> None
  | Enabled value -> Some value.snapshot_epoch

let state_root = function
  | Disabled _ -> None
  | Enabled value -> Some value.state_root

let bind_snapshot t lookup =
  match t with
  | Disabled _ -> Ok ()
  | Enabled value ->
    begin
      match lookup value.snapshot_epoch with
      | Error reason ->
        Error ("migration snapshot unavailable: " ^ reason)
      | Ok root when String.equal root value.state_root ->
        Ok ()
      | Ok _ ->
        Error "migration snapshot state root mismatch"
    end

let entry_count = function
  | Disabled _ -> 0
  | Enabled value -> Hashtbl.length value.entries

let exact_fields scope expected fields =
  let names = List.map fst fields in
  let unique = List.sort_uniq String.compare names in
  let expected = List.sort String.compare expected in
  if List.length names <> List.length unique then
    Error ("duplicate migration field in " ^ scope)
  else if unique <> expected then
    Error ("unexpected migration fields in " ^ scope)
  else
    Ok ()

let find t ~epoch ~address ~cipher =
  match t with
  | Disabled _ ->
    Error "migration entitlement artifact unavailable"
  | Enabled value when epoch < value.activation_epoch ->
    Error "migration entitlement is not active"
  | Enabled value ->
    begin
      match Hashtbl.find_opt value.entries address with
      | None -> Error "migration entitlement not found"
      | Some entry when
          not (String.equal entry.source_cipher_hash (source_cipher_hash cipher)) ->
        Error "migration entitlement source ciphertext mismatch"
      | Some entry -> Ok entry
    end

let decision t ~epoch ~address ~cipher =
  match find t ~epoch ~address ~cipher with
  | Ok entry -> entry.decision
  | Error reason -> blocked reason

let string_field fields name =
  match List.assoc_opt name fields with
  | Some (`String value) -> Ok value
  | _ -> Error ("migration field must be string = " ^ name)

let int_field fields name =
  match List.assoc_opt name fields with
  | Some (`Int value) -> Ok value
  | _ -> Error ("migration field must be integer = " ^ name)

let bool_field fields name =
  match List.assoc_opt name fields with
  | Some (`Bool value) -> Ok value
  | _ -> Error ("migration field must be boolean = " ^ name)

let string_list_field fields name =
  match List.assoc_opt name fields with
  | Some (`List values) ->
    let rec parse acc = function
      | [] -> Ok (List.rev acc)
      | `String value :: rest -> parse (value :: acc) rest
      | _ -> Error ("migration field must be a string list = " ^ name)
    in
    parse [] values
  | _ -> Error ("migration field must be a string list = " ^ name)

let option_string_field fields name =
  match List.assoc_opt name fields with
  | Some `Null | None -> Ok None
  | Some (`String value) -> Ok (Some value)
  | _ -> Error ("migration field must be string or null = " ^ name)

let option_z_field fields name =
  match List.assoc_opt name fields with
  | Some `Null | None -> Ok None
  | Some (`String value) ->
    begin
      try Ok (Some (Z.of_string value))
      with _ -> Error ("migration field must be decimal or null = " ^ name)
    end
  | _ -> Error ("migration field must be decimal or null = " ^ name)

let bind result next =
  match result with
  | Ok value -> next value
  | Error error -> Error error

let parse_entry = function
  | `Assoc fields ->
    bind
      (exact_fields
         "entry"
         [
           "address";
           "source_cipher_hash";
           "total";
           "audit_class";
           "can_public_migrate";
           "public_net";
           "commitment_net";
           "blockers";
           "reason";
         ]
         fields)
      (fun () ->
    bind (string_field fields "address") (fun address ->
    bind (string_field fields "source_cipher_hash") (fun source_cipher_hash ->
    bind (int_field fields "total") (fun total ->
    bind (string_field fields "audit_class") (fun audit_class ->
    bind (audit_class_of_string audit_class) (fun audit_class ->
    bind (bool_field fields "can_public_migrate") (fun can_public_migrate ->
    bind (option_z_field fields "public_net") (fun public_net ->
    bind (option_string_field fields "commitment_net") (fun commitment_net ->
    bind (string_list_field fields "blockers") (fun blockers ->
    bind (string_field fields "reason") (fun reason ->
      Ok {
        address;
        source_cipher_hash;
        total;
        decision = {
          Replay.audit_class;
          can_public_migrate;
          public_net;
          commitment_net;
          blockers;
          effects = [];
          reason;
        };
      })))))))))))
  | _ -> Error "migration entry must be an object"

let parse_entries = function
  | `List values ->
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | value :: rest ->
        bind (parse_entry value) (fun entry -> loop (entry :: acc) rest)
    in
    loop [] values
  | _ -> Error "migration entries must be a list"

let load_json ~chain_id ~expected_root = function
  | `Assoc fields ->
    bind
      (exact_fields
         "artifact"
         [
           "schema";
           "chain_id";
           "snapshot_epoch";
           "state_root";
           "activation_epoch";
           "root";
           "entries";
         ]
         fields)
      (fun () ->
    bind (string_field fields "schema") (fun artifact_schema ->
    if artifact_schema <> schema then
      Error ("unsupported migration schema = " ^ artifact_schema)
    else
      bind (string_field fields "chain_id") (fun artifact_chain_id ->
      if artifact_chain_id <> chain_id then
        Error "migration chain_id mismatch"
      else
        bind (int_field fields "snapshot_epoch") (fun snapshot_epoch ->
        bind (string_field fields "state_root") (fun state_root ->
        bind (int_field fields "activation_epoch") (fun activation_epoch ->
        bind (string_field fields "root") (fun artifact_root ->
        if artifact_root <> expected_root then
          Error "migration artifact root does not match configured root"
        else
        bind
          (match List.assoc_opt "entries" fields with
           | Some values -> parse_entries values
           | None -> Error "migration entries missing")
          (fun entries ->
          bind
            (create
               ~chain_id
               ~snapshot_epoch
               ~state_root
               ~activation_epoch
               entries)
            (fun value ->
          match root value with
          | Some actual when String.equal actual expected_root -> Ok value
          | Some _ -> Error "migration entitlement root mismatch"
          | None -> Error "migration entitlement artifact disabled")))))))))
  | _ -> Error "migration entitlement artifact must be an object"

let configured getenv name =
  match getenv name with
  | Some value when value <> "" -> Some value
  | Some _
  | None -> None

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let descriptor = Unix.descr_of_in_channel channel in
      let before = Unix.fstat descriptor in
      if before.Unix.st_kind <> Unix.S_REG then
        Error "migration entitlement artifact must be a regular file"
      else if before.Unix.st_size > max_artifact_bytes then
        Error "migration entitlement artifact exceeds size cap"
      else
        let body = really_input_string channel before.Unix.st_size in
        let has_suffix =
          match input_char channel with
          | _ -> true
          | exception End_of_file -> false
        in
        let after = Unix.fstat descriptor in
        if has_suffix || after.Unix.st_size <> before.Unix.st_size then
          Error "migration entitlement artifact changed while loading"
        else Ok body)

let load_body ~chain_id ~expected_root body =
  try
    load_json
      ~chain_id
      ~expected_root
      (Yojson.Safe.from_string body)
  with exn ->
    Error ("migration entitlement parse failed: " ^ Printexc.to_string exn)

let load_file ~chain_id ~expected_root path =
  bind
    (read_file path)
    (load_body ~chain_id ~expected_root)

let state_path data_dir =
  Filename.concat (Filename.concat data_dir "pvac") state_name

let load_env ~chain_id ~data_dir ~getenv =
  match configured getenv "OCTRA_PVAC_MIGRATION_ROOT" with
  | None ->
    Ok (disabled ~chain_id)
  | Some expected_root when not (valid_hash expected_root) ->
    Error "OCTRA_PVAC_MIGRATION_ROOT must be lowercase sha256 hex"
  | Some expected_root ->
    let path = state_path data_dir in
    if not (Sys.file_exists path) then
      Error "migration entitlement state is missing from node data"
    else begin
      try
        load_file ~chain_id ~expected_root path
      with exn ->
        Error ("migration entitlement load failed: " ^ Printexc.to_string exn)
    end

let option_json encode = function
  | None -> `Null
  | Some value -> encode value

let entry_json entry =
  let decision = entry.decision in
  `Assoc [
    "address", `String entry.address;
    "source_cipher_hash", `String entry.source_cipher_hash;
    "total", `Int entry.total;
    "audit_class", `String (audit_class_name decision.audit_class);
    "can_public_migrate", `Bool decision.can_public_migrate;
    "public_net", option_json (fun value -> `String (Z.to_string value)) decision.public_net;
    "commitment_net", option_json (fun value -> `String value) decision.commitment_net;
    "blockers", `List (List.map (fun value -> `String value) decision.blockers);
    "reason", `String decision.reason;
  ]

let to_yojson = function
  | Disabled _ ->
    Error "disabled migration entitlement cannot be serialized"
  | Enabled value ->
    let entries =
      Hashtbl.fold (fun _ entry acc -> entry :: acc) value.entries []
      |> List.sort (fun left right -> String.compare left.address right.address)
    in
    Ok
      (`Assoc [
        "schema", `String schema;
        "chain_id", `String value.chain_id;
        "snapshot_epoch", `Int value.snapshot_epoch;
        "state_root", `String value.state_root;
        "activation_epoch", `Int value.activation_epoch;
        "root", `String value.root;
        "entries", `List (List.map entry_json entries);
      ])