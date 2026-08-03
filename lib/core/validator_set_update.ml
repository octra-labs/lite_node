(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type validator_entry = {
  address : string;
  pubkey_b64 : string;
  weight : Z.t;
}

type t = {
  activate_epoch : int64;
  source_epoch : int64 option;
  validators : validator_entry list;
  weighted : bool;
  fingerprint : string;
}

type ready = {
  fingerprint : string;
  head_epoch : int64;
  state_root : string;
}

type ready_extra = {
  chain_id : string option;
  binary_hash : string option;
  config_hash : string option;
  catchup_head_epoch : int64 option;
  shadow_epochs : int option;
}

type ready_ext = {
  ready : ready;
  extra : ready_extra;
}

let pending_meta_key = "bft.validator_set.pending"
let active_meta_key = "bft.validator_set.active"

let ready_meta_key ~fingerprint ~address =
  "bft.validator_set.ready." ^ fingerprint ^ "." ^ address

let normalize_pubkey pubkey_b64 =
  try
    let raw = Base64.decode_exn pubkey_b64 in
    if String.length raw = 32 then Some (Base64.encode_exn raw)
    else None
  with _ -> None

let normalize_validator_entry ~weighted address pubkey_b64 weight =
  match normalize_pubkey pubkey_b64 with
  | None -> Error "invalid validator public key"
  | Some normalized_pubkey ->
    if not (Crypto.Address.is_valid_address address) then
      Error "invalid validator address"
    else if Z.sign weight <= 0 then
      Error "validator weight must be positive"
    else if not weighted
            && not
                 (Crypto.Address.verify_address_pubkey
                    address
                    normalized_pubkey) then
      Error "validator address does not match public key"
    else
      Ok {
        address;
        pubkey_b64 = normalized_pubkey;
        weight;
      }

let canonical_validators validators =
  let sorted = List.sort (fun a b -> String.compare a.address b.address) validators in
  let rec loop addresses pubkeys = function
    | [] -> Ok sorted
    | v :: rest ->
      if List.mem v.address addresses then
        Error "duplicate validator address"
      else if List.mem v.pubkey_b64 pubkeys then
        Error "duplicate validator public key"
      else
        loop
          (v.address :: addresses)
          (v.pubkey_b64 :: pubkeys)
          rest
  in
  loop [] [] sorted

let fingerprint validators =
  validators
  |> List.map (fun v -> v.address ^ ":" ^ v.pubkey_b64)
  |> String.concat ","
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex

let put_string buffer value =
  Buffer.add_string buffer (string_of_int (String.length value));
  Buffer.add_char buffer ':';
  Buffer.add_string buffer value

let weighted_fingerprint ~source_epoch ~activate_epoch validators =
  let buffer = Buffer.create 512 in
  put_string buffer "octra:validator_set_update:bonded";
  put_string buffer (Int64.to_string source_epoch);
  put_string buffer (Int64.to_string activate_epoch);
  List.iter
    (fun validator ->
      put_string buffer validator.address;
      put_string buffer validator.pubkey_b64;
      put_string buffer (Z.to_string validator.weight))
    validators;
  buffer
  |> Buffer.contents
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex

let validator_entry_to_yojson ~weighted v =
  let fields = [
    "address", `String v.address;
    "pubkey", `String v.pubkey_b64;
  ] in
  `Assoc (
    if weighted then
      fields @ ["weight", `String (Z.to_string v.weight)]
    else
      fields)

let parse_weight = function
  | `String raw ->
    begin
      try
        let weight = Z.of_string raw in
        if Z.sign weight <= 0 then
          Error "validator weight must be positive"
        else
          Ok weight
      with _ ->
        Error "invalid validator weight"
    end
  | `Int value when value > 0 -> Ok (Z.of_int value)
  | _ -> Error "validator weight must be a positive integer"

let validator_entry_of_yojson ~weighted = function
  | `Assoc fields ->
    begin
      match List.assoc_opt "address" fields, List.assoc_opt "pubkey" fields with
      | Some (`String address), Some (`String pubkey_b64) ->
        let weight =
          if weighted then
            match List.assoc_opt "weight" fields with
            | None -> Error "weighted validator entry requires weight"
            | Some value -> parse_weight value
          else
            Ok Z.one
        in
        begin
          match weight with
          | Error _ as error -> error
          | Ok weight ->
            normalize_validator_entry
              ~weighted
              address
              pubkey_b64
              weight
        end
      | _ -> Error "validator entry requires address and pubkey"
    end
  | _ -> Error "validator entry must be object"

let to_yojson t =
  let fields = [
    "activate_epoch", `String (Int64.to_string t.activate_epoch);
    "fingerprint", `String t.fingerprint;
    "validators",
      `List
        (List.map
           (validator_entry_to_yojson ~weighted:t.weighted)
           t.validators);
  ] in
  if t.weighted then
    `Assoc (
      fields @ [
        "standard", `String Validator_policy.standard_name;
        "weighted", `Bool true;
        "source_epoch",
          `String
            (Int64.to_string
               (Option.get t.source_epoch));
      ])
  else
    `Assoc fields

let parse_epoch label = function
  | `String raw ->
    begin
      try Ok (Int64.of_string raw)
      with _ -> Error ("invalid " ^ label)
    end
  | `Int value -> Ok (Int64.of_int value)
  | _ -> Error (label ^ " must be string or int")

let parse_weighted fields =
  match List.assoc_opt "weighted" fields with
  | None -> Ok false
  | Some (`Bool value) -> Ok value
  | Some _ -> Error "weighted must be bool"

let parse_source_epoch ~weighted fields =
  match weighted, List.assoc_opt "source_epoch" fields with
  | false, None -> Ok None
  | false, Some _ -> Error "legacy validator update rejects source_epoch"
  | true, Some value -> Result.map Option.some (parse_epoch "source_epoch" value)
  | true, None -> Error "weighted validator update requires source_epoch"

let validate_standard ~weighted fields =
  match weighted, List.assoc_opt "standard" fields with
  | false, None -> Ok ()
  | false, Some _ -> Error "legacy validator update rejects standard"
  | true, Some (`String value)
      when String.equal value Validator_policy.standard_name -> Ok ()
  | true, _ -> Error "weighted validator update standard mismatch"

let of_yojson = function
  | `Assoc fields ->
    begin
      match List.assoc_opt "activate_epoch" fields, List.assoc_opt "validators" fields with
      | Some epoch_json, Some (`List validator_jsons) ->
        begin
          match
            parse_epoch "activate_epoch" epoch_json,
            parse_weighted fields
          with
          | Error error, _
          | _, Error error -> Error error
          | Ok activate_epoch, Ok weighted ->
            begin
              match
                parse_source_epoch ~weighted fields,
                validate_standard ~weighted fields
              with
              | Error error, _
              | _, Error error -> Error error
              | Ok source_epoch, Ok () ->
                let validators =
                  List.fold_left
                    (fun acc json ->
                      match acc with
                      | Error _ -> acc
                      | Ok items ->
                        match validator_entry_of_yojson ~weighted json with
                        | Error e -> Error e
                        | Ok item -> Ok (item :: items))
                    (Ok [])
                    validator_jsons
                in
                begin
                  match validators with
                  | Error e -> Error e
                  | Ok validators_rev ->
                    let validators = List.rev validators_rev in
                    if validators = [] then
                      Error "validator set cannot be empty"
                    else
                      match canonical_validators validators with
                      | Error e -> Error e
                      | Ok validators ->
                        let actual =
                          match source_epoch with
                          | None -> fingerprint validators
                          | Some source_epoch ->
                            weighted_fingerprint
                              ~source_epoch
                              ~activate_epoch
                              validators
                        in
                        if
                          Option.fold
                            ~none:false
                            ~some:(fun source ->
                              Int64.compare source activate_epoch >= 0)
                            source_epoch
                        then
                          Error
                            "weighted validator source epoch must precede activation"
                        else
                          begin
                            match List.assoc_opt "fingerprint" fields with
                            | Some (`String expected)
                                when expected <> actual ->
                              Error "validator set fingerprint mismatch"
                            | _ ->
                              Ok {
                                activate_epoch;
                                source_epoch;
                                validators;
                                weighted;
                                fingerprint = actual;
                              }
                          end
                end
            end
        end
      | _ -> Error "validator_set_update requires activate_epoch and validators"
    end
  | _ -> Error "validator_set_update payload must be object"

let make_weighted ~source_epoch ~activate_epoch members =
  if Int64.compare source_epoch activate_epoch >= 0 then
    Error "weighted validator source epoch must precede activation"
  else
    let entries =
      List.map
        (fun (member : Validator_admission.member) ->
          {
            address = member.address;
            pubkey_b64 = Base64.encode_exn member.pubkey;
            weight = member.weight;
          })
        members
    in
    match canonical_validators entries with
    | Error _ as error -> error
    | Ok validators ->
      if validators = [] then
        Error "validator set cannot be empty"
      else
        Ok {
          activate_epoch;
          source_epoch = Some source_epoch;
          validators;
          weighted = true;
          fingerprint =
            weighted_fingerprint
              ~source_epoch
              ~activate_epoch
              validators;
        }

let of_message = function
  | None -> Error "validator_set_update requires message payload"
  | Some payload ->
    try payload |> Yojson.Safe.from_string |> of_yojson
    with exn -> Error (Printexc.to_string exn)

let to_string t =
  t |> to_yojson |> Yojson.Safe.to_string

let of_string s =
  try s |> Yojson.Safe.from_string |> of_yojson
  with exn -> Error (Printexc.to_string exn)

let validator_set update =
  let entries =
    List.fold_left
      (fun state entry ->
        match state, Base64.decode entry.pubkey_b64 with
        | Error _ as error, _ -> error
        | Ok _, Error (`Msg reason) -> Error reason
        | Ok items, Ok pubkey ->
            Ok (
              (Octra_consensus.C_types.{
                address = entry.address;
                pubkey;
              }, entry.weight)
              :: items))
      (Ok [])
      update.validators
    |> Result.map List.rev
  in
  match entries with
  | Error _ as error -> error
  | Ok entries when update.weighted ->
      Octra_consensus.C_types.make_weighted_validator_set entries
  | Ok entries ->
      Ok (
        entries
        |> List.map fst
        |> Octra_consensus.C_types.make_validator_set)

let empty_ready_extra = {
  chain_id = None;
  binary_hash = None;
  config_hash = None;
  catchup_head_epoch = None;
  shadow_epochs = None;
}

let ready_to_yojson r =
  `Assoc [
    "fingerprint", `String r.fingerprint;
    "head_epoch", `String (Int64.to_string r.head_epoch);
    "state_root", `String r.state_root;
  ]

let field_string fields key =
  match List.assoc_opt key fields with
  | None -> Ok None
  | Some (`String s) -> Ok (Some s)
  | Some _ -> Error (key ^ " must be string")

let field_int64 fields key =
  match List.assoc_opt key fields with
  | None -> Ok None
  | Some (`String s) ->
    begin
      try Ok (Some (Int64.of_string s))
      with _ -> Error ("invalid " ^ key)
    end
  | Some (`Int n) -> Ok (Some (Int64.of_int n))
  | Some _ -> Error (key ^ " must be string or int")

let field_int fields key =
  match List.assoc_opt key fields with
  | None -> Ok None
  | Some (`String s) ->
    begin
      try Ok (Some (int_of_string s))
      with _ -> Error ("invalid " ^ key)
    end
  | Some (`Int n) -> Ok (Some n)
  | Some _ -> Error (key ^ " must be string or int")

let bind f result =
  match result with
  | Error e -> Error e
  | Ok v -> f v

let validate_optional_hash key = function
  | None -> Ok None
  | Some value ->
    if String.length value = 64 then Ok (Some value)
    else Error ("invalid " ^ key)

let validate_optional_chain_id = function
  | None -> Ok None
  | Some value ->
    if value = "" then Error "invalid chain_id"
    else Ok (Some value)

let validate_optional_int64_nonnegative key = function
  | None -> Ok None
  | Some value ->
    if Int64.compare value 0L < 0 then Error ("invalid " ^ key)
    else Ok (Some value)

let validate_optional_int_nonnegative key = function
  | None -> Ok None
  | Some value ->
    if value < 0 then Error ("invalid " ^ key)
    else Ok (Some value)

let ready_ext_to_yojson r =
  let add_string key value fields =
    match value with
    | None -> fields
    | Some s -> (key, `String s) :: fields
  in
  let add_int64 key value fields =
    match value with
    | None -> fields
    | Some n -> (key, `String (Int64.to_string n)) :: fields
  in
  let add_int key value fields =
    match value with
    | None -> fields
    | Some n -> (key, `String (string_of_int n)) :: fields
  in
  let base =
    match ready_to_yojson r.ready with
    | `Assoc fields -> fields
    | _ -> []
  in
  base
  |> add_string "chain_id" r.extra.chain_id
  |> add_string "binary_hash" r.extra.binary_hash
  |> add_string "config_hash" r.extra.config_hash
  |> add_int64 "catchup_head_epoch" r.extra.catchup_head_epoch
  |> add_int "shadow_epochs" r.extra.shadow_epochs
  |> List.rev
  |> fun fields -> `Assoc fields

let ready_ext_of_yojson = function
  | `Assoc fields ->
    begin
      match List.assoc_opt "fingerprint" fields,
            List.assoc_opt "head_epoch" fields,
            List.assoc_opt "state_root" fields with
      | Some (`String fingerprint), Some epoch_json, Some (`String state_root) ->
        let head_epoch =
          match epoch_json with
          | `String s -> (try Ok (Int64.of_string s) with _ -> Error "invalid head_epoch")
          | `Int n -> Ok (Int64.of_int n)
          | _ -> Error "head_epoch must be string or int"
        in
        begin
          match head_epoch with
          | Error e -> Error e
          | Ok head_epoch ->
            if String.length fingerprint <> 64 then Error "invalid fingerprint"
            else if String.length state_root <> 64 && String.length state_root <> 128 then Error "invalid state_root"
            else
              field_string fields "chain_id"
              |> bind validate_optional_chain_id
              |> bind (fun chain_id ->
                field_string fields "binary_hash"
                |> bind (validate_optional_hash "binary_hash")
                |> bind (fun binary_hash ->
                  field_string fields "config_hash"
                  |> bind (validate_optional_hash "config_hash")
                  |> bind (fun config_hash ->
                    field_int64 fields "catchup_head_epoch"
                    |> bind (validate_optional_int64_nonnegative "catchup_head_epoch")
                    |> bind (fun catchup_head_epoch ->
                      field_int fields "shadow_epochs"
                      |> bind (validate_optional_int_nonnegative "shadow_epochs")
                      |> bind (fun shadow_epochs ->
                        Ok {
                          ready = { fingerprint; head_epoch; state_root };
                          extra = {
                            chain_id;
                            binary_hash;
                            config_hash;
                            catchup_head_epoch;
                            shadow_epochs;
                          };
                        })))))
        end
      | _ -> Error "validator_ready requires fingerprint, head_epoch, state_root"
    end
  | _ -> Error "validator_ready payload must be object"

let ready_of_yojson json =
  match ready_ext_of_yojson json with
  | Ok r -> Ok r.ready
  | Error e -> Error e

let ready_of_message = function
  | None -> Error "validator_ready requires message payload"
  | Some payload ->
    try payload |> Yojson.Safe.from_string |> ready_of_yojson
    with exn -> Error (Printexc.to_string exn)

let ready_ext_of_message = function
  | None -> Error "validator_ready requires message payload"
  | Some payload ->
    try payload |> Yojson.Safe.from_string |> ready_ext_of_yojson
    with exn -> Error (Printexc.to_string exn)

let ready_to_string r =
  r |> ready_to_yojson |> Yojson.Safe.to_string

let ready_of_string s =
  try s |> Yojson.Safe.from_string |> ready_of_yojson
  with exn -> Error (Printexc.to_string exn)

let ready_ext_to_string r =
  r |> ready_ext_to_yojson |> Yojson.Safe.to_string

let ready_ext_of_string s =
  try s |> Yojson.Safe.from_string |> ready_ext_of_yojson
  with exn -> Error (Printexc.to_string exn)