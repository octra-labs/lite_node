(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  activate_after_epoch : int64 option;
  expire_after_epoch : int64 option;
  revoked : bool;
  erased : bool;
}

type put_payload = {
  key_id : string;
  activate_after_epoch : int64 option;
  expire_after_epoch : int64 option;
  revoked : bool;
  erased : bool;
}

let default : t = {
  activate_after_epoch = None;
  expire_after_epoch = None;
  revoked = false;
  erased = false;
}

let key_base key_id =
  "key_policy:" ^ key_id ^ ":"

let activate_after_key key_id =
  key_base key_id ^ "activate_after_epoch"

let expire_after_key key_id =
  key_base key_id ^ "expire_after_epoch"

let revoked_key key_id =
  key_base key_id ^ "revoked"

let erased_key key_id =
  key_base key_id ^ "erased"

let yojson_of_t key_id (policy : t) =
  `Assoc [
    "key_id", `String key_id;
    "activate_after_epoch",
    begin
      match policy.activate_after_epoch with
      | Some value -> `String (Int64.to_string value)
      | None -> `Null
    end;
    "expire_after_epoch",
    begin
      match policy.expire_after_epoch with
      | Some value -> `String (Int64.to_string value)
      | None -> `Null
    end;
    "revoked", `Bool policy.revoked;
    "erased", `Bool policy.erased;
  ]

let put_payload_of_t key_id (policy : t) =
  {
    key_id;
    activate_after_epoch = policy.activate_after_epoch;
    expire_after_epoch = policy.expire_after_epoch;
    revoked = policy.revoked;
    erased = policy.erased;
  }

let yojson_of_put_payload payload =
  `Assoc [
    "key_id", `String payload.key_id;
    "activate_after_epoch",
    begin
      match payload.activate_after_epoch with
      | Some value -> `String (Int64.to_string value)
      | None -> `Null
    end;
    "expire_after_epoch",
    begin
      match payload.expire_after_epoch with
      | Some value -> `String (Int64.to_string value)
      | None -> `Null
    end;
    "revoked", `Bool payload.revoked;
    "erased", `Bool payload.erased;
  ]

let put_payload_of_yojson = function
  | `Assoc fields ->
    let read_string key =
      match List.assoc_opt key fields with
      | Some (`String value) -> Ok value
      | _ -> Error ("missing key policy field: " ^ key)
    in
    let read_optional_i64 key =
      match List.assoc_opt key fields with
      | Some (`String value) -> Ok (Some (Int64.of_string value))
      | Some (`Int value) -> Ok (Some (Int64.of_int value))
      | Some (`Intlit value) -> Ok (Some (Int64.of_string value))
      | Some `Null | None -> Ok None
      | _ -> Error ("invalid key policy field: " ^ key)
    in
    let read_bool key =
      match List.assoc_opt key fields with
      | Some (`Bool value) -> Ok value
      | Some `Null | None -> Ok false
      | _ -> Error ("invalid key policy field: " ^ key)
    in
    begin
      match read_string "key_id",
            read_optional_i64 "activate_after_epoch",
            read_optional_i64 "expire_after_epoch",
            read_bool "revoked",
            read_bool "erased" with
      | Ok key_id, Ok activate_after_epoch, Ok expire_after_epoch, Ok revoked, Ok erased ->
        Ok {
          key_id;
          activate_after_epoch;
          expire_after_epoch;
          revoked;
          erased;
        }
      | Error e, _, _, _, _
      | _, Error e, _, _, _
      | _, _, Error e, _, _
      | _, _, _, Error e, _
      | _, _, _, _, Error e ->
        Error e
    end
  | _ ->
    Error "key policy payload must be a json object"

let of_stored_values ~activate_after_raw ~expire_after_raw ~revoked_raw ~erased_raw : t =
  {
    activate_after_epoch = Circle_policy_value.int64_value activate_after_raw;
    expire_after_epoch = Circle_policy_value.int64_value expire_after_raw;
    revoked = Circle_policy_value.bool_value revoked_raw;
    erased = Circle_policy_value.bool_value erased_raw;
  }

let write_snapshot storage_tbl payload =
  Circle_policy_value.set_or_clear
    storage_tbl
    (activate_after_key payload.key_id)
    (Option.map Int64.to_string payload.activate_after_epoch);
  Circle_policy_value.set_or_clear
    storage_tbl
    (expire_after_key payload.key_id)
    (Option.map Int64.to_string payload.expire_after_epoch);
  Circle_policy_value.set_or_clear
    storage_tbl
    (revoked_key payload.key_id)
    (if payload.revoked then Some "true" else None);
  Circle_policy_value.set_or_clear
    storage_tbl
    (erased_key payload.key_id)
    (if payload.erased then Some "true" else None)

let live (policy : t) current_epoch =
  let activate_ok =
    match policy.activate_after_epoch with
    | Some value -> Int64.compare current_epoch value >= 0
    | None -> true
  in
  let expire_ok =
    match policy.expire_after_epoch with
    | Some value -> Int64.compare current_epoch value < 0
    | None -> true
  in
  not policy.revoked && not policy.erased && activate_ok && expire_ok