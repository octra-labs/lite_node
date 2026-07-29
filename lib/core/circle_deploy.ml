(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

type source =
  | Direct of {
      deployer : string;
      nonce : int;
    }
  | Spawn of {
      parent : string;
      caller : string;
      tx_hash : string;
      spawn_nonce : int;
      owner_mode : Circles.spawn_owner;
      payload_json : string;
    }

type prepared = {
  circle_id : string;
  owner : string;
  code_raw : string;
  info : Circles.circle_info;
}

let spawn_payload_json_cap = 2 * 1024 * 1024

let owner_of_source = function
  | Direct { deployer; _ } ->
    deployer
  | Spawn { parent; caller; owner_mode; _ } ->
    begin
      match owner_mode with
      | Circles.Spawn_owner_caller -> caller
      | Circles.Spawn_owner_parent -> parent
    end

let circle_id_of_source source (payload : Circles.deploy_payload) =
  match source with
  | Direct { deployer; nonce } ->
    Circles.circle_id_of_deploy ~deployer ~nonce payload
  | Spawn { parent; caller; tx_hash; spawn_nonce; owner_mode; payload_json } ->
    Circles.circle_id_of_spawn
      ~parent
      ~caller
      ~tx_hash
      ~spawn_nonce
      ~owner_mode
      ~payload_json

let decode_payload_json payload_json =
  try
    match Circles.deploy_payload_of_yojson (Yojson.Safe.from_string payload_json) with
    | Ok payload -> Ok payload
    | Error e -> Error ("malformed_transaction", e)
  with e ->
    Error ("malformed_transaction", Printexc.to_string e)

let decode_spawn_payload_json payload_json =
  if String.length payload_json > spawn_payload_json_cap then
    Error ("malformed_transaction", "circle spawn payload exceeds max size")
  else
    decode_payload_json payload_json

let prepare source (payload : Circles.deploy_payload) =
  try
    let code_raw =
      match payload.Circles.code_b64 with
      | Some code_b64 -> Base64.decode_exn code_b64
      | None -> ""
    in
    let code_size = Int64.of_int (String.length code_raw) in
    match Circles.validate_limits payload.limits with
    | Error reason -> Error ("circle_limits_invalid", reason)
    | Ok () ->
      if Int64.compare code_size payload.limits.max_wasm_bytes > 0 then
        Error ("circle_code_too_large", "circle code exceeds declared max_wasm_bytes")
      else if
        payload.resource_mode = Circles.Sealed_read
        && payload.browser_mode <> Circles.Native_sealed
      then
        Error ("circle_mode_invalid", "sealed_read circles require native_sealed browser mode")
      else
        let circle_id = circle_id_of_source source payload in
        let owner = owner_of_source source in
        let info = {
          Circles.circle_id;
          runtime = payload.runtime;
          version = 1L;
          owner;
          code_hash =
            if String.length code_raw = 0 then Circles.zero_hash_hex
            else Circles.sha256_hex code_raw;
          stable_root = Circles.zero_hash_hex;
          assets_root = Circles.zero_hash_hex;
          privacy_class = payload.privacy_class;
          browser_mode = payload.browser_mode;
          resource_mode = payload.resource_mode;
          policy_hash = payload.policy_hash;
          members_root = payload.members_root;
          export_policy = payload.export_policy;
          limits = payload.limits;
        } in
        Ok { circle_id; owner; code_raw; info }
  with e ->
    Error ("malformed_transaction", Printexc.to_string e)

let validate_runtime (payload : Circles.deploy_payload) =
  match payload.Circles.runtime with
  | Circles.Octb ->
    Lwt.return (Ok ())
  | Circles.Wasm_v1 ->
    begin
      match payload.code_b64 with
      | None ->
        Lwt.return
          (Error
             ("circle_program_missing", "wasm_v1 circles require program code"))
      | Some code_b64 ->
        let* validate_result =
          Circle_wasm_host.describe code_b64 in
        begin
          match validate_result with
          | Ok descriptor ->
            begin
              match
                Circle_wasm_public_read.declarations_of_manifest
                  descriptor.Circle_wasm_host.manifest
              with
              | Ok _ -> Lwt.return (Ok ())
              | Error e -> Lwt.return (Error ("circle_runtime_invalid", e))
            end
          | Error e -> Lwt.return (Error ("circle_runtime_invalid", e))
        end
    end

let check_available store source (payload : Circles.deploy_payload) =
  match prepare source payload with
  | Error e ->
    Lwt.return (Error e)
  | Ok prepared ->
    let* exists = Store_irmin.circle_exists store prepared.circle_id in
    if exists then
      Lwt.return (Error ("circle_exists", "circle already exists"))
    else
      let* runtime_ok = validate_runtime payload in
      begin
        match runtime_ok with
        | Error e -> Lwt.return (Error e)
        | Ok () -> Lwt.return (Ok prepared)
      end

let save_origin store source circle_id =
  match source with
  | Direct _ ->
    Lwt.return_unit
  | Spawn { parent; caller; tx_hash; spawn_nonce; owner_mode; _ } ->
    let root = ["circles"; circle_id; "origin"] in
    let* () = Store_irmin.write store (root @ ["kind"]) "spawn" in
    let* () = Store_irmin.write store (root @ ["parent"]) parent in
    let* () = Store_irmin.write store (root @ ["caller"]) caller in
    let* () = Store_irmin.write store (root @ ["tx_hash"]) tx_hash in
    let* () = Store_irmin.write store (root @ ["spawn_nonce"]) (string_of_int spawn_nonce) in
    Store_irmin.write store (root @ ["owner_mode"]) (Circles.string_of_spawn_owner owner_mode)

let write_prepared store source prepared (payload : Circles.deploy_payload) =
  let* () = Store_irmin.deploy_circle store prepared.info in
  let* () =
    match payload.Circles.code_b64 with
    | Some code_b64 -> Store_irmin.save_circle_program_code_b64 store prepared.circle_id code_b64
    | None -> Lwt.return_unit
  in
  let* () = Store_irmin.set_circle_asset_usage_bytes store prepared.circle_id 0L in
  let* () = save_origin store source prepared.circle_id in
  Lwt.return (Ok prepared.circle_id)

let apply store source (payload : Circles.deploy_payload) =
  let* checked = check_available store source payload in
  match checked with
  | Error e ->
    Lwt.return (Error e)
  | Ok prepared ->
    write_prepared store source prepared payload