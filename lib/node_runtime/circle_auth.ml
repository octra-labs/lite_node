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


type t = {
  addr : string;
  pub_b64 : string;
  sig_b64 : string;
}

type gate =
  | Any
  | Owner
  | Private_owner
  | Owner_private
  | Storage_owner
  | Storage_owner_if of bool

module Rpc = Octra_core.Rpc

let owner_required =
  "circle owner authorization required"

let storage_owner_required =
  "circle owner authorization required for storage snapshot"

let message ~op ~circle_id ~addr ~subject =
  if String.length subject = 0 then
    op ^ "|" ^ circle_id ^ "|" ^ addr
  else
    op ^ "|" ^ circle_id ^ "|" ^ addr ^ "|" ^ subject

let params_hash params =
  Yojson.Safe.to_string params
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex

let view_subject ~method_name ~call_params ~include_storage =
  method_name
  ^ "|"
  ^ params_hash (`List call_params)
  ^ "|"
  ^ (if include_storage then "1" else "0")

let asset_subject ~kind ~subject =
  kind ^ "|" ^ subject

let private_view_required (info : Octra_core.Circles.circle_info) =
  info.privacy_class <> Octra_core.Circles.Public

let owner_private_required (info : Octra_core.Circles.circle_info) =
  match info.privacy_class with
  | Octra_core.Circles.Scoped
  | Octra_core.Circles.Circle -> true
  | Octra_core.Circles.Public
  | Octra_core.Circles.Sealed -> false

let public_info_allowed (info : Octra_core.Circles.circle_info) =
  match info.privacy_class with
  | Octra_core.Circles.Public
  | Octra_core.Circles.Sealed -> true
  | Octra_core.Circles.Scoped
  | Octra_core.Circles.Circle -> false

let owner info addr =
  if info.Octra_core.Circles.owner = addr then
    Ok ()
  else
    Error owner_required

let private_owner info addr =
  if private_view_required info && info.Octra_core.Circles.owner <> addr then
    Error owner_required
  else
    Ok ()

let owner_private info addr =
  if owner_private_required info && info.Octra_core.Circles.owner <> addr then
    Error owner_required
  else
    Ok ()

let storage_owner info addr =
  if info.Octra_core.Circles.owner = addr then
    Ok ()
  else
    Error storage_owner_required

let check_gate info addr = function
  | Any ->
    Ok ()
  | Owner ->
    owner info addr
  | Private_owner ->
    private_owner info addr
  | Owner_private ->
    owner_private info addr
  | Storage_owner ->
    storage_owner info addr
  | Storage_owner_if enabled ->
    if enabled then storage_owner info addr else Ok ()

let verify auth msg =
  if not (Octra_core.Crypto.Address.verify_address_pubkey auth.addr auth.pub_b64) then
    Error "public key does not match address"
  else
    match Base64.decode auth.pub_b64 with
    | Error _ ->
      Error "invalid public key"
    | Ok pub_raw ->
      begin
        match Mirage_crypto_ec.Ed25519.pub_of_octets pub_raw with
        | Error _ ->
          Error "invalid public key"
        | Ok pk_ed ->
          begin
            match Base64.decode auth.sig_b64 with
            | Error _ ->
              Error "invalid signature"
            | Ok sig_raw ->
              if Mirage_crypto_ec.Ed25519.verify ~key:pk_ed ~msg sig_raw then
                Ok auth.addr
              else
                Error "signature verification failed"
          end
      end

let authenticate ~op ~circle_id ~subject auth =
  let msg = message ~op ~circle_id ~addr:auth.addr ~subject in
  verify auth msg

let authorize ?(gate=Any) ~op ~circle_id ~subject info auth =
  match authenticate ~op ~circle_id ~subject auth with
  | Error e ->
    Error e
  | Ok addr ->
    match check_gate info addr gate with
    | Error e -> Error e
    | Ok () -> Ok addr

let rpc_error e =
  Rpc.err (-32000) e None

let parse_rpc_auth params addr_idx pub_idx sig_idx =
  match Rpc.require_address params addr_idx "address",
        Rpc.require_string params pub_idx "public_key",
        Rpc.require_string params sig_idx "signature" with
  | Ok addr, Ok pub_b64, Ok sig_b64 ->
    Ok { addr; pub_b64; sig_b64 }
  | Error e, _, _ | _, Error e, _ | _, _, Error e ->
    Error e

let authorize_rpc info ~gate ~op ~circle_id ~subject auth =
  match authorize ~gate ~op ~circle_id ~subject info auth with
  | Ok addr -> Ok addr
  | Error e -> Error (rpc_error e)

let authenticate_rpc ~op ~circle_id ~subject auth =
  match authenticate ~op ~circle_id ~subject auth with
  | Ok addr -> Ok addr
  | Error e -> Error (rpc_error e)

let authenticate_rpc_params params addr_idx pub_idx sig_idx ~op ~circle_id ~subject =
  match parse_rpc_auth params addr_idx pub_idx sig_idx with
  | Error e -> Error e
  | Ok auth ->
    authenticate_rpc ~op ~circle_id ~subject auth

let auth_required0 params message =
  match Rpc.require_string params 0 "circle_id" with
  | Error e -> e
  | Ok _circle_id -> rpc_error message

let auth_required1 params field message =
  match Rpc.require_string params 0 "circle_id", Rpc.require_string params 1 field with
  | Error e, _ | _, Error e -> e
  | Ok _circle_id, Ok _value -> rpc_error message

let auth_required2 params field1 field2 message =
  match Rpc.require_string params 0 "circle_id",
        Rpc.require_string params 1 field1,
        Rpc.require_string params 2 field2 with
  | Error e, _, _ | _, Error e, _ | _, _, Error e -> e
  | Ok _circle_id, Ok _value1, Ok _value2 -> rpc_error message