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


module Rpc = Octra_core.Rpc
module Pvac_registry = Octra_core.Pvac_registry

type rpc_result = (Yojson.Safe.t, Rpc.rpc_error) result

let ok_lwt v =
  Lwt.return (Ok v)

let err_lwt e =
  Lwt.return (Error e)

let public_key ~bft_mode ~register params =
  if bft_mode then
    ok_lwt (Rpc_view.public_key_registration_ignored_bft
      ~addr:(match Rpc.require_address params 0 "address" with Ok a -> a | Error _ -> ""))
  else
    match Rpc.require_address params 0 "address" with
    | Error e ->
      err_lwt e
    | Ok addr ->
      match Rpc.require_string params 1 "public_key" with
      | Error e ->
        err_lwt e
      | Ok pubkey_b64 ->
        match Rpc.require_string params 2 "signature" with
        | Error e ->
          err_lwt e
        | Ok signature_b64 ->
          match Tx_view.public_key_registration_auth
                  ~addr
                  ~pubkey_b64
                  ~signature_b64 with
          | Error reason ->
            err_lwt (Rpc.err (-32000) reason None)
          | Ok () ->
            register addr pubkey_b64;
            ok_lwt (Rpc_view.public_key_registered ~addr)

let pvac_pubkey ~existing ~kat_mismatch params =
  match Rpc.require_address params 0 "address" with
  | Error e ->
    err_lwt e
  | Ok addr ->
    match Rpc.require_string params 1 "pubkey_blob" with
    | Error e ->
      err_lwt e
    | Ok pk_b64 ->
      match Pvac_registry.register_blob_of_b64 pk_b64 with
      | Error e ->
        err_lwt (Rpc.malformed_tx e)
      | Ok incoming ->
        match Pvac_registry.kat_admission (Rpc.param_string params 4) with
        | Pvac_registry.Kat_mismatch { got; expected } ->
          kat_mismatch ~addr ~got ~expected;
          err_lwt (Rpc.err (-32000)
            "AES implementation incompatible: KAT mismatch" None)
        | Pvac_registry.Kat_ok ->
          let open Lwt.Syntax in
          let* current = existing addr in
          match Pvac_registry.register_decision ~existing:current ~incoming with
          | Pvac_registry.Already_registered ->
            ok_lwt (Rpc_view.pvac_already_registered ~addr)
          | Pvac_registry.Existing_key_requires_key_switch ->
            err_lwt (Rpc.err (-32000)
              "pvac pubkey already registered: use key_switch transaction to rotate" None)
          | Pvac_registry.Canonical_key_switch_required ->
            err_lwt (Rpc.err (-32000)
              "pvac pubkey registration must be submitted as a canonical key_switch transaction" None)