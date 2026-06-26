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

type rpc_result = (Yojson.Safe.t, Rpc.rpc_error) result

let save_abi ~save_abi params =
  match Rpc.require_address params 0 "address" with
  | Error e ->
    Lwt.return (Error e)
  | Ok addr ->
    match Rpc.require_string params 1 "abi" with
    | Error e ->
      Lwt.return (Error e)
    | Ok abi ->
      let open Lwt.Syntax in
      let* () = save_abi addr abi in
      Lwt.return (Ok Rpc_view.program_abi_saved)