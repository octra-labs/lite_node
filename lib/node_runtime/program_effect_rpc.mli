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


type rpc_result = (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result

val save_abi :
  save_abi:(string -> string -> unit Lwt.t) ->
  Yojson.Safe.t ->
  rpc_result Lwt.t