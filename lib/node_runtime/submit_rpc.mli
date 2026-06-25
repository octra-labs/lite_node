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

type validate =
  Octra_core.Transaction.t ->
  (string, string * string) result

val submit :
  validate:validate ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val submit_batch :
  validate:validate ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val staging_remove :
  find_tx:(string -> Octra_core.Transaction.t option) ->
  remove_tx:(string -> bool) ->
  notify:(unit -> unit) ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val private_transfer :
  validate:validate ->
  Yojson.Safe.t ->
  rpc_result Lwt.t