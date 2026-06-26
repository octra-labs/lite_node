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


val handle :
  data_dir:string ->
  ledger:Octra_core.Ledger.t ->
  tree_ref:Octra_core.Tree.t ref ->
  validator:string ->
  current_epoch:int ref ->
  chaindata:Octra_core.Store_chaindata.t ->
  encrypted_supply:(unit -> Z.t) ->
  Cohttp.Request.t ->
  Cohttp_lwt.Body.t ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t