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

let err_lwt error =
  Lwt.return (Error error)

let with_address params f =
  match Rpc.require_address params 0 "address" with
  | Error e -> err_lwt e
  | Ok addr -> f addr

let with_address_string params idx field f =
  match Rpc.require_address params 0 "address", Rpc.require_string params idx field with
  | Error e, _ | _, Error e -> err_lwt e
  | Ok addr, Ok value -> f addr value

let with_account ~find_account params ctx f =
  with_address params (fun addr ->
    match find_account ctx addr with
    | Some account -> f addr account
    | None -> err_lwt Rpc.sender_not_found)

let store_read ~store f params ctx =
  f (store ctx) params

let ledger_params_read ~ledger f params ctx =
  f (ledger ctx) ~params

let ledger_address_lwt_read ~ledger ~with_address f params ctx =
  with_address params (fun addr ->
    f (ledger ctx) ~addr)

let ledger_params_no_ctx_read f params _ctx =
  f ~params

let store_address_lwt_read ~store ~with_address f params ctx =
  with_address params (fun addr ->
    f (store ctx) ~addr)

let store_params_lwt_read ~store f params ctx =
  f (store ctx) ~params

let store_ledger_address_lwt_read ~store ~ledger ~with_address f params ctx =
  with_address params (fun addr ->
    f (store ctx) (ledger ctx) ~params ~addr)

let chaindata_params_read ~chaindata f params ctx =
  f (chaindata ctx) ~params

let chaindata_address_read ~chaindata ~with_address f params ctx =
  with_address params (fun addr ->
    f (chaindata ctx) ~params ~addr)

let ledger_chaindata_params_read ~ledger ~chaindata f params ctx =
  f (ledger ctx) (chaindata ctx) ~params

let account_lwt_read ~with_account f params ctx =
  with_account params ctx (fun addr account ->
    f ~addr ~account)

let account_chaindata_lwt_read ~chaindata ~with_account f params ctx =
  with_account params ctx (fun addr account ->
    f (chaindata ctx) ~addr ~account)

let store_label_read ~store f params ctx =
  f ~store:(store ctx) params

let chaindata_read ~chaindata f params ctx =
  f ~chaindata:(chaindata ctx) params

let epoch_read ~store ~current_epoch f params ctx =
  f (store ctx) params ~current_epoch:!(current_epoch ctx)

let no_ctx f params _ctx =
  f params

let no_params f _params _ctx =
  f ()

let json0_read ~param_json f params _ctx =
  f ~json:(param_json params 0)