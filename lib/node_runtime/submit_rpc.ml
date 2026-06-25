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
module Transaction = Octra_core.Transaction

type rpc_result = (Yojson.Safe.t, Rpc.rpc_error) result

type validate =
  Transaction.t ->
  (string, string * string) result

let ok_lwt v =
  Lwt.return (Ok v)

let err_lwt e =
  Lwt.return (Error e)

let submit ~validate params =
  match Tx_view.submit_params params with
  | Error e ->
    err_lwt e
  | Ok tx ->
    match validate tx with
    | Error (etype, reason) ->
      err_lwt (Tx_view.submit_rpc_error etype reason)
    | Ok tx_hash ->
      ok_lwt (Tx_view.submit_accepted ~tx_hash tx)

let submit_batch ~validate params =
  match Tx_view.submit_batch_params params with
  | Error e ->
    err_lwt e
  | Ok txs_json ->
    let results =
      List.map
        (fun tx_json ->
          match Tx_view.decode_submit_batch_tx tx_json with
          | Error e ->
            Tx_view.submit_batch_decode_error e
          | Ok tx ->
            Tx_view.submit_batch_row tx (validate tx))
        txs_json
    in
    ok_lwt (Rpc_view.submit_batch results)

let staging_remove ~find_tx ~remove_tx ~notify params =
  match Tx_view.staging_remove_params params with
  | Error e ->
    err_lwt e
  | Ok req ->
    let tx_hash = req.Tx_view.staging_remove_hash in
    match find_tx tx_hash with
    | None ->
      err_lwt (Rpc.not_found "tx not in staging")
    | Some tx ->
      match Tx_view.staging_remove_request_auth ~sender:tx.Transaction.from req with
      | Error reason ->
        err_lwt (Rpc.err (-32000) reason None)
      | Ok () ->
        if remove_tx tx_hash then begin
          notify ();
          ok_lwt (Rest_view.staging_removed_response ~tx_hash)
        end else
          err_lwt (Rpc.not_found "tx not in staging")

let private_transfer_tx_json = function
  | `List ((`Assoc _ as tx_json) :: _) ->
    Some tx_json
  | `List [`String raw] ->
    Some (try Yojson.Safe.from_string raw with _ -> `Null)
  | _ ->
    None

let private_transfer ~validate params =
  match private_transfer_tx_json params with
  | None ->
    err_lwt (Rpc.malformed_tx "expected [tx_json]")
  | Some tx_json ->
    match Transaction.of_yojson tx_json with
    | Error e ->
      err_lwt (Rpc.malformed_tx e)
    | Ok tx ->
      match validate tx with
      | Ok tx_hash ->
        ok_lwt (Rpc_view.private_transfer_pending ~tx_hash)
      | Error (etype, reason) ->
        err_lwt (Rpc.err (-32000) (etype ^ ": " ^ reason) None)