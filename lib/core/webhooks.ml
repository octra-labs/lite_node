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


open Lwt.Infix

type event =
  | TxConfirmed of Transaction.t * int
  | TxRejected of Transaction.t * string
  | EpochFinalized of int * int
  | LargeTransfer of Transaction.t
  | NewAccount of string

type config = {
  url : string;
  events : string list;
  secret : string option;
  active : bool;
  mutable failures : int;
}

let webhooks : (string, config) Hashtbl.t = Hashtbl.create 10

let register_webhook id config =
  Hashtbl.replace webhooks id config;
  Octra_log.stdout "info [webhook] registered id = %s url = %s\n%!" id config.url

let unregister_webhook id =
  Hashtbl.remove webhooks id;
  Octra_log.stdout "info [webhook] unregistered id = %s\n%!" id

let create_signature body secret =
  Digestif.SHA256.digest_string (secret ^ body) |> Digestif.SHA256.to_hex

let event_payload = function
  | TxConfirmed (tx, epoch) ->
    "tx_confirmed", `Assoc ["tx", Transaction.to_yojson tx; "epoch", `Int epoch]
  | TxRejected (tx, reason) ->
    "tx_rejected", `Assoc ["tx", Transaction.to_yojson tx; "reason", `String reason]
  | EpochFinalized (epoch_id, tx_count) ->
    "epoch_finalized", `Assoc ["epoch_id", `Int epoch_id; "tx_count", `Int tx_count]
  | LargeTransfer tx ->
    "large_transfer", Transaction.to_yojson tx
  | NewAccount addr ->
    "new_account", `Assoc ["address", `String addr]

let deliver id config event_type body =
  let headers =
    ("Content-Type", "application/json") ::
    ("X-OCTRA-Event", event_type) ::
    (match config.secret with
     | Some s -> [("X-OCTRA-Signature", create_signature body s)]
     | None -> [])
    |> Cohttp.Header.of_list
  in
  Cohttp_lwt_unix.Client.post
    ~body:(Cohttp_lwt.Body.of_string body)
    ~headers (Uri.of_string config.url) >>= fun (resp, _) ->
  let code = Cohttp.Code.code_of_status (Cohttp.Response.status resp) in
  if code >= 200 && code < 300 then (
    config.failures <- 0;
    Octra_log.stdout "info [webhook] delivered id = %s event = %s\n%!" id event_type
  ) else (
    config.failures <- config.failures + 1;
    Octra_log.stdout "warn [webhook] delivery failed id = %s event = %s http = %d\n%!" id event_type code
  );
  Lwt.return_unit

let notify event =
  let event_type, data = event_payload event in
  Hashtbl.iter (fun id config ->
    if config.active && (List.mem event_type config.events || List.mem "*" config.events) then
      Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
            let body = Yojson.Safe.to_string (`Assoc [
              "event", `String event_type;
              "data", data;
              "timestamp", `Float (Unix.gettimeofday ())
            ]) in
            deliver id config event_type body)
          (fun exn ->
            config.failures <- config.failures + 1;
            Octra_log.stdout "error [webhook] exception id = %s event = %s err = %s\n%!"
              id event_type (Printexc.to_string exn);
            Lwt.return_unit))
  ) webhooks