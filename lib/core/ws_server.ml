(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type client_id = string

type client = {
  send : Yojson.Safe.t -> unit Lwt.t;
  subscriptions : string list;
  mutable sending : bool;
}

let clients : (client_id, client) Hashtbl.t = Hashtbl.create 100
let next_id = ref 0
let max_clients = 256
let max_subscriptions = 8
let event_types = ["new_tx"; "new_epoch"; "staging_update"; "*"]

let generate_id () =
  incr next_id;
  Printf.sprintf "ws_client_%d" !next_id

let add_client send subscriptions =
  let subscriptions = List.sort_uniq String.compare subscriptions in
  if Hashtbl.length clients >= max_clients then
    Error "websocket client cap reached"
  else if List.length subscriptions > max_subscriptions then
    Error "websocket subscription cap reached"
  else if not (List.for_all (fun event -> List.mem event event_types) subscriptions) then
    Error "unsupported websocket subscription"
  else
    let id = generate_id () in
    Hashtbl.add clients id {
      send;
      subscriptions;
      sending = false;
    };
    Ok id

let remove_client id =
  Hashtbl.remove clients id

let broadcast event_type data =
  let message = `Assoc [
    "type", `String event_type;
    "data", data;
    "timestamp", `Float (Unix.gettimeofday ())
  ] in
  Hashtbl.iter (fun _ client ->
    if
      not client.sending
      && (List.mem event_type client.subscriptions
          || List.mem "*" client.subscriptions)
    then begin
      client.sending <- true;
      Lwt.async (fun () ->
        Lwt.finalize
          (fun () ->
            Lwt.catch
              (fun () -> client.send message)
              (fun _ -> Lwt.return_unit))
          (fun () ->
            client.sending <- false;
            Lwt.return_unit))
    end
  ) clients

let notify_new_tx (tx : Transaction.t) =
  broadcast "new_tx" (`Assoc [
    "hash", `String (Transaction.hash tx);
    "from", `String tx.from;
    "to", `String tx.to_;
    "amount", `String (Denomination.format_balance tx.amount);
    "nonce", `Int tx.nonce;
    "ou", `String (Z.to_string (Transaction.ou_cost tx));
  ])

let notify_new_epoch epoch_id tx_count =
  broadcast "new_epoch" (`Assoc [
    "epoch_id", `Int epoch_id;
    "tx_count", `Int tx_count;
    "timestamp", `Float (Unix.gettimeofday ());
  ])

let notify_staging_update ~total_txs ~total_ou ~max_ou =
  broadcast "staging_update" (`Assoc [
    "total_txs", `Int total_txs;
    "total_ou", `String (Z.to_string total_ou);
    "ou_remaining", `String (Z.to_string (Z.sub max_ou total_ou));
  ])

let get_client_stats () =
  `Assoc [
    "connected_clients", `Int (Hashtbl.length clients);
    "max_clients", `Int max_clients;
  ]