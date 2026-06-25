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


type client_id = string

type client = {
  id : client_id;
  send : Yojson.Safe.t -> unit Lwt.t;
  subscriptions : string list ref;
  connected_at : float;
}

let clients : (client_id, client) Hashtbl.t = Hashtbl.create 100
let next_id = ref 0

let generate_id () =
  incr next_id;
  Printf.sprintf "ws_client_%d" !next_id

let add_client send subscriptions =
  let id = generate_id () in
  let client = { id; send; subscriptions = ref subscriptions;
                 connected_at = Unix.gettimeofday () } in
  Hashtbl.add clients id client;
  Octra_log.stdout "info [ws] connected client = %s\n%!" id;
  id

let remove_client id =
  Hashtbl.remove clients id;
  Octra_log.stdout "info [ws] disconnected client = %s\n%!" id

let broadcast event_type data =
  let message = `Assoc [
    "type", `String event_type;
    "data", data;
    "timestamp", `Float (Unix.gettimeofday ())
  ] in
  Hashtbl.iter (fun _ client ->
    if List.mem event_type !(client.subscriptions) || List.mem "*" !(client.subscriptions) then
      Lwt.async (fun () ->
        Lwt.catch (fun () -> client.send message) (fun _ -> Lwt.return_unit))
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
    "subscriptions", `List (
      Hashtbl.fold (fun _ client acc ->
        `Assoc [
          "client_id", `String client.id;
          "subscribed_to", `List (List.map (fun s -> `String s) !(client.subscriptions));
          "connected_at", `Float client.connected_at;
        ] :: acc
      ) clients []);
  ]