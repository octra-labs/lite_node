(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type config = {
  listen_port : int;
  chain_id : string;
  node_id : string;
  node_addr : string;
  pubkey_raw : string;
  consensus_config_hash : string;
  binary_hash : string;
  require_binary_hash : bool;
  upgrade_plan : P2p_upgrade_plan.t option;
  allowed_pubkeys : string list;
  bootstrap_peers : string list;
  max_peers : int;
  sign_fn : string -> string;
  best_epoch_fn : unit -> int64;
  best_root_fn : unit -> string;
}

type peer_role =
  | Validator
  | Observer

type dial_plan =
  | Hold
  | Start of float

type t = {
  config : config;
  peers : (string, P2p_conn.t) Hashtbl.t;
  dialing : (string, unit) Hashtbl.t;
  record_dialing : (string, unit) Hashtbl.t;
  record_due : (string, float) Hashtbl.t;
  registry : P2p_peer_registry.t;
  peer_guard : P2p_peer_guard.t;
  relayed_record_rejections : (string, int) Hashtbl.t;
  mutable relayed_record_accepted : int;
  mutable relayed_record_unchanged : int;
  mutable relayed_record_rejected : int;
  mutable validator_pubkeys : string list option;
  mutable on_message : (P2p_conn.t -> P2p_frame.frame -> unit Lwt.t);
  mutable running : bool;
}

let max_observer_peers = 8
let max_record_dials = 4
let record_retry_s = 30.0
let connect_wait_s = 10.0

let record_dial_plan ~now ~due ~active =
  if active >= max_record_dials then Hold
  else
    match due with
    | Some at when now < at -> Hold
    | _ -> Start (now +. record_retry_s)

let create config =
  {
    config;
    peers = Hashtbl.create 16;
    dialing = Hashtbl.create 16;
    record_dialing = Hashtbl.create 8;
    record_due = Hashtbl.create 32;
    registry = P2p_peer_registry.create ();
    peer_guard = P2p_peer_guard.create ();
    relayed_record_rejections = Hashtbl.create 8;
    relayed_record_accepted = 0;
    relayed_record_unchanged = 0;
    relayed_record_rejected = 0;
    validator_pubkeys =
      if config.allowed_pubkeys = [] then None
      else Some config.allowed_pubkeys;
    on_message = (fun _ _ -> Lwt.return_unit);
    running = false;
  }

let set_handler t handler =
  t.on_message <- handler

let peer_count t = Hashtbl.length t.peers

let connected_count t =
  Hashtbl.fold
    (fun _ conn n -> if P2p_conn.is_connected conn then n + 1 else n)
    t.peers
    0

let dialing_count t = Hashtbl.length t.dialing

let is_dialing t addr = Hashtbl.mem t.dialing addr

let peer_ids t =
  Hashtbl.fold (fun k _ acc -> k :: acc) t.peers []
  |> List.sort String.compare

let peer_scores t =
  P2p_peer_guard.snapshot t.peer_guard

let relayed_record_stats t =
  let rejections =
    Hashtbl.fold
      (fun reason count acc ->
        P2p_peer_diag.{ reason; count } :: acc)
      t.relayed_record_rejections
      []
    |> List.sort
         (fun
           (left : P2p_peer_diag.rejection)
           (right : P2p_peer_diag.rejection) ->
           String.compare left.reason right.reason)
  in
  P2p_peer_diag.{
    accepted = t.relayed_record_accepted;
    unchanged = t.relayed_record_unchanged;
    rejected = t.relayed_record_rejected;
    rejections;
  }

let validator_node_ids t =
  match t.validator_pubkeys with
  | None -> []
  | Some pubkeys ->
    List.map P2p_handshake.node_id_of_pubkey pubkeys

let peer_role t peer_id =
  match t.validator_pubkeys with
  | None -> Validator
  | Some _ ->
    if List.mem peer_id (validator_node_ids t) then Validator
    else Observer

let frame_peer_class = function
  | Validator -> P2p_frame_budget.Validator
  | Observer -> P2p_frame_budget.Observer

let local_is_validator t =
  match t.validator_pubkeys with
  | None -> true
  | Some pubkeys -> List.mem t.config.pubkey_raw pubkeys

let local_accepts_observers t =
  Option.is_some t.validator_pubkeys
  && local_is_validator t

let set_validator_pubkeys t pubkeys =
  if pubkeys = [] then
    Error "active validator public keys are empty"
  else if List.exists (fun pubkey -> String.length pubkey <> 32) pubkeys then
    Error "active validator public key must contain 32 bytes"
  else if List.length (List.sort_uniq String.compare pubkeys) <> List.length pubkeys then
    Error "active validator public keys contain duplicates"
  else if t.validator_pubkeys = Some pubkeys then
    Ok ()
  else begin
    t.validator_pubkeys <- Some pubkeys;
    Hashtbl.iter
      (fun peer_id conn ->
        P2p_conn.set_peer_class conn (frame_peer_class (peer_role t peer_id)))
      t.peers;
    P2p_peer_registry.clear t.registry;
    Hashtbl.clear t.record_due;
    Ok ()
  end

let role_count t role =
  Hashtbl.fold
    (fun peer_id conn count ->
      if P2p_conn.is_connected conn && peer_role t peer_id = role then
        count + 1
      else
        count)
    t.peers
    0

let validator_count t = role_count t Validator

let observer_count t = role_count t Observer

let peer_capacity ~max_peers ~validator_count ~observer_count = function
  | Validator -> validator_count < max_peers
  | Observer -> observer_count < max_observer_peers

let has_role_capacity t role =
  peer_capacity
    ~max_peers:t.config.max_peers
    ~validator_count:(validator_count t)
    ~observer_count:(observer_count t)
    role

let has_inbound_capacity t =
  has_role_capacity t Validator
  || (local_accepts_observers t && has_role_capacity t Observer)

let peer_records t =
  P2p_peer_registry.values ~now:(Unix.gettimeofday ()) t.registry

let find_peer t peer_id =
  Hashtbl.find_opt t.peers peer_id

let peer_key conn =
  P2p_peer_guard.identity_key conn.P2p_conn.peer_id

let identity_is_banned t peer_id =
  P2p_peer_guard.is_banned
    t.peer_guard
    ~now:(Unix.gettimeofday ())
    ~key:(P2p_peer_guard.identity_key peer_id)

let log_node addr fmt =
  Printf.ksprintf
    (fun msg -> Octra_log.info "p2p" "node = %s %s" addr msg)
    fmt

let err_node addr fmt =
  Printf.ksprintf
    (fun msg -> Octra_log.warn "p2p" "node = %s %s" addr msg)
    fmt

let report_bad_key t ~key ~reason ~on_ban =
  match P2p_peer_guard.report_bad t.peer_guard
    ~now:(Unix.gettimeofday ()) ~key ~reason with
  | P2p_peer_guard.Noted count ->
    err_node t.config.node_addr "event = bad_peer_noted key = %s count = %d reason = %s"
      key count reason
  | P2p_peer_guard.Banned until_ts ->
    err_node t.config.node_addr "event = peer_banned key = %s until = %.0f reason = %s"
      key until_ts reason;
    on_ban ()

let report_bad_identity t ~peer_id ~reason =
  let key = P2p_peer_guard.identity_key peer_id in
  report_bad_key
    t
    ~key
    ~reason
    ~on_ban:(fun () ->
      match find_peer t peer_id with
      | Some conn -> Lwt.async (fun () -> P2p_conn.close conn)
      | None -> ())

let report_bad_peer t conn ~reason =
  report_bad_key
    t
    ~key:(peer_key conn)
    ~reason
    ~on_ban:(fun () -> Lwt.async (fun () -> P2p_conn.close conn))

let is_peer_connected t peer_id =
  match Hashtbl.find_opt t.peers peer_id with
  | Some conn -> P2p_conn.is_connected conn
  | None -> false

let preferred_direction t peer_id =
  if String.compare t.config.node_id peer_id < 0 then
    P2p_conn.Outbound
  else
    P2p_conn.Inbound

let is_preferred t (conn : P2p_conn.t) =
  conn.direction = preferred_direction t conn.peer_id

let add_peer t (conn : P2p_conn.t) =
  let peer_id = conn.peer_id in
  if peer_id = t.config.node_id then begin
    Lwt.async (fun () -> P2p_conn.close conn);
    false
  end
  else match Hashtbl.find_opt t.peers peer_id with
    | Some existing when P2p_conn.is_connected existing ->
      if is_preferred t existing || not (is_preferred t conn) then begin
        Lwt.async (fun () -> P2p_conn.close conn);
        false
      end else begin
        Lwt.async (fun () -> P2p_conn.close existing);
        Hashtbl.replace t.peers peer_id conn;
        true
      end
    | Some _dead ->
      Hashtbl.replace t.peers peer_id conn;
      true
    | None ->
      let role = peer_role t peer_id in
      if role = Observer && not (local_accepts_observers t) then begin
        Lwt.async (fun () -> P2p_conn.close conn);
        false
      end else if not (has_role_capacity t role) then begin
        Lwt.async (fun () -> P2p_conn.close conn);
        false
      end else begin
        Hashtbl.replace t.peers peer_id conn;
        true
      end

let remove_peer t peer_id =
  (match Hashtbl.find_opt t.peers peer_id with
   | Some conn ->
     Lwt.async (fun () -> P2p_conn.close conn);
     Hashtbl.remove t.peers peer_id
   | None -> ())

let send_with_timeout conn frame timeout_s =
  Lwt.catch
    (fun () ->
      Lwt.pick [
        P2p_conn.send conn frame;
        (let open Lwt.Syntax in
         let* () = Lwt_unix.sleep timeout_s in
         Lwt.return_unit)
      ])
    (fun _exn -> Lwt.return_unit)

let connected_peers t =
  let conns = Hashtbl.fold (fun _ c acc -> c :: acc) t.peers [] in
  List.filter (fun conn ->
    if P2p_conn.is_connected conn then true
    else begin
      remove_peer t conn.P2p_conn.peer_id;
      false
    end
  ) conns

let peer_diagnostics t =
  P2p_peer_diag.make
    ~connected:(List.map (fun conn -> conn.P2p_conn.addr) (connected_peers t))
    ~records:(peer_records t)
    ~scores:(peer_scores t)
    ~relayed_records:(relayed_record_stats t)
    ()

let broadcast t (frame : P2p_frame.frame) =
  connected_peers t
  |> Lwt_list.iter_p (fun conn -> send_with_timeout conn frame 2.0)

let broadcast_except t ~except (frame : P2p_frame.frame) =
  connected_peers t
  |> List.filter (fun conn -> conn.P2p_conn.peer_id <> except)
  |> Lwt_list.iter_p (fun conn -> send_with_timeout conn frame 2.0)

let send_to t ~peer_id (frame : P2p_frame.frame) =
  match Hashtbl.find_opt t.peers peer_id with
  | Some conn when P2p_conn.is_connected conn ->
    send_with_timeout conn frame 2.0
  | _ -> Lwt.return_unit

let make_my_hello t =
  let best_epoch = t.config.best_epoch_fn () in
  let consensus_config_hash =
    P2p_upgrade_plan.handshake_hash
      ~epoch:best_epoch
      ~config_hash:t.config.consensus_config_hash
      ~binary_hash:t.config.binary_hash
      ~require_binary_hash:t.config.require_binary_hash
      t.config.upgrade_plan
  in
  P2p_handshake.make_hello
    ~chain_id:t.config.chain_id
    ~node_addr:t.config.node_addr
    ~pubkey_raw:t.config.pubkey_raw
    ~consensus_config_hash
    ~listen_port:t.config.listen_port
    ~best_epoch
    ~best_root:(t.config.best_root_fn ())
    ~sign_fn:t.config.sign_fn

let make_my_record t =
  let endpoint = P2p_endpoint.advertised ~default_port:t.config.listen_port in
  P2p_peer_record.make
    ~chain_id:t.config.chain_id
    ~node_addr:t.config.node_addr
    ~pubkey:t.config.pubkey_raw
    ~binary_hash:t.config.binary_hash
    ~config_hash:t.config.consensus_config_hash
    ~host:endpoint.host
    ~port:endpoint.port
    ~features:["bft"; "pex"]
    ~ts:(Int64.of_float (Unix.gettimeofday ()))
    ~sign_fn:t.config.sign_fn

let record_values t =
  let records = peer_records t in
  if local_is_validator t then make_my_record t :: records else records

let send_get_peers conn =
  P2p_conn.send conn { msg_type = P2p_frame.msg_get_peers; payload = "" }

let send_peers t conn =
  let records = record_values t |> List.filteri (fun i _ -> i < 32) in
  P2p_conn.send conn {
    msg_type = P2p_frame.msg_peers;
    payload = P2p_peer_record.encode_list records;
  }

let exchange_peers t conn =
  let open Lwt.Syntax in
  let* () = send_peers t conn in
  send_get_peers conn

let accept_record t r =
  let epoch = t.config.best_epoch_fn () in
  let binary_hash =
    if P2p_upgrade_plan.binary_required
      ~epoch
      ~require_binary_hash:t.config.require_binary_hash
      t.config.upgrade_plan
    then
      Some (P2p_upgrade_plan.expected_binary_hash
        ~epoch
        ~binary_hash:t.config.binary_hash
        t.config.upgrade_plan)
    else None
  in
  match P2p_peer_record.validate
    ?binary_hash
    ~chain_id:t.config.chain_id
    ~config_hash:t.config.consensus_config_hash
    ~allowed_pubkeys:t.config.allowed_pubkeys
    r
  with
  | P2p_peer_record.Invalid reason -> Error reason
  | P2p_peer_record.Valid ->
    if r.node_id = t.config.node_id then Ok false
    else
      match P2p_peer_registry.add ~now:(Unix.gettimeofday ()) t.registry r with
      | P2p_peer_registry.Added -> Ok true
      | P2p_peer_registry.Refreshed -> Ok false
      | P2p_peer_registry.Rejected reason -> Error reason

let dial t host port =
  let open Lwt.Syntax in
  if validator_count t >= t.config.max_peers then
    Lwt.return_none
  else begin
  log_node t.config.node_addr "event = dial host = %s port = %d" host port;
  let fd_ref = ref None in
  Lwt.catch
    (fun () ->
      let* addrs = Lwt_unix.getaddrinfo host (string_of_int port)
        [Unix.AI_FAMILY Unix.PF_INET; Unix.AI_SOCKTYPE Unix.SOCK_STREAM] in
      match addrs with
      | [] ->
        Lwt.return_none
      | ai :: _ ->
        let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
        fd_ref := Some fd;
        let* () =
          Lwt_unix.with_timeout connect_wait_s (fun () ->
            Lwt_unix.connect fd ai.Unix.ai_addr)
        in
        let my_hello = make_my_hello t in
        let* hs_result = P2p_handshake.dial_handshake fd ~my_hello
          ~allowed_pubkeys:t.config.allowed_pubkeys ~sign_fn:t.config.sign_fn in
        match hs_result with
        | P2p_handshake.Error reason ->
          err_node t.config.node_addr
            "event = dial_handshake_failed host = %s port = %d reason = %s"
            host port reason;
          (try let* () = Lwt_unix.close fd in Lwt.return_none with _ -> Lwt.return_none)
        | P2p_handshake.Ok peer_hello ->
          let peer_id = peer_hello.node_id in
          let addr_str = Printf.sprintf "%s:%d" host port in
          let conn = P2p_conn.create
            ~peer_class:(frame_peer_class (peer_role t peer_id))
            fd ~peer_id ~addr:addr_str
            ~direction:P2p_conn.Outbound in
          if identity_is_banned t peer_id then begin
            err_node t.config.node_addr
              "event = outbound_peer_rejected peer = %s reason = identity_banned"
              peer_id;
            let* () = P2p_conn.close conn in
            Lwt.return_none
          end else if add_peer t conn then begin
            log_node t.config.node_addr
              "event = connected peer = %s addr = %s"
              peer_id addr_str;
            Lwt.async (fun () -> P2p_conn.start conn ~on_message:t.on_message);
            Lwt.async (fun () -> exchange_peers t conn);
            Lwt.return_some conn
          end else begin
            let existing = Hashtbl.find_opt t.peers peer_id in
            let* () = P2p_conn.close conn in
            Lwt.return existing
          end)
    (fun exn ->
      (match !fd_ref with
       | Some fd -> (try Lwt_unix.close fd |> ignore with _ -> ())
       | None -> ());
      err_node t.config.node_addr
        "event = dial_exception host = %s port = %d error = %s"
        host port (Printexc.to_string exn);
      Lwt.return_none)
  end

let maybe_dial_record_at t ~now r =
  let endpoint = P2p_peer_record.endpoint r in
  if not t.running
     || r.P2p_peer_record.node_id = t.config.node_id
     || is_peer_connected t r.node_id
     || Hashtbl.mem t.dialing endpoint
     || validator_count t >= t.config.max_peers then
    ()
  else
    match P2p_endpoint.of_string endpoint with
    | None -> ()
    | Some { P2p_endpoint.host; port } ->
      match
        record_dial_plan
          ~now
          ~due:(Hashtbl.find_opt t.record_due endpoint)
          ~active:(Hashtbl.length t.record_dialing)
      with
      | Hold -> ()
      | Start due ->
        Hashtbl.replace t.record_due endpoint due;
        Hashtbl.replace t.dialing endpoint ();
        Hashtbl.replace t.record_dialing endpoint ();
        Lwt.async (fun () ->
          Lwt.finalize
            (fun () ->
              let open Lwt.Syntax in
              let* _ = dial t host port in
              Lwt.return_unit)
            (fun () ->
              Hashtbl.remove t.dialing endpoint;
              Hashtbl.remove t.record_dialing endpoint;
              Lwt.return_unit))

let maybe_dial_record t r =
  maybe_dial_record_at t ~now:(Unix.gettimeofday ()) r

let retry_records t =
  let now = Unix.gettimeofday () in
  let records = P2p_peer_registry.values ~now t.registry in
  let live = Hashtbl.create (List.length records) in
  List.iter
    (fun r -> Hashtbl.replace live (P2p_peer_record.endpoint r) ())
    records;
  Hashtbl.filter_map_inplace
    (fun endpoint due ->
      if Hashtbl.mem live endpoint then Some due else None)
    t.record_due;
  List.iter (maybe_dial_record_at t ~now) records

let accept_relayed_records t records =
  records
  |> List.iter (fun r ->
    match accept_record t r with
    | Ok true ->
      t.relayed_record_accepted <- t.relayed_record_accepted + 1;
      maybe_dial_record t r
    | Ok false ->
      t.relayed_record_unchanged <- t.relayed_record_unchanged + 1;
      maybe_dial_record t r
    | Error reason ->
      t.relayed_record_rejected <- t.relayed_record_rejected + 1;
      let count =
        Option.value
          ~default:0
          (Hashtbl.find_opt t.relayed_record_rejections reason)
        + 1
      in
      Hashtbl.replace t.relayed_record_rejections reason count;
      if count land (count - 1) = 0 then
        err_node t.config.node_addr
          "event = relayed_peer_record_rejected reason = %s count = %d node_id = %s"
          reason
          count
          r.P2p_peer_record.node_id)

let handle_peers t conn payload =
  Lwt.catch
    (fun () ->
      let records = P2p_peer_record.decode_list payload in
      accept_relayed_records t records;
      Lwt.return_unit)
    (fun _ ->
      report_bad_peer t conn ~reason:"invalid_frame_peers";
      Lwt.return_unit)

let handle_frame t user_handler conn frame =
  if frame.P2p_frame.msg_type = P2p_frame.msg_get_peers then
    send_peers t conn
  else if frame.msg_type = P2p_frame.msg_peers then
    handle_peers t conn frame.payload
  else
    user_handler conn frame

let bootstrap_endpoints t =
  P2p_endpoint.bootstrap t.config.bootstrap_peers

let accept t fd addr =
  let open Lwt.Syntax in
  Lwt.async (fun () ->
    Lwt.catch
      (fun () ->
        let key = P2p_peer_guard.unix_key addr in
        match P2p_peer_guard.admit_conn t.peer_guard
          ~now:(Unix.gettimeofday ()) ~key with
        | P2p_peer_guard.Drop reason ->
          err_node t.config.node_addr
            "event = inbound_peer_rejected key = %s reason = %s"
            key reason;
          (try Lwt_unix.close fd |> ignore; Lwt.return_unit with _ -> Lwt.return_unit)
        | P2p_peer_guard.Accept ->
        if not (has_inbound_capacity t) then begin
          err_node t.config.node_addr
            "event = inbound_peer_rejected key = %s reason = peer_capacity"
            key;
          (try Lwt_unix.close fd |> ignore; Lwt.return_unit with _ -> Lwt.return_unit)
        end else
        let my_hello = make_my_hello t in
        let allowed_pubkeys =
          if local_accepts_observers t then [] else t.config.allowed_pubkeys
        in
        let* hs_result = P2p_handshake.accept_handshake fd ~my_hello
          ~allowed_pubkeys ~sign_fn:t.config.sign_fn in
        match hs_result with
        | P2p_handshake.Error reason ->
          (match P2p_peer_guard.handshake_penalty reason with
           | None -> ()
           | Some penalty ->
             ignore (P2p_peer_guard.report_bad t.peer_guard
               ~now:(Unix.gettimeofday ()) ~key ~reason:penalty));
          (try Lwt_unix.close fd |> ignore; Lwt.return_unit with _ -> Lwt.return_unit)
        | P2p_handshake.Ok peer_hello ->
          let peer_id = peer_hello.node_id in
          let addr_str = match addr with
            | Unix.ADDR_INET (ip, port) ->
              Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port
            | Unix.ADDR_UNIX s -> s
          in
          let conn = P2p_conn.create
            ~peer_class:(frame_peer_class (peer_role t peer_id))
            fd ~peer_id ~addr:addr_str
            ~direction:P2p_conn.Inbound in
          if identity_is_banned t peer_id then begin
            err_node t.config.node_addr
              "event = inbound_peer_rejected peer = %s reason = identity_banned"
              peer_id;
            let* () = P2p_conn.close conn in
            Lwt.return_unit
          end else if add_peer t conn then begin
            Lwt.async (fun () -> P2p_conn.start conn ~on_message:t.on_message);
            Lwt.async (fun () -> exchange_peers t conn);
            Lwt.return_unit
          end else begin
            let* () = P2p_conn.close conn in
            Lwt.return_unit
          end)
      (fun _exn ->
        (try Lwt_unix.close fd |> ignore with _ -> ());
        Lwt.return_unit))

let listen t =
  let open Lwt.Syntax in
  let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt fd Lwt_unix.SO_REUSEADDR true;
  let* () = Lwt_unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_any, t.config.listen_port)) in
  Lwt_unix.listen fd 32;
  let rec accept_loop () =
    if not t.running then Lwt.return_unit
    else
      let* (client_fd, client_addr) = Lwt_unix.accept fd in
      accept t client_fd client_addr;
      let* () = Lwt.pause () in
      accept_loop ()
  in
  accept_loop ()

let dial_bootstrap t =
  let open Lwt.Syntax in
  let endpoints = bootstrap_endpoints t in
  let target = List.length endpoints in
  let dial_one endpoint =
    let addr = P2p_endpoint.to_string endpoint in
    if Hashtbl.mem t.dialing addr then
      Lwt.return_unit
    else begin
      Hashtbl.replace t.dialing addr ();
      let rec retry delay =
        if not t.running then begin
          Hashtbl.remove t.dialing addr;
          Lwt.return_unit
        end else if validator_count t >= target then begin
          Hashtbl.remove t.dialing addr;
          Lwt.return_unit
        end else
          let* result = dial t endpoint.host endpoint.port in
          match result with
          | Some _conn ->
            Hashtbl.remove t.dialing addr;
            Lwt.return_unit
          | None ->
            let next_delay = min 60.0 (delay *. 2.0) in
            let* () = Lwt_unix.sleep delay in
            retry next_delay
      in
      retry 1.0
    end
  in
  Lwt_list.iter_p dial_one endpoints

let reconnect_loop t =
  let open Lwt.Syntax in
  let rec loop () =
    if not t.running then Lwt.return_unit
    else begin
      let dead = Hashtbl.fold (fun id conn acc ->
        if not (P2p_conn.is_connected conn) then id :: acc else acc
      ) t.peers [] in
      List.iter (fun id -> Hashtbl.remove t.peers id) dead;
      if validator_count t < List.length (bootstrap_endpoints t) then
        Lwt.async (fun () -> dial_bootstrap t);
      retry_records t;
      let* () = Lwt_unix.sleep 10.0 in
      loop ()
    end
  in
  loop ()

let peer_exchange_loop t =
  let open Lwt.Syntax in
  let rec loop () =
    if not t.running then Lwt.return_unit
    else
      let* () = Lwt_unix.sleep 15.0 in
      let* () = Lwt_list.iter_p (exchange_peers t) (connected_peers t) in
      loop ()
  in
  loop ()

let start t ~on_message =
  t.running <- true;
  t.on_message <- handle_frame t on_message;
  Lwt.join [
    listen t;
    dial_bootstrap t;
    reconnect_loop t;
    peer_exchange_loop t;
  ]

let stop t =
  t.running <- false;
  let conns = Hashtbl.fold (fun _ c acc -> c :: acc) t.peers [] in
  Lwt_list.iter_p P2p_conn.close conns