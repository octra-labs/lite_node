(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type profile = {
  epoch : int64;
  config_hash : string;
  profile_hash : string;
}

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
  profile_plan : profile list;
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

type peer_profile = {
  pubkey : string;
  current : string;
  mutable offered : (int64 * string) list;
}

type t = {
  config : config;
  mutable active_hash : string;
  peers : (string, P2p_conn.t) Hashtbl.t;
  profiles : (string, peer_profile) Hashtbl.t;
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
  mutable on_peer : (P2p_conn.t -> unit);
  mutable running : bool;
  mutable started : bool;
  mutable generation : unit ref;
  mutable listener : unit Lwt.t option;
  mutable pending : (unit ref * unit Lwt.t) list;
  mutable accepting : P2p_accept.slot list;
  mutable resolved : (string * string list * float) list;
  bootstrap : P2p_endpoint.t list;
  eviction_key : string;
}

let max_observer_peers = 8
let max_accepts = P2p_accept.limit
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
    active_hash = config.consensus_config_hash;
    peers = Hashtbl.create 16;
    profiles = Hashtbl.create 16;
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
    on_peer = (fun _ -> ());
    running = false;
    started = false;
    generation = ref ();
    listener = None;
    pending = [];
    accepting = [];
    resolved = [];
    bootstrap = P2p_endpoint.bootstrap config.bootstrap_peers;
    eviction_key = Mirage_crypto_rng_unix.getrandom 32;
  }

let config_hash t = t.active_hash

let keep_profile ~target ~current ~offered =
  String.equal current target
  || Option.fold
       ~none:false
       ~some:(String.equal target)
       offered

let set_handler t handler =
  t.on_message <- handler

let set_peer_hook t hook =
  t.on_peer <- hook

let note_peer t conn =
  try t.on_peer conn with _ -> ()

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
  P2p_peer_guard.snapshot ~now:(Unix.gettimeofday ()) t.peer_guard

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

let record_keys t =
  Option.value ~default:t.config.allowed_pubkeys t.validator_pubkeys

let handshake_keys t =
  let observers = Hashtbl.fold (fun id conn count ->
    if P2p_conn.is_connected conn && peer_role t id = Observer then count + 1
    else count) t.peers 0 in
  if local_accepts_observers t && observers < max_observer_peers then []
  else record_keys t

let dial_keys t =
  if not (local_accepts_observers t) then record_keys t
  else match t.config.allowed_pubkeys with
    | [] -> []
    | keys -> List.sort_uniq String.compare (keys @ record_keys t)

let peer_capacity ~max_peers ~validator_count ~observer_count = function
  | Validator -> validator_count < max_peers
  | Observer -> observer_count < max_observer_peers

let peer_plan rows =
  let kept, removed = List.partition (fun (_, _, live) -> live) rows in
  List.map (fun (id, role, _) -> id, role) kept,
  List.map (fun (id, _, _) -> id) removed

let peer_replace ~node ~full ~role rows =
  if not full || role <> Validator then None
  else
    let rank id = Digestif.SHA256.(to_raw_string (digest_string (node ^ id))) in
    rows
    |> List.filter (fun (_, kind, live) -> kind = Observer && live)
    |> List.sort (fun (a, _, _) (b, _, _) ->
      let order = String.compare (rank a) (rank b) in
      if order = 0 then String.compare a b else order)
    |> function (id, _, _) :: _ -> Some id | [] -> None

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
    let rows = Hashtbl.fold (fun id conn rows ->
      (id, peer_role t id, P2p_conn.is_connected conn) :: rows) t.peers [] in
    let kept, removed = peer_plan rows in
    List.iter (fun (id, role) ->
      P2p_conn.set_peer_class (Hashtbl.find t.peers id) (frame_peer_class role)) kept;
    List.iter (fun id ->
      let conn = Hashtbl.find t.peers id in
      Hashtbl.remove t.peers id;
      Hashtbl.remove t.profiles id;
      Lwt.async (fun () -> P2p_conn.close conn)) removed;
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

let active_profile t =
  List.find_opt (fun profile -> String.equal t.active_hash profile.config_hash)
    t.config.profile_plan

let wire_epoch t =
  let current = t.config.best_epoch_fn () in
  match active_profile t with
  | Some profile ->
    Int64.max current profile.epoch
  | None -> current

let config_hash_at t epoch =
  P2p_upgrade_plan.handshake_hash
    ~epoch
    ~config_hash:t.active_hash
    ~binary_hash:t.config.binary_hash
    ~require_binary_hash:t.config.require_binary_hash
    t.config.upgrade_plan

let hello_current t (hello : P2p_handshake.hello) =
  String.equal
    hello.P2p_handshake.consensus_config_hash
    (config_hash_at t (wire_epoch t))

let set_profile t hello =
  Hashtbl.replace
    t.profiles
    hello.P2p_handshake.node_id
    {
      pubkey = hello.pubkey;
      current = hello.consensus_config_hash;
      offered = [];
    }

let add_peer t ~generation (conn : P2p_conn.t) hello =
  let peer_id = conn.peer_id in
  if not t.running || generation != t.generation
      || peer_id = t.config.node_id then begin
    Lwt.async (fun () -> P2p_conn.close conn);
    false
  end
  else if not (hello_current t hello) then begin
    err_node t.config.node_addr
      "event = peer_rejected peer = %s reason = profile_changed"
      peer_id;
    Lwt.async (fun () -> P2p_conn.close conn);
    false
  end
  else
    let role = peer_role t peer_id in
    let existing = Hashtbl.find_opt t.peers peer_id in
    let live = Option.fold ~none:false ~some:P2p_conn.is_connected existing in
    let full = connected_count t >= t.config.max_peers + max_observer_peers in
    let replaced = if live then None else peer_replace ~node:t.eviction_key ~full ~role
      (Hashtbl.fold (fun id peer rows ->
        (id, peer_role t id, P2p_conn.is_connected peer) :: rows) t.peers []) in
    if (role = Observer && not (local_accepts_observers t))
        || (not live && (not (has_role_capacity t role)
            || (full && replaced = None))) then begin
      Lwt.async (fun () -> P2p_conn.close conn);
      false
    end else match existing with
    | Some existing when P2p_conn.is_connected existing ->
      if is_preferred t existing || not (is_preferred t conn) then begin
        Lwt.async (fun () -> P2p_conn.close conn);
        false
      end else begin
        Lwt.async (fun () -> P2p_conn.close existing);
        P2p_conn.set_peer_class conn (frame_peer_class role);
        Hashtbl.replace t.peers peer_id conn;
        set_profile t hello;
        note_peer t conn;
        true
      end
    | Some _ | None ->
      Option.iter (fun id ->
        let prior = Hashtbl.find t.peers id in
        Hashtbl.remove t.peers id;
        Hashtbl.remove t.profiles id;
        Lwt.async (fun () -> P2p_conn.close prior)) replaced;
      P2p_conn.set_peer_class conn (frame_peer_class role);
      Hashtbl.replace t.peers peer_id conn;
      set_profile t hello;
      note_peer t conn;
      true

let remove_peer t peer_id =
  (match Hashtbl.find_opt t.peers peer_id with
   | Some conn ->
     Lwt.async (fun () -> P2p_conn.close conn);
     Hashtbl.remove t.peers peer_id;
     Hashtbl.remove t.profiles peer_id
   | None -> ())

let send_deadline = 2.0
let broadcast_deadline = 2.0 *. send_deadline

let enqueue_with_timeout conn frame timeout_s =
  Lwt.catch
    (fun () ->
      Lwt.pick [
        P2p_conn.enqueue conn frame;
        (let open Lwt.Syntax in
         let* () = Lwt_unix.sleep timeout_s in
         Lwt.return_false)
      ])
    (function
      | Lwt.Canceled as error -> Lwt.fail error
      | _ -> Lwt.return_false)

let send_with_timeout conn frame timeout_s =
  Lwt.map (fun _ -> ()) (enqueue_with_timeout conn frame timeout_s)

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

let broadcast ?(required = false) t (frame : P2p_frame.frame) =
  let open Lwt.Syntax in
  let* sent = connected_peers t
    |> Lwt_list.map_p (fun conn -> enqueue_with_timeout conn frame send_deadline) in
  if required && not (List.exists Fun.id sent) then
    Lwt.fail_with "broadcast queues unavailable"
  else Lwt.return_unit

let broadcast_except t ~except (frame : P2p_frame.frame) =
  connected_peers t
  |> List.filter (fun conn -> conn.P2p_conn.peer_id <> except)
  |> Lwt_list.iter_p (fun conn -> send_with_timeout conn frame send_deadline)

let send_to t ~peer_id (frame : P2p_frame.frame) =
  match Hashtbl.find_opt t.peers peer_id with
  | Some conn when P2p_conn.is_connected conn ->
    send_with_timeout conn frame send_deadline
  | _ -> Lwt.return_unit

let make_my_hello t =
  let best_epoch = wire_epoch t in
  let consensus_config_hash = config_hash_at t best_epoch in
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
    ~config_hash:t.active_hash
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

let target_hash t profile =
  P2p_upgrade_plan.handshake_hash
    ~epoch:profile.epoch
    ~config_hash:profile.config_hash
    ~binary_hash:t.config.binary_hash
    ~require_binary_hash:t.config.require_binary_hash
    t.config.upgrade_plan

let send_profile t conn =
  Lwt_list.iter_s (fun profile ->
    let value =
      P2p_handshake.make_profile
        ~chain_id:t.config.chain_id
        ~node_id:t.config.node_id
        ~epoch:profile.epoch
        ~config_hash:(target_hash t profile)
        ~sign_fn:t.config.sign_fn
    in
    P2p_conn.send conn {
      msg_type = P2p_frame.msg_profile;
      payload = P2p_handshake.encode_profile value;
    }) t.config.profile_plan

let accept_record t r =
  let epoch = wire_epoch t in
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
    ~config_hash:t.active_hash
    ~allowed_pubkeys:(record_keys t)
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

let close_socket fd =
  Lwt.catch
    (fun () ->
      if Lwt_unix.state fd = Lwt_unix.Closed then Lwt.return_unit
      else Lwt_unix.close fd)
    (fun _ -> Lwt.return_unit)

let track t run =
  let token = ref () in
  let task = Lwt.finalize run (fun () ->
    t.pending <- List.filter (fun (key, _) -> key != token) t.pending;
    Lwt.return_unit) in
  if Lwt.is_sleeping task then
    t.pending <- (token, Lwt.map (fun _ -> ()) task) :: t.pending;
  task

let bootstrap_endpoints t =
  t.bootstrap

let known_name t host =
  List.exists (fun (endpoint : P2p_endpoint.t) -> endpoint.host = host)
    (bootstrap_endpoints t)
  || (try ignore (Unix.inet_addr_of_string host);
       Option.is_some (P2p_peer_registry.host_key ~now:(Unix.gettimeofday ())
         ~host ~keys:(record_keys t) t.registry)
      with Failure _ -> false)

let resolve t host port =
  let open Lwt.Syntax in
  let generation = t.generation in
  let* addrs = Lwt_unix.with_timeout connect_wait_s (fun () ->
    Lwt_unix.getaddrinfo host (string_of_int port)
      [Unix.AI_FAMILY Unix.PF_INET; Unix.AI_SOCKTYPE Unix.SOCK_STREAM]) in
  if t.running && generation == t.generation && known_name t host then begin
    let now = Unix.gettimeofday () in
    let ips = List.filter_map (fun ai -> match ai.Unix.ai_addr with
      | Unix.ADDR_INET (ip, _) -> Some (Unix.string_of_inet_addr ip)
      | _ -> None) addrs |> List.sort_uniq String.compare
      |> List.filteri (fun index _ -> index < 16) in
    let retained = List.filter (fun (name, _, until) ->
      name <> host && until > now && known_name t name) t.resolved
      |> List.filteri (fun index _ -> index < 127) in
    t.resolved <- (host, ips, now +. 60.) :: retained
  end;
  Lwt.return addrs

let host_grant t host =
  let now = Unix.gettimeofday () in
  let boot = List.find_opt (fun (endpoint : P2p_endpoint.t) ->
    endpoint.host = host || List.exists (fun (name, ips, until) ->
      List.mem host ips && until > now && name = endpoint.host) t.resolved) t.bootstrap in
  match P2p_peer_registry.host_key ~now ~host ~keys:(record_keys t) t.registry with
  | Some id -> Some ("peer:" ^ id)
  | None -> Option.map (fun (endpoint : P2p_endpoint.t) -> "boot:" ^ endpoint.host) boot

let known_host t host = Option.is_some (host_grant t host)

let dial t host port = track t (fun () ->
  let open Lwt.Syntax in
  let generation = t.generation in
  if validator_count t >= t.config.max_peers then
    Lwt.return_none
  else begin
  log_node t.config.node_addr "event = dial host = %s port = %d" host port;
  let fd_ref = ref None in
  Lwt.catch
    (fun () ->
      let* addrs = resolve t host port in
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
          ~allowed_pubkeys:(dial_keys t) ~sign_fn:t.config.sign_fn in
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
          end else if add_peer t ~generation conn peer_hello then begin
            log_node t.config.node_addr
              "event = connected peer = %s addr = %s"
              peer_id addr_str;
            Lwt.async (fun () -> P2p_conn.start conn ~on_message:t.on_message);
            Lwt.async (fun () -> exchange_peers t conn);
            Lwt.async (fun () -> send_profile t conn);
            Lwt.return_some conn
          end else begin
            let existing = Hashtbl.find_opt t.peers peer_id in
            let* () = P2p_conn.close conn in
            Lwt.return existing
          end)
    (fun exn ->
      let* () = match !fd_ref with
        | Some fd -> close_socket fd
        | None -> Lwt.return_unit in
      match exn with
      | Lwt.Canceled -> Lwt.fail exn
      | _ ->
        err_node t.config.node_addr
          "event = dial_exception host = %s port = %d error = %s"
          host port (Printexc.to_string exn);
        Lwt.return_none)
  end)

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
          Lwt.catch (fun () -> Lwt.finalize
            (fun () ->
              let open Lwt.Syntax in
              let* _ = dial t host port in
              Lwt.return_unit)
            (fun () ->
              Hashtbl.remove t.dialing endpoint;
              Hashtbl.remove t.record_dialing endpoint;
              Lwt.return_unit))
            (function Lwt.Canceled -> Lwt.return_unit | error -> Lwt.fail error))

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

let handle_profile t conn payload =
  match Hashtbl.find_opt t.profiles conn.P2p_conn.peer_id with
  | None -> Lwt.return_unit
  | Some peer ->
    Lwt.catch
      (fun () ->
        let value = P2p_handshake.decode_profile payload in
        match
          P2p_handshake.validate_profile
            ~chain_id:t.config.chain_id
            ~node_id:conn.peer_id
            ~pubkey:peer.pubkey
            value
        with
        | Error reason ->
          report_bad_peer t conn ~reason;
          Lwt.return_unit
        | Ok () ->
          if List.exists (fun expected -> Int64.equal value.epoch expected.epoch)
               t.config.profile_plan then
            peer.offered <- (value.epoch, value.config_hash)
              :: List.remove_assoc value.epoch peer.offered;
          Lwt.return_unit)
      (fun _ ->
        report_bad_peer t conn ~reason:"invalid_frame_profile";
        Lwt.return_unit)

let handle_frame t user_handler conn frame =
  if frame.P2p_frame.msg_type = P2p_frame.msg_get_peers then
    send_peers t conn
  else if frame.msg_type = P2p_frame.msg_peers then
    handle_peers t conn frame.payload
  else if frame.msg_type = P2p_frame.msg_profile then
    handle_profile t conn frame.payload
  else
    user_handler conn frame

let accept ?(clock = P2p_conn.monotonic_seconds) t fd addr =
  let open Lwt.Syntax in
  let key, net = P2p_accept.address addr in
  let grant = host_grant t key in
  let known = Option.is_some grant in
  let started = Unix.gettimeofday () in
  let began = clock () in
  let grant = match grant with
    | Some value when P2p_peer_guard.holding t.peer_guard ~now:started ~key:("grant:" ^ value) -> None
    | value -> value in
  let grant = P2p_accept.select grant t.accepting in
  let verdict = if not t.running then P2p_peer_guard.Drop "stopped" else
    match P2p_peer_guard.admit_conn ~known t.peer_guard ~now:started ~key with
    | P2p_peer_guard.Drop _ as verdict -> verdict
    | Accept ->
      if not (has_inbound_capacity t) then Drop "peer_capacity"
      else if not (P2p_accept.room ~grant ~host:key ~net t.accepting) then Drop "handshake_capacity"
      else Accept in
  if verdict <> P2p_peer_guard.Accept then begin
    (match verdict with
     | Drop reason when known && t.running ->
       Option.iter (fun count -> err_node t.config.node_addr
         "event = handshake_refused host = %s reason = %s count = %d" key reason count)
         (P2p_peer_guard.note_drop t.peer_guard ~now:started ~key ~reason)
     | _ -> ());
    Lwt.async (fun () -> close_socket fd)
  end
  else begin
  let slot = ref () in
  t.accepting <- P2p_accept.{ token = slot; host = key; net; grant } :: t.accepting;
  let generation = t.generation in
  let owned = ref true in
  Lwt.async (fun () -> track t (fun () -> Lwt.finalize
    (fun () -> Lwt.catch
      (fun () ->
        let my_hello = make_my_hello t in
        let* hs_result = Lwt.catch
          (fun () -> Lwt_unix.with_timeout P2p_accept.wait_s (fun () ->
            P2p_handshake.accept_handshake fd ~my_hello
              ~allowed_pubkeys:(handshake_keys t) ~sign_fn:t.config.sign_fn))
          (function
            | Lwt_unix.Timeout -> Lwt.return (P2p_handshake.Error "handshake timeout")
            | error -> Lwt.fail error) in
        match hs_result with
        | P2p_handshake.Error reason ->
          (match P2p_peer_guard.handshake_penalty reason with
           | None -> ()
           | Some penalty when t.running && generation == t.generation ->
             ignore (P2p_peer_guard.report_bad t.peer_guard
               ~now:(Unix.gettimeofday ()) ~key ~reason:penalty)
           | Some _ -> ());
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
          end else if add_peer t ~generation conn peer_hello then begin
            owned := false;
            Lwt.async (fun () -> P2p_conn.start conn ~on_message:t.on_message);
            Lwt.async (fun () -> exchange_peers t conn);
            Lwt.async (fun () -> send_profile t conn);
            Lwt.return_unit
          end else begin
            let* () = P2p_conn.close conn in
            Lwt.return_unit
          end)
      (fun _exn ->
        Lwt.return_unit))
    (fun () ->
      if t.running && generation == t.generation then begin
        let now = Unix.gettimeofday () in
        let held_s = clock () -. began in
        P2p_peer_guard.report_hold t.peer_guard ~now ~key ~held_s;
        Option.iter (fun grant ->
          P2p_peer_guard.report_hold t.peer_guard ~now ~key:("grant:" ^ grant) ~held_s) grant
      end;
      t.accepting <- P2p_accept.release slot t.accepting;
      if !owned then close_socket fd else Lwt.return_unit)))
  end

let listen ?(take = fun fd -> Lwt_unix.accept ~cloexec:true fd)
    ?(sleep = Lwt_unix.sleep) ?socket t =
  let open Lwt.Syntax in
  if Option.fold ~none:false ~some:Lwt.is_sleeping t.listener then
    invalid_arg "p2p listener is active";
  let fd = match socket with
    | Some fd -> fd
    | None -> Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  let rec accept_loop attempt =
    if not t.running then Lwt.return_unit
    else
      let* result = Lwt.catch
        (fun () -> Lwt.map Result.ok (take fd))
        (fun error -> Lwt.return_error error) in
      match result with
      | Ok (client_fd, client_addr) ->
        if not t.running then close_socket client_fd
        else begin
          let* () = Lwt.catch
            (fun () ->
              if attempt > 0 then
                log_node t.config.node_addr "event = accept status = resumed";
              accept t client_fd client_addr;
              Lwt.return_unit)
            (fun error ->
              let* () = close_socket client_fd in
              Lwt.fail error) in
          let* () = Lwt.pause () in
          accept_loop 0
        end
      | Error error ->
        match P2p_conn.accept_delay attempt error with
        | None -> Lwt.fail error
        | Some delay ->
          if attempt = 0 then
            err_node t.config.node_addr
              "event = accept status = waiting reason = %s"
              (Printexc.to_string error);
          let* () = sleep delay in
          accept_loop (min 7 (attempt + 1))
  in
  let task = Lwt.catch
    (fun () -> Lwt.finalize
      (fun () ->
        Lwt_unix.set_close_on_exec fd;
        Lwt_unix.setsockopt fd Lwt_unix.SO_REUSEADDR true;
        let* () = Lwt_unix.bind fd
          (Unix.ADDR_INET (Unix.inet_addr_any, t.config.listen_port)) in
        Lwt_unix.listen fd 32;
        accept_loop 0)
      (fun () -> close_socket fd))
    (function
      | Lwt.Canceled when not t.running -> Lwt.return_unit
      | error -> Lwt.fail error) in
  t.listener <- Some task;
  task

let stop t =
  let open Lwt.Syntax in
  t.running <- false;
  t.generation <- ref ();
  t.resolved <- [];
  let listener = t.listener in
  t.listener <- None;
  let* () = match listener with
    | None -> Lwt.return_unit
    | Some task ->
      Lwt.cancel task;
      Lwt.catch (fun () -> task) (fun _ -> Lwt.return_unit) in
  let pending = List.map snd t.pending in
  List.iter Lwt.cancel pending;
  let* () = Lwt_list.iter_p (fun task ->
    Lwt.catch (fun () -> task) (fun _ -> Lwt.return_unit)) pending in
  let conns = Hashtbl.fold (fun _ conn rest -> conn :: rest) t.peers [] in
  Lwt_list.iter_p P2p_conn.close conns

let run_tasks ?(stop = fun () -> Lwt.return_unit) listener services =
  let open Lwt.Syntax in
  let tasks = listener :: services in
  let failed, report = Lwt.wait () in
  let watch task = Lwt.catch
    (fun () -> task)
    (fun error ->
      if Lwt.is_sleeping failed then Lwt.wakeup_later_exn report error;
      Lwt.return_unit) in
  let watched = List.map watch tasks in
  Lwt.finalize
    (fun () -> Lwt.pick [listener; Lwt.join watched; failed])
    (fun () ->
      let* () = stop () in
      List.iter Lwt.cancel tasks;
      Lwt.join watched)

let dial_bootstrap t = track t (fun () ->
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
        if not t.running || validator_count t >= target then begin
          Lwt.return_unit
        end else
          let* result = dial t endpoint.host endpoint.port in
          match result with
          | Some _conn ->
            Lwt.return_unit
          | None when not t.running -> Lwt.return_unit
          | None ->
            let next_delay = min 60.0 (delay *. 2.0) in
            let* () = Lwt_unix.sleep delay in
            retry next_delay
      in
      Lwt.finalize (fun () -> retry 1.0) (fun () ->
        Hashtbl.remove t.dialing addr;
        Lwt.return_unit)
    end
  in
  Lwt_list.iter_p dial_one endpoints)

let reconnect_loop t =
  let open Lwt.Syntax in
  let rec loop () =
    if not t.running then Lwt.return_unit
    else begin
      let dead = Hashtbl.fold (fun id conn acc ->
        if not (P2p_conn.is_connected conn) then id :: acc else acc
      ) t.peers [] in
      List.iter
        (fun id ->
          Hashtbl.remove t.peers id;
          Hashtbl.remove t.profiles id)
        dead;
      if validator_count t < List.length (bootstrap_endpoints t) then
        Lwt.async (fun () -> Lwt.catch
          (fun () -> dial_bootstrap t)
          (function Lwt.Canceled -> Lwt.return_unit | error -> Lwt.fail error));
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
  if t.started then invalid_arg "p2p swarm is active";
  t.started <- true;
  t.running <- true;
  t.on_message <- handle_frame t on_message;
  let listener = Lwt.apply listen t in
  let services = List.map (fun run -> Lwt.apply run t)
    [dial_bootstrap; reconnect_loop; peer_exchange_loop] in
  Lwt.finalize
    (fun () -> run_tasks ~stop:(fun () -> stop t) listener services)
    (fun () ->
      t.started <- false;
      Lwt.return_unit)

let switch_profile t ~epoch =
  match List.find_opt (fun profile -> Int64.equal epoch profile.epoch)
          t.config.profile_plan with
  | None -> Lwt.return_error "profile switch is absent"
  | Some _ when Option.fold ~none:false
      ~some:(fun current -> epoch < current.epoch) (active_profile t) ->
    Lwt.return_error "profile switch precedes active profile"
  | Some profile ->
    let target = target_hash t profile in
    if String.equal t.active_hash profile.config_hash then
      Lwt.return_ok profile.profile_hash
    else
      let removed =
        Hashtbl.fold
          (fun peer_id _ acc ->
            match Hashtbl.find_opt t.profiles peer_id with
            | Some peer when
                keep_profile
                  ~target
                  ~current:peer.current
                  ~offered:(List.assoc_opt epoch peer.offered) -> acc
            | _ -> peer_id :: acc)
          t.peers
          []
        |> List.sort String.compare
      in
      t.active_hash <- profile.config_hash;
      P2p_peer_registry.clear t.registry;
      Hashtbl.clear t.record_due;
      let open Lwt.Syntax in
      let* () =
        Lwt_list.iter_p
          (fun peer_id ->
            match Hashtbl.find_opt t.peers peer_id with
            | None -> Lwt.return_unit
            | Some conn ->
              Hashtbl.remove t.peers peer_id;
              Hashtbl.remove t.profiles peer_id;
              P2p_conn.close conn)
          removed
      in
      log_node t.config.node_addr
        "event = profile_switch epoch = %Ld kept = %d dropped = %d"
        epoch
        (Hashtbl.length t.peers)
        (List.length removed);
      Lwt.return_ok profile.profile_hash