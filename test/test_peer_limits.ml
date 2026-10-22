(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Swarm = Octra_net.P2p_swarm
module Conn = Octra_net.P2p_conn
module Handshake = Octra_net.P2p_handshake
module Record = Octra_net.P2p_peer_record
module Registry = Octra_net.P2p_peer_registry
module Frame = Octra_net.P2p_frame

let () = Mirage_crypto_rng_unix.use_default ()

let require ok reason = if not ok then failwith reason

let close fd = Swarm.close_socket fd

let identity () =
  let secret, public = Mirage_crypto_ec.Ed25519.generate () in
  let pubkey = Mirage_crypto_ec.Ed25519.pub_to_octets public in
  let sign value = Mirage_crypto_ec.Ed25519.sign ~key:secret value in
  pubkey, sign

let state ?(keys = []) ?(max_peers = 16) ?(bootstrap = []) (pubkey, sign_fn) =
  let config : Swarm.config = {
    listen_port = 9100; chain_id = "peer-limits";
    node_id = Handshake.node_id_of_pubkey pubkey; node_addr = "local";
    pubkey_raw = pubkey; consensus_config_hash = String.make 32 'c';
    binary_hash = String.make 32 'b'; require_binary_hash = false;
    upgrade_plan = None; profile_plan = []; allowed_pubkeys = keys;
    bootstrap_peers = bootstrap; max_peers; sign_fn;
    best_epoch_fn = (fun () -> 1L);
    best_root_fn = (fun () -> String.make 32 'r');
  } in
  let swarm = Swarm.create config in
  swarm.running <- true;
  swarm

let with_state swarm run =
  let sockets = ref [] in
  let pair () =
    let left, right = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    sockets := left :: right :: !sockets;
    left, right in
  Lwt.finalize (fun () -> run pair) (fun () ->
    let open Lwt.Syntax in
    let* () = Swarm.stop swarm in
    Lwt_list.iter_s close !sockets)

let hello swarm (pubkey, sign_fn) =
  Handshake.make_hello ~chain_id:swarm.Swarm.config.chain_id
    ~node_addr:"peer" ~pubkey_raw:pubkey
    ~consensus_config_hash:(Swarm.config_hash swarm) ~listen_port:9101
    ~best_epoch:1L ~best_root:(String.make 32 'r') ~sign_fn

let connect ?direction swarm pair key =
  let hello = hello swarm key in
  let left, _ = pair () in
  let direction = Option.value ~default:Conn.Inbound direction in
  let conn = Conn.create
    ~peer_class:(Swarm.frame_peer_class (Swarm.peer_role swarm hello.node_id))
    left ~peer_id:hello.node_id ~addr:"peer" ~direction in
  Swarm.add_peer swarm ~generation:swarm.generation conn hello, conn

let slot_plan () =
  let rows = List.init 20 (fun index ->
    string_of_int index, (if index mod 2 = 0 then Swarm.Validator else Swarm.Observer),
    index <> 0) in
  let kept, removed = Swarm.peer_plan rows in
  require (List.length kept = 19 && removed = ["0"]) "peer plan lost live peer";
  require (List.length (List.filter (fun (_, role) -> role = Swarm.Validator) kept) = 9)
    "peer plan lost validator";
  require (Swarm.peer_plan (List.rev rows) = (List.rev kept, List.rev removed))
    "peer plan chose peers by order";
  let replace node = Swarm.peer_replace ~node ~full:true ~role:Swarm.Validator rows in
  let choices = List.init 32 (fun index -> replace (string_of_int index))
    |> List.sort_uniq Stdlib.compare in
  require (List.length choices > 1) "replacement ignores local identity";
  List.iter (function
    | Some id -> require (List.mem (id, Swarm.Observer, true) rows)
        "replacement removed a validator"
    | None -> failwith "replacement ignored spare peers") choices;
  require (Swarm.peer_replace ~node:"local" ~full:false ~role:Swarm.Validator rows = None)
    "free slot displaced a peer";
  require (Swarm.peer_replace ~node:"local" ~full:true ~role:Swarm.Observer rows = None)
    "observer displaced a peer";
  Lwt.return_unit

let set_roles () =
  let local = identity () in
  let active = identity () in
  let retired = List.init 12 (fun _ -> identity ()) in
  let keys = List.map fst (local :: active :: retired) in
  let swarm = state ~keys local in
  with_state swarm (fun pair ->
    let peers = List.map (fun key ->
      let added, conn = connect swarm pair key in
      require added "initial validator refused";
      conn) (active :: retired) in
    require (Swarm.set_validator_pubkeys swarm [fst local; fst active] = Ok ())
      "set update refused";
    require (Swarm.validator_count swarm = 1) "active validator lost";
    require (Swarm.observer_count swarm = 12) "set update dropped live peer";
    require (Hashtbl.length swarm.profiles = 13) "live peer profile lost";
    let kept = Swarm.peer_ids swarm in
    require (Swarm.set_validator_pubkeys swarm [fst local; fst active] = Ok ())
      "same set refused";
    require (Swarm.peer_ids swarm = kept) "same set changed peers";
    let open Lwt.Syntax in
    let* () = Lwt.pause () in
    require (List.length (List.filter Conn.is_connected peers) = 13)
      "set update closed live socket";
    let added, _ = connect swarm pair (identity ()) in
    require (not added) "new observer exceeded capacity";
    List.iter (fun conn ->
      if Conn.is_connected conn then
        require (Conn.peer_class conn = Swarm.frame_peer_class (Swarm.peer_role swarm conn.Conn.peer_id))
          "peer frame class differs from current role") peers;
    Lwt.return_unit)

let local_role () =
  let local = identity () in
  let active = identity () in
  let retired = identity () in
  let swarm = state ~keys:(List.map fst [local; active; retired]) local in
  with_state swarm (fun pair ->
    let added, live = connect swarm pair active in
    require added "active peer refused";
    let added, removed = connect swarm pair retired in
    require added "initial peer refused";
    require (Swarm.set_validator_pubkeys swarm [fst active] = Ok ())
      "local set update refused";
    require (Swarm.connected_count swarm = 2 && Conn.is_connected live)
      "set update lost connected peer";
    let open Lwt.Syntax in
    let* () = Lwt.pause () in
    require (Conn.is_connected removed) "retired socket lost";
    let added, _ = connect swarm pair retired in
    require (not added) "observer accepted observer";
    Lwt.return_unit)

let slot_reuse () =
  let local = identity () in
  let old = identity () in
  let swarm = state ~keys:[fst local] local in
  with_state swarm (fun pair ->
    let added, first = connect swarm pair old in
    require added "first observer refused";
    let open Lwt.Syntax in
    let* () = Conn.close first in
    let peers = List.init 8 (fun _ ->
      let added, conn = connect swarm pair (identity ()) in
      require added "observer slot refused";
      conn) in
    let added, _ = connect swarm pair old in
    require (not added) "closed slot exceeded observer limit";
    require (Swarm.observer_count swarm = 8) "observer count differs";
    let* () = Conn.close (List.hd peers) in
    let id = Handshake.node_id_of_pubkey (fst old) in
    let preferred = Swarm.preferred_direction swarm id in
    let other = match preferred with Conn.Inbound -> Conn.Outbound | Conn.Outbound -> Conn.Inbound in
    let added, replaced = connect ~direction:other swarm pair old in
    require added "released observer slot not reused";
    let added, current = connect ~direction:preferred swarm pair old in
    require added "preferred peer replacement refused";
    require (Swarm.observer_count swarm = 8) "replacement changed peer count";
    let* () = Lwt.pause () in
    require (not (Conn.is_connected replaced) && Conn.is_connected current)
      "replacement socket state differs";
    Lwt.return_unit)

let record swarm ~ts (pubkey, sign_fn) =
  Record.make ~chain_id:swarm.Swarm.config.chain_id ~node_addr:"peer" ~pubkey
    ~binary_hash:swarm.config.binary_hash ~config_hash:(Swarm.config_hash swarm)
    ~host:"127.0.0.1" ~port:9101 ~features:["pex"] ~ts ~sign_fn

let record_keys () =
  let local = identity () in
  let old = identity () in
  let next = identity () in
  let swarm = state ~keys:[fst local; fst old] local in
  let ts = Int64.of_float (Unix.gettimeofday ()) in
  require (Swarm.accept_record swarm (record swarm ~ts old) = Ok true)
    "initial peer record refused";
  require (Swarm.set_validator_pubkeys swarm [fst local; fst next] = Ok ())
    "record set update refused";
  require (Swarm.peer_records swarm = []) "old records retained";
  require (Swarm.accept_record swarm (record swarm ~ts next) = Ok true)
    "new active peer record refused";
  require (Swarm.accept_record swarm (record swarm ~ts old) = Error "pubkey")
    "removed peer record accepted";
  let bad = record swarm ~ts next in
  require (Swarm.accept_record swarm { bad with signature = String.make 64 'x' }
    = Error "signature") "invalid record signature accepted";
  let open_swarm = state local in
  require (Swarm.accept_record open_swarm (record open_swarm ~ts next) = Ok true)
    "open discovery refused signed record";
  require (Swarm.set_validator_pubkeys open_swarm [fst local] = Ok ())
    "open discovery set refused";
  require (Swarm.accept_record open_swarm (record open_swarm ~ts next) = Error "pubkey")
    "active discovery accepted outsider";
  Lwt.return_unit

let record_limit () =
  let local = identity () in
  let swarm = state local in
  let base = record swarm ~ts:1000L local in
  let row index = { base with Record.node_id = Printf.sprintf "%064d" index } in
  let registry = Registry.create () in
  for index = 1 to 256 do
    require (Registry.add ~now:1000. registry (row index) = Registry.Added)
      "record below limit refused"
  done;
  require (Registry.add ~now:1000. registry (row 257) = Registry.Rejected "record_capacity")
    "record registry exceeded limit";
  require (Registry.add ~now:1000. registry (row 1) = Registry.Refreshed)
    "record refresh at limit refused";
  require (List.length (Registry.values ~now:1000. registry) = 256)
    "record refresh changed count";
  require (Registry.add ~now:1301. registry { (row 257) with ts = 1301L } = Registry.Added)
    "expired record slots not reused";
  require (List.length (Registry.values ~now:1301. registry) = 1)
    "expired records retained";
  Lwt.return_unit

let record_set () =
  let local = identity () in
  let swarm = state local in
  let first = List.init 200 (fun _ -> identity ()) in
  let second = List.init 200 (fun _ -> identity ()) in
  let install keys =
    require (Swarm.set_validator_pubkeys swarm (List.map fst keys) = Ok ())
      "full record set refused";
    let ts = Int64.of_float (Unix.gettimeofday ()) in
    List.iter (fun key ->
      require (Swarm.accept_record swarm (record swarm ~ts key) = Ok true)
        "full active record set lost peer") keys;
    require (List.length (Swarm.peer_records swarm) = 200)
      "full active record count differs" in
  install first;
  install second;
  let ts = Int64.of_float (Unix.gettimeofday ()) in
  List.iter (fun key ->
    require (Swarm.accept_record swarm (record swarm ~ts key) = Error "pubkey")
      "previous full set record accepted") first;
  Lwt.return_unit

let accept_limit () =
  let local = identity () in
  let swarm = state ~keys:[fst local] local in
  with_state swarm (fun pair ->
    let accept index =
      let left, right = pair () in
      let host = Unix.inet_addr_of_string (Printf.sprintf "198.18.%d.1" index) in
      Swarm.accept swarm left (Unix.ADDR_INET (host, 9100));
      left, right in
    let sockets = List.init 120 accept in
    let refused, _ = accept 128 in
    let open Lwt.Syntax in
    let* () = Lwt.pause () in
    require (List.length swarm.pending = 120)
      (Printf.sprintf "accept task count = %d slots = %d"
        (List.length swarm.pending) (List.length swarm.accepting));
    require (Lwt_unix.state refused = Lwt_unix.Closed) "excess accept socket retained";
    let* () = close (snd (List.hd sockets)) in
    let rec released () =
      if List.length swarm.pending = 119 then Lwt.return_unit
      else let* () = Lwt.pause () in released () in
    let* () = released () in
    let resumed, _ = accept 129 in
    require (List.length swarm.pending = 120) "released accept slot not reused";
    require (Lwt_unix.state resumed <> Lwt_unix.Closed) "resumed socket closed";
    let* () = Swarm.stop swarm in
    require (swarm.pending = [] && swarm.accepting = []) "stop retained accept tasks";
    swarm.running <- true;
    let restarted, _ = accept 130 in
    require (List.length swarm.pending = 1) "restart did not recover accept slots";
    require (Lwt_unix.state restarted <> Lwt_unix.Closed) "restart socket closed";
    Lwt.return_unit)

let wait_empty swarm =
  let rec wait () =
    if swarm.Swarm.pending = [] then Lwt.return_unit
    else let open Lwt.Syntax in let* () = Lwt.pause () in wait () in
  wait ()

let accept_keys () =
  let local = identity () in
  let old = identity () in
  let next = identity () in
  let swarm = state ~keys:[fst local; fst old] local in
  require (Swarm.set_validator_pubkeys swarm [fst next] = Ok ()) "accept set refused";
  with_state swarm (fun pair ->
    let client, server = pair () in
    Swarm.accept swarm server (Unix.ADDR_UNIX "active");
    let open Lwt.Syntax in
    let* result = Handshake.dial_handshake client ~my_hello:(hello swarm next)
      ~allowed_pubkeys:[] ~sign_fn:(snd next) in
    require (match result with Handshake.Ok _ -> true | _ -> false)
      "active incoming handshake refused";
    let* () = wait_empty swarm in
    require (Swarm.validator_count swarm = 1) "active incoming peer absent";
    let client, server = pair () in
    Swarm.accept swarm server (Unix.ADDR_UNIX "removed");
    let* result = Handshake.dial_handshake client ~my_hello:(hello swarm old)
      ~allowed_pubkeys:[] ~sign_fn:(snd old) in
    require (match result with Handshake.Error _ -> true | _ -> false)
      "removed incoming peer accepted";
    let* () = wait_empty swarm in
    require (Swarm.validator_count swarm = 1 && Swarm.observer_count swarm = 0)
      "incoming set check changed peers";
    Lwt.return_unit)

let dial_peer swarm remote ~on_ack ~expected =
  let listener = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let accepted = ref None in
  Lwt.finalize (fun () ->
    let open Lwt.Syntax in
    let* () = Lwt_unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) in
    Lwt_unix.listen listener 1;
    let port = match Lwt_unix.getsockname listener with
      | Unix.ADDR_INET (_, port) -> port | _ -> failwith "listener address differs" in
    let server () =
      let* fd, _ = Lwt_unix.accept listener in
      accepted := Some fd;
      let my_hello = hello swarm remote in
      let sign_fn message = on_ack (); snd remote message in
      Handshake.accept_handshake fd ~my_hello
        ~allowed_pubkeys:[] ~sign_fn in
    let* result, peer = Lwt.both (Swarm.dial swarm "127.0.0.1" port) (server ()) in
    require (Option.is_some result = expected) "outgoing peer decision differs";
    if expected then
      require (match peer with Handshake.Ok _ -> true | _ -> false)
        "honest outgoing handshake refused";
    Lwt.return_unit) (fun () ->
      let open Lwt.Syntax in
      let* () = Swarm.stop swarm in
      let* () = match !accepted with None -> Lwt.return_unit | Some fd -> close fd in
      close listener)

let dial_keys () =
  let local = identity () in
  let old = identity () in
  let next = identity () in
  let swarm = state ~keys:[fst local; fst old] local in
  require (Swarm.set_validator_pubkeys swarm [fst next] = Ok ()) "dial set refused";
  dial_peer swarm next ~on_ack:(fun () -> ()) ~expected:true

let dial_strict () =
  let local = identity () in
  let configured = identity () in
  let active = identity () in
  let outsider = identity () in
  Lwt_list.iter_s (fun (remote, expected) ->
    let swarm = state ~keys:[fst local; fst configured] local in
    require (Swarm.set_validator_pubkeys swarm [fst local; fst active] = Ok ())
      "strict dial set refused";
    dial_peer swarm remote ~on_ack:(fun () -> ()) ~expected)
    [outsider, false; configured, true; active, true]

let dial_cutover () =
  let local = identity () in
  let peer = identity () in
  let next = identity () in
  let swarm = state ~keys:[fst local; fst peer] local in
  let on_ack () =
    require (Swarm.set_validator_pubkeys swarm [fst next] = Ok ())
      "outgoing cutover refused" in
  dial_peer swarm peer ~on_ack ~expected:false

let accept_cutover () =
  let local = identity () in
  let peer = identity () in
  let next = identity () in
  let swarm = state ~keys:[fst local; fst peer] local in
  with_state swarm (fun pair ->
    let client, server = pair () in
    Swarm.accept swarm server (Unix.ADDR_UNIX "cutover");
    let request = hello swarm peer in
    let open Lwt.Syntax in
    let* () = Frame.write_frame client {
      msg_type = Frame.msg_hello; payload = Handshake.encode_hello request;
    } in
    let* response = Frame.read_handshake_frame client in
    require (response.msg_type = Frame.msg_hello_ack) "cutover ack absent";
    let ack = Handshake.decode_hello_ack response.payload in
    require (Swarm.set_validator_pubkeys swarm [fst next] = Ok ()) "cutover set refused";
    let finish = Handshake.make_hello_finish ~initiator:request ~responder:ack.hello
      ~sign_fn:(snd peer) in
    let* () = Frame.write_frame client {
      msg_type = Frame.msg_hello_finish; payload = Handshake.encode_hello_finish finish;
    } in
    let* () = wait_empty swarm in
    require (Swarm.connected_count swarm = 0) "late handshake bypassed current set";
    require (Lwt_unix.state server = Lwt_unix.Closed && swarm.accepting = [])
      "late handshake retained resources";
    Lwt.return_unit)

let accept_flood () =
  let local = identity () in
  let peer = identity () in
  let swarm = state ~keys:[fst local; fst peer] local in
  let joined, wake = Lwt.wait () in
  Swarm.set_peer_hook swarm (fun _ -> Lwt.wakeup_later wake ());
  with_state swarm (fun pair ->
    let accept host =
      let client, server = pair () in
      Swarm.accept swarm server (Unix.ADDR_INET (Unix.inet_addr_of_string host, 9100));
      client in
    for _ = 1 to 128 do ignore (accept "198.18.1.1") done;
    require (List.length swarm.accepting = 4) "one host consumed global slots";
    for index = 2 to 32 do ignore (accept (Printf.sprintf "198.18.1.%d" index)) done;
    require (List.length swarm.accepting = 16) "one network consumed global slots";
    let client = accept "198.18.2.1" in
    let open Lwt.Syntax in
    let* reply = Handshake.dial_handshake client ~my_hello:(hello swarm peer)
      ~allowed_pubkeys:[] ~sign_fn:(snd peer) in
    require (match reply with Handshake.Ok _ -> true | _ -> false)
      "second network validator refused";
    let* () = joined in
    require (Swarm.validator_count swarm = 1) "validator lost during slow handshake load";
    Lwt.return_unit)

let accept_reserve_at name host =
  let local = identity () in
  let peer = identity () in
  let swarm = state ~keys:[fst local; fst peer]
    ~bootstrap:[name ^ ":9100"] local in
  let joined, wake = Lwt.wait () in
  Swarm.set_peer_hook swarm (fun _ -> Lwt.wakeup_later wake ());
  with_state swarm (fun pair ->
    let open Lwt.Syntax in
    let* _ = Swarm.resolve swarm name 9100 in
    require (Swarm.known_host swarm host) "resolved host not known";
    for _ = 1 to 3 do
      ignore (Octra_net.P2p_peer_guard.report_bad swarm.peer_guard
        ~now:(Unix.gettimeofday ()) ~key:host ~reason:"invalid_frame")
    done;
    let accept host =
      let client, server = pair () in
      Swarm.accept swarm server (Unix.ADDR_INET (Unix.inet_addr_of_string host, 9100));
      client, server in
    for index = 0 to 119 do ignore (accept (Printf.sprintf "198.18.%d.1" index)) done;
    require (List.length swarm.accepting = 120) "public slots differ";
    let _, denied = accept "198.19.0.1" in
    require (List.length swarm.accepting = 120) "public peer used reserved slot";
    let client, _ = accept host in
    require (List.length swarm.accepting = 121) "known host lost reserved slot";
    let* reply = Handshake.dial_handshake client ~my_hello:(hello swarm peer)
      ~allowed_pubkeys:[] ~sign_fn:(snd peer) in
    require (match reply with Handshake.Ok _ -> true | _ -> false)
      "reserved validator refused";
    let* () = joined in
    require (Lwt_unix.state denied = Lwt_unix.Closed) "refused socket remained open";
    require (Swarm.validator_count swarm = 1) "reserved validator absent";
    Lwt.return_unit)

let accept_reserve () = accept_reserve_at "203.0.113.9" "203.0.113.9"

let accept_name () = accept_reserve_at "localhost" "127.0.0.1"

let name_expiry () =
  let swarm = state ~bootstrap:["localhost:9100"] (identity ()) in
  let open Lwt.Syntax in
  let* _ = Swarm.resolve swarm "localhost" 9100 in
  require (Swarm.known_host swarm "127.0.0.1") "resolved host absent";
  swarm.resolved <- List.map (fun (name, ips, _) -> name, ips, 0.) swarm.resolved;
  require (not (Swarm.known_host swarm "127.0.0.1")) "expired name retained";
  let* _ = Swarm.resolve swarm "localhost" 9100 in
  let* () = Swarm.stop swarm in
  require (swarm.resolved = []) "stopped swarm retained names";
  Lwt.return_unit

let reserve_limits () =
  let module A = Octra_net.P2p_accept in
  let grant = Some "peer" in
  let slots grant = List.init 4 (fun _ -> A.{token = ref (); host = "host"; net = "net"; grant}) in
  require (A.room ~grant ~host:"host" ~net:"net" (slots None))
    "public host slots block reserve";
  require (not (A.room ~grant ~host:"host" ~net:"net" (slots grant)))
    "reserved host cap missing";
  let full = List.init A.limit (fun n -> A.{token = ref (); host = string_of_int n;
    net = string_of_int n; grant = Some (string_of_int n)}) in
  require (not (A.room ~grant ~host:"next" ~net:"next" full)) "total cap missing";
  let boots = List.init A.reserve (fun n -> A.{token = ref (); host = string_of_int n;
    net = string_of_int n; grant = Some ("boot:" ^ string_of_int n)}) in
  require (not (A.room ~grant:(Some "boot:next") ~host:"next" ~net:"next" boots))
    "bootstrap pool exceeds reserve";
  require (A.room ~grant:(Some "peer:next") ~host:"next" ~net:"next" boots)
    "bootstrap pool blocks validator record";
  require (A.select (Some "boot:next") boots = None) "full bootstrap pool kept grant";
  require (A.room ~grant:(A.select (Some "boot:next") boots) ~host:"next" ~net:"next" boots)
    "bootstrap pool denied public lane";
  let public = List.init 120 (fun n -> A.{token = ref (); host = string_of_int n;
    net = string_of_int n; grant = None}) in
  require (A.room ~grant:(Some "peer:next") ~host:"next" ~net:"next" (public @ boots))
    "bootstrap pool consumed validator reserve";
  let local = identity () in
  let pubkey, sign_fn = identity () in
  let swarm = state ~keys:[fst local; pubkey] local in
  let record = Record.make ~chain_id:swarm.config.chain_id ~node_addr:"peer" ~pubkey
    ~binary_hash:swarm.config.binary_hash ~config_hash:(Swarm.config_hash swarm)
    ~host:"localhost" ~port:9101 ~features:["pex"]
    ~ts:(Int64.of_float (Unix.gettimeofday ())) ~sign_fn in
  require (Swarm.accept_record swarm record = Ok true) "DNS record refused for dialing";
  require (not (Swarm.known_name swarm "localhost")) "advertised DNS granted IP reserve";
  Lwt.return_unit

let accept_expiry () =
  let swarm = state (identity ()) in
  with_state swarm (fun pair ->
    let client, server = pair () in
    Swarm.accept swarm server (Unix.ADDR_UNIX "slow");
    let open Lwt.Syntax in
    let* () = Lwt_unix.sleep 2. in
    let* () = Frame.write_frame client { msg_type = Frame.msg_hello;
      payload = Handshake.encode_hello (hello swarm (identity ())) } in
    let* ack = Frame.read_handshake_frame client in
    require (ack.msg_type = Frame.msg_hello_ack) "deadline ack missing";
    let* count = Lwt_unix.with_timeout 120. (fun () ->
      Lwt_unix.read client (Bytes.create 1) 0 1) in
    require (count = 0) "expired handshake returned bytes";
    let* () = wait_empty swarm in
    require (swarm.accepting = [] && Lwt_unix.state server = Lwt_unix.Closed)
      "expired handshake retained slot";
    Lwt.return_unit)

let accept_net_reserve () =
  let local = identity () in
  let peer = identity () in
  let host = "198.18.1.99" in
  let swarm = state ~keys:[fst local; fst peer] ~bootstrap:[host ^ ":9100"] local in
  with_state swarm (fun pair ->
    for index = 1 to 4 do
      for _ = 1 to 4 do
        let _, server = pair () in
        Swarm.accept swarm server (Unix.ADDR_INET
          (Unix.inet_addr_of_string (Printf.sprintf "198.18.1.%d" index), 9100))
      done
    done;
    require (List.length swarm.accepting = 16) "public subnet not filled";
    let client, server = pair () in
    Swarm.accept swarm server (Unix.ADDR_INET (Unix.inet_addr_of_string host, 9100));
    require (List.length swarm.accepting = 17) "subnet blocked reserved host";
    let open Lwt.Syntax in
    let* reply = Handshake.dial_handshake client ~my_hello:(hello swarm peer)
      ~allowed_pubkeys:[] ~sign_fn:(snd peer) in
    require (match reply with Handshake.Ok _ -> true | _ -> false) "subnet reserve handshake failed";
    Lwt.return_unit)

let accept_held () =
  Lwt_list.iter_s (fun mismatch ->
  let swarm = state (identity ()) in
  with_state swarm (fun pair ->
    let open Lwt.Syntax in
    let rec loop left =
      if left = 0 then Lwt.return_unit else
      let clients = List.init 4 (fun _ ->
        let client, server = pair () in
        Swarm.accept swarm server (Unix.ADDR_UNIX "held"); client) in
      require (List.length swarm.accepting = 4) "held slots missing";
      let send client =
        let key = identity () in
        let request = hello swarm key in
        let request = if mismatch then
          let changed = {request with Handshake.consensus_config_hash = String.make 32 'x'} in
          {changed with signature = (snd key) (Handshake.hello_sign_bytes changed)} else request in
        let* () = Frame.write_frame client {msg_type = Frame.msg_hello;
          payload = Handshake.encode_hello request} in
        if mismatch then Lwt.return_unit else
        let* ack = Frame.read_handshake_frame client in
        require (ack.msg_type = Frame.msg_hello_ack) "held ack missing";
        Lwt.return_unit in
      let* () = if mismatch then Lwt.return_unit else Lwt_list.iter_s send clients in
      let* () = Lwt_unix.sleep 0.9 in
      let* () = if mismatch then Lwt_list.iter_s send clients else Lwt_list.iter_s Lwt_unix.close clients in
      let* () = wait_empty swarm in
      loop (left - 1) in
    let* () = loop 3 in
    let module G = Octra_net.P2p_peer_guard in
    let now = Unix.gettimeofday () in
    require (G.admit_conn swarm.peer_guard ~now ~key:"held" = G.Drop "handshake_budget")
      "held sockets not throttled";
    require (not (G.is_banned swarm.peer_guard ~now ~key:"held")) "held sockets banned IP";
    Lwt.return_unit)) [false; true]

let accept_grant_held () =
  let local = identity () in
  let peer = identity () in
  let other = identity () in
  let host = "203.0.113.7" in
  let swarm = state ~keys:[fst local; fst peer; fst other]
    ~bootstrap:[host ^ ":9100"; "203.0.113.8:9100"] local in
  with_state swarm (fun pair ->
    let open Lwt.Syntax in
    let accept host =
      let client, server = pair () in
      Swarm.accept swarm server (Unix.ADDR_INET (Unix.inet_addr_of_string host, 9100));
      client in
    let clients = List.init 4 (fun _ -> accept host) in
    let* () = Lwt_unix.sleep 2.7 in
    let* () = Lwt_list.iter_s Lwt_unix.close clients in
    let* () = wait_empty swarm in
    let module G = Octra_net.P2p_peer_guard in
    let now = Unix.gettimeofday () in
    require (G.holding swarm.peer_guard ~now ~key:("grant:boot:" ^ host))
      "known grant has no holding budget";
    ignore (accept host);
    require (swarm.accepting = []) "known host bypassed holding pause";
    let client = accept "203.0.113.8" in
    let* reply = Handshake.dial_handshake client ~my_hello:(hello swarm other)
      ~allowed_pubkeys:[] ~sign_fn:(snd other) in
    require (match reply with Handshake.Ok _ -> true | _ -> false)
      "holding pause blocked honest host in same subnet";
    Lwt.return_unit)

let accept_grant_move () =
  let local = identity () in
  let peer = identity () in
  let swarm = state ~keys:[fst local; fst peer] local in
  with_state swarm (fun pair ->
    let host = "203.0.113.42" in
    let pubkey, sign_fn = peer in
    let record = Record.make ~chain_id:swarm.config.chain_id ~node_addr:"peer" ~pubkey
      ~binary_hash:swarm.config.binary_hash ~config_hash:(Swarm.config_hash swarm)
      ~host ~port:9101 ~features:["pex"]
      ~ts:(Int64.of_float (Unix.gettimeofday ())) ~sign_fn in
    require (Result.is_ok (Swarm.accept_record swarm record)) "move record refused";
    let grant = Option.get (Swarm.host_grant swarm host) in
    let now = Unix.gettimeofday () in
    let module G = Octra_net.P2p_peer_guard in
    for _ = 1 to 3 do
      G.report_hold swarm.peer_guard ~now ~key:("grant:" ^ grant) ~held_s:4.9
    done;
    let _, server = pair () in
    Swarm.accept swarm server (Unix.ADDR_INET (Unix.inet_addr_of_string host, 9100));
    require (List.length swarm.accepting = 1) "paused grant lost public lane";
    require ((List.hd swarm.accepting).grant = None) "paused grant retained reserve";
    Lwt.return_unit)

let accept_identity_hold () =
  let local = identity () in
  let peer = identity () in
  let swarm = state ~keys:[fst local; fst peer] local in
  with_state swarm (fun pair ->
    let module G = Octra_net.P2p_peer_guard in
    let request = hello swarm peer in
    for _ = 1 to 3 do
      Swarm.report_bad_identity swarm ~peer_id:request.node_id ~reason:"invalid_frame"
    done;
    G.report_hold swarm.peer_guard ~now:(Unix.gettimeofday ()) ~key:"declined" ~held_s:9.9;
    let client, server = pair () in
    Swarm.accept swarm server (Unix.ADDR_UNIX "declined");
    let open Lwt.Syntax in
    let* () = Lwt_unix.sleep 0.2 in
    let* reply = Handshake.dial_handshake client ~my_hello:request
      ~allowed_pubkeys:[] ~sign_fn:(snd peer) in
    require (match reply with Handshake.Ok _ -> true | _ -> false) "identity check missed authenticated path";
    let* () = wait_empty swarm in
    require (Swarm.connected_count swarm = 0) "banned identity installed";
    require (G.holding swarm.peer_guard ~now:(Unix.gettimeofday ()) ~key:"declined")
      "authenticated decline did not charge holding time";
    Lwt.return_unit)

let accept_success_hold () =
  let swarm = state (identity ()) in
  let clock = ref 0. in
  with_state swarm (fun pair ->
    let open Lwt.Syntax in
    let rec loop left =
      if left = 0 then Lwt.return_unit else
      let clients = List.init 4 (fun _ ->
        let client, server = pair () in
        Swarm.accept ~clock:(fun () -> !clock) swarm server (Unix.ADDR_UNIX "success"); client) in
      clock := !clock +. 0.9;
      let* () = Lwt_list.iter_s (fun client ->
        let peer = identity () in
        let* reply = Handshake.dial_handshake client ~my_hello:(hello swarm peer)
          ~allowed_pubkeys:[] ~sign_fn:(snd peer) in
        require (match reply with Handshake.Ok _ -> true | _ -> false) "slow success handshake failed";
        Lwt.return_unit) clients in
      let* () = wait_empty swarm in
      require (Swarm.connected_count swarm = 4) "slow peers not installed";
      let* () = Lwt_list.iter_s Conn.close (Hashtbl.fold (fun _ conn acc -> conn :: acc) swarm.peers []) in
      let* () = Lwt_list.iter_s Lwt_unix.close clients in
      loop (left - 1) in
    let* () = loop 3 in
    require (Octra_net.P2p_peer_guard.holding swarm.peer_guard
      ~now:(Unix.gettimeofday ()) ~key:"success") "successful sockets escaped holding budget";
    Lwt.return_unit)

let accept_pools () =
  let local = identity () in
  let peer = identity () in
  let hosts = List.init 9 (fun n -> Printf.sprintf "203.0.%d.1" n) in
  let host = "203.0.100.1" in
  let make () = state ~keys:[fst local; fst peer]
    ~bootstrap:(List.map (fun host -> host ^ ":9100") (host :: hosts)) local in
  let open Lwt.Syntax in
  let swarm = make () in
  let* () = with_state swarm (fun pair ->
    List.iter (fun host ->
      let _, server = pair () in
      Swarm.accept swarm server (Unix.ADDR_INET (Unix.inet_addr_of_string host, 9101))) hosts;
    require (List.length swarm.accepting = 9) "full bootstrap pool denied public slot";
    require ((List.hd swarm.accepting).grant = None) "bootstrap overflow kept grant";
    Lwt.return_unit) in
  let swarm = make () in
  with_state swarm (fun pair ->
    let accept host =
      let client, server = pair () in
      Swarm.accept swarm server (Unix.ADDR_INET (Unix.inet_addr_of_string host, 9101)); client in
    for index = 0 to 119 do ignore (accept (Printf.sprintf "198.18.%d.1" index)) done;
    List.iteri (fun index host -> if index < 8 then ignore (accept host)) hosts;
    require (List.length swarm.accepting = 128) "public and bootstrap pools differ";
    let record = Record.make ~chain_id:swarm.config.chain_id ~node_addr:"peer" ~pubkey:(fst peer)
      ~binary_hash:swarm.config.binary_hash ~config_hash:(Swarm.config_hash swarm)
      ~host ~port:9101 ~features:["pex"] ~ts:(Int64.of_float (Unix.gettimeofday ())) ~sign_fn:(snd peer) in
    require (Result.is_ok (Swarm.accept_record swarm record)) "validator record refused";
    let client = accept host in
    require (List.length swarm.accepting = 129) "bootstrap pool took validator reserve";
    require (match (List.hd swarm.accepting).grant with
      | Some grant -> String.starts_with ~prefix:"peer:" grant | None -> false)
      "bootstrap took precedence over validator record";
    let* reply = Handshake.dial_handshake client ~my_hello:(hello swarm peer)
      ~allowed_pubkeys:[] ~sign_fn:(snd peer) in
    require (match reply with Handshake.Ok _ -> true | _ -> false) "reserved validator handshake failed";
    Lwt.return_unit)

let accept_record_move () =
  let local = identity () in
  let peer = identity () in
  let other = identity () in
  let swarm = state ~keys:[fst local; fst peer; fst other] local in
  with_state swarm (fun pair ->
    let add (pubkey, sign_fn) host =
      let record = Record.make ~chain_id:swarm.config.chain_id ~node_addr:"peer" ~pubkey
        ~binary_hash:swarm.config.binary_hash ~config_hash:(Swarm.config_hash swarm)
        ~host ~port:9101 ~features:["pex"]
        ~ts:(Int64.of_float (Unix.gettimeofday ())) ~sign_fn in
      require (Result.is_ok (Swarm.accept_record swarm record)) "record refused" in
    let accept host =
      let client, server = pair () in
      Swarm.accept swarm server (Unix.ADDR_INET (Unix.inet_addr_of_string host, 9101)); client in
    for index = 0 to 119 do ignore (accept (Printf.sprintf "198.18.%d.1" index)) done;
    add peer "203.0.113.1";
    for _ = 1 to 4 do ignore (accept "203.0.113.1") done;
    require (List.length swarm.accepting = 124) "record reserve missing";
    add peer "203.0.114.1";
    require (Swarm.known_host swarm "203.0.114.1") "record move missing";
    ignore (accept "203.0.114.1");
    require (List.length swarm.accepting = 124) "record move multiplied reserve";
    add other "203.0.115.1";
    let client = accept "203.0.115.1" in
    require (List.length swarm.accepting = 125) "other record lost reserve";
    let open Lwt.Syntax in
    let* reply = Handshake.dial_handshake client ~my_hello:(hello swarm other)
      ~allowed_pubkeys:[] ~sign_fn:(snd other) in
    require (match reply with Handshake.Ok _ -> true | _ -> false) "other record refused";
    Lwt.return_unit)

let accept_full_count () =
  let swarm = state (identity ()) in
  with_state swarm (fun pair ->
    for _ = 1 to 70 do
      let _, server = pair () in Swarm.accept swarm server (Unix.ADDR_UNIX "budget")
    done;
    require (Octra_net.P2p_peer_guard.is_banned swarm.peer_guard
      ~now:(Unix.gettimeofday ()) ~key:"budget") "capacity refused attempts uncounted";
    Lwt.return_unit)

let accept_soft_refusal () =
  Lwt_list.iter_s (fun capacity ->
    let local = identity () in
    let peer = identity () in
    let swarm = state ~keys:[fst local] local in
    with_state swarm (fun pair ->
      let observers = if capacity then List.init 8 (fun _ ->
        let accepted, conn = connect swarm pair (identity ()) in
        require accepted "observer setup failed"; conn) else [] in
      let request = hello swarm peer in
      let request = if capacity then request else
        let changed = { request with Handshake.consensus_config_hash = String.make 32 'x' } in
        { changed with signature = (snd peer) (Handshake.hello_sign_bytes changed) } in
      let attempt () =
        let client, server = pair () in
        Swarm.accept swarm server (Unix.ADDR_UNIX "honest");
        let open Lwt.Syntax in
        let* result = Handshake.dial_handshake client ~my_hello:request
          ~allowed_pubkeys:[] ~sign_fn:(snd peer) in
        let* () = wait_empty swarm in
        Lwt.return result in
      let open Lwt.Syntax in
      let* () = Lwt_list.iter_s (fun _ ->
        let* result = attempt () in
        require (match result with Handshake.Error _ -> true | _ -> false)
          "honest refusal not exercised";
        Lwt.return_unit) [1; 2; 3] in
      require (not (Octra_net.P2p_peer_guard.is_banned swarm.peer_guard
        ~now:(Unix.gettimeofday ()) ~key:"honest")) "honest refusal banned host";
      let* () = if capacity then Conn.close (List.hd observers) else begin
        swarm.active_hash <- request.consensus_config_hash; Lwt.return_unit end in
      let* result = attempt () in
      require (match result with Handshake.Ok _ -> true | _ -> false)
        "honest peer could not reconnect";
      Lwt.return_unit)) [true; false]

let set_promote () =
  let local = identity () in
  let active = List.init 26 (fun _ -> identity ()) in
  let guests = List.init 8 (fun _ -> identity ()) in
  let swarm = state ~max_peers:32 ~keys:(List.map fst (local :: active)) local in
  with_state swarm (fun pair ->
    List.iter (fun key -> require (fst (connect swarm pair key)) "initial peer refused")
      (active @ guests);
    require (Swarm.set_validator_pubkeys swarm (List.map fst (local :: active @ guests)) = Ok ())
      "promotion refused";
    require (Swarm.validator_count swarm = 34) "promotion dropped validator";
    List.iter (fun _ -> require (fst (connect swarm pair (identity ()))) "free slot refused")
      (List.init 6 Fun.id);
    require (not (fst (connect swarm pair (identity ())))) "total peer count exceeded";
    require (Swarm.connected_count swarm = 40) "total peer count differs";
    Lwt.return_unit)

let set_rotate () =
  Lwt_list.iter_s (fun direction ->
    let local = identity () in
    let old = List.init 32 (fun _ -> identity ()) in
    let next = List.init 32 (fun _ -> identity ()) in
    let swarm = state ~max_peers:32 ~keys:(List.map fst (local :: old)) local in
    with_state swarm (fun pair ->
      List.iter (fun key -> require (fst (connect swarm pair key)) "initial peer refused")
        (old @ List.init 8 (fun _ -> identity ()));
      let before = Swarm.peer_ids swarm in
      require (Swarm.set_validator_pubkeys swarm (List.map fst (local :: next)) = Ok ())
        "rotation refused";
      require (Swarm.peer_ids swarm = before) "rotation closed peers before replacement";
      List.iter (fun key ->
        require (fst (connect ~direction swarm pair key)) "new validator refused";
        require (Swarm.connected_count swarm = 40) "replacement changed total count") next;
      require (Swarm.validator_count swarm = 32 && Swarm.observer_count swarm = 8)
        "rotation did not restore validator slots";
      require (not (fst (connect swarm pair (identity ())))) "extra observer accepted";
      Lwt.return_unit)) [Conn.Inbound; Conn.Outbound]

let () =
  let failed = List.fold_left (fun count (name, run) ->
    try
      Lwt_main.run (Lwt_unix.with_timeout 30.0 run);
      Printf.printf "case = %s status = pass\n%!" name;
      count
    with error ->
      Printf.eprintf "case = %s status = fail reason = %s\n%!" name (Printexc.to_string error);
      count + 1) 0 [
      "slot_plan", slot_plan; "set_roles", set_roles; "local_role", local_role; "slot_reuse", slot_reuse;
      "set_rotate", set_rotate;
      "record_keys", record_keys; "record_limit", record_limit; "accept_limit", accept_limit;
      "record_set", record_set;
      "accept_keys", accept_keys; "dial_keys", dial_keys; "accept_cutover", accept_cutover;
      "dial_strict", dial_strict; "dial_cutover", dial_cutover;
      "accept_flood", accept_flood; "accept_reserve", accept_reserve;
      "accept_name", accept_name; "name_expiry", name_expiry;
      "reserve_limits", reserve_limits;
      "accept_pools", accept_pools;
      "accept_expiry", accept_expiry; "set_promote", set_promote;
      "accept_net_reserve", accept_net_reserve; "accept_held", accept_held;
      "accept_grant_held", accept_grant_held;
      "accept_grant_move", accept_grant_move;
      "accept_identity_hold", accept_identity_hold;
      "accept_success_hold", accept_success_hold;
      "accept_record_move", accept_record_move;
      "accept_full_count", accept_full_count;
      "accept_soft_refusal", accept_soft_refusal;
    ] in
  if failed > 0 then exit 1