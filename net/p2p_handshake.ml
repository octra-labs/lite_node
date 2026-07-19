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


type hello = {
  chain_id : string;
  proto_version : int;
  node_id : string;
  node_addr : string;
  pubkey : string;
  consensus_config_hash : string;
  listen_port : int;
  best_epoch : int64;
  best_root : string;
  nonce : string;
  signature : string;
}

let proto_version_current = 2

let short_hash h =
  String.concat "" (List.init (min 8 (String.length h)) (fun i ->
    Printf.sprintf "%02x" (Char.code h.[i])))


let hello_sign_bytes (h : hello) =
  Hash_domain.hash_encoded "octra:p2p_hello:v1" (fun buf ->
    Oce1.put_string buf h.chain_id;
    Oce1.put_u16 buf h.proto_version;
    Oce1.put_string buf h.node_id;
    Oce1.put_string buf h.node_addr;
    Oce1.put_raw buf h.pubkey;
    Oce1.put_hash32 buf h.consensus_config_hash;
    Oce1.put_u16 buf h.listen_port;
    Oce1.put_u64 buf h.best_epoch;
    Oce1.put_hash32 buf h.best_root;
    Oce1.put_hash32 buf h.nonce)


let encode_hello (h : hello) =
  Oce1.encode (fun buf ->
    Oce1.put_string buf h.chain_id;
    Oce1.put_u16 buf h.proto_version;
    Oce1.put_string buf h.node_id;
    Oce1.put_string buf h.node_addr;
    Oce1.put_raw buf h.pubkey;
    Oce1.put_hash32 buf h.consensus_config_hash;
    Oce1.put_u16 buf h.listen_port;
    Oce1.put_u64 buf h.best_epoch;
    Oce1.put_hash32 buf h.best_root;
    Oce1.put_hash32 buf h.nonce;
    Oce1.put_bytes buf h.signature)


let decode_hello payload =
  let c = Oce1.make_cursor payload in
  let chain_id = Oce1.get_string c in
  let proto_version = Oce1.get_u16 c in
  let node_id = Oce1.get_string c in
  let node_addr = Oce1.get_string c in
  let pubkey = Oce1.get_raw c 32 in
  let consensus_config_hash = Oce1.get_hash32 c in
  let listen_port = Oce1.get_u16 c in
  let best_epoch = Oce1.get_u64 c in
  let best_root = Oce1.get_hash32 c in
  let nonce = Oce1.get_hash32 c in
  let signature = Oce1.get_bytes c in
  { chain_id; proto_version; node_id; node_addr; pubkey; consensus_config_hash;
    listen_port; best_epoch; best_root; nonce; signature }


let random_nonce () =
  Mirage_crypto_rng.generate 32


let node_id_of_pubkey (pubkey_raw : string) =
  Hash_domain.hash_hex "octra:node_id:v1" pubkey_raw


let verify_hello_signature (h : hello) : bool =
  try
    let sign_bytes = hello_sign_bytes h in
    match Mirage_crypto_ec.Ed25519.pub_of_octets h.pubkey with
    | Ok pk ->
      Mirage_crypto_ec.Ed25519.verify ~key:pk ~msg:sign_bytes h.signature
    | Error _ -> false
  with _ -> false


let verify_node_id (h : hello) : bool =
  h.node_id = node_id_of_pubkey h.pubkey


let make_hello ~chain_id ~node_addr ~pubkey_raw ~consensus_config_hash
    ~listen_port ~best_epoch ~best_root ~sign_fn =
  let nonce = random_nonce () in
  let node_id = node_id_of_pubkey pubkey_raw in
  let h = {
    chain_id; proto_version = proto_version_current;
    node_id; node_addr; pubkey = pubkey_raw; consensus_config_hash; listen_port;
    best_epoch; best_root; nonce;
    signature = String.make 64 '\x00';
  } in
  let sign_bytes = hello_sign_bytes h in
  let signature = sign_fn sign_bytes in
  { h with signature }

type handshake_result =
  | Ok of hello
  | Error of string


let validate_hello ~my_chain_id ~my_config_hash ~allowed_pubkeys (h : hello) : (unit, string) result =
  if h.chain_id <> my_chain_id then
    Result.Error (Printf.sprintf "chain_id mismatch: %s vs %s" h.chain_id my_chain_id)
  else if h.proto_version <> proto_version_current then
    Result.Error (Printf.sprintf "p2p proto mismatch: peer=%d local=%d" h.proto_version proto_version_current)
  else if h.consensus_config_hash <> my_config_hash then
    Result.Error (Printf.sprintf "consensus_config_hash mismatch: peer=%s local=%s"
      (short_hash h.consensus_config_hash) (short_hash my_config_hash))
  else if not (verify_node_id h) then
    Result.Error "node_id does not match pubkey"
  else if not (verify_hello_signature h) then
    Result.Error "invalid Ed25519 signature on HELLO"
  else if allowed_pubkeys <> [] && not (List.mem h.pubkey allowed_pubkeys) then
    Result.Error (Printf.sprintf "unknown validator pubkey (node_id=%s)" (String.sub h.node_id 0 12))
  else
    Result.Ok ()


let dial_handshake fd ~my_hello ~allowed_pubkeys =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      let payload = encode_hello my_hello in
      let* () = P2p_frame.write_frame fd
        { msg_type = P2p_frame.msg_hello; payload } in
      let* frame = P2p_frame.read_frame fd in
      if frame.msg_type <> P2p_frame.msg_hello_ack then
        Lwt.return (Error (Printf.sprintf "expected HELLO_ACK, got 0x%02x" frame.msg_type))
      else
        let peer_hello = decode_hello frame.payload in
        match validate_hello ~my_chain_id:my_hello.chain_id
                ~my_config_hash:my_hello.consensus_config_hash
                ~allowed_pubkeys peer_hello with
        | Result.Error e -> Lwt.return (Error e)
        | Result.Ok () -> Lwt.return (Ok peer_hello))
    (fun exn ->
      Lwt.return (Error (Printf.sprintf "handshake failed: %s" (Printexc.to_string exn))))


let accept_handshake fd ~my_hello ~allowed_pubkeys =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      let* frame = P2p_frame.read_frame fd in
      if frame.msg_type <> P2p_frame.msg_hello then
        Lwt.return (Error (Printf.sprintf "expected HELLO, got 0x%02x" frame.msg_type))
      else
        let peer_hello = decode_hello frame.payload in
        match validate_hello ~my_chain_id:my_hello.chain_id
                ~my_config_hash:my_hello.consensus_config_hash
                ~allowed_pubkeys peer_hello with
        | Result.Error e -> Lwt.return (Error e)
        | Result.Ok () ->
          let payload = encode_hello my_hello in
          let* () = P2p_frame.write_frame fd
            { msg_type = P2p_frame.msg_hello_ack; payload } in
          Lwt.return (Ok peer_hello))
    (fun exn ->
      Lwt.return (Error (Printf.sprintf "handshake failed: %s" (Printexc.to_string exn))))