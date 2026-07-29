(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

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

type hello_ack = {
  hello : hello;
  challenge_nonce : string;
  signature : string;
}

type hello_finish = {
  chain_id : string;
  consensus_config_hash : string;
  initiator_node_id : string;
  responder_node_id : string;
  initiator_nonce : string;
  responder_nonce : string;
  signature : string;
}

let proto_version_current = 3

let max_chain_id_bytes = 128
let max_node_id_bytes = 64
let max_node_addr_bytes = 128
let signature_bytes = 64

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

let put_hello buf (h : hello) =
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
  Oce1.put_bytes buf h.signature

let get_hello c =
  let chain_id = Oce1.get_string_bounded ~max:max_chain_id_bytes c in
  let proto_version = Oce1.get_u16 c in
  let node_id = Oce1.get_string_bounded ~max:max_node_id_bytes c in
  let node_addr = Oce1.get_string_bounded ~max:max_node_addr_bytes c in
  let pubkey = Oce1.get_raw c 32 in
  let consensus_config_hash = Oce1.get_hash32 c in
  let listen_port = Oce1.get_u16 c in
  let best_epoch = Oce1.get_u64 c in
  let best_root = Oce1.get_hash32 c in
  let nonce = Oce1.get_hash32 c in
  let signature = Oce1.get_bytes_bounded ~max:signature_bytes c in
  if String.length node_id <> max_node_id_bytes then
    failwith "p2p hello: invalid node id length";
  if String.length signature <> signature_bytes then
    failwith "p2p hello: invalid signature length";
  { chain_id; proto_version; node_id; node_addr; pubkey; consensus_config_hash;
    listen_port; best_epoch; best_root; nonce; signature }

let encode_hello (h : hello) =
  Oce1.encode (fun buf -> put_hello buf h)

let decode_hello payload =
  Oce1.decode get_hello payload

let hello_ack_sign_bytes ack =
  Hash_domain.hash_encoded "octra:p2p_hello_ack:v1" (fun buf ->
    Oce1.put_hash32 buf (hello_sign_bytes ack.hello);
    Oce1.put_hash32 buf ack.challenge_nonce)

let make_hello_ack ~(hello : hello) ~challenge_nonce ~sign_fn =
  let ack = {
    hello;
    challenge_nonce;
    signature = String.make signature_bytes '\x00';
  } in
  { ack with signature = sign_fn (hello_ack_sign_bytes ack) }

let encode_hello_ack ack =
  Oce1.encode (fun buf ->
    put_hello buf ack.hello;
    Oce1.put_hash32 buf ack.challenge_nonce;
    Oce1.put_sig64 buf ack.signature)

let decode_hello_ack payload =
  Oce1.decode (fun c ->
    let hello = get_hello c in
    let challenge_nonce = Oce1.get_hash32 c in
    let signature = Oce1.get_sig64 c in
    { hello; challenge_nonce; signature }) payload

let hello_finish_sign_bytes finish =
  Hash_domain.hash_encoded "octra:p2p_hello_finish:v1" (fun buf ->
    Oce1.put_string buf finish.chain_id;
    Oce1.put_hash32 buf finish.consensus_config_hash;
    Oce1.put_string buf finish.initiator_node_id;
    Oce1.put_string buf finish.responder_node_id;
    Oce1.put_hash32 buf finish.initiator_nonce;
    Oce1.put_hash32 buf finish.responder_nonce)

let make_hello_finish ~(initiator : hello) ~(responder : hello) ~sign_fn =
  let finish = {
    chain_id = initiator.chain_id;
    consensus_config_hash = initiator.consensus_config_hash;
    initiator_node_id = initiator.node_id;
    responder_node_id = responder.node_id;
    initiator_nonce = initiator.nonce;
    responder_nonce = responder.nonce;
    signature = String.make signature_bytes '\x00';
  } in
  { finish with signature = sign_fn (hello_finish_sign_bytes finish) }

let encode_hello_finish finish =
  Oce1.encode (fun buf ->
    Oce1.put_string buf finish.chain_id;
    Oce1.put_hash32 buf finish.consensus_config_hash;
    Oce1.put_string buf finish.initiator_node_id;
    Oce1.put_string buf finish.responder_node_id;
    Oce1.put_hash32 buf finish.initiator_nonce;
    Oce1.put_hash32 buf finish.responder_nonce;
    Oce1.put_sig64 buf finish.signature)

let decode_hello_finish payload =
  Oce1.decode (fun c ->
    let chain_id = Oce1.get_string_bounded ~max:max_chain_id_bytes c in
    let consensus_config_hash = Oce1.get_hash32 c in
    let initiator_node_id = Oce1.get_string_bounded ~max:max_node_id_bytes c in
    let responder_node_id = Oce1.get_string_bounded ~max:max_node_id_bytes c in
    let initiator_nonce = Oce1.get_hash32 c in
    let responder_nonce = Oce1.get_hash32 c in
    let signature = Oce1.get_sig64 c in
    if String.length initiator_node_id <> max_node_id_bytes
       || String.length responder_node_id <> max_node_id_bytes then
      failwith "p2p finish: invalid node id length";
    { chain_id; consensus_config_hash; initiator_node_id; responder_node_id;
      initiator_nonce; responder_nonce; signature }) payload

let random_nonce () =
  Mirage_crypto_rng.generate 32

let node_id_of_pubkey (pubkey_raw : string) =
  Hash_domain.hash_hex "octra:node_id:v1" pubkey_raw

let verify_hello_signature (h : hello) : bool =
  Octra_ed25519.verify
    ~pub:h.pubkey
    ~msg:(hello_sign_bytes h)
    h.signature

let verify_signature ~pubkey ~message ~signature =
  Octra_ed25519.verify ~pub:pubkey ~msg:message signature

let verify_node_id (h : hello) : bool =
  h.node_id = node_id_of_pubkey h.pubkey

let make_hello ~chain_id ~node_addr ~pubkey_raw ~consensus_config_hash
    ~listen_port ~best_epoch ~best_root ~sign_fn =
  if String.length pubkey_raw <> 32 then invalid_arg "invalid p2p public key";
  if String.length consensus_config_hash <> 32 then
    invalid_arg "invalid consensus config hash";
  if String.length best_root <> 32 then invalid_arg "invalid best root";
  if listen_port <= 0 || listen_port > 65535 then invalid_arg "invalid listen port";
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

let validate_hello ~my_chain_id ~my_config_hash ~allowed_pubkeys
    (h : hello) : (unit, string) result =
  if h.chain_id <> my_chain_id then
    Result.Error (Printf.sprintf "chain_id mismatch: %s vs %s" h.chain_id my_chain_id)
  else if h.proto_version <> proto_version_current then
    Result.Error (Printf.sprintf "p2p proto mismatch: peer=%d local=%d" h.proto_version proto_version_current)
  else if h.consensus_config_hash <> my_config_hash then
    Result.Error (Printf.sprintf "consensus_config_hash mismatch: peer=%s local=%s"
      (short_hash h.consensus_config_hash) (short_hash my_config_hash))
  else if not (verify_node_id h) then
    Result.Error "node_id does not match pubkey"
  else if h.listen_port <= 0 || h.listen_port > 65535 then
    Result.Error "invalid listen port"
  else if not (verify_hello_signature h) then
    Result.Error "invalid Ed25519 signature on HELLO"
  else if allowed_pubkeys <> [] && not (List.mem h.pubkey allowed_pubkeys) then
    Result.Error (Printf.sprintf "unknown validator pubkey (node_id=%s)" (String.sub h.node_id 0 12))
  else
    Result.Ok ()

let validate_hello_ack ~(my_hello : hello) ~allowed_pubkeys
    (ack : hello_ack) =
  match validate_hello ~my_chain_id:my_hello.chain_id
    ~my_config_hash:my_hello.consensus_config_hash ~allowed_pubkeys ack.hello with
  | Result.Error error -> Result.Error error
  | Result.Ok () when ack.challenge_nonce <> my_hello.nonce ->
    Result.Error "HELLO_ACK challenge mismatch"
  | Result.Ok () when not (verify_signature ~pubkey:ack.hello.pubkey
      ~message:(hello_ack_sign_bytes ack) ~signature:ack.signature) ->
    Result.Error "invalid Ed25519 signature on HELLO_ACK"
  | Result.Ok () -> Result.Ok ()

let validate_hello_finish ~(initiator : hello) ~(responder : hello)
    (finish : hello_finish) =
  if finish.chain_id <> initiator.chain_id
     || finish.chain_id <> responder.chain_id then
    Result.Error "HELLO_FINISH chain mismatch"
  else if finish.consensus_config_hash <> initiator.consensus_config_hash
          || finish.consensus_config_hash <> responder.consensus_config_hash then
    Result.Error "HELLO_FINISH config mismatch"
  else if finish.initiator_node_id <> initiator.node_id
          || finish.responder_node_id <> responder.node_id then
    Result.Error "HELLO_FINISH identity mismatch"
  else if finish.initiator_nonce <> initiator.nonce
          || finish.responder_nonce <> responder.nonce then
    Result.Error "HELLO_FINISH challenge mismatch"
  else if not (verify_signature ~pubkey:initiator.pubkey
      ~message:(hello_finish_sign_bytes finish) ~signature:finish.signature) then
    Result.Error "invalid Ed25519 signature on HELLO_FINISH"
  else Result.Ok ()

let dial_handshake fd ~(my_hello : hello) ~allowed_pubkeys ~sign_fn =
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
        let ack = decode_hello_ack frame.payload in
        match validate_hello_ack ~my_hello ~allowed_pubkeys ack with
        | Result.Error e -> Lwt.return (Error e)
        | Result.Ok () ->
          let finish = make_hello_finish ~initiator:my_hello
            ~responder:ack.hello ~sign_fn in
          let* () = P2p_frame.write_frame fd {
            msg_type = P2p_frame.msg_hello_finish;
            payload = encode_hello_finish finish;
          } in
          Lwt.return (Ok ack.hello))
    (fun exn ->
      Lwt.return (Error (Printf.sprintf "handshake failed: %s" (Printexc.to_string exn))))

let accept_handshake fd ~(my_hello : hello) ~allowed_pubkeys ~sign_fn =
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
          let ack = make_hello_ack ~hello:my_hello
            ~challenge_nonce:peer_hello.nonce ~sign_fn in
          let* () = P2p_frame.write_frame fd
            { msg_type = P2p_frame.msg_hello_ack;
              payload = encode_hello_ack ack } in
          let* finish_frame = P2p_frame.read_frame fd in
          if finish_frame.msg_type <> P2p_frame.msg_hello_finish then
            Lwt.return (Error (Printf.sprintf "expected HELLO_FINISH, got 0x%02x"
              finish_frame.msg_type))
          else
            let finish = decode_hello_finish finish_frame.payload in
            match validate_hello_finish ~initiator:peer_hello
              ~responder:my_hello finish with
            | Result.Error error -> Lwt.return (Error error)
            | Result.Ok () -> Lwt.return (Ok peer_hello))
    (fun exn ->
      Lwt.return (Error (Printf.sprintf "handshake failed: %s" (Printexc.to_string exn))))