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


let max_frame_size = 10_000_000
let default_read_timeout_s = 10.0

let msg_hello = 0x01
let msg_hello_ack = 0x02
let msg_ping = 0x03
let msg_pong = 0x04
let msg_get_peers = 0x10
let msg_peers = 0x11
let msg_tx_gossip = 0x20
let msg_proofcert_gossip = 0x21
let msg_cons_propose = 0x30
let msg_cons_vote = 0x31
let msg_cons_finalize = 0x32
let msg_cons_timeout = 0x33
let msg_query_epoch_root = 0x34
let msg_epoch_root_response = 0x35
let msg_query_bundle = 0x36
let msg_bundle_response = 0x37
let msg_query_catchup_range = 0x38
let msg_catchup_range_response = 0x39
let msg_query_catchup_range_v2 = 0x3a
let msg_catchup_range_response_v2 = 0x3b
let msg_resource_attestation = 0x3c

let control_payload_max = 4 * 1024
let handshake_payload_max = 64 * 1024
let peers_payload_max = 512 * 1024
let tx_payload_max = P2p_tx_gossip.max_tx_json + 4_096

let known_msg_types = [
  msg_hello;
  msg_hello_ack;
  msg_ping;
  msg_pong;
  msg_get_peers;
  msg_peers;
  msg_tx_gossip;
  msg_proofcert_gossip;
  msg_cons_propose;
  msg_cons_vote;
  msg_cons_finalize;
  msg_cons_timeout;
  msg_query_epoch_root;
  msg_epoch_root_response;
  msg_query_bundle;
  msg_bundle_response;
  msg_query_catchup_range;
  msg_catchup_range_response;
  msg_query_catchup_range_v2;
  msg_catchup_range_response_v2;
  msg_resource_attestation;
]

let known_msg_type n =
  List.mem n known_msg_types

let max_payload_size msg_type =
  if msg_type = msg_hello || msg_type = msg_hello_ack then
    handshake_payload_max
  else if msg_type = msg_ping
       || msg_type = msg_pong
       || msg_type = msg_get_peers then
    control_payload_max
  else if msg_type = msg_peers then
    peers_payload_max
  else if msg_type = msg_tx_gossip then
    tx_payload_max
  else if known_msg_type msg_type then
    max_frame_size - 1
  else
    control_payload_max

let payload_error msg_type payload_len =
  let max_payload = max_payload_size msg_type in
  if payload_len <= max_payload then None
  else
    Some (Printf.sprintf
      "frame_payload_too_large type = 0x%02x size = %d max = %d"
      msg_type payload_len max_payload)

let msg_type_name = function
  | 0x01 -> "HELLO"
  | 0x02 -> "HELLO_ACK"
  | 0x03 -> "PING"
  | 0x04 -> "PONG"
  | 0x10 -> "GET_PEERS"
  | 0x11 -> "PEERS"
  | 0x20 -> "TX_GOSSIP"
  | 0x21 -> "PROOFCERT_GOSSIP"
  | 0x30 -> "CONS_PROPOSE"
  | 0x31 -> "CONS_VOTE"
  | 0x32 -> "CONS_FINALIZE"
  | 0x33 -> "CONS_TIMEOUT"
  | 0x34 -> "QUERY_EPOCH_ROOT"
  | 0x35 -> "EPOCH_ROOT_RESPONSE"
  | 0x36 -> "QUERY_BUNDLE"
  | 0x37 -> "BUNDLE_RESPONSE"
  | 0x38 -> "QUERY_CATCHUP_RANGE_V1"
  | 0x39 -> "CATCHUP_RANGE_RESPONSE_V1"
  | 0x3a -> "QUERY_CATCHUP_RANGE_V2"
  | 0x3b -> "CATCHUP_RANGE_RESPONSE_V2"
  | 0x3c -> "RESOURCE_ATTESTATION"
  | n -> Printf.sprintf "UNKNOWN(0x%02x)" n

type frame = {
  msg_type : int;
  payload : string;
}

let encode_frame (f : frame) : string =
  let payload_len = String.length f.payload in
  let frame_len = 1 + payload_len in
  if frame_len > max_frame_size then
    failwith (Printf.sprintf "frame_too_large size = %d max = %d" frame_len max_frame_size);
  (match payload_error f.msg_type payload_len with
   | Some error -> failwith error
   | None -> ());
  let buf = Bytes.create (4 + frame_len) in
  Bytes.set buf 0 (Char.chr ((frame_len lsr 24) land 0xFF));
  Bytes.set buf 1 (Char.chr ((frame_len lsr 16) land 0xFF));
  Bytes.set buf 2 (Char.chr ((frame_len lsr 8) land 0xFF));
  Bytes.set buf 3 (Char.chr (frame_len land 0xFF));
  Bytes.set buf 4 (Char.chr f.msg_type);
  Bytes.blit_string f.payload 0 buf 5 payload_len;
  Bytes.unsafe_to_string buf

let read_exact_raw fd n =
  let buf = Bytes.create n in
  let rec loop off =
    if off >= n then Lwt.return_unit
    else
      let open Lwt.Syntax in
      let* count = Lwt_unix.read fd buf off (n - off) in
      if count = 0 then Lwt.fail (Failure "connection_closed")
      else loop (off + count)
  in
  let open Lwt.Syntax in
  let* () = loop 0 in
  Lwt.return (Bytes.unsafe_to_string buf)

let write_all fd s =
  let len = String.length s in
  let rec loop off =
    if off >= len then Lwt.return_unit
    else
      let open Lwt.Syntax in
      let* count = Lwt_unix.write_string fd s off (len - off) in
      if count = 0 then Lwt.fail (Failure "write_failed")
      else loop (off + count)
  in
  loop 0

let read_frame ?(timeout_s=default_read_timeout_s) fd =
  Lwt_unix.with_timeout timeout_s (fun () ->
    let open Lwt.Syntax in
    let* header = read_exact_raw fd 4 in
    let b0 = Char.code header.[0] in
    let b1 = Char.code header.[1] in
    let b2 = Char.code header.[2] in
    let b3 = Char.code header.[3] in
    let frame_len = (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3 in
    if frame_len < 1 then Lwt.fail (Failure "empty_frame")
    else if frame_len > max_frame_size then
      Lwt.fail (Failure (Printf.sprintf "frame_too_large size = %d" frame_len))
    else
      let* type_data = read_exact_raw fd 1 in
      let msg_type = Char.code type_data.[0] in
      let payload_len = frame_len - 1 in
      match payload_error msg_type payload_len with
      | Some error -> Lwt.fail (Failure error)
      | None ->
        let* payload = read_exact_raw fd payload_len in
        Lwt.return { msg_type; payload })

let write_frame fd (f : frame) =
  write_all fd (encode_frame f)