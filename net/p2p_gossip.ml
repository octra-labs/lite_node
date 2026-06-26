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


type topic = Tx | ProofCert | Consensus

let topic_to_u8 = function Tx -> 1 | ProofCert -> 2 | Consensus -> 3
let topic_of_u8 = function 1 -> Tx | 2 -> ProofCert | 3 -> Consensus | _ -> failwith "bad topic"

let seen : (string, float) Hashtbl.t = Hashtbl.create 4096
let seen_max = 10000
let seen_ttl = 300.0

let msg_id topic payload =
  let tag = Printf.sprintf "octra:gossip_id:%d" (topic_to_u8 topic) in
  Hash_domain.hash tag payload

let is_seen id =
  match Hashtbl.find_opt seen id with
  | Some ts when Unix.gettimeofday () -. ts < seen_ttl -> true
  | Some _ -> Hashtbl.remove seen id; false
  | None -> false

let mark_seen id =

  if Hashtbl.length seen > seen_max then begin
    let now = Unix.gettimeofday () in
    let to_remove = Hashtbl.fold (fun k ts acc ->
      if now -. ts > seen_ttl then k :: acc else acc
    ) seen [] in
    List.iter (Hashtbl.remove seen) to_remove
  end;
  Hashtbl.replace seen id (Unix.gettimeofday ())

type t = {
  swarm : P2p_swarm.t;
  mutable handlers : (topic -> string -> unit Lwt.t) list;
}

let create swarm = { swarm; handlers = [] }

let on_message t handler =
  t.handlers <- handler :: t.handlers

let publish t topic payload =
  let id = msg_id topic payload in
  if is_seen id then Lwt.return_unit
  else begin
    mark_seen id;
    let msg_type = match topic with
      | Tx -> P2p_frame.msg_tx_gossip
      | ProofCert -> P2p_frame.msg_proofcert_gossip
      | Consensus -> P2p_frame.msg_cons_propose
    in
    P2p_swarm.broadcast t.swarm { msg_type; payload }
  end

let handle_incoming t ~from_peer topic payload =
  let open Lwt.Syntax in
  let id = msg_id topic payload in
  if is_seen id then Lwt.return_unit
  else begin
    mark_seen id;

    let* () = Lwt_list.iter_s (fun h -> h topic payload) t.handlers in

    let msg_type = match topic with
      | Tx -> P2p_frame.msg_tx_gossip
      | ProofCert -> P2p_frame.msg_proofcert_gossip
      | Consensus -> P2p_frame.msg_cons_propose
    in
    P2p_swarm.broadcast_except t.swarm ~except:from_peer { msg_type; payload }
  end