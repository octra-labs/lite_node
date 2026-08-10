(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type peer_class = Validator | Observer

type lane = Control | Consensus | Artifact | Tx | Data

type rejection_action = Close | Discard

type limit = {
  window_s : float;
  max_frames : int;
  max_bytes : int;
}

type bucket = {
  mutable start : float;
  mutable frames : int;
  mutable bytes : int;
}

type t = {
  mutable peer_class : peer_class;
  control : bucket;
  consensus : bucket;
  artifact : bucket;
  tx : bucket;
  data : bucket;
}

type rejection = {
  reason : string;
  lane : lane;
  peer_class : peer_class;
  frames : int;
  bytes : int;
  frame_bytes : int;
  max_frames : int;
  max_bytes : int;
  action : rejection_action;
}

type verdict =
  | Accept
  | Drop of rejection

let control_limit = {
  window_s = 60.0;
  max_frames = 2_048;
  max_bytes = 16 * 1024 * 1024;
}

let validator_consensus_limit = {
  window_s = 60.0;
  max_frames = 2_048;
  max_bytes = 128 * 1024 * 1024;
}

let observer_consensus_limit = {
  window_s = 60.0;
  max_frames = 256;
  max_bytes = 16 * 1024 * 1024;
}

let validator_artifact_limit = {
  window_s = 60.0;
  max_frames = 512;
  max_bytes = 512 * 1024 * 1024;
}

let observer_artifact_limit = {
  window_s = 60.0;
  max_frames = 512;
  max_bytes = 128 * 1024 * 1024;
}

let tx_limit = {
  window_s = 60.0;
  max_frames = 4_096;
  max_bytes = 64 * 1024 * 1024;
}

let data_limit = {
  window_s = 60.0;
  max_frames = 512;
  max_bytes = 128 * 1024 * 1024;
}

let bucket now = { start = now; frames = 0; bytes = 0 }

let create ~now ~peer_class = {
  peer_class;
  control = bucket now;
  consensus = bucket now;
  artifact = bucket now;
  tx = bucket now;
  data = bucket now;
}

let set_peer_class (state : t) peer_class =
  state.peer_class <- peer_class

let peer_class (state : t) =
  state.peer_class

let lane msg_type =
  if msg_type = P2p_frame.msg_tx_gossip then Tx
  else if msg_type = P2p_frame.msg_cons_propose
       || msg_type = P2p_frame.msg_cons_finalize then Consensus
  else if msg_type = P2p_frame.msg_bundle_response
       || msg_type = P2p_frame.msg_catchup_range_response
       || msg_type = P2p_frame.msg_catchup_range_response_v2
       || msg_type = P2p_frame.msg_epoch_broadcast then Artifact
  else if msg_type = P2p_frame.msg_peers
       || msg_type = P2p_frame.msg_resource_compute then Data
  else Control

let select t = function
  | Control -> t.control, control_limit
  | Consensus ->
    t.consensus,
    (match t.peer_class with
     | Validator -> validator_consensus_limit
     | Observer -> observer_consensus_limit)
  | Artifact ->
    t.artifact,
    (match t.peer_class with
     | Validator -> validator_artifact_limit
     | Observer -> observer_artifact_limit)
  | Tx -> t.tx, tx_limit
  | Data -> t.data, data_limit

let reset bucket now =
  bucket.start <- now;
  bucket.frames <- 0;
  bucket.bytes <- 0

let reject (state : t) lane (bucket : bucket) (limit : limit)
    ~reason ~frame_bytes =
  let action =
    match state.peer_class, lane with
    | Validator, Artifact -> Discard
    | _ -> Close
  in
  Drop {
    reason;
    lane;
    peer_class = state.peer_class;
    frames = bucket.frames;
    bytes = bucket.bytes;
    frame_bytes;
    max_frames = limit.max_frames;
    max_bytes = limit.max_bytes;
    action;
  }

let lane_name = function
  | Control -> "control"
  | Consensus -> "consensus"
  | Artifact -> "artifact"
  | Tx -> "tx"
  | Data -> "data"

let peer_class_name = function
  | Validator -> "validator"
  | Observer -> "observer"

let rejection_action_name = function
  | Close -> "close"
  | Discard -> "discard"

let admit t ~now ~msg_type ~bytes =
  let lane = lane msg_type in
  let bucket, limit = select t lane in
  if now -. bucket.start >= limit.window_s then reset bucket now;
  if bytes < 0 || bytes > limit.max_bytes then
    reject t lane bucket limit ~reason:"frame_byte_budget" ~frame_bytes:bytes
  else if bucket.frames >= limit.max_frames then
    reject t lane bucket limit ~reason:"frame_count_budget" ~frame_bytes:bytes
  else if bucket.bytes > limit.max_bytes - bytes then
    reject t lane bucket limit ~reason:"frame_byte_budget" ~frame_bytes:bytes
  else begin
    bucket.frames <- bucket.frames + 1;
    bucket.bytes <- bucket.bytes + bytes;
    Accept
  end