(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type lane = Control | Tx | Data

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
  control : bucket;
  tx : bucket;
  data : bucket;
}

type verdict =
  | Accept
  | Drop of string

let control_limit = {
  window_s = 60.0;
  max_frames = 2_048;
  max_bytes = 16 * 1024 * 1024;
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

let create ~now = {
  control = bucket now;
  tx = bucket now;
  data = bucket now;
}

let lane msg_type =
  if msg_type = P2p_frame.msg_tx_gossip then Tx
  else if msg_type = P2p_frame.msg_peers
       || msg_type = P2p_frame.msg_bundle_response
       || msg_type = P2p_frame.msg_catchup_range_response
       || msg_type = P2p_frame.msg_catchup_range_response_v2
       || msg_type = P2p_frame.msg_epoch_broadcast then Data
  else Control

let select t = function
  | Control -> t.control, control_limit
  | Tx -> t.tx, tx_limit
  | Data -> t.data, data_limit

let reset bucket now =
  bucket.start <- now;
  bucket.frames <- 0;
  bucket.bytes <- 0

let admit t ~now ~msg_type ~bytes =
  let bucket, limit = select t (lane msg_type) in
  if now -. bucket.start >= limit.window_s then reset bucket now;
  if bytes < 0 || bytes > limit.max_bytes then Drop "frame_byte_budget"
  else if bucket.frames >= limit.max_frames then Drop "frame_count_budget"
  else if bucket.bytes > limit.max_bytes - bytes then Drop "frame_byte_budget"
  else begin
    bucket.frames <- bucket.frames + 1;
    bucket.bytes <- bucket.bytes + bytes;
    Accept
  end