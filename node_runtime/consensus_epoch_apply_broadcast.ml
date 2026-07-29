(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type message = {
  epoch_id : int;
  root : string;
  confirmed_count : int;
  producer : string;
  txs_serialized : string list;
}

type log_view = {
  epoch_id : int;
  root_head : string;
  root_tail : string;
}

let frame_type = Octra_net.P2p_frame.msg_epoch_broadcast

let message ~epoch_id ~root ~confirmed_count ~producer ~txs_serialized =
  {
    epoch_id;
    root;
    confirmed_count;
    producer;
    txs_serialized;
  }

let root_head root =
  String.sub root 0 8

let root_tail root =
  String.sub root (String.length root - 8) 8

let payload (message : message) =
  Octra_net.Oce1.encode (fun buf ->
    Octra_net.Oce1.put_u64 buf (Int64.of_int message.epoch_id);
    Octra_net.Oce1.put_string buf message.root;
    Octra_net.Oce1.put_u32 buf (Int32.of_int message.confirmed_count);
    Octra_net.Oce1.put_string buf message.producer;
    Octra_net.Oce1.put_u32 buf (Int32.of_int (List.length message.txs_serialized));
    List.iter (Octra_net.Oce1.put_string buf) message.txs_serialized)

let frame message =
  {
    Octra_net.P2p_frame.msg_type = frame_type;
    payload = payload message;
  }

let log_view (message : message) =
  {
    epoch_id = message.epoch_id;
    root_head = root_head message.root;
    root_tail = root_tail message.root;
  }