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

val frame_type : int
val message :
  epoch_id:int ->
  root:string ->
  confirmed_count:int ->
  producer:string ->
  txs_serialized:string list ->
  message
val root_head : string -> string
val root_tail : string -> string
val payload : message -> string
val frame : message -> Octra_net.P2p_frame.frame
val log_view : message -> log_view