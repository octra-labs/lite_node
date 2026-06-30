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