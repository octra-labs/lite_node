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


type result = Preverify_submit.result = {
  delta_ok : bool;
  balance_ok : bool;
  sender_enc_snapshot : string;
}

type state =
  | Missing
  | Pending
  | Ready
  | Failed of string

type gate =
  | Ready_gate
  | Failed_gate of string
  | Timeout_gate of string
  | Defer_gate of {
    next_count : int;
    status : string;
  }

val pending_count :
  unit ->
  int

val pending_max :
  unit ->
  int

val has_capacity :
  unit ->
  bool

val start_task :
  (unit -> result Lwt.t) ->
  result Lwt.t

val insert_with_cap :
  string ->
  result Lwt.t ->
  unit

val remove :
  string ->
  unit

val state :
  string ->
  state

val gate :
  state:state ->
  defer_count:int ->
  max_defer:int ->
  gate

val ready_result :
  string ->
  sender_enc_snapshot:string ->
  result option

val retain :
  (string -> bool) ->
  unit

val prune :
  ?ttl:float ->
  ?max_entries:int ->
  unit ->
  unit