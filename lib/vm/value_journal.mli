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


type transfer = {
  from_addr : string;
  to_addr : string;
  amount : Z.t;
}

type snapshot

type t

val create :
  balance:(string -> Z.t) ->
  t

val effective :
  t ->
  string ->
  Z.t

val transfer :
  t ->
  from_addr:string ->
  to_addr:string ->
  amount:Z.t ->
  bool

val apply :
  t ->
  Call_plan.value_effect ->
  bool

val discard :
  t ->
  unit

val commit :
  t ->
  debit:(string -> Z.t -> unit) ->
  credit:(string -> Z.t -> unit) ->
  unit

val snapshot :
  t ->
  snapshot

val restore :
  t ->
  snapshot ->
  unit