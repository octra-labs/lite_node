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


type catchup = {
  head : int;
  reason : string;
  target_epoch : int64;
  queue_reason : string;
}

type already_applied = {
  head : int;
  next_epoch : int;
}

type plan =
  | Apply_now
  | Already_applied of already_applied
  | Need_catchup of catchup

val plan :
  consensus_mode:bool ->
  head:int ->
  current_epoch:int ->
  plan