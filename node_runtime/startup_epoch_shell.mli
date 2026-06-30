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


type source =
  | Meta of int
  | Files of int
  | Genesis

type deps = {
  last_epoch_meta : unit -> string option;
  saved_epochs : unit -> int list;
  set_current_epoch : int -> unit;
}

val source :
  last_epoch_meta:string option ->
  saved_epochs:int list ->
  source

val current_epoch :
  source ->
  int

val run :
  deps ->
  int