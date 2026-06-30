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


type repair_kind =
  | Tx_loc
  | Txid_loc

type repair_deps = {
  kind : repair_kind;
  env_name : string;
  window : int;
  last_epoch : unit -> int option;
  repair :
    from_epoch:int ->
    to_epoch:int ->
    Octra_core.Store_chaindata.repair_stats;
}

type repair_windows = {
  tx_loc : int;
  txid_loc : int;
  strict : int;
}

type repair_pair_deps = {
  windows : repair_windows;
  last_epoch : unit -> int option;
  repair_tx_loc :
    from_epoch:int ->
    to_epoch:int ->
    Octra_core.Store_chaindata.repair_stats;
  repair_txid_loc :
    from_epoch:int ->
    to_epoch:int ->
    Octra_core.Store_chaindata.repair_stats;
}

type strict_deps = {
  window : int;
  last_epoch : unit -> int option;
  status_at : int -> Octra_core.Store_chaindata.epoch_index_status option;
  exit_fatal : unit -> unit;
}

type reindex_marker_deps = {
  marker_path : string;
  exists : string -> bool;
  exit_fatal : unit -> unit;
}

type startup_checks_deps = {
  int_value : string -> int -> int;
  last_epoch : unit -> int option;
  repair_tx_loc :
    from_epoch:int ->
    to_epoch:int ->
    Octra_core.Store_chaindata.repair_stats;
  repair_txid_loc :
    from_epoch:int ->
    to_epoch:int ->
    Octra_core.Store_chaindata.repair_stats;
  status_at : int -> Octra_core.Store_chaindata.epoch_index_status option;
  marker_path : string;
  marker_exists : string -> bool;
  irmin_stealth_counter : unit -> int64;
  chaindata_next_txid : unit -> int64;
  exit_fatal : unit -> unit;
}

type window =
  | Disabled of int
  | No_epochs
  | Window of {
      from_epoch : int;
      to_epoch : int;
    }

val repair_window :
  window:int ->
  last_epoch:int option ->
  window

val repair_windows :
  int_value:(string -> int -> int) ->
  repair_windows

val strict_failures :
  from_epoch:int ->
  to_epoch:int ->
  status_at:(int -> Octra_core.Store_chaindata.epoch_index_status option) ->
  (int * Octra_core.Store_chaindata.epoch_index_status) list

val run_repair :
  repair_deps ->
  unit

val run_repair_pair :
  repair_pair_deps ->
  unit

val run_strict_verify :
  strict_deps ->
  unit

val run_reindex_marker_guard :
  reindex_marker_deps ->
  unit

val log_stealth_counter :
  irmin_stealth_counter:int64 ->
  chaindata_next_txid:int64 ->
  unit

val run_startup_checks :
  startup_checks_deps ->
  unit