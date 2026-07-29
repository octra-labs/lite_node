(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type deps = {
  chain_id : string;
  get_epoch_json : int -> string option;
  epoch_time : int -> float option;
  get_tx_by_txid : int64 -> (string * string) option;
  read_receipts : int -> string list;
  root_to_raw32 : string -> string;
  reward_source :
    int ->
    Octra_core.Epochlog.epoch_header ->
    (Octra_consensus.C_types.reward_source, string) result;
  read_finality :
    int ->
    Octra_consensus.C_codec.catchup_finality option;
  head_epoch : unit -> int option;
  lookup_bundle : string -> Consensus_bundle_cache.encoded option;
}

type node_readers = {
  chain_id : string;
  get_epoch_json : int -> string option;
  get_tx_by_txid : int64 -> (string * string) option;
  read_receipts_opt : int -> string list option;
  root_to_raw32 : string -> string;
  reward_source :
    int ->
    Octra_core.Epochlog.epoch_header ->
    (Octra_consensus.C_types.reward_source, string) result;
  read_finality :
    int ->
    Octra_consensus.C_codec.catchup_finality option;
  cached_head : unit -> Octra_core.Head_manifest.t option;
  lookup_bundle : string -> Consensus_bundle_cache.encoded option;
}

type cached_root = {
  root : string;
  eic : string option;
}

type node_root_readers = {
  current_epoch : unit -> int;
  root_to_raw32 : string -> string;
  read_head_hash : unit -> string option Lwt.t;
  cached_head : unit -> Octra_core.Head_manifest.t option;
  get_epoch_json : int -> string option;
}

type node_root_deps = {
  read_local_ledger_root_raw : unit -> string Lwt.t;
  read_local_root_raw : unit -> string Lwt.t;
  committed_head_epoch : unit -> int;
  committed_epoch_root_raw : int -> string option;
  cached_root : unit -> cached_root;
}

val raw_zero :
  string

val root_to_raw32_or_zero :
  root_to_raw32:(string -> string) ->
  string ->
  string

val eic_root_to_raw32_or_zero :
  hex_to_raw32:(string -> string) ->
  string ->
  string

val ledger_root_or_zero :
  root_to_raw32:(string -> string) ->
  string option ->
  string

val cached_state_root :
  root_to_raw32:(string -> string) ->
  Octra_core.Head_manifest.t option ->
  string option

val committed_head_epoch :
  current_epoch:int ->
  Octra_core.Head_manifest.t option ->
  int

val epoch_root_from_json :
  root_to_raw32:(string -> string) ->
  string option ->
  string option

val cached_root :
  root_to_raw32:(string -> string) ->
  Octra_core.Head_manifest.t option ->
  cached_root

val epoch_root :
  deps ->
  int64 ->
  string option

val epoch_time :
  deps ->
  int64 ->
  float option

val local_head_epoch :
  deps ->
  int64

val bundle :
  deps ->
  string ->
  Consensus_bundle_cache.encoded option

val catchup_range :
  deps ->
  from_epoch:int64 ->
  max_epochs:int ->
  [ `Ok of Octra_consensus.C_codec.catchup_epoch_record list * int64 option
  | `NotFound
  | `Internal of string ]

val node_deps :
  node_readers ->
  deps

val node_store_deps :
  chain_id:string ->
  chaindata:Octra_core.Store_chaindata.t ->
  data_dir:string ->
  root_to_raw32:(string -> string) ->
  reward_source:
    (int ->
     Octra_core.Epochlog.epoch_header ->
     (Octra_consensus.C_types.reward_source, string) result) ->
  cached_head:(unit -> Octra_core.Head_manifest.t option) ->
  lookup_bundle:(string -> Consensus_bundle_cache.encoded option) ->
  deps

val node_root_deps :
  node_root_readers ->
  node_root_deps

val node_store_root_deps :
  chaindata:Octra_core.Store_chaindata.t ->
  store:Octra_core.Store_irmin.t ->
  current_epoch:(unit -> int) ->
  root_to_raw32:(string -> string) ->
  cached_head:(unit -> Octra_core.Head_manifest.t option) ->
  node_root_deps