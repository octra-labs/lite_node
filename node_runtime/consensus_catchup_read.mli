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
}

val range :
  ?max_chunk:int ->
  ?max_bytes:int ->
  deps ->
  from_epoch:int64 ->
  max_epochs:int ->
  [ `Ok of Octra_consensus.C_codec.catchup_epoch_record list * int64 option
  | `NotFound
  | `Internal of string ]