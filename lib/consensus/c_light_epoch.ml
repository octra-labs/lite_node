(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  chain_id : string;
  epoch_id : int64;
  tx_hashes : string list;
  start_txid : int64;
  tx_count : int;
  prev_epoch_index_root : string;
  epoch_index_root : string;
}

type tx_inclusion = {
  epoch : t;
  tx_hash : string;
  index : int;
}

let is_hex_char = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let hex64 s =
  String.length s = 64 && String.for_all is_hex_char s

let computed_epoch_index_root t =
  Octra_net.Epoch_index_commitment.next_root_from_hashes_i64
    ~prev:t.prev_epoch_index_root
    ~epoch_id:t.epoch_id
    ~start_txid:t.start_txid
    t.tx_hashes
  |> snd

let sane t =
  t.chain_id <> ""
  && Int64.compare t.epoch_id 0L >= 0
  && Int64.compare t.start_txid 0L >= 0
  && t.tx_count >= 0
  && List.length t.tx_hashes = t.tx_count
  && List.for_all hex64 t.tx_hashes
  && hex64 t.prev_epoch_index_root
  && hex64 t.epoch_index_root

let verify t =
  sane t
  && String.lowercase_ascii t.epoch_index_root = computed_epoch_index_root t

let verify_chain_step ~prev t =
  verify prev
  && verify t
  && Int64.succ prev.epoch_id = t.epoch_id
  && t.chain_id = prev.chain_id
  && String.lowercase_ascii t.prev_epoch_index_root =
     String.lowercase_ascii prev.epoch_index_root

let same a b =
  a.chain_id = b.chain_id
  && a.epoch_id = b.epoch_id
  && a.tx_hashes = b.tx_hashes
  && a.start_txid = b.start_txid
  && a.tx_count = b.tx_count
  && a.prev_epoch_index_root = b.prev_epoch_index_root
  && a.epoch_index_root = b.epoch_index_root

let tx_position_ok proof =
  verify proof.epoch
  && proof.index >= 0
  && proof.index < proof.epoch.tx_count
  && List.nth proof.epoch.tx_hashes proof.index = String.lowercase_ascii proof.tx_hash