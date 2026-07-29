(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  txs : Octra_core.Transaction.t list ref;
  tx_hashes : string list ref;
  received : bool ref;
}

let create () = {
  txs = ref [];
  tx_hashes = ref [];
  received = ref false;
}

let set t txs tx_hashes =
  t.txs := txs;
  t.tx_hashes := tx_hashes;
  t.received := true

let reset_empty_received t =
  set t [] []

let reset_unsynced t =
  t.txs := [];
  t.tx_hashes := [];
  t.received := false

let mark_unsynced t =
  t.received := false

let txs t =
  !(t.txs)

let tx_hashes t =
  !(t.tx_hashes)

let received t =
  !(t.received)