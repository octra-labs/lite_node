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

let mark_unsynced t =
  t.received := false

let txs t =
  !(t.txs)

let tx_hashes t =
  !(t.tx_hashes)

let received t =
  !(t.received)