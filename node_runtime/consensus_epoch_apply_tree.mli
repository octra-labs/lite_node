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


module Node = Octra_core.Node
module Transaction = Octra_core.Transaction
module Tree = Octra_core.Tree

type tx_material = {
  txs_serialized : string list;
  tx_hashes_pairs : (string * string) list;
}

val parent : Tree.t -> string option
val txs_serialized : Transaction.t list -> string list
val tx_hashes_pairs : Transaction.t list -> (string * string) list
val tx_material : Transaction.t list -> tx_material
val node :
  epoch_id:int ->
  proposer_addr:string ->
  parent:string option ->
  state_hash:string ->
  tx_material ->
  Node.t
val performance :
  proposer_addr:string ->
  confirmed_count:int ->
  Tree.performance