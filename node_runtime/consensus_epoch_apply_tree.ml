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

let parent tree =
  match tree.Tree.roots with
  | h :: _ -> Some h
  | [] -> None

let txs_serialized txs =
  List.map (fun tx -> Yojson.Safe.to_string (Transaction.to_yojson tx)) txs

let tx_hashes_pairs txs =
  List.map (fun tx ->
    let json = Yojson.Safe.to_string (Transaction.to_yojson tx) in
    Transaction.hash tx, json) txs

let tx_material txs =
  {
    txs_serialized = txs_serialized txs;
    tx_hashes_pairs = tx_hashes_pairs txs;
  }

let metrics =
  Node.{ ths = 1.0; npt = 1.0; svb = 1.0; sp = 1.0; cps = 1.0 }

let node ~epoch_id ~proposer_addr ~parent ~state_hash material =
  let epoch_id_str = string_of_int epoch_id in
  Node.{
    id = Digestif.SHA256.(digest_string ("epoch:" ^ epoch_id_str ^ ":" ^ proposer_addr) |> to_hex);
    parent;
    children = [];
    state_hash;
    txs = material.txs_serialized;
    tx_hashes = material.tx_hashes_pairs;
    validator = proposer_addr;
    metrics;
    timestamp = float_of_int (epoch_id * 10);
    signature = "";
  }

let performance ~proposer_addr ~confirmed_count =
  Tree.{
    node_id = proposer_addr;
    solved_tasks = confirmed_count;
    avg_time = 0.;
    reliability = 1.;
  }