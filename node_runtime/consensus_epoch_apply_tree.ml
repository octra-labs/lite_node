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

type finalize_effects = {
  now : unit -> float;
  log : string -> unit;
  record_epoch_complete : int -> float -> unit;
  notify_new_epoch : int -> int -> unit;
  notify_epoch_finalized : int -> int -> unit;
}

type finalize_request = {
  tree : Tree.t ref;
  epoch_id : int;
  epoch_ts : float;
  epoch_start : float;
  proposer_addr : string;
  validator_addr : string;
  confirmed_txs : Transaction.t list;
  deferred_count : int;
  state_hash : string;
  short : string -> string;
}

type finalize_result = {
  tx_material : tx_material;
  confirmed_count : int;
  epoch_elapsed : float;
  new_commit : string;
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

let node ?timestamp ~epoch_id ~proposer_addr ~parent ~state_hash material =
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
    timestamp = Option.value timestamp ~default:(float_of_int (epoch_id * 10));
    signature = "";
  }

let performance ~proposer_addr ~confirmed_count =
  Tree.{
    node_id = proposer_addr;
    solved_tasks = confirmed_count;
    avg_time = 0.;
    reliability = 1.;
  }

let finalized_line request ~confirmed_count ~epoch_elapsed =
  Printf.sprintf
    "event = epoch_finalized epoch = %d confirmed = %d deferred = %d elapsed = %.2fs validator = %s"
    request.epoch_id
    confirmed_count
    request.deferred_count
    epoch_elapsed
    (request.short request.validator_addr)

let finalize_epoch effects request =
  let confirmed_count = List.length request.confirmed_txs in
  let tx_material = tx_material request.confirmed_txs in
  let node =
    node
      ~epoch_id:request.epoch_id
      ~proposer_addr:request.proposer_addr
      ~parent:(parent !(request.tree))
      ~state_hash:request.state_hash
      ~timestamp:request.epoch_ts
      tx_material
  in
  ignore (Tree.add_node !(request.tree) node);
  let finalized_tree =
    Tree.finalize ~finalized_at:request.epoch_ts !(request.tree) request.proposer_addr [
      performance
        ~proposer_addr:request.proposer_addr
        ~confirmed_count;
    ]
  in
  let epoch_elapsed = effects.now () -. request.epoch_start in
  effects.log (finalized_line request ~confirmed_count ~epoch_elapsed);
  effects.record_epoch_complete confirmed_count epoch_elapsed;
  effects.notify_new_epoch request.epoch_id confirmed_count;
  effects.notify_epoch_finalized request.epoch_id confirmed_count;
  {
    tx_material;
    confirmed_count;
    epoch_elapsed;
    new_commit = Tree.hash finalized_tree;
  }