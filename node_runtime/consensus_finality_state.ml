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
  finalized : (int, Octra_consensus.C_types.finalize) Hashtbl.t;
  proposers : (int, Octra_core.Epochlog.proposer_info) Hashtbl.t;
  expected_roots : (int, string) Hashtbl.t;
}

type callbacks = {
  find_finalized : int -> Octra_consensus.C_types.finalize option;
  has_finalized : int -> bool;
  store_finalized : epoch:int -> Octra_consensus.C_types.finalize -> unit;
  store_proposer : int -> Octra_core.Epochlog.proposer_info -> unit;
  store_flow_proposer : Consensus_finalized_flow.proposer_info -> unit;
  store_expected_root : epoch:int -> root:string -> unit;
  remove_finalized : epoch:int -> unit;
  remove_proposer : epoch:int -> unit;
  prune_after_epoch : int -> unit;
}

let create () = {
  finalized = Hashtbl.create 16;
  proposers = Hashtbl.create 16;
  expected_roots = Hashtbl.create 16;
}

let find_finalized t epoch =
  Hashtbl.find_opt t.finalized epoch

let has_finalized t epoch =
  Hashtbl.mem t.finalized epoch

let store_finalized t epoch finalize =
  Hashtbl.replace t.finalized epoch finalize

let remove_finalized t epoch =
  Hashtbl.remove t.finalized epoch

let find_proposer t epoch =
  Hashtbl.find_opt t.proposers epoch

let store_proposer t epoch proposer =
  Hashtbl.replace t.proposers epoch proposer

let store_flow_proposer t (proposer : Consensus_finalized_flow.proposer_info) =
  store_proposer
    t
    proposer.epoch
    {
      Octra_core.Epochlog.creator_addr = proposer.creator_addr;
      commit_round = proposer.commit_round;
    }

let remove_proposer t epoch =
  Hashtbl.remove t.proposers epoch

let prune_before table epoch =
  Hashtbl.filter_map_inplace (fun key value ->
    if key < epoch then None else Some value
  ) table

let prune_proposers_before t epoch =
  prune_before t.proposers epoch

let find_expected_root t epoch =
  Hashtbl.find_opt t.expected_roots epoch

let store_expected_root t epoch root =
  Hashtbl.replace t.expected_roots epoch root

let remove_expected_root t epoch =
  Hashtbl.remove t.expected_roots epoch

let prune_expected_roots_before t epoch =
  prune_before t.expected_roots epoch

let prune_after table epoch =
  Hashtbl.filter_map_inplace (fun key value ->
    if key > epoch then None else Some value
  ) table

let prune_after_epoch t epoch =
  prune_after t.finalized epoch;
  prune_after t.proposers epoch;
  prune_after t.expected_roots epoch

let callbacks t =
  {
    find_finalized = find_finalized t;
    has_finalized = has_finalized t;
    store_finalized = (fun ~epoch finalize ->
      store_finalized t epoch finalize);
    store_proposer = store_proposer t;
    store_flow_proposer = store_flow_proposer t;
    store_expected_root = (fun ~epoch ~root ->
      store_expected_root t epoch root);
    remove_finalized = (fun ~epoch -> remove_finalized t epoch);
    remove_proposer = (fun ~epoch -> remove_proposer t epoch);
    prune_after_epoch = prune_after_epoch t;
  }