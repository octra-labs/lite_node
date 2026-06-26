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


module C_hash = Octra_consensus.C_hash
module C_types = Octra_consensus.C_types
module Finality_log = Octra_consensus.Finality_log
module Finality_recovery = Octra_consensus.Finality_recovery
module Flow = Consensus_finalized_flow
module Head_manifest = Octra_core.Head_manifest
module Log = Octra_log

type codec = {
  root_to_raw32 : string -> string;
  raw_to_hex : string -> string;
}

type empty_replay_plan =
  | Not_empty_replayable
  | Proposal_id_mismatch of {
      logged : string;
      computed : string;
    }
  | Arm_empty_replay of {
      finalize : C_types.finalize;
      proposer : Flow.proposer_info option;
      expected_root : string option;
      proposal_id : string;
      root_short : string;
    }

type deps = {
  chain_id : string;
  head : unit -> int;
  last_finality : unit -> Finality_log.entry option;
  cached_head : unit -> Head_manifest.t option;
  codec : codec;
  store_finalized : epoch:int -> C_types.finalize -> unit;
  store_proposer : Flow.proposer_info -> unit;
  store_expected_root : epoch:int -> root:string -> unit;
  store_empty_bundle : proposal_id:string -> unit;
  reset_proposal_state : unit -> unit;
  set_consensus_finalized : bool -> unit;
  normalize_next_epoch_for_head : source:string -> unit;
  mark_quarantine : string -> unit;
}

let zero_root =
  String.make 32 '\x00'

let short16 s =
  String.sub s 0 (min 16 (String.length s))

let empty_tx_hash () =
  Octra_net.Hash_domain.hash "octra:tx_list:v1" ""

let empty_replay_head_matches codec entry head =
  entry.Finality_log.height = head.Head_manifest.epoch_id + 1
  && head.Head_manifest.ledger_state_root <> None
  && entry.txid_hi = head.Head_manifest.txid_hi
  && entry.tx_list_hash = codec.raw_to_hex (empty_tx_hash ())

let plan_empty_replay ~chain_id ~codec ~head entry =
  match head with
  | Some head when empty_replay_head_matches codec entry head ->
    let tx_list_hash = empty_tx_hash () in
    let proposed_state_root = codec.root_to_raw32 entry.Finality_log.state_root in
    let header : C_types.epoch_header = {
      proto_version = C_types.proto_version_current;
      chain_id;
      epoch_id = Int64.of_int entry.height;
      prev_state_root = codec.root_to_raw32 head.Head_manifest.state_root;
      tx_list_hash;
      receipt_root = C_hash.receipt_root [];
      proposed_state_root;
      creator_addr = entry.creator_addr;
      txid_hi = entry.txid_hi;
      ts = entry.ts;
    } in
    let proposal_id = C_hash.proposal_id header in
    let proposal_id_hex = codec.raw_to_hex proposal_id in
    if proposal_id_hex <> entry.proposal_id then
      Proposal_id_mismatch {
        logged = short16 entry.proposal_id;
        computed = short16 proposal_id_hex;
      }
    else
      let finalize : C_types.finalize = {
        chain_id;
        epoch_id = header.epoch_id;
        commit_round = entry.round;
        header;
        proposal_id;
        precommits = [];
      } in
      Arm_empty_replay {
        finalize;
        proposer =
          Flow.proposer_info
            ~epoch:entry.height
            ~creator_addr:header.creator_addr
            ~commit_round:entry.round;
        expected_root =
          if proposed_state_root = zero_root then None else Some proposed_state_root;
        proposal_id;
        root_short = short16 entry.state_root;
      }
  | _ -> Not_empty_replayable

let arm_empty_replay deps entry plan =
  match plan with
  | Arm_empty_replay replay ->
    deps.store_finalized ~epoch:entry.Finality_log.height replay.finalize;
    (match replay.proposer with
     | Some proposer -> deps.store_proposer proposer
     | None -> ());
    (match replay.expected_root with
     | Some root -> deps.store_expected_root ~epoch:entry.height ~root
     | None -> ());
    deps.store_empty_bundle ~proposal_id:replay.proposal_id;
    deps.reset_proposal_state ();
    deps.set_consensus_finalized true;
    deps.normalize_next_epoch_for_head ~source:"startup_empty_finality_replay";
    Log.warn "finality"
      "startup empty-finality replay armed height = %d round = %d root = %s"
      entry.height entry.round replay.root_short;
    true
  | Proposal_id_mismatch mismatch ->
    Log.warn "finality"
      "startup empty-finality replay refused height = %d reason = proposal_id_mismatch log = %s computed = %s"
      entry.height mismatch.logged mismatch.computed;
    false
  | Not_empty_replayable ->
    false

let handle_pending deps head action entry =
  Log.warn "finality"
    "startup finality log pending height = %d head = %d"
    entry.Finality_log.height head;
  let replay_plan =
    plan_empty_replay
      ~chain_id:deps.chain_id
      ~codec:deps.codec
      ~head:(deps.cached_head ())
      entry
  in
  if not (arm_empty_replay deps entry replay_plan) then
    deps.mark_quarantine (Finality_recovery.reason ~head action)

let handle_startup deps =
  let head = deps.head () in
  let action = Finality_recovery.decide ~head (deps.last_finality ()) in
  match action with
  | Finality_recovery.Clean -> ()
  | Finality_recovery.Pending entry ->
    handle_pending deps head action entry
  | Finality_recovery.Gap entry ->
    Log.warn "finality"
      "startup finality log gap height = %d head = %d"
      entry.Finality_log.height head;
    deps.mark_quarantine (Finality_recovery.reason ~head action)