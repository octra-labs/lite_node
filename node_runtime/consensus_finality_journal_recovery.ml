(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C_types = Octra_consensus.C_types
module Flow = Consensus_finalized_flow
module Journal = Consensus_finality_journal
module Log = Octra_log

type outcome =
  | Continue
  | Armed
  | Blocked

type deps = {
  read_journal : unit -> Journal.read_result;
  head_epoch : unit -> int;
  root_at_epoch : int -> string option;
  current_root : unit -> string option;
  write_finality : C_types.finalize -> unit;
  store_finalized : epoch:int -> C_types.finalize -> unit;
  store_proposer : Flow.proposer_info -> unit;
  store_expected_root : epoch:int -> root:string -> unit;
  store_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Octra_core.Transaction.t list ->
    receipts_json:string list ->
    unit;
  set_proposal :
    Octra_core.Transaction.t list ->
    string list ->
    unit;
  reset_proposal_state : unit -> unit;
  set_consensus_finalized : bool -> unit;
  clear_state_attested : unit -> unit;
  commit_journal : unit -> unit;
  mark_quarantine : string -> unit;
}

let short value =
  String.concat ""
    (List.init (min 8 (String.length value)) (fun index ->
      Printf.sprintf "%02x" (Char.code value.[index])))

let block deps reason =
  deps.mark_quarantine reason;
  Log.fatal "finality"
    "event = finality_journal_recovery action = block reason = %s"
    reason;
  Blocked

let applied deps record target =
  try
    match deps.root_at_epoch target with
    | Some root
      when root = record.Journal.finalize.C_types.header.proposed_state_root ->
      deps.commit_journal ();
      Log.info "finality"
        "event = finality_journal_recovery action = commit_applied epoch = %d"
        target;
      Continue
    | Some _ ->
      block deps "finality_journal_applied_root_mismatch"
    | None ->
      block deps "finality_journal_applied_root_missing"
  with exn ->
    block deps
      ("finality_journal_commit_applied_failed:" ^ Printexc.to_string exn)

let store_bundle deps finalize = function
  | None -> ()
  | Some bundle ->
    deps.set_proposal bundle.Journal.txs bundle.tx_hashes;
    deps.store_bundle
      ~proposal_id:finalize.C_types.proposal_id
      ~tx_hashes:bundle.tx_hashes
      ~txs:bundle.txs
      ~receipts_json:bundle.receipts_json

let arm deps record target =
  let finalize = record.Journal.finalize in
  let current_root =
    try Ok (deps.current_root ()) with
    | exn -> Error (Printexc.to_string exn)
  in
  match current_root with
  | Error reason ->
    block deps ("finality_journal_head_root_failed:" ^ reason)
  | Ok None ->
    block deps "finality_journal_head_root_missing"
  | Ok (Some root)
    when root <> finalize.C_types.header.prev_state_root ->
    block deps "finality_journal_prev_root_mismatch"
  | Ok (Some _) ->
    try
      deps.write_finality finalize;
      deps.store_finalized ~epoch:target finalize;
      (match
         Flow.proposer_info
           ~epoch:target
           ~creator_addr:finalize.C_types.header.creator_addr
           ~commit_round:finalize.C_types.commit_round
       with
       | Some proposer -> deps.store_proposer proposer
       | None -> ());
      (match Flow.expected_root finalize.C_types.header.proposed_state_root with
       | Some root -> deps.store_expected_root ~epoch:target ~root
       | None -> ());
      deps.reset_proposal_state ();
      store_bundle deps finalize record.bundle;
      deps.set_consensus_finalized true;
      deps.clear_state_attested ();
      Log.warn "finality"
        "event = finality_journal_recovery action = arm epoch = %d round = %d pid = %s bundle = %b"
        target
        finalize.C_types.commit_round
        (short finalize.C_types.proposal_id)
        (Option.is_some record.bundle);
      Armed
    with exn ->
      block deps
        ("finality_journal_arm_failed:" ^ Printexc.to_string exn)

let recover deps record =
  let target = Int64.to_int record.Journal.finalize.C_types.epoch_id in
  let head = deps.head_epoch () in
  if target <= head then
    applied deps record target
  else if target = head + 1 then
    arm deps record target
  else
    block deps "finality_journal_height_gap"

let run deps =
  match deps.read_journal () with
  | Journal.Missing -> Continue
  | Journal.Invalid reason ->
    block deps ("finality_journal_invalid:" ^ reason)
  | Journal.Valid record ->
    recover deps record