(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C_types = Octra_consensus.C_types
module Flow = Consensus_finalized_flow
module Journal = Consensus_finality_journal
module Log = Octra_log

type outcome =
  | Clean
  | Armed
  | Blocked

type deps = {
  read_backlog : unit -> (Journal.record list, string) result;
  write_finality : C_types.finalize -> unit;
  store_finalized :
    epoch:int ->
    validator_set:C_types.validator_set ->
    C_types.finalize ->
    unit;
  store_proposer : Flow.proposer_info -> unit;
  store_expected_root : epoch:int -> root:string -> unit;
  store_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Octra_core.Transaction.t list ->
    receipts_json:string list ->
    unit;
  set_consensus_finalized : bool -> unit;
  clear_state_attested : unit -> unit;
  mark_quarantine : string -> unit;
}

let block deps reason =
  deps.mark_quarantine reason;
  Log.fatal "finality"
    "event = finality_backlog action = block reason = %s"
    reason;
  Blocked

let stage deps record =
  let finalize = record.Journal.finalize in
  let epoch = Int64.to_int finalize.C_types.epoch_id in
  deps.write_finality finalize;
  deps.store_finalized
    ~epoch
    ~validator_set:record.Journal.validator_set
    finalize;
  (match
     Flow.proposer_info
       ~epoch
       ~creator_addr:finalize.C_types.header.creator_addr
       ~commit_round:finalize.C_types.commit_round
   with
   | Some proposer -> deps.store_proposer proposer
   | None -> ());
  (match Flow.expected_root finalize.C_types.header.proposed_state_root with
   | Some root -> deps.store_expected_root ~epoch ~root
   | None -> ());
  match record.Journal.bundle with
  | None ->
    failwith "finality backlog bundle is missing"
  | Some bundle ->
    deps.store_bundle
      ~proposal_id:finalize.C_types.proposal_id
      ~tx_hashes:bundle.tx_hashes
      ~txs:bundle.txs
      ~receipts_json:bundle.receipts_json

let run deps =
  match deps.read_backlog () with
  | Error reason ->
    block deps ("finality_backlog_invalid:" ^ reason)
  | Ok [] ->
    Clean
  | Ok (first_record :: _ as records) ->
    try
      List.iter (stage deps) records;
      deps.set_consensus_finalized true;
      deps.clear_state_attested ();
      let first =
        first_record.Journal.finalize.C_types.epoch_id
      in
      let last =
        List.fold_left
          (fun _ record -> record.Journal.finalize.C_types.epoch_id)
          first
          records
      in
      Log.warn "finality"
        "event = finality_backlog action = arm first = %Ld last = %Ld count = %d"
        first
        last
        (List.length records);
      Armed
    with exn ->
      block deps
        ("finality_backlog_stage_failed:" ^ Printexc.to_string exn)