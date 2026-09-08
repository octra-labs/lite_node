(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module X = Octra_core.Epoch_exec
module T = Octra_core.Transaction
module O = Octra_core.Tx_outcome
module W = Octra_core.Preverify_worker
module G = Octra_core.Preverify_commit
module E = Octra_core.Epoch_index_commitment
module J = Octra_node_runtime.Consensus_join_rpc
module P = Octra_node_runtime.Consensus_proposal

type applied = {
  result : X.exec_result;
  index_root : string;
}

type deps = {
  head : unit -> string Lwt.t;
  preverify : T.t list -> W.batch Lwt.t;
  preview : G.t -> T.t list -> (X.exec_result, string) result Lwt.t;
  apply : G.t -> J.prepared -> applied Lwt.t;
}

type trace = {
  epoch : int64;
  ledger_root : string;
  index_root : string;
  state_root : string;
  confirmed : string list;
  rejections : string list;
  fees : Z.t;
  candidate_root : string;
  candidate_fees : Z.t;
}

let get = function
  | Ok value -> value
  | Error reason -> failwith reason

let require reason value =
  if not value then failwith reason

let hashes txs = List.map T.hash txs

let positions txs result =
  require "replay execution count differs" (result.X.artifacts.tx_count = List.length txs);
  require "replay execution fee is negative" (Z.sign result.artifacts.confirmed_fees >= 0);
  List.iter (fun (tx, position) ->
    require "replay confirmed position differs"
      (position >= 0 && Option.map T.hash (List.nth_opt txs position) = Some (T.hash tx))
  ) result.artifacts.confirmed

let artifacts txs result =
  positions txs result;
  require "replay confirmed order differs"
    (hashes (List.map fst result.X.artifacts.confirmed) = hashes txs);
  require "replay confirmed execution rejected a transaction"
    (result.artifacts.rejected = [])

let stable deps expected =
  let open Lwt.Syntax in
  let* current = deps.head () in
  require "replay preview changed the starting state" (current = expected);
  Lwt.return_unit

let run deps ~cursor ~(prepared : J.prepared) =
  let open Lwt.Syntax in
  let record = prepared.record in
  require "replay epoch differs from cursor" (record.epoch_id = cursor.J.epoch);
  require "replay previous root differs" (record.prev_state_root = cursor.prev_root);
  require "replay prepared epoch differs"
    (Int64.of_int prepared.epoch_int = record.epoch_id);
  let count = Int64.of_int (List.length prepared.txs) in
  require "replay transaction cursor is invalid"
    (Int64.compare cursor.txid 0L > 0 &&
     Int64.compare cursor.txid (Int64.sub Int64.max_int count) <= 0);
  let next_txid = Int64.add cursor.txid count in
  require "replay finalized transaction cursor differs"
    (record.finality.finalize.header.txid_hi = Int64.pred next_txid);
  require "replay next cursor differs"
    (record.epoch_id <> Int64.max_int &&
     prepared.next_cursor.epoch = Int64.succ record.epoch_id &&
     prepared.next_cursor.prev_root = record.state_root &&
     prepared.next_cursor.eic = prepared.expected_eic &&
     prepared.next_cursor.txid = next_txid);
  require "replay prepared transactions differ" (hashes prepared.txs = record.tx_hashes);
  let outcomes = get (O.decode_final ~confirmed:prepared.txs record.receipts_json) in
  let candidates = get (O.merge ~confirmed:prepared.txs ~rejections:outcomes.rejections) in
  let* initial = deps.head () in
  require "replay starting root differs"
    (E.folded_state_root ~ledger_state_root:initial ~epoch_index_root:cursor.eic
      = cursor.prev_root);
  let* batch = deps.preverify candidates in
  require "replay preverify omitted a candidate"
    (batch.W.skipped = [] && hashes (W.txs batch) = hashes candidates);
  let confirmed_hashes = hashes prepared.txs in
  let actual_receipts = W.receipt_json_for_hashes batch.ready confirmed_hashes in
  require "replay preverify receipts differ" (actual_receipts = outcomes.preverify);
  let candidate_gate = G.create (W.receipts_for_hashes batch.ready (hashes candidates)) in
  let confirmed_gate = G.create (W.receipts_for_hashes batch.ready confirmed_hashes) in
  get (G.check candidate_gate candidates);
  get (G.check confirmed_gate prepared.txs);
  let* () = stable deps initial in
  let* checked = deps.preview candidate_gate candidates in
  get (P.verify_preview_partition ~candidates ~confirmed:prepared.txs
    ~rejections:outcomes.rejections checked);
  let checked = get checked in
  positions candidates checked;
  require "replay rejected order differs"
    (hashes (List.map (fun (item : X.tx_reject) -> item.tx) checked.artifacts.rejected)
      = hashes (List.map (fun (item : O.rejection) -> item.tx) outcomes.rejections));
  let rejections =
    get (O.build ~candidates
      (List.map (fun (item : X.tx_reject) -> item.tx, item.error_type, item.reason)
        checked.artifacts.rejected))
    |> List.map O.encode_rejection
  in
  let* () = stable deps initial in
  let* preview = deps.preview confirmed_gate prepared.txs in
  let preview = get preview in
  artifacts prepared.txs preview;
  let* () = stable deps initial in
  let _, index_root = E.next_root_from_hashes_i64 ~prev:cursor.eic
    ~epoch_id:record.epoch_id ~start_txid:cursor.txid confirmed_hashes in
  require "replay prepared index differs" (index_root = prepared.expected_eic);
  let fold ledger_root = E.folded_state_root
    ~ledger_state_root:ledger_root ~epoch_index_root:index_root in
  require "replay confirmed preview root differs"
    (fold preview.post_state_root = record.state_root);
  let* applied = deps.apply confirmed_gate prepared in
  artifacts prepared.txs applied.result;
  require "replay apply differs from confirmed preview"
    (applied.result.post_state_root = preview.post_state_root);
  require "replay applied index differs" (applied.index_root = index_root);
  require "replay applied fees differ"
    (Z.equal applied.result.artifacts.confirmed_fees preview.artifacts.confirmed_fees);
  let* actual_head = deps.head () in
  require "replay reported apply root differs from store"
    (actual_head = applied.result.post_state_root);
  let state_root = fold actual_head in
  require "replay finalized root differs" (state_root = record.state_root);
  Lwt.return {
    epoch = record.epoch_id;
    ledger_root = actual_head;
    index_root;
    state_root;
    confirmed = confirmed_hashes;
    rejections;
    fees = applied.result.artifacts.confirmed_fees;
    candidate_root = checked.post_state_root;
    candidate_fees = checked.artifacts.confirmed_fees;
  }

let equal left right =
  left.epoch = right.epoch
  && left.ledger_root = right.ledger_root
  && left.index_root = right.index_root
  && left.state_root = right.state_root
  && left.confirmed = right.confirmed
  && left.rejections = right.rejections
  && Z.equal left.fees right.fees
  && left.candidate_root = right.candidate_root
  && Z.equal left.candidate_fees right.candidate_fees