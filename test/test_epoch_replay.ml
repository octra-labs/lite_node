(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module R = Epoch_replay
module J = R.J
module X = R.X
module T = R.T
module O = R.O
module E = R.E
module W = R.W
module C = Octra_consensus.C_types
module H = Octra_node_runtime.Consensus_replay

let expect name value = if not value then failwith name

let tx nonce = T.{
  from = "octSender";
  to_ = "octReceiver";
  amount = Z.one;
  nonce;
  ou = Z.of_int 1000;
  timestamp = 1.;
  signature = "";
  public_key = None;
  message = None;
  op_type = EncryptOp;
  encrypted_data = None;
}

let first = String.make 128 'a'
let next = String.make 128 'b'
let candidate_root = String.make 128 'c'
let confirmed = tx 1
let rejected = tx 2
let proof tx =
  let module Receipt = Octra_core.Preverify_receipt in
  let state = Receipt.{
    pre_state_hash = W.state_hash first;
    source_cipher_hash = zero_hash;
    pvac_key_hash = zero_hash;
    transition_hash = Some zero_hash;
  } in
  R.get (Receipt.for_tx_bound
    ~input_hash:(W.bound_input_hash tx state)
    ~output_hash:(W.bound_output_hash tx state "ok")
    ~state ~ok:true ~reason:"" tx)

let rejection = O.{ position = 1; tx = rejected; error_type = "balance"; reason = "not funded" }
let index = E.genesis_root
let previous = E.folded_state_root ~ledger_state_root:first ~epoch_index_root:index
let cursor = J.{ epoch = 1L; prev_root = previous; eic = index; txid = 1L }

let prepare txs rejections =
  let _, next_index = E.next_root_from_hashes_i64 ~prev:index ~epoch_id:1L
    ~start_txid:1L (R.hashes txs) in
  let next_root = E.folded_state_root ~ledger_state_root:next ~epoch_index_root:next_index in
  let count = List.length txs in
  let header, _ = H.parse_header ~default_chain_id:"replay-test" (`Assoc [
    "epoch_id", `Int 1;
    "txid_hi", `Int count;
    "creator_addr", `String "octProposer";
    "prev_state_root", `String previous;
    "proposed_state_root", `String next_root;
  ]) in
  let record = J.{
    epoch_id = 1L;
    prev_state_root = previous;
    state_root = next_root;
    tx_list_hash = "";
    tx_hashes = R.hashes txs;
    txs_json = List.map (fun tx -> Yojson.Safe.to_string (T.to_yojson tx)) txs;
    receipts_json = O.encode (W.json_of_receipts (List.map proof txs)) rejections;
    receipt_root = "";
    epoch_ts = 1.;
    creator_addr = "octProposer";
    commit_round = 0;
    reward_source = C.{
      reward_proposer_addr = "octProposer";
      reward_proposer_public_key = None;
      reward_members = [];
    };
    finality = Octra_consensus.C_codec.{
      finalize = (H.build_plan ~parent_commit:None ~header ~commit_round:0
        ~txs).finalize;
      validator_set = C.make_validator_set [];
    };
  } in
  J.{
    record;
    txs;
    expected_eic = next_index;
    next_cursor = {
      epoch = 2L; prev_root = next_root; eic = next_index; txid = Int64.of_int (count + 1);
    };
    epoch_int = 1;
    proposer_info = None;
    reward = { X.proposer_addr = "octProposer"; proposer_public_key = None; validators = [] };
  }

let result ~txs ~root ~rejections ~fees = X.{
  post_state_root = root;
  artifacts = {
    confirmed = List.mapi (fun position tx -> tx, position) txs;
    rejected = rejections;
    confirmed_fees = fees;
    tx_count = List.length txs + List.length rejections;
  };
}

let execute ?(txs = [confirmed]) ?(rejections = [rejection]) fault =
  let prepared = prepare txs rejections in
  let candidates = R.get (O.merge ~confirmed:txs ~rejections) in
  let head = ref first in
  let previews = ref 0 in
  let writes = ref 0 in
  let deps = R.{
    head = (fun () -> Lwt.return !head);
    preverify = (fun txs ->
      expect "preverify candidates" (R.hashes txs = R.hashes candidates);
      if fault = "prewrite" then head := next;
      let ready = List.map (fun (tx : T.t) ->
        let item_receipt =
          if fault = "receipt_missing" && tx.nonce = 2 then None
          else if fault = "receipt_changed" && tx.nonce = 1 then
            Some { (proof tx) with reason = "different" }
          else Some (proof tx)
        in
        W.{ tx; receipt = item_receipt }
      ) txs in
      let ready = if fault = "skip" then List.tl ready else ready in
      Lwt.return W.{ ready; skipped = [] });
    preview = (fun gate scope ->
      incr previews;
      let candidate = !previews = 1 in
      expect "preview transaction scope"
        (R.hashes scope = R.hashes (if candidate then candidates else txs));
      expect "preview receipt scope"
        (List.map (fun receipt -> receipt.Octra_core.Preverify_receipt.tx_hash)
          gate.R.G.receipts = R.hashes scope);
      if (candidate && fault = "candidate_write") ||
         (not candidate && fault = "confirmed_write") then head := next;
      let root = if candidate then candidate_root else next in
      let root = if not candidate && fault = "preview_root" then first else root in
      let rejections =
        if candidate then List.map (fun (item : O.rejection) -> X.{
          tx = item.tx;
          error_type = item.error_type;
          reason = (if fault = "reason" then "different" else item.reason);
        }) (if fault = "order" then List.rev rejections else rejections)
        else []
      in
      let fees = if fault = "negative" then Z.neg Z.one else Z.one in
      let value = result ~txs ~root ~rejections ~fees in
      let value =
        if fault = "position" then
          { value with artifacts = { value.artifacts with confirmed = [confirmed, 1] } }
        else if fault = "count" then
          { value with artifacts = { value.artifacts with tx_count = 0 } }
        else value
      in
      Lwt.return_ok value);
    apply = (fun gate _ ->
      incr writes;
      expect "apply follows both previews" (!previews = 2);
      expect "apply receipt scope"
        (List.map (fun receipt -> receipt.Octra_core.Preverify_receipt.tx_hash)
          gate.R.G.receipts = R.hashes txs);
      head := (if fault = "store" then first else next);
      let root = if fault = "apply_root" then first else next in
      let fees = if fault = "fees" then Z.zero else Z.one in
      let index_root = if fault = "index" then index else prepared.expected_eic in
      Lwt.return { result = result ~txs ~root ~rejections:[] ~fees; index_root });
  } in
  let prepared =
    if fault = "next_cursor" then
      { prepared with next_cursor = { prepared.next_cursor with txid = 3L } }
    else if fault = "epoch" then { prepared with epoch_int = 2 }
    else prepared
  in
  let cursor = if fault = "txid" then { cursor with txid = 2L } else cursor in
  let outcome =
    try Ok (Lwt_main.run (R.run deps ~cursor ~prepared))
    with Failure reason -> Error reason
  in
  outcome, !writes

let () =
  let trace = match execute "" with
    | Ok trace, 1 -> trace
    | _ -> failwith "replay legitimate control failed"
  in
  expect "confirmed root, not candidate root" (trace.R.ledger_root = next);
  expect "ordered rejection retained" (trace.rejections = [O.encode_rejection rejection]);
  let faults = [
    "next_cursor", "replay next cursor differs", 0;
    "epoch", "replay prepared epoch differs", 0;
    "txid", "replay finalized transaction cursor differs", 0;
    "receipt_missing", "missing_receipt:" ^ T.hash rejected, 0;
    "receipt_changed", "replay preverify receipts differ", 0;
    "skip", "replay preverify omitted a candidate", 0;
    "prewrite", "replay preview changed the starting state", 0;
    "candidate_write", "replay preview changed the starting state", 0;
    "reason", "preview_rejection_mismatch", 0;
    "position", "replay confirmed position differs", 0;
    "count", "replay execution count differs", 0;
    "negative", "replay execution fee is negative", 0;
    "confirmed_write", "replay preview changed the starting state", 0;
    "preview_root", "replay confirmed preview root differs", 0;
    "apply_root", "replay apply differs from confirmed preview", 1;
    "index", "replay applied index differs", 1;
    "fees", "replay applied fees differ", 1;
    "store", "replay reported apply root differs from store", 1;
  ] in
  List.iter (fun (fault, reason, writes) ->
    match execute fault with
    | Error actual, count ->
      expect fault (actual = reason && count = writes)
    | Ok _, _ -> failwith ("replay accepted " ^ fault)
  ) faults;
  expect "same traces" (R.equal trace trace);
  expect "different fees" (not (R.equal trace { trace with fees = Z.zero }));
  expect "different rejections" (not (R.equal trace { trace with rejections = [] }));
  expect "different roots" (not (R.equal trace { trace with ledger_root = first }));
  expect "different candidate fees"
    (not (R.equal trace { trace with candidate_fees = Z.zero }));
  expect "different candidate roots"
    (not (R.equal trace { trace with candidate_root = first }));
  begin match execute ~txs:[] ~rejections:[] "" with
  | Ok empty, 1 -> expect "empty epoch" (empty.confirmed = [] && empty.rejections = [])
  | _ -> failwith "empty epoch rejected"
  end;
  begin match execute ~txs:[] ~rejections:[] "txid" with
  | Error "replay finalized transaction cursor differs", 0 -> ()
  | _ -> failwith "empty transaction cursor accepted"
  end;
  let later = O.{ rejection with position = 2; tx = tx 3 } in
  let rejections = [rejection; later] in
  begin match execute ~rejections "" with
  | Ok ordered, 1 ->
    expect "multiple ordered rejections"
      (ordered.rejections = List.map O.encode_rejection rejections)
  | _ -> failwith "multiple rejections rejected"
  end;
  begin match execute ~rejections "order" with
  | Error "replay rejected order differs", 0 -> ()
  | _ -> failwith "rejection permutation accepted"
  end;
  Printf.printf "event = replay_checks status = pass cases = %d execution = sample preverify = sample\n"
    (List.length faults + 11)