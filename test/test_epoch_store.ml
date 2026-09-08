(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module R = Epoch_replay
module A = Epoch_store
module S = Octra_core.Store_irmin
module D = Octra_core.Store_chaindata
module L = Octra_core.Ledger
module C = Octra_consensus.C_types
module N = Octra_node_runtime

let chain_id = "epoch-store-fixture"
let proposer = "octEpochStoreFixture"
let expect = R.require
let reward = R.X.{
  proposer_addr = proposer;
  proposer_public_key = None;
  validators = [{ address = proposer; public_key = None; weight = Z.one }];
}

let fixture (cursor : R.J.cursor) state_root =
  let epoch_int = Int64.to_int cursor.epoch in
  let _, expected_eic = R.E.next_root_from_hashes_i64 ~prev:cursor.eic
    ~epoch_id:cursor.epoch ~start_txid:cursor.txid [] in
  let tx_list_hash = R.J.root_hex64 (Octra_consensus.C_hash.tx_list_hash []) in
  let receipt_root = R.J.root_hex64 (Octra_consensus.C_hash.receipt_root []) in
  let epoch_ts = float_of_int (epoch_int * 10) in
  let header, _ = N.Consensus_replay.parse_header ~default_chain_id:chain_id
    (`Assoc [
      "epoch_id", `Int epoch_int;
      "txid_hi", `Intlit (Int64.to_string (Int64.pred cursor.txid));
      "creator_addr", `String proposer;
      "prev_state_root", `String cursor.prev_root;
      "proposed_state_root", `String state_root;
      "tx_list_hash", `String tx_list_hash;
      "receipt_root", `String receipt_root;
      "ts", `Float epoch_ts;
    ]) in
  let plan = N.Consensus_replay.build_plan
    ~parent_commit:None ~header ~commit_round:0 ~txs:[] in
  let record = R.J.{
    epoch_id = cursor.epoch;
    prev_state_root = cursor.prev_root;
    state_root;
    tx_list_hash;
    tx_hashes = [];
    txs_json = [];
    receipts_json = [];
    receipt_root;
    epoch_ts;
    creator_addr = proposer;
    commit_round = 0;
    reward_source = R.get (N.Consensus_reward_attribution.to_source reward);
    finality = { finalize = plan.finalize; validator_set = C.make_validator_set [] };
  } in
  R.J.{ record; txs = []; expected_eic; epoch_int;
    proposer_info = plan.proposer_info; reward;
    next_cursor = { epoch = Int64.succ cursor.epoch; prev_root = state_root;
      eic = expected_eic; txid = cursor.txid } }

let epoch (context : A.context) (cursor : R.J.cursor) =
  let provisional = fixture cursor cursor.prev_root in
  let deps = A.deps context ~cursor ~prepared:provisional in
  let* batch = deps.preverify [] in
  expect "empty preverify produced entries" (batch.ready = [] && batch.skipped = []);
  let gate = R.G.create (R.W.receipts_for_hashes batch.ready []) in
  let* preview = deps.preview gate [] in
  let preview = R.get preview in
  let state_root = R.E.folded_state_root ~ledger_state_root:preview.post_state_root
    ~epoch_index_root:provisional.expected_eic in
  let prepared = fixture cursor state_root in
  let* trace = R.run (A.deps context ~cursor ~prepared) ~cursor ~prepared in
  expect "empty epoch artifacts differ"
    (trace.confirmed = [] && trace.rejections = [] && Z.equal trace.fees Z.zero);
  let* account = S.get_account context.store proposer in
  expect "inactive emission created a reward account" (account = None);
  let* supply = S.get_meta context.store "total_supply" in
  let* pool = S.get_meta context.store "emission_remaining" in
  expect "reward supply metadata differs"
    (supply = Some "0" && pool = Some "30000");
  let* last = S.get_meta context.store "last_epoch" in
  let* next = S.get_meta context.store "current_epoch" in
  expect "epoch metadata differs"
    (last = Some (Int64.to_string cursor.epoch)
     && next = Some (Int64.to_string (Int64.succ cursor.epoch)));
  Lwt.return (prepared.next_cursor, trace)

let with_stores ?(readonly = false) path f =
  let* store = S.open_store ~readonly (Filename.concat path "irmin_store") in
  Lwt.finalize (fun () ->
    let chaindata = D.open_chaindata ~readonly (Filename.concat path "chaindata") in
    Lwt.finalize (fun () -> f store chaindata)
      (fun () -> D.close chaindata; Lwt.return_unit)
  ) (fun () -> S.close store)

let run path =
  let* traces = with_stores path (fun store chaindata ->
    let* () = S.begin_epoch_batch store in
    let* () = Lwt_list.iter_s (fun (key, value) -> S.set_meta store key value)
      ["total_supply", "0"; "emission_remaining", "30000";
       "last_epoch", "0"; "current_epoch", "1"] in
    let* () = S.commit_epoch_batch store "epoch store fixture" in
    let context = A.{
      store; chaindata; ledger = L.create store; chain_id;
      trust = Octra_vm.Program_trust.empty;
      rules = Octra_core.Rule_graph.create ~chain_id
        ~root_at:(fun _ -> Octra_core.Rule_graph.Missing);
      ready_root = (fun _ -> Lwt.return_none);
      legacy_replay = (fun ~epoch:_ ~address:_ ~cipher:_ ->
        failwith "empty epoch requested private replay");
      result_policy = (fun _ -> Octra_core.Private_result_policy.Recoverable);
    } in
    let* initial = S.state_hash store in
    let cursor = R.J.{ epoch = 1L; txid = 1L; eic = R.E.genesis_root;
      prev_root = R.E.folded_state_root ~ledger_state_root:initial
        ~epoch_index_root:R.E.genesis_root } in
    let* cursor, first = epoch context cursor in
    let* cursor, second = epoch context cursor in
    expect "consecutive empty epochs did not advance"
      (cursor.epoch = 3L && cursor.txid = 1L
       && first.ledger_root <> initial && second.ledger_root <> first.ledger_root
       && first.index_root <> second.index_root);
    Lwt.return [first; second]) in
  let* () = with_stores ~readonly:true path (fun store chaindata ->
    let* actual = S.state_hash store in
    expect "reopened ledger root differs" (actual = (List.nth traces 1).ledger_root);
    List.iter (fun (trace : R.trace) ->
      let hash, root = D.get_epoch_index_commitment chaindata (Int64.to_int trace.epoch) in
      expect "reopened epoch index differs"
        (Option.is_some hash && root = Some trace.index_root)) traces;
    Lwt.return_unit) in
  List.iter (fun (trace : R.trace) ->
    Printf.printf
      "event = epoch_store auth = fixture execution = production epoch = %Ld ledger_root = %s index_root = %s state_root = %s\n"
      trace.epoch trace.ledger_root trace.index_root trace.state_root) traces;
  Lwt.return_unit

let rec remove path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | stat when stat.Unix.st_kind = Unix.S_DIR ->
    Array.iter (fun name -> remove (Filename.concat path name)) (Sys.readdir path);
    Unix.rmdir path
  | _ -> Unix.unlink path

let rec node_root path =
  if Sys.file_exists (Filename.concat path "lib/core/store_irmin.ml") then path
  else
    let parent = Filename.dirname path in
    expect "node source root is missing" (parent <> path);
    node_root parent

let rec with_env values f =
  match values with
  | [] -> f ()
  | (name, value) :: rest ->
    let previous = Sys.getenv_opt name in
    Unix.putenv name value;
    Fun.protect
      ~finally:(fun () -> Unix.putenv name (Option.value ~default:"" previous))
      (fun () -> with_env rest f)

let () =
  let data = Filename.concat (node_root (Sys.getcwd ())) "runtime_data" in
  if not (Sys.file_exists data) then Unix.mkdir data 0o700;
  let path = Filename.concat data (Printf.sprintf "epoch-store-%d" (Unix.getpid ())) in
  with_env ["OCTRA_EMISSION_GUARD", "0"; "OCTRA_EMISSION_PROFILE", "";
    "OCTRA_EMISSION_ACTIVATION_EPOCH", "";
    "OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH", "";
    "OCTRA_PRIVATE_RESULT_ACTIVATION_EPOCH", "0"] (fun () ->
    Unix.mkdir path 0o700;
    Lwt_main.run (Lwt.finalize (fun () -> run path)
      (fun () -> remove path; Lwt.return_unit)));
  Printf.printf "event = epoch_store status = pass scope = adapter_control auth = fixture execution = production epochs = 2\n"