(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module P = Circle_proposal
module C = Recovery_case
module S = C.SI
module D = C.SC
module L = Octra_core.Ledger
module X = Octra_core.Epoch_exec
module G = Octra_core.Rule_graph
module N = Octra_node_runtime
module T = Octra_core.Transaction
module A = N.Consensus_epoch_apply_commit
module F = N.Consensus_epoch_apply_finalize
module Gate = Octra_core.Preverify_commit
module J = N.Consensus_join_rpc
module Journal = N.Consensus_finality_journal
module H = Octra_consensus.C_hash
module CT = Octra_consensus.C_types

type sample = {
  head : C.HM.t;
  env : X.env;
  rules : G.t;
  parent : Octra_consensus.C_types.parent_commit;
  reward : N.Consensus_reward_attribution.t;
  txs : T.t list;
  prepared : N.Consensus_proposal_preview_shell.prepared;
}

exception Write_error

let ready_root _ = Lwt.fail_with "unexpected local history read"
let legacy ~epoch:_ ~address:_ ~cipher:_ = failwith "unexpected private replay"
let result_policy _ = Octra_core.Private_result_policy.Recoverable
let metadata store key = N.Node_rest_facade.run_s (S.get_meta store key)

let receipts sample = List.map (fun receipt ->
  Octra_core.Preverify_receipt.to_yojson receipt |> Yojson.Safe.to_string)
  sample.prepared.preverify.receipts

let seed ?(create = true) dir epoch = C.with_stores dir (fun chaindata store ->
  let identities = List.init 8 (fun i -> P.identity (i + 1))
    |> List.sort (fun a b -> String.compare a.P.address b.P.address) in
  let caller = List.nth identities 0 in
  let payer = List.nth identities 6 in
  let member = List.nth identities 7 in
  let validators = List.init 4 (fun i -> List.nth identities (i + 1)) in
  let proposer = List.hd validators in
  let ledger = L.create store in
  if create then begin
    List.iter (fun item -> P.unwrap (L.add_account ledger item.P.address
      (Z.of_int 10_000_000_000))) identities;
    P.unwrap (L.add_account ledger Octra_core.Validator_registry.escrow_address
      Octra_core.Validator_policy.min_bond);
    Lwt_main.run (L.flush_dirty_lwt ledger);
    List.iter (fun (key, value) -> Lwt_main.run (S.set_meta store key value)) [
      "last_epoch", string_of_int (epoch - 1); "current_epoch", string_of_int epoch;
      "total_supply", Z.to_string (L.get_total_supply ledger); "emission_remaining", "0";
    ];
    let registry = `Assoc [
      "standard", `String Octra_core.Validator_policy.standard_name;
      "candidates", `List [`Assoc [
        "address", `String member.address;
        "consensus_pubkey", `String (Base64.encode_exn member.public);
        "bond", `String (Z.to_string Octra_core.Validator_policy.min_bond);
        "bonded_epoch", `String "100";
        "ready_epoch", `Null; "exit_epoch", `Null;
      ]]; "slashes", `List [];
    ] |> Octra_core.Validator_registry.of_yojson |> P.unwrap in
    Lwt_main.run (S.set_meta store Octra_core.Validator_registry.meta_key
      (Octra_core.Validator_registry.to_string registry));
    ignore (P.deploy store caller.address)
  end;
  let circle = "oct" ^ String.make 44 '2' in
  let root = Lwt_main.run (L.hash ledger) in
  let epoch_index_hash, epoch_index_root = C.Eic.next_root
    ~prev:C.Eic.genesis_root ~epoch_id:(epoch - 1) [] in
  let state_root = C.Eic.folded_state_root ~ledger_state_root:root ~epoch_index_root in
  let parent = P.make_parent ~state_root:(P.root_raw state_root)
    (Int64.of_int (epoch - 1)) validators in
  let floor = Octra_core.History_floor.create ~chain_id:P.chain ~epoch:(epoch - 1)
    ~state_root ~ledger_state_root:root ~txid_hi:0L ~config_hash:P.config
    ~validator_set_hash:(N.Text.raw_to_hex
      (Octra_consensus.C_config.validator_set_hash parent.validator_set))
    ~epoch_index_hash ~epoch_index_root ~parent_commit:parent |> P.unwrap in
  if create then begin
    P.unwrap (D.seed_history_floor chaindata floor);
    Lwt_main.run (S.tag_epoch store (epoch - 1))
  end;
  let txlog_seg, txlog_off = D.txlog_position chaindata in
  let head = C.HM.{schema_version; generation = epoch - 1; epoch_id = epoch - 1;
    state_root; ledger_state_root = Some root;
    irmin_commit = Lwt_main.run (S.get_commit_hash store); txid_hi = 0L;
    txlog_seg = Some txlog_seg; txlog_off = Some txlog_off;
    epochlog_off = Some (D.epochlog_offset chaindata); commit_id = "circle-origin";
    ts = 1.; quorum_cert_hash = None;
    epoch_index_hash = Some epoch_index_hash; epoch_index_root = Some epoch_index_root} in
  if create then C.HM.atomic_write dir head;
  let rules = G.create_ready ~chain_id:P.chain ~ready_config_hash:P.config
    ~root_at:(fun at -> match G.root_after_floor ~chain_id:P.chain
      ~floor_epoch:epoch ~epoch:at with
      | Some value -> G.Root value | None -> G.Missing) in
  let env = X.{chain_id = P.chain; epoch_id = epoch; proposer_addr = proposer.address;
    validator_addrs = List.map (fun item -> item.P.address) validators;
    validator_pubkeys = List.map (fun item -> item.P.address, item.public) validators;
    prev_state_root = state_root; epoch_ts = 99.;
    ready_state_root_at = Some ready_root; ready_max_lag = 0} in
  let call = P.signed ~ou:19_000_000 caller ~op_type:T.CircleCall ~to_:circle ~nonce:1
    ~message:"[]" ~method_:(Some "accept") in
  let payment = P.signed payer ~op_type:T.Standard ~to_:proposer.address ~nonce:1
    ~message:"" ~method_:None in
  let height = string_of_int (epoch - 1) in
  let message = `Assoc [
    "consensus_pubkey", `String (Base64.encode_exn member.public);
    "head_epoch", `String height;
    "head_proposal_id", `String (N.Text.raw_to_hex parent.certificate.proposal_id);
    "state_root", `String state_root; "chain_id", `String P.chain;
    "config_hash", `String P.config; "catchup_head_epoch", `String height;
  ] |> Yojson.Safe.to_string in
  let ready = P.signed member ~op_type:T.ValidatorReady ~to_:member.address
    ~nonce:1 ~message ~method_:None in
  let txs = T.consensus_order [ready; payment; call] in
  let reward = P.unwrap (N.Consensus_reward_attribution.of_parent_commit parent) in
  let backend = N.Consensus_proposal_preview_shell.node_backend
    ~program_trust:Octra_vm.Program_trust.empty ~rules ~legacy_replay:legacy
    ~private_result_policy:result_policy ~max_fhe:1 ~max_stealth:1 store ledger in
  let prepared = Lwt_main.run (backend.prepare ~epoch_id:epoch
    ~proposal_id:"circle-store" ~expected_prev_root:(Some root)
    ~preverify:(Gate.create []) ~parent_commit:(Some parent) ~reward ~env ~txs)
    |> P.unwrap in
  P.expect "mixed preparation lost inputs"
    (List.map fst prepared.execution.artifacts.confirmed = txs
     && prepared.execution.artifacts.rejected = []);
  P.expect "mixed preparation changed storage" (Lwt_main.run (L.hash ledger) = root);
  {head; env; rules; parent; reward; txs; prepared})

let apply dir sample point mode = C.with_stores dir (fun chaindata store ->
  let open Lwt.Syntax in
  let env = sample.env in
  let epoch = env.epoch_id in
  let ledger = L.create store in
  let pending = ref [] and fees = ref Z.zero and processed = ref [] in
  let abort () = failwith "unexpected mixed epoch refusal" in
  C.HM.cached := Some sample.head;
  P.unwrap (L.begin_journal ledger);
  let account_mode = G.account_pack sample.rules ~epoch
    |> Result.map_error G.fault_message |> P.unwrap in
  Lwt_main.run (S.begin_epoch_batch ~mode:account_mode store);
  D.begin_batch chaindata;
  let preverify = Gate.gate_of_strings ~required:true (receipts sample)
    |> P.unwrap |> Option.get in
  Lwt_main.run @@
  let* applied = N.Consensus_epoch_apply_shared.run_node
    ~preverify ~parent_commit:sample.parent {
      ledger; store; chaindata; rules = sample.rules;
      program_trust = Octra_vm.Program_trust.empty;
      wallet_addr = env.proposer_addr; pre_state_hash = C.HM.ledger_state_root sample.head;
      standard_env = (fun () -> env); current_epoch = (fun () -> epoch);
      consensus_mode = true; max_fhe_per_epoch = 1; max_stealth_per_epoch = 1;
      max_stealth_defer = 0; stealth_inline_verify_allowed = true;
      fhe_in_epoch_counter = ref 0; stealth_in_epoch_counter = ref 0;
      stealth_defer_count = Hashtbl.create 1; pending_tx_saves = pending;
      total_tx_count = ref 0; confirmed_fees = fees; processed_hashes = processed;
      short = Fun.id; log_shared = (fun ~tx_count:_ -> ()); fatal = failwith; exit = abort;
      notify_new_account = (fun _ -> ()); notify_confirmed = (fun _ _ -> ());
      notify_rejected = (fun _ reason -> failwith reason);
      legacy_replay = legacy; private_result_policy = result_policy;
    } sample.txs in
  P.expect "mixed apply deferred work" (!(applied.deferred_stealth_txs) = []);
  P.expect "mixed apply lost inputs" (List.rev_map fst !pending = sample.txs);
  P.expect "mixed apply fees differ"
    (Z.equal !fees sample.prepared.execution.artifacts.confirmed_fees);
  let* finalized = F.run (F.node_effects {store; ledger; get_meta = metadata store}) {
    tree_ref = ref (Octra_core.Tree.create ~epoch_id:epoch
      ~parent_commit:(Option.get sample.head.irmin_commit));
    chain_id = env.chain_id; epoch_id = epoch; epoch_ts = env.epoch_ts;
    epoch_start = Unix.gettimeofday (); proposer_addr = env.proposer_addr;
    validator_addr = env.proposer_addr; validator_pubkeys = env.validator_pubkeys;
    active_validators = env.validator_addrs; reward = sample.reward;
    ready_state_root_at = ready_root; ready_max_lag = 0; confirmed_fees = !fees;
    confirmed_txs = sample.txs; deferred_count = 0; short = Fun.id;
  } in
  let _, index = C.Eic.next_root_from_hashes_i64
    ~prev:(Option.get sample.head.epoch_index_root) ~epoch_id:(Int64.of_int epoch)
    ~start_txid:1L (List.map T.hash sample.txs) in
  let expected = C.Eic.folded_state_root
    ~ledger_state_root:sample.prepared.execution.post_state_root ~epoch_index_root:index in
  let effects = A.node_effects {data_dir = dir; store; ledger; chaindata;
    finality_state = N.Consensus_finality_state.create ();
    irmin_last_epoch = (fun () -> epoch - 1);
    require_sync = (fun _ -> abort ()); exit = abort} in
  let commit = {effects.commit with
    chaos = (fun event ->
      if event = point then
        if mode = "kill" then begin
          Unix.kill (Unix.getpid ()) Sys.sigkill;
          Unix._exit 99
        end else if mode = "error" then raise Write_error);
    fsync_chaindata = (fun () ->
      if point = "fsync" then raise (Unix.Unix_error (Unix.EIO, "fsync", ""));
      effects.commit.fsync_chaindata ());
  } in
  let* result = A.run {effects with commit} {
    epoch_id = epoch; epoch_ts = env.epoch_ts; current_epoch = epoch + 1;
    consensus_mode = true; layera_diag = false; replay_trace = false;
    pre_state_hash = C.HM.ledger_state_root sample.head;
    pre_consensus_root = sample.head.state_root; expected_root = Some (P.root_raw expected);
    parent_commit = Option.get sample.head.irmin_commit; proposer_addr = env.proposer_addr;
    proposer_info = {creator_addr = env.proposer_addr; commit_round = 0};
    proposer_source = "local"; round = 0; validators = List.length env.validator_addrs;
    validators_sha = ""; ordered_txs_count = List.length sample.txs;
    confirmed_txs = sample.txs; confirmed_count = List.length sample.txs;
    confirmed_fees = !fees; plan = finalized.plan;
    prev_supply = finalized.reward_meta.prev_supply;
    emission_remaining = finalized.reward_meta.emission_remaining;
    reward_recipients = finalized.reward_recipients;
    reward_source = P.unwrap (N.Consensus_reward_attribution.to_source sample.reward);
    epoch_receipts_json = receipts sample; account_addrs = []; short = Fun.id;
    find_account = (fun _ -> None); progress = N.Consensus_epoch_commit.commit_progress ();
  } in
  P.expect "committed mixed root differs from preview"
    (result.post_state_hash = sample.prepared.execution.post_state_root
     && result.post_consensus_root = expected);
  Lwt.return_unit)

let inspect dir sample committed = C.with_stores dir (fun chaindata store ->
  let head = Option.get (C.HM.load dir) in
  let root = if committed then sample.prepared.execution.post_state_root
    else C.HM.ledger_state_root sample.head in
  P.expect "recovery mixed ledger differs" (Lwt_main.run (S.state_hash store) = root);
  P.expect "recovery mixed head differs"
    (head.epoch_id = sample.env.epoch_id - (if committed then 0 else 1)
     && C.HM.ledger_state_root head = root);
  P.expect "recovery mixed cursor differs"
    (head.txid_hi = (if committed then Int64.of_int (List.length sample.txs) else 0L)
     && D.next_txid chaindata = Int64.succ head.txid_hi);
  P.expect "recovery retained mixed WAL" (C.Wal.read_pending dir = []);
  P.expect "recovery retained mixed auxiliary journal"
    (D.get_meta chaindata Octra_core.Aux_index.pending_key = None);
  let epoch = sample.env.epoch_id in
  let record = D.get_epoch_header chaindata epoch in
  P.expect "recovery epoch log differs from index"
    (record = C.EL.get chaindata.epochlog epoch && Option.is_some record = committed);
  Option.iter (fun (record : C.EL.epoch_header) ->
    P.expect "recovery epoch range differs"
      (record.start_txid = 1L && record.tx_count = List.length sample.txs
       && record.state_root = head.state_root && record.prev_state_root = sample.head.state_root);
    P.expect "recovery preverify receipts differ"
      (Octra_core.Preverify_receipt_store.read dir ~epoch_id:epoch = Some (receipts sample))) record;
  List.iter (fun tx ->
    P.expect "recovery transaction visibility differs"
      (Option.is_some (D.get_tx_by_hash chaindata (T.hash tx)) = committed);
    let account = Option.get (Lwt_main.run (S.get_account store tx.T.from)) in
    P.expect "recovery transaction nonce differs" (account.nonce = if committed then 1 else 0);
    if tx.T.op_type = T.CircleCall then
      P.expect "recovery circle receipt visibility differs"
        (Option.is_some (D.get_contract_receipt chaindata ~tx_hash:(T.hash tx)) = committed)
  ) sample.txs;
  head)

let signed sample head =
  let header = CT.{
    proto_version = proto_version_current; chain_id = P.chain;
    epoch_id = Int64.of_int head.C.HM.epoch_id;
    prev_state_root = P.root_raw sample.head.state_root;
    tx_list_hash = N.Consensus_proposal.tx_list_hash (List.map T.hash sample.txs);
    receipt_root = H.receipt_root (receipts sample);
    proposed_state_root = P.root_raw head.state_root;
    parent_commit_hash = H.parent_commit_hash sample.parent;
    creator_addr = sample.env.proposer_addr; txid_hi = head.txid_hi; ts = sample.env.epoch_ts;
  } in
  let proposal_id = H.proposal_id header in
  let precommits = List.init 8 (fun i -> P.identity (i + 1))
    |> List.filter (fun item -> CT.pubkey_of_addr sample.parent.validator_set item.P.address <> None)
    |> List.map (fun item ->
      let vote = CT.{chain_id = P.chain; epoch_id = header.epoch_id; round = 0;
        vote_type = Precommit; proposal_id; validator = item.P.address; signature = ""} in
      {vote with signature = Mirage_crypto_ec.Ed25519.sign ~key:item.secret
        (H.vote_sign_bytes vote)}) in
  CT.{chain_id = P.chain; epoch_id = header.epoch_id; commit_round = 0; header;
    proposal_id; precommits; parent_commit = Some sample.parent}

let stage dir sample finalize =
  Journal.stage dir ~chain_id:P.chain ~validator_set:sample.parent.validator_set
    ~bundle:{tx_hashes = List.map T.hash sample.txs; txs = sample.txs;
      receipts_json = receipts sample} finalize |> ignore

let read_range dir sample = C.with_stores dir (fun chaindata _ ->
  let head = Option.get (C.HM.load dir) in
  C.HM.cached := Some head;
  let reader = N.Sync_range_read.create ~chaindata ~data_dir:dir ~chain_id:P.chain in
  Fun.protect ~finally:(fun () -> Lwt_main.run (N.Sync_range.shutdown reader)) (fun () ->
    let task = N.Sync_range.load reader {
      from_epoch = Int64.of_int sample.env.epoch_id; max_epochs = 1;
      part = None; hash = None; head = Some head;
      pubkeys = sample.env.validator_pubkeys; activation = None;
    } in
    P.expect "mixed range reader did not yield" (Lwt.is_sleeping task);
    match Lwt_main.run task with
    | Ok loaded ->
      P.expect "mixed range did not return committed epoch"
        (loaded.status = "ok" && loaded.records = 1);
      begin match J.parse_range ~from_epoch:(Int64.of_int sample.env.epoch_id)
        (Yojson.Safe.from_string loaded.body) with
      | J.Records [record] -> record
      | _ -> failwith "mixed range record missing"
      end
    | Error _ -> failwith "mixed range read failed"))

let replay epoch =
  Test_workspace.with_dir "circle_range" (fun source ->
    let sample = seed source epoch in
    apply source sample "complete" "success";
    let head = inspect source sample true in
    let finalize = signed sample head in
    stage source sample finalize;
    Journal.promote_applied source ~epoch:finalize.epoch_id
      ~state_root:finalize.header.proposed_state_root;
    let record = read_range source sample in
    let cursor = J.{epoch = Int64.of_int epoch; prev_root = sample.head.state_root;
      eic = Option.get sample.head.epoch_index_root; txid = 1L} in
    let verify = J.prepare_record ~chain_id:P.chain
      ~expected_validator_set_hash:(Octra_consensus.C_config.validator_set_hash
        sample.parent.validator_set) ~cursor in
    let prepared = verify record in
    P.expect "mixed range lost inputs" (prepared.txs = sample.txs);
    P.expect "mixed range lost receipts" (record.receipts_json = receipts sample);
    let votes = List.map (fun (vote : CT.vote) ->
      {vote with signature = String.make 64 '\000'}) record.finality.finalize.precommits in
    let invalid = {record with finality = {record.finality with
      finalize = {record.finality.finalize with precommits = votes}}} in
    let refuse reason value =
      let rejected = try ignore (verify value); None with Failure error -> Some error in
      P.expect ("mixed range refusal differs: " ^ reason)
        (Option.fold ~none:false ~some:(String.starts_with ~prefix:reason) rejected) in
    refuse "join finality qc is invalid" invalid;
    refuse "join receipt_root mismatch" {record with receipts_json = []};
    refuse "join tx hash mismatch" {record with txs_json = []};
    refuse "join root break" {record with prev_state_root = String.make 64 '0'};
    Test_workspace.with_dir "circle_replay" (fun target ->
      let local = seed target epoch in
      P.expect "mixed replay initial state differs"
        (local.head.state_root = sample.head.state_root);
      let gate = Gate.gate_of_strings ~required:true record.receipts_json
        |> P.unwrap |> Option.get in
      let imported = {local with txs = prepared.txs; reward = prepared.reward;
        parent = Option.get record.finality.finalize.parent_commit;
        env = {local.env with epoch_ts = record.epoch_ts; proposer_addr = record.creator_addr};
        prepared = {local.prepared with preverify = gate}} in
      stage target imported record.finality.finalize;
      apply target imported "complete" "success";
      let actual = inspect target imported true in
      P.expect "mixed range replay root differs from certificate"
        (actual.state_root = record.state_root && actual.txid_hi = head.txid_hi
         && actual.epoch_index_root = Some prepared.expected_eic);
      Journal.promote_applied target ~epoch:record.epoch_id
        ~state_root:(P.root_raw actual.state_root);
      C.with_stores target (fun chaindata store ->
        ignore (Lwt_main.run (C.Recovery.recover ~data_dir:target ~chaindata ~store)));
      P.expect "mixed range recovery changed state" (inspect target imported true = actual);
      P.expect "mixed range replay changed exported record" (read_range target imported = record)));
  Printf.printf "event = circle_range epoch = %d status = pass\n%!" epoch

let child () =
  try
    let dir = Sys.argv.(2) in
    let epoch = int_of_string Sys.argv.(3) in
    let sample = seed ~create:false dir epoch in
    apply dir sample Sys.argv.(4) Sys.argv.(5);
    exit 0
  with
  | Write_error | Unix.Unix_error (Unix.EIO, "fsync", "") -> exit 3
  | error ->
    Printf.eprintf "event = circle_store status = fail reason = %s\n%!"
      (Printexc.to_string error);
    exit 2

let run_case dir epoch point mode expected =
  let sample = seed dir epoch in
  flush_all ();
  let args = [|Sys.executable_name; "circle-store"; dir; string_of_int epoch; point; mode|] in
  let pid = Unix.create_process Sys.executable_name args Unix.stdin Unix.stdout Unix.stderr in
  let until = Unix.gettimeofday () +. 60. in
  let rec wait () =
    match Unix.waitpid [Unix.WNOHANG] pid with
    | 0, _ when Unix.gettimeofday () >= until ->
      Unix.kill pid Sys.sigkill;
      ignore (C.wait pid);
      failwith "mixed epoch process timeout"
    | 0, _ -> Unix.sleepf 0.01; wait ()
    | _, status -> status
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait () in
  let status = wait () in
  P.expect "mixed interruption differs"
    (status = match mode with
     | "kill" -> Unix.WSIGNALED Sys.sigkill
     | "error" -> Unix.WEXITED 3
     | _ -> Unix.WEXITED 0);
  let recover () = C.with_stores dir (fun chaindata store ->
    ignore (Lwt_main.run (C.Recovery.recover ~data_dir:dir ~chaindata ~store))) in
  recover ();
  let head = inspect dir sample expected in
  recover ();
  P.expect "mixed repeat recovery changed state" (inspect dir sample expected = head);
  Printf.printf "event = circle_store epoch = %d point = %s mode = %s status = pass\n%!"
    epoch point mode

let run () =
  let settings = [Octra_core.Validator_policy.env_name, "100";
    "OCTRA_BFT_RELEASE_PROFILE", "devnet_full_v1"] in
  let before = List.map (fun (name, _) ->
    name, Option.value (Sys.getenv_opt name) ~default:"") settings in
  Fun.protect ~finally:(fun () ->
    List.iter (fun (name, value) -> Unix.putenv name value) before;
    C.HM.cached := None) (fun () ->
    List.iter (fun (name, value) -> Unix.putenv name value) settings;
    List.iter (fun epoch ->
      List.iter (fun (point, mode, expected) ->
        Test_workspace.with_dir "circle_store" (fun dir ->
          run_case dir epoch point mode expected)) [
        "complete", "success", true;
        "after_wal", "kill", false;
        "after_chaindata_committed", "kill", false;
        "after_chaindata_committed", "error", false;
        "fsync", "error", false;
        "after_irmin_committed", "kill", true;
        "after_head_write", "kill", true;
      ];
      replay epoch) [1_614_499; 1_614_500])