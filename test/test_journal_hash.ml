(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Journal = Octra_node_runtime.Consensus_finality_journal
module Cache = Octra_node_runtime.Consensus_bundle_cache
module Types = Octra_consensus.C_types
module Codec = Octra_consensus.C_codec
module Hash = Octra_consensus.C_hash
module Tx = Octra_core.Transaction
module Need = Octra_node_runtime.Sync_need
module Mark = Octra_node_runtime.Sync_mark
module Log = Octra_consensus.Finality_log

let expect name value = if not value then failwith name

let read_file path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in channel) (fun () ->
    let size = in_channel_length channel in
    if size > 16 * 1024 * 1024 then failwith "record size";
    really_input_string channel size)

let write_file path raw =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out channel) (fun () ->
    output_string channel raw)

let check_cache bundle =
  let cache = Cache.create ~cap:1 in
  ignore (Cache.store cache ~pid:"journal"
    ~tx_hashes:bundle.Journal.tx_hashes ~txs:bundle.txs
    ~receipts_json:bundle.receipts_json);
  match Cache.cached cache "journal" with
  | Cache.Cached decoded ->
    expect "recovered transaction hashes"
      (decoded.tx_hashes = List.map Tx.hash bundle.txs)
  | Cache.Missing -> failwith "recovered bundle missing"
  | Cache.Decode_error reason -> failwith reason

let check_record base chain validator_set =
  match Journal.read_validated ~chain_id:chain ~validator_set base with
  | Journal.Valid { bundle = Some bundle; _ } -> check_cache bundle
  | Journal.Invalid reason -> failwith reason
  | _ -> failwith "record unavailable"

let check_finish base validator_set (cert : Types.finalize) =
  let chain = cert.chain_id in
  let head = Int64.to_int cert.epoch_id in
  let root = cert.header.proposed_state_root in
  let need = Need.journal ~epoch:head ~head:(head - 1) in
  let entry = Log.of_finalize cert in
  let finish root =
    let checked =
      Journal.attested_root ~chain_id:chain ~validator_set ~head ~root ~entry base
    in
    Result.bind checked (fun _ ->
      Mark.consume_journal ~data_dir:base ~chain ~verified_head:head need)
  in
  expect "recovery marker stored"
    (Mark.write ~data_dir:base ~chain need = Ok Mark.Stored);
  expect "pending journal cannot finish" (Result.is_error (finish root));
  expect "pending marker preserved"
    (Mark.read ~data_dir:base ~chain = Mark.Ready need);
  Journal.promote_applied base ~epoch:cert.epoch_id ~state_root:root;
  Log.write base entry;
  expect "different root cannot finish"
    (Result.is_error (finish (String.make 32 '\000')));
  expect "root refusal preserves marker"
    (Mark.read ~data_dir:base ~chain = Mark.Ready need);
  expect "verified recovery finishes" (finish root = Ok ());
  expect "restart has no recovery plan"
    (Mark.need (Mark.read ~data_dir:base ~chain) = Ok None);
  expect "restart verifies committed head" (finish root = Ok ())

let check_conflict validator_set (cert : Types.finalize) changed =
  let base = Test_workspace.unique_dir "journal-conflict" in
  Journal.persist_certificate base ~validator_set cert;
  expect "first certificate verifies"
    (match Journal.read_validated ~chain_id:cert.chain_id ~validator_set base with
     | Journal.Valid _ -> true | _ -> false);
  let second = Test_workspace.unique_dir "journal-second" in
  Journal.persist_certificate second ~validator_set changed;
  expect "second certificate verifies"
    (match Journal.read_validated ~chain_id:cert.chain_id ~validator_set second with
     | Journal.Valid _ -> true | _ -> false);
  let pending = Filename.concat base "finality/pending_finalized.json" in
  let before = read_file pending in
  let refused =
    try Journal.persist_certificate base ~validator_set changed; false
    with Failure reason -> reason = "conflicting finality journal certificate"
  in
  expect "conflict refused" refused;
  expect "prior journal unchanged" (read_file pending = before);
  let need = Need.conflict ~epoch:12 ~head:11 in
  expect "conflict marker retained"
    (Mark.read ~data_dir:base ~chain:cert.chain_id = Mark.Ready need);
  let file = Filename.concat base "finality/conflict.json" in
  let evidence = read_file file in
  let open Yojson.Safe.Util in
  let decoded = Yojson.Safe.from_string evidence in
  let encoded value = Codec.encode_finalize value |> Base64.encode_exn in
  expect "incoming certificate retained"
    (decoded |> member "incoming" |> to_string = encoded changed);
  expect "prior certificate retained"
    (decoded |> member "pending" |> member "finalize" |> to_string
     = encoded cert);
  (try Journal.persist_certificate base ~validator_set changed with Failure _ -> ());
  expect "first evidence is immutable" (read_file file = evidence);
  expect "local attestation cannot consume conflict"
    (Result.is_error
       (Mark.consume_journal ~data_dir:base ~chain:cert.chain_id ~verified_head:20 need));
  let base = Test_workspace.unique_dir "journal-log" in
  expect "log conflict is recorded"
    (try
       Journal.guard base changed (fun () ->
         failwith "conflicting finality at committed height")
     with Failure reason -> reason = "conflicting finality at committed height");
  expect "log conflict keeps certificate"
    (Sys.file_exists (Filename.concat base "finality/conflict.json"));
  let base = Test_workspace.unique_dir "journal-write" in
  Journal.persist_certificate base ~validator_set cert;
  Unix.mkdir (Filename.concat base "finality/conflict.json") 0o750;
  (try Journal.persist_certificate base ~validator_set changed with Failure _ -> ());
  expect "failed evidence write still holds"
    (Mark.read ~data_dir:base ~chain:cert.chain_id = Mark.Ready need)

let check_pending validator_set (cert : Types.finalize) next bundle =
  let base = Test_workspace.unique_dir "journal-pending" in
  Journal.persist_certificate base ~validator_set cert;
  Journal.persist_bundle base cert bundle;
  Log.write base (Log.of_finalize cert);
  let before = read_file (Filename.concat base "finality/pending_finalized.json") in
  let rejected =
    try Journal.persist_certificate base ~validator_set next; false
    with Failure reason -> Journal.classify_conflict (Failure reason) = None
  in
  expect "different height is pending work" rejected;
  expect "pending height does not mark conflict"
    (Mark.read ~data_dir:base ~chain:cert.chain_id = Mark.Missing);
  expect "pending record preserved"
    (read_file (Filename.concat base "finality/pending_finalized.json") = before);
  Journal.promote_applied base ~epoch:cert.epoch_id
    ~state_root:cert.header.proposed_state_root;
  Journal.persist_certificate base ~validator_set next;
  expect "next height starts after promotion"
    (Journal.read_pending_epoch base = Ok (Some next.epoch_id));
  expect "normal restart has no conflict evidence"
    (not (Sys.file_exists (Filename.concat base "finality/conflict.json")))

let check_quorum validator_set (cert : Types.finalize) later bundle =
  let first = { cert with precommits = List.tl cert.precommits } in
  let other = { cert with precommits = List.rev cert.precommits |> List.tl } in
  let base = Test_workspace.unique_dir "journal-quorum" in
  Journal.persist_certificate base ~validator_set first;
  Journal.persist_bundle base first bundle;
  check_record base cert.chain_id validator_set;
  Journal.promote base;
  Journal.persist_certificate base ~validator_set other;
  Journal.persist_bundle base other bundle;
  check_record base cert.chain_id validator_set;
  Journal.promote base;
  expect "equivalent quorum seed"
    (Journal.seed ~chain_id:cert.chain_id ~validator_set ~finalize:first base
     = Ok Journal.Seed_current);
  expect "equivalent quorum has no marker"
    (Mark.read ~data_dir:base ~chain:cert.chain_id = Mark.Missing);
  let base = Test_workspace.unique_dir "journal-seeded" in
  expect "compact checkpoint seeded"
    (Journal.seed ~chain_id:cert.chain_id ~validator_set ~finalize:first base
     = Ok Journal.Seeded);
  Journal.persist_certificate base ~validator_set other;
  Journal.persist_bundle base other bundle;
  Journal.promote base;
  expect "full record completes compact checkpoint"
    (Mark.read ~data_dir:base ~chain:cert.chain_id = Mark.Missing);
  expect "later round seed proves same block"
    (Journal.seed ~chain_id:cert.chain_id ~validator_set ~finalize:later base
     = Ok Journal.Seed_current);
  Journal.persist_certificate base ~validator_set later;
  Journal.persist_bundle base later bundle;
  check_record base cert.chain_id validator_set;
  Journal.promote base;
  expect "later round has no conflict"
    (Mark.read ~data_dir:base ~chain:cert.chain_id = Mark.Missing)

let check_seed_faults validator_set (cert : Types.finalize) changed =
  let seed base finalize =
    Journal.seed ~chain_id:cert.chain_id ~validator_set ~finalize base in
  let base = Test_workspace.unique_dir "seed-pending" in
  Journal.persist_certificate base ~validator_set cert;
  expect "pending seed is not conflict" (seed base cert = Error Journal.Seed_pending);
  expect "pending seed has no marker"
    (Mark.read ~data_dir:base ~chain:cert.chain_id = Mark.Missing);
  let base = Test_workspace.unique_dir "seed-invalid" in
  Unix.mkdir (Filename.concat base "finality") 0o750;
  write_file (Filename.concat base "finality/committed_finalized.json") "invalid";
  expect "invalid seed has distinct fault"
    (match seed base cert with Error (Journal.Seed_invalid _) -> true | _ -> false);
  expect "invalid seed is not contradiction"
    (Mark.read ~data_dir:base ~chain:cert.chain_id = Mark.Missing);
  let base = Test_workspace.unique_path "seed-io" in
  expect "write failure has distinct fault"
    (match seed base cert with Error (Journal.Seed_io _) -> true | _ -> false);
  expect "write failure has no conflict marker"
    (Mark.read ~data_dir:base ~chain:cert.chain_id = Mark.Missing);
  let base = Test_workspace.unique_dir "seed-conflict" in
  expect "seed control passes" (seed base cert = Ok Journal.Seeded);
  expect "same height contradiction retained"
    (match seed base changed with Error (Journal.Seed_conflict _) -> true | _ -> false);
  expect "seed conflict marker saved"
    (match Mark.read ~data_dir:base ~chain:cert.chain_id with
     | Mark.Ready need -> need.cause = Need.Conflict | _ -> false)

let check_seed_log (cert : Types.finalize) =
  let module Sync = Octra_node_runtime.Sync_finality in
  let base = Test_workspace.unique_dir "seed-log" in
  let entry = Log.of_finalize cert in
  let prior = { entry with proposal_id = ""; creator_addr = "";
    txid_hi = -1L; qc_hash = None } in
  Log.write base prior;
  expect "legacy log accepts verified completion"
    (Sync.preflight_log base cert = Ok false);
  expect "legacy completion is not conflict"
    (Mark.read ~data_dir:base ~chain:cert.chain_id = Mark.Missing);
  Log.write base entry;
  expect "legacy log is completed" (Log.last_entry_fast base = Some entry);
  expect "completed log is current" (Sync.preflight_log base cert = Ok true);
  let base = Test_workspace.unique_dir "seed-log-root" in
  Log.write base { prior with state_root = "different" };
  expect "different legacy root remains conflict"
    (match Sync.preflight_log base cert with Error (Sync.Conflict _) -> true | _ -> false);
  expect "different root leaves marker"
    (match Mark.read ~data_dir:base ~chain:cert.chain_id with
     | Mark.Ready need -> need.cause = Need.Conflict | _ -> false)

let check_join_gate validator_set (cert : Types.finalize) bundle empty =
  let base = Test_workspace.unique_dir "join-gate" in
  let set_hash = Octra_consensus.C_config.validator_set_hash validator_set in
  let root = cert.header.proposed_state_root in
  let txid = cert.header.txid_hi in
  Journal.persist_certificate base ~validator_set cert;
  Journal.persist_bundle base cert bundle;
  List.iter (fun (chain, hash, head, state, txid) ->
    expect "inconsistent pending is refused"
      (try Journal.resume_join ~chain_id:chain ~set_hash:(fun _ -> Ok hash)
        ~head ~root:state ~txid base; false with Failure _ -> true);
    expect "refusal retains pending" (Journal.pending base);
    expect "refusal does not invent conflict"
      (Mark.read ~data_dir:base ~chain:cert.chain_id = Mark.Missing)
  ) [
    "other", set_hash, 12, root, txid;
    cert.chain_id, "other", 12, root, txid;
    cert.chain_id, set_hash, 12, String.make 32 'x', txid;
    cert.chain_id, set_hash, 12, root, Int64.succ txid;
    cert.chain_id, set_hash, 14, root, txid;
    cert.chain_id, set_hash, 11, root, txid;
  ];
  let resume base cert = Journal.resume_join ~chain_id:cert.Types.chain_id
    ~set_hash:(fun _ -> Ok set_hash) ~head:12
    ~root:cert.header.proposed_state_root ~txid:cert.header.txid_hi base in
  resume base cert;
  expect "verified head completes pending" (not (Journal.pending base));
  let base = Test_workspace.unique_dir "join-no-bundle" in
  Journal.persist_certificate base ~validator_set cert;
  expect "nonempty bundle is required"
    (try resume base cert; false with Failure _ -> true);
  expect "missing bundle remains pending" (Journal.pending base);
  let base = Test_workspace.unique_dir "join-empty" in
  Journal.persist_certificate base ~validator_set empty;
  resume base empty;
  expect "empty bundle is reconstructed" (not (Journal.pending base))

let check_rebind validator_set (cert : Types.finalize) (later : Types.finalize) bundle =
  let legacy = Types.make_validator_set
    (validator_set.Types.validators |> List.rev |> List.tl |> List.rev) in
  List.iter (fun original ->
    let base = Test_workspace.unique_dir "journal-rebind" in
    let entry = Log.of_finalize original in
    let current = Filename.concat base "finality/committed_finalized.json" in
    let history = Filename.concat base "finality/committed/12.json" in
    Journal.persist_certificate base ~validator_set:legacy original;
    Journal.persist_bundle base original bundle;
    Log.write base entry;
    Journal.promote base;
    let old = read_file history in
    let old_current = read_file current in
    let rebind () = Journal.rebind_committed ~chain_id:cert.chain_id
      ~validator_set ~entry base in
    expect "rebind succeeds" (rebind () = Ok Journal.Rebound);
    let repaired = read_file current in
    write_file history old;
    expect "rebind retry finishes interrupted copy" (rebind () = Ok Journal.Unchanged);
    expect "history receives complete checked record"
      (match Journal.read_history_epoch_validated ~chain_id:cert.chain_id
        ~validator_set ~epoch:12L base with
       | Journal.Valid record -> Codec.encode_finalize record.finalize = Codec.encode_finalize original
       | _ -> false);
    write_file current old_current;
    expect "rebind finishes history-first copy" (rebind () = Ok Journal.Rebound);
    write_file history old;
    expect "seed finishes interrupted copy"
      (Journal.seed ~chain_id:cert.chain_id ~validator_set ~finalize:original base
       = Ok Journal.Seeded);
    expect "repair leaves current proof unchanged" (read_file current = repaired);
    let invalid = { original with precommits = [] } in
    let json = Yojson.Safe.from_string old in
    let changed = match json with
      | `Assoc fields -> `Assoc (List.map (fun (name, value) ->
          name, if name = "finalize" then `String (Base64.encode_exn (Codec.encode_finalize invalid))
          else value) fields)
      | _ -> failwith "history record" in
    write_file history (Yojson.Safe.to_string changed);
    expect "invalid different certificate stays refused" (Result.is_error (rebind ()));
    expect "invalid history blocks seed"
      (match Journal.seed ~chain_id:cert.chain_id ~validator_set ~finalize:original base with
       | Error (Journal.Seed_invalid _) -> true | _ -> false);
    expect "refusal preserves current" (read_file current = repaired))
    [cert; { cert with precommits = List.rev cert.precommits |> List.tl }];
  let base = Test_workspace.unique_dir "journal-round-copy" in
  Journal.persist_certificate base ~validator_set:legacy
    { later with precommits = List.rev later.precommits |> List.tl };
  Journal.persist_bundle base later bundle;
  Journal.promote base;
  let donor = Test_workspace.unique_dir "journal-round-source" in
  Journal.persist_certificate donor ~validator_set:legacy
    { cert with precommits = List.rev cert.precommits |> List.tl };
  Journal.persist_bundle donor cert bundle;
  Journal.promote donor;
  write_file (Filename.concat base "finality/committed_finalized.json")
    (read_file (Filename.concat donor "finality/committed_finalized.json"));
  let selected = { cert with precommits = List.rev cert.precommits |> List.tl } in
  expect "history may carry another valid round"
    (Journal.rebind_committed ~chain_id:cert.chain_id ~validator_set
      ~entry:(Log.of_finalize selected) base = Ok Journal.Rebound);
  expect "history round and set are replaced together"
    (match Journal.read_history_epoch_validated ~chain_id:cert.chain_id
      ~validator_set ~epoch:12L base with
     | Journal.Valid record -> record.finalize.commit_round = selected.commit_round
     | _ -> false);
  List.iter (fun repair ->
    let base = Test_workspace.unique_dir "journal-other-quorum" in
    let current = { cert with precommits = List.rev cert.precommits |> List.tl } in
    let other = { cert with precommits = List.filteri (fun i _ -> i <> 2) cert.precommits } in
    let donor = Test_workspace.unique_dir "journal-other-source" in
    Journal.persist_certificate donor ~validator_set:legacy other;
    Journal.persist_certificate base ~validator_set:legacy current;
    Journal.persist_bundle base current bundle;
    Log.write base (Log.of_finalize current);
    Journal.promote base;
    write_file (Filename.concat base "finality/committed/12.json")
      (read_file (Filename.concat donor "finality/pending_finalized.json"));
    let repaired = if repair then
      Journal.repair_committed ~chain_id:cert.chain_id ~validator_set
        ~entry:(Log.of_finalize current) ~finalize:cert base = Ok Journal.Proof_repaired
    else Journal.rebind_committed ~chain_id:cert.chain_id ~validator_set
      ~entry:(Log.of_finalize current) base = Ok Journal.Rebound in
    expect "different checked quorum permits repair" repaired;
    expect "other quorum repair completes history"
      (match Journal.read_history_epoch_validated ~chain_id:cert.chain_id
        ~validator_set ~epoch:12L base with
       | Journal.Valid _ -> true | _ -> false)) [false; true]

let check_stage_order validator_set (cert : Types.finalize) changed bundle =
  let base = Test_workspace.unique_dir "stage-order" in
  Log.write base (Log.of_finalize cert);
  expect "log disagreement refuses staging"
    (try ignore (Journal.stage base ~chain_id:cert.chain_id ~validator_set ~bundle changed);
      false with Failure _ -> true);
  expect "refused input never becomes pending" (not (Journal.pending base));
  let evidence = read_file (Filename.concat base "finality/conflict.json")
    |> Yojson.Safe.from_string in
  expect "evidence preserves absent prior pending"
    (Yojson.Safe.Util.member "pending" evidence = `Null);
  let base = Test_workspace.unique_dir "stage-invalid" in
  let invalid = { cert with precommits = [] } in
  expect "invalid input cannot stage"
    (try ignore (Journal.prepare base ~chain_id:cert.chain_id ~validator_set invalid);
      false with Failure _ -> true);
  expect "invalid input leaves no pending or conflict"
    (not (Journal.pending base)
     && Mark.read ~data_dir:base ~chain:cert.chain_id = Mark.Missing)

let check_live_round ?(reverse = false) validator_set (cert : Types.finalize) later bundle =
  let module R = Octra_node_runtime.Consensus_finality_runtime in
  let module S = Octra_node_runtime.Consensus_finalized_shell in
  let module State = Octra_node_runtime.Consensus_finality_state in
  let base = Test_workspace.unique_dir "live-round" in
  Journal.persist_certificate base ~validator_set cert;
  Journal.persist_bundle base cert bundle;
  Log.write base (Log.of_finalize cert);
  let state = State.create () in
  let finality = State.callbacks state in
  let epoch = Int64.to_int cert.epoch_id in
  let head = ref (epoch - 1) in
  let pause, resume = Lwt.wait () in
  let cache = Cache.create ~cap:4 in
  ignore (Cache.store cache ~pid:cert.proposal_id ~tx_hashes:(List.map Tx.hash bundle.txs)
    ~txs:bundle.txs ~receipts_json:bundle.receipts_json);
  let runtime = R.create_node (R.node_deps_of_runtime R.{
    data_dir = base;
    bundles = Cache.node_runtime cache;
    driver_ref = ref None;
    proposal_state = Octra_node_runtime.Consensus_proposal_state.create ();
    catchup_queue = Octra_node_runtime.Consensus_catchup_queue.create ();
    consensus_finalized = ref false;
    current_epoch = ref epoch;
    committed_head_epoch = (fun () -> !head);
    sleep = (fun _ -> if !head < epoch then pause else Lwt.return_unit);
    read_pre_finalize_root = (fun () -> Some cert.header.prev_state_root);
    read_commit_root = (fun () -> Lwt.return (Some cert.header.proposed_state_root));
    read_local_root_raw = (fun () -> Lwt.return cert.header.proposed_state_root);
    apply_timeout_seconds = 1.;
    require_sync = (fun _ -> failwith "unexpected live sync");
    fatal_exit = (fun () -> failwith "unexpected live exit");
    catchup_active = ref false;
    runtime_state = Octra_node_runtime.Consensus_runtime_state.create ();
    set_state_attested = (fun ~head:_ ~root:_ -> ());
    finality;
  }) in
  let deps = S.{
    prune_frozen = (fun ~finalized_epoch:_ -> ());
    store_expected_root = finality.store_expected_root;
    store_finalized = finality.store_finalized_with_set;
    remove_finalized = finality.remove_finalized;
    remove_proposer = finality.remove_proposer;
    committed_head_epoch = (fun () -> !head);
    driver = (fun () -> None);
    observer_mode = false;
    catchup_active = (fun () -> false);
    quarantine_active = (fun () -> false);
    state_attested = (fun () -> true);
    current_epoch = (fun () -> epoch);
    read_local_root_raw = (fun () -> Lwt.return cert.header.prev_state_root);
    queue_catchup_target = (fun ~target_epoch:_ ~reason:_ -> failwith "unexpected live queue");
    run_catchup_to_target = (fun _ ~target_epoch:_ ~reason:_ -> failwith "unexpected live catchup");
    mark_quarantine = (fun _ -> failwith "unexpected live quarantine");
    clear_quarantine = (fun _ -> ());
    apply_finalized = runtime.apply_finalized;
    replay_stashed_while_safe = runtime.replay_stashed_while_safe;
  } in
  let first = S.handle deps ~validator_set (if reverse then later else cert) in
  expect "live apply is pending" (Lwt.state first = Lwt.Sleep);
  let second = S.handle deps ~validator_set (if reverse then cert else later) in
  let round () = Option.map (fun value -> value.Octra_core.Epochlog.commit_round)
    (State.find_proposer state epoch) in
  expect "later notice keeps selected round" (round () = Some cert.commit_round);
  head := epoch;
  Lwt.wakeup resume ();
  Lwt_main.run (Lwt.join [first; second]);
  let saved = match Journal.read_committed_epoch ~chain_id:cert.chain_id ~epoch:cert.epoch_id base with
    | Journal.Valid record -> record
    | _ -> failwith "live certificate missing" in
  let record = Codec.{
    epoch_id = cert.epoch_id;
    prev_state_root = cert.header.prev_state_root;
    state_root = cert.header.proposed_state_root;
    tx_list_hash = cert.header.tx_list_hash;
    tx_hashes = List.map Tx.hash bundle.txs;
    txs_json = List.map (fun tx -> Yojson.Safe.to_string (Tx.to_yojson tx)) bundle.txs;
    receipts_json = bundle.receipts_json;
    receipt_root = cert.header.receipt_root;
    epoch_ts = cert.header.ts;
    creator_addr = cert.header.creator_addr;
    commit_round = Option.get (round ());
    reward_source = None;
    finality = Some { validator_set; finalize = saved.finalize };
  } in
  expect "live history round matches certificate"
    (Result.is_ok (Octra_consensus.C_catchup.verify_record_finality
      ~chain_id:cert.chain_id
      ~expected_validator_set_hash:(Octra_consensus.C_config.validator_set_hash validator_set)
      ~expected_txid:(Int64.succ cert.header.txid_hi) ~record))

let check_hashes () =
  Mirage_crypto_rng_unix.use_default ();
  let keys = List.init 4 (fun i ->
    let key, pub = Mirage_crypto_ec.Ed25519.generate () in
    Types.{ address = "member" ^ string_of_int i;
      pubkey = Mirage_crypto_ec.Ed25519.pub_to_octets pub }, key) in
  let validator_set = Types.make_validator_set (List.map fst keys) in
  let tx = Tx.{ from = "sender"; to_ = "receiver"; amount = Z.one;
    nonce = 1; ou = Z.of_int 1_000; timestamp = 1.; signature = "signature";
    public_key = None; message = None; op_type = Standard; encrypted_data = None } in
  let hash = Tx.hash tx in
  let raw = Digestif.SHA256.(to_raw_string (of_hex hash)) in
  let header = Types.{
    proto_version = proto_version_current; chain_id = "journal-hash"; epoch_id = 12L;
    prev_state_root = String.make 32 'a';
    tx_list_hash = Octra_consensus.C_engine.tx_list_hash_for_header [hash];
    receipt_root = Hash.receipt_root []; proposed_state_root = String.make 32 'b';
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = "member0"; txid_hi = 1L; ts = 1.;
  } in
  let signed ?(round = 0) (header : Types.epoch_header) =
    let proposal_id = Hash.proposal_id header in
    let votes = List.map (fun (validator, key) ->
      let vote = Types.{ chain_id = header.chain_id; epoch_id = header.epoch_id; round;
        vote_type = Precommit; proposal_id; validator = validator.address; signature = "" } in
      { vote with signature = Mirage_crypto_ec.Ed25519.sign ~key (Hash.vote_sign_bytes vote) }) keys in
    Types.{ chain_id = header.chain_id; epoch_id = header.epoch_id; commit_round = round;
      header; proposal_id; precommits = votes; parent_commit = None }
  in
  let finalize = signed header in
  let bundle = Journal.{ tx_hashes = [raw]; txs = [tx]; receipts_json = [] } in
  check_rebind validator_set finalize (signed ~round:1 header) bundle;
  check_stage_order validator_set finalize
    (signed { header with proposed_state_root = String.make 32 'c' }) bundle;
  check_live_round validator_set finalize (signed ~round:1 header) bundle;
  check_live_round ~reverse:true validator_set finalize (signed ~round:1 header) bundle;
  check_pending validator_set finalize
    (signed { header with epoch_id = 13L; prev_state_root = header.proposed_state_root }) bundle;
  check_quorum validator_set finalize (signed ~round:1 header) bundle;
  check_seed_log finalize;
  check_join_gate validator_set finalize bundle
    (signed { header with tx_list_hash = Octra_consensus.C_engine.tx_list_hash_for_header [] });
  check_seed_faults validator_set finalize
    (signed { header with proposed_state_root = String.make 32 'c' });
  check_conflict validator_set finalize
    (signed { header with proposed_state_root = String.make 32 'c' });
  let base = Test_workspace.unique_path "journal_hash" in
  Unix.mkdir base 0o750;
  Journal.persist_certificate base ~validator_set finalize;
  Journal.persist_bundle base finalize bundle;
  Journal.persist_bundle base finalize { bundle with tx_hashes = [hash] };
  let file = Filename.concat base "finality/pending_finalized.json" in
  let saved = read_file file |> Yojson.Safe.from_string in
  let open Yojson.Safe.Util in
  expect "writer uses hexadecimal hashes"
    (saved |> member "bundle" |> member "tx_hashes" = `List [`String hash]);
  check_record base header.chain_id validator_set;
  let legacy digest =
    match saved with
    | `Assoc fields -> `Assoc (List.map (fun (name, value) ->
      if name <> "bundle" then name, value
      else match value with
        | `Assoc fields -> name, `Assoc (List.map (fun (name, value) ->
          name, if name = "tx_hashes" then `List [`String digest] else value) fields)
        | _ -> failwith "bundle object") fields)
    | _ -> failwith "record object"
  in
  write_file file (Yojson.Safe.to_string (legacy raw));
  check_record base header.chain_id validator_set;
  Journal.persist_bundle base finalize bundle;
  write_file file (Yojson.Safe.to_string (legacy (String.make 32 'x')));
  expect "different digest refused"
    (match Journal.read_validated ~chain_id:header.chain_id ~validator_set base with
     | Journal.Invalid _ -> true | _ -> false);
  write_file file (Yojson.Safe.to_string saved);
  check_finish base validator_set finalize

let check_saved record anchor =
  let json = read_file record |> Yojson.Safe.from_string in
  let trust = read_file anchor |> Yojson.Safe.from_string in
  let open Yojson.Safe.Util in
  let decode json = json |> member "finalize" |> to_string |> Base64.decode_exn |> Codec.decode_finalize in
  let cert = decode json in
  let known = decode trust in
  expect "independent certificate commitment" (Journal.same_block cert known);
  let validator_set = trust |> member "validator_set" |> to_string
    |> Base64.decode_exn |> Codec.decode_validator_set in
  let base = Test_workspace.unique_path "journal_record" in
  Unix.mkdir base 0o750;
  Journal.persist_certificate base ~validator_set cert;
  write_file (Filename.concat base "finality/pending_finalized.json") (read_file record);
  check_record base cert.chain_id validator_set;
  check_finish base validator_set cert;
  Printf.printf "status = pass epoch = %Ld gate = journal_record\n%!" cert.epoch_id

let () =
  check_hashes ();
  begin match Array.to_list Sys.argv with
  | [_] -> ()
  | [_; record; anchor] -> check_saved record anchor
  | _ -> invalid_arg "expected record and anchor paths"
  end;
  Printf.printf "status = pass test = journal_hash\n%!"