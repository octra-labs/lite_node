(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module S = Octra_node_runtime.Consensus_driver_boot_shell
module D = Octra_node_runtime.Consensus_driver_read
module J = Octra_node_runtime.Consensus_finality_journal
module R = Octra_node_runtime.Consensus_finality_journal_recovery
module A = Octra_node_runtime.Consensus_validator_anchor
module V = Octra_core.Validator_set_update
module T = Octra_consensus.C_types
module H = Octra_consensus.C_hash
module C = Octra_consensus.C_config
module F = Octra_consensus.Finality_log
module Health = Octra_node_runtime.Consensus_health_wiring

let fail msg =
  failwith ("test_driver_boot: " ^ msg)

let expect label cond =
  if not cond then fail label

let test_enabled () =
  expect "negative disabled" (not (S.enabled (-1)));
  expect "zero disabled" (not (S.enabled 0));
  expect "positive enabled" (S.enabled 1)

let test_validator_state_height () =
  let committed_head = ref 1272591 in
  let height () =
    S.validator_state_height
      ~committed_head_epoch:(fun () -> !committed_head)
  in
  expect "validator state uses committed head" (height () = 1272591L);
  committed_head := 1272592;
  expect "validator state follows committed head" (height () = 1272592L)

let reads () =
  D.{
    chain_id = "octra-test";
    get_epoch_json = (fun epoch -> Some (string_of_int epoch));
    epoch_time = (fun epoch -> Some (float_of_int epoch));
    get_tx_by_txid = (fun txid -> Some (Int64.to_string txid, "tx"));
    read_receipts = (fun epoch -> [string_of_int epoch]);
    root_to_raw32 = Fun.id;
    reward_source = (fun _ _ -> Error "unused");
    head_epoch = (fun () -> Some 12);
    lookup_bundle = (fun _ -> None);
    read_finality = (fun _ -> None);
  }

let test_committed_reads_open () =
  let guarded = S.committed_reads ~readable:(fun () -> true) (reads ()) in
  expect "epoch visible" (guarded.get_epoch_json 7 = Some "7");
  expect "time visible" (guarded.epoch_time 7 = Some 7.);
  expect "transaction visible" (guarded.get_tx_by_txid 7L = Some ("7", "tx"));
  expect "receipts visible" (guarded.read_receipts 7 = ["7"])

let test_committed_reads_closed () =
  let guarded = S.committed_reads ~readable:(fun () -> false) (reads ()) in
  expect "epoch hidden" (guarded.get_epoch_json 7 = None);
  expect "time hidden" (guarded.epoch_time 7 = None);
  expect "transaction hidden" (guarded.get_tx_by_txid 7L = None);
  expect "receipts hidden" (guarded.read_receipts 7 = []);
  expect "head stable" (guarded.head_epoch () = Some 12)

let test_sync_guard () =
  let module Need = Octra_node_runtime.Sync_need in
  let module Mark = Octra_node_runtime.Sync_mark in
  let root = Need.root ~epoch:7 ~head:6 in
  let range = { Need.cause = Need.Range; epoch = 7; head = 6;
    target = Some 5000L } in
  let carried = {Need.cause = Need.Root; epoch = 905; head = 904; target = Some 5002L} in
  let cases = [
    6, Mark.Missing, Ok None;
    6, Mark.Ready (Need.journal ~epoch:7 ~head:6), Ok None;
    5, Mark.Ready (Need.journal ~epoch:7 ~head:6),
      Ok (Some (Need.journal ~epoch:7 ~head:6));
    6, Mark.Ready (Need.conflict ~epoch:7 ~head:6),
      Ok (Some (Need.conflict ~epoch:7 ~head:6));
    20, Mark.Ready (Need.conflict ~epoch:7 ~head:6),
      Ok (Some (Need.conflict ~epoch:7 ~head:6));
    6, Mark.Ready range, Ok (Some range);
    903, Mark.Ready range, Ok (Some range);
    904, Mark.Ready range, Ok None;
    5001, Mark.Ready range, Ok None;
    6, Mark.Ready { range with target = Some 20L },
      Ok (Some { range with target = Some 20L });
    7, Mark.Ready { range with target = Some 20L }, Ok None;
    5, Mark.Ready root, Ok (Some root);
    6, Mark.Ready root, Ok (Some root);
    7, Mark.Ready root, Ok None;
    8, Mark.Ready root, Ok None;
    905, Mark.Ready carried, Ok (Some carried);
    906, Mark.Ready carried, Ok None;
    6, Mark.Invalid "invalid marker", Error "invalid marker";
  ] in
  List.iter
    (fun (head, state, expected) ->
      expect "sync guard selects one action"
        (S.sync_plan ~head ~limit:4096 state = expected))
    cases;
  let limit = Octra_node_runtime.Consensus_health_wiring.snapshot_policy_threshold
    ~getenv:(fun name -> if name = "OCTRA_CATCHUP_MAX_LAG" then Some "64" else None) in
  let need = { range with epoch = 101; head = 100; target = Some 200L } in
  expect "configured range is held"
    (S.sync_plan ~head:101 ~limit (Mark.Ready need) = Ok (Some need));
  expect "configured range is ready"
    (S.sync_plan ~head:136 ~limit (Mark.Ready need) = Ok None)

let test_seed_fault () =
  let module F = Octra_node_runtime.Sync_finality in
  let module N = Octra_node_runtime.Sync_need in
  expect "disk error is not conflict"
    (F.recovery ~head:12 (F.Journal "read failed") = None);
  expect "checkpoint mismatch needs root recovery"
    (F.recovery ~head:12 (F.Root "root differs")
     = Some (N.root ~epoch:13 ~head:12));
  expect "contradictory finality stays held"
    (F.recovery ~head:12 (F.Conflict "different block")
     = Some (N.conflict ~epoch:13 ~head:12));
  expect "invalid height cannot make marker"
    (F.recovery ~head:max_int (F.Conflict "different block") = None)

let get = function
  | Ok value -> value
  | Error reason -> fail reason

let members first =
  List.init 4 (fun index ->
    let key =
      Mirage_crypto_ec.Ed25519.priv_of_octets
        (String.make 32 (Char.chr (first + index)))
      |> function Ok key -> key | Error _ -> fail "private key rejected"
    in
    let pubkey = Mirage_crypto_ec.Ed25519.pub_of_priv key
      |> Mirage_crypto_ec.Ed25519.pub_to_octets in
    let address = Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn pubkey) in
    Octra_core.Validator_entry.{address; pubkey; weight = Z.of_int (index + 1)}, key)

let update ~epoch members =
  get (V.make_weighted ~source_epoch:(Int64.pred epoch)
    ~activate_epoch:epoch (List.map fst members))

let certificate ~chain ~epoch ~prev ~root members =
  let creator, _ = List.hd members in
  let header = T.{
    proto_version = proto_version_current;
    chain_id = chain;
    epoch_id = epoch;
    prev_state_root = prev;
    proposed_state_root = root;
    tx_list_hash = Octra_consensus.C_engine.tx_list_hash_for_header [];
    receipt_root = H.receipt_root [];
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = creator.Octra_core.Validator_entry.address;
    txid_hi = 0L;
    ts = 1.;
  } in
  let proposal_id = H.proposal_id header in
  let precommits = List.map (fun (member, key) ->
    let vote = T.{chain_id = chain; epoch_id = epoch; round = 0;
      vote_type = Precommit; proposal_id;
      validator = member.Octra_core.Validator_entry.address; signature = ""} in
    {vote with signature = Mirage_crypto_ec.Ed25519.sign ~key (H.vote_sign_bytes vote)}) members in
  T.{chain_id = chain; epoch_id = epoch; commit_round = 0;
    header; proposal_id; precommits; parent_commit = None}

let read path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in channel) (fun () ->
    really_input_string channel (in_channel_length channel))

let expect_health_set label (deps : S.deps) epoch validator_set =
  expect label
    (Option.map C.validator_set_hash (S.health_root_set deps epoch)
      = Some (C.validator_set_hash validator_set))

let test_health_refusals (deps : S.deps) epoch =
  List.iter (fun (label, changed) ->
    expect label (S.health_root_set changed epoch = None))
    ["health missing committed root", {deps with committed_epoch_root_raw = (fun _ -> None)};
     "health committed root differs", {deps with
       committed_epoch_root_raw = (fun _ -> Some (String.make 32 'z'))};
     "health unreadable set", {deps with state_readable = (fun () -> false)}]

let test_health_cache (deps : S.deps) (cert : T.finalize) validator_set keys =
  let epoch = cert.epoch_id in
  let height = Int64.to_int epoch in
  let missing = {deps with data_dir = Test_workspace.unique_dir "health-empty"} in
  expect "health empty journal"
    (J.read_committed_epoch ~chain_id:deps.chain_id ~epoch missing.data_dir = J.Missing);
  expect "health missing record" (S.health_root_set missing epoch = None);
  deps.finality.store_finalized ~epoch:height cert;
  expect "health cache without set" (S.health_root_set missing epoch = None);
  deps.finality.store_finalized_with_set ~epoch:height ~validator_set cert;
  expect_health_set "health cached historical set" missing epoch validator_set;
  test_health_refusals missing epoch;
  let wrong_epoch = certificate ~chain:deps.chain_id ~epoch:(Int64.succ epoch)
    ~prev:cert.header.prev_state_root ~root:cert.header.proposed_state_root keys in
  let wrong_root = certificate ~chain:deps.chain_id ~epoch
    ~prev:cert.header.prev_state_root ~root:(String.make 32 'z') keys in
  List.iter (fun (label, changed) ->
    deps.finality.store_finalized_with_set ~epoch:height ~validator_set changed;
    expect label (S.health_root_set deps epoch = None))
    ["health cached epoch differs", wrong_epoch; "health cached root differs", wrong_root];
  deps.finality.remove_finalized ~epoch:height;
  expect "health removed cache stays missing" (S.health_root_set missing epoch = None);
  expect_health_set "health journal after cache removal" deps epoch validator_set

let test_health_head (deps : S.deps) (head : Octra_core.Head_manifest.t) validator_set =
  let epoch = Int64.of_int head.epoch_id in
  expect_health_set "health cached head root" {deps with
    committed_epoch_root_raw = (fun _ -> Some (String.make 32 'z'))} epoch validator_set;
  List.iter (fun cached ->
    expect_health_set "health committed head root"
      {deps with cached_head = (fun () -> cached)} epoch validator_set)
    [None; Some {head with epoch_id = head.epoch_id - 1; state_root = String.make 64 '0'}];
  expect "health cached head root differs"
    (S.health_root_set {deps with cached_head = (fun () ->
      Some {head with state_root = String.make 64 '0'})} epoch = None);
  expect "health unreadable cached head"
    (S.health_root_set {deps with state_readable = (fun () -> false)} epoch = None)

let test_health_time (deps : S.deps) (cert : T.finalize) =
  let module Store = Octra_core.Store_chaindata in
  let module Epoch = Octra_core.Epochlog in
  let height = Int64.to_int cert.epoch_id in
  let hex = Octra_node_runtime.Text.hash32_hex in
  let header = Epoch.{empty_epoch_header with id = height;
    state_root = hex cert.header.proposed_state_root;
    prev_state_root = hex cert.header.prev_state_root; finalized_at = 42.} in
  expect "health missing header" (S.health_head_time deps height = None);
  Store.begin_batch deps.chaindata;
  Store.set_epoch deps.chaindata header;
  Store.commit_batch deps.chaindata;
  expect "health header stored" (Store.get_bound_epoch_header deps.chaindata height = Ok header);
  expect "health header time" (S.health_head_time deps height = Some 42.);
  List.iter (fun (label, changed) ->
    expect label (S.health_head_time changed height = None))
    ["health time missing root", {deps with committed_epoch_root_raw = (fun _ -> None)};
     "health time root differs", {deps with
       committed_epoch_root_raw = (fun _ -> Some (String.make 32 'z'))};
     "health unreadable time", {deps with state_readable = (fun () -> false)}];
  let index_header header =
    Store.begin_batch deps.chaindata;
    Octra_core.Chaindata_index.buffer_epoch (Store.index deps.chaindata)
      height (Epoch.epoch_to_json header);
    Store.commit_batch deps.chaindata in
  let changed = {header with finalized_at = 43.} in
  index_header changed;
  expect "health changed index stored" (Store.get_epoch_header deps.chaindata height = Some changed);
  expect "health index time differs" (S.health_head_time deps height = None);
  index_header header;
  expect "health restored header time" (S.health_head_time deps height = Some 42.)

let test_health_providers (deps : S.deps) =
  let chain = "octra-devnet-9871-cluster" in
  let activation = Option.get (Octra_consensus.C_quorum_policy.activation_for_chain chain) in
  let epoch = Int64.of_int activation.activation_epoch in
  let height = Int64.to_int epoch in
  let keys = members 1 in
  let stake = get (V.validator_set (update ~epoch:1L keys)) in
  let historical = T.validator_set_for_epoch ~chain_id:chain ~epoch_id:epoch stake in
  let next_keys = members 5 in
  let next = update ~epoch:(Int64.succ epoch) next_keys in
  let current = T.validator_set_for_epoch ~chain_id:chain ~epoch_id:(Int64.succ epoch)
    (get (V.validator_set next)) in
  expect "health historical weights differ"
    (C.validator_set_hash historical <> C.validator_set_hash stake);
  expect "health current set differs"
    (C.validator_set_hash historical <> C.validator_set_hash current);
  let root = String.make 32 'b' in
  let head_root = String.make 32 'c' in
  let cert = certificate ~chain ~epoch ~prev:(String.make 32 'a') ~root keys in
  let latest = certificate ~chain ~epoch:(Int64.succ epoch) ~prev:root ~root:head_root next_keys in
  List.iter (fun ((cert : T.finalize), validator_set) ->
    J.persist_certificate deps.data_dir ~validator_set cert;
    J.persist_bundle deps.data_dir cert J.{tx_hashes = []; txs = []; receipts_json = []};
    J.promote_applied deps.data_dir ~epoch:cert.epoch_id ~state_root:cert.header.proposed_state_root;
    F.write deps.data_dir (F.of_finalize cert))
    [cert, historical; latest, current];
  let head = Octra_core.Head_manifest.{schema_version = 3; generation = 1;
    epoch_id = height + 1; state_root = Octra_node_runtime.Text.hash32_hex head_root;
    ledger_state_root = None; irmin_commit = None; txid_hi = 0L;
    txlog_seg = None; txlog_off = None; epochlog_off = None; commit_id = "health";
    ts = 1.; quorum_cert_hash = None; epoch_index_hash = None; epoch_index_root = None} in
  let deps = {deps with chain_id = chain;
    sync_validators = (fun () -> Ok current);
    sync_exporters = (fun () -> Ok current);
    current_epoch = ref (height + 2);
    finality = Octra_node_runtime.Consensus_finality_state.(callbacks (create ()));
    p2p_refs = {deps.p2p_refs with consensus_validator_set = ref current;
      scheduled_validator_set = ref None};
    read_active_validator_meta = (fun () -> Some (V.to_string next));
    read_pending_validator_meta = (fun () -> None);
    cached_head = (fun () -> Some head);
    committed_head_epoch = (fun () -> height + 1);
    committed_epoch_root_raw = (fun at ->
      if at = height then Some root else if at = height + 1 then Some head_root else None)} in
  expect "health historical journal valid"
    (match J.read_committed_epoch ~chain_id:chain ~epoch deps.data_dir with
     | J.Valid record -> record.finalize = cert
       && C.validator_set_hash record.validator_set = C.validator_set_hash historical
     | J.Missing | J.Invalid _ -> false);
  expect_health_set "health committed historical set" deps epoch historical;
  test_health_refusals deps epoch;
  let guarded = {deps with committed_head_epoch = (fun () -> fail "health invalid epoch read")} in
  List.iter (fun at ->
    expect "health invalid epoch" (S.health_root_set guarded at = None)) [-1L; Int64.max_int];
  expect "health journal chain differs"
    (S.health_root_set {deps with chain_id = "health-other"} epoch = None);
  test_health_head deps head current;
  test_health_cache deps cert historical keys;
  test_health_time deps cert

exception Driver_ready
exception Store_stopped

let test_boot_set ?(seats = 8) ?(ahead = false) ?(wal = false) ?(recovered = false) ?(range = false)
    ?(root_retry = false) ?(health = false) ?(seatless = false)
    ?(mark_here = false) ?(old_vote = false) ?wal_round relief =
  let module N = Octra_node_runtime in
  let module Driver = Octra_consensus.C_driver in
  let chain = if relief || seatless then "octra-devnet-9871-cluster" else "boot-set" in
  let epoch = if seatless then
      let activation = Option.get (Octra_consensus.C_quorum_policy.seatless_activation_for_chain chain) in
      Int64.succ (Int64.of_int activation.activation_epoch)
    else if relief then 1_614_505L else 12L in
  let equal keys = List.map (fun (member, key) ->
    {member with Octra_core.Validator_entry.weight = Z.one}, key) keys in
  let start = if wal then epoch else Int64.succ epoch in
  let active_epoch = if relief || ahead then Int64.add epoch 8L else epoch in
  let old_keys = if relief then
      List.map (fun (member, key) ->
        {member with Octra_core.Validator_entry.weight = Z.one}, key)
        (members 21 @ members 25)
      |> List.filteri (fun index _ -> index < seats)
    else if seatless then equal (members 21) else members 21 in
  let keys = if ahead then old_keys
    else if relief then List.filteri (fun index _ -> index < seats - 1) old_keys
    else if seatless then equal (members 25) else members 25 in
  let old = update ~epoch:1L old_keys in
  let next = update ~epoch:active_epoch keys in
  let old_set = get (V.validator_set old) in
  let next_set = get (V.validator_set next) in
  let next_votes = T.validator_set_for_epoch ~chain_id:chain ~epoch_id:start next_set in
  let head = ref (Int64.to_int (Int64.pred epoch)) in
  let active = ref (Some (V.to_string old)) in
  let pending = ref (Some (V.to_string next)) in
  let root = String.make 32 'a' in
  let data_dir = Test_workspace.unique_dir "boot-set" in
  let store = Lwt_main.run (Octra_core.Store_irmin.open_store
    (Filename.concat data_dir "irmin")) in
  let chaindata = Octra_core.Store_chaindata.open_chaindata data_dir in
  Fun.protect ~finally:(fun () ->
    Octra_core.Store_chaindata.close chaindata;
    Lwt_main.run (Octra_core.Store_irmin.close store)) (fun () ->
    let manifest, root = if not root_retry && not seatless then None, root else begin
      Lwt_main.run (Octra_core.Store_irmin.set_meta store "last_epoch" (string_of_int !head));
      let ledger = Option.get (Lwt_main.run (Octra_core.Store_irmin.get_head_hash store)) in
      let index = String.make 64 '0' in
      let state_root = Octra_core.Epoch_index_commitment.folded_state_root
        ~ledger_state_root:ledger ~epoch_index_root:index in
      let manifest = Octra_core.Head_manifest.{schema_version = 3; generation = 1;
        epoch_id = !head; state_root; ledger_state_root = Some ledger;
        irmin_commit = Lwt_main.run (Octra_core.Store_irmin.get_commit_hash store);
        txid_hi = 0L; txlog_seg = None; txlog_off = None; epochlog_off = None;
        commit_id = "boot"; ts = 1.; quorum_cert_hash = None;
        epoch_index_hash = None; epoch_index_root = Some index} in
      Some manifest, Digestif.SHA256.(to_raw_string (of_hex state_root))
    end in
    if seatless then begin
      let committed = certificate ~chain ~epoch:(Int64.of_int !head)
        ~prev:(String.make 32 'p') ~root old_keys in
      J.persist_certificate data_dir ~validator_set:old_set committed;
      J.persist_bundle data_dir committed J.{tx_hashes = []; txs = []; receipts_json = []};
      J.promote_applied data_dir ~epoch:committed.epoch_id ~state_root:root;
      F.write data_dir (F.of_finalize committed)
    end;
    let cert = certificate ~chain ~epoch ~prev:root
      ~root:(String.make 32 'b') keys in
    if relief then begin
      let module Codec = Octra_consensus.C_codec in
      let module Relief = Octra_consensus.C_relief in
      let height = if mark_here then epoch else Int64.pred epoch in
      let proof = List.map (fun (member, key) ->
        let sync = Codec.{chain_id = chain; epoch_id = height; round = 64;
          step = T.PrecommitStep; request = false;
          validator = member.Octra_core.Validator_entry.address; signature = ""} in
        {sync with signature = Mirage_crypto_ec.Ed25519.sign ~key
          (H.round_sync_sign_bytes sync)}) old_keys in
      let current = T.validator_set_for_epoch ~chain_id:chain ~epoch_id:height old_set in
      let mark = match Relief.decide ~chain_id:chain ~height ~current
        ~activate_epoch:active_epoch ~target:next_votes ~fingerprint:next.fingerprint ~proof with
        | Relief.Apply mark -> mark
        | Relief.Refuse reason -> fail reason
        | Relief.Wait -> fail "relief not available" in
      let source saved = A.{getenv = (fun _ -> None); chain_id = chain;
        current_height = (fun () -> height);
        active_raw = (fun () -> !active); pending_raw = (fun () -> !pending);
        relief = (fun _ -> Ok (Some saved))} in
      expect "relief proof selects journal set"
        (C.validator_set_hash (get (A.expected_set (source mark) ~epoch))
         = C.validator_set_hash next_votes);
      List.iter (fun round ->
        let expected = if mark_here && round <= mark.round then
            T.validator_set_for_epoch ~chain_id:chain ~epoch_id:epoch old_set
          else next_votes in
        expect "relief selects signed round set"
          (C.validator_set_hash (get (A.expected_set ~round (source mark) ~epoch))
           = C.validator_set_hash expected)) [0; 63; 64; 65; 128];
      List.iter (fun changed ->
        expect "invalid relief proof refused"
          (Result.is_error (A.expected_set (source changed) ~epoch));
        expect "old round cannot bypass relief proof"
          (Result.is_error (A.expected_set ~round:0 (source changed) ~epoch)))
        [{mark with fingerprint = String.make 64 '0'};
         {mark with height = Int64.succ epoch};
         {mark with source_hash = String.make 64 '0'};
         {mark with target_hash = String.make 64 '0'};
         {mark with activate_epoch = Int64.succ active_epoch};
         {mark with round = mark.round + 1};
         {mark with proof = [List.hd mark.proof]};
         {mark with proof = List.map (fun _ -> List.hd mark.proof) mark.proof};
         {mark with proof = List.map (fun (sync : Codec.round_sync) ->
           {sync with Codec.signature = String.make 64 '\000'}) mark.proof}];
      ignore (get (Octra_consensus.C_relief_log.keep
        (Octra_consensus.C_relief_log.disk ~data_dir) mark))
    end;
    if not wal && not health then begin
      J.persist_certificate data_dir ~validator_set:next_votes cert;
      J.persist_bundle data_dir cert J.{tx_hashes = []; txs = []; receipts_json = []};
      F.write data_dir (F.of_finalize cert)
    end;
    let member, key = List.hd keys in
    let bundles = N.Consensus_bundle_cache.create ~cap:4 in
    let finality = N.Consensus_finality_state.(callbacks (create ())) in
    let p2p_set = ref old_set in
    let scheduled = ref None in
    let finalized = ref false in
    let driver_ref = ref None in
    let no_work _ _ = fail "unexpected preverify" in
    let deps = S.{
      env = (fun _ -> None);
      env_int = (fun _ value -> value);
      data_dir;
      chain_id = chain;
      sync_validators = (fun () -> Ok old_set);
      sync_exporters = (fun () -> Ok old_set);
      store;
      chaindata;
      consensus_mode = true;
      voting = false;
      observer = true;
      consensus_port = 1;
      consensus_peers = [];
      role_label = "observer";
      wallet = {address = member.address; pub = Base64.encode_exn member.pubkey;
        priv = Base64.encode_exn (Mirage_crypto_ec.Ed25519.priv_to_octets key)};
      p2p_refs = {consensus_config_hash = ref "";
        consensus_validator_set = p2p_set; scheduled_validator_set = scheduled;
        set_swarm = ignore};
      current_epoch = ref (Int64.to_int epoch);
      consensus_finalized = finalized;
      catchup_active = ref false;
      catchup_queue = N.Consensus_catchup_queue.create ();
      runtime_state = N.Consensus_runtime_state.create ();
      liveness_state = ref (N.Consensus_liveness.create ~now:0.);
      driver_ref;
      on_driver = (fun _ -> raise Driver_ready);
      proposal_state = N.Consensus_proposal_state.create ();
      proposal_bundles = bundles;
      bundle_runtime = N.Consensus_bundle_cache.node_runtime bundles;
      proposal_limits = N.Consensus_proposal.limits
        ~max_txs:10 ~max_bytes:1000 ~max_ou:(Z.of_int 1000);
      current_round = (fun () -> 0);
      finality;
      catchup_queue_node = {
        queue_catchup_target = (fun ~target_epoch:_ ~reason:_ -> fail "unexpected catchup");
        queue_finalized_gap = (fun ~target_epoch:_ ~reason:_ -> fail "unexpected gap")};
      read_active_validator_meta = (fun () -> !active);
      read_pending_validator_meta = (fun () -> !pending);
      read_head_hash = (fun () -> None);
      get_meta = (fun _ -> None);
      duty_state = (fun _ -> Error "unused");
      read_persistent_pending = (fun () -> Lwt.return !pending);
      root_of_head_hash = Fun.id;
      root_to_raw32 = (fun value -> if String.length value = 64 then
        Digestif.SHA256.(to_raw_string (of_hex value)) else value);
      raw_to_hex = Fun.id;
      read_prev_ledger_root = (fun () -> Lwt.return_some root);
      find_account = (fun _ -> None);
      build_preverify = N.Consensus_preverify_role.build no_work;
      validate_preverify = N.Consensus_preverify_role.validate no_work;
      proposal_preview = (fun ?catch_exn:_ _ -> fail "unexpected preview");
      prepare_at = (fun _ -> Ok None);
      apply_catchup_record = (fun _ -> fail "unexpected apply");
      catchup_base_eic = (fun () -> "");
      next_txid = (fun () -> 1L);
      cached_head = (fun () -> manifest);
      committed_epoch_root_raw = (fun epoch -> if epoch = !head then Some root else None);
      committed_head_epoch = (fun () -> !head);
      read_local_root_raw = (fun () -> Lwt.return root);
      read_local_ledger_root_raw = (fun () -> Lwt.return root);
      cached_root = (fun () -> {root; eic = None});
      clear_state_attested = (fun () -> ());
      set_state_attested = (fun ~head:_ ~root:_ -> ());
      clear_quarantine = ignore;
      mark_quarantine = ignore;
      validator_pubkeys_for_epoch = (fun ~wallet_addr:_ ~wallet_pub:_ ~epoch:_ -> []);
      proposal_capacity = Z.of_int 1000;
      save_drops = ignore;
      quarantine_mismatch_threshold = 3;
      soft_catchup_max_lag = 100;
      quarantine_ahead_streak_threshold = 3;
      quarantine_ahead_grace_epochs = 3;
      quarantine_ahead_drift_tolerance = 1;
      quarantine_poll_sec = 1.;
      liveness_stall_sec = 60.;
      state_readable = (fun () -> true);
      sleep = (fun _ -> fst (Lwt.wait ()));
      now = (fun () -> 0.);
      require_sync = (fun _ -> fail "unexpected snapshot request");
      exit_error = (fun () -> raise Store_stopped);
      exit_refused = (fun () -> raise Store_stopped);
    } in
    if health then test_health_providers deps
    else if root_retry then begin
      let module Mark = N.Sync_mark in
      let need = N.Sync_need.root ~head:!head ~epoch:(Int64.to_int epoch) in
      expect "retry root stored" (Mark.write ~data_dir ~chain need = Ok Mark.Stored);
      let marker = read (Mark.path data_dir) in
      let path = Filename.concat data_dir "finality/pending_finalized.json" in
      let saved = read path in
      let write raw =
        let channel = open_out_bin path in
        Fun.protect ~finally:(fun () -> close_out channel)
          (fun () -> output_string channel raw) in
      let held altered =
        S.run {altered with on_driver = (fun _ -> fail "invalid recovery started driver")};
        expect "invalid recovery armed apply" (not !finalized);
        expect "invalid recovery changed marker" (read (Mark.path data_dir) = marker) in
      List.iter (fun changed ->
        expect "other recovery position accepted" (S.root_replay deps changed = Ok false);
        expect "position check changed marker" (read (Mark.path data_dir) = marker);
        expect "position check changed journal" (read path = saved))
        [N.Sync_need.root ~head:(!head - 1) ~epoch:!head;
         {need with target = Some (Int64.succ epoch)}];
      Lwt_main.run (Octra_core.Store_irmin.set_meta store "last_epoch" (string_of_int (!head - 1)));
      let changed_manifest = Option.map (fun manifest ->
        {manifest with Octra_core.Head_manifest.irmin_commit =
          Lwt_main.run (Octra_core.Store_irmin.get_commit_hash store);
          ledger_state_root = Lwt_main.run (Octra_core.Store_irmin.get_head_hash store)}) manifest in
      expect "wrong store epoch accepted"
        (S.root_replay {deps with cached_head = (fun () -> changed_manifest)} need
          = Error "recovery store differs from head");
      expect "epoch check changed marker" (read (Mark.path data_dir) = marker);
      expect "epoch check changed journal" (read path = saved);
      Lwt_main.run (Octra_core.Store_irmin.set_meta store "last_epoch" (string_of_int !head));
      let restored_commit = Lwt_main.run (Octra_core.Store_irmin.get_commit_hash store) in
      let manifest = Option.map (fun manifest ->
        {manifest with Octra_core.Head_manifest.irmin_commit = restored_commit}) manifest in
      let deps = {deps with cached_head = (fun () -> manifest)} in
      let wrong_index = Option.map (fun head ->
        {head with Octra_core.Head_manifest.epoch_index_root = Some (String.make 64 'f')}) manifest in
      expect "wrong index accepted"
        (S.root_replay {deps with cached_head = (fun () -> wrong_index)} need
          = Error "store snapshot root differs");
      expect "index check changed marker" (read (Mark.path data_dir) = marker);
      expect "index check changed journal" (read path = saved);
      List.iter held [
        {deps with voting = true};
        {deps with observer = false};
        {deps with state_readable = (fun () -> false)};
        {deps with cached_head = (fun () -> None)};
        {deps with cached_head = (fun () -> Option.map (fun head ->
          {head with Octra_core.Head_manifest.irmin_commit = Some "wrong"}) manifest)};
        {deps with cached_head = (fun () -> Option.map (fun head ->
          {head with Octra_core.Head_manifest.state_root = String.make 64 '0'}) manifest)};
        {deps with read_pending_validator_meta = (fun () -> Some (V.to_string old))};
      ];
      let fields = match Yojson.Safe.from_string saved with
        | `Assoc fields -> fields | _ -> fail "journal shape" in
      let replace name value = Yojson.Safe.to_string
        (`Assoc ((name, value) :: List.remove_assoc name fields)) in
      let encoded cert = replace "finalize"
        (`String (Base64.encode_exn (Octra_consensus.C_codec.encode_finalize cert))) in
      let wrong_prev = certificate ~chain ~epoch ~prev:(String.make 32 'z')
        ~root:cert.header.proposed_state_root keys in
      let wrong_epoch = certificate ~chain ~epoch:(Int64.succ epoch) ~prev:root
        ~root:cert.header.proposed_state_root keys in
      List.iter (fun raw ->
        write raw;
        held deps;
        expect "invalid recovery edited evidence" (read path = raw))
        ["{";
         encoded {cert with precommits = []};
         encoded wrong_prev;
         encoded wrong_epoch;
         replace "bundle" (`Assoc ["tx_hashes", `List [`String "wrong"];
           "txs", `List []; "receipts_json", `List []])];
      write saved;
      Unix.rename path (path ^ ".saved");
      held deps;
      Unix.rename (path ^ ".saved") path;
      expect "certified next epoch did not resume"
        (try S.run deps; false with Driver_ready -> true);
      expect "retry did not arm finality" !finalized;
      expect "retry removed recovery marker" (read (Mark.path data_dir) = marker);
      expect "retry changed certificate"
        (read path = saved)
    end else if recovered then begin
      let module Need = N.Sync_need in
      let module Mark = N.Sync_mark in
      let module Mode = N.Consensus_mode in
      let exception Restart of int in
      let exception Held in
      head := Int64.to_int epoch;
      active := Some (V.to_string next);
      pending := None;
      J.promote_applied data_dir ~epoch ~state_root:cert.header.proposed_state_root;
      let base = Mode.of_inputs ~cli_observer:false ~env_mode:(Some "bft") in
      let getenv name = if name = "OCTRA_CATCHUP_MAX_LAG" then Some "64" else deps.env name in
      let need = if range then Option.get (Need.lost ~head:(!head - 2) ~target:(Int64.of_int (!head + 64)))
        else Need.root ~epoch:(!head - 1) ~head:(!head - 2) in
      if range then begin
        let outside = {need with target = Some (Int64.of_int (!head + 65))} in
        expect "outside window stored" (Mark.write ~data_dir ~chain outside = Ok Mark.Stored);
        let clears = ref 0 and holds = ref 0 in
        S.run {deps with env = getenv;
          set_state_attested = (fun ~head:_ ~root:_ -> fail "outside window attested");
          on_driver = (fun _ -> fail "outside window started driver");
          clear_state_attested = (fun () -> incr clears);
          mark_quarantine = (fun _ -> incr holds)};
        expect "boot uses configured window" (!clears = 1 && !holds = 1);
        expect "outside window remains" (Mark.read ~data_dir ~chain = Mark.Ready outside);
        Unix.unlink (Mark.path data_dir)
      end;
      expect "restored root stored" (Mark.write ~data_dir ~chain need = Ok Mark.Stored);
      let mode = Mode.with_need (Some need) base in
      let calls = ref 0 in
      let attest = N.Sync_attest.create ~data_dir ~chain ~getenv
        ~set:(fun ~head:_ ~root:_ -> incr calls)
        ~exit:(fun code -> raise (Restart code)) ~exit_error:(fun () -> fail "attestation refused")
        (Some need) in
      let deps = {deps with env = getenv; voting = mode.voting_enabled; observer = mode.observer_enabled;
        set_state_attested = attest;
        committed_epoch_root_raw = (fun at ->
          if at = !head then Some cert.header.proposed_state_root else None);
        read_local_root_raw = (fun () -> Lwt.return cert.header.proposed_state_root)} in
      expect "boot attestation exits once"
        (try S.run deps; false with Restart 75 -> true);
      expect "boot attested checked journal" (!calls = 1);
      expect "boot removed root marker" (Mark.read ~data_dir ~chain = Mark.Missing);
      let read_need () = get (Mark.need (Mark.read ~data_dir ~chain)) in
      let role = Mode.with_need (read_need ()) base in
      let restart = {deps with voting = role.voting_enabled; observer = role.observer_enabled;
        set_state_attested = (fun ~head:_ ~root:_ -> ())} in
      (try S.run restart; fail "restart driver absent" with Driver_ready -> ());
      expect "restart voting role restored" (restart.voting && not restart.observer);
      expect "second recovered root stored" (Mark.write ~data_dir ~chain need = Ok Mark.Stored);
      let anchor = Octra_bootstrap.State_sync.anchor_path data_dir in
      Unix.mkdir (Filename.dirname anchor) 0o700;
      Unix.mkdir anchor 0o700;
      let requires = ref 0 in
      let fresh = N.Sync_attest.create ~data_dir ~chain ~getenv
        ~set:(fun ~head:_ ~root:_ -> incr calls)
        ~exit:(fun code -> raise (Restart code)) ~exit_error:(fun () -> fail "attestation refused")
        (read_need ()) in
      let fault = {deps with set_state_attested = fresh; require_sync = (fun next ->
        incr requires;
        expect "seed fault replaced covered marker" (Mark.write ~data_dir ~chain next = Ok Mark.Stored);
        raise Held)} in
      expect "seed stops before attestation" (try S.run fault; false with Held -> true);
      S.run fault;
      expect "next boot waits without new exit" (!requires = 1 && !calls = 1);
      expect "seed fault keeps new root need"
        (Mark.read ~data_dir ~chain = Mark.Ready
          (Need.carry need (Need.root ~epoch:(!head + 1) ~head:!head)))
    end else begin
    if not relief && not ahead && not wal then begin
      let prior = Filename.concat data_dir "repair-era" in
      Unix.mkdir prior 0o700;
      if seatless then begin
        let committed = certificate ~chain ~epoch:(Int64.of_int !head)
          ~prev:(String.make 32 'p') ~root old_keys in
        J.persist_certificate prior ~validator_set:old_set committed;
        J.persist_bundle prior committed J.{tx_hashes = []; txs = []; receipts_json = []};
        J.promote_applied prior ~epoch:committed.epoch_id ~state_root:root;
        F.write prior (F.of_finalize committed)
      end;
      let legacy = certificate ~chain ~epoch ~prev:root
        ~root:(String.make 32 'b') old_keys in
      J.persist_certificate prior ~validator_set:old_set legacy;
      J.persist_bundle prior legacy J.{tx_hashes = []; txs = []; receipts_json = []};
      J.promote_applied prior ~epoch ~state_root:legacy.header.proposed_state_root;
      F.write prior (F.of_finalize legacy);
      let deps = {deps with S.data_dir = prior} in
      let before = Octra_consensus.C_quorum_policy.make ~count_epoch:0L
        ~stake_epoch:(Some (Int64.succ epoch)) ~round_epoch:(Some (Int64.succ epoch)) in
      let after = Octra_consensus.C_quorum_policy.make ~count_epoch:0L
        ~stake_epoch:(Some epoch) ~round_epoch:(Some epoch) in
      expect "repair used journal before stake"
        (C.validator_set_hash (get (S.repair_set ~policy:before deps epoch))
         = C.validator_set_hash next_votes);
      expect "repair used current stake for historical certificate"
        (C.validator_set_hash (get (S.repair_set ~policy:after deps epoch))
         = C.validator_set_hash old_set)
    end;
    if not wal then begin
      let saved = get (S.pending_runtime deps
        (fun _ ~target_epoch:_ ~reason:_ -> fail "unexpected catchup") ~epoch:start) in
      let peers count = if seatless then next_votes.validators
          |> List.filteri (fun index _ -> index < count)
          |> List.map (fun member -> member.T.address)
        else List.init count (fun index -> "peer" ^ string_of_int index) in
      let need = if relief then 4 else 2 in
      expect "promise follows boot set"
        (saved.validator_count = next_votes.n && saved.level = string_of_int need
          && saved.reached (peers need) && not (saved.reached (peers (need - 1))))
    end;
    let launch () =
      (try S.run deps; fail "driver missing" with Driver_ready -> ());
      match !driver_ref with Some driver -> driver | None -> fail "driver not published"
    in
    if wal then ignore (get (Octra_consensus.C_vote_log.set_floor
      (Octra_consensus.C_vote_log.disk ~data_dir) ~through_epoch:(Int64.pred epoch)));
    let driver = launch () in
    if seatless then
      expect "Boot did not pass authenticated activation epoch through Launch"
        (driver.set_epoch = Some (if ahead then old.activate_epoch else next.activate_epoch))
    else expect "legacy Boot changed activation epoch" (driver.set_epoch = None);
    let proposal_id = H.proposal_id cert.header in
    let query = Driver.query_bundle driver ~epoch_id:epoch ~proposal_id
      ~timeout_seconds:20. ~validate:(fun _ -> false) in
    let ready, _ = Lwt.task () in
    let waiting = N.Consensus_bundle_fetch.join ~ready query in
    expect "bundle query tracks response" (Hashtbl.mem driver.bundle_responses proposal_id);
    Lwt.cancel waiting;
    expect "bundle cancel clears response" (not (Hashtbl.mem driver.bundle_responses proposal_id));
    expect "bundle cancel reaches query"
      (match Lwt.state query with Lwt.Fail Lwt.Canceled -> true | _ -> false);
    if wal then begin
      let old_votes = T.validator_set_for_epoch ~chain_id:chain ~epoch_id:epoch old_set in
      let votes = if old_vote then old_votes else next_votes in
      let wrong_votes = if old_vote then next_votes else old_votes in
      let rounds = if mark_here && not old_vote then
          List.init 64 (fun index -> index + 65)
        else List.init 65 Fun.id in
      let round = match wal_round with
        | Some round -> round
        | None -> rounds |> List.find (fun round ->
          (T.leader_of old_votes ~epoch_id:epoch ~round).address
          <> (T.leader_of next_votes ~epoch_id:epoch ~round).address) in
      let leader = T.leader_of votes ~epoch_id:epoch ~round in
      let _, signer = List.find (fun (member, _) ->
        member.Octra_core.Validator_entry.address = leader.address) old_keys in
      let proposal = T.{chain_id = chain; epoch_id = epoch; round; valid_round = None;
        header = {cert.header with creator_addr = leader.address};
        tx_hashes = []; parent_commit = None; proposer = leader.address; signature = ""} in
      let proposal = {proposal with signature = Mirage_crypto_ec.Ed25519.sign
        ~key:signer (H.propose_sign_bytes proposal)} in
      let vote = T.{chain_id = chain; epoch_id = epoch; round; vote_type = Precommit;
        proposal_id = H.proposal_id proposal.header; validator = member.address; signature = ""} in
      let vote = {vote with signature = Mirage_crypto_ec.Ed25519.sign
        ~key (H.vote_sign_bytes vote)} in
      let hex = N.Text.hash32_hex in
      let pending_vote = Octra_core.Wal.{epoch_id = Int64.to_int epoch; round;
        proposal_id = hex vote.proposal_id; proposed_state_root = hex proposal.header.proposed_state_root;
        txid_hi = 0L; ts = 1.; validator_addr = member.address;
        proposal_b64 = Some (Base64.encode_exn (Octra_consensus.C_codec.encode_propose proposal));
        vote_b64 = Some (Base64.encode_exn (Octra_consensus.C_codec.encode_vote vote));
        tx_hashes = []; txs_json = []; receipts_json = []} in
      Octra_core.Wal.write_pending_commit data_dir pending_vote;
      if wal_round = None then
        expect "precommit differs under another set"
          (Result.is_error (N.Consensus_pending_commit_recovery.recover_record
            ~chain_id:chain ~validator_set:wrong_votes pending_vote));
      let manifest = Octra_core.Head_manifest.{schema_version = 3; generation = 1;
        epoch_id = !head; state_root = hex root; ledger_state_root = None; irmin_commit = None;
        txid_hi = 0L; txlog_seg = None; txlog_off = None; epochlog_off = None;
        commit_id = "boot"; ts = 1.; quorum_cert_hash = None;
        epoch_index_hash = None; epoch_index_root = None} in
      Octra_core.Head_manifest.set_cached manifest;
      let recover () =
        let installed = ref false in
        S.run {deps with on_driver = (fun driver ->
          let notify = driver.Driver.on_validator_set_relief in
          Driver.set_validator_set_relief_handler driver (fun selected fingerprint ->
            let open Lwt.Syntax in
            let* () = notify selected fingerprint in
            installed := true;
            fst (Lwt.wait ())))};
        expect "relief precommit restores before start" !installed;
        Option.get !driver_ref in
      let driver = recover () in
      expect "relief precommit keeps lock" (driver.engine.state.locked_value = Some proposal.header);
      expect "relief precommit keeps valid"
        (driver.engine.state.valid_round >= driver.engine.state.locked_round
          && driver.engine.state.valid_value = Some proposal.header);
      expect "relief precommit selects target"
        (C.validator_set_hash driver.engine.vs = C.validator_set_hash next_votes);
      let wires = match N.Consensus_driver_launch_shell.pending_vote_wires
          ~data_dir ~epoch_id:epoch with
        | Ok wires -> wires
        | Error _ -> fail "restored vote wire refused" in
      expect "relief preserves vote log preparation"
        (Result.is_ok (Driver.prepare_vote_log driver (Ok wires)));
      expect "relief precommit repeat"
        (Result.is_ok (Driver.restore_precommit_lock driver proposal)
          && Result.is_ok (Lwt_main.run (Driver.restore_relief driver)));
      let locked = driver.engine.state.locked_value in
      expect "relief cannot replace restored lock"
        (Result.is_error (Driver.restore_precommit_lock driver
          {proposal with header = {proposal.header with proposed_state_root = String.make 32 'c'}}));
      expect "relief refuses another lock round"
        (Result.is_error (Driver.restore_precommit_lock driver {proposal with round = round + 1}));
      expect "relief refusal keeps lock" (driver.engine.state.locked_value = locked);
      expect "relief precommit journal preserved"
        (Octra_core.Wal.read_pending_commits data_dir = [pending_vote]);
      let repeated = recover () in
      expect "relief precommit restart keeps lock"
        (repeated.engine.state.locked_value = Some proposal.header
          && C.validator_set_hash repeated.engine.vs = C.validator_set_hash next_votes);
      expect "relief restart keeps valid"
        (repeated.engine.state.valid_round >= repeated.engine.state.locked_round
          && repeated.engine.state.valid_value = Some proposal.header);
      Lwt_main.run (Driver.catchup_height repeated epoch);
      expect "same height catchup keeps lock"
        (repeated.engine.state.locked_value = Some proposal.header
          && repeated.engine.state.valid_round >= repeated.engine.state.locked_round
          && repeated.engine.state.round > round);
      ignore (Octra_core.Head_manifest.load_to_cache data_dir)
    end else begin
    expect "boot recovered certificate" (!finalized && finality.has_finalized (Int64.to_int epoch));
    expect "boot starts after certificate" (Driver.current_height driver = start);
    ignore (get (Lwt_main.run (Driver.restore_relief driver)));
    let hash = C.validator_set_hash in
    expect "boot driver selects recovered epoch set"
      (hash driver.engine.vs = hash next_votes);
    expect "boot retains stake weights" (hash driver.stake_vs = hash next_set);
    expect "boot updates p2p set" (hash !p2p_set = hash next_votes);
    if ahead then expect "unchanged membership retains future plan" (Option.is_some !scheduled);
    Lwt_main.run (Driver.maybe_activate_scheduled_validator_set driver ~target_epoch:start);
    expect "plan cannot restore old set" (hash driver.engine.vs = hash next_votes);
    expect "boot leaves committed metadata untouched"
      (!head = Int64.to_int (Int64.pred epoch)
        && !active = Some (V.to_string old) && !pending = Some (V.to_string next));
    let repeated = launch () in
    ignore (get (Lwt_main.run (Driver.restore_relief repeated)));
    expect "restart keeps selected set" (hash repeated.engine.vs = hash next_votes);
    if not relief then begin
      let path = Filename.concat data_dir "finality/pending_finalized.json" in
      let fields = Yojson.Safe.Util.to_assoc (Yojson.Safe.from_string (read path)) in
      let entries = F.read data_dir in
      List.iter (fun bundle ->
        let raw = Yojson.Safe.to_string (`Assoc
          (("bundle", bundle) :: List.remove_assoc "bundle" fields)) in
        let channel = open_out_bin path in
        Fun.protect ~finally:(fun () -> close_out channel) (fun () -> output_string channel raw);
        finalized := false;
        ignore (launch ());
        expect "damaged bundle certificate armed" !finalized;
        expect "damaged bundle retains finality" (F.read data_dir = entries);
        let saved = read path in
        expect "damaged bundle replaced" (saved <> raw);
        expect "damaged bundle proof retained"
          (match J.read_validated ~chain_id:chain ~validator_set:next_votes data_dir with
           | J.Valid record -> record.finalize = cert && Option.is_some record.bundle
           | _ -> false);
        ignore (launch ());
        expect "damaged bundle restart keeps proof" (read path = saved))
        [`Assoc ["tx_hashes", `List []; "txs", `List [];
          "receipts_json", `List [`String "{}"]]; `String "invalid bundle"];
      let raw = read path in
      let broken = String.sub raw 0 (String.length raw - 2) in
      let channel = open_out_bin path in
      Fun.protect ~finally:(fun () -> close_out channel) (fun () -> output_string channel broken);
      finalized := false;
      expect "truncated journal did not stop"
        (try ignore (launch ()); false with
         | Store_stopped | Fun.Finally_raised Store_stopped -> true);
      expect "truncated journal cannot arm" (not !finalized);
      expect "truncated journal retained" (read path = broken && F.read data_dir = entries);
      expect "truncated journal restart did not stop"
        (try ignore (launch ()); false with
         | Store_stopped | Fun.Finally_raised Store_stopped -> true);
      expect "truncated journal restart retained" (read path = broken)
    end
    end end)

let test_journal_set () =
  let chain = "journal-start" in
  let old = update ~epoch:1L (members 1) in
  let keys = members 5 in
  let next = update ~epoch:12L keys in
  let old_set = T.validator_set_for_epoch ~chain_id:chain ~epoch_id:11L
    (get (V.validator_set old)) in
  let next_set = T.validator_set_for_epoch ~chain_id:chain ~epoch_id:12L
    (get (V.validator_set next)) in
  let head = ref 11 in
  let active = ref (Some (V.to_string old)) in
  let pending = ref (Some (V.to_string next)) in
  let anchor = A.{getenv = (fun _ -> None); chain_id = chain;
    current_height = (fun () -> Int64.of_int !head);
    active_raw = (fun () -> !active); pending_raw = (fun () -> !pending);
    relief = (fun _ -> Ok None)} in
  let selected epoch = A.expected_set anchor ~epoch in
  expect "old epoch retains old set"
    (C.validator_set_hash (get (selected 11L)) = C.validator_set_hash old_set);
  expect "activation selects next set"
    (C.validator_set_hash (get (selected 12L)) = C.validator_set_hash next_set);
  let prev = String.make 32 'a' in
  let root = String.make 32 'b' in
  let cert = certificate ~chain ~epoch:12L ~prev ~root keys in
  let base = Test_workspace.unique_dir "journal-start" in
  J.persist_certificate base ~validator_set:next_set cert;
  let bundle = J.{tx_hashes = []; txs = []; receipts_json = []} in
  J.persist_bundle base cert bundle;
  F.write base (F.of_finalize cert);
  let file = Filename.concat base "finality/pending_finalized.json" in
  let before = read file in
  let entries = F.read base in
  expect "old head set reproduces failure"
    (match J.read_validated ~chain_id:chain ~validator_set:old_set base with
     | J.Invalid _ -> true | _ -> false);
  let selected_set = ref None in
  let held = ref 0 in
  let cleared = ref 0 in
  let finalized = ref false in
  let effects = ref [] in
  let record name = effects := name :: !effects in
  let deps = R.{
    read_journal = (fun () -> J.read_selected ~chain_id:chain ~expected_set:selected base);
    read_pending_epoch = (fun () -> J.read_pending_epoch base);
    drop_invalid_unapplied = (fun ~head_epoch:_ -> fail "unexpected journal removal");
    head_epoch = (fun () -> !head);
    root_at_epoch = (fun epoch -> if epoch = 12 then Some root else None);
    current_root = (fun () -> Some prev);
    write_finality = (fun value -> record "finality"; F.write base (F.of_finalize value));
    store_finalized = (fun ~epoch ~validator_set value ->
      expect "stored certificate epoch" (epoch = 12 && value = cert);
      record "certificate";
      selected_set := Some (C.validator_set_hash validator_set));
    store_proposer = (fun _ -> record "proposer");
    store_expected_root = (fun ~epoch ~root:value ->
      expect "stored root" (epoch = 12 && value = root);
      record "root");
    store_bundle = (fun ~proposal_id ~tx_hashes ~txs ~receipts_json ->
      expect "stored bundle" (proposal_id = cert.proposal_id
        && tx_hashes = [] && txs = [] && receipts_json = []);
      record "bundle");
    set_proposal = (fun _ _ -> record "proposal");
    reset_proposal_state = (fun () -> record "reset");
    set_consensus_finalized = (fun value -> finalized := value);
    clear_state_attested = (fun () -> incr cleared);
    commit_journal = (fun ~epoch ~state_root ->
      J.promote_applied base ~epoch ~state_root);
    mark_quarantine = (fun _ -> incr held);
    require_sync = (fun _ -> fail "unexpected snapshot recovery");
  } in
  List.iter (fun raw ->
    pending := raw;
    effects := [];
    let count = !held in
    let stopped = try ignore (R.run deps); false
      with Octra_node_runtime.Consensus_pending_commit_recovery.Store_fault _ -> true in
    expect "unknown set stops process" stopped;
    expect "unknown set only isolates" (!held = count + 1 && !effects = [] && not !finalized);
    expect "unknown set preserves certificate" (read file = before);
    expect "unknown set preserves finality" (F.read base = entries))
    [None; Some "invalid metadata"; Some (V.to_string (update ~epoch:12L (members 9)))];
  pending := Some (V.to_string next);
  expect "next set arms recovery" (R.run deps = R.Armed);
  expect "trusted next set stored" (!selected_set = Some (C.validator_set_hash next_set));
  expect "recovery prepared without publication"
    (!finalized && !cleared = 4 && read file = before && F.read base = entries);
  expect "recovery keeps effect order"
    (List.rev !effects = ["finality"; "certificate"; "proposer"; "root"; "reset"; "proposal"; "bundle"]);
  expect "repeated startup arms same work" (R.run deps = R.Armed);
  expect "repeated startup preserves proof" (read file = before && F.read base = entries);
  head := 12;
  active := Some (V.to_string next);
  pending := None;
  expect "applied root completes journal" (R.run deps = R.Continue);
  expect "only applied proof removes pending" (not (J.pending base));
  expect "completed restart is idle" (R.run deps = R.Continue);
  List.iter (fun (error, prefix) ->
    effects := [];
    let reason = ref "" in
    let blocked = {deps with
      read_journal = (fun () -> Error error);
      mark_quarantine = (fun value -> reason := value)} in
    let stopped = try ignore (R.run blocked); false
      with Octra_node_runtime.Consensus_pending_commit_recovery.Store_fault value ->
        value = prefix ^ "test" in
    expect "read error did not retain state" (stopped && !effects = []);
    expect "read error names failed check" (!reason = prefix ^ "test"))
    [J.Read_set "test", "finality_journal_set_unavailable:";
     J.Read_record "test", "finality_journal_record_unreadable:";
     J.Read_bundle "test", "finality_journal_bundle_invalid:"]

let test_journal_history () =
  let chain = "journal-history" in
  let keys_a = members 1 in
  let keys_b = members 5 in
  let set_a = get (V.validator_set (update ~epoch:1L keys_a)) in
  let set_b = get (V.validator_set (update ~epoch:12L keys_b)) in
  let base = Test_workspace.unique_dir "journal-history" in
  let root_a = String.make 32 'a' in
  let root_b = String.make 32 'b' in
  let root_c = String.make 32 'c' in
  List.iter (fun (epoch, prev, root, keys, validator_set) ->
    let cert = certificate ~chain ~epoch ~prev ~root keys in
    J.persist_certificate base ~validator_set cert;
    J.persist_bundle base cert J.{tx_hashes = []; txs = []; receipts_json = []};
    J.promote_applied base ~epoch ~state_root:root)
    [11L, root_a, root_b, keys_a, set_a; 12L, root_b, root_c, keys_b, set_b];
  let epochs = ref [] in
  let selected epoch =
    epochs := epoch :: !epochs;
    if epoch = 11L then Ok set_a
    else if epoch = 12L then Ok set_b
    else Error "unknown epoch"
  in
  let replay select = J.read_replay_backlog ~chain_id:chain ~expected_set:select
    ~head_epoch:10L ~head_root:root_a base in
  let records = get (replay selected) in
  expect "each history epoch selects its set" (List.rev !epochs = [11L; 12L]);
  expect "history traverses set change"
    (List.map (fun item -> item.J.finalize.T.epoch_id) records = [11L; 12L]);
  expect "history refuses constant head set"
    (Result.is_error (replay (fun _ -> Ok set_a)));
  expect "history refuses unknown next set"
    (Result.is_error (replay (fun epoch -> if epoch = 11L then Ok set_a else Error "missing set")));
  expect "history remains readable" (List.length (get (replay selected)) = 2)

let test_anchor_epoch () =
  let chain = "octra-devnet-9871-cluster" in
  let activation = Option.get (Octra_consensus.C_quorum_policy.activation_for_chain chain) in
  let epoch = Int64.of_int activation.activation_epoch in
  let active = update ~epoch:1L (members 1) in
  let stake = get (V.validator_set active) in
  let source = A.{getenv = (fun _ -> None); chain_id = chain;
    current_height = (fun () -> Int64.pred epoch);
    active_raw = (fun () -> Some (V.to_string active));
    relief = (fun _ -> Ok None);
    pending_raw = (fun () -> None)} in
  let hash set = C.validator_set_hash set in
  expect "weighted rule changes effective set"
    (hash stake <> hash (T.validator_set_for_epoch ~chain_id:chain ~epoch_id:epoch stake));
  List.iter (fun epoch ->
    expect "anchor applies requested epoch rule"
      (hash (get (A.expected_set source ~epoch))
       = hash (T.validator_set_for_epoch ~chain_id:chain ~epoch_id:epoch stake)))
    [Int64.pred epoch; epoch; Int64.succ epoch]

let eviction_state () =
  let chain = "octra-devnet-9871-cluster" in
  let activation = Option.get (Octra_consensus.C_quorum_policy.seatless_activation_for_chain chain) in
  let epoch = Int64.succ (Int64.of_int activation.activation_epoch) in
  let weights = [3; 3; 2; 1; 1] in
  let keys = members 41 @ members 45 |> List.filteri (fun index _ -> index < 5)
    |> List.mapi (fun index (member, key) ->
      {member with Octra_core.Validator_entry.weight = Z.of_int (List.nth weights index)}, key) in
  let active = update ~epoch:(Int64.pred epoch) keys in
  let source = get (V.validator_set active) in
  let offender = (fst (List.nth keys 4)).Octra_core.Validator_entry.address in
  let exact, _ = get (V.make_eviction ~source_epoch:epoch ~activate_epoch:(Int64.succ epoch)
    ~offenders:[offender] source) in
  chain, epoch, keys, active, source, exact

let eviction_runtime ~chain ~head ~active_raw ~pending ~effects (member, key) =
  let module Shell = Octra_node_runtime.Startup_p2p_shell in
  Shell.{getenv = (fun _ -> None); info = ignore; warn = ignore;
    fatal = fail; read_active_validator_meta = (fun () -> !active_raw);
    read_pending_validator_meta = (fun () -> !pending);
    read_head_hash = (fun () -> None); root_of_head_hash = Fun.id;
    install = {
      set_consensus_config_hash = (fun _ -> effects := "hash" :: !effects);
      set_consensus_validator_set = (fun _ -> effects := "active" :: !effects);
      set_scheduled_validator_set = (fun _ -> effects := "pending" :: !effects);
      set_swarm = (fun _ -> effects := "swarm" :: !effects);
    };
    activation_epoch = (fun () -> None); chain_id = chain; consensus_mode = true;
    consensus_port = 0; consensus_peers = [];
    address = member.Octra_core.Validator_entry.address;
    priv_b64 = Base64.encode_exn (Mirage_crypto_ec.Ed25519.priv_to_octets key);
    pub_b64 = Base64.encode_exn member.pubkey; voting = false; role_label = "observer";
    read_persistent_pending = (fun () -> Lwt.return !pending); current_height = (fun () -> !head);
  }

let test_eviction_metadata () =
  let module Config = Octra_node_runtime.Validator_config in
  let chain, epoch, keys, active, source, exact = eviction_state () in
  let next = get (V.validator_set exact) in
  let base = Config.build ~chain_id:chain ~consensus_mode:true ~current_height:epoch
    ~current_entries:[] ~next_entries:[] ~chain_pending_entries:[]
    ~next_activation_epoch:None ~program_trust_hash:None in
  let bind height active pending = Config.bind_persistent_updates ~chain_id:chain
    ~consensus_mode:true ~current_height:height ~active_raw:(Option.map V.to_string active)
    ~pending_raw:(Option.map V.to_string pending) base in
  let waiting = get (bind epoch (Some active) (Some exact)) in
  let scheduled = Option.get waiting.scheduled_driver_config in
  expect "persistent pending lost eviction witness"
    (scheduled.eviction = exact.eviction && scheduled.validator_set = next);
  expect "exact eviction bypassed entry cap without witness"
    (Result.is_error (bind (Int64.succ epoch) (Some {exact with eviction = None}) None));
  let restored = get (bind (Int64.succ epoch) (Some exact) None) in
  expect "persistent active changed eviction weights" (restored.active_vs = next && restored.stake_vs = next);
  expect "persistent pending used uncommitted epoch"
    (Result.is_error (bind (Int64.pred epoch) (Some active) (Some exact)));
  let forged_keys = List.mapi (fun index (member, key) ->
    if index = 2 then {member with Octra_core.Validator_entry.weight = Z.one}, key
    else member, key) keys in
  let forged_source = get (V.validator_set (update ~epoch forged_keys)) in
  let witness = Option.get exact.eviction in
  let forged, _ = get (V.make_eviction ~source_epoch:epoch ~activate_epoch:(Int64.succ epoch)
    ~offenders:witness.offenders forged_source) in
  expect "persistent pending trusted self declared source"
    (Result.is_error (bind epoch (Some active) (Some forged)));
  let source_at head active pending = A.{getenv = (fun _ -> None); chain_id = chain;
    current_height = (fun () -> head); active_raw = (fun () -> Some (V.to_string active));
    pending_raw = (fun () -> pending); relief = (fun _ -> Ok None)} in
  expect "anchor lost trusted eviction target"
    (get (A.expected_set (source_at epoch active (Some (V.to_string exact)))
      ~epoch:(Int64.succ epoch)) = next);
  expect "anchor lost committed eviction weights"
    (get (A.expected_set (source_at (Int64.succ epoch) exact None)
      ~epoch:(Int64.add epoch 2L)) = next);
  expect "anchor accepted forged pending source"
    (Result.is_error (A.expected_set (source_at epoch active (Some (V.to_string forged)))
      ~epoch:(Int64.succ epoch)));
  let loaded = Lwt_main.run (Config.load_persistent_update Config.{
    read_pending = (fun () -> Lwt.return_some (V.to_string exact)); log_invalid = ignore}) in
  expect "untrusted loader accepted eviction witness" (loaded = None);
  expect "metadata check changed old source" (get (V.validator_set active) = source)

let test_eviction_loader () =
  let module Shell = Octra_node_runtime.Startup_p2p_shell in
  let chain, epoch, keys, active, source, exact = eviction_state () in
  let head = ref (Int64.pred epoch) in
  let active_raw = ref (Some (V.to_string active)) in
  let future = update ~epoch:(Int64.add epoch 10L) keys in
  let pending = ref (Some (V.to_string future)) in
  let effects = ref [] in
  let runtime = eviction_runtime ~chain ~head ~active_raw ~pending ~effects (List.hd keys) in
  let start = Shell.start_node runtime in
  let load () = Lwt_main.run (start.load_scheduled_validator_set_config ()) in
  let initial = List.length !effects in
  pending := Some (V.to_string exact);
  expect "batch eviction escaped durable guard" (load () = None && List.length !effects = initial);
  head := epoch;
  let witness = Option.get exact.eviction in
  let forged_source = T.make_weighted_validator_set
    (T.weights_of_set source |> List.mapi (fun index (member, weight) ->
      member, if index = 2 then Z.succ weight else weight)) |> get in
  expect "loader source mutation did not change weights" (not (T.same_voting_set source forged_source));
  let forged, _ = get (V.make_eviction ~source_epoch:epoch ~activate_epoch:(Int64.succ epoch)
    ~offenders:witness.offenders forged_source) in
  List.iter (fun invalid ->
    pending := Some (V.to_string invalid);
    expect "forged loader plan installed side effects"
      (load () = None && List.length !effects = initial))
    [forged;
     {exact with eviction = None};
     {exact with activate_epoch = Int64.add epoch 2L};
     {exact with eviction = Some {witness with offenders = []}};
     {exact with validators = List.map (fun (entry : V.validator_entry) ->
       {entry with weight = Z.succ entry.weight}) exact.validators}];
  pending := Some (V.to_string exact);
  let loaded = Option.get (load ()) in
  expect "loader did not supersede future plan"
    (loaded.eviction = exact.eviction && loaded.activate_epoch = Int64.succ epoch
      && List.length !effects = initial + 3);
  expect "repeated loader changed configuration"
    (load () = Some loaded && List.length !effects = initial + 3);
  let next = get (V.validator_set exact) in
  Lwt_main.run (start.activate_validator_set next exact.fingerprint);
  active_raw := Some (V.to_string exact);
  pending := None;
  head := Int64.succ epoch;
  let restored = Shell.start_node runtime in
  expect "startup loader clipped trusted active set"
    (restored.view.active_vs = next && restored.view.stake_vs = next)

let test_eviction_relief_source () =
  let module Config = Octra_node_runtime.Validator_config in
  let module Shell = Octra_node_runtime.Startup_p2p_shell in
  let module Parent = Octra_node_runtime.Consensus_parent_commit in
  let chain, epoch, keys, _, source, exact = eviction_state () in
  let removed, key = List.hd (members 49) in
  let removed = {removed with Octra_core.Validator_entry.weight = Z.one} in
  let raw_active = update ~epoch:(Int64.pred epoch) (keys @ [removed, key]) in
  let actual = {source with T.authors = Some (epoch,
    [T.{address = removed.address; pubkey = removed.pubkey}, removed.weight])} in
  let base = Config.build ~chain_id:chain ~consensus_mode:true ~current_height:epoch
    ~current_entries:[] ~next_entries:[] ~chain_pending_entries:[]
    ~next_activation_epoch:None ~program_trust_hash:None in
  let bind ?eviction_source () = Config.bind_persistent_updates ?eviction_source
    ~chain_id:chain ~consensus_mode:true ~current_height:epoch
    ~active_raw:(Some (V.to_string raw_active)) ~pending_raw:(Some (V.to_string exact)) base in
  expect "relief source silently used old stake" (Result.is_error (bind ()));
  let data_dir = Test_workspace.unique_dir "eviction-head" in
  let chaindata = Octra_core.Store_chaindata.open_chaindata data_dir in
  Fun.protect ~finally:(fun () -> Octra_core.Store_chaindata.close chaindata) (fun () ->
    let root = String.make 32 'r' in
    let prior = certificate ~chain ~epoch:(Int64.pred epoch)
      ~prev:(String.make 32 'o') ~root:(String.make 32 'p') keys in
    J.persist_certificate data_dir ~validator_set:source prior;
    J.persist_bundle data_dir prior J.{tx_hashes = []; txs = []; receipts_json = []};
    J.promote_applied data_dir ~epoch:prior.epoch_id ~state_root:prior.header.proposed_state_root;
    F.write data_dir (F.of_finalize prior);
    let cert = certificate ~chain ~epoch ~prev:(String.make 32 'p') ~root keys in
    J.persist_certificate data_dir ~validator_set:actual cert;
    J.persist_bundle data_dir cert J.{tx_hashes = []; txs = []; receipts_json = []};
    let pending_path = Filename.concat data_dir "finality/pending_finalized.json" in
    let committed_path = Filename.concat data_dir "finality/committed_finalized.json" in
    let pending_bytes = read pending_path and committed_bytes = read committed_path in
    let entries = F.read data_dir in
    let parent = get (Parent.create ~chain_id:chain ~data_dir ~chaindata (fun _ -> None)) in
    let head = Octra_core.Head_manifest.{schema_version = 3; generation = 1;
      epoch_id = Int64.to_int epoch;
      state_root = Digestif.SHA256.(to_hex (of_raw_string root));
      ledger_state_root = Some (String.make 64 'a'); irmin_commit = None;
      txid_hi = cert.header.txid_hi; txlog_seg = None; txlog_off = None;
      epochlog_off = None; commit_id = "eviction-head"; ts = 1.;
      quorum_cert_hash = None; epoch_index_hash = None; epoch_index_root = None} in
    let resolve at = Parent.set_at_head parent ~epoch_id:at head in
    expect "HEAD resolver lost QC authors" (get (resolve epoch) = actual);
    let fields = Yojson.Safe.Util.to_assoc (Yojson.Safe.from_string pending_bytes) in
    let save raw =
      let channel = open_out_bin pending_path in
      Fun.protect ~finally:(fun () -> close_out channel) (fun () -> output_string channel raw) in
    List.iter (fun bundle ->
      let damaged = Yojson.Safe.to_string (`Assoc
        (("bundle", bundle) :: List.remove_assoc "bundle" fields)) in
      save damaged;
      expect "HEAD resolver accepted damaged matching bundle"
        (Result.is_error (resolve epoch));
      expect "HEAD resolver modified damaged matching journal"
        (read pending_path = damaged && read committed_path = committed_bytes
          && F.read data_dir = entries)) [`Null; `String "invalid bundle"];
    save pending_bytes;
    expect "HEAD resolver promoted pending journal"
      (read pending_path = pending_bytes && read committed_path = committed_bytes
        && F.read data_dir = entries && J.pending data_dir
        && J.read_committed_epoch ~chain_id:chain ~epoch data_dir = J.Missing);
    let waiting = get (bind ~eviction_source:resolve ()) in
    expect "authenticated relief eviction was not scheduled"
      ((Option.get waiting.scheduled_driver_config).eviction = exact.eviction);
    let anchor = A.{getenv = (fun _ -> None); chain_id = chain;
      current_height = (fun () -> epoch);
      active_raw = (fun () -> Some (V.to_string raw_active));
      pending_raw = (fun () -> Some (V.to_string exact));
      relief = (fun _ -> fail "eviction restored superseded relief plan")} in
    expect "source epoch lost full QC set"
      (get (A.expected_set ~eviction_source:resolve anchor ~epoch) = actual);
    expect "source stake selector used old stake"
      (get (A.stake_set ~eviction_source:resolve anchor ~epoch) = actual);
    expect "next epoch did not select exact target"
      (get (A.expected_set ~eviction_source:resolve anchor ~epoch:(Int64.succ epoch))
        = get (V.validator_set exact));
    expect "stake selector lost authenticated eviction"
      (get (A.stake_set ~eviction_source:resolve anchor ~epoch:(Int64.succ epoch))
        = get (V.validator_set exact));
    List.iter (fun changed ->
      expect "HEAD resolver accepted changed commitment"
        (Result.is_error (Parent.set_at_head parent ~epoch_id:epoch changed));
      expect "binder ignored HEAD resolver error"
        (Result.is_error (bind ~eviction_source:(fun at ->
          Parent.set_at_head parent ~epoch_id:at changed) ())))
      [{head with epoch_id = Int64.to_int (Int64.pred epoch)};
       {head with state_root = String.make 64 '0'};
       {head with txid_hi = Int64.succ head.txid_hi}];
    expect "resolver accepted different source epoch"
      (Result.is_error (resolve (Int64.pred epoch)));
    expect "resolver error changed to active match"
      (bind ~eviction_source:(fun _ -> Error "source unavailable") () = Error "source unavailable");
    expect "resolver accepted mismatched source weights"
      (Result.is_error (bind ~eviction_source:(fun _ -> V.validator_set raw_active) ()));
    let head_epoch = ref epoch and active_raw = ref (Some (V.to_string raw_active)) in
    let pending = ref (Some (V.to_string exact)) and effects = ref [] in
    let runtime = eviction_runtime ~chain ~head:head_epoch ~active_raw ~pending ~effects (List.hd keys) in
    let auth = ref (Ok actual) in
    let start = Shell.start_node ~eviction_source:(fun _ -> !auth) runtime in
    let count = List.length !effects in
    auth := Error "source unavailable";
    expect "loader installed after resolver error"
      (Lwt_main.run (start.load_scheduled_validator_set_config ()) = None
        && List.length !effects = count);
    auth := V.validator_set raw_active;
    expect "loader ignored mismatched authenticated set"
      (Lwt_main.run (start.load_scheduled_validator_set_config ()) = None
        && List.length !effects = count);
    auth := Ok actual;
    expect "loader did not use actual relief source"
      (Option.is_some (Lwt_main.run (start.load_scheduled_validator_set_config ()))
        && List.length !effects = count);
    expect "startup source reads changed pending journal"
      (read pending_path = pending_bytes && read committed_path = committed_bytes
        && F.read data_dir = entries);
    J.promote_applied data_dir ~epoch ~state_root:root;
    F.write data_dir (F.of_finalize cert);
    expect "promotion changed source resolver" (get (resolve epoch) = actual && not (J.pending data_dir));
    J.persist_certificate data_dir ~validator_set:actual cert;
    expect "certificate-only pending accepted at HEAD" (Result.is_error (resolve epoch));
    J.persist_bundle data_dir cert J.{tx_hashes = []; txs = []; receipts_json = []};
    expect "identical journals at HEAD refused" (get (resolve epoch) = actual);
    J.promote_applied data_dir ~epoch ~state_root:root;
    J.persist_certificate data_dir ~validator_set:source cert;
    J.persist_bundle data_dir cert J.{tx_hashes = []; txs = []; receipts_json = []};
    let before = read pending_path and committed = read committed_path in
    expect "conflicting source journals accepted" (Result.is_error (resolve epoch));
    expect "conflict resolution changed source journals"
      (read pending_path = before && read committed_path = committed))

let source_config ~chain ~head ~pending (member, key) =
  let module Driver = Octra_consensus.C_driver in
  Driver.{chain_id = chain; my_addr = member.Octra_core.Validator_entry.address;
    sign_fn = (fun bytes -> Mirage_crypto_ec.Ed25519.sign ~key bytes);
    verify_fn = (fun _ _ _ -> false); role_can_vote = (fun () -> false);
    can_vote = (fun () -> false); execute_fn = (fun _ -> true);
    verify_proposal = (fun _ -> Lwt.return Proposal_accept);
    verify_parent_commit = (fun ~epoch_id:_ _ -> Ok ());
    on_finalized = (fun ~validator_set:_ _ -> Lwt.return_unit);
    make_proposal = (fun _ -> Lwt.return_none);
    before_precommit_broadcast = (fun ~epoch_id:_ ~round:_ ~proposal_id:_
      ~proposed_state_root:_ ~txid_hi:_ ~proposal_wire:_ ~vote_wire:_ -> Lwt.return_false);
    lookup_epoch_root = (fun _ -> None); local_head_epoch = (fun () -> !head);
    lookup_bundle = (fun _ -> None);
    lookup_catchup_range = (fun ~from_epoch:_ ~max_epochs:_ -> `NotFound);
    on_resource_attestation = (fun _ -> Lwt.return_unit);
    scheduled_validator_set_config = None;
    load_scheduled_validator_set_config = (fun () -> Lwt.return !pending);
    resource_committee_config = None}

let pending_driver ~activation ~head ~pending ~on_finalized keys =
  let module Driver = Octra_consensus.C_driver in
  let module Policy = Octra_consensus.C_quorum_policy in
  let chain = "pending-epoch" in
  let policy = Policy.make ~count_epoch:0L ~stake_epoch:(Some 1L) ~round_epoch:None
    |> fun policy -> Policy.with_seatless_epoch policy (Some activation) in
  let source = get (V.validator_set (update ~epoch:1L keys)) in
  let member, _ = List.hd keys in
  let config = {(source_config ~chain ~head ~pending (List.hd keys)) with Driver.on_finalized} in
  let swarm = Octra_net.P2p_swarm.create Octra_net.P2p_swarm.{
    listen_port = 0; chain_id = chain;
    node_id = Octra_net.P2p_handshake.node_id_of_pubkey member.pubkey;
    node_addr = member.address; pubkey_raw = member.pubkey;
    consensus_config_hash = String.make 32 'c'; binary_hash = String.make 32 'b';
    require_binary_hash = false; upgrade_plan = None; profile_plan = [];
    allowed_pubkeys = []; bootstrap_peers = []; max_peers = 0; sign_fn = config.sign_fn;
    best_epoch_fn = (fun () -> !head); best_root_fn = (fun () -> String.make 32 'r')}
  in
  Driver.create_for_policy policy ~config ~validator_set:source ~swarm
    ~start_height:(Int64.pred activation)
    ~sync_log:(Octra_consensus.C_sync_log.memory ())
    ~relief_log:(Octra_consensus.C_relief_log.memory ())
    ~vote_log:(Octra_consensus.C_vote_log.memory ())

let test_pending_epoch activation =
  let module Driver = Octra_consensus.C_driver in
  let module Engine = Octra_consensus.C_engine in
  let keys = members 81 |> List.map (fun (member, key) ->
    {member with Octra_core.Validator_entry.weight = Z.one}, key) in
  let heavy = List.mapi (fun index (member, key) ->
    {member with Octra_core.Validator_entry.weight = Z.of_int (if index = 0 then 7 else 1)}, key) keys in
  let source = get (V.validator_set (update ~epoch:1L keys)) in
  let plan = Option.get (Octra_node_runtime.Validator_config.driver_config_of_update
    (update ~epoch:activation heavy)) in
  let head = ref (Int64.sub activation 2L) and pending = ref (Some plan) in
  let finalized = ref [] in
  let make () = pending_driver ~activation ~head ~pending keys
    ~on_finalized:(fun ~validator_set cert ->
      finalized := (cert.T.epoch_id, validator_set) :: !finalized;
      head := cert.epoch_id;
      Lwt.return_unit) in
  let driver = make () in
  expect "pending test lacks stake before activation"
    (Octra_consensus.C_quorum_policy.select driver.policy (Int64.pred activation)
      = Octra_consensus.C_quorum_policy.Stake);
  expect "pending test seatless epoch differs"
    (not (Octra_consensus.C_quorum_policy.seatless driver.policy (Int64.pred activation))
      && Octra_consensus.C_quorum_policy.seatless driver.policy activation);
  let selected epoch = Driver.scheduled_voting_set ~policy:driver.policy
    ~current:source ~chain_id:driver.config.chain_id ~epoch_id:epoch plan in
  expect "preactivation heavy plan refused" (Driver.accept_dynamic_plan driver plan);
  expect "preactivation selection changed" (selected (Int64.pred activation) = plan.validator_set);
  expect "entry cap did not retain active set" (selected activation = source);
  expect "pending snapshot decision differs"
    (Driver.scheduled_choice ~policy:driver.policy ~current:source
      ~chain_id:driver.config.chain_id ~epoch_id:activation plan = Ok (T.Active_set source));
  List.iter (fun epoch ->
    expect "planned set differs from active decision" (Driver.planned_set driver ~epoch (Some plan) = source);
    expect "validator set differs from active decision"
      (Driver.validator_set_at ~policy:driver.policy ~chain_id:driver.config.chain_id
        ~current:source ~epoch (Some plan) = source))
    [Int64.pred activation; activation; Int64.succ activation];
  let installed = ref [] in
  Driver.set_validator_set_activation_handler driver (fun set fingerprint ->
    installed := (set, fingerprint) :: !installed; Lwt.return_unit);
  let cert = certificate ~chain:driver.config.chain_id ~epoch:(Int64.pred activation)
    ~prev:(String.make 32 'p') ~root:(String.make 32 'q') keys in
  expect "pending epoch certificate is invalid"
    (Octra_consensus.C_qc.validate_certificate ~policy:driver.policy
      ~chain_id:driver.config.chain_id ~validator_set:source
      ~verify_vote:(fun vote ->
        match T.pubkey_of_addr source vote.T.validator with
        | None -> false | Some pubkey -> H.verify_vote ~pubkey_raw:pubkey vote)
      (T.certificate_of_finalize cert) = Octra_consensus.C_qc.Valid);
  expect "pending epoch finalize refused" (Engine.accept_finalize_batch driver.engine cert);
  Lwt_main.run (Driver.process_outputs driver);
  expect "pending epoch did not start next height" (driver.engine.state.height = activation);
  expect "pending epoch lost finalized source" (!finalized = [Int64.pred activation, source]);
  expect "pending epoch installed rejected weights"
    (driver.engine.vs = source && driver.stake_vs = source && !installed = [] && driver.set_epoch = None);
  head := Int64.sub activation 2L;
  let caught = make () in
  Lwt_main.run (Driver.catchup_height caught (Int64.succ activation));
  expect "catchup did not cross activation" (caught.engine.state.height = Int64.succ activation);
  expect "catchup installed rejected weights" (caught.engine.vs = source && caught.stake_vs = source);
  List.iter (fun malformed ->
    let refused action = try ignore (action ()); false with Invalid_argument _ -> true in
    let broken = {plan with Driver.validator_set = malformed} in
    expect "malformed pending decision was ignored"
      (Result.is_error (Driver.scheduled_choice ~policy:driver.policy ~current:source
        ~chain_id:driver.config.chain_id ~epoch_id:activation broken));
    expect "malformed scheduled set was ignored" (refused (fun () ->
      Driver.scheduled_voting_set ~policy:driver.policy ~current:source
        ~chain_id:driver.config.chain_id ~epoch_id:activation broken));
    expect "malformed catchup set was ignored" (refused (fun () ->
      Driver.planned_set driver ~epoch:activation (Some broken))))
    [{plan.validator_set with n = plan.validator_set.n + 1};
     {plan.validator_set with total_weight = Z.succ plan.validator_set.total_weight}]

let test_trusted_source_union () =
  let module Driver = Octra_consensus.C_driver in
  let module Shell = Octra_node_runtime.Startup_p2p_shell in
  let chain, epoch, keys, _, source, _ = eviction_state () in
  let member, key = List.hd keys in
  let removed, removed_key = List.hd (members 61) in
  let removed = {removed with Octra_core.Validator_entry.weight = Z.one} in
  let active = update ~epoch:(Int64.pred epoch) (keys @ [removed, removed_key]) in
  let stake = get (V.validator_set active) in
  let author = T.{address = removed.address; pubkey = removed.pubkey} in
  let actual = {source with T.authors = Some (epoch, [author, Z.one])} in
  let runtime = eviction_runtime ~chain ~head:(ref epoch)
    ~active_raw:(ref (Some (V.to_string active))) ~pending:(ref None)
    ~effects:(ref []) (member, key) in
  let view = (Shell.start_node runtime).view in
  let config = source_config ~chain ~head:(ref epoch) ~pending:(ref None) (member, key) in
  let create expected = Driver.create_trusted ~set_epoch:None ~resolve:(fun _ -> Ok expected)
    ~config ~validator_set:stake ~swarm:view.swarm ~start_height:epoch
    ~sync_log:(Octra_consensus.C_sync_log.memory ())
    ~relief_log:(Octra_consensus.C_relief_log.memory ())
    ~vote_log:(Octra_consensus.C_vote_log.memory ()) in
  let driver = create actual in
  expect "trusted source reconstruction applied relief before restore"
    (driver.engine.vs = stake && driver.stake_vs = stake);
  List.iter (fun expected ->
    let refused = try ignore (create expected); false with Invalid_argument _ -> true in
    expect "forged trusted source union accepted" refused)
    [source;
     {actual with authors = Some (epoch, [author, Z.of_int 2])};
     {actual with authors = Some (Int64.pred epoch, [author, Z.one])};
     {actual with authors = Some (epoch, [{author with pubkey = String.make 32 'z'}, Z.one])};
     {actual with authors = Some (epoch, [List.hd source.validators, Z.one])}]

let test_relief_set_epoch () =
  let module Driver = Octra_consensus.C_driver in
  let module Relief = Octra_consensus.C_relief in
  let module Log = Octra_consensus.C_relief_log in
  let module Engine = Octra_consensus.C_engine in
  let module Config = Octra_node_runtime.Validator_config in
  let module Shell = Octra_node_runtime.Startup_p2p_shell in
  let chain, epoch, _, _, _, _ = eviction_state () in
  let keys = members 65 @ members 69 |> List.filteri (fun index _ -> index < 7)
    |> List.map (fun (member, key) ->
      {member with Octra_core.Validator_entry.weight = Z.one}, key) in
  let retained = List.filteri (fun index _ -> index < 6) keys in
  let future = update ~epoch:(Int64.add epoch 20L) retained in
  let old = update ~epoch:(Int64.pred epoch) keys in
  let source = get (V.validator_set old) in
  let target = get (V.validator_set future) in
  let proof height keys = List.map (fun (member, key) ->
    let sync = Octra_consensus.C_codec.{chain_id = chain; epoch_id = height;
      round = 64; step = T.PrecommitStep; request = false;
      validator = member.Octra_core.Validator_entry.address; signature = ""} in
    {sync with signature = Mirage_crypto_ec.Ed25519.sign ~key (H.round_sync_sign_bytes sync)}) keys in
  let mark = match Relief.decide ~chain_id:chain ~height:epoch ~current:source
    ~activate_epoch:future.activate_epoch ~target ~fingerprint:future.fingerprint
    ~proof:(proof epoch keys) with
    | Relief.Apply mark -> mark
    | Relief.Refuse reason -> fail reason
    | Relief.Wait -> fail "relief set epoch proof is missing" in
  let actual = get (Relief.restore ~chain_id:chain ~height:epoch ~current:source
    ~activate_epoch:future.activate_epoch ~target ~fingerprint:future.fingerprint mark)
    |> Option.get in
  let data_dir = Test_workspace.unique_dir "relief-set-epoch" in
  let relief_log = Log.disk ~data_dir in
  ignore (get (Log.keep relief_log mark));
  let mark_path = Filename.concat data_dir (Printf.sprintf "relief_log/%020Ld.mark" epoch) in
  let mark_bytes = read mark_path in
  let head = ref epoch and pending = ref (Config.driver_config_of_update future) in
  let view = (Shell.start_node (eviction_runtime ~chain ~head
    ~active_raw:(ref (Some (V.to_string old))) ~pending:(ref (Some (V.to_string future)))
    ~effects:(ref []) (List.hd keys))).view in
  let config = source_config ~chain ~head ~pending (List.hd keys) in
  let create ~height ~set_epoch ~stake ~expected = Driver.create_trusted
    ~set_epoch ~resolve:(fun _ -> Ok expected) ~config ~validator_set:stake
    ~swarm:view.swarm ~start_height:height
    ~sync_log:(Octra_consensus.C_sync_log.memory ()) ~relief_log
    ~vote_log:(Octra_consensus.C_vote_log.memory ()) in
  let driver = create ~height:epoch ~set_epoch:(Some (Int64.pred epoch))
    ~stake:source ~expected:actual in
  let restores = ref 0 in
  Driver.set_validator_set_relief_handler driver
    (fun _ _ -> incr restores; Lwt.return_unit);
  expect "relief applied before journal restore" (driver.engine.vs = source);
  ignore (get (Lwt_main.run (Driver.restore_relief driver)));
  let round = driver.engine.state.round in
  ignore (get (Lwt_main.run (Driver.restore_relief driver)));
  expect "relief restored twice or advanced set epoch"
    (!restores = 1 && driver.engine.vs = actual && driver.engine.state.round = round
      && driver.set_epoch = Some (Int64.pred epoch));
  let offender = (fst (List.nth retained 5)).Octra_core.Validator_entry.address in
  let exact, _ = get (V.make_eviction ~source_epoch:epoch ~activate_epoch:(Int64.succ epoch)
    ~offenders:[offender] actual) in
  pending := Config.driver_config_of_update exact;
  Lwt_main.run (Driver.maybe_activate_scheduled_validator_set driver ~target_epoch:exact.activate_epoch);
  Engine.start_height driver.engine exact.activate_epoch;
  let next = get (V.validator_set exact) in
  expect "eviction did not advance authenticated set epoch"
    (driver.set_epoch = Some exact.activate_epoch && driver.engine.vs = next);
  ignore (get (Log.prune relief_log ~through_epoch:exact.activate_epoch));
  expect "original future relief mark was not retained"
    (read mark_path = mark_bytes && get (Log.latest relief_log ~through_height:exact.activate_epoch) = Some mark);
  let next_keys = List.filter (fun (member, _) -> member.Octra_core.Validator_entry.address <> offender) retained in
  let later_keys = List.filteri (fun index _ -> index < 4) next_keys in
  let later = update ~epoch:(Int64.add epoch 12L) later_keys in
  let later_config = Option.get (Config.driver_config_of_update later) in
  head := exact.activate_epoch;
  List.iter (fun height -> List.iter (fun plan ->
    pending := plan;
    let runtime = eviction_runtime ~chain ~head
      ~active_raw:(ref (Some (V.to_string exact)))
      ~pending:(ref (Option.map (fun _ -> V.to_string later) plan))
      ~effects:(ref []) (List.hd keys) in
    let restored = (Shell.start_node runtime).view.validator_config in
    let barrier = S.validator_set_epoch ~height restored in
    expect "Boot collector selected future plan epoch" (barrier = Some exact.activate_epoch);
    expect "Boot collector omitted due plan epoch"
      (S.validator_set_epoch ~height:later.activate_epoch restored
        = Some (if plan = None then exact.activate_epoch else later.activate_epoch));
    let anchor = A.{getenv = (fun _ -> None); chain_id = chain;
      current_height = (fun () -> !head);
      active_raw = runtime.read_active_validator_meta;
      pending_raw = runtime.read_pending_validator_meta;
      relief = (fun through_height -> Log.latest relief_log ~through_height)} in
    expect "anchor restored superseded future relief plan"
      (get (A.expected_set anchor ~epoch:height) = next);
    let restart = create ~height ~set_epoch:barrier ~stake:next ~expected:next in
    ignore (get (Lwt_main.run (Driver.restore_relief restart)));
    expect "old mark replaced authenticated survivors"
      (restart.engine.vs = next && restart.relief_plan = None && read mark_path = mark_bytes);
    List.iter (fun set_epoch ->
      let restart = create ~height ~set_epoch ~stake:next ~expected:next in
      expect "missing activation provenance ignored old mark"
        (Result.is_error (Lwt_main.run (Driver.restore_relief restart)))) [None; Some epoch])
    [None; Some later_config]) [exact.activate_epoch; Int64.succ exact.activate_epoch];
  List.iter (fun set_epoch ->
    let refused = try ignore (create ~height:exact.activate_epoch ~set_epoch
      ~stake:next ~expected:next); false with Invalid_argument _ -> true in
    expect "invalid activation epoch accepted" refused)
    [Some (-1L); Some (Int64.succ exact.activate_epoch)];
  let height = Int64.add epoch 2L in
  let later_target = get (V.validator_set later) in
  let current_mark = match Relief.decide ~chain_id:chain ~height ~current:next
    ~activate_epoch:later.activate_epoch ~target:later_target ~fingerprint:later.fingerprint
    ~proof:(proof height next_keys) with
    | Relief.Apply mark -> mark
    | Relief.Refuse reason -> fail reason
    | Relief.Wait -> fail "current relief proof is missing" in
  ignore (get (Log.keep relief_log current_mark));
  let current_anchor = A.{getenv = (fun _ -> None); chain_id = chain;
    current_height = (fun () -> !head);
    active_raw = (fun () -> Some (V.to_string exact));
    pending_raw = (fun () -> Some (V.to_string later));
    relief = (fun through_height -> Log.latest relief_log ~through_height)} in
  let selected = get (A.expected_set current_anchor ~epoch:height) in
  expect "anchor activation epoch ignored current relief mark"
    (T.same_voting_set selected later_target && Option.is_some selected.authors);
  let same_epoch = {current_anchor with relief = (fun _ ->
    Ok (Some {current_mark with height = exact.activate_epoch}))} in
  expect "anchor ignored mark at checkpoint epoch"
    (Result.is_error (A.expected_set same_epoch ~epoch:height));
  let before = Int64.sub epoch 2L in
  let prior = update ~epoch:(Int64.pred before) next_keys in
  let legacy = {current_anchor with
    current_height = (fun () -> before);
    active_raw = (fun () -> Some (V.to_string prior));
    relief = (fun _ -> Ok (Some {mark with height = Int64.sub before 2L}))} in
  expect "anchor skipped old relief before seatless activation"
    (Result.is_error (A.expected_set legacy ~epoch:before));
  pending := Some later_config;
  let restart = create ~height ~set_epoch:(Some exact.activate_epoch) ~stake:next ~expected:next in
  ignore (get (Lwt_main.run (Driver.restore_relief restart)));
  expect "activation epoch ignored current relief mark"
    (T.same_voting_set restart.engine.vs later_target && restart.relief_plan = Some later_config
      && restart.set_epoch = Some exact.activate_epoch && read mark_path = mark_bytes);
  pending := None;
  let restart = create ~height ~set_epoch:(Some exact.activate_epoch) ~stake:next ~expected:next in
  expect "activation epoch ignored missing current relief plan"
    (Result.is_error (Lwt_main.run (Driver.restore_relief restart)))

let test_relief_entry_epoch () =
  let module Driver = Octra_consensus.C_driver in
  let module Relief = Octra_consensus.C_relief in
  let module Engine = Octra_consensus.C_engine in
  let module Config = Octra_node_runtime.Validator_config in
  let module Shell = Octra_node_runtime.Startup_p2p_shell in
  let module Parent = Octra_node_runtime.Consensus_parent_commit in
  let chain, after, _, _, _, _ = eviction_state () in
  let epoch = Int64.sub after 3L in
  let keys = members 73 @ members 77 |> List.filteri (fun index _ -> index < 5)
    |> List.mapi (fun index (member, key) ->
      {member with Octra_core.Validator_entry.weight = Z.of_int (List.nth [3; 3; 2; 1; 1] index)}, key) in
  let retained = List.filteri (fun index _ -> index < 4) keys in
  let old = update ~epoch:(Int64.pred epoch) keys in
  let future = update ~epoch:(Int64.add epoch 4L) retained in
  let source = get (V.validator_set old) and target = get (V.validator_set future) in
  let proof = List.map (fun (member, key) ->
    let sync = Octra_consensus.C_codec.{chain_id = chain; epoch_id = epoch;
      round = 64; step = T.PrecommitStep; request = false;
      validator = member.Octra_core.Validator_entry.address; signature = ""} in
    {sync with signature = Mirage_crypto_ec.Ed25519.sign ~key (H.round_sync_sign_bytes sync)}) keys in
  let mark = match Relief.decide ~chain_id:chain ~height:epoch ~current:source
    ~activate_epoch:future.activate_epoch ~target ~fingerprint:future.fingerprint ~proof with
    | Relief.Apply mark -> mark
    | Relief.Refuse reason -> fail reason
    | Relief.Wait -> fail "pre-seatless relief proof is missing" in
  expect "future relief case does not cross entry cap"
    (Result.is_ok (T.checked_validator_set_for_epoch ~chain_id:chain ~epoch_id:epoch target)
      && T.validate_validator_set_for_epoch ~chain_id:chain ~epoch_id:future.activate_epoch target
        = Error T.Seat_cap);
  let activation = Int64.pred after in
  let historical = get (Relief.restore ~chain_id:chain ~height:activation ~current:source
    ~activate_epoch:future.activate_epoch ~target ~fingerprint:future.fingerprint mark) in
  expect "historical relief failed across seatless activation" (historical = Some target);
  let bad_proof = {mark with proof = List.map (fun (sync : Octra_consensus.C_codec.round_sync) ->
    {sync with signature = String.make 64 '\000'}) mark.proof} in
  expect "historical relief skipped signature verification"
    (Result.is_error (Relief.restore ~chain_id:chain ~height:activation ~current:source
      ~activate_epoch:future.activate_epoch ~target ~fingerprint:future.fingerprint bad_proof));
  expect "new relief skipped current entry cap"
    (match Relief.decide ~chain_id:chain ~height:activation ~current:source
      ~activate_epoch:future.activate_epoch ~target ~fingerprint:future.fingerprint ~proof with
     | Relief.Refuse "validator weight reaches blocking threshold" -> true | _ -> false);
  let config_plan = Option.get (Config.driver_config_of_update future) in
  let head = ref epoch and pending = ref (Some config_plan) in
  let view = (Shell.start_node (eviction_runtime ~chain ~head
    ~active_raw:(ref (Some (V.to_string old))) ~pending:(ref (Some (V.to_string future)))
    ~effects:(ref []) (List.hd keys))).view in
  let data_dir = Test_workspace.unique_dir "relief-entry-epoch" in
  let driver = Driver.create ~config:(source_config ~chain ~head ~pending (List.hd keys))
    ~validator_set:source ~swarm:view.swarm ~start_height:epoch
    ~sync_log:(Octra_consensus.C_sync_log.memory ())
    ~relief_log:(Octra_consensus.C_relief_log.disk ~data_dir)
    ~vote_log:(Octra_consensus.C_vote_log.memory ()) in
  ignore (get (Lwt_main.run (Driver.apply_relief driver config_plan mark target)));
  expect "proven relief did not retain original weights"
    (T.same_voting_set driver.engine.vs target && driver.relief_plan = Some config_plan);
  expect "future projection readmitted current relief set"
    (T.same_voting_set (Driver.planned_set driver ~epoch:future.activate_epoch (Some config_plan)) target);
  let installed = ref [] in
  Driver.set_validator_set_activation_handler driver (fun set fingerprint ->
    installed := (set, fingerprint) :: !installed; Lwt.return_unit);
  List.iter (fun next ->
    head := Int64.pred next;
    Lwt_main.run (Driver.maybe_activate_scheduled_validator_set driver ~target_epoch:next);
    Engine.start_height driver.engine next;
    expect "epoch advance changed trusted relief voting weights"
      (T.same_voting_set driver.engine.vs target && T.same_voting_set driver.stake_vs target))
    (List.init 5 (fun index -> Int64.add epoch (Int64.of_int (index + 1))));
  expect "original future activation did not complete relief"
    (driver.engine.state.height > future.activate_epoch && driver.relief_plan = None
      && driver.set_epoch = Some future.activate_epoch
      && List.length (List.filter (fun (_, fingerprint) -> fingerprint = future.fingerprint) !installed) = 1);
  let previous_root = ref (String.make 32 'p') in
  let recover at =
    let root = Digestif.SHA256.(to_raw_string (digest_string ("relief-head:" ^ Int64.to_string at))) in
    let cert = certificate ~chain ~epoch:at ~prev:!previous_root ~root retained in
    J.persist_certificate data_dir ~validator_set:target cert;
    J.persist_bundle data_dir cert J.{tx_hashes = []; txs = []; receipts_json = []};
    J.promote_applied data_dir ~epoch:at ~state_root:root;
    F.write data_dir (F.of_finalize cert);
    previous_root := root;
    let chaindata = Octra_core.Store_chaindata.open_chaindata data_dir in
    Fun.protect ~finally:(fun () -> Octra_core.Store_chaindata.close chaindata) (fun () ->
      let parent = get (Parent.create ~chain_id:chain ~data_dir ~chaindata (fun _ -> None)) in
      let manifest = Octra_core.Head_manifest.{schema_version = 3; generation = 1;
        epoch_id = Int64.to_int at; state_root = Digestif.SHA256.(to_hex (of_raw_string root));
        ledger_state_root = Some (String.make 64 'a'); irmin_commit = None;
        txid_hi = cert.header.txid_hi; txlog_seg = None; txlog_off = None; epochlog_off = None;
        commit_id = "relief-recovery"; ts = 1.; quorum_cert_hash = None;
        epoch_index_hash = None; epoch_index_root = None} in
      let resolve epoch_id = Parent.set_at_head parent ~epoch_id manifest in
      let durable = ref at in
      let raw_pending = if at < future.activate_epoch then Some (V.to_string future) else None in
      let runtime = eviction_runtime ~chain ~head:durable
        ~active_raw:(ref (Some (V.to_string old))) ~pending:(ref raw_pending)
        ~effects:(ref []) (List.hd retained) in
      let start = Shell.start_node ~eviction_source:resolve runtime in
      expect "HEAD recovery changed actual relief weights"
        (T.same_voting_set start.view.active_vs target && T.same_voting_set start.view.stake_vs target);
      if at < future.activate_epoch then expect "HEAD recovery dropped proven relief pending"
        (Option.is_some start.view.scheduled_validator_set_config);
      let anchor = A.{getenv = (fun _ -> None); chain_id = chain;
        current_height = runtime.current_height;
        active_raw = runtime.read_active_validator_meta;
        pending_raw = runtime.read_pending_validator_meta;
        relief = (fun through_height -> Octra_consensus.C_relief_log.latest
          driver.relief_log ~through_height)} in
      let height = Int64.succ at in
      let expected = get (A.expected_set ~eviction_source:resolve anchor ~epoch:height) in
      let stake = get (A.stake_set ~eviction_source:resolve anchor ~epoch:height) in
      expect "anchor recovery differs from uninterrupted relief"
        (T.same_voting_set expected driver.engine.vs && T.same_voting_set stake target);
      let config = {(source_config ~chain ~head:durable ~pending:(ref None) (List.hd retained)) with
        Driver.load_scheduled_validator_set_config = start.load_scheduled_validator_set_config} in
      let restart = Driver.create_trusted
        ~set_epoch:(S.validator_set_epoch ~height start.view.validator_config)
        ~resolve:(fun _ -> Ok expected) ~config ~validator_set:stake ~swarm:start.view.swarm
        ~start_height:height ~sync_log:(Octra_consensus.C_sync_log.memory ())
        ~relief_log:driver.relief_log ~vote_log:(Octra_consensus.C_vote_log.memory ()) in
      ignore (get (Lwt_main.run (Driver.restore_relief restart)));
      expect "driver recovery differs from uninterrupted relief weights"
        (T.same_voting_set restart.engine.vs driver.engine.vs
          && restart.engine.vs.weights = driver.engine.vs.weights);
      expect "driver recovery advanced durable HEAD"
        (!durable = at && restart.engine.state.height = Int64.succ at);
      if at < future.activate_epoch then
        expect "trusted relief identity rejected by dynamic plan"
          (match Lwt_main.run (Driver.load_dynamic_plan restart) with
           | Some selected -> selected.fingerprint = future.fingerprint
             && T.same_voting_set selected.validator_set restart.engine.vs
           | None -> false);
      let forged epoch_id = Parent.set_at_head parent ~epoch_id
        {manifest with state_root = String.make 64 '0'} in
      let stopped = try ignore (Shell.start_node ~eviction_source:forged runtime); false
        with Failure _ -> true in
      expect "boundary startup accepted forged HEAD source" stopped;
      let changed = retained |> List.mapi
        (fun index ((member : Octra_core.Validator_entry.member), key) ->
          (if index = 0 then {member with weight = Z.succ member.weight} else member), key) in
      let unsafe = update ~epoch:future.activate_epoch changed in
      let unsafe_config = Option.get (Config.driver_config_of_update unsafe) in
      expect "boundary startup source mutation did not change weights"
        (not (T.same_voting_set target unsafe_config.validator_set));
      expect "boundary identity accepted different unsafe entry"
        (Result.is_error (Driver.check_validator_plan ~chain_id:chain ~head:at
          ~current_epoch:height ~current:target ~finalized:None unsafe_config));
      expect "recovery admitted different unsafe weights"
        (match Relief.restore ~chain_id:chain ~height:height ~current:source
          ~activate_epoch:future.activate_epoch ~target:{target with total_weight = Z.succ target.total_weight}
          ~fingerprint:future.fingerprint mark with Error _ -> true | Ok _ -> false))
  in
  List.iter recover [Int64.pred activation; activation; Int64.succ activation;
    Int64.succ future.activate_epoch];
  pending := None;
  List.iter (fun malformed ->
    driver.relief_plan <- Some {config_plan with activate_epoch = driver.engine.state.height;
      validator_set = malformed};
    let refused = try Lwt_main.run (Driver.maybe_activate_scheduled_validator_set driver
      ~target_epoch:driver.engine.state.height); false with Invalid_argument _ -> true in
    expect "relief identity bypassed structural validation"
      (refused && T.same_voting_set driver.engine.vs target))
    [{target with n = target.n + 1}; {target with total_weight = Z.succ target.total_weight}];
  driver.relief_plan <- None;
  let different = retained |> List.mapi (fun index ((member : Octra_core.Validator_entry.member), key) ->
    (if index = 0 then {member with Octra_core.Validator_entry.weight = Z.succ member.weight}
     else member), key) in
  let unsafe = update ~epoch:driver.engine.state.height different in
  let unsafe_set = get (V.validator_set unsafe) in
  expect "unsafe relief mutation did not change weights" (not (T.same_voting_set target unsafe_set));
  let count = List.length !installed in
  pending := Config.driver_config_of_update unsafe;
  Lwt_main.run (Driver.maybe_activate_scheduled_validator_set driver
    ~target_epoch:driver.engine.state.height);
  expect "different unsafe plan replaced current relief set"
    (T.same_voting_set driver.engine.vs target && List.length !installed = count);
  let swapped = T.make_weighted_validator_set (T.weights_of_set target
    |> List.mapi (fun index (member, weight) ->
      (if index = 0 then {member with T.pubkey = String.make 32 'z'} else member), weight)) |> get in
  expect "altered pending key replaced trusted set"
    (Driver.scheduled_voting_set ~current:target ~chain_id:chain
      ~epoch_id:driver.engine.state.height {config_plan with validator_set = swapped} = target)

let test_active_entry_epoch () =
  let module Config = Octra_node_runtime.Validator_config in
  let chain = "octra-devnet-9871-cluster" in
  let activation = Option.get (Octra_consensus.C_quorum_policy.seatless_activation_for_chain chain) in
  let epoch = Int64.of_int activation.activation_epoch in
  let keys = members 57 |> List.mapi (fun index (member, key) ->
    {member with Octra_core.Validator_entry.weight = Z.of_int (if index = 0 then 4 else 1)}, key) in
  let old = update ~epoch:(Int64.pred epoch) keys in
  let source = get (V.validator_set old) in
  let base = Config.build ~chain_id:chain ~consensus_mode:true ~current_height:epoch
    ~current_entries:[] ~next_entries:[] ~chain_pending_entries:[]
    ~next_activation_epoch:None ~program_trust_hash:None in
  let bind active pending = Config.bind_persistent_updates ~chain_id:chain
    ~consensus_mode:true ~current_height:epoch ~active_raw:(Some (V.to_string active))
    ~pending_raw:(Option.map V.to_string pending) base in
  let restored = get (bind old None) in
  expect "historical active set was checked as a new entry"
    (restored.active_vs = source && restored.stake_vs = source);
  expect "new blocking entry was accepted"
    (Result.is_error (bind (update ~epoch keys) None));
  let waiting = get (bind old (Some (update ~epoch:(Int64.succ epoch) keys))) in
  expect "blocking pending replaced trusted active"
    (waiting.active_vs = source && waiting.scheduled_driver_config = None)

let test_eviction_three () =
  let module Config = Octra_node_runtime.Validator_config in
  let module Shell = Octra_node_runtime.Startup_p2p_shell in
  let chain, epoch, _, _, _, _ = eviction_state () in
  let keys = members 53 |> List.map (fun (member, key) ->
    {member with Octra_core.Validator_entry.weight = Z.one}, key) in
  let active = update ~epoch keys in
  let source = get (V.validator_set active) in
  let member, _ = List.hd keys in
  let exact, _ = get (V.make_eviction ~source_epoch:epoch ~activate_epoch:(Int64.succ epoch)
    ~offenders:[member.Octra_core.Validator_entry.address] source) in
  let next = get (V.validator_set exact) in
  expect "three survivor case differs" (source.n = 4 && next.n = 3);
  let head = ref (Int64.succ epoch) and active_raw = ref (Some (V.to_string exact)) in
  let pending = ref None and effects = ref [] in
  let runtime = eviction_runtime ~chain ~head ~active_raw ~pending ~effects (List.nth keys 1) in
  let restored = Shell.start_node runtime in
  expect "three authenticated survivors refused"
    (restored.view.active_vs = next
      && restored.view.validator_config.active_update = Some exact
      && Config.quorum_check ~allow_unsafe:false restored.view.validator_config = Config.Quorum_ok);
  expect "ordinary three validator config exempted"
    (match Config.quorum_check ~allow_unsafe:false
      {restored.view.validator_config with active_update = None} with
     | Config.Quorum_refused _ -> true | _ -> false);
  let count = List.length !effects in
  active_raw := Some (V.to_string {exact with eviction = None});
  let stopped = try ignore (Shell.start_node runtime); false with Failure _ -> true in
  expect "ordinary three validators entered startup" (stopped && List.length !effects = count)

let test_pending_replay_at_start () =
  let module Launch = Octra_node_runtime.Consensus_driver_launch_shell in
  let pending = ref true and replayed = ref 0 and recovered = ref 0 in
  let run () = Lwt_main.run (Launch.recover_before_start
    ~recover_pending:(fun () -> incr recovered; Lwt.return_true)
    ~replay_stashed:(fun ~source:_ -> Lwt.return_unit)
    ~replay_pending:(fun () -> incr replayed; pending := false)
    ~recovery_pending:(fun () -> !pending)) in
  expect "saved journal did not resume" (run ());
  expect "saved journal resumed once" (!replayed = 1 && !recovered = 1);
  pending := true;
  let held = Lwt_main.run (Launch.recover_before_start
    ~recover_pending:(fun () -> incr recovered; Lwt.return_true)
    ~replay_stashed:(fun ~source:_ -> Lwt.return_unit)
    ~replay_pending:(fun () -> incr replayed)
    ~recovery_pending:(fun () -> !pending)) in
  expect "unapplied journal released startup" (not held);
  expect "held journal bypassed recovery" (!replayed = 2 && !recovered = 1)

let test_recovery_faults () =
  let module P = Octra_node_runtime.Consensus_pending_commit_recovery in
  let module W = Octra_core.Wal in
  let record = W.{epoch_id = 11; round = 0; proposal_id = "test";
    proposed_state_root = String.make 64 '0'; txid_hi = 1L; ts = 0.;
    validator_addr = "member0"; proposal_b64 = None; vote_b64 = None;
    tx_hashes = []; txs_json = []; receipts_json = []} in
  let queried = ref 0 and deleted = ref 0 in
  let base = P.{
    read_pending_commits = (fun () -> [record]); head_epoch = (fun () -> 10);
    query_epoch_root = (fun ~epoch_id:_ -> incr queried; Lwt.return []);
    run_catchup_to_target = (fun ~target_epoch:_ ~reason:_ -> failwith "unexpected catchup");
    delete_pending_commit = (fun ~epoch_id:_ ~round:_ -> incr deleted);
    restore_pending = (fun _ -> P.Legacy);
  } in
  let run deps = Lwt_main.run (P.run_once deps ~validator_count:4
    ~reached:(fun matched -> List.length matched >= 3) ~level:"3") in
  expect "missing peers stop startup" (not (run base) && !queried = 1 && !deleted = 0);
  List.iter (fun deps ->
    queried := 0;
    let stopped = try ignore (run deps); false with P.Store_fault _ -> true in
    expect "local fault retried or changed journal" (stopped && !queried = 0 && !deleted = 0))
    [{base with read_pending_commits = (fun () -> raise (Sys_error "disk"))};
     {base with read_pending_commits = (fun () -> [{record with epoch_id = 12}])};
     {base with restore_pending = (fun _ -> P.Invalid "bad vote")}];
  expect "valid stored lock cannot resume" (run {base with restore_pending = (fun _ -> P.Restored)});
  let voters = T.make_validator_set (List.init 4 (fun index ->
    T.{address = "member" ^ string_of_int index; pubkey = String.make 32 (Char.chr index)})) in
  let root = String.make 32 '\000' in
  let reply index = Octra_consensus.C_driver.{responder_addr = "member" ^ string_of_int index;
    responder_head_epoch = 11L; state_root = Some root} in
  let promised count =
    let head = ref 10 and caught = ref 0 and removed = ref 0 in
    let deps = {base with head_epoch = (fun () -> !head);
      query_epoch_root = (fun ~epoch_id:_ -> Lwt.return (List.init count (fun index -> reply (index + 1))));
      run_catchup_to_target = (fun ~target_epoch:_ ~reason:_ -> incr caught; head := 11; Lwt.return_unit);
      delete_pending_commit = (fun ~epoch_id:_ ~round:_ -> incr removed)} in
    let released = Lwt_main.run (P.run_once deps ~validator_count:voters.n
      ~reached:(T.promise_at ~chain_id:"boot-set" ~epoch_id:11L voters)
      ~level:(Octra_consensus.C_measure.promise_text
        (T.space ~chain_id:"boot-set" ~epoch_id:11L voters))) in
    released, !caught, !removed in
  expect "voter set differs" (voters.n = 4 && voters.quorum = 3);
  expect "two matching peers did not start recovery" (promised 2 = (true, 1, 1));
  expect "one matching peer started recovery" (promised 1 = (false, 0, 0))

let health_inject delayed error =
  if delayed then
    let promise, resolver = Lwt.wait () in
    (fun () -> promise), (fun () -> Lwt.wakeup_exn resolver error)
  else
    (fun () -> raise error), (fun () -> ())

let health_deps emit probe = Health.{
  sleep = (fun _ -> emit "delay"; Lwt.return_unit);
  catchup_active = (fun () -> false);
  quarantine_active = (fun () -> false);
  replay_stashed = (fun ~source:_ -> emit "replay"; Lwt.return_unit);
  probe_health = (fun () -> emit "probe"; probe ());
  reset_ahead_streak = (fun () -> ());
  reset_liveness = (fun ~source:_ -> emit "reset"; Lwt.return_unit);
}

let health_launch emit probe = Health.{
  hold_startup = (fun () -> emit "hold");
  pending_recovery = (fun () -> emit "recover"; Lwt.return_true);
  start_driver = (fun () -> emit "start"; Lwt.return_unit);
  startup_probe = (fun () -> Health.startup_probe (health_deps emit probe));
  poll_loop = (fun () -> emit "poll"; Lwt.return_unit);
  log_started = (fun () -> emit "started");
}

let test_health_startup delayed error contained =
  let events = ref [] and escaped = ref [] in
  let emit event = events := event :: !events in
  let probe, finish = health_inject delayed error in
  let prior = !Lwt.async_exception_hook in
  Fun.protect
    ~finally:(fun () -> Lwt.async_exception_hook := prior)
    (fun () ->
      Lwt.async_exception_hook := (fun error -> escaped := error :: !escaped);
      Health.launch (health_launch emit probe);
      if delayed then begin
        expect "startup advanced before probe completion"
          (List.rev !events = ["hold"; "recover"; "start"; "delay"; "probe"]);
        expect "startup failure arrived before completion" (!escaped = [])
      end;
      finish ();
      expect "startup failure routing differs"
        (!escaped = if contained then [] else [error]);
      expect "startup failure changed readiness or launch effects"
        (List.rev !events = ["hold"; "recover"; "start"; "delay"; "probe"]
          @ if contained then ["poll"; "started"] else []))

let test_health_poll delayed error contained =
  let events = ref [] and streak = ref 2 in
  let emit event = events := event :: !events in
  let probe, finish = health_inject delayed error in
  let deps = { (health_deps emit probe) with reset_ahead_streak = (fun () -> streak := 0) } in
  let result = Lwt.catch
    (fun () -> Lwt.map (fun () -> None) (Health.poll_step deps))
    (fun error -> Lwt.return_some error) in
  if delayed then expect "poll advanced before probe completion" (Lwt.is_sleeping result);
  finish ();
  expect "poll failure routing differs"
    (Lwt_main.run result = if contained then None else Some error);
  expect "poll failure reset liveness" (List.rev !events = ["probe"]);
  expect "poll failure retained ahead streak" (!streak = 0);
  let next = {deps with probe_health = (fun () -> incr streak; Lwt.return_unit)} in
  Lwt_main.run (Health.poll_step next);
  expect "poll failure contaminated next ahead" (!streak = 1)

let test_health_start_success () =
  let events = ref [] and escaped = ref [] in
  let emit event = events := event :: !events in
  let prior = !Lwt.async_exception_hook in
  Fun.protect
    ~finally:(fun () -> Lwt.async_exception_hook := prior)
    (fun () ->
      Lwt.async_exception_hook := (fun error -> escaped := error :: !escaped);
      Health.launch (health_launch emit (fun () -> Lwt.return_unit));
      expect "successful startup escaped" (!escaped = []);
      expect "successful startup effects differ"
        (List.rev !events = ["hold"; "recover"; "start"; "delay"; "probe";
          "reset"; "poll"; "started"]))

let test_health_poll_success () =
  let events = ref [] and streak = ref 2 in
  let emit event = events := event :: !events in
  let deps = { (health_deps emit (fun () -> Lwt.return_unit)) with
    reset_ahead_streak = (fun () -> streak := 0) } in
  Lwt_main.run (Health.poll_step deps);
  expect "successful poll effects differ" (List.rev !events = ["probe"; "reset"]);
  expect "successful poll erased ahead streak" (!streak = 2)

let test_health_launch_scope delayed recovery =
  let events = ref [] and escaped = ref [] in
  let emit event = events := event :: !events in
  let error = Failure "launch phase" in
  let call, finish = health_inject delayed error in
  let deps = health_launch emit (fun () -> Lwt.return_unit) in
  let deps = if recovery then {deps with
    pending_recovery = (fun () -> emit "recover"; Lwt.map (fun () -> true) (call ()))
  } else {deps with start_driver = (fun () -> emit "start"; call ())} in
  let prior = !Lwt.async_exception_hook in
  Fun.protect
    ~finally:(fun () -> Lwt.async_exception_hook := prior)
    (fun () ->
      Lwt.async_exception_hook := (fun error -> escaped := error :: !escaped);
      Health.launch deps;
      finish ();
      expect "probe containment swallowed a launch failure" (!escaped = [error]);
      expect "launch failure continued startup"
        (List.rev !events = if recovery then ["hold"; "recover"]
          else ["hold"; "recover"; "start"]))

let test_health_reset_link () =
  let module N = Octra_node_runtime in
  let state = N.Consensus_runtime_state.create () in
  let catchup = ref true in
  let unused label = fail ("unexpected " ^ label) in
  let repair : Health.fork_repair_runtime = {
    committed_head_epoch = (fun () -> unused "repair head");
    rewind_allowed = (fun ~target:_ ~head:_ -> unused "rewind");
    target_matches = (fun ~target:_ ~root:_ -> unused "target match");
    empty_after = (fun ~target:_ ~head:_ -> unused "empty check");
    finality_target_ready = (fun ~target:_ ~root:_ -> unused "finality check");
    stage_empty = (fun ~target:_ ~root:_ -> unused "stage");
    mark_quarantine = (fun _ -> unused "repair mark");
    stop = (fun _ -> unused "stop");
  } in
  let node : Health.node_driver_health_runtime = {
    root_set = (fun _ -> unused "root set");
    head_time = (fun _ -> unused "head time");
    probe_range = (fun _ -> unused "probe range");
    env_int = (fun _ value -> value);
    getenv = (fun _ -> None);
    soft_catchup_max_lag = 0;
    quarantine_ahead_streak_threshold = 3;
    quarantine_ahead_grace_epochs = 0;
    quarantine_ahead_drift_tolerance = 0;
    normalize_next_epoch_for_head = (fun ~source:_ -> unused "normalize");
    committed_head_epoch = (fun () -> unused "head");
    current_epoch = (fun () -> unused "epoch");
    catchup_queue = N.Consensus_catchup_queue.create ();
    catchup_active = catchup;
    runtime_state = state;
    set_state_attested = (fun ~head:_ ~root:_ -> unused "attest");
    clear_quarantine = (fun _ -> unused "clear");
    mark_quarantine = (fun _ -> unused "mark");
    read_local_root_raw = (fun () -> unused "local root");
    committed_epoch_root_raw = (fun _ -> unused "epoch root");
    drain_pending_finalized = (fun () -> unused "drain");
    require_sync = (fun _ -> unused "sync");
    fork_repair = repair;
    run_catchup_to_target = (fun _ ~target_epoch:_ ~reason:_ -> unused "catchup");
    liveness_state = ref (N.Consensus_liveness.create ~now:0.);
    now = (fun () -> 0.);
    stall_sec = 0.;
    observer = (fun () -> true);
    voting = (fun () -> false);
    finality = N.Consensus_finality_state.(callbacks (create ()));
  } in
  let health = Health.node_driver_health_deps node in
  let runtime : Health.consensus_driver_runtime = {
    sleep = (fun _ -> unused "sleep");
    state = {catchup; quarantine = N.Consensus_runtime_state.quarantine_active_ref state};
    hold_startup = (fun () -> unused "hold");
    replay_stashed = (fun ~source:_ -> unused "replay");
    probe = health.probe;
    liveness = health.liveness;
    pending_recovery = (fun _ -> unused "recovery");
    poll_interval = 0.;
    pending_delay = 0.;
    log_started = (fun () -> unused "start log");
  } in
  let linked = Health.consensus_driver_deps runtime in
  let deps : unit Health.driver_deps = {
    sleep = linked.sleep;
    state = linked.state;
    hold_startup = linked.hold_startup;
    replay_stashed = linked.replay_stashed;
    start_driver = (fun () -> unused "start");
    probe_health = (fun () -> unused "probe");
    reset_ahead_streak = linked.reset_ahead_streak;
    reset_liveness = (fun () ~source:_ -> unused "liveness");
    pending_recovery = (fun () -> unused "recovery");
    poll_interval = linked.poll_interval;
    pending_delay = linked.pending_delay;
    log_started = linked.log_started;
  } in
  N.Consensus_runtime_state.incr_ahead_streak state;
  N.Consensus_runtime_state.incr_ahead_streak state;
  expect "ahead streak was not raised" (N.Consensus_runtime_state.ahead_streak state = 2);
  Lwt_main.run (Health.poll_step (Health.health_deps_of_driver deps ()));
  expect "driver link lost ahead reset" (N.Consensus_runtime_state.ahead_streak state = 0)

let test_health_errors () =
  let faults = [
    "ordinary", Failure "probe failed", true;
    "store", Octra_node_runtime.Consensus_pending_commit_recovery.Store_fault "disk", false;
    "worker", Octra_core.Private_ledger.Worker_stopped "probe", false;
    "memory", Out_of_memory, false;
    "stack", Stack_overflow, false;
    "cancel", Lwt.Canceled, false;
  ] in
  let cases = ["startup_success", test_health_start_success;
    "poll_success", test_health_poll_success]
    @ List.concat_map (fun delayed ->
      let mode = if delayed then "async" else "sync" in
      List.concat_map (fun (label, error, contained) ->
        ["startup_" ^ mode ^ "_" ^ label,
          (fun () -> test_health_startup delayed error contained);
         "poll_" ^ mode ^ "_" ^ label,
          (fun () -> test_health_poll delayed error contained)]) faults
      @ List.map (fun recovery ->
        (if recovery then "recovery_" else "driver_") ^ mode,
        (fun () -> test_health_launch_scope delayed recovery)) [false; true]) [false; true] in
  let failures = Fun.protect
    ~finally:(fun () ->
      Lwt.Exception_filter.set Lwt.Exception_filter.handle_all_except_runtime;
      Printf.printf "test = driver_boot_health context = restored filter = handle_all_except_runtime\n%!")
    (fun () ->
      Lwt.Exception_filter.set Lwt.Exception_filter.handle_all;
      List.iter (fun (label, error, _) ->
        List.iter (fun delayed ->
          let call, finish = health_inject delayed error in
          let handled = ref false in
          let result = Lwt.catch call (fun actual ->
            handled := actual == error;
            Lwt.fail actual) in
          finish ();
          expect ("health exception context differs: " ^ label)
            (!handled && match Lwt.state result with
              | Lwt.Fail actual -> actual == error
              | Lwt.Return _ | Lwt.Sleep -> false)) [false; true]) faults;
      Printf.printf
        "test = driver_boot_health context = production_lwt filter = handle_all checks = 12 status = pass\n%!";
      List.filter_map (fun (label, check) ->
        try
          check ();
          Printf.printf "test = driver_boot_health case = %s status = pass\n%!" label;
          None
        with error ->
          Printf.printf "test = driver_boot_health case = %s status = fail error = %s\n%!"
            label (Printexc.to_string error);
          Some label) cases) in
  expect ("health error cases failed: " ^ String.concat "," failures) (failures = [])

let () =
  List.iter (fun seconds -> assert (Octra_node_runtime.Consensus_post_finalize.stall_seconds
    seconds = float_of_int (max 600 seconds))) [-1; 1; 80; 599; 600; 1200];
  test_recovery_faults ();
  test_enabled ();
  test_validator_state_height ();
  test_committed_reads_open ();
  test_committed_reads_closed ();
  test_sync_guard ();
  test_seed_fault ();
  test_boot_set ~health:true false;
  test_boot_set false;
  test_boot_set ~root_retry:true false;
  test_boot_set ~recovered:true false;
  test_boot_set ~recovered:true ~range:true false;
  test_boot_set ~ahead:true false;
  test_boot_set ~seatless:true false;
  test_boot_set ~seatless:true ~ahead:true false;
  test_boot_set true;
  test_boot_set ~wal:true true;
  test_boot_set ~wal:true ~mark_here:true ~old_vote:true true;
  test_boot_set ~seats:7 true;
  test_boot_set ~seats:7 ~wal:true ~mark_here:true ~old_vote:true true;
  test_boot_set ~wal:true ~mark_here:true ~old_vote:true ~wal_round:64 true;
  test_boot_set ~wal:true ~mark_here:true ~wal_round:65 true;
  test_boot_set ~wal:true ~mark_here:true true;
  test_journal_set ();
  test_journal_history ();
  test_anchor_epoch ();
  test_eviction_metadata ();
  test_eviction_loader ();
  test_eviction_relief_source ();
  test_active_entry_epoch ();
  test_pending_epoch 32L;
  test_pending_epoch (Int64.of_int (Option.get
    (Octra_consensus.C_quorum_policy.seatless_activation_for_chain
      "octra-devnet-9871-cluster")).activation_epoch);
  test_trusted_source_union ();
  test_relief_set_epoch ();
  test_relief_entry_epoch ();
  test_eviction_three ();
  test_pending_replay_at_start ();
  test_health_errors ();
  test_health_reset_link ();
  print_endline "status = pass test = driver_boot"