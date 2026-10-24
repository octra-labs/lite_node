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

exception Driver_ready
exception Store_stopped

let test_boot_set ?(seats = 8) ?(ahead = false) ?(wal = false) ?(recovered = false) ?(range = false)
    ?(root_retry = false)
    ?(mark_here = false) ?(old_vote = false) ?wal_round relief =
  let module N = Octra_node_runtime in
  let module Driver = Octra_consensus.C_driver in
  let chain = if relief then "octra-devnet-9871-cluster" else "boot-set" in
  let epoch = if relief then 1_614_505L else 12L in
  let start = if wal then epoch else Int64.succ epoch in
  let active_epoch = if relief || ahead then Int64.add epoch 8L else epoch in
  let old_keys = if relief then
      List.map (fun (member, key) ->
        {member with Octra_core.Validator_entry.weight = Z.one}, key)
        (members 21 @ members 25)
      |> List.filteri (fun index _ -> index < seats)
    else members 21 in
  let keys = if ahead then old_keys
    else if relief then List.filteri (fun index _ -> index < seats - 1) old_keys
    else members 25 in
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
    let manifest, root = if not root_retry then None, root else begin
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
    if not wal then begin
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
      exit_error = (fun () -> fail "unexpected startup refusal");
      exit_refused = (fun () -> raise Store_stopped);
    } in
    if root_retry then begin
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
    let launch () =
      (try S.run deps; fail "driver missing" with Driver_ready -> ());
      match !driver_ref with Some driver -> driver | None -> fail "driver not published"
    in
    if wal then ignore (get (Octra_consensus.C_vote_log.set_floor
      (Octra_consensus.C_vote_log.disk ~data_dir) ~through_epoch:(Int64.pred epoch)));
    let driver = launch () in
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
        (try ignore (launch ()); false with Fun.Finally_raised Store_stopped -> true);
      expect "truncated journal cannot arm" (not !finalized);
      expect "truncated journal retained" (read path = broken && F.read data_dir = entries);
      expect "truncated journal restart did not stop"
        (try ignore (launch ()); false with Fun.Finally_raised Store_stopped -> true);
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
  let run deps = Lwt_main.run (P.run_once deps ~validator_count:4 ~peer_quorum:3) in
  expect "missing peers stop startup" (not (run base) && !queried = 1 && !deleted = 0);
  List.iter (fun deps ->
    queried := 0;
    let stopped = try ignore (run deps); false with P.Store_fault _ -> true in
    expect "local fault retried or changed journal" (stopped && !queried = 0 && !deleted = 0))
    [{base with read_pending_commits = (fun () -> raise (Sys_error "disk"))};
     {base with read_pending_commits = (fun () -> [{record with epoch_id = 12}])};
     {base with restore_pending = (fun _ -> P.Invalid "bad vote")}];
  expect "valid stored lock cannot resume" (run {base with restore_pending = (fun _ -> P.Restored)})

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
  test_boot_set false;
  test_boot_set ~root_retry:true false;
  test_boot_set ~recovered:true false;
  test_boot_set ~recovered:true ~range:true false;
  test_boot_set ~ahead:true false;
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
  test_pending_replay_at_start ();
  print_endline "status = pass test = driver_boot"