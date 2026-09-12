(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Join = Octra_node_runtime.Consensus_join_rpc
module Transaction = Octra_core.Transaction

let () =
  Mirage_crypto_rng_unix.use_default ()

let fail msg =
  failwith ("test_node_runtime_consensus_join_rpc: " ^ msg)

let expect label cond =
  if not cond then fail label

let synced = function
  | Join.Synced _ -> true
  | Join.Leader_stale _ | Join.Source_unavailable _ -> false

let raw ch =
  String.make 32 ch

let hex_of_raw s =
  Octra_node_runtime.Text.raw_to_hex s

let tx nonce =
  Transaction.{
    from = "oct_sender";
    to_ = "oct_receiver";
    amount = Z.of_int 1;
    nonce;
    ou = Z.of_int 1_000;
    timestamp = 1.0;
    signature = "sig";
    public_key = Some "pub";
    message = None;
    op_type = Standard;
    encrypted_data = None;
  }

let tx_list_hash hashes =
  Octra_net.Hash_domain.hash "octra:tx_list:v1" (String.concat "" hashes)
  |> Octra_node_runtime.Text.raw_to_hex

let finality_private_key, finality_public_key =
  Mirage_crypto_ec.Ed25519.generate ()

let finality_validator_set =
  Octra_consensus.C_types.make_validator_set [{
    Octra_consensus.C_types.address = "oct_creator";
    pubkey =
      Mirage_crypto_ec.Ed25519.pub_to_octets finality_public_key;
  }]

let trusted_validator_set_hash =
  Octra_consensus.C_config.validator_set_hash finality_validator_set

let finality_json
    ~epoch
    ~proto_version
    ~prev
    ~state
    ~tx_hashes =
  let header = Octra_consensus.C_types.{
    proto_version;
    chain_id = "octra-test";
    epoch_id = epoch;
    prev_state_root = prev;
    tx_list_hash =
      Octra_net.Hash_domain.hash
        "octra:tx_list:v1"
        (String.concat "" tx_hashes);
    receipt_root = Octra_consensus.C_hash.receipt_root [];
    proposed_state_root = state;
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = "oct_creator";
    txid_hi = 8L;
    ts = 120.25;
  } in
  let proposal_id = Octra_consensus.C_hash.proposal_id header in
  let unsigned_vote = Octra_consensus.C_types.{
    chain_id = "octra-test";
    epoch_id = epoch;
    round = 4;
    vote_type = Precommit;
    proposal_id;
    validator = "oct_creator";
    signature = String.make 64 '\x00';
  } in
  let vote = {
    unsigned_vote with
    signature =
      Mirage_crypto_ec.Ed25519.sign
        ~key:finality_private_key
        (Octra_consensus.C_hash.vote_sign_bytes unsigned_vote);
  } in
  let finalize = Octra_consensus.C_types.{
    chain_id = "octra-test";
    epoch_id = epoch;
    commit_round = 4;
    header;
    proposal_id;
    precommits = [vote];
    parent_commit = None;
  } in
  `Assoc [
    "finalize",
      `String
        (finalize
         |> Octra_consensus.C_codec.encode_finalize
         |> Base64.encode_exn);
    "validator_set",
      `String
        (finality_validator_set
         |> Octra_consensus.C_codec.encode_validator_set
         |> Base64.encode_exn);
  ]

let reward_source =
  Octra_consensus.C_types.{
    reward_proposer_addr = "oct_parent";
    reward_proposer_public_key = None;
    reward_members = [{
      reward_address = "oct_parent";
      reward_public_key = None;
      reward_weight = Z.one;
    }];
  }

let legacy_reward_source =
  let validator_pubkeys =
    List.map
      (fun (validator : Octra_consensus.C_types.validator_info) ->
        validator.address,
        Base64.encode_exn validator.pubkey)
      finality_validator_set.validators
  in
  Octra_node_runtime.Consensus_reward_attribution.full_set
    ~proposer_addr:"oct_creator"
    ~validator_pubkeys
  |> Octra_node_runtime.Consensus_reward_attribution.to_source
  |> Result.get_ok

let record_json
    ?(epoch = 12L)
    ?(prev = raw 'p')
    ?(state = raw 's')
    ?tx_hashes
    ?txs_json
    ?(source = reward_source)
    ?(proto_version = Octra_consensus.C_types.proto_version_current)
    () =
  let txs_json, tx_hashes =
    match tx_hashes, txs_json with
    | Some hashes, Some txs -> txs, hashes
    | _ ->
      let txs = [tx 1; tx 2] in
      let hashes = List.map Transaction.hash txs in
      List.map (fun tx -> Yojson.Safe.to_string (Transaction.to_yojson tx)) txs,
      hashes
  in
  `Assoc [
    "epoch_id", `String (Int64.to_string epoch);
    "prev_state_root", `String (hex_of_raw prev);
    "state_root", `String (hex_of_raw state);
    "tx_list_hash", `String (tx_list_hash tx_hashes);
    "tx_hashes", `List (List.map (fun h -> `String h) tx_hashes);
    "txs_json", `List (List.map (fun s -> `String s) txs_json);
    "epoch_ts", `Float 120.25;
    "creator_addr", `String "oct_creator";
    "commit_round", `Int 4;
    "reward_source",
      Octra_consensus.C_reward_source.to_yojson source;
    "finality", finality_json ~epoch ~proto_version ~prev ~state ~tx_hashes;
  ]

let cursor () =
  {
    Join.epoch = 12L;
    prev_root = hex_of_raw (raw 'p');
    eic = Octra_core.Epoch_index_commitment.genesis_root;
    txid = 7L;
  }

let head ?ledger_state_root ?epoch_index_root ?(state_root = hex_of_raw (raw 's')) () =
  let ledger_state_root_opt = ledger_state_root in
  let epoch_index_root_opt = epoch_index_root in
  Octra_core.Head_manifest.{
    schema_version = 3;
    generation = 1;
    epoch_id = 10;
    state_root;
    ledger_state_root = ledger_state_root_opt;
    irmin_commit = None;
    txid_hi = 9L;
    txlog_seg = None;
    txlog_off = None;
    epochlog_off = None;
    commit_id = "commit";
    ts = 1.0;
    quorum_cert_hash = None;
    epoch_index_hash = None;
    epoch_index_root = epoch_index_root_opt;
  }

let test_normalize_base () =
  expect "configured none" (Join.configured_base None = None);
  expect "configured blank" (Join.configured_base (Some "   ") = None);
  expect "configured trim" (Join.configured_base (Some " http://a/ ") = Some "http://a/");
  expect "source none" (Join.first_source None = None);
  expect "source blank" (Join.first_source (Some " , ") = None);
  expect "source first"
    (Join.first_source (Some " , https://a,https://b ") = Some "https://a");
  expect "join explicit"
    (Join.configured_join (function
       | "OCTRA_JOIN_RPC" -> Some " https://join "
       | "OCTRA_STATE_SYNC_SOURCES" -> Some "https://sync"
       | _ -> None) = Some "https://join");
  expect "join source default"
    (Join.configured_join (function
       | "OCTRA_STATE_SYNC_SOURCES" -> Some "https://sync-a,https://sync-b"
       | _ -> None) = Some "https://sync-a");
  expect "join disabled" (Join.configured_join (fun _ -> None) = None);
  expect "trim slash" (Join.normalize_base "http://a/" = "http://a");
  expect "keep base" (Join.normalize_base "http://a" = "http://a");
  expect "head url" (Join.head_url "http://a" = "http://a/state-sync/head");
  expect "range url"
    (Join.range_url "http://a" ~from_epoch:12L ~max_epochs:16 =
     "http://a/state-sync/range?from_epoch=12&max_epochs=16")

let with_http_response ?(delay = 0.0) response f =
  let open Lwt.Syntax in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  let* () =
    Lwt_unix.bind socket
      (Unix.ADDR_INET (Unix.inet_addr_loopback, 0))
  in
  Lwt_unix.listen socket 1;
  let port =
    match Lwt_unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> fail "unexpected socket address"
  in
  let server () =
    let* fd, _ = Lwt_unix.accept socket in
    let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
    let rec drain () =
      let* line = Lwt_io.read_line_opt ic in
      match line with
      | Some "" | None -> Lwt.return_unit
      | Some _ -> drain ()
    in
    let* () = drain () in
    let* () = Lwt_unix.sleep delay in
    Lwt_io.write oc response
  in
  Lwt.async server;
  Lwt.finalize
    (fun () -> f (Printf.sprintf "http://127.0.0.1:%d" port))
    (fun () -> Lwt_unix.close socket)

let test_http_get_json () =
  let ok_response =
    "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: 11\r\n\r\n{\"ok\":true}"
  in
  let json =
    Lwt_main.run
      (with_http_response ok_response Join.http_get_json)
  in
  expect "http json ok"
    (Yojson.Safe.Util.(json |> member "ok" |> to_bool));
  let err_response =
    "HTTP/1.1 503 Service Unavailable\r\nContent-Length: 4\r\n\r\nbusy"
  in
  try
    let _ =
      Lwt_main.run
        (with_http_response err_response Join.http_get_json)
    in
    fail "http error accepted"
  with Join.Fetch_retry msg ->
    expect "http error reason"
      (String.contains msg '5' && String.ends_with ~suffix:"busy" msg);
  begin
    try
      let _ =
        Lwt_main.run
          (with_http_response
             ~delay:0.05
             ok_response
             (Join.http_get_json ~timeout:0.01))
      in
      fail "http timeout accepted"
    with Lwt_unix.Timeout -> ()
  end;
  let bad_json =
    "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: 3\r\n\r\nbad"
  in
  begin
    try
      let _ =
        Lwt_main.run
          (with_http_response bad_json Join.http_get_json)
      in
      fail "bad json accepted"
    with Join.Fetch_retry msg ->
      expect "bad json reason"
        (String.starts_with ~prefix:"GET " msg
         && String.ends_with ~suffix:"Invalid token 'bad'\")" msg)
  end

let test_retry_delay () =
  expect "retry negative" (Join.retry_delay (-1) = 1.0);
  expect "retry zero" (Join.retry_delay 0 = 1.0);
  expect "retry one" (Join.retry_delay 1 = 2.0);
  expect "retry two" (Join.retry_delay 2 = 4.0);
  expect "retry three" (Join.retry_delay 3 = 8.0);
  expect "retry four" (Join.retry_delay 4 = 15.0);
  expect "retry capped" (Join.retry_delay 100 = 15.0)

let test_head_roots () =
  let genesis = Octra_core.Epoch_index_commitment.genesis_root in
  expect "local root none" (Join.local_root_from_head None = "");
  expect "local root truncates"
    (Join.local_root_from_head
       (Some (head ~state_root:(hex_of_raw (raw 's') ^ "tail") ())) =
     hex_of_raw (raw 's'));
  expect "base eic none" (Join.base_eic_root_from_head None = genesis);
  expect "base eic missing ledger"
    (Join.base_eic_root_from_head
       (Some (head ~epoch_index_root:"eic" ())) = genesis);
  expect "base eic present"
    (Join.base_eic_root_from_head
       (Some (head ~ledger_state_root:"ledger" ~epoch_index_root:"eic" ())) =
     "eic");
  expect "local eic none" (Join.local_eic_from_head None = None);
  expect "local eic present"
    (Join.local_eic_from_head
       (Some (head ~epoch_index_root:"eic" ())) = Some "eic")

let test_parse_head () =
  let head =
    Join.parse_head
      (`Assoc [
        "head_epoch", `String "42";
        "state_root", `String (hex_of_raw (raw 'r') ^ "extra");
      ])
  in
  expect "head epoch" (head.epoch = 42L);
  expect "head root" (head.root = hex_of_raw (raw 'r'))

let test_parse_range_retry () =
  expect "not found is missing"
    (Join.parse_range
       ~from_epoch:12L
       (`Assoc ["status", `String "not_found"]) = Join.Missing);
  expect "empty retries"
    (Join.parse_range
       ~from_epoch:12L
       (`Assoc ["status", `String "ok"; "records", `List []]) = Join.Retry)

let test_sync_plan () =
  let head root epoch = Join.{ epoch; root } in
  (match Join.sync_plan
           ~local_next:12L
           ~local_root:(hex_of_raw (raw 'r'))
           (head (hex_of_raw (raw 'r')) 11L) with
   | Join.Ready p ->
     expect "ready epoch" (p.ready_epoch = 11L);
     expect "ready root" (p.state_root = hex_of_raw (raw 'r'))
   | _ -> fail "ready plan expected");
  (match Join.sync_plan
           ~local_next:13L
           ~local_root:(hex_of_raw (raw 'r'))
           (head (hex_of_raw (raw 'r')) 11L) with
   | Join.Local_ahead p ->
     expect "ahead local" (p.local_head = 12L);
     expect "ahead leader" (p.leader_head = 11L)
   | _ -> fail "ahead plan expected");
  (match Join.sync_plan
           ~local_next:12L
           ~local_root:(hex_of_raw (raw 'x'))
           (head (hex_of_raw (raw 'r')) 11L) with
   | Join.Root_mismatch p ->
     expect "mismatch local" (p.local_root = hex_of_raw (raw 'x'));
     expect "mismatch leader" (p.leader_root = hex_of_raw (raw 'r'));
     expect "mismatch epoch" (p.epoch = 11L)
   | _ -> fail "mismatch plan expected");
  (match Join.sync_plan
           ~local_next:11L
           ~local_root:(hex_of_raw (raw 'x'))
           (head (hex_of_raw (raw 'r')) 11L) with
   | Join.Fetch_range from_epoch -> expect "fetch epoch" (from_epoch = 11L)
   | _ -> fail "fetch plan expected")

let test_prepare_record () =
  match Join.parse_range
          ~from_epoch:12L
          (`Assoc ["status", `String "ok"; "records", `List [record_json ()]]) with
  | Join.Retry -> fail "record range retried"
  | Join.Missing -> fail "record range missing"
  | Join.Records [record] ->
    let prepared =
      Join.prepare_record
        ~chain_id:"octra-test"
        ~expected_validator_set_hash:trusted_validator_set_hash
        ~cursor:(cursor ())
        record
    in
    expect "epoch" (prepared.epoch_int = 12);
    expect "txs" (List.length prepared.txs = 2);
    expect "proposer"
      (prepared.proposer_info =
       Some { Octra_core.Epochlog.creator_addr = "oct_creator"; commit_round = 4 });
    expect "parent reward proposer"
      (prepared.reward.proposer_addr = "oct_parent");
    let entry = Join.finality_entry ~chain_id:"octra-test" prepared in
    expect "entry height" (entry.height = 12);
    expect "entry round" (entry.round = 4);
    expect "entry proposal" (String.length entry.proposal_id = 64);
    expect "entry txid" (entry.txid_hi = 8L);
    expect "entry timestamp" (entry.ts = 120.25);
    expect "next epoch" (prepared.next_cursor.epoch = 13L);
    expect "next prev" (prepared.next_cursor.prev_root = hex_of_raw (raw 's'));
    expect "next txid" (prepared.next_cursor.txid = 9L)
  | Join.Records _ -> fail "unexpected record count"

let test_prepare_hash_mismatch () =
  let bad_hashes = ["bad"] in
  let bad_txs = [Yojson.Safe.to_string (Transaction.to_yojson (tx 1))] in
  match Join.parse_range
          ~from_epoch:12L
          (`Assoc [
            "status", `String "ok";
            "records", `List [record_json ~tx_hashes:bad_hashes ~txs_json:bad_txs ()];
          ]) with
  | Join.Retry -> fail "bad range retried"
  | Join.Missing -> fail "bad range missing"
  | Join.Records [record] ->
    (try
       let _ =
         Join.prepare_record
           ~chain_id:"octra-test"
           ~expected_validator_set_hash:trusted_validator_set_hash
           ~cursor:(cursor ())
           record
       in
       fail "bad record accepted"
     with Failure _ -> ())
  | Join.Records _ -> fail "unexpected bad record count"

let test_prepare_binds_legacy_reward () =
  let record source =
    match
      Join.parse_range
        ~from_epoch:12L
        (`Assoc [
          "status", `String "ok";
          "records", `List [
            record_json
              ~source
              ~proto_version:
                Octra_consensus.C_types.proto_version_parent_legacy
              ();
          ];
        ])
    with
    | Join.Records [value] -> value
    | Join.Retry -> fail "legacy record range retried"
    | Join.Missing -> fail "legacy record range missing"
    | Join.Records _ -> fail "unexpected legacy record count"
  in
  let prepared =
    Join.prepare_record
      ~chain_id:"octra-test"
      ~expected_validator_set_hash:trusted_validator_set_hash
      ~cursor:(cursor ())
      (record legacy_reward_source)
  in
  expect "legacy reward proposer"
    (prepared.reward.proposer_addr = "oct_creator");
  let expect_rejected label source =
    try
      let _ =
        Join.prepare_record
          ~chain_id:"octra-test"
          ~expected_validator_set_hash:trusted_validator_set_hash
          ~cursor:(cursor ())
          (record source)
      in
      fail (label ^ " accepted")
    with Failure message ->
      expect (label ^ " reason")
        (String.ends_with
           ~suffix:"reward source does not match legacy finality"
           message)
  in
  let injected_proposer = {
    legacy_reward_source with
    reward_proposer_addr = "oct_attacker";
    reward_proposer_public_key = None;
  } in
  let injected_weights = {
    legacy_reward_source with
    reward_members =
      List.map
        (fun (member : Octra_consensus.C_types.reward_member) ->
          {
            member with
            reward_weight = Z.succ member.reward_weight;
          })
        legacy_reward_source.reward_members;
  } in
  expect_rejected "legacy reward proposer injection" injected_proposer;
  expect_rejected "legacy reward weight injection" injected_weights

let test_ready_marker () =
  let secret = String.init 32 (fun i -> Char.chr (i + 1)) in
  let marker =
    Join.ready_marker
      ~data_dir:"data"
      ~consensus_role:"observer"
      ~leader_rpc:"http://leader"
      ~chain_id:"octra-test"
      ~validator:"oct_validator"
      ~validator_pubkey:"pub"
      ~priv_b64:(Base64.encode_exn secret)
      ~ready_epoch:42L
      ~state_root:(hex_of_raw (raw 'r'))
      ~records_verified:7
      ~generated_at:10.0
  in
  let module U = Yojson.Safe.Util in
  expect "marker path" (marker.path = Filename.concat "data" "ready_to_vote.json");
  expect "marker staged" (marker.staged_path = marker.path ^ ".staged");
  expect "marker epoch" (marker.ready_epoch = 42L);
  expect "marker root" (marker.state_root = hex_of_raw (raw 'r'));
  expect "marker records" (marker.records_verified = 7);
  expect "marker payload epoch"
    (marker.payload |> U.member "ready_epoch" |> U.to_string = "42");
  expect "marker payload root"
    (marker.payload |> U.member "state_root" |> U.to_string = hex_of_raw (raw 'r'));
  expect "marker payload signature"
    (String.length (marker.payload |> U.member "signature" |> U.to_string) > 20);
  let payload_text = Join.ready_marker_payload_text marker in
  expect "marker payload newline" (String.ends_with ~suffix:"\n" payload_text);
  let events = ref [] in
  let deps : Join.ready_marker_write_deps = Join.{
    write_text = (fun ~path ~contents ->
      expect "marker write path" (path = marker.staged_path);
      expect "marker write contents" (contents = payload_text);
      events := "write" :: !events);
    rename = (fun ~src ~dst ->
      expect "marker rename src" (src = marker.staged_path);
      expect "marker rename dst" (dst = marker.path);
      events := "rename" :: !events);
    sync_dir = (fun ~path ->
      expect "marker sync path" (path = marker.path);
      events := "sync" :: !events);
    log_written = (fun logged ->
      expect "marker logged" (logged = marker);
      events := "log" :: !events);
  } in
  Join.write_ready_marker_with deps marker;
  expect "marker write order"
    (List.rev !events = ["write"; "rename"; "sync"; "log"])

let parse_one_record () =
  match Join.parse_range
          ~from_epoch:12L
          (`Assoc ["status", `String "ok"; "records", `List [record_json ()]]) with
  | Join.Records [record] -> record
  | Join.Retry -> fail "apply record range retried"
  | Join.Missing -> fail "apply record range missing"
  | Join.Records _ -> fail "unexpected apply record count"

let test_apply_records_success () =
  let cursor = cursor () in
  let record = parse_one_record () in
  let prepared =
    Join.prepare_record
      ~chain_id:"octra-test"
      ~expected_validator_set_hash:trusted_validator_set_hash
      ~cursor
      record
  in
  let root = ref "" in
  let eic = ref None in
  let events = ref [] in
  let deps = Join.{
    chain_id = "octra-test";
    expected_validator_set_hash = (fun _ ->
      Ok trusted_validator_set_hash);
    current_epoch = (fun () -> 12);
    put_proposer = (fun epoch proposer ->
      events := Printf.sprintf "proposer:%d:%s" epoch proposer.creator_addr :: !events);
    put_root = (fun epoch state_root ->
      events := Printf.sprintf "root:%d:%s" epoch state_root :: !events);
    stage_finality = (fun prepared ->
      events := Printf.sprintf "stage:%d" prepared.Join.epoch_int :: !events);
    promote_finality = (fun () ->
      events := "promote" :: !events);
    apply = (fun ~txs ~receipts_json ~proposer_info ~reward:_ ~epoch_ts
        ~validator_set:_ ~parent_commit ->
      expect "apply txs" (List.length txs = 2);
      expect "apply receipts" (receipts_json = []);
      expect "apply proposer" (proposer_info = prepared.proposer_info);
      expect "apply epoch ts" (epoch_ts = 120.25);
      expect "apply parent commit" (parent_commit = None);
      root := prepared.record.state_root;
      eic := Some prepared.expected_eic;
      events := "apply" :: !events;
      Lwt.return_unit);
    root = (fun () -> !root);
    eic = (fun () -> !eic);
  } in
  let next, count = Lwt_main.run (Join.apply_records deps ~cursor [record]) in
  expect "apply count" (count = 1);
  expect "apply next cursor" (next = prepared.next_cursor);
  expect "apply event"
    (List.rev !events = [
      "proposer:12:oct_creator";
      "root:12:" ^ prepared.record.state_root;
      "stage:12";
      "apply";
      "promote";
    ])

let test_apply_root_mismatch () =
  let cursor = cursor () in
  let record = parse_one_record () in
  let deps : Join.apply_deps = Join.{
    chain_id = "octra-test";
    expected_validator_set_hash = (fun _ ->
      Ok trusted_validator_set_hash);
    current_epoch = (fun () -> 12);
    put_proposer = (fun _ _ -> ());
    put_root = (fun _ _ -> ());
    stage_finality = (fun _ -> ());
    promote_finality = (fun () -> ());
    apply = (fun ~txs:_ ~receipts_json:_ ~proposer_info:_ ~reward:_ ~epoch_ts:_
        ~validator_set:_ ~parent_commit:_ ->
      Lwt.return_unit);
    root = (fun () -> hex_of_raw (raw 'x'));
    eic = (fun () -> None);
  } in
  try
    let _ = Lwt_main.run (Join.apply_records deps ~cursor [record]) in
    fail "root mismatch accepted"
  with Failure msg ->
    expect "root mismatch reason" (String.starts_with ~prefix:"join post-apply root mismatch" msg)

let head_json ~epoch ~root =
  `Assoc [
    "head_epoch", `String (Int64.to_string epoch);
    "state_root", `String root;
  ]

let range_json records =
  `Assoc [
    "status", `String "ok";
    "records", `List records;
  ]

let http_env = function
  | "OCTRA_JOIN_RPC" -> Some "http://primary,http://secondary"
  | "OCTRA_STATE_SYNC_SOURCES" -> Some "http://primary"
  | _ -> None

let test_http_heads () =
  let value epoch = head_json ~epoch ~root:(hex_of_raw (raw 's')) in
  let primary = Join.head_url "http://primary" in
  let secondary = Join.head_url "http://secondary" in
  let validator_hash _ = trusted_validator_set_hash in
  List.iter
    (fun (left, right, expected) ->
      let calls = ref [] in
      let fetch_json url =
        calls := url :: !calls;
        if url = primary then left ()
        else if url = secondary then right ()
        else
          let epoch =
            Uri.of_string url |> fun uri ->
            Uri.get_query_param uri "from_epoch" |> Option.get |> Int64.of_string
          in
          Lwt.return (range_json [record_json ~epoch ()])
      in
      let result =
        Lwt_main.run
          (Join.http_head ~fetch_json http_env ~chain_id:"octra-test"
             ~validator_hash ~after:0L)
      in
      expect "proved head maximum" (result = expected);
      expect "head sources once"
        (List.sort String.compare
           (List.filter (fun url -> url = primary || url = secondary) !calls)
         = [primary; secondary]))
    [
      (fun () -> Lwt.return (value 10L)), (fun () -> Lwt.return (value 40L)), Some 40L;
      (fun () -> Lwt.return (value 40L)), (fun () -> Lwt.return (value 10L)), Some 40L;
      (fun () -> Lwt.return (value 40L)), (fun () -> Lwt.return (value 40L)), Some 40L;
      (fun () -> Lwt.fail (Failure "offline")), (fun () -> Lwt.return (value 40L)), Some 40L;
      (fun () -> Lwt.return `Null), (fun () -> Lwt.return (value 40L)), Some 40L;
      (fun () -> Lwt.return (value (-1L))), (fun () -> Lwt.return (value 40L)), Some 40L;
      (fun () -> Lwt.return (value Int64.max_int)), (fun () -> Lwt.return (value 40L)), Some 40L;
      (fun () -> Lwt.return (value 40L)), (fun () -> Lwt.fail (Failure "offline")), Some 40L;
      (fun () -> Lwt.return `Null), (fun () -> Lwt.return `Null), None;
    ];
  expect "head disabled"
    (Lwt_main.run
       (Join.http_head ~fetch_json:(fun _ -> fail "head fetched without source")
          (fun _ -> None) ~chain_id:"octra-test" ~validator_hash ~after:0L) = None);
  List.iter
    (fun proof ->
      let fetch_json url =
        if url = primary then Lwt.return (value 40L)
        else if url = secondary then Lwt.return (value 12L)
        else if String.starts_with ~prefix:"http://primary/" url then
          Lwt.return proof
        else Lwt.return (range_json [record_json ()])
      in
      expect "unproved high head ignored"
        (Lwt_main.run
           (Join.http_head ~fetch_json http_env ~chain_id:"octra-test"
              ~validator_hash ~after:0L) = Some 12L))
    [`Null; range_json []; range_json [record_json ()];
     range_json [record_json ~epoch:40L ~state:(raw 'x') ()]];
  expect "head below local avoids range"
    (Lwt_main.run
       (Join.http_head ~fetch_json:(fun url ->
          expect "no old head proof fetch" (url = primary || url = secondary);
          Lwt.return (value 12L))
          http_env ~chain_id:"octra-test" ~validator_hash ~after:12L) = None)

let test_http_prefix () =
  let env = function "OCTRA_JOIN_RPC" -> Some "http://primary" | _ -> None in
  let first = record_json () in
  let second = record_json ~epoch:13L ~prev:(raw 's') ~state:(raw 't') () in
  let third = record_json ~epoch:14L ~prev:(raw 't') ~state:(raw 'u') () in
  let validator_hash epoch =
    if epoch <= 13L then trusted_validator_set_hash else raw 'x'
  in
  let run ~far records =
    let calls = ref [] in
    let fetch_json url =
      calls := url :: !calls;
      if url = Join.head_url "http://primary" then
        Lwt.return (head_json ~epoch:40L ~root:(hex_of_raw (raw 's')))
      else if url = Join.range_url "http://primary" ~from_epoch:40L ~max_epochs:1 then
        far ()
      else if url = Join.range_url "http://primary" ~from_epoch:12L ~max_epochs:16 then
        Lwt.return (range_json records)
      else fail "unexpected prefix request"
    in
    let result =
      Lwt_main.run
        (Join.http_head ~fetch_json env ~chain_id:"octra-test" ~validator_hash ~after:11L)
    in
    expect "prefix request count" (List.length !calls = 3);
    result
  in
  List.iter
    (fun far ->
      expect "known validator prefix advances"
        (run ~far [first; second; third] = Some 13L))
    [(fun () -> Lwt.return (range_json [record_json ~epoch:40L ()]));
     (fun () -> Lwt.return (range_json []));
     (fun () -> Lwt.return `Null);
     (fun () -> Lwt.fail (Failure "range unavailable"))];
  let read records = run ~far:(fun () -> Lwt.return (range_json [])) records in
  expect "prefix empty" (read [] = None);
  expect "prefix must begin at next epoch" (read [second] = None);
  expect "prefix requires local validator set" (read [third] = None);
  expect "prefix stops at epoch gap" (read [first; third] = Some 12L);
  expect "prefix stops at root gap"
    (read [first; record_json ~epoch:13L ~prev:(raw 'x') ()] = Some 12L);
  expect "prefix count limit" (read (List.init 17 (fun _ -> first)) = None)

let test_head_proof () =
  let record =
    match Join.parse_range ~from_epoch:12L (range_json [record_json ()]) with
    | Join.Records [record] -> record
    | _ -> fail "head proof record"
  in
  let head = Join.{ epoch = 12L; root = hex_of_raw (raw 's') } in
  let check ?(chain_id = "octra-test") ?(hash = trusted_validator_set_hash) head record =
    Join.proved_head ~chain_id ~validator_hash:(fun _ -> hash) head record
  in
  expect "head proof accepted" (check head record);
  expect "head proof chain" (not (check ~chain_id:"other" head record));
  expect "head proof validator set" (not (check ~hash:(raw 'x') head record));
  expect "head proof epoch" (not (check { head with epoch = 13L } record));
  expect "head proof root" (not (check { head with root = hex_of_raw (raw 'x') } record));
  let finalize = record.finality.finalize in
  let finality = { record.finality with
    finalize = { finalize with precommits = [] } } in
  expect "head proof quorum" (not (check head { record with finality }));
  let votes =
    List.map
      (fun (vote : Octra_consensus.C_types.vote) ->
        { vote with signature = String.make 64 '\000' })
      finalize.precommits
  in
  let finality = { record.finality with
    finalize = { finalize with precommits = votes } } in
  expect "head proof signature" (not (check head { record with finality }))

let test_http_range_sources () =
  let module Shell = Octra_node_runtime.Consensus_catchup_shell in
  let valid = range_json [record_json ()] in
  let wrong_epoch =
    match record_json () with
    | `Assoc fields ->
      `Assoc (("epoch_id", `String "11") :: List.remove_assoc "epoch_id" fields)
    | _ -> fail "record shape"
  in
  List.iter
    (fun first ->
      let calls = ref [] in
      let fetch_json url =
        calls := url :: !calls;
        if String.starts_with ~prefix:"http://primary/" url then first ()
        else Lwt.return valid
      in
      let deps = Shell.{
        env_timeout = (fun () -> Some "1");
        read_query_root = (fun () -> Lwt.return (raw 'p'));
        range_query = {
          sleep = (fun _ -> Lwt.return_unit);
          query_range = (fun ~from_epoch:_ ~max_epochs:_ ~timeout_seconds:_
              ~validate:_ -> Lwt.return_none);
          http_range = Join.http_range ~fetch_json http_env;
        };
      } in
      let result =
        Lwt_main.run
          (Shell.query_chunk deps ~target_epoch:13L ~from_epoch:12L ~reason:"test")
      in
      begin
        match result with
        | Shell.Query_chunk response ->
          expect "secondary selected" (response.responder_addr = "http://secondary");
          expect "range remains exact"
            (List.map (fun r -> r.Octra_consensus.C_codec.epoch_id) response.records = [12L])
        | Shell.Query_failed _ -> fail "secondary range not selected"
      end;
      expect "range source order"
        (List.rev !calls =
         [Join.range_url "http://primary" ~from_epoch:12L ~max_epochs:16;
          Join.range_url "http://secondary" ~from_epoch:12L ~max_epochs:16]))
    [
      (fun () -> Lwt.return (`Assoc ["status", `String "not_found"]));
      (fun () -> Lwt.return (range_json []));
      (fun () -> Lwt.return (`Assoc ["status", `String "retry"]));
      (fun () -> Lwt.return `Null);
      (fun () -> Lwt.fail (Failure "offline"));
      (fun () -> Lwt.return (range_json [wrong_epoch]));
      (fun () -> Lwt.return (range_json (List.init 17 (fun _ -> record_json ()))));
      (fun () -> Lwt.return (range_json [record_json ~prev:(raw 'x') ()]));
      (fun () -> Lwt.return (range_json [record_json ~tx_hashes:[] ~txs_json:["invalid"] ()]));
    ];
  let calls = ref 0 in
  let fetch_json _ = incr calls; Lwt.return valid in
  let read validate =
    Lwt_main.run
      (Join.http_range ~fetch_json http_env ~from_epoch:12L ~max_epochs:1 ~validate)
  in
  expect "valid primary retained" (Option.is_some (read (fun _ -> true)));
  expect "unused secondary not fetched" (!calls = 1);
  calls := 0;
  expect "all rejected" (read (fun _ -> false) = None);
  expect "all sources checked" (!calls = 2);
  expect "range disabled"
    (Lwt_main.run
       (Join.http_range ~fetch_json:(fun _ -> fail "range fetched without source")
          (fun _ -> None) ~from_epoch:12L ~max_epochs:1 ~validate:(fun _ -> true)) = None)

let test_http_parts () =
  let module Parts = Octra_bootstrap.Range_part in
  let payload =
    match range_json [record_json ()] with
    | `Assoc fields ->
      `Assoc (("padding", `String (String.make Parts.body_max 'x')) :: fields)
    | _ -> fail "range shape"
  in
  let part index =
    match Parts.reply ~index payload with
    | Ok json -> json
    | Error error -> fail error
  in
  let first = part 0 and second = part 1 in
  let calls = ref 0 in
  let fetch_json url =
    incr calls;
    let json =
      if url = Join.range_url "http://primary" ~from_epoch:12L ~max_epochs:1 then first
      else if url = Join.range_url ~part:1 "http://primary" ~from_epoch:12L ~max_epochs:1 then second
      else fail "unexpected part source"
    in
    Lwt.map (fun () -> json) (Lwt_unix.sleep 0.2)
  in
  let result =
    Lwt_main.run
      (Join.http_range ~fetch_json ~timeout:0.3 http_env ~from_epoch:12L
         ~max_epochs:1 ~validate:(fun _ -> true))
  in
  expect "each part has its own deadline" (Option.is_some result && !calls = 2);
  let excess =
    match first with
    | `Assoc fields ->
      `Assoc (("count", `Int (Parts.part_max + 1)) :: List.remove_assoc "count" fields)
    | _ -> fail "part shape"
  in
  calls := 0;
  let fetch_json url =
    incr calls;
    if url = Join.range_url "http://primary" ~from_epoch:12L ~max_epochs:1 then
      Lwt.return excess
    else if url = Join.range_url "http://secondary" ~from_epoch:12L ~max_epochs:1 then
      Lwt.return (range_json [record_json ()])
    else fail "excess part requested"
  in
  let result =
    Lwt_main.run
      (Join.http_range ~fetch_json http_env ~from_epoch:12L ~max_epochs:1
         ~validate:(fun _ -> true))
  in
  expect "part count checked before collection"
    (Option.is_some result && !calls = 2)

let test_http_deadlines () =
  let run head_only =
    let canceled = ref false in
    let fetch_json url =
      if String.starts_with ~prefix:"http://primary/" url then begin
        let pending, _ = Lwt.task () in
        Lwt.on_cancel pending (fun () -> canceled := true);
        pending
      end else if head_only && url = Join.head_url "http://secondary" then
        Lwt.return (head_json ~epoch:40L ~root:(hex_of_raw (raw 's')))
      else if head_only then
        Lwt.return (range_json [record_json ~epoch:40L ()])
      else
        Lwt.return (range_json [record_json ()])
    in
    if head_only then
      expect "head timeout keeps secondary"
        (Lwt_main.run
           (Join.http_head ~fetch_json ~timeout:0.01 http_env
              ~chain_id:"octra-test" ~validator_hash:(fun _ -> trusted_validator_set_hash)
              ~after:0L) = Some 40L)
    else
      expect "range timeout keeps secondary"
        (Option.is_some
           (Lwt_main.run
              (Join.http_range ~fetch_json ~timeout:0.01 http_env
                 ~from_epoch:12L ~max_epochs:1 ~validate:(fun _ -> true))));
    expect "request canceled on timeout" !canceled
  in
  run true;
  run false;
  List.iter
    (fun head_only ->
      let calls = ref 0 in
      let fetch_json _ = incr calls; Lwt.fail Lwt.Canceled in
      let task () =
        if head_only then
          Lwt.map ignore
            (Join.http_head ~fetch_json http_env ~chain_id:"octra-test"
               ~validator_hash:(fun _ -> trusted_validator_set_hash) ~after:0L)
        else
          Lwt.map ignore
            (Join.http_range ~fetch_json http_env ~from_epoch:12L ~max_epochs:1
               ~validate:(fun _ -> true))
      in
      let canceled =
        Lwt_main.run
          (Lwt.catch
             (fun () -> Lwt.map (fun () -> false) (task ()))
             (function Lwt.Canceled -> Lwt.return_true | exn -> Lwt.fail exn))
      in
      expect "request cancellation preserved" canceled;
      if not head_only then expect "canceled range stops iteration" (!calls = 1))
    [true; false]

let test_run_catchup_ready () =
  let writes = ref [] in
  let starts = ref [] in
  let deps = Join.{
    fetch_head = (fun base ->
      expect "ready base" (base = "http://leader");
      Lwt.return (head_json ~epoch:11L ~root:(hex_of_raw (raw 'r'))));
    fetch_range = (fun _ ~from_epoch:_ ~max_epochs:_ -> fail "ready fetched range");
    local_next = (fun () -> 12L);
    local_root = (fun () -> hex_of_raw (raw 'r'));
    cursor = (fun ~from_epoch:_ -> fail "ready cursor");
    apply_range = (fun ~cursor:_ _ -> fail "ready apply");
    write_ready = (fun ~base ~ready_epoch ~state_root ~records_verified ->
      writes := (base, ready_epoch, state_root, records_verified) :: !writes);
    sleep = (fun _ -> fail "ready sleep");
    log_start = (fun ~base -> starts := base :: !starts);
    log_applied = (fun ~applied:_ -> fail "ready applied");
    log_retry = (fun ~phase:_ ~delay:_ ~error:_ -> fail "ready retry");
  } in
  expect "ready outcome"
    (synced (Lwt_main.run (Join.run_catchup deps "http://leader/")));
  expect "ready start" (!starts = ["http://leader"]);
  expect "ready not written" (!writes = [])

let test_run_catchup_stale_leader () =
  let deps = Join.{
    fetch_head = (fun _ ->
      Lwt.return (head_json ~epoch:11L ~root:(hex_of_raw (raw 'r'))));
    fetch_range = (fun _ ~from_epoch:_ ~max_epochs:_ -> fail "stale fetched range");
    local_next = (fun () -> 13L);
    local_root = (fun () -> hex_of_raw (raw 's'));
    cursor = (fun ~from_epoch:_ -> fail "stale cursor");
    apply_range = (fun ~cursor:_ _ -> fail "stale apply");
    write_ready = (fun ~base:_ ~ready_epoch:_ ~state_root:_ ~records_verified:_ ->
      fail "stale ready");
    sleep = (fun _ -> fail "stale sleep");
    log_start = (fun ~base:_ -> ());
    log_applied = (fun ~applied:_ -> fail "stale applied");
    log_retry = (fun ~phase:_ ~delay:_ ~error:_ -> fail "stale retry");
  } in
  match Lwt_main.run (Join.run_catchup deps "http://leader") with
  | Join.Leader_stale p ->
    expect "stale local" (p.local_head = 12L);
    expect "stale leader" (p.leader_head = 11L)
  | Join.Synced _ -> fail "stale leader synced"
  | Join.Source_unavailable _ -> fail "stale leader source unavailable"

let test_catchup_retry_apply_ready () =
  let record = parse_one_record () in
  let local_next = ref 12L in
  let local_root = ref (hex_of_raw (raw 'p')) in
  let range_calls = ref 0 in
  let sleeps = ref 0 in
  let retries = ref 0 in
  let applied = ref [] in
  let ready = ref None in
  let deps = Join.{
    fetch_head = (fun _ ->
      Lwt.return (head_json ~epoch:12L ~root:(hex_of_raw (raw 's'))));
    fetch_range = (fun _ ~from_epoch ~max_epochs ->
      incr range_calls;
      expect "fetch epoch" (from_epoch = 12L);
      expect "fetch max" (max_epochs = 16);
      if !range_calls = 1 then
        Lwt.return (range_json [])
      else
        Lwt.return (range_json [record_json ()]));
    local_next = (fun () -> !local_next);
    local_root = (fun () -> !local_root);
    cursor = (fun ~from_epoch ->
      expect "cursor epoch" (from_epoch = 12L);
      cursor ());
    apply_range = (fun ~cursor records ->
      expect "records" (records = [record]);
      let prepared =
        Join.prepare_record
          ~chain_id:"octra-test"
          ~expected_validator_set_hash:trusted_validator_set_hash
          ~cursor
          record
      in
      local_next := 13L;
      local_root := prepared.record.state_root;
      applied := prepared.record.epoch_id :: !applied;
      Lwt.return (prepared.next_cursor, 1));
    write_ready = (fun ~base ~ready_epoch ~state_root ~records_verified ->
      ready := Some (base, ready_epoch, state_root, records_verified));
    sleep = (fun seconds ->
      expect "sleep seconds" (seconds = 1.0);
      incr sleeps;
      Lwt.return_unit);
    log_start = (fun ~base -> expect "start base" (base = "http://leader"));
    log_applied = (fun ~applied:n -> applied := Int64.of_int n :: !applied);
    log_retry = (fun ~phase ~delay ~error ->
      expect "retry phase" (phase = "range_empty");
      expect "retry delay" (delay = 1.0);
      expect "retry reason" (error = "source returned no records");
      incr retries);
  } in
  expect "retry outcome"
    (synced (Lwt_main.run (Join.run_catchup deps "http://leader/")));
  expect "range retries" (!range_calls = 2);
  expect "sleep count" (!sleeps = 1);
  expect "retry count" (!retries = 1);
  expect "apply log" (!applied = [1L; 12L]);
  expect "ready after apply"
    (!ready = Some ("http://leader", 12L, hex_of_raw (raw 's'), 1))

let test_run_catchup_transport_retry () =
  let head_calls = ref 0 in
  let range_calls = ref 0 in
  let sleeps = ref [] in
  let retries = ref [] in
  let deps = Join.{
    fetch_head = (fun _ ->
      incr head_calls;
      if !head_calls = 1 then
        Lwt.fail (Unix.Unix_error (Unix.ETIMEDOUT, "connect", ""))
      else
        Lwt.return (head_json ~epoch:12L ~root:(hex_of_raw (raw 's'))));
    fetch_range = (fun _ ~from_epoch:_ ~max_epochs:_ ->
      incr range_calls;
      if !range_calls = 1 then
        Lwt.fail Lwt_unix.Timeout
      else
        Lwt.return (`Assoc ["status", `String "not_found"]));
    local_next = (fun () -> 12L);
    local_root = (fun () -> hex_of_raw (raw 'p'));
    cursor = (fun ~from_epoch:_ -> fail "transport cursor");
    apply_range = (fun ~cursor:_ _ -> fail "transport apply");
    write_ready = (fun ~base:_ ~ready_epoch:_ ~state_root:_ ~records_verified:_ ->
      fail "transport ready");
    sleep = (fun delay -> sleeps := delay :: !sleeps; Lwt.return_unit);
    log_start = (fun ~base:_ -> ());
    log_applied = (fun ~applied:_ -> fail "transport applied");
    log_retry = (fun ~phase ~delay ~error:_ ->
      retries := (phase, delay) :: !retries);
  } in
  match Lwt_main.run (Join.run_catchup deps "http://leader") with
  | Join.Source_unavailable gap ->
    expect "transport phase" (gap.phase = "range_missing");
    expect "transport head calls" (!head_calls = 3);
    expect "transport range calls" (!range_calls = 2);
    expect "transport retry log"
      (List.rev !retries = ["head", 1.0; "range", 2.0]);
    expect "transport sleeps"
      (List.rev !sleeps = [1.0; 2.0])
  | Join.Synced _ -> fail "transport unexpectedly synced"
  | Join.Leader_stale _ -> fail "transport leader became stale"

let test_run_node_catchup_ready () =
  let fetched = ref [] in
  let ready = ref None in
  let deps = Join.{
    chain_id = "octra-test";
    expected_validator_set_hash = (fun _ ->
      Ok trusted_validator_set_hash);
    fetch_json = (fun url ->
      fetched := url :: !fetched;
      Lwt.return (head_json ~epoch:11L ~root:(hex_of_raw (raw 'r'))));
    current_epoch = (fun () -> 12);
    local_root = (fun () -> hex_of_raw (raw 'r'));
    base_eic_root = (fun () -> fail "ready base eic");
    next_txid = (fun () -> fail "ready txid");
    put_proposer = (fun _ _ -> fail "ready proposer");
    put_root = (fun _ _ -> fail "ready root");
    stage_finality = (fun _ -> fail "ready finality stage");
    promote_finality = (fun () -> fail "ready finality promote");
    apply = (fun ~txs:_ ~receipts_json:_ ~proposer_info:_ ~reward:_ ~epoch_ts:_
        ~validator_set:_ ~parent_commit:_ ->
      fail "ready apply");
    local_eic = (fun () -> fail "ready eic");
    write_ready = (fun ~base ~ready_epoch ~state_root ~records_verified ->
      ready := Some (base, ready_epoch, state_root, records_verified));
    sleep = (fun _ -> fail "ready sleep");
    now = (fun () -> 10.0);
  } in
  expect "node ready outcome"
    (synced (Lwt_main.run (Join.run_node_catchup deps "http://leader/")));
  expect "node fetch head"
    (!fetched = ["http://leader/state-sync/head"]);
  expect "node ready not written" (!ready = None)

let test_node_catchup_apply () =
  let local_next = ref 12 in
  let local_root = ref (hex_of_raw (raw 'p')) in
  let eic = ref None in
  let fetched = ref [] in
  let ready = ref None in
  let events = ref [] in
  let deps = Join.{
    chain_id = "octra-test";
    expected_validator_set_hash = (fun _ ->
      Ok trusted_validator_set_hash);
    fetch_json = (fun url ->
      fetched := url :: !fetched;
      if String.ends_with ~suffix:"/state-sync/head" url then
        Lwt.return (head_json ~epoch:12L ~root:(hex_of_raw (raw 's')))
      else
        Lwt.return (range_json [record_json ()]));
    current_epoch = (fun () -> !local_next);
    local_root = (fun () -> !local_root);
    base_eic_root = (fun () -> Octra_core.Epoch_index_commitment.genesis_root);
    next_txid = (fun () -> 7L);
    put_proposer = (fun epoch proposer ->
      events := Printf.sprintf "proposer:%d:%s" epoch proposer.creator_addr :: !events);
    put_root = (fun epoch root ->
      events := Printf.sprintf "root:%d:%s" epoch root :: !events);
    stage_finality = (fun prepared ->
      events := Printf.sprintf "stage:%d" prepared.Join.epoch_int :: !events);
    promote_finality = (fun () ->
      events := "promote" :: !events);
    apply = (fun ~txs ~receipts_json ~proposer_info ~reward:_ ~epoch_ts
        ~validator_set:_ ~parent_commit ->
      expect "node apply txs" (List.length txs = 2);
      expect "node apply receipts" (receipts_json = []);
      expect "node apply epoch ts" (epoch_ts = 120.25);
      expect "node apply proposer"
        (proposer_info =
         Some { Octra_core.Epochlog.creator_addr = "oct_creator"; commit_round = 4 });
      expect "node apply parent commit" (parent_commit = None);
      local_next := 13;
      local_root := hex_of_raw (raw 's');
      let prepared =
        Join.prepare_record
          ~chain_id:"octra-test"
          ~expected_validator_set_hash:trusted_validator_set_hash
          ~cursor:(cursor ())
          (parse_one_record ())
      in
      eic := Some prepared.expected_eic;
      events := "apply" :: !events;
      Lwt.return_unit);
    local_eic = (fun () -> !eic);
    write_ready = (fun ~base ~ready_epoch ~state_root ~records_verified ->
      ready := Some (base, ready_epoch, state_root, records_verified));
    sleep = (fun _ -> fail "node apply sleep");
    now = (fun () -> 10.0);
  } in
  expect "node apply outcome"
    (synced (Lwt_main.run (Join.run_node_catchup deps "http://leader")));
  expect "node fetched head/range/head"
    (!fetched = [
      "http://leader/state-sync/head";
      "http://leader/state-sync/range?from_epoch=12&max_epochs=16";
      "http://leader/state-sync/head";
    ]);
  expect "node apply events"
    (List.rev !events = [
      "proposer:12:oct_creator";
      "root:12:" ^ hex_of_raw (raw 's');
      "stage:12";
      "apply";
      "promote";
    ]);
  expect "node apply ready"
    (!ready = Some ("http://leader", 12L, hex_of_raw (raw 's'), 1))

let runtime_deps ?(env = fun _ -> None) ?(head = fun () -> None) touched =
  Join.{
    env;
    expected_validator_set_hash = (fun _ ->
      Ok trusted_validator_set_hash);
    fetch_json = (fun _ -> touched := true; Lwt.return `Null);
    current_epoch = (fun () -> touched := true; 12);
    head;
    next_txid = (fun () -> touched := true; 7L);
    put_proposer = (fun _ _ -> touched := true);
    put_root_raw = (fun _ _ -> touched := true);
    write_entry = (fun _ -> touched := true);
    apply = (fun ~txs:_ ~receipts_json:_ ~proposer_info:_ ~reward:_ ~epoch_ts:_
        ~validator_set:_ ~parent_commit:_ ->
      touched := true;
      Lwt.return_unit);
    sleep = (fun _ -> touched := true; Lwt.return_unit);
    now = (fun () -> touched := true; 10.0);
    data_dir = "data";
    consensus_role = "observer";
    chain_id = "octra-test";
    validator = "oct_validator";
    validator_pubkey = "pub";
    priv_b64 = Base64.encode_exn (raw 'k');
  }

let test_node_deps_head_roots () =
  let touched = ref false in
  let proposer_seen = ref None in
  let root_seen = ref None in
  let runtime = {
    (runtime_deps touched) with
    head = (fun () ->
      Some (head
        ~ledger_state_root:"ledger"
        ~epoch_index_root:"eic-root"
        ~state_root:(hex_of_raw (raw 'r') ^ "tail")
        ()));
    next_txid = (fun () -> 99L);
    put_proposer = (fun epoch proposer ->
      proposer_seen := Some (epoch, proposer.Octra_core.Epochlog.creator_addr));
    put_root_raw = (fun epoch root ->
      root_seen := Some (epoch, root));
  } in
  let deps = Join.node_deps_of_runtime runtime in
  expect "runtime chain" (deps.chain_id = "octra-test");
  expect "runtime local root" (deps.local_root () = hex_of_raw (raw 'r'));
  expect "runtime base eic" (deps.base_eic_root () = "eic-root");
  expect "runtime local eic" (deps.local_eic () = Some "eic-root");
  expect "runtime txid" (deps.next_txid () = 99L);
  deps.put_proposer 12 { Octra_core.Epochlog.creator_addr = "oct_creator"; commit_round = 4 };
  deps.put_root 12 (hex_of_raw (raw 'q'));
  expect "runtime proposer" (!proposer_seen = Some (12, "oct_creator"));
  expect "runtime root raw" (!root_seen = Some (12, raw 'q'));
  expect "runtime untouched callbacks" (not !touched)

let test_node_deps_finality () =
  let state = Octra_node_runtime.Consensus_finality_state.create () in
  let touched = ref false in
  let runtime =
    Join.node_runtime_deps Join.{
      env = (fun _ -> None);
      expected_validator_set_hash = (fun _ ->
        Ok trusted_validator_set_hash);
      fetch_json = (fun _ -> touched := true; Lwt.return `Null);
      current_epoch = (fun () -> touched := true; 12);
      head = (fun () -> None);
      next_txid = (fun () -> touched := true; 7L);
      finality = Octra_node_runtime.Consensus_finality_state.callbacks state;
      write_entry = (fun _ -> touched := true);
      apply = (fun ~txs:_ ~receipts_json:_ ~proposer_info:_ ~reward:_ ~epoch_ts:_
          ~validator_set:_ ~parent_commit:_ ->
        touched := true;
        Lwt.return_unit);
      sleep = (fun _ -> touched := true; Lwt.return_unit);
      now = (fun () -> touched := true; 10.0);
      data_dir = "data";
      consensus_role = "observer";
      chain_id = "octra-test";
      validator = "oct_validator";
      validator_pubkey = "pub";
      priv_b64 = Base64.encode_exn (raw 'k');
    }
  in
  let proposer = {
    Octra_core.Epochlog.creator_addr = "oct_creator";
    commit_round = 4;
  } in
  runtime.put_proposer 15 proposer;
  runtime.put_root_raw 15 (raw 'z');
  expect "runtime finality proposer"
    (Octra_node_runtime.Consensus_finality_state.find_proposer state 15 =
     Some proposer);
  expect "runtime finality root"
    (Octra_node_runtime.Consensus_finality_state.find_expected_root state 15 =
     Some (raw 'z'));
  expect "runtime finality no unrelated effect" (not !touched)

let test_catchup_no_env () =
  let touched = ref false in
  ignore (Lwt_main.run (Join.run_configured_node_catchup (runtime_deps touched)));
  expect "join disabled no side effects" (not !touched)

let test_wiring_no_env () =
  let touched = ref false in
  let state = Octra_node_runtime.Consensus_finality_state.create () in
  ignore (Lwt_main.run
    (Join.run_configured_node_wiring Join.{
       env = (fun _ -> None);
       expected_validator_set_hash = (fun _ ->
         Ok trusted_validator_set_hash);
       fetch_json = (fun _ -> touched := true; Lwt.return `Null);
       current_epoch = (fun () -> touched := true; 12);
       head = (fun () -> None);
       next_txid = (fun () -> touched := true; 7L);
       finality = Octra_node_runtime.Consensus_finality_state.callbacks state;
       write_entry = (fun _ -> touched := true);
       apply = (fun ~txs:_ ~receipts_json:_ ~proposer_info:_ ~reward:_ ~epoch_ts:_
           ~validator_set:_ ~parent_commit:_ ->
         touched := true;
         Lwt.return_unit);
       sleep = (fun _ -> touched := true; Lwt.return_unit);
       now = (fun () -> touched := true; 10.0);
       data_dir = "data";
       consensus_role = "observer";
       chain_id = "octra-test";
       validator = "oct_validator";
       validator_pubkey = "pub";
       priv_b64 = Base64.encode_exn (raw 'k');
     }));
  expect "join wiring disabled no side effects" (not !touched)

let test_catchup_sources_exhausted () =
  let touched = ref false in
  let ranges = ref 0 in
  let sleeps = ref 0 in
  let local_head =
    head ~state_root:(hex_of_raw (raw 'p')) ()
  in
  let deps =
    {
      (runtime_deps
         ~env:(function
           | "OCTRA_STATE_SYNC_SOURCES" -> Some "http://leader,http://reserve"
           | _ -> None)
         ~head:(fun () -> Some local_head)
         touched) with
      fetch_json = (fun url ->
        if String.ends_with ~suffix:"/state-sync/head" url then
          Lwt.return (head_json ~epoch:12L ~root:(hex_of_raw (raw 's')))
        else begin
          incr ranges;
          Lwt.return (`Assoc ["status", `String "not_found"])
        end);
      current_epoch = (fun () -> 12);
      sleep = (fun _ -> incr sleeps; Lwt.return_unit);
    }
  in
  let result = Lwt_main.run (Join.run_configured_node_catchup deps) in
  expect "missing range rejected" (result = None);
  expect "missing range confirmations" (!ranges = 6);
  expect "missing range waits" (!sleeps = 4)

let () =
  test_normalize_base ();
  test_http_get_json ();
  test_retry_delay ();
  test_head_roots ();
  test_parse_head ();
  test_parse_range_retry ();
  test_http_heads ();
  test_http_prefix ();
  test_head_proof ();
  test_http_range_sources ();
  test_http_parts ();
  test_http_deadlines ();
  test_sync_plan ();
  test_prepare_record ();
  test_prepare_hash_mismatch ();
  test_prepare_binds_legacy_reward ();
  test_ready_marker ();
  test_apply_records_success ();
  test_apply_root_mismatch ();
  test_run_catchup_ready ();
  test_run_catchup_stale_leader ();
  test_catchup_retry_apply_ready ();
  test_run_catchup_transport_retry ();
  test_run_node_catchup_ready ();
  test_node_catchup_apply ();
  test_node_deps_head_roots ();
  test_node_deps_finality ();
  test_catchup_no_env ();
  test_wiring_no_env ();
  test_catchup_sources_exhausted ();
  print_endline "status = pass test = node_runtime_consensus_join_rpc"