(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Sync = Octra_bootstrap.State_sync
module Store = Octra_core.Store_chaindata
module Index = Octra_core.Chaindata_index
module Epoch = Octra_core.Epochlog
module Tx = Octra_core.Transaction
module C = Octra_consensus.C_types
module Codec = Octra_consensus.C_codec
module H = Octra_consensus.C_hash
module Range = Octra_node_runtime.Sync_range
module Reader = Octra_node_runtime.Sync_range_read
module Parts = Octra_bootstrap.Range_part
module Head = Octra_core.Head_manifest

let require condition reason =
  if not condition then failwith reason

let get = function
  | Ok value -> value
  | Error reason -> failwith reason

let chain_id = "octra-sync-range-test"
let private_key = String.make 32 '\027'

let public_key =
  match Mirage_crypto_ec.Ed25519.priv_of_octets private_key with
  | Error _ -> failwith "invalid test key"
  | Ok key -> Mirage_crypto_ec.Ed25519.(pub_of_priv key |> pub_to_octets)

let address =
  Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn public_key)

let validators = C.make_validator_set [C.{ address; pubkey = public_key }]

let reward = C.{
  reward_proposer_addr = address;
  reward_proposer_public_key = Some public_key;
  reward_members = [{
    reward_address = address;
    reward_public_key = Some public_key;
    reward_weight = Z.one;
  }];
}

let transaction nonce =
  Tx.of_yojson (`Assoc [
    "from", `String address;
    "to_", `String address;
    "amount", `String "1";
    "nonce", `Int nonce;
    "ou", `String "1000";
    "timestamp", `Float 1.;
    "signature", `String "sig";
    "op_type", `String "standard";
  ]) |> get

let finality (epoch : Epoch.epoch_header) hashes =
  let epoch_id = Int64.of_int epoch.id in
  let header = C.{
    proto_version = proto_version_current;
    chain_id;
    epoch_id;
    prev_state_root = Sync.hex_to_raw32 epoch.prev_state_root;
    tx_list_hash = Octra_net.Hash_domain.hash "octra:tx_list:v1"
      (String.concat "" hashes);
    receipt_root = H.receipt_root [];
    proposed_state_root = Sync.hex_to_raw32 epoch.state_root;
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = address;
    txid_hi = Int64.pred (Int64.add epoch.start_txid (Int64.of_int epoch.tx_count));
    ts = epoch.finalized_at;
  } in
  let proposal_id = H.proposal_id header in
  let vote = C.{
    chain_id;
    epoch_id;
    round = 0;
    vote_type = Precommit;
    proposal_id;
    validator = address;
    signature = String.make 64 '\000';
  } in
  let vote = C.{ vote with signature = H.sign_ed25519 ~priv_raw:private_key
    ~msg:(H.vote_sign_bytes vote) } in
  Codec.{
    finalize = C.{
      chain_id;
      epoch_id;
      commit_round = 0;
      header;
      proposal_id;
      precommits = [vote];
      parent_commit = None;
    };
    validator_set = validators;
  }

let with_store ?(txs = List.init 2 (fun index -> transaction (index + 1))) action =
  Test_workspace.with_dir "sync_range" (fun root ->
    let store = Store.open_chaindata (Filename.concat root "chaindata") in
    Fun.protect ~finally:(fun () -> Store.close store) (fun () ->
      let hashes = List.map Tx.hash txs in
      let epoch = Epoch.{ empty_epoch_header with
        id = 12;
        prev_state_root = String.make 64 'a';
        state_root = String.make 64 'b';
        tx_count = List.length txs;
        proposer = { creator_addr = address; commit_round = 0 };
        finalized_at = 120.;
        reward_source = Some reward;
      } in
      Store.begin_batch store;
      List.iter2 (fun tx hash ->
        Store.save_tx store ~hash ~epoch_id:epoch.id ~from_addr:address
          ~to_addr:address ~tx_json:(Tx.to_yojson tx |> Yojson.Safe.to_string)
          ~op_type:"standard" ~encrypted_data:"" ~message:"") txs hashes;
      Store.set_epoch store epoch;
      Store.commit_batch store;
      ignore (Unix.lseek store.txlog.current_fd 0 Unix.SEEK_SET);
      action store epoch (finality epoch hashes)))

let read store epoch read_finality =
  let reasons = ref [] in
  let rewards = ref 0 in
  let reply = Sync.build_range ~chain_id ~data_dir:None ~chaindata:store
    ~on_stop:(fun ~epoch ~reason -> reasons := (epoch, reason) :: !reasons)
    ~reward_source:(fun _ _ -> incr rewards; Ok reward)
    ~read_finality ~from_epoch:(Int64.of_int epoch) ~max_epochs:1 in
  reply, List.rev !reasons, !rewards

let position store =
  Unix.lseek store.Store.txlog.current_fd 0 Unix.SEEK_CUR

let test_missing_proof () =
  with_store (fun store epoch _ ->
    let calls = ref [] in
    let reply, reasons, rewards = read store epoch.id (fun id ->
      calls := id :: !calls;
      None) in
    require (reply = `NotFound) "missing proof returned records";
    require (!calls = [epoch.id]) "missing proof was not checked once";
    require (reasons = [12L, "finality_missing"]) "missing proof reason differs";
    require (rewards = 0) "missing proof read rewards";
    require (position store = 0) "missing proof read transaction log")

let test_missing_transaction () =
  with_store (fun store epoch proof ->
    Index.remove_txid_loc_direct (Store.index store) 0L;
    let reply, reasons, rewards = read store epoch.id (fun _ -> Some proof) in
    require (reply = `NotFound) "missing transaction returned records";
    require (reasons = [12L, "transaction_missing"]) "missing transaction reason differs";
    require (rewards = 0) "missing transaction read rewards";
    require (position store = 0) "missing transaction did not stop reads")

let test_valid () =
  with_store (fun store epoch proof ->
    let reply, reasons, rewards = read store epoch.id (fun _ -> Some proof) in
    require (reasons = [] && rewards = 1) "valid range read differs";
    match reply with
    | `Ok ([record], Some 13L) ->
      let rows = List.init 2 (fun id ->
        match Store.get_tx_by_txid store (Int64.of_int id) with
        | Some row -> row
        | None -> failwith "stored transaction missing") in
      let expected = Codec.{
        epoch_id = 12L;
        prev_state_root = proof.finalize.header.prev_state_root;
        state_root = proof.finalize.header.proposed_state_root;
        tx_list_hash = proof.finalize.header.tx_list_hash;
        tx_hashes = List.map fst rows;
        txs_json = List.map snd rows;
        receipt_root = proof.finalize.header.receipt_root;
        receipts_json = [];
        epoch_ts = epoch.finalized_at;
        creator_addr = address;
        commit_round = 0;
        reward_source = Some reward;
        finality = Some proof;
      } in
      require (record = expected) "signed range changed";
      require (Sync.record_json record = Sync.record_json expected)
        "range encoding changed"
    | _ -> failwith "valid range was not served")

let test_bad_proof () =
  with_store (fun store epoch proof ->
    let votes = List.map (fun (vote : C.vote) ->
      { vote with signature = String.make 64 '\000' }) proof.finalize.precommits in
    let proof = Codec.{ proof with finalize = { proof.finalize with precommits = votes } } in
    let reply, reasons, _ = read store epoch.id (fun _ -> Some proof) in
    require (reply = `NotFound) "invalid proof returned records";
    require (match reasons with
      | [12L, reason] -> String.starts_with ~prefix:"finality_invalid:" reason
      | _ -> false) "invalid proof was not verified")

let test_missing_epoch () =
  with_store (fun store epoch _ ->
    let reply, reasons, rewards = read store (epoch.id + 1)
      (fun _ -> failwith "missing epoch read finality") in
    require (reply = `NotFound) "missing epoch returned records";
    require (reasons = [13L, "epoch_missing"]) "missing epoch reason differs";
    require (rewards = 0 && position store = 0) "missing epoch read transactions")

let test_prefix () =
  with_store (fun store epoch proof ->
    let next = Epoch.{ epoch with id = 13; start_txid = 2L; tx_count = 0 } in
    Store.begin_batch store;
    Store.set_epoch store next;
    Store.commit_batch store;
    let calls = ref [] in
    let stops = ref [] in
    let reply = Sync.build_range ~chain_id ~data_dir:None ~chaindata:store
      ~on_stop:(fun ~epoch ~reason -> stops := (epoch, reason) :: !stops)
      ~reward_source:(fun _ _ -> Ok reward)
      ~read_finality:(fun id ->
        calls := id :: !calls;
        if id = epoch.id then Some proof else None)
      ~from_epoch:12L ~max_epochs:16 in
    require (!calls = [13; 12]) "range read past missing proof";
    require (!stops = [13L, "finality_missing"]) "prefix stop reason differs";
    require (match reply with
      | `Ok ([record], None) -> record.Codec.epoch_id = 12L
      | _ -> false) "verified prefix was lost")

let query epoch = Range.{
  from_epoch = Int64.of_int epoch;
  max_epochs = 1;
  part = None;
  hash = None;
  head = Octra_core.Head_manifest.get_cached ();
  pubkeys = [address, Base64.encode_exn public_key];
  activation = None;
}

let test_async () =
  with_store (fun store epoch proof ->
    let expected = Sync.range_json ~on_stop:(fun ~epoch:_ ~reason:_ -> ())
      ~chain_id ~data_dir:None ~chaindata:store ~reward_source:(fun _ _ -> Ok reward)
      ~read_finality:(fun _ -> Some proof) ~from_epoch:12L ~max_epochs:1
      |> Yojson.Safe.to_string in
    ignore (Unix.lseek store.txlog.current_fd 0 Unix.SEEK_SET);
    let request = Reader.read ~chaindata:store ~data_dir:None ~chain_id
      ~read_finality:(fun _ -> Lwt.return_some proof) ~cancelled:(fun () -> false)
      (query epoch.id) in
    require (Lwt.is_sleeping request) "range read did not yield";
    let loaded = Lwt_main.run request |> function
      | Ok value -> value
      | Error _ -> failwith "async range failed" in
    require (loaded.body = expected) "async range bytes changed";
    require (loaded.records = 1 && loaded.status = "ok") "async range status changed";
    require (position store = 0) "async read moved shared descriptor";
    let next = transaction 3 in
    Store.begin_batch store;
    Store.save_tx store ~hash:(Tx.hash next) ~epoch_id:13 ~from_addr:address
      ~to_addr:address ~tx_json:(Tx.to_yojson next |> Yojson.Safe.to_string)
      ~op_type:"standard" ~encrypted_data:"" ~message:"";
    Store.commit_batch store;
    require (Store.get_tx_by_txid store 2L <> None) "append after async read failed")

let test_interrupt () =
  with_store (fun store epoch proof ->
    let open Lwt.Syntax in
    let pending, finish = Lwt.wait () in
    let reached, reach = Lwt.wait () in
    let cancelled = ref false in
    let request = Reader.read ~chaindata:store ~data_dir:None ~chain_id
      ~read_finality:(fun _ -> Lwt.wakeup_later reach (); pending)
      ~cancelled:(fun () -> !cancelled) (query epoch.id) in
    Lwt_main.run (let* () = reached in
      cancelled := true;
      Lwt.wakeup_later finish (Some proof);
      let* reply = request in
      require (reply = Error Range.Stopped) "cancelled read returned data";
      require (position store = 0) "cancelled read accessed transactions";
      Lwt.return_unit))

let test_read_location () =
  with_store (fun store _ _ ->
    let dir = store.Store.txlog.dir in
    let seg_id, offset, len = Option.get (Index.get_txid_loc (Store.index store) 0L) in
    let read offset len = Octra_core.Txlog.read_location ~dir ~seg_id ~offset ~len in
    let expected = Octra_core.Txlog.read_record store.txlog ~seg_id ~offset ~len in
    ignore (Unix.lseek store.txlog.current_fd 0 Unix.SEEK_SET);
    require (read offset len = expected && position store = 0)
      "independent transaction read differs";
    List.iter (fun (offset, len) ->
      let failed = try ignore (read offset len); false with _ -> true in
      require failed "invalid read location accepted";
      require (position store = 0) "invalid read moved shared descriptor") [
      -1, len; offset, -1; offset, max_int; max_int, len; offset, len - 1;
      (Unix.fstat store.txlog.current_fd).Unix.st_size, len;
    ];
    let fd = Unix.openfile (Octra_core.Txlog.seg_path dir seg_id) [Unix.O_WRONLY] 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      ignore (Unix.lseek fd (offset + len) Unix.SEEK_SET);
      ignore (Unix.write_substring fd "bad!" 0 4));
    let failed = try ignore (read offset len); false with _ -> true in
    require failed "independent read skipped checksum")

let test_async_missing () =
  with_store (fun store epoch _ ->
    let reply = Reader.read ~chaindata:store ~data_dir:None ~chain_id
      ~read_finality:(fun _ -> Lwt.return_none) ~cancelled:(fun () -> false)
      (query epoch.id) |> Lwt_main.run in
    require (match reply with
      | Ok value -> value.status = "not_found" && value.records = 0
      | _ -> false) "async missing proof returned data";
    require (position store = 0) "async missing proof read transactions")

let test_async_bad_proof () =
  with_store (fun store epoch proof ->
    let proof = Codec.{ proof with finalize = {
      proof.finalize with precommits = [];
    }} in
    let reply = Reader.read ~chaindata:store ~data_dir:None ~chain_id
      ~read_finality:(fun _ -> Lwt.return_some proof) ~cancelled:(fun () -> false)
      (query epoch.id) |> Lwt_main.run in
    require (match reply with
      | Ok value -> value.status = "not_found" && value.records = 0
      | _ -> false) "async invalid proof returned data")

let next_head = Head.{
  schema_version = 3; generation = 1; epoch_id = 13;
  state_root = String.make 64 'c'; ledger_state_root = None;
  irmin_commit = None; txid_hi = 1L; txlog_seg = None; txlog_off = None;
  epochlog_off = None; commit_id = "new"; ts = 130.; quorum_cert_hash = None;
  epoch_index_hash = None; epoch_index_root = None;
}

let test_head_change () =
  with_store (fun store epoch proof ->
    let prior = Head.get_cached () in
    Fun.protect ~finally:(fun () -> Head.cached := prior) (fun () ->
      let open Lwt.Syntax in
      let pending, finish = Lwt.wait () in
      let reached, reach = Lwt.wait () in
      let request = Reader.read ~chaindata:store ~data_dir:None ~chain_id
        ~read_finality:(fun _ -> Lwt.wakeup_later reach (); pending)
        ~cancelled:(fun () -> false) (query epoch.id) in
      Lwt_main.run (let* () = reached in
        Head.set_cached next_head;
        Lwt.wakeup_later finish (Some proof);
        let* reply = request in
        require (reply = Error Range.Changed) "read crossed head generation";
        require (position store = 0) "changed head read transactions";
        Lwt.return_unit)))

let test_large_parts () =
  let txs = List.init 2 (fun index ->
    Tx.{ (transaction (index + 1)) with
      message = Some (String.make 3_000_257 (Char.chr (97 + index)));
    }) in
  with_store ~txs (fun store epoch proof ->
    let expected = Sync.range_json ~on_stop:(fun ~epoch:_ ~reason:_ -> ())
      ~chain_id ~data_dir:None ~chaindata:store ~reward_source:(fun _ _ -> Ok reward)
      ~read_finality:(fun _ -> Some proof) ~from_epoch:12L ~max_epochs:1 in
    let raw = Yojson.Safe.to_string expected in
    require (String.length raw > Parts.body_max) "large response is inline";
    ignore (Unix.lseek store.txlog.current_fd 0 Unix.SEEK_SET);
    let reads = ref 0 in
    let now = ref 0. in
    let actor = Range.create {
      now = (fun () -> !now);
      read = (fun ~cancelled query ->
        incr reads;
        Reader.read ~chaindata:store ~data_dir:None ~chain_id
          ~read_finality:(fun _ -> Lwt.return_some proof) ~cancelled query);
    } in
    let open Lwt.Syntax in
    let prior = Head.get_cached () in
    let load ?hash part =
      let ticks = ref 0 in
      let running = ref true in
      let rec pulse () =
        let* () = Lwt.pause () in
        if not !running then Lwt.return_unit
        else begin
          incr ticks;
          pulse ()
        end in
      let started = Mtime_clock.counter () in
      Lwt.async pulse;
      let* reply = Lwt.finalize
        (fun () -> Range.load actor { (query epoch.id) with part; hash })
        (fun () -> running := false; Lwt.return_unit) in
      let loaded = match reply with
        | Ok value -> value
        | Error _ -> failwith "large range failed" in
      require (part <> None || !ticks >= 4) "large range blocked event loop";
      require (position store = 0) "large range moved shared descriptor";
      let elapsed = Mtime.Span.to_float_ns (Mtime_clock.count started) /. 1e6 in
      Printf.printf "test = range_parts ticks = %d elapsed_ms = %.1f\n%!" !ticks elapsed;
      require (loaded.status = "ok" && loaded.records = 1) "large range status changed";
      let json = Yojson.Safe.from_string loaded.body in
      require (json = get (Parts.reply ?index:part expected)) "range part bytes changed";
      match get (Parts.view json) with
      | Parts.Full _ -> failwith "large range was not split"
      | Parts.Part value -> Lwt.return value in
    Lwt_main.run (Lwt.finalize (fun () ->
      let* first = load None in
      Head.set_cached next_head;
      let* rest = Lwt_list.map_s (fun index -> load ~hash:first.hash (Some (index + 1)))
        (List.init (first.count - 1) Fun.id) in
      let joined = get (Parts.join (first :: rest)) in
      require (Yojson.Safe.to_string joined = raw) "large range join changed bytes";
      require (!reads = 1) "range parts repeated store reads";
      let* invalid = Range.load actor { (query epoch.id) with part = Some first.count } in
      require (invalid = Error (Range.Invalid "range part index is invalid"))
        "invalid range part accepted";
      let part_query = { (query epoch.id) with part = Some 1; hash = Some first.hash } in
      let* wrong = Range.load actor { part_query with hash = Some (String.make 64 '0') } in
      require (wrong = Error Range.Missing) "unknown range hash accepted";
      let* wrong = Range.load actor { part_query with from_epoch = 13L } in
      require (wrong = Error Range.Missing) "range hash crossed requested epoch";
      let* wrong = Range.load actor { part_query with max_epochs = 2 } in
      require (wrong = Error Range.Missing) "range hash crossed requested count";
      require (!reads = 1) "unknown range hash reached store";
      now := Range.retention -. 1.;
      let* _ = load (Some 1) in
      require (!reads = 1) "legacy part repeated store reads";
      now := Range.retention;
      let* expired = Range.load actor part_query in
      require (expired = Error Range.Missing) "expired range hash accepted";
      require (!reads = 1) "expired range hash reached store";
      Lwt.return_unit) (fun () ->
        Head.cached := prior;
        Range.shutdown actor)))

let () =
  test_valid ();
  test_bad_proof ();
  test_missing_proof ();
  test_missing_transaction ();
  test_missing_epoch ();
  test_prefix ();
  test_read_location ();
  test_async ();
  test_interrupt ();
  test_async_missing ();
  test_async_bad_proof ();
  test_head_change ();
  test_large_parts ();
  print_endline "status = pass test = sync_range"