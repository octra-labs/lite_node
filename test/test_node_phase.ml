(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Case = Recovery_case
module Boot = Octra_node_runtime.Startup_node_boot_shell
module Start = Octra_node_runtime.Wal_start

let boot dir =
  flush_all ();
  match Unix.fork () with
  | 0 ->
    (try
      Case.expect "early WAL check refused" (Start.check dir = Ok ());
      Case.with_stores dir (fun chaindata store ->
        let exit_fatal () = failwith "boot refused" in
        Boot.run_store {data_dir = dir; store; exit_fatal};
        let ledger = Octra_core.Ledger.create store in
        let outcome = Start.recover ~data_dir:dir (fun () -> Boot.run_node {
          data_dir = dir; store; ledger; chaindata; total_tx_count = ref 0;
          observer_mode = true; wallet = {address = "octFROM"; pub = ""};
          consensus_mode = true; voting_consensus_mode = false;
          consensus_port_configured = (fun () -> true);
          validators = (fun () -> []); int_value = (fun _ value -> value);
          env = (fun _ -> None); exit_fatal;
        }) in
        let epoch = match outcome with
          | Ok (epoch, _) -> epoch
          | Error error ->
            Case.expect "recovery error path differs" (error.path = dir);
            Case.expect "recovery error reason missing" (error.reason <> "");
            exit Start.exit_code in
        Case.expect "boot next epoch differs" (epoch = 2));
      exit 0
    with exn -> Printf.eprintf "event = refused reason = %s\n%!" (Printexc.to_string exn); exit 2)
  | pid -> Case.wait pid

let verify dir =
  Case.with_stores dir (fun chaindata store ->
    let head = Option.get (Case.HM.load dir) in
    Case.expect "boot HEAD epoch differs" (head.epoch_id = 1);
    Case.expect "boot HEAD root differs"
      (Some (Case.HM.ledger_state_root head) = Lwt_main.run (Case.SI.get_head_hash store));
    Case.expect "boot HEAD commit differs"
      (head.irmin_commit = Lwt_main.run (Case.SI.get_commit_hash store));
    Case.expect "boot lost committed transaction"
      (Case.SC.get_tx_by_hash chaindata (Case.hash 'b') <> None);
    Case.expect "boot retained WAL" (Case.Wal.read_pending dir = []);
    Case.expect "boot retained guard" (not (Case.Marker.recovery_required dir));
    head)

let run root =
  Case.expect "unrelated exception was classified as WAL failure"
    (try ignore (Start.recover ~data_dir:root (fun () -> failwith "control")); false
     with Failure reason -> reason = "control" | _ -> false);
  let cases = [
    "forward", (fun _ -> ()), true;
    "wal_pre", (fun dir -> Case.change_wal dir (fun entry ->
      {entry with Case.Wal.pre_state_root = Case.hash '6'})), false;
    "wal_range", (fun dir -> Case.change_wal dir (fun entry ->
      {entry with Case.Wal.start_txid = 42L})), false;
    "prepare_root", (fun dir -> Case.change_prepare dir (function
      | Octra_core.Commit_journal.Prepare entry ->
        Octra_core.Commit_journal.Prepare {entry with planned_state_root = Case.hash '6'}
      | _ -> assert false)), false;
    "marker_epoch", (fun dir -> Case.Marker.write_marker dir 99 "irmin_committed"), false] in
  let failed = List.fold_left (fun failed (name, alter, valid) ->
    try
      let dir = Filename.concat root name in
      Unix.mkdir dir 0o700;
      let head, _ = Case.prepare dir in
      Case.advance dir head;
      alter dir;
      let before = Case.evidence dir in
      let outcome = boot dir in
      if valid then begin
        Case.expect "valid boot refused" (outcome = Unix.WEXITED 0);
        let first = verify dir in
        Case.expect "boot retry refused" (boot dir = Unix.WEXITED 0);
        Case.expect "boot retry changed HEAD" (verify dir = first)
      end else begin
        Case.expect "invalid boot changed evidence" (Case.evidence dir = before);
        Case.expect "invalid boot exit code differs" (outcome = Unix.WEXITED 78)
      end;
      Printf.printf "event = passed case = %s\n%!" name;
      failed
    with exn ->
      Printf.eprintf "event = failed case = %s reason = %s\n%!"
        name (Printexc.to_string exn);
      true) false cases in
  if failed then exit 1

let get = function
  | Ok value -> value
  | Error reason -> failwith reason

let contains text part =
  let rec at index =
    index + String.length part <= String.length text
    && (String.sub text index (String.length part) = part || at (index + 1))
  in
  at 0

let write_file path bytes =
  let fd = Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o600 in
  let channel = Unix.out_channel_of_descr fd in
  Fun.protect ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel bytes)

let file_hash path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
    let buffer = Bytes.create 65_536 in
    let rec read hash =
      match input channel buffer 0 (Bytes.length buffer) with
      | 0 -> Digestif.SHA256.(to_hex (get hash))
      | length -> read (Digestif.SHA256.feed_bytes hash ~off:0 ~len:length buffer)
    in
    read Digestif.SHA256.empty)

let node_tools () =
  let directory = Filename.dirname (Filename.dirname (Unix.realpath Sys.executable_name)) in
  let configured name file =
    match Sys.getenv_opt name with
    | Some value -> value
    | None -> Filename.concat directory ("bin/" ^ file)
  in
  let binary, worker = match Array.length Sys.argv with
    | 1 -> configured "OCTRA_TEST_NODE_BINARY" "octra_node.exe",
        configured "OCTRA_TEST_PVAC_WORKER" "octra_pvac_worker.exe"
    | 3 -> Sys.argv.(1), Sys.argv.(2)
    | _ -> failwith "test_node_phase expects node and worker paths"
  in
  let executable path =
    let path = Unix.realpath path in
    Case.expect ("required executable is not a file: " ^ path)
      ((Unix.stat path).Unix.st_kind = Unix.S_REG);
    Unix.access path [Unix.R_OK; Unix.X_OK];
    path
  in
  executable binary, executable worker

let node_keys () =
  List.init 4 (fun index ->
    let key = match Mirage_crypto_ec.Ed25519.priv_of_octets
      (String.make 32 (Char.chr (21 + index))) with
      | Ok key -> key
      | Error _ -> failwith "synthetic key rejected"
    in
    let pubkey = Mirage_crypto_ec.Ed25519.pub_of_priv key
      |> Mirage_crypto_ec.Ed25519.pub_to_octets in
    let address = Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn pubkey) in
    Octra_consensus.C_types.{address; pubkey}, key)

let floor_parent ~chain_id ~epoch ~state_root keys =
  let module T = Octra_consensus.C_types in
  let module H = Octra_consensus.C_hash in
  let creator, _ = List.hd keys in
  let header = T.{proto_version = proto_version_current; chain_id;
    epoch_id = Int64.of_int epoch; prev_state_root = Octra_net.Hash_domain.nil_hash;
    tx_list_hash = Octra_consensus.C_engine.tx_list_hash_for_header [];
    receipt_root = H.receipt_root []; proposed_state_root = state_root;
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = creator.address; txid_hi = 0L; ts = 1.} in
  let proposal_id = H.proposal_id header in
  let precommits = List.map (fun ((member : T.validator_info), key) ->
    let vote = T.{chain_id; epoch_id = header.epoch_id; round = 0;
      vote_type = Precommit; proposal_id; validator = member.address; signature = ""} in
    {vote with signature = Mirage_crypto_ec.Ed25519.sign ~key (H.vote_sign_bytes vote)}) keys in
  let validator_set = T.make_weighted_validator_set
    (List.map (fun (member, _) -> member, Z.one) keys) |> get in
  T.{certificate = {chain_id; epoch_id = header.epoch_id; commit_round = 0;
    header; proposal_id; precommits}; validator_set}

let node_head dir chain store epoch epoch_hash index_root =
  let ledger = Option.get (Lwt_main.run (Case.SI.get_head_hash store)) in
  let segment, offset = Case.SC.txlog_position chain in
  let head = Case.HM.{schema_version; generation = epoch; epoch_id = epoch;
    state_root = Case.Eic.folded_state_root ~ledger_state_root:ledger ~epoch_index_root:index_root;
    ledger_state_root = Some ledger;
    irmin_commit = Lwt_main.run (Case.SI.get_commit_hash store); txid_hi = 0L;
    txlog_seg = Some segment; txlog_off = Some offset;
    epochlog_off = Some (Case.SC.epochlog_offset chain);
    commit_id = "node-anchor-" ^ string_of_int epoch; ts = 1.; quorum_cert_hash = None;
    epoch_index_hash = Some epoch_hash; epoch_index_root = Some index_root} in
  Lwt_main.run (Case.SI.tag_epoch store epoch);
  Case.HM.atomic_write dir head;
  head

let node_sample dir ~chain_id ~anchor ~advance =
  let module T = Octra_consensus.C_types in
  let module Floor = Octra_core.History_floor in
  let hex raw = Digestif.SHA256.(to_hex (of_raw_string raw)) in
  let keys = node_keys () in
  let member, key = List.hd keys in
  let public_key = Base64.encode_exn member.T.pubkey in
  write_file (Filename.concat dir "wallet.json") (Yojson.Safe.to_string (`Assoc [
    "priv", `String (Base64.encode_exn (Mirage_crypto_ec.Ed25519.priv_to_octets key));
    "pub", `String public_key; "address", `String member.address]));
  let config = String.make 64 '1' in
  let head = Case.with_stores dir (fun chain store ->
    let epoch = anchor - 1 in
    Lwt_main.run (Case.SI.begin_epoch_batch store);
    List.iter (fun (name, value) -> Lwt_main.run (Case.SI.set_meta store name value))
      ["last_epoch", string_of_int epoch; "current_epoch", string_of_int anchor;
       "total_supply", "0"; "emission_remaining", "0"; "stealth_counter", "1"];
    Lwt_main.run (Case.SI.set_account store member.address
      {Octra_core.Ledger_types.empty_account with public_key = Some public_key});
    Lwt_main.run (Case.SI.commit_epoch_batch store "synthetic anchor floor");
    let ledger = Option.get (Lwt_main.run (Case.SI.get_head_hash store)) in
    let epoch_hash, index_root = Case.Eic.next_root ~prev:Case.Eic.genesis_root ~epoch_id:epoch [] in
    let state_root = Case.Eic.folded_state_root
      ~ledger_state_root:ledger ~epoch_index_root:index_root in
    let parent_commit = floor_parent ~chain_id ~epoch
      ~state_root:Digestif.SHA256.(to_raw_string (of_hex state_root)) keys in
    let floor = Floor.create ~chain_id ~epoch ~state_root ~ledger_state_root:ledger
      ~txid_hi:0L ~config_hash:config
      ~validator_set_hash:(hex (Octra_consensus.C_config.validator_set_hash parent_commit.validator_set))
      ~epoch_index_hash:epoch_hash ~epoch_index_root:index_root ~parent_commit |> get in
    get (Case.SC.seed_history_floor chain floor);
    Case.SC.fsync chain;
    let head = node_head dir chain store epoch epoch_hash index_root in
    if not advance then head else begin
      Lwt_main.run (Case.SI.begin_epoch_batch store);
      Lwt_main.run (Case.SI.set_meta store "last_epoch" (string_of_int anchor));
      Lwt_main.run (Case.SI.set_meta store "current_epoch" (string_of_int (anchor + 1)));
      Lwt_main.run (Case.SI.commit_epoch_batch store "synthetic anchor epoch");
      let ledger = Option.get (Lwt_main.run (Case.SI.get_head_hash store)) in
      let epoch_hash, index_root = Case.Eic.next_root ~prev:index_root ~epoch_id:anchor [] in
      let state_root = Case.Eic.folded_state_root
        ~ledger_state_root:ledger ~epoch_index_root:index_root in
      Case.SC.begin_batch chain;
      Case.SC.set_epoch chain {Case.EL.empty_epoch_header with id = anchor;
        state_root; prev_state_root = head.state_root; parent_commit = Option.get head.irmin_commit;
        start_txid = Case.SC.next_txid chain; tx_count = 0; finalized_at = 1.};
      Case.SC.set_epoch_index_commitment chain ~epoch_id:anchor ~epoch_hash ~root:index_root;
      Case.SC.commit_batch chain;
      Case.SC.fsync chain;
      let head = node_head dir chain store anchor epoch_hash index_root in
      Case.expect "anchor header is not durable"
        ((get (Case.SC.get_bound_epoch_header chain anchor)).state_root = head.state_root);
      head
    end) in
  config, head

let node_process ~binary ~worker ~worker_hash ~chain_id ~dir ~config ~stop =
  let environment = Unix.environment () |> Array.to_list
    |> List.filter (fun entry -> not (String.starts_with ~prefix:"OCTRA_" entry)) in
  let environment = Array.of_list (environment @ List.map (fun (name, value) -> name ^ "=" ^ value)
    ["OCTRA_DATA_DIR", dir; "OCTRA_CHAIN_ID", chain_id;
     "OCTRA_CONSENSUS_MODE", "observer"; "OCTRA_CONSENSUS_PORT", "0";
     "OCTRA_API_PORT", "0"; "OCTRA_P2P_PORT", "0"; "OCTRA_PEERS", "";
     "OCTRA_STORE_WAIT_SECONDS", "0"; "OCTRA_CONSENSUS_CONFIG_HASH", config;
     "OCTRA_PVAC_VERIFY_WORKER", worker; "OCTRA_PVAC_VERIFY_WORKER_HASH", worker_hash]) in
  let output = Buffer.create 4096 in
  let stopped = ref false in
  let status = Lwt_main.run (Lwt_process.with_process_full ~env:environment ~timeout:30.
    (binary, [|binary; "--observer"|]) (fun process ->
      let open Lwt.Syntax in
      let rec collect channel =
        let* line = Lwt_io.read_line_opt channel in
        match line with
        | None -> Lwt.return_unit
        | Some line ->
          if Buffer.length output + String.length line > 262_144 then
            Lwt.fail_with "node startup output exceeds limit"
          else begin
            Buffer.add_string output line;
            Buffer.add_char output '\n';
            if not !stopped && Option.fold ~none:false ~some:(contains line) stop then begin
              stopped := true;
              process#kill Sys.sigterm
            end;
            collect channel
          end
      in
      let* () = Lwt_io.close process#stdin in
      let* () = Lwt.join [collect process#stdout; collect process#stderr] in
      process#status)) in
  status, !stopped, Buffer.contents output

let test_anchor_start root =
  let module Graph = Octra_core.Rule_graph in
  let chain_id = "octra-devnet-9871-cluster" in
  let plan = Option.get (Graph.scalar_work_activation_for_chain chain_id) in
  Case.expect "anchor epoch cannot seed a floor" (plan.anchor_epoch > 0 && plan.anchor_epoch < max_int);
  let binary, worker = node_tools () in
  let worker_hash = file_hash worker in
  Printf.printf "test = node_anchor binary = %s worker = %s\n%!" (file_hash binary) worker_hash;
  let private_root = Filename.concat root ".keys" in
  Unix.mkdir private_root 0o700;
  List.iter (fun advance ->
    let name = if advance then "wrong_anchor" else "pre_anchor" in
    let dir = Filename.concat private_root name in
    Unix.mkdir dir 0o700;
    let config, head = node_sample dir ~chain_id ~anchor:plan.anchor_epoch ~advance in
    if advance then Case.expect "synthetic root equals rule anchor" (head.state_root <> plan.anchor_state_root);
    let head_before = Case.read (Case.HM.path dir) in
    let stage = Printf.sprintf
      "event = rule_graph rule = scalar_work status = awaiting_anchor anchor_epoch = %d activation_epoch = %d"
      plan.anchor_epoch plan.activation_epoch in
    let status, stopped, output = node_process ~binary ~worker ~worker_hash ~chain_id ~dir ~config
      ~stop:(if advance then None else Some stage) in
    let log = Filename.concat root (name ^ ".log") in
    write_file log output;
    let check message condition =
      if not condition then begin
        Printf.eprintf "test = node_anchor case = %s status = failed reason = %S log = %S\n%!" name message log;
        failwith message
      end
    in
    List.iter (fun marker -> check ("node did not reach " ^ marker) (contains output marker))
      ["event = pvac_worker status = ready"; "event = circle_worker status = ready";
       "event = storage_ready"; "observer mode = true local_account_creation = skipped";
       Printf.sprintf "starting epoch = %d" (head.epoch_id + 1)];
    check "startup changed HEAD" (Case.read (Case.HM.path dir) = head_before);
    if advance then begin
      let rejected = Printf.sprintf
        "event = rule_graph rule = scalar_work status = rejected reason = rule anchor mismatch epoch = %d expected = %s actual = %s"
        plan.anchor_epoch plan.anchor_state_root head.state_root in
      check "wrong anchor did not exit with rule refusal" (status = Unix.WEXITED 1 && not stopped);
      check "wrong anchor refusal differs" (contains output rejected);
      check "wrong anchor was accepted" (not (contains output "event = rule_graph rule = scalar_work status = bound"))
    end else begin
      check "pre-anchor control did not reach rule check" (stopped && contains output stage);
      check "pre-anchor control failed before controlled stop"
        (status = Unix.WSIGNALED Sys.sigterm || status = Unix.WEXITED 0);
      check "pre-anchor control rejected a rule"
        (not (List.exists (fun line -> contains line "event = rule_graph" && contains line "status = rejected")
          (String.split_on_char '\n' output)))
    end;
    Printf.printf "test = node_anchor case = %s epoch = %d status = pass\n%!" name head.epoch_id)
    [true; false]

let () = Test_workspace.with_dir "node_phase" (fun root ->
  run root;
  test_anchor_start root)