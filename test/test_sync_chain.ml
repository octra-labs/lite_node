(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Chain = Octra_node_runtime.Sync_chain
module Publish = Octra_node_runtime.Sync_publish
module Head = Octra_core.Head_manifest
module Journal = Octra_node_runtime.Consensus_finality_journal
module Anchor = Octra_bootstrap.Sync_anchor
module Checkpoint = Octra_bootstrap.State_sync_checkpoint
module Manifest = Octra_bootstrap.State_sync_manifest
module Irmin = Octra_core.Store_irmin
module Store = Octra_core.Store_chaindata
module Update = Octra_core.Validator_set_update
module C = Octra_consensus.C_types
module H = Octra_consensus.C_hash

let get = function
  | Ok value -> value
  | Error reason -> failwith reason

let check label value =
  if not value then failwith label

let sha value = Digestif.SHA256.(digest_string value |> to_hex)
let raw value = Digestif.SHA256.(digest_string value |> to_raw_string)

let private_key = String.make 32 '\123'

let wallet =
  let key = Mirage_crypto_ec.Ed25519.priv_of_octets private_key
    |> Result.map_error (fun _ -> "invalid test key") |> get in
  let pub = Mirage_crypto_ec.Ed25519.(pub_of_priv key |> pub_to_octets)
    |> Base64.encode_exn in
  Octra_core.Crypto.Wallet.{
    address = Octra_core.Crypto.Address.address_from_pubkey pub;
    pub;
    priv = Base64.encode_exn private_key;
  }

let trusted = C.make_validator_set [C.{ address = wallet.address; pubkey = wallet.pub }]
let initial = get (Anchor.raw_validator_set trusted)
let chain_id = "octra-sync-test"

let with_store path action =
  let store = Lwt_main.run (Irmin.open_store ~fresh:true path) in
  Fun.protect ~finally:(fun () -> Lwt_main.run (Irmin.close store))
    (fun () -> action store)

let set store key value =
  Lwt_main.run (Irmin.set_meta store key value)

let proof store epoch key =
  Lwt_main.run (Irmin.tag_epoch store epoch);
  get (Lwt_main.run (Irmin.merkle_proof_at_epoch store epoch ["meta"; key]))

let update ?weight index =
  let epoch = 3 * index in
  let member = Octra_core.Validator_admission.{
    address = wallet.address;
    pubkey = Base64.decode_exn wallet.pub;
    weight = Z.of_int (Option.value weight ~default:(index + 1));
  } in
  get (Update.make_weighted ~source_epoch:(Int64.of_int (epoch - 1))
    ~activate_epoch:(Int64.of_int (epoch + 1)) [member])

let finality ?index_root epoch validator_set (proof : Irmin.merkle_proof) =
  let epoch_id = Int64.of_int epoch in
  let index_root = Option.value index_root ~default:(sha ("index-" ^ string_of_int epoch)) in
  let state_root = Octra_core.Epoch_index_commitment.folded_state_root
    ~ledger_state_root:proof.ledger_state_root ~epoch_index_root:index_root in
  let header = C.{
    proto_version = proto_version_current;
    chain_id;
    epoch_id;
    prev_state_root = raw "parent";
    tx_list_hash = H.tx_list_hash [];
    receipt_root = H.receipt_root [];
    proposed_state_root = Digestif.SHA256.(of_hex state_root |> to_raw_string);
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = wallet.address;
    txid_hi = 0L;
    ts = float_of_int epoch;
  } in
  let proposal_id = H.proposal_id header in
  let vote = C.{
    chain_id; epoch_id; round = 0; vote_type = Precommit; proposal_id;
    validator = wallet.address; signature = String.make 64 '\000';
  } in
  let vote = C.{ vote with signature = H.sign_ed25519 ~priv_raw:private_key
    ~msg:(H.vote_sign_bytes vote) } in
  let finalize = C.{
    chain_id; epoch_id; commit_round = 0; header; proposal_id;
    precommits = [vote]; parent_commit = None;
  } in
  get (Anchor.verify_finalize ~chain_id ~validator_set finalize);
  Journal.{ finalize; validator_set; bundle = None }, index_root

let test_prepare ?kept steps record checkpoint =
  let finalize = record.Journal.finalize in
  let head = Head.{
    schema_version;
    generation = Int64.to_int checkpoint.Checkpoint.epoch;
    epoch_id = Int64.to_int checkpoint.epoch;
    state_root = checkpoint.state_root;
    ledger_state_root = Some checkpoint.ledger_state_root;
    irmin_commit = Some (sha "commit");
    txid_hi = checkpoint.txid_hi;
    txlog_seg = Some 0;
    txlog_off = Some 0;
    epochlog_off = Some 0;
    commit_id = Anchor.finalize_hash finalize;
    ts = finalize.header.ts;
    quorum_cert_hash = checkpoint.quorum_cert_hash;
    epoch_index_hash = checkpoint.epoch_index_hash;
    epoch_index_root = checkpoint.epoch_index_root;
  } in
  let prepare ~steps ~head record =
    Publish.prepare ~chain_id ~config_hash:checkpoint.config_hash
      ~trusted_validator_set:trusted ~validator_set:record.Journal.validator_set
      ~steps ~head record in
  let task = prepare ~steps ~head record in
  check "producer verification completed without yielding"
    (match Lwt.state task with Lwt.Sleep -> true | _ -> false);
  let prepared = get (Lwt_main.run task) in
  let expected = Checkpoint.{ checkpoint with
    valid_until = Int64.add checkpoint.created_at 2_678_400L } in
  check "producer checkpoint changed" (prepared.checkpoint = expected);
  check "producer checkpoint hash changed"
    (prepared.checkpoint_hash = get (Checkpoint.hash expected));
  let expected_steps = Option.value kept ~default:steps in
  let expected = Anchor.make ~steps:expected_steps ~finalize ~validator_set:record.validator_set in
  check "producer retained repeated validator path"
    (Anchor.encode prepared.anchor = Anchor.encode expected);
  let repeated = Lwt_main.run (Anchor.compact_lwt ~validator_set:trusted prepared.anchor) |> get in
  check "producer path is not idempotent" (Anchor.encode repeated = Anchor.encode prepared.anchor);
  ignore (get (Anchor.verify ~validator_set:trusted prepared.checkpoint
    (Anchor.encode prepared.anchor)));
  let anchor = Anchor.make ~steps ~finalize ~validator_set:record.validator_set in
  let reference = Anchor.derive ~validator_set:trusted anchor in
  check "producer derived set differs"
    (Lwt_main.run (Anchor.derive_lwt ~validator_set:trusted anchor) = reference);
  let stopped = ref false in
  let task = Anchor.derive_lwt ~cancelled:(fun () -> !stopped)
    ~validator_set:trusted anchor in
  stopped := true;
  check "producer cancellation was ignored"
    (Lwt_main.run task = Error "state sync verification cancelled");
  stopped := false;
  let task = Anchor.compact_lwt ~cancelled:(fun () -> !stopped) ~validator_set:trusted anchor in
  stopped := true;
  check "path cancellation was ignored"
    (Lwt_main.run task = Error "state sync verification cancelled");
  let reject ~steps ~head record expected =
    let actual = Lwt_main.run (prepare ~steps ~head record) in
    let status = function Ok _ -> "accepted" | Error reason -> reason in
    check (Printf.sprintf "producer refusal differs expected = %s actual = %s"
      (status expected) (status actual)) (actual = expected) in
  reject ~steps ~head:{ head with state_root = sha "different" } record
    (Error "state sync HEAD folded root is invalid");
  let damage (value : C.finalize) = C.{ value with
    precommits = List.map (fun (vote : C.vote) ->
      { vote with signature = String.make 64 '\000' }) value.precommits } in
  let broken = Journal.{ record with finalize = damage finalize } in
  let task = prepare ~steps ~head broken in
  check "producer accepted damaged checkpoint" (Result.is_error (Lwt_main.run task));
  List.iteri (fun index first ->
    if Option.is_some kept || index = 0 then
      List.iter (fun changed ->
        let steps = List.mapi (fun offset step ->
          if offset = index then changed else step) steps in
        let anchor = Anchor.make ~steps ~finalize ~validator_set:record.validator_set in
        let expected = match Anchor.derive ~validator_set:trusted anchor with
          | Error reason -> Error reason
          | Ok _ -> failwith "damaged producer chain verified" in
        reject ~steps ~head record expected;
        check "async damaged chain refusal differs"
          (Lwt_main.run (Anchor.derive_lwt ~validator_set:trusted anchor) = expected))
        [Anchor.{ first with finalize = damage first.finalize };
         Anchor.{ first with proof = "invalid" }]) steps;
  prepared

let certificate ?kept steps record index_root ledger_root =
  let finalize = record.Journal.finalize in
  let checkpoint = Checkpoint.{
    chain_id; epoch = finalize.epoch_id;
    state_root = Octra_node_runtime.Text.raw_to_hex finalize.header.proposed_state_root;
    ledger_state_root = ledger_root;
    txid_hi = 0L; config_hash = sha "config";
    validator_set_hash = Octra_consensus.C_config.validator_set_hash record.validator_set
      |> Octra_node_runtime.Text.raw_to_hex;
    quorum_cert_hash = Some (Anchor.finalize_hash finalize);
    epoch_index_hash = Some (sha "index");
    epoch_index_root = Some index_root;
    created_at = finalize.epoch_id;
    valid_until = Int64.add finalize.epoch_id 100_000L;
  } in
  let anchor = Anchor.make ~steps ~finalize ~validator_set:record.validator_set in
  let prepared = test_prepare ?kept steps record checkpoint in
  let encoded = Anchor.encode anchor in
  let verified = get (Anchor.verify ~validator_set:trusted checkpoint encoded) in
  let ticks = ref 0 in
  let running = ref true in
  let rec pulse () =
    let open Lwt.Syntax in
    let* () = Lwt.pause () in
    if !running then begin
      incr ticks;
      pulse ()
    end else Lwt.return_unit
  in
  let checked = Lwt_main.run (
    Lwt.async pulse;
    Lwt.finalize
      (fun () -> Anchor.verify_lwt ~validator_set:trusted checkpoint encoded)
      (fun () -> running := false; Lwt.return_unit)) |> get in
  check "async validator chain differs" (Anchor.encode verified = Anchor.encode checked);
  check "validator chain blocked event loop" (!ticks >= List.length steps);
  let checkpoint_hash = get (Checkpoint.hash checkpoint) in
  let files = ["HEAD.json"; "ledger.dat"; Octra_bootstrap.Root_win.name; "state_root"]
    |> List.sort String.compare
    |> List.map (fun path -> Manifest.{
      path; size = 1L; sha256 = sha "x";
      chunks = [{ index = 0; offset = 0L; size = 1; sha256 = sha "x" }];
    }) in
  let manifest = Manifest.{
    checkpoint_hash; snapshot_id = checkpoint_hash; irmin_commit = None;
    chunk_size = chunk_size_min; total_size = 4L; file_count = 4;
    chunk_count = 4; chunks_root = chunks_root files; files;
  } in
  let certificate = Manifest.{
    checkpoint; checkpoint_hash; authority = Finalized encoded; manifest;
    manifest_hash = get (manifest_hash manifest);
    exporter_signatures = [get (make_exporter_signature ~wallet manifest)];
  } in
  let verified = get (Manifest.verify_certificate ~validator_set:trusted ~exporter_set:trusted certificate) in
  let reduced = Manifest.{ certificate with authority = Finalized (Anchor.encode prepared.anchor) } in
  ignore (get (Manifest.verify_certificate ~validator_set:trusted ~exporter_set:trusted reduced));
  let checked = Lwt_main.run (Manifest.verify_certificate_lwt
    ~validator_set:trusted ~exporter_set:trusted certificate) |> get in
  check "async certificate differs" (Manifest.certificate_json verified = Manifest.certificate_json checked);
  verified

let test_repeated_sets () =
  List.iter (fun (weights, indices) ->
    Test_workspace.with_dir "sync_sets" (fun root ->
      with_store (Filename.concat root "history") (fun store ->
        let validator_set, reversed = List.fold_left (fun (prior, steps) (index, weight) ->
          let value = update ~weight index in
          let encoded = Update.to_string value in
          set store Update.pending_meta_key encoded;
          let proof = proof store (3 * index) Update.pending_meta_key in
          let record, index_root = finality (3 * index) prior proof in
          let step = Anchor.{ source = Pending; finalize = record.finalize;
            ledger_state_root = proof.ledger_state_root; epoch_index_root = index_root;
            update = encoded; proof = proof.proof } in
          set store Update.active_meta_key encoded;
          get (Update.validator_set value), step :: steps)
            (initial, []) (List.mapi (fun offset weight -> offset + 1, weight) weights) in
        let steps = List.rev reversed in
        let kept = List.map (fun index -> List.nth steps (index - 1)) indices in
        let epoch = 3 * List.length steps + 2 in
        let proof = proof store epoch Update.active_meta_key in
        let record, index_root = finality epoch validator_set proof in
        ignore (certificate ~kept steps record index_root proof.ledger_state_root))))
    [[2; 2; 2], [1]; [2; 3; 2; 4; 3; 5], [1; 2; 6]; [2; 3; 4; 2], [1]]

let saved ?(count = Anchor.max_steps) ?(width = 0) path =
  with_store path (fun store ->
    if width > 0 then set store (String.make width 'k') "preserved";
    let rec loop index validator_set acc =
      if index > count then List.rev acc, validator_set
      else
        let value = update index in
        let encoded = Update.to_string value in
        set store Update.pending_meta_key encoded;
        let proof = proof store (3 * index) Update.pending_meta_key in
        let record, index_root = finality (3 * index) validator_set proof in
        let step = Anchor.{
          source = Pending; finalize = record.finalize;
          ledger_state_root = proof.ledger_state_root;
          epoch_index_root = index_root; update = encoded; proof = proof.proof;
        } in
        set store Update.active_meta_key encoded;
        loop (index + 1) (get (Update.validator_set value)) (step :: acc)
    in
    let steps, validator_set = loop 1 initial [] in
    let epoch = 3 * count + 2 in
    let proof = proof store epoch Update.active_meta_key in
    let record, index_root = finality epoch validator_set proof in
    certificate steps record index_root proof.ledger_state_root)

let test_proof_work root certificate =
  let encoded = match certificate.Manifest.authority with
    | Finalized encoded -> encoded
    | Checkpoint_quorum _ -> failwith "missing finality proof" in
  let anchor = get (Anchor.decode encoded) in
  with_store (Filename.concat root "proof") (fun store ->
    set store Update.pending_meta_key (String.make 2_999_000 'x');
    let proof = proof store 3 Update.pending_meta_key in
    let record, index_root = finality 3 initial proof in
    let transitions = Anchor.steps anchor in
    let step = Anchor.{ (List.hd transitions) with
      finalize = record.finalize; ledger_state_root = proof.ledger_state_root;
      epoch_index_root = index_root; proof = proof.proof } in
    let encoded = Anchor.make ~steps:(step :: List.tl transitions)
      ~finalize:(Anchor.finality anchor) ~validator_set:(Anchor.validator_set anchor)
      |> Anchor.encode in
    let expected = Error "validator transition state proof mismatch" in
    check "large proof rejection differs"
      (Anchor.verify ~validator_set:trusted certificate.checkpoint encoded = expected);
    let running = ref true in
    let previous = ref (Unix.gettimeofday ()) in
    let maximum = ref 0. in
    let rec pulse () =
      let open Lwt.Syntax in
      let* () = Lwt_unix.sleep 0.001 in
      let now = Unix.gettimeofday () in
      maximum := max !maximum (now -. !previous);
      previous := now;
      if !running then pulse () else Lwt.return_unit in
    let result = Lwt_main.run (
      Lwt.async pulse;
      Lwt.finalize (fun () ->
        let open Lwt.Syntax in
        let* result = Anchor.verify_lwt ~validator_set:trusted certificate.checkpoint encoded in
        let* () = Lwt_unix.sleep 0.002 in
        Lwt.return result)
        (fun () -> running := false; Lwt.return_unit)) in
    check "async large proof rejection differs" (result = expected);
    Printf.printf "event = proof_work bytes = %d pause_ms = %.3f\n%!"
      (String.length proof.proof) (!maximum *. 1000.))

let test_limit () =
  Test_workspace.with_dir "sync_chain" (fun root ->
    let saved = saved (Filename.concat root "history") in
    test_proof_work root saved;
    let old_epoch = Int64.to_int saved.Manifest.checkpoint.epoch in
    let path = Filename.concat root "certificate.json" in
    Manifest.write_json path (Manifest.certificate_json saved);
    with_store (Filename.concat root "current") (fun store ->
      let chaindata = Store.open_chaindata (Filename.concat root "chaindata") in
      Fun.protect ~finally:(fun () -> Store.close chaindata) (fun () ->
        let old = update Anchor.max_steps in
        let old_raw = Update.to_string old in
        let old_set = get (Update.validator_set old) in
        set store Update.active_meta_key old_raw;
        set store Update.pending_meta_key old_raw;
        let old_proof = proof store old_epoch Update.active_meta_key in
        let old_record, old_index = finality old_epoch old_set old_proof in
        check "restored ledger root differs"
          (old_proof.ledger_state_root = saved.checkpoint.ledger_state_root);
        let entries = ref [old_record, old_index] in
        let deps = Chain.{
          data_dir = root; chain_id; store; chaindata;
          certificate_path = (fun () -> path);
          read_finality = (fun epoch ->
            match List.find_opt (fun (record, _) -> record.Journal.finalize.epoch_id = epoch) !entries with
            | Some (record, _) -> Journal.Valid record
            | None -> Journal.Missing);
        } in
        let build epoch validator_set = Lwt_main.run
          (Chain.build deps ~head_epoch:(Int64.of_int epoch) trusted validator_set) in
        check "stored chain at limit refused"
          (List.length (get (build old_epoch old_set)) = Anchor.max_steps);
        let next = update (Anchor.max_steps + 1) in
        let next_raw = Update.to_string next in
        let next_set = get (Update.validator_set next) in
        let epoch = old_epoch + 1 in
        set store Update.pending_meta_key next_raw;
        let pending = proof store epoch Update.pending_meta_key in
        let next_record, next_index = finality epoch old_set pending in
        entries := (next_record, next_index) :: !entries;
        List.iter (fun (record, root) ->
          Store.set_epoch_index_commitment_direct chaindata
            ~epoch_id:(Int64.to_int record.Journal.finalize.epoch_id)
            ~epoch_hash:(sha "index") ~root) !entries;
        set store Update.active_meta_key next_raw;
        let target = epoch + 2 in
        let current = proof store target Update.active_meta_key in
        let record, index_root = finality target next_set current in
        let steps = get (build target next_set) in
        check "stored extension exceeds limit instead of retrying trusted base"
          (List.length steps = 2);
        let compressed = certificate steps record index_root current.ledger_state_root in
        Manifest.write_json path (Manifest.certificate_json compressed);
        let reloaded = get (Manifest.load_certificate path) in
        ignore (get (Manifest.verify_certificate ~validator_set:trusted
          ~exporter_set:trusted reloaded));
        check "restarted chain changed" (get (build target next_set) = steps);
        Manifest.write_json path (Manifest.certificate_json saved);
        let invalid = Journal.{ old_record with
          finalize = C.{ old_record.finalize with
            precommits = List.map (fun (vote : C.vote) ->
              C.{ vote with signature = String.make 64 '\000' })
              old_record.finalize.precommits;
          };
        } in
        entries := [next_record, next_index; invalid, old_index];
        begin match build target next_set with
        | Error _ -> ()
        | Ok _ -> failwith "invalid bridge signature was accepted"
        end;
        entries := [next_record, next_index];
        begin match build target next_set with
        | Error _ -> ()
        | Ok _ -> failwith "missing short proof was accepted"
        end;
        let unchanged = get (Manifest.load_certificate path) in
        check "failed build replaced saved certificate"
          (Manifest.certificate_json unchanged = Manifest.certificate_json saved))))

exception Cycle_complete

exception Progress_failed

let test_watch deps prepared =
  let open Lwt.Infix in
  let module Archive = Octra_bootstrap.Sync_archive in
  List.iter (fun mode ->
    Test_workspace.with_dir "sync_watch" (fun root ->
      let started = Atomic.make false in
      let allowed = Atomic.make false in
      let finished = Atomic.make false in
      let clock = Mtime_clock.counter () in
      let tick, wake = Lwt.task () in
      let physical = ref (Lwt.return_ok 0L) in
      let watched = ref (Lwt.return_ok 0L) in
      let ticks = ref 0 in
      let deps = Publish.{ deps with
        sleep = (fun _ -> if mode = "clock" then raise Progress_failed else tick);
        info = (fun _ -> incr ticks; if mode = "log" then raise Progress_failed);
      } in
      let work = Archive.run_lwt root (fun owner ->
        let target = Archive.path owner "writing" in
        let stage = target ^ ".next" in
        Unix.mkdir stage 0o750;
        Archive.mark_stage owner "writing";
        let capture = Lwt_preemptive.detach (fun () ->
          Atomic.set started true;
          while not (Atomic.get allowed)
            && Mtime.Span.to_float_ns (Mtime_clock.count clock) < 5e9 do
            Thread.delay 0.001
          done;
          if not (Atomic.get allowed) then failwith "writer release timed out";
          In_channel.with_open_bin (Filename.concat stage ".writer")
            (fun channel -> ignore (In_channel.input_all channel));
          let output = open_out_bin (Filename.concat stage "ledger.dat") in
          Fun.protect ~finally:(fun () -> close_out_noerr output)
            (fun () -> output_string output "complete"; flush output;
              Unix.fsync (Unix.descr_of_out_channel output));
          Atomic.set finished true;
          if mode = "write" then failwith "writer failed";
          Ok 42L) () in
        physical := capture;
        let monitor = Publish.monitor_capture deps prepared target capture in
        watched := monitor;
        monitor) in
      let rec wait count =
        if Atomic.get started then Lwt.return_unit
        else if count = 0 then Lwt.fail_with "writer did not start"
        else Lwt_unix.sleep 0.001 >>= fun () -> wait (count - 1) in
      let settle task =
        Lwt.catch (fun () -> Lwt.protected task >|= fun _ -> ())
          (fun _ -> Lwt.return_unit) in
      Lwt_main.run (Lwt.finalize
        (fun () ->
          wait 2000 >>= fun () ->
          begin match mode with
          | "log" -> Lwt.wakeup wake ()
          | "timer" -> Lwt.wakeup_exn wake Progress_failed
          | "cancel" -> Lwt.cancel !watched
          | _ -> ()
          end;
          Lwt.pause () >>= fun () ->
          check "capture observer released active archive"
            (Archive.run root (fun _ -> ()) = Error "state sync archive is in use");
          if List.mem mode ["cancel"; "log"; "timer"; "clock"] then
            Lwt.cancel !watched;
          Lwt.pause () >>= fun () ->
          check "capture observer cancellation released active archive"
            (Archive.run root (fun _ -> ()) = Error "state sync archive is in use");
          check "capture observer completed before writer"
            (Lwt.state work = Lwt.Sleep && not (Atomic.get finished));
          Atomic.set allowed true;
          work >|= fun result ->
          check "capture observer lost writer completion" (Atomic.get finished);
          begin match mode, result with
          | ("log" | "timer" | "clock"), Error reason ->
              check "capture observer hid progress failure"
                (reason = Printexc.to_string Progress_failed)
          | "cancel", Error reason ->
              check "capture observer lost cancellation" (reason = Printexc.to_string Lwt.Canceled)
          | "write", Error reason ->
              check "capture observer hid write failure" (reason = Printexc.to_string (Failure "writer failed"))
          | "ok", Ok 42L -> ()
          | _ -> failwith "capture observer result differs"
          end;
          check "capture observer retained archive after completion"
            (Archive.run root (fun _ -> ()) = Ok ());
          check "capture observer lost written bytes"
            (In_channel.with_open_bin (Filename.concat root "writing.next/ledger.dat")
              In_channel.input_all = "complete");
          check "capture observer missed progress failure" (mode <> "log" || !ticks = 1))
        (fun () ->
          Atomic.set allowed true;
          settle !physical >>= fun () -> settle work))))
    ["log"; "timer"; "clock"; "cancel"; "write"; "ok"]

let test_publisher () =
  Test_workspace.with_dir "sync_publisher" (fun root ->
    with_store (Filename.concat root "irmin") (fun store ->
      set store "producer" "preserved";
      let proof = proof store 0 "producer" in
      let chain = Store.open_chaindata (Filename.concat root "chaindata") in
      Fun.protect ~finally:(fun () -> Store.close chain) (fun () ->
        let txid = Store.next_txid chain in
        check "producer input txid differs" (txid = 0L);
        let hash = sha "producer transaction" in
        let module Eic = Octra_core.Epoch_index_commitment in
        let index_hash, index_root = Eic.next_root ~prev:Eic.genesis_root ~epoch_id:0
          [Eic.item ~txid ~hash] in
        let record, _ = finality ~index_root 0 initial proof in
        Store.begin_batch chain;
        Store.save_tx chain ~hash ~epoch_id:0 ~from_addr:wallet.address
          ~to_addr:wallet.address ~tx_json:"{}" ~op_type:"standard"
          ~encrypted_data:"" ~message:"";
        Store.set_epoch chain Octra_core.Epochlog.{ empty_epoch_header with
          id = 0; start_txid = txid; tx_count = 1; finalized_at = 0.;
          state_root = Checkpoint.raw_to_hex record.finalize.header.proposed_state_root;
          proposer = { creator_addr = wallet.address; commit_round = 0 } };
        Store.set_epoch_index_commitment chain ~epoch_id:0 ~epoch_hash:index_hash ~root:index_root;
        Store.commit_batch chain;
        Store.fsync chain;
        let head = Lwt_main.run (Publish.epoch_head ~store ~chaindata:chain 0L record) |> get in
        let path = Filename.concat root "certificate.json" in
        let reads = ref 0 in
        let available = ref true in
        let deps = Publish.{
          data_dir = root; chain_id; store; chaindata = chain; wallet;
          force_publish = false;
          config_hash = (fun () -> Ok (sha "config"));
          trusted_validator_set = (fun () -> Ok trusted);
          head = (fun () -> Some head);
          read_finality = (fun epoch ->
            incr reads;
            if !available && epoch = 0L then Journal.Valid record else Journal.Missing);
          read_root = (fun _ -> failwith "genesis has no parent roots");
          exporter_set = (fun () -> Ok trusted);
          certificate_path = (fun () -> path);
          now = (fun () -> 1.);
          sleep = (fun seconds ->
            if seconds = 15. then Lwt.fail Cycle_complete else Lwt_unix.sleep seconds);
          info = ignore;
          warn = ignore;
        } in
        let run deps = Lwt_main.run (Lwt.catch
          (fun () -> Publish.run deps)
          (function Cycle_complete -> Lwt.return_unit | exn -> Lwt.fail exn)) in
        run deps;
        let certificate = Manifest.load_certificate path |> get in
        ignore (Manifest.verify_certificate ~validator_set:trusted ~exporter_set:trusted certificate |> get);
        check "producer captured wrong epoch" (certificate.checkpoint.epoch = 0L);
        check "producer changed ledger root" (certificate.checkpoint.ledger_state_root = proof.ledger_state_root);
        let count = !reads in
        run deps;
        check "producer did not reuse verified publication" (!reads = count);
        check "producer restart changed certificate"
          (Manifest.load_certificate path |> get = certificate);
        let prepared = Lwt_main.run (Publish.prepare ~chain_id
          ~config_hash:(sha "config") ~trusted_validator_set:trusted
          ~validator_set:initial ~steps:[] ~head record) |> get in
        test_watch deps prepared;
        check "capture observer changed published certificate"
          (Manifest.load_certificate path |> get = certificate);
        Sync_retry_case.retry ~run ~validators:trusted deps certificate;
        Sync_retry_case.retention ~run deps certificate;
        Sync_retry_case.publication ~run ~validators:trusted deps certificate;
        Sync_retry_case.stages ~run deps certificate;
        Sync_retry_case.links ~run deps certificate;
        let prior = saved ~count:1 (Filename.concat root "prior") in
        Sync_retry_case.archived_links ~run deps prior;
        let count = !reads in
        let invalid = Manifest.{ certificate with authority = Finalized "invalid" } in
        Manifest.write_json path (Manifest.certificate_json invalid);
        available := false;
        run deps;
        check "producer accepted invalid stored authority" (!reads > count);
        check "failed producer replaced stored certificate"
          (Manifest.load_certificate path |> get = invalid))))

let () =
  if Array.length Sys.argv <> 2 then failwith "manifest cli path is required";
  Test_workspace.with_dir "sync_cli" (fun root ->
    let certificate = saved ~count:5 ~width:650_000 (Filename.concat root "history") in
    Sync_cli_case.run ~command:Sys.argv.(1) ~root ~validators:trusted certificate);
  test_repeated_sets ();
  test_limit ();
  test_publisher ();
  print_endline "status = pass test = sync_chain"