(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module R = Epoch_replay
module J = R.J
module Store = Octra_core.Store_irmin
module Data = Octra_core.Store_chaindata
module Ledger = Octra_core.Ledger
module Head = Octra_core.Head_manifest
module Floor = Octra_core.History_floor
module Rule = Octra_core.Rule_graph
module C = Octra_consensus.C_config
module V = Octra_core.Validator_set_update
module Manifest = Octra_bootstrap.State_sync_manifest
module Anchor = Octra_bootstrap.Sync_anchor
module Roots = Octra_bootstrap.Root_win
module Verify = Octra_bootstrap.State_sync_verify
module Migration = Octra_core.Pvac_migration_admission

let read path limit =
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr input) (fun () ->
    let size = in_channel_length input in
    R.require "replay input exceeds size limit" (size <= limit);
    really_input_string input size)

let some name = function
  | Some value -> value
  | None -> failwith ("replay missing " ^ name)

let entries name =
  some name (Sys.getenv_opt name)
  |> String.split_on_char ','
  |> List.map String.trim
  |> Manifest.validator_set_of_entries
  |> R.get

let certificate path =
  let value = R.get (Manifest.load_certificate path) in
  let validators = entries "OCTRA_VALIDATORS" in
  let exporters = entries "OCTRA_STATE_SYNC_EXPORTERS" in
  R.get (Manifest.verify_certificate
    ~validator_set:validators ~exporter_set:exporters value)

let digest value = Digestif.SHA256.(to_hex (digest_string value))

let file_digest path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
    let buffer = Bytes.create 65536 in
    let rec loop hash =
      match input channel buffer 0 (Bytes.length buffer) with
      | 0 -> Digestif.SHA256.(to_hex (get hash))
      | length -> loop (Digestif.SHA256.feed_bytes hash ~off:0 ~len:length buffer)
    in
    loop Digestif.SHA256.empty)

let prepare cert_path =
  let cert = certificate cert_path in
  let checkpoint = cert.Manifest.checkpoint in
  let configured_hash =
    R.get (Octra_node_runtime.State_sync_http.configured_config_hash ()) in
  let trust = R.get (Epoch_store.network ~getenv:Sys.getenv_opt
    ~chain_id:checkpoint.chain_id ~config_hash:checkpoint.config_hash
    ~configured_hash) in
  let worker = some "proof worker executable" (Sys.getenv_opt "OCTRA_PVAC_VERIFY_WORKER") in
  Unix.access worker [Unix.X_OK];
  let worker_hash = file_digest worker in
  R.get (Octra_core.Pvac_verify_worker.ready_sync ());
  cert, trust, worker_hash

let environment () =
  Unix.environment ()
  |> Array.to_list
  |> List.filter (fun line ->
    String.starts_with ~prefix:"OCTRA_" line || String.starts_with ~prefix:"PVAC_" line)
  |> List.map (fun line ->
    if String.starts_with ~prefix:"OCTRA_PVAC_VERIFY_WORKER=" line then
      "OCTRA_PVAC_VERIFY_WORKER=<version-worker>"
    else line)
  |> List.sort String.compare
  |> String.concat "\n"
  |> digest

let trace_json (trace : R.trace) =
  let strings values = `List (List.map (fun value -> `String value) values) in
  `Assoc [
    "event", `String "epoch";
    "epoch", `String (Int64.to_string trace.epoch);
    "ledger_root", `String trace.ledger_root;
    "index_root", `String trace.index_root;
    "state_root", `String trace.state_root;
    "confirmed", strings trace.confirmed;
    "rejections", strings trace.rejections;
    "fees", `String (Z.to_string trace.fees);
    "candidate_root", `String trace.candidate_root;
    "candidate_fees", `String (Z.to_string trace.candidate_fees);
  ]

let write output value =
  output_string output (Yojson.Safe.to_string value);
  output_char output '\n';
  flush output;
  Unix.fsync (Unix.descr_of_out_channel output)

let run ~data ~cert_path ~range_paths ~output =
  let open Lwt.Syntax in
  let pages = R.Input.prepare range_paths in
  let cert, trust, worker_hash = prepare cert_path in
  let checkpoint = cert.Manifest.checkpoint in
  let chain_id = checkpoint.chain_id in
  let ready_config_hash = checkpoint.config_hash in
  let marker = read (Filename.concat data "replay_copy") 256 |> String.trim in
  R.require "replay requires an explicit snapshot copy" (marker = cert.manifest_hash);
  let* verified = Verify.verify checkpoint data in
  R.get verified;
  let head = some "snapshot head" (Head.load data) in
  let encoded = some "checkpoint finality" (Manifest.finality cert) in
  let anchor = R.get (Anchor.decode encoded) in
  let signed_roots = Roots.read (Filename.concat data Roots.name) |> R.get in
  R.require "replay ready root window is incomplete"
    (List.length signed_roots = min head.epoch_id Roots.width);
  let signed_roots = R.get (Roots.verify ~anchor:(Anchor.finality anchor) signed_roots) in
  let roots = ref ((head.epoch_id, head.state_root) :: signed_roots) in
  let chain = Data.open_chaindata (Filename.concat data "chaindata") in
  Lwt.finalize (fun () ->
    let floor = some "history floor" (R.get (Data.history_floor chain)) in
    let* store = Store.open_store (Filename.concat data "irmin_store") in
    Lwt.finalize (fun () ->
      let ledger = Ledger.create store in
      ignore (R.get (Ledger.freeze ledger));
      let root_at epoch =
        let stored = Option.map (fun header -> header.Octra_core.Epochlog.state_root)
          (Data.get_epoch_header chain epoch) in
        Roots.select ~stored ~signed:(List.assoc_opt epoch !roots)
      in
      let rules = Rule.create_ready ~ready_config_hash ~chain_id ~root_at:(fun epoch ->
        match root_at epoch with
        | Error reason -> Rule.Unreadable reason
        | Ok (Some root) -> Rule.Root root
        | Ok None ->
          match Rule.root_after_floor ~chain_id ~floor_epoch:(Floor.epoch floor) ~epoch with
          | Some root -> Rule.Root root
          | None -> Rule.Missing)
      in
      let migration = R.get (Migration.load_env ~chain_id ~data_dir:data ~getenv:Sys.getenv_opt) in
      R.get (Migration.bind_floor migration ~config_hash:ready_config_hash
        ~floor_config_hash:(Floor.config_hash floor) ~floor_epoch:(Floor.epoch floor));
      let activation = R.get (Octra_core.Private_result_policy.activation_epoch_of Sys.getenv_opt) in
      let context = Epoch_store.{
        store; ledger; chaindata = chain; rules; trust; chain_id;
        ready_root = (fun epoch -> Lwt.return (R.get (root_at epoch)));
        legacy_replay = Migration.decision migration;
        result_policy = Octra_core.Private_result_policy.for_epoch ~activation_epoch:activation;
      } in
      let cursor = J.{
        epoch = Int64.succ checkpoint.epoch;
        prev_root = head.state_root;
        eic = some "epoch index" head.epoch_index_root;
        txid = Int64.succ head.txid_hi;
      } in
      let rec loop cursor count = function
        | [] -> Lwt.return (cursor, count)
        | (record : J.record) :: rest ->
          let* active = Store.get_meta store V.active_meta_key in
          let* pending = Store.get_meta store V.pending_meta_key in
          let source = Octra_node_runtime.Consensus_validator_anchor.{
            getenv = Sys.getenv_opt; chain_id;
            current_height = (fun () -> Int64.pred cursor.J.epoch);
            active_raw = (fun () -> active);
            pending_raw = (fun () -> pending);
          } in
          let validators = R.get
            (Octra_node_runtime.Consensus_validator_anchor.expected_set source ~epoch:record.epoch_id) in
          let prepared = J.prepare_record ~chain_id
            ~expected_validator_set_hash:(C.validator_set_hash validators) ~cursor record in
          let deps = Epoch_store.deps context ~cursor ~prepared in
          let* trace = R.run deps ~cursor ~prepared in
          roots := (prepared.epoch_int, trace.state_root) :: !roots;
          write output (trace_json trace);
          Printf.printf "event = replay_epoch status = verified epoch = %Ld\n%!" trace.epoch;
          loop prepared.next_cursor (count + 1) rest
      in
      let rec ranges cursor count = function
        | [] -> Lwt.return (cursor, count)
        | page :: rest ->
          let records =
            R.Input.read page |> Yojson.Safe.from_string
            |> J.parse_range ~from_epoch:cursor.J.epoch
          in
          begin match records with
          | J.Records (_ :: _ as records) ->
            let* cursor, count = loop cursor count records in
            ranges cursor count rest
          | _ -> Lwt.fail_with "replay range is empty or unavailable"
          end
      in
      write output (`Assoc [
        "event", `String "start";
        "scope", `String "ledger_execution";
        "manifest", `String cert.manifest_hash;
        "range_sha256", `String (R.Input.hash pages);
        "environment_sha256", `String (environment ());
        "binary_sha256", `String (file_digest Sys.executable_name);
        "worker_sha256", `String worker_hash;
        "first_epoch", `String (Int64.to_string cursor.epoch);
      ]);
      let* final, count = ranges cursor 0 pages in
      write output (`Assoc [
        "event", `String "complete";
        "epochs", `Int count;
        "next_epoch", `String (Int64.to_string final.epoch);
        "state_root", `String final.prev_root;
      ]);
      Lwt.return_unit)
      (fun () -> Store.close store))
    (fun () -> Data.close chain; Lwt.return_unit)

let () =
  try
    match Array.to_list Sys.argv with
    | [_; "--check"; cert_path] ->
      let cert, _, worker_hash = prepare cert_path in
      Printf.printf "event = replay_check status = pass manifest = %s worker_sha256 = %s\n%!"
        cert.Manifest.manifest_hash worker_hash
    | _ :: data :: cert_path :: range_path :: output_path :: more ->
      let descriptor = Unix.openfile output_path
        [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o600 in
      let output = Unix.out_channel_of_descr descriptor in
      Fun.protect ~finally:(fun () -> close_out_noerr output) (fun () ->
        Lwt_main.run (run ~data ~cert_path ~range_paths:(range_path :: more) ~output))
    | _ -> failwith "usage: replay DATA_COPY CERTIFICATE RANGE OUTPUT [RANGE ...] | replay --check CERTIFICATE"
  with error ->
    Printf.eprintf "event = replay status = fail reason = %s\n%!" (Printexc.to_string error);
    exit 1