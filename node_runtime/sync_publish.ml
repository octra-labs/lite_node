(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Infix

module Anchor = Octra_bootstrap.Sync_anchor
module Capture = Octra_bootstrap.Sync_capture
module Chain = Sync_chain
module Checkpoint = Octra_bootstrap.State_sync_checkpoint
module Cycle = Octra_bootstrap.Sync_cycle
module Epochlog = Octra_core.Epochlog
module Head = Octra_core.Head_manifest
module Manifest = Octra_bootstrap.State_sync_manifest
module State_sync = Octra_bootstrap.State_sync
module C_config = Octra_consensus.C_config
module C_types = Octra_consensus.C_types
module Irmin = Octra_core.Store_irmin
module Store = Octra_core.Store_chaindata

type prepared = {
  head : Head.t;
  checkpoint : Checkpoint.body;
  checkpoint_hash : string;
  anchor : Anchor.t;
  trusted_validator_set : C_types.validator_set;
}

type deps = {
  data_dir : string;
  chain_id : string;
  store : Octra_core.Store_irmin.t;
  chaindata : Octra_core.Store_chaindata.t;
  wallet : Octra_core.Crypto.Wallet.t;
  config_hash : unit -> (string, string) result;
  trusted_validator_set : unit -> (C_types.validator_set, string) result;
  head : unit -> Head.t option;
  read_finality : int64 -> Consensus_finality_journal.read_result;
  exporter_set : unit -> (C_types.validator_set, string) result;
  certificate_path : unit -> string;
  now : unit -> float;
  sleep : float -> unit Lwt.t;
  info : string -> unit;
  warn : string -> unit;
}

let ( let* ) value f =
  match value with
  | Ok item -> f item
  | Error _ as error -> error

let timestamp value =
  match classify_float value with
  | FP_normal | FP_subnormal | FP_zero when value >= 0. ->
      Ok (Int64.of_float value)
  | FP_normal | FP_subnormal | FP_zero
  | FP_infinite | FP_nan ->
      Error "state sync finalized time is invalid"

let epoch_head ~store ~chaindata epoch record =
  if Int64.compare epoch 0L < 0
     || Int64.compare epoch (Int64.of_int max_int) >= 0 then
    Lwt.return_error "state sync epoch exceeds platform range"
  else if record.Consensus_finality_journal.finalize.C_types.epoch_id <> epoch then
    Lwt.return_error "state sync finality epoch differs from target"
  else
    let epoch_id = Int64.to_int epoch in
    match Store.epoch_boundary chaindata epoch_id with
    | Error reason -> Lwt.return_error reason
    | Ok boundary ->
        Irmin.epoch_binding store epoch_id >|= function
        | Error _ as error -> error
        | Ok binding ->
            let finalize = record.finalize in
            let finality = finalize.C_types.header in
            let state_root = Checkpoint.raw_to_hex finality.proposed_state_root in
            let folded =
              Octra_core.Epoch_index_commitment.folded_state_root
                ~ledger_state_root:binding.Irmin.root
                ~epoch_index_root:boundary.Store.index_root
            in
            if boundary.header.Epochlog.id <> epoch_id then
              Error "state sync epoch header differs from target"
            else if boundary.header.state_root <> state_root
                    || folded <> state_root then
              Error "state sync historical state root differs from finality"
            else if boundary.txid_hi <> finality.txid_hi then
              Error "state sync historical transaction high-water differs"
            else if boundary.header.finalized_at <> finality.ts then
              Error "state sync historical time differs from finality"
            else if boundary.header.proposer.creator_addr <> finality.creator_addr
                    || boundary.header.proposer.commit_round
                       <> finalize.commit_round then
              Error "state sync historical proposer differs from finality"
            else
              Ok Head.{
                schema_version = Head.schema_version;
                generation = epoch_id;
                epoch_id;
                state_root;
                ledger_state_root = Some binding.root;
                irmin_commit = Some binding.commit;
                txid_hi = boundary.txid_hi;
                txlog_seg = Some boundary.txlog_seg;
                txlog_off = Some boundary.txlog_off;
                epochlog_off = Some boundary.epochlog_off;
                commit_id = Anchor.finalize_hash finalize;
                ts = finality.ts;
                quorum_cert_hash = None;
                epoch_index_hash = Some boundary.index_hash;
                epoch_index_root = Some boundary.index_root;
              }

let prepare ~chain_id ~config_hash ~trusted_validator_set ~validator_set
    ~steps ~head record =
  let header = record.Consensus_finality_journal.finalize.C_types.header in
  let epoch = record.finalize.epoch_id in
  let active_hash = C_config.validator_set_hash validator_set in
  let finality_hash = C_config.validator_set_hash record.validator_set in
  if active_hash <> finality_hash then
    Error "state sync finality validator set is not active"
  else if Int64.compare epoch (Int64.of_int max_int) >= 0 then
    Error "state sync finalized epoch exceeds platform limit"
  else if head.Head.epoch_id <> Int64.to_int epoch then
    Error "state sync HEAD epoch differs from finality"
  else if head.ts <> header.ts then
    Error "state sync HEAD time differs from finality"
  else
    let* index_root =
      match head.epoch_index_root with
      | Some value -> Ok value
      | None -> Error "state sync HEAD index root is missing"
    in
    let folded =
      Octra_core.Epoch_index_commitment.folded_state_root
        ~ledger_state_root:(Head.ledger_state_root head)
        ~epoch_index_root:index_root
    in
    if folded <> head.state_root then
      Error "state sync HEAD folded root is invalid"
    else
      let* created_at = timestamp header.ts in
      let validity = 2_678_400L in
      if Int64.compare created_at (Int64.sub Int64.max_int validity) > 0 then
        Error "state sync validity interval overflows"
      else
        let anchor =
          Anchor.make
            ~steps
            ~finalize:record.finalize
            ~validator_set:record.validator_set
        in
        let quorum_cert_hash = Some (Anchor.finalize_hash record.finalize) in
        let head = Head.{ head with quorum_cert_hash } in
        let checkpoint =
          Checkpoint.of_head
            ~chain_id
            ~config_hash
            ~validator_set_hash:(Manifest.raw_to_hex finality_hash)
            ~created_at
            ~valid_until:(Int64.add created_at validity)
            head
        in
        let* () = Anchor.verify_checkpoint checkpoint anchor in
        let* derived = Anchor.derive ~validator_set:trusted_validator_set anchor in
        if C_config.validator_set_hash derived <> active_hash then
          Error "state sync validator transition does not reach active set"
        else
        let* checkpoint_hash = Checkpoint.hash checkpoint in
        Ok {
          head;
          checkpoint;
          checkpoint_hash;
          anchor;
          trusted_validator_set;
        }

let retention_plan ~retain ~current snapshots =
  let ordered =
    List.sort
      (fun (left_id, left_epoch) (right_id, right_epoch) ->
        let epoch_order = compare right_epoch left_epoch in
        if epoch_order <> 0 then epoch_order
        else String.compare left_id right_id)
      snapshots
  in
  let kept =
    ordered
    |> List.filter_map (fun (id, _) -> Some id)
    |> List.filteri (fun index _ -> index < retain)
    |> fun ids -> List.sort_uniq String.compare (current :: ids)
  in
  ordered
  |> List.filter_map (fun (id, _) ->
    if List.mem id kept then None else Some id)

let rec remove_tree path =
  if Sys.file_exists path then
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
        Sys.readdir path
        |> Array.iter (fun name ->
          if name <> "." && name <> ".." then
            remove_tree (Filename.concat path name));
        Unix.rmdir path
    | _ -> Unix.unlink path

let snapshot_entries root =
  if not (Sys.file_exists root) then []
  else
    Sys.readdir root
    |> Array.to_list
    |> List.filter_map (fun id ->
      if not (State_sync.valid_snapshot_id id) then None
      else
        let path = Filename.concat root id in
        match Head.load_result path with
        | Head.Present head -> Some (id, head.epoch_id)
        | Head.Missing | Head.Corrupt _ -> None)

let staged_snapshot name =
  let suffix = ".next" in
  if String.ends_with ~suffix name then
    let size = String.length name - String.length suffix in
    size > 0
    && State_sync.valid_snapshot_id (String.sub name 0 size)
  else
    false

let active_lease ~now root (id, _) =
  let snapshot = Filename.concat root id in
  let certificate = State_sync.snapshot_certificate_path snapshot in
  try
    let published_at = (Unix.stat certificate).Unix.st_mtime in
    Sync_lease.read ~now ~published_at ~snapshot_id:id snapshot
  with Unix.Unix_error _ -> None

let retain data_dir ~retain ~current =
  let root = State_sync.snapshot_root data_dir in
  if Sys.file_exists root then begin
    let snapshots = snapshot_entries root in
    let leased =
      snapshots
      |> List.filter_map (active_lease ~now:(Unix.gettimeofday ()) root)
      |> Sync_lease.retained
    in
    let removed =
      snapshots
      |> retention_plan ~retain ~current
      |> List.filter (fun id -> not (List.mem id leased))
    in
    List.iter (fun id -> remove_tree (Filename.concat root id)) removed;
    Sys.readdir root
    |> Array.iter (fun name ->
      if staged_snapshot name then remove_tree (Filename.concat root name))
  end

let publisher_count = 2

let publisher_addresses exporter_set =
  exporter_set.C_types.validators
  |> List.map (fun item -> item.C_types.address)
  |> List.sort_uniq String.compare
  |> List.filteri (fun index _ -> index < publisher_count)

let exporter_wallet exporter_set wallet =
  match C_types.pubkey_of_addr exporter_set wallet.Octra_core.Crypto.Wallet.address with
  | Some public_key
    when public_key = wallet.pub
         && List.mem wallet.address (publisher_addresses exporter_set) ->
      Ok ()
  | Some public_key when public_key = wallet.pub ->
      Error "state sync publisher is not selected"
  | Some _ -> Error "state sync exporter wallet public key differs"
  | None -> Error "state sync exporter wallet is not configured"

let build_draft prepared target =
  Lwt_preemptive.detach
    (fun () ->
      Manifest.build
        ~checkpoint:prepared.checkpoint
        ~source_dir:target
        ~chunk_size:(16 * 1024 * 1024))
    ()

let clear_stage target =
  Lwt_preemptive.detach
    (fun () -> remove_tree (target ^ ".next"))
    ()

let write_certificate path certificate =
  Lwt_preemptive.detach
    (fun () -> Manifest.write_json path (Manifest.certificate_json certificate))
    ()

let snapshot_certificate_path deps certificate =
  State_sync.snapshot_dir deps.data_dir certificate.Manifest.manifest.snapshot_id
  |> State_sync.snapshot_certificate_path

let archive_current deps =
  match Manifest.load_certificate (deps.certificate_path ()) with
  | Error _ -> Lwt.return_unit
  | Ok certificate ->
      let snapshot =
        State_sync.snapshot_dir deps.data_dir certificate.manifest.snapshot_id
      in
      if Sys.file_exists snapshot then
        write_certificate (State_sync.snapshot_certificate_path snapshot) certificate
      else
        Lwt.return_unit

let published_manifest_hash deps checkpoint_hash =
  match Manifest.load_certificate (deps.certificate_path ()) with
  | Ok certificate when certificate.checkpoint_hash = checkpoint_hash ->
      Some certificate.manifest_hash
  | Ok _
  | Error _ -> None

let publish deps exporter_set prepared =
  let target = State_sync.snapshot_dir deps.data_dir prepared.checkpoint_hash in
  deps.info
    (Printf.sprintf
       "event = sync_capture_start epoch = %Ld checkpoint = %s"
       prepared.checkpoint.epoch
       prepared.checkpoint_hash);
  Lwt.catch
    (fun () ->
      let existing () =
        match published_manifest_hash deps prepared.checkpoint_hash with
        | None -> Lwt.return_error "state sync snapshot has no local certificate"
        | Some expected_hash ->
            build_draft prepared target >|= function
            | Error reason -> Error reason
            | Ok draft when draft.manifest_hash <> expected_hash ->
                Error "state sync snapshot differs from local certificate"
            | Ok draft ->
                begin
                  match Manifest.validate_reference_body draft.manifest with
                  | Error reason -> Error reason
                  | Ok () ->
                      Ok
                        (draft,
                         draft.manifest.file_count,
                         draft.manifest.total_size)
                end
      in
      let fresh () =
        clear_stage target >>= fun () ->
        Capture.build
          Capture.{ data_dir = deps.data_dir; head = prepared.head; store = deps.store }
          ~target >>= function
        | Error reason -> Lwt.return_error reason
        | Ok report ->
            build_draft prepared target >|= function
          | Error reason -> Error reason
          | Ok draft -> Ok (draft, report.files, report.bytes)
      in
      let capture () =
        if not (Sys.file_exists target) then fresh ()
        else
          existing () >>= function
          | Ok _ as ready -> Lwt.return ready
          | Error _ ->
              Lwt_preemptive.detach (fun () -> remove_tree target) () >>= fresh
      in
      capture () >>= function
      | Error reason -> Lwt.return_error reason
      | Ok (draft, files, bytes) ->
          begin
            match Manifest.validate_reference_body draft.manifest with
            | Error reason -> Lwt.return_error reason
            | Ok () ->
              begin
                match Manifest.make_exporter_signature
                  ~wallet:deps.wallet
                  draft.manifest with
                | Error reason -> Lwt.return_error reason
                | Ok exporter_signature ->
                    let certificate = Manifest.{
                      checkpoint = prepared.checkpoint;
                      checkpoint_hash = prepared.checkpoint_hash;
                      authority = Finalized (Anchor.encode prepared.anchor);
                      manifest = draft.manifest;
                      manifest_hash = draft.manifest_hash;
                      exporter_signatures = [exporter_signature];
                    } in
                    begin
                      match Manifest.verify_certificate
                        ~validator_set:prepared.trusted_validator_set
                        ~exporter_set
                        certificate with
                      | Error reason -> Lwt.return_error reason
                      | Ok verified ->
                          archive_current deps >>= fun () ->
                          write_certificate
                            (snapshot_certificate_path deps verified)
                            verified >>= fun () ->
                          write_certificate (deps.certificate_path ()) verified
                          >>= fun () ->
                          deps.info
                            (Printf.sprintf
                               "event = sync_published epoch = %Ld checkpoint = %s files = %d bytes = %Ld"
                               verified.checkpoint.epoch
                               verified.checkpoint_hash
                               files
                               bytes);
                          Lwt.return_ok verified.manifest.snapshot_id
                    end
              end
          end)
    (fun exn -> Lwt.return_error (Printexc.to_string exn))

let load_published deps exporter_set =
  let path = deps.certificate_path () in
  if not (Sys.file_exists path) then Lwt.return_none
  else
    match deps.config_hash (),
      deps.trusted_validator_set (),
      Manifest.load_certificate path with
    | Ok expected_config, Ok validator_set, Ok certificate
      when Manifest.is_reference certificate
           && certificate.checkpoint.chain_id = deps.chain_id
           && certificate.checkpoint.config_hash = expected_config
           && Manifest.fresh
                ~now:(Int64.of_float (deps.now ()))
                certificate ->
        begin
          match Manifest.verify_certificate
            ~validator_set
            ~exporter_set
            certificate with
          | Error _ -> Lwt.return_none
          | Ok _ ->
              let target =
                State_sync.snapshot_dir
                  deps.data_dir
                  certificate.manifest.snapshot_id
              in
              Lwt_preemptive.detach
                (fun () ->
                  if not (Sys.file_exists target) then None
                  else
                    match
                      Manifest.build
                        ~checkpoint:certificate.checkpoint
                        ~source_dir:target
                        ~chunk_size:certificate.manifest.chunk_size
                    with
                    | Error _ -> None
                    | Ok draft
                      when draft.manifest_hash = certificate.manifest_hash ->
                        Some certificate.checkpoint.epoch
                    | Ok _ -> None)
                ()
        end
    | _ -> Lwt.return_none

let read_prepared deps epoch =
  match deps.config_hash (), deps.trusted_validator_set () with
  | Error reason, _
  | _, Error reason -> Lwt.return_error reason
  | Ok config_hash, Ok trusted_validator_set ->
      Lwt_preemptive.detach (fun () -> deps.read_finality epoch) () >>= function
      | Consensus_finality_journal.Missing ->
          Lwt.return_error "state sync finality record is missing"
      | Consensus_finality_journal.Invalid reason ->
          Lwt.return_error ("state sync finality record is invalid: " ^ reason)
      | Consensus_finality_journal.Valid record ->
          begin
            match deps.head () with
            | None -> Lwt.return_error "state sync HEAD is missing"
            | Some head when Int64.of_int head.Head.epoch_id < epoch ->
                Lwt.return_error "state sync target is ahead of HEAD"
            | Some _ ->
                epoch_head
                  ~store:deps.store
                  ~chaindata:deps.chaindata
                  epoch
                  record >>= function
                | Error reason -> Lwt.return_error reason
                | Ok head ->
                    let validator_set = record.validator_set in
                    Chain.build
                      Chain.{
                        data_dir = deps.data_dir;
                        chain_id = deps.chain_id;
                        store = deps.store;
                        chaindata = deps.chaindata;
                        read_finality = deps.read_finality;
                        certificate_path = deps.certificate_path;
                      }
                      ~head_epoch:epoch
                      trusted_validator_set
                      validator_set >>= function
                    | Error reason -> Lwt.return_error reason
                    | Ok steps ->
                        Lwt.return
                          (prepare
                             ~chain_id:deps.chain_id
                             ~config_hash
                             ~trusted_validator_set
                             ~validator_set
                             ~steps
                             ~head
                             record)
          end

let capture deps exporter_set epoch =
  read_prepared deps epoch >>= function
  | Error _ as error -> Lwt.return error
  | Ok prepared -> publish deps exporter_set prepared

let rec perform deps exporter_set state effects =
  match effects with
  | [] -> Lwt.return state
  | Cycle.Capture epoch :: rest ->
      capture deps exporter_set epoch >>= fun result ->
      let outcome =
        match result with
        | Ok _ -> Cycle.Published
        | Error reason ->
            deps.warn
              (Printf.sprintf
                 "event = sync_capture_failed epoch = %Ld reason = %s"
                 epoch
                 reason);
            Cycle.Failed
      in
      begin
        match Cycle.step Cycle.default state (Cycle.Completed { epoch; outcome }) with
        | Error reason ->
            deps.warn ("event = sync_cycle_failed reason = " ^ reason);
            Lwt.return state
        | Ok (next, completed) ->
            perform deps exporter_set next (rest @ completed)
      end
  | Cycle.Retain count :: rest ->
      begin
        match Cycle.published state with
        | None -> perform deps exporter_set state rest
        | Some _ ->
            let current =
              match Manifest.load_certificate (deps.certificate_path ()) with
              | Ok certificate -> certificate.manifest.snapshot_id
              | Error _ -> ""
            in
            Lwt.catch
              (fun () ->
                Lwt_preemptive.detach
                  (fun () -> retain deps.data_dir ~retain:count ~current)
                  ())
              (fun exn ->
                deps.warn
                  ("event = sync_retention_failed reason = "
                   ^ Printexc.to_string exn);
                Lwt.return_unit) >>= fun () ->
            perform deps exporter_set state rest
      end

let rec dormant deps =
  deps.sleep 3_600. >>= fun () -> dormant deps

let run_loop deps exporter_set initial =
  let rec loop state =
    Lwt.catch
      (fun () ->
        match deps.head () with
        | None -> Lwt.return state
        | Some head ->
            begin
              match Cycle.step
                Cycle.default
                state
                (Cycle.Finalized (Int64.of_int head.Head.epoch_id)) with
              | Error reason ->
                  deps.warn ("event = sync_cycle_failed reason = " ^ reason);
                  Lwt.return state
              | Ok (next, effects) -> perform deps exporter_set next effects
            end)
      (fun exn ->
        deps.warn
          ("event = sync_loop_failed reason = " ^ Printexc.to_string exn);
        Lwt.return state) >>= fun next ->
    deps.sleep 15. >>= fun () ->
    loop next
  in
  loop initial

let run deps =
  match deps.exporter_set () with
  | Error reason ->
      deps.warn ("event = sync_disabled reason = " ^ reason);
      dormant deps
  | Ok exporter_set ->
      begin
        match exporter_wallet exporter_set deps.wallet with
        | Error reason ->
            deps.info ("event = sync_cycle_dormant reason = " ^ reason);
            dormant deps
        | Ok () ->
            load_published deps exporter_set >>= fun published ->
            deps.info
              (Printf.sprintf
                 "event = sync_cycle_started published = %s interval_epochs = 360 retain = 10"
                 (Option.fold
                    ~none:"none"
                    ~some:Int64.to_string
                    published));
            run_loop deps exporter_set (Cycle.init ~published)
      end