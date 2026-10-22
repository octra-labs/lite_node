(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C_types = Octra_consensus.C_types
module C_hash = Octra_consensus.C_hash
module C_driver = Octra_consensus.C_driver
module Bundle_fetch = Consensus_bundle_fetch
module Bundle_validation = Consensus_bundle_validation
module Log = Octra_log

type deps = {
  check_finality : C_types.finalize -> unit;
  finish_applied : C_types.finalize -> bool;
  write_finality : C_types.finalize -> unit;
  persist_finality_certificate :
    validator_set:C_types.validator_set ->
    C_types.finalize ->
    C_types.finalize;
  store_proposer : C_types.finalize -> unit;
  persist_finality_bundle :
    C_types.finalize ->
    Consensus_finality_journal.bundle ->
    unit;
  chaos_after_finality_log : unit -> unit;
  cached_bundle : proposal_id:string -> bool;
  cached_bundle_data :
    proposal_id:string ->
    (string list * Octra_core.Transaction.t list * string list) option;
  cached_bundle_len : proposal_id:string -> int;
  header_has_empty_bundle : C_types.epoch_header -> bool;
  store_empty_bundle : C_types.epoch_header -> unit;
  query_bundle :
    epoch_id:int64 ->
    proposal_id:string ->
    validate:(C_driver.bundle_response_record -> bool) ->
    C_driver.bundle_response_record option Lwt.t;
  store_accepted_bundle :
    proposal_id:string ->
    Bundle_fetch.accepted ->
    unit;
  sleep : float -> unit Lwt.t;
  range_bundle : C_types.epoch_header -> timeout:float -> Bundle_fetch.accepted option Lwt.t;
  bundle_wait_timeout_seconds : float;
  bundle_wait_expired : epoch_id:int64 -> unit;
  post_finalize : epoch_id:int64 -> proposed_root:string -> Consensus_post_finalize.result Lwt.t;
}

type node_deps = {
  check_finality : C_types.finalize -> unit;
  finish_applied : C_types.finalize -> bool;
  write_finality : C_types.finalize -> unit;
  persist_finality_certificate :
    validator_set:C_types.validator_set ->
    C_types.finalize ->
    C_types.finalize;
  store_proposer : C_types.finalize -> unit;
  persist_finality_bundle :
    C_types.finalize ->
    Consensus_finality_journal.bundle ->
    unit;
  chaos_after_finality_log : unit -> unit;
  cached_bundle : proposal_id:string -> bool;
  cached_bundle_data :
    proposal_id:string ->
    (string list * Octra_core.Transaction.t list * string list) option;
  cached_bundle_len : proposal_id:string -> int;
  wait_bundle : proposal_id:string -> unit Lwt.t;
  header_has_empty_bundle : C_types.epoch_header -> bool;
  store_empty_bundle : C_types.epoch_header -> unit;
  driver : unit -> C_driver.t option;
  set_proposal : Octra_core.Transaction.t list -> string list -> unit;
  store_proposal_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Octra_core.Transaction.t list ->
    receipts_json:string list ->
    unit;
  sleep : float -> unit Lwt.t;
  bundle_wait_timeout_seconds : float;
  bundle_wait_expired : epoch_id:int64 -> unit;
  post_finalize : epoch_id:int64 -> proposed_root:string -> Consensus_post_finalize.result Lwt.t;
}

let range_payload ~(header : C_types.epoch_header) (response : C_driver.catchup_range_response_record) =
  match response.status, response.records with
  | "ok", [record] when record.epoch_id = header.C_types.epoch_id ->
    let bundle = C_driver.{ responder_addr = response.responder_addr;
      tx_hashes = record.tx_hashes; txs_json = record.txs_json;
      receipts_json = record.receipts_json } in
    Result.to_option (Bundle_validation.finalized ~header bundle)
    |> Option.map (fun bundle -> Bundle_fetch.{ responder_addr = response.responder_addr; bundle })
  | _ -> None

let range_bundle input header ~timeout =
  let open Lwt.Syntax in
  let validate response = Option.is_some (range_payload ~header response) in
  let from_epoch = header.C_types.epoch_id in
  let peer = match input.driver () with
    | None -> Lwt.return_none
    | Some driver -> C_driver.query_catchup_range driver ~from_epoch ~max_epochs:1
        ~timeout_seconds:timeout ~validate ~prove:validate in
  let http =
    let* () = input.sleep (min 1. (timeout /. 2.)) in
    Consensus_join_rpc.http_range ~budget:timeout Sys.getenv_opt
      ~from_epoch ~max_epochs:1 ~validate in
  let read () =
    let* response = Consensus_catchup_shell.first_success ~sleep:input.sleep [peer; http] in
    Lwt.return (Option.bind response (range_payload ~header)) in
  Lwt.catch (fun () -> Lwt_unix.with_timeout timeout read)
    (function Lwt_unix.Timeout -> Lwt.return_none | exn -> Lwt.fail exn)

let node_deps input =
  {
    check_finality = input.check_finality;
    finish_applied = input.finish_applied;
    write_finality = input.write_finality;
    persist_finality_certificate = input.persist_finality_certificate;
    store_proposer = input.store_proposer;
    persist_finality_bundle = input.persist_finality_bundle;
    chaos_after_finality_log = input.chaos_after_finality_log;
    cached_bundle = input.cached_bundle;
    cached_bundle_data = input.cached_bundle_data;
    cached_bundle_len = input.cached_bundle_len;
    header_has_empty_bundle = input.header_has_empty_bundle;
    store_empty_bundle = input.store_empty_bundle;
    query_bundle = (fun ~epoch_id ~proposal_id ~validate ->
      match input.driver () with
      | None -> Lwt.return_none
      | Some driver ->
        let query =
          C_driver.query_bundle
            driver
            ~epoch_id
            ~proposal_id
            ~timeout_seconds:5.0
            ~validate
        in
        Bundle_fetch.join ~ready:(input.wait_bundle ~proposal_id) query);
    store_accepted_bundle = (fun ~proposal_id accepted ->
      let bundle = accepted.Bundle_fetch.bundle in
      input.set_proposal bundle.txs bundle.tx_hashes;
      input.store_proposal_bundle
        ~proposal_id
        ~tx_hashes:bundle.tx_hashes
        ~txs:bundle.txs
        ~receipts_json:bundle.receipts_json);
    sleep = input.sleep;
    range_bundle = range_bundle input;
    bundle_wait_timeout_seconds = input.bundle_wait_timeout_seconds;
    bundle_wait_expired = input.bundle_wait_expired;
    post_finalize = input.post_finalize;
  }

let bundle_deps (deps : deps) header proposal_id =
  Bundle_fetch.{
    cached_bundle = (fun () ->
      deps.cached_bundle ~proposal_id);
    header_has_empty_bundle = (fun () ->
      deps.header_has_empty_bundle header);
    store_empty_bundle = (fun () ->
      deps.store_empty_bundle header);
    validate_bundle = (fun response ->
      match Bundle_validation.finalized ~header response with
      | Ok bundle -> Some bundle
      | Error _ -> None);
    query_bundle = (fun ~validate ->
      deps.query_bundle
        ~epoch_id:header.C_types.epoch_id
        ~proposal_id
        ~validate);
    store_accepted_bundle = (fun accepted ->
      deps.store_accepted_bundle ~proposal_id accepted);
  }

let journal_bundle tx_hashes txs receipts_json =
  Consensus_finality_journal.{
    tx_hashes;
    txs;
    receipts_json;
  }

let persist_available_bundle (deps : deps) finalize proposal_id =
  match deps.cached_bundle_data ~proposal_id with
  | Some (tx_hashes, txs, receipts_json) ->
    deps.persist_finality_bundle
      finalize
      (journal_bundle tx_hashes txs receipts_json);
    true
  | None ->
    false

let check_cached_bundle (deps : deps) finalize =
  let proposal_id = C_hash.proposal_id finalize.C_types.header in
  match deps.cached_bundle_data ~proposal_id with
  | None -> ()
  | Some (hashes, txs, receipts) ->
    match Consensus_finality_journal.validate_bundle finalize
            (journal_bundle hashes txs receipts) with
    | Ok () -> ()
    | Error reason -> failwith reason

let seconds () =
  Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e9

let rec await_bundle ?(ranged = false) (deps : deps) header proposal_id delay deadline =
  let open Lwt.Syntax in
  let remaining = deadline -. seconds () in
  let* state =
    if remaining <= 0. then Lwt.return Bundle_fetch.Finalized_deferred
    else
      Lwt.catch
        (fun () ->
          Lwt_unix.with_timeout remaining (fun () ->
            Bundle_fetch.ensure_finalized
              (bundle_deps deps header proposal_id)
              ~epoch_id:header.C_types.epoch_id))
        (function
          | Lwt_unix.Timeout -> Lwt.return Bundle_fetch.Finalized_deferred
          | exn -> Lwt.fail exn)
  in
  match state with
  | Bundle_fetch.Finalized_ready ->
    Lwt.return_true
  | Bundle_fetch.Finalized_deferred ->
    let remaining = deadline -. seconds () in
    if remaining <= 0. then begin
      Log.error "consensus"
        "event = finalized_bundle_wait epoch = %Ld action = quarantine"
        header.C_types.epoch_id;
      deps.bundle_wait_expired ~epoch_id:header.C_types.epoch_id;
      Lwt.return_false
    end else if not ranged then begin
      let* accepted = deps.range_bundle header ~timeout:(min 10. remaining) in
      Option.iter (deps.store_accepted_bundle ~proposal_id) accepted;
      if deps.cached_bundle ~proposal_id then Lwt.return_true
      else await_bundle ~ranged:true deps header proposal_id delay deadline
    end else
      let wait = min delay remaining in
      let* () = deps.sleep wait in
      await_bundle
        ~ranged
        deps
        header
        proposal_id
        (min 5.0 (delay *. 2.0))
        deadline

let run (deps : deps) ~validator_set finalize =
  let open Lwt.Syntax in
  check_cached_bundle deps finalize;
  deps.check_finality finalize;
  if deps.finish_applied finalize then Lwt.return Consensus_post_finalize.Applied else
  let finalize = deps.persist_finality_certificate ~validator_set finalize in
  deps.store_proposer finalize;
  let header = finalize.C_types.header in
  let round = finalize.C_types.commit_round in
  let proposal_id = C_hash.proposal_id header in
  let bundle_persisted =
    persist_available_bundle deps finalize proposal_id
  in
  deps.write_finality finalize;
  deps.chaos_after_finality_log ();
  Log.info "consensus"
    "event = finalized_certificate epoch = %Ld round = %d txs = %d"
    header.C_types.epoch_id
    round
    (deps.cached_bundle_len ~proposal_id);
  let waiting =
    await_bundle
      deps
      header
      proposal_id
      1.0
      (seconds () +. max 0.001 deps.bundle_wait_timeout_seconds)
  in
  let finished = ref false in
  let rec applied () =
    if deps.finish_applied finalize then begin
      finished := true;
      Lwt.return_true
    end else
    let* () = deps.sleep 0.05 in
    applied () in
  let* ready = if Lwt.is_sleeping waiting then Lwt.pick [waiting; applied ()] else waiting in
  if !finished || deps.finish_applied finalize then Lwt.return Consensus_post_finalize.Applied else
  if not ready then
    Lwt.return (Consensus_post_finalize.Deferred
      ("finalized_bundle_unavailable:" ^ Int64.to_string header.C_types.epoch_id))
  else begin
  check_cached_bundle deps finalize;
  if
    not bundle_persisted
    && not (persist_available_bundle deps finalize proposal_id)
  then
    failwith "finalized bundle ready without cached data";
  deps.post_finalize
    ~epoch_id:header.C_types.epoch_id
    ~proposed_root:header.C_types.proposed_state_root
  end