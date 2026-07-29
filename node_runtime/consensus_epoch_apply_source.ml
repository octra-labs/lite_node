(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Transaction = Octra_core.Transaction
module C_types = Octra_consensus.C_types
module C_hash = Octra_consensus.C_hash

type cached_bundle = string list * Transaction.t list * string list

type effect =
  | Store_empty_bundle of C_types.epoch_header

type selected = {
  txs : Transaction.t list;
  receipts_json : string list;
  effects : effect list;
}

type fatal =
  | Override_preverify_failed of string
  | Missing_finalized_header
  | Receipt_root_mismatch of { proposal_id : string }
  | Missing_canonical_bundle of { proposal_id : string }

type deps = {
  check_override_receipts :
    epoch_id:int ->
    receipts:string list ->
    Transaction.t list ->
    (unit, string) result;
  find_finalized : int -> C_types.finalize option;
  cached_bundle : string -> cached_bundle option;
  receipt_root_matches : C_types.epoch_header -> string list -> bool;
  header_has_empty_bundle : C_types.epoch_header -> bool;
  staging_txs : unit -> Transaction.t list;
}

type node_deps = {
  check_override_receipts :
    epoch_id:int ->
    receipts:string list ->
    Transaction.t list ->
    (unit, string) result;
  find_finalized : int -> C_types.finalize option;
  cached_bundle : string -> cached_bundle option;
  receipt_root_matches : C_types.epoch_header -> string list -> bool;
  header_has_empty_bundle : C_types.epoch_header -> bool;
  staging_txs : unit -> Transaction.t list;
  store_empty_bundle : C_types.epoch_header -> unit;
  fatal : string -> unit;
  exit : unit -> Transaction.t list * string list;
}

type request = {
  epoch_id : int;
  override_ordered_txs : Transaction.t list option;
  override_receipts_json : string list option;
  consensus_mode : bool;
}

let proposal_id_short proposal_id =
  Digestif.SHA256.(
    proposal_id
    |> of_raw_string
    |> to_hex)
  |> fun s -> String.sub s 0 16

let fatal_lines ~epoch_id = function
  | Override_preverify_failed e ->
    [
      Printf.sprintf
        "override_finalized epoch = %d preverify_gate = failed reason = %s"
        epoch_id
        e;
    ]
  | Missing_finalized_header ->
    [
      Printf.sprintf
        "bft_consensus_finalized epoch = %d pending_finalized_header = missing action = refuse_apply"
        epoch_id;
    ]
  | Receipt_root_mismatch { proposal_id } ->
    [
      Printf.sprintf
        "bft_finalized epoch = %d pid = %s receipt_root = mismatch action = refuse_apply"
        epoch_id
        (proposal_id_short proposal_id);
    ]
  | Missing_canonical_bundle { proposal_id } ->
    [
      Printf.sprintf
        "bft_finalized epoch = %d pid = %s canonical_bundle = missing action = refuse_apply"
        epoch_id
        (proposal_id_short proposal_id);
      "recovery = restart_node_and_bundle_catchup_or_wipe_state";
    ]

let selected ?(effects = []) txs receipts_json =
  { txs; receipts_json; effects }

let choose_override (deps : deps) request txs =
  let receipts =
    Option.value request.override_receipts_json ~default:[]
  in
  match deps.check_override_receipts ~epoch_id:request.epoch_id ~receipts txs with
  | Ok () -> Ok (selected txs receipts)
  | Error e -> Error (Override_preverify_failed e)

let choose_consensus (deps : deps) request =
  match deps.find_finalized request.epoch_id with
  | None -> Error Missing_finalized_header
  | Some finalize ->
    let header = finalize.C_types.header in
    let proposal_id = C_hash.proposal_id header in
    match deps.cached_bundle proposal_id with
    | Some (_tx_hashes, txs, receipts_json) ->
      if deps.receipt_root_matches header receipts_json then
        Ok (selected txs receipts_json)
      else
        Error (Receipt_root_mismatch { proposal_id })
    | None ->
      if deps.header_has_empty_bundle header then
        Ok (selected [] [] ~effects:[Store_empty_bundle header])
      else
        Error (Missing_canonical_bundle { proposal_id })

let choose (deps : deps) request =
  match request.override_ordered_txs with
  | Some txs -> choose_override deps request txs
  | None when request.consensus_mode -> choose_consensus deps request
  | None -> Ok (selected (deps.staging_txs ()) [])

let run (deps : deps) request ~apply_effect ~fatal ~exit =
  match choose deps request with
  | Ok selected ->
    List.iter apply_effect selected.effects;
    selected.txs, selected.receipts_json
  | Error error ->
    fatal_lines ~epoch_id:request.epoch_id error
    |> List.iter fatal;
    exit ()

let deps_of_node (deps : node_deps) =
  {
    check_override_receipts = deps.check_override_receipts;
    find_finalized = deps.find_finalized;
    cached_bundle = deps.cached_bundle;
    receipt_root_matches = deps.receipt_root_matches;
    header_has_empty_bundle = deps.header_has_empty_bundle;
    staging_txs = deps.staging_txs;
  }

let apply_node_effect (deps : node_deps) = function
  | Store_empty_bundle header ->
    deps.store_empty_bundle header

let run_node (deps : node_deps) request =
  run
    (deps_of_node deps)
    request
    ~apply_effect:(apply_node_effect deps)
    ~fatal:deps.fatal
    ~exit:deps.exit