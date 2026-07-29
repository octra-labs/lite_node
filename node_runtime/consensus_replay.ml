(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Transaction = Octra_core.Transaction
module C_types = Octra_consensus.C_types

type plan = {
  header : C_types.epoch_header;
  commit_round : int;
  finalize : C_types.finalize;
  txs : Transaction.t list;
  tx_hashes : string list;
  epoch : int;
  proposer_info : Octra_core.Epochlog.proposer_info option;
  expected_root : string option;
}

let zero32 = String.make 32 '\x00'

let hex_value = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' as c -> 10 + Char.code c - Char.code 'A'
  | _ -> -1

let is_hex_string s =
  String.length s > 0
  && String.for_all (fun c -> hex_value c >= 0) s

let raw32_of_hex64 s =
  String.init 32 (fun i ->
    let hi = hex_value s.[i * 2] in
    let lo = hex_value s.[(i * 2) + 1] in
    Char.chr ((hi lsl 4) lor lo))

let int64_field json name =
  let module U = Yojson.Safe.Util in
  match json |> U.member name with
  | `Int i -> Int64.of_int i
  | `Intlit s -> Int64.of_string s
  | `String s -> Int64.of_string s
  | _ -> failwith (Printf.sprintf "replay header missing %s" name)

let int_field json name default_value =
  let module U = Yojson.Safe.Util in
  match json |> U.member name with
  | `Int i -> i
  | `Intlit s -> int_of_string s
  | `String s -> int_of_string s
  | _ -> default_value

let float_field json name default_value =
  let module U = Yojson.Safe.Util in
  match json |> U.member name with
  | `Float f -> f
  | `Int i -> float_of_int i
  | `Intlit s -> float_of_string s
  | `String s -> float_of_string s
  | _ -> default_value

let raw32_field json name =
  let module U = Yojson.Safe.Util in
  match json |> U.member name |> U.to_string_option with
  | Some s when String.length s = 64 && is_hex_string s ->
    raw32_of_hex64 s
  | Some s when String.length s = 32 ->
    s
  | Some _ ->
    failwith (Printf.sprintf "replay header bad %s" name)
  | None ->
    zero32

let receipt_root_field json =
  let module U = Yojson.Safe.Util in
  match json |> U.member "receipt_root" |> U.to_string_option with
  | Some _ -> raw32_field json "receipt_root"
  | None -> Octra_consensus.C_hash.receipt_root []

let parse_header ~default_chain_id json =
  let module U = Yojson.Safe.Util in
  let proto_version =
    match json |> U.member "proto_version" |> U.to_int_option with
    | Some v -> v
    | None -> C_types.proto_version_current
  in
  let chain_id =
    match json |> U.member "chain_id" |> U.to_string_option with
    | Some s when s <> "" -> s
    | _ -> default_chain_id
  in
  let header = C_types.{
    proto_version;
    chain_id;
    epoch_id = int64_field json "epoch_id";
    prev_state_root = raw32_field json "prev_state_root";
    tx_list_hash = raw32_field json "tx_list_hash";
    receipt_root = receipt_root_field json;
    proposed_state_root = raw32_field json "proposed_state_root";
    parent_commit_hash = raw32_field json "parent_commit_hash";
    creator_addr = json |> U.member "creator_addr" |> U.to_string;
    txid_hi = int64_field json "txid_hi";
    ts = float_field json "ts" 0.0;
  } in
  header, int_field json "commit_round" 0

let parse_bundle json =
  let items =
    match json with
    | `List xs -> xs
    | _ -> failwith "replay bundle must be a JSON list"
  in
  List.map
    (fun item ->
       match Transaction.of_yojson item with
       | Ok tx -> tx
       | Error e -> failwith ("replay bundle tx decode: " ^ e))
    items

let proposer_info (header : C_types.epoch_header) commit_round =
  if String.length header.C_types.creator_addr > 3 then
    Some {
      Octra_core.Epochlog.creator_addr = header.creator_addr;
      commit_round;
    }
  else
    None

let expected_root (header : C_types.epoch_header) =
  if String.length header.C_types.proposed_state_root = 32
     && header.proposed_state_root <> zero32 then
    Some header.proposed_state_root
  else
    None

let finalize (header : C_types.epoch_header) commit_round =
  C_types.{
    chain_id = header.chain_id;
    epoch_id = header.epoch_id;
    commit_round;
    header;
    proposal_id = Octra_consensus.C_hash.proposal_id header;
    precommits = [];
    parent_commit = None;
  }

let build_plan ~(header : C_types.epoch_header) ~commit_round ~txs =
  {
    header;
    commit_round;
    finalize = finalize header commit_round;
    txs;
    tx_hashes = List.map Transaction.hash txs;
    epoch = Int64.to_int header.C_types.epoch_id;
    proposer_info = proposer_info header commit_round;
    expected_root = expected_root header;
  }

let load_plan ~default_chain_id ~header_path ~bundle_path =
  let header, commit_round =
    parse_header
      ~default_chain_id
      (Yojson.Safe.from_file header_path)
  in
  let txs =
    match bundle_path with
    | None -> []
    | Some path -> parse_bundle (Yojson.Safe.from_file path)
  in
  build_plan ~header ~commit_round ~txs

type one_shot_deps = {
  current_epoch : unit -> int;
  store_finalized : int -> C_types.finalize -> unit;
  store_proposer : int -> Octra_core.Epochlog.proposer_info -> unit;
  store_expected_root : int -> string -> unit;
  set_proposal : Transaction.t list -> string list -> unit;
  write_finality : plan -> unit;
  apply : plan -> unit Lwt.t;
  exit_error : unit -> unit;
  exit_success : unit -> unit;
}

type node_one_shot_deps = {
  current_epoch : unit -> int;
  finality : Consensus_finality_state.callbacks;
  set_proposal : Transaction.t list -> string list -> unit;
  write_entry : Octra_consensus.Finality_log.entry -> unit;
  apply : plan -> unit Lwt.t;
  exit_error : unit -> unit;
  exit_success : unit -> unit;
}

let finality_log_entry replay =
  Octra_consensus.Finality_log.of_header
    ~round:replay.commit_round
    replay.header

let node_one_shot_deps (deps : node_one_shot_deps) =
  {
    current_epoch = deps.current_epoch;
    store_finalized = (fun epoch finalize ->
      deps.finality.store_finalized ~epoch finalize);
    store_proposer = deps.finality.store_proposer;
    store_expected_root = (fun epoch root ->
      deps.finality.store_expected_root ~epoch ~root);
    set_proposal = deps.set_proposal;
    write_finality = (fun replay ->
      deps.write_entry (finality_log_entry replay));
    apply = deps.apply;
    exit_error = deps.exit_error;
    exit_success = deps.exit_success;
  }

let run_one_shot ~env ~default_chain_id (deps : one_shot_deps) =
  match env "OCTRA_REPLAY_HEADER_JSON" with
  | None -> Lwt.return_unit
  | Some header_path ->
    let bundle_path = env "OCTRA_REPLAY_BUNDLE_JSON" in
    let exit_after_replay = env "OCTRA_EXIT_AFTER_REPLAY" = Some "1" in
    let replay =
      load_plan
        ~default_chain_id
        ~header_path
        ~bundle_path
    in
    if replay.epoch <> deps.current_epoch () then begin
      Octra_log.fatal "replay"
        "replay header_epoch = %d current_epoch = %d reason = snapshot_not_at_target"
        replay.epoch (deps.current_epoch ());
      deps.exit_error ();
      Lwt.return_unit
    end else begin
      deps.store_finalized replay.epoch replay.finalize;
      Option.iter (deps.store_proposer replay.epoch) replay.proposer_info;
      Option.iter (deps.store_expected_root replay.epoch) replay.expected_root;
      deps.set_proposal replay.txs replay.tx_hashes;
      Octra_log.info "replay"
        "starting one-shot replay epoch = %d txs = %d header = %s bundle = %s"
        replay.epoch
        (List.length replay.txs)
        header_path
        (match bundle_path with Some p -> p | None -> "<none>");
      deps.write_finality replay;
      let open Lwt.Syntax in
      let* () = deps.apply replay in
      Octra_log.info "replay" "completed one-shot replay epoch = %d" replay.epoch;
      if exit_after_replay then deps.exit_success ();
      Lwt.return_unit
    end