(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C_codec = Octra_consensus.C_codec
module C_catchup = Octra_consensus.C_catchup
module C_hash = Octra_consensus.C_hash
module Epochlog = Octra_core.Epochlog
module Transaction = Octra_core.Transaction

type deps = {
  chain_id : string;
  get_epoch_json : int -> string option;
  epoch_time : int -> float option;
  get_tx_by_txid : int64 -> (string * string) option;
  read_receipts : int -> string list;
  root_to_raw32 : string -> string;
  reward_source :
    int ->
    Epochlog.epoch_header ->
    (Octra_consensus.C_types.reward_source, string) result;
  read_finality : int -> C_codec.catchup_finality option;
}

type state = {
  offset : int;
  records : C_codec.catchup_epoch_record list;
  total_bytes : int;
  next_epoch : int64 option;
}

let parse_txs txs_json =
  List.fold_left
    (fun acc tx_json ->
       match acc with
       | Error _ -> acc
       | Ok txs ->
         try
           match Yojson.Safe.from_string tx_json |> Transaction.of_yojson with
           | Ok tx -> Ok (tx :: txs)
           | Error e -> Error e
         with exn -> Error (Printexc.to_string exn))
    (Ok [])
    txs_json
  |> Result.map List.rev

let read_txs deps ~start_txid ~tx_count =
  let rec loop k hashes txs ok =
    if k >= tx_count then
      if ok then Some (List.rev hashes, List.rev txs)
      else None
    else
      let txid = Int64.add start_txid (Int64.of_int k) in
      match deps.get_tx_by_txid txid with
      | None -> loop (k + 1) hashes txs false
      | Some (hash, json) -> loop (k + 1) (hash :: hashes) (json :: txs) ok
  in
  loop 0 [] [] true

let wire_bytes strings =
  List.fold_left (fun acc s -> acc + 32 + String.length s) 0 strings

let record_size reward_source finality txs_json receipts_json =
  let reward_bytes =
    Octra_net.Oce1.encode
      (fun buf ->
        Octra_consensus.C_reward_source.encode_into buf reward_source)
    |> String.length
  in
  let finality_bytes =
    C_codec.encode_finalize finality.C_codec.finalize
    |> String.length
  in
  let validator_bytes =
    C_codec.encode_validator_set finality.C_codec.validator_set
    |> String.length
  in
  136
  + wire_bytes txs_json
  + wire_bytes receipts_json
  + reward_bytes
  + finality_bytes
  + validator_bytes

let tx_list_hash tx_hashes =
  Octra_net.Hash_domain.hash "octra:tx_list:v1" (String.concat "" tx_hashes)

let log_proposer_divergence ~epoch_id elog =
  if String.length elog.Epochlog.finalized_by > 3
     && String.length elog.proposer.Epochlog.creator_addr > 3
     && elog.finalized_by <> elog.proposer.creator_addr then
    Octra_log.warn "catchup"
      "lookup_catchup_range epoch = %Ld proposer = %s finalized_by = %s"
      epoch_id
      (Text.addr_short elog.proposer.creator_addr)
      (Text.addr_short elog.finalized_by)

let build_record deps ~epoch_id ~target_int ~finality elog tx_hashes txs_json parsed_txs =
  let receipts_json = deps.read_receipts target_int in
  match deps.reward_source target_int elog with
  | Error error ->
    Octra_log.warn "catchup"
      "lookup_catchup_range epoch = %Ld reward_source = invalid reason = %s"
      epoch_id error;
    None
  | Ok reward_source ->
  match deps.epoch_time target_int with
  | None ->
    Octra_log.warn "catchup"
      "lookup_catchup_range epoch = %Ld canonical_time = missing"
      epoch_id;
    None
  | Some epoch_ts ->
    begin
      match Octra_consensus.Epoch_time.of_seconds epoch_ts with
      | Error error ->
        Octra_log.warn "catchup"
          "lookup_catchup_range epoch = %Ld canonical_time = invalid reason = %s"
          epoch_id error;
        None
      | Ok _ ->
        match Octra_core.Tx_outcome.decode ~confirmed:parsed_txs receipts_json with
        | Error e ->
          Octra_log.warn "catchup"
            "lookup_catchup_range epoch = %Ld outcome_check_failed = %s"
            epoch_id e;
          None
        | Ok partition ->
          match Octra_core.Preverify_receipt_policy.check
            ~epoch_id:target_int
            ~receipts:partition.preverify
            parsed_txs with
          | Error e ->
            Octra_log.warn "catchup"
              "lookup_catchup_range epoch = %Ld receipt_check_failed = %s"
              epoch_id e;
            None
          | Ok () ->
          log_proposer_divergence ~epoch_id elog;
          let record =
            C_codec.{
              epoch_id;
              prev_state_root = deps.root_to_raw32 elog.Epochlog.prev_state_root;
              state_root = deps.root_to_raw32 elog.state_root;
              tx_list_hash = tx_list_hash tx_hashes;
              tx_hashes;
              txs_json;
              receipt_root = C_hash.receipt_root receipts_json;
              receipts_json;
              epoch_ts;
              creator_addr = elog.proposer.creator_addr;
              commit_round = elog.proposer.commit_round;
              reward_source = Some reward_source;
              finality = Some finality;
            }
          in
          let expected_txid =
            Int64.add
              elog.Epochlog.start_txid
              (Int64.of_int (List.length tx_hashes))
          in
          begin
            match
              C_catchup.verify_record_finality
                ~chain_id:deps.chain_id
                ~expected_validator_set_hash:
                  (Octra_consensus.C_config.validator_set_hash
                     finality.validator_set)
                ~expected_txid
                ~record
            with
            | Ok _ ->
              Some (
                record,
                record_size reward_source finality txs_json receipts_json)
            | Error error ->
              Octra_log.warn "catchup"
                "lookup_catchup_range epoch = %Ld finality = invalid reason = %s"
                epoch_id
                error;
              None
          end
    end

let read_record deps ~epoch_id ~target_int json =
  match Epochlog.epoch_of_json json with
  | None -> None
  | Some elog ->
    match deps.read_finality target_int with
    | None ->
      Octra_log.warn "catchup"
        "lookup_catchup_range epoch = %Ld finality = missing"
        epoch_id;
      None
    | Some finality ->
      match read_txs deps ~start_txid:elog.start_txid ~tx_count:elog.tx_count with
      | None -> None
      | Some (tx_hashes, txs_json) ->
        match parse_txs txs_json with
        | Error e ->
          Octra_log.warn "catchup"
            "lookup_catchup_range epoch = %Ld tx_parse_failed = %s"
            epoch_id e;
          None
        | Ok parsed_txs ->
          build_record
            deps
            ~epoch_id
            ~target_int
            ~finality
            elog
            tx_hashes
            txs_json
            parsed_txs

let finish state =
  match List.rev state.records with
  | [] -> `NotFound
  | records -> `Ok (records, state.next_epoch)

let next_after_full_chunk ~from_epoch offset =
  Some (Int64.add from_epoch (Int64.of_int offset))

let range ?(max_chunk = 16) ?(max_bytes = 4_000_000) deps ~from_epoch ~max_epochs =
  try
    let max_chunk = min max_epochs max_chunk in
    let rec collect state =
      if state.offset >= max_chunk then finish state
      else
        let epoch_id = Int64.add from_epoch (Int64.of_int state.offset) in
        let target_int = Int64.to_int epoch_id in
        match deps.get_epoch_json target_int with
        | None ->
          if state.records = [] then `NotFound
          else `Ok (List.rev state.records, Some epoch_id)
        | Some json ->
          match read_record deps ~epoch_id ~target_int json with
          | None -> finish state
          | Some (record, size) ->
            if state.total_bytes + size > max_bytes && state.records <> [] then
              `Ok (List.rev state.records, Some epoch_id)
            else
              let offset = state.offset + 1 in
              let next_epoch =
                if offset >= max_chunk then next_after_full_chunk ~from_epoch offset
                else state.next_epoch
              in
              collect {
                offset;
                records = record :: state.records;
                total_bytes = state.total_bytes + size;
                next_epoch;
              }
    in
    collect {
      offset = 0;
      records = [];
      total_bytes = 0;
      next_epoch = None;
    }
  with exn ->
    `Internal (Printexc.to_string exn)