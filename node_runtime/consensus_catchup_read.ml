(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


module C_codec = Octra_consensus.C_codec
module C_hash = Octra_consensus.C_hash
module Epochlog = Octra_core.Epochlog
module Transaction = Octra_core.Transaction

type deps = {
  get_epoch_json : int -> string option;
  get_tx_by_txid : int64 -> (string * string) option;
  read_receipts : int -> string list;
  root_to_raw32 : string -> string;
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

let record_size txs_json receipts_json =
  128 + wire_bytes txs_json + wire_bytes receipts_json

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

let build_record deps ~epoch_id ~target_int elog tx_hashes txs_json parsed_txs =
  let receipts_json = deps.read_receipts target_int in
  match Octra_core.Preverify_receipt_policy.check
    ~epoch_id:target_int
    ~receipts:receipts_json
    parsed_txs with
  | Error e ->
    Octra_log.warn "catchup"
      "lookup_catchup_range epoch = %Ld receipt_check_failed = %s"
      epoch_id e;
    None
  | Ok () ->
    log_proposer_divergence ~epoch_id elog;
    Some (
      C_codec.{
        epoch_id;
        prev_state_root = deps.root_to_raw32 elog.Epochlog.prev_state_root;
        state_root = deps.root_to_raw32 elog.state_root;
        tx_list_hash = tx_list_hash tx_hashes;
        tx_hashes;
        txs_json;
        receipt_root = C_hash.receipt_root receipts_json;
        receipts_json;
        creator_addr = elog.proposer.creator_addr;
        commit_round = elog.proposer.commit_round;
      },
      record_size txs_json receipts_json)

let read_record deps ~epoch_id ~target_int json =
  match Epochlog.epoch_of_json json with
  | None -> None
  | Some elog ->
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
        build_record deps ~epoch_id ~target_int elog tx_hashes txs_json parsed_txs

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