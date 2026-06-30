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


module Transaction = Octra_core.Transaction
module C_types = Octra_consensus.C_types
module C_engine = Octra_consensus.C_engine
module C_hash = Octra_consensus.C_hash

type encoded = string list * string list * string list

type decoded = {
  tx_hashes : string list;
  txs : Transaction.t list;
  receipts_json : string list;
}

type frozen = {
  header : C_types.epoch_header;
  tx_hashes : string list;
  txs : Transaction.t list;
  receipts_json : string list;
}

type stats = {
  stores : int;
  hits : int;
  misses : int;
  evictions : int;
  cache_size : int;
  fifo_size : int;
}

type cached =
  | Missing
  | Decode_error of string
  | Cached of decoded

type t = {
  cache : (string, encoded) Hashtbl.t;
  fifo : string Queue.t;
  cap : int;
  frozen : (string, frozen) Hashtbl.t;
  mutable stores : int;
  mutable hits : int;
  mutable misses : int;
  mutable evictions : int;
}

let create ~cap =
  {
    cache = Hashtbl.create 8;
    fifo = Queue.create ();
    cap;
    frozen = Hashtbl.create 16;
    stores = 0;
    hits = 0;
    misses = 0;
    evictions = 0;
  }

let stats t =
  {
    stores = t.stores;
    hits = t.hits;
    misses = t.misses;
    evictions = t.evictions;
    cache_size = Hashtbl.length t.cache;
    fifo_size = Queue.length t.fifo;
  }

let encode_txs txs =
  List.map
    (fun tx -> Yojson.Safe.to_string (Transaction.to_yojson tx))
    txs

let evict_excess t =
  let rec loop () =
    if Hashtbl.length t.cache > t.cap then
      match Queue.take_opt t.fifo with
      | Some old_pid ->
        if Hashtbl.mem t.cache old_pid then begin
          Hashtbl.remove t.cache old_pid;
          t.evictions <- t.evictions + 1
        end;
        loop ()
      | None -> ()
  in
  loop ()

let store t ~pid ~tx_hashes ~txs ~receipts_json =
  let txs_json = encode_txs txs in
  let is_new = not (Hashtbl.mem t.cache pid) in
  if is_new then Queue.push pid t.fifo;
  Hashtbl.replace t.cache pid (tx_hashes, txs_json, receipts_json);
  t.stores <- t.stores + 1;
  evict_excess t;
  if t.stores mod 50 = 0 then Some (stats t) else None

let peek_raw t pid =
  Hashtbl.find_opt t.cache pid

let lookup_raw t pid =
  match Hashtbl.find_opt t.cache pid with
  | Some raw ->
    t.hits <- t.hits + 1;
    Some raw
  | None ->
    t.misses <- t.misses + 1;
    None

let parse_txs (_tx_hashes, txs_json, _receipts_json) =
  List.fold_left
    (fun acc tx_json ->
       match acc with
       | Error _ -> acc
       | Ok lst ->
         try
           let json = Yojson.Safe.from_string tx_json in
           match Transaction.of_yojson json with
           | Ok tx -> Ok (tx :: lst)
           | Error e -> Error ("of_yojson: " ^ e)
         with exn ->
           Error (Printexc.to_string exn))
    (Ok [])
    txs_json
  |> Result.map List.rev

let decode (tx_hashes, txs_json, receipts_json as raw) =
  if List.length tx_hashes <> List.length txs_json then
    Error "bundle length mismatch"
  else
    match parse_txs raw with
    | Error _ as e -> e
    | Ok parsed ->
      let recomputed = List.map Transaction.hash parsed in
      if recomputed <> tx_hashes then
        Error "bundle tx hash mismatch"
      else
        match Octra_core.Preverify_commit.check_strings receipts_json parsed with
        | Ok () ->
          Ok {
            tx_hashes;
            txs = parsed;
            receipts_json;
          }
        | Error e ->
          Error ("bundle preverify: " ^ e)

let cached t pid =
  match lookup_raw t pid with
  | None -> Missing
  | Some raw ->
    match decode raw with
    | Ok bundle -> Cached bundle
    | Error e -> Decode_error e

let pid_label pid =
  Digestif.SHA256.(to_hex (of_raw_string pid))
  |> fun hex -> String.sub hex 0 16

let log_summary (stats : stats) =
  Octra_log.info "bundle_cache"
    "summary stores = %d hits = %d misses = %d evictions = %d cache_size = %d fifo_size = %d"
    stats.stores stats.hits stats.misses stats.evictions
    stats.cache_size stats.fifo_size

let store_with_log t ~pid ~tx_hashes ~txs ~receipts_json =
  match store t ~pid ~tx_hashes ~txs ~receipts_json with
  | None -> ()
  | Some stats -> log_summary stats

let cached_with_log t pid =
  match cached t pid with
  | Missing -> None
  | Cached bundle -> Some bundle
  | Decode_error e ->
    Octra_log.error "bundle_cache"
      "cached bundle decode failed pid = %s error = %s"
      (pid_label pid)
      e;
    None

let receipt_root_matches header receipts_json =
  C_hash.receipt_root receipts_json = header.C_types.receipt_root

let header_has_empty_bundle header =
  header.C_types.tx_list_hash = C_engine.tx_list_hash_for_header []
  && header.C_types.receipt_root = C_hash.receipt_root []

let store_empty_header_with_log t header =
  let pid = C_hash.proposal_id header in
  store_with_log t ~pid ~tx_hashes:[] ~txs:[] ~receipts_json:[]

let freeze_key ~epoch_id ~round =
  Printf.sprintf "%Ld:%d" epoch_id round

let freeze t key frozen =
  Hashtbl.replace t.frozen key frozen

let find_frozen t key =
  Hashtbl.find_opt t.frozen key

let prune_frozen t ~finalized_epoch =
  let doomed =
    Hashtbl.fold
      (fun key frozen acc ->
         if frozen.header.C_types.epoch_id <= finalized_epoch then key :: acc
         else acc)
      t.frozen
      []
  in
  List.iter (Hashtbl.remove t.frozen) doomed