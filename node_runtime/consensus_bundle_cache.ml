(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Transaction = Octra_core.Transaction
module C_types = Octra_consensus.C_types
module C_engine = Octra_consensus.C_engine
module C_hash = Octra_consensus.C_hash

type encoded = string list * string list * string list

type decoded = {
  tx_hashes : string list;
  txs : Transaction.t list;
  receipts_json : string list;
  rejections : Octra_core.Tx_outcome.rejection list;
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
  shared : (string, Transaction.t * int) Hashtbl.t;
  shared_fifo : string Queue.t;
  shared_cap : int;
  shared_limit : int;
  mutable shared_bytes : int;
  frozen : (string, frozen) Hashtbl.t;
  preverify : (string, Octra_core.Preverify_worker.checked Lwt.t) Hashtbl.t;
  preverify_fifo : string Queue.t;
  mutable stores : int;
  mutable hits : int;
  mutable misses : int;
  mutable evictions : int;
}

type node_runtime = {
  cached_bundle : string -> (string list * Transaction.t list * string list) option;
  store_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Transaction.t list ->
    receipts_json:string list ->
    unit;
  receipt_root_matches : C_types.epoch_header -> string list -> bool;
  header_has_empty_bundle : C_types.epoch_header -> bool;
  store_empty_bundle : C_types.epoch_header -> unit;
  store_empty_proposal : proposal_id:string -> unit;
  lookup_raw : string -> encoded option;
}

let create_with_limits ~cap ~shared_cap ~shared_limit =
  {
    cache = Hashtbl.create 8;
    fifo = Queue.create ();
    cap;
    shared = Hashtbl.create 16;
    shared_fifo = Queue.create ();
    shared_cap = max 1 shared_cap;
    shared_limit = max 1 shared_limit;
    shared_bytes = 0;
    frozen = Hashtbl.create 16;
    preverify = Hashtbl.create 8;
    preverify_fifo = Queue.create ();
    stores = 0;
    hits = 0;
    misses = 0;
    evictions = 0;
  }

let create ~cap =
  create_with_limits
    ~cap
    ~shared_cap:1024
    ~shared_limit:(32 * 1024 * 1024)

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

let tx_bytes tx =
  Yojson.Safe.to_string (Transaction.to_yojson tx)
  |> String.length

let evict_shared t =
  let rec loop () =
    if Hashtbl.length t.shared > t.shared_cap
       || t.shared_bytes > t.shared_limit then
      match Queue.take_opt t.shared_fifo with
      | Some hash ->
        begin
          match Hashtbl.find_opt t.shared hash with
          | Some (_, bytes) ->
            Hashtbl.remove t.shared hash;
            t.shared_bytes <- t.shared_bytes - bytes
          | None -> ()
        end;
        loop ()
      | None -> ()
  in
  loop ()

let share t txs =
  List.iter
    (fun tx ->
       let hash = Transaction.hash tx in
       if not (Hashtbl.mem t.shared hash) then begin
         let bytes = tx_bytes tx in
         if bytes <= t.shared_limit then begin
           Hashtbl.add t.shared hash (tx, bytes);
           Queue.push hash t.shared_fifo;
           t.shared_bytes <- t.shared_bytes + bytes;
           evict_shared t
         end
       end)
    txs;
  List.filter_map
    (fun tx ->
       let hash = Transaction.hash tx in
       if Hashtbl.mem t.shared hash then Some hash else None)
    txs

let find_shared t hash =
  Hashtbl.find_opt t.shared hash
  |> Option.map fst

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
           match Octra_core.Tx_payload.decode json with
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
        match Octra_core.Tx_outcome.split receipts_json with
        | Error error -> Error ("bundle outcome: " ^ error)
        | Ok artifacts ->
          begin
            match
              Octra_core.Preverify_commit.check_strings
                artifacts.preverify
                parsed
            with
            | Error error -> Error ("bundle preverify: " ^ error)
            | Ok () ->
              match
                Octra_core.Tx_outcome.merge
                  ~confirmed:parsed
                  ~rejections:artifacts.rejections
              with
              | Error error -> Error ("bundle outcome: " ^ error)
              | Ok _ ->
                Ok {
                  tx_hashes;
                  txs = parsed;
                  receipts_json;
                  rejections = artifacts.rejections;
                }
          end

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

let node_runtime t =
  {
    cached_bundle = (fun pid ->
      cached_with_log t pid
      |> Option.map (fun (bundle : decoded) ->
        bundle.tx_hashes,
        bundle.txs,
        bundle.receipts_json));
    store_bundle = (fun ~proposal_id ~tx_hashes ~txs ~receipts_json ->
      store_with_log t ~pid:proposal_id ~tx_hashes ~txs ~receipts_json);
    receipt_root_matches;
    header_has_empty_bundle;
    store_empty_bundle = store_empty_header_with_log t;
    store_empty_proposal = (fun ~proposal_id ->
      store_with_log t ~pid:proposal_id ~tx_hashes:[] ~txs:[] ~receipts_json:[]);
    lookup_raw = lookup_raw t;
  }

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

let add_key_part buffer value =
  Buffer.add_string buffer (string_of_int (String.length value));
  Buffer.add_char buffer ':';
  Buffer.add_string buffer value

type preverify_purpose =
  | Build_proposal
  | Validate_proposal

let preverify_purpose_tag = function
  | Build_proposal -> "build"
  | Validate_proposal -> "validate"

let preverify_item_key ~purpose ~state_root ~tx_hash =
  let buffer = Buffer.create 256 in
  add_key_part buffer (preverify_purpose_tag purpose);
  add_key_part buffer state_root;
  add_key_part buffer tx_hash;
  Octra_net.Hash_domain.hash
    "octra:consensus_preverify_item_cache:v1"
    (Buffer.contents buffer)

let evict_preverify t =
  let rec loop () =
    if Hashtbl.length t.preverify > t.cap then
      match Queue.take_opt t.preverify_fifo with
      | Some key ->
        Hashtbl.remove t.preverify key;
        loop ()
      | None -> ()
  in
  loop ()

let run_preverify_item_once t ~purpose ~state_root ~tx_hash verify =
  let key = preverify_item_key ~purpose ~state_root ~tx_hash in
  match Hashtbl.find_opt t.preverify key with
  | Some job ->
    Lwt.protected job
  | None ->
    let job =
      try verify () with exn -> Lwt.fail exn
    in
    Hashtbl.add t.preverify key job;
    Queue.push key t.preverify_fifo;
    Lwt.on_success job (fun checked ->
      if not (Octra_core.Preverify_worker.checked_cacheable checked) then
        match Hashtbl.find_opt t.preverify key with
        | Some current when current == job -> Hashtbl.remove t.preverify key
        | Some _ | None -> ());
    Lwt.on_failure job (fun _ ->
      match Hashtbl.find_opt t.preverify key with
      | Some current when current == job ->
        Hashtbl.remove t.preverify key
      | Some _ | None -> ());
    evict_preverify t;
    Lwt.protected job

let run_preverify_once t ~purpose ~state_root ~tx_hashes ~txs verify =
  let recomputed = List.map Transaction.hash txs in
  if recomputed <> tx_hashes then
    Lwt.fail_with "consensus preverify hash mismatch"
  else
    let verify_item tx =
      let tx_hash = Transaction.hash tx in
      run_preverify_item_once
        t
        ~purpose
        ~state_root
        ~tx_hash
        (fun () ->
          let open Lwt.Syntax in
          let* batch = verify state_root [tx] in
          match Octra_core.Preverify_worker.checked_of_single_batch tx batch with
          | Ok checked -> Lwt.return checked
          | Error reason -> Lwt.fail_with reason)
    in
    Octra_core.Preverify_worker.run_checked verify_item txs