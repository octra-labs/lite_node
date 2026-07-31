(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type rows_status = {
  total : int;
  rows : Yojson.Safe.t list;
  missing : int;
  incomplete : bool;
}

type token_rows_page = {
  total : int;
  rows : Yojson.Safe.t list;
  incoming : int;
  outgoing : int;
  has_more : bool;
  missing : int;
  incomplete : bool;
}

let max_txlog_record_len = 128_000_000

type page_profile = {
  fetched : int;
  selected : int;
  rows : int;
  fetch_ms : float;
  resolve_ms : float;
  sort_ms : float;
  page_ms : float;
  total_ms : float;
}

type epoch_index_status = {
  epoch_id : int;
  expected_start_txid : int64;
  expected_tx_count : int;
  checked : int;
  missing_epoch_meta : bool;
  missing_txid_loc : int;
  missing_tx_loc : int;
  missing_addr_refs : int;
  malformed_records : int;
  errors : string list;
}

let epoch_index_status_ok s =
  (not s.missing_epoch_meta)
  && s.missing_txid_loc = 0
  && s.missing_tx_loc = 0
  && s.missing_addr_refs = 0
  && s.malformed_records = 0
  && s.errors = []

let epoch_index_status_issue_count s =
  (if s.missing_epoch_meta then 1 else 0)
  + s.missing_txid_loc
  + s.missing_tx_loc
  + s.missing_addr_refs
  + s.malformed_records
  + List.length s.errors

let history_profile_enabled =
  match Sys.getenv_opt "OCTRA_PROFILE_HISTORY_READS" with
  | Some raw ->
      let value = String.lowercase_ascii raw in
      value = "1" || value = "true" || value = "yes"
  | None -> false

let emit_history_profile ~tag ~cache ~addr ~limit ~offset ~total ~missing profile =
  if history_profile_enabled then
    Octra_log.trace "history"
      "event = read_profile tag = %s cache = %s addr = %s limit = %d offset = %d total = %d fetched = %d selected = %d rows = %d missing = %d fetch_ms = %.2f resolve_ms = %.2f sort_ms = %.2f page_ms = %.2f total_ms = %.2f"
      tag
      cache
      addr
      limit
      offset
      total
      profile.fetched
      profile.selected
      profile.rows
      missing
      profile.fetch_ms
      profile.resolve_ms
      profile.sort_ms
      profile.page_ms
      profile.total_ms

type t = {
  txlog : Txlog.t;
  epochlog : Epochlog.t;
  index : Chaindata_index.t;
  mutable next_txid : int64;
  epoch_status_cache : (int, epoch_index_status) Hashtbl.t;
  epoch_rows_page_cache : (string, rows_status) Hashtbl.t;
  addr_rows_page_cache : (string, rows_status) Hashtbl.t;
  rejected_rows_page_cache : (string, Yojson.Safe.t list) Hashtbl.t;
  token_rows_page_cache : (string, token_rows_page) Hashtbl.t;
}

let rec mkdir_p path =
  if Sys.file_exists path then ()
  else begin
    mkdir_p (Filename.dirname path);
    (try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  end

let open_chaindata ?(readonly=false) base_dir =
  if not readonly then mkdir_p base_dir;
  let txlog_dir = Filename.concat base_dir "txlog" in
  let epochlog_path = Filename.concat (Filename.concat base_dir "epochlog") "epochs.dat" in
  let index_dir = Filename.concat base_dir "index" in
  let txlog = Txlog.open_log ~readonly txlog_dir in
  let epochlog = Epochlog.open_log ~readonly epochlog_path in
  let index = Chaindata_index.open_index ~readonly index_dir in
  let next_txid = match Chaindata_index.get_meta index "next_txid" with
    | Some s -> (try Int64.of_string s with _ -> 0L)
    | None -> 0L in
  {
    txlog;
    epochlog;
    index;
    next_txid;
    epoch_status_cache = Hashtbl.create 256;
    epoch_rows_page_cache = Hashtbl.create 256;
    addr_rows_page_cache = Hashtbl.create 256;
    rejected_rows_page_cache = Hashtbl.create 128;
    token_rows_page_cache = Hashtbl.create 128;
  }

let close t =
  Txlog.close t.txlog;
  Epochlog.close t.epochlog;
  Chaindata_index.close t.index

let begin_batch t =
  Chaindata_index.begin_write t.index

let commit_batch t =
  Chaindata_index.buffer_meta t.index "next_txid" (Int64.to_string t.next_txid);
  Chaindata_index.commit_write t.index

let abort_batch t =
  Chaindata_index.abort_write t.index

let extract_call_recipient ~op_type ~encrypted_data ~message ~from_addr ~to_addr =
  if op_type = "call" && encrypted_data <> "" && message <> "" then
    try
      match Yojson.Safe.from_string message with
      | `List (`String recipient :: _)
        when Crypto.is_octra_address recipient
             && recipient <> from_addr
             && recipient <> to_addr ->
        [recipient]
      | _ -> []
    with _ -> []
  else []

let save_tx t ~hash ~epoch_id ~from_addr ~to_addr ~tx_json
    ~op_type ~encrypted_data ~message =
  let payload = hash ^ tx_json in
  let (seg_id, offset, len) =
    Txlog.append t.txlog ~epoch_id ~payload in
  let txid = t.next_txid in
  t.next_txid <- Int64.add t.next_txid 1L;
  let addrs = extract_call_recipient ~op_type ~encrypted_data
    ~message ~from_addr ~to_addr in
  Chaindata_index.buffer_tx t.index ~hash ~seg_id ~offset ~len
    ~epoch_id ~txid ~from_addr ~to_addr ~addrs

let save_rejected t ~hash ~from_addr ~to_addr ~amount ~nonce
    ~error_type ~reason ~epoch_id ~ts =
  let rj = Yojson.Safe.to_string (`Assoc [
    "hash", `String hash;
    "from_addr", `String from_addr;
    "to_addr", `String to_addr;
    "amount", `String amount;
    "nonce", `Int nonce;
    "error_type", `String error_type;
    "reason", `String reason;
    "epoch_id", `Int epoch_id;
    "ts", `Float ts;
  ]) in
  Chaindata_index.buffer_rejected t.index ~hash ~addr:from_addr ~json:rj ~epoch_id;
  if to_addr <> from_addr then
    Chaindata_index.buffer_rejected t.index ~hash ~addr:to_addr ~json:rj ~epoch_id

let save_receipt t ~tx_hash ~contract_addr ~method_name
    ~success ~effort_used ~events_json ~error ~epoch_id =
  let json = Yojson.Safe.to_string (`Assoc [
    "contract", `String contract_addr;
    "method", `String method_name;
    "success", `Bool success;
    "effort", `Int effort_used;
    "events", events_json;
    "error", (match error with Some e -> `String e | None -> `Null);
    "epoch", `Int epoch_id;
    "ts", `Float (Unix.gettimeofday ());
  ]) in
  Chaindata_index.buffer_receipt t.index tx_hash json

let save_receipt_raw t ~tx_hash ~json =
  Chaindata_index.buffer_receipt t.index tx_hash json

let set_epoch t (h : Epochlog.epoch_header) =
  Epochlog.append t.epochlog h;
  Chaindata_index.buffer_epoch t.index h.id (Epochlog.epoch_to_json h);

  Chaindata_index.buffer_meta t.index "repaired_upto_epoch" (string_of_int h.id);
  Chaindata_index.buffer_meta t.index "index_schema_version" "v2_ascii64_int32be"

let set_epoch_index_commitment t ~epoch_id ~epoch_hash ~root =
  Chaindata_index.buffer_meta t.index
    (Printf.sprintf "eic_epoch_hash:%d" epoch_id)
    epoch_hash;
  Chaindata_index.buffer_meta t.index
    (Printf.sprintf "eic_epoch_root:%d" epoch_id)
    root;
  Chaindata_index.buffer_meta t.index "eic_latest_epoch" (string_of_int epoch_id);
  Chaindata_index.buffer_meta t.index "eic_latest_root" root

let set_epoch_index_commitment_direct t ~epoch_id ~epoch_hash ~root =
  Chaindata_index.set_meta_direct t.index
    (Printf.sprintf "eic_epoch_hash:%d" epoch_id)
    epoch_hash;
  Chaindata_index.set_meta_direct t.index
    (Printf.sprintf "eic_epoch_root:%d" epoch_id)
    root;
  Chaindata_index.set_meta_direct t.index "eic_latest_epoch" (string_of_int epoch_id);
  Chaindata_index.set_meta_direct t.index "eic_latest_root" root

type epoch_index_commitment_status = {
  eic_epoch_id : int;
  eic_stored_hash : string option;
  eic_stored_root : string option;
  eic_actual_hash : string option;
  eic_actual_root : string option;
  eic_ok : bool;
  eic_errors : string list;
}

let epoch_index_hash_key epoch_id =
  Printf.sprintf "eic_epoch_hash:%d" epoch_id

let epoch_index_root_key epoch_id =
  Printf.sprintf "eic_epoch_root:%d" epoch_id

let get_epoch_index_commitment t epoch_id =
  (Chaindata_index.get_meta t.index (epoch_index_hash_key epoch_id),
   Chaindata_index.get_meta t.index (epoch_index_root_key epoch_id))

let fsync t =
  Txlog.fsync t.txlog;
  Epochlog.fsync t.epochlog

type repair_result = {
  checked : int;
  repaired : int;
  errors : string list;
}

let split_payload payload =
  if String.length payload < 64 then ("", payload)
  else (String.sub payload 0 64, String.sub payload 64 (String.length payload - 64))

let find_sub_from s sub start =
  let sl = String.length s in
  let nl = String.length sub in
  let rec matches i j =
    if j = nl then true
    else if s.[i + j] = sub.[j] then matches i (j + 1)
    else false
  in
  let rec loop i =
    if nl = 0 then Some i
    else if i + nl > sl then None
    else if matches i 0 then Some i
    else loop (i + 1)
  in
  loop (max 0 start)

let skip_json_ws s i =
  let len = String.length s in
  let rec loop j =
    if j >= len then j
    else match s.[j] with
      | ' ' | '\n' | '\r' | '\t' -> loop (j + 1)
      | _ -> j
  in
  loop i

let json_field_value_start s key =
  match find_sub_from s ("\"" ^ key ^ "\"") 0 with
  | None -> None
  | Some key_pos ->
    match find_sub_from s ":" (key_pos + String.length key + 2) with
    | None -> None
    | Some colon -> Some (skip_json_ws s (colon + 1))

let json_string_field_prefix s key =
  match json_field_value_start s key with
  | Some start when start < String.length s && s.[start] = '"' ->
    let buf = Buffer.create 64 in
    let rec loop i escaped =
      if i >= String.length s then ""
      else
        let c = s.[i] in
        if escaped then begin
          Buffer.add_char buf c;
          loop (i + 1) false
        end else if c = '\\' then
          loop (i + 1) true
        else if c = '"' then
          Buffer.contents buf
        else begin
          Buffer.add_char buf c;
          loop (i + 1) false
        end
    in
    loop (start + 1) false
  | _ -> ""

let json_number_field_prefix s key =
  match json_field_value_start s key with
  | None -> ""
  | Some start ->
    let len = String.length s in
    let rec loop i =
      if i >= len then i
      else match s.[i] with
        | ',' | '}' | ' ' | '\n' | '\r' | '\t' -> i
        | _ -> loop (i + 1)
    in
    let stop = loop start in
    if stop <= start then "" else String.sub s start (stop - start)

let tx_prefix_to_summary_row hash epoch_id tx_prefix =
  let tx_json_prefix =
    if String.length tx_prefix <= 64 then ""
    else String.sub tx_prefix 64 (String.length tx_prefix - 64)
  in
  let ts =
    match json_number_field_prefix tx_json_prefix "timestamp" with
    | "" -> `Float 0.0
    | raw ->
      try `Float (float_of_string raw)
      with _ -> `Float 0.0
  in
  Some (`Assoc [
    "hash", `String hash;
    "epoch", `Int epoch_id;
    "from", `String (json_string_field_prefix tx_json_prefix "from");
    "to", `String (json_string_field_prefix tx_json_prefix "to_");
    "amount", `String (match json_string_field_prefix tx_json_prefix "amount" with "" -> "0" | s -> s);
    "ou", `String (match json_string_field_prefix tx_json_prefix "ou" with "" -> "0" | s -> s);
    "timestamp", ts;
    "op_type", `String (match json_string_field_prefix tx_json_prefix "op_type" with "" -> "standard" | s -> s);
    "summary_truncated", `Bool true;
  ])

let compute_epoch_index_hash_raw t ~epoch_id ~start_txid ~tx_count =
  let items = ref [] in
  let errors = ref [] in
  for i = 0 to tx_count - 1 do
    let txid = Int64.add start_txid (Int64.of_int i) in
    match Chaindata_index.get_txid_loc_raw t.index txid with
    | None ->
        errors := Printf.sprintf "missing txid_loc txid=%Ld" txid :: !errors
    | Some (seg_id, offset, len) ->
        try
          let record_epoch_id, payload =
            Txlog.read_record t.txlog ~seg_id ~offset ~len
          in
          if record_epoch_id <> epoch_id then
            errors := Printf.sprintf
              "txid=%Ld epoch mismatch expected=%d actual=%d"
              txid epoch_id record_epoch_id :: !errors;
          let hash, _tx_json = split_payload payload in
          try
            items := Epoch_index_commitment.item ~txid ~hash :: !items
          with Invalid_argument msg ->
            errors := Printf.sprintf "txid=%Ld %s" txid msg :: !errors
        with e ->
          errors := Printf.sprintf "txid=%Ld %s" txid (Printexc.to_string e) :: !errors
  done;
  match !errors with
  | [] ->
      Some (Epoch_index_commitment.epoch_hash ~epoch_id !items), []
  | xs -> None, List.rev xs

let verify_epoch_index_commitment_raw t ~epoch_id ~start_txid ~tx_count ~prev_root =
  let stored_epoch_hash, stored_root = get_epoch_index_commitment t epoch_id in
  let actual_epoch_hash, compute_errors =
    compute_epoch_index_hash_raw t ~epoch_id ~start_txid ~tx_count
  in
  let actual_root =
    match actual_epoch_hash with
    | Some h -> Some (Epoch_index_commitment.root_hash ~prev:prev_root ~epoch_hash:h)
    | None -> None
  in
  let errors =
    compute_errors
    @ (match stored_epoch_hash, actual_epoch_hash with
       | Some a, Some b when a = b -> []
       | Some a, Some b ->
           [Printf.sprintf "epoch hash mismatch stored=%s actual=%s" a b]
       | None, _ -> ["stored epoch hash missing"]
       | _, None -> [])
    @ (match stored_root, actual_root with
       | Some a, Some b when a = b -> []
       | Some a, Some b ->
           [Printf.sprintf "epoch root mismatch stored=%s actual=%s" a b]
       | None, _ -> ["stored epoch root missing"]
       | _, None -> [])
  in
  {
    eic_epoch_id = epoch_id;
    eic_stored_hash = stored_epoch_hash;
    eic_stored_root = stored_root;
    eic_actual_hash = actual_epoch_hash;
    eic_actual_root = actual_root;
    eic_ok = errors = [];
    eic_errors = errors;
  }

let get_tx_by_hash t hash =
  match Chaindata_index.get_tx_loc t.index hash with
  | None -> None
  | Some (seg_id, offset, len, epoch_id) ->
    (try
       let (_eid, payload) = Txlog.read_record t.txlog ~seg_id ~offset ~len in
       let (_h, tx_json) = split_payload payload in
       Some (epoch_id, tx_json)
     with _ -> None)

let capped_replace cache key value =
  if Hashtbl.length cache >= 256 && not (Hashtbl.mem cache key) then
    Hashtbl.clear cache;
  Hashtbl.replace cache key value

let cached_epoch_status_ok t ~epoch_id ~start_txid ~tx_count =
  match Hashtbl.find_opt t.epoch_status_cache epoch_id with
  | Some status
    when status.expected_start_txid = start_txid
      && status.expected_tx_count = tx_count
      && epoch_index_status_ok status ->
      Some status
  | _ -> None

let cache_epoch_status_if_ok t status =
  if epoch_index_status_ok status then
    capped_replace t.epoch_status_cache status.epoch_id status

let addr_rows_page_cache_key t addr ~limit ~offset =
  let total = Chaindata_index.addr_tx_count t.index addr in
  Printf.sprintf "%s|%d|%d|%d" addr total limit offset

let token_rows_page_cache_key t addr ~limit ~offset =
  let total = Chaindata_index.addr_tx_count t.index addr in
  Printf.sprintf "%s|%d|%d|%d" addr total limit offset

let rejected_rows_page_cache_key addr ~total ~limit ~offset =
  Printf.sprintf "%s|%d|%d|%d" addr total limit offset

let epoch_rows_page_cache_key ~epoch_id ~start_txid ~tx_count ~limit ~offset =
  Printf.sprintf "%d|%Ld|%d|%d|%d"
    epoch_id start_txid tx_count limit offset

let read_tx_record_at_txid t txid =
  match Chaindata_index.get_txid_loc t.index txid with
  | None -> None
  | Some (seg_id, offset, len) ->
    if seg_id < 0 || offset < 0 || len <= 8 || len > max_txlog_record_len then None
    else
      (try
         let (epoch_id, payload) = Txlog.read_record t.txlog ~seg_id ~offset ~len in
         let (hash, tx_json) = split_payload payload in
         Some (hash, epoch_id, tx_json, seg_id, offset, len)
       with _ -> None)

let get_tx_by_txid t txid =
  match Chaindata_index.get_txid_loc t.index txid with
  | None -> None
  | Some (seg_id, offset, len) ->
    (try
       let (_eid, payload) = Txlog.read_record t.txlog ~seg_id ~offset ~len in
       let (h, tx_json) = split_payload payload in
       Some (h, tx_json)
     with _ -> None)

let tx_count t = Int64.to_int t.next_txid

let next_txid t = t.next_txid

type repair_stats = {
  checked : int;
  repaired : int;
  errors : string list;
}

let repair_stats_zero = { checked = 0; repaired = 0; errors = [] }

let parse_tx_identity tx_json =
  let j = Yojson.Safe.from_string tx_json in
  let open Yojson.Safe.Util in
  let from_addr = j |> member "from" |> to_string in
  let to_addr = j |> member "to_" |> to_string in
  let op_type = try j |> member "op_type" |> to_string with _ -> "" in
  let encrypted_data = try j |> member "encrypted_data" |> to_string with _ -> "" in
  let message = try j |> member "message" |> to_string with _ -> "" in
  let addrs =
    extract_call_recipient ~op_type ~encrypted_data ~message ~from_addr ~to_addr
  in
  (from_addr, to_addr, addrs)

let dedupe_addrs addrs =
  let seen = Hashtbl.create (List.length addrs) in
  let acc = ref [] in
  List.iter (fun addr ->
    if String.length addr > 0 && not (Hashtbl.mem seen addr) then begin
      Hashtbl.add seen addr ();
      acc := addr :: !acc
    end
  ) addrs;
  List.rev !acc

let buffer_repair_tx_full t ~hash ~seg_id ~offset ~len ~epoch_id ~txid ~tx_json =
  let (from_addr, to_addr, addrs) = parse_tx_identity tx_json in
  Chaindata_index.buffer_tx t.index ~hash ~seg_id ~offset ~len
    ~epoch_id ~txid ~from_addr ~to_addr ~addrs

let verify_epoch_index_complete_raw t ~epoch_id ~start_txid ~tx_count =
  match cached_epoch_status_ok t ~epoch_id ~start_txid ~tx_count with
  | Some status -> status
  | None ->
      let missing_epoch_meta = ref false in
      let checked = ref 0 in
      let missing_txid_loc = ref 0 in
      let missing_tx_loc = ref 0 in
      let missing_addr_refs = ref 0 in
      let malformed_records = ref 0 in
      let errors = ref [] in
      begin
        match Chaindata_index.get_epoch_raw t.index epoch_id with
        | None ->
            missing_epoch_meta := true;
            errors := "epoch_meta missing" :: !errors
        | Some epoch_json ->
            (match Epochlog.epoch_of_json epoch_json with
             | None ->
                 errors := "epoch_meta malformed" :: !errors
             | Some h ->
                 if h.Epochlog.start_txid <> start_txid then
                   errors := (Printf.sprintf
                     "epoch_meta start_txid mismatch expected=%Ld actual=%Ld"
                     start_txid h.Epochlog.start_txid) :: !errors;
                 if h.Epochlog.tx_count <> tx_count then
                   errors := (Printf.sprintf
                     "epoch_meta tx_count mismatch expected=%d actual=%d"
                     tx_count h.Epochlog.tx_count) :: !errors)
      end;
      for i = 0 to tx_count - 1 do
        incr checked;
        let txid = Int64.add start_txid (Int64.of_int i) in
        match Chaindata_index.get_txid_loc_raw t.index txid with
        | None -> incr missing_txid_loc
        | Some (seg_id, offset, len) ->
            (try
               let (stored_epoch_id, payload) =
                 Txlog.read_record t.txlog ~seg_id ~offset ~len
               in
               if stored_epoch_id <> epoch_id then
                 errors := (Printf.sprintf
                   "txid_loc points to wrong epoch txid=%Ld expected=%d actual=%d"
                   txid epoch_id stored_epoch_id) :: !errors;
               let (hash, tx_json) = split_payload payload in
               if String.length hash <> 64 then begin
                 incr malformed_records;
                 errors := (Printf.sprintf "payload hash malformed txid=%Ld" txid) :: !errors
               end else begin
                 (match Chaindata_index.get_tx_loc_raw t.index hash with
                  | Some (seg2, off2, len2, epoch2)
                    when seg2 = seg_id && off2 = offset && len2 = len && epoch2 = epoch_id -> ()
                  | Some (seg2, off2, len2, epoch2) ->
                      incr missing_tx_loc;
                      errors := (Printf.sprintf
                        "tx_loc mismatch hash=%s txid=%Ld expected=(%d,%d,%d,%d) actual=(%d,%d,%d,%d)"
                        (String.sub hash 0 (min 16 (String.length hash)))
                        txid seg_id offset len epoch_id seg2 off2 len2 epoch2) :: !errors
                  | None ->
                      incr missing_tx_loc);
                 let (from_addr, to_addr, extra_addrs) = parse_tx_identity tx_json in
                 let addrs = dedupe_addrs (from_addr :: to_addr :: extra_addrs) in
                 List.iter (fun addr ->
                   if not (Chaindata_index.addr_has_txid_raw t.index addr txid) then
                     incr missing_addr_refs
                 ) addrs
               end
             with e ->
               incr malformed_records;
               errors := (Printf.sprintf "txid=%Ld: %s" txid (Printexc.to_string e)) :: !errors)
      done;
      let status = {
        epoch_id;
        expected_start_txid = start_txid;
        expected_tx_count = tx_count;
        checked = !checked;
        missing_epoch_meta = !missing_epoch_meta;
        missing_txid_loc = !missing_txid_loc;
        missing_tx_loc = !missing_tx_loc;
        missing_addr_refs = !missing_addr_refs;
        malformed_records = !malformed_records;
        errors = List.rev !errors;
      } in
      cache_epoch_status_if_ok t status;
      status

let get_visible_epoch_index_status t epoch_id =
  match Chaindata_index.get_epoch t.index epoch_id with
  | None -> None
  | Some epoch_json ->
      (match Epochlog.epoch_of_json epoch_json with
       | None -> Some {
           epoch_id;
           expected_start_txid = 0L;
           expected_tx_count = 0;
           checked = 0;
           missing_epoch_meta = false;
           missing_txid_loc = 0;
           missing_tx_loc = 0;
           missing_addr_refs = 0;
           malformed_records = 1;
           errors = [ "visible epoch_meta malformed" ];
         }
       | Some h ->
           Some (verify_epoch_index_complete_raw t
             ~epoch_id
             ~start_txid:h.Epochlog.start_txid
             ~tx_count:h.Epochlog.tx_count))

let verify_and_repair_tx_loc_only t ~max_epoch =
  let checked = ref 0 in
  let repaired = ref 0 in
  let errors = ref [] in
  let progress = ref 0 in
  let pending = ref 0 in
  let batch_size = 50_000 in
  Chaindata_index.begin_write t.index;
  Txlog.scan_all t.txlog (fun seg_id pos record_len epoch_id payload ->
    incr progress;
    if !progress mod 1_000_000 = 0 then
      Octra_log.info "chaindata"
        "event = tx_location_repair scanned_million = %d repaired = %d"
        (!progress / 1_000_000) !repaired;
    if epoch_id > max_epoch then ()
    else begin
      let (hash, _tx_json) = split_payload payload in
      if String.length hash = 64 then begin
        match Chaindata_index.get_tx_loc t.index hash with
        | Some _ -> ()
        | None ->
          incr checked;
          (try
            Chaindata_index.buffer_tx_loc_only t.index ~hash ~seg_id ~offset:pos
              ~len:record_len ~epoch_id;
            incr repaired;
            incr pending;
            if !pending >= batch_size then begin
              Chaindata_index.commit_tx_loc_only t.index;
              Chaindata_index.begin_write t.index;
              pending := 0
            end
          with e ->
            errors := (Printf.sprintf "hash=%s: %s"
              (String.sub hash 0 (min 16 (String.length hash)))
              (Printexc.to_string e)) :: !errors)
      end
    end
  );
  Chaindata_index.commit_tx_loc_only t.index;
  { checked = !checked; repaired = !repaired; errors = List.rev !errors }

let verify_and_repair_tx_loc_recent_epochs t ~from_epoch ~to_epoch =
  if to_epoch < from_epoch then
    { checked = 0; repaired = 0; errors = [] }
  else begin
    let checked = ref 0 in
    let repaired = ref 0 in
    let errors = ref [] in
    let pending = ref 0 in
    let batch_size = 10_000 in
    Chaindata_index.begin_write t.index;
    (try
       for epoch_id = from_epoch to to_epoch do
         match Epochlog.get t.epochlog epoch_id with
         | None -> ()
         | Some h ->
           for i = 0 to h.Epochlog.tx_count - 1 do
             let txid = Int64.add h.Epochlog.start_txid (Int64.of_int i) in
             match read_tx_record_at_txid t txid with
             | Some (hash, record_epoch_id, _tx_json, seg_id, offset, len)
               when String.length hash = 64 ->
               incr checked;
               (match Chaindata_index.get_tx_loc t.index hash with
                | Some _ -> ()
                | None ->
                  (try
                     Chaindata_index.buffer_tx_loc_only t.index ~hash ~seg_id ~offset
                       ~len ~epoch_id:record_epoch_id;
                     incr repaired;
                     incr pending;
                     if !pending >= batch_size then begin
                       Chaindata_index.commit_tx_loc_only t.index;
                       Chaindata_index.begin_write t.index;
                       pending := 0
                     end
                   with e ->
                     errors := (Printf.sprintf "epoch=%d txid=%Ld hash=%s: %s"
                       epoch_id txid
                       (String.sub hash 0 (min 16 (String.length hash)))
                       (Printexc.to_string e)) :: !errors))
             | _ -> ()
           done
       done;
       Chaindata_index.commit_tx_loc_only t.index
     with e ->
       Chaindata_index.abort_write t.index;
       raise e);
    { checked = !checked; repaired = !repaired; errors = List.rev !errors }
  end

let read_tx_hash_at_txid t txid =
  match Chaindata_index.get_txid_loc t.index txid with
  | None -> None
  | Some (seg_id, offset, len) ->
    if seg_id < 0 || offset < 0 || len <= 8 || len > max_txlog_record_len then None
    else
      try
        let epoch_id, prefix =
          Txlog.read_record_prefix t.txlog ~seg_id ~offset ~len ~prefix_len:64
        in
        if String.length prefix <> 64 then None
        else Some (prefix, epoch_id)
      with _ -> None

let heal_tx_by_hash_recent t ~hash ~recent_epochs ~max_records =
  match Epochlog.last t.epochlog with
  | None -> None
  | Some tip ->
    let window = max 1 recent_epochs in
    let limit = max 0 max_records in
    let from_epoch = max 0 (tip.Epochlog.id - window + 1) in
    let found = ref None in
    let epoch_id = ref tip.Epochlog.id in
    let checked = ref 0 in
    while !epoch_id >= from_epoch && !found = None && !checked < limit do
      match Epochlog.get t.epochlog !epoch_id with
      | None -> decr epoch_id
      | Some h ->
        let i = ref (h.Epochlog.tx_count - 1) in
        while !i >= 0 && !found = None && !checked < limit do
          let txid = Int64.add h.Epochlog.start_txid (Int64.of_int !i) in
          incr checked;
          begin
            match read_tx_hash_at_txid t txid with
            | Some (tx_hash, record_epoch_id) when tx_hash = hash ->
              begin
                match read_tx_record_at_txid t txid with
                | Some (_, _, tx_json, seg_id, offset, len) ->
                  (try
                     Chaindata_index.set_tx_loc_only t.index hash ~seg_id ~offset ~len
                       ~epoch_id:record_epoch_id
                   with _ -> ());
                  found := Some (record_epoch_id, tx_json)
                | None -> ()
              end
            | _ -> ()
          end;
          decr i
        done;
        decr epoch_id
    done;
    !found

let verify_and_repair_txid_loc_recent_epochs t ~from_epoch ~to_epoch =
  if to_epoch < from_epoch then
    repair_stats_zero
  else begin
    let epoch_plan = Hashtbl.create 32 in
    for epoch_id = from_epoch to to_epoch do
      match Epochlog.get t.epochlog epoch_id with
      | Some h when h.Epochlog.tx_count > 0 ->
          Hashtbl.replace epoch_plan epoch_id
            (h.Epochlog.start_txid, h.Epochlog.tx_count, ref 0)
      | _ -> ()
    done;
    if Hashtbl.length epoch_plan = 0 then
      repair_stats_zero
    else begin
      let checked = ref 0 in
      let repaired = ref 0 in
      let errors = ref [] in
      let pending = ref 0 in
      let batch_size = 10_000 in
      Chaindata_index.begin_write t.index;
      let flush_pending () =
        if !pending > 0 then begin
          Chaindata_index.commit_write t.index;
          Chaindata_index.begin_write t.index;
          pending := 0
        end
      in
      (try
         Txlog.scan_all t.txlog (fun seg_id offset len epoch_id payload ->
           match Hashtbl.find_opt epoch_plan epoch_id with
           | Some (start_txid, tx_count, seen_ref) when !seen_ref < tx_count ->
               let slot = !seen_ref in
               incr seen_ref;
               let txid = Int64.add start_txid (Int64.of_int slot) in
               incr checked;
               if Int64.compare txid 0L > 0
                  && not (Chaindata_index.txid_loc_present t.index txid) then begin
                 let (hash, tx_json) = split_payload payload in
                 if String.length hash = 64 then
                   (try
                      buffer_repair_tx_full t ~hash ~seg_id ~offset ~len
                        ~epoch_id ~txid ~tx_json;
                      incr repaired;
                      incr pending;
                      if !pending >= batch_size then flush_pending ()
                    with e ->
                      errors := (Printf.sprintf "epoch=%d txid=%Ld hash=%s: %s"
                        epoch_id txid
                        (String.sub hash 0 (min 16 (String.length hash)))
                        (Printexc.to_string e)) :: !errors)
               end
           | _ -> ()
         );
         if !pending > 0 then Chaindata_index.commit_write t.index
         else Chaindata_index.abort_write t.index
       with e ->
         Chaindata_index.abort_write t.index;
         raise e);
      { checked = !checked; repaired = !repaired; errors = List.rev !errors }
    end
  end

let heal_epoch_txids_recent t ~epoch_id ~recent_epochs =
  match Epochlog.last t.epochlog with
  | None -> repair_stats_zero
  | Some tip ->
      let window = max 1 recent_epochs in
      let from_epoch = max 0 (tip.Epochlog.id - window + 1) in
      if epoch_id < from_epoch || epoch_id > tip.Epochlog.id then
        repair_stats_zero
      else
        verify_and_repair_txid_loc_recent_epochs t ~from_epoch:epoch_id ~to_epoch:epoch_id

let verify_and_repair t ~last_irmin_epoch =
  let _ = last_irmin_epoch in
  let checked = ref 0 in
  let repaired = ref 0 in
  let errors = ref [] in
  let progress = ref 0 in
  Txlog.scan_all t.txlog (fun seg_id pos record_len epoch_id payload ->
    incr progress;
    if !progress mod 1_000_000 = 0 then
      Octra_log.info "chaindata"
        "event = verify_progress scanned_million = %d repaired = %d"
        (!progress / 1_000_000) !repaired;
    let (hash, tx_json) = split_payload payload in
    if String.length hash = 64 then begin
      match Chaindata_index.get_tx_loc t.index hash with
      | Some _ -> ()
      | None ->
        incr checked;
        let txid = t.next_txid in
        t.next_txid <- Int64.add t.next_txid 1L;
        (try
          let j = Yojson.Safe.from_string tx_json in
          let open Yojson.Safe.Util in
          let from_addr = j |> member "from" |> to_string in
          let to_addr = j |> member "to_" |> to_string in
          let op_type = try j |> member "op_type" |> to_string with _ -> "" in
          let encrypted_data = try j |> member "encrypted_data" |> to_string with _ -> "" in
          let message = try j |> member "message" |> to_string with _ -> "" in
          let addrs = extract_call_recipient ~op_type ~encrypted_data
            ~message ~from_addr ~to_addr in
          Chaindata_index.begin_write t.index;
          Chaindata_index.buffer_tx t.index ~hash ~seg_id ~offset:pos
            ~len:record_len ~epoch_id ~txid ~from_addr ~to_addr ~addrs;
          Chaindata_index.commit_write t.index;
          incr repaired
        with e ->
          errors := (Printf.sprintf "hash=%s: %s"
            (String.sub hash 0 (min 16 (String.length hash)))
            (Printexc.to_string e)) :: !errors)
    end
  );
  if !repaired > 0 then begin
    Chaindata_index.begin_write t.index;
    Chaindata_index.buffer_meta t.index "next_txid" (Int64.to_string t.next_txid);
    Chaindata_index.commit_write t.index
  end;
  { checked = !checked; repaired = !repaired; errors = List.rev !errors }

let tx_json_to_summary_row hash epoch_id tx_json =
  try
    let j = Yojson.Safe.from_string tx_json in
    let open Yojson.Safe.Util in
    let from_f = try j |> member "from" |> to_string with _ -> "" in
    let to_f = try j |> member "to_" |> to_string with _ -> "" in
    let amount = try j |> member "amount" |> to_string with _ -> "0" in
    let ts = try j |> member "timestamp" with _ -> `Float 0.0 in
    let op = try j |> member "op_type" |> to_string with _ -> "standard" in
    let ed = try j |> member "encrypted_data" |> to_string with _ -> "" in
    let msg = try j |> member "message" |> to_string with _ -> "" in
    let ou = try j |> member "ou" |> to_string with _ -> "0" in
    let base = [
      "hash", `String hash;
      "epoch", `Int epoch_id;
      "from", `String from_f;
      "to", `String to_f;
      "amount", `String amount;
      "ou", `String ou;
      "timestamp", ts;
      "op_type", `String op;
    ] in
    let base =
      if ed <> "" then
        base @ [
          "has_encrypted_data", `Bool true;
          "encrypted_data_len", `Int (String.length ed);
        ]
      else base
    in
    let base =
      if msg <> "" then
        base @ [
          "has_message", `Bool true;
          "message_len", `Int (String.length msg);
        ]
      else base
    in
    Some (`Assoc base)
  with _ -> None

let summary_row_mentions_addr addr = function
  | `Assoc fields ->
      (match List.assoc_opt "from" fields with Some (`String s) -> s = addr | _ -> false)
      || (match List.assoc_opt "to" fields with Some (`String s) -> s = addr | _ -> false)
  | _ -> false

let read_tx_at_txid t txid =
  match read_tx_record_at_txid t txid with
  | Some (hash, epoch_id, tx_json, _seg_id, _offset, _len) ->
    Some (hash, epoch_id, tx_json)
  | None -> None

let read_tx_summary_at_txid t txid =
  match Chaindata_index.get_txid_loc t.index txid with
  | None -> None
  | Some (seg_id, offset, len) ->
    if seg_id < 0 || offset < 0 || len <= 8 || len > max_txlog_record_len then None
    else
      try
        let prefix_len = min (len - 8) (64 + 16_384) in
        let (epoch_id, prefix) =
          Txlog.read_record_prefix t.txlog ~seg_id ~offset ~len ~prefix_len
        in
        if String.length prefix < 64 then None
        else
          let hash = String.sub prefix 0 64 in
          tx_prefix_to_summary_row hash epoch_id prefix
      with _ -> None

type token_transfer_match =
  | Token_transfer_absent
  | Token_transfer_present of bool * bool * Yojson.Safe.t
  | Token_transfer_malformed

let token_transfer_row_for_addr addr hash epoch_id tx_json =
  try
    let json = Yojson.Safe.from_string tx_json in
    let open Yojson.Safe.Util in
    let op_type = try json |> member "op_type" |> to_string with _ -> "" in
    let encrypted_data = try json |> member "encrypted_data" |> to_string with _ -> "" in
    if op_type <> "call" || encrypted_data <> "transfer" then Token_transfer_absent
    else
      let from_addr = try json |> member "from" |> to_string with _ -> "" in
      let recipient_matches =
        try
          match json |> member "message" with
          | `String msg ->
              (match Yojson.Safe.from_string msg with
               | `List (`String recipient :: _) -> recipient = addr
               | _ -> false)
          | _ -> false
        with _ -> false
      in
      match tx_json_to_summary_row hash epoch_id tx_json with
      | Some row ->
          let incoming = recipient_matches in
          let outgoing = from_addr = addr in
          Token_transfer_present (incoming, outgoing, row)
      | None -> Token_transfer_malformed
  with _ -> Token_transfer_malformed

let recent_txs_rows_status t ~limit ~offset =
  let total = Int64.to_int t.next_txid in
  let results = ref [] in
  let i = ref 0 in
  let collected = ref 0 in
  let missing = ref 0 in
  let scan_limit = max 512 (limit * 32) in
  while !collected < limit && !i < scan_limit && (total - 1 - offset - !i) >= 0 do
    let txid = Int64.of_int (total - 1 - offset - !i) in
    (match read_tx_summary_at_txid t txid with
     | Some row -> results := row :: !results; incr collected
     | None -> incr missing);
    incr i
  done;
  {
    total;
    rows = List.rev !results;
    missing = !missing;
    incomplete = !missing > 0 || (!collected < limit && (total - 1 - offset - !i) >= 0);
  }

let recent_txs_rows t ~limit ~offset =
  let status = recent_txs_rows_status t ~limit ~offset in
  (status.total, status.rows)

let recent_txs_by_addr_rows_status t addr ~limit =
  let total = Chaindata_index.addr_tx_count t.index addr in
  let scan_limit = min total (max 64 (limit * 4)) in
  let (_sample_total, sample_txids) =
    Chaindata_index.addr_txids_rev t.index addr ~limit:scan_limit ~offset:0
  in
  let missing = ref 0 in
  let resolved = List.filter_map (fun txid ->
    match read_tx_at_txid t txid with
    | Some (hash, epoch_id, tx_json) ->
        (match tx_json_to_summary_row hash epoch_id tx_json with
         | Some row -> Some (epoch_id, txid, row)
         | None -> incr missing; None)
    | None -> incr missing; None
  ) sample_txids in
  let sorted = List.sort (fun (e1, t1, _) (e2, t2, _) ->
    if e1 <> e2 then compare e2 e1 else compare t2 t1
  ) resolved in
  let rec take n = function
    | _ when n <= 0 -> []
    | [] -> []
    | h :: tl -> h :: take (n - 1) tl
  in
  {
    total;
    rows = List.map (fun (_, _, row) -> row) (take limit sorted);
    missing = !missing;
    incomplete = !missing > 0;
  }

let materialized_txs_by_addr_rows_status_profile t addr ~limit ~offset =
  let t0 = Unix.gettimeofday () in
  let total = Chaindata_index.addr_tx_count t.index addr in
  let remaining = max 0 (total - offset) in
  let rows_needed = min limit remaining in
  let page_stop = offset + rows_needed in
  match Chaindata_index.get_addr_recent t.index addr with
  | None -> None
  | Some _ when rows_needed = 0 ->
      let t1 = Unix.gettimeofday () in
      Some ({
        total;
        rows = [];
        missing = 0;
        incomplete = false;
      }, {
        fetched = 0;
        selected = 0;
        rows = 0;
        fetch_ms = (t1 -. t0) *. 1000.0;
        resolve_ms = 0.0;
        sort_ms = 0.0;
        page_ms = 0.0;
        total_ms = (t1 -. t0) *. 1000.0;
      })
  | Some refs ->
      let t1 = Unix.gettimeofday () in
      if List.length refs < page_stop then None
      else
        let missing = ref 0 in
        let rec drop n = function
          | l when n <= 0 -> l
          | [] -> []
          | _ :: tl -> drop (n - 1) tl
        in
        let rec take n xs =
          match n, xs with
          | n, _ when n <= 0 -> []
          | _, [] -> []
          | n, x :: tl -> x :: take (n - 1) tl
        in
        let selected_refs = take rows_needed (drop offset refs) in
        let rows = List.filter_map (fun (expected_epoch_id, txid) ->
          match read_tx_at_txid t txid with
          | Some (hash, actual_epoch_id, tx_json) when actual_epoch_id = expected_epoch_id ->
              (match tx_json_to_summary_row hash actual_epoch_id tx_json with
               | Some row -> Some row
               | None -> incr missing; None)
          | Some _ ->
              incr missing;
              None
          | None ->
              incr missing;
              None
        ) selected_refs in
        let t2 = Unix.gettimeofday () in
        if !missing > 0 || List.length rows <> rows_needed then None
        else
          Some ({
            total;
            rows;
            missing = 0;
            incomplete = false;
          }, {
            fetched = List.length refs;
            selected = List.length selected_refs;
            rows = List.length rows;
            fetch_ms = (t1 -. t0) *. 1000.0;
            resolve_ms = (t2 -. t1) *. 1000.0;
            sort_ms = 0.0;
            page_ms = 0.0;
            total_ms = (t2 -. t0) *. 1000.0;
          })

let compute_txs_by_addr_rows_status_profile t addr ~limit ~offset =
  let t0 = Unix.gettimeofday () in
  let (_raw_total, all_txids) = Chaindata_index.addr_txids_rev t.index addr ~limit:max_int ~offset:0 in
  let t1 = Unix.gettimeofday () in
  let missing = ref 0 in
  let resolved = List.filter_map (fun txid ->
    match read_tx_at_txid t txid with
    | Some (hash, epoch_id, tx_json) ->
        (match tx_json_to_summary_row hash epoch_id tx_json with
         | Some row when summary_row_mentions_addr addr row -> Some (epoch_id, txid, row)
         | Some _ -> None
         | None -> incr missing; None)
    | None -> incr missing; None
  ) all_txids in
  let total = List.length resolved in
  let t2 = Unix.gettimeofday () in
  let sorted = List.sort (fun (e1, t1, _) (e2, t2, _) ->
    if e1 <> e2 then compare e2 e1 else compare t2 t1
  ) resolved in
  let t3 = Unix.gettimeofday () in
  let rec drop n = function
    | l when n <= 0 -> l
    | [] -> []
    | _ :: tl -> drop (n - 1) tl
  in
  let rec take n = function
    | _ when n <= 0 -> []
    | [] -> []
    | h :: tl -> h :: take (n - 1) tl
  in
  let page = take limit (drop offset sorted) in
  let t4 = Unix.gettimeofday () in
  ({
    total;
    rows = List.map (fun (_, _, row) -> row) page;
    missing = !missing;
    incomplete = !missing > 0;
  }, {
    fetched = List.length all_txids;
    selected = List.length resolved;
    rows = List.length page;
    fetch_ms = (t1 -. t0) *. 1000.0;
    resolve_ms = (t2 -. t1) *. 1000.0;
    sort_ms = (t3 -. t2) *. 1000.0;
    page_ms = (t4 -. t3) *. 1000.0;
    total_ms = (t4 -. t0) *. 1000.0;
  })

let compute_txs_by_addr_rows_status t addr ~limit ~offset =
  fst (compute_txs_by_addr_rows_status_profile t addr ~limit ~offset)

let bounded_txs_by_addr_rows_status_profile t addr ~limit ~offset =
  let t0 = Unix.gettimeofday () in
  let indexed_total = Chaindata_index.addr_tx_count t.index addr in
  let remaining = max 0 (indexed_total - offset) in
  let scan_limit = min remaining (max 64 (limit * 4)) in
  let (_raw_total, txids) =
    Chaindata_index.addr_txids_rev t.index addr ~limit:scan_limit ~offset
  in
  let t1 = Unix.gettimeofday () in
  let missing = ref 0 in
  let rows =
    List.filter_map
      (fun txid ->
        match read_tx_at_txid t txid with
        | Some (hash, epoch_id, tx_json) ->
          begin
            match tx_json_to_summary_row hash epoch_id tx_json with
            | Some row when summary_row_mentions_addr addr row -> Some row
            | Some _ -> None
            | None ->
              incr missing;
              None
          end
        | None ->
          incr missing;
          None)
      txids
  in
  let scanned_all = offset = 0 && scan_limit = indexed_total in
  let total = if scanned_all then List.length rows else indexed_total in
  let rows_needed = min limit (max 0 (total - offset)) in
  let rec take n = function
    | _ when n <= 0 -> []
    | [] -> []
    | row :: rest -> row :: take (n - 1) rest
  in
  let page = take rows_needed rows in
  let t2 = Unix.gettimeofday () in
  ({
    total;
    rows = page;
    missing = !missing;
    incomplete = !missing > 0 || List.length page <> rows_needed;
  }, {
    fetched = List.length txids;
    selected = List.length rows;
    rows = List.length page;
    fetch_ms = (t1 -. t0) *. 1000.0;
    resolve_ms = (t2 -. t1) *. 1000.0;
    sort_ms = 0.0;
    page_ms = 0.0;
    total_ms = (t2 -. t0) *. 1000.0;
  })

let txs_by_addr_rows_status ?(profile_tag = "") t addr ~limit ~offset =
  let use_cache = limit <= 100 in
  if not use_cache then
    let (status, profile) =
      bounded_txs_by_addr_rows_status_profile t addr ~limit ~offset
    in
    emit_history_profile
      ~tag:profile_tag
      ~cache:"bypass"
      ~addr
      ~limit
      ~offset
      ~total:status.total
      ~missing:status.missing
      profile;
    status
  else
    let key = addr_rows_page_cache_key t addr ~limit ~offset in
    match Hashtbl.find_opt t.addr_rows_page_cache key with
    | Some status ->
        emit_history_profile
          ~tag:profile_tag
          ~cache:"hit"
          ~addr
          ~limit
          ~offset
          ~total:status.total
          ~missing:status.missing
          {
            fetched = 0;
            selected = 0;
            rows = List.length status.rows;
            fetch_ms = 0.0;
            resolve_ms = 0.0;
            sort_ms = 0.0;
            page_ms = 0.0;
            total_ms = 0.0;
          };
        status
    | None ->
        let (status, profile) =
          bounded_txs_by_addr_rows_status_profile t addr ~limit ~offset
        in
        if not status.incomplete then
          capped_replace t.addr_rows_page_cache key status;
        emit_history_profile
          ~tag:profile_tag
          ~cache:"miss"
          ~addr
          ~limit
          ~offset
          ~total:status.total
          ~missing:status.missing
          profile;
        status

let txs_by_addr_rows t addr ~limit ~offset =
  let status = txs_by_addr_rows_status t addr ~limit ~offset in
  (status.total, status.rows)

let token_history_scan_limit = 4096

let compute_token_txs_by_addr_page_profile t addr ~limit ~offset =
  let t0 = Unix.gettimeofday () in
  let addr_total = Chaindata_index.addr_tx_count t.index addr in
  let scan_limit = min addr_total token_history_scan_limit in
  let (_total, txids) =
    Chaindata_index.addr_txids_rev t.index addr ~limit:scan_limit ~offset:0
  in
  let t1 = Unix.gettimeofday () in
  let incoming = ref 0 in
  let outgoing = ref 0 in
  let missing = ref 0 in
  let token_rows = List.filter_map (fun txid ->
    match read_tx_at_txid t txid with
    | Some (hash, epoch_id, tx_json) ->
        (match token_transfer_row_for_addr addr hash epoch_id tx_json with
         | Token_transfer_present (is_incoming, is_outgoing, row) ->
             if is_incoming then incr incoming;
             if is_outgoing then incr outgoing;
             Some (epoch_id, txid, row)
         | Token_transfer_absent -> None
         | Token_transfer_malformed -> incr missing; None)
    | None -> incr missing; None
  ) txids in
  let t2 = Unix.gettimeofday () in
  let rec drop n = function
    | l when n <= 0 -> l
    | [] -> []
    | _ :: tl -> drop (n - 1) tl
  in
  let rec take n = function
    | _ when n <= 0 -> []
    | [] -> []
    | h :: tl -> h :: take (n - 1) tl
  in
  let total = List.length token_rows in
  let page_rows = take limit (drop offset token_rows) in
  let t3 = Unix.gettimeofday () in
  ({
    total;
    rows = List.map (fun (_, _, row) -> row) page_rows;
    incoming = !incoming;
    outgoing = !outgoing;
    has_more = offset + List.length page_rows < total;
    missing = !missing;
    incomplete = !missing > 0 || scan_limit < addr_total;
  }, {
    fetched = List.length txids;
    selected = List.length token_rows;
    rows = List.length page_rows;
    fetch_ms = (t1 -. t0) *. 1000.0;
    resolve_ms = (t2 -. t1) *. 1000.0;
    sort_ms = 0.0;
    page_ms = (t3 -. t2) *. 1000.0;
    total_ms = (t3 -. t0) *. 1000.0;
  })

let token_txs_by_addr_page ?(profile_tag = "") t addr ~limit ~offset =
  let use_cache = offset = 0 && limit <= 100 in
  if use_cache then
    let key = token_rows_page_cache_key t addr ~limit ~offset in
    match Hashtbl.find_opt t.token_rows_page_cache key with
    | Some page ->
        emit_history_profile
          ~tag:profile_tag
          ~cache:"hit"
          ~addr
          ~limit
          ~offset
          ~total:page.total
          ~missing:page.missing
          {
            fetched = 0;
            selected = 0;
            rows = List.length page.rows;
            fetch_ms = 0.0;
            resolve_ms = 0.0;
            sort_ms = 0.0;
            page_ms = 0.0;
            total_ms = 0.0;
          };
        page
    | None ->
        let (page, profile) =
          compute_token_txs_by_addr_page_profile t addr ~limit ~offset
        in
        if not page.incomplete then
          capped_replace t.token_rows_page_cache key page;
        emit_history_profile
          ~tag:profile_tag
          ~cache:"miss"
          ~addr
          ~limit
          ~offset
          ~total:page.total
          ~missing:page.missing
          profile;
        page
  else
    let (page, profile) =
      compute_token_txs_by_addr_page_profile t addr ~limit ~offset
    in
    emit_history_profile
      ~tag:profile_tag
      ~cache:"bypass"
      ~addr
      ~limit
      ~offset
      ~total:page.total
      ~missing:page.missing
      profile;
    page

let txs_by_epoch_rows_status t epoch_id ~limit ~offset =
  match Chaindata_index.get_epoch t.index epoch_id with
  | None -> { total = 0; rows = []; missing = 0; incomplete = false }
  | Some epoch_json ->
    (match Epochlog.epoch_of_json epoch_json with
     | None -> { total = 0; rows = []; missing = 1; incomplete = true }
     | Some h ->
       let total = h.Epochlog.tx_count in
       let key =
         epoch_rows_page_cache_key
           ~epoch_id
           ~start_txid:h.Epochlog.start_txid
           ~tx_count:total
           ~limit
           ~offset
       in
       match Hashtbl.find_opt t.epoch_rows_page_cache key with
       | Some status -> status
       | None ->
           let results = ref [] in
           let missing_txid_loc = ref 0 in
           let missing_tx_loc = ref 0 in
           let missing_addr_refs = ref 0 in
           let malformed_records = ref 0 in
           let stop = min (offset + limit) total in
           for i = offset to stop - 1 do
             let txid = Int64.add h.Epochlog.start_txid (Int64.of_int i) in
             match Chaindata_index.get_txid_loc_raw t.index txid with
             | None -> incr missing_txid_loc
             | Some (seg_id, offset, len) ->
                 (try
                    let (stored_epoch_id, payload) =
                      Txlog.read_record t.txlog ~seg_id ~offset ~len
                    in
                    if stored_epoch_id <> epoch_id then
                      incr missing_tx_loc
                    else
                      let (hash, tx_json) = split_payload payload in
                      if String.length hash <> 64 then
                        incr malformed_records
                      else begin
                        (match Chaindata_index.get_tx_loc_raw t.index hash with
                         | Some (seg2, off2, len2, epoch2)
                           when seg2 = seg_id && off2 = offset && len2 = len && epoch2 = epoch_id -> ()
                         | _ ->
                             incr missing_tx_loc);
                        let (from_addr, to_addr, extra_addrs) = parse_tx_identity tx_json in
                        let addrs = dedupe_addrs (from_addr :: to_addr :: extra_addrs) in
                        List.iter (fun addr ->
                          if not (Chaindata_index.addr_has_txid_raw t.index addr txid) then
                            incr missing_addr_refs
                        ) addrs;
                        (match tx_json_to_summary_row hash epoch_id tx_json with
                         | Some row -> results := row :: !results
                         | None -> incr malformed_records)
                      end
                  with _ ->
                    incr malformed_records)
           done;
           let missing =
             !missing_txid_loc + !missing_tx_loc + !missing_addr_refs + !malformed_records
           in
           let status = {
             total;
             rows = List.rev !results;
             missing;
             incomplete = missing > 0;
           } in
           if not status.incomplete then
             capped_replace t.epoch_rows_page_cache key status;
           status)

let txs_by_epoch_rows t epoch_id ~limit ~offset =
  let status = txs_by_epoch_rows_status t epoch_id ~limit ~offset in
  (status.total, status.rows)

let txs_by_epoch_full t epoch_id =
  match Chaindata_index.get_epoch t.index epoch_id with
  | None -> []
  | Some epoch_json ->
    (match Epochlog.epoch_of_json epoch_json with
     | None -> []
     | Some h ->
       let results = ref [] in
       for i = 0 to h.Epochlog.tx_count - 1 do
         let txid = Int64.add h.Epochlog.start_txid (Int64.of_int i) in
         (match read_tx_at_txid t txid with
          | Some (hash, _epoch_id, tx_json) ->
            results := (hash, tx_json) :: !results
          | None -> ())
       done;
       List.rev !results)

let txs_by_address t addr ~limit ~offset =
  let limit = max 0 limit in
  let offset = max 0 offset in
  let page_txids =
    if limit = 0 then
      []
    else
      snd (Chaindata_index.addr_txids_rev t.index addr ~limit ~offset)
  in
  let resolved = List.filter_map (fun txid ->
    match read_tx_at_txid t txid with
    | Some (hash, epoch_id, tx_json) -> Some (epoch_id, txid, hash, tx_json)
    | None -> None
  ) page_txids in
  let page = List.sort (fun (e1, t1, _, _) (e2, t2, _, _) ->
    if e1 <> e2 then compare e2 e1 else compare t2 t1
  ) resolved in
  List.filter_map (fun (epoch_id, _txid, hash, tx_json) ->
    (try
       let j = Yojson.Safe.from_string tx_json in
       Some (`Assoc [
         "hash", `String hash;
         "epoch_id", `Int epoch_id;
         "tx_json", j;
       ])
     with _ -> None)
  ) page

type pvac_legacy_public_replay_status = {
  total : int;
  scanned : int;
  complete : bool;
  decision : Pvac_legacy_public_replay.decision;
}

let pvac_legacy_public_replay_blocked reason =
  {
    Pvac_legacy_public_replay.audit_class = Poisoned;
    Pvac_legacy_public_replay.can_public_migrate = false;
    public_net = None;
    commitment_net = None;
    blockers = [reason];
    effects = [];
    reason;
  }

let tx_json_of_replay_row = function
  | `Assoc fields ->
    (match List.assoc_opt "tx_json" fields with
    | Some json -> Some json
    | None -> None)
  | _ -> None

let pvac_legacy_public_replay_by_addr t addr ~max_txs =
  let total = Chaindata_index.addr_tx_count t.index addr in
  if total > max_txs then
    {
      total;
      scanned = 0;
      complete = false;
      decision = pvac_legacy_public_replay_blocked "legacy public replay exceeds history cap";
    }
  else
    let rows = txs_by_address t addr ~limit:max_txs ~offset:0 in
    let txs = List.filter_map tx_json_of_replay_row rows in
    let complete = List.length txs = total in
    let decision =
      if complete then
        Pvac_legacy_public_replay.replay_history ~addr txs
      else
        pvac_legacy_public_replay_blocked "legacy public replay history index is incomplete"
    in
    {
      total;
      scanned = List.length txs;
      complete;
      decision;
    }

let get_epoch_summary t epoch_id =
  match Chaindata_index.get_epoch t.index epoch_id with
  | None -> None
  | Some s ->
    (try
       let j = Yojson.Safe.from_string s in
       let open Yojson.Safe.Util in
       let sr = j |> member "state_root" in
       Some (`Assoc [
         "epoch_id", `Int epoch_id;
         "tx_count", j |> member "tx_count";
         "finalized_by", j |> member "finalized_by";
         "finalized_at", j |> member "finalized_at";
         "parent_commit", j |> member "parent_commit";
         "state_root", sr;
         "tree_hash", sr;
         "fees_total", j |> member "fees_total";
         "fees_burned", j |> member "fees_burned";
         "base_reward", j |> member "base_reward";
         "total_reward", j |> member "total_reward";
         "proposer_reward", j |> member "proposer_reward";
         "validator_reward_each", j |> member "validator_reward_each";
         "reward_recipients", j |> member "reward_recipients";
       ])
     with _ -> None)

let get_epoch_header t epoch_id =
  match Chaindata_index.get_epoch t.index epoch_id with
  | None -> None
  | Some s -> Epochlog.epoch_of_json s

let get_bound_epoch_header t epoch_id =
  match get_epoch_header t epoch_id, Epochlog.get t.epochlog epoch_id with
  | None, _ -> Error "epoch not found"
  | Some _, None -> Error "epoch log entry missing"
  | Some indexed, Some durable when indexed = durable -> Ok durable
  | Some _, Some _ -> Error "epoch metadata does not match epoch log"

let list_epoch_ids t =
  Chaindata_index.list_epoch_ids t.index

let get_epoch_tx_count t epoch_id =
  match Chaindata_index.get_epoch t.index epoch_id with
  | None -> 0
  | Some s ->
    (try
       let j = Yojson.Safe.from_string s in
       Yojson.Safe.Util.(j |> member "tx_count" |> to_int)
     with _ -> 0)

let recent_tx_count t ~n_epochs ~current_epoch =
  let total = ref 0 in
  for i = 0 to n_epochs - 1 do
    let eid = current_epoch - 1 - i in
    if eid >= 0 then
      total := !total + get_epoch_tx_count t eid
  done;
  !total

let get_rejected_tx t hash =
  match Chaindata_index.get_rejected t.index hash with
  | None -> None
  | Some s ->
    (try
       let j = Yojson.Safe.from_string s in
       let open Yojson.Safe.Util in
       Some (
         j |> member "from_addr" |> to_string,
         j |> member "to_addr" |> to_string,
         j |> member "amount" |> to_string,
         j |> member "nonce" |> to_int,
         j |> member "error_type" |> to_string,
         j |> member "reason" |> to_string,
         j |> member "epoch_id" |> to_int,
         j |> member "ts" |> to_number
       )
     with _ -> None)

let rejected_to_row hash rj_json =
  try
    let j = Yojson.Safe.from_string rj_json in
    let open Yojson.Safe.Util in
    Some (`Assoc [
      "hash", `String hash;
      "epoch", `Int (j |> member "epoch_id" |> to_int);
      "from", `String (j |> member "from_addr" |> to_string);
      "to", `String (j |> member "to_addr" |> to_string);
      "amount", `String (j |> member "amount" |> to_string);
      "nonce", `Int (j |> member "nonce" |> to_int);
      "error_type", `String (j |> member "error_type" |> to_string);
      "reason", `String (j |> member "reason" |> to_string);
      "timestamp", j |> member "ts";
      "type", `String "rejected";
    ])
  with _ -> None

let get_rejected_txs_by_addr t addr ~limit =
  let hashes = Chaindata_index.rejected_by_addr t.index addr ~limit ~offset:0 in
  List.filter_map (fun hash ->
    match Chaindata_index.get_rejected t.index hash with
    | Some s ->
      (try
         let j = Yojson.Safe.from_string s in
         let open Yojson.Safe.Util in
         Some (hash,
               j |> member "epoch_id" |> to_int,
               j |> member "ts" |> to_number)
       with _ -> None)
    | None -> None
  ) hashes

let compute_rejected_by_addr_rows t addr ~limit ~offset =
  let hashes = Chaindata_index.rejected_by_addr_rev t.index addr ~limit ~offset in
  List.filter_map (fun hash ->
    match Chaindata_index.get_rejected t.index hash with
    | Some s -> rejected_to_row hash s
    | None -> None
  ) hashes

let rejected_by_addr_rows t addr ~limit ~offset =
  let total = Chaindata_index.rejected_count_by_addr t.index addr in
  if limit > 100 then
    compute_rejected_by_addr_rows t addr ~limit ~offset
  else
    let key = rejected_rows_page_cache_key addr ~total ~limit ~offset in
    match Hashtbl.find_opt t.rejected_rows_page_cache key with
    | Some rows -> rows
    | None ->
        let rows = compute_rejected_by_addr_rows t addr ~limit ~offset in
        capped_replace t.rejected_rows_page_cache key rows;
        rows

let rejected_by_epoch_rows t epoch_id ~limit ~offset =
  let hashes =
    Chaindata_index.rejected_by_epoch t.index epoch_id ~limit ~offset
  in
  List.filter_map (fun hash ->
    match Chaindata_index.get_rejected t.index hash with
    | Some s -> rejected_to_row hash s
    | None -> None
  ) hashes

let rejected_count_by_addr t addr =
  Chaindata_index.rejected_count_by_addr t.index addr

let get_contract_receipt t ~tx_hash =
  match Chaindata_index.get_receipt t.index tx_hash with
  | Some s -> (try Some (Yojson.Safe.from_string s) with _ -> None)
  | None -> None

let get_contract_receipt_raw t ~tx_hash =
  Chaindata_index.get_receipt t.index tx_hash

let get_tx_fields t hash =
  match get_tx_by_hash t hash with
  | None -> None
  | Some (_epoch_id, tx_json) ->
    (try
       let j = Yojson.Safe.from_string tx_json in
       let open Yojson.Safe.Util in
       Some (
         j |> member "from" |> to_string,
         j |> member "to_" |> to_string,
         j |> member "amount" |> to_string,
         (try j |> member "nonce" |> to_int with _ -> 0),
         j |> member "ou" |> to_string,
         j |> member "timestamp" |> to_number,
         (try Some (j |> member "message" |> to_string) with _ -> None)
       )
     with _ -> None)

let addr_tx_count t addr =
  Chaindata_index.addr_tx_count t.index addr

let get_meta t key =
  Chaindata_index.get_meta t.index key

let set_meta t key value =
  Chaindata_index.set_meta_direct t.index key value

let list_epochs_with_txs t =
  List.filter (fun eid -> get_epoch_tx_count t eid > 0)
    (Chaindata_index.list_epoch_ids t.index)
  |> List.sort (fun a b -> compare b a)

let get_last_epoch t =
  Epochlog.last t.epochlog

let read_all_epochs t = Epochlog.read_all t.epochlog

let index t = t.index

let txlog t = t.txlog

let txlog_position t = Txlog.current_position t.txlog

let epochlog_offset t = Epochlog.current_offset t.epochlog

let epochlog_offset_after t epoch_id =
  Epochlog.offset_after t.epochlog epoch_id

let epochs_empty_after t ~from_epoch ~to_epoch =
  let rec loop epoch_id =
    if epoch_id > to_epoch then true
    else
      match Epochlog.get t.epochlog epoch_id with
      | Some h when h.Epochlog.tx_count = 0 -> loop (epoch_id + 1)
      | _ -> false
  in
  loop (from_epoch + 1)

let rollback_to_head t ~head_epoch ~head_txlog_seg ~head_txlog_off
                       ~head_epochlog_off ~inflight_start_txid ~inflight_tx_count =
  Txlog.truncate_to t.txlog ~seg_id:head_txlog_seg ~offset:head_txlog_off;
  Epochlog.truncate_to t.epochlog ~offset:head_epochlog_off;
  let (n_tx, n_ep, n_addr, n_txid) = Chaindata_index.cleanup_after_epoch t.index
    ~max_epoch:head_epoch
    ~start_txid_inflight:inflight_start_txid
    ~tx_count_inflight:inflight_tx_count in

  Chaindata_index.set_meta_direct t.index "repaired_upto_epoch"
    (string_of_int head_epoch);
  t.next_txid <- Int64.add inflight_start_txid 0L;
  (n_tx, n_ep, n_addr, n_txid)

let rebuild_index t =
  let next_txid = ref 0L in
  Chaindata_index.begin_rebuild_write t.index;
  Txlog.scan_all t.txlog (fun seg_id offset len epoch_id payload ->
    let txid = !next_txid in
    next_txid := Int64.add !next_txid 1L;
    (try
       let (hash, tx_json) = split_payload payload in
       let j = Yojson.Safe.from_string tx_json in
       let open Yojson.Safe.Util in
       let from_addr = try j |> member "from" |> to_string with _ -> "" in
       let to_addr = try j |> member "to_" |> to_string with _ -> "" in
       let op_type = try j |> member "op_type" |> to_string with _ -> "standard" in
       let encrypted_data = try j |> member "encrypted_data" |> to_string with _ -> "" in
       let message = try j |> member "message" |> to_string with _ -> "" in
       let addrs = extract_call_recipient ~op_type ~encrypted_data
         ~message ~from_addr ~to_addr in
       Chaindata_index.buffer_tx t.index ~hash ~seg_id ~offset ~len
         ~epoch_id ~txid ~from_addr ~to_addr ~addrs
     with _ -> ())
  );
  List.iter (fun h ->
    Chaindata_index.buffer_epoch t.index h.Epochlog.id (Epochlog.epoch_to_json h)
  ) (Epochlog.read_all t.epochlog);
  Chaindata_index.buffer_meta t.index "next_txid" (Int64.to_string !next_txid);
  Chaindata_index.commit_write t.index;
  t.next_txid <- !next_txid