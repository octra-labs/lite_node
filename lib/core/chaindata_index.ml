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


type tx_write = {
  hash : string;
  seg_id : int;
  offset : int;
  len : int;
  epoch_id : int;
  txid : int64;
  from_addr : string;
  to_addr : string;
  extra_addrs : string list;
}

type batch = {
  mutable txs : tx_write list;
  mutable epochs : (int * string) list;
  mutable rejected : (string * string * string * int) list;
  mutable receipts : (string * string) list;
  mutable meta_writes : (string * string) list;
  mutable clear_rebuild_tables : bool;
}

type t = {
  env : Lmdb.Env.t;
  tx_loc : (string, string, [ `Uni ]) Lmdb.Map.t;
  addr_tx : (string, int64, [ `Dup | `Uni ]) Lmdb.Map.t;
  addr_recent : (string, string, [ `Uni ]) Lmdb.Map.t;
  txid_loc : (int64, string, [ `Uni ]) Lmdb.Map.t;
  epoch_meta : (int32, string, [ `Uni ]) Lmdb.Map.t;
  rejected : (string, string, [ `Uni ]) Lmdb.Map.t;
  rej_addr : (string, string, [ `Dup | `Uni ]) Lmdb.Map.t;
  rej_epoch : (int32, string, [ `Dup | `Uni ]) Lmdb.Map.t;
  receipts : (string, string, [ `Uni ]) Lmdb.Map.t;
  meta : (string, string, [ `Uni ]) Lmdb.Map.t;
  mutable batch : batch option;
}

let map_size_default = 64 * 1024 * 1024 * 1024
let addr_recent_limit = 128
let copy s = Bytes.unsafe_to_string (Bytes.of_string s)

let open_index path =
  if not (Sys.file_exists path) then begin
    try Unix.mkdir path 0o755 with Unix.Unix_error _ -> ()
  end;
  let env = Lmdb.Env.create Lmdb.Rw
    ~max_maps:13
    ~max_readers:2048
    ~map_size:map_size_default
    ~flags:Lmdb.Env.Flags.(no_tls + no_read_ahead)
    path in
  let tx_loc = Lmdb.Map.create Lmdb.Map.Nodup
    ~key:Lmdb.Conv.string ~value:Lmdb.Conv.string ~name:"tx_loc" env in
  let addr_tx = Lmdb.Map.create Lmdb.Map.Dup
    ~key:Lmdb.Conv.string ~value:Lmdb.Conv.int64_be ~name:"addr_tx" env in
  let addr_recent = Lmdb.Map.create Lmdb.Map.Nodup
    ~key:Lmdb.Conv.string ~value:Lmdb.Conv.string ~name:"addr_recent" env in
  let txid_loc = Lmdb.Map.create Lmdb.Map.Nodup
    ~key:Lmdb.Conv.int64_be ~value:Lmdb.Conv.string ~name:"txid_loc" env in
  let epoch_meta = Lmdb.Map.create Lmdb.Map.Nodup
    ~key:Lmdb.Conv.int32_be ~value:Lmdb.Conv.string ~name:"epoch_meta" env in
  let rejected = Lmdb.Map.create Lmdb.Map.Nodup
    ~key:Lmdb.Conv.string ~value:Lmdb.Conv.string ~name:"rejected" env in
  let rej_addr = Lmdb.Map.create Lmdb.Map.Dup
    ~key:Lmdb.Conv.string ~value:Lmdb.Conv.string ~name:"rej_addr" env in
  let rej_epoch = Lmdb.Map.create Lmdb.Map.Dup
    ~key:Lmdb.Conv.int32_be ~value:Lmdb.Conv.string ~name:"rej_epoch" env in
  let receipts = Lmdb.Map.create Lmdb.Map.Nodup
    ~key:Lmdb.Conv.string ~value:Lmdb.Conv.string ~name:"receipts" env in
  let meta = Lmdb.Map.create Lmdb.Map.Nodup
    ~key:Lmdb.Conv.string ~value:Lmdb.Conv.string ~name:"meta" env in
  { env; tx_loc; addr_tx; addr_recent; txid_loc; epoch_meta; rejected; rej_addr; rej_epoch;
    receipts; meta; batch = None }

let close t =
  Lmdb.Env.sync t.env;
  Lmdb.Env.close t.env

let encode_tx_loc ~seg_id ~offset ~len ~epoch_id =
  let buf = Bytes.create 20 in
  Bytes.set_int32_le buf 0 (Int32.of_int seg_id);
  Bytes.set_int64_le buf 4 (Int64.of_int offset);
  Bytes.set_int32_le buf 12 (Int32.of_int len);
  Bytes.set_int32_le buf 16 (Int32.of_int epoch_id);
  Bytes.to_string buf

let decode_tx_loc s =
  if String.length s < 20 then failwith "decode_tx_loc: too short";
  let buf = Bytes.of_string s in
  let seg_id = Int32.to_int (Bytes.get_int32_le buf 0) in
  let offset = Int64.to_int (Bytes.get_int64_le buf 4) in
  let len = Int32.to_int (Bytes.get_int32_le buf 12) in
  let epoch_id = Int32.to_int (Bytes.get_int32_le buf 16) in
  (seg_id, offset, len, epoch_id)

let encode_txid_loc ~seg_id ~offset ~len =
  let buf = Bytes.create 16 in
  Bytes.set_int32_le buf 0 (Int32.of_int seg_id);
  Bytes.set_int64_le buf 4 (Int64.of_int offset);
  Bytes.set_int32_le buf 12 (Int32.of_int len);
  Bytes.to_string buf

let decode_txid_loc s =
  if String.length s < 16 then failwith "decode_txid_loc: too short";
  let buf = Bytes.of_string s in
  let seg_id = Int32.to_int (Bytes.get_int32_le buf 0) in
  let offset = Int64.to_int (Bytes.get_int64_le buf 4) in
  let len = Int32.to_int (Bytes.get_int32_le buf 12) in
  (seg_id, offset, len)

let encode_addr_recent refs =
  let buf = Bytes.create (List.length refs * 12) in
  List.iteri (fun i (epoch_id, txid) ->
    let base = i * 12 in
    Bytes.set_int32_le buf base (Int32.of_int epoch_id);
    Bytes.set_int64_le buf (base + 4) txid
  ) refs;
  Bytes.unsafe_to_string buf

let decode_addr_recent s =
  if String.length s mod 12 <> 0 then None
  else
    let buf = Bytes.of_string s in
    let rec loop i acc =
      if i >= String.length s then Some (List.rev acc)
      else
        let epoch_id = Int32.to_int (Bytes.get_int32_le buf i) in
        let txid = Bytes.get_int64_le buf (i + 4) in
        loop (i + 12) ((epoch_id, txid) :: acc)
    in
    loop 0 []

let compare_recent_ref (epoch_a, txid_a) (epoch_b, txid_b) =
  if epoch_a <> epoch_b then compare epoch_b epoch_a else compare txid_b txid_a

let unique_addrs from_addr to_addr extra_addrs =
  let seen = Hashtbl.create (2 + List.length extra_addrs) in
  let acc = ref [] in
  List.iter (fun addr ->
    if String.length addr > 0 && not (Hashtbl.mem seen addr) then begin
      Hashtbl.add seen addr ();
      acc := addr :: !acc
    end
  ) (from_addr :: to_addr :: extra_addrs);
  List.rev !acc

let merge_addr_recent old_refs new_refs =
  let sorted = List.sort compare_recent_ref (new_refs @ old_refs) in
  let seen = Hashtbl.create (List.length sorted) in
  let rec take remaining acc = function
    | _ when remaining <= 0 -> List.rev acc
    | [] -> List.rev acc
    | (_, txid) as ref_ :: tl ->
        if Hashtbl.mem seen txid then take remaining acc tl
        else begin
          Hashtbl.add seen txid ();
          take (remaining - 1) (ref_ :: acc) tl
        end
  in
  take addr_recent_limit [] sorted

let get_addr_recent t addr =
  try
    decode_addr_recent (copy (Lmdb.Map.get t.addr_recent addr))
  with Not_found -> Some []

let drop_addr_recent_txn t txn =
  Lmdb.Map.drop ~txn ~delete:false t.addr_recent

let empty_batch () =
  { txs = []; epochs = []; rejected = []; receipts = []; meta_writes = []; clear_rebuild_tables = false }

let begin_write t =
  t.batch <- Some (empty_batch ())

let begin_rebuild_write t =
  let b = empty_batch () in
  b.clear_rebuild_tables <- true;
  t.batch <- Some b

let buffer_tx t ~hash ~seg_id ~offset ~len ~epoch_id ~txid ~from_addr ~to_addr ~addrs =
  match t.batch with
  | None -> failwith "history_index: no batch active"
  | Some b ->
    b.txs <- { hash; seg_id; offset; len; epoch_id; txid; from_addr; to_addr;
               extra_addrs = addrs } :: b.txs

let buffer_epoch t epoch_id json =
  match t.batch with
  | None -> failwith "history_index: no batch active"
  | Some b -> b.epochs <- (epoch_id, json) :: b.epochs

let buffer_rejected t ~hash ~addr ~json ~epoch_id =
  match t.batch with
  | None -> failwith "history_index: no batch active"
  | Some b -> b.rejected <- (hash, addr, json, epoch_id) :: b.rejected

let buffer_receipt t tx_hash json =
  match t.batch with
  | None -> failwith "history_index: no batch active"
  | Some b -> b.receipts <- (tx_hash, json) :: b.receipts

let buffer_meta t key value =
  match t.batch with
  | None -> failwith "history_index: no batch active"
  | Some b -> b.meta_writes <- (key, value) :: b.meta_writes

exception Index_commit_failed of string


let set_tx_loc_only t hash ~seg_id ~offset ~len ~epoch_id =
  let loc = encode_tx_loc ~seg_id ~offset ~len ~epoch_id in
  match
    Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
      Lmdb.Map.set t.tx_loc ~txn hash loc)
  with
  | Some () -> ()
  | None -> raise (Index_commit_failed "set_tx_loc_only: Txn.go returned None")


let buffer_tx_loc_only t ~hash ~seg_id ~offset ~len ~epoch_id =
  match t.batch with
  | None -> failwith "history_index: no batch active"
  | Some b ->
    b.txs <- { hash; seg_id; offset; len; epoch_id; txid = 0L;
               from_addr = ""; to_addr = ""; extra_addrs = [] } :: b.txs


let commit_tx_loc_only t =
  match t.batch with
  | None -> failwith "history_index: no batch to commit"
  | Some b ->
    let result =
      try
        Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
          if b.clear_rebuild_tables then begin
            Lmdb.Map.drop ~txn ~delete:false t.tx_loc;
            Lmdb.Map.drop ~txn ~delete:false t.txid_loc;
            Lmdb.Map.drop ~txn ~delete:false t.epoch_meta;
            Lmdb.Map.drop ~txn ~delete:false t.addr_tx;
            Lmdb.Map.drop ~txn ~delete:false t.addr_recent
          end;
          List.iter (fun tw ->
            let loc = encode_tx_loc ~seg_id:tw.seg_id ~offset:tw.offset
              ~len:tw.len ~epoch_id:tw.epoch_id in
            Lmdb.Map.set t.tx_loc ~txn tw.hash loc
          ) (List.rev b.txs)
        )
      with e ->
        let msg = Printf.sprintf "commit_tx_loc_only: %s" (Printexc.to_string e) in
        raise (Index_commit_failed msg)
    in
    (match result with
     | Some () -> ()
     | None -> raise (Index_commit_failed "commit_tx_loc_only: Txn.go returned None"));
    t.batch <- None

let commit_write t =
  match t.batch with
  | None -> failwith "history_index: no batch to commit"
  | Some b ->
    let result =
      try
        Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
          if b.clear_rebuild_tables then begin
            Lmdb.Map.drop ~txn ~delete:false t.tx_loc;
            Lmdb.Map.drop ~txn ~delete:false t.txid_loc;
            Lmdb.Map.drop ~txn ~delete:false t.epoch_meta;
            Lmdb.Map.drop ~txn ~delete:false t.addr_tx;
            Lmdb.Map.drop ~txn ~delete:false t.addr_recent
          end;
          List.iter (fun tw ->
            let loc = encode_tx_loc ~seg_id:tw.seg_id ~offset:tw.offset ~len:tw.len ~epoch_id:tw.epoch_id in
            Lmdb.Map.set t.tx_loc ~txn tw.hash loc;

            let tx_loc_only =
              String.length tw.from_addr = 0
              && String.length tw.to_addr = 0
              && tw.extra_addrs = []
            in
            if not tx_loc_only then begin
              let tloc = encode_txid_loc ~seg_id:tw.seg_id ~offset:tw.offset ~len:tw.len in
              Lmdb.Map.set t.txid_loc ~txn tw.txid tloc
            end;
            if String.length tw.from_addr > 0 then
              (try Lmdb.Map.add t.addr_tx ~txn tw.from_addr tw.txid with Lmdb.Exists -> ());
            if tw.to_addr <> tw.from_addr && String.length tw.to_addr > 0 then
              (try Lmdb.Map.add t.addr_tx ~txn tw.to_addr tw.txid with Lmdb.Exists -> ());
            List.iter (fun a ->
              if a <> tw.from_addr && a <> tw.to_addr && String.length a > 0 then
                (try Lmdb.Map.add t.addr_tx ~txn a tw.txid with Lmdb.Exists -> ())
            ) tw.extra_addrs
          ) (List.rev b.txs);
          let recent_updates = Hashtbl.create 64 in
          List.iter (fun tw ->
            let tx_loc_only =
              String.length tw.from_addr = 0
              && String.length tw.to_addr = 0
              && tw.extra_addrs = []
            in
            if not tx_loc_only then
              List.iter (fun addr ->
                let existing =
                  match Hashtbl.find_opt recent_updates addr with
                  | Some refs -> refs
                  | None -> []
                in
                Hashtbl.replace recent_updates addr ((tw.epoch_id, tw.txid) :: existing)
              ) (unique_addrs tw.from_addr tw.to_addr tw.extra_addrs)
          ) (List.rev b.txs);
          Hashtbl.iter (fun addr refs ->
            let old_refs_opt =
              try decode_addr_recent (copy (Lmdb.Map.get t.addr_recent ~txn addr))
              with Not_found -> Some []
            in
            match old_refs_opt with
            | Some old_refs ->
                let merged = merge_addr_recent old_refs refs in
                Lmdb.Map.set t.addr_recent ~txn addr (encode_addr_recent merged)
            | None ->
                try Lmdb.Map.remove t.addr_recent ~txn addr with Not_found -> ()
          ) recent_updates;
          List.iter (fun (eid, json) ->
            Lmdb.Map.set t.epoch_meta ~txn (Int32.of_int eid) json
          ) b.epochs;
          List.iter (fun (hash, addr, json, epoch_id) ->
            Lmdb.Map.set t.rejected ~txn hash json;
            (try Lmdb.Map.add t.rej_addr ~txn addr hash with Lmdb.Exists -> ());
            (try Lmdb.Map.add t.rej_epoch ~txn (Int32.of_int epoch_id) hash with Lmdb.Exists -> ())
          ) (List.rev b.rejected);
          List.iter (fun (tx_hash, json) ->
            Lmdb.Map.set t.receipts ~txn tx_hash json
          ) b.receipts;
          List.iter (fun (key, value) ->
            Lmdb.Map.set t.meta ~txn key value
          ) b.meta_writes
        )
      with e ->
        let msg = Printf.sprintf "LMDB commit failed: %s" (Printexc.to_string e) in
        Octra_log.stderr "[CHAINDATA_INDEX FATAL] %s\n%!" msg;
        raise (Index_commit_failed msg)
    in
    (match result with
     | Some () -> t.batch <- None
     | None ->
       let msg = "LMDB Txn.go returned None (commit failed)" in
       Octra_log.stderr "[CHAINDATA_INDEX FATAL] %s\n%!" msg;
       raise (Index_commit_failed msg))

let abort_write t =
  t.batch <- None

let get_tx_loc t hash =
  try
    let v = copy (Lmdb.Map.get t.tx_loc hash) in
    let (_, _, _, epoch_id) as decoded = decode_tx_loc v in

    if Head_manifest.is_epoch_visible (Head_manifest.get_cached ()) epoch_id
    then Some decoded
    else None
  with
  | Not_found -> None
  | _ -> None

let get_tx_loc_raw t hash =
  try
    let v = copy (Lmdb.Map.get t.tx_loc hash) in
    Some (decode_tx_loc v)
  with
  | Not_found -> None
  | _ -> None

let get_txid_loc t txid =
  try

    if not (Head_manifest.is_txid_visible (Head_manifest.get_cached ()) txid)
    then None
    else
      let v = copy (Lmdb.Map.get t.txid_loc txid) in
      Some (decode_txid_loc v)
  with Not_found -> None

let get_txid_loc_raw t txid =
  try
    let v = copy (Lmdb.Map.get t.txid_loc txid) in
    Some (decode_txid_loc v)
  with
  | Not_found -> None
  | _ -> None

let addr_txids_rev t addr ~limit ~offset =
  try
    Lmdb.Cursor.go Lmdb.Ro t.addr_tx (fun c ->
      ignore (Lmdb.Cursor.seek c addr);
      let (k0, _) = Lmdb.Cursor.current c in
      if k0 <> addr then (0, [])
      else
      let total = Lmdb.Cursor.count c in
      if total = 0 || offset >= total then (total, [])
      else begin

        ignore (Lmdb.Cursor.last_dup c);
        (try for _ = 1 to offset do
          ignore (Lmdb.Cursor.prev_dup c)
        done with Not_found -> ());

        let acc = ref [] in
        let n = ref 0 in
        let (_, v0) = Lmdb.Cursor.current c in
        acc := v0 :: !acc; incr n;
        (try while !n < limit do
          let v = Lmdb.Cursor.prev_dup c in
          acc := v :: !acc; incr n
        done with Not_found -> ());
        (total, List.rev !acc)
      end
    )
  with Not_found -> (0, [])

let addr_tx_count t addr =
  try
    Lmdb.Cursor.go Lmdb.Ro t.addr_tx (fun c ->
      ignore (Lmdb.Cursor.seek c addr);
      let (k0, _) = Lmdb.Cursor.current c in
      if k0 <> addr then 0 else Lmdb.Cursor.count c
    )
  with Not_found -> 0

let addr_has_txid_raw t addr txid =
  try
    Lmdb.Cursor.go Lmdb.Ro t.addr_tx (fun c ->
      ignore (Lmdb.Cursor.seek c addr);
      let (k0, _) = Lmdb.Cursor.current c in
      if k0 <> addr then false else
      let found = ref false in
      (try
         let (_, v0) = Lmdb.Cursor.current c in
         if v0 = txid then found := true;
         while not !found do
           let v = Lmdb.Cursor.next_dup c in
           if v = txid then found := true
         done
       with Not_found -> ());
      !found
    )
  with Not_found -> false

let get_epoch t epoch_id =

  if not (Head_manifest.is_epoch_visible (Head_manifest.get_cached ()) epoch_id)
  then None
  else
    try Some (copy (Lmdb.Map.get t.epoch_meta (Int32.of_int epoch_id)))
    with Not_found -> None

let get_epoch_raw t epoch_id =
  try Some (copy (Lmdb.Map.get t.epoch_meta (Int32.of_int epoch_id)))
  with
  | Not_found -> None
  | _ -> None

let list_epoch_ids t =
  try
    Lmdb.Cursor.go Lmdb.Ro t.epoch_meta (fun c ->
      let results = ref [] in
      (try
        let (k, _) = Lmdb.Cursor.last c in
        results := Int32.to_int k :: !results;
        (try while true do
          let (k, _) = Lmdb.Cursor.prev c in
          results := Int32.to_int k :: !results
        done with Not_found -> ())
      with Not_found -> ());
      !results
    )
  with Not_found -> []

let get_rejected t hash =
  try Some (copy (Lmdb.Map.get t.rejected hash))
  with Not_found -> None

let rejected_by_addr t addr ~limit ~offset =
  try
    Lmdb.Cursor.go Lmdb.Ro t.rej_addr (fun c ->
      ignore (Lmdb.Cursor.seek c addr);
      let (k0, _) = Lmdb.Cursor.current c in
      if k0 <> addr then []
      else
      let total = Lmdb.Cursor.count c in
      if offset >= total || limit <= 0 then []
      else begin
        let _ = Lmdb.Cursor.first_dup c in
        (try for _ = 1 to offset do
          ignore (Lmdb.Cursor.next_dup c)
        done with Not_found -> ());
        let max_to_collect = min limit (total - offset) in
        let acc = ref [] in
        let n = ref 0 in
        (try
          let (_, v0) = Lmdb.Cursor.current c in
          acc := copy v0 :: !acc; incr n;
          while !n < max_to_collect do
            let v = Lmdb.Cursor.next_dup c in
            acc := copy v :: !acc; incr n
          done
        with Not_found -> ());
        List.rev !acc
      end
    )
  with Not_found -> []

let rejected_by_addr_rev t addr ~limit ~offset =
  try
    Lmdb.Cursor.go Lmdb.Ro t.rej_addr (fun c ->
      ignore (Lmdb.Cursor.seek c addr);
      let (k0, _) = Lmdb.Cursor.current c in
      if k0 <> addr then []
      else
      let total = Lmdb.Cursor.count c in
      if total = 0 || offset >= total || limit <= 0 then []
      else begin
        ignore (Lmdb.Cursor.last_dup c);
        (try for _ = 1 to offset do
          ignore (Lmdb.Cursor.prev_dup c)
        done with Not_found -> ());
        let max_to_collect = min limit (total - offset) in
        let acc = ref [] in
        let n = ref 0 in
        (try
          let (_, v0) = Lmdb.Cursor.current c in
          acc := copy v0 :: !acc; incr n;
          while !n < max_to_collect do
            let v = Lmdb.Cursor.prev_dup c in
            acc := copy v :: !acc; incr n
          done
        with Not_found -> ());
        List.rev !acc
      end
    )
  with Not_found -> []

let rejected_count_by_addr t addr =
  try
    Lmdb.Cursor.go Lmdb.Ro t.rej_addr (fun c ->
      ignore (Lmdb.Cursor.seek c addr);
      let (k0, _) = Lmdb.Cursor.current c in
      if k0 <> addr then 0 else Lmdb.Cursor.count c
    )
  with Not_found -> 0

let get_receipt t tx_hash =
  try Some (copy (Lmdb.Map.get t.receipts tx_hash))
  with Not_found -> None

let get_meta t key =
  try Some (copy (Lmdb.Map.get t.meta key))
  with Not_found -> None


let get_meta_string t key = get_meta t key

let get_meta_int t key =
  match get_meta t key with
  | None -> None
  | Some s -> (try Some (int_of_string s) with _ -> None)

let get_meta_int64 t key =
  match get_meta t key with
  | None -> None
  | Some s -> (try Some (Int64.of_string s) with _ -> None)


let cleanup_after_epoch t ~max_epoch ~start_txid_inflight ~tx_count_inflight:_ =
  let to_del_hashes = ref [] in
  let to_del_epochs_int32 = ref [] in
  let to_del_addr_pairs = ref [] in
  let to_del_txids = ref [] in

  let walk_collect () =
    ignore (Lmdb.Txn.go Lmdb.Ro t.env (fun txn ->
      ignore (Lmdb.Cursor.go Lmdb.Ro t.tx_loc ~txn (fun cur ->
        try
          let rec loop (hash, loc_str) =
            (try
              let (_, _, _, eid) = decode_tx_loc loc_str in
              if eid > max_epoch then to_del_hashes := hash :: !to_del_hashes
            with _ -> ());
            loop (Lmdb.Cursor.next cur)
          in
          loop (Lmdb.Cursor.first cur)
        with Not_found -> ()));
      ignore (Lmdb.Cursor.go Lmdb.Ro t.epoch_meta ~txn (fun cur ->
        try
          let rec loop (eid32, _) =
            let eid = Int32.to_int eid32 in
            if eid > max_epoch then
              to_del_epochs_int32 := eid32 :: !to_del_epochs_int32;
            loop (Lmdb.Cursor.next cur)
          in
          loop (Lmdb.Cursor.first cur)
        with Not_found -> ()));
      ignore (Lmdb.Cursor.go Lmdb.Ro t.txid_loc ~txn (fun cur ->
        try
          let rec seek_or_next first =
            let (txid, _) =
              if first then Lmdb.Cursor.seek_range cur start_txid_inflight
              else Lmdb.Cursor.next cur
            in
            if Int64.compare txid start_txid_inflight >= 0 then begin
              to_del_txids := txid :: !to_del_txids;
              seek_or_next false
            end
          in
          seek_or_next true
        with Not_found -> ()));
      ignore (Lmdb.Cursor.go Lmdb.Ro t.addr_tx ~txn (fun cur ->
        try
          let rec loop (addr, txid) =
            if Int64.compare txid start_txid_inflight >= 0 then
              to_del_addr_pairs := (addr, txid) :: !to_del_addr_pairs;
            loop (Lmdb.Cursor.next cur)
          in
          loop (Lmdb.Cursor.first cur)
        with Not_found -> ()))
    ))
  in
  (try walk_collect () with _ -> ());
  let result =
    try
      Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
        List.iter (fun h ->
          try Lmdb.Map.remove t.tx_loc ~txn h with _ -> ()
        ) !to_del_hashes;
        List.iter (fun eid32 ->
          try Lmdb.Map.remove t.epoch_meta ~txn eid32 with _ -> ()
        ) !to_del_epochs_int32;
        List.iter (fun txid ->
          try Lmdb.Map.remove t.txid_loc ~txn txid with _ -> ()
        ) !to_del_txids;
        List.iter (fun (addr, txid) ->
          try Lmdb.Map.remove t.addr_tx ~txn ~value:txid addr with _ -> ()
        ) !to_del_addr_pairs;
        drop_addr_recent_txn t txn;
      )
    with e ->
      let msg = Printf.sprintf "cleanup_after_epoch txn failed: %s" (Printexc.to_string e) in
      Octra_log.stderr "[CHAINDATA_INDEX FATAL] %s\n%!" msg;
      raise (Index_commit_failed msg)
  in
  (match result with
   | Some () -> ()
   | None ->
     let msg = "cleanup_after_epoch: Txn.go returned None" in
     Octra_log.stderr "[CHAINDATA_INDEX FATAL] %s\n%!" msg;
     raise (Index_commit_failed msg));
  (List.length !to_del_hashes,
   List.length !to_del_epochs_int32,
   List.length !to_del_addr_pairs,
   List.length !to_del_txids)

let set_meta_direct t key value =
  let result =
    try
      Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
        Lmdb.Map.set t.meta ~txn key value
      )
    with e ->
      let msg = Printf.sprintf "LMDB set_meta_direct failed key=%S: %s" key (Printexc.to_string e) in
      Octra_log.stderr "[CHAINDATA_INDEX FATAL] %s\n%!" msg;
      raise (Index_commit_failed msg)
  in
  match result with
  | Some () -> ()
  | None ->
    let msg = Printf.sprintf "LMDB set_meta_direct Txn.go returned None key=%S" key in
    Octra_log.stderr "[CHAINDATA_INDEX FATAL] %s\n%!" msg;
    raise (Index_commit_failed msg)


let set_meta_many_direct t kvs =
  let result =
    try
      Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
        List.iter (fun (k, v) -> Lmdb.Map.set t.meta ~txn k v) kvs)
    with e ->
      raise (Index_commit_failed
        (Printf.sprintf "set_meta_many_direct: %s" (Printexc.to_string e)))
  in
  match result with
  | Some () -> ()
  | None ->
    raise (Index_commit_failed "set_meta_many_direct: Txn.go returned None")



let repair_tx_full t ~hash ~seg_id ~offset ~len ~epoch_id ~txid
    ~from_addr ~to_addr =
  let result =
    try
      Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
        let loc = encode_tx_loc ~seg_id ~offset ~len ~epoch_id in
        Lmdb.Map.set t.tx_loc ~txn hash loc;
        let tloc = encode_txid_loc ~seg_id ~offset ~len in
        Lmdb.Map.set t.txid_loc ~txn txid tloc;
        (try Lmdb.Map.add t.addr_tx ~txn from_addr txid with Lmdb.Exists -> ());
        if to_addr <> from_addr then
          (try Lmdb.Map.add t.addr_tx ~txn to_addr txid with Lmdb.Exists -> ());
        drop_addr_recent_txn t txn
      )
    with e ->
      raise (Index_commit_failed
        (Printf.sprintf "repair_tx_full: %s" (Printexc.to_string e)))
  in
  match result with
  | Some () -> ()
  | None -> raise (Index_commit_failed "repair_tx_full: Txn.go returned None")

let txid_loc_present t txid =
  try
    let _ = Lmdb.Map.get t.txid_loc txid in
    true
  with Not_found -> false

let remove_txid_loc_direct t txid =
  let result =
    try
      Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
        try Lmdb.Map.remove t.txid_loc ~txn txid with Not_found -> ())
    with e ->
      raise (Index_commit_failed
        (Printf.sprintf "remove_txid_loc_direct: %s" (Printexc.to_string e)))
  in
  match result with
  | Some () -> ()
  | None -> raise (Index_commit_failed "remove_txid_loc_direct: Txn.go returned None")

let remove_addr_tx_direct t ~addr ~txid =
  let result =
    try
      Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
        (try Lmdb.Map.remove t.addr_tx ~txn ~value:txid addr with Not_found -> ());
        drop_addr_recent_txn t txn)
    with e ->
      raise (Index_commit_failed
        (Printf.sprintf "remove_addr_tx_direct: %s" (Printexc.to_string e)))
  in
  match result with
  | Some () -> ()
  | None -> raise (Index_commit_failed "remove_addr_tx_direct: Txn.go returned None")


let find_txid_for_epoch_pointer t ~epoch_id ~seg_id ~offset =
  let em_v = try Some (copy (Lmdb.Map.get t.epoch_meta (Int32.of_int epoch_id)))
             with Not_found -> None in
  let em_next = try Some (copy (Lmdb.Map.get t.epoch_meta (Int32.of_int (epoch_id + 1))))
                with Not_found -> None in
  match em_v, em_next with
  | None, _ -> None
  | Some j, opt_next ->
    (try
      let open Yojson.Safe.Util in
      let json = Yojson.Safe.from_string j in
      let start_txid = Int64.of_string (json |> member "start_txid" |> to_string) in
      let max_drift =
        match opt_next with
        | Some j2 ->
          let json2 = Yojson.Safe.from_string j2 in
          let next_start = Int64.of_string (json2 |> member "start_txid" |> to_string) in
          Int64.to_int (Int64.sub next_start start_txid)
        | None -> 64
      in
      let max_drift = max 1 (min max_drift 1024) in
      let found = ref None in
      let i = ref 0 in
      while !found = None && !i < max_drift do
        let cand = Int64.add start_txid (Int64.of_int !i) in
        (match
           try Some (copy (Lmdb.Map.get t.txid_loc cand))
           with Not_found -> None
         with
         | Some v ->
           let (s, o, _) = decode_txid_loc v in
           if s = seg_id && o = offset then found := Some cand
         | None -> ());
        incr i
      done;
      !found
    with _ -> None)


let repair_class_b t ~hash ~seg_id ~offset ~len ~epoch_id ~txid
    ~from_addr ~to_addr =
  let result =
    try
      Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
        let loc = encode_tx_loc ~seg_id ~offset ~len ~epoch_id in
        Lmdb.Map.set t.tx_loc ~txn hash loc;
        (try Lmdb.Map.add t.addr_tx ~txn from_addr txid with Lmdb.Exists -> ());
        if to_addr <> from_addr then
          (try Lmdb.Map.add t.addr_tx ~txn to_addr txid with Lmdb.Exists -> ());
        drop_addr_recent_txn t txn
      )
    with e ->
      raise (Index_commit_failed
        (Printf.sprintf "repair_class_b: %s" (Printexc.to_string e)))
  in
  match result with
  | Some () -> ()
  | None -> raise (Index_commit_failed "repair_class_b: Txn.go returned None")

let set_epoch_meta_json t epoch_id json =
  let result =
    try
      Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
        Lmdb.Map.set t.epoch_meta ~txn (Int32.of_int epoch_id) json)
    with e ->
      raise (Index_commit_failed
        (Printf.sprintf "set_epoch_meta_json: %s" (Printexc.to_string e)))
  in
  match result with
  | Some () -> ()
  | None -> raise (Index_commit_failed "set_epoch_meta_json: Txn.go returned None")


type delta_row = {
  d_txid : int64;
  d_hash : string;
  d_seg_id : int;
  d_offset : int;
  d_len : int;
  d_epoch_id : int;
  d_addrs : string list;
}

let apply_delta_atomic t ~rows ~epoch_meta_updates ~meta_kvs =
  let result =
    try
      Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
        List.iter (fun r ->
          let tloc = encode_txid_loc ~seg_id:r.d_seg_id ~offset:r.d_offset ~len:r.d_len in
          Lmdb.Map.set t.txid_loc ~txn r.d_txid tloc;
          let xloc = encode_tx_loc ~seg_id:r.d_seg_id ~offset:r.d_offset
            ~len:r.d_len ~epoch_id:r.d_epoch_id in
          Lmdb.Map.set t.tx_loc ~txn r.d_hash xloc;
          List.iter (fun addr ->
            try Lmdb.Map.add t.addr_tx ~txn addr r.d_txid with Lmdb.Exists -> ()
          ) r.d_addrs
        ) rows;
        drop_addr_recent_txn t txn;
        List.iter (fun (eid, json) ->
          Lmdb.Map.set t.epoch_meta ~txn (Int32.of_int eid) json
        ) epoch_meta_updates;
        List.iter (fun (k, v) -> Lmdb.Map.set t.meta ~txn k v) meta_kvs)
    with e ->
      raise (Index_commit_failed
        (Printf.sprintf "apply_delta_atomic: %s" (Printexc.to_string e)))
  in
  match result with
  | Some () -> ()
  | None -> raise (Index_commit_failed "apply_delta_atomic: Txn.go returned None")


let clear_rebuild_tables t =
  let result =
    try
      Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
        Lmdb.Map.drop ~txn ~delete:false t.tx_loc;
        Lmdb.Map.drop ~txn ~delete:false t.txid_loc;
        Lmdb.Map.drop ~txn ~delete:false t.epoch_meta;
        Lmdb.Map.drop ~txn ~delete:false t.addr_tx;
        Lmdb.Map.drop ~txn ~delete:false t.addr_recent)
    with e ->
      raise (Index_commit_failed
        (Printf.sprintf "clear_rebuild_tables: %s" (Printexc.to_string e)))
  in
  match result with
  | Some () -> ()
  | None -> raise (Index_commit_failed "clear_rebuild_tables: Txn.go returned None")

let scan_all_rejected t f =
  try
    Lmdb.Cursor.go Lmdb.Ro t.rejected (fun c ->
      (try
        let (k, v) = Lmdb.Cursor.first c in
        f (copy k) (copy v);
        (try while true do
          let (k, v) = Lmdb.Cursor.next c in
          f (copy k) (copy v)
        done with Not_found -> ())
      with Not_found -> ())
    )
  with Not_found -> ()

let rejected_by_epoch t epoch_id =
  try
    Lmdb.Cursor.go Lmdb.Ro t.rej_epoch (fun c ->
      ignore (Lmdb.Cursor.seek c (Int32.of_int epoch_id));
      let acc = ref [] in
      (try
        let (_, v0) = Lmdb.Cursor.current c in
        acc := copy v0 :: !acc;
        (try while true do
          let v = Lmdb.Cursor.next_dup c in
          acc := copy v :: !acc
        done with Not_found -> ())
      with Not_found -> ());
      List.rev !acc
    )
  with Not_found -> []

let recent_txids t ~limit ~offset =
  try
    Lmdb.Cursor.go Lmdb.Ro t.txid_loc (fun c ->
      let (k, _) = Lmdb.Cursor.last c in
      let current = ref k in
      let ok = ref true in
      (try for _ = 1 to offset do
        let (k, _) = Lmdb.Cursor.prev c in
        current := k
      done with Not_found -> ok := false);
      if not !ok then []
      else begin
        let results = ref [!current] in
        let remaining = ref (limit - 1) in
        (try while !remaining > 0 do
          let (k, _) = Lmdb.Cursor.prev c in
          results := k :: !results;
          decr remaining
        done with Not_found -> ());
        !results
      end
    )
  with Not_found -> []