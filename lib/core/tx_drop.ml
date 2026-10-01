(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type row = Drop_record.t = {
  hash : string;
  from_addr : string;
  to_addr : string;
  nonce : int;
  ou : Z.t;
  op_type : Transaction.op_type;
  reason : string;
  detail : string;
  dropped_at : float;
}

type t = {
  env : Lmdb.Env.t;
  resources : Store_scope.t;
  rows : (string, string, [ `Uni ]) Lmdb.Map.t;
  times : (string, string, [ `Uni ]) Lmdb.Map.t;
  addresses : (string, string, [ `Dup | `Uni ]) Lmdb.Map.t;
  max_rows : int;
  mutable closed : bool;
}

let open_db ?(max_rows = 10_000) data_dir =
  if max_rows < 1 then invalid_arg "tx_drop max_rows must be positive";
  let dir = Filename.concat data_dir "local_drops" in
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o700;
  let owner = Store_lock.acquire dir in
  let resources = Store_scope.create ~release:(fun () -> Store_lock.release owner) in
  Store_scope.guard resources (fun () ->
    let env = Store_scope.acquire resources
      (fun () -> Lmdb.Env.create Lmdb.Rw ~max_maps:3
        ~map_size:(64 * 1024 * 1024) ~flags:Lmdb.Env.Flags.no_tls dir)
      Lmdb.Env.close in
    let rows = Store_scope.acquire resources
      (fun () -> Lmdb.Map.create Lmdb.Map.Nodup ~key:Lmdb.Conv.string
        ~value:Lmdb.Conv.string ~name:"rows" env) Lmdb_handle.close in
    let times = Store_scope.acquire resources
      (fun () -> Lmdb.Map.create Lmdb.Map.Nodup ~key:Lmdb.Conv.string
        ~value:Lmdb.Conv.string ~name:"times" env) Lmdb_handle.close in
    let addresses = Store_scope.acquire resources
      (fun () -> Lmdb.Map.create Lmdb.Map.Dup ~key:Lmdb.Conv.string
        ~value:Lmdb.Conv.string ~name:"addresses" env) Lmdb_handle.close in
    List.iter (fun path ->
      let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
      Fun.protect ~finally:(fun () -> Unix.close fd)
        (fun () -> Unix.fsync fd)) [dir; data_dir];
    {env; resources; rows; times; addresses; max_rows; closed = false})

let require_open t =
  if t.closed then invalid_arg "local drop store is closed"

let copy bytes = Bytes.to_string (Bytes.of_string bytes)

let read_row t txn hash =
  let row = Lmdb.Map.get t.rows ~txn hash |> copy |> Drop_record.decode in
  if row.hash <> hash then invalid_arg "local drop hash mismatch";
  row

let remove t txn row =
  let key = Drop_record.order_key row in
  Lmdb.Map.remove t.rows ~txn row.hash;
  Lmdb.Map.remove t.times ~txn key;
  List.iter (fun addr -> Lmdb.Map.remove t.addresses ~txn ~value:key addr)
    (Drop_record.addresses row)

let trim t txn =
  let rec loop count =
    if count > t.max_rows then begin
      let hash = Lmdb.Cursor.go Lmdb.Rw ~txn t.times
        (fun cursor -> snd (Lmdb.Cursor.first cursor) |> copy) in
      remove t txn (read_row t txn hash);
      loop (count - 1)
    end in
  loop (Lmdb.Map.stat t.rows ~txn).entries

let save_many t rows =
  try
    require_open t;
    let selected = Drop_record.newest ~limit:t.max_rows rows in
    let encoded = List.map (fun row -> row, Drop_record.encode row) selected in
    if encoded = [] then Ok ()
    else match Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
      List.iter (fun (row, bytes) ->
        let old = try Some (read_row t txn row.hash) with Not_found -> None in
        Option.iter (remove t txn) old;
        let key = Drop_record.order_key row in
        Lmdb.Map.set t.rows ~txn row.hash bytes;
        Lmdb.Map.set t.times ~txn key row.hash;
        List.iter (fun addr -> Lmdb.Map.add t.addresses ~txn addr key)
          (Drop_record.addresses row)) encoded;
      trim t txn) with
    | Some () -> Ok ()
    | None -> Error "local drop write aborted"
  with exn -> Error (Printexc.to_string exn)

let find t hash =
  require_open t;
  match Lmdb.Txn.go Lmdb.Ro t.env (fun txn ->
    try Some (read_row t txn hash) with Not_found -> None) with
  | Some row -> row
  | None -> failwith "local drop read aborted"

let by_addr t addr ~limit ~offset =
  require_open t;
  if limit <= 0 || addr = "" then []
  else match Lmdb.Txn.go Lmdb.Ro t.env (fun txn ->
    Lmdb.Cursor.go Lmdb.Ro ~txn t.addresses (fun cursor ->
      let start = try
        ignore (Lmdb.Cursor.seek cursor addr);
        Some (Lmdb.Cursor.last_dup cursor |> copy)
      with Not_found -> None in
      let rec collect key skip left rows =
        match key with
        | None -> List.rev rows
        | Some _ when left = 0 -> List.rev rows
        | Some key ->
          let next () = try Some (Lmdb.Cursor.prev_dup cursor |> copy)
            with Not_found -> None in
          if skip > 0 then collect (next ()) (skip - 1) left rows
          else begin
            let hash = try Lmdb.Map.get t.times ~txn key |> copy
              with Not_found -> failwith "local drop time index is incomplete" in
            let row = try read_row t txn hash
              with Not_found -> failwith "local drop address index is incomplete" in
            collect (next ()) 0 (left - 1) (row :: rows)
          end in
      collect start (max 0 offset) (min limit t.max_rows) [])) with
  | Some rows -> rows
  | None -> failwith "local drop address read aborted"

let close t =
  if not t.closed then begin
    t.closed <- true;
    Store_scope.close t.resources
  end