(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type row = {
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
  db : Sqlite3.db;
  put : Sqlite3.stmt;
  get : Sqlite3.stmt;
  by_addr : Sqlite3.stmt;
  trim : Sqlite3.stmt;
  max_rows : int;
}

let exec db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | code ->
    failwith
      (Printf.sprintf
         "tx_drop sql failed code = %s reason = %s"
         (Sqlite3.Rc.to_string code)
         (Sqlite3.errmsg db))

let ensure_dir path =
  if Sys.file_exists path then begin
    if (Unix.stat path).Unix.st_kind <> Unix.S_DIR then
      invalid_arg "tx_drop path is not a directory"
  end else
    Unix.mkdir path 0o700

let open_db ?(max_rows = 10_000) data_dir =
  if max_rows < 1 then invalid_arg "tx_drop max_rows must be positive";
  let dir = Filename.concat data_dir "drop_db" in
  ensure_dir dir;
  let db = Sqlite3.db_open (Filename.concat dir "events.sqlite") in
  exec db "PRAGMA journal_mode=WAL";
  exec db "PRAGMA synchronous=NORMAL";
  exec db "PRAGMA busy_timeout=5000";
  exec db
    "CREATE TABLE IF NOT EXISTS dropped(\
     hash TEXT PRIMARY KEY,\
     from_addr TEXT NOT NULL,\
     to_addr TEXT NOT NULL,\
     nonce INTEGER NOT NULL,\
     ou TEXT NOT NULL,\
     op_type TEXT NOT NULL,\
     reason TEXT NOT NULL,\
     detail TEXT NOT NULL,\
     dropped_at REAL NOT NULL)";
  exec db
    "CREATE INDEX IF NOT EXISTS dropped_time ON dropped(dropped_at,hash)";
  exec db
    "CREATE INDEX IF NOT EXISTS dropped_from ON dropped(from_addr,dropped_at,hash)";
  exec db
    "CREATE INDEX IF NOT EXISTS dropped_to ON dropped(to_addr,dropped_at,hash)";
  {
    db;
    put = Sqlite3.prepare db
      "INSERT OR REPLACE INTO dropped(\
       hash,from_addr,to_addr,nonce,ou,op_type,reason,detail,dropped_at)\
       VALUES(?,?,?,?,?,?,?,?,?)";
    get = Sqlite3.prepare db
      "SELECT from_addr,to_addr,nonce,ou,op_type,reason,detail,dropped_at \
       FROM dropped WHERE hash=?";
    by_addr = Sqlite3.prepare db
      "SELECT hash,from_addr,to_addr,nonce,ou,op_type,reason,detail,dropped_at \
       FROM dropped WHERE from_addr=? OR to_addr=? \
       ORDER BY dropped_at DESC,hash DESC LIMIT ? OFFSET ?";
    trim = Sqlite3.prepare db
      "DELETE FROM dropped WHERE hash IN (\
       SELECT hash FROM dropped ORDER BY dropped_at DESC,hash DESC \
       LIMIT -1 OFFSET ?)";
    max_rows;
  }

let bind_row statement row =
  ignore (Sqlite3.reset statement);
  ignore (Sqlite3.clear_bindings statement);
  ignore (Sqlite3.bind_text statement 1 row.hash);
  ignore (Sqlite3.bind_text statement 2 row.from_addr);
  ignore (Sqlite3.bind_text statement 3 row.to_addr);
  ignore (Sqlite3.bind_int statement 4 row.nonce);
  ignore (Sqlite3.bind_text statement 5 (Z.to_string row.ou));
  ignore
    (Sqlite3.bind_text
       statement
       6
       (Transaction.op_type_to_string row.op_type));
  ignore (Sqlite3.bind_text statement 7 row.reason);
  ignore (Sqlite3.bind_text statement 8 row.detail);
  ignore (Sqlite3.bind_double statement 9 row.dropped_at)

let rec skip count rows =
  if count <= 0 then rows
  else
    match rows with
    | [] -> []
    | _ :: rest -> skip (count - 1) rest

let bounded_rows max_rows rows =
  let ordered =
    List.sort
      (fun left right ->
         let time_order = compare left.dropped_at right.dropped_at in
         if time_order <> 0 then time_order
         else String.compare left.hash right.hash)
      rows
  in
  skip (max 0 (List.length ordered - max_rows)) ordered

let save_many t rows =
  let rows = bounded_rows t.max_rows rows in
  if rows = [] then Ok ()
  else
    try
      exec t.db "BEGIN IMMEDIATE TRANSACTION";
      List.iter
        (fun row ->
           bind_row t.put row;
           match Sqlite3.step t.put with
           | Sqlite3.Rc.DONE -> ()
           | code ->
             failwith
               (Printf.sprintf
                  "tx_drop insert failed code = %s"
                  (Sqlite3.Rc.to_string code)))
        rows;
      ignore (Sqlite3.reset t.trim);
      ignore (Sqlite3.clear_bindings t.trim);
      ignore (Sqlite3.bind_int t.trim 1 t.max_rows);
      begin
        match Sqlite3.step t.trim with
        | Sqlite3.Rc.DONE -> ()
        | code ->
          failwith
            (Printf.sprintf
               "tx_drop trim failed code = %s"
               (Sqlite3.Rc.to_string code))
      end;
      exec t.db "COMMIT";
      Ok ()
    with exn ->
      ignore (Sqlite3.exec t.db "ROLLBACK");
      Error (Printexc.to_string exn)

let row_at statement ~hash ~offset =
  let text index =
    Sqlite3.column statement (offset + index)
    |> Sqlite3.Data.to_string_exn
  in
  match Transaction.op_type_of_string (text 4) with
  | Error _ -> None
  | Ok op_type ->
    Some {
      hash;
      from_addr = text 0;
      to_addr = text 1;
      nonce =
        Sqlite3.column statement (offset + 2)
        |> Sqlite3.Data.to_int_exn;
      ou = Z.of_string (text 3);
      op_type;
      reason = text 5;
      detail = text 6;
      dropped_at =
        Sqlite3.column statement (offset + 7)
        |> Sqlite3.Data.to_float_exn;
    }

let find t hash =
  try
    ignore (Sqlite3.reset t.get);
    ignore (Sqlite3.clear_bindings t.get);
    ignore (Sqlite3.bind_text t.get 1 hash);
    Fun.protect
      ~finally:(fun () ->
        ignore (Sqlite3.reset t.get);
        ignore (Sqlite3.clear_bindings t.get))
      (fun () ->
         match Sqlite3.step t.get with
         | Sqlite3.Rc.ROW ->
           row_at t.get ~hash ~offset:0
         | Sqlite3.Rc.DONE -> None
         | _ -> None)
  with _ -> None

let by_addr t addr ~limit ~offset =
  if limit <= 0 then []
  else
    try
      ignore (Sqlite3.reset t.by_addr);
      ignore (Sqlite3.clear_bindings t.by_addr);
      ignore (Sqlite3.bind_text t.by_addr 1 addr);
      ignore (Sqlite3.bind_text t.by_addr 2 addr);
      ignore (Sqlite3.bind_int t.by_addr 3 limit);
      ignore (Sqlite3.bind_int t.by_addr 4 (max 0 offset));
      Fun.protect
        ~finally:(fun () ->
          ignore (Sqlite3.reset t.by_addr);
          ignore (Sqlite3.clear_bindings t.by_addr))
        (fun () ->
          let rec read rows =
            match Sqlite3.step t.by_addr with
            | Sqlite3.Rc.ROW ->
              let hash =
                Sqlite3.column t.by_addr 0
                |> Sqlite3.Data.to_string_exn
              in
              begin
                match row_at t.by_addr ~hash ~offset:1 with
                | Some row -> read (row :: rows)
                | None -> read rows
              end
            | Sqlite3.Rc.DONE -> List.rev rows
            | _ -> []
          in
          read [])
    with _ -> []

let close t =
  List.iter
    (fun statement -> ignore (Sqlite3.finalize statement))
    [t.put; t.get; t.by_addr; t.trim];
  ignore (Sqlite3.db_close t.db)