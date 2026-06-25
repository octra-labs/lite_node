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


type t = {
  db : Sqlite3.db;
  get_account_stmt : Sqlite3.stmt;
  set_account_stmt : Sqlite3.stmt;
  list_accounts_stmt : Sqlite3.stmt;
  get_epoch_stmt : Sqlite3.stmt;
  set_epoch_stmt : Sqlite3.stmt;
  list_epochs_stmt : Sqlite3.stmt;
  save_tx_stmt : Sqlite3.stmt;
  get_tx_by_hash_stmt : Sqlite3.stmt;
  get_txs_by_addr_stmt : Sqlite3.stmt;
  get_meta_stmt : Sqlite3.stmt;
  set_meta_stmt : Sqlite3.stmt;
  get_max_rowid_stmt : Sqlite3.stmt;
  get_encrypted_balance_stmt : Sqlite3.stmt;
  update_encrypted_balance_stmt : Sqlite3.stmt;
  save_encrypted_tx_stmt : Sqlite3.stmt;
  save_contract_abi_stmt : Sqlite3.stmt;
  save_rejected_tx_stmt : Sqlite3.stmt;
  get_rejected_tx_stmt : Sqlite3.stmt;
  get_rejected_txs_by_addr_stmt : Sqlite3.stmt;
  get_pvac_pubkey_stmt : Sqlite3.stmt;
  set_pvac_pubkey_stmt : Sqlite3.stmt;
  get_circle_balance_stmt : Sqlite3.stmt;
  update_circle_balance_stmt : Sqlite3.stmt;

  insert_stealth_output_stmt : Sqlite3.stmt;
  get_stealth_outputs_since_stmt : Sqlite3.stmt;
  get_stealth_output_by_id_stmt : Sqlite3.stmt;
  mark_stealth_claimed_stmt : Sqlite3.stmt;
  get_unclaimed_stealth_amount_stmt : Sqlite3.stmt;
  get_decrypt_allowance_stmt : Sqlite3.stmt;
  update_decrypt_allowance_stmt : Sqlite3.stmt;
}

let exec db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
    Octra_log.stdout "error [storage] sql err = %s msg = %s\n%!"
      (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)

let create_schema db =
  List.iter (exec db) [
    "PRAGMA journal_mode=WAL";
    "PRAGMA synchronous=NORMAL";
    "PRAGMA busy_timeout=5000";
    "PRAGMA cache_size=-2000000";
    "PRAGMA temp_store=MEMORY";
    "PRAGMA wal_autocheckpoint=1000";
    "PRAGMA journal_size_limit=536870912";

    "CREATE TABLE IF NOT EXISTS accounts(\
     address TEXT PRIMARY KEY,\
     balance TEXT NOT NULL,\
     nonce INTEGER NOT NULL DEFAULT 0,\
     public_key TEXT,\
     encrypted_balance TEXT DEFAULT '0')";

    "CREATE TABLE IF NOT EXISTS contracts(\
     address TEXT PRIMARY KEY,\
     code_hash TEXT NOT NULL,\
     version TEXT NOT NULL,\
     balance TEXT NOT NULL DEFAULT '0',\
     owner TEXT NOT NULL,\
     ctype TEXT NOT NULL,\
     bytecode TEXT NOT NULL)";

    "CREATE TABLE IF NOT EXISTS contract_storage(\
     contract_address TEXT,\
     key TEXT,\
     value TEXT,\
     PRIMARY KEY(contract_address,key))";

    "CREATE TABLE IF NOT EXISTS contract_abi(\
     address TEXT PRIMARY KEY,\
     abi_json TEXT NOT NULL)";

    "CREATE TABLE IF NOT EXISTS epochs(\
     id INTEGER PRIMARY KEY,\
     parent_commit TEXT NOT NULL,\
     tree_json TEXT NOT NULL,\
     finalized_by TEXT NOT NULL,\
     finalized_at REAL NOT NULL,\
     tx_count INTEGER NOT NULL DEFAULT 0)";

    "CREATE TABLE IF NOT EXISTS transactions(\
     hash TEXT PRIMARY KEY,\
     epoch_id INTEGER,\
     from_addr TEXT NOT NULL,\
     to_addr TEXT NOT NULL,\
     amount TEXT NOT NULL,\
     nonce INTEGER NOT NULL,\
     ou TEXT NOT NULL,\
     timestamp REAL NOT NULL,\
     signature TEXT NOT NULL,\
     public_key TEXT,\
     tx_json TEXT NOT NULL,\
     FOREIGN KEY(epoch_id) REFERENCES epochs(id))";

    "CREATE TABLE IF NOT EXISTS encrypted_transactions(\
     hash TEXT PRIMARY KEY,\
     epoch_id INTEGER,\
     from_addr TEXT NOT NULL,\
     to_addr TEXT NOT NULL,\
     encrypted_amount TEXT NOT NULL,\
     ephemeral_pubkey TEXT NOT NULL,\
     nonce INTEGER NOT NULL,\
     signature TEXT NOT NULL,\
     timestamp REAL NOT NULL,\
     tx_json TEXT NOT NULL,\
     FOREIGN KEY(epoch_id) REFERENCES epochs(id))";

    "CREATE TABLE IF NOT EXISTS pending_private_transfers(\
     id INTEGER PRIMARY KEY AUTOINCREMENT,\
     recipient TEXT NOT NULL,\
     sender TEXT NOT NULL,\
     amount TEXT NOT NULL,\
     ephemeral_key TEXT NOT NULL,\
     encrypted_data TEXT NOT NULL,\
     epoch_id INTEGER NOT NULL,\
     processed BOOLEAN DEFAULT 0,\
     created_at REAL DEFAULT(julianday('now')),\
     FOREIGN KEY(epoch_id) REFERENCES epochs(id))";

    "CREATE TABLE IF NOT EXISTS metadata(\
     key TEXT PRIMARY KEY,\
     value TEXT NOT NULL)";

    "CREATE TABLE IF NOT EXISTS key_images(\
     key_image TEXT PRIMARY KEY,\
     tx_hash TEXT NOT NULL,\
     epoch_id INTEGER NOT NULL,\
     created_at REAL DEFAULT(julianday('now')))";

    "CREATE TABLE IF NOT EXISTS rejected_txs(\
     hash TEXT PRIMARY KEY,\
     from_addr TEXT,\
     to_addr TEXT,\
     amount TEXT,\
     nonce INTEGER,\
     error_type TEXT NOT NULL,\
     reason TEXT NOT NULL,\
     epoch_id INTEGER,\
     ts REAL NOT NULL)";

    "CREATE INDEX IF NOT EXISTS idx_tx_from ON transactions(from_addr)";
    "CREATE INDEX IF NOT EXISTS idx_tx_to ON transactions(to_addr)";
    "CREATE INDEX IF NOT EXISTS idx_tx_epoch ON transactions(epoch_id)";
    "CREATE INDEX IF NOT EXISTS idx_enc_from ON encrypted_transactions(from_addr)";
    "CREATE INDEX IF NOT EXISTS idx_enc_to ON encrypted_transactions(to_addr)";
    "CREATE INDEX IF NOT EXISTS idx_rejected_ts ON rejected_txs(ts)";
    "CREATE INDEX IF NOT EXISTS idx_rejected_from ON rejected_txs(from_addr)";

    "CREATE TABLE IF NOT EXISTS stealth_outputs(\
     id INTEGER PRIMARY KEY AUTOINCREMENT,\
     stealth_tag TEXT NOT NULL,\
     eph_pub TEXT NOT NULL,\
     enc_amount TEXT NOT NULL,\
     amount TEXT NOT NULL,\
     epoch_id INTEGER NOT NULL,\
     tx_hash TEXT NOT NULL,\
     sender_addr TEXT NOT NULL,\
     claimed INTEGER DEFAULT 0,\
     claim_tx_hash TEXT,\
     created_at REAL DEFAULT(julianday('now')))";

    "CREATE INDEX IF NOT EXISTS idx_stealth_tag ON stealth_outputs(stealth_tag)";
    "CREATE INDEX IF NOT EXISTS idx_stealth_epoch ON stealth_outputs(epoch_id)";
  ];
  let migrate db sql =
    match Sqlite3.exec db sql with Sqlite3.Rc.OK -> () | _ -> ()
  in
  migrate db "ALTER TABLE rejected_txs ADD COLUMN error_type TEXT NOT NULL DEFAULT 'unknown'";
  migrate db "ALTER TABLE accounts ADD COLUMN pvac_pubkey BLOB";
  migrate db "ALTER TABLE accounts ADD COLUMN fhe_circle_balance TEXT DEFAULT '0'";
  migrate db "ALTER TABLE accounts ADD COLUMN view_pubkey TEXT";
  migrate db "ALTER TABLE accounts ADD COLUMN decrypt_allowance TEXT DEFAULT '0'";
  migrate db "ALTER TABLE stealth_outputs ADD COLUMN claim_pub TEXT";

  migrate db "ALTER TABLE stealth_outputs ADD COLUMN delta_cipher_stored TEXT DEFAULT ''";

  migrate db "ALTER TABLE stealth_outputs ADD COLUMN amount_hash TEXT DEFAULT ''";

  migrate db "ALTER TABLE stealth_outputs ADD COLUMN amount_commitment TEXT DEFAULT ''";

  migrate db "CREATE TABLE IF NOT EXISTS schema_meta(key TEXT PRIMARY KEY, value TEXT)";
  let purge_done = ref false in
  (let open Sqlite3 in
   let stmt = prepare db "SELECT value FROM schema_meta WHERE key='enc_purge_v43'" in
   (match step stmt with
    | Rc.ROW -> purge_done := true
    | _ -> ());
   ignore (finalize stmt));
  if not !purge_done then begin
    let n_purged = ref 0 in
    (match Sqlite3.exec db
       "UPDATE accounts SET encrypted_balance = '0', decrypt_allowance = '0' \
        WHERE encrypted_balance IS NOT NULL \
          AND encrypted_balance <> '0' \
          AND encrypted_balance <> ''"
     with
     | Sqlite3.Rc.OK ->
       n_purged := Sqlite3.changes db;
       if !n_purged > 0 then
         Octra_log.stdout "info [migration] v4.2: purged ALL %d encrypted balance(s) + allowance → 0 (one-time reset)\n%!" !n_purged
       else
         Octra_log.stdout "info [migration] v4.2: no encrypted balances to purge\n%!"
     | rc ->
       Octra_log.stdout "warn [migration] v4.2 enc_balance purge returned %s\n%!"
         (Sqlite3.Rc.to_string rc));

    (match Sqlite3.exec db
       "UPDATE accounts SET decrypt_allowance = '0' \
        WHERE decrypt_allowance IS NOT NULL \
          AND decrypt_allowance <> '0' \
          AND decrypt_allowance <> ''"
     with
     | Sqlite3.Rc.OK ->
       let n_da = Sqlite3.changes db in
       if n_da > 0 then
         Octra_log.stdout "info [migration] v4.2: reset %d stale decrypt_allowance(s) → 0\n%!" n_da
     | _ -> ());

    (match Sqlite3.exec db
       "INSERT OR REPLACE INTO schema_meta(key,value) VALUES('enc_purge_v43','done')"
     with
     | Sqlite3.Rc.OK -> ()
     | rc ->
       Octra_log.stdout "warn [migration] failed to mark enc_purge_v43: %s\n%!"
         (Sqlite3.Rc.to_string rc))
  end

let prep db s = Sqlite3.prepare db s

let create path =
  let db = Sqlite3.db_open path in
  create_schema db;
  {
    db;
    get_account_stmt = prep db "SELECT balance,nonce,public_key,encrypted_balance FROM accounts WHERE address=?";
    set_account_stmt = prep db "INSERT INTO accounts(address,balance,nonce,public_key,encrypted_balance) VALUES(?,?,?,?,?) ON CONFLICT(address) DO UPDATE SET balance=excluded.balance, nonce=excluded.nonce, public_key=excluded.public_key, encrypted_balance=excluded.encrypted_balance";
    list_accounts_stmt = prep db "SELECT address,balance,nonce,public_key,encrypted_balance FROM accounts";
    get_epoch_stmt = prep db "SELECT tree_json FROM epochs WHERE id=?";
    set_epoch_stmt = prep db "INSERT OR REPLACE INTO epochs(id,parent_commit,tree_json,finalized_by,finalized_at,tx_count) VALUES(?,?,?,?,?,?)";
    list_epochs_stmt = prep db "SELECT id FROM epochs ORDER BY id DESC";
    save_tx_stmt = prep db "INSERT OR REPLACE INTO transactions(hash,epoch_id,from_addr,to_addr,amount,nonce,ou,timestamp,signature,public_key,tx_json) VALUES(?,?,?,?,?,?,?,?,?,?,?)";
    get_tx_by_hash_stmt = prep db "SELECT epoch_id,tx_json FROM transactions WHERE hash=?";
    get_txs_by_addr_stmt = prep db "SELECT hash,epoch_id,tx_json FROM transactions WHERE from_addr=? OR to_addr=? ORDER BY timestamp DESC LIMIT ?";
    get_meta_stmt = prep db "SELECT value FROM metadata WHERE key=?";
    set_meta_stmt = prep db "INSERT OR REPLACE INTO metadata(key,value) VALUES(?,?)";
    get_max_rowid_stmt = prep db "SELECT COUNT(*) FROM transactions";
    get_encrypted_balance_stmt = prep db "SELECT COALESCE(encrypted_balance,'0') FROM accounts WHERE address=?";
    update_encrypted_balance_stmt = prep db "UPDATE accounts SET encrypted_balance=? WHERE address=?";
    save_encrypted_tx_stmt = prep db "INSERT OR REPLACE INTO encrypted_transactions(hash,epoch_id,from_addr,to_addr,encrypted_amount,ephemeral_pubkey,nonce,signature,timestamp,tx_json) VALUES(?,?,?,?,?,?,?,?,?,?)";
    save_contract_abi_stmt = prep db "INSERT OR REPLACE INTO contract_abi(address,abi_json) VALUES(?,?)";
    save_rejected_tx_stmt = prep db "INSERT OR REPLACE INTO rejected_txs(hash,from_addr,to_addr,amount,nonce,error_type,reason,epoch_id,ts) VALUES(?,?,?,?,?,?,?,?,?)";
    get_rejected_tx_stmt = prep db "SELECT from_addr,to_addr,amount,nonce,error_type,reason,epoch_id,ts FROM rejected_txs WHERE hash=?";
    get_rejected_txs_by_addr_stmt = prep db "SELECT hash,epoch_id,ts FROM rejected_txs WHERE from_addr=? ORDER BY ts DESC LIMIT ?";
    get_pvac_pubkey_stmt = prep db "SELECT pvac_pubkey FROM accounts WHERE address=?";
    set_pvac_pubkey_stmt = prep db "UPDATE accounts SET pvac_pubkey=? WHERE address=?";
    get_circle_balance_stmt = prep db "SELECT COALESCE(fhe_circle_balance,'0') FROM accounts WHERE address=?";
    update_circle_balance_stmt = prep db "UPDATE accounts SET fhe_circle_balance=? WHERE address=?";

    insert_stealth_output_stmt = prep db "INSERT INTO stealth_outputs(stealth_tag,eph_pub,enc_amount,amount,epoch_id,tx_hash,sender_addr,claim_pub,delta_cipher_stored,amount_hash,amount_commitment) VALUES(?,?,?,?,?,?,?,?,?,?,?)";
    get_stealth_outputs_since_stmt = prep db "SELECT id,stealth_tag,eph_pub,enc_amount,amount,epoch_id,tx_hash,sender_addr,claimed,claim_pub,COALESCE(delta_cipher_stored,''),COALESCE(amount_hash,''),COALESCE(amount_commitment,'') FROM stealth_outputs WHERE epoch_id>=? ORDER BY id ASC";
    get_stealth_output_by_id_stmt = prep db "SELECT id,stealth_tag,eph_pub,enc_amount,amount,epoch_id,tx_hash,sender_addr,claimed,claim_pub,COALESCE(delta_cipher_stored,''),COALESCE(amount_hash,''),COALESCE(amount_commitment,'') FROM stealth_outputs WHERE id=?";
    mark_stealth_claimed_stmt = prep db "UPDATE stealth_outputs SET claimed=1, claim_tx_hash=? WHERE id=? AND claimed=0";
    get_unclaimed_stealth_amount_stmt = prep db "SELECT COALESCE(SUM(CAST(amount AS INTEGER)),0) FROM stealth_outputs WHERE claimed=0";
    get_decrypt_allowance_stmt = prep db "SELECT COALESCE(decrypt_allowance,'0') FROM accounts WHERE address=?";
    update_decrypt_allowance_stmt = prep db "UPDATE accounts SET decrypt_allowance=? WHERE address=?";
  }

let exec_tx storage sql =
  match Sqlite3.exec storage.db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
    Octra_log.stdout "error [storage] %s failed rc = %s msg = %s\n%!"
      sql (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg storage.db)

let begin_transaction s = exec_tx s "BEGIN IMMEDIATE TRANSACTION"
let commit_transaction s = exec_tx s "COMMIT"
let rollback_transaction s = exec_tx s "ROLLBACK"

let get_meta s key =
  Sqlite3.reset s.get_meta_stmt |> ignore;
  Sqlite3.bind_text s.get_meta_stmt 1 key |> ignore;
  match Sqlite3.step s.get_meta_stmt with
  | Sqlite3.Rc.ROW -> Some (Sqlite3.Data.to_string_exn (Sqlite3.column s.get_meta_stmt 0))
  | _ -> None

let set_meta s key value =
  Sqlite3.reset s.set_meta_stmt |> ignore;
  Sqlite3.bind_text s.set_meta_stmt 1 key |> ignore;
  Sqlite3.bind_text s.set_meta_stmt 2 value |> ignore;
  Sqlite3.step s.set_meta_stmt |> ignore

let get_max_tx_rowid s =
  let count = ref 0 in
  (match Sqlite3.exec s.db "SELECT COUNT(*) FROM transactions"
     ~cb:(fun row _ -> count := int_of_string (Option.value row.(0) ~default:"0")) with
  | Sqlite3.Rc.OK -> () | _ -> ());
  !count

let all_stmts s = [
  s.get_account_stmt; s.set_account_stmt; s.list_accounts_stmt;
  s.get_epoch_stmt; s.set_epoch_stmt; s.list_epochs_stmt;
  s.save_tx_stmt; s.get_tx_by_hash_stmt; s.get_txs_by_addr_stmt;
  s.get_meta_stmt; s.set_meta_stmt; s.get_max_rowid_stmt;
  s.get_encrypted_balance_stmt; s.update_encrypted_balance_stmt;
  s.save_encrypted_tx_stmt; s.save_contract_abi_stmt;
  s.save_rejected_tx_stmt; s.get_rejected_tx_stmt; s.get_rejected_txs_by_addr_stmt;
  s.get_pvac_pubkey_stmt; s.set_pvac_pubkey_stmt;
  s.get_circle_balance_stmt; s.update_circle_balance_stmt;
  s.insert_stealth_output_stmt; s.get_stealth_outputs_since_stmt;
  s.get_stealth_output_by_id_stmt; s.mark_stealth_claimed_stmt;
  s.get_unclaimed_stealth_amount_stmt;
]

let close s =
  List.iter (fun stmt -> Sqlite3.finalize stmt |> ignore) (all_stmts s);
  Sqlite3.db_close s.db |> ignore

let reset_all_stmts s =
  List.iter (fun stmt -> Sqlite3.reset stmt |> ignore) (all_stmts s)

let try_checkpoint s mode =
  let busy = ref false in
  let wal_pages = ref 0 in
  let done_pages = ref 0 in
  ignore (Sqlite3.exec s.db (Printf.sprintf "PRAGMA wal_checkpoint(%s)" mode)
    ~cb:(fun row _ ->
      (try busy := int_of_string (Option.value row.(0) ~default:"0") <> 0 with _ -> ());
      (try wal_pages := int_of_string (Option.value row.(1) ~default:"0") with _ -> ());
      (try done_pages := int_of_string (Option.value row.(2) ~default:"0") with _ -> ())));
  (!busy, !wal_pages, !done_pages)

let wal_checkpoint s =
  reset_all_stmts s;
  let rec try_truncate attempt =
    if attempt > 3 then begin
      let (_, wp, dp) = try_checkpoint s "PASSIVE" in
      if wp > 0 then
        Octra_log.stdout "warn [wal] TRUNCATE failed 3x, PASSIVE: %d/%d pages\n%!" dp wp
    end else
      let (busy, wp, dp) = try_checkpoint s "TRUNCATE" in
      if busy then begin
        Octra_log.stdout "warn [wal] TRUNCATE busy (attempt %d/3, wal=%d done=%d)\n%!" attempt wp dp;
        Unix.sleepf 0.05;
        reset_all_stmts s;
        try_truncate (attempt + 1)
      end else if wp > 0 && dp < wp then begin
        Octra_log.stdout "warn [wal] TRUNCATE partial: %d/%d pages, retrying...\n%!" dp wp;
        try_truncate (attempt + 1)
      end
  in
  try_truncate 1