(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type account = Ledger_types.account = {
  balance : Z.t;
  nonce : int;
  public_key : string option;
  encrypted_balance : string option;
  decrypt_allowance : Z.t;
}

type epoch_journal = {
  accounts : (string, account option) Hashtbl.t;
  dirty_addrs : (string, unit) Hashtbl.t;
  dirty : bool;
  total_supply : Z.t;
  active_accounts : int;
  spent_nonces : (string * int, unit) Hashtbl.t;
}

type t = {
  store : Store_irmin.t;
  cache : (string, account) Hashtbl.t;
  dirty_addrs : (string, unit) Hashtbl.t;
  mutable dirty : bool;
  mutable total_supply : Z.t;
  mutable active_accounts : int;
  spent_nonces : (string * int, unit) Hashtbl.t;
  mutable epoch_journals : epoch_journal list;
}

type snap = {
  items : (string, account) Hashtbl.t;
  supply : Z.t;
  active : int;
}

type transfer_error =
  | Debit_rejected of string
  | Credit_rejected of string

let run_s p =
  match Lwt.state p with
  | Lwt.Return v -> v
  | Lwt.Fail e -> raise e
  | Lwt.Sleep ->
    let r = ref None in
    Lwt.on_any p (fun v -> r := Some (Ok v)) (fun e -> r := Some (Error e));
    let rec pump n =
      if n > 10000000 then failwith "run_s: irmin I/O timeout"
      else match !r with
      | Some (Ok v) -> v
      | Some (Error e) -> raise e
      | None -> ignore (Lwt_engine.iter false); pump (n + 1)
    in pump 0

let units_per_oct = Denomination.units_per_oct
let format_balance = Denomination.format_balance
let clear_spent_nonces l = Hashtbl.reset l.spent_nonces

let begin_journal l =
  l.epoch_journals <- {
    accounts = Hashtbl.create 64;
    dirty_addrs = Hashtbl.copy l.dirty_addrs;
    dirty = l.dirty;
    total_supply = l.total_supply;
    active_accounts = l.active_accounts;
    spent_nonces = Hashtbl.copy l.spent_nonces;
  } :: l.epoch_journals;
  Ok ()

let commit_journal l =
  match l.epoch_journals with
  | [] -> Error "ledger journal is not active"
  | _ :: rest ->
    l.epoch_journals <- rest;
    Ok ()

let restore_table target source =
  Hashtbl.reset target;
  Hashtbl.iter (Hashtbl.replace target) source

let abort_journal l =
  match l.epoch_journals with
  | [] -> Ok ()
  | journal :: rest ->
    Hashtbl.iter
      (fun addr account ->
        match account with
        | Some account -> Hashtbl.replace l.cache addr account
        | None -> Hashtbl.remove l.cache addr)
      journal.accounts;
    restore_table l.dirty_addrs journal.dirty_addrs;
    restore_table l.spent_nonces journal.spent_nonces;
    l.dirty <- journal.dirty;
    l.total_supply <- journal.total_supply;
    l.active_accounts <- journal.active_accounts;
    l.epoch_journals <- rest;
    Ok ()

let journal_active l =
  l.epoch_journals <> []

let record_account l addr =
  List.iter
    (fun journal ->
      if not (Hashtbl.mem journal.accounts addr) then
        Hashtbl.add journal.accounts addr (Hashtbl.find_opt l.cache addr))
    l.epoch_journals

let empty_account = Ledger_types.empty_account

let account_to_yojson = Ledger_types.account_to_yojson

let compute_total_supply store =
  run_s (Store_irmin.sum_balances store)

let is_valid_cipher = function
  | None -> true
  | Some "" -> true
  | Some s -> String.length s >= 7 && String.sub s 0 7 = "hfhe_v1"

let is_active_account acc =
  Z.gt acc.Ledger_types.balance Z.zero

let create_from_store store =
  let accounts = run_s (Store_irmin.load_all_accounts store) in
  let cache = Hashtbl.create (max 100 (List.length accounts)) in
  let dirty_addrs = Hashtbl.create 64 in
  let total_supply = ref Z.zero in
  let active_accounts = ref 0 in
  let sanitized = ref 0 in
  List.iter (fun (addr, acc) ->
    let acc =
      if is_valid_cipher acc.Ledger_types.encrypted_balance then acc
      else begin
        incr sanitized;
        Hashtbl.replace dirty_addrs addr ();
        { acc with encrypted_balance = None }
      end in
    Hashtbl.replace cache addr acc;
    total_supply := Z.add !total_supply acc.Ledger_types.balance;
    if is_active_account acc then incr active_accounts
  ) accounts;
  if !sanitized > 0 then
    Octra_log.warn "ledger"
      "event = legacy_cipher_sanitized count = %d persistence = pending"
      !sanitized;
  { store; cache; dirty_addrs; dirty = !sanitized > 0; total_supply = !total_supply;
    active_accounts = !active_accounts;
    spent_nonces = Hashtbl.create 1000;
    epoch_journals = [] }

let create store =
  create_from_store store

let freeze source =
  if source.dirty || Hashtbl.length source.dirty_addrs > 0 then
    Error "ledger clone requires flushed accounts"
  else if journal_active source then
    Error "ledger clone requires no active journal"
  else
    Ok {
      items = Hashtbl.copy source.cache;
      supply = source.total_supply;
      active = source.active_accounts;
    }

let thaw store snap =
  {
      store;
      cache = Hashtbl.copy snap.items;
      dirty_addrs = Hashtbl.create 64;
      dirty = false;
      total_supply = snap.supply;
      active_accounts = snap.active;
      spent_nonces = Hashtbl.create 1000;
      epoch_journals = [];
    }

let clone_clean store source =
  Result.map (thaw store) (freeze source)

let get_account_internal l addr =
  match Hashtbl.find_opt l.cache addr with
  | Some a -> Some a
  | None ->
    match run_s (Store_irmin.get_account l.store addr) with
    | Some a ->
      Hashtbl.add l.cache addr a; Some a
    | None -> None

let set_account_internal l addr a =
  record_account l addr;
  let was_active =
    match Hashtbl.find_opt l.cache addr with
    | Some old -> is_active_account old
    | None -> false
  in
  let is_active = is_active_account a in
  if was_active && not is_active then
    l.active_accounts <- max 0 (l.active_accounts - 1)
  else if not was_active && is_active then
    l.active_accounts <- l.active_accounts + 1;
  Hashtbl.replace l.cache addr a;
  Hashtbl.replace l.dirty_addrs addr ();
  l.dirty <- true

let supply_check l amount =
  if Z.gt (Z.add l.total_supply amount) Denomination.max_supply then (
    Octra_log.error "supply"
      "event = cap_rejected current = %s delta = %s"
      (Z.to_string l.total_supply) (Z.to_string amount);
    false)
  else true

let add_account l addr amount =
  if not (supply_check l amount) then
    Error "supply violation: would exceed 1B hard cap"
  else (
    set_account_internal l addr { empty_account with balance = amount };
    l.total_supply <- Z.add l.total_supply amount;
    Ok ())

let add_account_with_pubkey l addr amount pk =
  if not (supply_check l amount) then
    Error "supply violation: would exceed 1B hard cap"
  else (
    set_account_internal l addr { empty_account with balance = amount; public_key = Some pk };
    l.total_supply <- Z.add l.total_supply amount;
    Ok ())

let register_public_key l addr pk =
  match get_account_internal l addr with
  | None -> ()
  | Some a -> set_account_internal l addr { a with public_key = Some pk }

let credit ?encrypted_balance l addr amount =
  if Z.sign amount < 0 then Error "negative credit amount"
  else if not (supply_check l amount) then
    Error "supply violation: would exceed 1B hard cap"
  else
    let a = match get_account_internal l addr with Some x -> x | None -> empty_account in
    let eb = match encrypted_balance with None -> a.encrypted_balance | s -> s in
    set_account_internal l addr { a with balance = Z.add a.balance amount; encrypted_balance = eb };
    l.total_supply <- Z.add l.total_supply amount;
    Ok ()

let debit ?encrypted_balance l addr amount nonce =
  if Z.sign amount < 0 then Error "negative debit amount"
  else match get_account_internal l addr with
  | None -> Error "Sender not found"
  | Some a ->
    if Z.lt a.balance amount then Error "Insufficient balance"
    else if nonce <> a.nonce + 1 then Error "Invalid nonce"
    else if Hashtbl.mem l.spent_nonces (addr, nonce) then Error "Nonce already spent"
    else
      let eb = match encrypted_balance with None -> a.encrypted_balance | s -> s in
      set_account_internal l addr { a with balance = Z.sub a.balance amount; nonce; encrypted_balance = eb };
      l.total_supply <- Z.sub l.total_supply amount;
      Hashtbl.add l.spent_nonces (addr, nonce) (); Ok ()

let transfer l ~from ~to_ ~amount ~fee nonce =
  let total_cost = Z.add amount fee in
  if Z.sign total_cost < 0 then Error (Debit_rejected "negative debit amount")
  else
    match get_account_internal l from with
    | None -> Error (Debit_rejected "Sender not found")
    | Some sender ->
      if Z.lt sender.balance total_cost then
        Error (Debit_rejected "Insufficient balance")
      else if nonce <> sender.nonce + 1 then
        Error (Debit_rejected "Invalid nonce")
      else if Hashtbl.mem l.spent_nonces (from, nonce) then
        Error (Debit_rejected "Nonce already spent")
      else if Z.sign amount < 0 then
        Error (Credit_rejected "negative credit amount")
      else if Z.sign fee < 0 then
        Error (Credit_rejected "negative fee amount")
      else
        let sender = {
          sender with
          balance = Z.sub sender.balance total_cost;
          nonce;
        } in
        if String.equal from to_ then
          set_account_internal l from {
            sender with balance = Z.add sender.balance amount;
          }
        else begin
          let receiver =
            match get_account_internal l to_ with
            | Some account -> account
            | None -> empty_account
          in
          set_account_internal l from sender;
          set_account_internal l to_ {
            receiver with balance = Z.add receiver.balance amount;
          }
        end;
        l.total_supply <- Z.sub l.total_supply fee;
        Hashtbl.add l.spent_nonces (from, nonce) ();
        Ok ()

let debit_amount_only l addr amount =
  if Z.sign amount < 0 then Error "negative debit_amount_only"
  else match get_account_internal l addr with
  | None -> Error "Account not found"
  | Some a ->
    if Z.lt a.balance amount then Error "Insufficient balance"
    else (
      set_account_internal l addr { a with balance = Z.sub a.balance amount };
      l.total_supply <- Z.sub l.total_supply amount;
      Ok ())

let update_decrypt_allowance l addr new_val =
  (match get_account_internal l addr with
   | Some a -> set_account_internal l addr { a with decrypt_allowance = new_val }
   | None -> ())

let update_enc_balance l addr cipher =
  (match get_account_internal l addr with
   | Some a -> set_account_internal l addr { a with encrypted_balance = Some cipher }
   | None -> ());
  Ok ()

let create_private_transfer _l ~from_addr:_ ~to_addr:_ ~amount:_ ~from_priv:_ =
  Error "PrivateOp V1 is disabled — use StealthOp V5"

let collect_rows _st _ncols = []

let fhe_encrypt_balance l addr amt priv_b64 ~tx_hash ~epoch_id =
  match get_account_internal l addr with
  | None -> Error "Account missing"
  | Some a when Z.lt a.balance amt -> Error "Not enough public balance"
  | Some a ->
    let (pk, sk) = Crypto.FheBalance.derive_pvac_keys priv_b64 in
    let current = Option.value ~default:"0" a.encrypted_balance in
    (match Crypto.FheBalance.deposit pk sk ~current_cipher:(Some current) ~amount:amt ~tx_hash ~epoch_id with
     | Error e -> Error e
     | Ok new_enc ->
       match debit_amount_only l addr amt with
       | Error e -> Error e
       | Ok () -> update_enc_balance l addr new_enc)

let fhe_decrypt_balance l addr amt priv_b64 ~tx_hash ~epoch_id =
  match get_account_internal l addr with
  | None -> Error "Account missing"
  | Some a ->
    let (pk, sk) = Crypto.FheBalance.derive_pvac_keys priv_b64 in
    let current = Option.value ~default:"0" a.encrypted_balance in
    (match Crypto.FheBalance.get_balance pk sk current with
     | Error e -> Error e
     | Ok enc_bal when Z.lt enc_bal amt -> Error "Encrypted balance too small"
     | Ok _enc_bal ->
       (match Crypto.FheBalance.withdraw pk sk ~current_cipher:(Some current) ~amount:amt ~tx_hash ~epoch_id with
        | Error e -> Error e
        | Ok new_enc ->
          match credit l addr amt with
          | Error e -> Error e
          | Ok () -> update_enc_balance l addr new_enc))

let set_pvac_pubkey l addr pk_blob =
  let exists = Hashtbl.mem l.cache addr || get_account_internal l addr <> None in
  if not exists then
    ignore (add_account l addr Z.zero);
  let open Lwt.Syntax in
  let hash = Pvac_registry.full_key_hash pk_blob in
  let* bound = Store_irmin.get_pvac_hash l.store addr in
  if bound = Some hash then
    Lwt.return_unit
  else
    Store_irmin.set_pvac_pubkey l.store addr pk_blob

let get_pvac_pubkey l addr =
  Store_irmin.get_pvac_pubkey l.store addr

let get_pvac_key_hash l addr =
  Store_irmin.get_pvac_hash l.store addr

let pvac_key_is_bound l addr =
  Store_irmin.pvac_is_bound l.store addr

let get_pvac_kat l addr =
  Store_irmin.get_pvac_kat l.store addr

let set_pvac_kat l addr kat_hex =
  Store_irmin.set_pvac_kat l.store addr kat_hex

let clear_encrypted_balance l addr =
  match get_account_internal l addr with
  | Some a -> set_account_internal l addr { a with encrypted_balance = None }
  | None -> ()

let delete_pvac_pubkey l addr =
  Store_irmin.delete_pvac_pubkey l.store addr

let op01_burn_authority = "oct7xCozDD9JEsbeVpo5C7HXp2BJbKqfmNUHmDDCCTtWcGb"

let op01_burn_meta_key = "op01_2026_burn_executed"

let is_op01_burn_executed l =
  match run_s (Store_irmin.get_meta l.store op01_burn_meta_key) with
  | Some "true" -> true
  | _ -> false

let mark_op01_burn_executed l =
  run_s (Store_irmin.set_meta l.store op01_burn_meta_key "true")

let apply_op01_burn l ~from ~to_ amount nonce =
  if from <> op01_burn_authority then
    Error "op01_burn: restricted to mining pool authority"
  else if from <> to_ then
    Error "op01_burn: must be self-transaction (from equals to)"
  else if Z.sign amount <= 0 then
    Error "op01_burn: amount must be positive"
  else if is_op01_burn_executed l then
    Error "op01_burn: already executed (one-shot ceremony)"
  else
    match debit l from amount nonce with
    | Error e -> Error e
    | Ok () ->
      mark_op01_burn_executed l;
      Ok ()

let fhe_encrypt_with_cipher l addr amt ~delta_cipher_str =
  match get_account_internal l addr with
  | None -> Lwt.return (Error "Account missing")
  | Some a when Z.lt a.balance amt -> Lwt.return (Error "Not enough public balance")
  | Some a ->
    let open Lwt.Syntax in
    let* pk_opt = get_pvac_pubkey l addr in
    match pk_opt with
    | None -> Lwt.return (Error "No PVAC pubkey registered")
    | Some pk_blob ->
      (match Crypto.FheBalance.load_pubkey_result pk_blob with
       | Error e -> Lwt.return (Error e)
       | Ok pk ->
         let current = Option.value ~default:"0" a.encrypted_balance in
         (match Crypto.FheBalance.decode_cipher delta_cipher_str with
          | Error e -> Lwt.return (Error ("bad delta cipher: " ^ e))
          | Ok delta_ct ->
            (match Crypto.FheBalance.deposit_with_pubkey pk ~current_cipher:(Some current) ~delta_cipher:delta_ct with
             | Error e -> Lwt.return (Error e)
             | Ok new_enc ->
               match debit_amount_only l addr amt with
               | Error e -> Lwt.return (Error e)
               | Ok () -> Lwt.return (update_enc_balance l addr new_enc))))

let fhe_get_encrypted_balance l addr priv_b64 =
  if not (Crypto.WalletKey.verify_privkey_for_address addr priv_b64) then Error "Bad privkey"
  else
    let c = match get_account_internal l addr with
      | Some a -> Option.value ~default:"0" a.encrypted_balance
      | None -> "0"
    in
    if c = "0" || c = "" then Ok Z.zero
    else if Crypto.FheBalance.is_fhe_cipher c then
      let (pk, sk) = Crypto.FheBalance.derive_pvac_keys priv_b64 in
      Crypto.FheBalance.get_balance pk sk c
    else
      Error "Legacy encrypted balance is disabled"

let hash_with_contracts l =
  Store_irmin.state_hash l.store

let hash = hash_with_contracts

let flush l =
  if l.dirty then l.dirty <- false

let flush_dirty_lwt l =
  if Hashtbl.length l.dirty_addrs = 0 then Lwt.return_unit
  else
    let open Lwt.Infix in
    let addrs =
      Hashtbl.fold (fun a () acc -> a :: acc) l.dirty_addrs []
      |> List.sort String.compare in
    Hashtbl.clear l.dirty_addrs;
    l.dirty <- false;
    Lwt_list.iter_s (fun addr ->
      Lwt.pause () >>= fun () ->
      match Hashtbl.find_opt l.cache addr with
      | Some a -> Store_irmin.set_account l.store addr a
      | None -> Lwt.return_unit
    ) addrs

let mem l a = Hashtbl.mem l.cache a || get_account_internal l a <> None
let find l a = match get_account_internal l a with Some x -> x | None -> raise Not_found
let find_opt = get_account_internal
let fold f l init = Hashtbl.fold f l.cache init
let length l = Hashtbl.length l.cache
let active_count l = l.active_accounts

let get_public_key l addr =
  get_account_internal l addr |> Option.map (fun a -> a.public_key) |> Option.join

let get_total_supply l = l.total_supply
let get_max_supply () = Denomination.max_supply

let compute_unclaimed_stealth store =
  Store_irmin.get_unclaimed_stealth_amount store

let audit_supply l =
  let cache_supply = Hashtbl.fold (fun _ acc sum -> Z.add sum acc.Ledger_types.balance) l.cache Z.zero in
  let open Lwt.Syntax in
  let* pending_stealth = compute_unclaimed_stealth l.store in
  let total_accounted = Z.add cache_supply pending_stealth in
  let drift = Z.sub l.total_supply cache_supply in
  if not (Z.equal drift Z.zero) then
    Octra_log.error "supply"
      "event = audit_drift tracked = %s cache = %s pending_stealth = %s drift = %s"
      (Z.to_string l.total_supply) (Z.to_string cache_supply)
      (Z.to_string pending_stealth) (Z.to_string drift);
  l.total_supply <- cache_supply;
  Lwt.return (total_accounted, Z.leq total_accounted Denomination.max_supply)

let get_circle_balance l addr =
  Store_irmin.get_circle_balance l.store addr

let update_circle_balance l addr cipher =
  Store_irmin.set_circle_balance l.store addr cipher

let create_stealth_output l ~stealth_tag ~eph_pub ~enc_amount ~amount ~epoch_id ~tx_hash ~sender_addr ~claim_pub ?(delta_cipher_stored="") ?(amount_hash="") ?(amount_commitment="") () =
  Store_irmin.insert_stealth_output l.store
    ~stealth_tag ~eph_pub ~enc_amount ~amount:(Z.to_string amount)
    ~epoch_id ~tx_hash ~sender_addr ~claim_pub
    ~delta_cipher_stored ~amount_hash ~amount_commitment

let get_stealth_outputs_since l from_epoch =
  Store_irmin.get_stealth_outputs_since l.store from_epoch

let get_stealth_output_by_id l output_id =
  Store_irmin.get_stealth_output_by_id l.store output_id

let mark_stealth_claimed l output_id claim_tx_hash =
  Store_irmin.mark_stealth_claimed l.store output_id claim_tx_hash

let get_unclaimed_stealth_amount l =
  Store_irmin.get_unclaimed_stealth_amount l.store

module Overlay = struct
  type overlay = {
    base : t;
    accounts : (string, account) Hashtbl.t;
    deleted : (string, unit) Hashtbl.t;
    mutable supply_delta : Z.t;
    pending_nonces : (string * int, unit) Hashtbl.t;
    mutable promoted : bool;
    mutable dropped : bool;
  }

  let create base = {
    base;
    accounts = Hashtbl.create 32;
    deleted = Hashtbl.create 8;
    supply_delta = Z.zero;
    pending_nonces = Hashtbl.create 32;
    promoted = false;
    dropped = false;
  }

  let assert_active o =
    if o.promoted then failwith "Overlay: already promoted"
    else if o.dropped then failwith "Overlay: already dropped"

  let read o addr =
    assert_active o;
    if Hashtbl.mem o.deleted addr then None
    else match Hashtbl.find_opt o.accounts addr with
    | Some a -> Some a
    | None -> get_account_internal o.base addr

  let write o addr a =
    assert_active o;
    Hashtbl.replace o.accounts addr a;
    Hashtbl.remove o.deleted addr

  let get_account = read

  let total_supply o =
    assert_active o;
    Z.add o.base.total_supply o.supply_delta

  let supply_check o amount =
    Z.leq (Z.add (total_supply o) amount) Denomination.max_supply

  let mem o addr =
    assert_active o;
    if Hashtbl.mem o.deleted addr then false
    else Hashtbl.mem o.accounts addr || mem o.base addr

  let find_opt = read
  let find o a =
    match read o a with Some x -> x | None -> raise Not_found

  let get_public_key o addr =
    read o addr |> Option.map (fun a -> a.public_key) |> Option.join

  let add_account o addr amount =
    if not (supply_check o amount) then
      Error "supply violation: would exceed 1B hard cap"
    else (
      write o addr { empty_account with balance = amount };
      o.supply_delta <- Z.add o.supply_delta amount;
      Ok ())

  let add_account_with_pubkey o addr amount pk =
    if not (supply_check o amount) then
      Error "supply violation: would exceed 1B hard cap"
    else (
      write o addr { empty_account with balance = amount; public_key = Some pk };
      o.supply_delta <- Z.add o.supply_delta amount;
      Ok ())

  let register_public_key o addr pk =
    match read o addr with
    | None -> ()
    | Some a -> write o addr { a with public_key = Some pk }

  let credit ?encrypted_balance o addr amount =
    if Z.sign amount < 0 then Error "negative credit amount"
    else if not (supply_check o amount) then
      Error "supply violation: would exceed 1B hard cap"
    else
      let a = match read o addr with Some x -> x | None -> empty_account in
      let eb = match encrypted_balance with None -> a.encrypted_balance | s -> s in
      write o addr { a with balance = Z.add a.balance amount; encrypted_balance = eb };
      o.supply_delta <- Z.add o.supply_delta amount;
      Ok ()

  let nonce_already_used o addr nonce =
    Hashtbl.mem o.base.spent_nonces (addr, nonce)
    || Hashtbl.mem o.pending_nonces (addr, nonce)

  let debit ?encrypted_balance o addr amount nonce =
    if Z.sign amount < 0 then Error "negative debit amount"
    else match read o addr with
    | None -> Error "Sender not found"
    | Some a ->
      if Z.lt a.balance amount then Error "Insufficient balance"
      else if nonce <> a.nonce + 1 then Error "Invalid nonce"
      else if nonce_already_used o addr nonce then Error "Nonce already spent"
      else (
        let eb = match encrypted_balance with None -> a.encrypted_balance | s -> s in
        write o addr
          { a with balance = Z.sub a.balance amount; nonce; encrypted_balance = eb };
        o.supply_delta <- Z.sub o.supply_delta amount;
        Hashtbl.add o.pending_nonces (addr, nonce) ();
        Ok ())

  let debit_amount_only o addr amount =
    if Z.sign amount < 0 then Error "negative debit_amount_only"
    else match read o addr with
    | None -> Error "Account not found"
    | Some a ->
      if Z.lt a.balance amount then Error "Insufficient balance"
      else (
        write o addr { a with balance = Z.sub a.balance amount };
        o.supply_delta <- Z.sub o.supply_delta amount;
        Ok ())

  let update_decrypt_allowance o addr new_val =
    match read o addr with
    | Some a -> write o addr { a with decrypt_allowance = new_val }
    | None -> ()

  let update_enc_balance o addr cipher =
    (match read o addr with
     | Some a -> write o addr { a with encrypted_balance = Some cipher }
     | None -> ());
    Ok ()

  let clear_encrypted_balance o addr =
    match read o addr with
    | Some a -> write o addr { a with encrypted_balance = None }
    | None -> ()

  let apply_op01_burn o ~from ~to_ amount nonce =
    if not (String.equal from op01_burn_authority) then
      Error "op01_burn: restricted to mining pool authority"
    else if not (String.equal from to_) then
      Error "op01_burn: must be self-transaction (from equals to)"
    else if Z.sign amount <= 0 then
      Error "op01_burn: amount must be positive"
    else if is_op01_burn_executed o.base then
      Error "op01_burn: already executed (one-shot ceremony)"
    else
      match debit o from amount nonce with
      | Error error -> Error error
      | Ok () ->
        mark_op01_burn_executed o.base;
        Ok ()

  let length o =
    let base_count = Hashtbl.length o.base.cache in
    let new_only = Hashtbl.fold (fun addr _ acc ->
      if Hashtbl.mem o.base.cache addr then acc else acc + 1
    ) o.accounts 0 in
    let removed = Hashtbl.length o.deleted in
    base_count + new_only - removed

  let promote o =
    assert_active o;
    let sorted_promote_pairs =
      Hashtbl.fold (fun addr a acc -> (addr, a) :: acc) o.accounts []
      |> List.sort (fun (a, _) (b, _) -> String.compare a b) in
    let sorted_deleted_addrs =
      Hashtbl.fold (fun addr () acc -> addr :: acc) o.deleted []
      |> List.sort String.compare in
    let sorted_nonces =
      Hashtbl.fold (fun k () acc -> k :: acc) o.pending_nonces []
      |> List.sort (fun (a1, n1) (a2, n2) ->
          let c = String.compare a1 a2 in
          if c <> 0 then c else compare n1 n2) in
    List.iter (fun (addr, a) -> set_account_internal o.base addr a) sorted_promote_pairs;
    List.iter (fun addr ->
      record_account o.base addr;
      Hashtbl.remove o.base.cache addr;
      Hashtbl.replace o.base.dirty_addrs addr ()) sorted_deleted_addrs;
    o.base.total_supply <- Z.add o.base.total_supply o.supply_delta;
    List.iter (fun (addr, n) -> Hashtbl.replace o.base.spent_nonces (addr, n) ()) sorted_nonces;
    o.base.dirty <- o.base.dirty || Hashtbl.length o.accounts > 0 || Hashtbl.length o.deleted > 0;
    Hashtbl.clear o.accounts;
    Hashtbl.clear o.deleted;
    Hashtbl.clear o.pending_nonces;
    o.supply_delta <- Z.zero;
    o.promoted <- true

  let drop o =
    assert_active o;
    Hashtbl.clear o.accounts;
    Hashtbl.clear o.deleted;
    Hashtbl.clear o.pending_nonces;
    o.supply_delta <- Z.zero;
    o.dropped <- true

  let stats o =
    Printf.sprintf
      "Overlay{accounts=%d deleted=%d supply_delta=%s pending_nonces=%d promoted=%b dropped=%b}"
      (Hashtbl.length o.accounts) (Hashtbl.length o.deleted)
      (Z.to_string o.supply_delta) (Hashtbl.length o.pending_nonces)
      o.promoted o.dropped
end