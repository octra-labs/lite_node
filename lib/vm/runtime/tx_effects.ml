(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type status =
  | Open
  | Committed
  | Dropped

type t = {
  store : Octra_core.Store_irmin.t;
  overlay : Octra_core.Ledger.Overlay.overlay;
  value : Value_journal.t;
  program : Program_journal.t;
  savepoint : Octra_core.Store_irmin.batch_savepoint;
  mutable status : status;
}

exception Commit_failed of string

let require_open tx =
  match tx.status with
  | Open -> ()
  | Committed -> raise (Commit_failed "transaction effects already committed")
  | Dropped -> raise (Commit_failed "transaction effects already dropped")

let overlay_balance overlay address =
  match Octra_core.Ledger.Overlay.find_opt overlay address with
  | Some account -> account.Octra_core.Ledger.balance
  | None -> Z.zero

let create ~ledger ~store =
  let savepoint =
    match Octra_core.Store_irmin.save_batch store with
    | Ok savepoint -> savepoint
    | Error error -> raise (Commit_failed error)
  in
  let overlay = Octra_core.Ledger.Overlay.create ledger in
  {
    store;
    overlay;
    value = Value_journal.create ~balance:(overlay_balance overlay);
    program = Program_journal.create ();
    savepoint;
    status = Open;
  }

let value tx = tx.value
let program tx = tx.program
let balance tx = Value_journal.effective tx.value

let debit tx address amount nonce =
  require_open tx;
  Octra_core.Ledger.Overlay.debit tx.overlay address amount nonce

let apply tx effect =
  require_open tx;
  Value_journal.apply tx.value effect

let ensure_account tx address =
  require_open tx;
  if Octra_core.Ledger.Overlay.mem tx.overlay address then Ok ()
  else Octra_core.Ledger.Overlay.add_account tx.overlay address Z.zero

let rollback tx =
  let restored = Octra_core.Store_irmin.restore_batch tx.store tx.savepoint in
  Value_journal.discard tx.value;
  Program_journal.discard tx.program;
  Octra_core.Ledger.Overlay.drop tx.overlay;
  tx.status <- Dropped;
  restored

let fail tx error =
  match rollback tx with
  | Ok () -> Error error
  | Error restore_error ->
    Error (Printf.sprintf "%s; rollback failed: %s" error restore_error)

let commit tx =
  require_open tx;
  try
    Program_store.stage tx.store tx.program;
    Octra_core.Chaos.inject "vm_commit_after_program";
    match
      Value_journal.commit tx.value
        ~debit:(Octra_core.Ledger.Overlay.debit_amount_only tx.overlay)
        ~credit:(Octra_core.Ledger.Overlay.credit tx.overlay)
    with
    | Error error -> fail tx ("value commit rejected: " ^ error)
    | Ok () ->
      Octra_core.Chaos.inject "vm_commit_after_value";
      Program_journal.discard tx.program;
      Octra_core.Ledger.Overlay.promote tx.overlay;
      tx.status <- Committed;
      Ok ()
  with
  | Commit_failed _ as error -> raise error
  | error -> fail tx (Printexc.to_string error)

let commit_exn tx =
  match commit tx with
  | Ok () -> ()
  | Error error -> raise (Commit_failed error)

let discard tx =
  match tx.status with
  | Open ->
    begin
      match rollback tx with
      | Ok () -> ()
      | Error error -> raise (Commit_failed error)
    end
  | Committed | Dropped -> ()