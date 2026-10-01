(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Case = Recovery_case
module Delta = Octra_core.Aux_delta
module Aux = Octra_core.Aux_index

let previous = Delta.{epoch = 0; root = String.make 64 'a'; commit_id = "zero"}
let target = Delta.{epoch = 1; root = String.make 64 'b'; commit_id = "one"}
let body epoch = Printf.sprintf "{\"epoch\":%d,\"epoch_id\":%d}" epoch epoch
let reference epoch hash = Octra_core.Chaindata_index.rejected_addr_ref ~epoch_id:epoch ~hash

let with_index dir action = Case.with_stores dir (fun chain _ ->
  let index = Case.SC.index chain in
  let aux = Aux.{env = index.env; meta = index.meta; receipts = index.receipts;
    rejected = index.rejected; addresses = index.rej_addr; epochs = index.rej_epoch} in
  action aux)

let transaction aux action =
  match Lmdb.Txn.go Lmdb.Rw aux.Aux.env action with
  | Some result -> result | None -> failwith "test transaction aborted"

let rows aux =
  let entries map =
    let values = ref [] in
    ignore (Lmdb.Txn.go Lmdb.Ro aux.Aux.env (fun txn ->
      Aux.each map txn (fun key value -> values := (key, value) :: !values)));
    List.rev !values in
  entries aux.receipts, entries aux.rejected, entries aux.addresses,
  entries aux.epochs, entries aux.meta

let writes = Delta.[
  Receipt "old", Value (Some (body 1)); Receipt "new", Value (Some (body 1));
  Rejected "repeat", Value (Some "intermediate"); Rejected "fresh", Value (Some (body 1));
  Address ("alice", "repeat"), Member false;
  Address ("alice", reference 1 "repeat"), Member true;
  Address ("bob", reference 1 "fresh"), Member true;
  Epoch (1, "repeat"), Member true; Epoch (1, "fresh"), Member true;
  Metadata "note", Value (Some "new");
  Rejected "repeat", Value (Some (body 1))
]

let prepare root name =
  let dir = Filename.concat root name in
  Unix.mkdir dir 0o700;
  ignore (Case.prepare dir);
  with_index dir (fun aux ->
    transaction aux (fun txn ->
      List.iter (Aux.write aux txn) Delta.[
        Receipt "old", Value (Some (body 0)); Receipt "untouched", Value (Some (body 0));
        Rejected "repeat", Value (Some (body 0)); Rejected "untouched", Value (Some (body 0));
        Address ("alice", "repeat"), Member true;
        Address ("alice", reference 0 "repeat"), Member true;
        Epoch (0, "repeat"), Member true; Metadata "note", Value (Some "old")
      ]);
    Lmdb.Env.sync aux.env);
  dir

let refused action = try action (); false with
  | Aux.Refused _ | Invalid_argument _ -> true
  | Failure reason when List.mem reason ["injected action error"; "injected successor error"] -> true
let check name value =
  Case.expect name value;
  Printf.printf "event = aux_database test = %s status = pass\n%!" name

let run_child dir point =
  flush_all ();
  match Unix.fork () with
  | 0 ->
    (try with_index dir (fun aux ->
      Aux.with_write aux ~previous:(Some previous) ~target ~writes (fun txn ->
        if point = "during" then begin
          Aux.write aux txn (Delta.Metadata "note", Delta.Value (Some "interrupted"));
          Unix.kill (Unix.getpid ()) Sys.sigkill
        end);
      if point = "after" then Unix.kill (Unix.getpid ()) Sys.sigkill);
      Unix._exit 0
    with _ -> Unix._exit 2)
  | pid -> Case.wait pid

let recover aux =
  Aux.inspect aux ~head:(Some previous);
  transaction aux (fun txn ->
    Aux.verify aux txn ~head:(Some previous);
    Aux.apply_restore aux txn ~head:(Some previous));
  Lmdb.Env.sync aux.Aux.env

let run root =
  List.iter (fun point ->
    let dir = prepare root point in
    let before = with_index dir rows in
    let status = run_child dir point in
    check (point ^ "_process")
      (status = if point = "normal" then Unix.WEXITED 0 else Unix.WSIGNALED Sys.sigkill);
    with_index dir (fun aux ->
      if point <> "during" then begin
        let bytes = rows aux in
        check (point ^ "_retire_refused") (refused (fun () ->
          transaction aux (fun txn -> Aux.retire aux txn ~head:(Some previous))));
        check (point ^ "_refusal_preserved") (rows aux = bytes);
        Aux.inspect aux ~head:(Some target)
      end;
      recover aux;
      check (point ^ "_restored") (rows aux = before);
      recover aux;
      check (point ^ "_repeat") (rows aux = before))) ["normal"; "during"; "after"];
  let dir = prepare root "next" in
  with_index dir (fun aux ->
    Aux.with_write aux ~previous:(Some previous) ~target ~writes (fun _ -> ());
    let pending = rows aux in
    check "pending_write_refused" (refused (fun () ->
      Aux.with_write aux ~previous:(Some previous) ~target ~writes (fun _ -> ())));
    check "pending_write_preserved" (rows aux = pending);
    transaction aux (fun txn -> Aux.apply_restore aux txn ~head:(Some target));
    check "published_aux_retained" (rows aux = pending);
    transaction aux (fun txn -> Aux.retire aux txn ~head:(Some target));
    let committed = rows aux in
    let successor = Delta.{epoch = 2; root = String.make 64 'c'; commit_id = "two"} in
    Aux.with_write aux ~previous:(Some target) ~target:successor
      ~writes:[Delta.Rejected "repeat", Delta.Value (Some (body 2))] (fun _ -> ());
    Aux.inspect aux ~head:(Some target);
    transaction aux (fun txn -> Aux.apply_restore aux txn ~head:(Some target));
    check "successor_restored" (rows aux = committed));
  let dir = prepare root "error" in
  with_index dir (fun aux ->
    let before = rows aux in
    check "transaction_error" (refused (fun () ->
      Aux.with_write aux ~previous:(Some previous) ~target ~writes (fun txn ->
        Aux.write aux txn (Delta.Metadata "note", Delta.Value (Some "interrupted"));
        failwith "injected action error")));
    check "error_preserved" (rows aux = before));
  let dir = prepare root "legacy" in
  with_index dir (fun aux ->
    transaction aux (fun txn -> List.iter (Aux.write aux txn) writes);
    let before = rows aux in
    check "legacy_suffix_refused" (refused (fun () -> recover aux));
    check "legacy_suffix_preserved" (rows aux = before));
  List.iter (fun mode ->
    let dir = prepare root mode in
    with_index dir (fun aux ->
      Aux.with_write aux ~previous:(Some previous) ~target ~writes (fun _ -> ());
      transaction aux (fun txn ->
        if mode = "bad_journal" then Lmdb.Map.set aux.meta ~txn Aux.pending_key "{}"
        else Aux.write aux txn (Delta.Rejected "repeat", Delta.Value (Some "changed")));
      let before = rows aux in
      check (mode ^ "_refused") (refused (fun () -> recover aux));
      check (mode ^ "_preserved") (rows aux = before))) ["bad_journal"; "changed_value"];
  let dir = prepare root "retire_write" in
  with_index dir (fun aux ->
    Aux.with_write aux ~previous:(Some previous) ~target ~writes (fun _ -> ());
    let before = rows aux in
    let successor = Delta.{epoch = 2; root = String.make 64 'c'; commit_id = "two"} in
    check "retire_write_error" (refused (fun () ->
      Aux.with_write aux ~previous:(Some target) ~target:successor
        ~writes:[Delta.Rejected "repeat", Delta.Value (Some (body 2))]
        (fun _ -> failwith "injected successor error")));
    check "retire_write_preserved" (rows aux = before);
    Aux.with_write aux ~previous:(Some target) ~target:successor
      ~writes:[Delta.Rejected "repeat", Delta.Value (Some (body 2))] (fun _ -> ());
    transaction aux (fun txn -> Aux.apply_restore aux txn ~head:(Some target));
    let receipts, rejected, addresses, epochs, meta = before in
    let expected = receipts, rejected, addresses, epochs,
      List.filter (fun (key, _) -> key <> Aux.pending_key) meta in
    check "retire_write_restore" (rows aux = expected));
  List.iter (fun (name, entry) ->
    let dir = prepare root name in
    with_index dir (fun aux ->
      transaction aux (fun txn -> Aux.write aux txn entry);
      let before = rows aux in
      check (name ^ "_refused") (refused (fun () -> Aux.inspect aux ~head:(Some previous)));
      check (name ^ "_preserved") (rows aux = before))) Delta.[
        "address_suffix", (Address ("alice", reference 1 "repeat"), Member true);
        "epoch_suffix", (Epoch (1, "repeat"), Member true);
        "receipt_fields", (Receipt "old", Value (Some "{\"epoch\":0,\"epoch\":1}"));
        "receipt_unproven", (Receipt "old", Value (Some "{}"))
      ];
  let dir = prepare root "prior_ref" in
  with_index dir (fun aux ->
    let key = Delta.Address ("alice", reference 1 "repeat") in
    transaction aux (fun txn -> Aux.write aux txn (key, Delta.Member true));
    Aux.with_write aux ~previous:(Some previous) ~target ~writes:[key, Delta.Member false] (fun _ -> ());
    let before = rows aux in
    check "prior_reference_refused" (refused (fun () -> recover aux));
    check "prior_reference_preserved" (rows aux = before));
  let dir = prepare root "published_changed" in
  with_index dir (fun aux ->
    Aux.with_write aux ~previous:(Some previous) ~target ~writes (fun _ -> ());
    transaction aux (fun txn ->
      Aux.write aux txn (Delta.Rejected "repeat", Delta.Value (Some (body 0))));
    let before = rows aux in
    check "published_changed_refused"
      (refused (fun () -> Aux.inspect aux ~head:(Some target)));
    check "published_retire_refused"
      (refused (fun () -> transaction aux (fun txn -> Aux.retire aux txn ~head:(Some target))));
    check "published_changed_preserved" (rows aux = before))

let () = Test_workspace.with_dir "aux_index" run