(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let restore ledger store savepoint =
  match
    Ledger.abort_journal ledger,
    Store_irmin.restore_batch store savepoint
  with
  | Ok (), Ok () -> ()
  | Error error, _
  | _, Error error -> failwith error

let run ?checkpoint ~ledger ~store apply =
  let ( let* ) = Exec_resource.bind in
  let rewind = match checkpoint with None -> Fun.id | Some save -> save () in
  match Ledger.begin_journal ledger with
  | Error error -> Lwt.fail_with error
  | Ok () ->
    begin
      match Store_irmin.save_batch store with
      | Error error ->
        ignore (Ledger.abort_journal ledger);
        Lwt.fail_with error
      | Ok savepoint ->
        let rollback () =
          match restore ledger store savepoint with
          | () -> rewind ()
          | exception error -> rewind (); raise error
        in
        Lwt.try_bind
          (fun () ->
            Exec_resource.protect (fun () ->
              let* result = apply () in
              Lwt.return result) ())
          (fun result ->
            match result with
            | Error _ -> rollback (); Lwt.return result
            | Ok _ ->
              match Ledger.commit_journal ledger with
              | Ok () -> Lwt.return result
              | Error error -> rollback (); Lwt.fail_with error)
          (fun exn ->
            rollback ();
            Lwt.fail exn)
    end