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

let run ~ledger ~store apply =
  let open Lwt.Syntax in
  match Ledger.begin_journal ledger with
  | Error error -> Lwt.fail_with error
  | Ok () ->
    begin
      match Store_irmin.save_batch store with
      | Error error ->
        ignore (Ledger.abort_journal ledger);
        Lwt.fail_with error
      | Ok savepoint ->
        Lwt.catch
          (fun () ->
            let* result = apply () in
            begin
              match result with
              | Error _ ->
                restore ledger store savepoint;
                Lwt.return result
              | Ok _ ->
                begin
                  match Ledger.commit_journal ledger with
                  | Ok () -> Lwt.return result
                  | Error error -> Lwt.fail_with error
                end
            end)
          (fun exn ->
            restore ledger store savepoint;
            Lwt.fail exn)
    end