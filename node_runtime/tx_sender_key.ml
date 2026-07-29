(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let resolve ~find_account tx =
  match find_account tx.Octra_core.Transaction.from with
  | Some account ->
    begin
      match account.Octra_core.Ledger.public_key with
      | Some public_key -> Some public_key
      | None -> tx.Octra_core.Transaction.public_key
    end
  | None -> tx.Octra_core.Transaction.public_key