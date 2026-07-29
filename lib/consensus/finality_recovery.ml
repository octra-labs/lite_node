(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type action =
  | Clean
  | Pending of Finality_log.entry
  | Gap of Finality_log.entry

let decide ~head = function
  | None -> Clean
  | Some e ->
    match Finality_log.decide ~head e.Finality_log.height with
    | Finality_log.Already_applied -> Clean
    | Finality_log.Apply_now -> Pending e
    | Finality_log.Need_catchup -> Gap e

let reason ~head = function
  | Clean -> "finality_clean"
  | Pending e ->
    Printf.sprintf "finality_log_pending height = %d head = %d"
      e.Finality_log.height head
  | Gap e ->
    Printf.sprintf "finality_log_gap height = %d head = %d"
      e.Finality_log.height head

let height = function
  | Clean -> None
  | Pending e | Gap e -> Some e.Finality_log.height