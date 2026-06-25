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