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
  epoch : int64;
  root : string;
  declared_count : int32;
  producer : string;
  txs : Octra_core.Transaction.t list;
}

type observer_event =
  | Silent
  | Ignored of {
      epoch : int;
      tx_count : int;
      root : string;
      producer : string;
    }

val tx_count : t -> int

val root_short : string -> string

val producer_short : t -> string

val parse : string -> t

val observer_event :
  observer:bool ->
  string ->
  (observer_event, string) result