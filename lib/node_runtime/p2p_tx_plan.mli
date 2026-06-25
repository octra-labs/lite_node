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
  | Request of string list
  | Serve of string list
  | Receive of {
      hash : string;
      tx_json : string;
    }
  | Legacy

type tx_check =
  | Decoded of Octra_core.Transaction.t
  | Decode_error of string
  | Hash_mismatch of {
      advertised : string;
      actual : string;
    }

val plan_inv :
  ?cfg:Octra_net.P2p_tx_gossip_guard.cfg ->
  has:(string -> bool) ->
  string list ->
  string list

val plan_get :
  ?cfg:Octra_net.P2p_tx_gossip_guard.cfg ->
  has:(string -> bool) ->
  string list ->
  string list

val plan :
  ?cfg:Octra_net.P2p_tx_gossip_guard.cfg ->
  has:(string -> bool) ->
  string ->
  action

val check_tx :
  hash:string ->
  tx_json:string ->
  tx_check