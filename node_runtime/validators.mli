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


val entries_of_env_name : string -> string list

val pubkeys_of_entries : string list -> (string * string) list

val pubkeys_of_env_name_in_order : string -> (string * string) list

val raw32_of_base64 : string -> string option

val raw_pubkey_of_entry : string -> string option

val raw_pubkeys_of_entries : string list -> string list

val pubkeys_of_env_name : string -> (string * string) list

val pubkeys_of_env :
  wallet_addr:string ->
  wallet_pub:string ->
  (string * string) list

val addrs_of_env :
  wallet_addr:string ->
  wallet_pub:string ->
  string list

val pubkeys_for_epoch :
  wallet_addr:string ->
  wallet_pub:string ->
  epoch:int ->
  (string * string) list

val addrs_for_epoch :
  wallet_addr:string ->
  wallet_pub:string ->
  epoch:int ->
  string list

val activation_epoch_int64 : unit -> int64 option