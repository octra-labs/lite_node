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


type deploy = {
  address : string;
  code_hash : string;
  bytecode_b64 : string;
  owner : string;
  ctype : string;
  storage : (string, string) Hashtbl.t;
}

type snapshot
type t

val create : unit -> t
val snapshot : t -> snapshot
val restore : t -> snapshot -> unit
val discard : t -> unit
val add_deploy : t -> deploy -> unit
val find_deploy : t -> string -> deploy option
val has_deploy : t -> string -> bool
val load_storage : t -> string -> (string, string) Hashtbl.t option
val checkout_storage :
  t ->
  string ->
  fallback:(unit -> (string, string) Hashtbl.t) ->
  (string, string) Hashtbl.t
val deploys : t -> deploy list
val storage_entries : t -> (string * (string, string) Hashtbl.t) list