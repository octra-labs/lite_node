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


type kind =
  | Int
  | Bool
  | String
  | Bytes
  | Bytes32
  | U64
  | U128
  | U256
  | Addr
  | Cipher
  | PubKey
  | Unknown

type entry = {
  target : int;
  mem : (int * kind) list;
  effects : string list;
}

type call = {
  owner : int;
  pc : int;
  target : int;
  kind : kind;
}

type capability =
  | View
  | Storage_read
  | Storage_write
  | Transfer
  | Deploy
  | Fhe

type xcall = {
  pc : int;
  method_name : string;
  inputs : kind list;
  output : kind;
  capabilities : capability list;
}

type facts = {
  root : (int * kind) list;
  entries : entry list;
  calls : call list;
  xcalls : xcall list;
}

type error

val empty_facts : facts
val kind_name : kind -> string
val kind_of_name : string -> kind option
val capability_name : capability -> string
val capability_of_name : string -> capability option
val facts_hash : facts -> string
val check : ?facts:facts -> Contract_vm.instr array -> (unit, error) result
val error_message : error -> string