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


type effect =
  | Neutral
  | Public_encrypt of Z.t
  | Public_decrypt of Z.t
  | Hidden_commitment of string * int * string
  | Hidden_flow of string
  | Poisoned_flow of string

type audit_class =
  | Public_clean
  | Hidden_witness
  | Poisoned

type decision = {
  audit_class : audit_class;
  can_public_migrate : bool;
  public_net : Z.t option;
  commitment_net : string option;
  blockers : string list;
  effects : effect list;
  reason : string;
}

val classify_tx : addr:string -> Yojson.Safe.t -> effect

val string_of_audit_class : audit_class -> string

val replay_history : addr:string -> Yojson.Safe.t list -> decision