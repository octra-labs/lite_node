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


module Epoch_exec = Octra_core.Epoch_exec

type meta = {
  emission_remaining : Z.t;
  prev_supply : Z.t;
}

type trace = {
  replay : bool;
  layera : bool;
}

type validator_context = {
  active : string list;
  count : int;
  sha : string;
}

type node_deps = {
  get_meta : string -> string option;
  apply_footer : Epoch_exec.env -> Epoch_exec.reward_plan -> unit Lwt.t;
  log : string -> unit;
}

type node_request = {
  epoch_id : int;
  epoch_ts : float;
  proposer_addr : string;
  validator_pubkeys : (string * string) list;
  active_validators : string list;
  ready_state_root_at : int -> string option Lwt.t;
  ready_max_lag : int;
  validator_count : int;
  confirmed_fees : Z.t;
  short : string -> string;
}

type node_result = {
  meta : meta;
  plan : Epoch_exec.reward_plan;
  reward_recipients : Octra_core.Epochlog.reward_recipient list;
}

val z_of_meta : string option -> Z.t
val meta : emission_remaining:string option -> total_supply:string option -> meta
val reward_plan :
  validator_count:int ->
  confirmed_fees:Z.t ->
  meta ->
  Epoch_exec.reward_plan
val trace : env:(string -> string option) -> trace
val validators_sha :
  hash:(string -> string -> string) ->
  raw_to_hex:(string -> string) ->
  string list ->
  string
val validator_context :
  hash:(string -> string -> string) ->
  raw_to_hex:(string -> string) ->
  (string * string) list ->
  validator_context
val replay_proposer_line :
  epoch_id:int ->
  proposer_source:string ->
  proposer:string ->
  validators_sha:string ->
  string
val emit_replay_proposer :
  trace ->
  emit:(string -> unit) ->
  epoch_id:int ->
  proposer_source:string ->
  proposer:string ->
  validators_sha:string ->
  unit
val reward_line :
  fees:Z.t ->
  proposer:string ->
  short:(string -> string) ->
  validator_count:int ->
  Epoch_exec.reward_plan ->
  string
val env :
  epoch_id:int ->
  epoch_ts:float ->
  proposer_addr:string ->
  validator_pubkeys:(string * string) list ->
  ready_state_root_at:(int -> string option Lwt.t) ->
  ready_max_lag:int ->
  Epoch_exec.env

val run_node :
  node_deps ->
  node_request ->
  node_result Lwt.t