(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val work_limit : int
val map_limit : int -> int

type scope = { sequence : int64; request : string; body : string }

val open_db :
  ?max_sessions:int ->
  ?caller_limit:int ->
  chain_id:string ->
  node_id:string ->
  string -> t

val reserve :
  ?scope:string ->
  ?room:int ->
  t ->
  caller:string ->
  session:string ->
  sequence:int64 ->
  request:string ->
  (bool, string) result

val scope : t -> caller:string -> session:string -> (scope option, string) result
val find_scope :
  t -> caller:string -> session:string -> (scope -> bool) -> (scope option, string) result

val ensure : t -> caller:string -> session:string -> (bool, string) result

val cancel : ?expires:int64 -> t -> caller:string -> session:string -> (bool, string) result

val cancelled : t -> caller:string -> session:string -> (bool, string) result

val cancel_deadline : t -> caller:string -> session:string -> (int64 option, string) result
val claim_relay : t -> now:int64 -> (bool, string) result
val send_limit : int
val transit_limit : int
val claim_transit : t -> now:int64 -> (bool, string) result
val claim_send : ?local:bool -> t -> now:int64 -> (bool, string) result
val forward_limit : int

type notice = {
  caller : string;
  session : string;
  expires : int64;
  round : int;
  body : string;
}

val max_notices : int
val caller_notices : int
val notice_rounds : int
val max_notice_bytes : int

val queue_cancel :
  ?until:int64 -> t -> epoch:int64 -> expires:int64 -> caller:string -> session:string -> body:string ->
  (bool, string) result

val pending_notices : t -> epoch:int64 -> (notice list, string) result
val claim_notice :
  ?target:(string * string) -> ?now:int64 -> ?rounds:bool ->
  t -> epoch:int64 -> (notice option, string) result

val max_cancel_acks : int
val confirm_cancel :
  t -> epoch:int64 -> expires:int64 -> round:int -> caller:string -> session:string ->
  cancel:string -> node:string -> body:string -> (bool, string) result
val cancel_acks :
  t -> epoch:int64 -> caller:string -> session:string ->
  ((notice * string list) option, string) result

val relay_cancel :
  ?until:int64 -> ?close:bool -> ?now:int64 -> ?transit:bool ->
  t -> epoch:int64 -> expires:int64 -> round:int -> origin:string -> id:string ->
  caller:string -> session:string -> (bool, string) result

val max_reply_bytes : int
val max_context_bytes : int

type saved = { reply : string; context : string option }

val finish :
  ?context:string ->
  t -> caller:string -> session:string -> sequence:int64 -> request:string -> reply:string ->
  (bool, string) result

val saved :
  t -> caller:string -> session:string -> sequence:int64 -> request:string ->
  (saved option, string) result

val reply :
  t -> caller:string -> session:string -> sequence:int64 -> request:string ->
  (string option, string) result

val max_job_bytes : int

type work = Planned of string | Funding of string | Issued of string

val plan_job :
  ?expires:int64 -> t -> caller:string -> session:string -> id:string -> body:string -> (bool, string) result

val work : t -> caller:string -> id:string -> (work option, string) result

val known_job : t -> caller:string -> session:string -> (bool, string) result

val prune_jobs :
  t -> expired:(caller:string -> session:string -> id:string -> string -> bool) ->
  (int, string) result

val prune_cancels : t -> epoch:int64 -> (int, string) result

val claim_fetch :
  t -> caller:string -> session:string -> id:string -> limit:int -> (int option, string) result

val advance_job :
  ?scope:scope ->
  ?room:int ->
  t -> caller:string -> session:string -> id:string -> before:work -> after:work ->
  (bool, string) result

val wait_job :
  scope:scope -> room:int -> t -> caller:string -> session:string -> id:string ->
  before:string -> body:string -> (bool, string) result

val fund_job :
  t -> epoch:int64 -> state_root:string -> id:string -> body:string ->
  Compute_funding.funded -> (bool, string) result

val save_job :
  t -> caller:string -> session:string -> id:string -> body:string -> (bool, string) result

val load_job : t -> caller:string -> id:string -> (string option, string) result

val close : t -> unit