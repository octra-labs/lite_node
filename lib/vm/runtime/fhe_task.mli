(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type proof = Zero | Range | Claim of string
type key = Octra_core.Fhe_image.key
type cipher = Octra_core.Fhe_image.cipher

type request =
  | Load_key of string
  | Read_key of string
  | Write_key of key
  | Write_secret of string
  | Read_cipher of bool * bool * string
  | Write_cipher of cipher
  | Add of key * cipher * cipher
  | Sub of key * cipher * cipher
  | Mul of bool * bool * key * cipher * cipher * string
  | Scale of bool * key * cipher * int64
  | Divide of key * cipher * int64
  | Add_int of bool * key * cipher * int64
  | Sub_int of bool * key * cipher * int64
  | Commit of key * cipher
  | Pack of proof * key * cipher * string
  | Verify of bool * Octra_core.Pvac_verify_protocol.request

type value =
  | Key of key
  | Cipher of cipher
  | Text of string
  | Check of Octra_core.Pvac_verify_protocol.request
  | Verified of bool
type error = Invalid | Resource of Octra_core.Exec_resource.resource
type actor
type local

val eval : request -> (value, error) result
val local : unit -> local
val direct : local -> request -> (value, error) result
val key_effort : request -> int option
val create : ?clock:(unit -> int64) ->
  ?reap:(Octra_core.Pvac_verify_worker.session -> unit Lwt.t) -> unit -> actor
val run : ?actor:actor -> ?urgent:bool -> ticket:Proof_wait.ticket -> deadline:int64 -> request ->
  (value, error) result Lwt.t
val stop : actor -> unit
val stats : ?actor:actor -> unit -> bool * int