(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type limits
type t
type request =
  | Write of int * int
  | Erase of int
  | Copy of int * int
  | Allocate of int
  | Scan of int

type decoding = private {max_bytes : int; requests : request list}

val parsing : length:int -> cells:int -> (int * request list) option
val encoded_size : int -> int option
val cell_bytes : count:int -> bits:int -> int option
val decoding : length:int -> cells:int -> bits:int -> cached:bool -> decoding option

val limits :
  key_bytes:int -> value_bytes:int -> copy_bytes:int -> write_bytes:int ->
  alloc_bytes:int -> unit_bytes:int -> limits option

val create : limits -> t
val consensus_limits : limits
val consensus_id : string
val remaining : t -> int
val available : t -> int
val event_left : t -> int
val text_size : limit:int -> string -> int option
val output : t -> used:int -> limit:int -> int -> int option
val event_size :
  limit:int -> address:string -> depth:int -> name:string -> values:string list -> int option
val rules : t -> limits
val text_limit : t -> int

val plan :
  limits -> remaining:int -> available:int -> used:int -> limit:int -> base:int ->
  request list -> (int * int * int) option

val charge :
  t -> used:int -> limit:int -> base:int -> request list -> int option

val emit : t -> used:int -> limit:int -> int -> int option