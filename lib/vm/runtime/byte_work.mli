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
val decoding : length:int -> cells:int -> bits:int -> cached:bool -> decoding option

val limits :
  key_bytes:int -> value_bytes:int -> copy_bytes:int -> write_bytes:int ->
  alloc_bytes:int -> unit_bytes:int -> limits option

val create : limits -> t
val remaining : t -> int
val available : t -> int
val rules : t -> limits
val text_limit : t -> int

val plan :
  limits -> remaining:int -> available:int -> used:int -> limit:int -> base:int ->
  request list -> (int * int * int) option

val charge :
  t -> used:int -> limit:int -> base:int -> request list -> int option