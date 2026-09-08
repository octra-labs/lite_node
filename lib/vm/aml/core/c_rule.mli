(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type field =
  | Nat
  | Str
  | Text
  | Ty_depth
  | Ty_nodes
  | Tm_depth
  | Tm_nodes
  | Inputs
  | Fuel
  | Name
  | Tag

type limits = private {
  nat : int;
  str : int;
  text : int;
  ty_depth : int;
  ty_nodes : int;
  tm_depth : int;
  tm_nodes : int;
  inputs : int;
  fuel : int;
  name : int;
  tag : int;
}

type id
type t
type schedule

type error =
  | Limit of field * int
  | Shape
  | Version of int
  | Epoch of Z.t
  | Absent of Z.t
  | Unsupported of id
  | Dup_id of id
  | Dup_epoch of Z.t
  | Count of int * int

val limits :
  nat:int -> str:int -> text:int -> ty_depth:int -> ty_nodes:int ->
  tm_depth:int -> tm_nodes:int -> inputs:int -> fuel:int -> name:int ->
  tag:int -> (limits, error) result

val local : limits
val rule : version:int -> activate:Z.t -> limits -> (t, error) result
val schedule : t list -> (schedule, error) result
val at : schedule -> epoch:Z.t -> (t option, error) result
val select : schedule -> epoch:Z.t -> (t, error) result

val id : t -> id
val version : id -> int
val id_limits : id -> limits
val activate : t -> Z.t
val rule_limits : t -> limits
val same_id : id -> id -> bool
val same_limits : limits -> limits -> bool
val supported : t -> bool
val id_values : id -> int list
val id_text : id -> string
val text : error -> string