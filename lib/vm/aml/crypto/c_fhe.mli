(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type op =
  | Mul of C_nat.t
  | Recrypt of C_nat.t

type enc = private {
  key : C_nat.t;
  rem : C_nat.t;
}

type cfg
type profile
type param = private {
  id : C_nat.t;
  pkey : C_nat.t;
  cap : C_nat.t;
}
type catalog
type input
type env
type t

type node =
  | Nvar of C_syn.name
  | Ntrim of Z.t * t
  | Nadd of t * t
  | Nmul of t * t
  | Nre of t

type host = private
  | Harg of C_syn.name * enc
  | Htrim of C_nat.t * host * enc
  | Hadd of host * host * enc
  | Hmul of host * host * enc
  | Hre of host * enc

type ins = private
  | Load of C_syn.name * enc
  | Narrow of C_nat.t * enc
  | Add_cipher of enc
  | Mul_cipher of enc
  | Recrypt_cipher of enc

type info = private {
  typ : enc;
  steps : C_nat.t;
  work : C_nat.t;
  peak : C_nat.t;
  plan : op list;
}

type field =
  | Key
  | Depth
  | Add_work
  | Mul_work
  | Re_work
  | Param_id
  | Param_cap

type axis =
  | Steps
  | Work

type error =
  | Nat of field * Z.t
  | Zero of field
  | Pdup of C_nat.t
  | Pcount of int * int
  | Cdup_id of C_nat.t
  | Cdup_cap of C_nat.t * C_nat.t
  | Ccount of int * int
  | No_param of C_nat.t * C_nat.t
  | No_key of C_nat.t
  | Idup of string
  | Icount of int * int
  | Idepth of C_nat.t * C_nat.t * C_nat.t
  | Free of string
  | Need of enc * enc
  | Mul_zero of C_nat.t
  | Re_need of C_nat.t * C_nat.t
  | Depth_max of int * int
  | Nodes of int * int
  | Stat of axis * Z.t

val cfg :
  depth:Z.t -> add:Z.t -> mul:Z.t -> recrypt:Z.t -> (cfg, error) result

val profile : (Z.t * cfg) list -> (profile, error) result
val full : profile -> C_nat.t -> (C_nat.t, error) result
val param : id:Z.t -> key:Z.t -> cap:Z.t -> (param, error) result
val catalog : param list -> (catalog, error) result
val select : catalog -> key:C_nat.t -> need:C_nat.t -> (param, error) result
val params : catalog -> info -> (param, error) result
val input : C_syn.name -> key:Z.t -> rem:Z.t -> input
val env : profile -> input list -> (env, error) result

val var : C_syn.name -> t
val trim : Z.t -> t -> t
val add : t -> t -> t
val mul : t -> t -> t
val recrypt : t -> t
val node : t -> node
val host : profile -> env -> t -> (host, error) result
val host_type : host -> enc
val host_term : host -> t
val host_check : profile -> env -> host -> bool
val emit : host -> ins list
val emit_check : profile -> env -> host -> bool

val check : profile -> env -> t -> (info, error) result
val enc_text : enc -> string
val param_text : param -> string
val op_text : op -> string
val text : error -> string