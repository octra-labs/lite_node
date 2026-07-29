(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  addr : string;
  pub_b64 : string;
  sig_b64 : string;
}

type gate =
  | Any
  | Owner
  | Private_owner
  | Owner_private
  | Storage_owner

val message :
  op:string ->
  circle_id:string ->
  addr:string ->
  subject:string ->
  string

val params_hash :
  Yojson.Safe.t ->
  string

val view_subject :
  method_name:string ->
  call_params:Yojson.Safe.t list ->
  include_storage:bool ->
  string

val asset_subject :
  kind:string ->
  subject:string ->
  string

val owner_required :
  string

val storage_owner_required :
  string

val private_view_required :
  Octra_core.Circles.circle_info ->
  bool

val owner_private_required :
  Octra_core.Circles.circle_info ->
  bool

val public_info_allowed :
  Octra_core.Circles.circle_info ->
  bool

val owner :
  Octra_core.Circles.circle_info ->
  string ->
  (unit, string) result

val private_owner :
  Octra_core.Circles.circle_info ->
  string ->
  (unit, string) result

val owner_private :
  Octra_core.Circles.circle_info ->
  string ->
  (unit, string) result

val storage_owner :
  Octra_core.Circles.circle_info ->
  string ->
  (unit, string) result

val view_gate :
  include_storage:bool ->
  gate

val verify :
  t ->
  string ->
  (string, string) result

val authenticate :
  op:string ->
  circle_id:string ->
  subject:string ->
  t ->
  (string, string) result

val authorize :
  ?gate:gate ->
  op:string ->
  circle_id:string ->
  subject:string ->
  Octra_core.Circles.circle_info ->
  t ->
  (string, string) result

val rpc_error :
  string ->
  Octra_core.Rpc.rpc_error

val parse_rpc_auth :
  Yojson.Safe.t ->
  int ->
  int ->
  int ->
  (t, Octra_core.Rpc.rpc_error) result

val authorize_rpc :
  Octra_core.Circles.circle_info ->
  gate:gate ->
  op:string ->
  circle_id:string ->
  subject:string ->
  t ->
  (string, Octra_core.Rpc.rpc_error) result

val authenticate_rpc :
  op:string ->
  circle_id:string ->
  subject:string ->
  t ->
  (string, Octra_core.Rpc.rpc_error) result

val authenticate_rpc_params :
  Yojson.Safe.t ->
  int ->
  int ->
  int ->
  op:string ->
  circle_id:string ->
  subject:string ->
  (string, Octra_core.Rpc.rpc_error) result

val auth_required0 :
  Yojson.Safe.t ->
  string ->
  Octra_core.Rpc.rpc_error

val auth_required1 :
  Yojson.Safe.t ->
  string ->
  string ->
  Octra_core.Rpc.rpc_error

val auth_required2 :
  Yojson.Safe.t ->
  string ->
  string ->
  string ->
  Octra_core.Rpc.rpc_error