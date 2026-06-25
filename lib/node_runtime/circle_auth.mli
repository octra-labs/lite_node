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
  | Storage_owner_if of bool

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