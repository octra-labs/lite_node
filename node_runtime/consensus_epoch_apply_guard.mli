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


type prev_root_check =
  | Prev_root_ok
  | Prev_root_mismatch of {
      local_raw : string;
      expected_prev : string;
    }

val raw32_of_pre_root : string -> string

val raw_hex8 : string -> string

val check_prev_root :
  local_pre_root:string ->
  expected_prev:string ->
  prev_root_check

val mismatch_line :
  epoch_id:int ->
  local_raw:string ->
  expected_prev:string ->
  string