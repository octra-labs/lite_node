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


external run_json_unsafe : string -> string
  = "caml_octra_circle_wasm_host_run_json"

let run_json input =
  try Ok (run_json_unsafe input)
  with Failure message -> Error message