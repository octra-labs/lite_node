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


let int_value name default =
  match Sys.getenv_opt name with
  | Some s ->
    begin
      try int_of_string s with _ -> default
    end
  | None -> default

let flag name =
  match Sys.getenv_opt name with
  | Some s ->
    let value = String.lowercase_ascii (String.trim s) in
    value = "1" || value = "true" || value = "yes"
  | None -> false