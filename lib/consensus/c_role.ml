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


type t =
  | Single
  | Validator
  | Observer

let normalize s =
  String.lowercase_ascii (String.trim s)

let of_mode ?(cli_observer = false) mode =
  match mode with
  | Some s when normalize s = "observer" -> Observer
  | Some s when normalize s = "bft" && cli_observer -> Observer
  | Some s when normalize s = "bft" -> Validator
  | _ when cli_observer -> Observer
  | _ -> Single

let consensus_enabled = function
  | Single -> false
  | Validator | Observer -> true

let voting_enabled = function
  | Validator -> true
  | Single | Observer -> false

let is_observer = function
  | Observer -> true
  | Single | Validator -> false

let label = function
  | Single -> "single"
  | Validator -> "validator"
  | Observer -> "observer"