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


let units_per_oct = Z.of_int 1_000_000

let max_supply = Z.mul (Z.of_int 1_000_000_000) units_per_oct

let format_balance amount =
  let units = Z.div amount units_per_oct in
  let remainder = Z.rem amount units_per_oct in
  if Z.equal remainder Z.zero then Z.to_string units
  else Printf.sprintf "%s.%06d" (Z.to_string units) (Z.to_int remainder)