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


let int_of_nonnegative value =
  if Z.sign value < 0 || Z.gt value (Z.of_int max_int) then None
  else Some (Z.to_int value)

let add left right =
  if left < 0 || right < 0 then None
  else int_of_nonnegative Z.(add (of_int left) (of_int right))

let product values =
  if List.exists (fun value -> value < 0) values then None
  else
    values
    |> List.fold_left (fun total value -> Z.mul total (Z.of_int value)) Z.one
    |> int_of_nonnegative

let scaled_product values ~divisor =
  if divisor <= 0 || List.exists (fun value -> value < 0) values then None
  else
    values
    |> List.fold_left (fun total value -> Z.mul total (Z.of_int value)) Z.one
    |> fun total -> Z.div total (Z.of_int divisor)
    |> int_of_nonnegative

let charge ~used ~cost ~limit =
  if used < 0 || cost < 0 || limit < 0 || used > limit || cost > limit - used then None
  else Some (used + cost)