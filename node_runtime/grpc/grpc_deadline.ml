(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let unit_seconds = function
  | 'H' -> Some 3600.0
  | 'M' -> Some 60.0
  | 'S' -> Some 1.0
  | 'm' -> Some 0.001
  | 'u' -> Some 0.000_001
  | 'n' -> Some 0.000_000_001
  | _ -> None

let digits value =
  String.length value > 0
  && String.length value <= 8
  && String.for_all (function '0' .. '9' -> true | _ -> false) value

let parse raw =
  let value = String.trim raw in
  let length = String.length value in
  if length < 2 then Error "grpc-timeout is invalid"
  else
    let number = String.sub value 0 (length - 1) in
    match unit_seconds value.[length - 1] with
    | None -> Error "grpc-timeout unit is invalid"
    | Some _ when not (digits number) -> Error "grpc-timeout is invalid"
    | Some scale ->
      begin
        match int_of_string_opt number with
        | Some count when count > 0 -> Ok (float_of_int count *. scale)
        | _ -> Error "grpc-timeout is invalid"
      end

let seconds ~default ~limit = function
  | None -> Ok (min default limit)
  | Some raw -> Result.map (min limit) (parse raw)