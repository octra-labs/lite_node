(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {text : string; count : int; chars : int}

let is_space = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let fold text ~init ~visit =
  let size = String.length text in
  let rec scan index first finish state =
    if index = size then
      if first < 0 then Some state else visit state first (finish - first)
    else if text.[index] = ',' then
      if first < 0 then scan (index + 1) (-1) 0 state
      else
        match visit state first (finish - first) with
        | None -> None
        | Some state -> scan (index + 1) (-1) 0 state
    else if is_space text.[index] then scan (index + 1) first finish state
    else scan (index + 1) (if first < 0 then index else first) (index + 1) state
  in
  scan 0 (-1) 0 init

let plan ~capacity ~fits text =
  if capacity < 0 || not (fits ~count:0 ~chars:0) then None
  else
    let visit (count, chars) _ length =
      if count >= capacity || chars > max_int - length then None
      else
        let count = count + 1 in
        let chars = chars + length in
        if fits ~count ~chars then Some (count, chars) else None
    in
    Option.map (fun (count, chars) -> {text; count; chars})
      (fold text ~init:(0, 0) ~visit)

let read ~strict plan =
  let values = Array.make plan.count Z.zero in
  let visit index first length =
    if index >= plan.count then None
    else
      let value =
        try Some (Z.of_string (String.sub plan.text first length))
        with Invalid_argument _ | Failure _ -> if strict then None else Some Z.zero
      in
      Option.map (fun value -> values.(index) <- value; index + 1) value
  in
  match fold plan.text ~init:0 ~visit with
  | Some count when count = plan.count -> Some values
  | Some _ | None -> None