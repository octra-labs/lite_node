(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let valid base count =
  base >= 0 && count >= 0 && (count = 0 || base <= max_int - (count - 1))

let sized base count =
  base >= 0 && count >= 0 && base <= max_int - count

let offset base index width =
  match Cost.product [index; width] with
  | None -> None
  | Some shift ->
    if width <= 0 || base > max_int - shift then None
    else
      let target = base + shift in
      if valid target width then Some target else None