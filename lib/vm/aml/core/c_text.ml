(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  out : Buffer.t;
  mutable cut : bool;
}

let max = C_rule.local.text
let room out = max - 3 - Buffer.length out.out
let make () = { out = Buffer.create 64; cut = false }

let add out value =
  if not out.cut then
    let left = room out in
    if left <= 0 then out.cut <- true
    else if String.length value <= left then Buffer.add_string out.out value
    else begin
      Buffer.add_substring out.out value 0 left;
      out.cut <- true
    end

let full out = out.cut || room out <= 0

let get out =
  let value = Buffer.contents out.out in
  if out.cut || room out <= 0 then value ^ "..." else value

let clip value =
  if String.length value <= max then value
  else String.sub value 0 (max - 3) ^ "..."