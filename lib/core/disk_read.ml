(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

external read : Unix.file_descr -> int -> bytes -> int -> int -> int
  = "octra_disk_read_at"

let exact fd at buffer offset length =
  let rec loop at offset remaining =
    if remaining = 0 then true
    else
      let count = read fd at buffer offset remaining in
      count > 0 && loop (at + count) (offset + count) (remaining - count)
  in
  loop at offset length