(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

external build : int -> unit = "octra_ready_build"
external check : unit -> bool = "octra_ready_check"
external gates : unit -> int = "octra_ready_gates"
external sums : unit -> int = "octra_ready_sums"
external rejected : unit -> int = "octra_ready_rejected"
external capacity : unit -> int = "octra_ready_capacity"

let prepare () =
  build (capacity ());
  if not (check ()) then failwith "proof prepared image"