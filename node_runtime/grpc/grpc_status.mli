(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type code =
  | Ok
  | Cancelled
  | Unknown
  | Invalid_argument
  | Deadline_exceeded
  | Not_found
  | Already_exists
  | Permission_denied
  | Resource_exhausted
  | Failed_precondition
  | Aborted
  | Out_of_range
  | Unimplemented
  | Internal
  | Unavailable
  | Data_loss
  | Unauthenticated

type t = {
  code : code;
  message : string;
  rpc_error : Octra_core.Rpc.rpc_error option;
}

val make : code -> string -> t
val ok : t
val of_rpc : Octra_core.Rpc.rpc_error -> t
val number : code -> int
val trailers : t -> (string * string) list