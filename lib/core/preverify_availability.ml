(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type 'a t =
  | Unmanaged
  | Pending
  | Ready of 'a
  | Invalid of string