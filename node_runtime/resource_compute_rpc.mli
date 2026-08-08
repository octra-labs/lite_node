(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val submit :
  Resource_compute_service.t ->
  Yojson.Safe.t ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result Lwt.t

val status :
  Resource_compute_service.t ->
  Yojson.Safe.t ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result Lwt.t