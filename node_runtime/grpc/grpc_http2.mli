(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val start :
  Grpc_config.t ->
  call:Grpc_service.call ->
  unit Lwt.t

val accept :
  Grpc_config.t ->
  call:Grpc_service.call ->
  Unix.sockaddr ->
  Lwt_unix.file_descr ->
  unit Lwt.t