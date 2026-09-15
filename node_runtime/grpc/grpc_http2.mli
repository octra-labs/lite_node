(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val accept_codes : int list

val accept_delay : int -> exn -> float option

val take_client :
  ?take:(Lwt_unix.file_descr -> (Lwt_unix.file_descr * Unix.sockaddr) Lwt.t) ->
  ?sleep:(float -> unit Lwt.t) ->
  Lwt_unix.file_descr ->
  (Lwt_unix.file_descr * Unix.sockaddr) Lwt.t

val start :
  ?submit:Grpc_service.call ->
  ?socket:Lwt_unix.file_descr ->
  Grpc_config.t ->
  call:Grpc_service.call ->
  unit Lwt.t

val accept :
  ?submit:Grpc_service.call ->
  Grpc_config.t ->
  call:Grpc_service.call ->
  Unix.sockaddr ->
  Lwt_unix.file_descr ->
  unit Lwt.t

val serve :
  ?submit:Grpc_service.call ->
  ?socket:Lwt_unix.file_descr ->
  Grpc_config.t ->
  call:Grpc_service.call ->
  http:unit Lwt.t ->
  unit Lwt.t