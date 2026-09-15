(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type anchor = { chain : string; epoch : int; root : string }
type request = { start : int; limit : int; anchor : anchor option; previous : string }
type row = {
  epoch : int;
  root : string;
  previous : string;
  tx_start : int64;
  tx_count : int;
  time : float;
}
type stop = Complete | More | Gap
type page = { anchor : anchor; rows : row list; next : int; stop : stop }

val max_epoch : int
val max_count : int
val max_record : int
val max_bytes : int
val validate : request -> (request, Octra_core.Rpc.rpc_error) result
val parse : Yojson.Safe.t -> (request, Octra_core.Rpc.rpc_error) result
val request_json : request -> Yojson.Safe.t
val row_json : row -> Yojson.Safe.t
val json : page -> Yojson.Safe.t
val of_json : Yojson.Safe.t -> (page, string) result
val admit :
  Lwt_mutex.t -> (unit -> ('a, Octra_core.Rpc.rpc_error) result Lwt.t) ->
  ('a, Octra_core.Rpc.rpc_error) result Lwt.t
val read :
  head:anchor option ->
  load:(max_bytes:int -> int -> (string option, Octra_core.Rpc.rpc_error) result) ->
  request -> (page, Octra_core.Rpc.rpc_error) result Lwt.t