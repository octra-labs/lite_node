(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type lease = {
  snapshot_id : string;
  last_seen : float;
}

val renew : now:float -> string -> (unit, string) result

val read :
  now:float ->
  published_at:float ->
  snapshot_id:string ->
  string ->
  lease option

val retained : lease list -> string list