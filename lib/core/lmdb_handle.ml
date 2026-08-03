(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Mdb = Lmdb__.Lmdb_bindings

external map_env : ('key, 'value, 'dup) Lmdb.Map.t -> Mdb.env
  = "octra_lmdb_map_env"

external map_dbi : ('key, 'value, 'dup) Lmdb.Map.t -> Mdb.dbi
  = "octra_lmdb_map_dbi"

external invalidate : ('key, 'value, 'dup) Lmdb.Map.t -> unit
  = "octra_lmdb_map_invalidate"

let close map =
  let dbi = map_dbi map in
  if dbi != Mdb.invalid_dbi then begin
    Mdb.dbi_close (map_env map) dbi;
    invalidate map
  end