(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type source = {
  data_dir : string;
  head : Octra_core.Head_manifest.t;
  store : Octra_core.Store_irmin.t;
  roots : Octra_consensus.C_types.finalize list;
}

type report = {
  epoch : int;
  state_root : string;
  ledger_root : string;
  irmin_commit : string;
  files : int;
  bytes : int64;
}

val format : chain_id:string -> epoch:int -> Octra_core.Ledger_image.format

val stage_owned :
  ?format:Octra_core.Ledger_image.format ->
  Sync_archive.t -> source -> target:string -> (report, string) result Lwt.t

val build_owned :
  ?format:Octra_core.Ledger_image.format ->
  Sync_archive.t -> source -> target:string -> (report, string) result Lwt.t

val build : ?format:Octra_core.Ledger_image.format ->
  source -> target:string -> (report, string) result Lwt.t