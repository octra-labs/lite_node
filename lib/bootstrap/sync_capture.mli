(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type source = {
  data_dir : string;
  head : Octra_core.Head_manifest.t;
  store : Octra_core.Store_irmin.t;
}

type report = {
  epoch : int;
  state_root : string;
  ledger_root : string;
  irmin_commit : string;
  files : int;
  bytes : int64;
}

val build : source -> target:string -> (report, string) result Lwt.t