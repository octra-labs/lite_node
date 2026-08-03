(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type write_report = {
  commit : string;
  root : string;
  records : int;
  bytes : int64;
  pvac_hashes : string list;
}

type restore_report = {
  commit : string;
  root : string;
  records : int;
  bytes : int64;
}

val write :
  Store_irmin.t ->
  commit:string ->
  path:string ->
  (write_report, string) result Lwt.t

val restore :
  source:string ->
  target:string ->
  expected_root:string ->
  (restore_report, string) result Lwt.t