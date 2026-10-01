(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val schema_version : int

type t = {
  schema_version : int;
  generation : int;
  epoch_id : int;
  state_root : string;
  ledger_state_root : string option;
  irmin_commit : string option;
  txid_hi : int64;
  txlog_seg : int option;
  txlog_off : int option;
  epochlog_off : int option;
  commit_id : string;
  ts : float;
  quorum_cert_hash : string option;
  epoch_index_hash : string option;
  epoch_index_root : string option;
}

val opt_int_to_json : int option -> Yojson.Safe.t
val opt_str_to_json : string option -> Yojson.Safe.t
val opt_int_of_json : Yojson.Safe.t -> int option
val opt_str_of_json : Yojson.Safe.t -> string option
val ledger_state_root : t -> string
val validate : t -> t
val to_json : t -> string
val of_json : string -> t