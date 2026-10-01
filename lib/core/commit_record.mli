(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type record =
  | Prepare of {
      commit_id : string;
      prev_generation : int;
      epoch_id : int;
      planned_txid_hi : int64;
      planned_state_root : string;
      ts : float;
    }
  | Commit of {
      commit_id : string;
      generation : int;
      ts : float;
    }
  | Abort of {
      commit_id : string;
      reason : string;
      ts : float;
    }

val record_to_json : record -> Yojson.Safe.t
val decode : Yojson.Safe.t -> (record, string) result
val record_of_json : Yojson.Safe.t -> record option