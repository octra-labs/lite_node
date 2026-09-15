(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val decode_empty : bytes -> (unit, string) result
val decode_address : bytes -> (string, string) result
val decode_hash : bytes -> (string, string) result
val decode_submit : bytes -> (Yojson.Safe.t, string) result
val decode_epoch : bytes -> (int, string) result
val decode_health_service : bytes -> (string, string) result
val encode_json : string -> string
val encode_serving : unit -> string
val decode_page : bytes -> (Epoch_page.request, string) result
val encode_page : Epoch_page.page -> string