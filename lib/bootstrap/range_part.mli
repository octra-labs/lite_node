(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type part = {
  hash : string;
  index : int;
  count : int;
  data : string;
}

type view =
  | Full of Yojson.Safe.t
  | Part of part

val body_max : int
val full_max : int
val reply : ?index:int -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
val view : Yojson.Safe.t -> (view, string) result
val join : part list -> (Yojson.Safe.t, string) result