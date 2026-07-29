(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let read_nodes path =
  let ic = open_in path in
  let rec loop acc =
    match input_line ic with
    | line ->
      let s = String.trim line in
      loop (if s <> "" && s.[0] <> '#' then s :: acc else acc)
    | exception End_of_file ->
      close_in ic;
      List.rev acc
  in
  loop []