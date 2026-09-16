(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path

let rec find_root path =
  if Sys.file_exists (Filename.concat path "dune-project") then path
  else
    let parent = Filename.dirname path in
    if parent = path then failwith "project root not found" else find_root parent

let source_root =
  let executable_dir = Filename.dirname (absolute Sys.executable_name) in
  find_root executable_dir

let source path =
  Filename.concat source_root path

let root =
  Filename.concat (Filename.concat source_root "runtime_data") "tests"

let ensure_dir path =
  try Unix.mkdir path 0o700 with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let ensure_root () =
  ensure_dir (Filename.dirname root);
  ensure_dir root

let path name =
  ensure_root ();
  Filename.concat root name

let sequence = ref 0

let unique_name prefix suffix =
  incr sequence;
  Printf.sprintf "%s-%d-%d%s" prefix (Unix.getpid ()) !sequence suffix

let rec unique_path prefix =
  ensure_root ();
  let target = Filename.concat root (unique_name prefix "") in
  if Sys.file_exists target then unique_path prefix else target

let rec unique_dir prefix =
  ensure_root ();
  let target = Filename.concat root (unique_name prefix "") in
  try
    Unix.mkdir target 0o700;
    target
  with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> unique_dir prefix

let unique_file prefix suffix =
  ensure_root ();
  Filename.temp_file ~temp_dir:root prefix suffix