(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

include Head_record

module Scope = Store_scope

let max_record_bytes = 1024 * 1024
let path data_dir = Filename.concat data_dir "HEAD.json"

let atomic_write ?(sync = Unix.fsync) data_dir head =
  let json = to_json (validate head) in
  if String.length json > max_record_bytes then failwith "HEAD exceeds size limit";
  let target = path data_dir in
  let staged = target ^ ".staged" in
  let channel = open_out_bin staged in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    output_string channel json;
    flush channel;
    sync (Unix.descr_of_out_channel channel);
    close_out channel);
  Unix.rename staged target;
  let fd = Unix.openfile data_dir [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> sync fd)

type load_result = Missing | Present of t | Corrupt of string

let rec read_bytes fd bytes offset count =
  try Unix.read fd bytes offset count with
  | Unix.Unix_error (Unix.EINTR, _, _) -> read_bytes fd bytes offset count

let read_file target prior =
  let scope = Scope.create ~release:(fun () -> ()) in
  Scope.guard scope (fun () ->
    let fd = Scope.acquire scope (fun () ->
      Unix.openfile target [Unix.O_RDONLY; Unix.O_CLOEXEC; Unix.O_NONBLOCK] 0) Unix.close in
    let stat = Unix.fstat fd in
    if stat.Unix.st_kind <> Unix.S_REG || prior.Unix.st_dev <> stat.st_dev
       || prior.st_ino <> stat.st_ino then failwith "HEAD file identity changed";
    if stat.st_size > max_record_bytes then failwith "HEAD exceeds size limit";
    let bytes = Bytes.create stat.st_size in
    let rec read offset =
      if offset < Bytes.length bytes then
        let count = read_bytes fd bytes offset (Bytes.length bytes - offset) in
        if count = 0 then failwith "HEAD shortened during read" else read (offset + count) in
    read 0;
    if read_bytes fd (Bytes.create 1) 0 1 <> 0 then failwith "HEAD grew during read";
    let head = of_json (Bytes.to_string bytes) in
    Scope.close scope;
    Present head)

let load_result data_dir =
  let target = path data_dir in
  try
    match Unix.lstat target with
    | stat when stat.Unix.st_kind = Unix.S_REG -> read_file target stat
    | _ -> Corrupt "HEAD is not a regular file"
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Missing
  with
  | (Unix.Unix_error _ | Failure _ | Invalid_argument _ | Yojson.Json_error _
      | Yojson.Safe.Util.Type_error _ | Scope.Close_failed _) as error ->
    Corrupt (Printexc.to_string error)

let load data_dir =
  match load_result data_dir with
  | Missing -> None
  | Present head -> Some head
  | Corrupt reason -> failwith ("invalid HEAD: " ^ reason)

let cached : t option ref = ref None
let set_cached head = cached := Some head
let get_cached () = !cached
let load_to_cache data_dir = cached := load data_dir; !cached

let is_epoch_visible head epoch =
  match head with None -> true | Some head -> epoch <= head.epoch_id

let is_txid_visible head txid =
  match head with None -> true | Some head -> Int64.compare txid head.txid_hi <= 0

let is_txlog_pos_visible head ~seg ~off =
  match head with
  | None -> true
  | Some head ->
    match head.txlog_seg, head.txlog_off with
    | None, _ | _, None -> true
    | Some segment, Some offset -> seg < segment || (seg = segment && off <= offset)