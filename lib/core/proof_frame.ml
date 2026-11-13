(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type command = Check of int * int * int | Abort of int | Stop
type reply = Ready | Started of int * int | Answer of int * int * int * string
  | Cancelled of int * bool | Fault of int | Loaded of int

let limit = 4096
let now () = Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1_000_000_000.
let close fd = try Unix.close fd with Unix.Unix_error (Unix.EBADF, _, _) -> ()
let signal pid = try Unix.kill pid Sys.sigkill with Unix.Unix_error (Unix.ESRCH, _, _) -> ()

let rec reap pid =
  try snd (Unix.waitpid [] pid)
  with Unix.Unix_error (Unix.EINTR, _, _) -> reap pid

let write_part fd raw offset length =
  let previous = Thread.sigmask Unix.SIG_BLOCK [Sys.sigpipe] in
  Fun.protect ~finally:(fun () -> ignore (Thread.sigmask Unix.SIG_SETMASK previous)) (fun () ->
    let pending = List.mem Sys.sigpipe (Unix.sigpending ()) in
    try Unix.single_write_substring fd raw offset length with
    | Unix.Unix_error (Unix.EPIPE, _, _) as error ->
      if not pending && List.mem Sys.sigpipe (Unix.sigpending ()) then
        ignore (Thread.wait_signal [Sys.sigpipe]);
      raise error)

let write fd raw =
  let rec loop offset =
    if offset < String.length raw then
      try
        let count = write_part fd raw offset (String.length raw - offset) in
        if count = 0 then raise End_of_file;
        loop (offset + count)
      with Unix.Unix_error (Unix.EINTR, _, _) -> loop offset in
  loop 0

let read fd length =
  let bytes = Bytes.create length in
  let rec loop offset =
    if offset = length then Bytes.unsafe_to_string bytes
    else try
      let count = Unix.read fd bytes offset (length - offset) in
      if count = 0 then raise End_of_file;
      loop (offset + count)
    with Unix.Unix_error (Unix.EINTR, _, _) -> loop offset in
  loop 0

let pack values =
  let raw = Bytes.create (8 * List.length values) in
  List.iteri (fun index value -> Bytes.set_int64_be raw (8 * index) (Int64.of_int value)) values;
  Bytes.unsafe_to_string raw

let number raw offset =
  let value = String.get_int64_be raw offset in
  if value < 0L || value > Int64.of_int max_int then failwith "proof frame number";
  Int64.to_int value

let command_bytes = function
  | Check (id, bytes, memory) -> pack [1; id; bytes; memory]
  | Abort id -> pack [2; id; 0; 0]
  | Stop -> pack [3; 0; 0; 0]

let command raw =
  if String.length raw <> 32 then failwith "proof command size";
  let tag, id, bytes, memory = number raw 0, number raw 8, number raw 16, number raw 24 in
  match tag with
  | 1 when id > 0 && bytes > 0 && bytes <= Pvac_verify_protocol.max_request_bytes
      && memory >= 64 && memory <= 32768 -> Check (id, bytes, memory)
  | 2 when id > 0 && bytes = 0 && memory = 0 -> Abort id
  | 3 when id = 0 && bytes = 0 && memory = 0 -> Stop
  | _ -> failwith "proof command fields"

let reply_bytes = function
  | Ready -> pack [1; 0; 0]
  | Started (id, pid) -> pack [2; id; pid]
  | Answer (id, sums, rejected, raw) -> pack [7; id; sums; rejected] ^ raw
  | Cancelled (id, reuse) -> pack [4; id; if reuse then 1 else 0]
  | Fault id -> pack [5; id; 0]
  | Loaded id -> pack [6; id; 0]

let reply raw =
  if String.length raw < 24 then failwith "proof reply size";
  let tag, id, value = number raw 0, number raw 8, number raw 16 in
  match tag with
  | 1 when id = 0 && value = 0 && String.length raw = 24 -> Ready
  | 2 when id > 0 && value > 1 && String.length raw = 24 -> Started (id, value)
  | 7 when id > 0 && String.length raw >= 32 && number raw 24 <= value ->
    Answer (id, value, number raw 24, String.sub raw 32 (String.length raw - 32))
  | 4 when id > 0 && value <= 1 && String.length raw = 24 -> Cancelled (id, value = 1)
  | 5 when id > 0 && value = 0 && String.length raw = 24 -> Fault id
  | 6 when id > 0 && value = 0 && String.length raw = 24 -> Loaded id
  | _ -> failwith "proof reply fields"

let send fd value =
  let raw = reply_bytes value in
  if String.length raw > limit then failwith "proof reply limit";
  write fd (pack [String.length raw] ^ raw)

type reader = {data : bytes; used : int; size : int option}
let reader () = {data = Bytes.create (limit + 8); used = 0; size = None}

let receive fd state =
  let target = Option.fold ~none:8 ~some:((+) 8) state.size in
  let count = Unix.read fd state.data state.used (target - state.used) in
  if count = 0 then raise End_of_file;
  let used = state.used + count in
  let size = if used = 8 && state.size = None then begin
    let size = number (Bytes.unsafe_to_string state.data) 0 in
    if size < 24 || size > limit then failwith "proof reply limit";
    Some size
  end else state.size in
  match size with
  | Some size when used = size + 8 -> reader (), Some (reply (Bytes.sub_string state.data 8 size))
  | _ -> {state with used; size}, None

external rss : int -> int = "octra_proof_rss"
external prefer_oom : unit -> unit = "octra_proof_prefer_oom"
external shed : unit -> unit = "octra_proof_shed"
external children : int -> int list = "octra_proof_children"

let total_rss parent child =
  let left, right = rss parent, rss child in
  if left < 0 || right < 0 then None else Some (left + right)