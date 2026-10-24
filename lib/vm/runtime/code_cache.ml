(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type key = {
  mode : int;
  point_ops : bool;
  trusted : Program_attestation.key list;
  raw : string;
  hash : string;
}

type 'a entry = { key : key; value : 'a; bytes : int }

type ('a, _) message =
  | Read : key -> ('a, 'a option) message
  | Keep : 'a entry -> ('a, unit) message

type 'a t = { mutable entries : 'a entry list; lock : Mutex.t }

let max_entries = 64
let max_bytes = 32 * 1024 * 1024

let create () = { entries = []; lock = Mutex.create () }

let key ~mode ~point_ops ~trusted raw =
  { mode; point_ops; trusted; raw;
    hash = Digestif.SHA256.(digest_string raw |> to_raw_string) }

let same left right =
  left.mode = right.mode && left.point_ops = right.point_ops
  && String.equal left.hash right.hash && left.trusted = right.trusted
  && String.equal left.raw right.raw

let rec take slots bytes = function
  | entry :: rest when slots > 0 && entry.bytes <= bytes ->
    entry :: take (slots - 1) (bytes - entry.bytes) rest
  | _ -> []

let step : type a b. a entry list -> (a, b) message -> a entry list * b =
  fun entries -> function
  | Read key ->
    (match List.find_opt (fun entry -> same entry.key key) entries with
     | None -> entries, None
     | Some entry ->
       entry :: List.filter (fun item -> not (same item.key key)) entries,
       Some entry.value)
  | Keep entry ->
    take max_entries max_bytes
      (entry :: List.filter (fun item -> not (same item.key entry.key)) entries), ()

let request cache message =
  Mutex.lock cache.lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock cache.lock) (fun () ->
    let next, reply = step cache.entries message in
    cache.entries <- next;
    reply)

let size key value =
  let word = Sys.word_size / 8 in
  let words = Obj.reachable_words (Obj.repr (key, value)) in
  if words <= (max_bytes - 1024) / word then Some (1024 + words * word)
  else None

let fits key value = Option.is_some (size key value)

let run ?(retain = Fun.id) cache key compute =
  match request cache (Read key) with
  | Some value -> Ok value
  | None ->
    match compute () with
    | Error _ as error -> error
    | Ok value as result ->
      let value = retain value in
      Option.iter (fun bytes -> request cache (Keep {key; value; bytes}))
        (size key value);
      result