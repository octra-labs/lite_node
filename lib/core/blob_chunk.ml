(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type part = {
  id : string;
  raw : string;
}

type t = {
  id : string;
  size : int;
  parts : part list;
}

let min_size = 4 * 1024
let max_size = 64 * 1024
let mask = 0x3fffL
let gear = 0x5851f42d4c957f2dL

let hash tag raw =
  let state = Digestif.SHA256.init () in
  let state = Digestif.SHA256.feed_string state tag in
  let state = Digestif.SHA256.feed_string state "\000" in
  Digestif.SHA256.feed_string state raw
  |> Digestif.SHA256.get
  |> Digestif.SHA256.to_hex

let part_id raw = hash "octra.account.part" raw
let blob_id raw = hash "octra.account.blob" raw

let count_ok ~size count =
  if size = 0 then count = 0
  else
    size > 0
    && count > 0
    && count <= 1 + ((size - 1) / min_size)

let mix byte =
  let open Int64 in
  let value = mul (of_int (byte + 1)) gear in
  logxor value (shift_right_logical value 29)

let cut raw =
  let len = String.length raw in
  let rec scan start pos state acc =
    if pos = len then
      if pos = start then List.rev acc
      else
        let raw = String.sub raw start (pos - start) in
        List.rev ({ id = part_id raw; raw } :: acc)
    else
      let state =
        Int64.add
          (Int64.shift_left state 1)
          (mix (Char.code raw.[pos]))
      in
      let size = pos - start + 1 in
      if
        size >= min_size
        && (Int64.logand state mask = 0L || size >= max_size)
      then
        let raw = String.sub raw start size in
        scan
          (pos + 1)
          (pos + 1)
          0L
          ({ id = part_id raw; raw } :: acc)
      else
        scan start (pos + 1) state acc
  in
  {
    id = blob_id raw;
    size = len;
    parts = scan 0 0 0L [];
  }

let piece_ok ~final raw =
  let len = String.length raw in
  let rec scan pos state =
    let state =
      Int64.add
        (Int64.shift_left state 1)
        (mix (Char.code raw.[pos]))
    in
    let size = pos + 1 in
    let stop =
      size >= min_size
      && (Int64.logand state mask = 0L || size >= max_size)
    in
    if stop then pos + 1 = len
    else if pos + 1 = len then final
    else scan (pos + 1) state
  in
  len > 0 && scan 0 0L

let join ~id ~size parts =
  if size < 0 then Error "blob size is negative"
  else if not (count_ok ~size (List.length parts)) then
    Error "blob part count differs"
  else
    let rec check total acc = function
      | [] ->
        if total <> size then Error "blob size differs"
        else
          let raw = Buffer.contents acc in
          if String.equal (blob_id raw) id then Ok raw
          else Error "blob id differs"
      | (expected, raw) :: rest ->
        if not (String.equal (part_id raw) expected) then
          Error "blob part id differs"
        else
          let len = String.length raw in
          if len > size - total then Error "blob parts exceed size"
          else
            let total = total + len in
            let final = rest = [] && total = size in
            if not (piece_ok ~final raw) then
              Error "blob part split differs"
          else begin
            Buffer.add_string acc raw;
            check total acc rest
          end
    in
    check 0 (Buffer.create (min size max_size)) parts