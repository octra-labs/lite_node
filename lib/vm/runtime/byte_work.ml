(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type limits = {
  key_bytes : int;
  value_bytes : int;
  copy_bytes : int;
  write_bytes : int;
  alloc_bytes : int;
  unit_bytes : int;
}

type request =
  | Write of int * int
  | Erase of int
  | Copy of int * int
  | Allocate of int
  | Scan of int

type t = {
  limits : limits;
  mutable remaining : int;
  mutable available : int;
  mutable event_left : int;
  mutable event_count : int;
}

type decoding = {max_bytes : int; requests : request list}

let parsing ~length ~cells =
  if length < 0 || cells < 0 then None
  else
    let base = Z.mul (Z.of_int cells) (Z.of_int 2) in
    let memory = Z.add (Z.mul (Z.of_int cells) (Z.of_int 96))
      (Z.mul (Z.of_int length) (Z.of_int 2)) in
    if not (Z.fits_int base && Z.fits_int memory) then None
    else Some (Z.to_int base, [Scan length; Allocate (Z.to_int memory)])

let encoded_size length =
  if length < 0 then None
  else
    let size = Z.mul (Z.cdiv (Z.of_int length) (Z.of_int 3)) (Z.of_int 4) in
    if Z.gt size (Z.of_int Sys.max_string_length) then None else Some (Z.to_int size)

let cell_bytes ~count ~bits =
  if count < 0 || bits < 0 then None
  else
    let width = Z.add (Z.of_int 80) (Z.cdiv (Z.of_int bits) (Z.of_int 8)) in
    let size = Z.mul (Z.of_int count) width in
    if Z.fits_int size then Some (Z.to_int size) else None

let decoding ~length ~cells ~bits ~cached =
  if length < 0 then None
  else
    Option.bind (cell_bytes ~count:cells ~bits) (fun memory ->
    let capacity = Z.mul (Z.cdiv (Z.of_int length) (Z.of_int 4)) (Z.of_int 3) in
    let copies = if cached then Z.zero else Z.mul capacity (Z.of_int 2) in
    let allocated = Z.add (Z.of_int memory) copies in
    if Z.gt capacity (Z.of_int Sys.max_string_length) || not (Z.fits_int allocated)
    then None
    else
      let requests = [Allocate (Z.to_int allocated)] in
      let requests = if cached then requests else Scan length :: requests in
      Some {max_bytes = Z.to_int capacity; requests})

let limits ~key_bytes ~value_bytes ~copy_bytes ~write_bytes ~alloc_bytes ~unit_bytes =
  if List.exists (fun size -> size < 0 || size > Sys.max_string_length)
      [key_bytes; value_bytes; copy_bytes]
    || write_bytes < 0 || alloc_bytes < 0 || unit_bytes <= 0
  then None
  else Some {key_bytes; value_bytes; copy_bytes; write_bytes; alloc_bytes; unit_bytes}

let create limits =
  {limits; remaining = limits.write_bytes; available = limits.alloc_bytes;
    event_left = 4_194_304; event_count = 256}

let consensus_limits = {
  key_bytes = 16_777_216;
  value_bytes = 4_194_304;
  copy_bytes = 16_777_216;
  write_bytes = 4_194_304;
  alloc_bytes = 134_217_728;
  unit_bytes = 32;
}

let consensus_id =
  "bytes_v2:key_16777216:value_4194304:copy_16777216:write_4194304:alloc_134217728:unit_32:receipt_4194304:event_count_256:returns:shared_gross:inspect_isolated"

let event_left budget = budget.event_left

let text_size ~limit value =
  if limit < 2 || String.length value > limit - 2 then None else
  let exception Full in
  let size = ref 2 in
  try
    String.iter (fun byte ->
      let width = match byte with
        | '"' | '\\' | '\b' | '\012' | '\n' | '\r' | '\t' -> 2
        | '\x00'..'\x1f' | '\x7f' -> 6
        | _ -> 1 in
      if width > limit - !size then raise Full;
      size := !size + width) value;
    Some !size
  with Full -> None

let event_size ~limit ~address ~depth ~name ~values =
  let exception Full in
  let left = ref limit in
  let take size = if size > !left then raise Full else left := !left - size in
  let quoted value =
    match text_size ~limit:!left value with
    | None -> raise Full
    | Some size -> take size in
  if limit < 0 then None else
  try
    take (String.length "{\"program\":,\"contract\":,\"depth\":,\"event\":,\"values\":[]}");
    quoted address;
    quoted address;
    take (String.length (string_of_int depth));
    quoted name;
    List.iteri (fun index value ->
      if index > 0 then take 1;
      quoted value) values;
    Some (limit - !left)
  with Full -> None

let remaining budget = budget.remaining

let available budget = budget.available

let rules budget = budget.limits

let text_limit budget =
  max budget.limits.key_bytes
    (max budget.limits.value_bytes budget.limits.copy_bytes)

let plan limits ~remaining ~available ~used ~limit ~base requests =
  if remaining < 0 || remaining > limits.write_bytes
    || available < 0 || available > limits.alloc_bytes
    || used < 0 || limit < used || base < 0
  then None
  else
    let unit_bytes = Z.of_int limits.unit_bytes in
    let price bytes = Z.cdiv bytes unit_bytes in
    let rec sum effort written allocated = function
      | [] ->
        if Z.gt effort (Z.of_int (limit - used))
          || Z.gt written (Z.of_int remaining)
          || Z.gt allocated (Z.of_int available)
        then None
        else Some (used + Z.to_int effort, remaining - Z.to_int written,
          available - Z.to_int allocated)
      | request :: rest ->
        let amount = match request with
          | Write (key, value) ->
            if key <= 0 || value < 0
              || key > limits.key_bytes || value > limits.value_bytes
            then None
            else
              let bytes = Z.add (Z.of_int key) (Z.of_int value) in
              Some (bytes, bytes, Z.zero)
          | Erase key ->
            if key < 0 then None
            else let bytes = Z.of_int key in Some (bytes, bytes, Z.zero)
          | Copy (left, right) ->
            let bytes = Z.add (Z.of_int left) (Z.of_int right) in
            if left < 0 || right < 0 || Z.gt bytes (Z.of_int limits.copy_bytes)
            then None
            else Some (bytes, Z.zero, bytes)
          | Allocate size ->
            if size < 0 then None
            else let bytes = Z.of_int size in Some (bytes, Z.zero, bytes)
          | Scan size ->
            if size < 0 then None
            else Some (Z.of_int size, Z.zero, Z.zero)
        in
        match amount with
        | None -> None
        | Some (bytes, added, created) ->
          let effort = Z.add effort (price bytes) in
          let written = Z.add written added in
          let allocated = Z.add allocated created in
          if Z.gt effort (Z.of_int (limit - used))
            || Z.gt written (Z.of_int remaining)
            || Z.gt allocated (Z.of_int available)
          then None
          else sum effort written allocated rest
    in
    sum (Z.of_int base) Z.zero Z.zero requests

let charge budget ~used ~limit ~base requests =
  match plan budget.limits ~remaining:budget.remaining ~available:budget.available
    ~used ~limit ~base requests with
  | None -> None
  | Some (effort, remaining, available) ->
    budget.remaining <- remaining;
    budget.available <- available;
    Some effort

let output budget ~used ~limit size =
  if size < 0 || size > budget.event_left then None
  else match charge budget ~used ~limit ~base:0 [Allocate size] with
  | None -> None
  | Some effort ->
    budget.event_left <- budget.event_left - size;
    Some effort

let emit budget ~used ~limit size =
  if size < 0 || budget.event_count = 0 then None else
  let size = if budget.event_count = 256 && size < max_int then size + 1 else size in
  match output budget ~used ~limit size with
  | None -> None
  | Some effort ->
    budget.event_count <- budget.event_count - 1;
    Some effort