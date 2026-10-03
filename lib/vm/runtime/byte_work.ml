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

type t = {limits : limits; mutable remaining : int; mutable available : int}

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

let decoding ~length ~cells ~bits ~cached =
  if length < 0 || cells < 0 || bits < 0 then None
  else
    let capacity = Z.mul (Z.cdiv (Z.of_int length) (Z.of_int 4)) (Z.of_int 3) in
    let cell_size = Z.add (Z.of_int 80) (Z.cdiv (Z.of_int bits) (Z.of_int 8)) in
    let memory = Z.mul (Z.of_int cells) cell_size in
    let copies = if cached then Z.zero else Z.mul capacity (Z.of_int 2) in
    let allocated = Z.add memory copies in
    if Z.gt capacity (Z.of_int Sys.max_string_length) || not (Z.fits_int allocated)
    then None
    else
      let requests = [Allocate (Z.to_int allocated)] in
      let requests = if cached then requests else Scan length :: requests in
      Some {max_bytes = Z.to_int capacity; requests}

let limits ~key_bytes ~value_bytes ~copy_bytes ~write_bytes ~alloc_bytes ~unit_bytes =
  if List.exists (fun size -> size < 0 || size > Sys.max_string_length)
      [key_bytes; value_bytes; copy_bytes]
    || write_bytes < 0 || alloc_bytes < 0 || unit_bytes <= 0
  then None
  else Some {key_bytes; value_bytes; copy_bytes; write_bytes; alloc_bytes; unit_bytes}

let create limits =
  {limits; remaining = limits.write_bytes; available = limits.alloc_bytes}

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