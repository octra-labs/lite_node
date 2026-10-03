(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let measure ~fits keys =
  let rec scan total keys =
    if not (fits total) then None
    else
      match keys () with
      | Seq.Nil -> Some total
      | Seq.Cons (key, rest) ->
        let size = String.length key in
        if total > max_int - size - 32 then None
        else scan (total + size + 32) rest
  in
  scan 0 keys

let find text pattern =
  let size = String.length text in
  let width = String.length pattern in
  if width = 0 then 0
  else if width > size then -1
  else
    let links = Array.make width 0 in
    let matched = ref 0 in
    for index = 1 to width - 1 do
      while !matched > 0 && pattern.[index] <> pattern.[!matched] do
        matched := links.(!matched - 1)
      done;
      if pattern.[index] = pattern.[!matched] then incr matched;
      links.(index) <- !matched
    done;
    matched := 0;
    let index = ref 0 in
    while !index < size && !matched < width do
      while !matched > 0 && text.[!index] <> pattern.[!matched] do
        matched := links.(!matched - 1)
      done;
      if text.[!index] = pattern.[!matched] then incr matched;
      incr index
    done;
    if !matched = width then !index - width else -1

let has_prefix text prefix =
  let width = String.length prefix in
  let rec same index =
    index = width || (text.[index] = prefix.[index] && same (index + 1))
  in
  String.length text >= width && same 0

let compare_slice text start other =
  let size = String.length text - start in
  let width = min size (String.length other) in
  let rec compare_at index =
    if index = width then Int.compare size (String.length other)
    else
      let order = Char.compare text.[start + index] other.[index] in
      if order = 0 then compare_at (index + 1) else order
  in
  compare_at 0

let page ~prefix ~after ~capacity ~iter =
  if capacity < 0 || capacity > 1001 then invalid_arg "text page capacity";
  let heap = Array.make capacity "" in
  let count = ref 0 in
  let rec rise index value =
    if index = 0 then heap.(index) <- value
    else
      let parent = (index - 1) / 2 in
      if String.compare heap.(parent) value >= 0 then heap.(index) <- value
      else begin
        heap.(index) <- heap.(parent);
        rise parent value
      end
  in
  let rec sink index value =
    let left = index * 2 + 1 in
    if left >= !count then heap.(index) <- value
    else
      let right = left + 1 in
      let child =
        if right < !count && String.compare heap.(right) heap.(left) > 0
        then right else left in
      if String.compare value heap.(child) >= 0 then heap.(index) <- value
      else begin
        heap.(index) <- heap.(child);
        sink child value
      end
  in
  iter (fun key ->
    if capacity > 0 && (String.length key = 0 || key.[0] <> '\000')
      && has_prefix key prefix
      && (after = "" || compare_slice key (String.length prefix) after > 0)
    then
      if !count < capacity then begin
        rise !count key;
        incr count
      end else if String.compare key heap.(0) < 0 then sink 0 key);
  let selected = Array.sub heap 0 !count in
  Array.sort String.compare selected;
  Array.to_list selected