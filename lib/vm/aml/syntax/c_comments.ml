(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type pos = {
  off : int;
  line : int;
  col : int;
}

type error = {
  first : pos;
  last : pos;
}

type state = {
  src : string;
  len : int;
  pos : pos;
}

let point off line col = { off; line; col }
let zero = point 0 1 1

let char state =
  if state.pos.off < state.len then Some state.src.[state.pos.off] else None

let char_at state step =
  let off = state.pos.off + step in
  if off < state.len then Some state.src.[off] else None

let next state =
  match char state with
  | None -> state
  | Some '\n' ->
    { state with pos = point (state.pos.off + 1) (state.pos.line + 1) 1 }
  | Some _ ->
    { state with
      pos = point (state.pos.off + 1) state.pos.line (state.pos.col + 1) }

let twice state = next (next state)

let mask src first last =
  String.sub src first (last - first)
  |> String.map (fun char -> if Char.equal char '\n' then '\n' else ' ')

let rec line state =
  match char state with
  | None | Some '\n' -> state
  | Some _ -> line (next state)

let rec block first depth state =
  match char state, char_at state 1 with
  | None, _ -> Error { first; last = state.pos }
  | Some '/', Some '*' -> block first (depth + 1) (twice state)
  | Some '*', Some '/' ->
    let state = twice state in
    if depth = 1 then Ok state else block first (depth - 1) state
  | Some _, _ -> block first depth (next state)

let erase src =
  let rec scan plain out state =
    match char state, char_at state 1 with
    | None, _ ->
      let tail = String.sub src plain (state.len - plain) in
      Ok (String.concat "" (List.rev (tail :: out)))
    | Some '/', Some '/' ->
      let first = state.pos.off in
      let stop = line (twice state) in
      let raw = String.sub src plain (first - plain) in
      let hidden = mask src first stop.pos.off in
      scan stop.pos.off (hidden :: raw :: out) stop
    | Some '/', Some '*' ->
      let first = state.pos in
      begin
        match block first 1 (twice state) with
        | Error error -> Error error
        | Ok stop ->
          let raw = String.sub src plain (first.off - plain) in
          let hidden = mask src first.off stop.pos.off in
          scan stop.pos.off (hidden :: raw :: out) stop
      end
    | Some _, _ -> scan plain out (next state)
  in
  scan 0 [] { src; len = String.length src; pos = zero }

let text error =
  Printf.sprintf
    "comment is not closed line = %d column = %d offset = %d"
    error.first.line error.first.col error.first.off