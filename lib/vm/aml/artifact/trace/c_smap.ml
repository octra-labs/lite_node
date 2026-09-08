(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  spans : C_lex.span array;
  bits : C_bin.bits;
}

type error =
  | Empty
  | Size of int
  | Point of int
  | Span of int
  | Bits

let max = (4 * C_rule.local.tm_nodes) + 2
let source_max = C_rule.local.str
let coord_max = source_max + 1

let point_ok (value : C_lex.pos) =
  value.off >= 0 && value.off <= source_max
  && value.line > 0 && value.line <= coord_max
  && value.col > 0 && value.col <= coord_max

let point_le (left : C_lex.pos) (right : C_lex.pos) =
  left.off <= right.off
  && (left.line < right.line
      || (left.line = right.line && left.col <= right.col))

let span_ok (value : C_lex.span) =
  point_ok value.first && point_ok value.last
  && point_le value.first value.last

let rec same_bits left right =
  match left, right with
  | [], [] -> true
  | first :: left, second :: right when Bool.equal first second ->
    same_bits left right
  | _ -> false

let point_code (value : C_lex.pos) =
  C_bin.Tag (Z.zero,
    C_bin.Cons (C_bin.Num (Z.of_int value.off),
      C_bin.Cons (C_bin.Num (Z.of_int value.line),
        C_bin.Cons (C_bin.Num (Z.of_int value.col), C_bin.Nil))))

let span_code (value : C_lex.span) =
  C_bin.Tag (Z.one,
    C_bin.Cons (point_code value.first,
      C_bin.Cons (point_code value.last, C_bin.Nil)))

let map_code values =
  let rec fold out = function
    | [] -> out
    | value :: rest -> fold (C_bin.Cons (span_code value, out)) rest
  in
  C_bin.Tag (Z.of_int 2, fold C_bin.Nil (List.rev (Array.to_list values)))

let make values =
  let size = Array.length values in
  let rec walk at =
    if at = size then
      match C_bin.enc_code (map_code values) with
      | Some bits -> Ok { spans = Array.copy values; bits }
      | None -> Error Bits
    else
      let value : C_lex.span = values.(at) in
      if not (point_ok value.first && point_ok value.last) then Error (Point at)
      else if not (span_ok value) then Error (Span at)
      else walk (at + 1)
  in
  if size = 0 then Error Empty
  else if size > max then Error (Size size)
  else walk 0

let spans value = Array.copy value.spans
let length value = Array.length value.spans
let get value at = value.spans.(at)
let equal left right = same_bits left.bits right.bits

let point_get = function
  | C_bin.Tag (tag,
      C_bin.Cons (C_bin.Num off,
        C_bin.Cons (C_bin.Num line,
          C_bin.Cons (C_bin.Num col, C_bin.Nil)))) when Z.equal tag Z.zero ->
    begin
      match C_nat.make off, C_nat.make line, C_nat.make col with
      | Some off, Some line, Some col ->
        Some {
          C_lex.off = C_nat.to_int off;
          line = C_nat.to_int line;
          col = C_nat.to_int col;
        }
      | _ -> None
    end
  | _ -> None

let span_get = function
  | C_bin.Tag (tag, C_bin.Cons (first, C_bin.Cons (last, C_bin.Nil)))
      when Z.equal tag Z.one ->
    begin
      match point_get first, point_get last with
      | Some first, Some last -> Some { C_lex.first; last }
      | _ -> None
    end
  | _ -> None

let map_get = function
  | C_bin.Tag (tag, body) when Z.equal tag (Z.of_int 2) ->
    let rec walk count out = function
      | C_bin.Nil -> Some (Array.of_list (List.rev out))
      | C_bin.Cons (first, rest) when count < max ->
        begin
          match span_get first with
          | Some value -> walk (count + 1) (value :: out) rest
          | None -> None
        end
      | _ -> None
    in
    walk 0 [] body
  | _ -> None

let enc value = value.bits

let dec input =
  match C_bin.dec_code input with
  | None -> None
  | Some shape ->
    begin
      match map_get shape with
      | None -> None
      | Some raw ->
        begin
          match make raw with
          | Error _ -> None
          | Ok value when same_bits (enc value) input -> Some value
          | Ok _ -> None
        end
    end

let text = function
  | Empty -> "source map is empty"
  | Size size -> Printf.sprintf "source map size = %d maximum = %d" size max
  | Point at -> Printf.sprintf "source map point is invalid index = %d" at
  | Span at -> Printf.sprintf "source map span is invalid index = %d" at
  | Bits -> "source map bit image exceeds the local limit"