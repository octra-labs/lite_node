(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  typ : C_type.t;
  value : C_eval.value;
  raw : string;
}

type error =
  | Header
  | Bits
  | Size of int * int
  | Form

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let typ item = item.typ
let value item = item.value
let encode item = item.raw

let rec plain = function
  | C_type.Unit | C_type.Bool | C_type.Int | C_type.Num _ | C_type.Bytes _ -> true
  | C_type.Vec (_, elem) | C_type.Seq (_, elem) -> plain elem
  | C_type.Pair (lhs, rhs) | C_type.Sum (lhs, rhs) ->
    plain lhs && plain rhs
  | C_type.Cap _ | C_type.Enc _ -> false

let root = function
  | C_type.Unit | C_type.Vec _ | C_type.Seq _ | C_type.Pair _ | C_type.Sum _ -> true
  | C_type.Bool | C_type.Int | C_type.Num _ | C_type.Bytes _
  | C_type.Cap _ | C_type.Enc _ ->
    false

let bits raw =
  let rec loop index out =
    if index = String.length raw then Ok (List.rev out)
    else
      match raw.[index] with
      | '0' -> loop (index + 1) (false :: out)
      | '1' -> loop (index + 1) (true :: out)
      | _ -> Error Bits
  in
  loop 0 []

let chars values =
  let out = Bytes.create (List.length values) in
  let rec loop index = function
    | [] -> Bytes.unsafe_to_string out
    | value :: rest ->
      Bytes.set out index (if value then '1' else '0');
      loop (index + 1) rest
  in
  loop 0 values

let code typ value =
  match C_bin.ty_code typ, C_bin.value_code value with
  | Some typ, Some value ->
    Some (C_bin.Tag (Z.zero, C_bin.Cons (typ, C_bin.Cons (value, C_bin.Nil))))
  | _ -> None

let raw typ value =
  match code typ value with
  | None -> None
  | Some code ->
    begin
      match C_bin.enc_code code with
      | None -> None
      | Some bits ->
        let raw = "AR1\n" ^ chars bits in
        if String.length raw <= C_rule.local.str then Some raw else None
    end

let valid typ value =
  root typ && plain typ && C_type.valid typ
  && C_feed.typed typ value && C_feed.shaped value

let make typ value =
  if not (valid typ value) then Error Form
  else
    match raw typ value with
    | Some raw -> Ok { typ; value; raw }
    | None -> Error Form

let get = function
  | C_bin.Tag (tag, C_bin.Cons (typ, C_bin.Cons (value, C_bin.Nil)))
      when Z.equal tag Z.zero ->
    begin
      match C_bin.ty_get typ, C_bin.value_get value with
      | Some typ, Some value ->
        if valid typ value then Some (typ, value) else None
      | _ -> None
    end
  | _ -> None

let decode input =
  let size = String.length input in
  if size > C_rule.local.str then Error (Size (C_rule.local.str, size))
  else if size < 4 || String.sub input 0 4 <> "AR1\n" then Error Header
  else
    let* bits = bits (String.sub input 4 (size - 4)) in
    match C_bin.dec_code bits with
    | None -> Error Form
    | Some code ->
      begin
        match get code with
        | None -> Error Form
        | Some (typ, value) ->
          begin
            match make typ value with
            | Ok item when String.equal item.raw input -> Ok item
            | Ok _ | Error _ -> Error Form
          end
      end

let equal lhs rhs =
  C_type.equal lhs.typ rhs.typ && C_eval.equal lhs.value rhs.value

let text = function
  | Header -> "result file header is invalid"
  | Bits -> "result file bit image is invalid"
  | Size (maximum, actual) ->
    Printf.sprintf "result file size = %d maximum = %d" actual maximum
  | Form -> "result file form is invalid"