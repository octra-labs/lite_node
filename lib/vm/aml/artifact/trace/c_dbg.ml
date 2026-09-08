(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type point =
  | Pc of int
  | Line of int

type cfg = {
  cap : int;
  points : point list;
  expect : C_emit.lit option;
}

type choice =
  | Step
  | Pause
  | Limit

type session = {
  source : string;
  code : string;
  cfg : cfg;
  seen : int;
  trace : string;
  form : string;
}

type error =
  | Cap
  | Point
  | Form
  | Digest
  | Seen
  | Expect

let ( let* ) value next =
  match value with
  | Some value -> next value
  | None -> None

let cap value = value.cap
let points value = value.points
let expect value = value.expect
let source value = value.source
let code value = value.code
let config value = value.cfg
let seen value = value.seen
let trace value = value.trace

let point_compare left right =
  match left, right with
  | Pc lhs, Pc rhs | Line lhs, Line rhs -> Int.compare lhs rhs
  | Pc _, Line _ -> -1
  | Line _, Pc _ -> 1

let valid_point = function
  | Pc value | Line value -> value >= 0 && value <= C_nat.max

let valid_lit = function
  | C_emit.Bool _ | C_emit.Int _ -> true
  | C_emit.Bytes value -> String.length value <= C_nat.max
  | C_emit.Data _ -> true
  | C_emit.Cap (kind, id) -> C_nat.valid kind && C_nat.valid id

let make ~cap ~points ~expect =
  if cap < 1 || cap > C_nat.max then Error Cap
  else if not (List.for_all valid_point points) then Error Point
  else if not (Option.fold ~none:true ~some:valid_lit expect) then Error Expect
  else
    let points = List.sort_uniq point_compare points in
    if List.length points > C_nat.max then Error Point
    else Ok { cap; points; expect }

let hit pc line = function
  | Pc value -> value = pc
  | Line value -> value = line

let decide cfg ~skip ~seen ~pc ~line =
  if seen >= cfg.cap then Limit
  else
    match skip with
    | Some past when seen <= past -> Step
    | _ when List.exists (hit pc line) cfg.points -> Pause
    | _ -> Step

let equal left right =
  match left, right with
  | C_emit.Bool lhs, C_emit.Bool rhs -> Bool.equal lhs rhs
  | C_emit.Int lhs, C_emit.Int rhs -> Z.equal lhs rhs
  | C_emit.Bytes lhs, C_emit.Bytes rhs -> String.equal lhs rhs
  | C_emit.Data lhs, C_emit.Data rhs -> C_rval.equal lhs rhs
  | C_emit.Cap (lk, li), C_emit.Cap (rk, ri) ->
    C_nat.equal lk rk && C_nat.equal li ri
  | _ -> false

let check cfg actual =
  match cfg.expect with
  | None -> true
  | Some wanted -> equal wanted actual

let hex value =
  String.concat "" (List.init (String.length value) (fun index ->
    Printf.sprintf "%02x" (Char.code value.[index])))

let unhex value =
  let nibble = function
    | '0' .. '9' as char -> Some (Char.code char - Char.code '0')
    | 'a' .. 'f' as char -> Some (10 + Char.code char - Char.code 'a')
    | _ -> None
  in
  let len = String.length value in
  if len mod 2 <> 0 then None
  else
    let out = Bytes.create (len / 2) in
    let rec loop index =
      if index = len then Some (Bytes.unsafe_to_string out)
      else
        match nibble value.[index], nibble value.[index + 1] with
        | Some high, Some low ->
          Bytes.set out (index / 2) (Char.chr ((high lsl 4) lor low));
          loop (index + 2)
        | _ -> None
    in
    loop 0

let lit_text = function
  | C_emit.Bool false -> "b:0"
  | C_emit.Bool true -> "b:1"
  | C_emit.Int value -> "i:" ^ Z.to_string value
  | C_emit.Bytes value -> "x:" ^ hex value
  | C_emit.Data value -> "d:" ^ hex (C_rval.encode value)
  | C_emit.Cap (kind, id) ->
    "c:" ^ C_nat.text kind ^ ":" ^ C_nat.text id

let z value =
  try Some (Z.of_string value) with Invalid_argument _ -> None

let lit value =
  if String.equal value "b:0" then Some (C_emit.Bool false)
  else if String.equal value "b:1" then Some (C_emit.Bool true)
  else if String.starts_with ~prefix:"i:" value then
    let raw = String.sub value 2 (String.length value - 2) in
    Option.bind (z raw) (fun number ->
      if String.equal (Z.to_string number) raw then Some (C_emit.Int number)
      else None)
  else if String.starts_with ~prefix:"x:" value then
    let raw = String.sub value 2 (String.length value - 2) in
    Option.bind (unhex raw) (fun bytes ->
      if String.equal (hex bytes) raw then Some (C_emit.Bytes bytes) else None)
  else if String.starts_with ~prefix:"d:" value then
    let raw = String.sub value 2 (String.length value - 2) in
    Option.bind (unhex raw) (fun bytes ->
      match C_rval.decode bytes with
      | Ok item when String.equal (hex bytes) raw -> Some (C_emit.Data item)
      | Ok _ | Error _ -> None)
  else if String.starts_with ~prefix:"c:" value then
    begin
      match String.split_on_char ':' value with
      | ["c"; kind; id] ->
        Option.bind (z kind) (fun kind ->
          Option.bind (z id) (fun id ->
            match C_nat.make kind, C_nat.make id with
            | Some kind, Some id -> Some (C_emit.Cap (kind, id))
            | _ -> None))
      | _ -> None
    end
  else None

let digest value =
  String.length value = 64
  && String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) value

let raw_code value =
  let rec loop index out =
    if index < 0 then out
    else
      loop (index - 1)
        (C_bin.Cons (C_bin.Num (Z.of_int (Char.code value.[index])), out))
  in
  loop (String.length value - 1) C_bin.Nil

let raw_get input =
  let out = Buffer.create 64 in
  let rec loop = function
    | C_bin.Nil -> Some (Buffer.contents out)
    | C_bin.Cons (C_bin.Num value, rest) ->
      let* value = C_nat.byte value in
      Buffer.add_char out (Char.chr value);
      loop rest
    | _ -> None
  in
  loop input

let number value =
  let* value = C_nat.of_int value in
  C_bin.num value

let point_code = function
  | Pc value ->
    let* value = number value in
    Some (C_bin.Tag (Z.zero, value))
  | Line value ->
    let* value = number value in
    Some (C_bin.Tag (Z.one, value))

let point_get = function
  | C_bin.Tag (tag, body) ->
    let* value = C_bin.get_num body in
    if Z.equal tag Z.zero then Some (Pc (C_nat.to_int value))
    else if Z.equal tag Z.one then Some (Line (C_nat.to_int value))
    else None
  | _ -> None

let lit_code = function
  | C_emit.Bool value ->
    Some (C_bin.Tag (Z.zero, C_bin.Num (if value then Z.one else Z.zero)))
  | C_emit.Int value -> Some (C_bin.Tag (Z.one, C_bin.Int value))
  | C_emit.Bytes value -> Some (C_bin.Tag (Z.of_int 2, raw_code value))
  | C_emit.Data value ->
    Some (C_bin.Tag (Z.of_int 3, raw_code (C_rval.encode value)))
  | C_emit.Cap (kind, id) ->
    Some (C_bin.Tag (Z.of_int 4,
      C_bin.Cons (C_bin.Num (C_nat.to_z kind),
        C_bin.Cons (C_bin.Num (C_nat.to_z id), C_bin.Nil))))

let lit_get = function
  | C_bin.Tag (tag, C_bin.Num value) when Z.equal tag Z.zero ->
    if Z.equal value Z.zero then Some (C_emit.Bool false)
    else if Z.equal value Z.one then Some (C_emit.Bool true)
    else None
  | C_bin.Tag (tag, C_bin.Int value) when Z.equal tag Z.one ->
    Some (C_emit.Int value)
  | C_bin.Tag (tag, body) when Z.equal tag (Z.of_int 2) ->
    let* value = raw_get body in
    if valid_lit (C_emit.Bytes value) then Some (C_emit.Bytes value) else None
  | C_bin.Tag (tag, body) when Z.equal tag (Z.of_int 3) ->
    let* value = raw_get body in
    begin
      match C_rval.decode value with
      | Ok item -> Some (C_emit.Data item)
      | Error _ -> None
    end
  | C_bin.Tag (tag,
      C_bin.Cons (C_bin.Num kind, C_bin.Cons (C_bin.Num id, C_bin.Nil)))
      when Z.equal tag (Z.of_int 4) ->
    begin
      match C_nat.make kind, C_nat.make id with
      | Some kind, Some id -> Some (C_emit.Cap (kind, id))
      | _ -> None
    end
  | _ -> None

let expect_code = function
  | None -> Some (C_bin.Tag (Z.zero, C_bin.Nil))
  | Some value ->
    let* value = lit_code value in
    Some (C_bin.Tag (Z.one, value))

let expect_get = function
  | C_bin.Tag (tag, C_bin.Nil) when Z.equal tag Z.zero -> Some None
  | C_bin.Tag (tag, body) when Z.equal tag Z.one ->
    let* value = lit_get body in
    Some (Some value)
  | _ -> None

let config_code value =
  let* cap = number value.cap in
  let* points = C_bin.list_code point_code value.points in
  let* expect = expect_code value.expect in
  Some (C_bin.Tag (Z.of_int 3,
    C_bin.Cons (cap, C_bin.Cons (points, C_bin.Cons (expect, C_bin.Nil)))))

let config_get = function
  | C_bin.Tag (tag,
      C_bin.Cons (cap, C_bin.Cons (points, C_bin.Cons (expect, C_bin.Nil))))
      when Z.equal tag (Z.of_int 3) ->
    let* cap = C_bin.get_num cap in
    let* points = C_bin.list_get point_get points in
    let* expect = expect_get expect in
    begin
      match make ~cap:(C_nat.to_int cap) ~points ~expect with
      | Ok value -> Some value
      | Error _ -> None
    end
  | _ -> None

let state_code value =
  let* config = config_code value.cfg in
  let* seen = number value.seen in
  Some (C_bin.Tag (Z.of_int 4,
    C_bin.Cons (raw_code value.source,
      C_bin.Cons (raw_code value.code,
        C_bin.Cons (config,
          C_bin.Cons (seen, C_bin.Cons (raw_code value.trace, C_bin.Nil)))))))

let bit_text bits =
  let out = Buffer.create (4 + List.length bits) in
  Buffer.add_string out "CD1\n";
  List.iter (fun value -> Buffer.add_char out (if value then '1' else '0')) bits;
  Buffer.contents out

let state_form value =
  let* shape = state_code value in
  let* bits = C_bin.enc_code shape in
  let raw = bit_text bits in
  if String.length raw <= C_nat.str_max then Some raw else None

let session ~source ~code ~cfg ~seen ~trace =
  if not (digest source && digest code && digest trace) then Error Digest
  else if seen < 0 || seen >= cfg.cap then Error Seen
  else
    let value = { source; code; cfg; seen; trace; form = "" } in
    match state_form value with
    | Some form -> Ok { value with form }
    | None -> Error Form

let encode value = value.form

let state_get = function
  | C_bin.Tag (tag, C_bin.Cons (source, C_bin.Cons (code,
      C_bin.Cons (config, C_bin.Cons (seen, C_bin.Cons (trace, C_bin.Nil))))))
      when Z.equal tag (Z.of_int 4) ->
    let* source = raw_get source in
    let* code = raw_get code in
    let* config = config_get config in
    let* seen = C_bin.get_num seen in
    let* trace = raw_get trace in
    begin
      match session ~source ~code ~cfg:config ~seen:(C_nat.to_int seen) ~trace with
      | Ok value -> Some value
      | Error _ -> None
    end
  | _ -> None

let file_bits raw =
  if String.length raw < 4 || not (String.equal (String.sub raw 0 4) "CD1\n")
  then None
  else
    let rec loop index out =
      if index = String.length raw then Some (List.rev out)
      else
        match raw.[index] with
        | '0' -> loop (index + 1) (false :: out)
        | '1' -> loop (index + 1) (true :: out)
        | _ -> None
    in
    loop 4 []

let decode raw =
  if String.length raw > C_nat.str_max then Error Form
  else
    match file_bits raw with
    | None -> Error Form
    | Some bits ->
      begin
        match C_bin.dec_code bits with
        | None -> Error Form
        | Some shape ->
          begin
            match state_get shape with
            | Some value when String.equal (encode value) raw -> Ok value
            | _ -> Error Form
          end
      end

let text = function
  | Cap -> "step cap is invalid"
  | Point -> "breakpoint is invalid"
  | Form -> "debug session form is invalid"
  | Digest -> "debug session digest is invalid"
  | Seen -> "debug session step is invalid"
  | Expect -> "expected result is invalid"