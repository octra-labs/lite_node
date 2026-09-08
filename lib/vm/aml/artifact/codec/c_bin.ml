(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bits = bool list

type code =
  | Num of Z.t
  | Int of Z.t
  | Nil
  | Cons of code * code
  | Tag of Z.t * code

type reject =
  | DivideZero
  | ModuloZero

type snap =
  | Refuse
  | Reject of reject
  | Accept of C_type.t * C_eff.atom list * C_limit.t * C_eval.value
      * C_eff.atom list * C_limit.t

type frame =
  | First
  | Second of code
  | Mark of Z.t

let ( let* ) value next =
  match value with
  | Some value -> next value
  | None -> None

let same_bits left right =
  let rec loop left right =
    match left, right with
    | [], [] -> true
    | first :: left, second :: right when Bool.equal first second ->
      loop left right
    | _ -> false
  in
  loop left right

let put_pos value =
  let rec loop value out =
    if Z.equal value Z.one then
      List.rev (List.rev_append [false; false] out)
    else
      let pair =
        if Z.testbit value 0 then [true; false] else [false; true]
      in
      loop (Z.shift_right value 1) (List.rev_append pair out)
  in
  if Z.sign value <= 0 then None else Some (loop value [])

let put_nat value =
  if Z.sign value < 0 then None
  else if Z.equal value Z.zero then Some [false; false]
  else
    let* body = put_pos value in
    Some (false :: true :: body)

let put_z value =
  if Z.equal value Z.zero then Some [false; false]
  else
    let* body = put_pos (Z.abs value) in
    if Z.sign value > 0 then Some (false :: true :: body)
    else Some (true :: false :: body)

let get_pos input =
  let rec loop place value = function
    | false :: false :: rest -> Some (Z.add value place, rest)
    | false :: true :: rest -> loop (Z.shift_left place 1) value rest
    | true :: false :: rest ->
      loop (Z.shift_left place 1) (Z.add value place) rest
    | _ -> None
  in
  loop Z.one Z.zero input

let get_nat = function
  | false :: false :: rest -> Some (Z.zero, rest)
  | false :: true :: rest -> get_pos rest
  | _ -> None

let get_z = function
  | false :: false :: rest -> Some (Z.zero, rest)
  | false :: true :: rest -> get_pos rest
  | true :: false :: rest ->
    let* value, tail = get_pos rest in
    Some (Z.neg value, tail)
  | _ -> None

let put_code root =
  let add bits out = List.rev_append bits out in
  let rec loop out = function
    | [] -> Some (List.rev out)
    | Num value :: rest ->
      let* body = put_nat value in
      loop (add (false :: false :: body) out) rest
    | Int value :: rest ->
      let* body = put_z value in
      loop (add (false :: true :: body) out) rest
    | Nil :: rest -> loop (add [true; false] out) rest
    | Cons (first, second) :: rest ->
      loop (add [true; true; false] out) (first :: second :: rest)
    | Tag (tag, body) :: rest ->
      let* number = put_nat tag in
      loop (add (true :: true :: true :: number) out) (body :: rest)
  in
  loop [] [root]

let rec parse fuel input frames =
  if fuel = 0 then None
  else
    let fuel = fuel - 1 in
    match input with
    | false :: false :: rest ->
      let* value, tail = get_nat rest in
      reduce fuel tail frames (Num value)
    | false :: true :: rest ->
      let* value, tail = get_z rest in
      reduce fuel tail frames (Int value)
    | true :: false :: rest -> reduce fuel rest frames Nil
    | true :: true :: false :: rest -> parse fuel rest (First :: frames)
    | true :: true :: true :: rest ->
      let* tag, tail = get_nat rest in
      parse fuel tail (Mark tag :: frames)
    | _ -> None

and reduce fuel input frames value =
  match frames with
  | [] -> Some (value, input)
  | First :: rest -> parse fuel input (Second value :: rest)
  | Second first :: rest -> reduce fuel input rest (Cons (first, value))
  | Mark tag :: rest -> reduce fuel input rest (Tag (tag, value))

let get_code input =
  let size = List.length input in
  if size > C_nat.str_max then None
  else
    match parse (size + 1) input [] with
    | Some (value, []) ->
      let* exact = put_code value in
      if same_bits exact input then Some value else None
    | _ -> None

let enc put value =
  let* shape = put value in
  let* bits = put_code shape in
  if List.length bits <= C_nat.str_max then Some bits else None

let dec put get input =
  let* shape = get_code input in
  let* value = get shape in
  let* exact_shape = put value in
  let* exact = put_code exact_shape in
  if same_bits exact input then Some value else None

let num value =
  if C_nat.valid value then Some (Num (C_nat.to_z value)) else None

let get_num = function
  | Num value -> C_nat.make value
  | _ -> None

let rec ty_code = function
  | C_type.Unit -> Some (Tag (Z.zero, Nil))
  | C_type.Bool -> Some (Tag (Z.one, Nil))
  | C_type.Int -> Some (Tag (Z.of_int 2, Nil))
  | C_type.Bytes len ->
    let* len = num len in
    Some (Tag (Z.of_int 3, len))
  | C_type.Vec (len, elem) ->
    let* len = num len in
    let* elem = ty_code elem in
    Some (Tag (Z.of_int 4, Cons (len, Cons (elem, Nil))))
  | C_type.Cap kind ->
    let* kind = num kind in
    Some (Tag (Z.of_int 5, kind))
  | C_type.Pair (first, second) ->
    let* first = ty_code first in
    let* second = ty_code second in
    Some (Tag (Z.of_int 6, Cons (first, Cons (second, Nil))))
  | C_type.Sum (first, second) ->
    let* first = ty_code first in
    let* second = ty_code second in
    Some (Tag (Z.of_int 7, Cons (first, Cons (second, Nil))))
  | C_type.Enc (key, rem) ->
    let* key = num key in
    let* rem = num rem in
    Some (Tag (Z.of_int 8, Cons (key, Cons (rem, Nil))))
  | C_type.Num (sign, bits) ->
    let sign = match sign with C_type.Signed -> Z.zero | C_type.Unsigned -> Z.one in
    let* bits = num bits in
    Some (Tag (Z.of_int 9, Cons (Num sign, Cons (bits, Nil))))
  | C_type.Seq (cap, elem) ->
    let* cap = num cap in
    let* elem = ty_code elem in
    Some (Tag (Z.of_int 10, Cons (cap, Cons (elem, Nil))))

let rec ty_get_f fuel input =
  if fuel = 0 then None
  else
    let fuel = fuel - 1 in
    match input with
    | Tag (tag, Nil) when Z.equal tag Z.zero -> Some C_type.Unit
    | Tag (tag, Nil) when Z.equal tag Z.one -> Some C_type.Bool
    | Tag (tag, Nil) when Z.equal tag (Z.of_int 2) -> Some C_type.Int
    | Tag (tag, body) when Z.equal tag (Z.of_int 3) ->
      let* len = get_num body in
      Some (C_type.Bytes len)
    | Tag (tag, Cons (len, Cons (elem, Nil))) when Z.equal tag (Z.of_int 4) ->
      let* len = get_num len in
      let* elem = ty_get_f fuel elem in
      Some (C_type.Vec (len, elem))
    | Tag (tag, body) when Z.equal tag (Z.of_int 5) ->
      let* kind = get_num body in
      Some (C_type.Cap kind)
    | Tag (tag, Cons (first, Cons (second, Nil)))
        when Z.equal tag (Z.of_int 6) ->
      let* first = ty_get_f fuel first in
      let* second = ty_get_f fuel second in
      Some (C_type.Pair (first, second))
    | Tag (tag, Cons (first, Cons (second, Nil)))
        when Z.equal tag (Z.of_int 7) ->
      let* first = ty_get_f fuel first in
      let* second = ty_get_f fuel second in
      Some (C_type.Sum (first, second))
    | Tag (tag, Cons (key, Cons (rem, Nil))) when Z.equal tag (Z.of_int 8) ->
      let* key = get_num key in
      let* rem = get_num rem in
      Some (C_type.Enc (key, rem))
    | Tag (tag, Cons (Num sign, Cons (bits, Nil)))
        when Z.equal tag (Z.of_int 9) ->
      let* sign =
        if Z.equal sign Z.zero then Some C_type.Signed
        else if Z.equal sign Z.one then Some C_type.Unsigned
        else None
      in
      let* bits = get_num bits in
      let typ = C_type.Num (sign, bits) in
      if C_type.valid typ then Some typ else None
    | Tag (tag, Cons (cap, Cons (elem, Nil))) when Z.equal tag (Z.of_int 10) ->
      let* cap = get_num cap in
      let* elem = ty_get_f fuel elem in
      let typ = C_type.Seq (cap, elem) in
      if C_type.valid typ then Some typ else None
    | _ -> None

let ty_get = ty_get_f (C_type.max_depth + 1)

let atom_code = function
  | C_eff.Read kind ->
    let* kind = num kind in
    Some (Tag (Z.zero, kind))
  | C_eff.Write kind ->
    let* kind = num kind in
    Some (Tag (Z.one, kind))
  | C_eff.Emit kind ->
    let* kind = num kind in
    Some (Tag (Z.of_int 2, kind))
  | C_eff.Fail kind ->
    let* kind = num kind in
    Some (Tag (Z.of_int 3, kind))
  | C_eff.Close kind ->
    let* kind = num kind in
    Some (Tag (Z.of_int 4, kind))

let atom_get = function
  | Tag (tag, body) ->
    let* kind = get_num body in
    if Z.equal tag Z.zero then Some (C_eff.Read kind)
    else if Z.equal tag Z.one then Some (C_eff.Write kind)
    else if Z.equal tag (Z.of_int 2) then Some (C_eff.Emit kind)
    else if Z.equal tag (Z.of_int 3) then Some (C_eff.Fail kind)
    else if Z.equal tag (Z.of_int 4) then Some (C_eff.Close kind)
    else None
  | _ -> None

let list_code put values =
  let rec loop out = function
    | [] -> Some out
    | value :: rest ->
      let* value = put value in
      loop (Cons (value, out)) rest
  in
  loop Nil (List.rev values)

let list_get get input =
  let rec loop out = function
    | Nil -> Some (List.rev out)
    | Cons (first, rest) ->
      let* value = get first in
      loop (value :: out) rest
    | _ -> None
  in
  loop [] input

let bytes_code value =
  let rec loop index out =
    if index < 0 then out
    else loop (index - 1) (Cons (Num (Z.of_int (Char.code value.[index])), out))
  in
  loop (String.length value - 1) Nil

let bytes_get input =
  let out = Buffer.create 32 in
  let rec loop = function
    | Nil -> Some (Buffer.contents out)
    | Cons (Num value, rest) ->
      let* value = C_nat.byte value in
      Buffer.add_char out (Char.chr value);
      loop rest
    | _ -> None
  in
  loop input

let value_ok value =
  let rec loop nodes = function
    | [] -> true
    | _ when nodes >= C_nat.max -> false
    | (depth, _) :: _ when depth > C_type.max_depth -> false
    | (depth, value) :: rest ->
      let next = depth + 1 in
      begin
        match value with
        | C_eval.Unit | C_eval.Bool _ | C_eval.Int _ -> loop (nodes + 1) rest
        | C_eval.Bytes value ->
          String.length value <= C_nat.max && loop (nodes + 1) rest
        | C_eval.Vec values ->
          Array.length values <= C_nat.max
          && loop (nodes + 1)
            (Array.fold_left (fun out value -> (next, value) :: out) rest values)
        | C_eval.Cap (kind, id) ->
          C_nat.valid kind && C_nat.valid id && loop (nodes + 1) rest
        | C_eval.Enc (key, rem, _) ->
          C_nat.valid key && C_nat.valid rem && loop (nodes + 1) rest
        | C_eval.Pair (first, second) ->
          loop (nodes + 1) ((next, first) :: (next, second) :: rest)
        | C_eval.Inl value | C_eval.Inr value ->
          loop (nodes + 1) ((next, value) :: rest)
      end
  in
  loop 0 [0, value]

let rec value_code = function
  | C_eval.Unit -> Some (Tag (Z.zero, Nil))
  | C_eval.Bool false -> Some (Tag (Z.one, Num Z.zero))
  | C_eval.Bool true -> Some (Tag (Z.one, Num Z.one))
  | C_eval.Int value -> Some (Tag (Z.of_int 2, Int value))
  | C_eval.Bytes value -> Some (Tag (Z.of_int 3, bytes_code value))
  | C_eval.Vec values ->
    let rec loop index out =
      if index < 0 then Some out
      else
        let* value = value_code values.(index) in
        loop (index - 1) (Cons (value, out))
    in
    let* values = loop (Array.length values - 1) Nil in
    Some (Tag (Z.of_int 4, values))
  | C_eval.Cap (kind, id) ->
    let* kind = num kind in
    let* id = num id in
    Some (Tag (Z.of_int 5, Cons (kind, Cons (id, Nil))))
  | C_eval.Pair (first, second) ->
    let* first = value_code first in
    let* second = value_code second in
    Some (Tag (Z.of_int 6, Cons (first, Cons (second, Nil))))
  | C_eval.Inl value ->
    let* value = value_code value in
    Some (Tag (Z.of_int 7, value))
  | C_eval.Inr value ->
    let* value = value_code value in
    Some (Tag (Z.of_int 8, value))
  | C_eval.Enc (key, rem, field) ->
    let* key = num key in
    let* rem = num rem in
    Some (Tag (Z.of_int 9,
      Cons (key, Cons (rem, Cons (Int (C_fp.to_z field), Nil)))))

let rec value_get_f fuel input =
  if fuel = 0 then None
  else
    let fuel = fuel - 1 in
    match input with
    | Tag (tag, Nil) when Z.equal tag Z.zero -> Some C_eval.Unit
    | Tag (tag, Num value) when Z.equal tag Z.one && Z.equal value Z.zero ->
      Some (C_eval.Bool false)
    | Tag (tag, Num value) when Z.equal tag Z.one && Z.equal value Z.one ->
      Some (C_eval.Bool true)
    | Tag (tag, Int value) when Z.equal tag (Z.of_int 2) ->
      Some (C_eval.Int value)
    | Tag (tag, body) when Z.equal tag (Z.of_int 3) ->
      let* value = bytes_get body in
      Some (C_eval.Bytes value)
    | Tag (tag, body) when Z.equal tag (Z.of_int 4) ->
      let* values = list_get (value_get_f fuel) body in
      Some (C_eval.Vec (Array.of_list values))
    | Tag (tag, Cons (kind, Cons (id, Nil))) when Z.equal tag (Z.of_int 5) ->
      let* kind = get_num kind in
      let* id = get_num id in
      Some (C_eval.Cap (kind, id))
    | Tag (tag, Cons (first, Cons (second, Nil)))
        when Z.equal tag (Z.of_int 6) ->
      let* first = value_get_f fuel first in
      let* second = value_get_f fuel second in
      Some (C_eval.Pair (first, second))
    | Tag (tag, body) when Z.equal tag (Z.of_int 7) ->
      let* value = value_get_f fuel body in
      Some (C_eval.Inl value)
    | Tag (tag, body) when Z.equal tag (Z.of_int 8) ->
      let* value = value_get_f fuel body in
      Some (C_eval.Inr value)
    | Tag (tag, Cons (key, Cons (rem, Cons (Int field, Nil))))
        when Z.equal tag (Z.of_int 9) ->
      let* key = get_num key in
      let* rem = get_num rem in
      Some (C_eval.Enc (key, rem, C_fp.of_z field))
    | _ -> None

let value_get = value_get_f (C_type.max_depth + 1)

let row_code = list_code atom_code
let row_get = list_get atom_get

let res_code value =
  if Z.sign value.C_limit.steps < 0
    || Z.sign value.C_limit.depth < 0
    || Z.sign value.C_limit.work < 0 then None
  else
    Some (Tag (Z.zero,
      Cons (Num value.C_limit.steps,
        Cons (Num value.C_limit.depth, Cons (Num value.C_limit.work, Nil)))))

let res_get = function
  | Tag (tag, Cons (Num steps, Cons (Num depth, Cons (Num work, Nil))))
      when Z.equal tag Z.zero ->
    Some (C_limit.make3 ~steps ~depth ~work)
  | _ -> None

let reject_code = function
  | DivideZero -> Num Z.zero
  | ModuloZero -> Num Z.one

let reject_get = function
  | Num value when Z.equal value Z.zero -> Some DivideZero
  | Num value when Z.equal value Z.one -> Some ModuloZero
  | _ -> None

let snap_code = function
  | Refuse -> Some (Tag (Z.zero, Nil))
  | Reject reason -> Some (Tag (Z.of_int 2, reject_code reason))
  | Accept (typ, row, limit, out, plan, used) ->
    if not (C_type.valid typ && value_ok out) then None
    else
    let* typ = ty_code typ in
    let* row = row_code row in
    let* limit = res_code limit in
    let* out = value_code out in
    let* plan = row_code plan in
    let* used = res_code used in
    Some (Tag (Z.one,
      Cons (typ, Cons (row, Cons (limit,
        Cons (out, Cons (plan, Cons (used, Nil))))))))

let snap_get = function
  | Tag (tag, Nil) when Z.equal tag Z.zero -> Some Refuse
  | Tag (tag, body) when Z.equal tag (Z.of_int 2) ->
    let* reason = reject_get body in
    Some (Reject reason)
  | Tag (tag,
      Cons (typ, Cons (row, Cons (limit,
        Cons (out, Cons (plan, Cons (used, Nil))))))) when Z.equal tag Z.one ->
    let* typ = ty_get typ in
    let* row = row_get row in
    let* limit = res_get limit in
    let* out = value_get out in
    let* plan = row_get plan in
    let* used = res_get used in
    Some (Accept (typ, row, limit, out, plan, used))
  | _ -> None

let ty_put value = if C_type.valid value then ty_code value else None
let value_put value = if value_ok value then value_code value else None

let enc_ty = enc ty_put
let dec_ty = dec ty_put ty_get
let enc_value = enc value_put
let dec_value = dec value_put value_get
let enc_row = enc row_code
let dec_row = dec row_code row_get
let enc_res = enc res_code
let dec_res = dec res_code res_get
let enc_snap = enc snap_code
let dec_snap = dec snap_code snap_get

let enc_code value =
  let* bits = put_code value in
  if List.length bits <= C_nat.str_max then Some bits else None

let dec_code = get_code

let u32 value =
  if value < 0 || value > C_nat.str_max then None
  else
    Some (String.init 4 (fun index ->
      Char.chr ((value lsr ((3 - index) * 8)) land 255)))

let pack bits =
  let size = List.length bits in
  if size > C_nat.str_max then None
  else
    let out = Bytes.make ((size + 7) / 8) '\000' in
    let rec loop index count byte = function
      | [] ->
        if count <> 0 then
          Bytes.set out index (Char.chr (byte lsl (8 - count)));
        Some (Bytes.unsafe_to_string out)
      | bit :: rest ->
        let byte = (byte lsl 1) lor if bit then 1 else 0 in
        if count = 7 then begin
          Bytes.set out index (Char.chr byte);
          loop (index + 1) 0 0 rest
        end else loop index (count + 1) byte rest
    in
    loop 0 0 0 bits

let unpack size raw =
  if size < 0 || size > C_nat.str_max
      || String.length raw <> (size + 7) / 8 then None
  else
    let rec loop index out =
      if index = size then
        let bits = List.rev out in
        begin
          match pack bits with
          | Some exact when String.equal exact raw -> Some bits
          | Some _ | None -> None
        end
      else
        let byte = Char.code raw.[index / 8] in
        let shift = 7 - index mod 8 in
        let bit = byte land (1 lsl shift) <> 0 in
        loop (index + 1) (bit :: out)
    in
    loop 0 []

let transcript records =
  let count = List.length records in
  let* count = u32 count in
  let rec loop out = function
    | [] -> Some (String.concat "" (count :: List.rev out))
    | bits :: rest ->
      let* size = u32 (List.length bits) in
      let* body = pack bits in
      loop ((size ^ body) :: out) rest
  in
  loop [] records