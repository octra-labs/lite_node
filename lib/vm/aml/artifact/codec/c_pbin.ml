(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bits = C_bin.bits

let ( let* ) value next =
  match value with
  | Some value -> next value
  | None -> None

let tag value = Z.of_int value

let mul_code = function
  | C_type.Zero -> C_bin.Tag (Z.zero, C_bin.Nil)
  | C_type.One -> C_bin.Tag (Z.one, C_bin.Nil)
  | C_type.Many -> C_bin.Tag (tag 2, C_bin.Nil)

let mul_get = function
  | C_bin.Tag (value, C_bin.Nil) when Z.equal value Z.zero ->
      Some C_type.Zero
  | C_bin.Tag (value, C_bin.Nil) when Z.equal value Z.one ->
      Some C_type.One
  | C_bin.Tag (value, C_bin.Nil) when Z.equal value (tag 2) ->
      Some C_type.Many
  | _ -> None

let rel_code = function
  | C_term.Lt -> C_bin.Tag (Z.zero, C_bin.Nil)
  | C_term.Le -> C_bin.Tag (Z.one, C_bin.Nil)
  | C_term.Gt -> C_bin.Tag (tag 2, C_bin.Nil)
  | C_term.Ge -> C_bin.Tag (tag 3, C_bin.Nil)

let rel_get = function
  | C_bin.Tag (value, C_bin.Nil) when Z.equal value Z.zero -> Some C_term.Lt
  | C_bin.Tag (value, C_bin.Nil) when Z.equal value Z.one -> Some C_term.Le
  | C_bin.Tag (value, C_bin.Nil) when Z.equal value (tag 2) -> Some C_term.Gt
  | C_bin.Tag (value, C_bin.Nil) when Z.equal value (tag 3) -> Some C_term.Ge
  | _ -> None

let bind_code value =
  let* id = C_bin.num value.C_term.id in
  let* typ = C_bin.ty_code value.C_term.typ in
  Some (C_bin.Tag (Z.zero,
    C_bin.Cons (id,
      C_bin.Cons (mul_code value.C_term.mul, C_bin.Cons (typ, C_bin.Nil)))))

let bind_get = function
  | C_bin.Tag (mark,
      C_bin.Cons (id, C_bin.Cons (mode, C_bin.Cons (typ, C_bin.Nil))))
      when Z.equal mark Z.zero ->
      let* id = C_bin.get_num id in
      let* mode = mul_get mode in
      let* typ = C_bin.ty_get typ in
      Some (C_term.bind id mode typ)
  | _ -> None

let value_unit = C_bin.Tag (Z.zero, C_bin.Nil)
let value_bool value = C_bin.Tag (Z.one, C_bin.Num (if value then Z.one else Z.zero))
let value_int value = C_bin.Tag (tag 2, C_bin.Int value)

let rec lookup id = function
  | [] -> None
  | (key, typ) :: _ when C_nat.equal key id -> Some typ
  | _ :: rest -> lookup id rest

let bind_type env value =
  match value.C_term.mul with
  | C_type.Zero -> env
  | C_type.One | C_type.Many -> (value.C_term.id, value.C_term.typ) :: env

let rec vec_len env = function
  | C_term.Vec (_, values) -> C_nat.of_int (List.length values)
  | C_term.Seq (cap, _, _) -> Some cap
  | C_term.Var id ->
      begin
        match lookup id env with
        | Some (C_type.Vec (len, _)) | Some (C_type.Seq (len, _)) -> Some len
        | _ -> None
      end
  | C_term.Vcat (first, second) ->
      let* first = vec_len env first in
      let* second = vec_len env second in
      C_nat.add first second
  | _ -> None

let rec term_code env = function
  | C_term.Unit ->
      let* typ = C_bin.ty_code C_type.Unit in
      Some (C_bin.Tag (Z.zero,
        C_bin.Cons (value_unit, C_bin.Cons (typ, C_bin.Nil))))
  | C_term.Bool value ->
      let* typ = C_bin.ty_code C_type.Bool in
      Some (C_bin.Tag (Z.zero,
        C_bin.Cons (value_bool value, C_bin.Cons (typ, C_bin.Nil))))
  | C_term.Int value ->
      let* typ = C_bin.ty_code C_type.Int in
      Some (C_bin.Tag (Z.zero,
        C_bin.Cons (value_int value, C_bin.Cons (typ, C_bin.Nil))))
  | C_term.Narrow (typ, value) ->
      let* typ = C_bin.ty_code typ in
      Some (C_bin.Tag (tag 33,
        C_bin.Cons (typ, C_bin.Cons (C_bin.Int value, C_bin.Nil))))
  | C_term.Bytes value ->
      let bytes =
        List.init (String.length value)
          (fun index -> Char.code value.[index])
      in
      let* bytes = C_bin.list_code
          (fun byte -> Some (C_bin.Num (Z.of_int byte))) bytes in
      Some (C_bin.Tag (Z.one, bytes))
  | C_term.Vec (elem, values) -> vec_code env elem values
  | C_term.Seq (cap, elem, values) ->
      let* cap = C_bin.num cap in
      let* elem = C_bin.ty_code elem in
      let* values = C_bin.list_code (term_code env) values in
      Some (C_bin.Tag (tag 34,
        C_bin.Cons (cap, C_bin.Cons (elem, C_bin.Cons (values, C_bin.Nil)))))
  | C_term.Var id ->
      let* id = C_bin.num id in
      Some (C_bin.Tag (tag 3, id))
  | C_term.Let (binder, value, body) ->
      let body_env = bind_type env binder in
      let* binder = bind_code binder in
      let* value = term_code env value in
      let* body = term_code body_env body in
      Some (C_bin.Tag (tag 4,
        C_bin.Cons (binder, C_bin.Cons (value, C_bin.Cons (body, C_bin.Nil)))))
  | C_term.If (guard, yes, no) ->
      let* guard = term_code env guard in
      let* yes = term_code env yes in
      let* no = term_code env no in
      Some (C_bin.Tag (tag 5,
        C_bin.Cons (guard, C_bin.Cons (yes, C_bin.Cons (no, C_bin.Nil)))))
  | C_term.Pair (first, second) -> pair_code env 6 first second
  | C_term.Unpair (pair, first, second, body) ->
      let body_env = bind_type (bind_type env first) second in
      let* pair = term_code env pair in
      let* first = bind_code first in
      let* second = bind_code second in
      let* body = term_code body_env body in
      Some (C_bin.Tag (tag 7,
        C_bin.Cons (pair,
          C_bin.Cons (first, C_bin.Cons (second,
            C_bin.Cons (body, C_bin.Nil))))))
  | C_term.Fst value -> unary_code env 8 value
  | C_term.Snd value -> unary_code env 9 value
  | C_term.Inl (value, other) ->
      let* value = term_code env value in
      let* other = C_bin.ty_code other in
      Some (C_bin.Tag (tag 10,
        C_bin.Cons (value, C_bin.Cons (other, C_bin.Nil))))
  | C_term.Inr (other, value) ->
      let* other = C_bin.ty_code other in
      let* value = term_code env value in
      Some (C_bin.Tag (tag 11,
        C_bin.Cons (other, C_bin.Cons (value, C_bin.Nil))))
  | C_term.Case (value, first, yes, second, no) ->
      let yes_env = bind_type env first in
      let no_env = bind_type env second in
      let* value = term_code env value in
      let* first = bind_code first in
      let* yes = term_code yes_env yes in
      let* second = bind_code second in
      let* no = term_code no_env no in
      Some (C_bin.Tag (tag 12,
        C_bin.Cons (value,
          C_bin.Cons (first, C_bin.Cons (yes,
            C_bin.Cons (second, C_bin.Cons (no, C_bin.Nil)))))))
  | C_term.Act (action, body) ->
      let* action = C_bin.atom_code action in
      let* body = term_code env body in
      Some (C_bin.Tag (tag 13,
        C_bin.Cons (action, C_bin.Cons (body, C_bin.Nil))))
  | C_term.Add (first, second) -> pair_code env 14 first second
  | C_term.Eq (typ, first, second) ->
      let* typ = C_bin.ty_code typ in
      let* first = term_code env first in
      let* second = term_code env second in
      Some (C_bin.Tag (tag 15,
        C_bin.Cons (typ,
          C_bin.Cons (first, C_bin.Cons (second, C_bin.Nil)))))
  | C_term.Cat (first, second) -> pair_code env 16 first second
  | C_term.Take (len, value) -> nat_code env 17 len value
  | C_term.Drop (len, value) -> nat_code env 18 len value
  | C_term.Vcat (first, second) -> pair_code env 20 first second
  | C_term.At (index, value) -> nat_code env 21 index value
  | C_term.Uncons value -> unary_code env 22 value
  | C_term.Vfold (vector, seed, fold) ->
      let body_env =
        bind_type (bind_type env fold.C_term.item) fold.C_term.state
      in
      let* len = vec_len env vector in
      let* vector = term_code env vector in
      let* seed = term_code env seed in
      let* item = bind_code fold.C_term.item in
      let* state = bind_code fold.C_term.state in
      let* body = term_code body_env fold.C_term.body in
      let* len = C_bin.num len in
      Some (C_bin.Tag (tag 23,
        C_bin.Cons (len,
          C_bin.Cons (vector, C_bin.Cons (seed,
            C_bin.Cons (item, C_bin.Cons (state,
              C_bin.Cons (body, C_bin.Nil))))))))
  | C_term.Step (cap, value) -> pair_code env 24 cap value
  | C_term.Close cap -> unary_code env 25 cap
  | C_term.Sub (first, second) -> pair_code env 26 first second
  | C_term.Mul (first, second) -> pair_code env 27 first second
  | C_term.Div (first, second) -> pair_code env 28 first second
  | C_term.Mod (first, second) -> pair_code env 29 first second
  | C_term.Neg value -> unary_code env 30 value
  | C_term.Abs value -> unary_code env 31 value
  | C_term.Fit (typ, value) ->
      let* typ = C_bin.ty_code typ in
      let* value = term_code env value in
      Some (C_bin.Tag (tag 35,
        C_bin.Cons (typ, C_bin.Cons (value, C_bin.Nil))))
  | C_term.Wide value -> unary_code env 36 value
  | C_term.Length value -> unary_code env 37 value
  | C_term.Cmp (rel, first, second) ->
      let* first = term_code env first in
      let* second = term_code env second in
      Some (C_bin.Tag (tag 32,
        C_bin.Cons (rel_code rel,
          C_bin.Cons (first, C_bin.Cons (second, C_bin.Nil)))))

and pair_code env mark first second =
  let* first = term_code env first in
  let* second = term_code env second in
  Some (C_bin.Tag (tag mark,
    C_bin.Cons (first, C_bin.Cons (second, C_bin.Nil))))

and unary_code env mark value =
  let* value = term_code env value in
  Some (C_bin.Tag (tag mark, value))

and nat_code env mark number value =
  let* number = C_bin.num number in
  let* value = term_code env value in
  Some (C_bin.Tag (tag mark,
    C_bin.Cons (number, C_bin.Cons (value, C_bin.Nil))))

and vec_code env elem values =
  let* elem = C_bin.ty_code elem in
  let rec loop out = function
    | [] -> Some out
    | value :: rest ->
        let* value = term_code env value in
        loop
          (C_bin.Tag (tag 19,
            C_bin.Cons (value, C_bin.Cons (out, C_bin.Nil))))
          rest
  in
  loop (C_bin.Tag (tag 2, elem)) (List.rev values)

let rec term_get_f fuel input =
  if fuel = 0 then None
  else
    let next = fuel - 1 in
    match input with
    | C_bin.Tag (mark, C_bin.Cons (value, C_bin.Cons (typ, C_bin.Nil)))
        when Z.equal mark Z.zero ->
        if value = value_unit then
          let* typ = C_bin.ty_get typ in
          if C_type.equal typ C_type.Unit then Some C_term.Unit else None
        else
          begin
            match value with
            | C_bin.Tag (tag, C_bin.Num bit) when Z.equal tag Z.one ->
                let* typ = C_bin.ty_get typ in
                if not (C_type.equal typ C_type.Bool) then None
                else if Z.equal bit Z.zero then Some (C_term.Bool false)
                else if Z.equal bit Z.one then Some (C_term.Bool true)
                else None
            | C_bin.Tag (mark, C_bin.Int value) when Z.equal mark (tag 2) ->
                let* typ = C_bin.ty_get typ in
                if C_type.equal typ C_type.Int then Some (C_term.Int value)
                else None
            | _ -> None
          end
    | C_bin.Tag (mark, bytes) when Z.equal mark Z.one ->
        let* bytes = C_bin.list_get C_bin.get_num bytes in
        let out = Buffer.create (List.length bytes) in
        let rec fill = function
          | [] -> Some (C_term.Bytes (Buffer.contents out))
          | byte :: rest ->
              let value = C_nat.to_int byte in
              if value > 255 then None
              else begin
                Buffer.add_char out (Char.chr value);
                fill rest
              end
        in
        fill bytes
    | C_bin.Tag (mark, C_bin.Cons (typ, C_bin.Cons (C_bin.Int value, C_bin.Nil)))
        when Z.equal mark (tag 33) ->
        let* typ = C_bin.ty_get typ in
        if C_type.admits typ value then Some (C_term.Narrow (typ, value))
        else None
    | C_bin.Tag (mark,
        C_bin.Cons (cap, C_bin.Cons (elem, C_bin.Cons (values, C_bin.Nil))))
        when Z.equal mark (tag 34) ->
        let* cap = C_bin.get_num cap in
        let* elem = C_bin.ty_get elem in
        let* values = term_list_get next 0 [] values in
        if List.length values <= C_nat.to_int cap then
          Some (C_term.Seq (cap, elem, values))
        else None
    | C_bin.Tag (mark, _) when Z.equal mark (tag 2)
        || Z.equal mark (tag 19) -> vec_get next input
    | C_bin.Tag (mark, id) when Z.equal mark (tag 3) ->
        let* id = C_bin.get_num id in
        Some (C_term.Var id)
    | C_bin.Tag (mark,
        C_bin.Cons (binder, C_bin.Cons (value, C_bin.Cons (body, C_bin.Nil))))
        when Z.equal mark (tag 4) ->
        let* binder = bind_get binder in
        let* value = term_get_f next value in
        let* body = term_get_f next body in
        Some (C_term.Let (binder, value, body))
    | C_bin.Tag (mark,
        C_bin.Cons (guard, C_bin.Cons (yes, C_bin.Cons (no, C_bin.Nil))))
        when Z.equal mark (tag 5) ->
        let* guard = term_get_f next guard in
        let* yes = term_get_f next yes in
        let* no = term_get_f next no in
        Some (C_term.If (guard, yes, no))
    | C_bin.Tag (mark, C_bin.Cons (first, C_bin.Cons (second, C_bin.Nil)))
        when Z.equal mark (tag 6) ->
        let* first = term_get_f next first in
        let* second = term_get_f next second in
        Some (C_term.Pair (first, second))
    | C_bin.Tag (mark,
        C_bin.Cons (pair, C_bin.Cons (first,
          C_bin.Cons (second, C_bin.Cons (body, C_bin.Nil)))))
        when Z.equal mark (tag 7) ->
        let* pair = term_get_f next pair in
        let* first = bind_get first in
        let* second = bind_get second in
        let* body = term_get_f next body in
        Some (C_term.Unpair (pair, first, second, body))
    | C_bin.Tag (mark, value) when Z.equal mark (tag 8) ->
        let* value = term_get_f next value in Some (C_term.Fst value)
    | C_bin.Tag (mark, value) when Z.equal mark (tag 9) ->
        let* value = term_get_f next value in Some (C_term.Snd value)
    | C_bin.Tag (mark, C_bin.Cons (value, C_bin.Cons (other, C_bin.Nil)))
        when Z.equal mark (tag 10) ->
        let* value = term_get_f next value in
        let* other = C_bin.ty_get other in
        Some (C_term.Inl (value, other))
    | C_bin.Tag (mark, C_bin.Cons (other, C_bin.Cons (value, C_bin.Nil)))
        when Z.equal mark (tag 11) ->
        let* other = C_bin.ty_get other in
        let* value = term_get_f next value in
        Some (C_term.Inr (other, value))
    | C_bin.Tag (mark,
        C_bin.Cons (value, C_bin.Cons (first, C_bin.Cons (yes,
          C_bin.Cons (second, C_bin.Cons (no, C_bin.Nil))))))
        when Z.equal mark (tag 12) ->
        let* value = term_get_f next value in
        let* first = bind_get first in
        let* yes = term_get_f next yes in
        let* second = bind_get second in
        let* no = term_get_f next no in
        Some (C_term.Case (value, first, yes, second, no))
    | C_bin.Tag (mark, C_bin.Cons (action, C_bin.Cons (body, C_bin.Nil)))
        when Z.equal mark (tag 13) ->
        let* action = C_bin.atom_get action in
        let* body = term_get_f next body in
        Some (C_term.Act (action, body))
    | C_bin.Tag (mark, C_bin.Cons (first, C_bin.Cons (second, C_bin.Nil)))
        when Z.equal mark (tag 14) ->
        let* first = term_get_f next first in
        let* second = term_get_f next second in
        Some (C_term.Add (first, second))
    | C_bin.Tag (mark,
        C_bin.Cons (typ, C_bin.Cons (first, C_bin.Cons (second, C_bin.Nil))))
        when Z.equal mark (tag 15) ->
        let* typ = C_bin.ty_get typ in
        let* first = term_get_f next first in
        let* second = term_get_f next second in
        Some (C_term.Eq (typ, first, second))
    | C_bin.Tag (mark, C_bin.Cons (first, C_bin.Cons (second, C_bin.Nil)))
        when Z.equal mark (tag 16) ->
        let* first = term_get_f next first in
        let* second = term_get_f next second in
        Some (C_term.Cat (first, second))
    | C_bin.Tag (mark, C_bin.Cons (number, C_bin.Cons (value, C_bin.Nil)))
        when Z.equal mark (tag 17) ->
        let* number = C_bin.get_num number in
        let* value = term_get_f next value in
        Some (C_term.Take (number, value))
    | C_bin.Tag (mark, C_bin.Cons (number, C_bin.Cons (value, C_bin.Nil)))
        when Z.equal mark (tag 18) ->
        let* number = C_bin.get_num number in
        let* value = term_get_f next value in
        Some (C_term.Drop (number, value))
    | C_bin.Tag (mark, C_bin.Cons (first, C_bin.Cons (second, C_bin.Nil)))
        when Z.equal mark (tag 20) ->
        let* first = term_get_f next first in
        let* second = term_get_f next second in
        Some (C_term.Vcat (first, second))
    | C_bin.Tag (mark, C_bin.Cons (number, C_bin.Cons (value, C_bin.Nil)))
        when Z.equal mark (tag 21) ->
        let* number = C_bin.get_num number in
        let* value = term_get_f next value in
        Some (C_term.At (number, value))
    | C_bin.Tag (mark, value) when Z.equal mark (tag 22) ->
        let* value = term_get_f next value in Some (C_term.Uncons value)
    | C_bin.Tag (mark,
        C_bin.Cons (C_bin.Num _, C_bin.Cons (vector, C_bin.Cons (seed,
          C_bin.Cons (item, C_bin.Cons (state, C_bin.Cons (body, C_bin.Nil)))))))
        when Z.equal mark (tag 23) ->
        let* vector = term_get_f next vector in
        let* seed = term_get_f next seed in
        let* item = bind_get item in
        let* state = bind_get state in
        let* body = term_get_f next body in
        Some (C_term.Vfold (vector, seed, C_term.fold item state body))
    | C_bin.Tag (mark, C_bin.Cons (cap, C_bin.Cons (value, C_bin.Nil)))
        when Z.equal mark (tag 24) ->
        let* cap = term_get_f next cap in
        let* value = term_get_f next value in
        Some (C_term.Step (cap, value))
    | C_bin.Tag (mark, cap) when Z.equal mark (tag 25) ->
        let* cap = term_get_f next cap in Some (C_term.Close cap)
    | C_bin.Tag (mark, C_bin.Cons (first, C_bin.Cons (second, C_bin.Nil)))
        when Z.equal mark (tag 26) ->
        let* first = term_get_f next first in
        let* second = term_get_f next second in
        Some (C_term.Sub (first, second))
    | C_bin.Tag (mark, C_bin.Cons (first, C_bin.Cons (second, C_bin.Nil)))
        when Z.equal mark (tag 27) ->
        let* first = term_get_f next first in
        let* second = term_get_f next second in
        Some (C_term.Mul (first, second))
    | C_bin.Tag (mark, C_bin.Cons (first, C_bin.Cons (second, C_bin.Nil)))
        when Z.equal mark (tag 28) ->
        let* first = term_get_f next first in
        let* second = term_get_f next second in
        Some (C_term.Div (first, second))
    | C_bin.Tag (mark, C_bin.Cons (first, C_bin.Cons (second, C_bin.Nil)))
        when Z.equal mark (tag 29) ->
        let* first = term_get_f next first in
        let* second = term_get_f next second in
        Some (C_term.Mod (first, second))
    | C_bin.Tag (mark, value) when Z.equal mark (tag 30) ->
        let* value = term_get_f next value in
        Some (C_term.Neg value)
    | C_bin.Tag (mark, value) when Z.equal mark (tag 31) ->
        let* value = term_get_f next value in
        Some (C_term.Abs value)
    | C_bin.Tag (mark,
        C_bin.Cons (rel, C_bin.Cons (first, C_bin.Cons (second, C_bin.Nil))))
        when Z.equal mark (tag 32) ->
        let* rel = rel_get rel in
        let* first = term_get_f next first in
        let* second = term_get_f next second in
        Some (C_term.Cmp (rel, first, second))
    | C_bin.Tag (mark, C_bin.Cons (typ, C_bin.Cons (value, C_bin.Nil)))
        when Z.equal mark (tag 35) ->
        let* typ = C_bin.ty_get typ in
        let* value = term_get_f next value in
        Some (C_term.Fit (typ, value))
    | C_bin.Tag (mark, value) when Z.equal mark (tag 36) ->
        let* value = term_get_f next value in
        Some (C_term.Wide value)
    | C_bin.Tag (mark, value) when Z.equal mark (tag 37) ->
        let* value = term_get_f next value in
        Some (C_term.Length value)
    | _ -> None

and vec_get fuel input =
  let rec loop count out = function
    | _ when count > C_rule.local.tm_nodes -> None
    | C_bin.Tag (mark, elem) when Z.equal mark (tag 2) ->
        let* elem = C_bin.ty_get elem in
        Some (C_term.Vec (elem, List.rev out))
    | C_bin.Tag (mark, C_bin.Cons (first, C_bin.Cons (rest, C_bin.Nil)))
        when Z.equal mark (tag 19) ->
        let* first = term_get_f fuel first in
        loop (count + 1) (first :: out) rest
    | _ -> None
  in
  loop 0 [] input

and term_list_get fuel count out = function
  | C_bin.Nil -> Some (List.rev out)
  | _ when count = C_rule.local.tm_nodes -> None
  | C_bin.Cons (value, rest) ->
      let* value = term_get_f fuel value in
      term_list_get fuel (count + 1) (value :: out) rest
  | _ -> None

let term_get = term_get_f (C_rule.local.tm_depth + 1)

let checked value =
  match C_check.check_in value.C_low.inputs value.C_low.term with
  | Ok _ -> true
  | Error _ -> false

let prog_code value =
  if not (checked value) then None
  else
    let* inputs = C_bin.list_code bind_code value.C_low.inputs in
    let env =
      List.fold_left bind_type [] value.C_low.inputs
    in
    let* term = term_code env value.C_low.term in
    Some (C_bin.Tag (tag 26,
      C_bin.Cons (inputs, C_bin.Cons (term, C_bin.Nil))))

let bind_gets input =
  let rec loop count out = function
    | C_bin.Nil -> Some (List.rev out)
    | _ when count = C_rule.local.inputs -> None
    | C_bin.Cons (first, rest) ->
        let* first = bind_get first in
        loop (count + 1) (first :: out) rest
    | _ -> None
  in
  loop 0 [] input

let prog_get = function
  | C_bin.Tag (mark, C_bin.Cons (inputs, C_bin.Cons (term, C_bin.Nil)))
      when Z.equal mark (tag 26) ->
      let* inputs = bind_gets inputs in
      let* term = term_get term in
      let value = { C_low.inputs; term } in
      if checked value then Some value else None
  | _ -> None

let code = prog_code
let get = prog_get

let enc value =
  let* code = prog_code value in
  C_bin.enc_code code

let valid value = Option.is_some (enc value)

let dec input =
  let* code = C_bin.dec_code input in
  let* value = prog_get code in
  let* exact = enc value in
  if List.equal Bool.equal exact input then Some value else None