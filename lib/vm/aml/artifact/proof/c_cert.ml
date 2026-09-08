(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bits = C_bin.bits

type error =
  | Check of C_check.rule_error
  | Encode

type cert = {
  prog : C_low.prog;
  rule : Z.t list;
  typ : C_type.t;
  row : C_eff.atom list;
  res : C_limit.t;
}

let ( let* ) value next =
  match value with
  | Some value -> next value
  | None -> None

let rec rule_equal left right =
  match left, right with
  | [], [] -> true
  | first :: left, second :: right when Z.equal first second ->
    rule_equal left right
  | _ -> false

let atom_equal left right =
  match left, right with
  | C_eff.Read first, C_eff.Read second
  | C_eff.Write first, C_eff.Write second
  | C_eff.Emit first, C_eff.Emit second
  | C_eff.Fail first, C_eff.Fail second
  | C_eff.Close first, C_eff.Close second -> C_nat.equal first second
  | _ -> false

let rec atom_list_equal left right =
  match left, right with
  | [], [] -> true
  | first :: left, second :: right when atom_equal first second ->
    atom_list_equal left right
  | _ -> false

let rule_code values =
  let put value =
    if Z.sign value < 0 then None else Some (C_bin.Num value)
  in
  C_bin.list_code put values

let rule_get input =
  let get = function
    | C_bin.Num value when Z.sign value >= 0 -> Some value
    | _ -> None
  in
  let* values = C_bin.list_get get input in
  let rec loop count out = function
    | [] when count = 12 -> Some (List.rev out)
    | [] -> None
    | _ when count = 12 -> None
    | value :: rest -> loop (count + 1) (value :: out) rest
  in
  loop 0 [] values

let cert_code value =
  let* prog = C_pbin.code value.prog in
  let* rule = rule_code value.rule in
  let* typ = C_bin.ty_code value.typ in
  let* row = C_bin.list_code C_bin.atom_code value.row in
  let* res = C_bin.res_code value.res in
  Some (C_bin.Tag (Z.zero,
    C_bin.Cons (prog, C_bin.Cons (rule, C_bin.Cons (typ,
      C_bin.Cons (row, C_bin.Cons (res, C_bin.Nil)))))))

let cert_get = function
  | C_bin.Tag (tag,
      C_bin.Cons (prog, C_bin.Cons (rule, C_bin.Cons (typ,
        C_bin.Cons (row, C_bin.Cons (res, C_bin.Nil))))))
      when Z.equal tag Z.zero ->
    let* prog = C_pbin.get prog in
    let* rule = rule_get rule in
    let* typ = C_bin.ty_get typ in
    let* row = C_bin.list_get C_bin.atom_get row in
    let* res = C_bin.res_get res in
    Some { prog; rule; typ; row; res }
  | _ -> None

let enc value =
  let* code = cert_code value in
  C_bin.enc_code code

let dec input =
  let* code = C_bin.dec_code input in
  let* value = cert_get code in
  let* exact = enc value in
  if List.equal Bool.equal exact input then Some value else None

let issue schedule ~epoch prog =
  match C_check.check_in_at schedule ~epoch prog.C_low.inputs prog.C_low.term with
  | Error error -> Error (Check error)
  | Ok selected ->
    let value = {
      prog;
      rule = List.map Z.of_int (C_rule.id_values selected.rule);
      typ = selected.info.typ;
      row = C_eff.to_list selected.info.eff;
      res = selected.info.res;
    } in
    begin
      match enc value with
      | Some bits -> Ok bits
      | None -> Error Encode
    end

let verify schedule ~epoch input =
  match dec input with
  | None -> false
  | Some value ->
    begin
      match C_check.check_in_at schedule ~epoch
          value.prog.C_low.inputs value.prog.C_low.term with
      | Error _ -> false
      | Ok selected ->
        rule_equal value.rule
          (List.map Z.of_int (C_rule.id_values selected.rule))
        && C_type.equal value.typ selected.info.typ
        && atom_list_equal value.row (C_eff.to_list selected.info.eff)
        && C_limit.equal value.res selected.info.res
    end

let text = function
  | Check error -> C_check.rule_text error
  | Encode -> "certificate encoding failed"