(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bits = C_bin.bits

let ( let* ) value next =
  match value with
  | Some value -> next value
  | None -> None

let tag value = Z.of_int value

let rec codes = function
  | [] -> C_bin.Nil
  | value :: rest -> C_bin.Cons (value, codes rest)

let text_code value =
  let rec loop index out =
    if index < 0 then out
    else
      loop (index - 1)
        (C_bin.Cons (C_bin.Num (Z.of_int (Char.code value.[index])), out))
  in
  loop (String.length value - 1) C_bin.Nil

let text_get input =
  let out = Buffer.create 32 in
  let rec loop = function
    | C_bin.Nil -> Some (Buffer.contents out)
    | C_bin.Cons (C_bin.Num value, rest) ->
        let* value = C_nat.byte value in
        Buffer.add_char out (Char.chr value);
        loop rest
    | _ -> None
  in
  loop input

let rid_code value =
  C_bin.Tag (Z.zero,
    codes (List.map (fun value -> C_bin.Num (Z.of_int value))
      (C_rule.id_values value)))

let same_num value actual = Z.equal value (Z.of_int actual)

let rid_get = function
  | C_bin.Tag (mark, body) when Z.equal mark Z.zero ->
      let* values = C_bin.list_get (function
        | C_bin.Num value -> Some value
        | _ -> None) body in
      begin
        match values with
        | [version; nat; str; text; ty_depth; ty_nodes; tm_depth; tm_nodes;
            inputs; fuel; name; data_tag]
            when same_num nat C_rule.local.nat
              && same_num str C_rule.local.str
              && same_num text C_rule.local.text
              && same_num ty_depth C_rule.local.ty_depth
              && same_num ty_nodes C_rule.local.ty_nodes
              && same_num tm_depth C_rule.local.tm_depth
              && same_num tm_nodes C_rule.local.tm_nodes
              && same_num inputs C_rule.local.inputs
              && same_num fuel C_rule.local.fuel
              && same_num name C_rule.local.name
              && same_num data_tag C_rule.local.tag ->
            let* version = C_nat.make version in
            begin
              match C_rule.rule ~version:(C_nat.to_int version)
                  ~activate:Z.zero C_rule.local with
              | Ok value -> Some (C_rule.id value)
              | Error _ -> None
            end
        | _ -> None
      end
  | _ -> None

let target_code = function
  | C_proj.Octb1 -> C_bin.Tag (Z.zero, C_bin.Nil)
  | C_proj.Ocps1 -> C_bin.Tag (Z.one, C_bin.Nil)

let target_get = function
  | C_bin.Tag (mark, C_bin.Nil) when Z.equal mark Z.zero ->
      Some C_proj.Octb1
  | C_bin.Tag (mark, C_bin.Nil) when Z.equal mark Z.one ->
      Some C_proj.Ocps1
  | _ -> None

let src_code (value : C_proj.src) =
  C_bin.Tag (Z.one,
    codes [text_code value.path; text_code value.body;
      codes (List.map text_code value.deps)])

let src_get = function
  | C_bin.Tag (mark,
      C_bin.Cons (path, C_bin.Cons (body, C_bin.Cons (deps, C_bin.Nil))))
      when Z.equal mark Z.one ->
      let* path = text_get path in
      let* body = text_get body in
      let* deps = C_bin.list_get text_get deps in
      Some (C_proj.source ~path ~body ~deps)
  | _ -> None

let root_code (value : C_proj.root) =
  C_bin.Tag (tag 2,
    codes [text_code value.path; text_code value.name; text_code value.feed])

let root_get = function
  | C_bin.Tag (mark,
      C_bin.Cons (path, C_bin.Cons (name, C_bin.Cons (feed, C_bin.Nil))))
      when Z.equal mark (tag 2) ->
      let* path = text_get path in
      let* name = text_get name in
      let* feed = text_get feed in
      Some (C_proj.root_feed ~path ~name ~feed)
  | _ -> None

let image_code value =
  let rule = C_proj.rule value in
  if C_rule.version rule > C_rule.local.nat then None
  else
    Some (C_bin.Tag (tag 3,
      codes [codes (List.map src_code (C_proj.srcs value));
        codes (List.map root_code (C_proj.roots value));
        rid_code rule; target_code (C_proj.target value)]))

let image_get = function
  | C_bin.Tag (mark,
      C_bin.Cons (srcs,
        C_bin.Cons (roots, C_bin.Cons (rule, C_bin.Cons (target, C_bin.Nil)))))
      when Z.equal mark (tag 3) ->
      let* srcs = C_bin.list_get src_get srcs in
      let* roots = C_bin.list_get root_get roots in
      let* rule = rid_get rule in
      let* target = target_get target in
      begin
        match C_proj.make ~rule ~target ~srcs ~roots with
        | Ok value -> Some value
        | Error _ -> None
      end
  | _ -> None

let code = image_code
let get = image_get

let enc value =
  let* code = image_code value in
  C_bin.enc_code code

let valid value = Option.is_some (enc value)

let dec input =
  let* code = C_bin.dec_code input in
  let* value = image_get code in
  let* exact = enc value in
  if List.equal Bool.equal exact input then Some value else None