(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Smap = Map.Make (String)

type cipher = {
  key : C_nat.t;
  rem : C_nat.t;
  value : C_fp.t;
}

type input = {
  name : C_syn.name;
  raw_key : Z.t;
  raw_rem : Z.t;
  raw_value : Z.t;
}

type env = {
  vals : cipher Smap.t;
  types : C_fhe.env;
}

type run = {
  info : C_fhe.info;
  param : C_fhe.param;
  cipher : cipher;
}

type error =
  | Fhe of C_fhe.error
  | Free of string
  | Need of cipher * cipher
  | Mul_zero of C_nat.t
  | Re_need of C_nat.t * C_nat.t
  | Type of cipher * C_fhe.enc

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let fhe = function
  | Ok value -> Ok value
  | Error error -> Error (Fhe error)

let input name ~key ~rem ~value =
  { name; raw_key = key; raw_rem = rem; raw_value = value }

let nat field value =
  match C_nat.make value with
  | Some value -> Ok value
  | None -> Error (Fhe (C_fhe.Nat (field, value)))

let make ~key ~rem ~value =
  let* key = nat C_fhe.Key key in
  let* rem = nat C_fhe.Depth rem in
  Ok { key; rem; value = C_fp.of_z value }

let equal left right =
  C_nat.equal left.key right.key
  && C_nat.equal left.rem right.rem
  && C_fp.equal left.value right.value

let view value = value.key, value.rem, value.value

let core value =
  C_type.Enc (value.key, value.rem),
  C_eval.Enc (value.key, value.rem, value.value)

let env profile inputs =
  let types =
    List.map
      (fun value ->
        C_fhe.input value.name ~key:value.raw_key ~rem:value.raw_rem)
      inputs
  in
  let* types = fhe (C_fhe.env profile types) in
  let rec build vals = function
    | [] -> Ok { vals; types }
    | item :: rest ->
        let* key = nat C_fhe.Key item.raw_key in
        let* rem = nat C_fhe.Depth item.raw_rem in
        let value = { key; rem; value = C_fp.of_z item.raw_value } in
        let name = C_syn.name_text item.name in
        build (Smap.add name value vals) rest
  in
  build Smap.empty inputs

let same left right =
  C_nat.equal left.key right.key && C_nat.equal left.rem right.rem

let plain env term =
  let rec run term =
    match C_fhe.node term with
        | C_fhe.Nvar name ->
            Smap.find_opt (C_syn.name_text name) env.vals
            |> Option.map (fun value -> value.value)
        | C_fhe.Ntrim (_, body) | C_fhe.Nre body -> run body
        | C_fhe.Nadd (left, right) ->
            begin
              match run left, run right with
              | Some left, Some right -> Some (C_fp.add left right)
              | _, _ -> None
            end
        | C_fhe.Nmul (left, right) ->
            begin
              match run left, run right with
              | Some left, Some right -> Some (C_fp.mul left right)
              | _, _ -> None
            end
  in
  run term

let eval profile env term =
  let rec run term =
    match C_fhe.node term with
    | C_fhe.Nvar name ->
        begin
          match Smap.find_opt (C_syn.name_text name) env.vals with
          | Some value -> Ok value
          | None -> Error (Free (C_syn.name_text name))
        end
    | C_fhe.Ntrim (raw, body) ->
        let* prior = run body in
        let* rem = nat C_fhe.Depth raw in
        if not (C_nat.le rem prior.rem) then
          Error (Fhe (C_fhe.Idepth (prior.key, rem, prior.rem)))
        else
          let* _ = fhe (C_fhe.full profile prior.key) in
          Ok { prior with rem }
    | C_fhe.Nadd (left, right) ->
        let* left = run left in
        let* right = run right in
        if not (same left right) then
          Error (Need (left, right))
        else
          let* _ = fhe (C_fhe.full profile left.key) in
          Ok { left with value = C_fp.add left.value right.value }
    | C_fhe.Nmul (left, right) ->
        let* left = run left in
        let* right = run right in
        if not (same left right) then
          Error (Need (left, right))
        else if C_nat.equal left.rem C_nat.zero then
          Error (Mul_zero left.key)
        else
          let* _ = fhe (C_fhe.full profile left.key) in
          let rem =
            match C_nat.sub left.rem C_nat.one with
            | Some rem -> rem
            | None -> C_nat.zero
          in
          Ok { left with rem; value = C_fp.mul left.value right.value }
    | C_fhe.Nre body ->
        let* prior = run body in
        if not (C_nat.equal prior.rem C_nat.zero) then
          Error (Re_need (prior.key, prior.rem))
        else
          let* rem = fhe (C_fhe.full profile prior.key) in
          Ok { prior with rem }
  in
  run term

let exec profile catalog env term =
  let* info = fhe (C_fhe.check profile env.types term) in
  let* param = fhe (C_fhe.params catalog info) in
  let* cipher = eval profile env term in
  if C_nat.equal cipher.key info.typ.key
      && C_nat.equal cipher.rem info.typ.rem then
    Ok { info; param; cipher }
  else Error (Type (cipher, info.typ))

let text = function
  | Fhe error -> C_fhe.text error
  | Free name -> "cipher input is free = " ^ name
  | Need (left, right) ->
      "cipher types differ left = enc[" ^ C_nat.text left.key
      ^ "," ^ C_nat.text left.rem ^ "] right = enc["
      ^ C_nat.text right.key ^ "," ^ C_nat.text right.rem ^ "]"
  | Mul_zero key ->
      "cipher multiplication depth exhausted key = " ^ C_nat.text key
  | Re_need (key, rem) ->
      "cipher reset requires zero depth key = " ^ C_nat.text key
      ^ " remaining = " ^ C_nat.text rem
  | Type (value, typ) ->
      "cipher type differs dynamic = enc[" ^ C_nat.text value.key
      ^ "," ^ C_nat.text value.rem ^ "] static = " ^ C_fhe.enc_text typ