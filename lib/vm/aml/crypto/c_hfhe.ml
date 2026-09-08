(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type adm = {
  id : C_nat.t;
  terms : C_nat.t;
  out : C_nat.t;
  slots : C_nat.t;
  queries : C_nat.t;
  alpha : C_nat.t;
  row : C_nat.t;
}

type proof = Field

type pplan = {
  rounds : Z.t;
  sound : Z.t;
  commits : Z.t;
  opens : Z.t;
  trace : Z.t;
  cbytes : Z.t;
  obytes : Z.t;
  tbytes : Z.t;
  total : Z.t;
}

type cert = {
  proof : proof;
  bits : Z.t;
  traces : Z.t;
  tags : Z.t;
  bytes : Z.t;
  plan : pplan;
}

type shape = {
  terms : C_nat.t;
  slots : C_nat.t;
  query : C_nat.t;
  tags : C_nat.t;
}

type input = {
  name : C_syn.name;
  raw_terms : Z.t;
  raw_slots : Z.t;
  raw_query : Z.t;
}

module Smap = Map.Make (String)

type env = shape Smap.t

type info = {
  shape : shape;
  peak : C_nat.t;
  peak_tags : C_nat.t;
  resets : C_nat.t;
}

type field =
  | Terms
  | Slots
  | Query
  | Tags
  | Resets

type error =
  | Nat of field * Z.t
  | Zero of field
  | Idup of string
  | Icount of int * int
  | Free of string
  | Limit of field * C_nat.t * C_nat.t
  | Slot_need of C_nat.t * C_nat.t
  | Re_limit of C_nat.t * C_nat.t
  | Cert_invalid
  | Cert_limit of field * Z.t * Z.t
  | Depth_max of int * int
  | Nodes of int * int

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let fixed value =
  match C_nat.of_int value with
  | Some value -> value
  | None -> invalid_arg "hfhe fixed natural"

let div_up value unit =
  Z.div (Z.add value (Z.pred unit)) unit

let proof_plan bits =
  let rounds = div_up (Z.mul bits (Z.of_int 1000)) (Z.of_int 584) in
  let sound = Z.div (Z.mul rounds (Z.of_int 584)) (Z.of_int 1000) in
  let commits = Z.mul rounds (Z.of_int 3) in
  let opens = rounds in
  let trace = Z.add commits opens in
  let bytes units = Z.add (Z.mul units (Z.of_int 32)) (Z.of_int 128) in
  let cbytes = bytes commits in
  let obytes = bytes opens in
  let tbytes = bytes trace in
  { rounds; sound; commits; opens; trace; cbytes; obytes; tbytes;
    total = Z.add cbytes (Z.add obytes tbytes) }

let same_plan left right =
  Z.equal left.rounds right.rounds
  && Z.equal left.sound right.sound
  && Z.equal left.commits right.commits
  && Z.equal left.opens right.opens
  && Z.equal left.trace right.trace
  && Z.equal left.cbytes right.cbytes
  && Z.equal left.obytes right.obytes
  && Z.equal left.tbytes right.tbytes
  && Z.equal left.total right.total

let adm_ok adm =
  C_nat.valid adm.id
  && C_nat.valid adm.terms
  && not (C_nat.equal adm.terms C_nat.zero)
  && C_nat.valid adm.out
  && not (C_nat.equal adm.out C_nat.zero)
  && C_nat.le adm.out adm.terms
  && C_nat.valid adm.slots
  && not (C_nat.equal adm.slots C_nat.zero)
  && C_nat.valid adm.queries
  && not (C_nat.equal adm.queries C_nat.zero)
  && C_nat.valid adm.alpha
  && not (C_nat.equal adm.alpha C_nat.zero)
  && C_nat.valid adm.row
  && not (C_nat.equal adm.row C_nat.zero)

let cert_ok cert =
  let expected = proof_plan cert.bits in
  Z.geq cert.bits (Z.of_int 128)
  && Z.gt cert.traces Z.zero
  && Z.gt cert.tags Z.zero
  && Z.gt cert.bytes Z.zero
  && same_plan cert.plan expected
  && Z.geq cert.plan.sound cert.bits
  && Z.leq cert.plan.trace cert.traces
  && Z.leq cert.plan.total cert.bytes

let prod = {
  id = fixed 1;
  terms = fixed 32;
  out = fixed 4;
  slots = fixed 8;
  queries = fixed 64;
  alpha = fixed 8;
  row = fixed 8;
}

let prod_bits = Z.of_int 128

let prod_cert = {
  proof = Field;
  bits = prod_bits;
  traces = Z.of_int 1024;
  tags = Z.of_int 64;
  bytes = Z.of_int 1_048_576;
  plan = proof_plan prod_bits;
}

let input name ~terms ~slots ~query = {
  name;
  raw_terms = terms;
  raw_slots = slots;
  raw_query = query;
}

let nat field value =
  match C_nat.make value with
  | Some value -> Ok value
  | None -> Error (Nat (field, value))

let positive field value =
  let* value = nat field value in
  if C_nat.equal value C_nat.zero then Error (Zero field) else Ok value

let max left right = if C_nat.le left right then right else left

let within field value limit =
  if C_nat.le value limit then Ok value else Error (Limit (field, value, limit))

let calc field op left right =
  let raw = op (C_nat.to_z left) (C_nat.to_z right) in
  match C_nat.make raw with
  | Some value -> Ok value
  | None -> Error (Nat (field, raw))

let add field left right = calc field Z.add left right
let mul field left right = calc field Z.mul left right

let shaped terms slots query =
  let* tags = mul Tags terms slots in
  Ok { terms; slots; query; tags }

let shape (adm : adm) (input : input) =
  let* terms = positive Terms input.raw_terms in
  let* slots = positive Slots input.raw_slots in
  let* query = nat Query input.raw_query in
  let* terms = within Terms terms adm.terms in
  let* slots = within Slots slots adm.slots in
  let* query =
    if C_nat.lt query adm.queries then Ok query
    else Error (Limit (Query, query, adm.queries))
  in
  shaped terms slots query

let env (adm : adm) values =
  let rec walk count out = function
    | [] -> Ok out
    | _ when count = C_check.max_inputs ->
        Error (Icount (C_check.max_inputs, C_check.max_inputs + 1))
    | input :: rest ->
        let name = C_syn.name_text input.name in
        if Smap.mem name out then Error (Idup name)
        else
          let* value = shape adm input in
          walk (count + 1) (Smap.add name value out) rest
  in
  walk 0 Smap.empty values

let shape_guard term =
  let rec walk nodes = function
    | [] -> Ok ()
    | (depth, _) :: _ when depth > C_check.max_depth ->
        Error (Depth_max (C_check.max_depth, depth))
    | _ when nodes >= C_check.max_nodes ->
        Error (Nodes (C_check.max_nodes, nodes + 1))
    | (depth, term) :: rest ->
        let next = depth + 1 in
        begin
          match C_fhe.node term with
          | C_fhe.Nvar _ -> walk (nodes + 1) rest
          | C_fhe.Ntrim (_, term) | C_fhe.Nre term ->
              walk (nodes + 1) ((next, term) :: rest)
          | C_fhe.Nadd (left, right) | C_fhe.Nmul (left, right) ->
              walk (nodes + 1) ((next, left) :: (next, right) :: rest)
        end
  in
  walk 0 [0, term]

let same_slots left right =
  if C_nat.equal left.shape.slots right.shape.slots then Ok left.shape.slots
  else Error (Slot_need (left.shape.slots, right.shape.slots))

let rec run (adm : adm) env term =
  match C_fhe.node term with
  | C_fhe.Nvar name ->
      let name = C_syn.name_text name in
      begin
        match Smap.find_opt name env with
        | None -> Error (Free name)
        | Some shape ->
            Ok { shape; peak = shape.terms; peak_tags = shape.tags;
              resets = C_nat.zero }
      end
  | C_fhe.Ntrim (_, term) -> run adm env term
  | C_fhe.Nadd (left, right) ->
      let* left = run adm env left in
      let* right = run adm env right in
      let* slots = same_slots left right in
      let* terms = add Terms left.shape.terms right.shape.terms in
      let* terms = within Terms terms adm.terms in
      let query = max left.shape.query right.shape.query in
      let* resets = add Resets left.resets right.resets in
      let* shape = shaped terms slots query in
      Ok {
        shape;
        peak = max terms (max left.peak right.peak);
        peak_tags = max shape.tags (max left.peak_tags right.peak_tags);
        resets;
      }
  | C_fhe.Nmul (left, right) ->
      let* left = run adm env left in
      let* right = run adm env right in
      let* slots = same_slots left right in
      let* terms = mul Terms left.shape.terms right.shape.terms in
      let* terms = within Terms terms adm.terms in
      let query = max left.shape.query right.shape.query in
      let* resets = add Resets left.resets right.resets in
      let* shape = shaped terms slots query in
      Ok {
        shape;
        peak = max terms (max left.peak right.peak);
        peak_tags = max shape.tags (max left.peak_tags right.peak_tags);
        resets;
      }
  | C_fhe.Nre term ->
      let* prior = run adm env term in
      let* query = add Query prior.shape.query C_nat.one in
      if not (C_nat.lt query adm.queries) then
        Error (Re_limit (prior.shape.query, adm.queries))
      else
        let* resets = add Resets prior.resets C_nat.one in
        let* shape = shaped adm.out prior.shape.slots query in
        Ok {
          shape;
          peak = max prior.peak adm.out;
          peak_tags = max prior.peak_tags shape.tags;
          resets;
        }

let check (adm : adm) cert env term =
  if not (cert_ok cert) then Error Cert_invalid
  else
    let* () = shape_guard term in
    let* info = run adm env term in
    let actual = C_nat.to_z info.shape.tags in
    if Z.leq actual cert.tags then Ok info
    else Error (Cert_limit (Tags, actual, cert.tags))

let shape_text value =
  "hfhe[terms=" ^ C_nat.text value.terms
  ^ ",slots=" ^ C_nat.text value.slots
  ^ ",query=" ^ C_nat.text value.query
  ^ ",tags=" ^ C_nat.text value.tags ^ "]"

let field_text = function
  | Terms -> "terms"
  | Slots -> "slots"
  | Query -> "query"
  | Tags -> "tags"
  | Resets -> "resets"

let text error =
  let raw =
    match error with
    | Nat (field, value) ->
        "hfhe natural outside profile field = " ^ field_text field
        ^ " value = " ^ Z.to_string value
    | Zero field -> "hfhe field is zero field = " ^ field_text field
    | Idup name -> "duplicate hfhe input = " ^ name
    | Icount (limit, actual) ->
        Printf.sprintf "hfhe input count limit = %d actual = %d" limit actual
    | Free name -> "hfhe input is free = " ^ name
    | Limit (field, value, limit) ->
        "hfhe field exceeds profile field = " ^ field_text field
        ^ " value = " ^ C_nat.text value ^ " limit = " ^ C_nat.text limit
    | Slot_need (left, right) ->
        "hfhe slot mismatch left = " ^ C_nat.text left
        ^ " right = " ^ C_nat.text right
    | Re_limit (query, limit) ->
        "hfhe reset query exhausted query = " ^ C_nat.text query
        ^ " limit = " ^ C_nat.text limit
    | Cert_invalid -> "hfhe proof profile is invalid"
    | Cert_limit (field, value, limit) ->
        "hfhe proof limit exceeded field = " ^ field_text field
        ^ " value = " ^ Z.to_string value ^ " limit = " ^ Z.to_string limit
    | Depth_max (limit, actual) ->
        Printf.sprintf "hfhe term depth limit = %d actual = %d" limit actual
    | Nodes (limit, actual) ->
        Printf.sprintf "hfhe term nodes limit = %d actual = %d" limit actual
  in
  C_text.clip raw