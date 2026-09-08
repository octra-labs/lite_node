(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type kind =
  | Load
  | Move
  | Plus
  | Times
  | Quotient
  | Remainder
  | Negate
  | Absolute
  | Same
  | Different
  | Less
  | Greater
  | Join
  | Minus
  | Size
  | Slice
  | Cap_close
  | Jump
  | Jump_if
  | Mark
  | Noop
  | Stop

type frame = {
  pc : int;
  kind : kind;
  span : C_lex.span;
  slots : C_live.slot list;
}

type t = {
  code : C_mach.code;
  frames : frame array;
  shape : C_type.t;
  row : C_eff.atom list;
  eff : C_eff.atom list;
  used : C_limit.t;
  res : C_limit.t;
  depth : Z.t;
  result : C_emit.lit;
}

type cause =
  | Source
  | Feed
  | Machine
  | Slots
  | Map
  | Run

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let kind = function
  | C_octb.Load _ -> Load
  | C_octb.Move _ -> Move
  | C_octb.Plus _ -> Plus
  | C_octb.Times _ -> Times
  | C_octb.Quotient _ -> Quotient
  | C_octb.Remainder _ -> Remainder
  | C_octb.Negate _ -> Negate
  | C_octb.Absolute _ -> Absolute
  | C_octb.Same _ -> Same
  | C_octb.Different _ -> Different
  | C_octb.Less _ -> Less
  | C_octb.Greater _ -> Greater
  | C_octb.Join _ -> Join
  | C_octb.Minus _ -> Minus
  | C_octb.Size _ -> Size
  | C_octb.Slice _ -> Slice
  | C_octb.Cap_close _ -> Cap_close
  | C_octb.Jump _ -> Jump
  | C_octb.Jump_if _ -> Jump_if
  | C_octb.Mark _ -> Mark
  | C_octb.Noop -> Noop
  | C_octb.Stop -> Stop

let source source =
  let* parsed =
    match C_parse.parse source with
    | Ok value -> Ok value
    | Error _ -> Error Source
  in
  let* lowered, info =
    match C_parse.compile parsed with
    | Ok value -> Ok value
    | Error _ -> Error Source
  in
  Ok (lowered, info)

let finish (info : C_check.info) (artifact : C_octb.t) (out : C_eval.out) =
  let* result =
    match C_mach.literal info.typ out.value with
    | Some value -> Ok value
    | None -> Error Run
  in
  let* () =
    match artifact.result with
    | Some value when C_mach.equal value result -> Ok ()
    | None -> Ok ()
    | Some _ -> Error Run
  in
  let size = Array.length artifact.code in
  if C_smap.length artifact.map <> size then Error Map
  else if C_live.length artifact.live <> size then Error Slots
  else
    let frames = Array.mapi (fun pc op -> {
      pc;
      kind = kind op;
      span = C_smap.get artifact.map pc;
      slots = C_live.get artifact.live pc;
    }) artifact.code in
    Ok {
      code = artifact.plan;
      frames;
      shape = info.C_check.typ;
      row = C_eff.to_list info.eff;
      eff = out.plan;
      used = C_limit.used ~steps:out.steps ~work:out.work;
      res = info.res;
      depth = info.res.depth;
      result;
    }

let make source_text =
  let* lowered, info = source source_text in
  let* artifact =
    match C_octb.compile source_text with
    | Ok value -> Ok value
    | Error _ -> Error Machine
  in
  let* out =
    match C_eval.run lowered.C_low.term with
    | Ok value -> Ok value
    | Error _ -> Error Run
  in
  finish info artifact out

let make_feed source_text feed =
  let* lowered, info = source source_text in
  let* pairs =
    match C_feed.attach lowered.C_low.inputs feed with
    | Ok value -> Ok value
    | Error _ -> Error Feed
  in
  let* artifact =
    match C_octb.compile_feed source_text feed with
    | Ok value -> Ok value
    | Error _ -> Error Machine
  in
  let* out =
    match C_eval.run_in pairs lowered.term with
    | Ok value -> Ok value
    | Error _ -> Error Run
  in
  finish info artifact out

let make_in source_text values =
  let* lowered, info = source source_text in
  let rec attach inputs values =
    match inputs, values with
    | [], [] -> Some []
    | input :: inputs, value :: values ->
      Option.map (fun rest -> (input, value) :: rest) (attach inputs values)
    | [], _ :: _ | _ :: _, [] -> None
  in
  let* values =
    match attach lowered.C_low.inputs values with
    | Some value -> Ok value
    | None -> Error Run
  in
  let* artifact =
    match C_octb.compile source_text with
    | Ok value -> Ok value
    | Error _ -> Error Machine
  in
  let* out =
    match C_eval.run_in values lowered.term with
    | Ok value -> Ok value
    | Error _ -> Error Run
  in
  finish info artifact out

let replay value = C_mach.replay value.code

let text = function
  | Source -> "source refusal"
  | Feed -> "feed refusal"
  | Machine -> "machine refusal"
  | Slots -> "linear-slot refusal"
  | Map -> "source-map refusal"
  | Run -> "execution refusal"