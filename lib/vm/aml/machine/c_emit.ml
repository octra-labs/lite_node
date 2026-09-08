(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type lit =
  | Bool of bool
  | Int of Z.t
  | Bytes of string
  | Data of C_rval.t
  | Cap of C_nat.t * C_nat.t

type op =
  | Load of lit
  | Stop

type t = {
  lit : lit;
  ops : op list;
  octb : string;
  span : C_lex.span;
}

type error =
  | Source of C_parse.error
  | Inputs of int
  | Effects of C_eff.atom list
  | Run of C_eval.error
  | Plan of C_eff.atom list
  | Value of C_type.t * C_eval.value

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let put_u16 out value =
  Buffer.add_char out (Char.chr (value land 0xff));
  Buffer.add_char out (Char.chr ((value lsr 8) land 0xff))

let put_u32 out value =
  Buffer.add_char out (Char.chr (value land 0xff));
  Buffer.add_char out (Char.chr ((value lsr 8) land 0xff));
  Buffer.add_char out (Char.chr ((value lsr 16) land 0xff));
  Buffer.add_char out (Char.chr ((value lsr 24) land 0xff))

let data = function
  | Bool value -> 1, if value then "1" else "0"
  | Int value -> 0, Z.to_string value
  | Bytes value -> 3, value
  | Data value -> 2, C_rval.encode value
  | Cap _ -> invalid_arg "capability cannot be a constant"

let octb lit =
  let tag, raw = data lit in
  let out = Buffer.create (24 + String.length raw) in
  Buffer.add_string out "OCTB";
  put_u16 out 1;
  put_u16 out 1;
  put_u32 out 2;
  Buffer.add_char out (Char.chr tag);
  put_u32 out (String.length raw);
  Buffer.add_string out raw;
  Buffer.add_char out (Char.chr 0x0b);
  Buffer.add_char out (Char.chr 0);
  put_u16 out 0;
  Buffer.add_char out (Char.chr 0x17);
  Buffer.contents out

let lit typ value =
  match typ, value with
  | C_type.Bool, C_eval.Bool value -> Some (Bool value)
  | C_type.Int, C_eval.Int value -> Some (Int value)
  | C_type.Bytes size, C_eval.Bytes value
      when Z.equal (C_nat.to_z size) (Z.of_int (String.length value)) ->
      Some (Bytes value)
  | C_type.Cap kind, C_eval.Cap (found, id) when C_nat.equal kind found ->
    Some (Cap (kind, id))
  | _ ->
    begin
      match C_rval.make typ value with
      | Ok item -> Some (Data item)
      | Error _ -> None
    end

let compile source =
  let* parsed =
    match C_parse.parse source with
    | Ok value -> Ok value
    | Error error -> Error (Source error)
  in
  let* lowered, info =
    match C_parse.compile parsed with
    | Ok value -> Ok value
    | Error error -> Error (Source error)
  in
  let count = List.length lowered.C_low.inputs in
  if count <> 0 then Error (Inputs count)
  else
    let effects = C_eff.to_list info.C_check.eff in
    if effects <> [] then Error (Effects effects)
    else
      let* out =
        match C_eval.run lowered.term with
        | Ok value -> Ok value
        | Error error -> Error (Run error)
      in
      if out.plan <> [] then Error (Plan out.plan)
      else
        match lit info.typ out.value with
        | None -> Error (Value (info.typ, out.value))
        | Some lit ->
            let ops = [Load lit; Stop] in
            Ok { lit; ops; octb = octb lit; span = C_parse.body_span parsed }

let text = function
  | Source error -> C_parse.text error
  | Inputs count -> Printf.sprintf "OCTB input count = %d expected = 0" count
  | Effects effects -> "OCTB effects are not representable effects = " ^ C_eff.text (C_eff.of_list effects)
  | Run error -> C_eval.text error
  | Plan effects -> "OCTB execution plan is not empty effects = " ^ C_eff.text (C_eff.of_list effects)
  | Value (typ, value) ->
      "OCTB value is not representable type = " ^ C_type.text typ
      ^ " value = " ^ C_eval.value_text value