(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Bin = C_bin
module Effect = Aml_effect

type layout = {
  payload : int list;
  scratch : int list;
}

type t = {
  sealed : Effect.sealed;
  layouts : layout list;
  blocks : Effect.block list;
}

type error =
  | Effect of Effect.error
  | Lower of C_octb.error
  | Layout_count of int * int
  | Registry
  | Form

let ( let* ) value next = Option.bind value next

let ( let^ ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let option = function
  | Some value -> Ok value
  | None -> Error Form

let raw_code value =
  let rec walk index out =
    if index < 0 then out
    else
      walk (index - 1)
        (Bin.Cons (Bin.Num (Z.of_int (Char.code value.[index])), out))
  in
  walk (String.length value - 1) Bin.Nil

let raw_get input =
  let out = Buffer.create 32 in
  let rec walk = function
    | Bin.Nil -> Some (Buffer.contents out)
    | Bin.Cons (Bin.Num value, rest) ->
      begin
        match C_nat.byte value with
        | Some byte ->
          Buffer.add_char out (Char.chr byte);
          walk rest
        | None -> None
      end
    | _ -> None
  in
  walk input

let host_type_code = function
  | Oct_lang.TInt -> Some (Bin.Tag (Z.zero, Bin.Nil))
  | Oct_lang.TBool -> Some (Bin.Tag (Z.one, Bin.Nil))
  | Oct_lang.TBytes32 -> Some (Bin.Tag (Z.of_int 2, Bin.Nil))
  | Oct_lang.TString
  | Oct_lang.TAddress
  | Oct_lang.TBytes
  | Oct_lang.TU64
  | Oct_lang.TU128
  | Oct_lang.TU256
  | Oct_lang.TCipher
  | Oct_lang.TPubKey
  | Oct_lang.TMap _
  | Oct_lang.TList _
  | Oct_lang.TStruct _
  | Oct_lang.TEnum _
  | Oct_lang.TOption _
  | Oct_lang.TTuple _
  | Oct_lang.TVoid -> None

let host_type_get = function
  | Bin.Tag (tag, Bin.Nil) when Z.equal tag Z.zero -> Some Oct_lang.TInt
  | Bin.Tag (tag, Bin.Nil) when Z.equal tag Z.one -> Some Oct_lang.TBool
  | Bin.Tag (tag, Bin.Nil) when Z.equal tag (Z.of_int 2) ->
    Some Oct_lang.TBytes32
  | _ -> None

let entry_code (atom, entry) =
  let* atom = Bin.atom_code atom in
  match entry with
  | Effect.State_entry (name, typ) ->
    let* typ = host_type_code typ in
    Some (Bin.Tag (Z.zero,
      Bin.Cons (atom,
        Bin.Cons (raw_code name, Bin.Cons (typ, Bin.Nil)))))
  | Effect.Event_entry (name, types) ->
    let* types = Bin.list_code host_type_code types in
    Some (Bin.Tag (Z.one,
      Bin.Cons (atom,
        Bin.Cons (raw_code name, Bin.Cons (types, Bin.Nil)))))
  | Effect.Fault_entry (name, code, message) ->
    Some (Bin.Tag (Z.of_int 2,
      Bin.Cons (atom,
        Bin.Cons (raw_code name,
          Bin.Cons (Bin.Int (Z.of_int code),
            Bin.Cons (raw_code message, Bin.Nil))))))

let int_get = function
  | Bin.Int value when Z.fits_int value -> Some (Z.to_int value)
  | _ -> None

let entry_get = function
  | Bin.Tag (tag,
      Bin.Cons (atom, Bin.Cons (name, Bin.Cons (typ, Bin.Nil))))
      when Z.equal tag Z.zero ->
    let* atom = Bin.atom_get atom in
    let* name = raw_get name in
    let* typ = host_type_get typ in
    Some (atom, Effect.State_entry (name, typ))
  | Bin.Tag (tag,
      Bin.Cons (atom, Bin.Cons (name, Bin.Cons (types, Bin.Nil))))
      when Z.equal tag Z.one ->
    let* atom = Bin.atom_get atom in
    let* name = raw_get name in
    let* types = Bin.list_get host_type_get types in
    Some (atom, Effect.Event_entry (name, types))
  | Bin.Tag (tag,
      Bin.Cons (atom,
        Bin.Cons (name,
          Bin.Cons (code, Bin.Cons (message, Bin.Nil)))))
      when Z.equal tag (Z.of_int 2) ->
    let* atom = Bin.atom_get atom in
    let* name = raw_get name in
    let* code = int_get code in
    let* message = raw_get message in
    Some (atom, Effect.Fault_entry (name, code, message))
  | _ -> None

let origin_code = function
  | C_check.Direct -> Bin.Tag (Z.zero, Bin.Nil)
  | C_check.Held kind ->
    Bin.Tag (Z.one, Bin.Num (C_nat.to_z kind))

let site_code site =
  let* atom = Bin.atom_code site.C_check.atom in
  let* typ = Bin.ty_code site.payload in
  Some (Bin.Tag (Z.zero,
    Bin.Cons (atom,
      Bin.Cons (typ, Bin.Cons (origin_code site.origin, Bin.Nil)))))

let rec flow_code = function
  | C_check.Pure -> Some (Bin.Tag (Z.zero, Bin.Nil))
  | C_check.Action site ->
    let* site = site_code site in
    Some (Bin.Tag (Z.one, site))
  | C_check.Seq (first, second) -> flow_pair 2 first second
  | C_check.Fork (first, second) -> flow_pair 3 first second
  | C_check.Loop (count, body) ->
    let* body = flow_code body in
    Some (Bin.Tag (Z.of_int 4,
      Bin.Cons (Bin.Num (C_nat.to_z count), Bin.Cons (body, Bin.Nil))))

and flow_pair tag first second =
  let* first = flow_code first in
  let* second = flow_code second in
  Some (Bin.Tag (Z.of_int tag,
    Bin.Cons (first, Bin.Cons (second, Bin.Nil))))

let location_code location =
  let* site = site_code location.C_check.site in
  Some (Bin.Tag (Z.zero,
    Bin.Cons (Bin.Num (Z.of_int location.index),
      Bin.Cons (site, Bin.Cons (Bin.Int location.count, Bin.Nil)))))

let reg_code reg =
  if reg < 0 || reg >= Contract_vm.register_count then None
  else Some (Bin.Num (Z.of_int reg))

let layout_code value =
  let* payload = Bin.list_code reg_code value.payload in
  let* scratch = Bin.list_code reg_code value.scratch in
  Some (Bin.Tag (Z.zero,
    Bin.Cons (payload, Bin.Cons (scratch, Bin.Nil))))

let rollback_code = function
  | Effect.Preserve -> Bin.Tag (Z.zero, Bin.Nil)
  | Effect.Journal -> Bin.Tag (Z.one, Bin.Nil)
  | Effect.Record -> Bin.Tag (Z.of_int 2, Bin.Nil)
  | Effect.Abort -> Bin.Tag (Z.of_int 3, Bin.Nil)

let charge_code = function
  | Effect.Exact -> Bin.Tag (Z.zero, Bin.Nil)
  | Effect.Storage -> Bin.Tag (Z.one, Bin.Nil)

let ops_code ops =
  try
    Some (raw_code (Bytecode.encode (Array.of_list ops)))
  with Invalid_argument _ -> None

let block_code block =
  let* site = site_code block.Effect.site in
  let* ops = ops_code block.ops in
  Some (Bin.Tag (Z.zero,
    Bin.Cons (Bin.Num (Z.of_int block.index),
      Bin.Cons (site,
        Bin.Cons (rollback_code block.rollback,
          Bin.Cons (ops,
            Bin.Cons (Bin.Int block.cost,
              Bin.Cons (charge_code block.charge, Bin.Nil))))))))

let cost_code () =
  let values = [
    Contract_vm.register_count;
    Contract_vm.max_storage_value_len;
    Contract_vm.effort_cost (Contract_vm.LDI (0, Contract_vm.VInt Z.zero));
    Contract_vm.effort_cost (Contract_vm.SLOAD (0, "x"));
    Contract_vm.effort_cost (Contract_vm.EQ (0, 1, 2));
    Contract_vm.effort_cost (Contract_vm.ASSERT 0);
    Contract_vm.effort_cost (Contract_vm.SSTORE ("x", 0));
    Contract_vm.effort_cost (Contract_vm.EMIT ("x", []));
    Contract_vm.effort_cost Contract_vm.REVERT;
    Contract_vm.storage_work (Contract_vm.VInt Z.zero);
    Contract_vm.storage_work (Contract_vm.VBool false);
    Contract_vm.storage_work (Contract_vm.VBytes32 (String.make 32 '\000'));
  ] in
  Bin.Tag (Z.zero,
    List.fold_right
      (fun value rest -> Bin.Cons (Bin.Int (Z.of_int value), rest))
      values Bin.Nil)

let code value =
  let sealed = value.sealed in
  let* program = C_pbin.code (Effect.prog sealed) in
  let entries = Effect.entries (Effect.registry sealed) in
  let* entries = Bin.list_code entry_code entries in
  let* typ = Bin.ty_code (Effect.typ sealed) in
  let* flow = flow_code (Effect.flow sealed) in
  let* locations =
    Bin.list_code location_code (Array.to_list (Effect.locations sealed))
  in
  let* layouts = Bin.list_code layout_code value.layouts in
  let* blocks = Bin.list_code block_code value.blocks in
  Some (Bin.Tag (Z.zero,
    Bin.Cons (program,
      Bin.Cons (entries,
        Bin.Cons (typ,
          Bin.Cons (flow,
            Bin.Cons (locations,
              Bin.Cons (layouts,
                Bin.Cons (blocks,
                  Bin.Cons (cost_code (), Bin.Nil))))))))))

let enc value =
  let* code = code value in
  Bin.enc_code code

let occurrences locations =
  Array.fold_left
    (fun count location ->
      if Z.fits_int location.C_check.count then
        count + Z.to_int location.count
      else max_int)
    0 locations

let assemble sealed layouts =
  let locations = Effect.locations sealed in
  let expected = occurrences locations in
  let actual = List.length layouts in
  if expected <> actual then Error (Layout_count (expected, actual))
  else
    let rec repeat index left layouts out =
      if left = 0 then Ok (layouts, out)
      else
        match layouts with
        | [] -> Error (Layout_count (expected, actual))
        | layout :: rest ->
          begin
            match Effect.lower sealed ~index ~payload:layout.payload
                ~scratch:layout.scratch with
            | Ok block -> repeat index (left - 1) rest (block :: out)
            | Error error -> Error (Effect error)
          end
    in
    let rec walk index layouts out =
      if index = Array.length locations then Ok (List.rev out)
      else if not (Z.fits_int locations.(index).C_check.count) then
        Error (Layout_count (expected, actual))
      else
        let count = Z.to_int locations.(index).C_check.count in
        let^ layouts, out = repeat index count layouts out in
        walk (index + 1) layouts out
    in
    walk 0 layouts []

let make sealed =
  let^ found =
    match C_octb.effect_layouts (Effect.prog sealed) with
    | Ok value -> Ok value
    | Error error -> Error (Lower error)
  in
  let layouts =
    List.map
      (fun layout -> { payload = layout.C_octb.payload; scratch = layout.scratch })
      found
  in
  let^ blocks = assemble sealed layouts in
  let value = { sealed; layouts; blocks } in
  match enc value with
  | Some _ -> Ok value
  | None -> Error Form

let root = function
  | Bin.Tag (tag,
      Bin.Cons (program,
        Bin.Cons (entries,
          Bin.Cons (_, Bin.Cons (_, Bin.Cons (_,
            Bin.Cons (_, Bin.Cons (_, Bin.Cons (_, Bin.Nil)))))))))
      when Z.equal tag Z.zero -> Some (program, entries)
  | _ -> None

type defs = {
  state : Oct_lang.state_field list;
  events : Oct_lang.event_def list;
  errors : Oct_lang.error_def list;
  binds : Effect.bind list;
}

let empty_defs = { state = []; events = []; errors = []; binds = [] }

let state_def defs name typ =
  match List.find_opt (fun item -> String.equal item.Oct_lang.sf_name name)
      defs.state with
  | None ->
    Ok { defs with state = { Oct_lang.sf_name = name; sf_typ = typ } :: defs.state }
  | Some item when item.sf_typ = typ -> Ok defs
  | Some _ -> Error Registry

let event_def defs name types =
  match List.find_opt (fun item -> String.equal item.Oct_lang.ev_name name)
      defs.events with
  | None ->
    let fields =
      List.mapi
        (fun index typ -> "value" ^ string_of_int index, typ, false)
        types
    in
    Ok {
      defs with
      events = { Oct_lang.ev_name = name; ev_fields = fields } :: defs.events;
    }
  | Some item
      when List.map (fun (_, typ, _) -> typ) item.ev_fields = types -> Ok defs
  | Some _ -> Error Registry

let error_def defs name code message =
  match List.find_opt (fun item -> String.equal item.Oct_lang.err_name name)
      defs.errors with
  | None ->
    Ok {
      defs with
      errors = { Oct_lang.err_name = name; err_code = code; err_msg = message }
        :: defs.errors;
    }
  | Some item when item.err_code = code && String.equal item.err_msg message ->
    Ok defs
  | Some _ -> Error Registry

let restore entries =
  let rec walk defs = function
    | [] -> Ok defs
    | (atom, entry) :: rest ->
      let^ defs, target =
        match entry with
        | Effect.State_entry (name, typ) ->
          let^ defs = state_def defs name typ in
          Ok (defs, Effect.State name)
        | Effect.Event_entry (name, types) ->
          let^ defs = event_def defs name types in
          Ok (defs, Effect.Event name)
        | Effect.Fault_entry (name, code, message) ->
          let^ defs = error_def defs name code message in
          Ok (defs, Effect.Fault name)
      in
      let defs = { defs with binds = Effect.bind atom target :: defs.binds } in
      walk defs rest
  in
  let^ defs = walk empty_defs entries in
  let program = {
    Oct_lang.declaration = Oct_lang.ProgramDecl;
    name = "Proof";
    imports = [];
    structs = [];
    enums = [];
    consts = [];
    invariants_decl = [];
    state = List.rev defs.state;
    events = List.rev defs.events;
    errors = List.rev defs.errors;
    interfaces = [];
    implements = [];
    ctor = None;
    funcs = [];
    forms = [];
  } in
  match Effect.make program (List.rev defs.binds) with
  | Ok value when Effect.entries value = entries -> Ok value
  | Ok _ -> Error Registry
  | Error error -> Error (Effect error)

let dec expected bits =
  let^ raw = option (Bin.dec_code bits) in
  let^ program_code, entries_code = option (root raw) in
  let^ program = option (C_pbin.get program_code) in
  let^ entries = option (Bin.list_get entry_get entries_code) in
  if entries <> Effect.entries expected then Error Registry
  else
    let^ sealed =
      match Effect.seal expected program.C_low.inputs program.term with
      | Ok value -> Ok value
      | Error error -> Error (Effect error)
    in
    let^ value = make sealed in
    begin
      match enc value with
      | Some exact when List.equal Bool.equal exact bits -> Ok value
      | Some _ | None -> Error Form
    end

let dec_any bits =
  let^ raw = option (Bin.dec_code bits) in
  let^ _, entries_code = option (root raw) in
  let^ entries = option (Bin.list_get entry_get entries_code) in
  let^ expected = restore entries in
  dec expected bits

let blocks value = value.blocks
let program value = Effect.prog value.sealed
let output value = Effect.typ value.sealed

let runtime value =
  let^ layouts =
    match C_octb.effect_layouts (program value) with
    | Ok found -> Ok found
    | Error error -> Error (Lower error)
  in
  let expected = List.length layouts in
  let actual = List.length value.blocks in
  if expected <> actual then Error (Layout_count (expected, actual))
  else
    let rec pair out layouts blocks =
      match layouts, blocks with
      | [], [] -> Ok (List.rev out)
      | layout :: layouts, block :: blocks ->
        pair ((layout, block.Effect.ops) :: out) layouts blocks
      | _, _ -> Error (Layout_count (expected, actual))
    in
    let^ blocks = pair [] layouts value.blocks in
    begin
      match C_octb.emit_effects (program value) blocks with
      | Ok code -> Ok code
      | Error error -> Error (Lower error)
    end

let text = function
  | Effect error -> "effect evidence failed reason = " ^ Effect.text error
  | Lower error -> "effect evidence lowering failed reason = " ^ C_octb.text error
  | Layout_count (expected, actual) ->
    Printf.sprintf
      "effect evidence layout count differs expected = %d actual = %d"
      expected actual
  | Registry -> "effect evidence registry differs"
  | Form -> "effect evidence form is invalid"