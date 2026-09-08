(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  code : Contract_vm.instr array;
  next : int;
  body : int;
  size : int;
  entry : int;
  first : int;
  evidence : Aml_evidence.t;
}

type error =
  | Evidence of Aml_evidence.error
  | Scope of Vm_program.scope_error
  | Entry of int
  | Label_start of int
  | Label_space of int
  | Input of C_type.t
  | Output of C_type.t
  | Tail
  | Verify
  | Bits
  | Count of int
  | Form
  | Repeated of int
  | Absent of int
  | Differs of int
  | Overlap of int * int
  | Image

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let fresh next =
  if next = max_int then Error (Label_space next)
  else Ok (next, next + 1)

let input index reg typ next =
  let load = Contract_vm.MLOAD (reg, 1001 + index) in
  match typ with
  | C_type.Int ->
    Ok ([
      load;
      Contract_vm.LDI (61, Contract_vm.VInt Z.zero);
      Contract_vm.SUB (reg, reg, 61);
    ], next)
  | C_type.Bool ->
    let* label, next = fresh next in
    Ok ([
      load;
      Contract_vm.JIF (reg, label);
      Contract_vm.JMP label;
      Contract_vm.JDEST label;
    ], next)
  | C_type.Unit
  | C_type.Num _
  | C_type.Bytes _
  | C_type.Vec _
  | C_type.Seq _
  | C_type.Pair _
  | C_type.Sum _
  | C_type.Cap _
  | C_type.Enc _ -> Error (Input typ)

let inputs binds next =
  let rec walk index out next = function
    | [] -> Ok (List.rev out |> List.concat, next)
    | bind :: rest ->
      let* ops, next = input index (index + 1) bind.C_term.typ next in
      walk (index + 1) (ops :: out) next rest
  in
  walk 0 [] next binds

let output typ next =
  match typ with
  | C_type.Int ->
    Ok ([
      Contract_vm.LDI (61, Contract_vm.VInt Z.zero);
      Contract_vm.SUB (0, 0, 61);
      Contract_vm.STOP;
    ], next)
  | C_type.Bool ->
    let* label, next = fresh next in
    Ok ([
      Contract_vm.JIF (0, label);
      Contract_vm.JMP label;
      Contract_vm.JDEST label;
      Contract_vm.STOP;
    ], next)
  | C_type.Unit
  | C_type.Num _
  | C_type.Bytes _
  | C_type.Vec _
  | C_type.Seq _
  | C_type.Pair _
  | C_type.Sum _
  | C_type.Cap _
  | C_type.Enc _ -> Error (Output typ)

let make ~entry ~first evidence =
  if entry < 0 then Error (Entry entry)
  else if first <= entry then Error (Label_start first)
  else
    let* runtime =
      match Aml_evidence.runtime evidence with
      | Ok value -> Ok value
      | Error error -> Error (Evidence error)
    in
    let count = Array.length runtime in
    if count = 0 || runtime.(count - 1) <> Contract_vm.STOP then Error Tail
    else
      let* runtime, next =
        match Vm_program.scope ~first runtime with
        | Ok value -> Ok value
        | Error error -> Error (Scope error)
      in
      let program = Aml_evidence.program evidence in
      let* input_ops, next = inputs program.C_low.inputs next in
      let* output_ops, next = output (Aml_evidence.output evidence) next in
      let size = Array.length runtime - 1 in
      let body = 1 + List.length input_ops in
      let code =
        Array.concat [
          [|Contract_vm.JDEST entry|];
          Array.of_list input_ops;
          Array.sub runtime 0 size;
          Array.of_list output_ops;
        ]
      in
      begin
        match Contract_vm.Verifier.verify code with
        | Ok () -> Ok { code; next; body; size; entry; first; evidence }
        | Error _ -> Error Verify
      end

let code value = value.code
let next value = value.next
let body value = value.body
let size value = value.size

let int_code value = C_bin.Num (Z.of_int value)

let int_get = function
  | C_bin.Num value when Z.fits_int value ->
    let value = Z.to_int value in
    if value >= 0 then Some value else None
  | _ -> None

let item_code value =
  let* bits =
    match Aml_evidence.enc value.evidence with
    | Some bits -> Ok bits
    | None -> Error Bits
  in
  let* evidence =
    match C_bin.dec_code bits with
    | Some code -> Ok code
    | None -> Error Bits
  in
  Ok (C_bin.Tag (Z.zero,
    C_bin.Cons (int_code value.entry,
      C_bin.Cons (int_code value.first,
        C_bin.Cons (evidence, C_bin.Nil)))))

let unique values =
  let rec walk seen = function
    | [] -> Ok ()
    | value :: _ when List.mem value.entry seen -> Error (Repeated value.entry)
    | value :: rest -> walk (value.entry :: seen) rest
  in
  walk [] values

let size_raw value =
  String.init 4 (fun index ->
    Char.chr ((value lsr ((3 - index) * 8)) land 255))

let read_size raw =
  if String.length raw < 4 then None
  else
    let rec walk index value =
      if index = 4 then Some value
      else
        let byte = Char.code raw.[index] in
        if value > (C_nat.str_max - byte) / 256 then None
        else walk (index + 1) (value * 256 + byte)
    in
    walk 0 0

let stamp image =
  Digestif.SHA256.(
    digest_string ("octra:aml:call-image" ^ image) |> to_raw_string)

let stamp_code value =
  value
  |> String.to_seq
  |> List.of_seq
  |> List.map (fun byte -> C_bin.Num (Z.of_int (Char.code byte)))
  |> C_bin.list_code Option.some

let stamp_get code =
  match C_bin.list_get int_get code with
  | Some values
      when List.length values = Digestif.SHA256.digest_size
        && List.for_all (fun value -> value <= 255) values ->
    Some (String.init (List.length values) (fun index -> Char.chr (List.nth values index)))
  | Some _ | None -> None

let encode_stamp stamp values =
  let count = List.length values in
  if count = 0 || count > Program_limits.max_functions then Error (Count count)
  else
    let* () = unique values in
    let rec items out = function
      | [] -> Ok (List.rev out)
      | value :: rest ->
        let* item = item_code value in
        items (item :: out) rest
    in
    let* items = items [] values in
    let* list =
      match C_bin.list_code Option.some items with
      | Some value -> Ok value
      | None -> Error Bits
    in
    let* stamp =
      match stamp_code stamp with
      | Some value -> Ok value
      | None -> Error Bits
    in
    let* bits =
      match
        C_bin.enc_code
          (C_bin.Tag
            (Z.zero, C_bin.Cons (stamp, C_bin.Cons (list, C_bin.Nil))))
      with
      | Some value -> Ok value
      | None -> Error Bits
    in
    let* body =
      match C_bin.pack bits with
      | Some value -> Ok value
      | None -> Error Bits
    in
    Ok (size_raw (List.length bits) ^ body)

let encode ~image values = encode_stamp (stamp image) values

let item_get = function
  | C_bin.Tag (tag,
      C_bin.Cons (entry,
        C_bin.Cons (first, C_bin.Cons (evidence, C_bin.Nil))))
      when Z.equal tag Z.zero ->
    begin
      match int_get entry, int_get first, C_bin.enc_code evidence with
      | Some entry, Some first, Some bits ->
        begin
          match Aml_evidence.dec_any bits with
          | Ok evidence -> make ~entry ~first evidence
          | Error error -> Error (Evidence error)
        end
      | _ -> Error Form
    end
  | _ -> Error Form

let decode_all raw =
  match read_size raw with
  | None -> Error Bits
  | Some size ->
    let body = String.sub raw 4 (String.length raw - 4) in
    begin
      match C_bin.unpack size body with
      | None -> Error Bits
      | Some bits ->
        begin
          match C_bin.dec_code bits with
          | Some
              (C_bin.Tag
                (tag, C_bin.Cons (stamp, C_bin.Cons (list, C_bin.Nil))))
              when Z.equal tag Z.zero ->
            begin
              match stamp_get stamp, C_bin.list_get Option.some list with
              | Some stamp, Some items ->
                let count = List.length items in
                if count = 0 || count > Program_limits.max_functions then
                  Error (Count count)
                else
                  let rec values out = function
                    | [] -> Ok (List.rev out)
                    | item :: rest ->
                      let* value = item_get item in
                      values (value :: out) rest
                  in
                  let* values = values [] items in
                  let* () = unique values in
                  begin
                    match encode_stamp stamp values with
                    | Ok exact when String.equal exact raw -> Ok (stamp, values)
                    | Ok _ | Error _ -> Error Form
                  end
              | Some _, None | None, Some _ | None, None -> Error Form
            end
          | Some _ | None -> Error Form
        end
    end

let decode raw = Result.map snd (decode_all raw)

let segment code value =
  let starts =
    Array.to_list (Array.mapi (fun pc op -> pc, op) code)
    |> List.filter_map
      (fun (pc, op) ->
        if op = Contract_vm.JDEST value.entry then Some pc else None)
  in
  match starts with
  | [first] ->
    let count = Array.length value.code in
    if first + count > Array.length code then Error (Differs value.entry)
    else
      let rec check index =
        if index = count then Ok (first, first + count)
        else if code.(first + index) = value.code.(index) then check (index + 1)
        else Error (Differs value.entry)
      in
      check 0
  | [] -> Error (Absent value.entry)
  | _ -> Error (Repeated value.entry)

let verify ~image code raw =
  match Contract_vm.Verifier.verify code with
  | Error _ -> Error Verify
  | Ok () ->
    let* expected, values = decode_all raw in
    let rec segments out = function
      | [] -> Ok out
      | value :: rest ->
        let* span = segment code value in
        segments (span :: out) rest
    in
    let* spans = segments [] values in
    let spans = List.sort (fun (left, _) (right, _) -> Int.compare left right) spans in
    let rec disjoint prior = function
      | [] ->
        if String.equal expected (stamp image) then Ok () else Error Image
      | (first, last) :: rest when first >= prior -> disjoint last rest
      | (first, _) :: _ -> Error (Overlap (prior, first))
    in
    disjoint 0 spans

let text = function
  | Evidence error ->
    "call evidence failed reason = " ^ Aml_evidence.text error
  | Scope error -> "call label scope failed reason = " ^ Vm_program.scope_text error
  | Entry value -> Printf.sprintf "call entry is invalid value = %d" value
  | Label_start value ->
    Printf.sprintf "call label start is invalid value = %d" value
  | Label_space value ->
    Printf.sprintf "call label space is exhausted value = %d" value
  | Input typ -> "call input type is unsupported type = " ^ C_type.text typ
  | Output typ -> "call output type is unsupported type = " ^ C_type.text typ
  | Tail -> "call runtime tail is invalid"
  | Verify -> "call runtime verification failed"
  | Bits -> "call evidence bits are invalid"
  | Count value -> Printf.sprintf "call evidence count is invalid value = %d" value
  | Form -> "call evidence form is invalid"
  | Repeated value ->
    Printf.sprintf "call evidence entry is repeated value = %d" value
  | Absent value ->
    Printf.sprintf "call evidence entry is absent value = %d" value
  | Differs value ->
    Printf.sprintf "call evidence code differs entry = %d" value
  | Overlap (left, right) ->
    Printf.sprintf "call evidence code overlaps left = %d right = %d" left right
  | Image -> "call image differs"