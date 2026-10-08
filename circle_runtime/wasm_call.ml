(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Vm = Octra_vm.Contract_vm
module Host = Octra_core.Circle_wasm_host

type operation =
  | Value
  | Balance
  | Transfer of string * Z.t
  | Call of string * string * Vm.v list

let field name = function
  | `Assoc fields -> Option.value (List.assoc_opt name fields) ~default:`Null
  | _ -> `Null

let integer = function
  | `Int value when value >= 0 -> Ok value
  | _ -> Error "circle counter invalid"

let scalar json =
  match field "tag" json, field "value" json with
  | `String "int", `String value ->
    begin try Ok (Vm.VInt (Z.of_string value))
    with Invalid_argument _ -> Error "circle integer invalid" end
  | `String "string", `String value -> Ok (Vm.VString value)
  | `String "bool", `Bool value -> Ok (Vm.VBool value)
  | _ -> Error "circle argument invalid"

let operation json =
  let ( let* ) = Result.bind in
  let* params = match field "params" json with
    | `List params ->
      List.fold_left (fun result param ->
        let* values = result in
        let* value = scalar param in
        Ok (value :: values)) (Ok []) params |> Result.map List.rev
    | _ -> Error "circle arguments invalid" in
  match field "method" json, params with
  | `String "call_value", [] -> Ok Value
  | `String "oct_balance", [] -> Ok Balance
  | `String "oct_transfer", [Vm.VString target; Vm.VInt amount] -> Ok (Transfer (target, amount))
  | `String "program_call", Vm.VString target :: Vm.VString method_name :: values ->
    Ok (Call (target, method_name, values))
  | _ -> Error "circle operation invalid"

let response value =
  let result = match value with
    | Vm.VBool false -> Ok (1, "")
    | Vm.VBool true -> Ok (2, "")
    | Vm.VInt number | Vm.VU64 number | Vm.VU128 number | Vm.VU256 number ->
      Ok (3, Z.to_string number)
    | Vm.VString text | Vm.VAddr text -> Ok (4, text)
    | Vm.VBytes text | Vm.VBytes32 text -> Ok (4, Base64.encode_exn text)
    | _ -> Error "circle result type invalid" in
  Result.bind result (fun (tag, text) ->
    if String.length text > 2_097_152 - 10 then
      Error "circle response exceeds limit"
    else
    let bytes = Buffer.create (String.length text + 10) in
    Buffer.add_string bytes "OCWS1";
    Buffer.add_char bytes (Char.chr tag);
    Octra_core.Circle_wasm_codec.add_u32 bytes (String.length text);
    Buffer.add_string bytes text;
    Ok (Base64.encode_exn (Buffer.contents bytes)))

let reply ~id ~bytes ~effort storage =
  let payload pairs = `Assoc [
    "id", id;
    "response_b64", `String bytes;
    "effort", `Int effort;
    "storage_pairs", `List pairs;
  ] in
  let header = Yojson.Safe.to_string (payload []) |> String.length |> Z.of_int in
  let entry = Yojson.Safe.to_string (`Assoc [
    "key_b64", `String ""; "value_b64", `String "";
  ]) |> String.length |> Z.of_int in
  let encoded text = Z.mul (Z.of_int 4)
    (Z.cdiv (Z.of_int (String.length text)) (Z.of_int 3)) in
  let size = Hashtbl.fold (fun key value size ->
    Z.add size (Z.add entry (Z.add (encoded key) (encoded value)))) storage header in
  let size = Z.add size (Z.of_int (max 0 (Hashtbl.length storage - 1))) in
  if Z.gt size (Z.of_int Octra_core.Circle_wasm_native.max_call_input_bytes) then
    Error "circle reply exceeds limit"
  else Ok (payload (Host.make_storage_pairs_json storage))

let error_text text =
  let output = Buffer.create (min 256 (String.length text)) in
  let rec copy offset =
    if offset < String.length text && Buffer.length output < 256 then
      let decoded = String.get_utf_8_uchar text offset in
      if Uchar.utf_decode_is_valid decoded then begin
        let size = Uchar.utf_decode_length decoded in
        if size <= 256 - Buffer.length output then begin
          Buffer.add_substring output text offset size;
          copy (offset + size)
        end
      end else begin
        Buffer.add_char output '?';
        copy (offset + 1)
      end
  in
  copy 0;
  Buffer.contents output

let dispatch ~(ctx : Vm.exec_ctx) ~depth ~address ~value ~storage ~events json =
  let checked =
    let ( let* ) = Result.bind in
    let* fuel = integer (field "fuel" json) in
    let* index = integer (field "events" json) in
    let* action = operation json in
    let* values = Host.decode_storage_tbl (field "storage_pairs" json) in
    let cost = Z.add (Octra_vm.Program_journal.storage_effort storage)
      (Octra_vm.Program_journal.storage_effort values) in
    let* spent = match Octra_vm.Cost.charge_z ~used:0 ~cost ~limit:fuel with
      | None -> Error "circle storage effort exceeds limit"
      | Some spent -> Ok spent in
    let* () = Circle_runtime_storage.validate_runtime_storage_delta
      ~proof_mode:Octra_core.Rule_graph.Active storage values
      |> Result.map_error (fun (_, key, _) -> "circle reserved key write: " ^ key) in
    Ok (fuel - spent, spent, index, action, values) in
  let id = field "id" json in
  let refused reason = `Assoc ["id", id; "error", `String (error_text reason)] in
  match checked with
  | Error error -> Lwt.return (refused error)
  | Ok (fuel, spent, index, action, values) ->
    Hashtbl.reset storage;
    Hashtbl.iter (Hashtbl.replace storage) values;
    let open Lwt.Syntax in
    let* result = match action with
      | Value -> Lwt.return (Ok (Vm.VInt value, 0))
      | Balance -> Lwt.return (Ok (Vm.VInt (ctx.get_balance address), 0))
      | Transfer (target, amount) ->
        if fuel < 50 then Lwt.return (Error "circle transfer effort exhausted") else
        let result = Vm.is_valid_addr target && Z.sign amount >= 0
          && (Z.equal amount Z.zero || ctx.do_transfer address target amount) in
        Lwt.return (Ok (Vm.VBool result, 50))
      | Call (target, method_name, params) ->
        if depth >= Vm.call_depth_max then Lwt.return (Error "circle call depth exceeded")
        else if not (Vm.is_valid_addr target) then Lwt.return (Error "circle address invalid")
        else
          let scope = Vm.{depth = depth + 1; limit = Some fuel;
            memory = ctx.fhe_memory; bytes = ctx.byte_work} in
          let* result = ctx.call_async address target method_name params scope in
          Lwt.return (Result.map (fun (result : Vm.subcall_result) ->
            events := (index, result.Vm.events) :: !events;
            result.return_value, result.effort_used) result) in
    match result with
    | Error error -> Lwt.return (refused error)
    | Ok (value, effort) ->
      match response value with
      | Error error -> Lwt.return (refused error)
      | Ok bytes -> Lwt.return (match reply ~id ~bytes ~effort:(spent + effort) storage with
          | Ok payload -> payload
          | Error error -> refused error)

let merge_events native nested =
  let rec merge index result pending = function
    | [] -> List.rev result @ List.concat_map snd pending
    | event :: rest ->
      let ready, pending = List.partition (fun (at, _) -> at = index) pending in
      let result = List.fold_left (fun result (_, events) -> List.rev_append events result) result ready in
      merge (index + 1) (event :: result) pending rest in
  merge 0 [] (List.rev nested) native