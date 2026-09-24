(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Rpc = Octra_core.Rpc
module Ledger = Octra_core.Ledger
module Store_irmin = Octra_core.Store_irmin
module Store_chaindata = Octra_core.Store_chaindata

type rpc_result = (Yojson.Safe.t, Rpc.rpc_error) result

let view_effort_limit = 1_000_000
let max_compile_source_bytes = 1_048_576
let max_compile_total_bytes = 2_097_152
let max_compile_files = 64
let max_compile_path_bytes = 512

let ok_lwt value =
  Lwt.return (Ok value)

let err_lwt err =
  Lwt.return (Error err)

let decode_real_abi = function
  | None ->
    None
  | Some raw ->
    try
      let json = Yojson.Safe.from_string raw in
      if Yojson.Safe.Util.member "functions" json <> `Null then Some json
      else None
    with _ ->
      None

let abi_result addr = function
  | Some abi_json ->
    ok_lwt (`Assoc [
      "address", `String addr;
      "abi", abi_json;
    ])
  | None ->
    err_lwt (Rpc.not_found "program ABI not found")

let derived_abi ~trusted ~point_ops ~store ~addr =
  match Contract.load_bytecode ~trusted ~point_ops store addr with
  | None ->
    err_lwt (Rpc.not_found "contract not found")
  | Some bytecode ->
    let methods = Contract.extract_methods bytecode in
    let method_list =
      List.map
        (fun (name, view) -> `Assoc ["name", `String name; "view", `Bool view])
        methods
    in
    ok_lwt (`Assoc [
      "address", `String addr;
      "methods", `List method_list;
      "instruction_count", `Int (Array.length bytecode);
    ])

let source_admission = function
  | Some meta -> String.equal meta.Store_irmin.admission "source"
  | None -> false

let current_program_record ~store ~chaindata ~addr =
  let open Lwt.Syntax in
  let* info = Store_irmin.get_contract_info store addr in
  let* meta = Store_irmin.get_contract_meta store addr in
  match info, source_admission meta with
  | None, _ -> Lwt.return_ok None
  | Some _, false -> Lwt.return_ok None
  | Some (_, code_hash, _, _), true ->
    begin
      match
        Store_chaindata.get_program_record
          chaindata
          ~address:addr
          ~code_hash
      with
      | Error _ as error -> Lwt.return error
      | Ok record ->
        let* current = Store_irmin.get_contract_info store addr in
        let* current_meta = Store_irmin.get_contract_meta store addr in
        begin
          match current, source_admission current_meta with
          | Some (_, current_hash, _, _), true
            when String.equal code_hash current_hash ->
            Lwt.return_ok record
          | Some _, _ -> Lwt.return_error "program changed during metadata read"
          | None, _ -> Lwt.return_error "program disappeared during metadata read"
        end
    end

let abi ~trusted ~point_ops ~store ~chaindata ~addr =
  let open Lwt.Syntax in
  let* record = current_program_record ~store ~chaindata ~addr in
  match record with
  | Error reason -> err_lwt (Rpc.err (-32000) reason None)
  | Ok (Some record) ->
    begin
      match decode_real_abi (Some record.Store_chaindata.abi) with
      | Some abi_json -> abi_result addr (Some abi_json)
      | None -> derived_abi ~trusted ~point_ops ~store ~addr
    end
  | Ok None -> derived_abi ~trusted ~point_ops ~store ~addr

let abi_params ~trusted ~point_ops ~store ~chaindata params =
  match Rpc.require_address params 0 "address" with
  | Error e ->
    err_lwt e
  | Ok addr ->
    abi ~trusted ~point_ops ~store ~chaindata ~addr

let file_source item =
  match item with
  | `Assoc fields ->
    let path =
      match List.assoc_opt "path" fields with
      | Some (`String value) -> value
      | _ -> ""
    in
    let source =
      match List.assoc_opt "source" fields with
      | Some (`String value) -> value
      | _ -> ""
    in
    if String.equal path "" then None else Some (path, source)
  | _ ->
    None

let compile_path_char = function
  | 'a'..'z'
  | 'A'..'Z'
  | '0'..'9'
  | '.'
  | '_'
  | '-'
  | '/' -> true
  | _ -> false

let valid_compile_path path =
  let length = String.length path in
  length > 0
  && length <= max_compile_path_bytes
  && path.[0] <> '/'
  && String.for_all compile_path_char path
  && (String.split_on_char '/' path
      |> List.for_all (fun part -> part <> "" && part <> "." && part <> ".."))

let validate_compile_input ?reserved_path source files =
  if String.length source > max_compile_source_bytes then
    Error "program source exceeds compile limit"
  else
    match files with
    | None -> Ok ()
    | Some items when List.length items > max_compile_files ->
      Error "program file count exceeds compile limit"
    | Some items ->
      let paths = Hashtbl.create (List.length items) in
      let rec check total = function
        | [] -> Ok ()
        | item :: rest ->
          begin
            match file_source item with
            | None -> Error "invalid program source file"
            | Some (path, item_source) ->
              let next = total + String.length path + String.length item_source in
              if not (valid_compile_path path) then
                Error "program source path is invalid"
              else if
                match reserved_path with
                | Some value -> String.equal path value
                | None -> false
              then
                Error "program source path is reserved"
              else if Hashtbl.mem paths path then
                Error "program source path is duplicated"
              else if String.length item_source > max_compile_source_bytes then
                Error "program source file exceeds compile limit"
              else if next > max_compile_total_bytes then
                Error "program sources exceed compile limit"
              else begin
                Hashtbl.add paths path ();
                check next rest
              end
          end
      in
      check (String.length source) items

let source_files source files_json =
  let files =
    List.filter_map
      (fun item ->
        match file_source item with
        | Some (path, item_source) -> Some (path, `String item_source)
        | None -> None)
      files_json
    |> List.sort (fun (left, _) (right, _) -> String.compare left right)
  in
  `Assoc (("main.aml", `String source) :: files)

let compile_source source files_json =
  match files_json with
  | Some files_json ->
    let file_map = Hashtbl.create 16 in
    List.iter
      (fun item ->
        match file_source item with
        | Some (path, item_source) -> Hashtbl.replace file_map path item_source
        | None -> ())
      files_json;
    Hashtbl.replace file_map "main.aml" source;
    let resolver path = Hashtbl.find_opt file_map path in
    Oct_compile.compile_multi resolver "main.aml"
  | None ->
    Oct_compile.compile source

let parse_optional_json raw =
  if String.equal raw "" then []
  else
    try ["verification", Yojson.Safe.from_string raw]
    with _ -> []

let parse_certificate_json raw =
  if String.equal raw "" then []
  else
    try ["certificate", Yojson.Safe.from_string raw]
    with _ -> []

let verified_record_response ~published record =
  let report =
    match record.Store_chaindata.report with
    | Some raw -> parse_optional_json raw
    | None -> []
  in
  let certificate =
    match record.Store_chaindata.certificate with
    | Some raw -> parse_certificate_json raw
    | None -> []
  in
  `Assoc
    ([
       "verified", `Bool true;
       "published", `Bool published;
       "code_hash", `String record.Store_chaindata.code_hash;
     ]
     @ report
     @ certificate)

let aml_result ~syntax source =
  match Aml_source.compile ~syntax source with
  | Error error -> Error error
  | Ok compiled ->
    Ok
      (Oct_compile.source_result ~syntax ~abi:Oct_compile.Source_abi
         ~source_mode:"single"
         ~source_material:source
         compiled)

let aml_multi_result ~syntax resolver main_path sources =
  match Aml_source.compile_multi ~syntax resolver main_path with
  | Error error -> Error error
  | Ok compiled ->
    Ok
      (Oct_compile.source_result ~syntax ~abi:Oct_compile.Source_abi
         ~source_mode:"multi"
         ~source_material:(Oct_compile.ordered_sources sources)
         compiled)

let aml_source_result ~syntax source files_json =
  match files_json with
  | None -> aml_result ~syntax source
  | Some files_json ->
    let file_map = Hashtbl.create 16 in
    List.iter
      (fun item ->
        match file_source item with
        | Some (path, item_source) -> Hashtbl.replace file_map path item_source
        | None -> ())
      files_json;
    Hashtbl.replace file_map "main.aml" source;
    let resolver path = Hashtbl.find_opt file_map path in
    let sources =
      Hashtbl.fold (fun path body rows -> (path, body) :: rows) file_map []
    in
    aml_multi_result ~syntax resolver "main.aml" sources

let compile_assembly_response ~bytecode_b64 ~bytecode_size ~instructions =
  `Assoc [
    "bytecode", `String bytecode_b64;
    "size", `Int bytecode_size;
    "instructions", `Int instructions;
  ]

let compile_result_response ?deploy_payload (result : Oct_compile.compile_result) =
  let executable =
    Option.value result.program_envelope ~default:result.bytecode
  in
  let bytecode_b64 = Base64.encode_exn executable in
  let disasm =
    match Admission.decode result.bytecode with
    | Ok admitted -> Assembler.emit (Admission.code admitted)
    | Error _ -> ""
  in
  let envelope =
    match result.program_envelope with
    | None -> []
    | Some raw -> ["program_envelope", `String (Base64.encode_exn raw)]
  in
  let package =
    match deploy_payload with
    | None -> []
    | Some raw -> ["deploy_payload", `String (Base64.encode_exn raw)]
  in
  `Assoc ([
    "bytecode", `String bytecode_b64;
    "size", `Int (String.length executable);
    "instructions", `Int result.instructions;
    "abi", `String result.abi_json;
    "version", `String result.version;
    "disasm", `String disasm;
  ] @ envelope @ package @ parse_optional_json result.verification_json
    @ parse_certificate_json result.certificate_json)

let compile_assembly ~source =
  if String.length source > max_compile_source_bytes then
    err_lwt (Rpc.invalid_params "assembly source exceeds compile limit")
  else try
    let instrs = Assembler.parse source in
    match Admission.of_code instrs with
    | Error error ->
      err_lwt (Rpc.err (-32000) (Admission.error_message error) None)
    | Ok admitted ->
      let code = Admission.code admitted in
      let bytecode_raw = Bytecode.encode code in
      ok_lwt (compile_assembly_response
        ~bytecode_b64:(Base64.encode_exn bytecode_raw)
        ~bytecode_size:(String.length bytecode_raw)
        ~instructions:(Array.length code))
  with exn ->
    err_lwt (Rpc.err (-32000)
      (Printf.sprintf "compile error: %s" (Printexc.to_string exn)) None)

let compile_assembly_params params =
  match Rpc.require_string params 0 "source" with
  | Error e ->
    err_lwt e
  | Ok source ->
    compile_assembly ~source

let source_is_program source =
  try
    let ast = Oct_parse.parse source in
    ast.Oct_lang.declaration = Oct_lang.ProgramDecl
  with _ ->
    false

let compile_program_source ?(compiler = Program_package.Protocol) ~point_ops source =
  match
    Program_package.compile_with ~compiler
      ~point_ops
      ~main:"main.aml"
      ~sources:[Program_package.{ path = "main.aml"; body = source }]
  with
  | Error error ->
    err_lwt
      (Rpc.err
         (-32000)
         (Program_package.error_message error)
         None)
  | Ok compiled ->
    ok_lwt
      (compile_result_response
         ~deploy_payload:compiled.package
         compiled.result)

let compiler_syntax = function
  | Program_package.Protocol -> Oct_gen.Forms
  | Program_package.Source -> Oct_gen.Source

let compile_aml_with ~compiler ~point_ops ~program:_ ~source =
  match validate_compile_input source None with
  | Error msg -> err_lwt (Rpc.invalid_params msg)
  | Ok () when source_is_program source ->
    compile_program_source ~compiler ~point_ops source
  | Ok () ->
    begin
      match aml_result ~syntax:(compiler_syntax compiler) source with
      | Error msg -> err_lwt (Rpc.err (-32000) msg None)
      | Ok result ->
      ok_lwt (compile_result_response result)
    end

let compile_aml_request = compile_aml_with ~compiler:Program_package.Protocol

let compile_aml ~source =
  compile_aml_request ~point_ops:true ~program:false ~source

let compile_file_map files_json =
  let file_map = Hashtbl.create 16 in
  begin
    match files_json with
    | `List items ->
      List.iter
        (fun item ->
          match file_source item with
          | Some (path, item_source) -> Hashtbl.replace file_map path item_source
          | None -> ())
        items
    | _ ->
      ()
  end;
  file_map

let compile_aml_multi_with ~compiler ~point_ops ~json =
  match json with
  | None ->
    err_lwt (Rpc.invalid_params "expected {files, main}")
  | Some obj ->
    let files_json =
      match obj with
      | `Assoc fields ->
        begin
          match List.assoc_opt "files" fields with
          | Some value -> value
          | None -> `Null
        end
      | _ ->
        `Null
    in
    let main_path =
      match obj with
      | `Assoc fields ->
        begin
          match List.assoc_opt "main" fields with
          | Some (`String value) -> value
          | _ -> "main.aml"
        end
      | _ ->
        "main.aml"
    in
    let program_field =
      match obj with
      | `Assoc fields ->
        (match List.assoc_opt "program" fields with
         | None
         | Some (`Bool _) -> Ok ()
         | Some _ -> Error "program must be boolean")
      | _ -> Error "expected object"
    in
    match program_field, validate_compile_input "" (Some (match files_json with
      | `List items -> items
      | _ -> [])) with
    | Error msg, _ -> err_lwt (Rpc.invalid_params msg)
    | _, Error msg -> err_lwt (Rpc.invalid_params msg)
    | Ok (), Ok () ->
      let file_map = compile_file_map files_json in
      let resolver path = Hashtbl.find_opt file_map path in
      if
        match resolver main_path with
        | Some source -> source_is_program source
        | None -> false
      then
        let sources =
          match files_json with
          | `List items ->
            List.filter_map
              (fun item ->
                Option.map
                  (fun (path, body) -> Program_package.{ path; body })
                  (file_source item))
              items
          | _ -> []
        in
        begin
          match Program_package.compile_with ~compiler ~point_ops ~main:main_path ~sources with
          | Error error ->
            err_lwt
              (Rpc.err
                 (-32000)
                 (Program_package.error_message error)
                 None)
          | Ok compiled ->
            ok_lwt
              (compile_result_response
                 ~deploy_payload:compiled.package
                 compiled.result)
        end
      else
        let sources =
          Hashtbl.fold (fun path body rows -> (path, body) :: rows) file_map []
        in
        begin
          match aml_multi_result ~syntax:(compiler_syntax compiler) resolver main_path sources with
          | Error msg -> err_lwt (Rpc.err (-32000) msg None)
          | Ok result -> ok_lwt (compile_result_response result)
        end

let compile_aml_multi_for ~point_ops ~json =
  compile_aml_multi_with ~compiler:Program_package.Protocol ~point_ops ~json

let compile_aml_multi ~json =
  compile_aml_multi_for ~point_ops:true ~json

let compile_aml_params ?(compiler = Program_package.Protocol)
    ?(point_ops = true) params =
  match Rpc.require_string params 0 "source" with
  | Error e ->
    err_lwt e
  | Ok source ->
    let program =
      match Rpc.param_json params 1 with
      | Some (`Bool value) -> Ok value
      | None -> Ok false
      | Some _ -> Error (Rpc.invalid_params "program must be boolean")
    in
    begin
      match program with
      | Error error -> err_lwt error
      | Ok program -> compile_aml_with ~compiler ~point_ops ~program ~source
    end

let compute_address ~bytecode_b64 ~deployer ~nonce =
  try
    let bytecode_raw = Base64.decode_exn bytecode_b64 in
    let address = Contract.addr_from_code bytecode_raw deployer nonce in
    ok_lwt (`Assoc [
      "address", `String address;
      "deployer", `String deployer;
      "nonce", `Int nonce;
    ])
  with exn ->
    err_lwt (Rpc.err (-32000)
      (Printf.sprintf "error: %s" (Printexc.to_string exn)) None)

let compute_address_params params =
  match Rpc.require_string params 0 "bytecode_b64",
        Rpc.require_address params 1 "deployer" with
  | Error e, _ | _, Error e ->
    err_lwt e
  | Ok bytecode_b64, Ok deployer ->
    let nonce =
      match Rpc.param_json params 2 with
      | Some (`Int n) -> n
      | _ -> 0
    in
    compute_address ~bytecode_b64 ~deployer ~nonce

let contract_row ~ledger ~addr ~code_hash ~version ~owner =
  let balance =
    match Ledger.find_opt ledger addr with
    | Some account -> Z.to_string account.Ledger.balance
    | None -> "0"
  in
  `Assoc [
    "address", `String addr;
    "owner", `String owner;
    "code_hash", `String code_hash;
    "version", `String version;
    "balance", `String balance;
  ]

let program_info ~store ~ledger ~addr =
  let open Lwt.Syntax in
  let* info = Store_irmin.get_contract_info store addr in
  match info with
  | Some (_, code_hash, version, owner) ->
    ok_lwt (`Assoc [
      "address", `String addr;
      "version", `String version;
      "code_hash", `String code_hash;
      "balance",
      `String
        (match Ledger.find_opt ledger addr with
        | Some account -> Z.to_string account.Ledger.balance
        | None -> "0");
      "owner", `String owner;
    ])
  | None ->
    err_lwt (Rpc.not_found "contract not found")

let program_info_params ~store ~ledger params =
  match Rpc.require_address params 0 "address" with
  | Error e ->
    err_lwt e
  | Ok addr ->
    program_info ~store ~ledger ~addr

let token_page_int params index default_value =
  Option.value ~default:default_value (Rpc.param_int params index)

let list_contracts ~store ~ledger ~offset ~limit =
  let open Lwt.Syntax in
  let* page = Store_irmin.list_contracts_page store ~offset ~limit in
  let* contracts =
    Lwt_list.filter_map_s
      (fun addr ->
        let* info = Store_irmin.get_contract_info store addr in
        match info with
        | Some (_, code_hash, version, owner) ->
          Lwt.return_some (contract_row ~ledger ~addr ~code_hash ~version ~owner)
        | None ->
          Lwt.return_none)
      page.addresses
  in
  ok_lwt (`Assoc [
    "contracts", `List contracts;
    "count", `Int (List.length contracts);
    "offset", `Int offset;
    "limit", `Int limit;
    "next_offset",
      (if page.more then `Int (offset + List.length contracts) else `Null);
    "more", `Bool page.more;
  ])

let list_contracts_params ~store ~ledger params =
  let offset = token_page_int params 0 0 in
  let limit = token_page_int params 1 Token_rpc_policy.max_page_rows in
  if offset < 0 || offset > Token_rpc_policy.max_scan_programs then
    err_lwt (Rpc.invalid_params "program offset outside read limit")
  else if limit <= 0 || limit > Token_rpc_policy.max_page_rows then
    err_lwt (Rpc.invalid_params "program page size outside read limit")
  else
    list_contracts ~store ~ledger ~offset ~limit

let max_storage_display_len = 4096

let storage_read_limit = function
  | Some (`String "full") ->
    Contract_vm.max_storage_value_len
  | Some (`String value) ->
    begin
      try min Contract_vm.max_storage_value_len (max 0 (int_of_string value))
      with _ -> max_storage_display_len
    end
  | Some (`Int value) ->
    min Contract_vm.max_storage_value_len (max 0 value)
  | _ ->
    max_storage_display_len

let storage_visible ~limit value =
  if String.length value > limit then String.sub value 0 limit else value

let storage_value ~key ~value ~limit =
  let size = String.length value in
  `Assoc [
    "key", `String key;
    "value", `String (storage_visible ~limit value);
    "size", `Int size;
    "truncated", `Bool (size > limit);
    "limit", `Int limit;
  ]

let storage_missing ~key =
  `Assoc [
    "key", `String key;
    "value", `Null;
    "size", `Int 0;
    "truncated", `Bool false;
  ]

let contract_storage ~store ~addr ~key ~limit_json =
  let open Lwt.Syntax in
  let limit = storage_read_limit limit_json in
  let* value = Store_irmin.read_contract_storage_key store addr key in
  match value with
  | Some value ->
    ok_lwt (storage_value ~key ~value ~limit)
  | None ->
    ok_lwt (storage_missing ~key)

let contract_storage_params ~store params =
  match Rpc.require_address params 0 "address",
        Rpc.require_string params 1 "key" with
  | Error e, _ | _, Error e ->
    err_lwt e
  | Ok addr, Ok key ->
    contract_storage
      ~store
      ~addr
      ~key
      ~limit_json:(Rpc.param_json params 2)

let contract_storage_dump_params ~store:_ _params =
  err_lwt
    (Rpc.err
       (-32601)
       "program storage dump is disabled; query explicit keys"
       None)

let program_bytecode ~store ~addr =
  let open Lwt.Syntax in
  let* bytecode = Store_irmin.load_bytecode store addr in
  match bytecode with
  | Some bytecode_b64 ->
    let* info = Store_irmin.get_contract_info store addr in
    let code_hash =
      match info with
      | Some (_, hash, _, _) -> hash
      | None -> ""
    in
    ok_lwt (`Assoc [
      "address", `String addr;
      "bytecode", `String bytecode_b64;
      "code_hash", `String code_hash;
      "size", `Int (String.length bytecode_b64);
    ])
  | None ->
    err_lwt (Rpc.not_found "bytecode not found for address")

let program_bytecode_params ~store params =
  match Rpc.require_address params 0 "address" with
  | Error e ->
    err_lwt e
  | Ok addr ->
    program_bytecode ~store ~addr

let token_actor_error = function
  | Token_rpc_actor.Busy ->
    Rpc.err (-32005) "Program token read busy" None
  | Token_rpc_actor.Stopped ->
    Rpc.service_unavailable
  | Token_rpc_actor.Read_failed ->
    Rpc.err (-32000) "Program token read failed" None

let tokens_by_address_params ~actor params =
  match Rpc.require_address params 0 "address" with
  | Error e -> err_lwt e
  | Ok holder ->
    let offset = token_page_int params 1 0 in
    let limit =
      token_page_int params 2 Token_rpc_policy.max_page_rows
    in
    if offset < 0 || offset > Token_rpc_policy.max_scan_programs then
      err_lwt (Rpc.invalid_params "token offset outside read limit")
    else if limit <= 0 || limit > Token_rpc_policy.max_page_rows then
      err_lwt (Rpc.invalid_params "token page size outside read limit")
    else
      let open Lwt.Syntax in
      let* result = Token_rpc_actor.query actor ~holder ~offset ~limit in
      match result with
      | Ok payload -> ok_lwt payload
      | Error error -> err_lwt (token_actor_error error)

let verify_active = ref false

let run_verify handler =
  if !verify_active then
    Lwt.return_error
      (Rpc.err (-32005) "Program verification busy" None)
  else begin
    verify_active := true;
    Lwt.finalize
      handler
      (fun () ->
        verify_active := false;
        Lwt.return_unit)
    |> Lwt.protected
  end

let verify_compilation ~meta ~source ~files_json =
  match meta with
  | Some meta when String.equal meta.Store_irmin.admission "source" ->
    let sources =
      Program_package.{ path = "main.aml"; body = source }
      ::
      (Option.value files_json ~default:[]
       |> List.filter_map (fun item ->
         match file_source item with
         | Some (path, body) ->
           Some Program_package.{ path; body }
         | None -> None))
    in
    let current = Program_package.compile ~main:"main.aml" ~sources in
    let source_result = Program_package.compile_with ~compiler:Program_package.Source
      ~point_ops:true ~main:"main.aml" ~sources in
    let prior = Program_package.compile_for ~point_ops:false
      ~main:"main.aml" ~sources in
    let results = List.filter_map (function
      | Ok (compiled : Program_package.compiled) ->
        Some (compiled.envelope, compiled.result)
      | Error _ -> None
    ) [current; prior; source_result] in
    begin
      match results, current with
      | [], Error error -> Error (Program_package.error_message error)
      | _ -> Ok results
    end
  | Some _
  | None ->
    let current = aml_source_result ~syntax:Oct_gen.Source source files_json in
    let forms = aml_source_result ~syntax:Oct_gen.Forms source files_json in
    let prior = compile_source source files_json in
    let results =
      List.filter_map (function
        | Ok result -> Some (result.Oct_compile.bytecode, result)
        | Error _ -> None) [current; forms]
    in
    let results =
      match prior.error with
      | None when
          not
            (List.exists
               (fun (code, _) -> String.equal code prior.bytecode)
               results) ->
        results @ [prior.bytecode, prior]
      | None | Some _ -> results
    in
    begin
      match results, current, prior.error with
      | _ :: _, _, _ -> Ok results
      | [], Error msg, _ -> Error msg
      | [], Ok _, Some msg -> Error msg
      | [], Ok _, None -> Error "compiler result is absent"
    end

let run_verify_worker handler =
  Lwt.catch
    (fun () -> Lwt_preemptive.detach handler ())
    (function
      | Out_of_memory as error -> Lwt.fail error
      | Lwt.Canceled as error -> Lwt.fail error
      | Stack_overflow ->
        Lwt.return_error "compiler complexity limit exceeded"
      | _ -> Lwt.return_error "compiler failed")

let verify_request ~store ~chaindata ~addr ~source ~files_json =
  let open Lwt.Syntax in
  match validate_compile_input ~reserved_path:"main.aml" source files_json with
  | Error msg -> err_lwt (Rpc.invalid_params msg)
  | Ok () ->
    let* stored_b64 =
      Store_irmin.read store ["contracts"; addr; "bytecode"]
    in
    match stored_b64 with
    | None ->
      err_lwt (Rpc.not_found "contract not found or no bytecode")
    | Some b64 ->
      let* meta = Store_irmin.get_contract_meta store addr in
      let stored =
        match Base64.decode b64 with
        | Ok raw when String.equal (Base64.encode_exn raw) b64 -> Ok raw
        | Ok _
        | Error _ -> Error "stored Program encoding is invalid"
      in
      match stored with
      | Error msg -> err_lwt (Rpc.err (-32000) msg None)
      | Ok stored ->
        let* compilation =
          run_verify_worker (fun () ->
            verify_compilation ~meta ~source ~files_json)
        in
        match compilation with
        | Error msg ->
          err_lwt
            (Rpc.err
               (-32000)
               (Printf.sprintf "compile error: %s" msg)
               None)
        | Ok compilations ->
          let stored_hash =
            Digestif.SHA256.(digest_string stored |> to_hex)
          in
          let matching =
            List.find_opt
              (fun (compiled, _) ->
                String.equal
                  stored_hash
                  Digestif.SHA256.(digest_string compiled |> to_hex))
              compilations
          in
          begin
            match matching with
            | None ->
              let compiled_hash =
                match compilations with
                | (compiled, _) :: _ ->
                  Digestif.SHA256.(digest_string compiled |> to_hex)
                | [] -> ""
              in
            err_lwt
              (Rpc.err
                 (-32000)
                 (Printf.sprintf
                    "bytecode mismatch: stored=%s compiled=%s"
                    (String.sub stored_hash 0 16)
                    (String.sub compiled_hash 0 16))
                 None)
            | Some (_, result) ->
              let source_json =
                match files_json with
                | Some files -> Yojson.Safe.to_string (source_files source files)
                | None -> source
              in
              let record = Store_chaindata.{
                source = source_json;
                abi = result.abi_json;
                report =
                  (if String.equal result.verification_json "" then None
                   else Some result.verification_json);
                certificate =
                  (if String.equal result.certificate_json "" then None
                   else Some result.certificate_json);
                code_hash = stored_hash;
              } in
              let* current = Store_irmin.get_contract_info store addr in
              let* current_meta = Store_irmin.get_contract_meta store addr in
              begin
                match current with
                | Some (_, code_hash, _, _)
                  when String.equal code_hash stored_hash ->
                  if not (source_admission meta) then
                    ok_lwt (verified_record_response ~published:false record)
                  else if not (source_admission current_meta) then
                    err_lwt
                      (Rpc.err
                         (-32000)
                         "program changed during verification"
                         None)
                  else begin
                    let* saved =
                      Lwt_preemptive.detach
                        (fun () ->
                          Store_chaindata.save_program_record
                            chaindata
                            addr
                            record)
                        ()
                    in
                    match saved with
                    | Error Store_chaindata.Program_record_conflict ->
                      err_lwt
                        (Rpc.err
                           (-32000)
                           "program record differs"
                           (Some
                              (`Assoc
                                 ["reason", `String "record_conflict"])))
                    | Error (Store_chaindata.Program_record_store_error reason) ->
                      err_lwt (Rpc.err (-32000) reason None)
                    | Ok saved ->
                      ok_lwt
                        (verified_record_response ~published:true saved)
                  end
                | Some _ ->
                  err_lwt
                    (Rpc.err
                       (-32000)
                       "program changed during verification"
                       None)
                | None -> err_lwt (Rpc.not_found "program not found")
              end
          end

let verify ~store ~chaindata ~addr ~source ~files_json =
  run_verify (fun () ->
    verify_request ~store ~chaindata ~addr ~source ~files_json)

let verify_params ~store ~chaindata params =
  match Rpc.require_address params 0 "address",
        Rpc.require_string params 1 "source" with
  | Error e, _ | _, Error e ->
    err_lwt e
  | Ok addr, Ok source ->
    let files_json =
      match Rpc.param_json params 2 with
      | Some (`List files) -> Some files
      | _ -> None
    in
    verify ~store ~chaindata ~addr ~source ~files_json

let source_meta_fields ~verification ~certificate =
  let verification_fields =
    match verification with
    | Some raw ->
      begin
        try ["verification", Yojson.Safe.from_string raw]
        with _ -> []
      end
    | None ->
      []
  in
  let certificate_fields =
    match certificate with
    | Some raw ->
      begin
        try ["certificate", Yojson.Safe.from_string raw]
        with _ -> []
      end
    | None ->
      []
  in
  verification_fields @ certificate_fields

let source_response source meta_fields =
  match source with
  | None ->
    `Assoc (["source", `Null] @ meta_fields)
  | Some raw ->
    try
      match Yojson.Safe.from_string raw with
      | `Assoc files ->
        let main =
          match List.assoc_opt "main.aml" files with
          | Some (`String value) -> value
          | _ -> raw
        in
        `Assoc ([
          "source", `String main;
          "files", `Assoc files;
        ] @ meta_fields)
      | _ ->
        `Assoc (["source", `String raw] @ meta_fields)
    with _ ->
      `Assoc (["source", `String raw] @ meta_fields)

let source ~store ~chaindata ~addr =
  let open Lwt.Syntax in
  let* record = current_program_record ~store ~chaindata ~addr in
  match record with
  | Error reason -> err_lwt (Rpc.err (-32000) reason None)
  | Ok (Some record) ->
    let meta_fields =
      source_meta_fields
        ~verification:record.Store_chaindata.report
        ~certificate:record.certificate
    in
    ok_lwt (source_response (Some record.source) meta_fields)
  | Ok None -> ok_lwt (source_response None [])

let source_params ~store ~chaindata params =
  match Rpc.require_address params 0 "address" with
  | Error e ->
    err_lwt e
  | Ok addr ->
    source ~store ~chaindata ~addr

let receipt ~chaindata ~tx_hash =
  match Store_chaindata.get_contract_receipt_raw chaindata ~tx_hash with
  | Some raw ->
    begin
      try ok_lwt (Yojson.Safe.from_string raw)
      with _ -> ok_lwt (`String raw)
    end
  | None ->
    err_lwt (Rpc.not_found "receipt not found")

let receipt_params ~chaindata params =
  match Rpc.require_hash params 0 "hash" with
  | Error e ->
    err_lwt e
  | Ok tx_hash ->
    receipt ~chaindata ~tx_hash

let view_fhe_capability_gate () =
  let verifier_available = ref true in
  function
    | Contract_vm.Fhe_verify_zero_cap
    | Contract_vm.Fhe_verify_range_cap
    | Contract_vm.Fhe_verify_bound_cap ->
      if !verifier_available then begin
        verifier_available := false;
        true
      end else
        false
    | _ ->
      true

let view_active = ref false
let view_seconds = 10.

let view_clock () =
  let active = Atomic.make true in
  let deadline = Octra_core.Pvac_verify_worker.monotonic_seconds () +. view_seconds in
  let running () =
    Atomic.get active
    && Octra_core.Pvac_verify_worker.monotonic_seconds () < deadline
  in
  running, (fun () -> Atomic.set active false)

let run_view ?(seconds = view_seconds) ?(stop = Fun.id) handler =
  if !view_active then
    Lwt.return_error
      (Rpc.err (-32005) "Program view busy" None)
  else begin
    view_active := true;
    let work = Lwt.finalize
      (fun () ->
        Lwt_preemptive.detach handler ()
        |> Lwt.map (fun value -> Ok value))
      (fun () ->
        view_active := false;
        Lwt.return_unit)
    in
    let timer =
      let open Lwt.Syntax in
      let* () = Lwt_unix.sleep seconds in
      stop ();
      Lwt.return_error (Rpc.err (-32005) "Program view time limit exceeded" None)
    in
    let response = Lwt.pick [Lwt.protected work; timer] in
    Lwt.on_cancel response stop;
    response
  end

type view_profile = {
  epoch : int;
  point_ops : bool;
  math : bool;
  object_cost : bool;
  int_work : Int_work.mode;
}

let view_profile rules ~epoch =
  let module R = Octra_core.Rule_graph in
  let ( let* ) = Result.bind in
  let* standard = R.standard rules ~epoch in
  let* math = R.math rules ~epoch in
  let* object_cost = R.object_cost rules ~epoch in
  Ok {
    epoch;
    point_ops = standard = R.Active;
    math = math = R.Active;
    object_cost = object_cost = R.Active;
    int_work = if standard = R.Active then Int_work.Active else Int_work.Prior;
  }

let make_view_ctx ?running ~trusted ~profile ~store ~ledger ~get_fhe_pubkey () =
  let get_balance addr =
    match Ledger.find_opt ledger addr with
    | Some account -> account.Ledger.balance
    | None -> Z.zero
  in
  let allow_fhe_capability = view_fhe_capability_gate () in
  let rec view_ctx = {
    Contract_vm.default_ctx with
    get_balance;
    get_fhe_pubkey;
    allow_fhe_capability;
    int_work = profile.int_work;
    point_ops = profile.point_ops;
    math = profile.math;
    object_cost = profile.object_cost;
    current_epoch = profile.epoch;
    do_transfer = (fun _ _ _ -> false);
    deploy_contract = (fun _ _ _ _ _ -> Error "deploy in view context");
    call_contract = (fun caller target method_name args depth ->
      let params = List.map Receipt_view.call_arg_json args in
      let result =
        Contract.execute_view_call
          ?running
          ~trusted
          ~ctx:view_ctx
          ~depth
          ~limit:view_effort_limit
          store
          target
          method_name
          params
          caller
      in
      match Contract.exec_result_to_result result with
      | Ok value ->
        Ok {
          Contract_vm.return_value = value;
          effort_used = result.Contract.effort_used;
          events = result.Contract.events;
        }
      | Error err ->
        Error err);
  } in
  view_ctx

let view_storage_key_limit = 64
let view_storage_value_limit = 4096

let call_result ~store ~addr ~include_storage ~storage_json value =
  let open Lwt.Syntax in
  if include_storage then
    let* page =
      Store_irmin.list_contract_storage_page
        store
        addr
        ~limit:view_storage_key_limit
        ~value_limit:view_storage_value_limit
    in
    ok_lwt (`Assoc [
      "result", value;
      "storage", storage_json page.entries;
      "storage_more", `Bool page.more;
      "storage_limit", `Int view_storage_key_limit;
    ])
  else
    ok_lwt (`Assoc ["result", value])

let call ~trusted ~profile ~store ~ledger ~get_fhe_pubkey ~storage_json
    ~addr ~method_name ~call_params ~caller_addr ~include_storage =
  let open Lwt.Syntax in
  if String.equal method_name "balance_of" then
    match call_params with
    | [`String holder_addr] ->
      let* balance =
        Store_irmin.read_contract_storage_key
          store
          addr
          ("balances:" ^ holder_addr)
      in
      call_result
        ~store
        ~addr
        ~include_storage
        ~storage_json
        (`String (Option.value ~default:"0" balance))
    | _ ->
      err_lwt (Rpc.invalid_params "balance_of expects exactly one address parameter")
  else
    let running, stop = view_clock () in
    let view_ctx = make_view_ctx ~trusted ~profile ~running ~store ~ledger ~get_fhe_pubkey () in
    let* executed =
      run_view ~stop (fun () ->
        Contract.execute_view_call
          ~running
          ~trusted
          ~ctx:view_ctx
          ~limit:view_effort_limit
          store
          addr
          method_name
          call_params
          caller_addr)
    in
    match executed with
    | Error error ->
      Lwt.return_error error
    | Ok result ->
      if result.Contract.success then
        call_result
          ~store
          ~addr
          ~include_storage
          ~storage_json
          (Receipt_view.return_json result.return_value)
      else
        err_lwt (Rpc.err (-32000) (Receipt_view.view_error result.error) None)

let call_params ~trusted ~profile ~store ~ledger ~get_fhe_pubkey ~storage_json params =
  match Rpc.require_address params 0 "address",
        Rpc.require_string params 1 "method" with
  | Error e, _ | _, Error e ->
    err_lwt e
  | Ok addr, Ok method_name ->
    let view_call =
      Call_plan.plan_readonly_call
        ~method_name
        ~params:(Rpc.param_json params 2)
        ~caller_addr:(Rpc.param_string params 3)
        ~include_storage:(Rpc.param_json params 4)
    in
    call
      ~trusted
      ~profile
      ~store
      ~ledger
      ~get_fhe_pubkey
      ~storage_json
      ~addr
      ~method_name:view_call.readonly_method_name
      ~call_params:view_call.readonly_params
      ~caller_addr:view_call.readonly_caller_addr
      ~include_storage:view_call.readonly_include_storage