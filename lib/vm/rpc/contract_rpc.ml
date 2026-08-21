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

let abi ~store ~addr =
  let open Lwt.Syntax in
  let* stored = Store_irmin.get_contract_abi store addr in
  match decode_real_abi stored with
  | Some abi_json ->
    ok_lwt (`Assoc [
      "address", `String addr;
      "abi", abi_json;
    ])
  | None ->
    match Contract.load_bytecode store addr with
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

let abi_params ~store params =
  match Rpc.require_address params 0 "address" with
  | Error e ->
    err_lwt e
  | Ok addr ->
    abi ~store ~addr

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

let validate_compile_input source files =
  if String.length source > max_compile_source_bytes then
    Error "program source exceeds compile limit"
  else
    match files with
    | None -> Ok ()
    | Some items when List.length items > max_compile_files ->
      Error "program file count exceeds compile limit"
    | Some items ->
      let rec check total = function
        | [] -> Ok ()
        | item :: rest ->
          begin
            match file_source item with
            | None -> Error "invalid program source file"
            | Some (path, item_source) ->
              let next = total + String.length path + String.length item_source in
              if String.length path > max_compile_path_bytes then
                Error "program source path exceeds compile limit"
              else if String.length item_source > max_compile_source_bytes then
                Error "program source file exceeds compile limit"
              else if next > max_compile_total_bytes then
                Error "program sources exceed compile limit"
              else
                check next rest
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

let compile_aml_request ~program ~source =
  match validate_compile_input source None with
  | Error msg -> err_lwt (Rpc.invalid_params msg)
  | Ok () when program ->
    begin
      match
        Program_package.compile
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
    end
  | Ok () ->
    let result = Oct_compile.compile source in
    match result.error with
    | Some msg ->
      err_lwt (Rpc.err (-32000) msg None)
    | None ->
      ok_lwt (compile_result_response result)

let compile_aml ~source =
  compile_aml_request ~program:false ~source

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

let compile_aml_multi ~json =
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
    let program_only =
      match obj with
      | `Assoc fields ->
        (match List.assoc_opt "program" fields with
         | None -> Ok false
         | Some (`Bool value) -> Ok value
         | Some _ -> Error "program must be boolean")
      | _ -> Error "expected object"
    in
    match program_only, validate_compile_input "" (Some (match files_json with
      | `List items -> items
      | _ -> [])) with
    | Error msg, _ -> err_lwt (Rpc.invalid_params msg)
    | _, Error msg -> err_lwt (Rpc.invalid_params msg)
    | Ok program_only, Ok () ->
      if program_only then
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
          match Program_package.compile ~main:main_path ~sources with
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
        let file_map = compile_file_map files_json in
        let resolver path = Hashtbl.find_opt file_map path in
        let result = Oct_compile.compile_multi resolver main_path in
        match result.error with
        | Some msg ->
          err_lwt (Rpc.err (-32000) msg None)
        | None ->
          ok_lwt (compile_result_response result)

let compile_aml_params params =
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
      | Ok program -> compile_aml_request ~program ~source
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

let list_contracts ~store ~ledger =
  let open Lwt.Syntax in
  let* addrs = Store_irmin.list_contracts store in
  let* contracts =
    Lwt_list.filter_map_s
      (fun addr ->
        let* info = Store_irmin.get_contract_info store addr in
        match info with
        | Some (_, code_hash, version, owner) ->
          Lwt.return_some (contract_row ~ledger ~addr ~code_hash ~version ~owner)
        | None ->
          Lwt.return_none)
      addrs
  in
  ok_lwt (`Assoc [
    "contracts", `List contracts;
    "count", `Int (List.length contracts);
  ])

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

let storage_dump ~address storage_pairs =
  `Assoc [
    "address", `String address;
    "storage", `Assoc (List.map (fun (key, value) -> key, `String value) storage_pairs);
    "storage_sizes", `Assoc (List.map (fun (key, value) -> key, `Int (String.length value)) storage_pairs);
    "count", `Int (List.length storage_pairs);
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

let contract_storage_dump ~store ~addr =
  let open Lwt.Syntax in
  let* storage_pairs = Store_irmin.list_contract_storage store addr in
  ok_lwt (storage_dump ~address:addr storage_pairs)

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

let token_page_int params index default_value =
  Option.value ~default:default_value (Rpc.param_int params index)

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

let verify ~store ~addr ~source ~files_json =
  let open Lwt.Syntax in
  match validate_compile_input source files_json with
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
        let compilation =
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
            begin
              match Program_package.compile ~main:"main.aml" ~sources with
              | Ok compiled -> Ok (compiled.envelope, compiled.result)
              | Error error ->
                Error (Program_package.error_message error)
            end
          | Some _
          | None ->
            let result = compile_source source files_json in
            begin
              match result.error with
              | Some msg -> Error msg
              | None -> Ok (result.bytecode, result)
            end
        in
        match compilation with
        | Error msg ->
          err_lwt
            (Rpc.err
               (-32000)
               (Printf.sprintf "compile error: %s" msg)
               None)
        | Ok (compiled, result) ->
          let stored_hash =
            Digestif.SHA256.(digest_string stored |> to_hex)
          in
          let compiled_hash =
            Digestif.SHA256.(digest_string compiled |> to_hex)
          in
          if not (String.equal stored_hash compiled_hash) then
            err_lwt
              (Rpc.err
                 (-32000)
                 (Printf.sprintf
                    "bytecode mismatch: stored=%s compiled=%s"
                    (String.sub stored_hash 0 16)
                    (String.sub compiled_hash 0 16))
                 None)
          else
            ok_lwt
              (`Assoc
                 ([
                    "verified", `Bool true;
                    "code_hash", `String stored_hash;
                  ]
                  @ parse_optional_json result.verification_json
                  @ parse_certificate_json result.certificate_json))

let verify_params ~store params =
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
    verify ~store ~addr ~source ~files_json

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

let source ~store ~addr =
  let open Lwt.Syntax in
  let* raw_source = Store_irmin.get_contract_source store addr in
  let* verification = Store_irmin.get_contract_verification store addr in
  let* certificate = Store_irmin.get_contract_certificate store addr in
  let meta_fields = source_meta_fields ~verification ~certificate in
  ok_lwt (source_response raw_source meta_fields)

let source_params ~store params =
  match Rpc.require_address params 0 "address" with
  | Error e ->
    err_lwt e
  | Ok addr ->
    source ~store ~addr

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

let run_view handler =
  if !view_active then
    Lwt.return_error
      (Rpc.err (-32005) "Program view busy" None)
  else begin
    view_active := true;
    Lwt.finalize
      (fun () ->
        Lwt_preemptive.detach handler ()
        |> Lwt.map (fun value -> Ok value))
      (fun () ->
        view_active := false;
        Lwt.return_unit)
  end

let make_view_ctx ~store ~ledger ~current_epoch ~get_fhe_pubkey =
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
    current_epoch;
    do_transfer = (fun _ _ _ -> false);
    deploy_contract = (fun _ _ _ _ _ -> Error "deploy in view context");
    call_contract = (fun caller target method_name args depth ->
      let params = List.map Receipt_view.call_arg_json args in
      let result =
        Contract.execute_view_call
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

let call_result ~store ~addr ~include_storage ~storage_json value =
  let open Lwt.Syntax in
  if include_storage then
    let* storage_pairs = Store_irmin.list_contract_storage store addr in
    ok_lwt (`Assoc [
      "result", value;
      "storage", storage_json storage_pairs;
    ])
  else
    ok_lwt (`Assoc ["result", value])

let call ~store ~ledger ~current_epoch ~get_fhe_pubkey ~storage_json
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
    let view_ctx = make_view_ctx ~store ~ledger ~current_epoch ~get_fhe_pubkey in
    let* executed =
      run_view (fun () ->
        Contract.execute_view_call
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

let call_params ~store ~ledger ~current_epoch ~get_fhe_pubkey ~storage_json params =
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
      ~store
      ~ledger
      ~current_epoch
      ~get_fhe_pubkey
      ~storage_json
      ~addr
      ~method_name:view_call.readonly_method_name
      ~call_params:view_call.readonly_params
      ~caller_addr:view_call.readonly_caller_addr
      ~include_storage:view_call.readonly_include_storage