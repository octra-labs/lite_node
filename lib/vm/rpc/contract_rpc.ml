(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


module Rpc = Octra_core.Rpc
module Ledger = Octra_core.Ledger
module Store_irmin = Octra_core.Store_irmin
module Store_chaindata = Octra_core.Store_chaindata

type rpc_result = (Yojson.Safe.t, Rpc.rpc_error) result

let view_effort_limit = 10_000_000

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

let compile_result_response (result : Oct_compile.compile_result) =
  let bytecode_b64 = Base64.encode_exn result.bytecode in
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
  `Assoc ([
    "bytecode", `String bytecode_b64;
    "size", `Int (String.length result.bytecode);
    "instructions", `Int result.instructions;
    "abi", `String result.abi_json;
    "version", `String result.version;
    "disasm", `String disasm;
  ] @ envelope @ parse_optional_json result.verification_json
    @ parse_certificate_json result.certificate_json)

let compile_assembly ~source =
  try
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

let compile_aml ~source =
  let result = Oct_compile.compile source in
  match result.error with
  | Some msg ->
    err_lwt (Rpc.err (-32000) msg None)
  | None ->
    ok_lwt (compile_result_response result)

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
    match program_only with
    | Error msg -> err_lwt (Rpc.invalid_params msg)
    | Ok program_only ->
      let file_map = compile_file_map files_json in
      let resolver path = Hashtbl.find_opt file_map path in
      let result =
        if program_only then
          Oct_compile.compile_program_multi resolver main_path
        else
          Oct_compile.compile_multi resolver main_path
      in
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
    compile_aml ~source

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

let contract_storage_dump_params ~store params =
  match Rpc.require_address params 0 "address" with
  | Error e ->
    err_lwt e
  | Ok addr ->
    contract_storage_dump ~store ~addr

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

type token_contract_meta = {
  address : string;
  owner : string;
  symbol : string;
  name : string;
  decimals : string;
  total_supply : string;
}

let token_contracts_cache : token_contract_meta list ref = ref []
let token_contracts_cache_ts = ref 0.0
let token_balances_cache : (string, float * Yojson.Safe.t) Hashtbl.t = Hashtbl.create 128

let trim_max limit value =
  if String.length value > limit then String.sub value 0 limit else value

let load_token_contracts store =
  let now = Unix.gettimeofday () in
  if now -. !token_contracts_cache_ts < 60.0 then
    Lwt.return !token_contracts_cache
  else
    let open Lwt.Syntax in
    let* addrs = Store_irmin.list_contracts store in
    let* metas =
      Lwt_list.filter_map_p
        (fun addr ->
          let* symbol_opt = Store_irmin.read_contract_storage_key store addr "symbol" in
          match symbol_opt with
          | None ->
            Lwt.return_none
          | Some symbol when String.equal symbol "" || String.equal symbol "0" ->
            Lwt.return_none
          | Some symbol ->
            let* name_opt = Store_irmin.read_contract_storage_key store addr "name" in
            let* decimals_opt = Store_irmin.read_contract_storage_key store addr "decimals" in
            let* total_supply_opt = Store_irmin.read_contract_storage_key store addr "total_supply" in
            let* info = Store_irmin.get_contract_info store addr in
            let owner =
              match info with
              | Some (_, _, _, owner) -> owner
              | None -> ""
            in
            Lwt.return_some {
              address = addr;
              owner;
              symbol = trim_max 32 symbol;
              name = trim_max 64 (Option.value ~default:symbol name_opt);
              decimals = Option.value ~default:"0" decimals_opt;
              total_supply = Option.value ~default:"0" total_supply_opt;
            })
        addrs
    in
    token_contracts_cache := metas;
    token_contracts_cache_ts := now;
    Lwt.return metas

let is_plain_token_balance value =
  let len = String.length value in
  let rec loop index =
    if index >= len then true
    else
      match value.[index] with
      | '0' .. '9' -> loop (index + 1)
      | _ -> false
  in
  len > 0 && loop 0

let token_row ~holder_addr ~store token_meta =
  let open Lwt.Syntax in
  let* balance_opt =
    Store_irmin.read_contract_storage_key
      store
      token_meta.address
      ("balances:" ^ holder_addr)
  in
  match balance_opt with
  | None ->
    Lwt.return_none
  | Some balance when String.equal balance "" || String.equal balance "0" ->
    Lwt.return_none
  | Some balance ->
    let balance_kind, balance_is_encrypted =
      if is_plain_token_balance balance then "plain", false else "encrypted", true
    in
    Lwt.return_some (`Assoc [
      "address", `String token_meta.address;
      "name", `String token_meta.name;
      "symbol", `String token_meta.symbol;
      "total_supply", `String token_meta.total_supply;
      "balance", `String balance;
      "balance_kind", `String balance_kind;
      "balance_is_encrypted", `Bool balance_is_encrypted;
      "decimals", `String token_meta.decimals;
      "owner", `String token_meta.owner;
    ])

let tokens_by_address ~store ~holder_addr =
  let now = Unix.gettimeofday () in
  match Hashtbl.find_opt token_balances_cache holder_addr with
  | Some (ts, payload) when now -. ts < 10.0 ->
    ok_lwt payload
  | _ ->
    let open Lwt.Syntax in
    let* token_contracts = load_token_contracts store in
    let* token_rows =
      Lwt_list.filter_map_p
        (token_row ~holder_addr ~store)
        token_contracts
    in
    let payload = `Assoc [
      "address", `String holder_addr;
      "tokens", `List token_rows;
      "count", `Int (List.length token_rows);
    ] in
    Hashtbl.replace token_balances_cache holder_addr (now, payload);
    ok_lwt payload

let tokens_by_address_params ~store params =
  match Rpc.require_address params 0 "address" with
  | Error e ->
    err_lwt e
  | Ok holder_addr ->
    tokens_by_address ~store ~holder_addr

let verify ~store ~addr ~source ~files_json =
  let open Lwt.Syntax in
  let* stored_b64 =
    Store_irmin.read store ["contracts"; addr; "bytecode"]
  in
  match stored_b64 with
  | None ->
    err_lwt (Rpc.not_found "contract not found or no bytecode")
  | Some b64 ->
    let stored_hash =
      Digestif.SHA256.(digest_string (Base64.decode_exn b64) |> to_hex)
    in
    let result = compile_source source files_json in
    match result.error with
    | Some msg ->
      err_lwt (Rpc.err (-32000) (Printf.sprintf "compile error: %s" msg) None)
    | None ->
      let compiled_hash =
        Digestif.SHA256.(digest_string result.bytecode |> to_hex)
      in
      if not (String.equal stored_hash compiled_hash) then
        err_lwt (Rpc.err (-32000)
          (Printf.sprintf "bytecode mismatch: stored=%s compiled=%s"
             (String.sub stored_hash 0 16) (String.sub compiled_hash 0 16)) None)
      else
        let source_json =
          match files_json with
          | Some files_json -> Yojson.Safe.to_string (source_files source files_json)
          | None -> source
        in
        let* () = Store_irmin.save_contract_source store addr source_json in
        let* () = Store_irmin.save_contract_abi store addr result.abi_json in
        let* () =
          if String.equal result.verification_json "" then Lwt.return_unit
          else Store_irmin.save_contract_verification store addr result.verification_json
        in
        let* () =
          if String.equal result.certificate_json "" then Lwt.return_unit
          else Store_irmin.save_contract_certificate store addr result.certificate_json
        in
        ok_lwt (`Assoc ([
          "verified", `Bool true;
          "code_hash", `String stored_hash;
        ] @ parse_optional_json result.verification_json
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
  match Rpc.require_string params 0 "hash" with
  | Error e ->
    err_lwt e
  | Ok tx_hash ->
    receipt ~chaindata ~tx_hash

let make_view_ctx ~store ~ledger ~current_epoch ~get_fhe_pubkey =
  let get_balance addr =
    match Ledger.find_opt ledger addr with
    | Some account -> account.Ledger.balance
    | None -> Z.zero
  in
  let rec view_ctx = {
    Contract_vm.default_ctx with
    get_balance;
    get_fhe_pubkey;
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
    let result =
      Contract.execute_view_call
        ~ctx:view_ctx
        ~limit:view_effort_limit
        store
        addr
        method_name
        call_params
        caller_addr
    in
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