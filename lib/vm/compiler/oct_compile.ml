(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let lang_version = "1.0 Rehovot"

type compile_result = {
  bytecode : string;
  abi_json : string;
  instructions : int;
  error : string option;
  version : string;
  verification_json : string;
  certificate_json : string;
  program_envelope : string option;
  program_facts : Program_type_flow.facts option;
}

type xcall_abi = {
  method_name : string;
  inputs : Program_type_flow.kind list;
  output : Program_type_flow.kind;
  capabilities : Program_type_flow.capability list;
}

exception Compile_limit of string

let error_result msg =
  { bytecode = ""; abi_json = ""; instructions = 0; error = Some msg; version = lang_version; verification_json = ""; certificate_json = ""; program_envelope = None; program_facts = None }

let ok_result ~bytecode ~abi_json ~instructions ~verification_json ~certificate_json =
  { bytecode; abi_json; instructions; error = None; version = lang_version; verification_json; certificate_json; program_envelope = None; program_facts = None }

let verification_json ast =
  Aml_verify.verify_ast ast
  |> Aml_verify.json_of_report
  |> Yojson.Safe.to_string

let sha256_hex value =
  Digestif.SHA256.(digest_string value |> to_hex)

let canonical_sources sources =
  sources
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)
  |> List.map (fun (path, source) -> `Assoc ["path", `String path; "source_hash", `String (sha256_hex source)])
  |> fun values -> Yojson.Safe.to_string (`List values)

let total_length items length =
  List.fold_left (fun total item -> total + length item) 0 items

let require_ast_shape ast =
  let imports = ast.Oct_lang.imports in
  let interfaces = ast.Oct_lang.interfaces in
  if List.length imports > Program_limits.max_imports then
    raise (Compile_limit "Program import count exceeds compiler limit")
  else if
    total_length imports (fun imp -> List.length imp.Oct_lang.imp_names)
    > Program_limits.max_import_names
  then
    raise (Compile_limit "Program import name count exceeds compiler limit")
  else if List.length interfaces > Program_limits.max_interfaces then
    raise (Compile_limit "Program interface count exceeds compiler limit")
  else if
    total_length interfaces (fun iface -> List.length iface.Oct_lang.if_methods)
    > Program_limits.max_interface_methods
  then
    raise (Compile_limit "Program interface method count exceeds compiler limit")

let interface_index ast =
  let index = Hashtbl.create (List.length ast.Oct_lang.interfaces) in
  List.iter
    (fun iface ->
       let name = iface.Oct_lang.if_name in
       if Hashtbl.mem index name then
         raise (Compile_limit ("Program interface is duplicated: " ^ name))
       else
         Hashtbl.add index name iface)
    ast.Oct_lang.interfaces;
  index

let flow_kind = function
  | Oct_lang.TInt -> Program_type_flow.Int
  | Oct_lang.TBool -> Program_type_flow.Bool
  | Oct_lang.TString -> Program_type_flow.String
  | Oct_lang.TAddress -> Program_type_flow.Addr
  | Oct_lang.TBytes -> Program_type_flow.Bytes
  | Oct_lang.TBytes32 -> Program_type_flow.Bytes32
  | Oct_lang.TU64 -> Program_type_flow.U64
  | Oct_lang.TU128 -> Program_type_flow.U128
  | Oct_lang.TU256 -> Program_type_flow.U256
  | Oct_lang.TCipher -> Program_type_flow.Cipher
  | Oct_lang.TPubKey -> Program_type_flow.PubKey
  | Oct_lang.TMap _
  | Oct_lang.TList _
  | Oct_lang.TStruct _
  | Oct_lang.TEnum _
  | Oct_lang.TOption _
  | Oct_lang.TTuple _
  | Oct_lang.TVoid -> Program_type_flow.Unknown

let param_facts params =
  params
  |> List.mapi (fun i param -> (1001 + i, flow_kind param.Oct_lang.p_typ))

let facts_of_ast ast code =
  let labels = Hashtbl.create (List.length ast.Oct_lang.funcs + 1) in
  Array.iteri
    (fun pc op ->
      match op with
      | Contract_vm.JDEST label -> Hashtbl.replace labels label pc
      | _ -> ())
    code;
  let root =
    match ast.Oct_lang.ctor with
    | Some ctor -> param_facts ctor.Oct_lang.fn_params
    | None -> []
  in
  let storage =
    ast.Oct_lang.state
    |> List.filter_map (fun field ->
      let kind = flow_kind field.Oct_lang.sf_typ in
      match kind with
      | Program_type_flow.Int
      | Program_type_flow.Bool
      | Program_type_flow.String
      | Program_type_flow.Bytes
      | Program_type_flow.Bytes32
      | Program_type_flow.U64
      | Program_type_flow.U128
      | Program_type_flow.U256
      | Program_type_flow.Addr -> Some (field.sf_name, kind)
      | Program_type_flow.Cipher
      | Program_type_flow.PubKey
      | Program_type_flow.Unknown -> None)
  in
  let entries =
    ast.Oct_lang.funcs
    |> List.mapi (fun i fn ->
      match Hashtbl.find_opt labels (200 + i * 100) with
      | Some target ->
        Some { Program_type_flow.target; mem = param_facts fn.Oct_lang.fn_params; effects = [] }
      | None -> None)
    |> List.filter_map (fun value -> value)
  in
  let functions = Hashtbl.create (List.length ast.Oct_lang.funcs) in
  List.iteri
    (fun i fn -> Hashtbl.replace functions (200 + i * 100) fn)
    ast.Oct_lang.funcs;
  let entry_targets = Hashtbl.create (List.length entries) in
  List.iter
    (fun (entry : Program_type_flow.entry) ->
      Hashtbl.replace entry_targets entry.target ())
    entries;
  let owners = Array.make (Array.length code) None in
  let owner = ref None in
  Array.iteri
    (fun pc _ ->
      if Hashtbl.mem entry_targets pc then owner := Some pc;
      owners.(pc) <- !owner)
    code;
  let calls =
    let found = ref [] in
    Array.iteri
      (fun pc op ->
      match op with
      | Contract_vm.CALL_INT (_, label) ->
        (match
           Hashtbl.find_opt labels label,
           owners.(pc),
           Hashtbl.find_opt functions label
         with
         | Some target, Some owner, Some fn ->
           found := {
             Program_type_flow.owner;
             pc;
             target;
             kind = flow_kind fn.Oct_lang.fn_ret;
           } :: !found
         | _ -> ())
      | _ -> ())
      code;
    List.rev !found
  in
  { Program_type_flow.root; storage; entries; calls; xcalls = [] }

let fact_json key (slot, kind) =
  `Assoc [key, `Int slot; "kind", `String (Program_type_flow.kind_name kind)]

let facts_json facts =
  let root = List.map (fact_json "slot") facts.Program_type_flow.root in
  let storage =
    facts.Program_type_flow.storage
    |> List.map (fun (key, kind) ->
      `Assoc [
        "key", `String key;
        "kind", `String (Program_type_flow.kind_name kind);
      ])
  in
  let entries =
    List.map (fun (entry : Program_type_flow.entry) ->
      `Assoc [
        "target", `Int entry.Program_type_flow.target;
        "memory", `List (List.map (fact_json "slot") entry.Program_type_flow.mem);
        "effects", `List (List.map (fun value -> `String value) entry.Program_type_flow.effects)
      ]) facts.Program_type_flow.entries
  in
  let calls =
    List.map (fun (call : Program_type_flow.call) ->
      `Assoc [
        "owner", `Int call.Program_type_flow.owner;
        "pc", `Int call.Program_type_flow.pc;
        "target", `Int call.Program_type_flow.target;
        "kind", `String (Program_type_flow.kind_name call.Program_type_flow.kind)
      ]) facts.Program_type_flow.calls
  in
  let xcalls =
    facts.Program_type_flow.xcalls
    |> List.sort (fun (left : Program_type_flow.xcall) right -> compare left.pc right.pc)
    |> List.map (fun (call : Program_type_flow.xcall) ->
      let kind_json kind = `String (Program_type_flow.kind_name kind) in
      `Assoc [
        "pc", `Int call.pc;
        "method", `String call.method_name;
        "inputs", `List (List.map kind_json call.inputs);
        "output", kind_json call.output;
        "capabilities", `List (List.map (fun value -> `String (Program_type_flow.capability_name value)) call.capabilities)
      ])
  in
  let fields = [
    "root", `List root;
    "entries", `List entries;
    "calls", `List calls;
    "xcalls", `List xcalls;
  ] in
  let fields =
    if storage = [] then fields
    else ("storage", `List storage) :: fields
  in
  `Assoc fields

let program_certificate raw effects facts =
  try
    match Yojson.Safe.from_string raw with
    | `Assoc fields ->
      let fields =
        List.filter
          (fun (key, _) -> key <> "schema" && key <> "effects" && key <> "facts")
          fields
      in
      let effect_json = `List (List.map (fun name -> `String name) effects) in
      Ok (Yojson.Safe.to_string (`Assoc (
        ("schema", `String "aml_bytecode_certificate_v2")
        :: ("effects", effect_json)
        :: ("facts_hash", `String (Program_type_flow.facts_hash facts))
        :: ("facts", facts_json facts)
        :: fields)))
    | _ -> Error "program certificate must be an object"
  with _ -> Error "invalid program certificate"

let certificate_json ~declaration ~source_mode ~source_material ~bytecode ~verification_json =
  let verification_hash = sha256_hex verification_json in
  `Assoc [
    "schema", `String "aml_bytecode_certificate_v1";
    "compiler", `String "octra_aml";
    "compiler_version", `String lang_version;
    "declaration", `String declaration;
    "verification_schema", `String Aml_verify.schema;
    "source_mode", `String source_mode;
    "source_hash", `String (sha256_hex source_material);
    "bytecode_hash", `String (sha256_hex bytecode);
    "verification_hash", `String verification_hash;
  ]
  |> Yojson.Safe.to_string

let abi_type_name = function
  | Ocs01.Credits -> "int"
  | Ocs01.Flag -> "bool"
  | Ocs01.Text -> "string"
  | Ocs01.Account -> "address"
  | Ocs01.Bytes -> "bytes"

let quote_json value =
  Printf.sprintf "%S" value

let bool_json value =
  if value then "true" else "false"

let abi_output_name = function
  | [kind] -> abi_type_name kind
  | _ -> "void"

let abi_function_json (name, fs) =
  let inputs =
    fs.Ocs01.inputs
    |> List.map abi_type_name
    |> List.map quote_json
    |> String.concat ","
  in
  Printf.sprintf "{\"name\":%S,\"inputs\":[%s],\"output\":%S,\"view\":%s,\"payable\":%s}"
    name
    inputs
    (abi_output_name fs.Ocs01.outputs)
    (bool_json fs.Ocs01.pure)
    (bool_json fs.Ocs01.payable)

let abi_event_field_json (name, kind) =
  Printf.sprintf "{\"name\":%S,\"type\":%S}" name (abi_type_name kind)

let abi_event_json (name, fields) =
  let fields_json =
    fields
    |> List.map abi_event_field_json
    |> String.concat ","
  in
  Printf.sprintf "{\"name\":%S,\"fields\":[%s]}" name fields_json

let abi_json declaration abi =
  let fns =
    abi.Ocs01.fns
    |> List.map abi_function_json
    |> String.concat ","
  in
  let events =
    abi.Ocs01.events
    |> List.map abi_event_json
    |> String.concat ","
  in
  Printf.sprintf "{\"declaration\":%S,\"functions\":[%s],\"events\":[%s]}" declaration fns events

let compile_ast ~source_mode ~source_material ast =
  require_ast_shape ast;
  ignore (interface_index ast);
  let declaration = Oct_lang.declaration_to_string ast.Oct_lang.declaration in
  let program = ast.Oct_lang.declaration = Oct_lang.ProgramDecl in
  if program
     && List.length ast.Oct_lang.funcs > Program_limits.max_functions then
    error_result "Program function limit exceeded"
  else
    let code = Oct_gen.generate ast in
    if program && Array.length code > Program_limits.max_instructions then
      error_result "Program instruction limit exceeded"
    else
      let abi_json = Oct_gen.to_abi ast |> abi_json declaration in
      let bytecode = Bytecode.encode code in
      let verification_json = verification_json ast in
      let certificate_json =
        certificate_json
          ~declaration
          ~source_mode
          ~source_material
          ~bytecode
          ~verification_json
      in
      let program_facts = facts_of_ast ast code in
      {
        (ok_result
           ~bytecode
           ~abi_json
           ~instructions:(Array.length code)
           ~verification_json
           ~certificate_json)
        with
        program_facts = Some program_facts;
      }

let load_required load path =
  match load path with
  | Some source -> source
  | None -> failwith (Printf.sprintf "file not found: %s" path)

let imported_interfaces load_import ast =
  let bindings = Hashtbl.create (List.length ast.Oct_lang.imports) in
  let add_import acc (imp : Oct_lang.import_decl) name =
    let interfaces = load_import imp in
    match Hashtbl.find_opt interfaces name with
    | None ->
      raise
        (Compile_limit
           (Printf.sprintf
              "Program interface %s is missing from %s"
              name
              imp.Oct_lang.imp_path))
    | Some iface ->
      begin
        match Hashtbl.find_opt bindings name with
        | None ->
          Hashtbl.add bindings name imp.Oct_lang.imp_path;
          iface :: acc
        | Some path when String.equal path imp.Oct_lang.imp_path -> acc
        | Some _ ->
          raise
            (Compile_limit
               ("Program interface import is ambiguous: " ^ name))
      end
  in
  List.fold_left
    (fun acc (imp : Oct_lang.import_decl) ->
       List.fold_left
         (fun acc name -> add_import acc imp name)
         acc
         imp.Oct_lang.imp_names)
    []
    ast.Oct_lang.imports
  |> List.rev

let merge_interfaces ast interfaces =
  { ast with Oct_lang.interfaces = interfaces @ ast.Oct_lang.interfaces }

let compile_exception = function
  | Compile_limit message -> error_result message
  | Stack_overflow -> error_result "Program compiler complexity limit exceeded"
  | Out_of_memory -> raise Out_of_memory
  | error ->
    error_result (Printf.sprintf "compile error: %s" (Printexc.to_string error))

let compile source =
  try
    let ast = Oct_parse.parse source in
    compile_ast ~source_mode:"single" ~source_material:source ast
  with
  | Oct_lex.LexError (msg, line, _col) ->
    error_result (Printf.sprintf "line %d: %s" line msg)
  | Oct_parse.ParseError (msg, line) ->
    error_result (Printf.sprintf "line %d: %s" line msg)
  | Oct_gen.GenError (msg, line) ->
    error_result (Printf.sprintf "line %d: %s" line msg)
  | error -> compile_exception error

let compile_multi_mode ~program_only (resolver : string -> string option) main_path =
  try
    let loaded_sources = ref [] in
    let source_cache = Hashtbl.create 16 in
    let interface_cache = Hashtbl.create 16 in
    let load path =
      match Hashtbl.find_opt source_cache path with
      | Some source -> Some source
      | None ->
        match resolver path with
        | Some source ->
          Hashtbl.add source_cache path source;
          loaded_sources := (path, source) :: !loaded_sources;
          Some source
        | None -> None
    in
    let load_import imp =
      let path = imp.Oct_lang.imp_path in
      match Hashtbl.find_opt interface_cache path with
      | Some interfaces -> interfaces
      | None ->
        let source =
          match load path with
          | Some source -> source
          | None -> failwith (Printf.sprintf "import not found: %s" path)
        in
        let ast = Oct_parse.parse source in
        require_ast_shape ast;
        let interfaces = interface_index ast in
        Hashtbl.add interface_cache path interfaces;
        interfaces
    in
    let main_source = load_required load main_path in
    let ast = Oct_parse.parse main_source in
    require_ast_shape ast;
    ignore (interface_index ast);
    if program_only && ast.Oct_lang.declaration <> Oct_lang.ProgramDecl then
      error_result "Program declaration required"
    else
      let interfaces = imported_interfaces load_import ast in
      let source_material = canonical_sources !loaded_sources in
      let merged_ast = merge_interfaces ast interfaces in
      compile_ast ~source_mode:"multi" ~source_material merged_ast
  with
  | Oct_lex.LexError (msg, line, _col) ->
    error_result (Printf.sprintf "line %d: %s" line msg)
  | Oct_parse.ParseError (msg, line) ->
    error_result (Printf.sprintf "line %d: %s" line msg)
  | Oct_gen.GenError (msg, line) ->
    error_result (Printf.sprintf "line %d: %s" line msg)
  | error -> compile_exception error

let compile_multi resolver main_path =
  compile_multi_mode ~program_only:false resolver main_path

let external_pcs code =
  Array.to_list code
  |> List.mapi (fun pc op ->
    match op with
    | Contract_vm.XCALL _ -> Some pc
    | _ -> None)
  |> List.filter_map (fun value -> value)

let attach_xcalls code facts specs =
  let pcs = external_pcs code in
  if List.length specs > Program_limits.max_external_calls then
    Error "external ABI limit exceeded"
  else if List.length pcs <> List.length specs then
    Error "external ABI count does not match XCALL count"
  else
    let xcalls =
      List.map2 (fun pc spec ->
        {
          Program_type_flow.pc;
          method_name = spec.method_name;
          inputs = spec.inputs;
          output = spec.output;
          capabilities = spec.capabilities;
        })
        pcs specs
    in
    Ok { facts with Program_type_flow.xcalls }

let emit_program_with_facts result facts =
  match result.error with
  | Some _ -> result
  | None ->
    (match Bytecode.decode result.bytecode with
     | Error error -> error_result ("Program bytecode: " ^ error)
     | Ok code ->
       (match Program_policy.complete code facts with
        | Error error -> error_result ("Program effect policy: " ^ error)
        | Ok facts ->
          (match Program_policy.effects code facts with
           | Error error -> error_result ("Program effect policy: " ^ error)
           | Ok effects ->
             match Admission.of_program ~facts code with
             | Error error -> error_result ("Program admission: " ^ Admission.error_message error)
             | Ok _ ->
               (match program_certificate result.certificate_json effects facts with
                | Error error -> error_result error
                | Ok cert ->
                  match Program_envelope.encode ~code:result.bytecode ~cert with
                  | Error error -> error_result ("Program envelope: " ^ Program_envelope.error_message error)
                  | Ok envelope -> { result with certificate_json = cert; program_envelope = Some envelope }))))

let emit_program result =
  match result.program_facts with
  | None -> error_result "Program facts missing"
  | Some facts -> emit_program_with_facts result facts

let attest_program ~key_id ~private_key result =
  match result.error, result.program_envelope with
  | Some _, _ -> result
  | None, None -> error_result "Program envelope missing"
  | None, Some _ ->
    (match Program_attestation.attach ~key_id ~private_key result.certificate_json with
     | Error error -> error_result (Program_attestation.error_message error)
     | Ok certificate_json ->
       (match Program_envelope.encode ~code:result.bytecode ~cert:certificate_json with
        | Error error -> error_result ("Program envelope: " ^ Program_envelope.error_message error)
        | Ok program_envelope -> { result with certificate_json; program_envelope = Some program_envelope }))

let compile_program source =
  try
    let ast = Oct_parse.parse source in
    if ast.Oct_lang.declaration <> Oct_lang.ProgramDecl then
      error_result "Program declaration required"
    else
      emit_program
        (compile_ast ~source_mode:"single" ~source_material:source ast)
  with
  | Oct_lex.LexError (msg, line, _col) ->
    error_result (Printf.sprintf "line %d: %s" line msg)
  | Oct_parse.ParseError (msg, line) ->
    error_result (Printf.sprintf "line %d: %s" line msg)
  | Oct_gen.GenError (msg, line) ->
    error_result (Printf.sprintf "line %d: %s" line msg)
  | error -> compile_exception error

let compile_program_with_xcalls source specs =
  try
    let ast = Oct_parse.parse source in
    if ast.Oct_lang.declaration <> Oct_lang.ProgramDecl then
      error_result "Program declaration required"
    else
      let result =
        compile_ast ~source_mode:"single" ~source_material:source ast
      in
      match result.error, result.program_facts with
      | Some _, _ -> result
      | None, None -> error_result "Program facts missing"
      | None, Some facts ->
        (match Bytecode.decode result.bytecode with
         | Error error -> error_result ("Program bytecode: " ^ error)
         | Ok code ->
           (match attach_xcalls code facts specs with
            | Error error -> error_result error
            | Ok facts -> emit_program_with_facts result facts))
  with
  | Oct_lex.LexError (msg, line, _col) ->
    error_result (Printf.sprintf "line %d: %s" line msg)
  | Oct_parse.ParseError (msg, line) ->
    error_result (Printf.sprintf "line %d: %s" line msg)
  | Oct_gen.GenError (msg, line) ->
    error_result (Printf.sprintf "line %d: %s" line msg)
  | error -> compile_exception error

let admit_program_source source raw =
  let result = compile_program source in
  match result.error, result.program_envelope with
  | Some error, _ -> Error error
  | None, None -> Error "Program source admission envelope missing"
  | None, Some expected when not (String.equal expected raw) ->
    Error "Program source does not match envelope"
  | None, Some _ ->
    (match Admission.decode_program_source raw with
     | Ok admitted -> Ok admitted
     | Error error -> Error (Admission.error_message error))

let compile_program_multi resolver main_path =
  emit_program (compile_multi_mode ~program_only:true resolver main_path)