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


let lang_version = "1.0 Rehovot"

type compile_result = {
  bytecode : string;
  abi_json : string;
  instructions : int;
  error : string option;
  version : string;
  verification_json : string;
  certificate_json : string;
}

let error_result msg =
  { bytecode = ""; abi_json = ""; instructions = 0; error = Some msg; version = lang_version; verification_json = ""; certificate_json = "" }

let ok_result ~bytecode ~abi_json ~instructions ~verification_json ~certificate_json =
  { bytecode; abi_json; instructions; error = None; version = lang_version; verification_json; certificate_json }

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

let certificate_json ~source_mode ~source_material ~bytecode ~verification_json =
  let verification_hash = sha256_hex verification_json in
  `Assoc [
    "schema", `String "aml_bytecode_certificate_v1";
    "compiler", `String "octra_aml";
    "compiler_version", `String lang_version;
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

let abi_json abi =
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
  Printf.sprintf "{\"functions\":[%s],\"events\":[%s]}" fns events

let compile_ast ~source_mode ~source_material ast =
  let code = Oct_gen.generate ast in
  let abi_json = Oct_gen.to_abi ast |> abi_json in
  let bytecode = Bytecode.encode code in
  let verification_json = verification_json ast in
  let certificate_json = certificate_json ~source_mode ~source_material ~bytecode ~verification_json in
  ok_result ~bytecode ~abi_json ~instructions:(Array.length code) ~verification_json ~certificate_json

let load_required load path =
  match load path with
  | Some source -> source
  | None -> failwith (Printf.sprintf "file not found: %s" path)

let load_import load imp =
  match load imp.Oct_lang.imp_path with
  | None -> failwith (Printf.sprintf "import not found: %s" imp.imp_path)
  | Some source -> Oct_parse.parse source

let imported_interfaces load ast =
  ast.Oct_lang.imports
  |> List.concat_map (fun imp ->
    let dep_ast = load_import load imp in
    dep_ast.interfaces
    |> List.filter (fun iface -> List.mem iface.Oct_lang.if_name imp.imp_names))

let merge_interfaces ast interfaces =
  { ast with Oct_lang.interfaces = interfaces @ ast.Oct_lang.interfaces }

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
  | e ->
    error_result (Printf.sprintf "compile error: %s" (Printexc.to_string e))

let compile_multi (resolver : string -> string option) main_path =
  try
    let loaded_sources = ref [] in
    let load path =
      match resolver path with
      | Some source ->
        loaded_sources := (path, source) :: !loaded_sources;
        Some source
      | None -> None
    in
    let main_source = load_required load main_path in
    let ast = Oct_parse.parse main_source in
    let source_material = canonical_sources !loaded_sources in
    ast
    |> imported_interfaces load
    |> merge_interfaces ast
    |> compile_ast ~source_mode:"multi" ~source_material
  with
  | Oct_lex.LexError (msg, line, _col) ->
    error_result (Printf.sprintf "line %d: %s" line msg)
  | Oct_parse.ParseError (msg, line) ->
    error_result (Printf.sprintf "line %d: %s" line msg)
  | Oct_gen.GenError (msg, line) ->
    error_result (Printf.sprintf "line %d: %s" line msg)
  | e ->
    error_result (Printf.sprintf "compile error: %s" (Printexc.to_string e))