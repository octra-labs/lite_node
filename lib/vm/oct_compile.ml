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

let compile source =
  try
    let ast = Oct_parse.parse source in
    let code = Oct_gen.generate ast in
    let abi = Oct_gen.to_abi ast in
    let bytecode = Bytecode.encode code in
    let abi_fields = List.map (fun (name, fs) ->
      let inputs = List.map (fun a ->
        match a with
        | Ocs01.Credits -> "int" | Ocs01.Flag -> "bool"
        | Ocs01.Text -> "string" | Ocs01.Account -> "address"
        | Ocs01.Bytes -> "bytes"
      ) fs.Ocs01.inputs in
      let output = match fs.Ocs01.outputs with
        | [Ocs01.Credits] -> "int" | [Ocs01.Flag] -> "bool"
        | [Ocs01.Text] -> "string" | [Ocs01.Account] -> "address"
        | [Ocs01.Bytes] -> "bytes" | _ -> "void"
      in
      Printf.sprintf "{\"name\":%S,\"inputs\":[%s],\"output\":%S,\"view\":%s,\"payable\":%s}"
        name
        (String.concat "," (List.map (fun s -> Printf.sprintf "%S" s) inputs))
        output
        (if fs.Ocs01.pure then "true" else "false")
        (if fs.Ocs01.payable then "true" else "false")
    ) abi.Ocs01.fns in
    let abi_events = List.map (fun (name, fields) ->
      let fs = List.map (fun (fname, a) ->
        let t = match a with
          | Ocs01.Credits -> "int" | Ocs01.Flag -> "bool"
          | Ocs01.Text -> "string" | Ocs01.Account -> "address"
          | Ocs01.Bytes -> "bytes"
        in Printf.sprintf "{\"name\":%S,\"type\":%S}" fname t
      ) fields in
      Printf.sprintf "{\"name\":%S,\"fields\":[%s]}" name (String.concat "," fs)
    ) abi.Ocs01.events in
    let abi_json = Printf.sprintf "{\"functions\":[%s],\"events\":[%s]}"
      (String.concat "," abi_fields)
      (String.concat "," abi_events) in
    let verification_json = verification_json ast in
    let certificate_json = certificate_json ~source_mode:"single" ~source_material:source ~bytecode ~verification_json in
    { bytecode; abi_json; instructions = Array.length code; error = None; version = lang_version; verification_json; certificate_json }
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
    let main_source = match load main_path with
      | Some s -> s
      | None -> failwith (Printf.sprintf "file not found: %s" main_path)
    in
    let ast = Oct_parse.parse main_source in

    let imported_ifaces = List.concat_map (fun imp ->
      match load imp.Oct_lang.imp_path with
      | None -> failwith (Printf.sprintf "import not found: %s" imp.imp_path)
      | Some src ->
        let dep_ast = Oct_parse.parse src in

        List.filter (fun iface ->
          List.mem iface.Oct_lang.if_name imp.imp_names
        ) dep_ast.interfaces
    ) ast.imports in

    let merged_ast = { ast with
      interfaces = imported_ifaces @ ast.interfaces
    } in
    let code = Oct_gen.generate merged_ast in
    let abi = Oct_gen.to_abi merged_ast in
    let bytecode = Bytecode.encode code in
    let abi_fields = List.map (fun (name, fs) ->
      let inputs = List.map (fun a ->
        match a with
        | Ocs01.Credits -> "int" | Ocs01.Flag -> "bool"
        | Ocs01.Text -> "string" | Ocs01.Account -> "address"
        | Ocs01.Bytes -> "bytes"
      ) fs.Ocs01.inputs in
      let output = match fs.Ocs01.outputs with
        | [Ocs01.Credits] -> "int" | [Ocs01.Flag] -> "bool"
        | [Ocs01.Text] -> "string" | [Ocs01.Account] -> "address"
        | [Ocs01.Bytes] -> "bytes" | _ -> "void"
      in
      Printf.sprintf "{\"name\":%S,\"inputs\":[%s],\"output\":%S,\"view\":%s,\"payable\":%s}"
        name
        (String.concat "," (List.map (fun s -> Printf.sprintf "%S" s) inputs))
        output
        (if fs.Ocs01.pure then "true" else "false")
        (if fs.Ocs01.payable then "true" else "false")
    ) abi.Ocs01.fns in
    let abi_events = List.map (fun (name, fields) ->
      let fs = List.map (fun (fname, a) ->
        let t = match a with
          | Ocs01.Credits -> "int" | Ocs01.Flag -> "bool"
          | Ocs01.Text -> "string" | Ocs01.Account -> "address"
          | Ocs01.Bytes -> "bytes"
        in Printf.sprintf "{\"name\":%S,\"type\":%S}" fname t
      ) fields in
      Printf.sprintf "{\"name\":%S,\"fields\":[%s]}" name (String.concat "," fs)
    ) abi.Ocs01.events in
    let abi_json = Printf.sprintf "{\"functions\":[%s],\"events\":[%s]}"
      (String.concat "," abi_fields)
      (String.concat "," abi_events) in
    let verification_json = verification_json merged_ast in
    let source_material = canonical_sources !loaded_sources in
    let certificate_json = certificate_json ~source_mode:"multi" ~source_material ~bytecode ~verification_json in
    { bytecode; abi_json; instructions = Array.length code; error = None; version = lang_version; verification_json; certificate_json }
  with
  | Oct_lex.LexError (msg, line, _col) ->
    error_result (Printf.sprintf "line %d: %s" line msg)
  | Oct_parse.ParseError (msg, line) ->
    error_result (Printf.sprintf "line %d: %s" line msg)
  | Oct_gen.GenError (msg, line) ->
    error_result (Printf.sprintf "line %d: %s" line msg)
  | e ->
    error_result (Printf.sprintf "compile error: %s" (Printexc.to_string e))