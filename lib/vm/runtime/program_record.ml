(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type sources = {
  main : string;
  files : Program_package.source list;
  compiler : string;
  loops : bool;
}

let profiles = [
  "source", false; "forms", false; "source", true; "forms", true;
  "protocol", false; "historical", false;
  "package", false; "package_historical", false;
  "package_source", false; "package_preview", false;
  "package", true; "package_source", true; "package_preview", true;
]

let schema = "octra_program_source"
let key address = ["contracts"; address; "record"]
let max_bytes = 2_097_152
let consensus_id =
  "program_source:owner:code_hash:report:state:deploy:historical:declared_compiler:base_200000:bytes_16:limit_2097152:constant_graph:scoped_code:for_limit:"
  ^ Source_stamp.id

let encode_sources value =
  Yojson.Safe.to_string (`Assoc [
    "main", `String value.main;
    "files", `List (List.map (fun (file : Program_package.source) ->
      `Assoc ["path", `String file.path; "source", `String file.body]) value.files);
    "compiler", `String value.compiler;
    "loops", `Bool value.loops;
  ])

let decode_sources encoded =
  let open Result in
  let ( let* ) = bind in
  if String.length encoded > max_bytes then Error "program source payload too large"
  else
    try
      let raw = Base64.decode_exn encoded in
      if Base64.encode_exn raw <> encoded then Error "program source encoding differs"
      else match Octra_core.Json_tree.read raw with
      | `Assoc fields when List.sort String.compare (List.map fst fields)
          = ["compiler"; "files"; "loops"; "main"] ->
        let* main, files, compiler, loops = match List.assoc "main" fields,
          List.assoc "files" fields, List.assoc "compiler" fields, List.assoc "loops" fields with
        | `String main, `List files, `String compiler, `Bool loops
          when List.mem (compiler, loops) profiles -> Ok (main, files, compiler, loops)
        | _ -> Error "program source compiler invalid" in
        let rec decode acc = function
          | [] -> Ok (List.rev acc)
          | (`Assoc [("path", `String path); ("source", `String body)]
            | `Assoc [("source", `String body); ("path", `String path)]) :: rest ->
            decode (Program_package.{path; body} :: acc) rest
          | _ -> Error "program source file invalid"
        in
        let* files = decode [] files in
        let* files = Program_package.normalize ~main files
          |> map_error Program_package.error_message in
        Ok {main; files; compiler; loops}
      | _ -> Error "program source payload invalid"
    with
    | (Out_of_memory | Stack_overflow) as error -> raise error
    | _ -> Error "program source payload invalid"

let make ~source ~code (result : Oct_compile.compile_result) =
  Octra_core.Store_chaindata.{
    source;
    abi = result.abi_json;
    report = if result.verification_json = "" then None else Some result.verification_json;
    certificate = if result.certificate_json = "" then None else Some result.certificate_json;
    code_hash = Digestif.SHA256.(digest_string code |> to_hex);
  }

let source_text ~main files =
  match files with
  | [{Program_package.path = "main.aml"; body}] when main = "main.aml" -> body
  | _ ->
    let files = `Assoc (List.map (fun (file : Program_package.source) ->
      file.path, `String file.body) files) in
    Yojson.Safe.to_string
      (if main = "main.aml" then files else `Assoc ["main", `String main; "files", files])

let source_fee sources =
  Z.(add (of_int 200_000) (cdiv
    (of_int (String.length (source_text ~main:sources.main sources.files))) (of_int 16)))

let check_prior source =
  let open Prior_lang in
  let module Names = Oct_scope.Names in
  let parsed = Prior_parse.parse source in
  let names = Names.of_list (List.map (fun item -> item.c_name) parsed.consts) in
  let rec references found = function
    | [] -> found
    | EVar name :: rest when Names.mem name names -> references (Names.add name found) rest
    | (EIndex (_, values) | ECall (_, values) | EArray values | ETuple values
      | EStoragePath (_, values, _) | EIndexField (_, values, _)) :: rest ->
      references found (List.rev_append values rest)
    | EBinop (_, left, right) :: rest -> references found (left :: right :: rest)
    | (EUnop (_, value) | EBalance value) :: rest -> references found (value :: rest)
    | ETernary (guard, yes, no) :: rest -> references found (guard :: yes :: no :: rest)
    | (EInt _ | EBool _ | EString _ | EVar _ | EField _ | ECaller | EOrigin | ESelfAddr
      | EEpoch | EEpochTime | EValue | ETreeHash | ENodeId | ETxHash | EFieldProp _
      | EEnumVariant _) :: rest -> references found rest
  in
  Oct_scope.check_const_refs (List.map (fun item -> item.c_name,
    references Names.empty [item.c_value]) parsed.consts)

let check_sources sources =
  let check = if List.mem sources.compiler ["historical"; "package_historical"]
    then check_prior else fun source -> Oct_scope.check_consts (Oct_parse.parse source) in
  List.fold_left (fun checked (file : Program_package.source) ->
    Result.bind checked (fun () ->
      try check file.body with
      | (Out_of_memory | Stack_overflow) as error -> raise error
      | _ -> Error "program source compilation failed")) (Ok ()) sources.files

let compile sources =
  let main = sources.main in
  let resolver path = List.find_opt (fun (file : Program_package.source) -> file.path = path)
    sources.files |> Option.map (fun file -> file.Program_package.body) in
  let source = Option.get (resolver main) in
  let single = main = "main.aml" && List.length sources.files = 1 in
  let success result = match result.Oct_compile.error with
    | None -> Ok (result.bytecode, result)
    | Some _ -> Error "program source compilation failed" in
  let ( let* ) = Result.bind in
  let* () = check_sources sources |> Result.map_error (fun _ -> "program source compilation failed") in
  match sources.compiler with
  | "package" | "package_historical" | "package_source" | "package_preview" ->
    let compiler = match sources.compiler with
      | "package_source" -> Program_package.Source
      | "package_preview" -> Program_package.Preview
      | _ -> Program_package.Protocol in
    Program_package.compile_at ~loops:sources.loops ~compiler
      ~point_ops:(sources.compiler <> "package_historical") ~main ~sources:sources.files
    |> Result.map (fun (value : Program_package.compiled) -> value.envelope, value.result)
    |> Result.map_error (fun _ -> "program source compilation failed")
  | "protocol" -> success (if single then Oct_compile.compile source
      else Oct_compile.compile_multi resolver main)
  | "historical" -> success (if single then Prior_compile.compile source
      else Prior_compile.compile_multi resolver main)
  | "source" | "forms" ->
    let syntax = if sources.compiler = "source" then Oct_gen.Source else Oct_gen.Forms in
    let compiled = if single then Aml_source.compile ~loops:sources.loops ~syntax source
      else Aml_source.compile_multi ~loops:sources.loops ~syntax resolver main in
    Result.bind (Result.map_error (fun _ -> "program source compilation failed") compiled)
      (fun compiled -> success (Oct_compile.source_result ~abi:Oct_compile.Source_abi ~syntax
        ~source_mode:(if single then "single" else "multi")
        ~source_material:(if single then source else Oct_compile.ordered_sources
          (List.map (fun (file : Program_package.source) -> file.path, file.body) sources.files)) compiled))
  | _ -> Error "program source compiler invalid"

type scope_check = Unchanged | Images of string list

let source_scope files =
  let open Oct_lang in
  let rec statement = function
    | SLocated (_, _, value) -> statement value
    | SIf _ | SWhile _ | SMatch _ | SFor _ -> true
    | SForEach (_, _, body) -> List.exists statement body
    | SLet _ | SLetTuple _ | SAssign _ | SFieldSet _ | SIndexSet _ | SIndexUpdate _
    | SReturn _ | SAssert _ | SRequire _ | SEmit _ | SFieldCall _
    | SStoragePathSet _ | SStoragePathUpdate _ | SIndexFieldSet _
    | SExpr _ | SRevertError _ -> false in
  List.exists (fun (file : Program_package.source) ->
    try
      let ast = Oct_parse.parse file.body in
      ast.forms <> [] || List.exists (fun fn -> List.exists statement fn.fn_body)
        (Option.to_list ast.ctor @ ast.funcs)
    with
    | (Out_of_memory | Stack_overflow) as error -> raise error
    | _ -> true) files

let scope_check ~main ~files =
  if not (source_scope files) then Unchanged else
  let resolver path = List.find_opt (fun (file : Program_package.source) -> file.path = path)
    files |> Option.map (fun file -> file.Program_package.body) in
  let codes = List.filter_map (fun syntax ->
    match Aml_source.compile_multi ~loops:true ~syntax resolver main with
    | Ok value -> Some value.octb
    | Error _ -> None) [Oct_gen.Source; Oct_gen.Forms] in
  let protocol = Oct_compile.compile_multi_mode ~loops:true ~program_only:false resolver main in
  match protocol.error with
  | None -> Images (protocol.bytecode :: codes)
  | Some _ -> Images codes

let scope_matches check code = match check with
  | Unchanged -> true
  | Images codes -> List.mem code codes

let executable code =
  if Program_envelope.is_program code then
    Result.bind (Program_envelope.decode code
      |> Result.map_error (fun _ -> "program bytecode invalid")) (fun value ->
      Program_attestation.payload ~key_id:"source-check" value.Program_envelope.cert
      |> Result.map (fun certificate -> value.code, Some certificate)
      |> Result.map_error (fun _ -> "program certificate invalid"))
  else Ok (code, None)

let record_scope ~stored record =
  let raw = record.Octra_core.Store_chaindata.source in
  let ( let* ) = Result.bind in
  let* main, entries =
    try match Octra_core.Json_tree.read raw with
    | `Assoc [("main", `String main); ("files", `Assoc files)]
    | `Assoc [("files", `Assoc files); ("main", `String main)] -> Ok (main, files)
    | `Assoc files -> Ok ("main.aml", files)
    | _ -> Error "program source record invalid"
    with
    | (Out_of_memory | Stack_overflow) as error -> raise error
    | _ -> Ok ("main.aml", ["main.aml", `String raw]) in
  let* files = List.fold_right (fun (path, value) rest ->
    let* rest = rest in
    match value with
    | `String body -> Ok (Program_package.{path; body} :: rest)
    | _ -> Error "program source record invalid") entries (Ok []) in
  let* files = Program_package.normalize ~main files
    |> Result.map_error Program_package.error_message in
  let* code, _ = executable stored in
  if scope_matches (scope_check ~main ~files) code then Ok ()
  else Error "program source scope differs"

let verify sources ~exact ~stored =
  let ( let* ) = Result.bind in
  let* () = if List.mem (sources.compiler, sources.loops) profiles then Ok ()
    else Error "program source compiler invalid" in
  let* code, result = compile sources in
  let* () = if scope_matches (scope_check ~main:sources.main ~files:sources.files)
      result.Oct_compile.bytecode then Ok ()
    else Error "program source scope differs" in
  let options = if exact then [code, result] else
    let envelope = (if sources.compiler = "historical" then Prior_compile.emit_program result
      else Oct_compile.emit_program result) in
    (code, result) :: Option.fold ~none:[]
      ~some:(fun code -> [code, envelope]) envelope.program_envelope in
  let* stored_code = executable stored in
  match List.find_opt (fun (code, _) -> code = stored
      || not exact && executable code = Ok stored_code) options with
  | None -> Error "program source bytecode mismatch"
  | Some (_, result) ->
    Ok (make ~source:(source_text ~main:sources.main sources.files) ~code:stored result)

let of_package (package : Program_package.admitted) =
  make ~source:(source_text ~main:package.main package.sources)
    ~code:package.envelope package.result

let raw record = Octra_core.Store_chaindata.program_record_json record

let matches_verification left right =
  left.Octra_core.Store_chaindata.source = right.Octra_core.Store_chaindata.source
  && left.code_hash = right.code_hash
  && left.report = right.report
  && left.certificate = right.certificate

let fee record =
  Z.(add (of_int 200_000) (cdiv (of_int (String.length (raw record))) (of_int 16)))

let check record =
  if String.length (raw record) > max_bytes then Error "program record too large"
  else if not (Octra_core.Store_chaindata.valid_program_hash record.Octra_core.Store_chaindata.code_hash)
  then Error "program record code hash invalid"
  else Ok ()

let read ?snapshot ~store ~address ~code_hash () =
  let open Lwt.Syntax in
  let* value = Octra_core.Store_irmin.read ?snapshot store (key address) in
  match value with
  | None -> Lwt.return_ok None
  | Some raw when String.length raw > max_bytes ->
    Lwt.return_error "program record too large"
  | Some raw -> Lwt.return (Octra_core.Store_chaindata.matching_program_record code_hash raw)

let stage store (address, record) =
  let open Lwt.Syntax in
  match check record with
  | Error reason -> Lwt.fail_with reason
  | Ok () ->
    let* meta = Octra_core.Store_irmin.get_contract_meta store address in
    match meta with
    | Some meta when meta.code_hash = record.Octra_core.Store_chaindata.code_hash ->
      Octra_core.Store_irmin.write store (key address) (raw record)
    | _ -> Lwt.fail_with "program record code changed"