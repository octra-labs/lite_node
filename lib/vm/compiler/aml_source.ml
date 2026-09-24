(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  name : string;
  declaration : Oct_lang.declaration;
  ast : Oct_lang.contract;
  code : Contract_vm.instr array;
  octb : string;
}

let owns source =
  let stream = Oct_lex.make_stream source in
  let modifier = function
    | Oct_lang.TkFn
    | Oct_lang.TkView
    | Oct_lang.TkPure
    | Oct_lang.TkPublic
    | Oct_lang.TkPrivate
    | Oct_lang.TkInternal
    | Oct_lang.TkPayable -> true
    | _ -> false
  in
  let rec scan braces parens brackets before prior =
    let token = Oct_lex.peek_token stream in
    Oct_lex.eat stream;
    let next = Oct_lex.peek_token stream in
    let outer = braces = 1 && parens = 0 && brackets = 0 in
    let owned =
      match token with
      | Oct_lang.TkContract
      | Oct_lang.TkInterface
      | Oct_lang.TkImport -> braces = 0
      | Oct_lang.TkImplements -> braces = 0
      | Oct_lang.TkLBrace ->
        outer && (prior = Oct_lang.TkState
          || (match before, prior with
            | (Oct_lang.TkStruct | Oct_lang.TkEnum), Oct_lang.TkIdent _ -> true
            | _ -> false))
      | Oct_lang.TkLParen ->
        outer && (prior = Oct_lang.TkConstructor
          || (match before with
            | Oct_lang.TkEvent | Oct_lang.TkFn | Oct_lang.TkError ->
              Oct_parse.ident prior
            | _ -> false))
      | Oct_lang.TkColon ->
        outer && (match before, prior with
          | Oct_lang.TkConst, Oct_lang.TkIdent _ -> true
          | _ -> false)
      | Oct_lang.TkEq ->
        outer && (match before, prior with
          | Oct_lang.TkConst, Oct_lang.TkIdent _
          | Oct_lang.TkIdent "invariant", Oct_lang.TkIdent _ -> true
          | _ -> false)
      | Oct_lang.TkPublic ->
        outer && (modifier next || next = Oct_lang.TkIdent "main")
      | Oct_lang.TkView
      | Oct_lang.TkPure
      | Oct_lang.TkPrivate
      | Oct_lang.TkInternal
      | Oct_lang.TkPayable -> outer && modifier next
      | _ -> false
    in
    if owned then true
    else
      match token with
      | Oct_lang.TkEOF -> false
      | Oct_lang.TkLBrace ->
        scan (braces + 1) parens brackets prior token
      | Oct_lang.TkRBrace ->
        scan (max 0 (braces - 1)) parens brackets prior token
      | Oct_lang.TkLParen ->
        scan braces (parens + 1) brackets prior token
      | Oct_lang.TkRParen ->
        scan braces (max 0 (parens - 1)) brackets prior token
      | Oct_lang.TkLBrack ->
        scan braces parens (brackets + 1) prior token
      | Oct_lang.TkRBrack ->
        scan braces parens (max 0 (brackets - 1)) prior token
      | _ -> scan braces parens brackets prior token
  in
  try scan 0 0 0 Oct_lang.TkEOF Oct_lang.TkEOF
  with Oct_lex.LexError _ -> false

module Names = Set.Make (String)

let repeated names =
  let rec walk seen = function
    | [] -> None
    | name :: rest ->
      if Names.mem name seen then Some name
      else walk (Names.add name seen) rest
  in
  walk Names.empty names

let verifier_text = function
  | Contract_vm.Verifier.InvalidReg (pc, reg) ->
    Printf.sprintf "register r%d is invalid at pc %d" reg pc
  | Contract_vm.Verifier.InvalidRegSpan (pc, base, count) ->
    Printf.sprintf "register span r%d+%d is invalid at pc %d" base count pc
  | Contract_vm.Verifier.InvalidJumpDest dest ->
    Printf.sprintf "jump destination %d is absent" dest
  | Contract_vm.Verifier.DuplicateJDest dest ->
    Printf.sprintf "jump destination %d is repeated" dest
  | Contract_vm.Verifier.CodeTooLarge count ->
    Printf.sprintf "instruction count %d exceeds capacity" count
  | Contract_vm.Verifier.EmptyCode -> "instruction stream is empty"
  | Contract_vm.Verifier.ReservedKey (pc, _) ->
    Printf.sprintf "storage key is reserved at pc %d" pc
  | Contract_vm.Verifier.CapabilityLiteral pc ->
    Printf.sprintf "capability literal is forbidden at pc %d" pc
  | Contract_vm.Verifier.CapabilityKind (pc, kind) ->
    Printf.sprintf "capability kind is invalid at pc %d kind = %s"
      pc (Z.to_string kind)

let body_empty ast =
  match ast.Oct_lang.declaration with
  | Oct_lang.InterfaceDecl -> ast.interfaces = []
  | Oct_lang.ProgramDecl | Oct_lang.ContractDecl ->
    ast.structs = []
    && ast.enums = []
    && ast.consts = []
    && ast.invariants_decl = []
    && ast.state = []
    && ast.events = []
    && ast.errors = []
    && Option.is_none ast.ctor
    && ast.funcs = []
    && ast.forms = []

let shape syntax ast =
  let total items size = List.fold_left (fun n item -> n + size item) 0 items in
  if syntax <> Oct_gen.Source then Ok ()
  else if List.length ast.Oct_lang.imports > Program_limits.max_imports then
    Error "Program import count exceeds compiler limit"
  else if total ast.imports (fun item -> List.length item.Oct_lang.imp_names)
      > Program_limits.max_import_names then
    Error "Program import name count exceeds compiler limit"
  else if List.length ast.interfaces > Program_limits.max_interfaces then
    Error "Program interface count exceeds compiler limit"
  else if total ast.interfaces (fun item -> List.length item.Oct_lang.if_methods)
      > Program_limits.max_interface_methods then
    Error "Program interface method count exceeds compiler limit"
  else match repeated (List.map (fun item -> item.Oct_lang.if_name) ast.interfaces) with
    | Some name -> Error ("duplicate interface name = " ^ name)
    | None -> Ok ()

let compile_ast ~syntax ast =
  let ( let* ) = Result.bind in
  let* () = shape syntax ast in
  let program = ast.Oct_lang.declaration = Oct_lang.ProgramDecl in
  let functions = List.length ast.funcs + List.length ast.forms in
  let interfaces =
    List.map (fun item -> item.Oct_lang.if_name) ast.interfaces
  in
  match repeated interfaces with
  | Some name -> Error ("duplicate interface name = " ^ name)
  | None ->
    if body_empty ast then Error "source declaration body is empty"
    else if program && functions > Program_limits.max_functions then
      Error "program function count exceeds capacity"
    else
      match Oct_form.link ast with
      | Error reason -> Error reason
      | Ok (ast, direct, calls) ->
        let code, seals = Oct_gen.generate ~syntax ~direct ~calls ast in
        if program && Array.length code > Program_limits.max_instructions then
          Error "program instruction count exceeds capacity"
        else
          match Contract_vm.Verifier.verify code with
          | Error error -> Error (verifier_text error)
          | Ok () ->
            let state =
              match ast.Oct_lang.declaration with
              | Oct_lang.ProgramDecl ->
                begin
                  match Aml_input.storage_kinds ast with
                  | [] -> None
                  | rows -> Some rows
                end
              | Oct_lang.ContractDecl | Oct_lang.InterfaceDecl -> None
            in
            let proof =
              match seals with
              | [] -> Ok None
              | _ ->
                begin
                  let image = Bytecode.encode ?state code in
                  match Aml_call.encode ~image seals with
                  | Ok raw -> Ok (Some raw)
                  | Error error -> Error (Aml_call.text error)
                end
            in
            begin
              match proof with
              | Error reason -> Error reason
              | Ok proof ->
                let octb = Bytecode.encode ?state ?proof code in
                let checked =
                  match proof with
                  | None -> Ok ()
                  | Some raw ->
                    let image = Bytecode.encode ?state code in
                    Aml_call.verify ~image code raw
                in
                begin
                  match checked with
                  | Error error -> Error (Aml_call.text error)
                  | Ok () ->
                    Ok {
                      name = ast.Oct_lang.name;
                      declaration = ast.declaration;
                      ast;
                      code;
                      octb;
                    }
                end
            end

let diagnostic source line column message =
  let file =
    match source with
    | Some path -> "source = " ^ path ^ " "
    | None -> ""
  in
  if line < 1 then file ^ message
  else
    match column with
    | Some value ->
      Printf.sprintf "%sline %d column %d: %s" file line value message
    | None -> Printf.sprintf "%sline %d: %s" file line message

let caught ?source action =
  try action () with
  | Oct_lex.LexError (message, line, column) ->
    Error (diagnostic source line (Some column) message)
  | Oct_parse.ParseError (message, line, column) ->
    Error (diagnostic source line (Some column) message)
  | Oct_gen.GenError (message, line, column) ->
    let column = if column < 1 then None else Some column in
    Error (diagnostic source line column message)
  | (Stack_overflow | Out_of_memory) as error -> raise error
  | Failure message | Invalid_argument message -> Error message

let compile ~syntax source =
  caught (fun () ->
    let ast = Oct_parse.parse source in
    compile_ast ~syntax ast)

let compile_multi ~syntax resolver main_path =
  let cache = Hashtbl.create 16 in
  let load path =
    match Hashtbl.find_opt cache path with
    | Some source -> Ok source
    | None ->
      begin
        match resolver path with
        | Some source ->
          Hashtbl.add cache path source;
          Ok source
        | None -> Error ("source is absent path = " ^ path)
      end
  in
  let parse path =
    match load path with
    | Error reason -> Error reason
    | Ok source ->
      caught ~source:path (fun () ->
        let ast = Oct_parse.parse source in
        Result.map (fun () -> ast) (shape syntax ast))
  in
  let select path parsed names =
    let rec loop out = function
      | [] -> Ok (List.rev out)
      | name :: rest ->
        begin
          match
            List.find_opt
              (fun iface -> String.equal iface.Oct_lang.if_name name)
              parsed.Oct_lang.interfaces
          with
          | Some iface -> loop (iface :: out) rest
          | None ->
            Error
              (Printf.sprintf
                "source = %s imported interface is absent name = %s"
                path name)
        end
    in
    loop [] names
  in
  let rec imports out = function
    | [] -> Ok (List.rev out |> List.concat)
    | import :: rest ->
      let path = import.Oct_lang.imp_path in
      begin
        match parse path with
        | Error reason -> Error reason
        | Ok parsed ->
          begin
            match select path parsed import.imp_names with
            | Error reason -> Error reason
            | Ok selected -> imports (selected :: out) rest
          end
      end
  in
  match parse main_path with
  | Error reason -> Error reason
  | Ok main ->
    begin
      match imports [] main.Oct_lang.imports with
      | Error reason -> Error reason
      | Ok interfaces ->
        caught ~source:main_path (fun () ->
          compile_ast ~syntax
            { main with Oct_lang.interfaces = interfaces @ main.interfaces })
    end