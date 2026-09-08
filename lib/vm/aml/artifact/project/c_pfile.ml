(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type source = {
  path : string;
  deps : string list;
}

type root = {
  path : string;
  name : string;
  feed_path : string option;
}

type t = {
  version : int;
  target : C_proj.target;
  sources : source list;
  roots : root list;
}

type image = {
  project : C_proj.t;
  rules : C_rule.schedule;
}

type error =
  | Header
  | Line of int
  | Character of int * int * int
  | Comment of C_comments.error
  | Roots of int * int
  | Version of string
  | Bodies of int * int
  | Source of string * C_parse.error
  | Binds of string * C_decl.error
  | Feed of string * C_feed.error
  | Rule of C_rule.error
  | Project of C_proj.error

type scan =
  | Need_rule
  | Need_target of int
  | Take_source of int * C_proj.target * source list
  | Take_root of int * C_proj.target * source list * root list

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let words value =
  match
    String.split_on_char ' ' value
    |> List.filter (fun value -> not (String.equal value ""))
  with
  | [] -> None
  | values -> Some values

let version value =
  match Z.of_string value with
  | number when Z.sign number > 0 && String.equal value (Z.to_string number) ->
    begin
      match C_nat.make number with
      | Some number -> Ok (C_nat.to_int number)
      | None -> Error (Version value)
    end
  | _ -> Error (Version value)
  | exception Invalid_argument _ -> Error (Version value)

let root path name feed_path = { path; name; feed_path }

let step line state raw =
  match state, words raw with
  | Need_rule, Some ["rule"; value] ->
    let* value = version value in
    Ok (Need_target value)
  | Need_target version, Some ["target"; "octb"] ->
    Ok (Take_source (version, C_proj.Octb1, []))
  | Need_target version, Some ["target"; "ocps"] ->
    Ok (Take_source (version, C_proj.Ocps1, []))
  | Take_source (version, target, sources), Some ("source" :: path :: deps)
      when C_proj.path_ok path && List.for_all C_proj.path_ok deps ->
    Ok (Take_source (version, target, { path; deps } :: sources))
  | Take_source (version, target, (_ :: _ as sources)),
      Some ["root"; path; name]
      when C_proj.path_ok path && C_proj.name_ok name ->
    Ok (Take_root (version, target, sources, [root path name None]))
  | Take_source (version, target, (_ :: _ as sources)),
      Some ["root"; path; name; feed]
      when C_proj.path_ok path && C_proj.name_ok name
        && C_proj.path_ok feed ->
    Ok (Take_root (version, target, sources, [root path name (Some feed)]))
  | Take_root (version, target, sources, roots), Some ["root"; path; name]
      when C_proj.path_ok path && C_proj.name_ok name ->
    Ok (Take_root
      (version, target, sources, root path name None :: roots))
  | Take_root (version, target, sources, roots),
      Some ["root"; path; name; feed]
      when C_proj.path_ok path && C_proj.name_ok name
        && C_proj.path_ok feed ->
    Ok (Take_root
      (version, target, sources, root path name (Some feed) :: roots))
  | _, Some _ | _, None -> Error (Line line)

let invalid_char input =
  let rec loop off line col =
    if off = String.length input then None
    else
      match input.[off] with
      | ('\r' | '\t') as char -> Some (line, col, Char.code char)
      | '\n' -> loop (off + 1) (line + 1) 1
      | _ -> loop (off + 1) line (col + 1)
  in
  loop 0 1 1

let parse input =
  match C_comments.erase input with
  | Error error -> Error (Comment error)
  | Ok input ->
    begin
      match invalid_char input with
      | Some (line, col, code) -> Error (Character (line, col, code))
      | None ->
        let lines =
          String.split_on_char '\n' input
          |> List.mapi (fun index value -> index + 1, String.trim value)
          |> List.filter (fun (_, value) -> not (String.equal value ""))
        in
        match lines with
        | (_, "folio 1") :: rest ->
          let rec loop state = function
        | [] ->
          begin
            match state with
            | Take_root (version, target, sources, roots) ->
              Ok ({
                version;
                target;
                sources = List.rev sources;
                roots = List.rev roots;
              } : t)
            | Take_source (_, _, _ :: _) -> Error (Roots (1, 0))
            | _ -> Error Header
          end
        | (line, value) :: values ->
          let* state = step line state value in
          loop state values
          in
          loop Need_rule rest
        | _ -> Error Header
    end

let paths (value : t) =
  List.map (fun (source : source) -> source.path) value.sources
  @ List.filter_map (fun (root : root) -> root.feed_path) value.roots

let rec split count out values =
  if count = 0 then List.rev out, values
  else
    match values with
    | [] -> List.rev out, []
    | value :: rest -> split (count - 1) (value :: out) rest

let source path values =
  List.find_opt (fun (value : C_proj.src) -> String.equal value.path path) values

let feed path body input =
  let* parsed =
    match C_parse.parse body with
    | Ok value -> Ok value
    | Error error -> Error (Source (path, error))
  in
  let* binds =
    match C_parse.binds parsed with
    | Ok value -> Ok value
    | Error error -> Error (Binds (path, error))
  in
  let* specs =
    match C_feed.specs binds with
    | Ok value -> Ok value
    | Error error -> Error (Feed (path, error))
  in
  match C_feed.parse specs input with
  | Ok value -> Ok (C_feed.encode value)
  | Error error -> Error (Feed (path, error))

let rec roots sources out specs inputs =
  match specs with
  | [] -> Ok (List.rev out)
  | value :: rest ->
    begin
      match value.feed_path, inputs with
      | None, _ ->
        roots sources
          (C_proj.root ~path:value.path ~name:value.name :: out)
          rest inputs
      | Some _, input :: input_rest ->
        begin
          match source value.path sources with
          | None -> Error (Project (C_proj.Missing_root value.path))
          | Some source ->
            let* feed = feed value.path source.body input in
            roots sources
              (C_proj.root_feed ~path:value.path ~name:value.name ~feed :: out)
              rest input_rest
        end
      | Some _, [] -> Error (Bodies (1, 0))
    end

let make (value : t) bodies =
  let feeds =
    List.length
      (List.filter_map (fun (root : root) -> root.feed_path) value.roots)
  in
  let expected = List.length value.sources + feeds in
  let actual = List.length bodies in
  if expected <> actual then Error (Bodies (expected, actual))
  else
    let* rule =
      match C_rule.rule ~version:value.version ~activate:Z.zero C_rule.local with
      | Ok value -> Ok value
      | Error error -> Error (Rule error)
    in
    let* rules =
      match C_rule.schedule [rule] with
      | Ok value -> Ok value
      | Error error -> Error (Rule error)
    in
    let source_bodies, feed_bodies = split (List.length value.sources) [] bodies in
    let sources =
      List.map2
        (fun (source : source) body ->
          C_proj.source ~path:source.path ~body ~deps:source.deps)
        value.sources source_bodies
    in
    let* roots = roots sources [] value.roots feed_bodies in
    match C_proj.make ~rule:(C_rule.id rule) ~target:value.target
        ~srcs:sources ~roots with
    | Ok project -> Ok { project; rules }
    | Error error -> Error (Project error)

let text = function
  | Header -> "project file header is invalid"
  | Line line -> Printf.sprintf "project file line is invalid line = %d" line
  | Character (line, col, code) ->
    Printf.sprintf
      "project file character is invalid line = %d column = %d byte = %d"
      line col code
  | Comment error -> C_comments.text error
  | Roots (expected, actual) ->
    Printf.sprintf "project root count expected = %d actual = %d"
      expected actual
  | Version value -> "project rule version is invalid value = " ^ value
  | Bodies (expected, actual) ->
    Printf.sprintf "project source count expected = %d actual = %d" expected actual
  | Source (path, error) ->
    "project root source refusal path = " ^ path ^ " reason = " ^ C_parse.text error
  | Binds (path, error) ->
    "project root input refusal path = " ^ path ^ " reason = " ^ C_decl.text error
  | Feed (path, error) ->
    "project root feed refusal path = " ^ path ^ " reason = " ^ C_feed.text error
  | Rule error -> C_rule.text error
  | Project error -> C_proj.text error