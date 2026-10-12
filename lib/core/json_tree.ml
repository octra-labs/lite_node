(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type frame =
  | Array of Yojson.Safe.t list
  | Object of string * (string * Yojson.Safe.t) list

let parse ~extensions raw =
  let lex = Lexing.from_string raw in
  let state = Yojson.Safe.init_lexer () in
  let space () = Yojson.Safe.read_space state lex in
  let peek () =
    if lex.lex_curr_pos < lex.lex_buffer_len then
      Some (Bytes.get lex.lex_buffer lex.lex_curr_pos)
    else None
  in
  let key () =
    let name = Yojson.Safe.read_ident state lex in
    space ();
    Yojson.Safe.read_colon state lex;
    name
  in
  let rec value frames =
    space ();
    match peek () with
    | Some ('(' | '<') when not extensions ->
      Yojson.json_error "JSON container is invalid"
    | Some '[' ->
      Yojson.Safe.read_lbr state lex;
      space ();
      let empty = try Yojson.Safe.read_array_end lex; false
        with Yojson.End_of_array -> true in
      if empty then finish frames (`List []) else value (Array [] :: frames)
    | Some '{' ->
      Yojson.Safe.read_lcurl state lex;
      space ();
      let empty = try Yojson.Safe.read_object_end lex; false
        with Yojson.End_of_object -> true in
      if empty then finish frames (`Assoc [])
      else let name = key () in value (Object (name, []) :: frames)
    | _ ->
      let json = Yojson.Safe.read_json state lex in
      finish frames json
  and finish frames json =
    space ();
    match frames with
    | [] -> json
    | Array values :: rest ->
      let closed = try Yojson.Safe.read_array_sep state lex; false
        with Yojson.End_of_array -> true in
      let values = json :: values in
      if closed then finish rest (`List (List.rev values))
      else value (Array values :: rest)
    | Object (name, fields) :: rest ->
      let closed = try Yojson.Safe.read_object_sep state lex; false
        with Yojson.End_of_object -> true in
      let fields = (name, json) :: fields in
      if closed then finish rest (`Assoc (List.rev fields))
      else begin
        space ();
        let name = key () in
        value (Object (name, fields) :: rest)
      end
  in
  space ();
  if Yojson.Safe.read_eof lex then Yojson.json_error "Blank input data";
  let json = value [] in
  if not (Yojson.Safe.read_eof lex) then begin
    let start = lex.lex_curr_pos in
    let count = min 32 (String.length raw - start) in
    let pos = lex.lex_abs_pos + lex.lex_start_pos - state.bol in
    let last = start + min 33 (String.length raw - start) - state.bol in
    let bytes = if pos = last then Printf.sprintf "byte %i" pos
      else Printf.sprintf "bytes %i-%i" pos last in
    Yojson.json_error (Printf.sprintf
      "Line %i, %s:\nJunk after end of JSON value: '%s'"
      state.lnum bytes (String.sub raw start count))
  end;
  json

let read raw = parse ~extensions:true raw

let read_plain raw = parse ~extensions:false raw

type output =
  | Value of bool * Yojson.Safe.t
  | Values of bool * char * Yojson.Safe.t list
  | Fields of bool * (string * Yojson.Safe.t) list

let measure ~limit json =
  let exception Full in
  let left = ref limit in
  let take bytes =
    if bytes > !left then raise Full else left := !left - bytes in
  let string raw =
    let size = String.length raw in
    take 2;
    if size > !left then raise Full;
    for pos = 0 to size - 1 do
      take (match raw.[pos] with
        | '"' | '\\' | '\b' | '\012' | '\n' | '\r' | '\t' -> 2
        | '\x00'..'\x1f' | '\x7f' -> 6
        | _ -> 1)
    done
  in
  let rec digits value size =
    if value / 10 = 0 then size else digits (value / 10) (size + 1) in
  let floats = lazy (Buffer.create 32) in
  let atom = function
    | `Null -> take 4; true
    | `Bool true -> take 4; true
    | `Bool false -> take 5; true
    | `Int value -> take (digits value (if value < 0 then 2 else 1)); true
    | `Intlit raw -> take (String.length raw); true
    | `String raw -> string raw; true
    | `Float value ->
      let buffer = Lazy.force floats in
      Buffer.clear buffer;
      Yojson.Safe.write_float buffer value;
      take (Buffer.length buffer);
      true
    | `List [] | `Assoc [] -> take 2; true
    | `List _ | `Assoc _ -> false in
  let rec value json rest =
    if atom json then loop rest
    else match json with
    | `Assoc fields -> take 2; fields_next fields rest
    | `List values -> take 2; values_next values rest
    | _ -> assert false
  and loop = function
    | [] -> ()
    | Values (_, _, values) :: rest -> values_next values rest
    | Fields (_, fields) :: rest -> fields_next fields rest
    | Value (_, json) :: rest -> value json rest
  and values_next values rest =
    match values with
    | [] -> loop rest
    | json :: suffix ->
      if suffix <> [] then take 1;
      if atom json then values_next suffix rest
      else value json (Values (false, ']', suffix) :: rest)
  and fields_next fields rest =
    match fields with
    | [] -> loop rest
    | (name, json) :: suffix ->
      string name;
      take (if suffix = [] then 1 else 2);
      if atom json then fields_next suffix rest
      else value json (Fields (false, suffix) :: rest)
  in
  if limit < 0 then None
  else try value json []; Some (limit - !left) with Full -> None

let write ?(sort = false) json =
  let buf = Buffer.create 256 in
  let emit = Buffer.add_char buf in
  let scalar json = Buffer.add_string buf (Yojson.Safe.to_string json) in
  let rec loop = function
    | [] -> Buffer.contents buf
    | Value (sort, `Assoc fields) :: rest ->
      let fields = if sort then
        List.sort (fun (left, _) (right, _) -> String.compare left right) fields
        else fields in
      emit '{';
      fields_next sort fields rest
    | Value (sort, `List values) :: rest ->
      emit '[';
      values_next sort ']' values rest
    | Value (_, json) :: rest ->
      scalar json;
      loop rest
    | Values (sort, close, values) :: rest ->
      if values <> [] then emit ',';
      values_next sort close values rest
    | Fields (sort, fields) :: rest ->
      if fields <> [] then emit ',';
      fields_next sort fields rest
  and values_next sort close values rest =
    match values with
    | [] -> emit close; loop rest
    | json :: suffix -> loop (Value (sort, json) :: Values (sort, close, suffix) :: rest)
  and fields_next sort fields rest =
    match fields with
    | [] -> emit '}'; loop rest
    | (name, json) :: suffix ->
      scalar (`String name);
      emit ':';
      loop (Value (sort, json) :: Fields (sort, suffix) :: rest)
  in
  loop [Value (sort, json)]