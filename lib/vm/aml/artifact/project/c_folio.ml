(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bits = C_bin.bits

type part = {
  path : string;
  name : string;
  octb : string;
  seal : bits;
  map : bits;
  live : bits;
}

type t = {
  project : C_proj.t;
  epoch : Z.t;
  parts : part list;
}

type origin = {
  src : C_proj.src;
  root : C_proj.root;
  part : part;
  input : C_feed.t option;
  spans : C_lex.span array;
  rows : C_live.slot list array;
}

type error =
  | Target
  | Rule of C_rule.error
  | Rule_id
  | Root of string
  | Feed of string * C_feed.error
  | Octb of string * C_octb.error
  | Seal of string * C_seal.error
  | Image
  | Header
  | Size of int * int
  | Epoch of Z.t
  | Form

let epoch_max = Z.(sub (shift_left one 63) one)

let epoch_valid value =
  Z.sign value >= 0 && Z.leq value epoch_max

let part ~path ~name ~octb ~seal ~map ~live =
  { path; name; octb; seal; map; live }

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let ( let+ ) value next =
  match value with
  | Some value -> next value
  | None -> None

let text_code value =
  let rec loop index out =
    if index < 0 then out
    else
      loop (index - 1)
        (C_bin.Cons (C_bin.Num (Z.of_int (Char.code value.[index])), out))
  in
  loop (String.length value - 1) C_bin.Nil

let text_get input =
  let out = Buffer.create 32 in
  let rec loop = function
    | C_bin.Nil -> Some (Buffer.contents out)
    | C_bin.Cons (C_bin.Num value, rest) ->
      let+ value = C_nat.byte value in
      Buffer.add_char out (Char.chr value);
      loop rest
    | _ -> None
  in
  loop input

let bit_code value =
  Some (C_bin.Tag ((if value then Z.one else Z.zero), C_bin.Nil))

let bit_get = function
  | C_bin.Tag (tag, C_bin.Nil) when Z.equal tag Z.zero -> Some false
  | C_bin.Tag (tag, C_bin.Nil) when Z.equal tag Z.one -> Some true
  | _ -> None

let bits_code values = C_bin.list_code bit_code values
let bits_get input = C_bin.list_get bit_get input

let part_code value =
  let+ seal = bits_code value.seal in
  let+ map = bits_code value.map in
  let+ live = bits_code value.live in
  Some (C_bin.Tag (Z.of_int 4,
    C_bin.Cons (text_code value.path,
      C_bin.Cons (text_code value.name,
        C_bin.Cons (text_code value.octb,
          C_bin.Cons (seal, C_bin.Cons (map,
            C_bin.Cons (live, C_bin.Nil))))))))

let part_get = function
  | C_bin.Tag (tag,
      C_bin.Cons (path,
        C_bin.Cons (name,
          C_bin.Cons (octb,
            C_bin.Cons (seal, C_bin.Cons (map,
              C_bin.Cons (live, C_bin.Nil)))))))
      when Z.equal tag (Z.of_int 4) ->
    let+ path = text_get path in
    let+ name = text_get name in
    let+ octb = text_get octb in
    let+ seal = bits_get seal in
    let+ map = bits_get map in
    let+ live = bits_get live in
    Some { path; name; octb; seal; map; live }
  | _ -> None

let code value =
  let+ project = C_pimg.code value.project in
  let+ parts = C_bin.list_code part_code value.parts in
  Some (C_bin.Tag (Z.of_int 5,
    C_bin.Cons (project,
      C_bin.Cons (C_bin.Num value.epoch,
        C_bin.Cons (parts, C_bin.Nil)))))

let get = function
  | C_bin.Tag (tag,
      C_bin.Cons (project,
        C_bin.Cons (C_bin.Num epoch, C_bin.Cons (parts, C_bin.Nil))))
      when Z.equal tag (Z.of_int 5) && epoch_valid epoch ->
    let+ project = C_pimg.get project in
    let+ parts = C_bin.list_get part_get parts in
    Some { project; epoch; parts }
  | _ -> None

let valid_part value =
  String.length value.octb > 0
  && String.length value.octb <= C_rule.local.str
  && Option.is_some (C_seal.decode value.seal)
  && Option.is_some (C_smap.dec value.map)
  && Option.is_some (C_live.dec value.live)

let rec parts_valid roots parts =
  match roots, parts with
  | [], [] -> true
  | (root : C_proj.root) :: root_rest, part :: part_rest ->
    String.equal root.path part.path
    && String.equal root.name part.name
    && valid_part part
    && parts_valid root_rest part_rest
  | _ -> false

let valid value =
  epoch_valid value.epoch
  && C_pimg.valid value.project
  && C_proj.target value.project = C_proj.Octb1
  && parts_valid (C_proj.roots value.project) value.parts

let form ~project ~epoch ~parts =
  let value = { project; epoch; parts } in
  if valid value then Some value else None

let enc value =
  if not (valid value) then None
  else
    let+ code = code value in
    let+ bits = C_bin.enc_code code in
    if List.length bits <= C_rule.local.str then Some bits else None

let dec input =
  if List.length input > C_rule.local.str then None
  else
    let+ code = C_bin.dec_code input in
    let+ value = get code in
    let+ exact = enc value in
    if List.equal Bool.equal exact input then Some value else None

let src_at path values =
  List.find_opt
    (fun (value : C_proj.src) -> String.equal value.path path)
    values

let source project path = src_at path (C_proj.srcs project)

let feed_at input =
  if String.equal input "" then Some None
  else
    match C_feed.decode input with
    | Ok value -> Some (Some value)
    | Error _ -> None

let origin_one srcs (root : C_proj.root) part =
  if not
      (String.equal root.path part.path && String.equal root.name part.name)
  then None
  else
    match src_at root.path srcs, feed_at root.feed,
      C_smap.dec part.map, C_live.dec part.live with
    | Some src, Some input, Some map, Some live ->
      Some {
        src;
        root;
        part;
        input;
        spans = C_smap.spans map;
        rows = C_live.rows live;
      }
    | _, _, _, _ -> None

let rec origins_at srcs roots parts =
  match roots, parts with
  | [], [] -> Some []
  | root :: root_rest, part :: part_rest ->
    let+ first = origin_one srcs root part in
    let+ rest = origins_at srcs root_rest part_rest in
    Some (first :: rest)
  | _, _ -> None

let origins value =
  origins_at (C_proj.srcs value.project) (C_proj.roots value.project)
    value.parts

let one schedule epoch project (root : C_proj.root) =
  match source project root.path with
  | None -> Error (Root root.path)
  | Some source ->
    let* image =
      if String.equal root.feed "" then
        match C_octb.compile source.body with
        | Ok value -> Ok value
        | Error error -> Error (Octb (root.path, error))
      else
        let* feed =
          match C_feed.decode root.feed with
          | Ok value -> Ok value
          | Error error -> Error (Feed (root.path, error))
        in
        match C_octb.compile_feed source.body feed with
        | Ok value -> Ok value
        | Error error -> Error (Octb (root.path, error))
    in
    let* seal =
      match C_seal.make schedule ~epoch source.body with
      | Ok value -> Ok value
      | Error error -> Error (Seal (root.path, error))
    in
    Ok {
      path = root.path;
      name = root.name;
      octb = image.octb;
      seal;
      map = C_smap.enc image.map;
      live = C_live.enc image.live;
    }

let make schedule ~epoch project =
  if not (epoch_valid epoch) then Error (Epoch epoch)
  else if C_proj.target project <> C_proj.Octb1 then Error Target
  else
    let* selected =
      match C_rule.select schedule ~epoch with
      | Ok value -> Ok value
      | Error error -> Error (Rule error)
    in
    if not (C_rule.same_id (C_rule.id selected) (C_proj.rule project)) then
      Error Rule_id
    else
      let rec loop out = function
        | [] -> Ok (List.rev out)
        | root :: rest ->
          let* part = one schedule epoch project root in
          loop (part :: out) rest
      in
      let* parts = loop [] (C_proj.roots project) in
      let value = { project; epoch; parts } in
      if Option.is_some (enc value) then Ok value else Error Image

let magic = "CF1\000"

let be64 value =
  let out = Bytes.make 8 '\000' in
  let raw = Int64.of_int value in
  for index = 0 to 7 do
    let shift = (7 - index) * 8 in
    let octet =
      Int64.to_int
        (Int64.logand (Int64.shift_right_logical raw shift) 0xffL)
    in
    Bytes.set out index (Char.chr octet)
  done;
  Bytes.unsafe_to_string out

let pack bits =
  let count = (List.length bits + 7) / 8 in
  let out = Bytes.make count '\000' in
  let rec loop index = function
    | [] -> Bytes.unsafe_to_string out
    | bit :: rest ->
      if bit then begin
        let at = index / 8 in
        let shift = 7 - index mod 8 in
        let value = Char.code (Bytes.get out at) lor (1 lsl shift) in
        Bytes.set out at (Char.chr value)
      end;
      loop (index + 1) rest
  in
  loop 0 bits

let file value =
  let+ bits = enc value in
  Some (magic ^ be64 (List.length bits) ^ pack bits)

let read_size input =
  let rec loop index value =
    if index = 12 then Some value
    else
      let octet = Int64.of_int (Char.code input.[index]) in
      let value = Int64.logor (Int64.shift_left value 8) octet in
      loop (index + 1) value
  in
  let+ value = loop 4 Int64.zero in
  if Int64.compare value Int64.zero >= 0
      && Int64.compare value (Int64.of_int C_rule.local.str) <= 0 then
    Some (Int64.to_int value)
  else None

let unpack input size =
  List.init size (fun index ->
    let octet = Char.code input.[12 + index / 8] in
    octet land (1 lsl (7 - index mod 8)) <> 0)

let of_file input =
  let actual = String.length input in
  if actual < 12 || not (String.equal (String.sub input 0 4) magic) then
    Error Header
  else
    match read_size input with
    | None -> Error (Size (C_rule.local.str, actual))
    | Some size ->
      let expected = 12 + (size + 7) / 8 in
      if actual <> expected then Error (Size (expected, actual))
      else
        let bits = unpack input size in
        match dec bits with
        | None -> Error Form
        | Some value ->
          begin
            match file value with
            | Some exact when String.equal exact input -> Ok value
            | Some _ | None -> Error Form
          end

let rules project =
  let id = C_proj.rule project in
  let* rule =
    match C_rule.rule ~version:(C_rule.version id) ~activate:Z.zero
        (C_rule.id_limits id) with
    | Ok value -> Ok value
    | Error error -> Error (Rule error)
  in
  match C_rule.schedule [rule] with
  | Ok value -> Ok value
  | Error error -> Error (Rule error)

let verify input =
  let* value = of_file input in
  let* schedule = rules value.project in
  let* rebuilt = make schedule ~epoch:value.epoch value.project in
  match file rebuilt with
  | Some exact when String.equal exact input -> Ok rebuilt
  | Some _ | None -> Error Form

let text = function
  | Target -> "project target is not OCTB"
  | Rule error -> C_rule.text error
  | Rule_id -> "project rule differs from selected rule"
  | Root path -> "project root source is absent path = " ^ path
  | Feed (path, error) ->
    "project root feed refusal path = " ^ path ^ " reason = " ^ C_feed.text error
  | Octb (path, error) ->
    "project root OCTB refusal path = " ^ path ^ " reason = " ^ C_octb.text error
  | Seal (path, error) ->
    "project root seal refusal path = " ^ path ^ " reason = " ^ C_seal.text error
  | Image -> "project folio encoding failed"
  | Header -> "project folio header is invalid"
  | Size (expected, actual) ->
    Printf.sprintf "project folio size expected = %d actual = %d" expected actual
  | Epoch value ->
    Printf.sprintf "project folio epoch is invalid value = %s maximum = %s"
      (Z.to_string value) (Z.to_string epoch_max)
  | Form -> "project folio form is invalid"