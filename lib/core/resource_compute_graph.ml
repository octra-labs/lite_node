(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type limits = {
  max_graph_bytes : int;
  max_shards : int;
  max_segments : int;
  max_chunks : int;
  max_tensors : int;
  max_segment_bytes : int;
  max_chunk_bytes : int;
  max_shard_bytes : int;
  max_model_bytes : int64;
  max_cache_entries : int;
  max_cache_bytes : int64;
}

type segment = {
  index : int;
  offset : int;
  bytes : int;
  path : string;
  sha256 : string;
}

type chunk = {
  index : int;
  bytes : int;
  sha256 : string;
  source : string;
  stream_offset : int64;
  tensor : string;
  tensor_chunk : int;
  shard : int;
  shard_offset : int;
}

type shard = {
  index : int;
  circle_id : string;
  bytes : int;
  sha256 : string;
  stream_offset : int64;
  chunk_indices : int list;
  segments : segment list;
}

type tensor = {
  name : string;
  source_name : string;
  scale_bits : int64;
  shape : int list;
  chunks : int list;
}

type model = {
  id : string;
  root_domain : string;
  root : string;
  raw_bytes : int64;
  chunks : int;
  tensors : tensor list;
}

type t = {
  format : string;
  root_circle_id : string;
  graph_root : string;
  layout_root : string;
  model : model;
  chunks : chunk array;
  shards : shard list;
}

type cache_source =
  | Inline of string
  | Model_chunk of chunk

type cache_entry = {
  key : string;
  bytes : int;
  sha256 : string;
  source : cache_source;
}

type cache_plan = {
  key : string;
  root : string;
  bytes : int64;
  entries : cache_entry list;
}

let default_limits = {
  max_graph_bytes = 2 * 1024 * 1024;
  max_shards = 64;
  max_segments = 1024;
  max_chunks = 2048;
  max_tensors = 1024;
  max_segment_bytes = 4 * 1024 * 1024;
  max_chunk_bytes = 8 * 1024 * 1024;
  max_shard_bytes = 32 * 1024 * 1024;
  max_model_bytes = 536_870_912L;
  max_cache_entries = 1024;
  max_cache_bytes = 536_870_912L;
}

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error _ as error -> error

let rec map_result f = function
  | [] -> Ok []
  | value :: values ->
    let* value = f value in
    let* values = map_result f values in
    Ok (value :: values)

let is_ascii value =
  String.for_all (fun char -> Char.code char < 128) value

let bounded_ascii maximum value =
  let bytes = String.length value in
  bytes > 0 && bytes <= maximum && is_ascii value

let is_lower_hex value =
  String.length value = 64
  && String.for_all
       (function
        | '0' .. '9' | 'a' .. 'f' -> true
        | _ -> false)
       value

let exact_fields names fields =
  let actual = List.map fst fields |> List.sort String.compare in
  let expected = List.sort String.compare names in
  List.length actual = List.length expected && actual = expected

let field fields name =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error ("resource graph field missing: " ^ name)

let assoc = function
  | `Assoc fields -> Ok fields
  | _ -> Error "resource graph object required"

let list = function
  | `List values -> Ok values
  | _ -> Error "resource graph list required"

let string_value = function
  | `String value when is_ascii value -> Ok value
  | _ -> Error "resource graph ASCII string required"

let int64_value = function
  | `Int value -> Ok (Int64.of_int value)
  | `Intlit value ->
    begin
      match Int64.of_string_opt value with
      | Some value -> Ok value
      | None -> Error "resource graph integer required"
    end
  | _ -> Error "resource graph integer required"

let int_value json =
  let* value = int64_value json in
  if value < 0L || value > Int64.of_int max_int then
    Error "resource graph integer exceeds finite limits"
  else
    Ok (Int64.to_int value)

let field_string fields name =
  let* value = field fields name in
  string_value value

let field_int fields name =
  let* value = field fields name in
  int_value value

let field_int64 fields name =
  let* value = field fields name in
  int64_value value

let field_list fields name =
  let* value = field fields name in
  list value

let rec ordered_json = function
  | `Assoc fields ->
    `Assoc
      (fields
       |> List.map (fun (name, value) -> name, ordered_json value)
       |> List.sort (fun (left, _) (right, _) -> String.compare left right))
  | `List values -> `List (List.map ordered_json values)
  | value -> value

let stable_json value =
  Yojson.Safe.to_string (ordered_json value)

let without name = function
  | `Assoc fields -> `Assoc (List.filter (fun (key, _) -> key <> name) fields)
  | value -> value

let digest_string value =
  Digestif.SHA256.(to_hex (digest_string value))

let digest_json domain value =
  digest_string (domain ^ "\000" ^ stable_json value)

let json_int64 value =
  if value <= Int64.of_int max_int && value >= Int64.of_int min_int then
    `Int (Int64.to_int value)
  else
    `Intlit (Int64.to_string value)

let valid_circle_id value =
  Crypto.Address.is_valid_address value

let valid_name value =
  bounded_ascii 96 value
  && String.for_all
       (function
        | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
        | _ -> false)
       value

let valid_source value =
  bounded_ascii 256 value
  && Filename.basename value = value
  && value <> "."
  && value <> ".."

let valid_asset_path value =
  bounded_ascii 128 value
  && String.length value > 12
  && String.sub value 0 12 = "/model/data/"
  && Filename.basename value <> "."
  && Filename.basename value <> ".."

let parse_segment limits json =
  let* fields = assoc json in
  if not (exact_fields ["bytes"; "index"; "offset"; "path"; "sha256"] fields) then
    Error "resource segment fields differ"
  else
    let* index = field_int fields "index" in
    let* offset = field_int fields "offset" in
    let* bytes = field_int fields "bytes" in
    let* path = field_string fields "path" in
    let* sha256 = field_string fields "sha256" in
    if bytes <= 0 || bytes > limits.max_segment_bytes then
      Error "resource segment byte limit exceeded"
    else if not (valid_asset_path path) then
      Error "resource segment path refused"
    else if not (is_lower_hex sha256) then
      Error "resource segment digest refused"
    else
      Ok { index; offset; bytes; path; sha256 }

let parse_chunk limits json =
  let* fields = assoc json in
  if not
       (exact_fields
          [
            "bytes";
            "index";
            "sha256";
            "shard";
            "shard_offset";
            "source";
            "stream_offset";
            "tensor";
            "tensor_chunk";
          ]
          fields)
  then
    Error "resource chunk fields differ"
  else
    let* index = field_int fields "index" in
    let* bytes = field_int fields "bytes" in
    let* sha256 = field_string fields "sha256" in
    let* source = field_string fields "source" in
    let* stream_offset = field_int64 fields "stream_offset" in
    let* tensor = field_string fields "tensor" in
    let* tensor_chunk = field_int fields "tensor_chunk" in
    let* shard = field_int fields "shard" in
    let* shard_offset = field_int fields "shard_offset" in
    if bytes <= 0 || bytes > limits.max_chunk_bytes then
      Error "resource chunk byte limit exceeded"
    else if not (is_lower_hex sha256) then
      Error "resource chunk digest refused"
    else if not (valid_source source) then
      Error "resource chunk source refused"
    else if stream_offset < 0L then
      Error "resource chunk stream offset refused"
    else if not (valid_name tensor) then
      Error "resource tensor name refused"
    else
      Ok {
        index;
        bytes;
        sha256;
        source;
        stream_offset;
        tensor;
        tensor_chunk;
        shard;
        shard_offset;
      }

let parse_int_list json =
  let* values = list json in
  map_result int_value values

let parse_tensor json =
  let* fields = assoc json in
  if not (exact_fields ["chunks"; "name"; "scale_bits"; "shape"; "source_name"] fields) then
    Error "resource tensor fields differ"
  else
    let* name = field_string fields "name" in
    let* source_name = field_string fields "source_name" in
    let* scale_bits = field_int64 fields "scale_bits" in
    let* chunks_json = field fields "chunks" in
    let* chunks = parse_int_list chunks_json in
    let* shape_json = field fields "shape" in
    let* shape = parse_int_list shape_json in
    if not (valid_name name) || not (bounded_ascii 256 source_name) then
      Error "resource tensor identity refused"
    else if chunks = [] || shape = [] || List.exists ((>=) 0) shape then
      Error "resource tensor dimensions refused"
    else
      Ok { name; source_name; scale_bits; shape; chunks }

let parse_model limits ~format json =
  let* fields = assoc json in
  let legacy = format = "octra-smollm-resource-graph" in
  let names =
    if legacy then
      ["chunks"; "config"; "id"; "raw_bytes"; "root"; "tensors"]
    else
      ["chunks"; "config"; "id"; "raw_bytes"; "root"; "root_domain"; "tensors"] in
  if not (exact_fields names fields) then
    Error "resource model fields differ"
  else
    let* id = field_string fields "id" in
    let* root_domain =
      if legacy then Ok "octra.smollm360.model.1"
      else field_string fields "root_domain" in
    let* root = field_string fields "root" in
    let* raw_bytes = field_int64 fields "raw_bytes" in
    let* chunks = field_int fields "chunks" in
    let* tensors_json = field_list fields "tensors" in
    let* tensors = map_result parse_tensor tensors_json in
    if
      not (bounded_ascii 256 id)
      || not (bounded_ascii 128 root_domain)
      || not (is_lower_hex root)
    then
      Error "resource model identity refused"
    else if raw_bytes <= 0L || raw_bytes > limits.max_model_bytes then
      Error "resource model byte limit exceeded"
    else if chunks <= 0 || chunks > limits.max_chunks then
      Error "resource model chunk limit exceeded"
    else if tensors = [] || List.length tensors > limits.max_tensors then
      Error "resource model tensor limit exceeded"
    else
      Ok { id; root_domain; root; raw_bytes; chunks; tensors }

let parse_shard limits json =
  let* fields = assoc json in
  if not
       (exact_fields
          [
            "bytes";
            "chunk_indices";
            "circle_id";
            "index";
            "segments";
            "sha256";
            "stream_offset";
          ]
          fields)
  then
    Error "resource shard fields differ"
  else
    let* index = field_int fields "index" in
    let* circle_id = field_string fields "circle_id" in
    let* bytes = field_int fields "bytes" in
    let* sha256 = field_string fields "sha256" in
    let* stream_offset = field_int64 fields "stream_offset" in
    let* chunk_indices_json = field fields "chunk_indices" in
    let* chunk_indices = parse_int_list chunk_indices_json in
    let* segments_json = field_list fields "segments" in
    let* segments = map_result (parse_segment limits) segments_json in
    if not (valid_circle_id circle_id) || not (is_lower_hex sha256) then
      Error "resource shard identity refused"
    else if bytes <= 0 || bytes > limits.max_shard_bytes then
      Error "resource shard byte limit exceeded"
    else if stream_offset < 0L || chunk_indices = [] || segments = [] then
      Error "resource shard layout refused"
    else
      Ok { index; circle_id; bytes; sha256; stream_offset; chunk_indices; segments }

let contiguous_index values index_of =
  List.for_all (fun (expected, value) -> index_of value = expected) (List.mapi (fun index value -> index, value) values)

let sum_int64 values =
  List.fold_left Int64.add 0L values

let unique_strings values =
  let module Strings = Set.Make (String) in
  List.fold_left (fun seen value -> Strings.add value seen) Strings.empty values
  |> Strings.cardinal
  |> Int.equal (List.length values)

let unique_ints values =
  let module Ints = Set.Make (Int) in
  List.fold_left (fun seen value -> Ints.add value seen) Ints.empty values
  |> Ints.cardinal
  |> Int.equal (List.length values)

let validate_segment_layout limits (shards : shard list) =
  let segment_count =
    List.fold_left
      (fun total (shard : shard) -> total + List.length shard.segments)
      0
      shards in
  if segment_count > limits.max_segments then
    Error "resource segment count exceeded"
  else
    let validate_shard (shard : shard) =
      let offset = ref 0 in
      let valid =
        contiguous_index shard.segments (fun (segment : segment) -> segment.index)
        && unique_strings
             (List.map (fun (segment : segment) -> segment.path) shard.segments)
        && List.for_all
             (fun (segment : segment) ->
               let matches = segment.offset = !offset in
               offset := !offset + segment.bytes;
               matches)
             shard.segments
        && !offset = shard.bytes
      in
      if valid then Ok () else Error "resource segment layout differs" in
    let* _ = map_result validate_shard shards in
    Ok ()

let validate_chunk_layout
    (model : model)
    (chunks : chunk array)
    (shards : shard list) =
  let chunk_count = Array.length chunks in
  if chunk_count <> model.chunks then
    Error "resource model chunk count differs"
  else if
    not
      (Array.for_all
         (fun (chunk : chunk) ->
           chunk.index >= 0 && chunk.index < chunk_count)
         chunks)
  then
    Error "resource chunk index refused"
  else
    let stream_offset = ref 0L in
    let chunk_stream_valid =
      Array.for_all
        (fun (chunk : chunk) ->
          let matches = chunk.stream_offset = !stream_offset in
          stream_offset := Int64.add !stream_offset (Int64.of_int chunk.bytes);
          matches)
        chunks in
    let assigned =
      List.concat_map (fun (shard : shard) -> shard.chunk_indices) shards in
    let expected = List.init chunk_count Fun.id in
    let tensor_indices =
      List.concat_map (fun (tensor : tensor) -> tensor.chunks) model.tensors in
    if not chunk_stream_valid || !stream_offset <> model.raw_bytes then
      Error "resource chunk stream differs"
    else if assigned <> expected then
      Error "resource shard partition differs"
    else if List.sort Int.compare tensor_indices <> expected || not (unique_ints tensor_indices) then
      Error "resource tensor partition differs"
    else
      let tensors_valid =
        List.for_all
          (fun (tensor : tensor) ->
            List.for_all
              (fun (tensor_chunk, index) ->
                let chunk = chunks.(index) in
                chunk.tensor = tensor.name && chunk.tensor_chunk = tensor_chunk)
              (List.mapi (fun index chunk -> index, chunk) tensor.chunks))
          model.tensors in
      let shard_offset = ref 0L in
      let shards_valid =
        List.for_all
          (fun (shard : shard) ->
            let local_offset = ref 0 in
            let valid =
              shard.stream_offset = !shard_offset
              && List.for_all
                   (fun index ->
                     let chunk = chunks.(index) in
                     let matches =
                       chunk.shard = shard.index
                       && chunk.shard_offset = !local_offset in
                     local_offset := !local_offset + chunk.bytes;
                     matches)
                   shard.chunk_indices
              && !local_offset = shard.bytes in
            shard_offset := Int64.add !shard_offset (Int64.of_int shard.bytes);
            valid)
          shards in
      if not tensors_valid then Error "resource tensor chunks differ"
      else if not shards_valid || !shard_offset <> model.raw_bytes then Error "resource shard chunks differ"
      else Ok ()

let add_u16 buffer value =
  Buffer.add_char buffer (Char.chr ((value lsr 8) land 0xff));
  Buffer.add_char buffer (Char.chr (value land 0xff))

let add_u32 buffer value =
  Buffer.add_char buffer (Char.chr ((value lsr 24) land 0xff));
  Buffer.add_char buffer (Char.chr ((value lsr 16) land 0xff));
  Buffer.add_char buffer (Char.chr ((value lsr 8) land 0xff));
  Buffer.add_char buffer (Char.chr (value land 0xff))

let add_u64 buffer value =
  for shift = 7 downto 0 do
    Buffer.add_char
      buffer
      (Char.chr
         (Int64.to_int
            (Int64.logand
               (Int64.shift_right_logical value (shift * 8))
               0xffL)))
  done

let model_root (model : model) (chunks : chunk array) =
  let buffer = Buffer.create 32_000 in
  Buffer.add_string buffer model.root_domain;
  List.iter
    (fun (tensor : tensor) ->
      add_u16 buffer (String.length tensor.name);
      Buffer.add_string buffer tensor.name;
      add_u64 buffer tensor.scale_bits;
      add_u16 buffer (List.length tensor.chunks);
      List.iteri
        (fun tensor_chunk index ->
          let chunk = chunks.(index) in
          add_u16 buffer tensor_chunk;
          add_u32 buffer chunk.bytes;
          Buffer.add_string buffer chunk.sha256)
        tensor.chunks)
    model.tensors;
  digest_string (Buffer.contents buffer)

let segment_json (segment : segment) =
  `Assoc [
    "bytes", `Int segment.bytes;
    "index", `Int segment.index;
    "offset", `Int segment.offset;
    "path", `String segment.path;
    "sha256", `String segment.sha256;
  ]

let chunk_json (chunk : chunk) =
  `Assoc [
    "bytes", `Int chunk.bytes;
    "index", `Int chunk.index;
    "sha256", `String chunk.sha256;
    "shard", `Int chunk.shard;
    "shard_offset", `Int chunk.shard_offset;
    "source", `String chunk.source;
    "stream_offset", json_int64 chunk.stream_offset;
    "tensor", `String chunk.tensor;
    "tensor_chunk", `Int chunk.tensor_chunk;
  ]

let shard_json (shard : shard) =
  `Assoc [
    "bytes", `Int shard.bytes;
    "chunk_indices", `List (List.map (fun index -> `Int index) shard.chunk_indices);
    "circle_id", `String shard.circle_id;
    "index", `Int shard.index;
    "segments", `List (List.map segment_json shard.segments);
    "sha256", `String shard.sha256;
    "stream_offset", json_int64 shard.stream_offset;
  ]

let graph_domains = function
  | "octra-smollm-resource-graph" ->
    Ok
      ( "octra:smollm360_resource_layout",
        "octra:smollm360_resource_graph",
        "octra:smollm360_resource_shard" )
  | "octra-resource-tensor-graph-v1" ->
    Ok
      ( "octra:resource_tensor_layout:v1",
        "octra:resource_tensor_graph:v1",
        "octra:resource_tensor_shard:v1" )
  | _ -> Error "resource graph format refused"

let validate_roots
    graph_json
    format
    layout_root
    graph_root
    (model : model)
    (chunks : chunk array)
    (shards : shard list) =
  let* layout_domain, graph_domain, _ = graph_domains format in
  let graph_fields =
    match graph_json with
    | `Assoc fields -> fields
    | _ -> [] in
  let layout_shards =
    shards
    |> List.map (fun (shard : shard) -> shard_json shard)
    |> List.map (without "circle_id") in
  let* chunks_json = field graph_fields "chunks" in
  let* format_json = field graph_fields "format" in
  let* model_json = field graph_fields "model" in
  let layout_body =
    `Assoc [
      "chunks", chunks_json;
      "format", format_json;
      "model", model_json;
      "shards", `List layout_shards;
    ] in
  if digest_json layout_domain layout_body <> layout_root then
    Error "resource layout root differs"
  else if digest_json graph_domain (without "graph_root" graph_json) <> graph_root then
    Error "resource graph root differs"
  else if model_root model chunks <> model.root then
    Error "resource model root differs"
  else
    Ok ()

let parse ~limits ~root_circle_id raw =
  if String.length raw = 0 || String.length raw > limits.max_graph_bytes then
    Error "resource graph byte limit exceeded"
  else if not (valid_circle_id root_circle_id) then
    Error "resource root circle refused"
  else
    let json_result =
      try Ok (Yojson.Safe.from_string raw) with _ -> Error "resource graph JSON refused" in
    let* json = json_result in
    let* fields = assoc json in
    if not
         (exact_fields
            ["chunks"; "format"; "graph_root"; "layout_root"; "model"; "root_circle_id"; "shards"]
            fields)
    then
      Error "resource graph fields differ"
    else
      let* format = field_string fields "format" in
      let* graph_root = field_string fields "graph_root" in
      let* layout_root = field_string fields "layout_root" in
      let* graph_circle_id = field_string fields "root_circle_id" in
      let* model_json = field fields "model" in
      let* model = parse_model limits ~format model_json in
      let* chunks_json = field_list fields "chunks" in
      let* chunks_list = map_result (parse_chunk limits) chunks_json in
      let* shards_json = field_list fields "shards" in
      let* shards = map_result (parse_shard limits) shards_json in
      let* _ = graph_domains format in
      if graph_circle_id <> root_circle_id then
        Error "resource root circle differs"
      else if not (is_lower_hex graph_root) || not (is_lower_hex layout_root) then
        Error "resource graph digest refused"
      else if chunks_list = [] || List.length chunks_list > limits.max_chunks then
        Error "resource chunk count exceeded"
      else if shards = [] || List.length shards > limits.max_shards then
        Error "resource shard count exceeded"
      else if not (contiguous_index chunks_list (fun (chunk : chunk) -> chunk.index)) then
        Error "resource chunk indexes differ"
      else if not (contiguous_index shards (fun (shard : shard) -> shard.index)) then
        Error "resource shard indexes differ"
      else if
        not
          (unique_strings
             (List.map (fun (shard : shard) -> shard.circle_id) shards))
      then
        Error "resource shard circle ids repeat"
      else if
        not
          (unique_strings
             (List.map (fun (tensor : tensor) -> tensor.name) model.tensors))
      then
        Error "resource tensor names repeat"
      else
        let chunks = Array.of_list chunks_list in
        let* () = validate_segment_layout limits shards in
        let* () = validate_chunk_layout model chunks shards in
        let* () = validate_roots json format layout_root graph_root model chunks shards in
        Ok { format; root_circle_id; graph_root; layout_root; model; chunks; shards }

let shard_manifest_json (graph : t) (shard : shard) =
  let shard_domain =
    match graph_domains graph.format with
    | Ok (_, _, domain) -> domain
    | Error message -> invalid_arg message in
  let chunks =
    List.map (fun index -> chunk_json graph.chunks.(index)) shard.chunk_indices in
  let body =
    `Assoc [
      "bytes", `Int shard.bytes;
      "chunks", `List chunks;
      "circle_id", `String shard.circle_id;
      "graph_root", `String graph.graph_root;
      "index", `Int shard.index;
      "layout_root", `String graph.layout_root;
      "model_root", `String graph.model.root;
      "segments", `List (List.map segment_json shard.segments);
      "sha256", `String shard.sha256;
    ] in
  match body with
  | `Assoc fields ->
    `Assoc
      (fields @ [
         "shard_root",
         `String (digest_json shard_domain body);
       ])
  | _ -> assert false

let hex_value = function
  | '0' .. '9' as value -> Char.code value - Char.code '0'
  | 'a' .. 'f' as value -> Char.code value - Char.code 'a' + 10
  | _ -> invalid_arg "hex_value"

let raw_of_hex value =
  String.init
    (String.length value / 2)
    (fun index ->
      Char.chr
        ((hex_value value.[index * 2] lsl 4)
         lor hex_value value.[index * 2 + 1]))

let cache_entry key source value bytes sha256 =
  { key; source; bytes; sha256 = if sha256 = "" then digest_string value else sha256 }

let cache_plan ~limits ~state_root graph =
  if not (is_lower_hex state_root) then
    Error "resource compute state root refused"
  else
    let inline_entries =
      List.concat_map
        (fun (tensor : tensor) ->
          let scale = Int64.to_string tensor.scale_bits in
          let chunks = string_of_int (List.length tensor.chunks) in
          [
            cache_entry ("meta:scale:" ^ tensor.name) (Inline scale) scale (String.length scale) "";
            cache_entry ("meta:chunks:" ^ tensor.name) (Inline chunks) chunks (String.length chunks) "";
          ])
        graph.model.tensors in
    let chunk_entries =
      Array.to_list graph.chunks
      |> List.map (fun (chunk : chunk) ->
        cache_entry
          (Printf.sprintf "w:%s:%d" chunk.tensor chunk.tensor_chunk)
          (Model_chunk chunk)
          ""
          chunk.bytes
          chunk.sha256) in
    let entries =
      List.sort
        (fun (left : cache_entry) (right : cache_entry) ->
          String.compare left.key right.key)
        (inline_entries @ chunk_entries) in
    let bytes =
      entries
      |> List.map (fun (entry : cache_entry) -> Int64.of_int entry.bytes)
      |> sum_int64 in
    if List.length entries > limits.max_cache_entries then
      Error "resource compute cache entry limit exceeded"
    else if bytes > limits.max_cache_bytes then
      Error "resource compute cache byte limit exceeded"
    else if
      not
        (unique_strings
           (List.map (fun (entry : cache_entry) -> entry.key) entries))
    then
      Error "resource compute cache keys repeat"
    else
      let buffer = Buffer.create 64_000 in
      Buffer.add_string buffer "octra:compute_storage_cache:v1\000";
      List.iter
        (fun (entry : cache_entry) ->
          add_u32 buffer (String.length entry.key);
          Buffer.add_string buffer entry.key;
          add_u64 buffer (Int64.of_int entry.bytes);
          Buffer.add_string buffer (raw_of_hex entry.sha256))
        entries;
      let key =
        digest_string
          (String.concat
             "\000"
             [
               "octra:resource_compute_model_cache:v2";
               graph.root_circle_id;
               graph.graph_root;
             ]) in
      Ok {
        key;
        root = digest_string (Buffer.contents buffer);
        bytes;
        entries;
      }