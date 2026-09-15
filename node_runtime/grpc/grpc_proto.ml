(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let decode run bytes =
  try Ok (run (Pbrt.Decoder.of_bytes bytes)) with
  | Pbrt.Decoder.Failure _
  | Invalid_argument _ -> Error "protobuf message is invalid"

let rec skip_all decoder =
  match Pbrt.Decoder.key decoder with
  | None -> ()
  | Some (_, kind) ->
    Pbrt.Decoder.skip decoder kind;
    skip_all decoder

let decode_empty bytes =
  decode skip_all bytes

let rec string_field name value decoder =
  match Pbrt.Decoder.key decoder with
  | None ->
    begin
      match value with
      | Some field when field <> "" -> field
      | _ -> Pbrt.Decoder.missing_field name
    end
  | Some (1, Pbrt.Bytes) ->
    string_field name (Some (Pbrt.Decoder.string decoder)) decoder
  | Some (_, kind) ->
    Pbrt.Decoder.skip decoder kind;
    string_field name value decoder

let decode_address bytes =
  decode (string_field "address" None) bytes

let decode_hash bytes =
  decode (string_field "hash" None) bytes

let decode_submit bytes =
  let limit = Octra_net.P2p_tx_gossip.max_tx_json in
  if Bytes.length bytes > limit + 5 then Error "transaction message exceeds limit"
  else
    let result = decode (fun decoder ->
      let rec loop value =
        match Pbrt.Decoder.key decoder with
        | None ->
          (match value with
           | Some json when json <> "" -> json
           | _ -> Pbrt.Decoder.missing_field "transaction_json")
        | Some (1, Pbrt.Bytes) -> loop (Some (Pbrt.Decoder.string decoder))
        | Some (1, _) -> invalid_arg "transaction field type differs"
        | Some (_, kind) -> Pbrt.Decoder.skip decoder kind; loop value
      in
      loop None) bytes
    in
    Result.bind result (fun json ->
      if String.length json > limit then Error "transaction JSON exceeds limit"
      else
        try
          match Yojson.Safe.from_string json with
          | `Assoc _ as transaction -> Ok transaction
          | _ -> Error "transaction JSON must be an object"
        with
        | Yojson.Json_error _ -> Error "transaction JSON is invalid"
        | Stack_overflow -> Error "transaction JSON nesting exceeds limit")

let rec epoch_field value decoder =
  match Pbrt.Decoder.key decoder with
  | None ->
    let epoch = Option.value ~default:0L value in
    if Int64.compare epoch 0L < 0 then
      raise (Pbrt.Decoder.Failure (Pbrt.Decoder.Overflow "epoch"))
    else if Int64.compare epoch (Int64.of_int max_int) > 0 then
      raise (Pbrt.Decoder.Failure (Pbrt.Decoder.Overflow "epoch"))
    else
      Int64.to_int epoch
  | Some (1, Pbrt.Varint) ->
    let `unsigned epoch = Pbrt.Decoder.uint64_as_varint decoder in
    epoch_field (Some epoch) decoder
  | Some (_, kind) ->
    Pbrt.Decoder.skip decoder kind;
    epoch_field value decoder

let decode_epoch bytes =
  decode (epoch_field None) bytes

let decode_health_service bytes =
  decode (fun decoder ->
    let rec loop service =
      match Pbrt.Decoder.key decoder with
      | None -> Option.value ~default:"" service
      | Some (1, Pbrt.Bytes) ->
        loop (Some (Pbrt.Decoder.string decoder))
      | Some (_, kind) ->
        Pbrt.Decoder.skip decoder kind;
        loop service
    in
    loop None) bytes

let encode field =
  let encoder = Pbrt.Encoder.create () in
  field encoder;
  Pbrt.Encoder.to_string encoder

let encode_json json =
  encode (fun encoder ->
    Pbrt.Encoder.bytes (Bytes.of_string json) encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder)

let encode_serving () =
  encode (fun encoder ->
    Pbrt.Encoder.int_as_varint 1 encoder;
    Pbrt.Encoder.key 1 Pbrt.Varint encoder)

let page_integer name decoder =
  let `unsigned value = Pbrt.Decoder.uint64_as_varint decoder in
  if value < 0L || value > Int64.of_int Epoch_page.max_epoch then
    raise (Pbrt.Decoder.Failure (Pbrt.Decoder.Overflow name));
  Int64.to_int value

let page_anchor initial decoder =
  let rec loop (anchor : Epoch_page.anchor) =
    match Pbrt.Decoder.key decoder with
    | None -> anchor
    | Some (1, Pbrt.Bytes) -> loop { anchor with chain = Pbrt.Decoder.string decoder }
    | Some (2, Pbrt.Varint) -> loop { anchor with epoch = page_integer "epoch" decoder }
    | Some (3, Pbrt.Bytes) -> loop { anchor with root = Pbrt.Decoder.string decoder }
    | Some ((1 | 2 | 3), _) -> invalid_arg "anchor field type differs"
    | Some (_, kind) -> Pbrt.Decoder.skip decoder kind; loop anchor
  in
  loop initial

let decode_page bytes =
  let result = decode (fun decoder ->
    let rec loop (request : Epoch_page.request) =
      match Pbrt.Decoder.key decoder with
      | None -> request
      | Some (1, Pbrt.Varint) -> loop { request with start = page_integer "start" decoder }
      | Some (2, Pbrt.Varint) -> loop { request with limit = page_integer "limit" decoder }
      | Some (3, Pbrt.Bytes) ->
        let initial = Option.value ~default:Epoch_page.{ chain = ""; epoch = 0; root = "" }
          request.anchor in
        loop { request with anchor = Some (page_anchor initial (Pbrt.Decoder.nested decoder)) }
      | Some (4, Pbrt.Bytes) -> loop { request with previous = Pbrt.Decoder.string decoder }
      | Some ((1 | 2 | 3 | 4), _) -> invalid_arg "page field type differs"
      | Some (_, kind) -> Pbrt.Decoder.skip decoder kind; loop request
    in
    loop { start = 0; limit = 32; anchor = None; previous = "" }) bytes
  in
  Result.bind result (fun request ->
    Result.map_error (fun error -> error.Octra_core.Rpc.message) (Epoch_page.validate request))

let put_int field value encoder =
  Pbrt.Encoder.uint64_as_varint (`unsigned value) encoder;
  Pbrt.Encoder.key field Pbrt.Varint encoder

let put_string field value encoder =
  Pbrt.Encoder.string value encoder;
  Pbrt.Encoder.key field Pbrt.Bytes encoder

let put_anchor (anchor : Epoch_page.anchor) encoder =
  put_string 3 anchor.root encoder;
  put_int 2 (Int64.of_int anchor.epoch) encoder;
  put_string 1 anchor.chain encoder

let put_row (row : Epoch_page.row) encoder =
  Pbrt.Encoder.float_as_bits64 row.time encoder;
  Pbrt.Encoder.key 6 Pbrt.Bits64 encoder;
  put_int 5 (Int64.of_int row.tx_count) encoder;
  put_int 4 row.tx_start encoder;
  put_string 3 row.previous encoder;
  put_string 2 row.root encoder;
  put_int 1 (Int64.of_int row.epoch) encoder

let encode_page (page : Epoch_page.page) =
  encode (fun encoder ->
    put_int 4 (match page.stop with Complete -> 1L | More -> 2L | Gap -> 3L) encoder;
    put_int 3 (Int64.of_int page.next) encoder;
    List.iter (fun row ->
      Pbrt.Encoder.nested put_row row encoder;
      Pbrt.Encoder.key 2 Pbrt.Bytes encoder) (List.rev page.rows);
    Pbrt.Encoder.nested put_anchor page.anchor encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder)