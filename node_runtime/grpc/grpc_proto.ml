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