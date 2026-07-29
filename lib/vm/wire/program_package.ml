(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type source = {
  path : string;
  body : string;
}

type compiled = {
  package : string;
  envelope : string;
  result : Oct_compile.compile_result;
}

type admitted = {
  envelope : string;
  program : Admission.t;
}

type package = {
  compiler_profile : int;
  main : string;
  sources : source list;
  envelope : string;
}

type error =
  | Invalid_encoding
  | Too_short
  | Bad_magic
  | Bad_version
  | Bad_compiler_profile
  | Bad_length
  | Trailing_bytes
  | Invalid_main
  | Invalid_path
  | Duplicate_path
  | Missing_main
  | Unused_source of string
  | Too_many_sources
  | Source_too_large
  | Sources_too_large
  | Envelope_too_large
  | Compile_failed of string
  | Envelope_missing
  | Envelope_mismatch
  | Admission_failed of string

let error_message = function
  | Invalid_encoding -> "invalid Program package encoding"
  | Too_short -> "Program package too short"
  | Bad_magic -> "bad Program package magic"
  | Bad_version -> "unsupported Program package version"
  | Bad_compiler_profile -> "unsupported Program compiler profile"
  | Bad_length -> "invalid Program package length"
  | Trailing_bytes -> "Program package trailing bytes"
  | Invalid_main -> "invalid Program main path"
  | Invalid_path -> "invalid Program source path"
  | Duplicate_path -> "duplicate Program source path"
  | Missing_main -> "Program main source missing"
  | Unused_source path -> "unused Program source: " ^ path
  | Too_many_sources -> "too many Program sources"
  | Source_too_large -> "Program source too large"
  | Sources_too_large -> "Program sources too large"
  | Envelope_too_large -> "Program envelope too large"
  | Compile_failed reason -> "Program compile failed: " ^ reason
  | Envelope_missing -> "Program envelope missing"
  | Envelope_mismatch -> "Program source and envelope mismatch"
  | Admission_failed reason -> "Program admission failed: " ^ reason

let magic = "OCPS"
let wire_version = 1
let compiler_profile = 1
let compiler_profile_id = "rehovot_1"
let supported_compiler_profiles = [1]
let max_sources = 32
let max_path = 256
let max_source = 65_536
let max_sources_bytes = 262_144
let max_envelope = 2_097_152

let bind result next =
  match result with
  | Ok value -> next value
  | Error error -> Error error

let ( let* ) = bind

let path_char = function
  | 'a'..'z'
  | 'A'..'Z'
  | '0'..'9'
  | '.'
  | '_'
  | '-'
  | '/' -> true
  | _ -> false

let valid_path path =
  let length = String.length path in
  length > 0
  && length <= max_path
  && path.[0] <> '/'
  && String.for_all path_char path
  && (String.split_on_char '/' path
      |> List.for_all (fun part -> part <> "" && part <> "." && part <> ".."))

let normalize ~main sources =
  if not (valid_path main) then Error Invalid_main
  else if sources = [] || List.length sources > max_sources then
    Error Too_many_sources
  else
    let sorted =
      List.sort (fun left right -> String.compare left.path right.path) sources
    in
    let rec check previous total = function
      | [] ->
        if total > max_sources_bytes then Error Sources_too_large
        else if List.exists (fun source -> source.path = main) sorted then Ok sorted
        else Error Missing_main
      | source :: rest ->
        if not (valid_path source.path) then Error Invalid_path
        else if Some source.path = previous then Error Duplicate_path
        else if String.length source.body > max_source then Error Source_too_large
        else
          check
            (Some source.path)
            (total + String.length source.path + String.length source.body)
            rest
    in
    check None 0 sorted

let put_u16 buffer value =
  Buffer.add_char buffer (Char.chr (value land 0xff));
  Buffer.add_char buffer (Char.chr ((value lsr 8) land 0xff))

let put_u32 buffer value =
  Buffer.add_char buffer (Char.chr (value land 0xff));
  Buffer.add_char buffer (Char.chr ((value lsr 8) land 0xff));
  Buffer.add_char buffer (Char.chr ((value lsr 16) land 0xff));
  Buffer.add_char buffer (Char.chr ((value lsr 24) land 0xff))

let encode package =
  if String.length package.envelope > max_envelope then Error Envelope_too_large
  else
    let buffer = Buffer.create 4096 in
    Buffer.add_string buffer magic;
    put_u16 buffer wire_version;
    put_u16 buffer package.compiler_profile;
    put_u16 buffer (String.length package.main);
    Buffer.add_string buffer package.main;
    put_u16 buffer (List.length package.sources);
    List.iter
      (fun source ->
        put_u16 buffer (String.length source.path);
        Buffer.add_string buffer source.path;
        put_u32 buffer (String.length source.body);
        Buffer.add_string buffer source.body)
      package.sources;
    put_u32 buffer (String.length package.envelope);
    Buffer.add_string buffer package.envelope;
    Ok (Buffer.contents buffer)

let take raw offset length =
  if length < 0 || offset < 0 || offset > String.length raw - length then
    Error Bad_length
  else
    Ok (String.sub raw offset length, offset + length)

let u16 raw offset =
  bind (take raw offset 2) (fun (bytes, next) ->
    Ok (Char.code bytes.[0] lor (Char.code bytes.[1] lsl 8), next))

let u32 raw offset =
  bind (take raw offset 4) (fun (bytes, next) ->
    Ok
      (Char.code bytes.[0]
       lor (Char.code bytes.[1] lsl 8)
       lor (Char.code bytes.[2] lsl 16)
       lor (Char.code bytes.[3] lsl 24),
       next))

let rec read_sources raw count offset sources =
  if count = 0 then Ok (List.rev sources, offset)
  else
    bind (u16 raw offset) (fun (path_length, path_offset) ->
    bind (take raw path_offset path_length) (fun (path, body_length_offset) ->
    bind (u32 raw body_length_offset) (fun (body_length, body_offset) ->
    bind (take raw body_offset body_length) (fun (body, next) ->
      read_sources raw (count - 1) next ({ path; body } :: sources)))))

let decode raw =
  if String.length raw < 12 then Error Too_short
  else if String.sub raw 0 4 <> magic then Error Bad_magic
  else
    let* decoded_version, profile_offset = u16 raw 4 in
    let* decoded_profile, main_length_offset = u16 raw profile_offset in
    let* main_length, main_offset = u16 raw main_length_offset in
    let* main, count_offset = take raw main_offset main_length in
    let* count, sources_offset = u16 raw count_offset in
    if decoded_version <> wire_version then Error Bad_version
    else if not (List.mem decoded_profile supported_compiler_profiles) then
      Error Bad_compiler_profile
    else if count = 0 || count > max_sources then Error Too_many_sources
    else
      let* sources, envelope_length_offset =
        read_sources raw count sources_offset []
      in
      let* envelope_length, envelope_offset =
        u32 raw envelope_length_offset
      in
      if envelope_length > max_envelope then Error Envelope_too_large
      else
        let* envelope, final_offset =
          take raw envelope_offset envelope_length
        in
        if final_offset <> String.length raw then Error Trailing_bytes
        else
          let* normalized = normalize ~main sources in
          if normalized <> sources then Error Invalid_path
          else
            match Program_envelope.decode envelope with
            | Error error ->
              Error
                (Admission_failed
                   (Program_envelope.error_message error))
            | Ok _ ->
              Ok {
                compiler_profile = decoded_profile;
                main;
                sources;
                envelope;
              }

let decode_base64 encoded =
  match Base64.decode encoded with
  | Ok raw when String.equal (Base64.encode_exn raw) encoded -> decode raw
  | Ok _
  | Error _ -> Error Invalid_encoding

let compile_sources package =
  let sources = Hashtbl.create (List.length package.sources) in
  let used = Hashtbl.create (List.length package.sources) in
  List.iter
    (fun source -> Hashtbl.add sources source.path source.body)
    package.sources;
  let resolver path =
    match Hashtbl.find_opt sources path with
    | None -> None
    | Some source ->
      Hashtbl.replace used path ();
      Some source
  in
  let result =
    match package.compiler_profile with
    | 1 -> Oct_compile.compile_program_multi resolver package.main
    | _ -> Oct_compile.error_result "unsupported Program compiler profile"
  in
  result, used

let build package =
  let result, used = compile_sources package in
  match result.Oct_compile.error, result.program_envelope with
  | Some reason, _ -> Error (Compile_failed reason)
  | None, None -> Error Envelope_missing
  | None, Some envelope ->
    match
      List.find_opt
        (fun source -> not (Hashtbl.mem used source.path))
        package.sources
    with
    | Some source -> Error (Unused_source source.path)
    | None ->
      bind
        (match Admission.decode_program_source envelope with
         | Ok admitted -> Ok admitted
         | Error error ->
           Error (Admission_failed (Admission.error_message error)))
        (fun _ ->
          bind (encode { package with envelope }) (fun encoded ->
            Ok { package = encoded; envelope; result }))

let compile ~main ~sources =
  bind (normalize ~main sources) (fun sources ->
    build {
      compiler_profile;
      main;
      sources;
      envelope = "";
    })

let validate_base64 encoded =
  bind (decode_base64 encoded) (fun _ -> Ok ())

let admit_base64 encoded =
  bind (decode_base64 encoded) (fun package ->
    bind (build { package with envelope = "" }) (fun compiled ->
      if not (String.equal compiled.envelope package.envelope) then
        Error Envelope_mismatch
      else
        match Admission.decode_program_source package.envelope with
        | Error error ->
          Error (Admission_failed (Admission.error_message error))
        | Ok admitted ->
          Ok {
            envelope = package.envelope;
            program = admitted;
          }))