(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  admitted_code : Contract_vm.instr array;
  admitted_effects : Program_effects.t;
  profile : profile;
  compiler_version : string option;
}

and profile =
  | Legacy
  | Program of Program_type_flow.facts

type error =
  | Decode_error of string
  | Verify_error of string
  | Unsafe_error of string

let verifier_error = function
  | Contract_vm.Verifier.InvalidReg (pc, reg) ->
    Printf.sprintf "invalid register r%d at pc %d" reg pc
  | Contract_vm.Verifier.InvalidRegSpan (pc, base, count) ->
    Printf.sprintf "invalid register span r%d+%d at pc %d" base count pc
  | Contract_vm.Verifier.InvalidJumpDest dest ->
    Printf.sprintf "invalid jump destination %d" dest
  | Contract_vm.Verifier.DuplicateJDest name ->
    Printf.sprintf "duplicate JDEST %d" name
  | Contract_vm.Verifier.CodeTooLarge size ->
    Printf.sprintf "code too large: %d instructions" size
  | Contract_vm.Verifier.EmptyCode ->
    "empty code"
  | Contract_vm.Verifier.ReservedKey (pc, _) ->
    Printf.sprintf "write to reserved key at pc %d" pc
  | Contract_vm.Verifier.CapabilityLiteral pc ->
    Printf.sprintf "capability literal at pc %d" pc
  | Contract_vm.Verifier.CapabilityKind (pc, kind) ->
    Printf.sprintf "capability kind %s at pc %d" (Z.to_string kind) pc

let admit ~program ~point_ops code =
  match Contract_vm.Verifier.verify code with
  | Error err -> Error (Verify_error (verifier_error err))
  | Ok () ->
    match if point_ops then None else Opcode_policy.first_standard code with
    | Some hit ->
      Error (Unsafe_error (Opcode_policy.standard_error_message hit))
    | None ->
      let policy_error =
        if program then
          Option.map
            (fun hit -> `Consensus_unsafe hit)
            (Opcode_policy.first_host_float code)
        else
          match Opcode_policy.legacy_error code with
          | Some (Opcode_policy.Program_only hit) -> Some (`Program_only hit)
          | Some (Opcode_policy.Consensus_unsafe hit) -> Some (`Consensus_unsafe hit)
          | None -> None
      in
      match policy_error with
      | Some (`Program_only hit) ->
        Error (Unsafe_error (Opcode_policy.program_only_error_message hit))
      | Some (`Consensus_unsafe hit) ->
        Error (Unsafe_error (Opcode_policy.error_message hit))
      | None ->
        Ok {
          admitted_code = Array.copy code;
          admitted_effects = Program_effects.scan code;
          profile = if program then Program Program_type_flow.empty_facts else Legacy;
          compiler_version = None;
        }

let of_code ?(point_ops = false) code = admit ~program:false ~point_ops code

module Decode : sig
  val code : active:bool -> string -> (Contract_vm.instr array, string) result
end = struct
  type key = { active : bool; hash : string; raw : string }
  type entry = { key : key; code : Contract_vm.instr array; bytes : int }
  type _ message =
    | Read : key -> Contract_vm.instr array option message
    | Keep : entry -> unit message

  let max_entries = 64
  let max_bytes = 32 * 1024 * 1024

  let same left right =
    left.active = right.active && String.equal left.hash right.hash
    && String.equal left.raw right.raw

  let rec take slots bytes = function
    | entry :: rest when slots > 0 && entry.bytes <= bytes ->
      entry :: take (slots - 1) (bytes - entry.bytes) rest
    | _ -> []

  let step : type a. entry list -> a message -> entry list * a = fun entries -> function
    | Read key ->
      (match List.find_opt (fun entry -> same entry.key key) entries with
       | None -> entries, None
       | Some entry ->
         take max_entries max_bytes
           (entry :: List.filter (fun item -> not (same item.key key)) entries),
         Some entry.code)
    | Keep entry ->
      let rest = List.filter (fun item -> not (same item.key entry.key)) entries in
      take max_entries max_bytes (entry :: rest), ()

  let entries = ref []
  let lock = Mutex.create ()

  let request : type a. a message -> a = fun message ->
    Mutex.lock lock;
    Fun.protect ~finally:(fun () -> Mutex.unlock lock) (fun () ->
      let next, reply = step !entries message in
      entries := next;
      reply)

  let code ~active raw =
    let length = String.length raw in
    if length > max_bytes - 1024 then Bytecode.decode ~active raw
    else
      let key = { active; raw; hash = Digestif.SHA256.(digest_string raw |> to_raw_string) } in
      match request (Read key) with
      | Some code -> Ok (Array.copy code)
      | None ->
        match Bytecode.decode ~active raw with
        | Error error -> Error error
        | Ok code ->
          let word = Sys.word_size / 8 in
          let bytes = 1024 + word * (length / word + 2) in
          let words = Obj.reachable_words (Obj.repr code) in
          if bytes > max_bytes || words > (max_bytes - bytes) / word then Ok code
          else begin
            let bytes = bytes + word * words in
            request (Keep { key; code; bytes });
            Ok (Array.copy code)
          end
end

let decode ?(point_ops = false) raw =
  match Decode.code ~active:point_ops raw with
  | Error err -> Error (Decode_error err)
  | Ok code -> of_code ~point_ops code

let of_program ?(point_ops = false) ?(facts = Program_type_flow.empty_facts) code =
  match admit ~program:true ~point_ops code with
  | Error error -> Error error
  | Ok admitted ->
    (match Program_type_flow.check ~facts admitted.admitted_code with
     | Ok () -> Ok { admitted with profile = Program facts }
     | Error error -> Error (Verify_error ("Program type flow: " ^ Program_type_flow.error_message error)))

let cert_field name fields =
  match List.filter (fun (key, _) -> String.equal key name) fields with
  | [(_, value)] -> Some value
  | _ -> None

let cert_text name fields =
  match cert_field name fields with
  | Some (`String value) -> Some value
  | _ -> None

let cert_effects fields =
  let rec read acc = function
    | [] -> Some (List.rev acc)
    | `String value :: rest -> read (value :: acc) rest
    | _ -> None
  in
  match cert_field "effects" fields with
  | Some (`List values) -> read [] values
  | _ -> None

let fact_kind fields name =
  match cert_field name fields with
  | Some (`String value) -> Program_type_flow.kind_of_name value
  | _ -> None

let fact_int fields name =
  match cert_field name fields with
  | Some (`Int value) -> Some value
  | _ -> None

module Slots = Set.Make (Int)
module Keys = Set.Make (String)

let fact_pairs value key =
  match value with
  | `List values ->
    let rec read seen acc = function
      | [] -> Some (List.rev acc)
      | `Assoc fields :: rest ->
        (match fact_int fields key, fact_kind fields "kind" with
         | Some slot, Some kind when slot >= 0 && not (Slots.mem slot seen) ->
           read (Slots.add slot seen) ((slot, kind) :: acc) rest
         | _ -> None)
      | _ -> None
    in
    read Slots.empty [] values
  | _ -> None

let fact_effects value =
  match value with
  | `List values ->
    let rec read acc = function
      | [] -> Some (List.rev acc)
      | `String value :: rest -> read (value :: acc) rest
      | _ -> None
    in
    read [] values
  | _ -> None

let fact_storage value =
  match value with
  | `List values ->
    let rec read seen acc = function
      | [] -> Some (List.rev acc)
      | `Assoc fields :: rest ->
        (match cert_text "key" fields, fact_kind fields "kind" with
         | Some key, Some kind
           when key <> "" && kind <> Program_type_flow.Unknown
             && not (Keys.mem key seen) ->
           read (Keys.add key seen) ((key, kind) :: acc) rest
         | _ -> None)
      | _ -> None
    in
    read Keys.empty [] values
  | _ -> None

let fact_entries value =
  match value with
  | `List values ->
    let rec read seen acc = function
      | [] -> Some (List.rev acc)
      | `Assoc fields :: rest ->
        (match fact_int fields "target", cert_field "memory" fields,
               cert_field "effects" fields with
         | Some target, Some memory, Some effects
           when target >= 0 && not (Slots.mem target seen) ->
           (match fact_pairs memory "slot", fact_effects effects with
            | Some mem, Some effects ->
              read (Slots.add target seen)
                ({ Program_type_flow.target; mem; effects } :: acc)
                rest
            | _ -> None)
         | _ -> None)
      | _ -> None
    in
    read Slots.empty [] values
  | _ -> None

let fact_kinds value =
  match value with
  | `List values ->
    let rec read acc = function
      | [] -> Some (List.rev acc)
      | `String value :: rest ->
        (match Program_type_flow.kind_of_name value with
         | Some kind when kind <> Program_type_flow.Unknown -> read (kind :: acc) rest
         | _ -> None)
      | _ -> None
    in
    read [] values
  | _ -> None

let fact_capabilities value =
  match value with
  | `List values ->
    let rec read acc = function
      | [] -> Some (List.rev acc)
      | `String value :: rest ->
        (match Program_type_flow.capability_of_name value with
         | Some capability -> read (capability :: acc) rest
         | None -> None)
      | _ -> None
    in
    read [] values
  | _ -> None

let fact_xcalls value =
  match value with
  | `List values ->
    let rec read seen acc = function
      | [] -> Some (List.rev acc)
      | `Assoc fields :: rest ->
        (match fact_int fields "pc", cert_text "method" fields,
               cert_field "inputs" fields, fact_kind fields "output",
               cert_field "capabilities" fields with
         | Some pc, Some method_name, Some inputs, Some output, Some capabilities
           when pc >= 0 && method_name <> "" && not (Slots.mem pc seen) ->
           (match fact_kinds inputs, fact_capabilities capabilities with
            | Some inputs, Some capabilities ->
              read (Slots.add pc seen)
                ({ Program_type_flow.pc; method_name; inputs; output; capabilities } :: acc)
                rest
            | _ -> None)
         | _ -> None)
      | _ -> None
    in
    read Slots.empty [] values
  | _ -> None

let fact_calls value =
  match value with
  | `List values ->
    let rec read seen acc = function
      | [] -> Some (List.rev acc)
      | `Assoc fields :: rest ->
        (match fact_int fields "owner", fact_int fields "pc", fact_int fields "target",
               fact_kind fields "kind" with
         | Some owner, Some pc, Some target, Some kind
           when owner >= 0 && pc >= 0 && target >= 0 && not (Slots.mem pc seen) ->
           read (Slots.add pc seen)
             ({ Program_type_flow.owner; pc; target; kind } :: acc) rest
         | _ -> None)
      | _ -> None
    in
    read Slots.empty [] values
  | _ -> None

let cert_facts fields =
  match cert_field "facts" fields with
  | Some (`Assoc values) ->
    (match cert_field "root" values,
           cert_field "entries" values,
           cert_field "calls" values with
     | Some root, Some entries, Some calls ->
       (match fact_pairs root "slot", fact_entries entries, fact_calls calls with
        | Some root, Some entries, Some calls ->
          let storage =
            match cert_field "storage" values with
            | None -> Some []
            | Some value -> fact_storage value
          in
          let xcalls =
            match cert_field "xcalls" values with
            | None -> Some []
            | Some value -> fact_xcalls value
          in
          (match storage, xcalls with
           | Some storage, Some xcalls ->
             Some { Program_type_flow.root; storage; entries; calls; xcalls }
           | _ -> None)
        | _ -> None)
     | _ -> None)
  | _ -> None

let valid_digest = function
  | Some value ->
    String.length value = 64
    && String.for_all
         (function
           | '0' .. '9' | 'a' .. 'f' -> true
           | _ -> false)
         value
  | None -> false

let valid_provenance fields =
  let compiler = cert_text "compiler" fields in
  let version = cert_text "compiler_version" fields in
  let source_mode = cert_text "source_mode" fields in
  let source_hash = cert_text "source_hash" fields in
  let verification_hash = cert_text "verification_hash" fields in
  compiler = Some "octra_aml"
  && (match version with Some value -> value <> "" | None -> false)
  && (match source_mode with
      | Some value -> value = "single" || value = "multi"
      | None -> false)
  && valid_digest source_hash
  && valid_digest verification_hash

let verify_program_cert ~attested ~trusted raw_code code raw =
  try
    match Octra_core.Json_tree.read raw with
    | `Assoc fields ->
      let schema = cert_text "schema" fields in
      let declaration = cert_text "declaration" fields in
      let bytecode_hash = cert_text "bytecode_hash" fields in
      let facts_hash = cert_text "facts_hash" fields in
      let expected = Digestif.SHA256.(digest_string raw_code |> to_hex) in
      if schema <> Some "aml_bytecode_certificate_v2" then
        Error "program certificate schema mismatch"
      else if declaration <> Some "program" then
        Error "program certificate declaration mismatch"
      else if bytecode_hash <> Some expected then
        Error "program certificate bytecode mismatch"
      else if not (valid_provenance fields) then
        Error "program certificate provenance mismatch"
      else
        (match if attested then Program_attestation.verify ~trusted raw else Ok () with
         | Error error -> Error ("program compiler attestation: " ^ Program_attestation.error_message error)
         | Ok () ->
           (match cert_facts fields with
            | None -> Error "program certificate facts mismatch"
            | Some facts ->
              (match cert_effects fields with
               | None -> Error "program certificate effects mismatch"
               | Some effects ->
                 (match Program_policy.verify code facts effects with
                  | Error error -> Error ("program effect policy: " ^ error)
                  | Ok () ->
                    if facts_hash <> Some (Program_type_flow.facts_hash facts) then
                      Error "program certificate facts hash mismatch"
                    else match cert_text "compiler_version" fields with
                    | Some version -> Ok (facts, version)
                    | None -> Error "program certificate provenance mismatch"))))
    | _ -> Error "program certificate must be an object"
  with
  | (Stack_overflow | Out_of_memory) as error -> raise error
  | _ -> Error "invalid program certificate"

let decode_program ?(trusted = []) ?(point_ops = false) raw =
  match Program_envelope.decode raw with
  | Error error -> Error (Decode_error (Program_envelope.error_message error))
  | Ok envelope ->
    match Decode.code ~active:point_ops envelope.code with
    | Error error -> Error (Decode_error error)
    | Ok code ->
      match
        verify_program_cert
          ~attested:true
          ~trusted
          envelope.code
          code
          envelope.cert
      with
      | Error error -> Error (Verify_error error)
      | Ok (facts, version) ->
        Result.map
          (fun program -> { program with compiler_version = Some version })
          (of_program ~point_ops ~facts code)

let decode_deploy ?(trusted = []) ?(point_ops = false) raw =
  if Program_envelope.is_program raw then decode_program ~trusted ~point_ops raw
  else decode ~point_ops raw

let decode_program_source ?(point_ops = false) raw =
  match Program_envelope.decode raw with
  | Error error -> Error (Decode_error (Program_envelope.error_message error))
  | Ok envelope ->
    match Decode.code ~active:point_ops envelope.code with
    | Error error -> Error (Decode_error error)
    | Ok code ->
      match
        verify_program_cert
          ~attested:false
          ~trusted:[]
          envelope.code
          code
          envelope.cert
      with
      | Error error -> Error (Verify_error error)
      | Ok (facts, version) ->
        Result.map
          (fun program -> { program with compiler_version = Some version })
          (of_program ~point_ops ~facts code)

let compiler_version program = program.compiler_version

let code admitted =
  Array.copy admitted.admitted_code

let effects admitted =
  admitted.admitted_effects

let profile admitted =
  admitted.profile

let check_standard ~point_ops admitted =
  match if point_ops then None else Opcode_policy.first_standard admitted.admitted_code with
  | None -> Ok ()
  | Some hit -> Error (Unsafe_error (Opcode_policy.standard_error_message hit))

let error_message = function
  | Decode_error message
  | Verify_error message
  | Unsafe_error message -> message