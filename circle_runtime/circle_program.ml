(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


open Lwt.Syntax

type method_info = {
  name : string;
  view : bool;
}

type descriptor = {
  circle_id : string;
  info : Octra_core.Circles.circle_info;
  has_code : bool;
  code_hash : string;
  code_b64 : string option;
  code_bytes : int;
  methods : method_info list;
}

type octb = {
  bytecode : Octra_vm.Contract_vm.instr array;
  profile : Octra_vm.Admission.profile;
}

type code =
  | Octb of octb
  | Wasm_v1 of {
      code_b64 : string;
      code_raw : string;
      exports : Octra_core.Circle_wasm_host.export_info list;
      methods : method_info list;
    }

type loaded = {
  circle_id : string;
  info : Octra_core.Circles.circle_info;
  code : code;
}

type loaded_cache_entry = {
  loaded : loaded;
  updated_at : float;
}

let loaded_cache : (string, loaded_cache_entry) Hashtbl.t =
  Hashtbl.create 16

let loaded_cache_ttl_secs = 300.0
let loaded_cache_limit = 64

let loaded_cache_key circle_id (info : Octra_core.Circles.circle_info) =
  circle_id ^ ":" ^ Int64.to_string info.version ^ ":" ^ info.code_hash ^ ":" ^ info.stable_root

let prune_loaded_cache () =
  let now = Unix.gettimeofday () in
  let stale = ref [] in
  Hashtbl.iter
    (fun key entry ->
      if now -. entry.updated_at > loaded_cache_ttl_secs then
        stale := key :: !stale)
    loaded_cache;
  List.iter (fun key -> Hashtbl.remove loaded_cache key) !stale;
  if Hashtbl.length loaded_cache > loaded_cache_limit then
    Hashtbl.reset loaded_cache

let decode_bytecode ?(trusted = []) code_b64 =
  try
    match Octra_vm.Admission.decode_deploy ~trusted (Base64.decode_exn code_b64) with
    | Ok admitted ->
      Ok {
        bytecode = Octra_vm.Admission.code admitted;
        profile = Octra_vm.Admission.profile admitted;
      }
    | Error e -> Error (Octra_vm.Admission.error_message e)
  with e ->
    Error (Printexc.to_string e)

let decode_descriptor_bytecode code_b64 =
  try
    let raw = Base64.decode_exn code_b64 in
    if Octra_vm.Program_envelope.is_program raw then
      match Octra_vm.Admission.decode_program_source raw with
      | Ok admitted ->
        Ok {
          bytecode = Octra_vm.Admission.code admitted;
          profile = Octra_vm.Admission.profile admitted;
        }
      | Error e -> Error (Octra_vm.Admission.error_message e)
    else
      decode_bytecode code_b64
  with e ->
    Error (Printexc.to_string e)

let methods_of_bytecode bytecode =
  Octra_vm.Contract.extract_methods bytecode
  |> List.map (fun (name, view) -> { name; view })

let methods_of_wasm_descriptor descriptor =
  let manifest_methods =
    match (descriptor : Octra_core.Circle_wasm_host.descriptor).manifest with
    | Some (`Assoc fields) ->
      begin
        match List.assoc_opt "methods" fields with
        | Some (`List entries) ->
          let parse_entry = function
            | `Assoc entry_fields ->
              begin
                match List.assoc_opt "name" entry_fields, List.assoc_opt "view" entry_fields with
                | Some (`String name), Some (`Bool view) ->
                  Some { name; view }
                | _ ->
                  None
              end
            | _ ->
              None
          in
          List.filter_map parse_entry entries
        | _ ->
          []
      end
    | _ ->
      []
  in
  if manifest_methods <> [] then
    manifest_methods
  else
    let names =
      (descriptor : Octra_core.Circle_wasm_host.descriptor).exports
      |> List.filter (fun (export : Octra_core.Circle_wasm_host.export_info) -> String.equal export.kind "function")
      |> List.map (fun (export : Octra_core.Circle_wasm_host.export_info) -> export.name) in
    let methods = ref [] in
    if List.mem "octra_query" names then
      methods := { name = "octra_query"; view = true } :: !methods;
    if List.mem "octra_update" names then
      methods := { name = "octra_update"; view = false } :: !methods;
    if List.mem "octra_http_request" names then
      methods := { name = "octra_http_request"; view = true } :: !methods;
    List.rev !methods

let yojson_of_method_info t =
  `Assoc [
    "name", `String t.name;
    "view", `Bool t.view;
  ]

let yojson_of_descriptor (t : descriptor) =
  `Assoc [
    "circle_id", `String t.circle_id;
    "runtime", `String (Octra_core.Circles.string_of_runtime t.info.runtime);
    "version", `String (Int64.to_string t.info.version);
    "owner", `String t.info.owner;
    "code_hash", `String t.code_hash;
    "has_program", `Bool t.has_code;
    "code_bytes", `Int t.code_bytes;
    "code_b64", `Null;
    "methods", `List (List.map yojson_of_method_info t.methods);
    "stable_root", `String t.info.stable_root;
    "assets_root", `String t.info.assets_root;
    "privacy_class", `String (Octra_core.Circles.string_of_privacy_class t.info.privacy_class);
    "browser_mode", `String (Octra_core.Circles.string_of_browser_mode t.info.browser_mode);
    "resource_mode", `String (Octra_core.Circles.string_of_resource_mode t.info.resource_mode);
    "limits", Octra_core.Circles.yojson_of_limits t.info.limits;
  ]

let describe store circle_id =
  let* info_opt = Octra_core.Store_irmin.get_circle_info store circle_id in
  match info_opt with
  | None -> Lwt.return (Error "circle not found")
  | Some info ->
    let* code_opt = Octra_core.Store_irmin.get_circle_program_code_b64 store circle_id in
    begin
      match code_opt with
      | None ->
        Lwt.return (Ok {
          circle_id;
          info;
          has_code = false;
          code_hash = info.code_hash;
          code_b64 = None;
          code_bytes = 0;
          methods = [];
        })
      | Some code_b64 ->
        begin
          try
            let raw_code = Base64.decode_exn code_b64 in
            let actual_code_hash = Octra_core.Circles.sha256_hex raw_code in
            if actual_code_hash <> info.code_hash then
              Lwt.return (Error "circle code hash mismatch")
            else
              match info.runtime with
              | Octra_core.Circles.Octb ->
                begin
                  match decode_descriptor_bytecode code_b64 with
                  | Ok admitted ->
                    Lwt.return (Ok {
                      circle_id;
                      info;
                      has_code = true;
                      code_hash = info.code_hash;
                      code_b64 = None;
                      code_bytes = String.length raw_code;
                      methods = methods_of_bytecode admitted.bytecode;
                    })
                  | Error e ->
                    Lwt.return (Error ("invalid circle bytecode: " ^ e))
                end
              | Octra_core.Circles.Wasm_v1 ->
                let* wasm_descriptor_result =
                  Octra_core.Circle_wasm_host.describe code_b64 in
                begin
                  match wasm_descriptor_result with
                  | Error e ->
                    Lwt.return (Error ("invalid circle wasm: " ^ e))
                  | Ok wasm_descriptor ->
                    Lwt.return (Ok {
                      circle_id;
                      info;
                      has_code = true;
                      code_hash = info.code_hash;
                      code_b64 = None;
                      code_bytes = String.length raw_code;
                      methods = methods_of_wasm_descriptor wasm_descriptor;
                    })
                end
          with e ->
            Lwt.return (Error ("invalid circle code: " ^ Printexc.to_string e))
        end
    end

let load ?(trusted = []) store circle_id =
  let* info_opt = Octra_core.Store_irmin.get_circle_info store circle_id in
  match info_opt with
  | None -> Lwt.return (Error "circle not found")
  | Some info ->
    prune_loaded_cache ();
    let cache_key = loaded_cache_key circle_id info in
    begin
      match Hashtbl.find_opt loaded_cache cache_key with
      | Some entry ->
        Lwt.return (Ok entry.loaded)
      | None ->
    let* code_opt = Octra_core.Store_irmin.get_circle_program_code_b64 store circle_id in
    begin
      match code_opt with
      | None -> Lwt.return (Error "circle program not found")
      | Some code_b64 ->
        begin
          try
            let raw_code = Base64.decode_exn code_b64 in
            let actual_code_hash = Octra_core.Circles.sha256_hex raw_code in
            if actual_code_hash <> info.code_hash then
              Lwt.return (Error "circle code hash mismatch")
            else
              match info.runtime with
              | Octra_core.Circles.Octb ->
                begin
                  match decode_bytecode ~trusted code_b64 with
                  | Ok admitted ->
                    Lwt.return (Ok {
                      circle_id;
                      info;
                      code = Octb admitted;
                    })
                  | Error e ->
                    Lwt.return (Error ("invalid circle bytecode: " ^ e))
                end
              | Octra_core.Circles.Wasm_v1 ->
                let* wasm_descriptor_result =
                  Octra_core.Circle_wasm_host.describe code_b64 in
                begin
                  match wasm_descriptor_result with
                  | Error e ->
                    Lwt.return (Error ("invalid circle wasm: " ^ e))
                  | Ok wasm_descriptor ->
                    let loaded = {
                      circle_id;
                      info;
                      code =
                        Wasm_v1 {
                          code_b64;
                          code_raw = raw_code;
                          exports = wasm_descriptor.exports;
                          methods = methods_of_wasm_descriptor wasm_descriptor;
                        };
                    } in
                    Hashtbl.replace
                      loaded_cache
                      cache_key
                      { loaded; updated_at = Unix.gettimeofday () };
                    Lwt.return (Ok loaded)
                end
          with e ->
            Lwt.return (Error ("invalid circle code: " ^ Printexc.to_string e))
        end
    end
    end