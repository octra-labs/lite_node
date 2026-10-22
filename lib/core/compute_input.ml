(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module Store = Store_irmin
module Tree = Store.Store.Tree
module Wire = Octra_net.Oce1
module C = Octra_consensus.C_types
module Codec = Octra_consensus.C_codec
module Protocol = Octra_consensus.Resource_compute_protocol
module Certificate = Octra_consensus.Resource_compute_certificate
module Graph = Resource_compute_graph

type root = {
  chain_id : string;
  epoch : int64;
  state : string;
  validators : C.validator_set;
  tree : Store.Store.tree;
}

type plan = { call : root; model : root; session : string list; fee : Z.t }

let window = Int64.to_int Protocol.call_ttl + 1
let max_bytes = Int64.to_int Compute_bill.model_limit
let hex = Digestif.SHA256.(fun raw -> to_hex (of_raw_string raw))
let digest raw = Digestif.SHA256.(to_hex (digest_string raw))
let tree_hash tree = Irmin.Type.to_string Store.Store.Hash.t (Tree.hash tree)
let root_path epoch = ["compute"; "roots"; string_of_int (Int64.to_int (Int64.rem epoch (Int64.of_int window)))]
let input_path id = ["compute"; "inputs"; hex id]
let session_path (request : Certificate.request) =
  ["compute"; "models"; digest (Wire.encode (fun buffer ->
    Wire.put_string buffer request.chain_id;
    Wire.put_string buffer request.caller;
    Wire.put_hash32 buffer request.session_id))]

let graft store path tree =
  match store.Store.batch_tree with
  | None -> Lwt.fail_with "compute input requires epoch batch"
  | Some current ->
    let+ updated = Tree.add_tree current path tree in
    store.batch_tree <- Some updated

let save store path root =
  let raw = Wire.encode (fun buffer ->
    Wire.put_string buffer root.chain_id;
    Wire.put_u64 buffer root.epoch;
    Wire.put_hash32 buffer root.state;
    Wire.put_string buffer (Codec.encode_validator_set root.validators);
    Wire.put_string buffer (tree_hash root.tree)) in
  let* () = Store.write store (path @ ["meta"]) raw in
  graft store (path @ ["data"]) root.tree

let load store path =
  let* raw = Store.read store (path @ ["meta"]) in
  let* tree = Store.read_tree store (path @ ["data"]) in
  match raw, tree with
  | None, None -> Lwt.return_none
  | Some raw, tree ->
    let tree = Option.value tree ~default:(Tree.empty ()) in
    let chain_id, epoch, state, validators, hash = Wire.decode (fun cursor ->
      let chain_id = Wire.get_string_bounded ~max:128 cursor in
      let epoch = Wire.get_u64 cursor in
      let state = Wire.get_hash32 cursor in
      let validators = Wire.get_string_bounded ~max:1_000_000 cursor
        |> Codec.decode_validator_set in
      let hash = Wire.get_string_bounded ~max:128 cursor in
      chain_id, epoch, state, validators, hash) raw in
    if chain_id = "" || epoch < 0L || validators.n = 0 || hash <> tree_hash tree then
      Lwt.fail_with "compute input state corrupt"
    else Lwt.return_some {chain_id; epoch; state; validators; tree}
  | _ -> Lwt.fail_with "compute input state incomplete"

let snapshot (store : Store.t) (root : root) = Store.{read_repo = store.repo; tree = root.tree;
  state_root = hex root.state; commit_hash = ""; epoch_id = root.epoch}

let remember store ~epoch ~state (parent : C.parent_commit) =
  let cert = parent.certificate in
  let state = if String.length state = 64 then
    Digestif.SHA256.(to_raw_string (of_hex state)) else state in
  if epoch <= 0 || cert.epoch_id <> Int64.of_int (epoch - 1)
      || cert.header.proposed_state_root <> state then
    Lwt.fail_with "compute input parent differs"
  else
    let* captured = Store.capture_read_snapshot store in
    match captured with
    | Error reason -> Lwt.fail_with reason
    | Ok captured when captured.epoch_id <> cert.epoch_id ->
      Lwt.fail_with "compute input head epoch differs"
    | Ok captured ->
      let* circles = Tree.find_tree captured.tree ["circles"] in
      let* tree = match circles with
        | None -> Lwt.return (Tree.empty ())
        | Some circles -> Tree.add_tree (Tree.empty ()) ["circles"] circles in
      save store (root_path cert.epoch_id)
        {chain_id = cert.chain_id; epoch = cert.epoch_id; state; validators = parent.validator_set; tree}

let recent store epoch state =
  let+ root = load store (root_path epoch) in
  match root with
  | Some root when root.epoch = epoch && root.state = state -> Ok root
  | _ -> Error "compute input origin not retained"

let retained store epoch =
  let+ saved = load store (root_path epoch) in
  Option.bind saved (fun root -> if root.epoch = epoch then Some root else None)

let retained_set ?snapshot store epoch =
  let+ raw = Store.read ?snapshot store (root_path epoch @ ["meta"]) in
  Option.bind raw (fun raw ->
    let chain_id, saved, validators = Wire.decode (fun cursor ->
      let chain_id = Wire.get_string_bounded ~max:128 cursor in
      let saved = Wire.get_u64 cursor in
      ignore (Wire.get_hash32 cursor);
      let validators = Wire.get_string_bounded ~max:1_000_000 cursor
        |> Codec.decode_validator_set in
      ignore (Wire.get_string_bounded ~max:128 cursor);
      chain_id, saved, validators) raw in
    if chain_id = "" || saved < 0L || validators.n = 0 then
      failwith "compute input state corrupt"
    else if saved <> epoch then None
    else Some (saved, chain_id, validators))

let pin_fee bytes entries = Z.of_int (10_000 + (bytes + 15) / 16 + 32 * entries)

let prepare store ~max_fee ~(header : Protocol.header) =
  let request = header.request in
  if request.sequence < 1L || request.sequence > 64L then
    Lwt.return_error "compute input session length" else
  let* found = recent store request.epoch_id request.state_root in
  match found with
  | Error _ as error -> Lwt.return error
  | Ok call when call.chain_id <> request.chain_id
      || Octra_consensus.C_config.validator_set_hash call.validators
      <> request.validator_set_root -> Lwt.return_error "compute input validators differ"
  | Ok call when not (Option.fold ~none:false ~some:(fun key ->
      Octra_consensus.C_hash.verify_ed25519 ~pubkey_raw:key
        ~msg:(Protocol.header_sign header) ~signature:header.signature)
        (C.pubkey_of_addr call.validators header.coordinator)) ->
    Lwt.return_error "compute input coordinator signature refused"
  | Ok call ->
    let session = session_path request in
    let id = Certificate.request_id request in
    let* raw = Store.read store (session @ ["uses"]) in
    let ids = match raw with None -> [] | Some raw ->
      Wire.decode (Wire.get_list_bounded ~max:64 Wire.get_hash32) raw in
    if List.mem id ids || List.length ids >= 64 then
      Lwt.return_error "compute input session capacity" else
    let* saved = load store session in
    let* model = match saved with
      | Some model when model.chain_id = request.chain_id
          && model.epoch = request.model_epoch && model.state = request.model_state_root ->
        Lwt.return_ok model
      | Some _ -> Lwt.return_error "compute input session model differs"
      | None -> recent store request.model_epoch request.model_state_root in
    match model with
    | Error _ as error -> Lwt.return error
    | Ok model when model.chain_id <> request.chain_id -> Lwt.return_error "compute input model chain differs"
    | Ok model ->
      let bytes = ref 0 in
      let entries = ref 0 in
      let add tree source path =
        let* value = Store.read_snapshot (snapshot store source) path in
        match value with
        | None -> Lwt.return_error "compute input value absent"
        | Some value ->
          if String.length value > 2 * max_bytes - !bytes || !entries >= 8192 then
            Lwt.return_error "compute input size exceeds limit"
          else begin
            bytes := !bytes + String.length value;
            incr entries;
            if Z.gt (pin_fee !bytes !entries) max_fee then
              Lwt.return_error "compute input fee exceeds budget"
            else Lwt.map Result.ok (Tree.add tree path value)
          end in
      let rec copy tree source = function
        | [] -> Lwt.return_ok tree
        | path :: paths ->
          let* next = add tree source path in
          match next with Error _ as error -> Lwt.return error
          | Ok next -> copy next source paths in
      let program tree source =
        let* found = Store.get_circle_program_at (snapshot store source) request.circle_id in
        match found with
        | Error _ as error -> Lwt.return error
        | Ok program when program.runtime <> "wasm_v1" || program.privacy_class <> "public"
            || program.resource_mode <> "public_resources"
            || program.code_hash <> hex request.program_root
            || String.length program.code_b64 > max_bytes ->
          Lwt.return_error "compute input program differs"
        | Ok program ->
          (match Base64.decode program.code_b64 with
          | Error _ -> Lwt.return_error "compute input program encoding"
          | Ok code when digest code <> program.code_hash ->
            Lwt.return_error "compute input program digest differs"
          | Ok _ -> copy tree source (List.map (fun path -> ["circles"; request.circle_id] @ path)
              [["runtime"]; ["code_hash"]; ["privacy"; "class"];
               ["resources"; "mode"]; ["program"; "code_b64"]])) in
      let asset tree source circle path limit =
        let* found = Store.get_circle_public_asset_at ~max_bytes:limit (snapshot store source) circle path in
        match found with
        | Error _ as error -> Lwt.return error
        | Ok asset when asset.browser_mode <> "gateway_allowed" || asset.resource_mode <> "public_resources"
            || asset.meta.encoding <> "identity" -> Lwt.return_error "compute input asset mode differs"
        | Ok asset ->
          let base = ["circles"; circle; "assets"; "by_hash"; asset.path_key] in
          let* copied = copy tree source [["circles"; circle; "browser"; "mode"];
            ["circles"; circle; "resources"; "mode"]; base @ ["meta"]; base @ ["body_b64"]] in
          (match copied with Error _ as error -> Lwt.return error
          | Ok copied ->
            match Base64.decode asset.body_b64 with
            | Error _ -> Lwt.return_error "compute input asset encoding"
            | Ok body when String.length body <> Int64.to_int asset.meta.size_bytes
                || digest body <> asset.meta.blob_hash ->
              Lwt.return_error "compute input asset digest differs"
            | Ok body -> Lwt.return_ok (copied, body)) in
      let* call_tree = program (Tree.empty ()) call in
      let* model_tree = program (Tree.empty ()) model in
      match call_tree, model_tree with
      | Error reason, _ | _, Error reason -> Lwt.return_error reason
      | Ok call_tree, Ok model_tree ->
        let limits = {Graph.default_limits with max_model_bytes = Int64.div Compute_bill.model_limit 2L;
          max_cache_bytes = Int64.div Compute_bill.model_limit 2L} in
        let* graph = asset model_tree model request.circle_id "/model/resource-graph.json" limits.max_graph_bytes in
        match graph with
        | Error _ as error -> Lwt.return error
        | Ok (model_tree, raw) ->
          match Graph.parse ~limits ~root_circle_id:request.circle_id raw with
          | Error _ as error -> Lwt.return error
          | Ok graph when graph.graph_root <> hex request.graph_root || graph.model.root <> hex request.model_root ->
            Lwt.return_error "compute input graph differs"
          | Ok graph ->
            let rec shards tree = function
              | [] -> Lwt.return_ok tree
              | (shard : Graph.shard) :: rest ->
                let* manifest = asset tree model shard.circle_id "/model/shard.json" limits.max_graph_bytes in
                match manifest with Error _ as error -> Lwt.return error
                | Ok (tree, raw) ->
                  let checked = try Graph.stable_json (Json_tree.read_plain raw)
                      = Graph.stable_json (Graph.shard_manifest_json graph shard)
                    with Yojson.Json_error _ -> false in
                  if not checked then Lwt.return_error "compute input shard differs" else
                  let rec segments tree = function
                    | [] -> shards tree rest
                    | (segment : Graph.segment) :: rest ->
                      let* found = asset tree model shard.circle_id segment.path limits.max_segment_bytes in
                      match found with Error _ as error -> Lwt.return error
                      | Ok (tree, body) when String.length body = segment.bytes && digest body = segment.sha256 ->
                        segments tree rest
                      | Ok _ -> Lwt.return_error "compute input segment differs" in
                  segments tree shard.segments in
            let+ model_tree = shards model_tree graph.shards in
            Result.map (fun tree -> {call = {call with tree = call_tree};
              model = {model with tree}; session; fee = pin_fee !bytes !entries}) model_tree

let commit store id plan =
  let* raw = Store.read store (plan.session @ ["uses"]) in
  let ids = match raw with None -> [] | Some raw ->
    Wire.decode (Wire.get_list_bounded ~max:64 Wire.get_hash32) raw in
  if List.mem id ids || List.length ids >= 64 then Lwt.fail_with "compute input session capacity" else
  let* () = Store.write store (plan.session @ ["uses"])
    (Wire.encode (fun buffer -> Wire.put_list Wire.put_hash32 buffer (id :: ids))) in
  let* () = Store.write store (input_path id @ ["session"]) (List.hd (List.rev plan.session)) in
  let* () = save store (input_path id @ ["call"]) plan.call in
  let* () = save store (input_path id @ ["model"]) plan.model in
  save store plan.session plan.model

let release store id =
  let* key = Store.read store (input_path id @ ["session"]) in
  match key with
  | None -> Lwt.return_unit
  | Some key when String.length key <> 64 -> Lwt.fail_with "compute input session key corrupt"
  | Some key ->
    let session = ["compute"; "models"; key] in
    let* raw = Store.read store (session @ ["uses"]) in
    let ids = match raw with None -> failwith "compute input references missing" | Some raw ->
      Wire.decode (Wire.get_list_bounded ~max:64 Wire.get_hash32) raw in
    if not (List.mem id ids) then Lwt.fail_with "compute input reference missing" else
    let ids = List.filter ((<>) id) ids in
    let* () = Store.remove_path store (input_path id) in
    if ids = [] then Store.remove_path store session else
      Store.write store (session @ ["uses"])
        (Wire.encode (fun buffer -> Wire.put_list Wire.put_hash32 buffer ids))

let read store ~(header : Protocol.header) =
  let request = header.request in
  let base = input_path (Certificate.request_id request) in
  let* call = load store (base @ ["call"]) in
  let+ model = load store (base @ ["model"]) in
  match call, model with
  | Some call, Some model when call.chain_id = request.chain_id && model.chain_id = request.chain_id
      && call.epoch = request.epoch_id && call.state = request.state_root
      && model.epoch = request.model_epoch && model.state = request.model_state_root
      && Octra_consensus.C_config.validator_set_hash call.validators = request.validator_set_root ->
    Ok (call, model)
  | _ -> Error "compute input not pinned"