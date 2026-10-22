(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module Core = Octra_core
module Bill = Core.Compute_bill
module Anchor = Octra_bootstrap.Sync_anchor
module Record = Compute_record
module Protocol = Octra_consensus.Resource_compute_protocol
module Engine = Resource_compute_provider_engine
module Rpc = Resource_compute_provider_rpc

type origin = { proof : Anchor.origin; index : string }

let request (call : Protocol.call) =
  let value = call.request in
  Rpc.{memory_bytes = Option.get value.memory_bytes;
    guest_bytes = Option.get value.guest_bytes; fuel_limit = Option.get value.fuel_limit;
    request_id = Text.raw_to_hex (Octra_consensus.Resource_compute_certificate.request_id value);
    session_id = Text.raw_to_hex value.session_id; epoch_id = value.epoch_id;
    state_root = Text.raw_to_hex value.state_root; model_epoch = value.model_epoch;
    model_state_root = Text.raw_to_hex value.model_state_root; circle_id = value.circle_id;
    graph_root = Text.raw_to_hex value.graph_root; model_root = Text.raw_to_hex value.model_root;
    program_root = Text.raw_to_hex value.program_root; executor_root = Text.raw_to_hex value.executor_root;
    caller = value.caller; method_name = call.method_name; params_json = call.params_json;
    program_b64 = ""; program_runtime = ""; storage = []; max_output_bytes = value.max_output_bytes}

let decode raw =
  Result.bind (Bill.decode_request raw) (fun (fuel, encoded) ->
    try
      let records = List.map Record.decode encoded in
      let total = List.fold_left (fun total record ->
        let call = record.Record.call.request in
        if Result.is_error (Bill.resources call) then invalid_arg "compute payment resource limits";
        let fuel = Option.get call.fuel_limit in
        if fuel > Bill.max_fuel - total then invalid_arg "compute payment resource limits";
        (match record.validators with
        | Some validators when Octra_consensus.C_config.validator_set_hash validators = call.validator_set_root
            && Record.verify_node validators record.call.coordinator
              (Protocol.call_sign_bytes record.call) record.call.signature -> ()
        | _ -> invalid_arg "compute payment coordinator signature");
        total + fuel) 0 records in
      if total <> fuel then Error "compute payment work differs" else Ok records
    with Invalid_argument _ | Failure _ -> Error "compute payment records refused")

let verify ~lookup ~engine ~epoch ~funded raw =
  match decode raw with
  | Error reason -> Lwt.return_error (Bill.Rejected reason)
  | Ok [] -> Lwt.return_error (Bill.Rejected "compute payment empty")
  | Ok records ->
    let rec deposits unpaid paid = function
      | [] -> Lwt.return (if unpaid then Ok paid
          else Error (Bill.Rejected "compute payment has no unpaid work"))
      | record :: rest ->
        match Protocol.header record.Record.call with
        | Error reason -> Lwt.return_error (Bill.Rejected reason)
        | Ok header ->
          let* found = funded header in
          match found with
          | Error _ as error -> Lwt.return error
          | Ok (Bill.Due at) ->
            deposits true ((Octra_consensus.Resource_compute_certificate.request_id header.request, at) :: paid) rest
          | Ok (Bill.Paid at) ->
            deposits unpaid ((Octra_consensus.Resource_compute_certificate.request_id header.request, at) :: paid) rest in
    let* funding = deposits false [] records in
    match funding with
    | Error _ as error -> Lwt.return error
    | Ok paid ->
    let first = (List.hd records).Record.call.request in
    let epochs = first.model_epoch :: List.map (fun record -> record.Record.call.request.epoch_id) records
      |> List.sort_uniq Int64.compare in
    let rec read saved = function
      | [] -> Lwt.return_ok saved
      | height :: rest when height < epoch ->
        let* found = lookup height in
        (match found with
        | Error reason -> Lwt.return_error (Bill.Unavailable reason)
        | Ok origin -> read ((height, origin) :: saved) rest)
      | _ -> Lwt.return_error (Bill.Rejected "compute payment origin not finalized") in
    let* origins = read [] epochs in
    match origins with
    | Error _ as error -> Lwt.return error
    | Ok origins ->
      let origin height = (List.assoc height origins).proof in
      let at call = Option.value ~default:epoch (List.assoc_opt
        (Octra_consensus.Resource_compute_certificate.request_id call.Protocol.request) paid) in
      match Record.Session.verify ~at ~epoch ~model:(origin first.model_epoch)
          (List.map (fun record -> origin record.Record.call.request.epoch_id, record) records) with
      | Error reason -> Lwt.return_error (Bill.Rejected reason)
      | Ok session ->
        let deps, program_at, load_model = engine origins in
        let calls = List.map (fun call -> call, request call) (Record.Session.calls session) in
        let limits = Resource_compute_provider_config.{accelerator = Cpu; lanes = 1;
          memory_bytes = Bill.model_limit; max_request_bytes = Bill.max_wire;
          max_response_bytes = 1_048_576; timeout_seconds = 300} in
        let* result = Engine.replay_loaded ~limits ~deps ~program_at ~load_model calls in
        Lwt.return (match result with
        | Error (Core.Circle_wasm_host.Rejected reason) -> Error (Bill.Rejected reason)
        | Error (Core.Circle_wasm_host.Unavailable reason) -> Error (Bill.Unavailable reason)
        | Ok measured -> Record.Session.bills session measured
          |> Result.map_error (fun reason -> Bill.Rejected reason))

let make () =
  fun ~store ~epoch ~funded raw ->
    Lwt.catch (fun () ->
    match decode raw with
    | Error reason -> Lwt.return_error (Bill.Rejected reason)
    | Ok records ->
      let rec inputs saved = function
        | [] -> Lwt.return_ok (List.rev saved)
        | record :: rest ->
          match Protocol.header record.Record.call with
          | Error reason -> Lwt.return_error reason
          | Ok header ->
            let* found = Core.Compute_input.read store ~header in
            match found with
            | Error _ as error -> Lwt.return error
            | Ok (call, model) -> inputs ((header.request.chain_id, call, model) :: saved) rest in
      let* loaded = inputs [] records in
      match loaded with
      | Error reason -> Lwt.return_error (Bill.Rejected reason)
      | Ok loaded ->
      let roots = List.concat_map (fun (chain, call, model) -> [chain, model; chain, call]) loaded in
      let find height state = List.find_opt (fun (_, (root : Core.Compute_input.root)) ->
        root.epoch = height && Option.fold ~none:true ~some:((=) root.state) state) roots in
      let lookup height = Lwt.return (match find height None with
        | None -> Error "compute input epoch missing"
        | Some (_, root) -> Ok {proof = Anchor.from_input root; index = ""}) in
      let engine _ =
        let limits = Resource_compute_provider_config.{accelerator = Cpu; lanes = 1;
          memory_bytes = Bill.model_limit; max_request_bytes = Bill.max_wire;
          max_response_bytes = 1_048_576; timeout_seconds = 300} in
        let snapshot height state = match find height (Some (Text.hex_to_string state)) with
          | None -> Error "compute input snapshot missing"
          | Some (_, root) -> Ok (Core.Compute_input.snapshot store root) in
        let program_at ~epoch_id ~state_root ~circle_id =
          match snapshot epoch_id state_root with
          | Error reason -> Lwt.return_error (Core.Circle_wasm_host.Rejected reason)
          | Ok source ->
            let+ found = Core.Store_irmin.get_circle_program_at source circle_id in
            Result.map_error (fun reason -> Core.Circle_wasm_host.Rejected reason)
              (Result.map (fun (program : Core.Store_irmin.circle_program_snapshot) -> Engine.{circle_id; runtime = program.runtime;
                code_b64 = program.code_b64; code_hash = program.code_hash}) found) in
        let load_model ~cancel (request : Rpc.prepare) =
          match snapshot request.model_epoch request.model_state_root with
          | Error reason -> Lwt.return_error (Core.Circle_wasm_host.Rejected reason)
          | Ok source ->
            let module Cache = Resource_compute_model_cache in
            let snapshot, fetch_asset = Cache.store_source_of_snapshot
              ~state_root:request.model_state_root source in
            let+ loaded = Cache.load_checked ~cancel ~limits:(Engine.graph_limits limits.memory_bytes)
              ~snapshot ~root_circle_id:request.circle_id ~graph_root:request.graph_root
              ~model_root:request.model_root ~fetch_asset ~sink:Cache.native_sink in
            Result.bind loaded (fun loaded -> Result.map_error
              (fun reason -> Core.Circle_wasm_host.Rejected reason) (Engine.model_of_loaded request loaded)) in
        Engine.replay_deps ~chain_id:(List.hd records).Record.call.request.chain_id ~limits,
          program_at, load_model in
      verify ~lookup ~engine ~epoch ~funded raw)
        (function
        | (Lwt.Canceled | Out_of_memory | Stack_overflow) as error -> Lwt.fail error
        | error -> Lwt.return_error (Bill.Unavailable (Printexc.to_string error)))