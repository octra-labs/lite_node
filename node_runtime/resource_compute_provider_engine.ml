(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module Cache = Resource_compute_model_cache
module Graph = Octra_core.Resource_compute_graph
module Provider_rpc = Resource_compute_provider_rpc
module Owner = Cache.Owner
module Native = Octra_core.Circle_wasm_native

type program = {
  circle_id : string;
  code_b64 : string;
  code_hash : string;
  runtime : string;
}

type model = Owner.model = {
  snapshots : (int64 * string) list;
  circle_id : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
  cache_key : string;
  cache_root : string;
  entries : int;
  bytes : int64;
}

type native_result = {
  output_json : string;
  trace_root : string;
  steps : int64;
  operations : int64;
  effort_used : int;
}

type deps = {
  program :
    epoch_id:int64 ->
    state_root:string ->
    circle_id:string ->
    (program, string) result Lwt.t;
  storage :
    epoch_id:int64 ->
    state_root:string ->
    circle_id:string ->
    ((string * string) list, string) result Lwt.t;
  load_model : cancel:string Lwt.t -> Provider_rpc.prepare -> (model, string) result Lwt.t;
  drop_model : string -> (unit, string) result Lwt.t;
  hold_model : model -> (Native.lease, string) result;
  self_test : unit -> (Provider_rpc.self_test, string) result Lwt.t;
  execute :
    model:model ->
    program:program ->
    storage:(string * string) list ->
    Provider_rpc.execute ->
    (native_result, string) result Lwt.t;
}

type _ action =
  | Manage : 'reply Owner.message -> 'reply action
  | Select : Provider_rpc.execute -> ((Owner.token * model * Native.lease), string) result action

type command = Command : {
  message : 'reply action;
  promise : 'reply Lwt.t;
  reply : 'reply Lwt.u;
  deadline : float option;
} -> command

type t = {
  limits : Resource_compute_provider_config.limits;
  deps : deps;
  mutable state : Owner.state;
  data : command Queue.t;
  control : command Queue.t;
  ready : unit Lwt_condition.t;
  space : unit Lwt_condition.t;
  mutable open_channel : bool;
  closing : unit Lwt.t;
  close_reply : unit Lwt.u;
  stopped : unit Lwt.t;
  stop_reply : unit Lwt.u;
}

let ( let*? ) value next =
  match value with
  | Ok value -> next value
  | Error _ as error -> error

let ( let*| ) promise next =
  let* result = promise in
  match result with
  | Ok value -> next value
  | Error reason -> Lwt.return (Error reason)

let lower_hex_64 value =
  String.length value = 64
  && String.for_all
       (function
        | '0' .. '9' | 'a' .. 'f' -> true
        | _ -> false)
       value

let program_digest code_b64 =
  match Base64.decode code_b64 with
  | Error _ -> Error "resource compute program base64 refused"
  | Ok code -> Ok Digestif.SHA256.(to_hex (digest_string code))

let response_json = function
  | Octra_core.Circle_wasm_codec.Resp_null -> `Null
  | Octra_core.Circle_wasm_codec.Resp_bool value -> `Bool value
  | Octra_core.Circle_wasm_codec.Resp_int value -> `Intlit value
  | Octra_core.Circle_wasm_codec.Resp_string value -> `String value

let self_test_of_json = function
  | Error _ as error -> error
  | Ok json -> Provider_rpc.self_test_of_json json

let host_result =
  Result.map_error Octra_core.Circle_wasm_host.error_message

let native_accelerator = function
  | Resource_compute_provider_config.Cpu -> Ok ()
  | accelerator ->
    Error ("resource compute accelerator unavailable: " ^
      Resource_compute_provider_config.accelerator_name accelerator)

let native_self_test ?(accelerator = Resource_compute_provider_config.Cpu) () =
  match native_accelerator accelerator with
  | Error _ as error -> Lwt.return error
  | Ok () ->
    let* result = Octra_core.Circle_wasm_host.compute_self_test () in
    Lwt.return (self_test_of_json (host_result result))

let graph_limits memory_bytes =
  let max_cache_bytes =
    Int64.min 536_870_912L (Int64.div memory_bytes 2L) in
  {
    Graph.default_limits with
    max_model_bytes = max_cache_bytes;
    max_cache_bytes;
  }

let model_of_loaded
    (request : Provider_rpc.prepare)
    (loaded : Cache.loaded) =
  let graph = loaded.Cache.graph in
  let cache = loaded.cache in
  if graph.Graph.graph_root <> request.graph_root then
    Error "resource compute graph root differs"
  else if graph.model.root <> request.model_root then
    Error "resource compute model root differs"
  else
    Ok {
      snapshots = [request.model_epoch, request.model_state_root];
      circle_id = request.circle_id;
      graph_root = request.graph_root;
      model_root = request.model_root;
      program_root = request.program_root;
      executor_root = request.executor_root;
      cache_key = cache.Graph.key;
      cache_root = cache.root;
      entries = List.length cache.entries;
      bytes = cache.bytes;
    }

let committed_snapshot state_root_at index_root_at ~epoch_id ~state_root =
  if not (Text.is_hex_len 64 state_root) then
    Error "resource compute snapshot root refused"
  else if state_root_at epoch_id <> Some (Text.hex_to_string state_root) then
    Error "resource compute snapshot root differs"
  else
    match index_root_at epoch_id with
    | None -> Error "resource compute snapshot index missing"
    | Some root -> Ok root

let native_program state_root_at index_root_at store ~epoch_id ~state_root ~circle_id =
  match committed_snapshot state_root_at index_root_at ~epoch_id ~state_root with
  | Error _ as error -> Lwt.return error
  | Ok index_root ->
  let* snapshot_result =
    Octra_core.Store_irmin.capture_read_snapshot_at
      ~index_root store ~epoch_id ~state_root in
  match snapshot_result with
  | Error _ as error -> Lwt.return error
  | Ok snapshot ->
    let* program_result =
      Octra_core.Store_irmin.get_circle_program_at snapshot circle_id in
    Lwt.return
      (match program_result with
       | Error _ as error -> error
       | Ok program
         when program.privacy_class <> "public"
           || program.resource_mode <> "public_resources" ->
         Error "resource compute circle is not public"
       | Ok program ->
         Ok {
           circle_id;
           code_b64 = program.code_b64;
           code_hash = program.code_hash;
           runtime = program.runtime;
         })

let program_checked state_root_at index_root_at store ~epoch_id ~state_root ~circle_id =
  let module Host = Octra_core.Circle_wasm_host in
  match committed_snapshot state_root_at index_root_at ~epoch_id ~state_root with
  | Error reason -> Lwt.return_error (Host.Unavailable reason)
  | Ok index_root ->
    let* captured = Octra_core.Store_irmin.capture_read_snapshot_at
      ~index_root store ~epoch_id ~state_root in
    match captured with
    | Error reason -> Lwt.return_error (Host.Unavailable reason)
    | Ok snapshot ->
      let* found = Octra_core.Store_irmin.get_circle_program_at snapshot circle_id in
      Lwt.return (match found with
      | Error reason -> Error (Host.Rejected reason)
      | Ok program when program.privacy_class <> "public"
          || program.resource_mode <> "public_resources" ->
        Error (Host.Rejected "resource compute circle is not public")
      | Ok program -> Ok {circle_id; code_b64 = program.code_b64;
          code_hash = program.code_hash; runtime = program.runtime})

let model_checked limits state_root_at index_root_at store ~cancel (request : Provider_rpc.prepare) =
  let module Host = Octra_core.Circle_wasm_host in
  match committed_snapshot state_root_at index_root_at
      ~epoch_id:request.model_epoch ~state_root:request.model_state_root with
  | Error reason -> Lwt.return_error (Host.Unavailable reason)
  | Ok index_root ->
    let* source = Cache.store_source_at store ~epoch_id:request.model_epoch
      ~state_root:request.model_state_root ~index_root in
    match source with
    | Error reason -> Lwt.return_error (Host.Unavailable reason)
    | Ok (snapshot, fetch_asset) ->
      let* loaded = Cache.load_checked ~cancel
        ~limits:(graph_limits limits.Resource_compute_provider_config.memory_bytes)
        ~snapshot ~root_circle_id:request.circle_id ~graph_root:request.graph_root
        ~model_root:request.model_root ~fetch_asset ~sink:Cache.native_sink in
      Lwt.return (Result.bind loaded (fun loaded ->
        Result.map_error (fun reason -> Host.Rejected reason) (model_of_loaded request loaded)))

let native_load_model limits state_root_at index_root_at store ~cancel (request : Provider_rpc.prepare) =
  match
    committed_snapshot
      state_root_at
      index_root_at
      ~epoch_id:request.model_epoch
      ~state_root:request.model_state_root
  with
  | Error _ as error -> Lwt.return error
  | Ok index_root ->
  let* source_result =
    Cache.store_source_at
      store
      ~epoch_id:request.model_epoch
      ~state_root:request.model_state_root
      ~index_root in
  match source_result with
  | Error _ as error -> Lwt.return error
  | Ok (snapshot, fetch_asset) ->
    if
      snapshot.Cache.epoch_id <> request.model_epoch
      || snapshot.state_root <> request.model_state_root
    then
      Lwt.return (Error "resource compute snapshot differs")
    else
      let* result =
        Cache.load
          ~cancel
          ~limits:(graph_limits limits.Resource_compute_provider_config.memory_bytes)
          ~snapshot
          ~root_circle_id:request.circle_id
          ~graph_root:request.graph_root
          ~model_root:request.model_root
          ~fetch_asset
          ~sink:Cache.native_sink in
      Lwt.return
        (match result with
         | Error _ as error -> error
         | Ok loaded -> model_of_loaded request loaded)

let native_drop_model key =
  let* result = Cache.native_drop key in
  Lwt.return (Result.map (fun _ -> ()) result)

let storage_table entries =
  let table = Hashtbl.create (max 1 (List.length entries)) in
  List.iter (fun (key, value) -> Hashtbl.add table key value) entries;
  table

let params_of_json raw =
  match Yojson.Safe.from_string raw with
  | `List params -> Ok params
  | _ -> Error "resource compute params list required"
  | exception _ -> Error "resource compute params JSON refused"

let session_scope (request : Provider_rpc.execute) =
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_session" (fun buffer ->
    Octra_net.Oce1.put_string buffer request.circle_id;
    Octra_net.Oce1.put_hash32 buffer (Text.hex_to_raw32_lossy request.model_root);
    Octra_net.Oce1.put_string buffer request.caller;
    Octra_net.Oce1.put_hash32 buffer (Text.hex_to_raw32_lossy request.session_id))
  |> Text.raw_to_hex

let code_mode chain epoch =
  let module Rule = Octra_core.Rule_graph in
  match Rule.proof_key_activation_for_chain chain with
  | Some plan when epoch >= Int64.of_int plan.activation_epoch -> Rule.Active
  | _ -> Rule.Prior

let native_run ~chain_id ~model ~program ~storage (request : Provider_rpc.execute) =
  match params_of_json request.params_json with
  | Error error -> Lwt.return_error (Octra_core.Circle_wasm_host.Rejected error)
  | Ok params ->
    begin
      match Octra_core.Circle_wasm_codec.encode_request ~method_name:request.method_name params with
      | Error error -> Lwt.return_error (Octra_core.Circle_wasm_host.Rejected error)
      | Ok request_bytes ->
        let* result =
          Octra_core.Circle_wasm_host.execute_compute_isolated_with_storage
            ~table_mode:Octra_core.Rule_graph.Active
            ~code_mode:(code_mode chain_id request.epoch_id)
            ~memory_bytes:request.memory_bytes
            ~guest_bytes:request.guest_bytes
            ~float_mode:Octra_core.Rule_graph.Prior
            ~keyed:false
            ~math:false
            ~compute_session_scope:(session_scope request)
            ~compute_storage_cache_key:(Some model.cache_key)
            ~code_b64:program.code_b64
            ~export_name:"octra_query"
            ~request_bytes
            ~storage_tbl:(storage_table storage)
            ~storage_cache_key:None
            ~caller:request.caller
            ~address:request.circle_id
            ~tx_hash:request.request_id
            ~current_epoch:(Int64.to_int request.epoch_id)
            ~hfhe_caps:[]
            ~hfhe_pubkeys:[]
            ~hfhe_active_key:None
            ~hfhe_strict:true
            ~hfhe_mode:Octra_core.Circle_hfhe_transcript.Direct
            ~public_reads:[]
            ~fuel_limit:request.fuel_limit in
        Lwt.return result
    end

let native_observation (result : Octra_core.Circle_wasm_host.exec_result) =
  let module Bill = Octra_core.Compute_bill in
  let observed output_json outcome trace_root steps operations =
    Ok ({output_json; trace_root; steps; operations; effort_used = result.effort_used}, outcome) in
  match result.compute_exit, result.compute_trace_root, result.compute_steps, result.compute_operations with
  | Some (-1), Some trace, Some steps, Some operations when result.compute_fault = Some "fuel"
      && not result.success && steps = Int64.of_int result.effort_used ->
    observed (Graph.stable_json (`Assoc ["compute_stop", `String "fuel"])) Bill.Exhausted
      trace steps operations
  | Some code, Some trace, Some steps, Some operations when code >= 0
      && result.success = (code = 0) && steps = Int64.of_int result.effort_used ->
    let output = if code = 0 then Option.map (fun value -> Graph.stable_json (response_json value))
        result.response_value
      else Some (Graph.stable_json (`Assoc ["compute_stop", `Int code])) in
    (match output with
    | None -> Error "compute measurement output missing"
    | Some output -> observed output (if code = 0 then Bill.Complete else Bill.Stopped code)
        trace steps operations)
  | _ -> Error "compute measurement has no payable outcome"

let native_execute ~chain_id ~model ~program ~storage request =
  let* result = native_run ~chain_id ~model ~program ~storage request in
  Lwt.return (match result with
    | Error error -> Error (Octra_core.Circle_wasm_host.error_message error)
    | Ok result -> Result.map fst (native_observation result))

let verify_program expected_root (program : program) =
  let*? actual_root = program_digest program.code_b64 in
  if program.code_hash <> expected_root || actual_root <> expected_root || program.runtime <> "wasm_v1" then
    Error "resource compute program root differs"
  else Ok ()

let check_measure ~(model : model) ~(program : program)
    ~(call : Octra_consensus.Resource_compute_protocol.call)
    (request : Provider_rpc.execute) =
  let module Protocol = Octra_consensus.Resource_compute_protocol in
  let same raw hex = Text.raw_to_hex raw = hex in
  if not (Octra_consensus.Resource_compute_certificate.request_shape call.request
      && same (Octra_consensus.Resource_compute_certificate.request_id call.request) request.request_id
      && same call.request.state_root request.state_root
      && same call.request.model_state_root request.model_state_root
      && call.request.epoch_id = request.epoch_id && call.request.model_epoch = request.model_epoch
      && call.request.caller = request.caller && call.request.circle_id = request.circle_id
      && call.method_name = request.method_name && call.params_json = request.params_json
      && call.request.input_hash = Protocol.input_hash ~method_name:call.method_name ~params_json:call.params_json
      && call.request.input_bytes = String.length call.params_json
      && same call.request.session_id request.session_id
      && same call.request.graph_root request.graph_root
      && same call.request.model_root request.model_root
      && same call.request.program_root request.program_root
      && same call.request.executor_root request.executor_root
      && call.request.fuel_limit = Some request.fuel_limit
      && call.request.memory_bytes = Some request.memory_bytes
      && call.request.guest_bytes = Some request.guest_bytes
      && call.request.max_output_bytes = request.max_output_bytes) then
    Error "compute measurement scope differs"
  else if model.circle_id <> request.circle_id || program.circle_id <> request.circle_id
      || model.graph_root <> request.graph_root || model.model_root <> request.model_root
      || model.program_root <> request.program_root || model.executor_root <> request.executor_root
      || not (List.mem (request.model_epoch, request.model_state_root) model.snapshots) then
    Error "compute measurement model differs"
  else verify_program request.program_root program

let measure ~model ~program ~storage ~call request =
  let module Bill = Octra_core.Compute_bill in
  match check_measure ~model ~program ~call request with
  | Error _ as error -> Lwt.return error
  | Ok () ->
    let* result = native_run ~chain_id:call.request.chain_id ~model ~program ~storage request in
    Lwt.return (match result with
    | Error error -> Error (Octra_core.Circle_wasm_host.error_message error)
    | Ok result ->
      Result.bind (native_observation result) (fun (result, outcome) ->
        Bill.measure ~request:call.request ~output:result.output_json
          ~trace_root:(Text.hex_to_string result.trace_root)
          ~steps:result.steps ~operations:result.operations ~outcome))

let replay ~model ~program calls =
  let module Host = Octra_core.Circle_wasm_host in
  let module Protocol = Octra_consensus.Resource_compute_protocol in
  let module Bill = Octra_core.Compute_bill in
  let rec check index first chain epoch saved = function
    | [] -> Ok (List.rev saved)
    | ((call : Protocol.call), (request : Provider_rpc.execute)) :: rest ->
      let*? () = check_measure ~model ~program ~call request in
      if call.request.sequence <> Int64.of_int index then
        Error "compute replay sequence differs"
      else if request.caller <> first.Provider_rpc.caller
          || request.session_id <> first.session_id
          || call.request.chain_id <> chain || request.epoch_id < epoch then
        Error "compute replay session differs"
      else
        let*? params = params_of_json request.params_json in
        let*? request_bytes = Octra_core.Circle_wasm_codec.encode_request
          ~method_name:request.method_name params in
        let step = Host.{request_bytes; tx_hash = request.request_id;
          epoch = Int64.to_int request.epoch_id; fuel = request.fuel_limit;
          memory = request.memory_bytes; guest = request.guest_bytes} in
        check (index + 1) first chain request.epoch_id (step :: saved) rest in
  match calls with
  | [] -> Lwt.return_error (Host.Rejected "compute replay empty")
  | _ when List.length calls > 64 -> Lwt.return_error (Host.Rejected "compute replay step count")
  | (call, first) :: _ ->
    match check 1 first call.Protocol.request.chain_id first.epoch_id [] calls with
    | Error reason -> Lwt.return_error (Host.Rejected reason)
    | Ok steps ->
      match Cache.native_hold model with
      | Error reason -> Lwt.return_error (Host.Unavailable reason)
      | Ok lease -> Lwt.finalize (fun () ->
      let* result = Host.replay_compute ~code_b64:program.code_b64
        ~table_mode:Octra_core.Rule_graph.Active
        ~code_mode:(fun epoch -> code_mode call.request.chain_id (Int64.of_int epoch))
        ~cache_key:model.cache_key ~root:model.cache_root ~entries:model.entries ~bytes:model.bytes
        ~scope:(session_scope first)
        ~caller:first.caller ~address:first.circle_id steps in
      Lwt.return (match result with
        | Error _ as error -> error
        | Ok results ->
          let rec measure saved calls results = match calls, results with
            | [], [] -> Ok (List.rev saved)
            | (call, _) :: rest, result :: remaining ->
              let*? result, outcome = native_observation result in
              let*? measured = Bill.measure ~request:call.Protocol.request
                ~output:result.output_json ~trace_root:(Text.hex_to_string result.trace_root)
                ~steps:result.steps ~operations:result.operations ~outcome in
              measure (measured :: saved) rest remaining
            | _ -> Error "compute replay result count" in
          measure [] calls results |> Result.map_error (fun reason -> Host.Rejected reason)))
        (fun () -> Native.lease_close lease; Lwt.return_unit)

let native_deps ~chain_id ~limits ~store ~state_root_at ~index_root_at =
  {
    program = native_program state_root_at index_root_at store;
    storage = (fun ~epoch_id ~state_root ~circle_id ->
      let* snapshot_result =
          match committed_snapshot state_root_at index_root_at ~epoch_id ~state_root with
          | Error _ as error -> Lwt.return error
          | Ok index_root ->
            Octra_core.Store_irmin.capture_read_snapshot_at
              ~index_root store ~epoch_id ~state_root in
      let* result =
        match snapshot_result with
        | Error _ as error -> Lwt.return error
        | Ok snapshot ->
          Octra_core.Store_irmin.load_circle_stable_storage_at
            ~limits:(2048, 2 * 1024 * 1024)
            snapshot
            circle_id in
      Lwt.return
        (Result.map_error
          (function
           | "circle storage entries exceed limit" ->
             "resource compute storage entries exceed limit"
           | "circle storage bytes exceed limit" ->
             "resource compute storage exceeds limit"
           | error -> error)
          result));
    load_model = native_load_model limits state_root_at index_root_at store;
    drop_model = native_drop_model;
    hold_model = Cache.native_hold;
    self_test = native_self_test ~accelerator:limits.accelerator;
    execute = (fun ~model ~program ~storage request ->
      match native_accelerator limits.accelerator with
      | Error _ as error -> Lwt.return error
      | Ok () -> native_execute ~chain_id ~model ~program ~storage request);
  }

let replay_deps ~chain_id ~(limits : Resource_compute_provider_config.limits) = {
  program = (fun ~epoch_id:_ ~state_root:_ ~circle_id:_ -> Lwt.return_error "compute input program required");
  storage = (fun ~epoch_id:_ ~state_root:_ ~circle_id:_ -> Lwt.return_error "compute replay storage is session-owned");
  load_model = (fun ~cancel:_ _ -> Lwt.return_error "compute input model required");
  drop_model = native_drop_model;
  hold_model = Cache.native_hold;
  self_test = native_self_test ~accelerator:limits.accelerator;
  execute = (fun ~model ~program ~storage request -> native_execute ~chain_id ~model ~program ~storage request);
}

let reject error (Command command) =
  if Lwt.is_sleeping command.promise then Lwt.wakeup_later_exn command.reply error

let drain queue error =
  Queue.iter (reject error) queue;
  Queue.clear queue

let close_channel t error =
  t.open_channel <- false;
  drain t.data error;
  drain t.control error;
  Lwt_condition.broadcast t.ready ();
  Lwt_condition.broadcast t.space ()

let hold_model t model =
  try t.deps.hold_model model with
  | Failure reason -> Error reason
  | error -> Error ("resource compute cache hold failed: " ^ Printexc.to_string error)

let transition : type reply. t -> reply action -> reply = fun t action ->
  match action with
  | Manage (Owner.Remember (token, model)) ->
    let state, response = Owner.delta t.state (Owner.Remember (token, model)) in
    begin match response with
    | Error _ as error -> error
    | Ok () ->
      begin match hold_model t model with
      | Error _ as error -> error
      | Ok lease ->
        t.state <- state;
        Native.lease_close lease;
        Ok ()
      end
    end
  | Manage (Owner.Reserve request) ->
    let state, response = Owner.delta t.state (Owner.Reserve request) in
    t.state <- state;
    begin match response with
    | Ok (Owner.Ready model) ->
      begin match hold_model t model with
      | Ok lease -> Native.lease_close lease; response
      | Error ("compute storage cache missing" | "compute storage cache identity differs") as error ->
        let state, () = Owner.delta t.state (Owner.Lost model) in
        let state, response = Owner.delta state (Owner.Reserve request) in
        t.state <- state;
        begin match response with
        | Ok (Owner.Ready _) -> error
        | _ -> response
        end
      | Error _ as error -> error
      end
    | _ -> response
    end
  | Manage message ->
    let state, response = Owner.delta t.state message in
    t.state <- state;
    response
  | Select request ->
    let state, response = Owner.delta t.state (Owner.Acquire request) in
    t.state <- state;
    begin match response with
    | Error _ as error -> error
    | Ok (token, model) ->
      let held = hold_model t model in
      begin match held with
      | Ok lease -> Ok (token, model, lease)
      | Error reason ->
        let state = match reason with
          | "compute storage cache missing" | "compute storage cache identity differs" ->
            fst (Owner.delta t.state (Owner.Forget token))
          | _ -> t.state in
        let state, () = Owner.delta state (Owner.Release (token, false)) in
        t.state <- state;
        Error reason
      end
    end

let dispatch t control (Command command) =
  if control || Lwt.is_sleeping command.promise then
    match command.deadline with
    | Some deadline when Unix.gettimeofday () >= deadline ->
      reject (Failure "resource compute request expired") (Command command)
    | _ ->
      let response = transition t command.message in
      if t.state.closed && Lwt.is_sleeping t.closing then Lwt.wakeup_later t.close_reply ();
      if Lwt.is_sleeping command.promise then Lwt.wakeup_later command.reply response

let rec owner_loop t =
  if t.state.closed && t.state.loading = None && t.state.leases = [] then
    let models = List.sort_uniq
      (fun left right -> String.compare left.cache_key right.cache_key) t.state.models in
    close_channel t (Failure "resource compute provider stopped");
    let* () = Lwt_list.iter_s (fun model ->
      let* result = t.deps.drop_model model.cache_key in
      match result with
      | Ok () -> Lwt.return_unit
      | Error reason -> Lwt.fail (Failure reason)) models in
    if Lwt.is_sleeping t.stopped then Lwt.wakeup_later t.stop_reply ();
    Lwt.return_unit
  else
    let selected = match Queue.take_opt t.control with
      | Some command -> Some (true, command)
      | None -> Option.map (fun command -> false, command) (Queue.take_opt t.data) in
    match selected with
    | None ->
      let* () = Lwt_condition.wait t.ready in
      owner_loop t
    | Some (control, command) ->
      Lwt_condition.broadcast t.space ();
      dispatch t control command;
      let* () = Lwt.pause () in
      owner_loop t

let actor_call : type reply. t -> reply action -> reply Lwt.t = fun t message ->
  let control = match message with
    | Select _ -> false
    | Manage (Owner.Reserve _) -> false
    | Manage (Owner.Acquire _) -> false
    | Manage (Owner.Dropped _) -> true
    | Manage (Owner.Remember _) -> true
    | Manage (Owner.Check _) -> true
    | Manage (Owner.Forget _) -> true
    | Manage (Owner.Lost _) -> true
    | Manage (Owner.Finish _) -> true
    | Manage (Owner.Release _) -> true
    | Manage Owner.Close -> true in
  let queue, limit = if control then t.control, 16 else t.data, 32 in
  let deadline = if control then None else
    Some (Unix.gettimeofday () +. float_of_int t.limits.timeout_seconds) in
  let rec enqueue () =
    if not t.open_channel then Lwt.fail (Failure "resource compute provider stopped")
    else if Queue.length queue >= limit then
      if not control then Lwt.fail (Failure "resource compute provider busy")
      else let* () = Lwt_condition.wait t.space in enqueue ()
    else
      let promise, reply = Lwt.wait () in
      Queue.push (Command { message; promise; reply; deadline }) queue;
      Lwt_condition.signal t.ready ();
      promise in
  enqueue ()

let create ~limits ~deps =
  let stopped, stop_reply = Lwt.wait () in
  let closing, close_reply = Lwt.wait () in
  let t = {
    limits;
    deps;
    state = Owner.create limits.Resource_compute_provider_config.lanes;
    data = Queue.create ();
    control = Queue.create ();
    ready = Lwt_condition.create ();
    space = Lwt_condition.create ();
    open_channel = true;
    closing;
    close_reply;
    stopped;
    stop_reply;
  } in
  Lwt.async (fun () -> Lwt.catch (fun () -> owner_loop t) (fun error ->
    close_channel t error;
    if Lwt.is_sleeping t.closing then Lwt.wakeup_later t.close_reply ();
    if Lwt.is_sleeping t.stopped then Lwt.wakeup_later_exn t.stop_reply error;
    Lwt.return_unit));
  t

let self_test t =
  if not t.open_channel || t.state.closed then
    Lwt.return (Error "resource compute provider stopped")
  else t.deps.self_test ()

let stop t =
  let* () = if t.open_channel then actor_call t (Manage Owner.Close) else Lwt.return_unit in
  Lwt.protected t.stopped

let drop_model t key =
  let* result = Lwt.catch (fun () -> t.deps.drop_model key) (function
    | Failure reason -> Lwt.return (Error reason)
    | error -> Lwt.return (Error ("resource compute cache removal failed: " ^ Printexc.to_string error))) in
  match result with
  | Ok () -> Lwt.return (Ok ())
  | Error reason ->
    let* () = actor_call t (Manage Owner.Close) in
    Lwt.return (Error reason)

let owned t work =
  let aborted, notify = Lwt.wait () in
  let abort reason =
    if Lwt.is_sleeping aborted then Lwt.wakeup_later notify reason in
  let work = Lwt.no_cancel (Lwt.catch (fun () -> work aborted) (function
    | Failure reason -> Lwt.return (Error reason)
    | error -> Lwt.return (Error ("resource compute operation failed: " ^ Printexc.to_string error)))) in
  let timeout =
    let* () = Lwt_unix.sleep (float_of_int t.limits.timeout_seconds) in
    abort "resource compute request expired";
    Lwt.return (Error "resource compute request expired") in
  let result = Lwt.pick [Lwt.protected work; timeout] in
  Lwt.on_cancel result (fun () -> abort "resource compute request cancelled");
  result

let read t aborted run =
  if not (Lwt.is_sleeping t.closing) then
    Lwt.return (Error "resource compute provider stopped")
  else match Lwt.state aborted with
    | Lwt.Return reason -> Lwt.return (Error reason)
    | Lwt.Fail error -> Lwt.fail error
    | Lwt.Sleep ->
      let pending = run () in
      let abort = let* reason = Lwt.protected aborted in Lwt.return (Error reason) in
      let close = let* () = Lwt.protected t.closing in
        Lwt.return (Error "resource compute provider stopped") in
      Lwt.pick [Lwt.protected pending; abort; close]

let allowed t aborted token =
  let* result = actor_call t (Manage (Owner.Check token)) in
  match result, Lwt.state aborted with
  | Error _ as error, _ -> Lwt.return error
  | Ok (), Lwt.Return reason -> Lwt.return (Error reason)
  | Ok (), Lwt.Fail error -> Lwt.fail error
  | Ok (), Lwt.Sleep -> Lwt.return (Ok ())

let prepared (model : model) = Provider_rpc.{
  cache_key = model.cache_key;
  graph_root = model.graph_root;
  model_root = model.model_root;
  program_root = model.program_root;
  executor_root = model.executor_root;
  entries = model.entries;
  bytes = model.bytes;
}

let build_model t aborted (plan : Owner.plan) =
  let request = plan.request in
  Lwt.finalize (fun () ->
    let*| () = allowed t aborted plan.token in
    let*| test = t.deps.self_test () in
    if test.executor_root <> request.executor_root then
      Lwt.return (Error "resource compute executor root differs")
    else
      let*| program = read t aborted (fun () -> t.deps.program ~epoch_id:request.model_epoch
        ~state_root:request.model_state_root ~circle_id:request.circle_id) in
      let*| () = Lwt.return (verify_program request.program_root program) in
      let*| () = allowed t aborted plan.token in
      let*| () = match plan.victim with
        | None -> Lwt.return (Ok ())
        | Some model ->
          let*| () = drop_model t model.cache_key in
          let* dropped = actor_call t (Manage (Owner.Dropped plan.token)) in
          Lwt.return (if dropped then Ok () else Error "resource compute preparation changed") in
      let*| () = allowed t aborted plan.token in
      let cancel = Lwt.pick [
        Lwt.protected aborted;
        (let* () = Lwt.protected t.closing in Lwt.return "resource compute provider stopped");
      ] in
      let*| loaded = Lwt.finalize (fun () -> t.deps.load_model ~cancel request)
        (fun () -> Lwt.cancel cancel; Lwt.return_unit) in
      if not (Owner.prepared loaded request) then
        Lwt.return (Error "resource compute loaded model differs")
      else
        let*| () = allowed t aborted plan.token in
        let* result = actor_call t (Manage (Owner.Remember (plan.token, loaded))) in
        Lwt.return (Result.map (fun () -> prepared loaded) result))
    (fun () ->
      actor_call t (Manage (Owner.Finish plan.token)))

let prepare t request =
  owned t (fun aborted ->
    let* decision = actor_call t (Manage (Owner.Reserve request)) in
    match decision with
    | Error _ as error -> Lwt.return error
    | Ok (Owner.Ready model) -> Lwt.return (Ok (prepared model))
    | Ok (Owner.Build plan) -> build_model t aborted plan)

let execute_ready t aborted token spent model request =
  let output_limit =
    min request.Provider_rpc.max_output_bytes t.limits.max_response_bytes in
  let* program_result = read t aborted (fun () -> t.deps.program
      ~epoch_id:request.epoch_id
      ~state_root:request.state_root
      ~circle_id:request.circle_id) in
        begin
          match program_result with
          | Error _ as error -> Lwt.return error
          | Ok program ->
            begin
              match verify_program request.program_root program with
              | Error _ as error -> Lwt.return error
              | Ok () ->
                let* storage_result = read t aborted (fun () -> t.deps.storage
                    ~epoch_id:request.epoch_id
                    ~state_root:request.state_root
                    ~circle_id:request.circle_id) in
                begin
                  match storage_result with
                  | Error _ as error -> Lwt.return error
                  | Ok storage ->
                    let*| () = allowed t aborted token in
                    spent := true;
                    let* result =
                      t.deps.execute ~model ~program ~storage request in
                    Lwt.return
                      (match result with
                               | Error _ as error -> error
                               | Ok result ->
                                 let output_bytes = String.length result.output_json in
                                 if result.effort_used < 0 || result.effort_used > request.fuel_limit then
                                   Error "resource compute fuel exceeds requested limit"
                                 else if result.steps <> Int64.of_int result.effort_used then
                                   Error "resource compute fuel report differs"
                                 else if output_bytes > output_limit then
                                   Error "resource compute output exceeds requested limit"
                                 else if not (lower_hex_64 result.trace_root) then
                                   Error "resource compute trace root refused"
                                 else if result.steps <= 0L || result.operations <= 0L then
                                   Error "resource compute counters refused"
                                 else
                                   let output_hash =
                                     result.output_json
                                     |> Octra_consensus.Resource_compute_protocol.output_hash
                                     |> Text.raw_to_hex in
                                   Ok Provider_rpc.{
                                     output_json = result.output_json;
                                     output_hash;
                                     trace_root = result.trace_root;
                                     steps = result.steps;
                                     operations = result.operations;
                                     effort_used = result.effort_used;
                                   })
                end
            end
        end

let execute t request =
  if request.Provider_rpc.memory_bytes <= 0
     || request.memory_bytes > Octra_consensus.Resource_compute_certificate.max_memory_bytes
     || Int64.of_int request.memory_bytes > t.limits.memory_bytes then
    Lwt.return (Error "resource compute memory refused")
  else if request.guest_bytes < 65_536 || request.guest_bytes > 67_108_864
     || request.guest_bytes mod 65_536 <> 0 then
    Lwt.return (Error "resource compute pages refused")
  else if request.fuel_limit <= 0 || request.fuel_limit > 2_000_000_000 then
    Lwt.return (Error "resource compute fuel refused")
  else if String.length request.params_json > t.limits.max_request_bytes then
    Lwt.return (Error "resource compute input exceeds provider limit")
  else
    let request = {request with Provider_rpc.program_b64 = ""; storage = []} in
    owned t (fun aborted ->
      let* acquired = actor_call t (Select request) in
      match acquired with
      | Error _ as error -> Lwt.return error
      | Ok (token, model, lease) ->
        let spent = ref false in
        Lwt.finalize (fun () ->
          let*| () = allowed t aborted token in
          execute_ready t aborted token spent model request)
          (fun () ->
            Native.lease_close lease;
            actor_call t (Manage (Owner.Release (token, !spent)))))

let replay_loaded ~limits ~deps ~program_at ~load_model calls =
  let module Host = Octra_core.Circle_wasm_host in
  match calls with
  | [] -> Lwt.return_error (Host.Rejected "compute replay empty")
  | (_, (first : Provider_rpc.execute)) :: _ ->
    let request = Provider_rpc.{model_epoch = first.model_epoch;
      model_state_root = first.model_state_root; circle_id = first.circle_id;
      graph_root = first.graph_root; model_root = first.model_root;
      program_root = first.program_root; executor_root = first.executor_root} in
    let rec programs saved = function
      | [] -> Lwt.return_ok saved
      | (height, root) :: rest ->
        let* found = program_at ~epoch_id:height ~state_root:root ~circle_id:first.circle_id in
        match found with
        | Error _ as error -> Lwt.return error
        | Ok program ->
          match verify_program first.program_root program with
          | Error reason -> Lwt.return_error (Host.Rejected reason)
          | Ok () ->
            match saved with
            | Some prior when prior <> program ->
              Lwt.return_error (Host.Rejected "compute replay program changed")
            | _ -> programs (Some program) rest in
    let roots = (first.model_epoch, first.model_state_root) ::
      List.map (fun (_, (call : Provider_rpc.execute)) -> call.epoch_id, call.state_root) calls in
    let* checked = programs None roots in
    match checked with
    | Error _ as error -> Lwt.return error
    | Ok None -> Lwt.return_error (Host.Rejected "compute replay program missing")
    | Ok (Some program) ->
    let* test = deps.self_test () in
    match test with
    | Error reason -> Lwt.return_error (Host.Unavailable reason)
    | Ok test when test.executor_root <> first.executor_root ->
      Lwt.return_error (Host.Rejected "compute executor root differs")
    | Ok _ ->
    let cancel, _ = Lwt.wait () in
    let* loaded = load_model ~cancel request in
    match loaded with
    | Error _ as error -> Lwt.return error
    | Ok model ->
    let deps = {deps with load_model = (fun ~cancel:_ _ -> Lwt.return_ok model);
      program = (fun ~epoch_id:_ ~state_root:_ ~circle_id:_ -> Lwt.return_ok program)} in
    let engine = create ~limits ~deps in
    Lwt.finalize (fun () ->
      let* prepared = prepare engine request in
      match prepared with
      | Error reason -> Lwt.return_error (Host.Unavailable reason)
      | Ok _ ->
        let* found = actor_call engine (Select first) in
        match found with
        | Error reason -> Lwt.return_error (Host.Unavailable reason)
        | Ok (token, model, lease) ->
        Lwt.finalize (fun () ->
        replay ~model ~program calls)
          (fun () -> Native.lease_close lease;
            actor_call engine (Manage (Owner.Release (token, false)))))
      (fun () -> stop engine)