(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Rpc = Octra_core.Rpc

let view_capacity = Circle_view_capacity.create ~limit:2

type rpc_result = (Yojson.Safe.t, Rpc.rpc_error) result

type 'handler dispatch_adapters = {
  store_read : (Octra_core.Store_irmin.t -> Yojson.Safe.t -> rpc_result Lwt.t) -> 'handler;
  epoch_read :
    (Octra_core.Store_irmin.t ->
     Yojson.Safe.t ->
     current_epoch:int ->
     rpc_result Lwt.t) ->
    'handler;
  public_asset_read :
    (Octra_core.Store_irmin.t ->
     Yojson.Safe.t ->
     current_epoch:int ->
     reveal_sensitive_fields:bool ->
     rpc_result Lwt.t) ->
    'handler;
  circle_view : 'handler;
  circle_view_auth : 'handler;
}

let ok value =
  Lwt.return (Ok value)

let rpc_error message =
  Rpc.err (-32000) message None

let query_result result view =
  match result with
  | Ok value ->
    Ok (view value)
  | Error message ->
    Error (rpc_error message)

let option_result option ~not_found view =
  match option with
  | Some value ->
    Ok (view value)
  | None ->
    Error (Rpc.not_found not_found)

let program_descriptor store ~circle_id =
  let open Lwt.Syntax in
  let* described = Octra_circle_runtime.Circle_program.describe store circle_id in
  match described with
  | Ok descriptor ->
    ok (Octra_circle_runtime.Circle_program.yojson_of_descriptor descriptor)
  | Error "circle not found" ->
    Lwt.return (Error (Rpc.not_found "circle not found"))
  | Error e ->
    Lwt.return (Error (rpc_error e))

let info_public store ~circle_id =
  let open Lwt.Syntax in
  let* info_opt = Octra_core.Store_irmin.get_circle_info store circle_id in
  match info_opt with
  | None ->
    Lwt.return (Error (Rpc.not_found "circle not found"))
  | Some info ->
    if not (Circle_auth.public_info_allowed info) then
      Lwt.return (Error (Rpc.err (-32000) "authenticated circle info required" None))
    else
      ok (Octra_core.Circles.yojson_of_circle_info info)

let circle_id_param params f =
  match Rpc.require_string params 0 "circle_id" with
  | Error e -> Lwt.return (Error e)
  | Ok circle_id -> f circle_id

let info_public_params store params =
  circle_id_param params (fun circle_id ->
    info_public store ~circle_id)

let err_lwt error =
  Lwt.return (Error error)

let with_auth params info addr_idx pub_idx sig_idx ~gate ~op ~circle_id ~subject f =
  match Circle_auth.parse_rpc_auth params addr_idx pub_idx sig_idx with
  | Error e ->
    err_lwt e
  | Ok auth ->
    match Circle_auth.authorize_rpc info ~gate ~op ~circle_id ~subject auth with
    | Error e -> err_lwt e
    | Ok addr -> f addr

let auth_required0 params message =
  err_lwt (Circle_auth.auth_required0 params message)

let auth_required1 params field message =
  err_lwt (Circle_auth.auth_required1 params field message)

let auth_required2 params field1 field2 message =
  err_lwt (Circle_auth.auth_required2 params field1 field2 message)

let slot_policy_required_route params _ctx =
  auth_required1 params "slot_ref" "authenticated circle slot policy read required"

let state_policy_required_route params _ctx =
  auth_required1 params "state_ref" "authenticated circle state policy read required"

let state_descriptor_required_route params _ctx =
  auth_required1 params "state_ref" "authenticated circle state descriptor read required"

let balance_cell_required_route params _ctx =
  auth_required1 params "state_ref" "authenticated circle balance cell read required"

let register_cell_required_route params _ctx =
  auth_required1 params "state_ref" "authenticated circle register cell read required"

let balance_binding_required_route params _ctx =
  auth_required1 params "subject_addr" "authenticated circle balance binding read required"

let register_binding_required_route params _ctx =
  auth_required1 params "register_ref" "authenticated circle register binding read required"

let balance_workflow_required_route params _ctx =
  auth_required1 params "workflow_ref" "authenticated circle balance workflow read required"

let register_workflow_required_route params _ctx =
  auth_required1 params "workflow_ref" "authenticated circle register workflow read required"

let object_summary_required_route params _ctx =
  auth_required1 params "object_ref" "authenticated circle object summary read required"

let object_members_required_route params _ctx =
  auth_required1 params "object_ref" "authenticated circle object members read required"

let object_detail_required_route params _ctx =
  auth_required1 params "object_ref" "authenticated circle object detail read required"

let object_member_required_route params _ctx =
  auth_required2 params "object_ref" "member_ref" "authenticated circle object member read required"

let object_refs_required_route params _ctx =
  auth_required0 params "authenticated circle object refs read required"

let object_list_required_route params _ctx =
  auth_required0 params "authenticated circle object list read required"

let transport_policy_required_route params _ctx =
  auth_required0 params "authenticated circle transport policy read required"

let hfhe_policy_required_route params _ctx =
  auth_required0 params "authenticated circle hfhe policy read required"

let key_policy_required_route params _ctx =
  auth_required1 params "key_id" "authenticated circle key policy read required"

let storage_required_route params _ctx =
  auth_required1 params "key" "authenticated circle storage read required"

let storage_dump_required_route params _ctx =
  auth_required0 params "authenticated circle storage dump required"

let outbox_intent_required_route params _ctx =
  auth_required1 params "intent_id" "authenticated circle outbox read required"

let outbox_claim_required_route params _ctx =
  auth_required1 params "intent_id" "authenticated circle outbox read required"

let outbox_status_required_route params _ctx =
  auth_required1 params "intent_id" "authenticated circle outbox read required"

let ingress_packet_required_route params _ctx =
  auth_required1 params "intent_id" "authenticated circle ingress read required"

let with_scope_auth store params ~gate ~op f =
  match Rpc.require_string params 0 "circle_id" with
  | Error e -> err_lwt e
  | Ok circle_id ->
    let open Lwt.Syntax in
    let* info_opt = Octra_core.Store_irmin.get_circle_info store circle_id in
    match info_opt with
    | None ->
      err_lwt (Rpc.not_found "circle not found")
    | Some info ->
      with_auth
        params
        info
        1
        2
        3
        ~gate
        ~op
        ~circle_id
        ~subject:""
        (fun _ -> f circle_id info)

let with_owner_scope_auth store params ~op f =
  with_scope_auth store params ~gate:Circle_auth.Owner ~op (fun circle_id _info ->
    f circle_id)

let info_auth_params store params =
  with_scope_auth
    store
    params
    ~gate:Circle_auth.Owner_private
    ~op:"octra_circle_info"
    (fun _circle_id info ->
      ok (Octra_core.Circles.yojson_of_circle_info info))

let program_descriptor_auth_params store params =
  with_scope_auth
    store
    params
    ~gate:Circle_auth.Private_owner
    ~op:"octra_circle_program_info"
    (fun circle_id _info ->
      program_descriptor store ~circle_id)

let with_owner_subject_auth store params ~field ~op f =
  match Rpc.require_string params 0 "circle_id", Rpc.require_string params 1 field with
  | Error e, _ | _, Error e -> err_lwt e
  | Ok circle_id, Ok subject ->
    let open Lwt.Syntax in
    let* info_opt = Octra_core.Store_irmin.get_circle_info store circle_id in
    match info_opt with
    | None ->
      err_lwt (Rpc.not_found "circle not found")
    | Some info ->
      with_auth
        params
        info
        2
        3
        4
        ~gate:Circle_auth.Owner
        ~op
        ~circle_id
        ~subject
        (fun _ -> f circle_id subject)

let with_owner_asset_auth store params ~circle_id ~op ~subject f =
  let open Lwt.Syntax in
  let* info_opt = Octra_core.Store_irmin.get_circle_info store circle_id in
  match info_opt with
  | None ->
    err_lwt (Rpc.not_found "circle not found")
  | Some info ->
    with_auth
      params
      info
      2
      3
      4
      ~gate:Circle_auth.Owner
      ~op
      ~circle_id
      ~subject
      (fun _ -> f ())

let with_owner_validated_subject_auth store params ~field ~op ~validate f =
  match Rpc.require_string params 0 "circle_id", Rpc.require_string params 1 field with
  | Error e, _ | _, Error e -> err_lwt e
  | Ok circle_id, Ok value ->
    begin
      match validate value with
      | Error e ->
        err_lwt (Rpc.invalid_params e)
      | Ok subject ->
        let open Lwt.Syntax in
        let* info_opt = Octra_core.Store_irmin.get_circle_info store circle_id in
        match info_opt with
        | None ->
          err_lwt (Rpc.not_found "circle not found")
        | Some info ->
          with_auth
            params
            info
            2
            3
            4
            ~gate:Circle_auth.Owner
            ~op
            ~circle_id
            ~subject
            (fun _ -> f circle_id subject)
    end

let with_owner_locator_auth store params ~field ~op ~parse_locator f =
  match Rpc.require_string params 0 "circle_id", Rpc.require_string params 1 field with
  | Error e, _ | _, Error e -> err_lwt e
  | Ok circle_id, Ok locator_value ->
    begin
      match parse_locator locator_value with
      | Error e ->
        err_lwt (Rpc.invalid_params e)
      | Ok locator ->
        let open Lwt.Syntax in
        let* info_opt = Octra_core.Store_irmin.get_circle_info store circle_id in
        match info_opt with
        | None ->
          err_lwt (Rpc.not_found "circle not found")
        | Some info ->
          with_auth
            params
            info
            2
            3
            4
            ~gate:Circle_auth.Owner
            ~op
            ~circle_id
            ~subject:locator.Circle_view.value
            (fun _ -> f circle_id locator)
    end

let with_object_member_auth store params f =
  match Circle_view.object_member_params params with
  | Error e -> err_lwt e
  | Ok req ->
    let open Lwt.Syntax in
    let* info_opt = Octra_core.Store_irmin.get_circle_info store req.Circle_view.circle_id in
    match info_opt with
    | None ->
      err_lwt (Rpc.not_found "circle not found")
    | Some info ->
      with_auth
        params
        info
        3
        4
        5
        ~gate:Circle_auth.Owner
        ~op:"octra_circle_object_member"
        ~circle_id:req.circle_id
        ~subject:(Circle_view.object_member_subject req)
        (fun _ -> f req.circle_id req.object_ref req.member_ref)

let program_info_public store ~circle_id =
  let open Lwt.Syntax in
  let* info_opt = Octra_core.Store_irmin.get_circle_info store circle_id in
  match info_opt with
  | None ->
    Lwt.return (Error (Rpc.not_found "circle not found"))
  | Some info ->
    if Circle_auth.private_view_required info then
      Lwt.return (Error (Rpc.err (-32000) "authenticated circle program info required" None))
    else
      program_descriptor store ~circle_id

let program_info_public_params store params =
  circle_id_param params (fun circle_id ->
    program_info_public store ~circle_id)

let program_value store (info : Octra_core.Circles.circle_info) =
  let open Lwt.Syntax in
  let* code_opt =
    Octra_core.Store_irmin.get_circle_program_code_b64 store info.Octra_core.Circles.circle_id in
  ok
    (Circle_view.program
       ~circle_id:info.circle_id
       ~code_b64:code_opt
       ~code_hash:info.code_hash
       ~runtime:info.runtime)

let program store ~circle_id =
  let open Lwt.Syntax in
  let* info_opt = Octra_core.Store_irmin.get_circle_info store circle_id in
  match info_opt with
  | None ->
    Lwt.return (Error (Rpc.not_found "circle not found"))
  | Some info ->
    if Circle_auth.private_view_required info then
      Lwt.return (Error (Rpc.err (-32000) "authenticated circle program read required" None))
    else
      program_value store info

let program_params store params =
  circle_id_param params (fun circle_id ->
    program store ~circle_id)

let program_auth_params store params =
  with_scope_auth
    store
    params
    ~gate:Circle_auth.Private_owner
    ~op:"octra_circle_program"
    (fun _circle_id info ->
      program_value store info)

let view_call
    ?(trusted=[])
    store
    ~view_ctx
    ~circle_id
    ~method_name
    ~call_params
    ~caller_addr
    ~include_storage =
  Circle_view_capacity.with_slot
    view_capacity
    ~busy:(fun () ->
      Lwt.return
        (Error (Rpc.err (-32005) "circle view busy" None)))
    (fun () ->
      let open Lwt.Syntax in
      let* result =
        Octra_circle_runtime.Circle_exec.execute_view_call
          ~trusted
          ~ctx:view_ctx
          store
          circle_id
          method_name
          call_params
          caller_addr in
      match Octra_vm.Receipt_view.view_call_outcome result with
      | Octra_vm.Receipt_view.View_call_ok return_value ->
        if include_storage then
          let* storage_result =
            Octra_circle_runtime.Circle_exec.list_storage_pairs store circle_id in
          begin
            match storage_result with
            | Ok storage_pairs ->
              ok (Circle_view.view_result ~storage_pairs return_value)
            | Error e ->
              Lwt.return (Error (rpc_error e))
          end
        else
          ok (Circle_view.view_result return_value)
      | Octra_vm.Receipt_view.View_call_failed error ->
        Lwt.return (Error (rpc_error error)))

let view_call_public
    ?(trusted=[])
    store
    ~view_ctx
    ~circle_id
    ~method_name
    ~call_params
    ~caller_addr =
  let open Lwt.Syntax in
  let* info_opt = Octra_core.Store_irmin.get_circle_info store circle_id in
  match info_opt with
  | None ->
    Lwt.return (Error (Rpc.not_found "circle not found"))
  | Some info ->
    if Circle_auth.private_view_required info then
      Lwt.return (Error (Rpc.err (-32000) "authenticated circle view required" None))
    else
      view_call
        ~trusted
        store
        ~view_ctx
        ~circle_id
        ~method_name
        ~call_params
        ~caller_addr
        ~include_storage:false

let view_call_public_params ?(trusted=[]) store params ~view_ctx =
  match Circle_view.view_call_params params with
  | Error e ->
    err_lwt e
  | Ok call ->
    view_call_public
      ~trusted
      store
      ~view_ctx
      ~circle_id:call.Circle_view.circle_id
      ~method_name:call.method_name
      ~call_params:call.call_params
      ~caller_addr:"oct00000000000000000000000000000000000000000000"

let view_call_auth ?(trusted=[]) store params ~view_ctx =
  match Circle_view.view_call_params params with
  | Error e ->
    err_lwt e
  | Ok call ->
    let include_storage =
      Octra_vm.Call_plan.bool_json ~default:false (Rpc.param_json params 6) in
    let open Lwt.Syntax in
    let* info_opt =
      Octra_core.Store_irmin.get_circle_info store call.Circle_view.circle_id in
    match info_opt with
    | None ->
      err_lwt (Rpc.not_found "circle not found")
    | Some info ->
      with_auth
        params
        info
        3
        4
        5
        ~gate:(Circle_auth.view_gate ~include_storage)
        ~op:"octra_circle_view"
        ~circle_id:call.Circle_view.circle_id
        ~subject:(Circle_view.view_subject
          ~method_name:call.method_name
          ~call_params:call.call_params
          ~include_storage)
        (fun caller_addr ->
          view_call
            ~trusted
            store
            ~view_ctx
            ~circle_id:call.Circle_view.circle_id
            ~method_name:call.method_name
            ~call_params:call.call_params
            ~caller_addr
            ~include_storage)

let object_read store ~circle_id ~object_ref ~load ~view =
  let open Lwt.Syntax in
  let* result = load store circle_id object_ref in
  match query_result result (view object_ref) with
  | Ok value -> ok value
  | Error e -> Lwt.return (Error e)

let object_read_auth_params store params ~op ~load ~view =
  with_owner_validated_subject_auth
    store
    params
    ~field:"object_ref"
    ~op
    ~validate:(Octra_core.Circle_private_common.normalize_hex64 "object_ref")
    (fun circle_id object_ref ->
      object_read
        store
        ~circle_id
        ~object_ref
        ~load
        ~view:(view circle_id))

let object_summary_auth_params store params =
  object_read_auth_params
    store
    params
    ~op:"octra_circle_object_summary"
    ~load:Octra_core.Circle_object_query.load_summary
    ~view:(fun circle_id _ summary ->
      Circle_view.object_summary ~circle_id summary)

let object_members_auth_params store params =
  object_read_auth_params
    store
    params
    ~op:"octra_circle_object_members"
    ~load:Octra_core.Circle_object_query.load_member_summaries
    ~view:(fun circle_id object_ref members ->
      Circle_view.object_members
        ~circle_id
        ~object_ref
        members)

let object_detail_auth_params store params =
  object_read_auth_params
    store
    params
    ~op:"octra_circle_object_detail"
    ~load:Octra_core.Circle_object_detail_query.load_detail
    ~view:(fun circle_id _ detail ->
      Circle_view.object_detail ~circle_id detail)

let object_member_detail store ~circle_id ~object_ref ~member_ref =
  let open Lwt.Syntax in
  let* detail_result =
    Octra_core.Circle_object_detail_query.load_member_detail
      store
      circle_id
      object_ref
      member_ref in
  match query_result detail_result (Circle_view.object_member_detail ~circle_id ~object_ref) with
  | Ok value -> ok value
  | Error e -> Lwt.return (Error e)

let object_member_auth_params store params =
  with_object_member_auth store params (fun circle_id object_ref member_ref ->
    object_member_detail
      store
      ~circle_id
      ~object_ref
      ~member_ref)

let object_discovery store ~circle_id ~load ~view =
  let open Lwt.Syntax in
  let* result = load store circle_id in
  match query_result result view with
  | Ok value -> ok value
  | Error e -> Lwt.return (Error e)

let object_refs_auth_params store params =
  with_owner_scope_auth store params ~op:"octra_circle_object_refs" (fun circle_id ->
    object_discovery
      store
      ~circle_id
      ~load:Octra_core.Circle_object_discovery_query.load_refs
      ~view:(Circle_view.object_refs ~circle_id))

let object_list_auth_params store params =
  with_owner_scope_auth store params ~op:"octra_circle_object_list" (fun circle_id ->
    object_discovery
      store
      ~circle_id
      ~load:Octra_core.Circle_object_discovery_query.load_summaries
      ~view:(Circle_view.object_list ~circle_id))

let transport_policy store ~circle_id =
  let open Lwt.Syntax in
  let* policy =
    Octra_core.Circle_policy_store.load_transport_policy store circle_id in
  ok (Circle_view.transport_policy ~circle_id policy)

let hfhe_policy store ~circle_id =
  let open Lwt.Syntax in
  let* policy =
    Octra_core.Circle_policy_store.load_hfhe_policy store circle_id in
  ok (Circle_view.hfhe_policy ~circle_id policy)

let key_policy store ~circle_id ~key_id ~current_epoch =
  let open Lwt.Syntax in
  let* policy =
    Octra_core.Circle_policy_store.load_key_policy store circle_id key_id in
  let current_epoch = Int64.of_int current_epoch in
  let key_state = Octra_core.Circle_key_state.classify policy current_epoch in
  ok
    (Circle_view.key_policy
       ~circle_id
       ~key_id
       ~current_epoch
       ~key_state
       policy)

let policy_view (policy : Octra_circle_runtime.Circle_exec.slot_policy) =
  Circle_view.policy
    ~delivery_key_id:policy.delivery_key_id
    ~activate_after_epoch:policy.activate_after_epoch
    ~expire_after_epoch:policy.expire_after_epoch
    ~tombstone:policy.tombstone
    ~revoked:policy.revoked

let read_locator_policy store circle_id locator_mode path_key =
  match locator_mode with
  | Octra_core.Circles.State_locator ->
    Octra_circle_runtime.Circle_exec.read_state_policy store circle_id path_key
  | Octra_core.Circles.Path_locator
  | Octra_core.Circles.Slot_locator ->
    Octra_circle_runtime.Circle_exec.read_slot_policy store circle_id path_key

let delivery_key_summary store ~circle_id ~current_epoch policy =
  let open Lwt.Syntax in
  match policy.Octra_circle_runtime.Circle_exec.delivery_key_id with
  | Some key_id ->
    let* key_policy =
      Octra_core.Circle_policy_store.load_key_policy
        store
        circle_id
        key_id in
    let key_state =
      Octra_core.Circle_key_state.classify
        key_policy
        (Int64.of_int current_epoch) in
    Lwt.return (Some (key_id, key_state))
  | None ->
    Lwt.return_none

let locator_policy store ~circle_id ~(locator : Circle_view.locator) ~current_epoch ~read_policy =
  let open Lwt.Syntax in
  let* policy = read_policy store circle_id locator.Circle_view.path_key in
  let* delivery_key =
    delivery_key_summary store ~circle_id ~current_epoch policy in
  ok
    (Circle_view.locator_policy_of
       ~circle_id
       locator
       (policy_view policy)
       delivery_key)

let slot_locator_policy store ~circle_id ~locator ~current_epoch =
  locator_policy
    store
    ~circle_id
    ~locator
    ~current_epoch
    ~read_policy:Octra_circle_runtime.Circle_exec.read_slot_policy

let state_locator_policy store ~circle_id ~locator ~current_epoch =
  locator_policy
    store
    ~circle_id
    ~locator
    ~current_epoch
    ~read_policy:Octra_circle_runtime.Circle_exec.read_state_policy

let locator_policy_auth_params store params ~op ~field ~parse_locator ~read_policy ~current_epoch =
  with_owner_locator_auth store params ~field ~op ~parse_locator (fun circle_id locator ->
    read_policy
      store
      ~circle_id
      ~locator
      ~current_epoch)

let slot_locator_policy_auth_params store params ~current_epoch =
  locator_policy_auth_params
    store
    params
    ~op:"octra_circle_slot_policy"
    ~field:"slot_ref"
    ~parse_locator:Circle_view.slot_locator
    ~read_policy:slot_locator_policy
    ~current_epoch

let state_locator_policy_auth_params store params ~current_epoch =
  locator_policy_auth_params
    store
    params
    ~op:"octra_circle_state_policy"
    ~field:"state_ref"
    ~parse_locator:Circle_view.state_locator
    ~read_policy:state_locator_policy
    ~current_epoch

let state_descriptor store ~circle_id ~(locator : Circle_view.locator) =
  let open Lwt.Syntax in
  let* descriptor_opt =
    Octra_core.Circle_state_descriptor_store.load_descriptor_opt
      store
      circle_id
      locator.path_key in
  ok
    (Circle_view.state_descriptor
       ~circle_id
       ~canonical_path:locator.canonical_path
       ~path_key:locator.path_key
       ~state_ref:locator.value
       descriptor_opt)

let state_descriptor_auth_params store params =
  with_owner_locator_auth
    store
    params
    ~field:"state_ref"
    ~op:"octra_circle_state_descriptor"
    ~parse_locator:Circle_view.state_locator
    (fun circle_id locator ->
      state_descriptor store ~circle_id ~locator)

let asset_delivery_key_fields store ~circle_id ~current_epoch meta path_key =
  let open Lwt.Syntax in
  let* policy =
    read_locator_policy store circle_id meta.Octra_core.Circles.locator_mode path_key in
  match Circle_view.asset_effective_key_id (policy_view policy) meta with
  | Some key_id ->
    let* key_policy =
      Octra_core.Circle_policy_store.load_key_policy
        store
        circle_id
        key_id in
    let key_state =
      Octra_core.Circle_key_state.classify
        key_policy
        (Int64.of_int current_epoch) in
    Lwt.return (Circle_view.asset_delivery_key_fields (Some (key_id, key_state)))
  | None ->
    Lwt.return (Circle_view.asset_delivery_key_fields None)

let asset_state_descriptor_fields store ~circle_id meta =
  let open Lwt.Syntax in
  match Circle_view.asset_state_descriptor_path_key meta with
  | Some path_key ->
    let* descriptor_opt =
      Octra_core.Circle_state_descriptor_store.load_descriptor_opt
        store
        circle_id
        path_key in
    Lwt.return (Circle_view.asset_descriptor_fields (Some descriptor_opt))
  | None ->
    Lwt.return (Circle_view.asset_descriptor_fields None)

let asset_plaintext store ~circle_id ~path_key ~canonical_path =
  let open Lwt.Syntax in
  let* info_opt = Octra_core.Store_irmin.get_circle_info store circle_id in
  match info_opt with
  | None ->
    Lwt.return (Error (Rpc.not_found "circle not found"))
  | Some info ->
    let* meta_opt =
      Octra_core.Store_irmin.get_circle_asset_meta store circle_id path_key in
    match meta_opt with
    | Some meta
      when meta.body_mode = Octra_core.Circles.Sealed_read ||
           info.resource_mode = Octra_core.Circles.Sealed_read ->
      Lwt.return
        (Error (Rpc.invalid_params "circle asset is sealed; use circle_asset_ciphertext"))
    | Some meta ->
      let* body_opt =
        Octra_core.Store_irmin.get_circle_asset_body_b64
          store
          circle_id
          path_key in
      begin
        match body_opt with
        | Some body_b64 ->
          ok
            (Circle_view.asset_plaintext
               ~circle_id
               ~canonical_path
               ~path_key
               ~browser_mode:info.browser_mode
               ~resource_mode:info.resource_mode
               ~meta
               ~body_b64)
        | None ->
          Lwt.return (Error (Rpc.not_found "circle asset not found"))
      end
    | None ->
      Lwt.return (Error (Rpc.not_found "circle asset not found"))

let asset_plaintext_params store params =
  match Circle_view.asset_path_params params with
  | Error e -> Lwt.return (Error e)
  | Ok path ->
    asset_plaintext
      store
      ~circle_id:path.Circle_view.circle_id
      ~path_key:path.path_key
      ~canonical_path:path.canonical_path

let asset_ciphertext
    store
    ~circle_id
    ~path_key
    ~canonical_path
    ~current_epoch
    ~reveal_sensitive_fields =
  let open Lwt.Syntax in
  let* info_opt = Octra_core.Store_irmin.get_circle_info store circle_id in
  match info_opt with
  | None ->
    Lwt.return (Error (Rpc.not_found "circle not found"))
  | Some info ->
    let* meta_opt =
      Octra_core.Store_irmin.get_circle_asset_meta store circle_id path_key in
    let* body_opt =
      Octra_core.Store_irmin.get_circle_asset_ciphertext_b64
        store
        circle_id
        path_key in
    match meta_opt, body_opt with
    | Some meta, Some ciphertext_b64 ->
      let* visible =
        Octra_circle_runtime.Circle_exec.asset_visible_now
          store
          circle_id
          (Int64.of_int current_epoch)
          meta in
      if not visible then
        Lwt.return (Error (Rpc.not_found "circle ciphertext asset not found"))
      else
        let* delivery_key_fields =
          if reveal_sensitive_fields then
            asset_delivery_key_fields store ~circle_id ~current_epoch meta path_key
          else
            Lwt.return [] in
        let* state_descriptor_fields =
          if reveal_sensitive_fields then
            asset_state_descriptor_fields store ~circle_id meta
          else
            Lwt.return [] in
        ok
          (Circle_view.asset_ciphertext
             ~circle_id
             ~ciphertext_b64
             ~canonical_path
             ~path_key
             ~browser_mode:info.browser_mode
             ~resource_mode:info.resource_mode
             ~meta
             ~delivery_key_fields
             ~state_descriptor_fields
             ~reveal_sensitive_fields)
    | _ ->
      Lwt.return (Error (Rpc.not_found "circle ciphertext asset not found"))

let asset_ciphertext_params store params ~current_epoch ~reveal_sensitive_fields =
  match Circle_view.asset_path_params params with
  | Error e -> Lwt.return (Error e)
  | Ok path ->
    asset_ciphertext
      store
      ~circle_id:path.Circle_view.circle_id
      ~path_key:path.path_key
      ~canonical_path:path.canonical_path
      ~current_epoch
      ~reveal_sensitive_fields

let asset_ciphertext_by_resource_key
    store
    ~circle_id
    ~resource_key
    ~current_epoch
    ~reveal_sensitive_fields =
  let open Lwt.Syntax in
  let* path_key_opt =
    Octra_core.Store_irmin.get_circle_asset_path_key_by_resource_key
      store
      circle_id
      resource_key in
  match path_key_opt with
  | None ->
    Lwt.return (Error (Rpc.not_found "circle ciphertext asset not found"))
  | Some path_key ->
    let* meta_opt =
      Octra_core.Store_irmin.get_circle_asset_meta store circle_id path_key in
    match meta_opt with
    | None ->
      Lwt.return (Error (Rpc.not_found "circle ciphertext asset not found"))
    | Some meta ->
      asset_ciphertext
        store
        ~circle_id
        ~path_key
        ~canonical_path:meta.canonical_path
        ~current_epoch
        ~reveal_sensitive_fields

let asset_ciphertext_by_resource_key_params
    store
    params
    ~current_epoch
    ~reveal_sensitive_fields =
  match Circle_view.asset_resource_params params with
  | Error e -> Lwt.return (Error e)
  | Ok req ->
    asset_ciphertext_by_resource_key
      store
      ~circle_id:req.Circle_view.circle_id
      ~resource_key:req.resource_key
      ~current_epoch
      ~reveal_sensitive_fields

let asset_ciphertext_by_locator_params
    store
    params
    ~field
    ~parse_locator
    ~current_epoch
    ~reveal_sensitive_fields =
  match Circle_view.asset_locator_params params field parse_locator with
  | Error e -> Lwt.return (Error e)
  | Ok parsed ->
    asset_ciphertext
      store
      ~circle_id:parsed.Circle_view.circle_id
      ~path_key:parsed.locator.Circle_view.path_key
      ~canonical_path:parsed.locator.Circle_view.canonical_path
      ~current_epoch
      ~reveal_sensitive_fields

let asset_ciphertext_by_slot_ref_params
    store
    params
    ~current_epoch
    ~reveal_sensitive_fields =
  asset_ciphertext_by_locator_params
    store
    params
    ~field:"slot_ref"
    ~parse_locator:Circle_view.slot_locator
    ~current_epoch
    ~reveal_sensitive_fields

let asset_ciphertext_by_state_ref_params
    store
    params
    ~current_epoch
    ~reveal_sensitive_fields =
  asset_ciphertext_by_locator_params
    store
    params
    ~field:"state_ref"
    ~parse_locator:Circle_view.state_locator
    ~current_epoch
    ~reveal_sensitive_fields

let asset_ciphertext_auth_params store params ~current_epoch =
  match Circle_view.asset_path_params params with
  | Error e -> Lwt.return (Error e)
  | Ok path ->
    with_owner_asset_auth
      store
      params
      ~circle_id:path.Circle_view.circle_id
      ~op:"octra_circle_asset_ciphertext"
      ~subject:(Circle_view.asset_subject ~kind:"path" ~subject:path.canonical_path)
      (fun () ->
        asset_ciphertext
          store
          ~circle_id:path.Circle_view.circle_id
          ~path_key:path.path_key
          ~canonical_path:path.canonical_path
          ~current_epoch
          ~reveal_sensitive_fields:true)

let asset_ciphertext_by_resource_key_auth_params store params ~current_epoch =
  match Circle_view.asset_resource_params params with
  | Error e -> Lwt.return (Error e)
  | Ok req ->
    with_owner_asset_auth
      store
      params
      ~circle_id:req.Circle_view.circle_id
      ~op:"octra_circle_asset_ciphertext_by_resource_key"
      ~subject:(Circle_view.asset_resource_subject req)
      (fun () ->
        asset_ciphertext_by_resource_key
          store
          ~circle_id:req.circle_id
          ~resource_key:req.resource_key
          ~current_epoch
          ~reveal_sensitive_fields:true)

let asset_ciphertext_by_locator_auth_params
    store
    params
    ~op
    ~field
    ~parse_locator
    ~current_epoch =
  match Circle_view.asset_locator_params params field parse_locator with
  | Error e -> Lwt.return (Error e)
  | Ok parsed ->
    with_owner_asset_auth
      store
      params
      ~circle_id:parsed.Circle_view.circle_id
      ~op
      ~subject:(Circle_view.asset_locator_subject field parsed.locator)
      (fun () ->
        asset_ciphertext
          store
          ~circle_id:parsed.Circle_view.circle_id
          ~path_key:parsed.locator.Circle_view.path_key
          ~canonical_path:parsed.locator.Circle_view.canonical_path
          ~current_epoch
          ~reveal_sensitive_fields:true)

let asset_ciphertext_by_slot_ref_auth_params store params ~current_epoch =
  asset_ciphertext_by_locator_auth_params
    store
    params
    ~op:"octra_circle_asset_ciphertext_by_slot_ref"
    ~field:"slot_ref"
    ~parse_locator:Circle_view.slot_locator
    ~current_epoch

let asset_ciphertext_by_state_ref_auth_params store params ~current_epoch =
  asset_ciphertext_by_locator_auth_params
    store
    params
    ~op:"octra_circle_asset_ciphertext_by_state_ref"
    ~field:"state_ref"
    ~parse_locator:Circle_view.state_locator
    ~current_epoch

let private_state_cell
    store
    ~circle_id
    ~(locator : Circle_view.locator)
    ~current_epoch
    ~not_found
    ~cell_field
    ~empty_cell
    ~load_cell
    ~cell_json =
  let open Lwt.Syntax in
  let* meta_opt =
    Octra_core.Store_irmin.get_circle_asset_meta
      store
      circle_id
      locator.path_key in
  let* body_opt =
    Octra_core.Store_irmin.get_circle_asset_ciphertext_b64
      store
      circle_id
      locator.path_key in
  match meta_opt, body_opt with
  | Some meta, Some ciphertext_b64 ->
    let* visible =
      Octra_circle_runtime.Circle_exec.asset_visible_now
        store
        circle_id
        (Int64.of_int current_epoch)
        meta in
    if not visible then
      Lwt.return (Error (Rpc.not_found not_found))
    else
      let* descriptor_opt =
        Octra_core.Circle_state_descriptor_store.load_descriptor_opt
          store
          circle_id
          locator.path_key in
      let* policy =
        Octra_circle_runtime.Circle_exec.read_state_policy
          store
          circle_id
          locator.path_key in
      let* delivery_key =
        delivery_key_summary store ~circle_id ~current_epoch policy in
      let* cell_opt =
        load_cell store circle_id locator.path_key in
      let cell =
        Option.value ~default:empty_cell cell_opt in
      ok
        (Circle_view.private_state
           ~circle_id
           ~state_ref:locator.value
           ~canonical_path:locator.canonical_path
           ~path_key:locator.path_key
           ~ciphertext_b64
           ~meta
           ~descriptor_opt
           ~policy:(policy_view policy)
           ~delivery_key
           ~cell_field
           ~cell_materialized:(Option.is_some cell_opt)
           ~cell_json:(cell_json cell))
  | _ ->
    Lwt.return (Error (Rpc.not_found not_found))

let private_state_cell_auth_params
    store
    params
    ~op
    ~not_found
    ~cell_field
    ~empty_cell
    ~load_cell
    ~cell_json
    ~current_epoch =
  with_owner_locator_auth
    store
    params
    ~field:"state_ref"
    ~op
    ~parse_locator:Circle_view.state_locator
    (fun circle_id locator ->
      private_state_cell
        store
        ~circle_id
        ~locator
        ~current_epoch
        ~not_found
        ~cell_field
        ~empty_cell
        ~load_cell
        ~cell_json)

let balance_cell_auth_params store params ~current_epoch =
  private_state_cell_auth_params
    store
    params
    ~op:"octra_circle_balance_cell"
    ~not_found:"circle balance cell not found"
    ~cell_field:"balance_cell"
    ~empty_cell:Octra_core.Circle_balance_cell.empty
    ~load_cell:Octra_core.Circle_balance_cell_store.load_opt
    ~cell_json:Octra_core.Circle_balance_cell.yojson_of_t
    ~current_epoch

let register_cell_auth_params store params ~current_epoch =
  private_state_cell_auth_params
    store
    params
    ~op:"octra_circle_register_cell"
    ~not_found:"circle register cell not found"
    ~cell_field:"register_cell"
    ~empty_cell:Octra_core.Circle_register_cell.empty
    ~load_cell:Octra_core.Circle_register_cell_store.load_opt
    ~cell_json:Octra_core.Circle_register_cell.yojson_of_t
    ~current_epoch

let descriptor_for_state_ref store circle_id = function
  | None ->
    Lwt.return_none
  | Some state_ref ->
    begin
      match Octra_core.Circles.path_key_of_state_ref state_ref with
      | Error _ ->
        Lwt.return_none
      | Ok (_, _, path_key) ->
        Octra_core.Circle_state_descriptor_store.load_descriptor_opt
          store
          circle_id
          path_key
    end

let binding
    store
    ~circle_id
    ~binding_field
    ~binding_key_field
    ~binding_key_value
    ~empty_binding
    ~load_binding
    ~binding_json
    ~current_state_ref =
  let open Lwt.Syntax in
  let* binding_opt =
    load_binding store circle_id binding_key_value in
  let binding =
    Option.value ~default:empty_binding binding_opt in
  let state_ref = current_state_ref binding in
  let* descriptor_opt =
    descriptor_for_state_ref store circle_id state_ref in
  ok
    (Circle_view.binding
       ~circle_id
       ~binding_field
       ~binding_key_field
       ~binding_key_value
       ~binding_json:(binding_json binding)
       ~binding_materialized:(Option.is_some binding_opt)
       ~current_state_ref:state_ref
       ~descriptor_opt)

let validate_subject_addr subject_addr =
  if Octra_core.Crypto.Address.is_valid_address subject_addr then
    Ok subject_addr
  else
    Error "subject_addr must be a valid octra address"

let balance_binding_auth_params store params =
  with_owner_validated_subject_auth
    store
    params
    ~field:"subject_addr"
    ~op:"octra_circle_balance_binding"
    ~validate:validate_subject_addr
    (fun circle_id subject_addr ->
      binding
        store
        ~circle_id
        ~binding_field:"balance_binding"
        ~binding_key_field:"subject_addr"
        ~binding_key_value:subject_addr
        ~empty_binding:Octra_core.Circle_balance_binding.empty
        ~load_binding:Octra_core.Circle_balance_binding_store.load_opt
        ~binding_json:Octra_core.Circle_balance_binding.yojson_of_t
        ~current_state_ref:(fun (binding : Octra_core.Circle_balance_binding.t) ->
          binding.current_state_ref))

let register_binding_auth_params store params =
  with_owner_validated_subject_auth
    store
    params
    ~field:"register_ref"
    ~op:"octra_circle_register_binding"
    ~validate:(Octra_core.Circle_private_common.normalize_hex64 "register_ref")
    (fun circle_id register_ref ->
      binding
        store
        ~circle_id
        ~binding_field:"register_binding"
        ~binding_key_field:"register_ref"
        ~binding_key_value:register_ref
        ~empty_binding:Octra_core.Circle_register_binding.empty
        ~load_binding:Octra_core.Circle_register_binding_store.load_opt
        ~binding_json:Octra_core.Circle_register_binding.yojson_of_t
        ~current_state_ref:(fun (binding : Octra_core.Circle_register_binding.t) ->
          binding.current_state_ref))

let workflow
    store
    ~circle_id
    ~workflow_field
    ~workflow_ref
    ~empty_workflow
    ~load_workflow
    ~workflow_json =
  let open Lwt.Syntax in
  let* workflow_opt =
    load_workflow store circle_id workflow_ref in
  let workflow =
    Option.value ~default:empty_workflow workflow_opt in
  ok
    (Circle_view.workflow
       ~circle_id
       ~workflow_field
       ~workflow_ref
       ~workflow_json:(workflow_json workflow)
       ~workflow_materialized:(Option.is_some workflow_opt))

let balance_workflow_auth_params store params =
  with_owner_validated_subject_auth
    store
    params
    ~field:"workflow_ref"
    ~op:"octra_circle_balance_workflow"
    ~validate:(Octra_core.Circle_private_common.normalize_hex64 "workflow_ref")
    (fun circle_id workflow_ref ->
      workflow
        store
        ~circle_id
        ~workflow_field:"balance_workflow"
        ~workflow_ref
        ~empty_workflow:Octra_core.Circle_balance_workflow.empty
        ~load_workflow:Octra_core.Circle_balance_workflow_store.load_opt
        ~workflow_json:Octra_core.Circle_balance_workflow.yojson_of_t)

let register_workflow_auth_params store params =
  with_owner_validated_subject_auth
    store
    params
    ~field:"workflow_ref"
    ~op:"octra_circle_register_workflow"
    ~validate:(Octra_core.Circle_private_common.normalize_hex64 "workflow_ref")
    (fun circle_id workflow_ref ->
      workflow
        store
        ~circle_id
        ~workflow_field:"register_workflow"
        ~workflow_ref
        ~empty_workflow:Octra_core.Circle_register_workflow.empty
        ~load_workflow:Octra_core.Circle_register_workflow_store.load_opt
        ~workflow_json:Octra_core.Circle_register_workflow.yojson_of_t)

let storage store ~circle_id ~key =
  let open Lwt.Syntax in
  let* value_opt =
    Octra_circle_runtime.Circle_exec.read_storage_key store circle_id key in
  ok (Circle_view.storage ~circle_id ~key ~value:value_opt)

let storage_dump_limit = 128

let storage_dump store ~circle_id =
  let open Lwt.Syntax in
  let* result =
    Octra_circle_runtime.Circle_exec.list_storage_page
      store
      circle_id
      ~limit:storage_dump_limit in
  match
    query_result
      result
      (fun (storage_pairs, total) ->
        Circle_view.storage_dump ~circle_id ~total storage_pairs)
  with
  | Ok value -> ok value
  | Error e -> Lwt.return (Error e)

let outbox_claim store ~circle_id ~intent_id ~current_epoch =
  let open Lwt.Syntax in
  let* claims =
    Octra_core.Circle_transport_state.get_outbox_claims
      store
      circle_id
      intent_id in
  let* active_claims =
    Octra_core.Circle_transport_state.active_outbox_claims
      store
      circle_id
      intent_id
      current_epoch in
  match claims with
  | [] ->
    Lwt.return (Error (Rpc.not_found "circle outbox claim not found"))
  | claims ->
    ok
      (Circle_view.outbox_claim_of_state
         ~circle_id
         ~claims
         ~active_claims)

let outbox_intent store ~circle_id ~intent_id =
  let open Lwt.Syntax in
  let* intent_opt =
    Octra_core.Store_irmin.get_circle_outbox_intent store circle_id intent_id in
  Lwt.return
    (option_result
       intent_opt
       ~not_found:"circle outbox intent not found"
       (Circle_view.outbox_intent ~circle_id))

let outbox_status store ~circle_id ~intent_id ~current_epoch =
  let open Lwt.Syntax in
  let* intent_opt =
    Octra_core.Store_irmin.get_circle_outbox_intent store circle_id intent_id in
  let* status_opt =
    Octra_core.Store_irmin.get_circle_outbox_status store circle_id intent_id in
  match intent_opt, status_opt with
  | Some intent, Some status ->
    let* active_claims =
      Octra_core.Circle_transport_state.active_outbox_claims
        store
        circle_id
        intent_id
        current_epoch in
    let* claims =
      Octra_core.Circle_transport_state.get_outbox_claims
        store
        circle_id
        intent_id in
    let* stored_resolution =
      Octra_core.Circle_transport_state.get_outbox_resolution
        store
        circle_id
        intent_id in
    let* transport_policy =
      Octra_core.Circle_policy_store.load_transport_policy store circle_id in
    let* delivery_key_status =
      Octra_core.Circle_transport_state.delivery_key_status
        store
        circle_id
        current_epoch
        intent in
    ok
      (Circle_view.outbox_status_of_state
         ~circle_id
         ~intent_id
         ~current_epoch
         ~intent
         ~stored_status:status
         ~stored_resolution
         ~delivery_key_status
         ~transport_policy
         ~active_claims
         ~claims)
  | _ ->
    Lwt.return (Error (Rpc.not_found "circle outbox status not found"))

let ingress_packet store ~circle_id ~intent_id =
  let open Lwt.Syntax in
  let* packet_opt =
    Octra_core.Store_irmin.get_circle_ingress_packet store circle_id intent_id in
  Lwt.return
    (option_result
       packet_opt
       ~not_found:"circle ingress packet not found"
       (Circle_view.ingress_packet ~circle_id))

let owner_scope_read store params ~op read =
  with_owner_scope_auth store params ~op (fun circle_id ->
    read store ~circle_id)

let owner_subject_read store params ~field ~op read =
  with_owner_subject_auth store params ~field ~op (fun circle_id subject ->
    read store ~circle_id subject)

let transport_policy_auth store params =
  owner_scope_read
    store
    params
    ~op:"octra_circle_transport_policy"
    transport_policy

let hfhe_policy_auth store params =
  owner_scope_read
    store
    params
    ~op:"octra_circle_hfhe_policy"
    hfhe_policy

let key_policy_auth store params ~current_epoch =
  owner_subject_read
    store
    params
    ~field:"key_id"
    ~op:"octra_circle_key_policy"
    (fun store ~circle_id key_id ->
      key_policy store ~circle_id ~key_id ~current_epoch)

let storage_auth store params =
  owner_subject_read
    store
    params
    ~field:"key"
    ~op:"octra_circle_storage"
    (fun store ~circle_id key ->
      storage store ~circle_id ~key)

let storage_dump_auth store params =
  owner_scope_read
    store
    params
    ~op:"octra_circle_storage_dump"
    storage_dump

let outbox_claim_auth store params ~current_epoch =
  owner_subject_read
    store
    params
    ~field:"intent_id"
    ~op:"octra_circle_outbox_claim"
    (fun store ~circle_id intent_id ->
      outbox_claim store ~circle_id ~intent_id ~current_epoch)

let outbox_intent_auth store params =
  owner_subject_read
    store
    params
    ~field:"intent_id"
    ~op:"octra_circle_outbox_intent"
    (fun store ~circle_id intent_id ->
      outbox_intent store ~circle_id ~intent_id)

let outbox_status_auth store params ~current_epoch =
  owner_subject_read
    store
    params
    ~field:"intent_id"
    ~op:"octra_circle_outbox_status"
    (fun store ~circle_id intent_id ->
      outbox_status store ~circle_id ~intent_id ~current_epoch)

let ingress_packet_auth store params =
  owner_subject_read
    store
    params
    ~field:"intent_id"
    ~op:"octra_circle_ingress_packet"
    (fun store ~circle_id intent_id ->
      ingress_packet store ~circle_id ~intent_id)

let dispatch
    (adapters : (Yojson.Safe.t -> 'ctx -> rpc_result Lwt.t) dispatch_adapters) =
  Rpc_dispatch.circle_routes Rpc_dispatch.{
    circle_info = adapters.store_read info_public_params;
    circle_info_auth = adapters.store_read info_auth_params;
    circle_program_info = adapters.store_read program_info_public_params;
    circle_program_info_auth = adapters.store_read program_descriptor_auth_params;
    circle_asset = adapters.store_read asset_plaintext_params;
    circle_view = adapters.circle_view;
    circle_view_auth = adapters.circle_view_auth;
    circle_slot_policy = slot_policy_required_route;
    circle_slot_policy_auth = adapters.epoch_read slot_locator_policy_auth_params;
    circle_state_policy = state_policy_required_route;
    circle_state_policy_auth = adapters.epoch_read state_locator_policy_auth_params;
    circle_state_descriptor = state_descriptor_required_route;
    circle_state_descriptor_auth = adapters.store_read state_descriptor_auth_params;
    circle_balance_cell = balance_cell_required_route;
    circle_balance_cell_auth = adapters.epoch_read balance_cell_auth_params;
    circle_register_cell = register_cell_required_route;
    circle_register_cell_auth = adapters.epoch_read register_cell_auth_params;
    circle_balance_binding = balance_binding_required_route;
    circle_balance_binding_auth = adapters.store_read balance_binding_auth_params;
    circle_register_binding = register_binding_required_route;
    circle_register_binding_auth = adapters.store_read register_binding_auth_params;
    circle_balance_workflow = balance_workflow_required_route;
    circle_balance_workflow_auth = adapters.store_read balance_workflow_auth_params;
    circle_register_workflow = register_workflow_required_route;
    circle_register_workflow_auth = adapters.store_read register_workflow_auth_params;
    circle_object_summary = object_summary_required_route;
    circle_object_summary_auth = adapters.store_read object_summary_auth_params;
    circle_object_members = object_members_required_route;
    circle_object_members_auth = adapters.store_read object_members_auth_params;
    circle_object_detail = object_detail_required_route;
    circle_object_detail_auth = adapters.store_read object_detail_auth_params;
    circle_object_member = object_member_required_route;
    circle_object_member_auth = adapters.store_read object_member_auth_params;
    circle_object_refs = object_refs_required_route;
    circle_object_refs_auth = adapters.store_read object_refs_auth_params;
    circle_object_list = object_list_required_route;
    circle_object_list_auth = adapters.store_read object_list_auth_params;
    circle_transport_policy = transport_policy_required_route;
    circle_transport_policy_auth = adapters.store_read transport_policy_auth;
    circle_hfhe_policy = hfhe_policy_required_route;
    circle_hfhe_policy_auth = adapters.store_read hfhe_policy_auth;
    circle_key_policy = key_policy_required_route;
    circle_key_policy_auth = adapters.epoch_read key_policy_auth;
    circle_storage = storage_required_route;
    circle_storage_auth = adapters.store_read storage_auth;
    circle_storage_dump = storage_dump_required_route;
    circle_storage_dump_auth = adapters.store_read storage_dump_auth;
    circle_outbox_claim = outbox_claim_required_route;
    circle_outbox_claim_auth = adapters.epoch_read outbox_claim_auth;
    circle_outbox_intent = outbox_intent_required_route;
    circle_outbox_intent_auth = adapters.store_read outbox_intent_auth;
    circle_outbox_status = outbox_status_required_route;
    circle_outbox_status_auth = adapters.epoch_read outbox_status_auth;
    circle_ingress_packet = ingress_packet_required_route;
    circle_ingress_packet_auth = adapters.store_read ingress_packet_auth;
    circle_asset_ciphertext = adapters.public_asset_read asset_ciphertext_params;
    circle_asset_ciphertext_by_resource_key =
      adapters.public_asset_read asset_ciphertext_by_resource_key_params;
    circle_asset_ciphertext_by_slot_ref =
      adapters.public_asset_read asset_ciphertext_by_slot_ref_params;
    circle_asset_ciphertext_by_state_ref =
      adapters.public_asset_read asset_ciphertext_by_state_ref_params;
    circle_asset_ciphertext_auth = adapters.epoch_read asset_ciphertext_auth_params;
    circle_asset_ciphertext_by_resource_key_auth =
      adapters.epoch_read asset_ciphertext_by_resource_key_auth_params;
    circle_asset_ciphertext_by_slot_ref_auth =
      adapters.epoch_read asset_ciphertext_by_slot_ref_auth_params;
    circle_asset_ciphertext_by_state_ref_auth =
      adapters.epoch_read asset_ciphertext_by_state_ref_auth_params;
    circle_program = adapters.store_read program_params;
    circle_program_auth = adapters.store_read program_auth_params;
  }