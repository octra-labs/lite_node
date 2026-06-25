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

type member_summary = {
  member_ref : string;
  member_state : Circle_object_member.t;
}

type summary = {
  object_ref : string;
  binding : Circle_object_binding.t;
  policy : Circle_object_policy.t option;
  active_member_count : int;
  member_refs : string list;
}

let load_binding_from_storage_tbl storage_tbl object_ref =
  Circle_object_binding.of_stored_values
    ~current_state_ref_raw:(Hashtbl.find_opt storage_tbl (Circle_object_binding.current_state_ref_key object_ref))
    ~version_raw:(Hashtbl.find_opt storage_tbl (Circle_object_binding.version_key object_ref))
    ~status_raw:(Hashtbl.find_opt storage_tbl (Circle_object_binding.status_key object_ref))
    ~last_transition_ref_raw:(Hashtbl.find_opt storage_tbl (Circle_object_binding.last_transition_ref_key object_ref))

let load_policy_from_storage_tbl storage_tbl object_ref =
  let policy =
    Circle_object_policy.of_stored_values
      ~delivery_key_id_raw:(Hashtbl.find_opt storage_tbl (Circle_object_policy.delivery_key_id_key object_ref))
      ~activate_after_epoch_raw:(Hashtbl.find_opt storage_tbl (Circle_object_policy.activate_after_epoch_key object_ref))
      ~expire_after_epoch_raw:(Hashtbl.find_opt storage_tbl (Circle_object_policy.expire_after_epoch_key object_ref))
      ~tombstone_raw:(Hashtbl.find_opt storage_tbl (Circle_object_policy.tombstone_key object_ref))
      ~revoked_raw:(Hashtbl.find_opt storage_tbl (Circle_object_policy.revoked_key object_ref))
      ~transition_mode_raw:(Hashtbl.find_opt storage_tbl (Circle_object_policy.transition_mode_key object_ref))
      ~required_proof_kind_raw:(Hashtbl.find_opt storage_tbl (Circle_object_policy.required_proof_kind_key object_ref))
      ~member_quorum_raw:(Hashtbl.find_opt storage_tbl (Circle_object_policy.member_quorum_key object_ref))
      ~allow_detach_raw:(Hashtbl.find_opt storage_tbl (Circle_object_policy.allow_detach_key object_ref))
      ~allow_root_state_rotation_raw:(Hashtbl.find_opt storage_tbl (Circle_object_policy.allow_root_state_rotation_key object_ref))
  in
  if Circle_object_policy.materialized policy then
    Some policy
  else
    None

let load_member_summaries_from_storage_tbl storage_tbl object_ref =
  Circle_object_member_query.member_refs_from_storage_tbl storage_tbl object_ref
  |> List.filter_map (fun member_ref ->
    match Circle_object_member_query.load_member_from_storage_tbl storage_tbl object_ref member_ref with
    | Some member_state ->
      Some {
        member_ref;
        member_state;
      }
    | None ->
      None)

let load_summary_from_storage_tbl storage_tbl object_ref =
  let binding = load_binding_from_storage_tbl storage_tbl object_ref in
  let member_refs =
    Circle_object_member_query.member_refs_from_storage_tbl storage_tbl object_ref in
  {
    object_ref;
    binding;
    policy = load_policy_from_storage_tbl storage_tbl object_ref;
    active_member_count = List.length member_refs;
    member_refs;
  }

let load_summary store circle_id object_ref =
  let* storage_result = Store_irmin.load_circle_stable_storage store circle_id in
  match storage_result with
  | Error message ->
    Lwt.return (Error message)
  | Ok storage_tbl ->
    Lwt.return (Ok (load_summary_from_storage_tbl storage_tbl object_ref))

let load_member_summaries store circle_id object_ref =
  let* storage_result = Store_irmin.load_circle_stable_storage store circle_id in
  match storage_result with
  | Error message ->
    Lwt.return (Error message)
  | Ok storage_tbl ->
    Lwt.return (Ok (load_member_summaries_from_storage_tbl storage_tbl object_ref))