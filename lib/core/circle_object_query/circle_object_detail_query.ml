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

type member_detail = {
  member_ref : string;
  member_state : Circle_object_member.t;
  descriptor : Circle_state_descriptor.t option;
}

type detail = {
  summary : Circle_object_query.summary;
  current_descriptor : Circle_state_descriptor.t option;
  last_transition : Circle_object_transition.t option;
}

let load_descriptor_opt_from_storage_tbl storage_tbl path_key =
  let descriptor =
    Circle_state_descriptor.of_stored_values
      ~state_class_raw:(Hashtbl.find_opt storage_tbl (Circle_state_descriptor.state_class_key path_key))
      ~codec_raw:(Hashtbl.find_opt storage_tbl (Circle_state_descriptor.codec_key path_key))
      ~schema_hash_raw:(Hashtbl.find_opt storage_tbl (Circle_state_descriptor.schema_hash_key path_key))
      ~subject_addr_raw:(Hashtbl.find_opt storage_tbl (Circle_state_descriptor.subject_addr_key path_key))
      ~hfhe_profile_raw:(Hashtbl.find_opt storage_tbl (Circle_state_descriptor.hfhe_profile_key path_key))
      ~mutable_state_raw:(Hashtbl.find_opt storage_tbl (Circle_state_descriptor.mutable_state_key path_key))
  in
  if descriptor = Circle_state_descriptor.default then
    None
  else
    Some descriptor

let load_descriptor_opt_for_state_ref storage_tbl state_ref =
  match Circles.path_key_of_state_ref state_ref with
  | Error _ ->
    None
  | Ok (_, _, path_key) ->
    load_descriptor_opt_from_storage_tbl storage_tbl path_key

let load_transition_opt_from_storage_tbl storage_tbl transition_ref =
  let transition =
    Circle_object_transition.of_stored_values
      ~object_ref_raw:(Hashtbl.find_opt storage_tbl (Circle_object_transition.object_ref_key transition_ref))
      ~previous_state_ref_raw:(Hashtbl.find_opt storage_tbl (Circle_object_transition.previous_state_ref_key transition_ref))
      ~next_state_ref_raw:(Hashtbl.find_opt storage_tbl (Circle_object_transition.next_state_ref_key transition_ref))
      ~touched_members_hash_raw:(Hashtbl.find_opt storage_tbl (Circle_object_transition.touched_members_hash_key transition_ref))
      ~proof_kind_raw:(Hashtbl.find_opt storage_tbl (Circle_object_transition.proof_kind_key transition_ref))
      ~proof_receipt_hash_raw:(Hashtbl.find_opt storage_tbl (Circle_object_transition.proof_receipt_hash_key transition_ref))
      ~status_raw:(Hashtbl.find_opt storage_tbl (Circle_object_transition.status_key transition_ref))
      ~intent_id_raw:(Hashtbl.find_opt storage_tbl (Circle_object_transition.intent_id_key transition_ref))
  in
  if Circle_object_transition.materialized transition then
    Some transition
  else
    None

let load_member_detail_from_storage_tbl storage_tbl object_ref member_ref =
  match Circle_object_member_query.load_member_from_storage_tbl storage_tbl object_ref member_ref with
  | Some member_state ->
    {
      member_ref;
      member_state;
      descriptor =
        begin
          match member_state.Circle_object_member.state_ref with
          | Some state_ref ->
            load_descriptor_opt_for_state_ref storage_tbl state_ref
          | None ->
            None
        end;
    }
  | None ->
    {
      member_ref;
      member_state = Circle_object_member.empty;
      descriptor = None;
    }

let load_member_details_from_storage_tbl storage_tbl object_ref =
  Circle_object_member_query.member_refs_from_storage_tbl storage_tbl object_ref
  |> List.map (load_member_detail_from_storage_tbl storage_tbl object_ref)

let load_detail_from_storage_tbl storage_tbl object_ref =
  let summary =
    Circle_object_query.load_summary_from_storage_tbl storage_tbl object_ref in
  let current_descriptor =
    match summary.binding.Circle_object_binding.current_state_ref with
    | Some state_ref ->
      load_descriptor_opt_for_state_ref storage_tbl state_ref
    | None ->
      None in
  let last_transition =
    match summary.binding.Circle_object_binding.last_transition_ref with
    | Some transition_ref ->
      load_transition_opt_from_storage_tbl storage_tbl transition_ref
    | None ->
      None in
  {
    summary;
    current_descriptor;
    last_transition;
  }

let load_member_detail store circle_id object_ref member_ref =
  let* storage_result = Store_irmin.load_circle_stable_storage store circle_id in
  match storage_result with
  | Error message ->
    Lwt.return (Error message)
  | Ok storage_tbl ->
    Lwt.return (Ok (load_member_detail_from_storage_tbl storage_tbl object_ref member_ref))

let load_member_details store circle_id object_ref =
  let* storage_result = Store_irmin.load_circle_stable_storage store circle_id in
  match storage_result with
  | Error message ->
    Lwt.return (Error message)
  | Ok storage_tbl ->
    Lwt.return (Ok (load_member_details_from_storage_tbl storage_tbl object_ref))

let load_detail store circle_id object_ref =
  let* storage_result = Store_irmin.load_circle_stable_storage store circle_id in
  match storage_result with
  | Error message ->
    Lwt.return (Error message)
  | Ok storage_tbl ->
    Lwt.return (Ok (load_detail_from_storage_tbl storage_tbl object_ref))