(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type write =
  | Set of string * string
  | Del of string

type result = {
  version : int64;
  writes : write list;
  active_member_count : int;
}

let bool_value = function
  | Some true -> true
  | Some false
  | None ->
    false

let i64_value default = function
  | Some value -> value
  | None -> default

let read_opt storage_tbl key =
  Hashtbl.find_opt storage_tbl key

let load_binding storage_tbl object_ref =
  Circle_object_binding.of_stored_values
    ~current_state_ref_raw:(read_opt storage_tbl (Circle_object_binding.current_state_ref_key object_ref))
    ~version_raw:(read_opt storage_tbl (Circle_object_binding.version_key object_ref))
    ~status_raw:(read_opt storage_tbl (Circle_object_binding.status_key object_ref))
    ~last_transition_ref_raw:(read_opt storage_tbl (Circle_object_binding.last_transition_ref_key object_ref))

let load_policy storage_tbl object_ref =
  Circle_object_policy.of_stored_values
    ~delivery_key_id_raw:(read_opt storage_tbl (Circle_object_policy.delivery_key_id_key object_ref))
    ~activate_after_epoch_raw:(read_opt storage_tbl (Circle_object_policy.activate_after_epoch_key object_ref))
    ~expire_after_epoch_raw:(read_opt storage_tbl (Circle_object_policy.expire_after_epoch_key object_ref))
    ~tombstone_raw:(read_opt storage_tbl (Circle_object_policy.tombstone_key object_ref))
    ~revoked_raw:(read_opt storage_tbl (Circle_object_policy.revoked_key object_ref))
    ~transition_mode_raw:(read_opt storage_tbl (Circle_object_policy.transition_mode_key object_ref))
    ~required_proof_kind_raw:(read_opt storage_tbl (Circle_object_policy.required_proof_kind_key object_ref))
    ~member_quorum_raw:(read_opt storage_tbl (Circle_object_policy.member_quorum_key object_ref))
    ~allow_detach_raw:(read_opt storage_tbl (Circle_object_policy.allow_detach_key object_ref))
    ~allow_root_state_rotation_raw:(read_opt storage_tbl (Circle_object_policy.allow_root_state_rotation_key object_ref))

let hash_member_bundle member_bundle =
  Digestif.SHA256.digest_string member_bundle
  |> Digestif.SHA256.to_hex

let write_snapshot_as_ops snapshot_tbl =
  Hashtbl.fold
    (fun key value acc -> (key, value) :: acc)
    snapshot_tbl
    []
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)
  |> List.map (fun (key, value) -> Set (key, value))

let writes_of_member_delta object_ref (delta : Circle_object_member_delta.t) =
  match delta.operation with
  | Circle_object_member_delta.Attach ->
    let snapshot_tbl = Hashtbl.create 8 in
    Circle_object_member_delta.write_snapshot snapshot_tbl object_ref delta;
    write_snapshot_as_ops snapshot_tbl
  | Circle_object_member_delta.Detach ->
    [
      Del (Circle_object_member.state_ref_key object_ref delta.member_ref);
      Del (Circle_object_member.member_kind_key object_ref delta.member_ref);
      Del (Circle_object_member.state_class_key object_ref delta.member_ref);
      Del (Circle_object_member.codec_key object_ref delta.member_ref);
      Del (Circle_object_member.status_key object_ref delta.member_ref);
    ]

let collect_existing_members storage_tbl object_ref =
  let prefix = "object_member:" ^ object_ref ^ ":" in
  let suffix = ":state_ref" in
  let prefix_len = String.length prefix in
  let suffix_len = String.length suffix in
  Hashtbl.fold
    (fun key _value acc ->
      if String.starts_with ~prefix key && String.ends_with ~suffix key then
        let member_len = String.length key - prefix_len - suffix_len in
        if member_len > 0 then
          String.sub key prefix_len member_len :: acc
        else
          acc
      else
        acc)
    storage_tbl
    []

let active_members_after_deltas members deltas =
  let active = Hashtbl.create 16 in
  List.iter
    (fun member_ref -> Hashtbl.replace active member_ref true)
    members;
  List.iter
    (fun (delta : Circle_object_member_delta.t) ->
      match delta.operation with
      | Circle_object_member_delta.Attach ->
        Hashtbl.replace active delta.member_ref true
      | Circle_object_member_delta.Detach ->
        Hashtbl.remove active delta.member_ref)
    deltas;
  Hashtbl.fold (fun member_ref _value acc -> member_ref :: acc) active []

let validate_unique_member_refs deltas =
  let seen = Hashtbl.create 16 in
  let rec go = function
    | [] -> Ok ()
    | (delta : Circle_object_member_delta.t) :: rest ->
      if Hashtbl.mem seen delta.member_ref then
        Error "object member bundle contains duplicate member_ref"
      else begin
        Hashtbl.replace seen delta.member_ref true;
        go rest
      end
  in
  go deltas

let validate_policy_window current_epoch (policy : Circle_object_policy.t) =
  let current_epoch_i64 = Int64.of_int current_epoch in
  if bool_value policy.tombstone then
    Error "object policy is tombstoned"
  else if bool_value policy.revoked then
    Error "object policy is revoked"
  else
    begin
      match policy.activate_after_epoch, policy.expire_after_epoch with
      | Some activate_after_epoch, _ when Int64.compare current_epoch_i64 activate_after_epoch < 0 ->
        Error "object policy is not active yet"
      | _, Some expire_after_epoch when Int64.compare current_epoch_i64 expire_after_epoch >= 0 ->
        Error "object policy has expired"
      | _ ->
        Ok ()
    end

let validate_transition_policy
    (policy : Circle_object_policy.t)
    ~current_state_ref
    ~next_state_ref
    ~proof_kind
    ~detached_members =
  if not (Circle_object_policy.transition_controls_materialized policy) then
    Error "object transition policy is incomplete"
  else
    let transition_mode =
      match policy.transition_mode with
      | Some value -> value
      | None -> Circle_object_transition_mode.Frozen in
    let allow_detach =
      match policy.allow_detach with
      | Some value -> value
      | None -> false in
    let allow_root_state_rotation =
      match policy.allow_root_state_rotation with
      | Some value -> value
      | None -> false in
    if detached_members > 0 && not allow_detach then
      Error "object policy forbids member detach"
    else if
      not allow_root_state_rotation
      &&
      match current_state_ref with
      | Some current_ref -> current_ref <> next_state_ref
      | None -> false
    then
      Error "object policy forbids root state rotation"
    else
      begin
        match transition_mode with
        | Circle_object_transition_mode.Frozen ->
          Error "object policy is frozen"
        | Circle_object_transition_mode.Proof_required ->
          begin
            match proof_kind with
            | Some _ -> Ok ()
            | None -> Error "object policy requires a transition proof"
          end
        | Circle_object_transition_mode.Open ->
          Ok ()
      end

let validate_quorum (policy : Circle_object_policy.t) ~bootstrap ~active_member_count ~next_active_member_count =
  match policy.member_quorum with
  | Some member_quorum ->
    let required = Int64.to_int member_quorum in
    let satisfied_count =
      if bootstrap then
        next_active_member_count
      else
        active_member_count in
    if satisfied_count < required then
      Error "object member quorum not met"
    else
      Ok ()
  | None ->
    Ok ()

let validate_required_proof_kind (policy : Circle_object_policy.t) proof_kind =
  match policy.required_proof_kind, proof_kind with
  | None, _ ->
    Ok ()
  | Some required, Some actual when required = actual ->
    Ok ()
  | Some _, _ ->
    Error "object policy proof kind mismatch"

let normalize_transition_inputs
    ~transition_ref
    ~object_ref
    ~previous_state_ref
    ~next_state_ref
    ~touched_members_hash
    ~proof_kind_raw
    ~proof_receipt_hash_raw
    ~status
    ~intent_id =
  match
    Circle_private_common.normalize_hex64 "transition_ref" transition_ref,
    Circle_private_common.normalize_hex64 "object_ref" object_ref,
    Circles.normalize_state_ref previous_state_ref,
    Circles.normalize_state_ref next_state_ref,
    Circle_private_common.normalize_hex64 "touched_members_hash" touched_members_hash,
    Circle_private_common.normalize_non_empty "status" status,
    Circle_private_common.normalize_hex64 "intent_id" intent_id
  with
  | Ok transition_ref, Ok object_ref, Ok previous_state_ref, Ok next_state_ref, Ok touched_members_hash, Ok status, Ok intent_id ->
    let proof_kind =
      match Circle_hfhe_proof.of_string (String.trim proof_kind_raw) with
      | Ok Circle_hfhe_proof.No_proof -> Ok None
      | Ok value -> Ok (Some value)
      | Error message -> Error message in
    let proof_receipt_hash =
      if String.trim proof_receipt_hash_raw = "" then
        Ok None
      else
        Circle_private_common.normalize_optional_hex64 "proof_receipt_hash" (Some proof_receipt_hash_raw) in
    begin
      match proof_kind, proof_receipt_hash with
      | Ok proof_kind, Ok proof_receipt_hash ->
        begin
          match Circle_private_common.validate_proof_receipt_binding ~proof_kind ~proof_receipt_hash with
          | Ok () ->
            Ok
              ( transition_ref,
                object_ref,
                previous_state_ref,
                next_state_ref,
                touched_members_hash,
                proof_kind,
                proof_receipt_hash,
                status,
                intent_id )
          | Error message ->
            Error message
        end
      | Error message, _
      | _, Error message ->
        Error message
    end
  | Error message, _, _, _, _, _, _
  | _, Error message, _, _, _, _, _
  | _, _, Error message, _, _, _, _
  | _, _, _, Error message, _, _, _
  | _, _, _, _, Error message, _, _
  | _, _, _, _, _, Error message, _
  | _, _, _, _, _, _, Error message ->
    Error message

let apply
    ?member_refs
    ~current_epoch
    ~storage_tbl
    ~transition_ref
    ~object_ref
    ~previous_state_ref
    ~next_state_ref
    ~member_bundle
    ~touched_members_hash
    ~proof_kind_raw
    ~proof_receipt_hash_raw
    ~status
    ~intent_id
    () =
  match
    normalize_transition_inputs
      ~transition_ref
      ~object_ref
      ~previous_state_ref
      ~next_state_ref
      ~touched_members_hash
      ~proof_kind_raw
      ~proof_receipt_hash_raw
      ~status
      ~intent_id
  with
  | Error message ->
    Error message
  | Ok
      ( transition_ref,
        object_ref,
        previous_state_ref,
        next_state_ref,
        touched_members_hash,
        proof_kind,
        proof_receipt_hash,
        status,
        intent_id ) ->
    begin
      match Circle_object_member_delta.parse_bundle member_bundle with
      | Error message ->
        Error message
      | Ok deltas ->
        begin
          match validate_unique_member_refs deltas with
          | Error message ->
            Error message
          | Ok () ->
            let computed_hash = hash_member_bundle member_bundle in
            if computed_hash <> touched_members_hash then
              Error "object member bundle hash mismatch"
            else
              let binding = load_binding storage_tbl object_ref in
              let policy = load_policy storage_tbl object_ref in
              let detached_members =
                List.fold_left
                  (fun acc (delta : Circle_object_member_delta.t) ->
                    match delta.operation with
                    | Circle_object_member_delta.Detach -> acc + 1
                    | Circle_object_member_delta.Attach -> acc)
                  0
                  deltas in
              let active_members_before =
                match member_refs with
                | Some load -> load ()
                | None -> collect_existing_members storage_tbl object_ref in
              let active_members =
                active_members_after_deltas active_members_before deltas in
              let bootstrap =
                Option.is_none binding.current_state_ref
                && active_members_before = [] in
              begin
                match
                  validate_policy_window current_epoch policy,
                  validate_transition_policy
                    policy
                    ~current_state_ref:binding.current_state_ref
                    ~next_state_ref
                    ~proof_kind
                    ~detached_members,
                  validate_required_proof_kind policy proof_kind,
                  validate_quorum
                    policy
                    ~bootstrap
                    ~active_member_count:(List.length active_members_before)
                    ~next_active_member_count:(List.length active_members)
                with
                | Ok (), Ok (), Ok (), Ok () ->
                  begin
                    match binding.current_state_ref with
                    | Some current_state_ref when current_state_ref <> previous_state_ref ->
                      Error "object transition previous_state_ref mismatch"
                    | _ ->
                      let version =
                        Int64.succ (i64_value 0L binding.version) in
                      let transition_snapshot = Hashtbl.create 16 in
                      let binding_snapshot = Hashtbl.create 16 in
                      Circle_object_transition.write_snapshot
                        transition_snapshot
                        transition_ref
                        {
                          Circle_object_transition.object_ref = Some object_ref;
                          previous_state_ref = Some previous_state_ref;
                          next_state_ref = Some next_state_ref;
                          touched_members_hash = Some touched_members_hash;
                          proof_kind;
                          proof_receipt_hash;
                          status = Some status;
                          intent_id = Some intent_id;
                        };
                      Circle_object_binding.write_snapshot
                        binding_snapshot
                        object_ref
                        {
                          Circle_object_binding.current_state_ref = Some next_state_ref;
                          version = Some version;
                          status = Some status;
                          last_transition_ref = Some transition_ref;
                        };
                      Ok {
                        version;
                        writes =
                          write_snapshot_as_ops transition_snapshot
                          @ write_snapshot_as_ops binding_snapshot
                          @ List.concat_map (writes_of_member_delta object_ref) deltas;
                        active_member_count = List.length active_members;
                      }
                  end
                | Error message, _, _, _
                | _, Error message, _, _
                | _, _, Error message, _
                | _, _, _, Error message ->
                  Error message
              end
        end
    end