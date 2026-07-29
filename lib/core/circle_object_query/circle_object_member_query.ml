(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let state_ref_key_suffix = ":state_ref"

let member_prefix object_ref =
  "object_member:" ^ object_ref ^ ":"

let member_ref_of_key object_ref raw_key =
  let prefix = member_prefix object_ref in
  let prefix_len = String.length prefix in
  let suffix_len = String.length state_ref_key_suffix in
  if String.starts_with ~prefix raw_key && String.ends_with ~suffix:state_ref_key_suffix raw_key then
    let member_len = String.length raw_key - prefix_len - suffix_len in
    if member_len > 0 then
      Some (String.sub raw_key prefix_len member_len)
    else
      None
  else
    None

let member_refs_from_storage_tbl storage_tbl object_ref =
  Hashtbl.fold
    (fun raw_key _value acc ->
      match member_ref_of_key object_ref raw_key with
      | Some member_ref -> member_ref :: acc
      | None -> acc)
    storage_tbl
    []
  |> List.sort_uniq String.compare

let load_member_from_storage_tbl storage_tbl object_ref member_ref =
  let state =
    Circle_object_member.of_stored_values
      ~state_ref_raw:(Hashtbl.find_opt storage_tbl (Circle_object_member.state_ref_key object_ref member_ref))
      ~member_kind_raw:(Hashtbl.find_opt storage_tbl (Circle_object_member.member_kind_key object_ref member_ref))
      ~state_class_raw:(Hashtbl.find_opt storage_tbl (Circle_object_member.state_class_key object_ref member_ref))
      ~codec_raw:(Hashtbl.find_opt storage_tbl (Circle_object_member.codec_key object_ref member_ref))
      ~status_raw:(Hashtbl.find_opt storage_tbl (Circle_object_member.status_key object_ref member_ref))
  in
  if Circle_object_member.materialized state then
    Some state
  else
    None

let has_member_in_storage_tbl storage_tbl object_ref member_ref =
  match load_member_from_storage_tbl storage_tbl object_ref member_ref with
  | Some member_state -> Option.is_some member_state.Circle_object_member.state_ref
  | None -> false

let member_count_in_storage_tbl storage_tbl object_ref =
  member_refs_from_storage_tbl storage_tbl object_ref
  |> List.length

let member_ref_at_in_storage_tbl storage_tbl object_ref index =
  if index < 0 then
    None
  else
    member_refs_from_storage_tbl storage_tbl object_ref
    |> List.mapi (fun idx member_ref -> (idx, member_ref))
    |> List.find_opt (fun (idx, _) -> idx = index)
    |> Option.map snd