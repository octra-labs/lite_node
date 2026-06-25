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

let object_ref_of_key raw_key =
  match String.split_on_char ':' raw_key with
  | ["object_binding"; object_ref; _suffix]
  | ["object_policy"; object_ref; _suffix] ->
    begin
      match Circle_private_common.normalize_hex64 "object_ref" object_ref with
      | Ok normalized -> Some normalized
      | Error _ -> None
    end
  | ["object_member"; object_ref; _member_ref; _suffix] ->
    begin
      match Circle_private_common.normalize_hex64 "object_ref" object_ref with
      | Ok normalized -> Some normalized
      | Error _ -> None
    end
  | _ ->
    None

let object_refs_from_storage_tbl storage_tbl =
  Hashtbl.fold
    (fun raw_key _value acc ->
      match object_ref_of_key raw_key with
      | Some object_ref -> object_ref :: acc
      | None -> acc)
    storage_tbl
    []
  |> List.sort_uniq String.compare

let summary_materialized (summary : Circle_object_query.summary) =
  Circle_object_binding.materialized summary.binding
  || Option.is_some summary.policy
  || summary.active_member_count > 0

let load_summaries_from_storage_tbl storage_tbl =
  object_refs_from_storage_tbl storage_tbl
  |> List.map (Circle_object_query.load_summary_from_storage_tbl storage_tbl)
  |> List.filter summary_materialized

let load_refs store circle_id =
  let* storage_result = Store_irmin.load_circle_stable_storage store circle_id in
  match storage_result with
  | Error message ->
    Lwt.return (Error message)
  | Ok storage_tbl ->
    Lwt.return (Ok (object_refs_from_storage_tbl storage_tbl))

let load_summaries store circle_id =
  let* storage_result = Store_irmin.load_circle_stable_storage store circle_id in
  match storage_result with
  | Error message ->
    Lwt.return (Error message)
  | Ok storage_tbl ->
    Lwt.return (Ok (load_summaries_from_storage_tbl storage_tbl))