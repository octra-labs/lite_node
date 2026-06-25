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


type operation =
  | Attach
  | Detach

type t = {
  operation : operation;
  member_ref : string;
  state_ref : string option;
  member_kind : string option;
  state_class : string option;
  codec : string option;
  status : string option;
}

let operation_of_string = function
  | "attach" -> Ok Attach
  | "detach" -> Ok Detach
  | other -> Error ("unknown object member delta operation: " ^ other)

let operation_to_string = function
  | Attach -> "attach"
  | Detach -> "detach"

let normalize_fragment field value =
  let normalized = String.trim value in
  if String.length normalized = 0 then
    Error (field ^ " must not be empty")
  else if String.contains normalized '|' || String.contains normalized ';' then
    Error (field ^ " must not contain bundle separators")
  else
    Ok normalized

let parse_attach_fields fields =
  match fields with
  | [operation_raw; member_ref_raw; state_ref_raw; member_kind_raw; state_class_raw; codec_raw; status_raw] ->
    begin
      match
        operation_of_string (String.trim operation_raw),
        Circle_object_member.validate_member_ref member_ref_raw,
        Circles.normalize_state_ref state_ref_raw,
        normalize_fragment "member_kind" member_kind_raw,
        normalize_fragment "state_class" state_class_raw,
        normalize_fragment "codec" codec_raw,
        normalize_fragment "status" status_raw
      with
      | Ok Attach, Ok member_ref, Ok state_ref, Ok member_kind, Ok state_class, Ok codec, Ok status ->
        Ok {
          operation = Attach;
          member_ref;
          state_ref = Some state_ref;
          member_kind = Some member_kind;
          state_class = Some state_class;
          codec = Some codec;
          status = Some status;
        }
      | Ok Detach, _, _, _, _, _, _ ->
        Error "attach entry must use attach operation"
      | Error message, _, _, _, _, _, _ ->
        Error message
      | _, Error message, _, _, _, _, _ ->
        Error message
      | _, _, Error message, _, _, _, _ ->
        Error message
      | _, _, _, Error message, _, _, _ ->
        Error message
      | _, _, _, _, Error message, _, _ ->
        Error message
      | _, _, _, _, _, Error message, _ ->
        Error message
      | _, _, _, _, _, _, Error message ->
        Error message
    end
  | _ ->
    Error "attach entry must have 7 fields"

let parse_detach_fields fields =
  match fields with
  | [operation_raw; member_ref_raw] ->
    begin
      match operation_of_string (String.trim operation_raw), Circle_object_member.validate_member_ref member_ref_raw with
      | Ok Detach, Ok member_ref ->
        Ok {
          operation = Detach;
          member_ref;
          state_ref = None;
          member_kind = None;
          state_class = None;
          codec = None;
          status = None;
        }
      | Ok Attach, _ ->
        Error "detach entry must use detach operation"
      | Error message, _ ->
        Error message
      | _, Error message ->
        Error message
    end
  | _ ->
    Error "detach entry must have 2 fields"

let parse_entry raw_entry =
  let trimmed = String.trim raw_entry in
  if String.length trimmed = 0 then
    Error "object member delta entry must not be empty"
  else
    let fields =
      String.split_on_char '|' trimmed
      |> List.map String.trim in
    match fields with
    | "attach" :: _ -> parse_attach_fields fields
    | "detach" :: _ -> parse_detach_fields fields
    | _ -> Error "object member delta entry must start with attach or detach"

let parse_bundle raw_bundle =
  let trimmed = String.trim raw_bundle in
  if String.length trimmed = 0 then
    Ok []
  else
    let entries =
      String.split_on_char ';' trimmed
      |> List.map String.trim
      |> List.filter (fun value -> String.length value > 0) in
    let rec collect acc = function
      | [] -> Ok (List.rev acc)
      | entry :: rest ->
        begin
          match parse_entry entry with
          | Ok delta -> collect (delta :: acc) rest
          | Error message -> Error message
        end
    in
    collect [] entries

let write_snapshot storage_tbl object_ref (delta : t) =
  match delta.operation with
  | Attach ->
    Circle_object_member.write_snapshot
      storage_tbl
      object_ref
      delta.member_ref
      {
        Circle_object_member.state_ref = delta.state_ref;
        member_kind = delta.member_kind;
        state_class = delta.state_class;
        codec = delta.codec;
        status = delta.status;
      }
  | Detach ->
    let open Circle_policy_value in
    set_or_clear
      storage_tbl
      (Circle_object_member.state_ref_key object_ref delta.member_ref)
      None;
    set_or_clear
      storage_tbl
      (Circle_object_member.member_kind_key object_ref delta.member_ref)
      None;
    set_or_clear
      storage_tbl
      (Circle_object_member.state_class_key object_ref delta.member_ref)
      None;
    set_or_clear
      storage_tbl
      (Circle_object_member.codec_key object_ref delta.member_ref)
      None;
    set_or_clear
      storage_tbl
      (Circle_object_member.status_key object_ref delta.member_ref)
      None