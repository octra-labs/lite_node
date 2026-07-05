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


let value_json v =
  `String (Contract_vm.to_string v)

let call_arg_json = function
  | Contract_vm.VString s -> `String s
  | Contract_vm.VInt z -> `Intlit (Z.to_string z)
  | Contract_vm.VBool b -> `String (if b then "true" else "false")
  | Contract_vm.VAddr a -> `String a
  | Contract_vm.VBytes b -> `String (Base64.encode_exn b)
  | Contract_vm.VBytes32 b -> `String (Base64.encode_exn b)
  | Contract_vm.VU64 n -> `String (Int64.to_string n)
  | Contract_vm.VU128 z -> `String (Z.to_string z)
  | Contract_vm.VU256 z -> `String (Z.to_string z)
  | v -> `String (Contract_vm.to_string v)

let nested_call_arg_json = function
  | Contract_vm.VString s -> `String s
  | Contract_vm.VInt z -> `Intlit (Z.to_string z)
  | Contract_vm.VBool b -> `String (if b then "true" else "false")
  | v -> `String (Contract_vm.to_string v)

let result_json = function
  | Contract_vm.VString s -> `String s
  | Contract_vm.VInt z -> `String (Z.to_string z)
  | Contract_vm.VBool b -> `Bool b
  | Contract_vm.VAddr a -> `String a
  | Contract_vm.VBytes b -> `String (Base64.encode_exn b)
  | Contract_vm.VBytes32 b -> `String (Base64.encode_exn b)
  | Contract_vm.VU64 n -> `String (Int64.to_string n)
  | Contract_vm.VU128 z -> `String (Z.to_string z)
  | Contract_vm.VU256 z -> `String (Z.to_string z)
  | Contract_vm.VCipher _ -> `String "<cipher>"
  | Contract_vm.VPubKey _ -> `String "<pubkey>"

let return_json = function
  | Some value -> result_json value
  | None -> `Null

let view_error error =
  Option.value ~default:"execution failed" error

type view_call_outcome =
  | View_call_ok of Yojson.Safe.t
  | View_call_failed of string

let view_call_outcome (result : Contract.exec_result) =
  if result.Contract.success then
    View_call_ok (return_json result.return_value)
  else
    View_call_failed (view_error result.error)

let event_json (er : Contract_vm.event_record) =
  `Assoc [
    "contract", `String er.contract;
    "depth", `Int er.depth;
    "event", `String er.event;
    "values", `List (List.map value_json er.values);
  ]

let events_json events =
  `List (List.map event_json events)

let program_event_json (er : Contract_vm.event_record) =
  `Assoc [
    "program", `String er.contract;
    "contract", `String er.contract;
    "depth", `Int er.depth;
    "event", `String er.event;
    "values", `List (List.map value_json er.values);
  ]

let program_events_json events =
  `List (List.map program_event_json events)

let revert_reason events =
  let rec find = function
    | [] -> None
    | { Contract_vm.event = "Require"; values = v :: _; _ } :: _ ->
      Some (Contract_vm.to_string v)
    | _ :: rest -> find rest
  in
  find events

let constructor_revert_message events error =
  match revert_reason events with
  | Some reason -> "constructor reverted [" ^ reason ^ "]"
  | None -> Option.value ~default:"constructor reverted" error

let execution_revert_message events error =
  match revert_reason events with
  | Some reason -> "execution reverted [" ^ reason ^ "]"
  | None -> Option.value ~default:"execution reverted" error

type call_outcome =
  | Call_ok
  | Call_failed of string

type direct_call_domain =
  | Circle_call
  | Program_call

type direct_call_meta = {
  scope : string;
  target_key : string;
  failure_type : string;
  exception_type : string;
}

type direct_call_decision =
  | Direct_call_ok of direct_call_meta
  | Direct_call_failed of {
      meta : direct_call_meta;
      error : string;
    }

let call_outcome ~success ~events ~error =
  if success then
    Call_ok
  else
    Call_failed (execution_revert_message events error)

let receipt_outcome (receipt : Contract.exec_result) =
  call_outcome
    ~success:receipt.Contract.success
    ~events:receipt.Contract.events
    ~error:receipt.Contract.error

let direct_call_meta = function
  | Circle_call ->
    {
      scope = "circle";
      target_key = "id";
      failure_type = "circle_call_failed";
      exception_type = "circle_call_exception";
    }
  | Program_call ->
    {
      scope = "contract";
      target_key = "addr";
      failure_type = "contract_call_failed";
      exception_type = "contract_call_exception";
    }

let direct_call_decision domain receipt =
  let meta = direct_call_meta domain in
  match receipt_outcome receipt with
  | Call_ok ->
    Direct_call_ok meta
  | Call_failed error ->
    Direct_call_failed { meta; error }

let receipt_events_json ?(program = false) (receipt : Contract.exec_result) =
  if program then
    program_events_json receipt.Contract.events
  else
    events_json receipt.Contract.events

type receipt_save = {
  success : bool;
  effort_used : int;
  events_json : Yojson.Safe.t;
  error : string option;
}

let receipt_save ?(program = false) (receipt : Contract.exec_result) =
  {
    success = receipt.Contract.success;
    effort_used = receipt.Contract.effort_used;
    events_json = receipt_events_json ~program receipt;
    error = receipt.Contract.error;
  }

let multi_exec_call_report ~index ~program ~method_name ~amount ~success ~effort ~error ~return_value =
  `Assoc [
    "index", `Int index;
    "program", `String program;
    "method", `String method_name;
    "amount", `String (Z.to_string amount);
    "success", `Bool success;
    "effort", `Int effort;
    "error", (match error with Some e -> `String e | None -> `Null);
    "return", (match return_value with Some v -> `String (Contract_vm.to_string v) | None -> `Null);
  ]

type multi_exec_trace = {
  effort : int;
  calls : Yojson.Safe.t list;
  events : Contract_vm.event_record list;
}

let empty_multi_exec_trace = {
  effort = 0;
  calls = [];
  events = [];
}

let add_multi_exec_trace trace ~index ~program ~method_name ~amount ~success ~effort ~error ~return_value ~events =
  {
    effort = trace.effort + effort;
    calls = trace.calls @ [
      multi_exec_call_report
        ~index
        ~program
        ~method_name
        ~amount
        ~success
        ~effort
        ~error
        ~return_value
    ];
    events = trace.events @ events;
  }

let multi_exec_step_error ~index ~success ~events ~error =
  if success then
    None
  else
    Some (Printf.sprintf "call %d failed: %s" index (execution_revert_message events error))

let multi_exec_receipt_json ~epoch ~now ~effort ~calls ~events ~success ~error =
  Yojson.Safe.to_string (`Assoc [
    "program", `String "multi_exec";
    "contract", `String "multi_exec";
    "method", `String "multi_exec";
    "success", `Bool success;
    "effort", `Int effort;
    "calls", `List calls;
    "events", program_events_json events;
    "error", (match error with Some e -> `String e | None -> `Null);
    "epoch", `Int epoch;
    "ts", `Float now;
  ])