(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type config = {
  method_name : string;
  args : Contract_vm.v list;
  storage : (string * string) list;
  storage_kinds : (string * Contract_vm.storage_kind) list;
  strict_values : bool;
  caller : string;
  origin : string;
  address : string;
  value : Z.t;
  limit : int;
  step_cap : int;
  epoch : int;
  epoch_time : int64;
  tree_hash : string;
  node_id : string;
  tx_hash : string;
  view : bool;
  byte_result : Contract_vm.byte_result;
  grants : Contract_vm.cap list;
}

type stop =
  | Returned
  | Reverted
  | Step_cap
  | Host_operation of int * string

type frame = {
  index : int;
  pc : int;
  next_pc : int;
  effort_before : int;
  effort_after : int;
  op : Contract_vm.instr;
  result : Contract_vm.v;
}

type outcome = {
  stop : stop;
  result : Contract_vm.v;
  regs : Contract_vm.v array;
  effort : int;
  steps : int;
  storage : (string * string) list;
  events : Contract_vm.event_record list;
  closes : Contract_vm.cap list;
  frames : frame list;
}

type error =
  | Dispatcher_absent
  | Duplicate_storage of string
  | Invalid_step_cap
  | Program_counter of int
  | Grant_count of int * int
  | Grant_repeat
  | Grant of Z.t * Z.t * Z.t
  | Session of C_sess.error

let local_address =
  "oct11111111111111111111111111111111111111111111"

let config
    ?(storage = [])
    ?(storage_kinds = [])
    ?(strict_values = true)
    ?(caller = local_address)
    ?origin
    ?(address = local_address)
    ?(value = Z.zero)
    ?(limit = 1_000_000)
    ?(step_cap = 100_000)
    ?(epoch = 0)
    ?(epoch_time = 0L)
    ?(tree_hash = String.make 64 '0')
    ?(node_id = "local")
    ?(tx_hash = String.make 64 '0')
    ?(view = false)
    ?(byte_result = Contract_vm.String_bytes)
    ?(grants = [])
    ~method_name
    ~args
    () =
  {
    method_name;
    args;
    storage;
    storage_kinds;
    strict_values;
    caller;
    origin = Option.value origin ~default:caller;
    address;
    value;
    limit;
    step_cap;
    epoch;
    epoch_time;
    tree_hash;
    node_id;
    tx_hash;
    view;
    byte_result;
    grants;
  }

let host_operation = function
  | Contract_vm.BALANCE _ -> Some "balance"
  | Contract_vm.TRANSFER _ -> Some "transfer"
  | Contract_vm.XCALL _ -> Some "program_call"
  | Contract_vm.SPAWN _ | Contract_vm.SPAWN2 _ -> Some "program_spawn"
  | Contract_vm.STATE_PATH_KEY _ -> Some "state_path"
  | Contract_vm.OBJECT_MEMBER_COUNT _
  | Contract_vm.OBJECT_HAS_MEMBER _
  | Contract_vm.OBJECT_MEMBER_REF_AT _
  | Contract_vm.OBJECT_TRANSITION_APPLY _ -> Some "object_state"
  | Contract_vm.ED25519_OK _ -> Some "ed25519"
  | Contract_vm.GROTH16_VERIFY_BN254 _ -> Some "groth16"
  | Contract_vm.FHE_LOAD_PK _
  | Contract_vm.FHE_ADD _
  | Contract_vm.FHE_SUB _
  | Contract_vm.FHE_MUL _
  | Contract_vm.FHE_SCALE _
  | Contract_vm.FHE_DIV_CONST _
  | Contract_vm.FHE_ADD_CONST _
  | Contract_vm.FHE_SUB_CONST _
  | Contract_vm.FHE_VERIFY_ZERO _
  | Contract_vm.FHE_VERIFY_RANGE _
  | Contract_vm.FHE_VERIFY_BOUND _
  | Contract_vm.FHE_COMMIT _
  | Contract_vm.FHE_PEDERSEN _
  | Contract_vm.FHE_PEDERSEN_ADD _
  | Contract_vm.FHE_PEDERSEN_SUB _
  | Contract_vm.FHE_PEDERSEN_IDENTITY _
  | Contract_vm.FHE_SER _
  | Contract_vm.FHE_DESER _
  | Contract_vm.FHE_SER_PK _
  | Contract_vm.FHE_DESER_PK _ -> Some "fhe"
  | _ -> None

let storage rows =
  let table = Hashtbl.create (List.length rows) in
  let rec add = function
    | [] -> Ok table
    | (key, value) :: rest ->
      if Hashtbl.mem table key then Error (Duplicate_storage key)
      else begin
        Hashtbl.add table key value;
        add rest
      end
  in
  add rows

let storage_rows table =
  Hashtbl.fold (fun key value rows -> (key, value) :: rows) table []
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)

let make_state config storage =
  let ctx = {
    Contract_vm.default_ctx with
    cap_live = (fun cap -> List.exists (Contract_vm.cap_equal cap) config.grants);
    point_ops = true;
    int_work = Int_work.Active;
    current_epoch = config.epoch;
    epoch_time_ms = config.epoch_time;
    tree_hash = config.tree_hash;
    node_id = config.node_id;
    tx_hash = config.tx_hash;
  }
  in
  let state =
    Contract_vm.create_state
      ~limit:config.limit
      ~ctx
      ~is_view:config.view
      ~strict_values:config.strict_values
      ~byte_result:config.byte_result
      ~storage_kinds:config.storage_kinds
      ~caller:config.caller
      ~origin:config.origin
      ~address:config.address
      ~value:config.value
      ~storage
      ()
  in
  Hashtbl.replace state.memory.data 999 (Contract_vm.VString "call");
  Hashtbl.replace state.memory.data 1000
    (Contract_vm.VString config.method_name);
  List.iteri
    (fun index value ->
      Hashtbl.replace state.memory.data (1001 + index) value)
    config.args;
  state

let outcome state stop steps frames initial storage =
  let storage, closes =
    match stop with
    | Returned -> storage_rows storage, List.rev state.Contract_vm.closes
    | Reverted | Step_cap | Host_operation _ -> List.sort compare initial, []
  in
  {
    stop;
    result = state.Contract_vm.regs.(0);
    regs = Array.copy state.Contract_vm.regs;
    effort = state.effort_used;
    steps;
    storage;
    events = List.rev !(state.logs);
    closes;
    frames = List.rev frames;
  }

let rec grants_fit left = function
  | [] -> true
  | _ when left = 0 -> false
  | _ :: rest -> grants_fit (left - 1) rest

let rec grants_distinct seen = function
  | [] -> true
  | cap :: _ when List.exists (Contract_vm.cap_equal cap) seen -> false
  | cap :: rest -> grants_distinct (cap :: seen) rest

let execute ~trace config code entry =
  if config.step_cap < 1 then Error Invalid_step_cap
  else if not (grants_fit Contract_vm.input_limit config.grants) then
    Error (Grant_count (Contract_vm.input_limit, Contract_vm.input_limit + 1))
  else if not (grants_distinct [] config.grants) then Error Grant_repeat
  else if entry < 0 || entry >= Array.length code then
    Error (Program_counter entry)
  else
    begin
      begin
        match storage config.storage with
        | Error error -> Error error
        | Ok storage ->
          let state = make_state config storage in
          state.Contract_vm.pc <- entry;
          let rec run index frames =
            if index >= config.step_cap then
              Ok (outcome state Step_cap index frames config.storage storage)
            else
              let pc = state.pc in
              if pc < 0 || pc >= Array.length code then
                Error (Program_counter pc)
              else
                let effort_before = state.effort_used in
                let op = code.(pc) in
                match host_operation op with
                | Some name ->
                  Ok (outcome state (Host_operation (pc, name)) index frames config.storage storage)
                | None ->
                  let progress = Contract_vm.step state code in
                  let frame = {
                    index;
                    pc;
                    next_pc = state.pc;
                    effort_before;
                    effort_after = state.effort_used;
                    op;
                    result = state.regs.(0);
                  }
                  in
                  let frames = if trace then frame :: frames else frames in
                  match progress with
                  | Contract_vm.Running -> run (index + 1) frames
                  | Contract_vm.Finished ->
                    Ok (outcome state Returned (index + 1) frames config.storage storage)
                  | Contract_vm.Refused ->
                    Ok (outcome state Reverted (index + 1) frames config.storage storage)
          in
          run 0 []
      end
    end

let run_at ~trace config ~entry raw =
  execute ~trace config (Vm_program.fix raw) entry

let run ~trace config raw =
  let code = Vm_program.fix raw in
  match Vm_program.entry code with
  | None -> Error Dispatcher_absent
  | Some entry -> execute ~trace config code entry

let be64 size =
  let out = Bytes.make 8 '\000' in
  let value = Int64.of_int size in
  for index = 0 to 7 do
    let shift = (7 - index) * 8 in
    let byte =
      Int64.to_int
        (Int64.logand (Int64.shift_right_logical value shift) 0xffL)
    in
    Bytes.set out index (Char.chr byte)
  done;
  Bytes.unsafe_to_string out

let field value = be64 (String.length value) ^ value

let scope_id (scope : C_sess.scope) =
  C_sha.hash
    (String.concat ""
      ["AMLCAP\001"; field scope.chain; field scope.prog; field scope.root])

let token_cap scope (token : C_sess.token) = {
  Contract_vm.scope;
  kind = C_nat.to_int token.kind;
  id = C_nat.to_int token.id;
  rev = C_nat.to_int token.rev;
}

let grants state tokens =
  if not (grants_fit Contract_vm.input_limit tokens) then None
  else
    let scope = scope_id (C_sess.scope_of state) in
    let rec walk out = function
      | [] -> Some (List.rev out)
      | token :: rest when C_sess.current state token ->
        let cap = token_cap scope token in
        if List.exists (Contract_vm.cap_equal cap) out then None
        else walk (cap :: out) rest
      | _ -> None
    in
    Option.map
      (List.map (fun cap -> Contract_vm.VCap cap))
      (walk [] tokens)

let grant state token =
  match grants state [token] with
  | Some [value] -> Some value
  | Some _ | None -> None

let settle state tokens outcome =
  let scope = scope_id (C_sess.scope_of state) in
  let caps = List.map (token_cap scope) tokens in
  let rec remove cap left = function
    | [] -> None
    | token :: rest when Contract_vm.cap_equal cap (token_cap scope token) ->
      Some (token, List.rev_append left rest)
    | token :: rest -> remove cap (token :: left) rest
  in
  let rec walk state tokens = function
    | [] -> Ok (state, tokens)
    | cap :: rest ->
      begin
        match remove cap [] tokens with
        | None ->
          Error (Grant (Z.of_int cap.kind, Z.of_int cap.id, Z.of_int cap.rev))
        | Some (token, tokens) ->
          begin
            match C_sess.take state token ~keep:false with
            | Ok (state, None) -> walk state tokens rest
            | Ok (_, Some _) ->
              Error (Grant (Z.of_int cap.kind, Z.of_int cap.id, Z.of_int cap.rev))
            | Error error -> Error (Session error)
          end
      end
  in
  if not (grants_fit Contract_vm.input_limit tokens) then
    Error (Grant_count (Contract_vm.input_limit, Contract_vm.input_limit + 1))
  else if not (grants_distinct [] caps) then Error Grant_repeat
  else
    match outcome.stop with
    | Returned -> walk state tokens outcome.closes
    | Reverted | Step_cap | Host_operation _ -> Ok (state, tokens)

let hex value =
  let out = Bytes.create (String.length value * 2) in
  let digit value =
    if value < 10 then Char.chr (Char.code '0' + value)
    else Char.chr (Char.code 'a' + value - 10)
  in
  String.iteri
    (fun index char ->
      let code = Char.code char in
      Bytes.set out (index * 2) (digit (code lsr 4));
      Bytes.set out (index * 2 + 1) (digit (code land 15)))
    value;
  Bytes.to_string out

let text tag value =
  if String.length value <= 96 then tag ^ String.escaped value
  else
    Printf.sprintf "%ssize:%d:sha256:%s"
      tag
      (String.length value)
      Digestif.SHA256.(to_hex (digest_string value))

let value_text = function
  | Contract_vm.VInt value -> "int:" ^ Z.to_string value
  | Contract_vm.VBool value -> "bool:" ^ string_of_bool value
  | Contract_vm.VString value -> text "text:" value
  | Contract_vm.VBytes value -> text "bytes:" (hex value)
  | Contract_vm.VBytes32 value -> text "bytes32:" (hex value)
  | Contract_vm.VU64 value -> "u64:" ^ Z.to_string value
  | Contract_vm.VU128 value -> "u128:" ^ Z.to_string value
  | Contract_vm.VU256 value -> "u256:" ^ Z.to_string value
  | Contract_vm.VAddr value -> "addr:" ^ value
  | Contract_vm.VCap cap ->
    Printf.sprintf "cap kind = %d id = %d rev = %d"
      cap.kind cap.id cap.rev
  | Contract_vm.VCipher _ -> "cipher"
  | Contract_vm.VPubKey _ -> "pubkey"

let stop_text = function
  | Returned -> "returned"
  | Reverted -> "reverted"
  | Step_cap -> "step_cap"
  | Host_operation (pc, name) ->
    Printf.sprintf "host_operation pc = %d operation = %s" pc name

let error_text = function
  | Dispatcher_absent -> "program dispatcher is absent"
  | Duplicate_storage key ->
    Printf.sprintf "storage key is repeated key = %s" key
  | Invalid_step_cap -> "step cap is invalid"
  | Program_counter pc ->
    Printf.sprintf "program counter is invalid pc = %d" pc
  | Grant_count (maximum, actual) ->
    Printf.sprintf "capability grant count exceeds maximum = %d actual = %d"
      maximum actual
  | Grant_repeat -> "capability grant is repeated"
  | Grant (kind, id, rev) ->
    Printf.sprintf "capability grant is absent kind = %s id = %s rev = %s"
      (Z.to_string kind) (Z.to_string id) (Z.to_string rev)
  | Session error -> C_sess.text error