(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  address : string;
  code_hash : string;
  version : string;
  balance : Z.t;
  owner : string;
  bytecode : Contract_vm.instr array;
}

type exec_result = {
  success : bool;
  return_value : Contract_vm.v option;
  effort_used : int;
  events : Contract_vm.event_record list;
  error : string option;
  storage_writes : int;
}

type upgrade_result = {
  old_code_hash : string;
  new_code_hash : string;
}

let run_s p =
  match Lwt.state p with
  | Lwt.Return v -> v
  | Lwt.Fail e -> raise e
  | Lwt.Sleep ->
    let r = ref None in
    Lwt.on_any p (fun v -> r := Some (Ok v)) (fun e -> r := Some (Error e));
    let rec pump n =
      if n > 100000 then failwith "run_s: irmin I/O timeout"
      else match !r with
      | Some (Ok v) -> v
      | Some (Error e) -> raise e
      | None -> ignore (Lwt_engine.iter true); pump (n + 1)
    in pump 0

let addr_from_code code deployer nonce =
  let unique_data = Printf.sprintf "%s:%s:%d" code deployer nonce in
  let hash = Digestif.SHA256.digest_string unique_data in
  let base58_hash = Octra_core.Crypto.Base58.encode (Digestif.SHA256.to_raw_string hash) in
  let base58_part =
    if String.length base58_hash >= 44 then String.sub base58_hash 0 44
    else
      let rec extend s suffix =
        if String.length s >= 44 then String.sub s 0 44
        else
          let more = Digestif.SHA256.digest_string (unique_data ^ string_of_int suffix) in
          extend (s ^ Octra_core.Crypto.Base58.encode (Digestif.SHA256.to_raw_string more)) (suffix + 1)
      in
      extend base58_hash 0
  in
  let addr = "oct" ^ base58_part in
  assert (Octra_core.Crypto.is_octra_address addr);
  addr

type loaded = {
  code : Contract_vm.instr array;
  profile : Admission.profile;
}

let decode_loaded ?(trusted = []) ?(point_ops = false) raw =
  try
    match Admission.decode_deploy ~trusted ~point_ops raw with
    | Ok admitted ->
      Some { code = Admission.code admitted; profile = Admission.profile admitted }
    | Error _ -> None
  with
  | (Stack_overflow | Out_of_memory) as error -> raise error
  | _ -> None

let decode_mode ?(trusted = []) ?(point_ops = false) mode raw =
  if String.equal mode "source" then
    try
      match Admission.decode_program_source ~point_ops raw with
      | Ok admitted ->
        Some {
          code = Admission.code admitted;
          profile = Admission.profile admitted;
        }
      | Error _ -> None
    with
    | (Stack_overflow | Out_of_memory) as error -> raise error
    | _ -> None
  else
    decode_loaded ~trusted ~point_ops raw

let load_loaded ?snapshot ?(trusted = []) ?(point_ops = false) store contract_addr =
  match run_s (Octra_core.Store_irmin.load_bytecode ?snapshot store contract_addr) with
  | Some b64 ->
    let mode =
      match run_s (Octra_core.Store_irmin.get_contract_meta ?snapshot store contract_addr) with
      | Some meta -> meta.admission
      | None -> "binary"
    in
    (try
       decode_mode
         ~trusted
         ~point_ops
         mode
         (Base64.decode_exn b64)
     with
     | (Stack_overflow | Out_of_memory) as error -> raise error
     | _ -> None)
  | None -> None

let load_bytecode ?(trusted = []) ?(point_ops = false) store contract_addr =
  Option.map (fun loaded -> loaded.code)
    (load_loaded ~trusted ~point_ops store contract_addr)

let load_async ?snapshot ~trusted ~point_ops store address =
  let ( let* ) work next = Lwt.bind work (Octra_core.Exec_resource.protect next) in
  let* encoded = Octra_core.Store_irmin.load_bytecode ?snapshot store address in
  match encoded with
  | None -> Lwt.return_none
  | Some encoded ->
    let* meta = Octra_core.Store_irmin.get_contract_meta ?snapshot store address in
    let mode = match meta with Some meta -> meta.admission | None -> "binary" in
    let work = Octra_core.Exec_resource.detach (fun () ->
      match Base64.decode encoded with
      | Error _ -> None
      | Ok raw -> decode_mode ~trusted ~point_ops mode raw) () in
    Lwt.finalize
      (fun () -> Lwt.protected work)
      (fun () -> Lwt.catch
        (fun () -> Lwt.map (fun _ -> ()) (Lwt.protected work))
        (fun _ -> Lwt.return_unit))

let load_storage ?snapshot store contract_addr =
  run_s (Octra_core.Store_irmin.load_contract_storage ?snapshot store contract_addr)

let fix_jumps bytecode =
  let jump_table = Hashtbl.create 10 in
  Array.iteri (fun i op -> match op with
    | Contract_vm.JDEST addr -> Hashtbl.add jump_table addr i
    | _ -> ()
  ) bytecode;
  Array.map (fun op -> match op with
    | Contract_vm.JIF (r, addr) ->
      (match Hashtbl.find_opt jump_table addr with Some pos -> Contract_vm.JIF (r, pos) | None -> op)
    | Contract_vm.JMP addr ->
      (match Hashtbl.find_opt jump_table addr with Some pos -> Contract_vm.JMP pos | None -> op)
    | Contract_vm.CALL_INT (rd, addr) ->
      (match Hashtbl.find_opt jump_table addr with Some pos -> Contract_vm.CALL_INT (rd, pos) | None -> op)
    | _ -> op
  ) bytecode

let find_dispatcher bytecode =
  let len = Array.length bytecode in
  let rec go pc =
    if pc >= len then None
    else match bytecode.(pc) with
    | Contract_vm.JDEST 100 -> Some pc
    | _ -> go (pc + 1)
  in
  go 0

let is_view_method bytecode target =
  let len = Array.length bytecode in
  let pc = ref target in
  let found_write = ref false in
  while !pc < len && not !found_write do
    (match bytecode.(!pc) with
     | Contract_vm.SSTORE _ | Contract_vm.SSTOREK _
     | Contract_vm.SDEL _ | Contract_vm.SDELK _
     | Contract_vm.TRANSFER _ -> found_write := true
     | Contract_vm.STOP | Contract_vm.REVERT -> pc := len
     | Contract_vm.JDEST _ when !pc > target -> pc := len
     | _ -> ());
    if not !found_write then pc := !pc + 1
  done;
  not !found_write

let extract_methods bytecode =
  match find_dispatcher bytecode with
  | None -> []
  | Some entry ->
    let len = Array.length bytecode in
    let jdest_map = Hashtbl.create 16 in
    Array.iteri (fun i instr -> match instr with
      | Contract_vm.JDEST label -> Hashtbl.replace jdest_map label i
      | _ -> ()
    ) bytecode;
    let methods = ref [] in
    let pc = ref (entry + 1) in
    while !pc < len do
      (match bytecode.(!pc) with
       | Contract_vm.LDI (_, Contract_vm.VString name) ->
         if !pc + 2 < len then
           (match bytecode.(!pc + 1), bytecode.(!pc + 2) with
            | Contract_vm.EQ _, Contract_vm.JIF (_, label) ->
              let view = match Hashtbl.find_opt jdest_map label with
                | Some idx -> is_view_method bytecode idx
                | None -> true in
              methods := (name, view) :: !methods
            | _ -> ())
       | Contract_vm.REVERT | Contract_vm.STOP -> pc := len
       | _ -> ());
      pc := !pc + 1
    done;
    List.rev !methods

let extract_method_arity bytecode method_name =
  match find_dispatcher bytecode with
  | None -> None
  | Some entry ->
    let len = Array.length bytecode in
    let rec scan pc =
      if pc >= len then None
      else match bytecode.(pc) with
      | Contract_vm.LDI (_, Contract_vm.VString name) when String.equal name method_name ->
        if pc + 2 < len then
          match bytecode.(pc + 1), bytecode.(pc + 2) with
          | Contract_vm.EQ _, Contract_vm.JIF (_, target_pc)
            when target_pc > 0 && target_pc < len ->
            let rec count p arity =
              if p >= len then arity
              else match bytecode.(p) with
              | Contract_vm.MLOAD (_, idx) when idx >= 1001 ->
                count (p + 1) (arity + 1)
              | _ -> arity
            in
            Some (count (target_pc + 1) 0)
          | _ -> scan (pc + 1)
        else None
      | Contract_vm.REVERT | Contract_vm.STOP -> None
      | _ -> scan (pc + 1)
    in
    scan (entry + 1)

let extract_method_target bytecode method_name =
  match find_dispatcher bytecode with
  | None -> None
  | Some entry ->
    let len = Array.length bytecode in
    let rec scan pc =
      if pc >= len then None
      else match bytecode.(pc) with
      | Contract_vm.LDI (_, Contract_vm.VString name)
        when String.equal name method_name && pc + 2 < len ->
        (match bytecode.(pc + 1), bytecode.(pc + 2) with
         | Contract_vm.EQ _, Contract_vm.JIF (_, target) -> Some target
         | _ -> scan (pc + 1))
      | Contract_vm.REVERT | Contract_vm.STOP -> None
      | _ -> scan (pc + 1)
    in
    scan (entry + 1)

let parse_param = function
  | `String s -> Contract_vm.VString s
  | `Bool b -> Contract_vm.VBool b
  | `Int i -> Contract_vm.VInt (Z.of_int i)
  | `Intlit s -> Contract_vm.VInt (Z.of_string s)
  | _ -> Contract_vm.VString ""

let entry_kinds facts target =
  let slots =
    if target = 0 then facts.Program_type_flow.root
    else
      match List.find_opt
        (fun (entry : Program_type_flow.entry) -> entry.target = target)
        facts.entries with
      | Some entry -> entry.mem
      | None -> []
  in
  slots
  |> List.sort (fun (left, _) (right, _) -> compare left right)
  |> List.map snd

let runtime_params profile target params =
  match profile with
  | Admission.Legacy -> Ok (List.rev (List.rev_map parse_param params))
  | Admission.Program facts -> Program_input.parse (entry_kinds facts target) params

let runtime_values profile target values =
  match profile with
  | Admission.Legacy -> Ok values
  | Admission.Program facts ->
    (match Program_input.validate (entry_kinds facts target) values with
     | Ok () -> Ok values
     | Error error -> Error error)

let strict_values = function
  | Admission.Legacy -> false
  | Admission.Program _ -> true

let storage_kind = function
  | Program_type_flow.Int -> Some Contract_vm.StorageInt
  | Program_type_flow.Bool -> Some Contract_vm.StorageBool
  | Program_type_flow.String -> Some Contract_vm.StorageString
  | Program_type_flow.Bytes -> Some Contract_vm.StorageBytes
  | Program_type_flow.Bytes32 -> Some Contract_vm.StorageBytes32
  | Program_type_flow.U64 -> Some Contract_vm.StorageU64
  | Program_type_flow.U128 -> Some Contract_vm.StorageU128
  | Program_type_flow.U256 -> Some Contract_vm.StorageU256
  | Program_type_flow.Addr -> Some Contract_vm.StorageAddr
  | Program_type_flow.Cipher
  | Program_type_flow.PubKey
  | Program_type_flow.Unknown -> None

let storage_kinds = function
  | Admission.Legacy -> []
  | Admission.Program facts ->
    List.filter_map
      (fun (key, kind) ->
        Option.map (fun value -> key, value) (storage_kind kind))
      facts.Program_type_flow.storage

let count_storage_writes state =
  List.fold_left (fun acc e -> match e with
    | Contract_vm.UndoWrite _ -> acc + 1
    | Contract_vm.UndoMarker _ | Contract_vm.UndoClose _ -> acc
  ) 0 state.Contract_vm.undo_stack

let setup_call_state_values ?(ctx = Contract_vm.default_ctx) ?(depth = 0) ?(limit = 1_000_000)
    ?(strict_values = false) ?(storage_kinds = [])
    ~caller ~address ~value ~storage_tbl ~method_name ~params () =
  let state = Contract_vm.create_state ~ctx ~depth ~limit ~strict_values
    ~storage_kinds
    ~caller ~origin:caller ~address ~value ~storage:storage_tbl () in
  state.memory.data <- Hashtbl.create 1024;
  Hashtbl.add state.memory.data 999 (Contract_vm.VString "call");
  Hashtbl.add state.memory.data 1000 (Contract_vm.VString method_name);
  List.iteri (fun i value -> Hashtbl.add state.memory.data (1001 + i) value) params;
  state

let setup_call_state ?(ctx = Contract_vm.default_ctx) ?(depth = 0) ?(limit = 1_000_000)
    ~caller ~address ~value ~storage_tbl ~method_name ~params () =
  setup_call_state_values ~ctx ~depth ~limit ~caller ~address ~value ~storage_tbl
    ~method_name ~params:(List.rev (List.rev_map parse_param params)) ()

let run_fixed_from_dispatcher ?running state fixed =
  let run () =
    match running with
    | None -> Contract_vm.run state fixed
    | Some running ->
      let rec loop () =
        if not (running ()) then begin
          ignore (Contract_vm.refuse state);
          false
        end else
          match Contract_vm.step state fixed with
          | Contract_vm.Running -> loop ()
          | Contract_vm.Finished -> true
          | Contract_vm.Refused -> false
      in
      loop ()
  in
  let ok = match find_dispatcher fixed with
    | Some entry ->
      state.Contract_vm.pc <- entry;
      let success = run () in
      success && not state.reverted
    | None ->
      let success = run () in
      success && not state.reverted
  in
  let return_value = if ok then
    Some state.regs.(0)
  else None in
  {
    success = ok;
    return_value;
    effort_used = state.effort_used;
    events = List.rev !(state.logs);
    error = if ok then None else Some "execution reverted";
    storage_writes = count_storage_writes state;
  }

let run_from_dispatcher state bytecode =
  run_fixed_from_dispatcher state (fix_jumps bytecode)

let run_fixed_async ?running state fixed =
  Option.iter (fun entry -> state.Contract_vm.pc <- entry) (find_dispatcher fixed);
  let ( let* ) = Octra_core.Exec_resource.bind in
  let* success = Contract_vm.run_async ?running state fixed in
  let ok = success && not state.reverted in
  Lwt.return {
    success = ok;
    return_value = if ok then Some state.regs.(0) else None;
    effort_used = state.effort_used;
    events = List.rev !(state.logs);
    error = if ok then None else Some "execution reverted";
    storage_writes = count_storage_writes state;
  }

let trim_error msg =
  if String.length msg <= 256 then msg
  else String.sub msg 0 256

let exec_result_to_result r =
  if r.success then
    Ok (Option.value r.return_value ~default:(Contract_vm.VBool true))
  else
    Error (trim_error (Option.value r.error ~default:"execution failed"))

let deploy_with ~async_exec ~return ~bind ~run ~journal ?(trusted = []) ?admitted ?(ctx = Contract_vm.default_ctx)
    ?(params = []) store deployer ctype (_code : Contract_vm.instr array) bytecode_raw nonce =
  let ctx = { ctx with async_exec } in
  let addr = addr_from_code bytecode_raw deployer nonce in
  let hash = Digestif.SHA256.(digest_string bytecode_raw |> to_hex) in
  Octra_log.info "program" "event = deploy_start addr = %s deployer = %s size = %d hash = %s"
    addr (String.sub deployer 0 (min 12 (String.length deployer)))
    (String.length bytecode_raw) (String.sub hash 0 16);
  let source_checked = Option.is_some admitted in
  let checked =
    match admitted with
    | Some value ->
      Result.map (fun () -> value)
        (Admission.check_standard ~point_ops:ctx.point_ops value)
    | None -> Admission.decode_deploy ~trusted ~point_ops:ctx.point_ops bytecode_raw
  in
  match checked with
  | Error error ->
    return (addr, { success = false; return_value = None; effort_used = 0;
             events = []; error = Some (Admission.error_message error); storage_writes = 0 })
| Ok admitted ->
  let admission = if source_checked then "source" else "binary" in
  let version =
    if source_checked then
      Option.value (Admission.compiler_version admitted) ~default:Oct_compile.lang_version
    else Oct_compile.lang_version
  in
  let code = Admission.code admitted in
  let profile = Admission.profile admitted in
  let strict_values = strict_values profile in
  let storage_kinds = storage_kinds profile in
  let exists = run_s (Octra_core.Store_irmin.contract_exists store addr) in
  let staged = Program_journal.has_deploy journal addr in
  if exists || staged then (
    Octra_log.warn "program" "event = deploy_exists addr = %s" addr;
    return (addr, { success = true; return_value = None; effort_used = 0;
             events = []; error = None; storage_writes = 0 })
  ) else
    match runtime_params (Admission.profile admitted) 0 params with
    | Error error ->
      return (addr, { success = false; return_value = None; effort_used = 0;
               events = []; error = Some (Program_input.error_message error); storage_writes = 0 })
    | Ok values ->
      let storage_tbl = Hashtbl.create 100 in
      let state = Contract_vm.create_state ~ctx ~strict_values ~storage_kinds
        ~caller:deployer ~origin:deployer ~address:addr ~value:Z.zero
        ~storage:storage_tbl () in
      state.memory.data <- Hashtbl.create 1024;
      Hashtbl.add state.memory.data 999 (Contract_vm.VString "constructor");
      Hashtbl.add state.memory.data 1000 (Contract_vm.VString "constructor");
      List.iteri (fun i value -> Hashtbl.add state.memory.data (1001 + i) value) values;
      Octra_log.info "program" "event = constructor_start addr = %s params = %d"
        addr (List.length params);
      let fixed = fix_jumps code in
      bind (run state fixed) (fun success ->
      let result = {
        success = success && not state.reverted;
        return_value = None;
        effort_used = state.effort_used;
        events = List.rev !(state.logs);
        error = if success && not state.reverted then None
                else Some "constructor failed";
        storage_writes = count_storage_writes state;
      } in
      if result.success then (
        Program_journal.add_deploy journal {
          address = addr;
          code_hash = hash;
          bytecode_b64 = Base64.encode_exn bytecode_raw;
          owner = deployer;
          ctype;
          admission;
          version;
          storage = storage_tbl;
        };
        Octra_log.info "program"
          "event = constructor_done addr = %s effort = %d persistence = deferred"
          addr result.effort_used;
        return (addr, result)
      ) else (
        Octra_log.error "program"
          "event = constructor_failed addr = %s effort = %d"
          addr result.effort_used;
        return (addr, result)
      ))

let deploy = deploy_with ~async_exec:false ~return:Fun.id ~bind:(fun value next -> next value)
  ~run:Contract_vm.run

let deploy_async = deploy_with ~async_exec:true ~return:Lwt.return
  ~bind:Octra_core.Exec_resource.bind
  ~run:(fun state code -> Contract_vm.run_async state code)

let spawn_with ~async_exec ~return ~bind ~run ~journal ?(trusted = []) ~(ctx : Contract_vm.exec_ctx) ~depth ?(limit = 1_000_000) ?(params = []) store ~deployer
    ~bytecode_raw ~nonce =
  let ctx = { ctx with async_exec } in
  match Admission.decode_deploy ~trusted ~point_ops:ctx.point_ops bytecode_raw with
  | Error (Admission.Decode_error error) -> return (Error (Printf.sprintf "bad bytecode: %s" error))
  | Error (Admission.Verify_error _) -> return (Error "verify failed")
  | Error (Admission.Unsafe_error error) -> return (Error error)
  | Ok admitted ->
      let code = Admission.code admitted in
      let profile = Admission.profile admitted in
      let strict_values = strict_values profile in
      let storage_kinds = storage_kinds profile in
      let addr = addr_from_code bytecode_raw deployer nonce in
      let exists = run_s (Octra_core.Store_irmin.contract_exists store addr) in
      let already_pending = Program_journal.has_deploy journal addr in
      if exists || already_pending then return (Error (Printf.sprintf "collision: %s" addr))
      else
        match runtime_values (Admission.profile admitted) 0 params with
        | Error error -> return (Error (Program_input.error_message error))
        | Ok values ->
          let hash = Digestif.SHA256.(digest_string bytecode_raw |> to_hex) in
          Octra_log.info "program"
            "event = spawn_start addr = %s deployer = %s nonce = %d depth = %d"
            addr (String.sub deployer 0 (min 12 (String.length deployer))) nonce depth;
          let storage_tbl = Hashtbl.create 100 in
          let depth = if ctx.fhe_work = Octra_core.Rule_graph.Active
            || ctx.byte_work <> None then depth else 0 in
          let state = Contract_vm.create_state ~ctx ~depth ~limit ~strict_values ~storage_kinds
            ~caller:deployer ~origin:deployer ~address:addr ~value:Z.zero
            ~storage:storage_tbl () in
          state.memory.data <- Hashtbl.create 1024;
          Hashtbl.add state.memory.data 999 (Contract_vm.VString "constructor");
          Hashtbl.add state.memory.data 1000 (Contract_vm.VString "constructor");
          List.iteri (fun i value -> Hashtbl.replace state.memory.data (1001 + i) value) values;
          let fixed = fix_jumps code in
          bind (run state fixed) (fun success ->
          if success && not state.reverted then (
            Program_journal.add_deploy journal {
              address = addr;
              code_hash = hash;
              bytecode_b64 = Base64.encode_exn bytecode_raw;
              owner = deployer;
              ctype = "CUSTOM";
              admission = "binary";
              version = Oct_compile.lang_version;
              storage = storage_tbl;
            };
            Octra_log.info "program"
              "event = spawn_done addr = %s effort = %d persistence = deferred"
              addr state.effort_used;
            return (Ok {
              Contract_vm.spawned_addr = addr;
              effort_used = state.effort_used;
              events = List.rev !(state.logs);
            })
          ) else (
            Octra_log.error "program" "event = spawn_failed addr = %s" addr;
            return (Error "constructor failed")
          ))

let deploy_internal = spawn_with ~async_exec:false ~return:Fun.id ~bind:(fun value next -> next value)
  ~run:Contract_vm.run

let deploy_inner_async = spawn_with ~async_exec:true ~return:Lwt.return
  ~bind:Octra_core.Exec_resource.bind
  ~run:(fun state code -> Contract_vm.run_async state code)

let upgrade ~journal ?(trusted = []) ~(ctx : Contract_vm.exec_ctx) store ~address ~caller
    ~expected_code_hash ~bytecode_raw =
  if Program_journal.has_upgrade journal address then
    Error "program upgrade already staged"
  else if Program_journal.has_deploy journal address then
    Error "staged program cannot be upgraded"
  else
    match run_s (Octra_core.Store_irmin.get_contract_meta store address) with
    | None ->
      Error "program not found"
    | Some meta when not (String.equal meta.owner caller) ->
      Error "only the program owner can upgrade"
    | Some meta when not (String.equal meta.code_hash expected_code_hash) ->
      Error "program upgrade code hash mismatch"
    | Some meta ->
      begin
        match Admission.decode_program ~trusted ~point_ops:ctx.point_ops bytecode_raw with
        | Error error ->
          Error (Admission.error_message error)
        | Ok _ ->
          let new_code_hash =
            Digestif.SHA256.(digest_string bytecode_raw |> to_hex) in
          if String.equal new_code_hash meta.code_hash then
            Error "program upgrade bytecode unchanged"
          else begin
            Program_journal.add_upgrade journal {
              address;
              expected_code_hash;
              code_hash = new_code_hash;
              bytecode_b64 = Base64.encode_exn bytecode_raw;
              owner = meta.owner;
              ctype = meta.ctype;
              admission = "binary";
              version = Oct_compile.lang_version;
            };
            Ok {
              old_code_hash = meta.code_hash;
              new_code_hash;
            }
          end
      end

let load_storage_with_overlay journal store program_addr =
  match Program_journal.load_storage journal program_addr with
  | Some storage -> storage
  | None -> load_storage store program_addr

let load_loaded_with_overlay ?(trusted = []) ?(point_ops = false) journal store program_addr =
  match Program_journal.find_upgrade journal program_addr with
  | Some upgrade ->
    (try
       decode_mode
         ~trusted
         ~point_ops
         upgrade.admission
         (Base64.decode_exn upgrade.bytecode_b64)
     with
     | (Stack_overflow | Out_of_memory) as error -> raise error
     | _ -> None)
  | None ->
    match Program_journal.find_deploy journal program_addr with
    | Some deploy ->
      (try
         decode_mode
           ~trusted
           ~point_ops
           deploy.admission
           (Base64.decode_exn deploy.bytecode_b64)
       with
       | (Stack_overflow | Out_of_memory) as error -> raise error
       | _ -> None)
    | None -> load_loaded ~trusted ~point_ops store program_addr

let load_bytecode_with_overlay ?(trusted = []) ?(point_ops = false) journal store program_addr =
  Option.map (fun loaded -> loaded.code)
    (load_loaded_with_overlay ~trusted ~point_ops journal store program_addr)

let contract_exists_with_overlay journal store program_addr =
  if Program_journal.has_deploy journal program_addr then true
  else run_s (Octra_core.Store_irmin.contract_exists store program_addr)

let run_checked ~journal ~(ctx : Contract_vm.exec_ctx) action =
  match ctx.proof_exec with
  | Octra_core.Rule_graph.Prior -> action ()
  | Octra_core.Rule_graph.Active ->
    let saved = Program_journal.snapshot journal in
    match action () with
    | result ->
      if not result.success then Program_journal.restore journal saved;
      result
    | exception error ->
      Program_journal.restore journal saved;
      raise error

let call_with ~return ~run ?(trusted = []) ?(ctx = Contract_vm.default_ctx) ?(depth = 0) ?(limit = 1_000_000)
    ~journal store program_addr method_name params caller value =
  Octra_log.info "program"
    "event = call_start addr = %s method = %s depth = %d limit = %d"
    program_addr method_name depth limit;
  let work = if ctx.proof_exec = Octra_core.Rule_graph.Active then
    Program_journal.snapshot_effort journal else Z.zero in
  match Cost.charge_z ~used:0 ~cost:work
      ~limit:(if ctx.proof_exec = Octra_core.Rule_graph.Active then limit else max_int) with
  | None ->
    return {success = false; return_value = None; effort_used = limit;
      events = []; error = Some "call journal effort exceeds limit"; storage_writes = 0}
  | Some spent ->
  let limit = limit - spent in
  match load_loaded_with_overlay
    ~trusted ~point_ops:ctx.point_ops journal store program_addr with
  | None ->
    return { success = false; return_value = None; effort_used = 0;
      events = []; error = Some "bytecode not found"; storage_writes = 0 }
  | Some loaded ->
    let fixed = fix_jumps loaded.code in
    match extract_method_arity fixed method_name with
    | Some arity when arity <> List.length params ->
      return { success = false; return_value = None; effort_used = 0;
        events = [];
        error = Some (Printf.sprintf "arity mismatch: %s expects %d args, got %d"
          method_name arity (List.length params));
        storage_writes = 0 }
    | _ ->
      (match extract_method_target fixed method_name with
       | None ->
         return { success = false; return_value = None; effort_used = 0;
           events = []; error = Some "method not found"; storage_writes = 0 }
       | Some target ->
         match runtime_params loaded.profile target params with
         | Error error ->
           return { success = false; return_value = None; effort_used = 0;
             events = []; error = Some (Program_input.error_message error); storage_writes = 0 }
         | Ok values ->
           run ~journal ~ctx ~spent (fun () ->
             let storage_tbl =
               Program_journal.checkout_storage journal program_addr
                 ~fallback:(fun () -> load_storage store program_addr)
             in
             let state = setup_call_state_values ~ctx ~depth ~limit ~caller
               ~strict_values:(strict_values loaded.profile)
               ~storage_kinds:(storage_kinds loaded.profile)
               ~address:program_addr ~value ~storage_tbl ~method_name ~params:values () in
             state, fixed))

let execute_call = call_with ~return:Fun.id ~run:(fun ~journal ~ctx ~spent prepare ->
  let result = run_checked ~journal ~ctx (fun () ->
    let state, fixed = prepare () in
    run_fixed_from_dispatcher state fixed) in
  {result with effort_used = result.effort_used + spent})

let execute_call_async = call_with ~return:Lwt.return ~run:(fun ~journal ~ctx ~spent prepare ->
  let saved = Program_journal.snapshot journal in
  Octra_core.Exec_resource.catch (fun () ->
    let state, fixed = prepare () in
    let state = { state with Contract_vm.ctx = { state.ctx with async_exec = true } } in
    let ( let* ) = Octra_core.Exec_resource.bind in
    let* result = run_fixed_async state fixed in
    if not result.success && ctx.proof_exec = Octra_core.Rule_graph.Active then
      Program_journal.restore journal saved;
    Lwt.return {result with effort_used = result.effort_used + spent})
    (fun error ->
      Program_journal.restore journal saved;
      Lwt.fail error))

let view_plan loaded method_name params =
  match loaded with
  | None ->
    Error { success = false; return_value = None; effort_used = 0;
      events = []; error = Some "bytecode not found"; storage_writes = 0 }
  | Some loaded ->
    let fixed = fix_jumps loaded.code in
    match extract_method_arity fixed method_name with
    | Some arity when arity <> List.length params ->
      Error { success = false; return_value = None; effort_used = 0;
        events = [];
        error = Some (Printf.sprintf "arity mismatch: %s expects %d args, got %d"
          method_name arity (List.length params));
        storage_writes = 0 }
    | _ ->
      (match extract_method_target fixed method_name with
       | None ->
         Error { success = false; return_value = None; effort_used = 0;
           events = []; error = Some "method not found"; storage_writes = 0 }
       | Some target ->
         match runtime_params loaded.profile target params with
         | Error error ->
           Error { success = false; return_value = None; effort_used = 0;
             events = []; error = Some (Program_input.error_message error); storage_writes = 0 }
         | Ok values -> Ok (loaded, fixed, values))

let view_state ~ctx ~depth ~limit ~caller ~address ~method_name loaded values storage =
  let state = setup_call_state_values ~ctx ~depth ~limit ~caller
    ~strict_values:(strict_values loaded.profile)
    ~storage_kinds:(storage_kinds loaded.profile)
    ~address ~value:Z.zero ~storage_tbl:(Hashtbl.copy storage)
    ~method_name ~params:values () in
  state.is_view <- true;
  state

let execute_view_call ?snapshot ?running ?(trusted = []) ?(ctx = Contract_vm.default_ctx)
    ?(depth = 0) ?(limit = 2_000_000_000) store address method_name params caller =
  let loaded = load_loaded ?snapshot ~trusted ~point_ops:ctx.point_ops store address in
  match view_plan loaded method_name params with
  | Error result -> result
  | Ok (loaded, fixed, values) ->
    let storage = load_storage ?snapshot store address in
    let state = view_state ~ctx ~depth ~limit ~caller ~address ~method_name
      loaded values storage in
    run_fixed_from_dispatcher ?running state fixed

let view_async ?snapshot ?running ?(trusted = []) ?(ctx = Contract_vm.default_ctx)
    ?(depth = 0) ?(limit = 2_000_000_000) store address method_name params caller =
  let ( let* ) work next = Lwt.bind work (Octra_core.Exec_resource.protect next) in
  let* loaded = load_async ?snapshot ~trusted ~point_ops:ctx.point_ops store address in
  match view_plan loaded method_name params with
  | Error result -> Lwt.return result
  | Ok (loaded, fixed, values) ->
    let* storage = Octra_core.Store_irmin.load_contract_storage ?snapshot store address in
    let ctx = {ctx with async_exec = true} in
    let state = view_state ~ctx ~depth ~limit ~caller ~address ~method_name
      loaded values storage in
    run_fixed_async ?running state fixed

let contract_exists store addr =
  run_s (Octra_core.Store_irmin.contract_exists store addr)