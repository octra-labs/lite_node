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

let load_bytecode store contract_addr =
  match run_s (Octra_core.Store_irmin.load_bytecode store contract_addr) with
  | None -> None
  | Some b64 ->
    (try
       match Bytecode.decode (Base64.decode_exn b64) with
       | Ok code -> Some code
       | Error _ -> None
     with _ -> None)

let load_storage store contract_addr =
  run_s (Octra_core.Store_irmin.load_contract_storage store contract_addr)

let save_storage store contract_addr storage_tbl =
  run_s (Octra_core.Store_irmin.save_contract_storage store contract_addr storage_tbl)

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

let parse_param = function
  | `String s -> Contract_vm.VString s
  | `Bool b -> Contract_vm.VBool b
  | `Int i -> Contract_vm.VInt (Z.of_int i)
  | `Intlit s -> Contract_vm.VInt (Z.of_string s)
  | _ -> Contract_vm.VString ""

let count_storage_writes state =
  List.fold_left (fun acc e -> match e with
    | Contract_vm.UndoWrite _ -> acc + 1
    | Contract_vm.UndoMarker _ -> acc
  ) 0 state.Contract_vm.undo_stack

let setup_call_state ?(ctx=Contract_vm.default_ctx) ?(depth=0) ?(limit=1_000_000) ~caller ~address ~value ~storage_tbl ~method_name ~params () =
  let state = Contract_vm.create_state ~ctx ~depth ~limit ~caller ~origin:caller ~address ~value ~storage:storage_tbl () in
  state.memory.data <- Hashtbl.create 1024;
  Hashtbl.add state.memory.data 999 (Contract_vm.VString "call");
  Hashtbl.add state.memory.data 1000 (Contract_vm.VString method_name);
  List.iteri (fun i p -> Hashtbl.add state.memory.data (1001 + i) (parse_param p)) params;
  state

let run_fixed_from_dispatcher state fixed =
  let ok = match find_dispatcher fixed with
    | Some entry ->
      state.Contract_vm.pc <- entry;
      let success = Contract_vm.run state fixed in
      success && not state.reverted
    | None ->
      let success = Contract_vm.run state fixed in
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

let trim_error msg =
  if String.length msg <= 256 then msg
  else String.sub msg 0 256

let exec_result_to_result r =
  if r.success then
    Ok (Option.value r.return_value ~default:(Contract_vm.VBool true))
  else
    Error (trim_error (Option.value r.error ~default:"execution failed"))

let consensus_safety_error code =
  match Opcode_policy.require_consensus_safe code with
  | Ok () -> None
  | Error hit -> Some (Opcode_policy.error_message hit)

let deploy ?(ctx=Contract_vm.default_ctx) ?(params=[]) store deployer ctype code bytecode_raw nonce =
  let addr = addr_from_code bytecode_raw deployer nonce in
  let hash = Digestif.SHA256.(digest_string bytecode_raw |> to_hex) in
  Octra_log.stdout "info [contract] deploying addr = %s deployer = %s size = %d hash = %s\n%!"
    addr (String.sub deployer 0 (min 12 (String.length deployer)))
    (String.length bytecode_raw) (String.sub hash 0 16);
  match consensus_safety_error code with
  | Some error ->
    (addr, { success = false; return_value = None; effort_used = 0;
             events = []; error = Some error; storage_writes = 0 })
  | None ->
  let exists = run_s (Octra_core.Store_irmin.contract_exists store addr) in
  if exists then (
    Octra_log.stdout "warn [contract] already exists addr = %s\n%!" addr;
    (addr, { success = true; return_value = None; effort_used = 0;
             events = []; error = None; storage_writes = 0 })
  ) else (
    let storage_tbl = Hashtbl.create 100 in
    let state = Contract_vm.create_state ~ctx ~caller:deployer ~origin:deployer
      ~address:addr ~value:Z.zero ~storage:storage_tbl () in
    state.memory.data <- Hashtbl.create 1024;
    Hashtbl.add state.memory.data 999 (Contract_vm.VString "constructor");
    Hashtbl.add state.memory.data 1000 (Contract_vm.VString "constructor");
    List.iteri (fun i p -> Hashtbl.add state.memory.data (1001 + i) (parse_param p)) params;
    Octra_log.stdout "info [contract] running constructor addr = %s params = %d\n%!" addr (List.length params);
    let fixed = fix_jumps code in
    let success = Contract_vm.run state fixed in
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
      Octra_log.stdout "info [contract] constructor ok addr = %s effort = %d\n%!" addr result.effort_used;
      run_s (Octra_core.Store_irmin.deploy_contract store
        ~address:addr ~code_hash:hash ~version:Oct_compile.lang_version
        ~owner:deployer ~ctype ~bytecode_b64:(Base64.encode_exn bytecode_raw));
      save_storage store addr storage_tbl;
      (try
         let abi_json = Yojson.Safe.to_string (`Assoc [
           "contract_type", `String ctype; "address", `String addr;
           "deployer", `String deployer; "version", `String Oct_compile.lang_version]) in
         run_s (Octra_core.Store_irmin.save_contract_abi store addr abi_json);
         Octra_log.stdout "info [contract] abi saved addr = %s\n%!" addr
       with e ->
         Octra_log.stdout "warn [contract] abi save failed addr = %s err = %s\n%!" addr (Printexc.to_string e));
      (addr, result)
    ) else (
      Octra_log.stdout "error [contract] constructor failed addr = %s effort = %d\n%!" addr result.effort_used;
      (addr, result)
    )
  )

type pending_deploy = {
  pd_address : string;
  pd_code_hash : string;
  pd_bytecode_b64 : string;
  pd_owner : string;
  pd_storage : (string, string) Hashtbl.t;
}

let pending_deploys : pending_deploy list ref = ref []

let pending_storage : (string, (string, string) Hashtbl.t) Hashtbl.t = Hashtbl.create 16

let commit_pending_deploys store =
  List.iter (fun pd ->
    run_s (Octra_core.Store_irmin.deploy_contract store
      ~address:pd.pd_address ~code_hash:pd.pd_code_hash
      ~version:Oct_compile.lang_version
      ~owner:pd.pd_owner ~ctype:"CUSTOM" ~bytecode_b64:pd.pd_bytecode_b64);
    save_storage store pd.pd_address pd.pd_storage;
    (try
       let abi_json = Yojson.Safe.to_string (`Assoc [
         "contract_type", `String "CUSTOM"; "address", `String pd.pd_address;
         "deployer", `String pd.pd_owner; "version", `String Oct_compile.lang_version]) in
       run_s (Octra_core.Store_irmin.save_contract_abi store pd.pd_address abi_json)
     with _ -> ())
  ) (List.rev !pending_deploys);
  pending_deploys := []

let commit_pending_storage store =
  let pending =
    Hashtbl.fold (fun addr tbl acc -> (addr, tbl) :: acc) pending_storage []
    |> List.sort (fun (left, _) (right, _) -> String.compare left right)
  in
  List.iter (fun (addr, tbl) -> save_storage store addr tbl) pending;
  Hashtbl.clear pending_storage

let discard_pending_deploys () =
  pending_deploys := []

let discard_pending_storage () =
  Hashtbl.clear pending_storage

let deploy_internal ~ctx ~depth ?(params=[]) store ~deployer ~bytecode_raw ~nonce =
  match Bytecode.decode bytecode_raw with
  | Error e -> Error (Printf.sprintf "bad bytecode: %s" e)
  | Ok code ->
    (match Contract_vm.Verifier.verify code with
    | Error _ -> Error "verify failed"
    | Ok () ->
      match consensus_safety_error code with
      | Some error -> Error error
      | None ->
      let addr = addr_from_code bytecode_raw deployer nonce in
      let exists = run_s (Octra_core.Store_irmin.contract_exists store addr) in
      let already_pending = List.exists (fun pd -> pd.pd_address = addr) !pending_deploys in
      if exists || already_pending then Error (Printf.sprintf "collision: %s" addr)
      else
        let hash = Digestif.SHA256.(digest_string bytecode_raw |> to_hex) in
        Octra_log.stdout "info [contract] spawn addr = %s deployer = %s nonce = %d depth = %d\n%!"
          addr (String.sub deployer 0 (min 12 (String.length deployer))) nonce depth;
        let storage_tbl = Hashtbl.create 100 in
        let state = Contract_vm.create_state ~ctx ~caller:deployer ~origin:deployer
          ~address:addr ~value:Z.zero ~storage:storage_tbl () in
        state.memory.data <- Hashtbl.create 1024;
        Hashtbl.add state.memory.data 999 (Contract_vm.VString "constructor");
        Hashtbl.add state.memory.data 1000 (Contract_vm.VString "constructor");
        List.iteri (fun i p -> Hashtbl.replace state.memory.data (1001 + i) p) params;
        let fixed = fix_jumps code in
        let success = Contract_vm.run state fixed in
        if success && not state.reverted then (
          pending_deploys := {
            pd_address = addr;
            pd_code_hash = hash;
            pd_bytecode_b64 = Base64.encode_exn bytecode_raw;
            pd_owner = deployer;
            pd_storage = storage_tbl;
          } :: !pending_deploys;
          Hashtbl.replace pending_storage addr (Hashtbl.copy storage_tbl);
          Octra_log.stdout "info [contract] spawn ok addr = %s effort = %d (deferred)\n%!" addr state.effort_used;
          Ok {
            Contract_vm.spawned_addr = addr;
            effort_used = state.effort_used;
            events = List.rev !(state.logs);
          }
        ) else (
          Octra_log.stdout "error [contract] spawn constructor failed addr = %s\n%!" addr;
          Error "constructor failed"
        ))

let load_storage_with_overlay store contract_addr =
  match Hashtbl.find_opt pending_storage contract_addr with
  | Some tbl -> Hashtbl.copy tbl
  | None -> load_storage store contract_addr

let load_bytecode_with_overlay store contract_addr =
  match List.find_opt (fun pd -> pd.pd_address = contract_addr) !pending_deploys with
  | Some pd ->
    (try
       match Bytecode.decode (Base64.decode_exn pd.pd_bytecode_b64) with
       | Ok code -> Some code
       | Error _ -> None
     with _ -> None)
  | None -> load_bytecode store contract_addr

let contract_exists_with_overlay store contract_addr =
  if List.exists (fun pd -> pd.pd_address = contract_addr) !pending_deploys then true
  else run_s (Octra_core.Store_irmin.contract_exists store contract_addr)

let execute_call ?(ctx=Contract_vm.default_ctx) ?(depth=0) ?(limit=1_000_000) store contract_addr method_name params caller value =
  Octra_log.stdout "info [contract] call addr = %s method = %s depth = %d limit = %d\n%!" contract_addr method_name depth limit;
  match load_bytecode_with_overlay store contract_addr with
  | None ->
    { success = false; return_value = None; effort_used = 0;
      events = []; error = Some "bytecode not found"; storage_writes = 0 }
  | Some bytecode ->
    let fixed = fix_jumps bytecode in
    match extract_method_arity fixed method_name with
    | Some arity when arity <> List.length params ->
      { success = false; return_value = None; effort_used = 0;
        events = [];
        error = Some (Printf.sprintf "arity mismatch: %s expects %d args, got %d"
          method_name arity (List.length params));
        storage_writes = 0 }
    | _ ->
    let storage_tbl = load_storage_with_overlay store contract_addr in
    let state = setup_call_state ~ctx ~depth ~limit ~caller ~address:contract_addr
      ~value ~storage_tbl ~method_name ~params () in
    let result = run_fixed_from_dispatcher state fixed in
    if result.success then Hashtbl.replace pending_storage contract_addr storage_tbl;
    result

let execute_view_call ?(ctx=Contract_vm.default_ctx) ?(depth=0) ?(limit=2_000_000_000) store contract_addr method_name params caller =
  match load_bytecode_with_overlay store contract_addr with
  | None ->
    { success = false; return_value = None; effort_used = 0;
      events = []; error = Some "bytecode not found"; storage_writes = 0 }
  | Some bytecode ->
    let fixed = fix_jumps bytecode in
    match extract_method_arity fixed method_name with
    | Some arity when arity <> List.length params ->
      { success = false; return_value = None; effort_used = 0;
        events = [];
        error = Some (Printf.sprintf "arity mismatch: %s expects %d args, got %d"
          method_name arity (List.length params));
        storage_writes = 0 }
    | _ ->
    let storage_tbl = load_storage store contract_addr in
    let storage_copy = Hashtbl.copy storage_tbl in
    let state = setup_call_state ~ctx ~depth ~limit ~caller ~address:contract_addr
      ~value:Z.zero ~storage_tbl:storage_copy ~method_name ~params () in
    state.is_view <- true;
    run_fixed_from_dispatcher state fixed

let contract_exists store addr =
  run_s (Octra_core.Store_irmin.contract_exists store addr)