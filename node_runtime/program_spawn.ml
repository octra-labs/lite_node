(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module V = Octra_vm
module VM = V.Contract_vm
module R = Octra_core.Rule_graph
module T = Octra_core.Transaction

type t = {
  parent : string;
  package : V.Program_package.admitted;
  record : Octra_core.Store_chaindata.program_record;
  mutable used : bool;
  mutable failed_work : int;
}

let non_key address =
  let alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" in
  if String.length address <> 47 || not (String.starts_with ~prefix:"oct" address) then false
  else
    let number = String.sub address 3 44 |> String.fold_left (fun number byte ->
      Option.bind number (fun value -> Option.map (fun digit ->
        Z.(add (mul value (of_int 58)) (of_int digit))) (String.index_opt alphabet byte))) (Some Z.zero) in
    Option.fold ~none:false ~some:(fun value -> Z.numbits value > 256) number

let request mode (tx : T.t) =
  if mode = R.Prior || tx.op_type <> T.ProgramExec then Ok None
  else match tx.message with
  | None -> Ok None
  | Some raw ->
    try match Octra_core.Json_tree.read raw with
    | `Assoc fields when List.mem_assoc "program_create" fields ->
      if List.sort String.compare (List.map fst fields) <> ["params"; "program_create"] then
        Error "program creation fields invalid"
      else begin match List.assoc "program_create" fields, List.assoc "params" fields with
      | `String package, `List params ->
        if V.Scalar_work.json_cost params <> None &&
            Tx_view.call_params_ok {tx with message = Some (Octra_core.Json_tree.write (`List params))} then Ok (Some (package, params))
        else Error "program creation parameters refused"
      | _ -> Error "program creation payload invalid"
      end
    | _ -> Ok None
    with
    | (Out_of_memory | Stack_overflow) as error -> raise error
    | _ -> Error "program creation JSON invalid"

let prepare ~rules ~metered ~(ctx : VM.exec_ctx) ~limit (tx : T.t) encoded =
  let program, records = rules () in
  if program <> R.Active || records <> R.Active || not ctx.scalar_work || not ctx.point_ops || ctx.fhe_work <> R.Active
      || ctx.proof_exec <> R.Active || ctx.byte_work = None then
    Lwt.return_error "program creation rules inactive"
  else if not (non_key tx.to_) then Lwt.return_error "program creation parent can be a wallet"
  else if Z.lt tx.ou (Z.of_int 200_000) then Lwt.return_error "program source fee below minimum"
  else if Z.gt (V.Scalar_work.load (String.length encoded)) (Z.of_int limit) then
    Lwt.return_error "program compilation work exceeds limit"
  else match V.Program_package.scalar_cost encoded with
  | None -> Lwt.return_error "program compilation work invalid"
  | Some cost ->
    match V.Cost.charge_z ~used:0 ~cost ~limit with
    | None -> Lwt.return_error "program compilation work exceeds limit"
    | Some spent ->
      Octra_core.Exec_resource.detach (fun () ->
        let ( let* ) = Result.bind in
        let* package = V.Program_package.admit_base64 ~metered
          ~compiler:V.Program_package.Preview ~point_ops:true ~loops:true ~consts:true encoded
          |> Result.map_error V.Program_package.error_message in
        let* () = if V.Code_check.compiler_version package.program = Some "2.0 AML-preview" then Ok ()
          else Error "program child compiler differs" in
        let record = V.Program_record.of_package package in
        let* () = V.Program_record.check record in
        if Z.lt tx.ou (V.Program_record.fee record) then Error "program source fee below byte cost"
        else Ok ({parent = tx.to_; package; record; used = false; failed_work = 0}, spent)) ()

let execute checked ~journal ~(ctx : VM.exec_ctx) ~depth ~limit ~params store
    ~deployer ~bytecode_raw ~nonce =
  let ( let* ) = Octra_core.Exec_resource.bind in
  if checked.used then Lwt.return_error "program creation already used"
  else if deployer <> checked.parent || not (non_key deployer) || depth <> 1 || nonce < 0 then
    Lwt.return_error "program child scope differs"
  else if bytecode_raw <> checked.package.envelope then Lwt.return_error "program child envelope differs"
  else if not ctx.scalar_work || not (List.for_all VM.scalar_valid params) then
    Lwt.return_error "program child scalar invalid"
  else
    let json = List.map (V.Receipt_view.nested_call_arg_json ~typed:true) params in
    match V.Contract.creation_work true limit bytecode_raw json with
    | None -> Lwt.return_error "program creation work refused"
    | Some spent ->
      let address = V.Contract.addr_from_code bytecode_raw deployer nonce in
      let* exists = Octra_core.Store_irmin.contract_exists store address in
      if exists || V.Program_journal.has_deploy journal address then
        Lwt.return_error "program child address exists"
      else match V.Contract.runtime_values (V.Code_check.profile checked.package.program) 0 params with
      | Error error -> Lwt.return_error (V.Program_input.error_message error)
      | Ok values ->
        checked.used <- true;
        let profile = V.Code_check.profile checked.package.program in
        let storage = Hashtbl.create 100 in
        let state = VM.create_state ~ctx:{ctx with async_exec = true} ~depth ~limit
          ~strict_values:(V.Contract.strict_values profile) ~storage_kinds:(V.Contract.storage_kinds profile)
          ~caller:deployer ~origin:deployer ~address ~value:Z.zero ~storage () in
        state.effort_used <- spent;
        Hashtbl.replace state.memory.data 999 (VM.VString "constructor");
        Hashtbl.replace state.memory.data 1000 (VM.VString "constructor");
        List.iteri (fun index value -> Hashtbl.replace state.memory.data (1001 + index) value) values;
        let* success = VM.run_async state (V.Contract.fix_jumps (V.Code_check.code checked.package.program)) in
        if not success || state.reverted then begin
          checked.failed_work <- state.effort_used;
          Lwt.return_error "program child constructor failed"
        end
        else begin
          V.Program_journal.add_deploy journal {address;
            code_hash = checked.record.code_hash;
            bytecode_b64 = Base64.encode_exn bytecode_raw;
            owner = deployer; ctype = "CUSTOM"; check = "source";
            version = "2.0 AML-preview"; storage};
          V.Program_journal.add_record journal address checked.record;
          Lwt.return_ok {VM.spawned_addr = address; effort_used = state.effort_used;
            events = List.rev !(state.logs)}
        end

let refused effort reason : V.Contract.exec_result = {
  success = false; return_value = None; effort_used = effort;
  events = []; error = Some reason; storage_writes = 0;
}

let run ~mode ~rules ~metered ~(ctx : VM.exec_ctx) ~make_ctx ~(tx : T.t)
    ~(call : V.Call_plan.direct_exec) ~exec =
  let ( let* ) = Octra_core.Exec_resource.bind in
  let limit = V.Scalar_work.allowance ctx.scalar_work tx.ou call.effort_limit in
  match request mode tx with
  | Error reason -> Lwt.return (refused 0 reason)
  | Ok None -> exec ~ctx ~limit ~params:call.params
  | Ok (Some (encoded, params)) ->
    let* prepared = prepare ~rules ~metered ~ctx ~limit tx encoded in
    match prepared with
    | Error reason -> Lwt.return (refused limit reason)
    | Ok (checked, spent) ->
      let* receipt = exec ~ctx:(make_ctx checked) ~limit:(limit - spent) ~params in
      let work = V.Cost.charge ~used:receipt.effort_used ~cost:checked.failed_work ~limit:(limit - spent) in
      if receipt.success && not checked.used then Lwt.return (refused (spent + receipt.effort_used)
        "program child was not created")
      else match Option.bind work (fun work -> V.Cost.charge ~used:spent ~cost:work ~limit) with
      | None -> Lwt.return (refused limit "program creation total work exceeds limit")
      | Some effort_used -> Lwt.return {receipt with effort_used}