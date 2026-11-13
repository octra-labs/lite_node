(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_core
module V = Octra_vm
module VM = V.Contract_vm
module R = C.Rule_graph
module S = C.Store_irmin

let need reason value = if not value then failwith reason
let run = Lwt_main.run
let unwrap = function Ok value -> value | Error _ -> failwith "expected success"
let base58 value =
  let alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" in
  let rec encode value result =
    if Z.equal value Z.zero then result else
      let quotient, digit = Z.ediv_rem value (Z.of_int 58) in
      encode quotient (String.make 1 alphabet.[Z.to_int digit] ^ result) in
  "oct" ^ encode value ""

let receipt success effort error : V.Contract.exec_result = {
  success; effort_used = effort; error; return_value = None; events = []; storage_writes = 0;
}

let () =
  need "at most one output path required" (Array.length Sys.argv <= 2);
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--invalid-plan" then begin
    let chain_id = "octra-devnet-9871-cluster" in
    let refused read = try ignore (read ()); false with Invalid_argument _ -> true in
    need "invalid source schedule accepted" (refused (fun () -> R.source_create_at ~chain_id ~epoch:max_int));
    need "invalid source profile accepted" (refused (fun () -> R.consensus_id ~chain_id ~epoch:max_int));
    need "invalid source epochs accepted" (refused (fun () -> R.profile_epochs ~chain_id));
    print_endline "event = source_plan status = refused";
    exit 0
  end;
  let output = if Array.length Sys.argv = 2 then Sys.argv.(1) else begin
    if not (Sys.file_exists "runtime_data") then Unix.mkdir "runtime_data" 0o700;
    Filename.concat "runtime_data/source_rules" (string_of_int (Unix.getpid ()))
  end in
  need "output exists" (not (Sys.file_exists output));
  let parent = Filename.dirname output in
  if not (Sys.file_exists parent) then Unix.mkdir parent 0o700;
  Unix.mkdir output 0o700;
  let power = Z.shift_left Z.one 256 in
  need "wallet maximum accepted" (not (Program_spawn.non_key (base58 Z.(pred power))));
  need "program minimum refused" (Program_spawn.non_key (base58 power));
  need "program next refused" (Program_spawn.non_key (base58 Z.(succ power)));
  List.iter (fun address -> need "invalid address accepted" (not (Program_spawn.non_key address)))
    [""; "oct"; "oct" ^ String.make 44 '0'; "oct" ^ String.make 43 'z'; "abc" ^ String.make 44 'z'];
  let parent = base58 power and other = base58 Z.(succ power) in
  let compiled = V.Program_package.compile_sources_at ~metered:true ~loops:true ~point_ops:true
    ~compiler:V.Program_package.Preview ~main:"main.aml"
    ~sources:[V.Program_package.{path = "main.aml";
      body = "program Child { state { n: int } constructor(n: int) { self.n = n } }"}] |> unwrap in
  let encoded = Base64.encode_exn compiled.package in
  let tx : C.Transaction.t = {from = "oct" ^ String.make 44 '1'; to_ = parent;
    amount = Z.zero; nonce = 1; ou = Z.of_int 1_000_000; timestamp = 1800000000.;
    signature = ""; public_key = None; encrypted_data = Some "create"; op_type = ProgramExec;
    message = Some (C.Json_tree.write (`Assoc ["program_create", `String encoded; "params", `List []]))} in
  let call : V.Call_plan.direct_exec = {method_name = "create"; params = []; amount = Z.zero;
    value_effect = V.Call_plan.Value_noop; effort_limit = 1_000_000} in
  let store = run (S.open_store ~fresh:true (Filename.concat output "store")) in
  Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
    let ctx = {VM.default_ctx with scalar_work = true; point_ops = true; math = true;
      proof_exec = R.Active; fhe_work = R.Active; async_exec = true;
      byte_work = Some (V.Byte_work.create V.Byte_work.consensus_limits)} in
    List.iter (fun (mode, input, success) ->
      let result = run (Program_spawn.run ~mode ~metered:true ~ctx ~tx:input ~call
        ~rules:(fun () -> failwith "ordinary call resolved creation rules")
        ~make_ctx:(fun _ -> failwith "ordinary call prepared creation")
        ~exec:(fun ~ctx:_ ~limit:_ ~params ->
          need "ordinary parameters changed" (params = call.params);
          Lwt.return (receipt true 0 None))) in
      need "ordinary call status changed" (result.success = success))
      [R.Prior, tx, true; R.Active, {tx with message = Some "[]"}, true;
       R.Active, {tx with op_type = C.Transaction.ContractCall}, true;
       R.Active, {tx with message = Some "{\"program_create\":1,\"params\":[]}"}, false];
    let check name ?(deployer = parent) ?(depth = 1) ?(nonce = 0)
        ?(params = [VM.VInt Z.one]) ?(context = ctx) ?(collision = false) ?(stored = false)
        ?(repeat = false) ?(excess = false) ?(program = R.Active) ?(records = R.Active) expected =
      let journal = V.Program_journal.create () in
      let address = V.Contract.addr_from_code compiled.envelope deployer nonce in
      if collision || stored then begin
        V.Program_journal.add_deploy journal {address; code_hash = "occupied";
          bytecode_b64 = Base64.encode_exn compiled.envelope; owner = parent;
          ctype = "CUSTOM"; check = "source"; version = "2.0 AML-preview"; storage = Hashtbl.create 1};
        if stored then begin
          run (S.begin_epoch_batch store);
          V.Program_store.stage store journal;
          run (S.commit_epoch_batch store "occupied");
          V.Program_journal.discard journal
        end
      end;
      let before = V.Program_journal.snapshot journal in
      let checked = ref None and called = ref false in
      let result = run (Program_spawn.run ~mode:R.Active ~metered:true ~ctx:context
        ~rules:(fun () -> program, records) ~tx ~call
        ~make_ctx:(fun value -> checked := Some value; context)
        ~exec:(fun ~ctx ~limit ~params:_ ->
          called := true;
          let preparation = V.Program_package.scalar_cost encoded |> Option.get |> Z.to_int in
          need "compilation budget not shared" (limit = call.effort_limit - preparation);
          if excess then Lwt.return (receipt false (limit + 1) (Some "excess")) else
            let create () = Program_spawn.execute (Option.get !checked) ~journal ~ctx ~depth ~limit
              ~params store ~deployer ~bytecode_raw:compiled.envelope ~nonce in
            let open Lwt.Syntax in
            let* first = create () in
            let* final = if repeat && Result.is_ok first then create () else Lwt.return first in
            Lwt.return (match final with
              | Ok child -> receipt true child.VM.effort_used None
              | Error error -> receipt false 0 (Some error)))) in
      need (name ^ " reason differs") (result.error = expected);
      need (name ^ " status differs") (result.success = Option.is_none expected);
      if program = R.Prior || records = R.Prior || not context.scalar_work then
        need (name ^ " executed inactive program") (not !called);
      if Option.is_some expected && not repeat then
        need (name ^ " changed journal") (V.Program_journal.snapshot journal = before);
      V.Program_journal.discard journal;
      Printf.printf "event = source_rule case = %s status = passed\n%!" name in
    check "accepted" None;
    check "depth" ~depth:2 (Some "program child scope differs");
    check "parent" ~deployer:other (Some "program child scope differs");
    check "nonce" ~nonce:(-1) (Some "program child scope differs");
    check "collision" ~collision:true (Some "program child address exists");
    check "repeat" ~repeat:true (Some "program creation already used");
    check "excess" ~excess:true (Some "program creation total work exceeds limit");
    check "program" ~program:R.Prior (Some "program creation rules inactive");
    check "records" ~records:R.Prior (Some "program creation rules inactive");
    check "scalar_mode" ~context:{ctx with scalar_work = false} (Some "program creation rules inactive");
    check "wide_value" ~params:[VM.VInt (Z.shift_left Z.one V.Scalar_work.bit_limit)]
      (Some "program child scalar invalid");
    check "stored" ~stored:true (Some "program child address exists"));
  print_endline "event = source_rules status = passed"