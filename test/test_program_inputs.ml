(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_core
module V = Octra_vm
module N = Octra_node_runtime
module S = C.Store_irmin
module X = C.Epoch_exec

let check reason value = if not value then failwith reason
let run = Lwt_main.run
let get = function Ok value -> value | Error reason -> failwith reason
let chain = "octra-devnet-9871-cluster"
let activation = Octra_consensus.C_quorum_policy.round_epoch ~chain_id:chain
  |> Option.get |> Int64.to_int
let owner = "oct" ^ String.make 44 '1'
let address = "oct" ^ String.make 44 '2'
let child = "oct" ^ String.make 44 '3'

let with_store path action =
  let store = run (S.open_store path) in
  Fun.protect ~finally:(fun () -> run (S.close store)) (fun () -> action store)

let write path bytes =
  let out = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out out) (fun () -> output_string out bytes)

let host_error action =
  try ignore (action ()); false with C.Exec_resource.Unavailable Host -> true

let test_registry () =
  Test_workspace.with_dir "program-keys" (fun dir ->
    with_store (Filename.concat dir "state") (fun store ->
      run (S.begin_epoch_batch ~mode:C.Rule_graph.Active store);
      run (S.set_meta store "last_epoch" "1");
      run (S.set_pvac_pubkey store owner "before");
      run (S.commit_epoch_batch store "keys");
      let snapshot = run (S.capture_read_snapshot store) |> get in
      run (S.begin_epoch_batch ~mode:C.Rule_graph.Active store);
      run (S.set_pvac_pubkey store owner "after");
      let strict ?snapshot name = run (S.get_pvac_pubkey ?snapshot ~registered:true store name) in
      check "live registered key differs" (strict owner = Some "after");
      check "snapshot key changed" (strict ~snapshot owner = Some "before");
      let old = "blobs/" ^ S.pvac_hash "before" in
      write (S.pvac_legacy_path store child) "local-key";
      check "prior local read changed" (run (S.get_pvac_pubkey store old) = Some "before");
      check "prior legacy read changed" (run (S.get_pvac_pubkey store child) = Some "local-key");
      List.iter (fun name ->
        check "unregistered file escaped registry" (strict name = None);
        check "unregistered snapshot read differs" (strict ~snapshot name = None))
        [old; child; "../pvac/blobs/" ^ S.pvac_hash "before";
         S.pvac_blob_path store (S.pvac_hash "before"); "oct/" ^ String.make 43 '1'];
      run (S.write store (S.pvac_hash_path child) "none");
      check "deleted key read legacy file" (strict child = None);
      Unix.unlink (S.pvac_blob_path store (S.pvac_hash "after"));
      check "missing registered key became absence" (host_error (fun () -> strict owner));
      write (S.pvac_blob_path store (S.pvac_hash "before")) "damaged";
      check "damaged snapshot key became absence" (host_error (fun () -> strict ~snapshot owner));
      S.abort_epoch_batch store));
  print_endline "status = pass test = key_registry"

let test_emission () =
  let open V in
  let check_emit declaration cap =
    let env = Oct_emit.make_env ~metered:true false false declaration [] [] [] [] [] [] [] in
    check "legacy emitter capacity differs" (env.capacity = cap);
    for _ = 1 to cap - 1 do Oct_emit.emit env Contract_vm.NOP done;
    check "legacy emitter refused below limit" (env.emitted = cap - 1);
    Oct_emit.emit env Contract_vm.NOP;
    check "legacy emitter exact limit differs" (List.length env.code = cap);
    let refused = try Oct_emit.emit env Contract_vm.NOP; false
      with Oct_emit.Emission_limit -> true in
    check "legacy emitter exceeded capacity" refused;
    check "legacy emitter mutated on refusal" (env.emitted = cap && List.length env.code = cap)
  in
  let check_gen declaration cap =
    let env = Oct_gen.make_env ~metered:true ~loops:false Oct_gen.Source declaration
      [] [] [] [] [] [] [] [] [] in
    check "source emitter capacity differs" (env.capacity = cap);
    for _ = 1 to cap do Oct_gen.emit env Contract_vm.NOP done;
    let refused = try Oct_gen.emit env Contract_vm.NOP; false
      with Oct_gen.Emission_limit -> true in
    check "source emitter exceeded capacity" refused;
    check "source emitter mutated on refusal" (env.emitted = cap && List.length env.code = cap)
  in
  List.iter (fun (declaration, cap) -> check_emit declaration cap; check_gen declaration cap)
    [Oct_lang.ProgramDecl, Program_limits.max_instructions;
     Oct_lang.ContractDecl, Bytecode.max_instrs];
  print_endline "status = pass test = emission_capacity"

let orbit count suffix = Printf.sprintf
  "program Orbit { public fn read(): int { let result = orbit[%d] from 0 with once x: int => (orbit[%d] from x with once y: int => y + 1) %s } }"
  count count suffix

let test_compile () =
  let open V in
  let small = orbit 2 "return result" in
  let left = Oct_compile.compile small in
  let right = Oct_compile.compile ~metered:true small in
  check "small orbit refused" (left.error = None && right.error = None);
  check "meter changed ordinary compiler bytes" (left = right);
  check "RPC refused ordinary source" (Result.is_ok (run (Contract_rpc.compile_aml ~source:small)));
  let text = orbit 160 "return missing_name" in
  let old = Oct_compile.compile text in
  let now = Oct_compile.compile ~metered:true text in
  check "prior compiler lost late error" (old.error <> None && old.error <> now.error);
  check "emission refusal differs" (now.error = Some "Program instruction limit exceeded");
  check "RPC omitted emission counter" ((Contract_rpc.compile_source text None).error = now.error);
  check "RPC accepted excessive orbit" (Result.is_error (run (Contract_rpc.compile_aml
    ~source:(orbit 160 "return result"))));
  let resolver path = if path = "main.aml" then Some text else None in
  check "multi-file emitter escaped limit"
    ((Oct_compile.compile_multi ~metered:true resolver "main.aml").error = now.error);
  let huge = Oct_parse.parse (orbit 1_000_000 "return result") in
  check "nested legacy orbit escaped capacity"
    (try ignore (Oct_emit.generate ~metered:true huge); false with Oct_emit.Emission_limit -> true);
  check "nested source orbit escaped capacity"
    (try ignore (Oct_gen.generate ~metered:true ~syntax:Oct_gen.Source huge); false
      with Oct_gen.Emission_limit -> true);
  let files = [Program_package.{path = "main.aml"; body = orbit 160 "return result"}] in
  List.iter (fun (compiler, loops) ->
    let source = Program_record.{main = "main.aml"; files; compiler; loops} in
    check "source publication compiled excessive orbit"
      (Result.is_error (Program_record.verify ~metered:true source ~exact:true ~stored:left.bytecode)))
    Program_record.profiles;
  print_endline "status = pass test = orbit_compile"

let env epoch = X.{chain_id = chain; epoch_id = epoch; proposer_addr = owner;
  validator_addrs = []; validator_pubkeys = []; prev_state_root = String.make 64 'a';
  epoch_ts = 170.; ready_state_root_at = None; ready_max_lag = -1}

let code = lazy (
  let result = V.Oct_compile.compile
    "program KeyRead { state { n: int } public fn read(key: string): int { let pk = fhe_load_pk(key) self.n = 1 return 1 } public fn relay(target: address, key: string): int { return to_int(call(target, \"read\", key)) } }" in
  match result.error with None -> result.bytecode | Some reason -> failwith reason)

let key = lazy (
  let pk, _ = Pvac_ffi.keygen_from_seed (Pvac_ffi.default_params ()) (Bytes.make 32 '\011') in
  Pvac_ffi.serialize_pubkey pk |> Bytes.to_string)

let old_key = lazy (
  let pk, _ = Pvac_ffi.keygen_from_seed (Pvac_ffi.default_params ()) (Bytes.make 32 '\012') in
  Pvac_ffi.serialize_pubkey pk |> Bytes.to_string)

let setup store old =
  let bytes = Lazy.force code in
  run (S.begin_epoch_batch ~mode:C.Rule_graph.Prior store);
  run (S.set_meta store "last_epoch" "1");
  run (S.set_pvac_pubkey store child (Lazy.force key));
  if old then S.write_blob store (S.pvac_hash (Lazy.force old_key)) (Lazy.force old_key);
  List.iter (fun target -> run (S.deploy_contract store ~address:target ~owner ~ctype:"CUSTOM"
    ~version:"1" ~check:"binary" ~code_hash:Digestif.SHA256.(digest_string bytes |> to_hex)
    ~bytecode_b64:(Base64.encode_exn bytes))) [address; child];
  run (S.commit_epoch_batch store "program keys")

let execute store epoch kind nested key_name =
  let ledger = C.Ledger.create store in
  get (C.Ledger.add_account ledger owner (Z.of_int 10_000_000));
  let fold height = X.prior_fold height |> Result.map (fun plan ->
    {plan with X.proof_exec = C.Rule_graph.proof_exec_at ~chain_id:chain ~epoch:height;
      proof_key = C.Rule_graph.proof_key_at ~chain_id:chain ~epoch:height;
      fhe_work = C.Rule_graph.fhe_work_at ~chain_id:chain ~epoch:height}) in
  let backend = X.make_live_backend ~fold store ledger in
  run (backend.begin_batch C.Rule_graph.Prior);
  let method_name, params = if nested then "relay", [`String child; `String key_name]
    else "read", [`String key_name] in
  let message = if kind = C.Transaction.MultiExec then
    `List [`Assoc ["to", `String address; "method", `String method_name;
      "params", `List params; "amount", `String "0"]] else `List params in
  let tx = C.Transaction.{from = owner; to_ = address; amount = Z.zero; nonce = 1;
    ou = Z.of_int 5_000_000; timestamp = 170.; signature = ""; public_key = None;
    message = Some (Yojson.Safe.to_string message); op_type = kind;
    encrypted_data = Some method_name} in
  let receipts = ref [] in
  let outcome = run (N.Consensus_vm_transition.process_tx ~backend ~env:(env epoch)
    ~circle_mode:C.Rule_graph.Prior ~wasm_compute_mode:C.Rule_graph.Prior
    ~program_trust:V.Program_trust.empty ~object_cost:false
    ~save_receipt_raw:(fun ~tx_hash ~json -> receipts := (tx_hash, json) :: !receipts) tx) in
  run (C.Ledger.flush_dirty_lwt ledger);
  let root = run (S.get_batch_tree_hash store) in
  let payer = Option.get (C.Ledger.find_opt ledger owner) in
  S.abort_epoch_batch store;
  outcome, !receipts, root, payer.balance, payer.nonce

let test_transition () =
  Test_workspace.with_dir "program-key-pair" (fun dir ->
    with_store (Filename.concat dir "history") (fun history ->
      with_store (Filename.concat dir "image") (fun image ->
        setup history true;
        setup image false;
        check "test stores have different state" (run (S.get_head_hash history) = run (S.get_head_hash image));
        let old = "blobs/" ^ S.pvac_hash (Lazy.force old_key) in
        List.iter (fun epoch ->
          let lhs, _, left_root, _, _ = execute history epoch C.Transaction.ContractCall false old in
          let rhs, _, right_root, _, _ = execute image epoch C.Transaction.ContractCall false old in
          check "prior history key control failed" (match lhs with Ok (X.Confirmed _) -> true | _ -> false);
          check "prior image key control failed" (match rhs with Ok (X.Rejected_after_fee _) -> true | _ -> false);
          check "pre-activation key rule changed" (left_root <> right_root)) [1_737_332; 1_737_333; activation - 1];
        List.iter (fun epoch -> List.iter (fun kind -> List.iter (fun nested ->
          let lhs = execute history epoch kind nested old in
          let rhs = execute image epoch kind nested old in
          check "same state produced different key outcome" (lhs = rhs);
          let outcome, receipts, _, _, nonce = lhs in
          check "missing key was confirmed" (match outcome with Ok (X.Rejected_after_fee _) -> true | _ -> false);
          check "missing key lost receipt or nonce" (receipts <> [] && nonce = 1);
          let lhs = execute history epoch kind nested child in
          let rhs = execute image epoch kind nested child in
          check "registered key differs between stores" (lhs = rhs);
          let outcome, _, _, _, _ = lhs in
          check "registered key failed" (match outcome with Ok (X.Confirmed _) -> true | _ -> false)
        ) [false; true]) [C.Transaction.ContractCall; C.Transaction.ProgramExec; C.Transaction.MultiExec])
          [activation; activation + 1];
        Unix.unlink (S.pvac_blob_path history (S.pvac_hash (Lazy.force key)));
        List.iter (fun kind -> List.iter (fun nested ->
          let refused = host_error (fun () -> execute history activation kind nested child) in
          S.abort_epoch_batch history;
          check "missing registered key became transaction outcome" refused
        ) [false; true]) [C.Transaction.ContractCall; C.Transaction.ProgramExec; C.Transaction.MultiExec])));
  print_endline "status = pass test = program_key_transition"

let package_text envelope body =
  let buffer = Buffer.create 1024 in
  let number width value = for index = 0 to width - 1 do
    Buffer.add_char buffer (Char.chr ((value lsr (index * 8)) land 255)) done in
  let string width value = number width (String.length value); Buffer.add_string buffer value in
  Buffer.add_string buffer "OCPS";
  number 2 1;
  number 2 1;
  string 2 "main.aml";
  number 2 1;
  string 2 "main.aml";
  string 4 body;
  string 4 envelope;
  Base64.encode_exn (Buffer.contents buffer)

let test_deploy_compile () =
  let source = orbit 2 "return result" in
  let compiled = V.Program_package.compile ~main:"main.aml"
    ~sources:[{path = "main.aml"; body = source}]
    |> Result.map_error V.Program_package.error_message |> get in
  let encoded = package_text compiled.envelope (orbit 160 "return missing_name") in
  let expected metered = match V.Program_package.admit_base64 ~metered ~point_ops:true encoded with
    | Error error -> V.Program_package.error_message error
    | Ok _ -> failwith "invalid package accepted" in
  let prior = expected false in
  let active = expected true in
  check "package error order control ineffective" (prior <> active);
  check "package did not stop during emission"
    (active = "Program compile failed: Program instruction limit exceeded");
  Test_workspace.with_dir "program-deploy-meter" (fun dir ->
    with_store (Filename.concat dir "state") (fun store ->
      List.iter (fun epoch ->
        let ledger = C.Ledger.create store in
        get (C.Ledger.add_account ledger owner (Z.of_int 100_000_000));
        let backend = X.make_live_backend ~fold:X.prior_fold store ledger in
        run (backend.begin_batch C.Rule_graph.Prior);
        let tx = C.Transaction.{from = owner; to_ = address; amount = Z.zero;
          nonce = 1; ou = Z.of_int 1_000_000; timestamp = 170.; signature = "";
          public_key = None; message = None; op_type = ProgramDeploy;
          encrypted_data = Some encoded} in
        let outcome = run (N.Consensus_vm_transition.process_tx ~backend ~env:(env epoch)
          ~circle_mode:C.Rule_graph.Prior ~wasm_compute_mode:C.Rule_graph.Prior
          ~program_trust:V.Program_trust.empty ~object_cost:false tx) in
        let reason = match outcome with
          | Ok (X.Rejected_after_fee error) -> error.reason
          | Error (_, reason) -> failwith reason
          | Ok (X.Confirmed _) -> failwith "invalid deploy confirmed" in
        check "BFT compiler used wrong activation rule"
          (reason = if epoch < activation then prior else active);
        let payer = Option.get (C.Ledger.find_opt ledger owner) in
        check "compiler refusal lost fee or nonce"
          (payer.nonce = 1 && Z.equal payer.balance (Z.of_int 99_000_000));
        S.abort_epoch_batch store)
        [1_737_332; 1_737_333; activation - 1; activation; activation + 1]));
  print_endline "status = pass test = deploy_emission"

let test_declaration () =
  let open V in
  let ordinary = orbit 2 "return result" in
  let rejected = "contract OnlyOld { public fn read(): int { let n = orbit[160] from 0 with once x: int => (orbit[160] from x with once y: int => y + 1) return missing_name } }" in
  let resolver body path = if path = "main.aml" then Some body else None in
  List.iter (fun compile ->
    let old : Oct_compile.compile_result = compile false (resolver ordinary) "main.aml" in
    let fresh = compile true (resolver ordinary) "main.aml" in
    check "declaration check changed a valid program" (old.error = None && old = fresh);
    let measure metered =
      Gc.compact ();
      let start = Gc.allocated_bytes () in
      let result = compile metered (resolver rejected) "main.aml" in
      result, Gc.allocated_bytes () -. start in
    let prior, prior_work = measure false in
    let active, active_work = measure true in
    Printf.printf "event = declaration_error prior = %S active = %S\n%!"
      (Option.value prior.error ~default:"none") (Option.value active.error ~default:"none");
    check "declaration refusal occurred after generation"
      (active.error = Some "Program declaration required"
       && prior.error = Some "orbit node limit = 100000 actual = 102721");
    check "rejected declaration still generated code" (active_work *. 10. < prior_work);
    let reads = ref [] in
    let load path = reads := path :: !reads;
      resolver ("import Value from \"absent.aml\"\n" ^ rejected) path in
    let result = compile true load "main.aml" in
    check "rejected declaration loaded imports"
      (result.error = active.error && !reads = ["main.aml"]))
    [(fun metered -> Oct_compile.compile_program_source ~metered ~loops:false);
     (fun metered -> Oct_compile.compile_program_preview ~metered ~loops:false)];
  List.iter (fun loops ->
    let source = "contract Ordinary { public fn read(): int { return 7 } }" in
    let result = Aml_source.compile_multi ~metered:true ~loops ~program_only:false
      ~syntax:Oct_gen.Source (resolver source) "main.aml" |> get in
    check "generic compiler lost Contract support" (result.declaration = Oct_lang.ContractDecl))
    [false; true];
  let compiled = Program_package.compile ~main:"main.aml"
    ~sources:[{path = "main.aml"; body = ordinary}]
    |> Result.map_error Program_package.error_message |> get in
  let package = package_text compiled.envelope rejected in
  List.iter (fun compiler ->
    check "package declaration gate was bypassed"
      (match Program_package.admit_base64 ~metered:true ~point_ops:true ~compiler package with
       | Error error -> Program_package.error_message error = "Program compile failed: Program declaration required"
       | Ok _ -> false))
    [Program_package.Source; Program_package.Preview];
  Test_workspace.with_dir "declaration-route" (fun dir ->
    with_store (Filename.concat dir "state") (fun store ->
      List.iter (fun epoch ->
        let ledger = C.Ledger.create store in
        get (C.Ledger.add_account ledger owner (Z.of_int 100_000_000));
        let fold height = X.prior_fold height |> Result.map (fun plan ->
          {plan with X.program_mode = C.Rule_graph.Active; fhe_work = C.Rule_graph.Active}) in
        let backend = X.make_live_backend ~fold store ledger in
        run (backend.begin_batch C.Rule_graph.Prior);
        let tx = C.Transaction.{from = owner; to_ = address; amount = Z.zero;
          nonce = 1; ou = Z.of_int 1_000_000; timestamp = 170.; signature = "";
          public_key = None; message = None; op_type = ProgramDeploy;
          encrypted_data = Some package} in
        let result = run (N.Consensus_vm_transition.process_tx ~backend ~env:(env epoch)
          ~circle_mode:C.Rule_graph.Prior ~wasm_compute_mode:C.Rule_graph.Prior
          ~program_trust:Program_trust.empty ~object_cost:false tx) in
        let reason = match result with Ok (X.Rejected_after_fee error) -> error.reason
          | _ -> failwith "declaration deploy produced wrong outcome" in
        check "BFT declaration check ignored activation"
          ((reason = "Program compile failed: Program declaration required") = (epoch >= activation));
        let payer = Option.get (C.Ledger.find_opt ledger owner) in
        check "declaration refusal changed fee or nonce"
          (payer.nonce = 1 && Z.equal payer.balance (Z.of_int 99_000_000));
        S.abort_epoch_batch store) [activation - 1; activation; activation + 1]));
  print_endline "status = pass test = declaration_route"

let test_source_work () =
  let plain = V.Oct_compile.compile (orbit 2 "return result") in
  check "ordinary source did not compile" (plain.error = None);
  let code_hash = Digestif.SHA256.(digest_string plain.bytecode |> to_hex) in
  Test_workspace.with_dir "source-work" (fun dir ->
    with_store (Filename.concat dir "store") (fun store ->
      run (S.deploy_contract store ~address ~owner ~ctype:"CUSTOM" ~version:plain.version
        ~check:"binary" ~code_hash ~bytecode_b64:(Base64.encode_exn plain.bytecode));
      let source_files multi body =
        if multi then V.Program_package.[
          {path = "main.aml"; body = "import Value from \"other.aml\"\n" ^ body};
          {path = "other.aml"; body = "interface Value { fn read(): int }"}]
        else V.Program_package.[{path = "main.aml"; body}] in
      let execute ?(compiler = "protocol") ?(multi = false) ?(expected_hash = code_hash) epoch body =
        let ledger = C.Ledger.create store in
        get (C.Ledger.add_account ledger owner (Z.of_int 100_000_000));
        let fold height = X.prior_fold height |> Result.map (fun plan ->
          {plan with X.program_records = C.Rule_graph.Active}) in
        let backend = X.make_live_backend ~fold store ledger in
        run (backend.begin_batch C.Rule_graph.Prior);
        let files = source_files multi body in
        let sources = V.Program_record.{main = "main.aml"; compiler; loops = false; files} in
        let tx = C.Transaction.{from = owner; to_ = address; amount = Z.zero;
          nonce = 1; ou = Z.of_int 1_000_000; timestamp = 170.; signature = "";
          public_key = None; op_type = ContractUpgrade;
          message = Some (V.Program_upgrade.source_message ~expected_code_hash:expected_hash);
          encrypted_data = Some (Base64.encode_exn (V.Program_record.encode_sources sources))} in
        Gc.compact ();
        let before = Gc.allocated_bytes () in
        let result = run (N.Consensus_vm_transition.process_tx ~backend ~env:(env epoch)
          ~circle_mode:C.Rule_graph.Prior ~wasm_compute_mode:C.Rule_graph.Prior
          ~program_trust:V.Program_trust.empty ~object_cost:false tx) in
        let allocated = Gc.allocated_bytes () -. before in
        S.abort_epoch_batch store;
        result, allocated in
      List.iter (fun epoch ->
        let result, _ = execute epoch (orbit 2 "return result") in
        check "ordinary publication failed" (match result with Ok (X.Confirmed _) -> true | _ -> false))
        [activation - 1; activation];
      let text = orbit 400 "return missing_name" in
      let prior, old_work = execute (activation - 1) text in
      let active, new_work = execute activation text in
      let rejected = function Ok (X.Rejected_after_fee error) ->
        error.reason = "program source compilation failed" | _ -> false in
      check "publication error changed" (rejected prior && rejected active);
      Printf.printf "event = publication_work prior = %.0f active = %.0f\n" old_work new_work;
      check "BFT publication ignored emission budget" (new_work *. 2. < old_work);
      List.iter (fun compiler -> List.iter (fun multi ->
        let body = "program Honest { public fn read(): int { return 1 } }" in
        let files = source_files multi body in
        let sources = V.Program_record.{main = "main.aml"; compiler; loops = false; files} in
        let code, compiled = match V.Program_record.compile ~metered:false sources with
          | Ok value -> value
          | Error reason ->
            let package = V.Program_package.compile_sources_at ~metered:false ~loops:false
              ~compiler:V.Program_package.Protocol ~point_ops:false ~main:"main.aml" ~sources:files in
            failwith (Printf.sprintf "historical compiler = %s multi = %b reason = %s package = %s"
              compiler multi reason (match package with Ok _ -> "ok" | Error e -> V.Program_package.error_message e)) in
        check "honest historical output differs" (compiled.error = None && code <> "");
        let expected_hash = Digestif.SHA256.(digest_string code |> to_hex) in
        run (S.deploy_contract store ~address ~owner ~ctype:"CUSTOM" ~version:compiled.version
          ~check:"binary" ~code_hash:expected_hash
          ~bytecode_b64:(Base64.encode_exn code));
        let prior, _ = execute ~compiler ~multi ~expected_hash (activation - 1) body in
        check "honest historical publication refused before activation"
          (match prior with Ok (X.Confirmed _) -> true | _ -> false);
        let active, _ = execute ~compiler ~multi ~expected_hash activation body in
        check "honest historical publication accepted after activation" (rejected active))
        [false; true]) ["historical"; "package_historical"];
      run (S.deploy_contract store ~address ~owner ~ctype:"CUSTOM" ~version:plain.version
        ~check:"binary" ~code_hash ~bytecode_b64:(Base64.encode_exn plain.bytecode));
      let text = "program History { state { n: u256 } public fn read(): int { "
        ^ String.concat " " (List.init 5_800 (fun _ -> "self.n = 1"))
        ^ " return missing_name } }" in
      let plain = V.Prior_compile.compile text in
      check "historical refusal changed"
        (plain.error = Some "line 0: undefined variable: missing_name");
      List.iter (fun compiler -> List.iter (fun multi ->
        let prior, old_work = execute ~compiler ~multi (activation - 1) text in
        let active, new_work = execute ~compiler ~multi activation text in
        check "historical publication error changed" (rejected prior && rejected active);
        Printf.printf "event = historical_work compiler = %s multi = %b prior = %.0f active = %.0f\n%!"
          compiler multi old_work new_work;
        check "historical publication ignored emission budget" (new_work +. 100_000. < old_work))
        [false; true]) ["historical"; "package_historical"]));
  print_endline "status = pass test = source_work"

let all () =
  test_emission ();
  test_compile ();
  test_registry ();
  test_transition ();
  test_deploy_compile ();
  test_declaration ();
  test_source_work ()

let () = if Array.length Sys.argv = 2 then match Sys.argv.(1) with
  | "source" -> test_source_work ()
  | "declaration" -> test_declaration ()
  | _ -> failwith "unknown program input test"
  else all ()