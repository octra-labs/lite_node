(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module R = Octra_vm.Program_record
module Rpc = Octra_vm.Contract_rpc
module Compile = Octra_vm.Oct_compile
module Vm = Octra_vm.Contract_vm
module S = Octra_core.Store_irmin
module C = Octra_core.Store_chaindata
module E = Octra_core.Epoch_exec
module T = Octra_core.Transaction
module Graph = Octra_core.Rule_graph

let expect text condition = if not condition then failwith text
let ok = function Ok value -> value | Error reason -> failwith reason
let run = Lwt_main.run
let address = "oct" ^ String.make 44 '3'
let owner = "oct" ^ String.make 44 '1'

let source = {|contract Scope {
  enum Choice { First }
  public fn run(): int {
    let kept = 1
    match Choice.First { Choice.First => { let kept = 2 } }
    let padding = 0
    let separate = 7
    kept = 9
    return separate
  }
}|}

let inputs ?(body = source) compiler loops = R.{main = "main.aml";
  files = [Octra_vm.Program_package.{path = "main.aml"; body}];
  compiler; loops}

let output (compiled : Compile.compile_result) =
  expect "test source did not compile" (compiled.error = None);
  let code = Octra_vm.Bytecode.decode compiled.bytecode |> Result.get_ok in
  let state = Octra_vm.Contract.setup_call_state ~caller:"caller" ~address:"program"
    ~value:Z.zero ~storage_tbl:(Hashtbl.create 1) ~method_name:"run" ~params:[] () in
  let result = Octra_vm.Contract.run_from_dispatcher state code in
  expect "test program did not finish" result.success;
  result.return_value

let test_compilers () =
  List.iter (fun syntax ->
    let old = Rpc.aml_result ~loops:false ~syntax source |> ok in
    let scoped = Rpc.aml_result ~loops:true ~syntax source |> ok in
    expect "old image did not expose local alias"
      (output old = Some (Vm.VInt (Z.of_int 9)));
    expect "scoped image changed separate local"
      (output scoped = Some (Vm.VInt (Z.of_int 7)));
    let result = Rpc.compile_record ~main:"main.aml" ~meta:None ~source
      ~files_json:None ~stored:old.bytecode in
    expect "RPC verified image with escaped local" (Result.is_error result);
    ignore (Rpc.compile_record ~main:"main.aml" ~meta:None ~source
      ~files_json:None ~stored:scoped.bytecode |> ok);
    let compiler = if syntax = Octra_vm.Oct_gen.Source then "source" else "forms" in
    expect "publication verified image with escaped local"
      (Result.is_error (R.verify (inputs compiler false) ~exact:false ~stored:old.bytecode));
    ignore (R.verify (inputs compiler true) ~exact:false ~stored:scoped.bytecode |> ok))
    [Octra_vm.Oct_gen.Source; Octra_vm.Oct_gen.Forms]

let stored_cases body main =
  R.profiles |> List.filter_map (fun (compiler, loops) ->
    if loops then None else
    let files = [Octra_vm.Program_package.{path = main; body}] in
    let inputs = R.{main; files; compiler; loops} in
    match R.compile inputs with
    | Error _ -> None
    | Ok (stored, result) ->
      expect ("profile did not expose local alias = " ^ compiler)
        (output result = Some (Vm.VInt (Z.of_int 9)));
      Some (inputs, stored, result))

let test_formats () =
  let program = "program" ^ String.sub source 8 (String.length source - 8) in
  let count = ref 0 in
  List.iter (fun body -> List.iter (fun main ->
    List.iter (fun (inputs, stored, result) ->
      incr count;
      let files_json = if main = "main.aml" then None else Some [] in
      let signed = Compile.attest_program ~key_id:"scope-test"
        ~private_key:(String.make 32 's') (Compile.emit_program result) in
      let images = List.sort_uniq String.compare
        (stored :: result.bytecode :: Option.to_list signed.program_envelope) in
      List.iter (fun image ->
        List.iter (fun kind ->
          let meta = S.{address; owner; code_hash = Digestif.SHA256.(digest_string image |> to_hex);
            version = result.version; ctype = "CUSTOM"; check = kind} in
          expect "verification list retained unsafe code"
            (match Rpc.verify_compilation ~main ~meta:(Some meta) ~source:body ~files_json with
            | Error _ -> true
            | Ok rows -> not (List.exists (fun (_, compiled) ->
                compiled.Compile.bytecode = result.bytecode) rows));
          begin match Rpc.compile_record ~main ~meta:(Some meta) ~source:body
              ~files_json ~stored:image with
          | Error reason -> expect ("RPC refused before byte comparison: " ^ reason)
              (String.starts_with ~prefix:"bytecode mismatch:" reason)
          | Ok _ -> failwith "RPC accepted unsafe stored format"
          end;
          expect "publication accepted unsafe stored format"
            (Result.is_error (R.verify inputs ~exact:(kind = "source") ~stored:image)))
          (if body = source then ["binary"] else ["binary"; "source"])) images)
      (stored_cases body main);
    let safe = { (inputs "source" true) with R.main = main;
      files = [Octra_vm.Program_package.{path = main; body}]} in
    let stored, _ = R.compile safe |> ok in
    ignore (Rpc.compile_record ~main ~meta:None ~source:body
      ~files_json:(if main = "main.aml" then None else Some []) ~stored |> ok))
    ["main.aml"; "entry.aml"]) [source; program];
  expect "compiler coverage missing" (!count = 24)

let test_saved () =
  let harmless = "program Safe { public fn run(): int { if false { let x = 1 } let y = 7 return y } }" in
  List.iter (fun body ->
  let images = List.concat_map (fun loops ->
    let result = Rpc.aml_result ~loops ~syntax:Octra_vm.Oct_gen.Source body |> ok in
    if body = harmless then expect "harmless branch changed execution"
      (output result = Some (Vm.VInt (Z.of_int 7)));
    let signed = Compile.attest_program ~key_id:"scope-read"
      ~private_key:(String.make 32 's') (Compile.emit_program result) in
    expect "saved signed image missing" (signed.error = None && signed.program_envelope <> None);
    [result.bytecode, result, loops; Option.get signed.program_envelope, signed, loops])
    [false; true] in
  List.iter (fun (image, result, accepted) ->
    List.iter (fun mode -> List.iter (fun main ->
      Test_workspace.with_dir "source-scope" (fun dir ->
        let store = run (S.open_store (Filename.concat dir "state")) in
        let cache = C.open_chaindata (Filename.concat dir "cache") in
        Fun.protect ~finally:(fun () -> run (S.close store); C.close cache) (fun () ->
          run (S.set_meta store "last_epoch" "0");
          let hash = Digestif.SHA256.(digest_string image |> to_hex) in
          run (S.deploy_contract store ~address ~owner ~code_hash:hash ~version:result.Compile.version
            ~ctype:"CUSTOM" ~check:"binary" ~bytecode_b64:(Base64.encode_exn image));
          let files = [Octra_vm.Program_package.{path = main; body}] in
          let text = if main = "main.aml" && mode = 0 then body
            else if main = "main.aml" then
              Yojson.Safe.to_string (`Assoc [main, `String body])
            else R.source_text ~main files in
          let record = R.make ~source:text ~code:image result in
          let save record = match C.save_program_record cache address record with
            | Ok _ -> ()
            | Error _ -> failwith "source record cache write failed" in
          if mode = 0 then begin
            run (R.stage store (address, record));
            if not accepted then save {record with C.source =
              "contract Other { fn run(): int { return 9 } }"}
          end
          else if mode = 1 then save record
          else Octra_core.Chaindata_index.set_meta_direct cache.C.index
            (C.program_record_legacy_key address) (R.raw record);
          let before = run (S.get_head_hash store) in
          let response = run (Rpc.source ~store ~chaindata:cache ~addr:address) |> Result.get_ok in
          let field name = Yojson.Safe.Util.member name response in
          expect "saved source became unavailable" (field "source" = `String body);
          expect "source read changed state" (before = run (S.get_head_hash store));
          if accepted then
            expect "saved verified report disappeared" (field "verification" <> `Null)
          else expect "saved source retained unsafe report"
            (field "verification" = `Null && field "certificate" = `Null
              && field "verified" = `Bool false
              && field "verification_status" = `String "scope_unproven"))))
      ["main.aml"; "entry.aml"]) [0; 1; 2])
    images) ["program" ^ String.sub source 8 (String.length source - 8); harmless]

let test_publication ?(body = source) ?(compiler = "source") () =
  List.iter (fun loops ->
  let inputs = inputs ~body compiler loops in
  let stored, result = R.compile inputs |> ok in
  Test_workspace.with_dir "scope-publish" (fun dir ->
    let store = run (S.open_store (Filename.concat dir "state")) in
    Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
      run (S.set_meta store "last_epoch" "0");
      let code_hash = Digestif.SHA256.(digest_string stored |> to_hex) in
      run (S.deploy_contract store ~address ~owner ~code_hash ~version:result.version
        ~ctype:"CUSTOM" ~check:"binary" ~bytecode_b64:(Base64.encode_exn stored));
      let ledger = Octra_core.Ledger.create store in
      List.iter (fun addr -> Octra_core.Ledger.add_account ledger addr (Z.of_int 10_000_000)
        |> ok) [owner; address];
      run (Octra_core.Ledger.flush_dirty_lwt ledger);
      let fold epoch = E.prior_fold epoch |> Result.map (fun value ->
        {value with E.program_records = Graph.Active}) in
      let backend = E.make_live_backend ~fold store ledger in
      run (backend.begin_batch Graph.Prior);
      let tx = T.{from = owner; to_ = address; amount = Z.zero; nonce = 1;
        ou = Z.of_int 1_000_000; timestamp = 170.; signature = ""; public_key = None;
        message = Some (Octra_vm.Program_upgrade.source_message ~expected_code_hash:code_hash);
        op_type = ContractUpgrade;
        encrypted_data = Some (Base64.encode_exn (R.encode_sources inputs))} in
      let secret = Base64.encode_exn (String.make 32 's') in
      let public_key = Mirage_crypto_ec.Ed25519.priv_of_octets (Base64.decode_exn secret)
        |> Result.get_ok |> Mirage_crypto_ec.Ed25519.pub_of_priv
        |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn in
      let tx = T.sign_with_privkey {tx with public_key = Some public_key} secret in
      expect "source publication signature invalid" (T.verify tx public_key);
      let env = E.{chain_id = "scope-test"; epoch_id = 17; proposer_addr = owner;
        validator_addrs = []; validator_pubkeys = []; prev_state_root = String.make 64 'a';
        epoch_ts = 170.; ready_state_root_at = None; ready_max_lag = -1} in
      let response = run (Octra_node_runtime.Consensus_vm_transition.process_tx ~backend ~env
        ~circle_mode:Graph.Prior ~wasm_compute_mode:Graph.Prior
        ~program_trust:Octra_vm.Program_trust.empty ~object_cost:false tx) in
      expect "BFT publication did not enforce scope"
        (match response with
        | Ok (E.Confirmed _) -> loops
        | Ok (E.Rejected_after_fee error) -> not loops && error.reason = "program source scope differs"
        | _ -> false);
      expect "publication stored wrong record"
        (Option.is_some (run (S.read store (R.key address))) = loops);
      expect "publication changed code"
        (run (S.read store ["contracts"; address; "bytecode"]) = Some (Base64.encode_exn stored)))))
    [false; true]

let test_for () =
  List.iter (fun (step, expected) ->
    let body = "program Count { public fn run(): int { let total = 0 for i in 0..3 { "
      ^ step ^ " total += 1 } return total } }" in
    let old_code, old = R.compile (inputs ~body "protocol" false) |> ok in
    let resolver name = if name = "main.aml" then Some body else None in
    let scoped = Compile.compile_multi_mode ~loops:true ~program_only:false resolver "main.aml" in
    expect "old for did not expose limit reuse" (output old = Some (Vm.VInt (Z.of_int expected)));
    expect "scoped for changed total" (output scoped = Some (Vm.VInt (Z.of_int 3)));
    expect "for RPC accepted reused limit"
      (Result.is_error (Rpc.compile_record ~main:"main.aml" ~meta:None ~source:body
        ~files_json:None ~stored:old_code));
    expect "for publication accepted reused limit"
      (Result.is_error (R.verify (inputs ~body "protocol" false) ~exact:false ~stored:old_code));
    expect "saved for accepted reused limit"
      (Result.is_error (R.record_scope ~stored:old_code (R.make ~source:body ~code:old_code old)));
    ignore (Rpc.compile_record ~main:"main.aml" ~meta:None ~source:body
      ~files_json:None ~stored:scoped.bytecode |> ok);
    test_publication ~body ~compiler:"package" ())
    ["let step = 100", 100; "", 1]

let test_reads () =
  let module Read = Octra_vm.Source_read in
  let compiled = Rpc.aml_result ~loops:true ~syntax:Octra_vm.Oct_gen.Source source |> ok in
  let input suffix = Read.{stored = Some (Base64.encode_exn compiled.bytecode);
    record = R.make ~source:(source ^ suffix) ~code:compiled.bytecode compiled} in
  let jobs = ref [] in
  let reader = Read.create ~check:(fun _ ->
    let task, wake = Lwt.wait () in
    jobs := !jobs @ [wake]; task) () in
  let first = Read.query reader (input "") in
  let second = Read.query reader (input " ") in
  expect "read capacity was not enforced" (run (Read.query reader (input "  ")) = Read.Pending);
  expect "duplicate read started compilation" (run (Read.query reader (input "")) = Read.Pending && List.length !jobs = 2);
  Lwt.cancel first;
  expect "cancelled read released active work" (run (Read.query reader (input "  ")) = Read.Pending);
  Lwt.wakeup (List.nth !jobs 0) Read.Checked;
  expect "completed read missing cache" (run (Read.query reader (input "")) = Read.Checked);
  let third = Read.query reader (input "  ") in
  expect "source digest did not isolate reads" (List.length !jobs = 3);
  Lwt.wakeup (List.nth !jobs 1) Read.Different;
  expect "different source result missing" (run second = Read.Different);
  Read.shutdown reader;
  Lwt.wakeup (List.nth !jobs 2) Read.Checked;
  expect "closed reader accepted completion" (run third = Read.Pending);
  expect "closed reader exposed cache" (run (Read.query reader (input "")) = Read.Pending);
  let state = ref Read.initial in
  for i = 0 to 200 do
    let key = string_of_int i in
    let active, effect = Read.step !state (Read.Ask key) in
    expect "read start did not return effect" (effect = Read.Check (0, key));
    state := fst (Read.step active (Read.Finish (0, key, Read.Checked)));
    expect "read cache exceeded capacity" (List.length !state.cache <= Read.cache_limit)
  done;
  let held, release = Lwt.wait () in
  let verification = Rpc.run_verify (fun () -> held) in
  Fun.protect ~finally:(fun () -> Lwt.wakeup release (Ok ()); ignore (run verification))
    (fun () -> test_saved ())

let test_deployment () =
  let module P = Octra_vm.Program_package in
  let module J = Octra_vm.Program_journal in
  let body = "program" ^ String.sub source 8 (String.length source - 8) in
  let base = T.{from = owner; to_ = address; amount = Z.zero; nonce = 1;
    ou = Z.of_int 1_000_000; timestamp = 170.; signature = ""; public_key = None;
    message = None; op_type = ProgramDeploy; encrypted_data = None} in
  List.iter (fun loops ->
    let package = P.compile_at ~loops ~compiler:P.Source ~point_ops:true ~main:"main.aml"
      ~sources:[P.{path = "main.aml"; body}]
      |> Result.map_error P.error_message |> ok in
    let tx = {base with encrypted_data = Some (Base64.encode_exn package.package)} in
    List.iter (fun mode ->
      let journal = J.create () in
      let prepare proof_exec = run (Octra_node_runtime.Consensus_epoch_vm_shell.prepare_program_package
        ~records:mode ~proof_exec ~overlap:false ~program_mode:Graph.Active ~point_ops:true tx) in
      let result = prepare (if loops then Graph.Active else Graph.Prior) in
      expect "deployment scope decision differs"
        (Result.is_ok result = (mode = Graph.Prior || loops));
      expect "active package verification lost scope"
        (Result.is_ok (prepare Graph.Active) = loops);
      Option.iter (fun parsed ->
        ignore (Octra_node_runtime.Consensus_epoch_vm_shell.stage_program_record
          ~mode ~journal tx parsed |> ok)) (Result.to_option result);
      expect "deployment published wrong record"
        (List.length (J.records journal) = if mode = Graph.Active && loops then 1 else 0))
      [Graph.Prior; Graph.Active]) [false; true]

let () =
  let checks = ["compilers", test_compilers; "formats", test_formats;
    "saved", test_saved; "publication", (fun () -> test_publication ()); "deployment", test_deployment;
    "for", test_for; "reads", test_reads] in
  let selected = match Array.to_list Sys.argv with
    | [_] -> checks
    | [_; name] -> [name, List.assoc name checks]
    | _ -> failwith "source scope test arguments invalid" in
  List.iter (fun (name, check) ->
    check ();
    Printf.printf "status = pass case = %s\n%!" name) selected;
  print_endline "status = pass test = source_scope"