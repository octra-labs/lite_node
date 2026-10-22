(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module S = Octra_core.Store_irmin
module C = Octra_core.Store_chaindata
module E = Octra_core.Epoch_exec
module T = Octra_core.Transaction
module R = Octra_vm.Program_record
module Rpc = Octra_vm.Contract_rpc
module U = Octra_vm.Program_upgrade
module Graph = Octra_core.Rule_graph

let run = Lwt_main.run
let expect label value = if not value then failwith label
let ok = function Ok value -> value | Error reason -> failwith reason
let field name = Yojson.Safe.Util.member name
let () =
  expect "source publication rule differs"
    (R.consensus_id =
      "program_source:owner:code_hash:report:state:deploy:historical:declared_compiler:base_200000:bytes_16:limit_2097152:constant_graph:scoped_code:for_limit:"
      ^ Octra_vm.Source_stamp.id)
let source = "contract Record { state { n: int } constructor() { self.n = 0 } view fn read(): int { return self.n } }"
let address = "oct" ^ String.make 44 '3'
let owner = "oct" ^ String.make 44 '1'
let stranger = "oct" ^ String.make 44 '2'
let secret = Base64.encode_exn (String.make 32 's')
let public_key =
  Mirage_crypto_ec.Ed25519.priv_of_octets (Base64.decode_exn secret)
  |> Result.get_ok |> Mirage_crypto_ec.Ed25519.pub_of_priv
  |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn

let with_store path action =
  let store = run (S.open_store path) in
  Fun.protect ~finally:(fun () -> run (S.close store)) (fun () -> action store)

let with_cache path action =
  let cache = C.open_chaindata path in
  Fun.protect ~finally:(fun () -> C.close cache) (fun () -> action cache)

let compiled = Octra_vm.Oct_compile.compile source
let code =
  match compiled.error with None -> compiled.bytecode | Some reason -> failwith reason
let code_hash = Digestif.SHA256.(digest_string code |> to_hex)

let install store =
  run (S.set_meta store "last_epoch" "0");
  run (S.deploy_contract store ~address ~owner ~code_hash ~version:compiled.version
    ~ctype:"CUSTOM" ~check:"binary" ~bytecode_b64:(Base64.encode_exn code));
  let storage = Hashtbl.create 1 in
  Hashtbl.add storage "n" "73";
  run (S.save_contract_storage store address storage)

let install_code store ~kind stored =
  run (S.set_meta store "last_epoch" "0");
  run (S.deploy_contract store ~address ~owner
    ~code_hash:Digestif.SHA256.(digest_string stored |> to_hex)
    ~version:compiled.version ~ctype:"CUSTOM" ~check:kind
    ~bytecode_b64:(Base64.encode_exn stored))

let request ?(sender = owner) ?(expected = code_hash) ?(text = source) ?fee nonce =
  let data = R.{main = "main.aml"; compiler = "protocol"; loops = false;
    files = [Octra_vm.Program_package.{path = "main.aml"; body = text}]} in
  let record = Rpc.compile_record ~main:"main.aml" ~meta:None ~source
    ~files_json:None ~stored:code |> ok in
  let tx = T.{from = sender; to_ = address; amount = Z.zero; nonce;
    ou = Option.value fee ~default:(R.fee record); timestamp = 170.;
    signature = ""; public_key = Some public_key;
    message = Some (U.source_message ~expected_code_hash:expected);
    op_type = ContractUpgrade;
    encrypted_data = Some (Base64.encode_exn (R.encode_sources data))} in
  T.sign_with_privkey tx secret

let env = E.{chain_id = "record-test"; epoch_id = 17; proposer_addr = owner;
  validator_addrs = []; validator_pubkeys = []; prev_state_root = String.make 64 'a';
  epoch_ts = 170.; ready_state_root_at = None; ready_max_lag = -1}

let make_backend store mode =
  let ledger = Octra_core.Ledger.create store in
  List.iter (fun who -> Octra_core.Ledger.add_account ledger who (Z.of_int 10_000_000)
    |> ok) [owner; stranger; address];
  run (Octra_core.Ledger.flush_dirty_lwt ledger);
  let fold epoch = E.prior_fold epoch |> Result.map (fun value ->
    {value with E.program_records = mode}) in
  let backend = E.make_live_backend ~fold store ledger in
  run (backend.begin_batch Graph.Prior);
  backend

let apply backend tx =
  Octra_node_runtime.Consensus_vm_transition.process_tx ~backend ~env
    ~circle_mode:Graph.Prior ~wasm_compute_mode:Graph.Prior
    ~program_trust:Octra_vm.Program_trust.empty ~object_cost:false tx |> run

let confirmed = function
  | Ok (E.Confirmed _) -> ()
  | Ok (E.Rejected_after_fee error) -> failwith error.reason
  | Error (_, reason) -> failwith reason

let refused = function
  | Ok (E.Rejected_after_fee error) -> error.reason
  | Error (_, reason) -> reason
  | Ok (E.Confirmed _) -> failwith "invalid publication accepted"

let invariant store =
  List.map (fun path -> path, run (S.read store path))
    [["contracts"; address; "bytecode"]; ["contracts"; address; "meta"];
     ["contracts"; address; "storage"; "n"]]

let test_sources () =
  let tx = request 1 in
  let _, decoded = U.source_request tx |> ok in
  expect "decoded source differs"
    (decoded.main = "main.aml" && List.map (fun file -> file.Octra_vm.Program_package.body)
      decoded.files = [source]);
  let refuse payload =
    expect "invalid source package accepted"
      (Result.is_error (U.source_request {tx with encrypted_data = Some payload})) in
  refuse (Option.get tx.encrypted_data ^ "\n");
  refuse (String.make (R.max_bytes + 1) 'a');
  let duplicate = R.{decoded with files = decoded.files @ decoded.files} in
  refuse (Base64.encode_exn (R.encode_sources duplicate));
  refuse (Base64.encode_exn "{\"main\":\"main.aml\",\"main\":\"main.aml\",\"files\":[]}");
  refuse (Base64.encode_exn (R.encode_sources {decoded with compiler = "unknown"}));
  refuse (Base64.encode_exn (R.encode_sources {decoded with compiler = "historical"; loops = true}));
  expect "different bytecode accepted"
    (Result.is_error (R.verify decoded ~exact:true ~stored:(code ^ "\000")));
  expect "nonzero source value accepted"
    (Result.is_error (U.source_request {tx with amount = Z.one}));
  expect "changed source retained signature"
    (not (T.verify {tx with encrypted_data = Some "changed"} public_key));
  expect "changed expected code retained signature"
    (not (T.verify {tx with message = Some (U.source_message
      ~expected_code_hash:(String.make 64 '0'))} public_key));
  print_endline "status = pass test = program_record_sources"

let test_const_graph () =
  let open Octra_vm.Oct_lang in
  let base = Octra_vm.Oct_parse.parse
    "program P { const A: int = 7 const B: int = 9 }" in
  let first = List.hd base.consts in
  let second = List.nth base.consts 1 in
  let check value = Octra_vm.Oct_scope.check_consts
    {base with consts = [{first with c_value = value}; second]} in
  let mark = "A", Once, TInt in
  let uses = [
    ELet ("A", Once, TInt, EInt Z.zero, EVar "A");
    ESplit (ETuple [EInt Z.zero; EInt Z.one], mark, ("other", Once, TInt), EVar "A");
    EOrbit (Octra_vm.C_nat.one, Some (EVar "A"), EInt Z.zero, mark, EVar "A");
    EUse {ux_name = "read"; ux_caps = [EVar "A"]; ux_arg = EInt Z.zero;
      ux_mult = Once; ux_bind = "A"; ux_typ = TInt; ux_body = EVar "A"};
    EEqual (TInt, EVar "A", EInt Z.zero);
    EStoragePath ("slot", [EVar "A"], ["value"]);
  ] in
  List.iter (fun value ->
    expect "constant reference under expression accepted"
      (Result.is_error (check value))) uses;
  let duplicate values = Octra_vm.Oct_scope.check_consts
    {base with consts = List.map (fun value -> {first with c_value = value}) values} in
  expect "first cyclic definition accepted"
    (Result.is_error (duplicate [EVar "A"; EInt Z.zero]));
  expect "later unused cycle refused"
    (duplicate [EInt Z.zero; EVar "A"] = Ok ());
  expect "local without constant refused"
    (Octra_vm.Oct_scope.check_consts
      {base with consts = [{first with c_value = ELet ("local", Once, TInt,
        EInt Z.zero, EVar "local")}; second]} = Ok ());
  print_endline "status = pass test = program_record_const_graph"

let test_publish () =
  Test_workspace.with_dir "program-record" (fun dir ->
    with_store (Filename.concat dir "state") (fun store ->
      install store;
      let original = invariant store in
      let backend = make_backend store Graph.Active in
      let balance = (Option.get (backend.ops.find_opt address)).balance in
      let tx = request 1 in
      expect "publication signature invalid" (T.verify tx public_key);
      let decoded = T.of_yojson (T.to_yojson tx) |> ok in
      expect "transaction encoding changed" (decoded = tx);
      with_cache (Filename.concat dir "cache") (fun chaindata ->
        let result = run (Rpc.verify ~store ~chaindata ~addr:address ~source ~files_json:None)
          |> Result.get_ok in
        expect "RPC claimed chain publication" (field "published" result = `Bool false);
        expect "publication request missing" (field "publication" result <> `Null);
        expect "RPC modified state" (run (S.read store (R.key address)) = None));
      let root = run (S.get_head_hash store) in
      apply backend decoded |> confirmed;
      expect "program code or state changed" (invariant store = original);
      expect "program balance changed" ((Option.get (backend.ops.find_opt address)).balance = balance);
      let stored = run (R.read ~store ~address ~code_hash ()) |> ok in
      expect "confirmed publication missing" (Option.is_some stored);
      let record = Option.get stored in
      expect "source missing" (record.source = source);
      expect "report missing" (Option.is_some record.report);
      expect "certificate missing" (Option.is_some record.certificate);
      with_cache (Filename.concat dir "before-commit") (fun chaindata ->
        let checked = run (Rpc.verify ~store ~chaindata ~addr:address
          ~source ~files_json:None) |> Result.get_ok in
        expect "uncommitted record reported as published"
          (field "published" checked = `Bool false);
        let visible = run (Rpc.source ~store ~chaindata ~addr:address) |> Result.get_ok in
        expect "uncommitted source escaped batch" (field "source" visible = `Null));
      let old = R.raw record in
      apply backend (request 2) |> confirmed;
      expect "republication changed record" (run (S.read store (R.key address)) = Some old);
      run (backend.flush_dirty ());
      run (backend.commit_batch ());
      expect "state root omitted record" (run (S.get_head_hash store) <> root);
      let commit = run (S.get_commit_hash store) |> Option.get in
      let expected_root = run (S.get_head_hash store) |> Option.get in
      with_store (Filename.concat dir "replayed") (fun replay ->
        install replay;
        let replay_backend = make_backend replay Graph.Active in
        apply replay_backend decoded |> confirmed;
        apply replay_backend (request 2) |> confirmed;
        run (replay_backend.flush_dirty ());
        run (replay_backend.commit_batch ());
        expect "replay state root differs" (run (S.get_head_hash replay) = Some expected_root));
      List.iteri (fun index format ->
        let image = Filename.concat dir (Printf.sprintf "image-%d.dat" index) in
        let target = Filename.concat dir (Printf.sprintf "restored-%d" index) in
        ignore (run (Octra_core.Ledger_image.write ~format store ~commit ~path:image) |> ok);
        ignore (run (Octra_core.Ledger_image.restore ~source:image ~target ~expected_root) |> ok);
        with_store target (fun restored ->
          expect "restored program differs" (invariant restored = original);
          expect "restored record differs" (run (S.read restored (R.key address)) = Some old);
          with_cache (Filename.concat dir (Printf.sprintf "empty-%d" index)) (fun chaindata ->
            expect "new cache not empty" (C.get_program_record chaindata ~address ~code_hash = Ok None);
            let result = run (Rpc.source ~store:restored ~chaindata ~addr:address) |> Result.get_ok in
            expect "restored source missing" (field "source" result = `String source);
            expect "restored record not committed" (field "committed" result = `Bool true);
            let verified = run (Rpc.verify ~store:restored ~chaindata ~addr:address
              ~source ~files_json:None) |> Result.get_ok in
            expect "restored publication missing" (field "published" verified = `Bool true))))
        [Octra_core.Ledger_image.Flat; Octra_core.Ledger_image.Shared]));
  print_endline "status = pass test = program_record_restore"

let test_refusals () =
  Test_workspace.with_dir "program-fee" (fun dir ->
    with_store (Filename.concat dir "state") (fun store ->
      install store;
      let backend = make_backend store Graph.Active in
      expect "low fee reached source compilation"
        (apply backend (request ~fee:Z.one ~text:"const A: int = A" 1) |> refused
          = "program source fee below byte cost")));
  let cyclic = "program Cycle { const A: int = B const B: int = balance(A) public fn read(): int { return A } }" in
  Test_workspace.with_dir "program-cycle" (fun dir ->
    with_store (Filename.concat dir "state") (fun store ->
      install store;
      let before = invariant store in
      let backend = make_backend store Graph.Active in
      let tx = request ~text:cyclic ~fee:(Z.of_int 1_000_000) 1 in
      let payer = backend.ops.find_opt owner |> Option.get in
      expect "source cycle became resource failure"
        (apply backend tx |> refused = "program source compilation failed");
      let charged = backend.ops.find_opt owner |> Option.get in
      expect "source cycle did not settle fee"
        (Z.equal charged.balance (Z.sub payer.balance tx.ou) && charged.nonce = tx.nonce);
      expect "source cycle changed program" (invariant store = before);
      expect "source cycle stored report" (run (S.read store (R.key address)) = None);
      apply backend (request 2) |> confirmed));
  List.iter (fun (compiler, loops) ->
    let value = R.{main = "main.aml"; compiler; loops;
      files = [Octra_vm.Program_package.{path = "main.aml"; body = cyclic}]} in
    expect "source profile accepts cycle" (Result.is_error (R.compile value))) R.profiles;
  let parse = Octra_vm.Oct_parse.parse in
  expect "acyclic forward reference refused" (Octra_vm.Oct_scope.check_consts
    (parse "program P { const A: int = B const B: int = 7 fn read(): int { return A } }") = Ok ());
  expect "unused cycle accepted" (Result.is_error (Octra_vm.Oct_scope.check_consts
    (parse "program P { const A: int = A fn read(): int { return 7 } }")));
  let chain = List.init 4_000 (fun index -> Printf.sprintf "const a%d = %s" index
    (if index = 3_999 then "7" else "a" ^ string_of_int (index + 1))) in
  expect "long finite constants refused" (Octra_vm.Oct_scope.check_consts
    (parse ("program P { " ^ String.concat " " chain ^ " }")) = Ok ());
  Test_workspace.with_dir "program-refuse" (fun dir ->
    with_store (Filename.concat dir "state") (fun store ->
      install store;
      let before = invariant store in
      let backend = make_backend store Graph.Active in
      let cases = [request ~sender:stranger 1;
        request ~expected:(String.make 64 '0') 1;
        request ~text:"contract Other { fn value(): int { return 99 } }" 2;
        request ~fee:Z.one 3] in
      List.iter (fun tx ->
        let payer = backend.ops.find_opt tx.T.from |> Option.get in
        ignore (apply backend tx |> refused);
        let charged = backend.ops.find_opt tx.from |> Option.get in
        expect "refusal did not charge declared fee"
          (Z.equal charged.balance (Z.sub payer.balance tx.ou) && charged.nonce = tx.nonce);
        expect "refusal changed program" (invariant store = before);
        expect "refusal stored report" (run (S.read store (R.key address)) = None)) cases;
      let payer = backend.ops.find_opt owner in
      run (S.write store ["contracts"; address; "bytecode"] "invalid");
      let unavailable = try ignore (apply backend (request 4)); false with
        | Octra_core.Exec_resource.Unavailable Host -> true in
      expect "corrupt code became a paid refusal" unavailable;
      expect "corrupt code consumed funds" (backend.ops.find_opt owner = payer);
      expect "corrupt code stored report" (run (S.read store (R.key address)) = None);
      run (S.write store ["contracts"; address; "bytecode"] (Base64.encode_exn code));
      apply backend (request 4) |> confirmed;
      let saved = run (S.read store (R.key address)) in
      ignore (apply backend (request ~text:"contract Other { fn value(): int { return 99 } }" 5)
        |> refused);
      expect "refused publication replaced saved report" (run (S.read store (R.key address)) = saved);
      let journal = Octra_vm.Program_journal.create () in
      let snap = Octra_vm.Program_journal.snapshot journal in
      Octra_vm.Program_journal.add_record journal address (R.make ~source ~code compiled);
      Octra_vm.Program_journal.restore journal snap;
      expect "journal restore retained report" (Octra_vm.Program_journal.records journal = []);
      expect "journal restore changed chain" (run (S.read store (R.key address)) = saved)));
  print_endline "status = pass test = program_record_refusals"

let test_prior () =
  Test_workspace.with_dir "program-prior" (fun dir ->
    with_store (Filename.concat dir "state") (fun store ->
      install store;
      let backend = make_backend store Graph.Prior in
      let tx = request 1 in
      let prior = match U.parse tx with Error reason -> reason | Ok _ -> failwith "prior parser accepted" in
      expect "prior result changed" (apply backend tx |> refused = prior);
      expect "prior publication stored" (run (S.read store (R.key address)) = None)));
  List.iter (fun chain_id ->
    expect "publication rule activated without schedule"
      (Graph.program_records_at ~chain_id ~epoch:max_int = Graph.Prior))
    ["octra-mainnet"; "record-test"];
  let chain_id = "octra-devnet-9871-cluster" in
  let plan = Option.get (Graph.program_records_activation_for_chain chain_id) in
  expect "devnet publication activates"
    (Graph.program_records_at ~chain_id ~epoch:plan.activation_epoch = Graph.Active);
  print_endline "status = pass test = program_record_prior"

let test_rollback () =
  Test_workspace.with_dir "program-rollback" (fun dir ->
    with_store (Filename.concat dir "state") (fun store ->
      install store;
      let before = invariant store in
      let backend = make_backend store Graph.Active in
      let account = backend.ops.find_opt owner in
      let setting = "OCTRA_CHAOS_FAIL_AT" in
      let previous = Sys.getenv_opt setting in
      let failed = Fun.protect
        ~finally:(fun () -> Unix.putenv setting (Option.value previous ~default:""))
        (fun () ->
          Unix.putenv setting "vm_commit_after_program";
          match apply backend (request 1) with
          | Error _ -> true
          | Ok _ -> false) in
      expect "injected storage failure accepted" failed;
      expect "storage failure kept report" (run (S.read store (R.key address)) = None);
      expect "storage failure changed program" (invariant store = before);
      expect "storage failure changed payer" (backend.ops.find_opt owner = account);
      apply backend (request 1) |> confirmed));
  print_endline "status = pass test = program_record_rollback"

let publish_fields nonce response =
  let publication = field "publication" response in
  let text name = field name publication |> Yojson.Safe.Util.to_string in
  T.sign_with_privkey {(request nonce) with
    T.to_ = text "to"; ou = Z.of_string (text "ou");
    message = Some (text "message"); encrypted_data = Some (text "encrypted_data")} secret

let test_history () =
  let historical = "contract Old { state { values: map[int]int } fn add(k: int): int { self.values[k] += 1 return self.values[k] } }" in
  let old = Octra_vm.Prior_compile.compile historical in
  expect "historical source did not compile" (old.error = None);
  expect "historical test uses current bytes"
    (old.bytecode <> (Octra_vm.Oct_compile.compile historical).bytecode);
  let named = "program Named { view fn read(): int { return 73 } }" in
  let package = Octra_vm.Program_package.compile ~main:"entry.aml"
    ~sources:[{path = "entry.aml"; body = named}]
    |> Result.map_error Octra_vm.Program_package.error_message |> ok in
  let attested = Octra_vm.Oct_compile.attest_program ~key_id:"record-key"
    ~private_key:(Base64.decode_exn secret) package.result in
  expect "test attestation failed" (attested.error = None);
  let bundle = "import Value from \"api/value.aml\"\nprogram Bundle implements Value { public pure fn read(): int { return 7 } }" in
  let interface = "interface Value { fn read(): int }" in
  let sources = Octra_vm.Program_package.[{path = "entry.aml"; body = bundle};
    {path = "api/value.aml"; body = interface}] in
  let bundled = Octra_vm.Program_package.compile ~main:"entry.aml" ~sources
    |> Result.map_error Octra_vm.Program_package.error_message |> ok in
  List.iter (fun (label, source, main, files, stored, kind) ->
    Test_workspace.with_dir label (fun dir ->
      with_store (Filename.concat dir "state") (fun store ->
        install_code store ~kind stored;
        let backend = make_backend store Graph.Active in
        with_cache (Filename.concat dir "cache") (fun chaindata ->
          let verify () = run (Rpc.verify_params ~store ~chaindata
            (`List [`String address; `Assoc ["main", `String main;
              "files", `List files]])) |> Result.get_ok in
          let response = if main = "main.aml" then
            run (Rpc.verify ~store ~chaindata ~addr:address ~source ~files_json:None)
            |> Result.get_ok else verify () in
          let tx = publish_fields 1 response in
          apply backend tx |> confirmed;
          run (backend.flush_dirty ());
          run (backend.commit_batch ());
          let checked = if main = "main.aml" then
            run (Rpc.verify ~store ~chaindata ~addr:address ~source ~files_json:None)
            |> Result.get_ok else verify () in
          expect "historical publication differs" (field "published" checked = `Bool true);
          expect "historical publication not committed" (field "committed" checked = `Bool true);
          let visible = run (Rpc.source ~store ~chaindata ~addr:address) |> Result.get_ok in
          expect "published source not committed" (field "committed" visible = `Bool true);
          if main <> "main.aml" then
            expect "source filename lost" (field "main" visible = `String main);
          let hash = Digestif.SHA256.(digest_string stored |> to_hex) in
          let record = run (R.read ~store ~address ~code_hash:hash ()) |> ok |> Option.get in
          expect "published report missing"
            (field "verification" visible = Yojson.Safe.from_string (Option.get record.report));
          expect "published certificate missing"
            (field "certificate" visible = Yojson.Safe.from_string (Option.get record.certificate));
          let commit = run (S.get_commit_hash store) |> Option.get in
          let expected_root = run (S.get_head_hash store) |> Option.get in
          List.iteri (fun index format ->
            let image = Filename.concat dir (Printf.sprintf "image-%d.dat" index) in
            let target = Filename.concat dir (Printf.sprintf "restored-%d" index) in
            ignore (run (Octra_core.Ledger_image.write ~format store ~commit ~path:image) |> ok);
            ignore (run (Octra_core.Ledger_image.restore ~source:image ~target ~expected_root) |> ok);
            with_store target (fun restored ->
              expect "source package record changed on restore"
                (run (R.read ~store:restored ~address ~code_hash:hash ()) = Ok (Some record));
              with_cache (Filename.concat dir (Printf.sprintf "empty-%d" index)) (fun empty ->
                expect "source package cache not empty"
                  (C.get_program_record empty ~address ~code_hash:hash = Ok None);
                let actual = run (Rpc.source ~store:restored ~chaindata:empty ~addr:address)
                  |> Result.get_ok in
                expect "restored source report or certificate changed" (actual = visible);
                expect "source read wrote local cache"
                  (C.get_program_record empty ~address ~code_hash:hash = Ok None))))
            [Octra_core.Ledger_image.Flat; Octra_core.Ledger_image.Shared]))))
    ["record-old", historical, "main.aml", [], old.bytecode, "binary";
     "record-envelope", named, "entry.aml",
       [`Assoc ["path", `String "entry.aml"; "source", `String named]],
       package.envelope, "binary";
     "record-attested", named, "entry.aml",
       [`Assoc ["path", `String "entry.aml"; "source", `String named]],
       Option.get attested.program_envelope, "binary";
     "record-named", named, "entry.aml",
       [`Assoc ["path", `String "entry.aml"; "source", `String named]],
       package.envelope, "source";
     "record-bundle", bundle, "entry.aml",
       List.map (fun (file : Octra_vm.Program_package.source) ->
         `Assoc ["path", `String file.path; "source", `String file.body]) sources,
       bundled.envelope, "source"];
  print_endline "status = pass test = program_record_history"

let test_deploy () =
  let source = "program Saved { view fn read(): int { return 73 } }" in
  let package = Octra_vm.Program_package.compile_for ~point_ops:false ~main:"main.aml"
    ~sources:[{path = "main.aml"; body = source}]
    |> Result.map_error Octra_vm.Program_package.error_message |> ok in
  List.iter (fun mode ->
    Test_workspace.with_dir "record-deploy" (fun dir ->
      with_store (Filename.concat dir "state") (fun store ->
        run (S.set_meta store "last_epoch" "0");
        let backend = make_backend store mode in
        let target = Octra_vm.Contract.addr_from_code package.envelope owner 1 in
        let tx = T.sign_with_privkey {(request 1) with T.to_ = target;
          op_type = T.ProgramDeploy; ou = Z.of_int 1_000_000;
          message = Some "[]"; encrypted_data = Some (Base64.encode_exn package.package)} secret in
        apply backend tx |> confirmed;
        run (backend.flush_dirty ());
        run (backend.commit_batch ());
        let stored = run (S.read store (R.key target)) in
        expect "deployment record activation differs" (Option.is_some stored = (mode = Graph.Active));
        if mode = Graph.Active then begin
          run (backend.begin_batch Graph.Prior);
          let target = Octra_vm.Contract.addr_from_code package.envelope owner 2 in
          let next = T.sign_with_privkey {tx with T.to_ = target; nonce = 2} secret in
          let payer = backend.ops.find_opt owner in
          let setting = "OCTRA_CHAOS_FAIL_AT" in
          let previous = Sys.getenv_opt setting in
          let failed = Fun.protect
            ~finally:(fun () -> Unix.putenv setting (Option.value previous ~default:""))
            (fun () ->
              Unix.putenv setting "vm_commit_after_program";
              match apply backend next with Error _ -> true | Ok _ -> false) in
          expect "failed deployment accepted" failed;
          expect "failed deployment kept record" (run (S.read store (R.key target)) = None);
          expect "failed deployment kept code" (not (run (S.contract_exists store target)));
          expect "failed deployment changed payer" (backend.ops.find_opt owner = payer);
          run (backend.commit_batch ())
        end;
        if mode = Graph.Active then with_cache (Filename.concat dir "empty") (fun chaindata ->
          let checked = run (Rpc.verify ~store ~chaindata ~addr:target ~source ~files_json:None)
            |> Result.get_ok in
          if field "published" checked <> `Bool true then
            let meta = run (S.get_contract_meta store target) in
            let actual = Rpc.compile_record ~main:"main.aml" ~meta ~source ~files_json:None
              ~stored:package.envelope |> ok |> R.raw |> Yojson.Safe.from_string in
            let expected = Option.get stored |> Yojson.Safe.from_string in
            let differences = ["source"; "abi"; "report"; "certificate"; "code_hash"]
              |> List.filter_map (fun name -> if field name actual = field name expected then None
                else Some (name ^ ": " ^ Yojson.Safe.to_string (field name expected)
                  ^ " != " ^ Yojson.Safe.to_string (field name actual))) in
            failwith ("deployment record differs from verification: " ^ String.concat " | " differences)))))
    [Graph.Prior; Graph.Active];
  print_endline "status = pass test = program_record_deploy"

let test_reads () =
  Test_workspace.with_dir "record-reads" (fun dir ->
    with_store (Filename.concat dir "state") (fun store ->
      install store;
      let record = R.make ~source ~code compiled in
      run (R.stage store (address, record));
      with_cache (Filename.concat dir "cache") (fun chaindata ->
        let local = {record with C.source = "local-only"} in
        ignore (C.save_program_record chaindata address local);
        let visible () = run (Rpc.source ~store ~chaindata ~addr:address) in
        expect "local cache replaced committed source"
          (field "source" (visible () |> Result.get_ok) = `String source);
        let changed = Octra_vm.Oct_compile.compile "contract Changed { fn value(): int { return 99 } }" in
        let hash = Digestif.SHA256.(digest_string changed.bytecode |> to_hex) in
        run (S.upgrade_contract store ~address ~owner ~expected_code_hash:code_hash
          ~code_hash:hash ~version:changed.version ~ctype:"CUSTOM" ~check:"binary"
          ~bytecode_b64:(Base64.encode_exn changed.bytecode)) |> ok;
        expect "old report survived code replacement"
          (field "source" (visible () |> Result.get_ok) = `Null);
        run (S.write store (R.key address) "{}");
        expect "corrupt record hidden by local cache" (Result.is_error (visible ())))));
  print_endline "status = pass test = program_record_reads"

let test_facts () =
  let boolean = "program P { view fn f(x: bool): int { return 1 } }" in
  let text = "program P { view fn f(x: string): int { return 1 } }" in
  let compiled = Octra_vm.Prior_compile.compile_program boolean in
  let other = Octra_vm.Prior_compile.compile_program text in
  expect "type example did not compile" (compiled.error = None && other.error = None);
  expect "type example instruction bytes differ" (compiled.bytecode = other.bytecode);
  let signed = Octra_vm.Oct_compile.attest_program ~key_id:"record-key"
    ~private_key:(Base64.decode_exn secret) compiled in
  let stored = Option.get signed.program_envelope in
  expect "envelope argument types ignored"
    (Result.is_error (Rpc.compile_record ~main:"main.aml" ~meta:None
      ~source:text ~files_json:None ~stored));
  ignore (Rpc.compile_record ~main:"main.aml" ~meta:None
    ~source:boolean ~files_json:None ~stored |> ok);
  print_endline "status = pass test = program_record_facts"

let read_file path =
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr input)
    (fun () -> really_input_string input (in_channel_length input))

let check_program source_path code_path =
  let source = read_file source_path in
  let json = Yojson.Safe.from_string (read_file code_path) |> field "result" in
  let text name = field name json |> Yojson.Safe.Util.to_string in
  let address = text "address" in
  let bytes = text "bytecode" in
  let code_hash = text "code_hash" in
  expect "network bytecode hash differs"
    (Digestif.SHA256.(digest_string (Base64.decode_exn bytes) |> to_hex) = code_hash);
  Test_workspace.with_dir "program-check" (fun dir ->
    with_store (Filename.concat dir "state") (fun store ->
      run (S.deploy_contract store ~address ~owner ~code_hash ~version:"1.0 Rehovot"
        ~ctype:"CUSTOM" ~check:"binary" ~bytecode_b64:bytes);
      with_cache (Filename.concat dir "cache") (fun chaindata ->
        let result = run (Rpc.verify ~store ~chaindata ~addr:address ~source ~files_json:None)
          |> Result.get_ok in
        expect "program verification failed" (field "verified" result = `Bool true);
        expect "local check claimed publication" (field "published" result = `Bool false);
        print_endline (Yojson.Safe.pretty_to_string result))))

let () =
  match Array.to_list Sys.argv with
  | [_] -> test_sources (); test_const_graph (); test_publish (); test_refusals ();
    test_prior (); test_rollback ();
    test_history (); test_deploy (); test_reads (); test_facts ()
  | [_; "--consts"] -> test_const_graph ()
  | [_; "--facts"] -> test_facts ()
  | [_; source_path; code_path] -> check_program source_path code_path
  | _ -> failwith "expected source and bytecode response paths"