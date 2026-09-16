(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let fail msg =
  failwith ("test_contract_rpc_guard: " ^ msg)

let rec remove_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
    end else
      Sys.remove path

let rec make_dir path =
  if not (Sys.file_exists path) then begin
    let parent = Filename.dirname path in
    if not (String.equal parent path) then make_dir parent;
    Unix.mkdir path 0o755
  end

let with_store run =
  let root =
    Filename.concat
      (Sys.getcwd ())
      (Printf.sprintf "runtime_data/contract_rpc_guard_%d" (Unix.getpid ()))
  in
  remove_tree root;
  make_dir root;
  let store =
    Lwt_main.run
      (Octra_core.Store_irmin.open_store (Filename.concat root "irmin"))
  in
  Fun.protect
    ~finally:(fun () ->
      ignore (Lwt_main.run (Octra_core.Store_irmin.close store));
      remove_tree root)
    (fun () -> run store)

let with_chaindata run =
  let root =
    Filename.concat
      (Sys.getcwd ())
      (Printf.sprintf "runtime_data/contract_rpc_receipt_%d" (Unix.getpid ()))
  in
  remove_tree root;
  make_dir root;
  let chaindata = Octra_core.Store_chaindata.open_chaindata root in
  Fun.protect
    ~finally:(fun () ->
      Octra_core.Store_chaindata.close chaindata;
      remove_tree root)
    (fun () -> run chaindata)

let test_view_effort_limit () =
  let expected = Octra_vm.Call_plan.effort_limit Z.zero in
  if Octra_vm.Contract_rpc.view_effort_limit <> expected then
    fail "view effort limit changed"

let test_compile_limit () =
  let source = String.make 1_048_577 'x' in
  match Lwt_main.run (Octra_vm.Contract_rpc.compile_aml ~source) with
  | Error error when error.Octra_core.Rpc.code = -32602 -> ()
  | Error _ -> fail "compile limit returned wrong error"
  | Ok _ -> fail "compile limit accepted oversized source"

let test_storage_dump_disabled () =
  match
    Lwt_main.run
      (Octra_vm.Contract_rpc.contract_storage_dump_params
         ~store:()
         (`List []))
  with
  | Error error when error.Octra_core.Rpc.code = -32601 -> ()
  | Error _ -> fail "storage dump returned wrong error"
  | Ok _ -> fail "storage dump remained public"

let test_receipt_hash_gate () =
  with_chaindata (fun chaindata ->
    begin
      match
        Lwt_main.run
          (Octra_vm.Contract_rpc.receipt_params
             ~chaindata
             (`List [`String ""]))
      with
      | Error error when error.Octra_core.Rpc.code = -32602 -> ()
      | Error _ -> fail "empty receipt hash returned wrong error"
      | Ok _ -> fail "empty receipt hash accepted"
    end;
    match
      Lwt_main.run
        (Octra_vm.Contract_rpc.receipt_params
           ~chaindata
           (`List [`String (String.make 64 '0')]))
    with
    | Error error when error.Octra_core.Rpc.code = 112 -> ()
    | Error _ -> fail "well-formed receipt hash returned wrong error"
    | Ok _ -> fail "missing receipt returned success")

let test_view_fhe_capability_gate () =
  let open Octra_vm.Contract_vm in
  let gate = Octra_vm.Contract_rpc.view_fhe_capability_gate () in
  if not (gate Fhe_cipher_arithmetic_cap) then
    fail "view arithmetic capability rejected";
  if not (gate Fhe_verify_zero_cap) then
    fail "first view verifier rejected";
  if gate Fhe_verify_bound_cap then
    fail "second view verifier admitted";
  if not (gate Fhe_cipher_arithmetic_cap) then
    fail "view arithmetic capability consumed"

let test_view_lane () =
  let first =
    Octra_vm.Contract_rpc.run_view (fun () ->
      Thread.delay 0.05;
      7)
  in
  let second =
    Octra_vm.Contract_rpc.run_view (fun () -> 8)
  in
  let pulse =
    let open Lwt.Syntax in
    let* () = Lwt_unix.sleep 0.005 in
    Lwt.return true
  in
  let (first_result, second_result), responsive =
    Lwt_main.run (Lwt.both (Lwt.both first second) pulse)
  in
  begin
    match first_result with
    | Ok 7 -> ()
    | _ -> fail "first view lane execution failed"
  end;
  begin
    match second_result with
    | Error error when error.Octra_core.Rpc.code = -32005 -> ()
    | _ -> fail "concurrent view lane execution admitted"
  end;
  if not responsive then fail "view lane blocked Lwt";
  match Lwt_main.run (Octra_vm.Contract_rpc.run_view (fun () -> 9)) with
  | Ok 9 -> ()
  | _ -> fail "view lane did not reset"

let test_view_timeout () =
  let stopped = ref false in
  let work =
    Octra_vm.Contract_rpc.run_view ~seconds:0.01
      ~stop:(fun () -> stopped := true)
      (fun () -> Thread.delay 0.1; 7)
  in
  begin match Lwt_main.run work with
  | Error error when error.Octra_core.Rpc.message = "Program view time limit exceeded" -> ()
  | _ -> fail "view time limit missing"
  end;
  if not !stopped then fail "view stop not requested";
  begin match Lwt_main.run (Octra_vm.Contract_rpc.run_view (fun () -> 8)) with
  | Error error when error.Octra_core.Rpc.code = -32005 -> ()
  | _ -> fail "view released before worker exit"
  end;
  Lwt_main.run (Lwt_unix.sleep 0.15);
  begin match Lwt_main.run (Octra_vm.Contract_rpc.run_view (fun () -> 9)) with
  | Ok 9 -> ()
  | _ -> fail "view did not release after worker exit"
  end;
  stopped := false;
  let work =
    Octra_vm.Contract_rpc.run_view
      ~stop:(fun () -> stopped := true)
      (fun () -> Thread.delay 0.05; 0)
  in
  Lwt.cancel work;
  if not !stopped then fail "cancel did not request stop";
  begin match Lwt_main.run (Octra_vm.Contract_rpc.run_view (fun () -> 1)) with
  | Error error when error.Octra_core.Rpc.code = -32005 -> ()
  | _ -> fail "cancel released active worker"
  end;
  Lwt_main.run (Lwt_unix.sleep 0.1)

let test_view_steps () =
  let open Octra_vm.Contract_vm in
  let state () =
    create_state ~caller:"a" ~origin:"a" ~address:"b" ~value:Z.zero
      ~storage:(Hashtbl.create 1) ()
  in
  let fixed = [|LDI (0, VInt (Z.of_int 7)); STOP|] in
  let direct = Octra_vm.Contract.run_fixed_from_dispatcher (state ()) fixed in
  let stepped = Octra_vm.Contract.run_fixed_from_dispatcher ~running:(fun () -> true) (state ()) fixed in
  if direct <> stepped then fail "view step result differs";
  let count = ref 0 in
  let running () = incr count; !count <= 8 in
  let state = state () in
  let result = Octra_vm.Contract.run_fixed_from_dispatcher ~running state [|JMP 0|] in
  if result.success || not state.reverted || !count <> 9 then
    fail "view step stop missing"

let test_source_program_verify () =
  with_store (fun store ->
    let source =
      "program Verify { fn value(): int { return 7 } }"
    in
    let compiled =
      match
        Octra_vm.Program_package.compile
          ~main:"main.aml"
          ~sources:[
            Octra_vm.Program_package.{
              path = "main.aml";
              body = source;
            };
          ]
      with
      | Ok value -> value
      | Error error ->
        fail (Octra_vm.Program_package.error_message error)
    in
    let address = "oct" ^ String.make 44 '1' in
    let code_hash = Digestif.SHA256.(digest_string compiled.envelope |> to_hex) in
    ignore
      (Lwt_main.run
         (Octra_core.Store_irmin.deploy_contract
           store
           ~address
           ~code_hash
            ~version:"1"
            ~owner:address
            ~ctype:"CUSTOM"
            ~admission:"source"
           ~bytecode_b64:(Base64.encode_exn compiled.envelope)));
    with_chaindata (fun chaindata ->
      let verify value =
        Lwt_main.run
          (Octra_vm.Contract_rpc.verify
             ~store
             ~chaindata
             ~addr:address
             ~source:value
             ~files_json:None)
      in
      let verify_exact () =
        match verify source with
        | Ok (`Assoc fields) ->
          if List.assoc_opt "published" fields <> Some (`Bool true) then
            fail "source Program metadata was not published";
          if List.assoc_opt "code_hash" fields <> Some (`String code_hash) then
            fail "source Program verification hash differs"
        | Ok _ -> fail "source Program verification response is invalid"
        | Error error -> fail error.Octra_core.Rpc.message
      in
      verify_exact ();
      verify_exact ();
      begin
        match verify (source ^ "\n") with
        | Error error when error.Octra_core.Rpc.code = -32000 -> ()
        | Error _ -> fail "source Program variation returned wrong error"
        | Ok _ -> fail "source Program variation matched deployed package"
      end;
      begin
        match
          Octra_core.Store_chaindata.get_program_record
            chaindata
            ~address
            ~code_hash
        with
        | Ok (Some record) when String.equal record.source source -> ()
        | Ok (Some _) -> fail "source Program record changed"
        | Ok None -> fail "source Program record is absent"
        | Error reason -> fail reason
      end;
      match
        Lwt_main.run
          (Octra_vm.Contract_rpc.source ~store ~chaindata ~addr:address)
      with
      | Ok (`Assoc fields)
        when List.assoc_opt "source" fields = Some (`String source) -> ()
      | Ok _ -> fail "source Program metadata response differs"
      | Error error -> fail error.Octra_core.Rpc.message);
    let ledger = Octra_core.Ledger.create store in
    match
      Lwt_main.run
        (Octra_vm.Contract_rpc.call ~math:false
           ~store
           ~ledger
           ~current_epoch:0
           ~get_fhe_pubkey:(fun _ -> None)
           ~storage_json:(fun _ -> `Assoc [])
           ~addr:address
           ~method_name:"value"
           ~call_params:[]
           ~caller_addr:address
           ~include_storage:false)
    with
    | Ok _ -> ()
    | Error error -> fail error.Octra_core.Rpc.message)

let token_address digit =
  "oct" ^ String.make 44 digit

let indexed_address index =
  let alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" in
  let body = Bytes.make 44 '1' in
  Bytes.set body 42 alphabet.[index mod String.length alphabet];
  Bytes.set body 43 alphabet.[(index / String.length alphabet) mod String.length alphabet];
  "oct" ^ Bytes.to_string body

let install_token store ~address ~owner fields =
  ignore
    (Lwt_main.run
       (Octra_core.Store_irmin.deploy_contract
          store
          ~address
          ~code_hash:(String.make 64 '0')
          ~version:"1"
          ~owner
          ~ctype:"OCS01"
          ~admission:"source"
          ~bytecode_b64:""));
  let storage = Hashtbl.create 8 in
  List.iter (fun (key, value) -> Hashtbl.replace storage key value) fields;
  Lwt_main.run (Octra_core.Store_irmin.save_contract_storage store address storage)

let token_meta ~decimals ~total_supply =
  Octra_vm.Token_rpc_policy.meta
    ~address:(token_address '2')
    ~owner:(token_address '1')
    ~symbol:"TOK"
    ~name:(Some "Token")
    ~decimals:(Some decimals)
    ~total_supply:(Some total_supply)

let test_token_value_policy () =
  let max_u128 = "340282366920938463463374607431768211455" in
  let overflow = "340282366920938463463374607431768211456" in
  begin
    match token_meta ~decimals:"18" ~total_supply:max_u128 with
    | Some token ->
      begin
        match Octra_vm.Token_rpc_policy.row token ~balance:max_u128 with
        | Some _ -> ()
        | None -> fail "maximum u128 token value rejected"
      end;
      begin
        match
          Octra_vm.Token_rpc_policy.row
            token
            ~balance:(Base64.encode_exn "encrypted-token-balance")
        with
        | Some row ->
          if not Yojson.Safe.Util.(row |> member "balance_is_encrypted" |> to_bool)
          then fail "encoded token balance lost encryption marker"
        | None -> fail "encoded token balance rejected"
      end
    | None -> fail "valid token metadata rejected"
  end;
  if Option.is_some (token_meta ~decimals:"19" ~total_supply:"1") then
    fail "invalid decimal places accepted";
  if Option.is_some (token_meta ~decimals:"18" ~total_supply:overflow) then
    fail "overflowing token supply accepted";
  match token_meta ~decimals:"18" ~total_supply:"1" with
  | None -> fail "token sample rejected"
  | Some token ->
    if Option.is_some
      (Octra_vm.Token_rpc_policy.row
         token
         ~balance:(String.make Octra_vm.Contract_vm.max_storage_value_len '9'))
    then
      fail "storage-sized token balance accepted"

let test_token_page_policy () =
  let token =
    match token_meta ~decimals:"18" ~total_supply:"10" with
    | Some token -> token
    | None -> fail "token page sample rejected"
  in
  let row =
    match Octra_vm.Token_rpc_policy.row token ~balance:"1" with
    | Some row -> row
    | None -> fail "token page row rejected"
  in
  let page =
    Octra_vm.Token_rpc_policy.empty_page
      ~address:(token_address '1')
      ~offset:0
      ~limit:1
  in
  let page =
    match Octra_vm.Token_rpc_policy.add page row with
    | Octra_vm.Token_rpc_policy.Continue page -> page
    | Octra_vm.Token_rpc_policy.Complete _ -> fail "token page filled early"
  in
  let page =
    match Octra_vm.Token_rpc_policy.add page row with
    | Octra_vm.Token_rpc_policy.Complete page -> page
    | Octra_vm.Token_rpc_policy.Continue _ -> fail "token page exceeded row limit"
  in
  let payload = Octra_vm.Token_rpc_policy.finish ~limited:false page in
  if not Yojson.Safe.Util.(payload |> member "limited" |> to_bool) then
    fail "token page continuation missing";
  if Yojson.Safe.Util.(payload |> member "next_offset" |> to_int) <> 1 then
    fail "token page continuation offset changed"

let test_token_actor_limits () =
  with_store (fun store ->
    let holder = token_address '1' in
    let huge = String.make Octra_vm.Contract_vm.max_storage_value_len '9' in
    let max_u128 = "340282366920938463463374607431768211455" in
    install_token
      store
      ~address:(token_address '2')
      ~owner:holder
      [
        "symbol", "TOK";
        "name", "Token";
        "decimals", "18";
        "total_supply", max_u128;
        "balances:" ^ holder, "7";
      ];
    install_token
      store
      ~address:(token_address '3')
      ~owner:holder
      [
        "symbol", "BAD";
        "name", "Bad metadata";
        "decimals", huge;
        "total_supply", huge;
        "balances:" ^ holder, huge;
      ];
    install_token
      store
      ~address:(token_address '4')
      ~owner:holder
      [
        "symbol", "BIG";
        "name", "Bad balance";
        "decimals", "18";
        "total_supply", "1";
        "balances:" ^ holder, huge;
      ];
    let actor = Octra_vm.Token_rpc_actor.create ~store () in
    let payload =
      match
        Lwt_main.run
          (Octra_vm.Token_rpc_actor.query
             actor
             ~holder
             ~offset:0
             ~limit:Octra_vm.Token_rpc_policy.max_page_rows)
      with
      | Ok payload -> payload
      | Error _ -> fail "token actor query failed"
    in
    let tokens = Yojson.Safe.Util.(payload |> member "tokens" |> to_list) in
    if List.length tokens <> 1 then
      fail "invalid token storage reached response";
    if
      Octra_vm.Token_rpc_policy.page_bytes payload
      > Octra_vm.Token_rpc_policy.max_page_bytes
    then
      fail "token response exceeded byte limit";
    for index = 0 to 160 do
      let holder = indexed_address index in
      match
        Lwt_main.run
          (Octra_vm.Token_rpc_actor.query actor ~holder ~offset:0 ~limit:1)
      with
      | Ok _ -> ()
      | Error _ -> fail "token cache fill query failed"
    done;
    let stats = Lwt_main.run (Octra_vm.Token_rpc_actor.stats actor) in
    if stats.cache_entries > Octra_vm.Token_rpc_actor.cache_entry_limit then
      fail "token cache exceeded entry limit";
    if stats.cache_bytes > Octra_vm.Token_rpc_actor.cache_byte_limit then
      fail "token cache exceeded byte limit";
    Lwt_main.run (Octra_vm.Token_rpc_actor.shutdown actor))

let test_nested_view_stop () =
  with_store (fun store ->
    let address = token_address '3' in
    let compiled = Octra_vm.Oct_compile.compile
      "contract ViewCase { view fn echo(): int { return 7 } }" in
    if compiled.error <> None then fail "nested view compile";
    Lwt_main.run
      (Octra_core.Store_irmin.deploy_contract store
        ~address
        ~code_hash:Digestif.SHA256.(digest_string compiled.bytecode |> to_hex)
        ~version:"1" ~owner:address ~ctype:"CUSTOM" ~admission:"bytecode"
        ~bytecode_b64:(Base64.encode_exn compiled.bytecode));
    let ledger = Octra_core.Ledger.create store in
    let run running =
      let ctx = Octra_vm.Contract_rpc.make_view_ctx ~running ~store ~ledger
        ~current_epoch:0 ~get_fhe_pubkey:(fun _ -> None) () in
      Lwt_main.run (Lwt_preemptive.detach
        (fun () -> ctx.call_contract address address "echo" [] 0) ())
    in
    begin match run (fun () -> true) with
    | Ok value when value.Octra_vm.Contract_vm.return_value = Octra_vm.Contract_vm.VInt (Z.of_int 7) -> ()
    | _ -> fail "nested view control"
    end;
    let steps = ref 0 in
    let result = run (fun () -> incr steps; !steps <= 1) in
    if !steps <> 2 then fail "nested stop not reached";
    match result with
    | Error _ -> ()
    | Ok _ -> fail "nested view ignored stop")

let test_token_actor_overload () =
  let limit = Octra_vm.Token_rpc_actor.data_channel_limit in
  if not (Octra_vm.Token_rpc_actor.data_admitted ~queued:(limit - 1)) then
    fail "token actor refused available channel";
  if Octra_vm.Token_rpc_actor.data_admitted ~queued:limit then
    fail "token actor admitted full channel";
  if Octra_vm.Token_rpc_actor.data_admitted ~queued:(-1) then
    fail "token actor admitted invalid channel state"

let () =
  test_view_effort_limit ();
  test_compile_limit ();
  test_storage_dump_disabled ();
  test_receipt_hash_gate ();
  test_view_fhe_capability_gate ();
  test_view_lane ();
  test_view_timeout ();
  test_view_steps ();
  test_nested_view_stop ();
  test_source_program_verify ();
  test_token_value_policy ();
  test_token_page_policy ();
  test_token_actor_limits ();
  test_token_actor_overload ();
  print_endline "test_contract_rpc_guard: ok"