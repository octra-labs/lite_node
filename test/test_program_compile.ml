(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let require condition message =
  if not condition then failwith message

let compile source =
  Octra_vm.Oct_compile.compile source

let compile_program source =
  Octra_vm.Oct_compile.compile_program_checked source

let compile_program_base source =
  Octra_vm.Oct_compile.compile_program source

let compile_linked name source =
  match Octra_vm.Aml_source.compile ~syntax:Octra_vm.Oct_gen.Source source with
  | Ok value -> value
  | Error error -> failwith (name ^ ": " ^ error)

let require_ok name result =
  match result.Octra_vm.Oct_compile.error with
  | None -> result
  | Some error -> failwith (name ^ ": " ^ error)

let require_error name source fragment =
  match (compile_program source).Octra_vm.Oct_compile.error with
  | Some error when
      let length = String.length fragment in
      let rec find at =
        at + length <= String.length error
        && (String.sub error at length = fragment || find (at + 1))
      in
      find 0 -> ()
  | Some error -> failwith (name ^ ": " ^ error)
  | None -> failwith (name ^ ": accepted")

let hash value =
  Digestif.SHA256.(digest_string value |> to_hex)

let ledger_source = {|
contract Ledger {
  state {
    balances: map[address]int
    grants: map[address]map[address]int
    values: list[int]
    pending: option[int]
  }
  public fn grant(spender: address, amount: int): bool {
    self.grants[caller][spender] = amount
    return true
  }
  nonreentrant fn send(to: address, amount: int): bool {
    let have = self.balances[caller]
    require(have >= amount, "balance")
    self.balances[caller] = have - amount
    self.balances[to] = self.balances[to] + amount
    return true
  }
  public fn append(item: int): int {
    self.values.push(item)
    return self.values.length
  }
  public fn stage(item: int): int {
    self.pending = some(item)
    return unwrap(self.pending)
  }
}
|}

let loose_source = {|
contract Loose {
  public fn value(): int {
    let enabled: bool = 7
    return enabled
  }
}
|}

let builtin_source = {|
contract Context {
  public fn amount(): int { return value + epoch }
}
|}

let valid_source = {|
program Safe {
  state { total: int }
  public fn add(value: int): int {
    self.total = self.total + value
    return self.total
  }
}
|}

let stable_source = {|
program Stable {
  state { total: int }
  public fn add(amount: int): int {
    self.total = self.total + amount
    return self.total
  }
}
|}

let bool_source = {|
program Flag {
  state { enabled: bool }
  public fn set(value: bool): bool {
    self.enabled = value
    return self.enabled
  }
}
|}

let keyed_source = {|
program Keyed {
  state {
    by_address: map[address]u128
    by_index: map[int]u128
    items: list[string]
  }
  public fn write(account: address, index: int, amount: u128): u128 {
    self.by_address[account] = amount
    self.by_index[index] = amount
    return self.by_address[account] + self.by_index[index]
  }
  public fn append(item: string): bool {
    self.items.push(item)
    return true
  }
}
|}

let wrong_local = {|
program WrongLocal {
  public fn value(): int {
    let enabled: bool = 7
    return 1
  }
}
|}

let wrong_return = {|
program WrongReturn {
  public fn value(): bool { return 7 }
}
|}

let missing_return = {|
program MissingReturn {
  public fn value(): address { let number = 1 }
}
|}

let wrong_call = {|
program WrongCall {
  private fn add(value: int): int { return value + 1 }
  public fn value(): int { return add("7") }
}
|}

let nested_loop_source = {|
contract NestedLoop {
  private pure fn inner(limit: int): int {
    let total: int = 0
    for index in 0..limit {
      total = total + 1
    }
    return total
  }

  public pure fn run(): int {
    let total: int = 0
    for index in 0..20 {
      total = total + inner(index) + 1
    }
    return total
  }
}
|}

let context_name_source = {|
contract ContextName {
  public pure fn digest(value: string): string {
    return value
  }
}
|}

let mixed_source = {|
program Mixed {
  state { total: int }

  form plus [many left: int] (many right: int) ->[many] int marks {} =
    left + right

  public fn add(amount: int): int {
    self.total = plus(self.total, amount)
    return self.total
  }
}
|}

let main_source = {|
program Main {
  public main(many left: int, many right: int) ->[many] int marks {} =
    left + right
}
|}

let multi_main_source = {|
import Value from "value.aml"
contract Multi implements Value {
  public pure fn read(): int { return 7 }
}
|}

let multi_value_source = {|
interface Value { fn read(): int }
|}

let point_source = {|
program Points {
  public fn identity(): bytes {
    return pedersen_identity()
  }
  public fn add(left: bytes, right: bytes): bytes {
    return pedersen_add(left, right)
  }
  public fn sub(left: bytes, right: bytes): bytes {
    return pedersen_sub(left, right)
  }
}
|}

let point_code left right = [|
  Octra_vm.Contract_vm.LDI (1, Octra_vm.Contract_vm.VString left);
  Octra_vm.Contract_vm.LDI (2, Octra_vm.Contract_vm.VString right);
  Octra_vm.Contract_vm.FHE_PEDERSEN_IDENTITY 4;
  Octra_vm.Contract_vm.FHE_PEDERSEN_ADD (3, 1, 2);
  Octra_vm.Contract_vm.FHE_PEDERSEN_SUB (0, 3, 2);
  Octra_vm.Contract_vm.STOP;
|]

let run_points ctx code =
  let state =
    Octra_vm.Contract_vm.create_state
      ~ctx
      ~strict_values:true
      ~caller:"caller"
      ~origin:"caller"
      ~address:"program"
      ~value:Z.zero
      ~storage:(Hashtbl.create 1)
      ()
  in
  Octra_vm.Contract_vm.run state code, state

let square_code count =
  Array.init
    (count + 2)
    (fun index ->
      if index = 0 then
        Octra_vm.Contract_vm.LDI (0, Octra_vm.Contract_vm.VInt (Z.of_int 2))
      else if index <= count then
        Octra_vm.Contract_vm.MUL (0, 0, 0)
      else
        Octra_vm.Contract_vm.STOP)

let run_math ctx code limit =
  let state =
    Octra_vm.Contract_vm.create_state
      ~ctx
      ~limit
      ~strict_values:true
      ~caller:"caller"
      ~origin:"caller"
      ~address:"program"
      ~value:Z.zero
      ~storage:(Hashtbl.create 1)
      ()
  in
  Octra_vm.Contract_vm.run state code, state

let run_squares ctx count limit =
  run_math ctx (square_code count) limit

let vector_square_code count =
  Array.init
    ((count * 2) + 6)
    (fun index ->
      if index = 0 then
        Octra_vm.Contract_vm.LDI (0, Octra_vm.Contract_vm.VInt (Z.of_int 2))
      else if index = 1 then
        Octra_vm.Contract_vm.MSTORE (0, 0)
      else if index = 2 || index = 3 then
        Octra_vm.Contract_vm.LDI (index - 1, Octra_vm.Contract_vm.VInt Z.zero)
      else if index = 4 then
        Octra_vm.Contract_vm.LDI (3, Octra_vm.Contract_vm.VInt Z.one)
      else if index = (count * 2) + 5 then
        Octra_vm.Contract_vm.STOP
      else if (index - 5) mod 2 = 0 then
        Octra_vm.Contract_vm.VECDOT (0, 1, 2, 3)
      else
        Octra_vm.Contract_vm.MSTORE (0, 0))

let run_vector_squares ctx count limit =
  run_math ctx (vector_square_code count) limit

let matrix_cap_code = [|
  Octra_vm.Contract_vm.LDI (0, Octra_vm.Contract_vm.VInt Z.zero);
  Octra_vm.Contract_vm.LDI (1, Octra_vm.Contract_vm.VInt Z.zero);
  Octra_vm.Contract_vm.LDI (2, Octra_vm.Contract_vm.VInt Z.zero);
  Octra_vm.Contract_vm.LDI (3, Octra_vm.Contract_vm.VInt (Z.of_int 1001));
  Octra_vm.Contract_vm.LDI (4, Octra_vm.Contract_vm.VInt (Z.of_int 1000));
  Octra_vm.Contract_vm.LDI (5, Octra_vm.Contract_vm.VInt Z.one);
  Octra_vm.Contract_vm.MATMUL (0, 1, 2, 3, 4, 5);
  Octra_vm.Contract_vm.STOP;
|]

let load_cap_code = [|
  Octra_vm.Contract_vm.LDI (0, Octra_vm.Contract_vm.VInt Z.zero);
  Octra_vm.Contract_vm.LDI
    (1, Octra_vm.Contract_vm.VString (String.make 1_000_001 'x'));
  Octra_vm.Contract_vm.LDI (2, Octra_vm.Contract_vm.VInt Z.zero);
  Octra_vm.Contract_vm.LDI
    (3, Octra_vm.Contract_vm.VInt (Z.of_int 1_000_001));
  Octra_vm.Contract_vm.LDI (4, Octra_vm.Contract_vm.VInt Z.one);
  Octra_vm.Contract_vm.LOAD_INT8_BYTES_TO_MEM (0, 1, 2, 3, 4);
  Octra_vm.Contract_vm.STOP;
|]

let overlap_code = [|
  Octra_vm.Contract_vm.LDI (0, Octra_vm.Contract_vm.VInt Z.one);
  Octra_vm.Contract_vm.LDI (1, Octra_vm.Contract_vm.VInt Z.zero);
  Octra_vm.Contract_vm.LDI (2, Octra_vm.Contract_vm.VInt (Z.of_int 2));
  Octra_vm.Contract_vm.LDI (3, Octra_vm.Contract_vm.VInt (Z.of_int 131_072));
  Octra_vm.Contract_vm.MSTORE (0, 3);
  Octra_vm.Contract_vm.LDI (3, Octra_vm.Contract_vm.VInt (Z.of_int 196_608));
  Octra_vm.Contract_vm.MSTORE (1, 3);
  Octra_vm.Contract_vm.LDI (3, Octra_vm.Contract_vm.VInt (Z.of_int 262_144));
  Octra_vm.Contract_vm.MSTORE (2, 3);
  Octra_vm.Contract_vm.ELEMWISE_MUL_INPLACE (0, 1, 2);
  Octra_vm.Contract_vm.STOP;
|]

let require_bits name expected state =
  match state.Octra_vm.Contract_vm.regs.(0) with
  | Octra_vm.Contract_vm.VInt value ->
    require (Z.numbits value = expected) (name ^ " integer width differs")
  | _ -> failwith (name ^ " result type differs")

let require_memory_int name state index expected =
  match Hashtbl.find_opt state.Octra_vm.Contract_vm.memory.data index with
  | Some (Octra_vm.Contract_vm.VInt value) ->
    require (Z.equal value expected) (name ^ " memory value differs")
  | _ -> failwith (name ^ " memory type differs")

let run_bytecode ?(args = []) ?(value = Z.zero) name method_name bytecode =
  let image =
    match Octra_vm.Bytecode.decode_image bytecode with
    | Ok image -> image
    | Error error -> failwith (name ^ ": " ^ error)
  in
  let config =
    Octra_vm.Local_vm.config
      ~method_name
      ~args
      ~value
      ()
  in
  match Octra_vm.Local_vm.run ~trace:false config image.code with
  | Error error -> failwith (name ^ ": " ^ Octra_vm.Local_vm.error_text error)
  | Ok outcome when outcome.stop = Octra_vm.Local_vm.Returned -> outcome.result
  | Ok outcome ->
    failwith (name ^ ": " ^ Octra_vm.Local_vm.stop_text outcome.stop)

let rpc_compile name source program =
  let params = `List [`String source; `Bool program] in
  match Lwt.state (Octra_vm.Contract_rpc.compile_aml_params
      ~compiler:Octra_vm.Program_package.Source params) with
  | Lwt.Return (Ok (`Assoc fields)) -> fields
  | Lwt.Return (Ok _) -> failwith (name ^ ": response is invalid")
  | Lwt.Return (Error error) -> failwith (name ^ ": " ^ error.Octra_core.Rpc.message)
  | Lwt.Fail error -> raise error
  | Lwt.Sleep -> failwith (name ^ ": RPC did not finish")

let rpc_field name fields field =
  match List.assoc_opt field fields with
  | Some (`String value) -> value
  | _ -> failwith (name ^ ": field is absent name = " ^ field)

let rpc_bytecode name source =
  match Lwt.state (Octra_vm.Contract_rpc.compile_aml_with
      ~compiler:Octra_vm.Program_package.Source ~point_ops:true ~program:false ~source) with
  | Lwt.Return (Ok (`Assoc fields)) ->
    begin
      match List.assoc_opt "bytecode" fields with
      | Some (`String value) -> Base64.decode_exn value
      | _ -> failwith (name ^ ": bytecode is absent")
    end
  | Lwt.Return (Ok _) -> failwith (name ^ ": response is invalid")
  | Lwt.Return (Error _) -> failwith (name ^ ": RPC rejected source")
  | Lwt.Fail error -> raise error
  | Lwt.Sleep -> failwith (name ^ ": RPC did not finish")

let rec remove_path path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | stat when stat.Unix.st_kind = Unix.S_DIR ->
    Sys.readdir path
    |> Array.iter (fun name -> remove_path (Filename.concat path name));
    Unix.rmdir path
  | _ -> Unix.unlink path

let ensure_dir path =
  if Sys.file_exists path then
    require (Sys.is_directory path) "runtime data path is not a directory"
  else
    Unix.mkdir path 0o700

let verify_lane_test () =
  let release, release_wakener = Lwt.wait () in
  let finished, finished_wakener = Lwt.wait () in
  let first =
    Octra_vm.Contract_rpc.run_verify (fun () ->
      let open Lwt.Syntax in
      let* () = release in
      Lwt.wakeup_later finished_wakener ();
      Lwt.return_ok 7)
  in
  let expect_busy request =
    match Lwt_main.run request with
    | Error error when error.Octra_core.Rpc.code = -32005 -> ()
    | _ -> failwith "concurrent program verification was admitted"
  in
  expect_busy (Octra_vm.Contract_rpc.run_verify (fun () -> Lwt.return_ok 8));
  Lwt.cancel first;
  expect_busy (Octra_vm.Contract_rpc.run_verify (fun () -> Lwt.return_ok 8));
  Lwt.wakeup_later release_wakener ();
  Lwt_main.run finished;
  Lwt_main.run (Lwt.pause ());
  match
    Lwt_main.run
      (Octra_vm.Contract_rpc.run_verify (fun () -> Lwt.return_ok 9))
  with
  | Ok 9 -> ()
  | _ -> failwith "program verification lane did not reset"

let verify_worker_test () =
  let first =
    Octra_vm.Contract_rpc.run_verify (fun () ->
      let open Lwt.Syntax in
      let+ result =
        Octra_vm.Contract_rpc.run_verify_worker (fun () ->
          Unix.sleepf 0.1;
          Ok 10)
      in
      match result with
      | Ok value -> Ok value
      | Error message -> Error (Octra_core.Rpc.err (-32000) message None))
  in
  begin
    match
      Lwt_main.run
        (Octra_vm.Contract_rpc.run_verify (fun () -> Lwt.return_ok 11))
    with
    | Error error when error.Octra_core.Rpc.code = -32005 -> ()
    | _ -> failwith "program verification worker did not retain its lane"
  end;
  match Lwt_main.run first with
  | Ok 10 -> ()
  | _ -> failwith "program verification worker did not finish"

let verify_record_test (compiled : Octra_vm.Aml_source.t) =
  let data = Filename.concat (Sys.getcwd ()) "runtime_data" in
  let scope = Filename.concat data "program-record-tests" in
  ensure_dir data;
  ensure_dir scope;
  let path =
    Filename.concat
      scope
      (Printf.sprintf "run-%d-%.0f"
         (Unix.getpid ())
         (Unix.gettimeofday () *. 1_000_000.))
  in
  let irmin = Filename.concat path "irmin" in
  let history = Filename.concat path "history" in
  Unix.mkdir path 0o700;
  let store = Lwt_main.run (Octra_core.Store_irmin.open_store ~fresh:true irmin) in
  let chain = ref (Some (Octra_core.Store_chaindata.open_chaindata history)) in
  let close_chain () =
    match !chain with
    | Some value ->
      Octra_core.Store_chaindata.close value;
      chain := None
    | None -> ()
  in
  Fun.protect
    ~finally:(fun () ->
      close_chain ();
      Lwt_main.run (Octra_core.Store_irmin.close store);
      remove_path path)
    (fun () ->
      let ledger = Octra_core.Ledger.create store in
      let view_ctx =
        Octra_vm.Contract_rpc.make_view_ctx
          ~trusted:[]
          ~profile:{epoch = 0; math = false; point_ops = false;
                    object_cost = false; int_work = Octra_vm.Int_work.Active}
          ~store
          ~ledger
          ~get_fhe_pubkey:(fun _ -> None)
          ()
      in
      require
        (view_ctx.Octra_vm.Contract_vm.int_work = Octra_vm.Int_work.Active)
        "view integer work is disabled";
      let accepted, state = run_squares view_ctx 33 1_000_000 in
      require (not accepted && state.reverted)
        "view integer work limit accepted repeated squares";
      require (state.effort_used = 351_636)
        "view integer work charge differs";
      require_bits "view integer work" 65_537 state;
      let accepted, state = run_vector_squares view_ctx 33 1_000_000 in
      require (not accepted && state.reverted)
        "view vector work limit accepted repeated squares";
      require (state.effort_used = 351_812)
        "view vector work charge differs";
      require_bits "view vector work" 65_537 state;
      let accepted, state = run_math view_ctx matrix_cap_code 1_000_000 in
      require (not accepted && state.reverted)
        "view matrix work item cap accepted";
      require (state.effort_used = 1083)
        "view matrix work item cap charge differs";
      let accepted, state = run_math view_ctx load_cap_code 2_000_000 in
      require (not accepted && state.reverted)
        "view loader work item cap accepted";
      require (state.effort_used = 500_015)
        "view loader work item cap charge differs";
      let accepted, state =
        run_math Octra_vm.Contract_vm.default_ctx overlap_code 1000
      in
      require (accepted && state.effort_used = 22)
        "base overlap work differs";
      require_memory_int "base overlap" state 2 (Z.of_int 1_572_864);
      let accepted, state = run_math view_ctx overlap_code 1000 in
      require (accepted && state.effort_used = 24)
        "view overlap work differs";
      require_memory_int "view overlap" state 2 (Z.of_int 786_432);
      let owner = "oct" ^ String.make 44 '1' in
      let address = Octra_vm.Contract.addr_from_code compiled.octb owner 0 in
      let code_hash = hash compiled.octb in
      Lwt_main.run
        (Octra_core.Store_irmin.deploy_contract
           store
           ~address
           ~code_hash
           ~version:"OCTB/1"
           ~owner
           ~ctype:"CUSTOM"
           ~admission:"binary"
           ~bytecode_b64:(Base64.encode_exn compiled.octb));
      let address2 = Octra_vm.Contract.addr_from_code compiled.octb owner 1 in
      Lwt_main.run
        (Octra_core.Store_irmin.deploy_contract
           store
           ~address:address2
           ~code_hash
           ~version:"OCTB/1"
           ~owner
           ~ctype:"CUSTOM"
           ~admission:"binary"
           ~bytecode_b64:(Base64.encode_exn compiled.octb));
      let program_page params =
        Lwt_main.run
          (Octra_vm.Contract_rpc.list_contracts_params
             ~store
             ~ledger
             params)
      in
      begin
        match program_page (`List [`Int 0; `Int 1]) with
        | Ok (`Assoc fields) ->
          require
            (List.assoc_opt "count" fields = Some (`Int 1))
            "program page count differs";
          require
            (List.assoc_opt "more" fields = Some (`Bool true))
            "program page continuation is absent";
          require
            (List.assoc_opt "next_offset" fields = Some (`Int 1))
            "program page offset differs"
        | _ -> failwith "program page failed"
      end;
      begin
        match program_page (`List [`Int 0; `Int 129]) with
        | Error _ -> ()
        | Ok _ -> failwith "program page accepted excess limit"
      end;
      let storage = Hashtbl.create 67 in
      for index = 0 to 65 do
        Hashtbl.add
          storage
          (Printf.sprintf "key-%02d" index)
          (String.make 5000 (Char.chr (65 + (index mod 26))))
      done;
      Lwt_main.run
        (Octra_core.Store_irmin.save_contract_storage store address storage);
      let page =
        Lwt_main.run
          (Octra_core.Store_irmin.list_contract_storage_page
             store
             address
             ~limit:64
             ~value_limit:4096)
      in
      require (List.length page.entries = 64) "program storage page length differs";
      require page.more "program storage page continuation is absent";
      require
        (List.for_all (fun (_, value) -> String.length value = 4096) page.entries)
        "program storage page value limit differs";
      let storage_json pairs =
        `Assoc (List.map (fun (key, value) -> key, `String value) pairs)
      in
      let call params =
        Lwt_main.run
          (Octra_vm.Contract_rpc.call_params
             ~trusted:[]
             ~profile:{epoch = 0; math = false; point_ops = false;
                       object_cost = false; int_work = Octra_vm.Int_work.Active}
             ~store
             ~ledger
             ~get_fhe_pubkey:(fun _ -> None)
             ~storage_json
             params)
      in
      let holder = "oct" ^ String.make 44 '2' in
      let default_view =
        call (`List [
          `String address;
          `String "balance_of";
          `List [`String holder];
        ])
      in
      begin
        match default_view with
        | Ok (`Assoc fields) ->
          require
            (List.assoc_opt "storage" fields = None)
            "program call returned storage by default"
        | Ok _ -> failwith "program call response is invalid"
        | Error error -> failwith error.Octra_core.Rpc.message
      end;
      let storage_view =
        call (`List [
          `String address;
          `String "balance_of";
          `List [`String holder];
          `Null;
          `Bool true;
        ])
      in
      begin
        match storage_view with
        | Ok (`Assoc fields) ->
          require
            (List.assoc_opt "storage_more" fields = Some (`Bool true))
            "program call storage continuation is absent";
          require
            (List.assoc_opt "storage_limit" fields = Some (`Int 64))
            "program call storage limit differs"
        | Ok _ -> failwith "program storage response is invalid"
        | Error error -> failwith error.Octra_core.Rpc.message
      end;
      let head_before = Lwt_main.run (Octra_core.Store_irmin.get_head_hash store) in
      let verify_record chaindata source =
        match
          Lwt_main.run
            (Octra_vm.Contract_rpc.verify
               ~store
               ~chaindata
               ~addr:address
               ~source
               ~files_json:None)
        with
        | Ok (`Assoc fields) ->
          require
            (List.assoc_opt "verified" fields = Some (`Bool true))
            "program verification did not report success";
          require
            (List.assoc_opt "published" fields = Some (`Bool false))
            "binary program verification published metadata";
          require
            (List.assoc_opt "code_hash" fields = Some (`String code_hash))
            "program verification record hash differs"
        | Ok _ -> failwith "program verification response is invalid"
        | Error error -> failwith error.Octra_core.Rpc.message
      in
      let source_file path body =
        `Assoc [
          "path", `String path;
          "source", `String body;
        ]
      in
      let verify_input_rejected files reason =
        match
          Lwt_main.run
            (Octra_vm.Contract_rpc.verify
               ~store
               ~chaindata:(Option.get !chain)
               ~addr:address
               ~source:nested_loop_source
               ~files_json:(Some files))
        with
        | Error error ->
          require
            (error.Octra_core.Rpc.code = -32602
             && error.Octra_core.Rpc.data = Some (`String reason))
            "program source input reason differs"
        | Ok _ -> failwith "program source input was accepted"
      in
      verify_input_rejected
        [source_file "main.aml" nested_loop_source]
        "program source path is reserved";
      verify_input_rejected
        [source_file "lib/value.aml" ""; source_file "lib/value.aml" ""]
        "program source path is duplicated";
      verify_input_rejected
        [source_file "lib/../main.aml" nested_loop_source]
        "program source path is invalid";
      begin
        match
          Octra_core.Store_chaindata.get_program_record
            (Option.get !chain)
            ~address
            ~code_hash
        with
        | Ok None -> ()
        | Ok (Some _) -> failwith "program source input wrote a record"
        | Error reason -> failwith reason
      end;
      let first_files =
        [source_file "z/value.aml" ""; source_file "a/value.aml" ""]
      in
      let second_files = List.rev first_files in
      let verify_files files =
        match
          Lwt_main.run
            (Octra_vm.Contract_rpc.verify
               ~store
               ~chaindata:(Option.get !chain)
               ~addr:address2
               ~source:nested_loop_source
               ~files_json:(Some files))
        with
        | Ok (`Assoc fields) ->
          require
            (List.assoc_opt "published" fields = Some (`Bool false))
            "binary program source map was published"
        | Ok _ -> failwith "binary program verification response is invalid"
        | Error error -> failwith error.Octra_core.Rpc.message
      in
      verify_files first_files;
      verify_files second_files;
      begin
        match
          Octra_core.Store_chaindata.get_program_record
            (Option.get !chain)
            ~address:address2
            ~code_hash
        with
        | Ok None -> ()
        | Ok (Some _) -> failwith "binary program source map was stored"
        | Error reason -> failwith reason
      end;
      verify_record (Option.get !chain) nested_loop_source;
      verify_record (Option.get !chain) (nested_loop_source ^ "\n");
      let head_after = Lwt_main.run (Octra_core.Store_irmin.get_head_hash store) in
      require (head_before = head_after) "program verification changed state root";
      require
        (Lwt_main.run (Octra_core.Store_irmin.get_contract_source store address) = None)
        "program verification wrote source into state";
      close_chain ();
      let reopened =
        Octra_core.Store_chaindata.open_chaindata ~readonly:true history
      in
      chain := Some reopened;
      verify_record reopened nested_loop_source;
      verify_record reopened (nested_loop_source ^ "\n");
      begin
        match
          Octra_core.Store_chaindata.get_program_record
            reopened
            ~address
            ~code_hash
        with
        | Ok None -> ()
        | Ok (Some _) -> failwith "binary program record survived reopen"
        | Error reason -> failwith reason
      end;
      begin
        match
          Octra_core.Store_chaindata.get_program_record
            reopened
            ~address
            ~code_hash:(String.make 64 '0')
        with
        | Ok None -> ()
        | Ok (Some _) -> failwith "program record matched another code hash"
        | Error reason -> failwith reason
      end;
      begin
        match
          Lwt_main.run
            (Octra_vm.Contract_rpc.source
               ~store
               ~chaindata:reopened
               ~addr:address)
        with
        | Ok (`Assoc fields) ->
          require
            (List.assoc_opt "source" fields = Some `Null)
            "binary program source response differs"
        | Ok _ -> failwith "program source response is invalid"
        | Error error -> failwith error.Octra_core.Rpc.message
      end;
      Lwt_main.run
        (Octra_core.Store_irmin.save_contract_source
           store
           address
           nested_loop_source);
      Lwt_main.run
        (Octra_core.Store_irmin.save_contract_abi
           store
           address
           "{\"functions\":[{\"name\":\"prior\"}]}");
      Lwt_main.run
        (Octra_core.Store_irmin.save_contract_verification
           store
           address
           "{\"verified\":true}");
      Lwt_main.run
        (Octra_core.Store_irmin.save_contract_certificate
           store
           address
           "{\"source\":\"prior\"}");
      let next_code = (compile_linked "program upgrade" stable_source).octb in
      let next_hash = hash next_code in
      Lwt_main.run
        (Octra_core.Store_irmin.deploy_contract
           store
           ~address
           ~code_hash:next_hash
           ~version:"OCTB/1"
           ~owner
           ~ctype:"CUSTOM"
           ~admission:"binary"
           ~bytecode_b64:(Base64.encode_exn next_code));
      begin
        match
          Lwt_main.run
            (Octra_vm.Contract_rpc.source
               ~store
               ~chaindata:reopened
               ~addr:address)
        with
        | Ok (`Assoc fields) ->
          require
            (List.assoc_opt "source" fields = Some `Null)
            "program upgrade returned prior source";
          require
            (List.assoc_opt "verification" fields = None)
            "program upgrade returned prior verification";
          require
            (List.assoc_opt "certificate" fields = None)
            "program upgrade returned prior certificate"
        | Ok _ -> failwith "upgraded program source response is invalid"
        | Error error -> failwith error.Octra_core.Rpc.message
      end;
      begin
        match
          Lwt_main.run
            (Octra_vm.Contract_rpc.abi
               ~trusted:[]
               ~point_ops:true
               ~store
               ~chaindata:reopened
               ~addr:address)
        with
        | Ok (`Assoc fields) ->
          require
            (List.assoc_opt "abi" fields = None)
            "program upgrade returned prior ABI"
        | Ok _ -> failwith "upgraded program ABI response is invalid"
        | Error error -> failwith error.Octra_core.Rpc.message
      end)

let rpc_multi_bytecode name =
  let json =
    Some
      (`Assoc [
        "main", `String "main.aml";
        "files", `List [
          `Assoc [
            "path", `String "main.aml";
            "source", `String multi_main_source;
          ];
          `Assoc [
            "path", `String "value.aml";
            "source", `String multi_value_source;
          ];
        ];
      ])
  in
  match Lwt.state (Octra_vm.Contract_rpc.compile_aml_multi_with
      ~compiler:Octra_vm.Program_package.Source ~point_ops:true ~json) with
  | Lwt.Return (Ok (`Assoc fields)) ->
    begin
      match List.assoc_opt "bytecode" fields with
      | Some (`String value) -> Base64.decode_exn value
      | _ -> failwith (name ^ ": bytecode is absent")
    end
  | Lwt.Return (Ok _) -> failwith (name ^ ": response is invalid")
  | Lwt.Return (Error _) -> failwith (name ^ ": RPC rejected sources")
  | Lwt.Fail error -> raise error
  | Lwt.Sleep -> failwith (name ^ ": RPC did not finish")

let () =
  let accepted, state =
    run_squares Octra_vm.Contract_vm.default_ctx 4 1000
  in
  require accepted "base integer work refused";
  require (state.effort_used = 14) "base integer work charge changed";
  require_bits "base integer work" 17 state;
  let accepted, state =
    run_vector_squares Octra_vm.Contract_vm.default_ctx 4 1000
  in
  require accepted "base vector work refused";
  require (state.effort_used = 60) "base vector work charge changed";
  require_bits "base vector work" 17 state;
  let ledger = require_ok "legacy ledger" (compile ledger_source) in
  let ledger_hash = hash ledger.bytecode in
  Printf.printf "legacy_hash = %s\n" ledger_hash;
  require
    (String.equal
      ledger_hash
      "1f227efb461eb1e638dd6790962952052c4e72ea6f086a6bc17d9440d9f40b92")
    "legacy bytecode differs";
  let linked_ledger = compile_linked "linked ledger" ledger_source in
  require
    (String.equal (rpc_bytecode "RPC ledger" ledger_source) linked_ledger.octb)
    "RPC compiler differs";
  let builtin = require_ok "legacy builtin" (compile builtin_source) in
  let builtin_hash = hash builtin.bytecode in
  Printf.printf "builtin_hash = %s\n" builtin_hash;
  require
    (String.equal
      builtin_hash
      "fb6cf083686fb2666aa929d9428917cb3de83474f01b395b4ed5a0763502debf")
    "builtin bytecode differs";
  ignore (require_ok "legacy loose" (compile loose_source));
  let stable_base = require_ok "base program" (compile_program_base stable_source) in
  let stable_checked = require_ok "checked program" (compile_program stable_source) in
  require
    (String.equal stable_base.bytecode stable_checked.bytecode)
    "checked program changed stable bytecode";
  let program_hash = hash stable_base.bytecode in
  Printf.printf "program_hash = %s\n" program_hash;
  require
    (String.equal
      program_hash
      "94c2c6014098af83a2e97fb0f806de4cad02fc259987374982e9ca3297065843")
    "base Program bytecode differs";
  let envelope = Option.get stable_base.program_envelope in
  let envelope_hash = hash envelope in
  Printf.printf "envelope_hash = %s\n" envelope_hash;
  require
    (String.equal
      envelope_hash
      "b23f84aadcf303f27aec8c2b093fc0cf2aa808029eb094f7097ff0aa3f252c18")
    "base Program envelope differs";
  let valid_base = require_ok "base contextual program" (compile_program_base valid_source) in
  let valid = require_ok "valid program" (compile_program valid_source) in
  require
    (not (String.equal valid_base.bytecode valid.bytecode))
    "checked contextual binding is absent";
  require (Option.is_some valid.program_envelope) "Program envelope absent";
  let repeated = require_ok "repeated program" (compile_program valid_source) in
  require
    (valid.program_envelope = repeated.program_envelope)
    "Program compile differs";
  require
    (Option.is_some (compile_program_base bool_source).error)
    "base Program profile changed";
  ignore (require_ok "bool program" (compile_program bool_source));
  ignore (require_ok "keyed program" (compile_program keyed_source));
  require_error "wrong local" wrong_local "local initializer type differs";
  require_error "wrong return" wrong_return "return type differs";
  require_error "missing return" missing_return "function return is not total";
  require_error "wrong call" wrong_call "function argument type differs";
  let context_name = compile_linked "context name" context_name_source in
  begin
    match
      run_bytecode
        ~args:[Octra_vm.Contract_vm.VString "payload"]
        ~value:(Z.of_int 17)
        "context name"
        "digest"
        context_name.octb
    with
    | Octra_vm.Contract_vm.VString value ->
      require (String.equal value "payload") "context name value differs"
    | _ -> failwith "context name result type differs"
  end;
  let context_rpc = rpc_bytecode "RPC context name" context_name_source in
  begin
    match
      run_bytecode
        ~args:[Octra_vm.Contract_vm.VString "payload"]
        ~value:(Z.of_int 17)
        "RPC context name"
        "digest"
        context_rpc
    with
    | Octra_vm.Contract_vm.VString value ->
      require (String.equal value "payload") "RPC context name value differs"
    | _ -> failwith "RPC context name result type differs"
  end;
  let nested_marked = rpc_compile "RPC marked contract" nested_loop_source true in
  require
    (String.equal
       (Base64.decode_exn (rpc_field "RPC marked contract" nested_marked "bytecode"))
       (compile_linked "marked contract" nested_loop_source).octb)
    "client Program flag changed contract compilation";
  let stable_auto = rpc_compile "RPC inferred Program" stable_source false in
  let stable_marked = rpc_compile "RPC marked Program" stable_source true in
  require
    (String.equal
       (rpc_field "RPC inferred Program" stable_auto "deploy_payload")
       (rpc_field "RPC marked Program" stable_marked "deploy_payload"))
    "Program declaration inference differs";
  let linked_nested = compile_linked "linked nested loop" nested_loop_source in
  require
    (String.equal
      (hash linked_nested.octb)
      "3236a1c9b9ca5a4051743fff3ef2936fdf490eb3d9b7931d338532d6d43a85a0")
    "nested loop compiler differs";
  begin
    match run_bytecode "nested loop" "run" linked_nested.octb with
    | Octra_vm.Contract_vm.VInt value ->
      require (Z.equal value (Z.of_int 210)) "nested loop result differs"
    | _ -> failwith "nested loop result type differs"
  end;
  require
    (String.equal
      (rpc_bytecode "RPC nested loop" nested_loop_source)
      linked_nested.octb)
    "nested loop RPC compiler differs";
  List.iter
    (fun (name, source) ->
      let linked = compile_linked name source in
      match
        Octra_vm.Program_package.compile
          ~main:"main.aml"
          ~sources:[Octra_vm.Program_package.{ path = "main.aml"; body = source }]
      with
      | Error error ->
        failwith (name ^ ": " ^ Octra_vm.Program_package.error_message error)
      | Ok packed ->
        require
          (String.equal packed.result.bytecode linked.octb)
          (name ^ " package compiler differs"))
    ["mixed", mixed_source; "main", main_source];
  verify_lane_test ();
  verify_worker_test ();
  verify_record_test linked_nested;
  let multi_resolver = function
    | "main.aml" -> Some multi_main_source
    | "value.aml" -> Some multi_value_source
    | _ -> None
  in
  let linked_multi =
    match Octra_vm.Aml_source.compile_multi ~syntax:Octra_vm.Oct_gen.Source multi_resolver "main.aml" with
    | Ok value -> value
    | Error error -> failwith ("linked multi: " ^ error)
  in
  require
    (String.equal (rpc_multi_bytecode "RPC multi") linked_multi.octb)
    "multi RPC compiler differs";
  let points = require_ok "point program" (compile_program point_source) in
  let point_image =
    match Octra_vm.Bytecode.decode_image points.bytecode with
    | Ok image -> image
    | Error error -> failwith error
  in
  require
    (Array.exists
      (function Octra_vm.Contract_vm.FHE_PEDERSEN_ADD _ -> true | _ -> false)
      point_image.code)
    "point addition opcode absent";
  require
    (Array.exists
      (function Octra_vm.Contract_vm.FHE_PEDERSEN_SUB _ -> true | _ -> false)
      point_image.code)
    "point subtraction opcode absent";
  require
    (Array.exists
      (function Octra_vm.Contract_vm.FHE_PEDERSEN_IDENTITY _ -> true | _ -> false)
      point_image.code)
    "point identity opcode absent";
  require
    (Octra_vm.Bytecode.op_tag
      (Octra_vm.Contract_vm.FHE_PEDERSEN_ADD (0, 1, 2)) = 0x5D
     && Octra_vm.Bytecode.op_tag
       (Octra_vm.Contract_vm.FHE_PEDERSEN_SUB (0, 1, 2)) = 0x5E
     && Octra_vm.Bytecode.op_tag
       (Octra_vm.Contract_vm.FHE_PEDERSEN_IDENTITY 0) = 0x5F)
    "point opcode tag differs";
  let point_hit = Option.get (Octra_vm.Opcode_policy.first_point point_image.code) in
  let point_cell = point_image.cells.(point_hit.pc) in
  let point_error =
    Printf.sprintf
      "unknown opcode 0x%02x at %d"
      (Octra_vm.Bytecode.op_tag point_image.code.(point_hit.pc))
      point_cell.at
  in
  let point_envelope = Option.get points.program_envelope in
  begin
    match Octra_vm.Admission.decode_program_source point_envelope with
    | Error (Octra_vm.Admission.Decode_error reason) ->
      require (String.equal reason point_error) "inactive point reason differs"
    | Error error -> failwith (Octra_vm.Admission.error_message error)
    | Ok _ -> failwith "inactive point program admitted"
  end;
  begin
    match
      Octra_vm.Admission.decode_program_source
        ~point_ops:true
        point_envelope
    with
    | Ok _ -> ()
    | Error error -> failwith (Octra_vm.Admission.error_message error)
  end;
  let marker_code = [|
    Octra_vm.Contract_vm.LDI
      (0, Octra_vm.Contract_vm.VString "\000OCTRA_STATE_V1\000bad");
    Octra_vm.Contract_vm.STOP;
  |] in
  let marker_raw = Octra_vm.Bytecode.encode marker_code in
  begin
    match Octra_vm.Bytecode.decode_image ~active:false marker_raw with
    | Ok image ->
      require (image.state = None) "prior metadata became active";
      require (image.code = marker_code) "prior metadata changed code"
    | Error error -> failwith ("prior metadata rejected: " ^ error)
  end;
  begin
    match Octra_vm.Bytecode.decode_image marker_raw with
    | Error reason ->
      require
        (String.equal reason "OCTB state schema is invalid")
        "active metadata reason differs"
    | Ok _ -> failwith "active malformed metadata accepted"
  end;
  let trailing_raw = Octra_vm.Bytecode.encode [|Octra_vm.Contract_vm.STOP|] ^ "\000" in
  begin
    match Octra_vm.Bytecode.decode_image ~active:false trailing_raw with
    | Error reason ->
      require
        (String.equal reason "OCTB trailing bytes")
        "prior trailing bytes reason differs"
    | Ok _ -> failwith "prior trailing bytes accepted"
  end;
  begin
    match Octra_vm.Bytecode.decode_image trailing_raw with
    | Error reason ->
      require
        (String.equal reason "OCTB trailing bytes: 1")
        "active trailing bytes reason differs"
    | Ok _ -> failwith "active trailing bytes accepted"
  end;
  let left =
    Pvac_ffi.pedersen_commit_amount 7L (Bytes.make 32 '\001')
    |> Bytes.to_string
    |> Base64.encode_exn
  in
  let right =
    Pvac_ffi.pedersen_commit_amount 11L (Bytes.make 32 '\002')
    |> Bytes.to_string
    |> Base64.encode_exn
  in
  let code = point_code left right in
  begin
    match Octra_vm.Admission.of_code code with
    | Error (Octra_vm.Admission.Unsafe_error _) -> ()
    | Error error -> failwith (Octra_vm.Admission.error_message error)
    | Ok _ -> failwith "inactive point opcode admitted"
  end;
  begin
    match Octra_vm.Admission.of_code ~point_ops:true code with
    | Ok _ -> ()
    | Error error -> failwith (Octra_vm.Admission.error_message error)
  end;
  begin
    match
      Octra_vm.Bytecode.decode_image (Octra_vm.Bytecode.encode code)
    with
    | Ok image when image.code = code -> ()
    | Ok _ | Error _ -> failwith "point opcode wire differs"
  end;
  let active =
    { Octra_vm.Contract_vm.default_ctx with point_ops = true }
  in
  let ok, state = run_points active code in
  require ok "active point arithmetic reverted";
  begin
    match state.Octra_vm.Contract_vm.regs.(4) with
    | Octra_vm.Contract_vm.VString value ->
      let identity =
        Pvac_ffi.pedersen_identity ()
        |> Bytes.to_string
        |> Base64.encode_exn
      in
      require (String.equal value identity) "point identity differs"
    | _ -> failwith "point identity result type differs"
  end;
  begin
    match state.Octra_vm.Contract_vm.regs.(0) with
    | Octra_vm.Contract_vm.VString value ->
      require (String.equal value left) "point group inverse differs"
    | _ -> failwith "point group result type differs"
  end;
  let inactive_ok, inactive_state =
    run_points Octra_vm.Contract_vm.default_ctx code
  in
  require
    (not inactive_ok && inactive_state.Octra_vm.Contract_vm.reverted)
    "inactive point arithmetic executed";
  let short_code = point_code "short" right in
  let short_ok, short_state = run_points active short_code in
  require
    (not short_ok && short_state.Octra_vm.Contract_vm.reverted)
    "short point encoding executed";
  let invalid = Base64.encode_exn (String.make 32 '\255') in
  let invalid_code = point_code invalid right in
  let invalid_ok, invalid_state = run_points active invalid_code in
  require
    (not invalid_ok && invalid_state.Octra_vm.Contract_vm.reverted)
    "invalid point encoding executed";
  Printf.printf "status = pass\n%!"

let () =
  let open Octra_vm.Contract_vm in
  let encoded hex =
    require (String.length hex mod 2 = 0) "vector hex length";
    String.init (String.length hex / 2) (fun i ->
      Char.chr (int_of_string ("0x" ^ String.sub hex (i * 2) 2)))
    |> Base64.encode_exn
  in
  let key = encoded "505641430501010000000100000001000000c000000080000000800000000000000000005e409a9999999999e13f0000000000003040804f12000000000000100000004000000100000008000000b81e85eb51b8de3fa4703d0ad7a3e03f0800000000000000000000000100000000000000010000000000000001000000000000000000000000000000010000000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000010000000000000001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001" in
  let cipher = encoded "50564143030001000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000" in
  List.iter (fun point_ops ->
    List.iter (fun is_view ->
      let ctx = { default_ctx with point_ops } in
      let state = create_state ~ctx ~is_view ~limit:1_000_000
        ~caller:"sender" ~origin:"sender" ~address:"program" ~value:Z.zero
        ~storage:(Hashtbl.create 1) () in
      let code = [|
        LDI (0, VString key); FHE_DESER_PK (1, 0);
        LDI (2, VString cipher); FHE_DESER (3, 2);
        FHE_MUL (4, 1, 3, 3); STOP
      |] in
      for _ = 0 to 3 do
        require (step state code = Running) "product input was not decoded"
      done;
      require (step state code = Refused && state.reverted) "product domain was not refused";
      require (state.regs.(4) = VInt Z.zero) "product wrote a result after refusal"
    ) [false; true]
  ) [false; true];
  Printf.printf "status = pass test = product_domain\n%!"