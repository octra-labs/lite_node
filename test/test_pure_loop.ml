(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Graph = Octra_core.Rule_graph
module Package = Octra_vm.Program_package
module Shell = Octra_node_runtime.Consensus_epoch_vm_shell
module Read_rpc = Octra_node_runtime.Program_read_rpc

let expect ok text = if not ok then failwith text

let source ?(head = "program Loop") write = head ^ {| {
  public pure fn run(): int {
    let total = 0
    for i in 0..1 {
|} ^ write ^ {|
      total += i
    }
    return total
  }
}
|}

let package ?(loops = false) compiler body =
  match Package.compile_at ~loops ~compiler ~point_ops:true ~main:"entry.aml"
    ~sources:[Package.{ path = "entry.aml"; body }] with
  | Error error -> failwith (Package.error_message error)
  | Ok result -> result

let check ?(loops = false) ~epoch compiler body =
  let chain_id = "octra-devnet-9871-cluster" in
  let plan = Option.get (Graph.proof_activation_for_chain chain_id) in
  let graph = Graph.create ~chain_id ~root_at:(fun epoch ->
    expect (epoch = plan.anchor_epoch) "proof anchor epoch differs";
    Graph.Root plan.anchor_state_root)
  in
  let proof_exec = match Graph.proof_exec graph ~epoch with
    | Ok value -> value
    | Error _ -> failwith "proof policy failed"
  in
  let compiled = package ~loops compiler body in
  let tx = Octra_core.Transaction.{
    from = "oct" ^ String.make 44 '1';
    to_ = "oct" ^ String.make 44 '2';
    amount = Z.zero;
    nonce = 1;
    ou = Z.of_int 1_000_000;
    timestamp = 0.;
    signature = "";
    public_key = None;
    message = None;
    op_type = ProgramDeploy;
    encrypted_data = Some (Base64.encode_exn compiled.package);
  } in
  let program_mode, preview = match compiler with
    | Package.Protocol -> Graph.Prior, Graph.Prior
    | Package.Source -> Graph.Active, Graph.Prior
    | Package.Preview -> Graph.Active, Graph.Active
  in
  Lwt_main.run (Shell.prepare_program_package
    ~proof_exec ~preview ~overlap:false ~program_mode ~point_ops:true tx)

let rpc ~chain_id ~epoch name params =
  let unused _ = failwith "unexpected rpc route" in
  let skip _ = unused in
  let routes = Read_rpc.dispatch Read_rpc.{
    store_label_read = skip;
    store_chaindata_read = skip;
    chaindata_read = skip;
    no_ctx = skip;
    json0_read = skip;
    compile_read = compile_at ~chain_id ~epoch;
    program_info = unused;
    program_list = unused;
    program_call = unused;
    program_abi = unused;
    program_save_abi = unused;
    program_tokens_by_address = unused;
  } in
  Lwt_main.run ((List.assoc name routes) params)

let rpc_cases write =
  let multi = `Assoc [
    "main", `String "entry.aml";
    "files", `List [
      `Assoc [
        "path", `String "entry.aml";
        "source", `String ("import Value from \"api.aml\"\n" ^
          source ~head:"program Loop implements Value" write);
      ];
      `Assoc [
        "path", `String "api.aml";
        "source", `String "interface Value { fn run(): int }";
      ];
    ];
  ] in
  [
    "octra_compileAml", `List [`String (source write)];
    "octra_compileAml", `List [`String (source ~head:"contract Loop" write)];
    "octra_compileAmlMulti", `List [multi];
  ]

let check_rpc () =
  let chain_id = "octra-devnet-9871-cluster" in
  List.iter (fun (name, params) ->
    List.iter (fun epoch ->
      expect (Result.is_ok (rpc ~chain_id ~epoch name params))
        "historical rpc changed") [1_649_999; 1_650_000; 1_650_001; 1_662_999];
    begin match rpc ~chain_id ~epoch:1_663_000 name params with
    | Error error ->
      expect (error.Octra_core.Rpc.code = -32000)
        "rpc loop error code differs";
      expect (String.ends_with ~suffix:"pure loop index is immutable = i" error.message)
        ("rpc loop error differs = " ^ error.message)
    | Ok _ -> failwith "rpc accepted loop index write"
    end;
    expect (Result.is_ok (rpc ~chain_id:"octra-mainnet" ~epoch:1_663_000 name params))
      "other network rpc changed")
    (rpc_cases "      i -= 1");
  List.iter (fun (name, params) ->
    let old = rpc ~chain_id ~epoch:1_662_999 name params in
    let fresh = rpc ~chain_id ~epoch:1_663_000 name params in
    expect (Result.is_ok old && old = fresh) "valid rpc output changed")
    (rpc_cases "      let j = i + 1")

let run_image ?(value = 7) image =
  let code = match Octra_vm.Admission.decode_program_source ~point_ops:true image with
    | Ok value -> Octra_vm.Admission.code value
    | Error _ -> failwith "program decode failed"
  in
  let state = Octra_vm.Contract.setup_call_state ~caller:"caller" ~address:"program"
    ~value:Z.zero ~storage_tbl:(Hashtbl.create 1) ~method_name:"run" ~params:[] () in
  let result = Octra_vm.Contract.run_from_dispatcher state code in
  expect (result.success && result.return_value = Some (Octra_vm.Contract_vm.VInt (Z.of_int value)))
    "scoped program result differs"

let check_branches () =
  let wrap body = "program Branch { public fn run(): int { let total = 0 "
    ^ body ^ " return total } }" in
  let cases = [
    8, "let k = 8 if false { let k = 2 } total += k";
    3, "for i in 0..3 { total += 1 }";
    6, "for i in 0..2 { for j in 0..3 { total += 1 } }";
    3, "let stop = 3 for i in 0..stop { stop += 1 total += 1 }";
    10, "let k = 8 if true { total += k } else { let k = 3 } total += 2";
    13, "let k = 8 if true { let k = 2 if true { let k = 3 total += k } total += k } total += k";
    9, "let k = 8 if true { k += 1 } else { k += 2 } total += k";
    8, "let k = 8 while false { let k = 2 } total += k";
    12, "let k = 8 let i = 0 while i < 2 { let k = 2 total += k i += 1 } total += k";
  ] in
  let compile loops compiler body =
    Package.compile_at ~loops ~compiler ~point_ops:true ~main:"entry.aml"
      ~sources:[Package.{path = "entry.aml"; body}] in
  List.iter (fun compiler ->
    List.iter (fun (value, body) ->
      let body = wrap body in
      let fresh = package ~loops:true compiler body in
      run_image ~value fresh.envelope;
      expect (Result.is_ok (check ~loops:true ~epoch:1_663_000 compiler body))
        "scoped branch refused";
      match compile false compiler body with
      | Error _ -> ()
      | Ok old ->
        expect (Result.is_ok (check ~epoch:1_662_999 compiler body))
          "historical branch refused";
        if old.envelope <> fresh.envelope then
          expect (Result.is_error (check ~epoch:1_663_000 compiler body))
            "old branch package accepted") cases;
    List.iter (fun body ->
      let body = wrap body in
      expect (Result.is_error (compile true compiler body))
        "branch local escaped")
      ["if false { let lost = 4 } return lost";
       "if true { total += lost } else { let lost = 4 }";
       "while false { let lost = 4 } return lost"])
    [Package.Protocol; Package.Source; Package.Preview];
  let body = wrap (snd (List.hd cases)) in
  let chain_id = "octra-devnet-9871-cluster" in
  let params = `List [`String body] in
  expect (Result.is_error (rpc ~chain_id ~epoch:1_662_999 "octra_compileAml" params))
    "historical branch typing changed";
  expect (Result.is_error (rpc ~chain_id:"octra-mainnet" ~epoch:1_663_000 "octra_compileAml" params))
    "other network branch typing changed";
  List.iter (fun epoch ->
    match rpc ~chain_id ~epoch "octra_compileAml" params with
    | Error _ -> failwith "branch rpc refused"
    | Ok json ->
      let image = Yojson.Safe.Util.(json |> member "program_envelope" |> to_string) in
      run_image ~value:8 (Base64.decode_exn image))
    [1_663_000; 1_663_001]

let check_alias () =
  List.iter (fun write ->
    let body = {|
program Loop {
  enum E { A }
  public pure fn run(): int {
    let k = 0
    let z = 0
    match E.A { E.A => { let k = 0 let z = 0 } }
    let pad = 0
    for i in 0..1 {
|} ^ write ^ {|
    }
    return 7
  }
}
|} in
    List.iter (fun compiler ->
      expect (Result.is_ok (check ~epoch:1_662_999 compiler body))
        "historical register alias changed";
      match check ~epoch:1_663_000 compiler body with
      | Error _ ->
        let compiled = package ~loops:true compiler body in
        run_image compiled.envelope;
        let meta = Octra_core.Store_irmin.{
          address = "program";
          code_hash = Digestif.SHA256.(digest_string compiled.envelope |> to_hex);
          version = "1"; owner = "owner"; ctype = "CUSTOM"; admission = "source";
        } in
        begin match Octra_vm.Contract_rpc.verify_compilation ~main:"entry.aml"
            ~meta:(Some meta) ~source:body ~files_json:None with
        | Ok results ->
          expect (List.exists (fun (bytes, _) -> bytes = compiled.envelope) results)
            "source verification lost scoped bytes"
        | Error _ -> failwith "source verification refused scoped program"
        end;
        expect (Result.is_ok (check ~loops:true ~epoch:1_663_000 compiler body))
          "scoped package refused"
      | Ok _ -> failwith "old match package accepted")
      [Package.Protocol; Package.Source; Package.Preview];
    let chain_id = "octra-devnet-9871-cluster" in
    let inputs = [
      "octra_compileAml", `List [`String body];
      "octra_compileAmlMulti", `List [`Assoc [
        "main", `String "entry.aml";
        "files", `List [`Assoc ["path", `String "entry.aml"; "source", `String body]];
      ]];
    ] in
    List.iter (fun (route, params) ->
      expect (Result.is_ok (rpc ~chain_id ~epoch:1_662_999 route params))
        "historical rpc alias changed";
      match rpc ~chain_id ~epoch:1_663_000 route params with
      | Error _ -> failwith "scoped rpc refused"
      | Ok json ->
        let image = Yojson.Safe.Util.(json |> member "program_envelope" |> to_string) in
        run_image (Base64.decode_exn image)) inputs)
    ["      k = 0 - 1"; "      z += 1"]

let () =
  check_branches ();
  check_alias ();
  check_rpc ();
  List.iter (fun compiler ->
    let invalid = source "      i = 0 - 1" in
    expect (Result.is_ok (check ~epoch:1_662_999 compiler invalid))
      "historical loop source changed";
    begin match check ~epoch:1_663_000 compiler invalid with
    | Error reason ->
      expect (String.ends_with ~suffix:"pure loop index is immutable = i" reason)
        ("wrong loop refusal = " ^ reason)
    | Ok _ -> failwith "active loop index write accepted"
    end;
    let valid = source "      let j = i + 1" in
    let old = check ~epoch:1_662_999 compiler valid in
    let fresh = check ~loops:true ~epoch:1_663_000 compiler valid in
    begin match old, fresh with
    | Ok left, Ok right ->
      if compiler = Package.Protocol then
        expect (left.envelope <> right.envelope) "loop registers did not change"
      else
        expect (left.envelope = right.envelope) "valid loop bytes changed"
    | _ -> failwith "valid loop refused"
    end)
    [Package.Protocol; Package.Source; Package.Preview];
  Printf.printf "event = pure_loop status = passed\n%!"