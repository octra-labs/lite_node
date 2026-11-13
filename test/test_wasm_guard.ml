(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Exec = Octra_circle_runtime.Circle_exec
module Storage = Octra_circle_runtime.Circle_runtime_storage
module Transcript = Octra_core.Circle_hfhe_transcript
module Wire = Octra_core.Circle_wasm_codec

let need value reason =
  if not value then failwith reason

let table entries =
  let values = Hashtbl.create (List.length entries) in
  List.iter (fun (key, value) -> Hashtbl.replace values key value) entries;
  values

let cell_key prefix suffix =
  prefix ^ String.make 64 'a' ^ ":" ^ suffix

let expect_cell_guard before_tbl after_tbl =
  match
    Storage.validate_runtime_storage_delta
      ~proof_mode:Octra_core.Rule_graph.Active
      before_tbl
      after_tbl
  with
  | Error ("circle_runtime_cell_write_denied", _, _) -> ()
  | Error (code, _, _) -> failwith code
  | Ok () -> failwith "circle cell write accepted"

let rec remove_path path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | stat when stat.Unix.st_kind = Unix.S_DIR ->
    Sys.readdir path
    |> Array.iter (fun name -> remove_path (Filename.concat path name));
    Unix.rmdir path
  | _ -> Unix.unlink path

let commit_guard balance before_tbl changed_tbl =
  let data = Filename.concat (Sys.getcwd ()) "runtime_data" in
  let scope = Filename.concat data "wasm-guard" in
  if not (Sys.file_exists data) then Unix.mkdir data 0o700;
  if not (Sys.file_exists scope) then Unix.mkdir scope 0o700;
  let path = Filename.concat scope (string_of_int (Unix.getpid ())) in
  remove_path path;
  let store = Lwt_main.run (Octra_core.Store_irmin.open_store ~fresh:true path) in
  let circle_id = "circle-test" in
  let zero = Octra_core.Circles.zero_hash_hex in
  let info : Octra_core.Circles.circle_info = {
    circle_id;
    runtime = Octra_core.Circles.Octb;
    version = 1L;
    owner = "owner";
    code_hash = zero;
    stable_root = zero;
    assets_root = zero;
    privacy_class = Octra_core.Circles.Public;
    browser_mode = Octra_core.Circles.Gateway_allowed;
    resource_mode = Octra_core.Circles.Public_resources;
    policy_hash = None;
    members_root = None;
    export_policy = None;
    limits = Octra_core.Circles.default_limits;
  } in
  let receipt : Octra_vm.Contract.exec_result = {
    success = true;
    return_value = None;
    effort_used = 0;
    events = [];
    error = None;
    storage_writes = 1;
  } in
  let result storage_tbl : Exec.call_result = {
    calls = false;
    receipt;
    storage_tbl;
    baseline_storage_tbl = before_tbl;
    spawns = [];
    assets = [];
    encrypted_assets = [];
    caller = "caller";
    tx_hash = zero;
    hfhe_binding = {
      circle_id;
      code_hash = zero;
      stable_root = zero;
      public_reads_hash = zero;
      context_hash = zero;
      transcript = [];
    };
  } in
  Fun.protect
    ~finally:(fun () ->
      Lwt_main.run (Octra_core.Store_irmin.close store);
      remove_path path)
    (fun () ->
      Lwt_main.run (Octra_core.Store_irmin.deploy_circle store info);
      ignore
        (Lwt_main.run
           (Octra_core.Store_irmin.save_circle_stable_storage
              store circle_id before_tbl));
      begin
        match
          Lwt_main.run
            (Exec.commit_call_result
               ~table_mode:Octra_core.Rule_graph.Prior
               ~proof_mode:Octra_core.Rule_graph.Active
               store circle_id (result changed_tbl))
        with
        | Error reason ->
          need
            (String.starts_with
               ~prefix:"circle runtime attempted to write reserved stable key "
               reason)
            "circle commit guard reason changed"
        | Ok () -> failwith "circle commit guard accepted"
      end;
      let stored =
        Lwt_main.run
          (Octra_core.Store_irmin.load_circle_stable_storage store circle_id)
      in
      match stored with
      | Error reason -> failwith reason
      | Ok values ->
        need
          (Hashtbl.find_opt values balance = Hashtbl.find_opt before_tbl balance)
          "circle commit guard changed storage")

let test_response_cost () =
  let module VM = Octra_vm.Contract_vm in
  let module Work = Octra_vm.Scalar_work in
  let read active used limit text =
    Exec.vm_response_value ~active ~used ~limit (Some (Wire.Resp_int text)) in
  let check text =
    let cost = Work.parse (String.length text) |> Z.to_int in
    let expected = Ok (Some (VM.VInt (Z.of_string text))) in
    need (read false 17 17 text = (expected, 17)) "prior response work changed";
    need (read true 17 (17 + cost) text = (expected, 17 + cost))
      "response parse effort differs";
    need (read true 17 (16 + cost) text =
      (Error "wasm response integer effort exceeds limit", 16 + cost))
      "response parsed without effort" in
  List.iter check [""; "0x"; "42"; "-0x2a"; "+42"; "0b10_1010"; "000000000";
    "0x" ^ String.make 16_384 'f'; String.make Work.text_limit '0'];
  let wide = "0x1" ^ String.make 16_384 '0' in
  let cost = Work.parse (String.length wide) |> Z.to_int in
  need (read true 17 (17 + cost) wide =
    (Error "wasm response integer width exceeds limit", 17 + cost))
    "response width or effort differs";
  List.iter (fun length ->
    need (read true 17 max_int (String.make length '0') =
      (Error "wasm response integer text exceeds limit", 17))
      "response text limit bypassed") [Work.text_limit + 1; 2_097_142];
  List.iter (fun text ->
    let cost = Work.parse (String.length text) |> Z.to_int in
    need (read true 17 (17 + cost) text =
      (Error "wasm response integer is invalid", 17 + cost))
      "invalid response lost parsing cost";
    need (read false 17 17 text = (Error "wasm response integer is invalid", 17))
      "prior invalid response changed") ["invalid"; "1,2"];
  List.iter (fun (input, output) ->
    List.iter (fun active ->
      need (Exec.vm_response_value ~active ~used:17 ~limit:17 input = (Ok output, 17))
        "nonnumeric response changed") [false; true])
    [None, None; Some Wire.Resp_null, Some (VM.VString "");
     Some (Wire.Resp_bool true), Some (VM.VBool true);
     Some (Wire.Resp_string wide), Some (VM.VString wide)];
  List.iter (fun number ->
    let value = VM.VInt number in
    let cost = Work.decimal number |> Z.to_int in
    need (VM.output_work VM.default_ctx ~used:17 ~limit:17 value = Some 17)
      "prior response decimal changed";
    let ctx = {VM.default_ctx with scalar_work = true} in
    need (VM.output_work ctx ~used:17 ~limit:(17 + cost) value = Some (17 + cost))
      "response decimal effort differs";
    need (VM.output_work ctx ~used:17 ~limit:(16 + cost) value = None)
      "response decimal exceeded effort") [Z.zero; Z.minus_one; Z.shift_left Z.one 65_535];
  need (VM.output_work {VM.default_ctx with scalar_work = true} ~used:0 ~limit:max_int
    (VM.VInt (Z.shift_left Z.one 65_536)) = None) "wide output accepted"

let test_child_number () =
  let module VM = Octra_vm.Contract_vm in
  List.iter (fun active -> List.iter (fun async_exec -> List.iter (fun bits ->
    let number = Z.shift_left Z.one (bits - 1) in
    let child = Ok VM.{return_value = VInt number; effort_used = 7; events = []} in
    let ctx = {VM.default_ctx with scalar_work = active; async_exec;
      call_contract = (fun _ _ _ _ _ -> child);
      call_async = (fun _ _ _ _ _ -> Lwt.return child)} in
    let state = VM.create_state ~ctx ~limit:20_000_000 ~caller:"owner" ~origin:"owner"
      ~address:"program" ~value:Z.zero ~storage:(Hashtbl.create 0) () in
    let code = VM.[|LDI (1, VString "target"); LDI (2, VString "value");
      XCALL (0, 1, 2, 3, 0); STOP|] in
    let result = if async_exec then Lwt_main.run (VM.run_async state code)
      else VM.run state code in
    let accepted = not active || bits <= Octra_vm.Scalar_work.bit_limit in
    need (result = accepted && state.reverted = not accepted)
      (Printf.sprintf "child scalar outcome differs active = %b async = %b bits = %d result = %b reverted = %b"
        active async_exec bits result state.reverted);
    if accepted then need (state.regs.(0) = VM.VInt number) "child scalar value changed"
    else need (state.regs.(0) = VM.VInt Z.zero) "child scalar wrote wide value")
    [65_536; 65_537]) [false; true]) [false; true]

let () =
  if Array.exists (String.equal "--child") Sys.argv then begin
    test_child_number ();
    exit 0
  end;
  test_response_cost ();
  test_child_number ();
  let module R = Octra_core.Rule_graph in
  let chain_id = "octra-devnet-9871-cluster" in
  let plan = Option.get (R.wasm_float_activation_for_chain chain_id) in
  need (plan.activation_epoch = 1_614_500) "WASM float activation changed";
  need (R.fhe_work_activation_for_chain chain_id = Some plan)
    "WASM float and FHE activations differ";
  List.iter (fun epoch ->
    let expected = if epoch < plan.activation_epoch then R.Prior else R.Active in
    let graph = R.create ~chain_id ~root_at:(fun at ->
      need (at = plan.anchor_epoch) "WASM float anchor epoch differs";
      R.Root plan.anchor_state_root) in
    need (R.wasm_float graph ~epoch = Ok expected) "WASM float mode differs";
    need (R.wasm_float_at ~chain_id ~epoch = expected) "WASM profile mode differs";
    let view_graph = R.create ~chain_id ~root_at:(fun at ->
      let plans = List.filter_map Fun.id [
        R.standard_activation graph; R.math_activation graph;
        R.object_cost_activation graph; Some plan] in
      match List.find_opt (fun (p : R.activation) -> p.anchor_epoch = at) plans with
      | Some p -> R.Root p.anchor_state_root
      | None -> R.Missing) in
    let view = Result.get_ok (Octra_vm.Contract_rpc.view_profile view_graph ~epoch) in
    need (view.wasm_float = expected) "WASM view mode differs";
    List.iter (fun root ->
      let graph = R.create ~chain_id ~root_at:(fun _ -> root) in
      need
        (if expected = R.Prior then R.wasm_float graph ~epoch = Ok R.Prior
         else Result.is_error (R.wasm_float graph ~epoch))
        "WASM float accepted unverified anchor")
      [R.Missing; R.Unreadable "read failed"; R.Root "wrong"])
    [1_601_500; 1_611_499; 1_611_500; 1_611_501; 1_612_999; 1_613_000; 1_613_001;
     plan.activation_epoch - 1; plan.activation_epoch;
     plan.activation_epoch + 1];
  List.iter (fun chain_id ->
    let graph = R.create ~chain_id ~root_at:(fun _ -> R.Missing) in
    need (R.wasm_float graph ~epoch:max_int = Ok R.Prior)
      "WASM float activated on unapproved chain") ["octra-mainnet"; "local"];
  need
    (String.equal Transcript.consensus_id "receipt_mode:amount_link_v1")
    "circle hfhe consensus id changed";
  let prior_context = Exec.hfhe_context_hash ~keyed:false ~math:false ~strict:false [] [] None in
  let active_context = Exec.hfhe_context_hash ~keyed:false ~math:false ~strict:true [] [] None in
  let float_context = Exec.hfhe_context_hash ~keyed:false
    ~float_mode:R.Active ~math:false ~strict:true [] [] None in
  need (float_context <> active_context) "WASM float modes share receipt context";
  need
    (String.equal
       prior_context
       "971ef73959dc93c466ddf9dca85886299a1df18a7808a86695283471e45fe895")
    "prior hfhe context hash changed";
  need
    (not (String.equal prior_context active_context))
    "hfhe proof modes share a context hash";
  need
    (String.equal
       Storage.consensus_id
       "circle_storage:cell_owner:standard")
    "circle storage consensus id changed";
  begin
    match Exec.vm_response_value ~active:false ~used:0 ~limit:100 (Some (Wire.Resp_int "invalid")) with
    | Error "wasm response integer is invalid", 0 -> ()
    | Error reason, _ -> failwith reason
    | Ok _, _ -> failwith "invalid wasm integer accepted"
  end;
  begin
    match Exec.vm_response_value ~active:false ~used:0 ~limit:100 (Some (Wire.Resp_int "42")) with
    | Ok (Some (Octra_vm.Contract_vm.VInt value)), 0 ->
      need (Z.equal value (Z.of_int 42)) "wasm integer changed"
    | Ok _, _ -> failwith "wasm integer type changed"
    | Error reason, _ -> failwith reason
  end;
  let cleared = ref false in
  Lwt_main.run
    (Exec.run_preview_prefetch
       ~clear:(fun () -> cleared := true)
       (fun () -> Lwt.fail Exit));
  need !cleared "preview prefetch remained inflight";
  let balance = cell_key "balance_cell:" "ciphertext_commitment" in
  let register = cell_key "register_cell:" "ciphertext_commitment" in
  let first_commitment = Base64.encode_exn (String.make 32 '\000') in
  let second_commitment = Base64.encode_exn (String.make 32 '\001') in
  let changed_commitment = Base64.encode_exn (String.make 32 '\002') in
  let before_tbl =
    table [
      balance, first_commitment;
      register, second_commitment;
      "user:key", "value";
    ] in
  begin
    match
      Storage.validate_runtime_storage_delta
        ~proof_mode:Octra_core.Rule_graph.Active
        before_tbl
        (Hashtbl.copy before_tbl)
    with
    | Ok () -> ()
    | Error (code, _, _) -> failwith code
  end;
  let changed_balance = Hashtbl.copy before_tbl in
  Hashtbl.replace changed_balance balance changed_commitment;
  expect_cell_guard before_tbl changed_balance;
  commit_guard balance before_tbl changed_balance;
  let added_balance = Hashtbl.copy before_tbl in
  let new_balance = cell_key "balance_cell:" "proof" in
  Hashtbl.replace added_balance new_balance changed_commitment;
  expect_cell_guard before_tbl added_balance;
  let removed_register = Hashtbl.copy before_tbl in
  Hashtbl.remove removed_register register;
  expect_cell_guard before_tbl removed_register;
  let changed_user = Hashtbl.copy before_tbl in
  Hashtbl.replace changed_user "user:key" "changed";
  begin
    match
      Storage.validate_runtime_storage_delta
        ~proof_mode:Octra_core.Rule_graph.Active
        before_tbl
        changed_user
    with
    | Ok () -> ()
    | Error (code, _, _) -> failwith code
  end;
  begin
    match
      Storage.validate_runtime_storage_delta
        ~proof_mode:Octra_core.Rule_graph.Prior
        before_tbl
        changed_balance
    with
    | Ok () -> ()
    | Error (code, _, _) -> failwith code
  end;
  Printf.printf "status = pass test = wasm_guard\n%!"