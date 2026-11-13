(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module V = Octra_vm
module VM = V.Contract_vm
module C = Octra_core
module R = C.Rule_graph
module S = C.Store_irmin
module T = C.Transaction
module E = C.Epoch_exec
module Shell = Consensus_epoch_vm_shell
module Transition = Consensus_vm_transition

let run = Lwt_main.run
let need reason value = if not value then failwith reason
let unwrap = function Ok value -> value | Error _ -> failwith "expected accepted value"
let compile body = V.Program_package.compile_sources_at ~metered:true ~loops:true
  ~compiler:V.Program_package.Preview ~point_ops:true ~main:"main.aml"
  ~sources:[V.Program_package.{path = "main.aml"; body}] |> unwrap
let owner = "oct" ^ String.make 44 '1'
let receiver = "oct" ^ String.make 44 '2'
let initial = Z.of_int 100_000_000
let child_source = "program Child { state { number: int } constructor(n: int) { require(n > 0, \"positive\") self.number = n } }"
let parent_source = "program Factory { state { made: bool } constructor() { self.made = false } " ^
  "public fn create(code: string, n: int): address { self.made = true return deploy(code, n) } " ^
  "public fn abort(code: string, n: int): address { self.made = true let child = deploy(code, n) require(false, \"parent refusal\") return child } " ^
  "public fn twice(code: string, n: int): address { let child = deploy(code, n) return deploy(code, n) } " ^
  "public fn move_abort(code: string, n: int, destination: address): address { require(transfer(destination, 1), \"transfer\") let child = deploy(code, n) require(false, \"parent refusal\") return child } " ^
  "public fn nothing(): bool { self.made = true return true } }"
let env : E.env = {chain_id = "octra-devnet-9871-cluster"; epoch_id = 2_000_000; proposer_addr = owner;
  validator_addrs = []; validator_pubkeys = []; prev_state_root = String.make 64 'a';
  epoch_ts = 1800000000.; ready_state_root_at = None; ready_max_lag = -1}
let fold epoch = E.prior_fold epoch |> Result.map (fun value -> {value with E.fhe_work = R.Active;
  proof_exec = R.Active; proof_key = R.Active; vm_bytes = R.Active; program_records = R.Active;
  program_mode = R.Active; wasm_float = R.Active; math = true})
let receipt_ok = function Ok (E.Confirmed _) -> true | _ -> false
let reason = function
  | Ok (E.Rejected_after_fee value) -> value.reason
  | Error (_, reason) -> reason
  | Ok (E.Confirmed _) -> "confirmed"

let signatures () =
  let key = String.make 32 '\005' in
  let secret = Mirage_crypto_ec.Ed25519.priv_of_octets key |> unwrap in
  let public = Mirage_crypto_ec.Ed25519.pub_of_priv secret |> Mirage_crypto_ec.Ed25519.pub_to_octets
    |> Base64.encode_exn in
  let tx : T.t = {from = owner; to_ = receiver; amount = Z.zero; nonce = 1; ou = Z.of_int 4_000_000;
    timestamp = env.epoch_ts; signature = ""; public_key = Some public;
    message = Some "{\"program_create\":\"package\",\"params\":[7]}";
    op_type = T.ProgramExec; encrypted_data = Some "create"} in
  let signed = T.sign_with_privkey tx (Base64.encode_exn key) in
  need "test signature refused" (T.verify signed public);
  List.iter (fun changed -> need "changed creation signature accepted" (not (T.verify changed public))) [
    {signed with message = Some "{\"program_create\":\"changed\",\"params\":[7]}"};
    {signed with message = Some "{\"program_create\":\"package\",\"params\":[8]}"};
    {signed with encrypted_data = Some "nothing"}; {signed with to_ = owner};
    {signed with ou = Z.of_int 1_000_000}; {signed with nonce = 2};
  ]

let leader ~program_mode ~records ~scalar path mode backend tx =
  let chaindata = C.Store_chaindata.open_chaindata (Filename.concat path "receipts") in
  Fun.protect ~finally:(fun () -> C.Store_chaindata.close chaindata) (fun () ->
  C.Store_chaindata.begin_batch chaindata;
  let effects = V.Tx_effects.create ~ledger:backend.E.ledger ~store:backend.store in
  let outcome = ref None in
  let reject ?consume_nonce:_ ?notify_reason:_ ?(persist_state = false) tag reason =
    if not persist_state then V.Tx_effects.discard effects;
    outcome := Some (if persist_state then Ok (E.Rejected_after_fee {fee = tx.T.ou; error_type = tag; reason})
      else Error (tag, reason));
    Lwt.return_unit in
  let context ?spawn tx_hash = Shell.make_live_contract_ctx ?spawn ~scalar_work:scalar {
    Shell.value_journal = V.Tx_effects.value effects; program_journal = V.Tx_effects.program effects;
    trusted_program_keys = V.Program_trust.empty; store = backend.store; get_fhe_pubkey = (fun _ -> None);
    proof_mode = R.Active; fhe_work = R.Active; proof_exec = R.Active; proof_key = R.Active;
    vm_bytes = R.Active; wasm_float = R.Active; math = true; object_cost = false;
    current_epoch = env.epoch_id; epoch_time_ms = 1800000000000L;
    tree_hash = Digestif.SHA256.(of_hex env.prev_state_root |> to_raw_string);
    node_id = tx.to_; tx_hash;
  } in
  let runtime = Shell.make_live_call_runtime {
    Shell.reject; debit = V.Tx_effects.debit effects; tx; make_ctx = context ?spawn:None;
    balance = V.Tx_effects.balance effects;
    apply_value_effect = (fun value -> need "value application refused" (V.Tx_effects.apply effects value));
    discard_effects = (fun () -> V.Tx_effects.discard effects);
    discard_fee = (fun fee -> V.Tx_effects.discard effects; backend.ops.debit tx.from fee tx.nonce |> unwrap);
    commit_effects = (fun () -> V.Tx_effects.commit_exn effects);
    confirm = (fun () -> outcome := Some (Ok (E.Confirmed tx.ou)); Lwt.return_unit);
  } in
  let deps = Shell.make_live_vm_tx_deps ~source_create:mode ~metered:true ~inspect_mode:R.Active
    ~records ~spawn_ctx:(fun spawn -> context ~spawn (T.hash tx)) {
      Shell.runtime; trusted_program_keys = V.Program_trust.empty; program_journal = V.Tx_effects.program effects;
      ledger = backend.ledger; store = backend.store; chaindata;
      receipt_epoch = (fun () -> env.epoch_id); ctx_for_hash = context ?spawn:None;
      ensure_account = (fun address -> V.Tx_effects.ensure_account effects address |> unwrap);
      reject_malformed = (fun message -> reject "malformed" message); max_multi_exec_calls = 8;
      proof_mode = R.Active; program_mode; preview = R.Active; program_overlap = false;
      math = true; epoch = env.epoch_id; now = (fun () -> env.epoch_ts);
    } in
  run (Shell.run_vm_tx deps tx);
  C.Store_chaindata.commit_batch chaindata;
  Option.get !outcome, Option.get (C.Store_chaindata.get_contract_receipt chaindata ~tx_hash:(T.hash tx)))

let case ?pool_plan ?message ?(body = child_source) ?(fee = 4_000_000) ?(wallet_parent = false)
    ?(program_mode = R.Active) ?(records = R.Active) ?(scalar = true)
    folder engine label mode method_name package args success expected =
  let path = Filename.concat folder (engine ^ "_" ^ label) in
  Unix.mkdir path 0o700;
  let store = run (S.open_store ~fresh:true (Filename.concat path "store")) in
  Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
    let ledger = C.Ledger.create store in
    C.Ledger.add_account ledger owner initial |> unwrap;
    C.Ledger.add_account ledger receiver Z.zero |> unwrap;
    run (C.Ledger.flush_dirty_lwt ledger);
    let child, parent, package, args = match pool_plan with
      | None -> compile body, compile parent_source, package, args
      | Some path ->
        let plan = Yojson.Safe.from_file path in
        let source = Yojson.Safe.Util.(member "source" plan |> to_string) in
        let body = In_channel.with_open_bin source In_channel.input_all in
        let child = compile body in
        let params = Yojson.Safe.Util.(member "params" plan |> to_list) in
        need "pool arguments" (List.length params = 18);
        let names = List.init 18 (fun index -> "p" ^ string_of_int index) in
        let declarations = List.mapi (fun index name -> name ^ ": " ^
          (if index >= 10 then "bytes32" else "string")) names in
        let body = "program Factory { state { made: bool } constructor() { self.made = false } " ^
          "public fn create(code: string, " ^ String.concat ", " declarations ^ "): address { " ^
          "self.made = true return deploy(code, " ^ String.concat ", " names ^ ") } }" in
        child, compile body, Some (Base64.encode_exn child.package),
          `String (Base64.encode_exn child.envelope) :: params in
    let checked = V.Program_package.admit_base64 ~metered:true ~consts:true ~loops:true
      ~compiler:V.Program_package.Preview ~point_ops:true (Base64.encode_exn parent.package) |> unwrap in
    let rec choose nonce =
      need "parent search limit" (nonce < 100);
      if Program_spawn.non_key (V.Contract.addr_from_code parent.envelope owner nonce) <> wallet_parent
      then nonce else choose (nonce + 1) in
    let journal = V.Program_journal.create () in
    let address, created = run (V.Contract.deploy_async ~journal ~admitted:checked.program
      ~ctx:{VM.default_ctx with point_ops = true; math = true; scalar_work = true; fhe_work = R.Active;
        proof_exec = R.Active; byte_work = Some (V.Byte_work.create V.Byte_work.consensus_limits)}
      ~limit:1_000_000 store owner "CUSTOM" [||] parent.envelope (choose 1)) in
    need ("parent creation: " ^ Option.value ~default:"none" created.error) created.success;
    run (S.begin_epoch_batch store);
    V.Program_store.stage store journal;
    run (S.commit_epoch_batch store "parent");
    C.Ledger.add_account ledger address (Z.of_int 10) |> unwrap;
    run (C.Ledger.flush_dirty_lwt ledger);
    let fold epoch = fold epoch |> Result.map (fun value ->
      {value with E.program_mode; program_records = records}) in
    let backend = E.make_live_backend ~fold ~math:true store ledger in
    run (backend.begin_batch R.Prior);
    let before = run (S.load_contract_storage store address) |> S.sorted_storage_pairs in
    let encoded_message = match package with
      | None -> Yojson.Safe.to_string (`List args)
      | Some encoded -> Yojson.Safe.to_string (`Assoc ["program_create", `String encoded; "params", `List args]) in
    let tx : T.t = {from = owner; to_ = address; amount = Z.zero; nonce = 1; ou = Z.of_int fee;
      timestamp = env.epoch_ts; signature = ""; public_key = None; message = Some (Option.value ~default:encoded_message message);
      op_type = T.ProgramExec; encrypted_data = Some method_name} in
    let execute () = if engine = "leader" then leader ~program_mode ~records ~scalar path mode backend tx else
      let receipt = ref None in
      let result = run (Transition.process_tx ~source_create:mode
        ~save_receipt_raw:(fun ~tx_hash ~json -> need "receipt hash differs" (tx_hash = T.hash tx);
          receipt := Some (Yojson.Safe.from_string json))
        ~backend ~env ~circle_mode:R.Prior ~wasm_compute_mode:R.Active
        ~program_trust:V.Program_trust.empty ~object_cost:false tx) in
      result, Option.get !receipt in
    let result, receipt = try execute () with
      | C.Exec_resource.Unavailable _ when label = "deep_params" -> failwith "deep parameters exhausted host" in
    need (label ^ ": " ^ reason result) (receipt_ok result = success);
    Option.iter (fun text -> need (label ^ " refusal: " ^ reason result) (reason result = text)) expected;
    let child_address = V.Contract.addr_from_code child.envelope address 0 in
    let meta = run (S.get_contract_meta store child_address) in
    let record = run (S.read store (V.Program_record.key child_address)) in
    let after = run (S.load_contract_storage store address) |> S.sorted_storage_pairs in
    let balance name = (Option.get (C.Ledger.find_opt ledger name)).balance in
    need (label ^ " fee debit") (Z.equal (balance owner) (Z.sub initial tx.ou));
    need (label ^ " nonce") ((Option.get (C.Ledger.find_opt ledger owner)).nonce = 1);
    let effort = Yojson.Safe.Util.(member "effort" receipt |> to_int) in
    let child_work = if body = child_source then 0 else begin
      let admitted = V.Program_package.admit_base64 ~metered:true ~consts:true ~loops:true
        ~compiler:V.Program_package.Preview ~point_ops:true (Base64.encode_exn child.package) |> unwrap in
      let _, reference = run (V.Contract.deploy_async ~journal:(V.Program_journal.create ())
        ~admitted:admitted.program ~ctx:{VM.default_ctx with point_ops = true; math = true;
          scalar_work = true; fhe_work = R.Active; proof_exec = R.Active;
          byte_work = Some (V.Byte_work.create V.Byte_work.consensus_limits)}
        ~params:(List.tl args) ~limit:4_000_000 store address "CUSTOM" [||] child.envelope 0) in
      need "reference constructor accepted" (not reference.success);
      let preparation = V.Program_package.scalar_cost (Option.get package) |> Option.get |> Z.to_int in
      need "failed child effort missing" (effort >= preparation + reference.effort_used);
      reference.effort_used
    end in
    need (label ^ " receipt success") (Yojson.Safe.Util.(member "success" receipt |> to_bool) = success);
    if success && (label = "create" || label = "pool") then begin
      let meta = Option.get meta in
      need "child metadata" (meta.owner = address && meta.check = "source" && meta.version = "2.0 AML-preview");
      need "child record missing" (Option.is_some record);
      need "child record hash" (Option.get record |> C.Store_chaindata.matching_program_record meta.code_hash |> Result.is_ok);
      need "wallet upgrade accepted" (V.Contract.upgrade ~journal:(V.Program_journal.create ())
        ~ctx:VM.default_ctx
        store ~address:child_address ~caller:owner ~expected_code_hash:meta.code_hash ~bytecode_raw:parent.envelope
        = Error "only the program owner can upgrade");
      need "parent state missing" (List.assoc "made" after = "true");
      Option.iter (fun path ->
        let plan = Yojson.Safe.from_file path in
        let params = Yojson.Safe.Util.(member "params" plan |> to_list) in
        let storage = run (S.load_contract_storage store child_address) |> S.sorted_storage_pairs in
        need "pool field count" (List.length storage = 53);
        let preparation = V.Program_package.scalar_cost (Option.get package) |> Option.get |> Z.to_int in
        let creation = V.Contract.creation_work true fee child.envelope params |> Option.get in
        need "pool preparation effort missing" (effort >= preparation + creation);
        List.iter (fun (index, field) -> need ("pool " ^ field)
          (List.assoc field storage = Yojson.Safe.Util.to_string (List.nth params index)))
          [0,"scope"; 1,"asset"; 6,"image"; 8,"config"; 9,"checkpoint"])
        pool_plan
    end else if not success then begin
      need (label ^ " metadata survived") (meta = None && record = None);
      need (label ^ " parent writes survived") (after = before);
      need (label ^ " value survived") (Z.equal (balance receiver) Z.zero && Z.equal (balance address) (Z.of_int 10))
    end;
    run (backend.flush_dirty ());
    run (backend.commit_batch ());
    let root = run (S.get_head_hash store) |> Option.get in
    `Assoc ["engine", `String engine; "case", `String label; "accepted", `Bool success;
      "reason", `String (reason result); "root", `String root; "effort", `Int effort;
      "child_work", `Int child_work])

let () =
  need "use test_program_spawn [OUTPUT [POOL]]" (Array.length Sys.argv <= 3);
  let folder = if Array.length Sys.argv > 1 then Sys.argv.(1) else
    let parent = "runtime_data/program_spawn" in
    if not (Sys.file_exists "runtime_data") then Unix.mkdir "runtime_data" 0o700;
    if not (Sys.file_exists parent) then Unix.mkdir parent 0o700;
    Filename.concat parent (string_of_int (Unix.getpid ())) in
  need "output exists" (not (Sys.file_exists folder));
  Unix.mkdir folder 0o700;
  signatures ();
  let child = compile child_source in
  let encoded = Base64.encode_exn child.package in
  let args = [`String (Base64.encode_exn child.envelope); `Int 7] in
  let rejected = "program Child { state { number: int } constructor(n: int) { let i: int = 0 " ^
    "while i < n { i = i + 1 } self.number = i require(false, \"child refusal\") } }" in
  let refused = compile rejected in
  let deep = "{\"program_create\":\"a\",\"params\":[" ^ String.make 1_000_000 '[' ^
    "0" ^ String.make 1_000_000 ']' ^ "]}" in
  let source = V.Program_package.compile_sources_at ~metered:true ~loops:true
    ~compiler:V.Program_package.Source ~point_ops:true ~main:"main.aml"
    ~sources:[V.Program_package.{path = "main.aml"; body = child_source}] |> unwrap in
  let checks engine = [
    case folder engine "create" R.Active "create" (Some encoded) args true None;
    case ~program_mode:R.Prior folder engine "program_inactive" R.Active "create" (Some encoded) args
      false (Some "program creation rules inactive");
    case ~records:R.Prior folder engine "records_inactive" R.Active "create" (Some encoded) args
      false (Some "program creation rules inactive");
    case folder engine "parent_abort" R.Active "abort" (Some encoded) args false None;
    case folder engine "value_abort" R.Active "move_abort" (Some encoded) (args @ [`String receiver]) false None;
    case folder engine "second_child" R.Active "twice" (Some encoded) args false None;
    case folder engine "child_abort" R.Active "create" (Some encoded)
      [`String (Base64.encode_exn child.envelope); `Int 0] false None;
    case folder engine "no_child" R.Active "nothing" (Some encoded) [] false (Some "program child was not created");
    case folder engine "wrong_code" R.Active "create" (Some encoded)
      [`String (Base64.encode_exn (compile parent_source).envelope); `Int 7] false None;
    case folder engine "ordinary" R.Active "nothing" None [] true None;
    case folder engine "prior" R.Prior "nothing" (Some "not a package") [] true None;
    case folder engine "bad_package" R.Active "create" (Some "not a package") args false (Some "program compilation work invalid");
    case ~fee:199_999 folder engine "fee_floor" R.Active "create" (Some encoded) args false (Some "program source fee below minimum");
    case ~fee:200_000 folder engine "record_fee" R.Active "create" (Some encoded) args false (Some "program source fee below byte cost");
    case folder engine "load_limit" R.Active "create" (Some (String.make 300_000 'A')) args false (Some "program compilation work exceeds limit");
    case ~wallet_parent:true folder engine "wallet_parent" R.Active "create" (Some encoded) args false (Some "program creation parent can be a wallet");
    case folder engine "source_profile" R.Active "create" (Some (Base64.encode_exn source.package)) args false (Some "program child compiler differs");
    case ~message:"{\"program_create\":\"a\",\"program_create\":\"b\",\"params\":[]}"
      folder engine "duplicate" R.Active "create" None [] false (Some "program creation fields invalid");
    case ~message:"{\"program_create\":\"a\",\"extra\":true,\"params\":[]}"
      folder engine "extra" R.Active "create" None [] false (Some "program creation fields invalid");
    case folder engine "html_input" R.Active "create" (Some encoded) [`String "<script>"]
      false (Some "program creation parameters refused");
    case folder engine "nested_params" R.Active "create" (Some encoded) [`List [`Int 7]]
      false (Some "program creation parameters refused");
    case ~body:rejected folder engine "short_abort" R.Active "create"
      (Some (Base64.encode_exn refused.package)) [`String (Base64.encode_exn refused.envelope); `Int 1] false None;
    case ~body:rejected folder engine "long_abort" R.Active "create"
      (Some (Base64.encode_exn refused.package)) [`String (Base64.encode_exn refused.envelope); `Int 9] false None;
    case ~message:deep folder engine "deep_params" R.Active "create" None []
      false (Some "program creation parameters refused");
  ] in
  let pool engine = if Array.length Sys.argv < 3 then [] else [
    case ~pool_plan:Sys.argv.(2) folder engine "pool" R.Active "create" None [] true None;
    case ~pool_plan:Sys.argv.(2) ~fee:1_000_000 folder engine "pool_work" R.Active "create" None []
      false (Some "program compilation work exceeds limit");
  ] in
  let left = checks "leader" @ pool "leader" in
  let right = checks "validator" @ pool "validator" in
  List.iter (fun cases ->
    let number label field = List.find (fun json -> Yojson.Safe.Util.(member "case" json |> to_string) = label) cases
      |> Yojson.Safe.Util.member field |> Yojson.Safe.Util.to_int in
    let child_work = number "long_abort" "child_work" - number "short_abort" "child_work" in
    need "failed child work difference missing" (child_work > 0 &&
      number "long_abort" "effort" - number "short_abort" "effort" = child_work)) [left; right];
  List.iter2 (fun left right -> need "leader and validator roots differ"
    (Yojson.Safe.Util.member "root" left = Yojson.Safe.Util.member "root" right);
    need "leader and validator effort differs"
      (Yojson.Safe.Util.member "effort" left = Yojson.Safe.Util.member "effort" right)) left right;
  let output = `Assoc ["status", `String "passed"; "cases", `List (left @ right);
    "synthetic_signature_checks", `Int 7;
    "network_activation", `Bool false; "deployment_ready", `Bool false] in
  let channel = open_out_gen [Open_wronly; Open_creat; Open_excl; Open_binary] 0o600 (Filename.concat folder "result.json") in
  Fun.protect ~finally:(fun () -> close_out channel) (fun () ->
    output_string channel (Yojson.Safe.pretty_to_string output ^ "\n"));
  print_endline "event = program_spawn status = passed"