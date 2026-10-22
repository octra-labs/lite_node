(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_core
module N = Octra_node_runtime
module V = Octra_vm
module S = C.Store_irmin
module X = C.Epoch_exec

let check reason value = if not value then failwith reason
let get = function Ok value -> value | Error reason -> failwith reason
let run = Lwt_main.run
let owner = "oct" ^ String.make 44 '1'
let address = "oct" ^ String.make 44 '2'
let child = "oct" ^ String.make 44 '3'
let raw = String.init 32 (fun i -> Char.chr ((i * 19) mod 256))
let encoded = Digestif.SHA256.(of_raw_string raw |> to_hex)

let rec remove path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Sys.readdir path |> Array.iter (fun name -> remove (Filename.concat path name));
      Unix.rmdir path
    end else Sys.remove path

let code =
  let output = V.Oct_compile.compile {|
program RootRecord {
  state { th: string }
  public fn rec(): string {
    self.th = tree_hash
    return self.th
  }
  public fn relay(target: address): string {
    self.th = call(target, "rec")
    return self.th
  }
}
|} in
  match output.error with Some reason -> failwith reason | None -> output.bytecode

let preview epoch root =
  N.Consensus_proposal.epoch_exec_env ~chain_id:"tree-context" ~epoch_id:(Int64.of_int epoch)
    ~epoch_ts:(float_of_int (epoch * 10)) ~proposer:owner ~validator_pubkeys:[]
    ~prev_state_root:root ~ready_state_root_at:(fun _ -> Lwt.return_none) ~ready_max_lag:64

let applied epoch root =
  N.Consensus_epoch_apply_env.standard ~chain_id:"tree-context" ~epoch_id:epoch
    ~proposer_addr:owner ~validator_pubkeys:[] ~prev_state_root:root
    ~ready_state_root_at:(fun _ -> Lwt.return_none) ~ready_max_lag:64

let exercise ?(refuse = false) store op_type nested env =
  let ledger = C.Ledger.create store in
  get (C.Ledger.add_account ledger owner (Z.of_int 1_000));
  let fold height = X.prior_fold height |> Result.map (fun plan ->
    {plan with X.proof_exec = C.Rule_graph.proof_exec_at
      ~chain_id:"octra-devnet-9871-cluster" ~epoch:height}) in
  let backend = X.make_live_backend ~fold store ledger in
  run (backend.begin_batch C.Rule_graph.Prior);
  let before = run (S.get_batch_tree_hash store) in
  let method_name, params = if nested then "relay", [`String child] else "rec", [] in
  let message = if op_type = C.Transaction.MultiExec then
      `List [`Assoc ["to", `String address; "method", `String method_name;
        "params", `List params; "amount", `String "0"]]
    else `List params in
  let tx = C.Transaction.{from = owner; to_ = address; amount = Z.zero;
    nonce = 1; ou = Z.of_int 100; timestamp = 0.; signature = ""; public_key = None;
    message = Some (Yojson.Safe.to_string message); op_type;
    encrypted_data = Some method_name} in
  let trust = V.Program_trust.of_env (fun _ -> None)
    |> Result.map_error V.Program_trust.error_message |> get in
  let receipts = ref [] in
  let result = try Ok (run (C.Tx_savepoint.run ~ledger ~store (fun () ->
    N.Consensus_vm_transition.process_tx ~backend ~env ~circle_mode:C.Rule_graph.Prior
      ~wasm_compute_mode:C.Rule_graph.Prior ~program_trust:trust ~object_cost:false
      ~save_receipt_raw:(fun ~tx_hash ~json -> receipts := (tx_hash, json) :: !receipts) tx)))
    with N.Consensus_vm_transition.Policy_unavailable reason -> Error reason in
  match result with
  | Error reason ->
    check reason refuse;
    check "wrong root refusal" (reason = "previous state root is invalid");
    check "invalid root changed state" (before = run (S.get_batch_tree_hash store));
    let account = Option.get (C.Ledger.find_opt ledger owner) in
    check "invalid root charged fee" (Z.equal account.balance (Z.of_int 1_000));
    check "invalid root consumed nonce" (account.nonce = 0);
    check "invalid root published receipt" (!receipts = []);
    S.abort_epoch_batch store;
    None
  | Ok result ->
  check "invalid root was accepted" (not refuse);
  begin match result with
  | Ok (X.Confirmed _) -> ()
  | Ok (X.Rejected_after_fee error) -> failwith error.reason
  | Error (_, reason) -> failwith reason
  end;
  run (C.Ledger.flush_dirty_lwt ledger);
  let after = run (S.get_batch_tree_hash store) in
  check "program did not change state" (before <> after);
  let storage = run (S.load_contract_storage store (if nested then child else address)) in
  let value = Hashtbl.find storage "th" in
  S.abort_epoch_batch store;
  Some (after, value, List.rev !receipts)

let () =
  let path = Printf.sprintf "runtime_data/tree_context_%d" (Unix.getpid ()) in
  if not (Sys.file_exists "runtime_data") then Unix.mkdir "runtime_data" 0o755;
  remove path;
  let store = run (S.open_store path) in
  Fun.protect ~finally:(fun () -> run (S.close store); remove path) (fun () ->
    run (S.begin_epoch_batch ~mode:C.Rule_graph.Prior store);
    List.iter (fun address -> run (S.deploy_contract store ~address ~owner ~ctype:"CUSTOM"
      ~version:"1" ~check:"binary" ~code_hash:Digestif.SHA256.(digest_string code |> to_hex)
      ~bytecode_b64:(Base64.encode_exn code))) [address; child];
    run (S.commit_epoch_batch store "program setup");
    List.iter (fun epoch -> List.iter (fun op_type -> List.iter (fun nested ->
      let left = exercise store op_type nested (preview epoch raw) in
      let right = exercise store op_type nested (applied epoch encoded) in
      check "preview and apply produced different state or receipts" (left = right);
      check "uppercase root changed state"
        (left = exercise store op_type nested (applied epoch (String.uppercase_ascii encoded)));
      List.iter (fun root ->
        check "full ledger root changed state"
          (left = exercise store op_type nested (preview epoch root));
        check "full ledger root changed application"
          (left = exercise store op_type nested (applied epoch root)))
        [encoded ^ encoded; String.uppercase_ascii (encoded ^ encoded);
         encoded ^ String.make 64 'f'];
      List.iter (fun env ->
        let other = exercise store op_type nested env in
        let _, value, _ = Option.get other in
        check "ledger suffix selected as root" (value = String.make 32 '\255' && other <> left))
        [preview epoch (String.make 64 'f' ^ encoded);
         applied epoch (String.make 64 'f' ^ encoded)];
      let _, value, _ = Option.get left in
      check "program did not receive root bytes" (value = raw);
      List.iter (fun invalid -> ignore (exercise ~refuse:true store op_type nested
        (applied epoch invalid))) [""; String.make 31 'a'; String.make 33 'a';
          String.make 64 'z'; String.make 63 '0'; encoded ^ "0";
          encoded ^ String.make 64 'z'; encoded ^ encoded ^ "0"]) [false; true])
      [C.Transaction.ContractCall; C.Transaction.ProgramExec; C.Transaction.MultiExec])
      [1_662_999; 1_663_000; 1_728_099;
        (Option.get (Octra_consensus.C_quorum_policy.stake_activation_for_chain
        "octra-devnet-9871-cluster")).activation_epoch]);
  print_endline "event = tree_context status = pass"