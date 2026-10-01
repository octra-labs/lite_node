(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module VM = Octra_vm.Contract_vm
module R = Octra_core.Rule_graph

let expect reason value = if not value then failwith reason

let mode epoch =
  let plan = R.{anchor_epoch = 10; anchor_state_root = "root"; activation_epoch = 20} in
  R.activation_mode ~root_at:(fun _ -> R.Root "root") (Some plan) ~epoch
  |> Result.get_ok

let () =
  let plan = R.{anchor_epoch = 10; anchor_state_root = "root"; activation_epoch = 20} in
  expect "prior activation changed" (mode 19 = R.Prior);
  expect "activation missing" (mode 20 = R.Active && mode 21 = R.Active);
  List.iter (fun root ->
    expect "unverified anchor accepted"
      (Result.is_error (R.activation_mode ~root_at:(fun _ -> root) (Some plan) ~epoch:20));
    expect "prior epoch requires future anchor"
      (R.activation_mode ~root_at:(fun _ -> root) (Some plan) ~epoch:19 = Ok R.Prior))
    [R.Missing; R.Unreadable "read error"; R.Root "wrong"];
  List.iter (fun chain_id ->
    expect "unapproved activation scheduled" (R.fhe_work_activation_for_chain chain_id = None);
    List.iter (fun epoch ->
      let graph = R.create ~chain_id ~root_at:(fun _ -> R.Missing) in
      expect "unscheduled rule active"
        (R.fhe_work graph ~epoch = Ok R.Prior && R.fhe_work_at ~chain_id ~epoch = R.Prior))
      [0; 1_601_500; 1_607_500; 1_609_500; 1_611_500; max_int]) ["octra-mainnet"; "local"];
  let chain_id = "octra-devnet-9871-cluster" in
  let plan = match R.fhe_work_activation_for_chain chain_id with
    | Some plan -> plan
    | None -> failwith "devnet FHE work activation missing" in
  expect "FHE work epoch differs" (plan.activation_epoch = 1_611_500);
  expect "FHE and envelope epochs differ"
    (R.tx_envelope_activation_for_chain chain_id = Some plan);
  List.iter (fun epoch ->
    let expected = if epoch < plan.activation_epoch then R.Prior else R.Active in
    let graph = R.create ~chain_id ~root_at:(fun at ->
      expect "FHE anchor epoch differs" (at = plan.anchor_epoch);
      R.Root plan.anchor_state_root) in
    expect "FHE graph mode differs" (R.fhe_work graph ~epoch = Ok expected);
    expect "FHE profile mode differs" (R.fhe_work_at ~chain_id ~epoch = expected);
    List.iter (fun root ->
      let graph = R.create ~chain_id ~root_at:(fun _ -> root) in
      expect "FHE anchor validation differs"
        (if expected = R.Prior then R.fhe_work graph ~epoch = Ok R.Prior
         else Result.is_error (R.fhe_work graph ~epoch)))
      [R.Missing; R.Unreadable "read error"; R.Root "wrong"])
    [0; 1_589_000; 1_609_499; 1_609_500; 1_609_501;
     plan.activation_epoch - 1; plan.activation_epoch;
     plan.activation_epoch + 1; max_int];
  let pk, sk = Pvac_ffi.keygen_from_seed (Pvac_ffi.default_params ()) (Bytes.make 32 '\001') in
  let cipher = Pvac_ffi.enc_value_seeded pk sk 1L (Bytes.make 32 '\002') in
  let original = Pvac_ffi.serialize_cipher cipher in
  let state ?(limit = 20_000) ~epoch ~math () =
    let ctx = {VM.default_ctx with fhe_work = mode epoch; math; point_ops = true} in
    let st = VM.create_state ~limit ~ctx ~caller:"sender" ~origin:"sender"
      ~address:"program" ~value:Z.zero ~storage:(Hashtbl.create 1) () in
    st.regs.(0) <- VM.VPubKey pk;
    st.regs.(1) <- VM.VCipher cipher;
    st.regs.(2) <- VM.VInt Z.one;
    st in
  let rec expand count value =
    if count = 0 then value else expand (count - 1) (Pvac_ffi.ct_add pk value value) in
  let expanded = expand 4 cipher in
  let expanded_bytes = Pvac_ffi.serialize_cipher expanded in
  List.iter (fun op ->
    let st = state ~limit:2_000 ~epoch:20 ~math:true () in
    st.regs.(1) <- VM.VCipher expanded;
    expect "expensive stateful consumer accepted" (not (VM.run st [|op; VM.STOP|]));
    expect "failed consumer published result" (st.regs.(3) = VM.VInt Z.zero);
    expect "failed consumer mutated input" (Pvac_ffi.serialize_cipher expanded = expanded_bytes))
    [VM.FHE_ADD (3, 0, 1, 1); VM.FHE_SUB (3, 0, 1, 1); VM.FHE_MUL (3, 0, 1, 1);
     VM.FHE_SCALE (3, 0, 1, 2); VM.FHE_DIV_CONST (3, 0, 1, 2);
     VM.FHE_ADD_CONST (3, 0, 1, 2); VM.FHE_SUB_CONST (3, 0, 1, 2);
     VM.FHE_SER (3, 1); VM.FHE_COMMIT (3, 0, 1)];
  List.iter (fun aliases ->
    let st = state ~limit:2_000 ~epoch:20 ~math:true () in
    st.regs.(1) <- VM.VCipher expanded;
    let code = Array.append aliases [|VM.FHE_SER (3, 4); VM.STOP|] in
    expect "alias escaped work check" (not (VM.run st code)))
    [[|VM.MOV (4, 1)|]; [|VM.MSTORE (3, 1); VM.MLOAD (4, 3)|]];
  List.iter (fun epoch ->
    let received = ref None in
    let st = state ~limit:10_000 ~epoch ~math:true () in
    let ctx = {st.ctx with call_contract = (fun _ _ _ _ scope ->
      received := Some scope;
      Ok VM.{return_value = VInt Z.zero; effort_used = 12; events = []})} in
    let child = {st with ctx} in
    child.regs.(5) <- VM.VString "target";
    child.regs.(6) <- VM.VString "method";
    expect "child call failed" (VM.exec_one child (VM.XCALL (3, 5, 6, 7, 0)));
    let scope = Option.get !received in
    expect "child depth not propagated" (scope.depth = 1);
    expect "child budget not propagated"
      (scope.limit = if epoch < 20 then None else Some 9_900)) [19; 20];
  List.iter (fun epoch ->
    List.iter (fun op ->
      let received = ref None in
      let st = state ~limit:30_000 ~epoch ~math:true () in
      let ctx = {st.ctx with deploy_contract = (fun _ _ _ scope _ ->
        received := Some scope;
        Ok VM.{spawned_addr = "program"; effort_used = 12; events = []})} in
      let child = {st with ctx} in
      let raw = Octra_vm.Bytecode.encode [|VM.NOP; VM.STOP|] in
      child.regs.(5) <- VM.VString raw;
      expect "spawn failed" (VM.exec_one child op);
      let scope = Option.get !received in
      expect "spawn depth not propagated" (scope.depth = 1);
      expect "spawn budget not propagated"
        (scope.limit = if epoch < 20 then None
          else Some (20_000 - String.length raw / 100)))
      [VM.SPAWN (3, 5); VM.SPAWN2 (3, 5, 7, 0)]) [19; 20];
  let public_image = Bytes.make (38 + 25 * 513) '\000' in
  Bytes.blit_string "PVAC\004\000" 0 public_image 0 6;
  Bytes.set public_image 6 (Char.chr 8);
  Bytes.set public_image 14 (Char.chr 1);
  Bytes.set public_image 15 (Char.chr 2);
  List.iter (fun encode ->
    List.iter (fun epoch ->
      let st = state ~limit:100_000 ~epoch ~math:false () in
      let st = {st with ctx = {st.ctx with point_ops = false}} in
      st.regs.(2) <- VM.VString (encode (Bytes.to_string public_image));
      expect "public allocation cap not selected independently"
        (VM.exec_one st (VM.FHE_DESER (3, 2)) = (epoch < 20))) [19; 20])
    [Fun.id; Base64.encode_exn];
  List.iter (fun math ->
    List.iter (fun op ->
      let code = Array.append (Array.make 10 op) [|VM.STOP|] in
      let prior = state ~epoch:19 ~math () in
      expect "historical execution changed" (VM.run prior code);
      expect "historical effort changed" (prior.effort_used = 10 * VM.effort_cost op + 1);
      List.iter (fun epoch ->
        let st = state ~epoch ~math () in
        expect "stateful expansion accepted" (not (VM.run st code));
        expect "failure not recorded" st.reverted;
        expect "effort exceeded limit" (st.effort_used <= st.effort_limit)) [20; 21])
      [VM.FHE_ADD (1, 0, 1, 1); VM.FHE_SUB (1, 0, 1, 1)];
    List.iter (fun op ->
      let prior = state ~limit:1_000_000 ~epoch:19 ~math () in
      let active = state ~limit:1_000_000 ~epoch:20 ~math () in
      expect "small historical operation refused" (VM.run prior [|op; VM.STOP|]);
      expect "small active operation refused" (VM.run active [|op; VM.STOP|]);
      let encoded st = match st.VM.regs.(3) with
        | VM.VCipher value -> Pvac_ffi.serialize_cipher value
        | _ -> failwith "cipher result missing" in
      expect "successful cipher bytes changed" (encoded prior = encoded active))
      [VM.FHE_ADD (3, 0, 1, 1); VM.FHE_SUB (3, 0, 1, 1);
       VM.FHE_MUL (3, 0, 1, 1);
       VM.FHE_SCALE (3, 0, 1, 2); VM.FHE_DIV_CONST (3, 0, 1, 2);
       VM.FHE_ADD_CONST (3, 0, 1, 2); VM.FHE_SUB_CONST (3, 0, 1, 2)])
    [false; true];
  let slots = (Pvac_ffi.cipher_shape cipher).slots in
  let constant_image = Bytes.make (38 + 16 * slots) '\000' in
  Bytes.blit_string "PVAC\003\000" 0 constant_image 0 6;
  Bytes.set_int64_le constant_image 6 (Int64.of_int slots);
  Bytes.set_int64_le constant_image 22 (Int64.of_int slots);
  let constant = Pvac_ffi.deserialize_cipher constant_image in
  expect "constant control has layers" ((Pvac_ffi.cipher_shape constant).layers = 0);
  List.iter (fun (left, right) ->
    let run epoch =
      let st = state ~limit:1_000_000 ~epoch ~math:true () in
      st.regs.(1) <- VM.VCipher left;
      st.regs.(2) <- VM.VCipher right;
      expect "constant product refused" (VM.run st [|VM.FHE_MUL (3, 0, 1, 2); VM.STOP|]);
      match st.regs.(3) with
      | VM.VCipher value -> Pvac_ffi.serialize_cipher value
      | _ -> failwith "product result missing" in
    expect "constant product bytes changed" (run 19 = run 20))
    [constant, cipher; cipher, constant; constant, constant];
  expect "input mutated" (Pvac_ffi.serialize_cipher cipher = original);
  print_endline "event = fhe_state status = passed"