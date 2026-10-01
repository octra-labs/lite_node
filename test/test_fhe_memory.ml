(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module VM = Octra_vm.Contract_vm
module Memory = Octra_vm.Fhe_memory

let expect reason condition = if not condition then failwith reason

let state ?(mode = Octra_core.Rule_graph.Active) ?(view = false) ?(limit = 10_000_000) ?memory source =
  let ctx = {VM.default_ctx with fhe_work = mode; fhe_memory = memory;
    get_fhe_pubkey = (fun _ -> Some source)} in
  VM.create_state ~ctx ~is_view:view ~limit ~caller:"sender"
    ~origin:"sender" ~address:"program" ~value:Z.zero ~storage:(Hashtbl.create 1) ()

let allowance amount =
  let budget = Memory.create () in
  expect "cannot set memory allowance" (Memory.reserve budget (Z.sub Memory.limit amount));
  budget

let image_size raw =
  expect "expected compressed key" (String.length raw >= 5 && Char.code raw.[0] = 0xec);
  let size = ref 0 in
  for index = 1 to 4 do size := (!size lsl 8) lor Char.code raw.[index] done;
  !size

let () =
  let key, secret = Pvac_ffi.keygen_from_seed (Pvac_ffi.default_params ()) (Bytes.make 32 '\001') in
  let raw = Pvac_ffi.serialize_pubkey key |> Bytes.to_string in
  let cost = Option.get (Memory.key_decode raw) in
  Printf.printf "event = key_volume packed = %d image = %d reservation = %s\n%!"
    (String.length raw) (Pvac_ffi.pubkey_image_size key) (Z.to_string cost);
  expect "key image size does not match writer" (Pvac_ffi.pubkey_image_size key = image_size raw);
  List.iter (fun (source, cost) ->
    List.iter (fun (mode, view) ->
      let cost = Option.get cost in
      let accepted = state ~mode ~view ~limit:(cost + 100) source in
      accepted.regs.(0) <- VM.VString "sender";
      expect "priced key load refused" (VM.exec_one accepted (VM.FHE_LOAD_PK (1, 0)));
      expect "key load not charged by volume" (accepted.effort_used = cost + 100 && cost > 100);
      let budget = Memory.create () in
      let refused = state ~mode ~view ~limit:(cost + 99) ~memory:budget source in
      refused.regs.(0) <- VM.VString "sender";
      expect "key load ignored effort" (not (VM.exec_one refused (VM.FHE_LOAD_PK (1, 0))));
      expect "exhausted key effort reported a missing key" (!(refused.logs) = []);
      expect "refused key load allocated budget" (Z.equal (Memory.used budget) Z.zero);
      expect "refused key load changed output" (refused.regs.(1) = VM.VInt Z.zero))
      [Octra_core.Rule_graph.Active, false; Octra_core.Rule_graph.Prior, true])
    [VM.Key_bytes raw, Memory.key_read_effort raw; VM.Key_value key, Memory.key_write_effort key];
  let write_cost = Option.get (Memory.key_write_effort key) in
  let priced = state ~limit:(write_cost + 50) (VM.Key_value key) in
  priced.regs.(0) <- VM.VPubKey key;
  expect "priced key writer refused" (VM.exec_one priced (VM.FHE_SER_PK (1, 0)));
  expect "key writer not charged by volume" (priced.effort_used = write_cost + 50);
  let refused = state ~limit:(write_cost + 49) (VM.Key_value key) in
  refused.regs.(0) <- VM.VPubKey key;
  expect "key writer ignored effort" (not (VM.exec_one refused (VM.FHE_SER_PK (1, 0))));
  expect "refused key writer changed output" (refused.regs.(1) = VM.VInt Z.zero);
  let budget = allowance (Z.mul (Z.of_int 2) cost) in
  let st = state ~memory:budget (VM.Key_bytes raw) in
  st.regs.(0) <- VM.VString "sender";
  expect "first key load refused" (VM.exec_one st (VM.FHE_LOAD_PK (1, 0)));
  expect "key retention refused" (VM.exec_one st (VM.MSTORE (0, 1)));
  expect "second key load refused" (VM.exec_one st (VM.FHE_LOAD_PK (1, 0)));
  expect "repeated key load escaped shared budget" (not (VM.exec_one st (VM.FHE_LOAD_PK (2, 0))));
  expect "refused key load published output" (st.regs.(2) = VM.VInt Z.zero);
  expect "key reservation not retained" (Z.equal (Memory.used budget) Memory.limit);
  expect "memory alias lost key" (match Hashtbl.find_opt st.memory.data 0 with
    | Some (VM.VPubKey _) -> true | _ -> false);
  let child = VM.child_scope st in
  expect "child scope lost reservation owner" (match child.memory with
    | Some memory -> memory == budget | None -> false);
  let nested = state ?memory:child.memory (VM.Key_bytes raw) in
  nested.regs.(0) <- VM.VString "sender";
  expect "child reset exhausted budget" (not (VM.exec_one nested (VM.FHE_LOAD_PK (1, 0))));
  let before = Memory.used budget in
  List.iter (fun amount -> expect "invalid reservation accepted" (not (Memory.reserve budget amount)))
    [Z.minus_one; Z.one; Z.of_int max_int];
  expect "refusal changed budget" (Z.equal before (Memory.used budget));
  expect "empty compressed header accepted" (Memory.key_decode "\236" = None);
  expect "oversized compressed key accepted" (Memory.key_decode "\236\255\255\255\255" = None);
  expect "unsupported expansion accepted" (Memory.key_decode "\236\001\000\000\000" = None);
  let resident = Memory.key_value key in
  let budget = allowance (Z.pred resident) in
  let st = state ~memory:budget (VM.Key_value key) in
  st.regs.(0) <- VM.VString "sender";
  expect "resident key load ignored budget" (not (VM.exec_one st (VM.FHE_LOAD_PK (1, 0))));
  let encoded = Base64.encode_exn raw in
  List.iter (fun (op, input, cost) ->
    let budget = allowance (Z.pred cost) in
    let st = state ~memory:budget (VM.Key_value key) in
    st.regs.(0) <- input;
    expect "key codec ignored budget" (not (VM.exec_one st op));
    expect "key codec published refused output" (st.regs.(1) = VM.VInt Z.zero);
    let accepted = state (VM.Key_value key) in
    accepted.regs.(0) <- input;
    expect "small key codec refused" (VM.exec_one accepted op))
    [VM.FHE_SER_PK (1, 0), VM.VPubKey key, resident;
     VM.FHE_DESER_PK (1, 0), VM.VString encoded, cost];
  let cipher = Pvac_ffi.enc_value_seeded key secret 1L (Bytes.make 32 '\002') in
  List.iter (fun source ->
    List.iter (fun (mode, view) ->
      let ordinary = state ~mode ~view ~limit:1_000_000 ~memory:(Memory.create ()) source in
      ordinary.regs.(0) <- VM.VString "sender";
      ordinary.regs.(2) <- VM.VCipher cipher;
      expect "standard key exceeds default effort" (VM.exec_one ordinary (VM.FHE_LOAD_PK (1, 0)));
      expect "standard addition exceeds default effort" (VM.exec_one ordinary (VM.FHE_ADD (3, 1, 2, 2)));
      expect "standard key writer exceeds default effort" (VM.exec_one ordinary (VM.FHE_SER_PK (4, 1)));
      (match ordinary.regs.(3) with
       | VM.VCipher output -> expect "standard addition changed value" (Pvac_ffi.dec_value key secret output = 2L)
       | _ -> failwith "standard addition lost output");
      let repeated = state ~mode ~view ~limit:1_000_000 source in
      repeated.regs.(0) <- VM.VString "sender";
      let accepted = ref 0 in
      while !accepted < 4 && VM.exec_one repeated (VM.FHE_LOAD_PK (1, 0)) do incr accepted done;
      expect "standard key decodes escaped effort" (!accepted > 0 && !accepted < 4 && repeated.reverted))
      [Octra_core.Rule_graph.Active, false; Octra_core.Rule_graph.Prior, true])
    [VM.Key_bytes raw; VM.Key_value key];
  let cipher_raw = Pvac_ffi.serialize_cipher cipher |> Bytes.to_string in
  let budget = allowance (Z.pred (Memory.cipher_decode cipher_raw)) in
  let st = state ~memory:budget (VM.Key_value key) in
  st.regs.(0) <- VM.VString (Base64.encode_exn cipher_raw);
  expect "cipher decode ignored budget" (not (VM.exec_one st (VM.FHE_DESER (1, 0))));
  List.iter (fun op ->
    let budget = allowance Z.zero in
    let st = state ~memory:budget (VM.Key_value key) in
    st.regs.(0) <- VM.VPubKey key;
    st.regs.(1) <- VM.VCipher cipher;
    st.regs.(2) <- VM.VInt Z.one;
    expect "cipher consumer ignored budget" (not (VM.exec_one st op));
    expect "cipher consumer published refused output" (st.regs.(3) = VM.VInt Z.zero))
    [VM.FHE_ADD (3, 0, 1, 1); VM.FHE_SUB (3, 0, 1, 1); VM.FHE_MUL (3, 0, 1, 1);
     VM.FHE_SCALE (3, 0, 1, 2); VM.FHE_DIV_CONST (3, 0, 1, 2);
     VM.FHE_ADD_CONST (3, 0, 1, 2); VM.FHE_SUB_CONST (3, 0, 1, 2);
     VM.FHE_SER (3, 1); VM.FHE_COMMIT (3, 0, 1)];
  let prior = state ~mode:Octra_core.Rule_graph.Prior (VM.Key_value key) in
  prior.regs.(0) <- VM.VString "sender";
  for _ = 1 to 100 do expect "historical key load changed" (VM.exec_one prior (VM.FHE_LOAD_PK (1, 0))) done;
  expect "historical key load charge changed" (prior.effort_used = 10_000 && prior.ctx.fhe_memory = None);
  expect "historical key writer changed" (VM.exec_one prior (VM.FHE_SER_PK (2, 1)));
  expect "historical key writer charge changed" (prior.effort_used = 10_050);
  let view = state ~mode:Octra_core.Rule_graph.Prior ~view:true ~memory:(allowance Z.zero) (VM.Key_value key) in
  view.regs.(0) <- VM.VString "sender";
  expect "prior view escaped memory guard" (not (VM.exec_one view (VM.FHE_LOAD_PK (1, 0))));
  expect "refusal mutated cipher" (Pvac_ffi.serialize_cipher cipher |> Bytes.to_string = cipher_raw);
  print_endline "event = fhe_memory status = passed"