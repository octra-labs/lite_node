(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module VM = Octra_vm.Contract_vm
module Memory = Octra_vm.Fhe_memory

let expect reason condition = if not condition then failwith reason

let state ?(mode = Octra_core.Rule_graph.Active) ?(proof = Octra_core.Rule_graph.Prior)
    ?(view = false) ?(limit = 10_000_000) ?memory source =
  let ctx = {VM.default_ctx with fhe_work = mode; fhe_memory = memory;
    proof_exec = proof; get_fhe_pubkey = (fun _ -> Some source)} in
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

let async_memory key =
  let module V = Octra_vm in
  let module J = V.Program_journal in
  let source = {|
program KeyRead {
  state { count: int }

  public fn read(times: int): int {
    self.count = times
    for index in 0..times {
      let key = fhe_load_pk(caller)
      require(len(fhe_ser_pk(key)) > 0, "key missing")
    }
    return times
  }
}
|} in
  let compiled = match V.Program_package.compile_at
      ~compiler:V.Program_package.Preview ~loops:true ~point_ops:true ~main:"main.aml"
      ~sources:V.Program_package.[{path = "main.aml"; body = source}] with
    | Ok value -> value
    | Error error -> failwith (V.Program_package.error_message error) in
  let checked = match V.Program_package.admit_base64
      ~compiler:V.Program_package.Preview ~loops:true ~point_ops:true
      (Base64.encode_exn compiled.package) with
    | Ok value -> value.program
    | Error error -> failwith (V.Program_package.error_message error) in
  let sender = "oct" ^ String.make 44 '1' in
  let ctx = {VM.default_ctx with math = true; point_ops = true;
    fhe_work = Octra_core.Rule_graph.Active; proof_exec = Octra_core.Rule_graph.Active;
    get_fhe_pubkey = (fun _ -> Some (VM.Key_value key))} in
  Test_workspace.with_dir "fhe-memory" (fun path ->
    let store = Lwt_main.run (Octra_core.Store_irmin.open_store ~fresh:true path) in
    Fun.protect ~finally:(fun () -> Lwt_main.run (Octra_core.Store_irmin.close store))
      (fun () ->
        let journal = J.create () in
        let address, created = V.Contract.deploy ~journal ~admitted:checked ~ctx
          store sender "CUSTOM" [||] compiled.envelope 0 in
        expect "memory program creation refused" created.success;
        V.Program_store.stage store journal;
        J.discard journal;
        let call count = Lwt_main.run (V.Contract.execute_call_async
          ~journal ~ctx ~limit:40_000_000 store address "read" [`Int count] sender Z.zero) in
        let refused = call 10 in
        expect "async call lost its memory budget" (not refused.success);
        expect "async memory refusal kept writes" (J.storage_entries journal = []);
        let accepted = call 1 in
        expect "async memory not renewed per call"
          (accepted.success && accepted.return_value = Some (VM.VInt Z.one))))

let () =
  List.iter (fun size ->
    let value = Z.of_int size in
    let cost = Option.get (Memory.key_effort ~active:true value) |> Z.of_int in
    expect "active key price does not cover volume" (Z.leq value (Z.mul (Z.of_int 16) cost));
    expect "active key price rounds twice" (Z.lt (Z.mul (Z.of_int 16) cost) (Z.add value (Z.of_int 16))))
    [0; 1; 15; 16; 17; 1_024; 1_025; max_int];
  expect "active negative volume accepted" (Memory.key_effort ~active:true Z.minus_one = None);
  expect "active key cost overflow accepted"
    (Memory.key_effort ~active:true (Z.mul (Z.of_int 16) (Z.succ (Z.of_int max_int))) = None);
  List.iter (fun size ->
    let value = Z.of_int size in
    let cost = Option.get (Memory.key_effort value) |> Z.of_int in
    expect "key cost does not cover volume" (Z.leq value (Z.mul (Z.of_int 64) cost));
    expect "key cost rounds twice" (Z.lt (Z.mul (Z.of_int 64) cost) (Z.add value (Z.of_int 64))))
    [0; 1; 63; 64; 65; 1_024; 1_025; max_int];
  expect "negative key volume accepted" (Memory.key_effort Z.minus_one = None);
  expect "key cost overflow accepted"
    (Memory.key_effort (Z.mul (Z.of_int 64) (Z.succ (Z.of_int max_int))) = None);
  let native, secret = Pvac_ffi.keygen_from_seed (Pvac_ffi.default_params ()) (Bytes.make 32 '\001') in
  let key = Octra_core.Fhe_image.of_key native in
  let raw = Pvac_ffi.serialize_pubkey native |> Bytes.to_string in
  let cost = Option.get (Memory.key_decode raw) in
  Printf.printf "event = key_volume packed = %d image = %d reservation = %s\n%!"
    (String.length raw) (Pvac_ffi.pubkey_image_size native) (Z.to_string cost);
  expect "key image size does not match writer" (Pvac_ffi.pubkey_image_size native = image_size raw);
  List.iter (fun (source, volume) ->
    List.iter (fun (mode, proof, view) ->
      let step = if view || proof = Octra_core.Rule_graph.Active then 16 else 64 in
      let cost = (volume + step - 1) / step in
      let accepted = state ~mode ~proof ~view ~limit:(cost + 100) source in
      accepted.regs.(0) <- VM.VString "sender";
      expect "priced key load refused" (VM.exec_one accepted (VM.FHE_LOAD_PK (1, 0)));
      expect "key load not charged by volume" (accepted.effort_used = cost + 100 && cost > 100);
      let budget = Memory.create () in
      let refused = state ~mode ~proof ~view ~limit:(cost + 99) ~memory:budget source in
      refused.regs.(0) <- VM.VString "sender";
      expect "key load ignored effort" (not (VM.exec_one refused (VM.FHE_LOAD_PK (1, 0))));
      expect "exhausted key effort reported a missing key" (!(refused.logs) = []);
      expect "refused key load allocated budget" (Z.equal (Memory.used budget) Z.zero);
      expect "refused key load changed output" (refused.regs.(1) = VM.VInt Z.zero))
      [Octra_core.Rule_graph.Active, Octra_core.Rule_graph.Prior, false;
       Octra_core.Rule_graph.Active, Octra_core.Rule_graph.Active, false;
       Octra_core.Rule_graph.Prior, Octra_core.Rule_graph.Prior, true])
    [VM.Key_bytes raw, String.length raw + image_size raw;
     VM.Key_value key, Pvac_ffi.pubkey_image_size native];
  let write_cost = Option.get (Memory.key_write_effort key) in
  List.iter (fun view ->
    let cost = (Pvac_ffi.pubkey_image_size native + 15) / 16 in
    List.iter (fun (limit, success) ->
      let st = state ~view ~proof:Octra_core.Rule_graph.Active ~limit
        ~memory:(Memory.create ()) (VM.Key_value key) in
      st.regs.(0) <- VM.VPubKey key;
      expect "active key encoding price differs" (VM.exec_one st (VM.FHE_SER_PK (1, 0)) = success);
      if success then expect "active encoding cost differs" (st.effort_used = cost + 50)
      else expect "refused encoding published output" (st.regs.(1) = VM.VInt Z.zero))
      [cost + 50, true; cost + 49, false]) [false; true];
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
  List.iter (fun bytes ->
    let cost = 50 + (String.length bytes + 15) / 16 + (image_size raw + 15) / 16 in
    List.iter (fun proof ->
      List.iter (fun (limit, success) ->
        let st = state ~proof ~limit (VM.Key_value key) in
        st.regs.(0) <- VM.VString bytes;
        expect "key decoding charged twice" (VM.exec_one st (VM.FHE_DESER_PK (1, 0)) = success);
        if success then expect "key decoding cost differs" (st.effort_used = cost)
        else expect "refused decoding published output" (st.regs.(1) = VM.VInt Z.zero))
        [cost, true; cost - 1, false]) Octra_core.Rule_graph.[Prior; Active]) [raw; encoded];
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
  let cipher = Pvac_ffi.enc_value_seeded native secret 1L (Bytes.make 32 '\002') in
  let image = Octra_core.Fhe_image.of_cipher cipher in
  List.iter (fun source ->
    List.iter (fun (mode, proof, view) ->
      let start limit =
        let st = state ~mode ~proof ~view ~limit ~memory:(Memory.create ()) source in
        st.regs.(0) <- VM.VString "sender";
        st.regs.(2) <- VM.VCipher image;
        st in
      let limit = if view || proof = Octra_core.Rule_graph.Active then 4_000_000 else 1_000_000 in
      let ordinary = start limit in
      expect "standard key exceeds default effort" (VM.exec_one ordinary (VM.FHE_LOAD_PK (1, 0)));
      expect "standard addition exceeds declared effort" (VM.exec_one ordinary (VM.FHE_ADD (3, 1, 2, 2)));
      let cost = ordinary.effort_used in
      expect "key transfer changed the wrong mode"
        ((cost > 1_000_000) = (view || proof = Octra_core.Rule_graph.Active));
      expect "standard key writer exceeds declared effort" (VM.exec_one ordinary (VM.FHE_SER_PK (4, 1)));
      (match ordinary.regs.(3) with
       | VM.VCipher output ->
         let decoded = Pvac_ffi.deserialize_cipher (Bytes.of_string output.data) in
         expect "standard addition changed value" (Pvac_ffi.dec_value native secret decoded = 2L)
       | _ -> failwith "standard addition lost output");
      List.iter (fun (limit, accepted) ->
        let st = start limit in
        expect "addition setup exceeds effort" (VM.exec_one st (VM.FHE_LOAD_PK (1, 0)));
        expect "addition effort threshold differs"
          (VM.exec_one st (VM.FHE_ADD (3, 1, 2, 2)) = accepted);
        if not accepted then
          expect "refused addition published output" (st.reverted && st.regs.(3) = VM.VInt Z.zero))
        [cost - 1, false; cost, true];
      let repeated = state ~mode ~proof ~view ~limit:1_000_000 source in
      repeated.regs.(0) <- VM.VString "sender";
      let accepted = ref 0 in
      while !accepted < 4 && VM.exec_one repeated (VM.FHE_LOAD_PK (1, 0)) do incr accepted done;
      let active = view || proof = Octra_core.Rule_graph.Active in
      expect "standard key decodes escaped effort"
        ((!accepted = 0) = active && !accepted < 4 && repeated.reverted))
      [Octra_core.Rule_graph.Active, Octra_core.Rule_graph.Prior, false;
       Octra_core.Rule_graph.Active, Octra_core.Rule_graph.Active, false;
       Octra_core.Rule_graph.Prior, Octra_core.Rule_graph.Prior, true])
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
    st.regs.(1) <- VM.VCipher image;
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
  async_memory key;
  print_endline "event = fhe_memory status = passed"