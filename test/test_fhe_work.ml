(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module VM = Octra_vm.Contract_vm
module Image = Octra_core.Fhe_image

let expect message value = if not value then failwith message

let edge_image bits =
  let buffer = Buffer.create 256 in
  let word size value =
    for index = 0 to size - 1 do
      Buffer.add_char buffer (Char.chr ((value lsr (index * 8)) land 255))
    done in
  Buffer.add_string buffer "PVAC\003\000";
  word 8 1;
  word 8 1;
  word 1 0;
  List.iter (word 8) [0; 0; 0];
  Buffer.add_string buffer (String.make 32 '\000');
  List.iter (word 8) [0; 0; 1; 0; 0; 1];
  word 4 0;
  word 2 0;
  word 1 0;
  List.iter (word 8) [1; 1; 0; bits; bits / 64];
  Buffer.add_string buffer (String.make (bits / 8) '\000');
  Buffer.contents buffer |> Base64.encode_exn

let () =
  let module Work = Octra_vm.Fhe_view_policy in
  let shape slots layers edges = Work.{slots; layers; edges} in
  let work = Work.work ~left_words:0 ~right_words:0 ~product_words:0 ~sample_work:0 in
  List.iter (fun kind ->
    List.iter (fun invalid ->
      expect "invalid shape accepted"
        (work kind ~base:500 ~left:invalid ~right:invalid = None))
      [shape max_int max_int max_int; shape 0 2 40; shape 1 (-1) 40;
       shape 1 2 (-1); shape 1 4_097 0; shape 65_537 0 0])
    [Work.Copy; Work.Join; Work.Product];
  expect "product expansion accepted"
    (work Work.Product ~base:10_000 ~left:(shape 1 256 40)
       ~right:(shape 1 256 40) = None);
  expect "slot mismatch accepted"
    (work Work.Join ~base:500 ~left:(shape 1 2 40)
       ~right:(shape 2 2 40) = None);
  expect "constant cipher rejected"
    (Option.is_some (work Work.Copy ~base:500
      ~left:(shape 1 0 0) ~right:(shape 1 0 0)));
  List.iter (fun words ->
    expect "bit vector storage limit ignored"
      (Work.work Work.Join ~base:500 ~left_words:words ~right_words:words
        ~product_words:0 ~sample_work:0 ~left:(shape 1 1 1) ~right:(shape 1 1 1) = None))
    [-1; max_int; 1_048_576];
  expect "product bit vector storage limit ignored"
    (Work.work Work.Product ~base:10_000 ~left_words:0 ~right_words:0
      ~product_words:max_int ~sample_work:0 ~left:(shape 1 1 1) ~right:(shape 1 1 1) = None);
  let pk, sk = Pvac_ffi.keygen_from_seed (Pvac_ffi.default_params ()) (Bytes.make 32 '\001') in
  let key = Image.of_key pk in
  let key_cost = (String.length key.data + Image.key_size key + 15) / 16 in
  let edge_effort bits =
    let st = VM.create_state ~limit:(1_000_000 + 4 * key_cost) ~ctx:VM.default_ctx ~is_view:true
      ~caller:"sender" ~origin:"sender" ~address:"program" ~value:Z.zero
      ~storage:(Hashtbl.create 1) () in
    st.regs.(0) <- VM.VPubKey (Image.of_key pk);
    st.regs.(2) <- VM.VString (edge_image bits);
    expect "edge image refused" (VM.exec_one st (VM.FHE_DESER (1, 2)));
    let initial = st.effort_used in
    expect "small edge expansion refused" (VM.run st [|
      VM.FHE_ADD (1, 0, 1, 1); VM.FHE_ADD (1, 0, 1, 1);
      VM.FHE_ADD (1, 0, 1, 1); VM.FHE_ADD (1, 0, 1, 1);
      VM.FHE_SER (3, 1); VM.STOP|]);
    let used = st.effort_used - initial in
    expect "edge key charge missing" (used >= 4 * key_cost);
    used - 4 * key_cost in
  expect "bit vector storage is unmetered" (edge_effort 65_536 > 2 * edge_effort 64);
  let base = Pvac_ffi.enc_value_seeded pk sk 1L (Bytes.make 32 '\002') in
  let run ?(limit = 20_000) ~is_view op =
    let limit = limit + (if is_view then 10 * key_cost else 0) in
    let st = VM.create_state ~limit ~ctx:VM.default_ctx ~is_view
      ~caller:"sender" ~origin:"sender" ~address:"program" ~value:Z.zero
      ~storage:(Hashtbl.create 1) () in
    st.regs.(0) <- VM.VPubKey (Image.of_key pk);
    st.regs.(1) <- VM.VCipher (Image.of_cipher base);
    st.regs.(2) <- VM.VInt Z.one;
    let program = Array.init 11 (fun index -> if index = 10 then VM.STOP else op) in
    let success = VM.run st program in
    expect "effort exceeds limit" (st.effort_used <= st.effort_limit);
    if success && is_view then
      expect "view key charge missing"
        (st.effort_used >= 10 * (VM.effort_cost op + key_cost) + 1);
    success, st
  in
  List.iter (fun op ->
    let success, st = run ~is_view:true op in
    expect "view expansion accepted" (not success && st.reverted);
    let success, _ = run ~is_view:false op in
    expect "historical execution changed" success)
    [VM.FHE_ADD (1, 0, 1, 1); VM.FHE_SUB (1, 0, 1, 1)];
  List.iter (fun op ->
    let success, _ = run ~limit:200_000 ~is_view:true op in
    expect "small cipher arithmetic rejected" success)
    [VM.FHE_SCALE (1, 0, 1, 2); VM.FHE_DIV_CONST (1, 0, 1, 2);
     VM.FHE_ADD_CONST (1, 0, 1, 2); VM.FHE_SUB_CONST (1, 0, 1, 2)];
  let rec expand count cipher =
    if count = 0 then cipher else expand (count - 1) (Pvac_ffi.ct_add pk cipher cipher) in
  let expanded = expand 4 base in
  let before = Pvac_ffi.serialize_cipher expanded in
  List.iter (fun op ->
    let key_cost = match op with VM.FHE_SER _ -> 0 | _ -> key_cost in
    let st = VM.create_state ~limit:(2_000 + key_cost) ~ctx:VM.default_ctx ~is_view:true
      ~caller:"sender" ~origin:"sender" ~address:"program" ~value:Z.zero
      ~storage:(Hashtbl.create 1) () in
    st.regs.(0) <- VM.VPubKey (Image.of_key pk);
    st.regs.(1) <- VM.VCipher (Image.of_cipher expanded);
    st.regs.(2) <- VM.VInt Z.one;
    expect "expensive cipher consumer accepted" (not (VM.run st [|op; VM.STOP|]));
    expect "rejected operation published a result" (st.regs.(3) = VM.VInt Z.zero);
    expect "rejected operation changed input" (Pvac_ffi.serialize_cipher expanded = before))
    [VM.FHE_SCALE (3, 0, 1, 2); VM.FHE_DIV_CONST (3, 0, 1, 2);
     VM.FHE_ADD_CONST (3, 0, 1, 2); VM.FHE_SUB_CONST (3, 0, 1, 2);
     VM.FHE_SER (3, 1); VM.FHE_COMMIT (3, 0, 1)];
  List.iter (fun math ->
    List.iter (fun (op, native) ->
      let st = VM.create_state ~limit:20_000 ~ctx:{VM.default_ctx with math}
        ~caller:"sender" ~origin:"sender" ~address:"program" ~value:Z.zero
        ~storage:(Hashtbl.create 1) () in
      st.regs.(0) <- VM.VPubKey (Image.of_key pk);
      st.regs.(1) <- VM.VCipher (Image.of_cipher expanded);
      st.regs.(2) <- VM.VInt Z.one;
      expect "stateful operation refused" (VM.run st [|op; VM.STOP|]);
      expect "stateful effort changed" (st.effort_used = VM.effort_cost op + 1);
      match st.regs.(3) with
      | VM.VCipher cipher ->
        expect "stateful bytes changed"
          (cipher.data = Bytes.to_string (Pvac_ffi.serialize_cipher (native math)))
      | _ -> failwith "stateful result missing")
      [VM.FHE_ADD (3, 0, 1, 1), (fun _ -> Pvac_ffi.ct_add pk expanded expanded);
       VM.FHE_SUB (3, 0, 1, 1), (fun _ -> Pvac_ffi.ct_sub pk expanded expanded);
       VM.FHE_SCALE (3, 0, 1, 2), (fun math -> Pvac_ffi.ct_scale ~math pk expanded 1L)])
    [false; true];
  print_endline "event = fhe_work status = passed"