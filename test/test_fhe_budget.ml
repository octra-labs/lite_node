(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module VM = Octra_vm.Contract_vm

let expect message value = if not value then failwith message

let key ?(columns = 65_536) weight =
  let buffer = Buffer.create 1_600_000 in
  let word size value =
    for index = 0 to size - 1 do
      Buffer.add_char buffer (Char.chr ((value lsr (index * 8)) land 255))
    done in
  Buffer.add_string buffer "PVAC\003\001";
  List.iter (word 4) [8; 64; columns; 0; weight; 0];
  List.iter (word 8) [0; 0; 0; 100_000];
  List.iter (word 4) [64; 64; 1; 8];
  word 8 0; word 8 0; word 4 0; word 8 0;
  word 8 columns;
  for _ = 1 to columns do word 8 64; word 8 1; word 8 0 done;
  for _ = 1 to 2 do
    word 8 64;
    for index = 0 to 63 do word 4 index done
  done;
  Buffer.add_string buffer (String.make 32 '\000');
  word 8 1; word 8 0; word 8 8;
  for _ = 1 to 8 do word 8 1; word 8 0 done;
  Buffer.add_string buffer (String.make 32 '\000');
  Pvac_ffi.deserialize_pubkey (Bytes.of_string (Buffer.contents buffer))

let chain version count =
  let buffer = Buffer.create (count * 80) in
  let word size value =
    for index = 0 to size - 1 do
      Buffer.add_char buffer (Char.chr ((value lsr (index * 8)) land 255))
    done in
  Buffer.add_string buffer "PVAC";
  word 1 version; word 1 0; word 8 1; word 8 count;
  for index = 0 to count - 1 do
    if index = 0 then begin word 1 0; word 8 0; word 8 0; word 8 0 end
    else begin word 1 1; word 4 (index - 1); word 4 (index - 1) end;
    if version = 3 then begin
      Buffer.add_string buffer (String.make 32 '\000'); word 8 0; word 8 0
    end
  done;
  word 8 1; word 8 0; word 8 0; word 8 1;
  word 4 (count - 1); word 2 0; word 1 0;
  word 8 1; word 8 1; word 8 0; word 8 64; word 8 1; word 8 1;
  Buffer.contents buffer

let () =
  let faults = ref [] in
  let check message value = if not value then faults := message :: !faults in
  let pk, sk = Pvac_ffi.keygen_from_seed (Pvac_ffi.default_params ()) (Bytes.make 32 '\001') in
  let cipher = Pvac_ffi.enc_value_seeded pk sk 1L (Bytes.make 32 '\002') in
  let sparse = key 1 in
  let dense = key 65_536 in
  let state limit key left right =
    let ctx = {VM.default_ctx with fhe_work = Octra_core.Rule_graph.Active; math = true} in
    let st = VM.create_state ~ctx ~limit ~caller:"sender" ~origin:"sender"
      ~address:"program" ~value:Z.zero ~storage:(Hashtbl.create 1) () in
    st.regs.(0) <- VM.VPubKey key;
    st.regs.(1) <- VM.VCipher left;
    st.regs.(2) <- VM.VCipher right;
    st in
  let op = VM.FHE_MUL (3, 0, 1, 2) in
  let accepted = VM.prepare_fhe_work (state 1_000_000 sparse cipher cipher) op in
  let refused = not (VM.prepare_fhe_work (state 1_000_000 dense cipher cipher) op) in
  check "pubkey sampling work ignored" (accepted && refused);
  List.iter (fun version ->
    List.iter (fun encode ->
      let raw = chain version 512 in
      let left = Pvac_ffi.deserialize_cipher (Bytes.of_string raw) in
      let small = Pvac_ffi.deserialize_cipher (Bytes.of_string (chain version 1)) in
      let st = state 20_000 pk left small in
      st.regs.(4) <- VM.VString (encode raw);
      expect "layer chain decode refused" (VM.exec_one st (VM.FHE_DESER (1, 4)));
      check "layer traversal work ignored"
        (not (VM.prepare_fhe_work st (VM.FHE_ADD (3, 0, 1, 2)))))
      [Fun.id; Base64.encode_exn]) [3; 4];
  expect (String.concat "; " (List.rev !faults)) (!faults = []);
  let key = key ~columns:64 64 in
  let input = Pvac_ffi.deserialize_cipher (Bytes.of_string (chain 3 1)) in
  let seed = Bytes.make 32 '\003' in
  let old = Pvac_ffi.ct_mul_seeded ~math:true key input input seed in
  let next = Pvac_ffi.ct_mul_work (true, Octra_vm.Fhe_view_policy.sample_factor)
    key input input seed in
  expect "sampling changed successful cipher bytes"
    (Pvac_ffi.serialize_cipher old = Pvac_ffi.serialize_cipher next);
  let refused = try
    ignore (Pvac_ffi.ct_mul_work (true, 1) key input input seed); false
    with Failure reason -> reason = "pvac: sampling effort exhausted" in
  expect "native sampling did not stop at its budget" refused;
  print_endline "event = fhe_budget status = passed"