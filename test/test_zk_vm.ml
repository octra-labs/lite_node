(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module VM = Octra_vm.Contract_vm

let refuses label run =
  match run () with
  | _ -> failwith ("mcl initialization error was hidden: " ^ label)
  | exception Zk_ffi.Backend_unavailable reason when reason = "mcl initialization failed" -> ()

let () =
  refuses "initialize" Zk_ffi.initialize;
  refuses "ffi" (fun () -> Zk_ffi.groth16_verify_bn254 Bytes.empty Bytes.empty Bytes.empty);
  let key = "OG16V1\000\000\000\000" ^ String.make (64 + 3 * 128 + 64) '\000' in
  let proof = "OG16P1" ^ String.make (64 + 128 + 64) '\000' in
  let state = VM.create_state ~is_view:true ~caller:"sender" ~origin:"sender"
    ~address:"program" ~value:Z.zero ~storage:(Hashtbl.create 1) () in
  state.regs.(0) <- VM.VString (Base64.encode_exn key);
  state.regs.(1) <- VM.VString (Base64.encode_exn proof);
  state.regs.(2) <- VM.VString "";
  refuses "vm" (fun () -> VM.run state [|VM.GROTH16_VERIFY_BN254 (3, 0, 1, 2); VM.STOP|]);
  if state.reverted || state.regs.(3) <> VM.VInt Z.zero then
    failwith "mcl initialization error became a transaction outcome";
  print_endline "event = zk_vm status = passed"