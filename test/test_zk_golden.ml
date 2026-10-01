(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let read_all path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let b = Bytes.create n in
  really_input ic b 0 n;
  close_in ic;
  b

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let read_text path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let b = Bytes.create n in
  really_input ic b 0 n;
  close_in ic;
  Bytes.to_string b

let python_in_path () =
  match Sys.command "command -v python3 >/dev/null 2>&1" with
  | 0 -> true
  | _ -> false

let make_workspace () =
  let dir = Test_workspace.unique_dir "zk_golden" in
  Unix.mkdir (Filename.concat dir "bin") 0o755;
  dir

let rm_rf path =
  let _ = Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote path)) in
  ()

let () =
  if not (python_in_path ()) then begin
    failwith "zk conversion requires python3"
  end;

  Random.self_init ();
  if Array.length Sys.argv <> 3 then failwith "expected converter and vectors";
  let converter = Sys.argv.(1) in
  let vectors = Sys.argv.(2) in
  if not (Sys.file_exists converter) then begin
    failwith ("required converter missing: " ^ converter)
  end;
  if not (Sys.file_exists vectors) then begin
    failwith ("required vectors missing: " ^ vectors)
  end;

  let workspace = make_workspace () in
  at_exit (fun () -> rm_rf workspace);

  let bin_dir = Filename.concat workspace "bin" in
  let vk_json = read_text (Filename.concat vectors "verification_key.json") in
  let proof_json = read_text (Filename.concat vectors "proof.json") in
  let public_json = read_text (Filename.concat vectors "public.json") in
  write_file (Filename.concat workspace "verification_key.json") vk_json;
  write_file (Filename.concat workspace "proof.json") proof_json;
  write_file (Filename.concat workspace "public.json") public_json;

  let cmd = Printf.sprintf "python3 %s all %s %s 2>&1"
    (Filename.quote converter) (Filename.quote workspace) (Filename.quote bin_dir)
  in
  let rc = Sys.command cmd in
  if rc <> 0 then begin
    Printf.eprintf "status = fail case = converter exit = %d\n%!" rc; exit 1
  end;

  let vk = read_all (Filename.concat bin_dir "vk.bin") in
  let pf = read_all (Filename.concat bin_dir "proof.bin") in
  let inp = read_all (Filename.concat bin_dir "inputs.bin") in
  Printf.printf "event = converted vk_bytes = %d proof_bytes = %d input_bytes = %d\n%!"
    (Bytes.length vk) (Bytes.length pf) (Bytes.length inp);

  let t0 = Unix.gettimeofday () in
  let result = Zk_ffi.groth16_verify_bn254 vk pf inp in
  let t1 = Unix.gettimeofday () in
  let elapsed_ms = (t1 -. t0) *. 1000.0 in
  Printf.printf "case = real_proof result = %b elapsed_ms = %.2f\n%!" result elapsed_ms;

  if not result then begin
    Printf.eprintf "status = fail case = real_proof reason = valid_proof_rejected\n%!";
    exit 1
  end;

  let proof_tampered = Bytes.copy pf in
  let last = Bytes.length proof_tampered - 1 in
  Bytes.set proof_tampered last (Char.chr ((Char.code (Bytes.get proof_tampered last)) lxor 0x01));
  let r2 = Zk_ffi.groth16_verify_bn254 vk proof_tampered inp in
  if r2 then begin
    Printf.eprintf "status = fail case = proof_mutation reason = invalid_proof_accepted\n%!"; exit 1
  end;
  Printf.printf "case = proof_mutation result = false\n%!";

  let inputs_tampered = Bytes.copy inp in
  let li = Bytes.length inputs_tampered - 1 in
  Bytes.set inputs_tampered li (Char.chr (((Char.code (Bytes.get inputs_tampered li)) + 1) land 0xff));
  let r3 = Zk_ffi.groth16_verify_bn254 vk pf inputs_tampered in
  if r3 then begin
    Printf.eprintf "status = fail case = input_mutation reason = invalid_input_accepted\n%!"; exit 1
  end;
  Printf.printf "case = input_mutation result = false\n%!";

  let mut_iters = 100 in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to mut_iters do
    let _ = Zk_ffi.groth16_verify_bn254 vk pf inp in ()
  done;
  let t1 = Unix.gettimeofday () in
  let avg_ms = ((t1 -. t0) *. 1000.0) /. (float_of_int mut_iters) in
  Printf.printf "event = benchmark mean_ms = %.3f calls = %d\n%!" avg_ms mut_iters;

  Printf.printf "status = pass test = zk_golden cases = 3\n%!"