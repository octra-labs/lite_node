(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let vk_json = {|{
  "protocol": "groth16",
  "curve": "bn128",
  "nPublic": 2,
  "vk_alpha_1": ["1", "2", "1"],
  "vk_beta_2": [["3", "4"], ["5", "6"], ["1", "0"]],
  "vk_gamma_2": [["7", "8"], ["9", "10"], ["1", "0"]],
  "vk_delta_2": [["11", "12"], ["13", "14"], ["1", "0"]],
  "IC": [
    ["100", "200", "1"],
    ["300", "400", "1"],
    ["500", "600", "1"]
  ]
}|}

let proof_json = {|{
  "protocol": "groth16",
  "pi_a": ["20", "30", "1"],
  "pi_b": [["40", "50"], ["60", "70"], ["1", "0"]],
  "pi_c": ["80", "90", "1"]
}|}

let public_json = {|["111", "222"]|}

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let read_all path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let b = Bytes.create n in
  really_input ic b 0 n;
  close_in ic;
  b

let python_in_path () =
  match Sys.command "command -v python3 >/dev/null 2>&1" with
  | 0 -> true
  | _ -> false

let make_workspace () =
  let dir = Test_workspace.unique_dir "zk_e2e" in
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
  if Array.length Sys.argv <> 2 then failwith "expected converter path";
  let converter = Sys.argv.(1) in
  if not (Sys.file_exists converter) then begin
    failwith ("required converter missing: " ^ converter)
  end;

  let workspace = make_workspace () in
  let cleanup () = rm_rf workspace in
  at_exit cleanup;

  let bin_dir = Filename.concat workspace "bin" in
  write_file (Filename.concat workspace "verification_key.json") vk_json;
  write_file (Filename.concat workspace "proof.json") proof_json;
  write_file (Filename.concat workspace "public.json") public_json;

  let cmd = Printf.sprintf "python3 %s all %s %s 2>&1"
    (Filename.quote converter)
    (Filename.quote workspace)
    (Filename.quote bin_dir)
  in
  let rc = Sys.command cmd in
  if rc <> 0 then begin
    Printf.eprintf "status = fail case = converter exit = %d\n%!" rc;
    exit 1
  end;

  let vk = read_all (Filename.concat bin_dir "vk.bin") in
  let pf = read_all (Filename.concat bin_dir "proof.bin") in
  let inp = read_all (Filename.concat bin_dir "inputs.bin") in
  Printf.printf "event = converted vk_bytes = %d proof_bytes = %d input_bytes = %d\n%!"
    (Bytes.length vk) (Bytes.length pf) (Bytes.length inp);

  if Bytes.length vk <> 650 then begin
    Printf.eprintf "status = fail case = vk_length actual = %d expected = 650\n%!" (Bytes.length vk);
    exit 1
  end;
  if Bytes.length pf <> 262 then begin
    Printf.eprintf "status = fail case = proof_length actual = %d expected = 262\n%!" (Bytes.length pf);
    exit 1
  end;
  if Bytes.length inp <> 64 then begin
    Printf.eprintf "status = fail case = input_length actual = %d expected = 64\n%!" (Bytes.length inp);
    exit 1
  end;

  let result = Zk_ffi.groth16_verify_bn254 vk pf inp in
  Printf.printf "case = curve_check result = %b\n%!" result;
  if result then begin
    Printf.eprintf "status = fail case = curve_check reason = invalid_points_accepted\n%!";
    exit 1
  end;
  Printf.printf "status = pass test = zk_e2e\n%!"