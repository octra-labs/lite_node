(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_vm

let check value reason = if not value then failwith reason
let get = function Some value -> value | None -> failwith "codec refused value"

let old_pos value =
  let rec loop value out =
    if Z.equal value Z.one then List.rev (false :: false :: out)
    else loop (Z.shift_right value 1)
      (if Z.testbit value 0 then false :: true :: out else true :: false :: out)
  in loop value []

let old_int value =
  false :: true ::
  (if Z.equal value Z.zero then [false; false]
   else (if Z.sign value > 0 then [false; true] else [true; false])
     @ old_pos (Z.abs value))

let compat () =
  let values = List.init 8193 (fun i -> Z.of_int (i - 4096))
    @ List.concat_map (fun bit ->
      let value = Z.shift_left Z.one bit in
      [Z.pred value; value; Z.succ value; Z.neg value])
      [7; 8; 9; 15; 16; 17; 63; 64; 65; 255; 256; 257; 1024] in
  List.iter (fun value ->
    let expected = old_int value in
    check (C_bin.enc_code (C_bin.Int value) = Some expected) "integer bytes changed";
    check (C_bin.dec_code expected = Some (C_bin.Int value)) "integer decode changed";
    check (C_bin.dec_code (expected @ [false]) = None) "extra bit accepted") values;
  List.iter (fun bits -> check (C_bin.dec_code bits = None) "incomplete integer accepted")
    [[]; [false]; [false; true]; [false; true; true; true];
     [false; true; false; true; true; true]]

let cost () =
  List.iter (fun width ->
    let value = Z.pred (Z.shift_left Z.one width) in
    let before = Gc.allocated_bytes () in
    let start = Sys.time () in
    let bits = get (C_bin.enc_code (C_bin.Int value)) in
    check (C_bin.dec_code bits = Some (C_bin.Int value)) "large integer changed";
    let bytes = Gc.allocated_bytes () -. before in
    Printf.printf "codec_bits = %d allocated = %.0f cpu_s = %.6f\n%!"
      width bytes (Sys.time () -. start);
    check (bytes < float_of_int width *. 350. +. 100_000.)
      "integer codec repeats growing allocations") [8192; 16384; 65536; 262144]

let compile () =
  let prefix = "program Literal { state { total: int } form put [] (many value: int) ->[many] int marks {write[7]:total} = write[7](" in
  let suffix = ") public fn set(): int { return use put[](0) as many out: int in out } }" in
  let body = prefix ^ String.make (65_536 - String.length prefix - String.length suffix) '7' ^ suffix in
  let start = Sys.time () in
  let result = Program_package.compile_at ~loops:true ~compiler:Program_package.Source
    ~point_ops:true ~main:"main.aml" ~sources:[Program_package.{path = "main.aml"; body}] in
  Printf.printf "marked_source_bytes = %d cpu_s = %.6f result = %s\n%!"
    (String.length body) (Sys.time () -. start)
    (match result with Ok _ -> "accepted" | Error e -> Program_package.error_message e);
  let built = match result with Ok v -> v | Error _ -> failwith "marked literal compilation failed" in
  let start = Sys.time () in
  let accepted = Program_package.admit_base64 ~loops:true ~consts:true
    ~compiler:Program_package.Source ~point_ops:true (Base64.encode_exn built.package) in
  Printf.printf "marked_package_check_s = %.6f\n%!" (Sys.time () -. start);
  check (Result.is_ok accepted) "marked package verification failed"

let () = compat (); cost (); compile (); print_endline "status = pass test = literal_codec"