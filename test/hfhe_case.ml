(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let u8 output value = Buffer.add_uint8 output value
let u16 output value = Buffer.add_uint16_le output value
let u32 output value = Buffer.add_int32_le output (Int32.of_int value)
let u64 output value = Buffer.add_int64_le output (Int64.of_int value)
let zeros output count = Buffer.add_string output (String.make count '\000')

let field output value =
  u64 output value;
  u64 output 0

let bits output count =
  u64 output count;
  u64 output 1;
  u64 output 0

let key ?(budget = 0) () =
  let output = Buffer.create 256 in
  Buffer.add_string output "PVAC\001\001";
  List.iter (u32 output) [2; 1; 1; 1; 1; 1];
  zeros output 24;
  u64 output budget;
  List.iter (u32 output) [1; 1; 1; 1];
  zeros output 16;
  u32 output 1;
  u64 output 0;
  u64 output 1;
  bits output 1;
  List.iter (fun _ ->
    u64 output 1;
    u32 output 0) [(); ()];
  zeros output 32;
  field output 1;
  u64 output 2;
  List.iter (field output) [1; 1];
  Buffer.contents output |> Bytes.of_string

let cipher ?(index = 0) ?(width = 1) ?(edges = true) ?(slots = 1) ?(c0 = true) () =
  let output = Buffer.create 192 in
  Buffer.add_string output "PVAC\003\000";
  u64 output slots;
  u64 output 1;
  u8 output 0;
  zeros output 56;
  u64 output 0;
  u64 output 0;
  u64 output (if c0 then slots else 0);
  if c0 then List.iter (field output) (List.init slots (fun _ -> 7));
  u64 output (if edges then 1 else 0);
  if edges then begin
    u32 output 0;
    u16 output index;
    u8 output 0;
    u64 output slots;
    List.iter (field output) (List.init slots (fun _ -> 1));
    bits output width
  end;
  Buffer.contents output |> Bytes.of_string |> Pvac_ffi.deserialize_cipher