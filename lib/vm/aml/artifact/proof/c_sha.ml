(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let word value = Int32.of_string value

let init = [|
  word "0x6a09e667"; word "0xbb67ae85"; word "0x3c6ef372";
  word "0xa54ff53a"; word "0x510e527f"; word "0x9b05688c";
  word "0x1f83d9ab"; word "0x5be0cd19";
|]

let keys = Array.map word [|
  "0x428a2f98"; "0x71374491"; "0xb5c0fbcf"; "0xe9b5dba5";
  "0x3956c25b"; "0x59f111f1"; "0x923f82a4"; "0xab1c5ed5";
  "0xd807aa98"; "0x12835b01"; "0x243185be"; "0x550c7dc3";
  "0x72be5d74"; "0x80deb1fe"; "0x9bdc06a7"; "0xc19bf174";
  "0xe49b69c1"; "0xefbe4786"; "0x0fc19dc6"; "0x240ca1cc";
  "0x2de92c6f"; "0x4a7484aa"; "0x5cb0a9dc"; "0x76f988da";
  "0x983e5152"; "0xa831c66d"; "0xb00327c8"; "0xbf597fc7";
  "0xc6e00bf3"; "0xd5a79147"; "0x06ca6351"; "0x14292967";
  "0x27b70a85"; "0x2e1b2138"; "0x4d2c6dfc"; "0x53380d13";
  "0x650a7354"; "0x766a0abb"; "0x81c2c92e"; "0x92722c85";
  "0xa2bfe8a1"; "0xa81a664b"; "0xc24b8b70"; "0xc76c51a3";
  "0xd192e819"; "0xd6990624"; "0xf40e3585"; "0x106aa070";
  "0x19a4c116"; "0x1e376c08"; "0x2748774c"; "0x34b0bcb5";
  "0x391c0cb3"; "0x4ed8aa4a"; "0x5b9cca4f"; "0x682e6ff3";
  "0x748f82ee"; "0x78a5636f"; "0x84c87814"; "0x8cc70208";
  "0x90befffa"; "0xa4506ceb"; "0xbef9a3f7"; "0xc67178f2";
|]

let rotr value count =
  Int32.logor
    (Int32.shift_right_logical value count)
    (Int32.shift_left value (32 - count))

let shr value count = Int32.shift_right_logical value count

let xor3 first second third =
  Int32.logxor first (Int32.logxor second third)

let choose x y z =
  Int32.logxor (Int32.logand x y) (Int32.logand (Int32.lognot x) z)

let major x y z =
  xor3 (Int32.logand x y) (Int32.logand x z) (Int32.logand y z)

let sum0 value = xor3 (rotr value 2) (rotr value 13) (rotr value 22)
let sum1 value = xor3 (rotr value 6) (rotr value 11) (rotr value 25)
let sig0 value = xor3 (rotr value 7) (rotr value 18) (shr value 3)
let sig1 value = xor3 (rotr value 17) (rotr value 19) (shr value 10)

let get32 bytes offset =
  let byte index = Int32.of_int (Char.code (Bytes.get bytes (offset + index))) in
  Int32.logor
    (Int32.shift_left (byte 0) 24)
    (Int32.logor
      (Int32.shift_left (byte 1) 16)
      (Int32.logor (Int32.shift_left (byte 2) 8) (byte 3)))

let set32 bytes offset value =
  let put index shift =
    let byte = Int32.to_int (Int32.logand (Int32.shift_right_logical value shift) 0xffl) in
    Bytes.set bytes (offset + index) (Char.chr byte)
  in
  put 0 24;
  put 1 16;
  put 2 8;
  put 3 0

let pad input =
  let size = String.length input in
  let zeros = (56 - ((size + 1) mod 64) + 64) mod 64 in
  let out = Bytes.make (size + 1 + zeros + 8) '\000' in
  Bytes.blit_string input 0 out 0 size;
  Bytes.set out size (Char.chr 128);
  let bits = Int64.mul (Int64.of_int size) 8L in
  for index = 0 to 7 do
    let shift = (7 - index) * 8 in
    let byte = Int64.to_int (Int64.logand (Int64.shift_right_logical bits shift) 0xffL) in
    Bytes.set out (Bytes.length out - 8 + index) (Char.chr byte)
  done;
  out

let block state bytes offset =
  let plan = Array.make 64 0l in
  for index = 0 to 15 do
    plan.(index) <- get32 bytes (offset + (index * 4))
  done;
  for index = 16 to 63 do
    plan.(index) <-
      Int32.add
        (Int32.add (sig1 plan.(index - 2)) plan.(index - 7))
        (Int32.add (sig0 plan.(index - 15)) plan.(index - 16))
  done;
  let a = ref state.(0) in
  let b = ref state.(1) in
  let c = ref state.(2) in
  let d = ref state.(3) in
  let e = ref state.(4) in
  let f = ref state.(5) in
  let g = ref state.(6) in
  let h = ref state.(7) in
  for index = 0 to 63 do
    let first =
      Int32.add
        (Int32.add (Int32.add !h (sum1 !e)) (choose !e !f !g))
        (Int32.add keys.(index) plan.(index))
    in
    let second = Int32.add (sum0 !a) (major !a !b !c) in
    h := !g;
    g := !f;
    f := !e;
    e := Int32.add !d first;
    d := !c;
    c := !b;
    b := !a;
    a := Int32.add first second
  done;
  state.(0) <- Int32.add state.(0) !a;
  state.(1) <- Int32.add state.(1) !b;
  state.(2) <- Int32.add state.(2) !c;
  state.(3) <- Int32.add state.(3) !d;
  state.(4) <- Int32.add state.(4) !e;
  state.(5) <- Int32.add state.(5) !f;
  state.(6) <- Int32.add state.(6) !g;
  state.(7) <- Int32.add state.(7) !h

let hash input =
  let bytes = pad input in
  let state = Array.copy init in
  for offset = 0 to (Bytes.length bytes / 64) - 1 do
    block state bytes (offset * 64)
  done;
  let out = Bytes.make 32 '\000' in
  Array.iteri (fun index value -> set32 out (index * 4) value) state;
  Bytes.unsafe_to_string out