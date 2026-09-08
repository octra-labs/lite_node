(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bits = C_bin.bits

let be64 value =
  let out = Bytes.make 8 '\000' in
  let value = Int64.of_int value in
  for index = 0 to 7 do
    let shift = (7 - index) * 8 in
    let byte =
      Int64.to_int
        (Int64.logand (Int64.shift_right_logical value shift) 0xffL)
    in
    Bytes.set out index (Char.chr byte)
  done;
  Bytes.unsafe_to_string out

let pack input =
  let size = List.length input in
  let count = size / 8 + if size mod 8 = 0 then 0 else 1 in
  let out = Bytes.make count '\000' in
  let rec fill index = function
    | [] -> ()
    | bit :: rest ->
        if bit then begin
          let slot = index / 8 in
          let shift = 7 - (index mod 8) in
          let byte = Char.code (Bytes.get out slot) lor (1 lsl shift) in
          Bytes.set out slot (Char.chr byte)
        end;
        fill (index + 1) rest
  in
  fill 0 input;
  Bytes.unsafe_to_string out

let image input = be64 (List.length input) ^ pack input

let frame program prior state =
  String.concat "" ["AML2ROOT\001"; image program; prior; image state]

let root ~program ~prior ~state =
  match C_pbin.dec program, C_sbin.dec_state state with
  | Some _, Some value when String.length prior = 32 ->
      let domain, _ = C_sess.view value in
      if String.equal domain.C_sess.prog (image program)
        && String.equal domain.C_sess.root prior
      then Some (C_sha.hash (frame program prior state))
      else None
  | _, _ -> None