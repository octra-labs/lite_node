(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type error =
  | Compressed
  | Extra
  | Incomplete
  | Too_large

let encode payload =
  let length = String.length payload in
  let frame = Bytes.create (length + 5) in
  Bytes.set frame 0 '\000';
  Bytes.set frame 1 (Char.chr ((length lsr 24) land 0xff));
  Bytes.set frame 2 (Char.chr ((length lsr 16) land 0xff));
  Bytes.set frame 3 (Char.chr ((length lsr 8) land 0xff));
  Bytes.set frame 4 (Char.chr (length land 0xff));
  Bytes.blit_string payload 0 frame 5 length;
  Bytes.unsafe_to_string frame

let byte input index =
  Char.code input.[index]

let length input =
  Int64.logor
    (Int64.shift_left (Int64.of_int (byte input 1)) 24)
    (Int64.logor
       (Int64.shift_left (Int64.of_int (byte input 2)) 16)
       (Int64.logor
          (Int64.shift_left (Int64.of_int (byte input 3)) 8)
          (Int64.of_int (byte input 4))))

let decode ~max_message input =
  if String.length input < 5 then Error Incomplete
  else if byte input 0 <> 0 then Error Compressed
  else
    let encoded = length input in
    if Int64.compare encoded (Int64.of_int max_message) > 0 then Error Too_large
    else
      let payload_length = Int64.to_int encoded in
      let frame_length = payload_length + 5 in
      if String.length input < frame_length then Error Incomplete
      else if String.length input > frame_length then Error Extra
      else Ok (Bytes.of_string (String.sub input 5 payload_length))