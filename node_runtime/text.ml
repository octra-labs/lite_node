(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let addr_short s =
  if String.length s > 12 then String.sub s 0 12 ^ ".." else s

let prefix_or_unknown n s =
  try String.sub s 0 n with _ -> "?"

let hash_short s =
  prefix_or_unknown 12 s

let peer_short s =
  prefix_or_unknown 12 s

let addr14 s =
  prefix_or_unknown 14 s

let root_short root =
  if String.length root >= 16 then
    String.sub root 0 8 ^ ".." ^ String.sub root (String.length root - 8) 8
  else root

let msg_hex t =
  Printf.sprintf "0x%02x" t

let frame_len frame =
  String.length frame.Octra_net.P2p_frame.payload

let is_hex_string s =
  String.length s mod 2 = 0
  && String.for_all
       (function
         | '0'..'9'
         | 'a'..'f'
         | 'A'..'F' -> true
         | _ -> false)
       s

let is_hex_len n s =
  String.length s = n && is_hex_string s

let hex_to_string hex =
  let rec aux i acc =
    if i >= String.length hex then
      String.concat "" (List.rev acc)
    else if i + 1 < String.length hex then
      let byte = "0x" ^ String.sub hex i 2 in
      try
        let c = Char.chr (int_of_string byte) in
        aux (i + 2) (String.make 1 c :: acc)
      with _ -> aux (i + 2) acc
    else
      String.concat "" (List.rev acc)
  in
  aux 0 []

let decode_message_if_hex msg =
  if is_hex_string msg then hex_to_string msg else msg

let raw_to_hex s =
  String.concat ""
    (List.init
       (String.length s)
       (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let hash32_hex value =
  if String.length value = 32 then raw_to_hex value else value

let hex_to_raw32_lossy hex =
  let raw =
    if String.length hex mod 2 <> 0 then ""
    else
      let len = String.length hex / 2 in
      String.init len (fun i ->
        Char.chr (int_of_string ("0x" ^ String.sub hex (i * 2) 2)))
  in
  if String.length raw = 32 then raw
  else if String.length raw > 32 then String.sub raw 0 32
  else raw ^ String.make (32 - String.length raw) '\x00'