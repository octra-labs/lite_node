(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type observer_event =
  | Silent
  | Ignored of {
      epoch : int;
      tx_count : int;
      root : string;
      producer : string;
    }

let root_short root =
  if String.length root >= 16 then
    String.sub root 0 8 ^ ".." ^ String.sub root (String.length root - 8) 8
  else root

let observer_event ~observer payload =
  if not observer then Ok Silent
  else try
    let c = Octra_net.Oce1.make_cursor payload in
    let text () =
      let length = Octra_net.Oce1.get_u32_int c in
      if length < 0 || length > 256 then failwith "epoch header field exceeds limit";
      Octra_net.Oce1.get_raw c length
    in
    let epoch = Octra_net.Oce1.get_u64 c in
    let root = text () in
    let _ = Octra_net.Oce1.get_u32 c in
    let producer = text () in
    let count = Octra_net.Oce1.get_u32 c in
    if epoch < 0L || epoch > Int64.of_int max_int || count < 0l then
      failwith "epoch header count is invalid";
    Ok (Ignored {
      epoch = Int64.to_int epoch;
      tx_count = Int32.to_int count;
      root = root_short root;
      producer = String.sub producer 0 (min 14 (String.length producer));
    })
  with
  | Out_of_memory | Stack_overflow as error -> raise error
  | _ -> Error "epoch_header_decode"