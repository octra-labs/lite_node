(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let read value =
  match Octra_core.Transaction.of_yojson value with
  | Ok tx -> Octra_core.Transaction.hash tx
  | Error message -> failwith message

let () =
  match Array.to_list Sys.argv with
  | [_; ("--hash" | "--signed" | "--signing" as mode)] ->
    let value = Yojson.Safe.from_channel stdin in
    if mode = "--signed" then begin
      match Octra_core.Transaction.of_yojson value with
      | Ok tx when Option.fold ~none:false
          ~some:(Octra_core.Transaction.verify tx) tx.public_key -> ()
      | _ -> failwith "transaction signature differs"
    end;
    if mode = "--signing" then begin
      match Octra_core.Transaction.of_yojson value with
      | Ok tx -> print_endline (Octra_core.Transaction.serialize_for_signing tx)
      | Error error -> failwith error
    end else read value |> print_endline
  | [_; file] ->
    let open Yojson.Safe.Util in
    let values = Yojson.Safe.from_file file |> to_list in
    List.iter (fun value ->
      let actual = value |> member "tx" |> read in
      let expected = value |> member "hash" |> to_string in
      if actual <> expected then
        failwith (value |> member "name" |> to_string);
      match Octra_core.Transaction.of_yojson (member "tx" value) with
      | Ok tx ->
        let expected = value |> member "signing" |> to_string in
        if Octra_core.Transaction.serialize_for_signing tx <> expected then
          failwith ("signing: " ^ (value |> member "name" |> to_string))
      | Error error -> failwith error) values;
    Printf.printf "status = pass test = tx_hash count = %d\n" (List.length values)
  | _ -> failwith "expected vector path or --hash"