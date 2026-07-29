(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module VM = Contract_vm
module Policy = Opcode_policy

type report = {
  instructions : int;
  hits : Policy.host_float_hit list;
}

type opcode_count = {
  opcode : string;
  count : int;
}

let of_code code =
  let hits = ref [] in
  Array.iteri
    (fun pc op ->
      match Policy.host_float_opcode op with
      | Some opcode -> hits := { Policy.pc; opcode } :: !hits
      | None -> ())
    code;
  {
    instructions = Array.length code;
    hits = List.rev !hits;
  }

let is_safe report =
  report.hits = []

let hit_count report =
  List.length report.hits

let first_hit report =
  match report.hits with
  | hit :: _ -> Some hit
  | [] -> None

let opcode_counts report =
  let counts = Hashtbl.create 8 in
  List.iter
    (fun hit ->
      let next =
        match Hashtbl.find_opt counts hit.Policy.opcode with
        | Some count -> count + 1
        | None -> 1
      in
      Hashtbl.replace counts hit.opcode next)
    report.hits;
  Hashtbl.fold
    (fun opcode count acc -> { opcode; count } :: acc)
    counts
    []
  |> List.sort (fun left right -> String.compare left.opcode right.opcode)

let of_raw raw =
  match Bytecode.decode raw with
  | Ok code -> Ok (of_code code)
  | Error err -> Error err

let of_base64 b64 =
  try of_raw (Base64.decode_exn b64) with
  | exn -> Error (Printexc.to_string exn)

let unsafe_opcodes report =
  opcode_counts report |> List.map (fun item -> item.opcode)