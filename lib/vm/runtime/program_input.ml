(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type error =
  | Invalid_count
  | Invalid_value of int
  | Unsupported_kind of Program_type_flow.kind

let error_message = function
  | Invalid_count -> "program parameter count mismatch"
  | Invalid_value index -> Printf.sprintf "invalid program parameter at index %d" index
  | Unsupported_kind kind ->
    Printf.sprintf "unsupported program parameter kind: %s"
      (Program_type_flow.kind_name kind)

let max_u64 = Z.of_string "18446744073709551615"
let max_u128 = Z.sub (Z.shift_left Z.one 128) Z.one
let max_u256 = Z.sub (Z.shift_left Z.one 256) Z.one

let bounded max make value =
  if Z.sign value >= 0 && Z.compare value max <= 0 then Some (make value)
  else None

let valid_addr value =
  let alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" in
  String.length value = 47
  && String.sub value 0 3 = "oct"
  && String.for_all (fun ch -> String.contains alphabet ch) (String.sub value 3 44)

let json_z = function
  | `Int value -> Some (Z.of_int value)
  | `Intlit value -> (try Some (Z.of_string value) with _ -> None)
  | _ -> None

let raw_bytes32 value =
  if String.length value = 32 then Some value
  else
    match Base64.decode value with
    | Ok raw when String.length raw = 32 -> Some raw
    | _ -> None

let value kind json =
  match kind, json with
  | Program_type_flow.Int, (`Int _ | `Intlit _) ->
    Option.map (fun value -> Contract_vm.VInt value) (json_z json)
  | Program_type_flow.Bool, `Bool value -> Some (Contract_vm.VBool value)
  | Program_type_flow.String, `String value -> Some (Contract_vm.VString value)
  | Program_type_flow.Bytes, `String value -> Some (Contract_vm.VBytes value)
  | Program_type_flow.Bytes32, `String value ->
    Option.map (fun raw -> Contract_vm.VBytes32 raw) (raw_bytes32 value)
  | Program_type_flow.U64, (`Int _ | `Intlit _) ->
    Option.map (fun v -> Contract_vm.VU64 v) (Option.bind (json_z json) (bounded max_u64 Fun.id))
  | Program_type_flow.U128, (`Int _ | `Intlit _) ->
    Option.map (fun v -> Contract_vm.VU128 v) (Option.bind (json_z json) (bounded max_u128 Fun.id))
  | Program_type_flow.U256, (`Int _ | `Intlit _) ->
    Option.map (fun v -> Contract_vm.VU256 v) (Option.bind (json_z json) (bounded max_u256 Fun.id))
  | Program_type_flow.Addr, `String value when valid_addr value ->
    Some (Contract_vm.VAddr value)
  | Program_type_flow.Cipher, _
  | Program_type_flow.PubKey, _
  | Program_type_flow.Unknown, _ -> None
  | _ -> None

let one kind json =
  value kind json

let kind_of_value = function
  | Contract_vm.VInt _ -> Some Program_type_flow.Int
  | Contract_vm.VBool _ -> Some Program_type_flow.Bool
  | Contract_vm.VString _ -> Some Program_type_flow.String
  | Contract_vm.VBytes _ -> Some Program_type_flow.Bytes
  | Contract_vm.VBytes32 _ -> Some Program_type_flow.Bytes32
  | Contract_vm.VU64 _ -> Some Program_type_flow.U64
  | Contract_vm.VU128 _ -> Some Program_type_flow.U128
  | Contract_vm.VU256 _ -> Some Program_type_flow.U256
  | Contract_vm.VAddr _ -> Some Program_type_flow.Addr
  | Contract_vm.VCipher _ -> Some Program_type_flow.Cipher
  | Contract_vm.VPubKey _ -> Some Program_type_flow.PubKey

let validate kinds values =
  if List.length kinds <> List.length values then Error Invalid_count
  else
    let rec loop index kinds values =
      match kinds, values with
      | [], [] -> Ok ()
      | kind :: kinds, value :: values ->
        (match kind_of_value value with
         | Some actual when actual = kind -> loop (index + 1) kinds values
         | _ -> Error (Invalid_value index))
      | _ -> Error Invalid_count
    in
    loop 0 kinds values

let parse kinds jsons =
  if List.length kinds <> List.length jsons then Error Invalid_count
  else
    let rec loop index acc kinds jsons =
      match kinds, jsons with
      | [], [] -> Ok (List.rev acc)
      | kind :: kinds, json :: jsons ->
        (match one kind json with
         | Some value -> loop (index + 1) (value :: acc) kinds jsons
         | None ->
           (match kind with
            | Program_type_flow.Cipher
            | Program_type_flow.PubKey
            | Program_type_flow.Unknown -> Error (Unsupported_kind kind)
            | _ -> Error (Invalid_value index)))
      | _ -> Error Invalid_count
    in
    loop 0 [] kinds jsons