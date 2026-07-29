(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Infix

let fail msg =
  prerr_endline msg;
  exit 1

let arg_value name args =
  let rec loop = function
    | [] -> None
    | key :: value :: _ when key = name -> Some value
    | _ :: rest -> loop rest
  in
  loop args

let require_arg name args =
  match arg_value name args with
  | Some value -> value
  | None -> fail ("missing " ^ name)

let json_string key = function
  | `Assoc fields ->
    begin
      match List.assoc_opt key fields with
      | Some (`String value) -> value
      | _ -> fail ("missing " ^ key)
    end
  | _ -> fail "expected object"

let json_int key = function
  | `Assoc fields ->
    begin
      match List.assoc_opt key fields with
      | Some (`Int value) -> value
      | _ -> fail ("missing " ^ key)
    end
  | _ -> fail "expected object"

let rpc ~url ~method_name ~params =
  let body =
    `Assoc [
      "jsonrpc", `String "2.0";
      "id", `Int (int_of_float (Unix.gettimeofday () *. 1000.) mod 1_000_000);
      "method", `String method_name;
      "params", `List params;
    ]
    |> Yojson.Safe.to_string
    |> Cohttp_lwt.Body.of_string
  in
  let headers =
    Cohttp.Header.init ()
    |> fun value -> Cohttp.Header.add value "content-type" "application/json"
    |> fun value -> Cohttp.Header.add value "user-agent" "octra-validator-control/1"
  in
  Cohttp_lwt_unix.Client.post
    ~headers
    ~body
    (Uri.of_string url)
  >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >|= fun raw ->
  let json =
    try Yojson.Safe.from_string raw
    with _ -> fail ("invalid rpc json: " ^ raw)
  in
  match json with
  | `Assoc fields ->
    begin
      match List.assoc_opt "error" fields with
      | Some error -> fail (Yojson.Safe.to_string error)
      | None ->
        begin
          match List.assoc_opt "result" fields with
          | Some result -> result
          | None -> fail ("missing rpc result: " ^ raw)
        end
    end
  | _ -> fail ("invalid rpc response: " ^ raw)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      really_input_string ic (in_channel_length ic))

let next_nonce ~rpc_url ~addr =
  rpc
    ~url:rpc_url
    ~method_name:"octra_account"
    ~params:[`String addr; `Int 1]
  >|= fun account ->
  json_int "nonce" account + 1

let nonce ~rpc_url ~addr args =
  match arg_value "--nonce" args with
  | None -> next_nonce ~rpc_url ~addr
  | Some raw ->
    begin
      try
        let value = int_of_string raw in
        if value < 0 then fail "nonce must be nonnegative";
        Lwt.return value
      with Failure _ ->
        fail "invalid --nonce"
    end

let submit ~rpc_url tx =
  rpc
    ~url:rpc_url
    ~method_name:"octra_submit"
    ~params:[Octra_core.Transaction.to_yojson tx]

let validator_bond_payload ~chain_id ~address ~amount ~nonce ~pub ~priv =
  let pubkey =
    try Base64.decode_exn pub
    with _ -> fail "invalid wallet public key"
  in
  let key =
    match Mirage_crypto_ec.Ed25519.priv_of_octets (Base64.decode_exn priv) with
    | Ok value -> value
    | Error _ -> fail "invalid wallet private key"
  in
  let message =
    Octra_core.Validator_registry.bond_message
      ~chain_id
      ~address
      ~amount
      ~nonce
      ~pubkey
  in
  let proof =
    Mirage_crypto_ec.Ed25519.sign ~key message
    |> Base64.encode_exn
  in
  `Assoc [
    "consensus_pubkey", `String pub;
    "proof", `String proof;
  ]
  |> Yojson.Safe.to_string

let main () =
  Mirage_crypto_rng_unix.use_default ();
  let args = Array.to_list Sys.argv |> List.tl in
  let wallet_path = require_arg "--wallet" args in
  let rpc_url = require_arg "--rpc" args in
  let op_raw = require_arg "--op" args in
  let print_only = List.mem "--print-only" args in
  let message =
    match arg_value "--message-file" args, arg_value "--message" args with
    | Some path, _ -> Some (read_file path)
    | None, Some value -> Some value
    | None, None -> None
  in
  let wallet = Yojson.Safe.from_file wallet_path in
  let from = json_string "address" wallet in
  let pub = json_string "pub" wallet in
  let priv = json_string "priv" wallet in
  let amount = Z.of_string (match arg_value "--amount" args with Some value -> value | None -> "0") in
  let op_type =
    match Octra_core.Transaction.op_type_of_string op_raw with
    | Ok op -> op
    | Error e -> fail e
  in
  let to_ =
    match arg_value "--to" args, op_type with
    | Some value, _ -> value
    | None, Octra_core.Transaction.ValidatorBond ->
      Octra_core.Validator_registry.escrow_address
    | None, _ -> from
  in
  let message_required =
    match op_type with
    | Octra_core.Transaction.ValidatorSetUpdate
    | Octra_core.Transaction.ValidatorReady
    | Octra_core.Transaction.ValidatorEvidence -> true
    | _ -> false
  in
  let has_message =
    match message with
    | Some value -> String.trim value <> ""
    | None -> false
  in
  if message_required && not has_message then
    fail (op_raw ^ " requires non-empty --message or --message-file");
  Lwt_main.run (
    nonce ~rpc_url ~addr:from args >>= fun nonce ->
    let message =
      match op_type, message with
      | Octra_core.Transaction.ValidatorBond, None ->
        Some
          (validator_bond_payload
             ~chain_id:(require_arg "--chain-id" args)
             ~address:from
             ~amount
             ~nonce
             ~pub
             ~priv)
      | _ -> message
    in
    let draft = Octra_core.Transaction.{
      from;
      to_;
      amount;
      nonce;
      ou = Z.zero;
      timestamp = Unix.gettimeofday ();
      signature = "";
      public_key = Some pub;
      message;
      op_type;
      encrypted_data = None;
    } in
    let ou =
      match arg_value "--ou" args with
      | Some value -> Z.of_string value
      | None -> Octra_core.Transaction.ou_cost draft
    in
    let tx = { draft with Octra_core.Transaction.ou } in
    let signed = Octra_core.Transaction.sign_with_privkey tx priv in
    if print_only then begin
      print_endline (Yojson.Safe.to_string (Octra_core.Transaction.to_yojson signed));
      Lwt.return_unit
    end else
      submit ~rpc_url signed >|= fun result ->
      print_endline (Yojson.Safe.to_string result)
  )

let () = main ()