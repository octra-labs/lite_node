(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


module FB = Crypto.FheBalance
module PT = Crypto.PrivateTransferV4
module SC = Crypto.StealthClaimV5
module SA = Crypto.StealthAddress
module R = Preverify_receipt
module Q = Preverify_queue
module T = Transaction

type ready = {
  tx : T.t;
  receipt : R.t option;
}

type skip = {
  tx : T.t;
  reason : string;
}

type batch = {
  ready : ready list;
  skipped : skip list;
}

type verdict =
  | Ready of R.t
  | Skip of string

let hex_hash tag payload =
  Digestif.SHA256.digest_string (tag ^ "\000" ^ payload)
  |> Digestif.SHA256.to_hex

let tx_json tx =
  T.to_yojson tx |> Yojson.Safe.to_string

let input_hash tx =
  hex_hash "octra:preverify_input:v1" (tx_json tx)

let output_hash tx status =
  hex_hash "octra:preverify_output:v1" (T.hash tx ^ "\000" ^ status)

let receipt tx =
  match R.for_tx
    ~input_hash:(input_hash tx)
    ~output_hash:(output_hash tx "ok")
    ~ok:true
    ~reason:""
    tx with
  | Ok r -> Ready r
  | Error e -> Skip e

let json_of_receipts receipts =
  List.map (fun r -> R.canonical r) receipts

let has_hash hashes hash =
  List.exists (String.equal hash) hashes

let receipts_for_hashes (ready : ready list) hashes =
  ready
  |> List.filter_map (fun item ->
    match item.receipt with
    | Some r when has_hash hashes r.R.tx_hash -> Some r
    | Some _ | None -> None)

let receipt_json_for_hashes ready hashes =
  receipts_for_hashes ready hashes
  |> json_of_receipts

let txs (batch : batch) =
  List.map (fun (item : ready) -> item.tx) batch.ready

let missing name =
  Error (name ^ "_missing")

let field name json =
  match Yojson.Safe.Util.(member name json |> to_string_option) with
  | Some s -> Ok s
  | None -> missing name

let int_field name json =
  match Yojson.Safe.Util.(member name json |> to_int_option) with
  | Some n -> Ok n
  | None -> missing name

let enc_json tx =
  match tx.T.encrypted_data with
  | Some raw when raw <> "" ->
    begin
      try Ok (Yojson.Safe.from_string raw)
      with _ -> Error "encrypted_data_json"
    end
  | Some _ | None -> Error "encrypted_data_missing"

let pkt tx =
  match enc_json tx with
  | Error e -> Error e
  | Ok json ->
    match field "cipher" json,
          field "amount_commitment" json,
          field "zero_proof" json,
          field "blinding" json with
    | Ok cipher, Ok com, Ok proof, Ok blind ->
      let rp = Yojson.Safe.Util.(member "range_proof_balance" json |> to_string_option) in
      Ok (cipher, com, proof, blind, rp)
    | Error e, _, _, _
    | _, Error e, _, _
    | _, _, Error e, _
    | _, _, _, Error e -> Error e

let sender_enc ledger addr =
  match Ledger.find_opt ledger addr with
  | Some acc -> Ok (Option.value ~default:"0" acc.Ledger.encrypted_balance)
  | None -> Error "sender_missing"

let with_pk ledger addr f =
  let open Lwt.Syntax in
  let* pk_opt = Ledger.get_pvac_pubkey ledger addr in
  match pk_opt with
  | None -> Lwt.return (Error "no_pvac_pubkey")
  | Some blob ->
    Lwt_preemptive.detach (fun () ->
      match FB.load_pubkey_result blob with
      | Error e -> Error e
      | Ok pk -> f pk)
      ()

let verify_encrypt ledger tx =
  match pkt tx with
  | Error e -> Lwt.return (Error e)
  | Ok (cipher, com, proof, blind, _) ->
    with_pk ledger tx.T.from (fun pk ->
      FB.verify_encrypt_proof pk cipher tx.T.amount proof com blind)

let verify_decrypt ledger tx =
  match pkt tx with
  | Error e -> Lwt.return (Error e)
  | Ok (cipher, com, proof, blind, rp_opt) ->
    match sender_enc ledger tx.T.from with
    | Error e -> Lwt.return (Error e)
    | Ok current ->
      with_pk ledger tx.T.from (fun pk ->
        match FB.verify_encrypt_proof pk cipher tx.T.amount proof com blind with
        | Error e -> Error e
        | Ok () ->
          match rp_opt with
          | None -> Error "range_proof_balance_missing"
          | Some rp ->
            match FB.decode_cipher cipher with
            | Error e -> Error ("bad_delta_cipher:" ^ e)
            | Ok delta ->
              match FB.withdraw_with_pubkey pk ~current_cipher:(Some current) ~delta_cipher:delta with
              | Error e -> Error ("encrypted_balance_update:" ^ e)
              | Ok next ->
                if FB.verify_range pk next rp then Ok ()
                else Error "range_proof_balance_failed")

let stealth_data tx =
  match enc_json tx with
  | Error e -> Error e
  | Ok json ->
    match int_field "version" json with
    | Ok 5 -> PT.of_json json
    | Ok _ -> Error "stealth_version"
    | Error e -> Error e

let verify_stealth ledger tx =
  match stealth_data tx with
  | Error e -> Lwt.return (Error e)
  | Ok ptd ->
    if String.length ptd.PT.claim_pub <> 64 then Lwt.return (Error "claim_pub_invalid")
    else if String.length ptd.PT.send_zero_proof = 0 then Lwt.return (Error "send_zero_proof_missing")
    else
      match sender_enc ledger tx.T.from with
      | Error e -> Lwt.return (Error e)
      | Ok current ->
        with_pk ledger tx.T.from (fun pk ->
          if not (FB.verify_commitment pk ptd.PT.delta_cipher ptd.PT.commitment) then
            Error "commitment_failed"
          else
            match FB.decode_cipher ptd.PT.delta_cipher with
            | Error e -> Error ("bad_delta_cipher:" ^ e)
            | Ok delta ->
              match FB.withdraw_with_pubkey pk ~current_cipher:(Some current) ~delta_cipher:delta with
              | Error e -> Error ("encrypted_balance_update:" ^ e)
              | Ok next ->
                if not (FB.verify_range pk ptd.PT.delta_cipher ptd.PT.range_proof_delta) then
                  Error "range_proof_delta_failed"
                else if not (FB.verify_range pk next ptd.PT.range_proof_balance) then
                  Error "range_proof_balance_failed"
                else FB.verify_claim_amount_v5 pk ptd.PT.delta_cipher ptd.PT.send_zero_proof ptd.PT.amount_commitment)

let claim_data tx =
  match enc_json tx with
  | Error e -> Error e
  | Ok json ->
    match int_field "version" json with
    | Ok 5 -> SC.of_json json
    | Ok _ -> Error "claim_version"
    | Error e -> Error e

let verify_claim ledger tx =
  let open Lwt.Syntax in
  match claim_data tx with
  | Error e -> Lwt.return (Error e)
  | Ok claim ->
    let* out_opt = Ledger.get_stealth_output_by_id ledger claim.SC.output_id in
    match out_opt with
    | None -> Lwt.return (Error "output_missing")
    | Some out ->
      match Private_ledger.claim_gate tx.T.from out claim with
      | Error e -> Lwt.return (Error e.Private_ledger.tag)
      | Ok () ->
        match sender_enc ledger tx.T.from with
        | Error e -> Lwt.return (Error e)
        | Ok current ->
          with_pk ledger tx.T.from (fun pk ->
            if not (FB.verify_commitment pk claim.SC.claim_cipher claim.SC.commitment) then
              Error "commitment_failed"
            else
              match FB.verify_claim_amount_v5 pk claim.SC.claim_cipher claim.SC.zero_proof out.amount_commitment with
              | Error e -> Error e
              | Ok () ->
                match FB.decode_cipher claim.SC.claim_cipher with
                | Error e -> Error ("bad_claim_cipher:" ^ e)
                | Ok delta ->
                  match FB.deposit_with_pubkey pk ~current_cipher:(Some current) ~delta_cipher:delta with
                  | Error e -> Error ("encrypted_balance_update:" ^ e)
                  | Ok _ -> Ok ())

let verify_key_switch tx =
  match enc_json tx with
  | Error e -> Lwt.return (Error e)
  | Ok json ->
    match field "new_pubkey" json, field "aes_kat" json with
    | Ok new_pk_b64, Ok kat_hex ->
      Lwt_preemptive.detach (fun () ->
        try
          if kat_hex <> FB.aes_kat_hex () then Error "aes_kat_mismatch"
          else
            let blob = Base64.decode_exn new_pk_b64 in
            if String.length blob > 5_000_000 then Error "pubkey_too_large"
            else
              match FB.load_pubkey_result blob with
              | Error e -> Error e
              | Ok pk ->
                ignore (Pvac_ffi.serialize_pubkey pk);
                Ok ()
        with e -> Error (Printexc.to_string e))
        ()
    | Error e, _
    | _, Error e -> Lwt.return (Error e)

let run_heavy ?ledger tx =
  match tx.T.op_type, ledger with
  | T.EncryptOp, Some ledger -> verify_encrypt ledger tx
  | T.DecryptOp, Some ledger -> verify_decrypt ledger tx
  | T.StealthOp, Some ledger -> verify_stealth ledger tx
  | T.ClaimOp, Some ledger -> verify_claim ledger tx
  | T.KeySwitch, _ -> verify_key_switch tx
  | T.PrivateOp, _ -> Lwt.return (Error "private_disabled")
  | T.RecryptOp, _ -> Lwt.return (Error "recrypt_disabled")
  | T.CircleBalanceCellPut, _ -> Lwt.return (Error "circle_balance_preverify_not_ready")
  | T.CircleRegisterCellPut, _ -> Lwt.return (Error "circle_register_preverify_not_ready")
  | _, None -> Lwt.return (Error "ledger_required")
  | _ -> Lwt.return (Error "lane_not_heavy")

let run ?ledger tx =
  if not (Q.needs_preverify tx) then Lwt.return (Skip "lane_not_heavy")
  else
    let open Lwt.Syntax in
    let* v = run_heavy ?ledger tx in
    match v with
    | Error e -> Lwt.return (Skip e)
    | Ok () -> Lwt.return (receipt tx)

let run_many ?ledger txs =
  let open Lwt.Syntax in
  let rec go ready skipped = function
    | [] -> Lwt.return { ready = List.rev ready; skipped = List.rev skipped }
    | tx :: rest ->
      if not (Q.needs_preverify tx) then
        go ({ tx; receipt = None } :: ready) skipped rest
      else
        let* v = run ?ledger tx in
        match v with
        | Ready receipt -> go ({ tx; receipt = Some receipt } :: ready) skipped rest
        | Skip reason -> go ready ({ tx; reason } :: skipped) rest in
  go [] [] txs