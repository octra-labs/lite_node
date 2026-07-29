(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module P = Private_ledger
module T = Transaction

type limits = {
  max_fhe : int;
  max_stealth : int;
}

type t = {
  ledger : Ledger.t;
  epoch_id : int;
  result_policy : Private_result_policy.t;
  limits : limits;
  preverify : Preverify_commit.t option;
  legacy_replay :
    epoch:int ->
    address:string ->
    cipher:string ->
    Pvac_legacy_public_replay.decision;
  mutable fhe : int;
  mutable stealth : int;
  debits : (string, unit) Hashtbl.t;
}

type verification =
  | Verify_proof
  | Apply_receipt of string

let create
    ~preverify
    ~legacy_replay
    ~ledger
    ~epoch_id
    ~result_policy
    ~limits =
  {
    ledger;
    epoch_id;
    result_policy;
    limits;
    preverify;
    legacy_replay;
    fhe = 0;
    stealth = 0;
    debits = Hashtbl.create 32;
  }

let failure e =
  Error (e.P.tag, e.reason)

let cap tag count max =
  if count >= max then
    Error (tag, "private operation cap reached")
  else
    Ok ()

let self tx =
  if String.equal tx.T.from tx.to_ then
    Ok ()
  else
    Error ("private_target_rejected", "private balance operation must be self-targeted")

let bound t addr =
  let open Lwt.Syntax in
  let* value = Ledger.pvac_key_is_bound t.ledger addr in
  if value then
    Lwt.return_ok ()
  else
    Lwt.return_error
      ("pvac_key_unbound", "pvac public key is not bound to consensus state")

let debit_open t addr =
  if Hashtbl.mem t.debits addr then
    Error
      ("encrypted_op_limit",
       "only one encrypted debit operation per sender per epoch")
  else
    Ok ()

let mark_fhe t =
  t.fhe <- t.fhe + 1

let mark_debit t addr =
  Hashtbl.replace t.debits addr ()

let verification t tx =
  match t.preverify with
  | None -> Ok Verify_proof
  | Some gate ->
    begin
      match Preverify_commit.receipt_for_tx gate tx with
      | Error e -> Error ("preverify_receipt_missing", e)
      | Ok receipt ->
        begin
          match receipt.Preverify_receipt.state with
          | Some { transition_hash = Some hash; _ } ->
            Ok (Apply_receipt hash)
          | Some { transition_hash = None; _ } ->
            Ok Verify_proof
          | None ->
            Error
              ("preverify_transition_missing",
               "private transition receipt is not state-bound")
        end
    end

let prepared expected prepared =
  let actual = P.hash_prepared prepared in
  if String.equal actual expected then
    Ok ()
  else
    Error
      ("preverify_transition_mismatch",
       "private transition does not match the certified receipt")

let encrypt t tx =
  let open Lwt.Syntax in
  match self tx, cap "fhe_epoch_cap" t.fhe t.limits.max_fhe with
  | Error e, _
  | _, Error e -> Lwt.return_error e
  | Ok (), Ok () ->
    let* key = bound t tx.T.from in
    begin
      match key with
      | Error e -> Lwt.return_error e
      | Ok () ->
        let* plan =
          match verification t tx with
          | Error e -> Lwt.return_error e
          | Ok Verify_proof ->
            let* result =
              P.encrypt_plan ~result_policy:t.result_policy t.ledger tx
            in
            Lwt.return (Result.map_error (fun e -> e.P.tag, e.reason) result)
          | Ok (Apply_receipt expected) ->
            let* result =
              P.prepare_encrypt_plan ~result_policy:t.result_policy t.ledger tx
            in
            begin
              match result with
              | Error e -> Lwt.return_error (e.P.tag, e.reason)
              | Ok plan ->
                match prepared expected (P.Prepared_encrypt plan) with
                | Error e -> Lwt.return_error e
                | Ok () -> Lwt.return_ok plan
            end
        in
        let* applied =
          match plan with
          | Error _ as result -> Lwt.return result
          | Ok plan ->
            let* result = P.apply_encrypt_plan t.ledger tx plan in
            Lwt.return (Result.map_error (fun e -> e.P.tag, e.reason) result)
        in
        begin
          match applied with
          | Error e -> Lwt.return_error e
          | Ok _ ->
            mark_fhe t;
            Lwt.return_ok tx.T.ou
        end
    end

let decrypt t tx =
  let open Lwt.Syntax in
  match self tx,
        cap "fhe_epoch_cap" t.fhe t.limits.max_fhe,
        debit_open t tx.T.from with
  | Error e, _, _
  | _, Error e, _
  | _, _, Error e -> Lwt.return_error e
  | Ok (), Ok (), Ok () ->
    let* key = bound t tx.T.from in
    begin
      match key with
      | Error e -> Lwt.return_error e
      | Ok () ->
        let* plan =
          match verification t tx with
          | Error e -> Lwt.return_error e
          | Ok Verify_proof ->
            let* result =
              P.decrypt_plan ~result_policy:t.result_policy t.ledger tx
            in
            Lwt.return (Result.map_error (fun e -> e.P.tag, e.reason) result)
          | Ok (Apply_receipt expected) ->
            let* result =
              P.prepare_decrypt_plan ~result_policy:t.result_policy t.ledger tx
            in
            begin
              match result with
              | Error e -> Lwt.return_error (e.P.tag, e.reason)
              | Ok plan ->
                match prepared expected (P.Prepared_decrypt plan) with
                | Error e -> Lwt.return_error e
                | Ok () -> Lwt.return_ok plan
            end
        in
        let* applied =
          match plan with
          | Error _ as result -> Lwt.return result
          | Ok plan ->
            let* result = P.apply_decrypt_plan t.ledger tx plan in
            Lwt.return (Result.map_error (fun e -> e.P.tag, e.reason) result)
        in
        begin
          match applied with
          | Error e -> Lwt.return_error e
          | Ok _ ->
            mark_fhe t;
            mark_debit t tx.T.from;
            Lwt.return_ok tx.T.ou
        end
    end

let key_switch t tx =
  let open Lwt.Syntax in
  match self tx, cap "fhe_epoch_cap" t.fhe t.limits.max_fhe with
  | Error e, _
  | _, Error e -> Lwt.return_error e
  | Ok (), Ok () ->
    let replay =
      if P.key_switch_requests_legacy_audit tx then
        let cipher =
          match Ledger.find_opt t.ledger tx.T.from with
          | Some account ->
            Option.value ~default:"0" account.Ledger_types.encrypted_balance
          | None -> "0"
        in
        Some
          (t.legacy_replay
             ~epoch:t.epoch_id
             ~address:tx.from
             ~cipher)
      else
        None
    in
    let* plan =
      match verification t tx with
      | Error e -> Lwt.return_error e
      | Ok Verify_proof ->
        let* result =
          P.key_switch_plan ?legacy_public_replay:replay t.ledger tx
        in
        Lwt.return (Result.map_error (fun e -> e.P.tag, e.reason) result)
      | Ok (Apply_receipt expected) ->
        let* result = P.prepare_key_switch_plan t.ledger tx in
        begin
          match result with
          | Error e -> Lwt.return_error (e.P.tag, e.reason)
          | Ok plan ->
            match prepared expected (P.Prepared_key_switch plan) with
            | Error e -> Lwt.return_error e
            | Ok () -> Lwt.return_ok plan
        end
    in
    let* applied =
      match plan with
      | Error (tag, reason) ->
        Lwt.return
          (P.Key_switch_rejected {
             failure = { P.tag; reason; user_reason = reason };
             consume_nonce = false;
           })
      | Ok plan -> P.apply_key_switch_plan t.ledger tx plan
    in
    begin
      match applied with
      | P.Key_switch_rejected rejected ->
        Lwt.return (failure rejected.failure)
      | P.Key_switch_applied _ ->
        mark_fhe t;
        Lwt.return_ok tx.T.ou
    end

let verified_stealth_plan t tx =
  let open Lwt.Syntax in
  match verification t tx with
  | Error e -> Lwt.return_error e
  | Ok (Apply_receipt expected) ->
    let* result =
      P.prepare_stealth_plan ~result_policy:t.result_policy t.ledger tx
    in
    begin
      match result with
      | Error e -> Lwt.return_error (e.P.tag, e.reason)
      | Ok plan ->
        begin
          match prepared expected (P.Prepared_stealth plan) with
          | Error e -> Lwt.return_error e
          | Ok () -> Lwt.return_ok plan
        end
    end
  | Ok Verify_proof ->
    let* result =
      P.stealth_plan ~result_policy:t.result_policy t.ledger tx
    in
    begin
      match result with
      | Error e -> Lwt.return_error (e.P.tag, e.reason)
      | Ok plan ->
        let* range = P.stealth_inline_range t.ledger tx plan in
        begin
          match range with
          | Error e -> Lwt.return_error (e.P.tag, e.reason)
          | Ok range ->
            begin
              match P.stealth_accept_range range with
              | Error e -> Lwt.return_error (e.P.tag, e.reason)
              | Ok () ->
                let* binding = P.stealth_binding t.ledger tx plan in
                Lwt.return
                  (Result.map_error
                     (fun e -> e.P.tag, e.reason)
                     binding
                  |> Result.map (fun () -> plan))
            end
        end
    end

let stealth t tx =
  let open Lwt.Syntax in
  match cap "fhe_epoch_cap" t.fhe t.limits.max_fhe,
        cap "stealth_epoch_cap" t.stealth t.limits.max_stealth,
        debit_open t tx.T.from with
  | Error e, _, _
  | _, Error e, _
  | _, _, Error e -> Lwt.return_error e
  | Ok (), Ok (), Ok () when not (String.equal tx.T.to_ "stealth") ->
    Lwt.return_error
      ("invalid_stealth_target", "stealth transfer must target stealth")
  | Ok (), Ok (), Ok () ->
    let* key = bound t tx.T.from in
    begin
      match key with
      | Error e -> Lwt.return_error e
      | Ok () ->
        let* plan = verified_stealth_plan t tx in
        begin
          match plan with
          | Error e -> Lwt.return_error e
          | Ok plan ->
            begin
              match Ledger.debit t.ledger tx.T.from tx.ou tx.nonce with
              | Error e -> Lwt.return_error ("insufficient_balance", e)
              | Ok () ->
                begin
                  match
                    Ledger.update_enc_balance
                      t.ledger
                      tx.from
                      plan.stealth_next_cipher
                  with
                  | Error e ->
                    Lwt.fail_with
                      ("stealth balance commit failed: " ^ e)
                  | Ok () ->
                    let* output =
                      Ledger.create_stealth_output
                        t.ledger
                        ~stealth_tag:plan.stealth_tag
                        ~eph_pub:plan.stealth_eph_pub
                        ~enc_amount:plan.stealth_enc_amount
                        ~amount:Z.zero
                        ~epoch_id:t.epoch_id
                        ~tx_hash:(T.hash tx)
                        ~sender_addr:tx.from
                        ~claim_pub:plan.stealth_claim_pub
                        ~delta_cipher_stored:plan.stealth_delta_cipher
                        ~amount_hash:P.key_bound_stealth_output_marker
                        ~amount_commitment:plan.stealth_amount_commitment
                        ()
                    in
                    begin
                      match output with
                      | Error e ->
                        Lwt.fail_with
                          ("stealth output commit failed: " ^ e)
                      | Ok _ ->
                        mark_fhe t;
                        t.stealth <- t.stealth + 1;
                        mark_debit t tx.from;
                        Lwt.return_ok tx.ou
                    end
                end
            end
        end
    end

let verified_claim_plan t tx =
  let open Lwt.Syntax in
  match verification t tx with
  | Error e -> Lwt.return_error e
  | Ok (Apply_receipt expected) ->
    let* claim = P.prepare_claim_plan t.ledger tx in
    begin
      match claim with
      | Error e -> Lwt.return_error (e.P.tag, e.reason)
      | Ok claim ->
        let* balance =
          P.claim_balance_plan ~result_policy:t.result_policy t.ledger tx claim
        in
        begin
          match balance with
          | Error e -> Lwt.return_error (e.P.tag, e.reason)
          | Ok balance ->
            begin
              match
                prepared expected (P.Prepared_claim (claim, balance))
              with
              | Error e -> Lwt.return_error e
              | Ok () -> Lwt.return_ok (claim, balance)
            end
        end
    end
  | Ok Verify_proof ->
    let* claim = P.claim_plan t.ledger tx in
    begin
      match claim with
      | Error e -> Lwt.return_error (e.P.tag, e.reason)
      | Ok claim ->
        let* balance =
          P.claim_balance_plan ~result_policy:t.result_policy t.ledger tx claim
        in
        Lwt.return
          (Result.map_error
             (fun e -> e.P.tag, e.reason)
             balance
          |> Result.map (fun balance -> claim, balance))
    end

let claim t tx =
  let open Lwt.Syntax in
  match self tx, cap "fhe_epoch_cap" t.fhe t.limits.max_fhe with
  | Error e, _
  | _, Error e -> Lwt.return_error e
  | Ok (), Ok () ->
    let* key = bound t tx.T.from in
    begin
      match key with
      | Error e -> Lwt.return_error e
      | Ok () ->
        let* plans = verified_claim_plan t tx in
        begin
          match plans with
          | Error e -> Lwt.return_error e
          | Ok (claim, balance) ->
            begin
              match Ledger.debit t.ledger tx.from tx.ou tx.nonce with
              | Error e -> Lwt.return_error ("insufficient_balance", e)
              | Ok () ->
                begin
                  match
                    Ledger.update_enc_balance
                      t.ledger
                      tx.from
                      balance.next_cipher
                  with
                  | Error e ->
                    Lwt.fail_with ("claim balance commit failed: " ^ e)
                  | Ok () ->
                    let* marked =
                      Ledger.mark_stealth_claimed
                        t.ledger
                        claim.claim_output_id
                        (T.hash tx)
                    in
                    begin
                      match marked with
                      | Error e ->
                        Lwt.fail_with
                          ("claim output commit failed: " ^ e)
                      | Ok () ->
                        mark_fhe t;
                        Lwt.return_ok tx.ou
                    end
                end
            end
        end
    end

let process t ~backend ~env tx =
  match tx.T.op_type with
  | T.EncryptOp -> encrypt t tx
  | T.DecryptOp -> decrypt t tx
  | T.StealthOp -> stealth t tx
  | T.ClaimOp -> claim t tx
  | T.KeySwitch -> key_switch t tx
  | T.RecryptOp ->
    Lwt.return_error
      ("recrypt_disabled", "recrypt transaction profile is not released")
  | T.PrivateOp ->
    Lwt.return_error
      ("private_disabled", "legacy private operation is disabled")
  | _ -> Epoch_exec.process_standard_tx ~backend ~env tx