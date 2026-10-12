(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Protocol = Octra_consensus.Resource_compute_protocol
module Certificate = Octra_consensus.Resource_compute_certificate
module Wire = Octra_net.Oce1

type terms = {
  chain_id : string;
  payer : string;
  public_key : string;
  target : string;
  call_hash : string;
  nonce : int64;
  amount : int64;
  expires_epoch : int64;
}

type t = { terms : terms; signature : string }

let terms value = value.terms

let shape terms =
  terms.chain_id <> "" && String.length terms.chain_id <= 128
  && Crypto.Address.is_valid_address terms.payer
  && Crypto.Address.is_valid_address terms.target
  && String.length terms.public_key = 32
  && Crypto.Address.verify_address_pubkey terms.payer (Base64.encode_exn terms.public_key)
  && String.length terms.call_hash = 32
  && terms.nonce > 0L && terms.amount > 0L && terms.expires_epoch >= 0L

let put_terms buffer terms =
  Wire.put_string buffer terms.chain_id;
  Wire.put_string buffer terms.payer;
  Wire.put_hash32 buffer terms.public_key;
  Wire.put_string buffer terms.target;
  Wire.put_hash32 buffer terms.call_hash;
  Wire.put_u64 buffer terms.nonce;
  Wire.put_u64 buffer terms.amount;
  Wire.put_u64 buffer terms.expires_epoch

let sign_bytes terms =
  if not (shape terms) then invalid_arg "compute funding terms";
  Octra_net.Hash_domain.hash_encoded "octra:compute_funding" (fun buffer -> put_terms buffer terms)

let signature_valid terms signature =
  String.length signature = 64
  && Octra_consensus.C_hash.verify_ed25519 ~pubkey_raw:terms.public_key
       ~msg:(sign_bytes terms) ~signature

let prepare_header_until ~expires_epoch ~(header : Protocol.header) ~target ~nonce =
  let request = header.request in
  if expires_epoch < request.expires_epoch
      || expires_epoch > Option.value header.valid_until ~default:request.expires_epoch then
    Error "compute funding payment deadline"
  else if Result.is_error (Compute_bill.resources request) then Error "compute funding resource limits"
  else if not (Crypto.Address.is_valid_address request.caller
      && Crypto.Address.verify_address_pubkey request.caller (Base64.encode_exn header.caller_public_key)) then
    Error "compute funding payer identity"
  else if not (Octra_consensus.C_hash.verify_ed25519 ~pubkey_raw:header.caller_public_key
      ~msg:(Protocol.header_intent header) ~signature:header.caller_signature) then
    Error "compute funding call signature"
  else match request.fee_cap with
  | None -> Error "compute funding fee absent"
  | Some amount ->
    let call_hash = Octra_net.Hash_domain.hash_encoded "octra:compute_funding_call" (fun buffer ->
      Wire.put_hash32 buffer (Protocol.header_sign header);
      Wire.put_sig64 buffer header.signature) in
    let terms = {chain_id = request.chain_id; payer = request.caller;
      public_key = header.caller_public_key; target; call_hash; nonce; amount;
      expires_epoch} in
    if shape terms then Ok terms else Error "compute funding terms"

let prepare_header ~(header : Protocol.header) ~target ~nonce =
  prepare_header_until ~expires_epoch:header.request.expires_epoch ~header ~target ~nonce

let prepare_until ~expires_epoch ~call ~target ~nonce =
  match Protocol.header call with
  | Error _ -> Error "compute funding call shape"
  | Ok header -> prepare_header_until ~expires_epoch ~header ~target ~nonce

let prepare ~call ~target ~nonce =
  prepare_until ~expires_epoch:call.Protocol.request.expires_epoch ~call ~target ~nonce

let authorize terms ~signature =
  if shape terms && signature_valid terms signature then Ok {terms; signature}
  else Error "compute funding signature"

let encode value =
  Wire.encode (fun buffer ->
    Wire.put_string buffer "compute_funding";
    put_terms buffer value.terms;
    Wire.put_sig64 buffer value.signature)

let decode raw =
  if String.length raw > 1024 then Error "compute funding size"
  else try
    Wire.decode (fun cursor ->
      if Wire.get_string_bounded ~max:15 cursor <> "compute_funding" then
        invalid_arg "compute funding format";
      let chain_id = Wire.get_string_bounded ~max:128 cursor in
      let payer = Wire.get_string_bounded ~max:47 cursor in
      let public_key = Wire.get_hash32 cursor in
      let target = Wire.get_string_bounded ~max:47 cursor in
      let call_hash = Wire.get_hash32 cursor in
      let nonce = Wire.get_u64 cursor in
      let amount = Wire.get_u64 cursor in
      let expires_epoch = Wire.get_u64 cursor in
      let terms = {chain_id; payer; public_key; target; call_hash; nonce; amount; expires_epoch} in
      let signature = Wire.get_sig64 cursor in
      authorize terms ~signature) raw
  with
  | (Out_of_memory | Stack_overflow) as error -> raise error
  | _ -> Error "compute funding encoding"

let verify_header ?(extended = false) ~chain_id ~epoch ~nonce ~target ~(header : Protocol.header) value =
  if chain_id <> value.terms.chain_id then Error "compute funding chain differs"
  else if epoch < header.request.epoch_id || epoch > header.request.expires_epoch then
    Error "compute funding epoch outside window"
  else match prepare_header_until
      ~expires_epoch:(if extended then value.terms.expires_epoch else header.request.expires_epoch)
      ~header ~target ~nonce with
  | Error _ as error -> error
  | Ok expected when expected <> value.terms -> Error "compute funding scope differs"
  | Ok _ when not (signature_valid value.terms value.signature) -> Error "compute funding signature"
  | Ok _ -> Ok value.terms.amount

let verify ~chain_id ~epoch ~nonce ~target ~call value =
  match Protocol.header call with
  | Error _ -> Error "compute funding call shape"
  | Ok header -> verify_header ~chain_id ~epoch ~nonce ~target ~header value

type deposit = {
  request_id : string;
  permission : t;
  opened_epoch : int64;
  returned_epoch : int64 option;
  paid : (int64 * string) option;
}

let escrow =
  Digestif.SHA256.(to_raw_string (digest_string "octra:compute_funding:escrow"))
  |> Base64.encode_exn |> Crypto.Address.address_from_pubkey

let reserve ?(extended = false) ~chain_id ~epoch ~nonce ~header permission =
  Result.bind (verify_header ~extended ~chain_id ~epoch ~nonce ~target:escrow ~header permission)
    (fun _ ->
      if permission.terms.payer = escrow then Error "compute funding payer is escrow"
      else Ok {request_id = Certificate.request_id header.Protocol.request;
        permission; opened_epoch = epoch; returned_epoch = None; paid = None})

let return_deposit ~chain_id ~epoch deposit =
  if deposit.permission.terms.chain_id <> chain_id then Error "compute funding chain differs"
  else if Option.is_some deposit.returned_epoch then Error "compute deposit returned"
  else if Option.is_some deposit.paid then Error "compute deposit paid"
  else if epoch <= deposit.permission.terms.expires_epoch then Error "compute deposit has not expired"
  else Ok {deposit with returned_epoch = Some epoch}

let encode_request ~header permission =
  Wire.encode (fun buffer ->
    Wire.put_string buffer (Protocol.encode_header header);
    Wire.put_string buffer (encode permission))
  |> Base64.encode_exn

let decode_request raw =
  if String.length raw > 8192 then Error "compute funding message size"
  else try
    let raw = Base64.decode_exn raw in
    Wire.decode (fun cursor ->
      let header = Wire.get_string_bounded ~max:4096 cursor |> Protocol.decode_header in
      let permission = Wire.get_string_bounded ~max:1024 cursor |> decode in
      Result.bind header (fun header -> Result.map (fun permission -> header, permission) permission)) raw
  with
  | (Out_of_memory | Stack_overflow) as error -> raise error
  | _ -> Error "compute funding message encoding"

let deposit_path id =
  if String.length id <> 32 then invalid_arg "compute deposit id";
  ["compute"; "funds"; Digestif.SHA256.(to_hex (of_raw_string id))]

let encode_deposit deposit =
  Wire.encode (fun buffer ->
    Wire.put_string buffer "compute_deposit";
    Wire.put_hash32 buffer deposit.request_id;
    Wire.put_string buffer (encode deposit.permission);
    Wire.put_u64 buffer deposit.opened_epoch;
    match deposit.paid, deposit.returned_epoch with
    | Some (epoch, root), None ->
      Wire.put_u8 buffer 2;
      Wire.put_u64 buffer epoch;
      Wire.put_hash32 buffer root
    | None, returned ->
      Wire.put_u8 buffer (if Option.is_some returned then 1 else 0);
      Option.iter (Wire.put_u64 buffer) returned
    | Some _, Some _ -> invalid_arg "compute deposit closed twice")

let decode_deposit raw =
  if String.length raw > 2048 then Error "compute deposit size"
  else try
    Wire.decode (fun cursor ->
      if Wire.get_string_bounded ~max:15 cursor <> "compute_deposit" then
        invalid_arg "compute deposit format";
      let request_id = Wire.get_hash32 cursor in
      match decode (Wire.get_string_bounded ~max:1024 cursor) with
      | Error _ as error -> error
      | Ok permission ->
        let opened_epoch = Wire.get_u64 cursor in
        let returned_epoch, paid = match Wire.get_u8 cursor with
          | 0 -> None, None | 1 -> Some (Wire.get_u64 cursor), None
          | 2 ->
            let epoch = Wire.get_u64 cursor in
            let root = Wire.get_hash32 cursor in
            None, Some (epoch, root)
          | _ -> invalid_arg "compute deposit status" in
        if permission.terms.target <> escrow
            || opened_epoch < 0L || opened_epoch > permission.terms.expires_epoch then
          Error "compute deposit scope"
        else if Option.fold ~none:false
              ~some:(fun epoch -> epoch <= permission.terms.expires_epoch) returned_epoch then
            Error "compute deposit return epoch"
        else if Option.fold ~none:false ~some:(fun (epoch, _) ->
          epoch < opened_epoch || epoch > permission.terms.expires_epoch) paid then
          Error "compute deposit payment epoch"
        else Ok {request_id; permission; opened_epoch; returned_epoch; paid}) raw
  with
  | (Out_of_memory | Stack_overflow) as error -> raise error
  | _ -> Error "compute deposit encoding"

let protect action = try action () with error -> Lwt.fail error

let verify_deposit ~chain_id ~epoch ~(header : Protocol.header) raw =
  match decode_deposit raw with
  | Error _ as error -> error
  | Ok deposit ->
    let terms = deposit.permission.terms in
    if deposit.request_id <> Certificate.request_id header.request then
      Error "compute deposit request differs"
    else if Option.is_some deposit.returned_epoch then Error "compute deposit returned"
    else if Option.is_some deposit.paid then Error "compute deposit paid"
    else if epoch < deposit.opened_epoch || epoch > terms.expires_epoch then
      Error "compute deposit epoch outside window"
    else
      Result.map (fun _ -> deposit)
        (verify_header ~extended:true ~chain_id ~epoch:deposit.opened_epoch ~nonce:terms.nonce
          ~target:escrow ~header deposit.permission)

let ( let* ) work next = Lwt.bind work (fun value -> protect (fun () -> next value))

let read ?snapshot store ~id =
  if String.length id <> 32 then Lwt.return_error "compute deposit id"
  else
    let* raw = Store_irmin.read ?snapshot store (deposit_path id) in
    match raw with
    | None -> Lwt.return_ok None
    | Some raw ->
      match decode_deposit raw with
      | Error _ as error -> Lwt.return error
      | Ok deposit when deposit.request_id <> id -> Lwt.return_error "compute deposit id differs"
      | Ok deposit -> Lwt.return_ok (Some deposit)

type funded = {
  header : Protocol.header;
  deposit : deposit;
  epoch : int64;
  state_root : string;
}

let observe ?index_root ~chain_id ~epoch ~state_root ~(header : Protocol.header) store =
  if header.Protocol.request.chain_id <> chain_id
      || epoch < header.request.epoch_id || epoch > header.request.expires_epoch then
    Lwt.return_error "compute funding observation scope"
  else
    let* captured = Store_irmin.capture_read_snapshot_at ?index_root store
      ~epoch_id:epoch ~state_root in
    match captured with
    | Error _ as error -> Lwt.return error
    | Ok snapshot ->
      let id = Certificate.request_id header.request in
      let* raw = Store_irmin.read ~snapshot store (deposit_path id) in
      match raw with
      | None -> Lwt.return_ok None
      | Some raw ->
        match verify_deposit ~chain_id ~epoch ~header raw with
        | Error _ as error -> Lwt.return error
        | Ok deposit -> Lwt.return_ok (Some {header; deposit; epoch; state_root})

let change ledger action =
  let store = ledger.Ledger.store in
  match Store_irmin.save_batch store with
  | Error _ as error -> Lwt.return error
  | Ok _ when not (Ledger.journal_active ledger) ->
    Lwt.return_error "compute funding requires epoch journal"
  | Ok savepoint ->
    match Ledger.begin_journal ledger with
    | Error _ as error -> Lwt.return error
    | Ok () ->
      let restore () =
        let saved = Store_irmin.restore_batch store savepoint in
        let accounts = Ledger.abort_journal ledger in
        if Result.is_error saved || Result.is_error accounts then
          failwith "compute funding rollback failed" in
      Lwt.try_bind (fun () -> protect action)
        (function
          | Error _ as error -> restore (); Lwt.return error
          | Ok value ->
            Lwt.catch
              (fun () -> protect (fun () ->
                let* () = Ledger.flush_dirty_lwt ledger in
                Chaos.inject "compute_fund_after_accounts";
                match Ledger.commit_journal ledger with
                | Ok () -> Lwt.return_ok value
                | Error error -> failwith error))
              (fun error -> restore (); Lwt.fail error))
        (fun error -> restore (); Lwt.fail error)

let hold_header ~ledger ~chain_id ~epoch ~fee ~(header : Protocol.header) permission =
  if Z.sign fee < 0 then Lwt.return_error "compute funding fee is negative"
  else change ledger (fun () ->
    match Ledger.find_opt ledger header.request.caller with
    | None -> Lwt.return_error "compute funding payer missing"
    | Some account when account.nonce < 0 || account.nonce = max_int ->
      Lwt.return_error "compute funding nonce exhausted"
    | Some account ->
      let nonce = account.nonce + 1 in
      match verify_header ~chain_id ~epoch ~nonce:(Int64.of_int nonce) ~target:escrow ~header permission with
      | Error _ as error -> Lwt.return error
      | Ok _ when permission.terms.payer = escrow -> Lwt.return_error "compute funding payer is escrow"
      | Ok amount ->
        let id = Certificate.request_id header.request in
        let deposit = {request_id = id; permission; opened_epoch = epoch;
          returned_epoch = None; paid = None} in
        let* found = read ledger.store ~id in
        match found with
        | Error _ as error -> Lwt.return error
        | Ok (Some _) -> Lwt.return_error "compute deposit exists"
        | Ok None ->
          let amount = Z.of_int64 amount in
          match Ledger.debit ledger permission.terms.payer (Z.add amount fee) nonce with
          | Error _ as error -> Lwt.return error
          | Ok () ->
            Chaos.inject "compute_fund_after_debit";
            match Ledger.credit ledger escrow amount with
            | Error _ as error -> Lwt.return error
            | Ok () ->
              let* () = Store_irmin.write ledger.store (deposit_path id) (encode_deposit deposit) in
              Chaos.inject "compute_fund_after_record";
              Lwt.return_ok (id, fee))

let hold ~ledger ~chain_id ~epoch ~fee ~call permission =
  match Protocol.header call with
  | Error _ -> Lwt.return_error "compute funding call shape"
  | Ok header -> hold_header ~ledger ~chain_id ~epoch ~fee ~header permission

let refund ~ledger ~chain_id ~epoch ~id =
  change ledger (fun () ->
    let* found = read ledger.Ledger.store ~id in
    match found with
    | Error _ as error -> Lwt.return error
    | Ok None -> Lwt.return_error "compute deposit missing"
    | Ok (Some deposit) ->
      let terms = deposit.permission.terms in
      if terms.chain_id <> chain_id then Lwt.return_error "compute funding chain differs"
      else if Option.is_some deposit.returned_epoch then Lwt.return_error "compute deposit returned"
      else if Option.is_some deposit.paid then Lwt.return_error "compute deposit paid"
      else if epoch <= terms.expires_epoch then Lwt.return_error "compute deposit has not expired"
      else
        let amount = Z.of_int64 terms.amount in
        match Ledger.debit_amount_only ledger escrow amount with
        | Error _ as error -> Lwt.return error
        | Ok () ->
          Chaos.inject "compute_fund_after_debit";
          match Ledger.credit ledger terms.payer amount with
          | Error _ as error -> Lwt.return error
          | Ok () ->
            let deposit = {deposit with returned_epoch = Some epoch} in
            let* () = Store_irmin.write ledger.store (deposit_path id) (encode_deposit deposit) in
            Chaos.inject "compute_fund_after_record";
            Lwt.return_ok amount)

let payment ~chain_id ~epoch (bill : Compute_bill.t) deposit =
  match verify_deposit ~chain_id ~epoch ~header:bill.header (encode_deposit deposit) with
  | Error _ as error -> error
  | Ok _ ->
    let amount = Z.of_int64 deposit.permission.terms.amount in
    let total = List.fold_left (fun total (_, value) -> Z.add total value)
      bill.refund bill.payments in
    if not (Z.equal total amount) || Z.sign bill.refund < 0
        || List.exists (fun (_, value) -> Z.sign value < 0) bill.payments then
      Error "compute payment total differs"
    else Ok ({deposit with paid = Some (epoch, bill.root)},
      (deposit.permission.terms.payer, bill.refund) :: bill.payments)

let settle ~ledger ~chain_id ~epoch (bill : Compute_bill.t) =
  change ledger (fun () ->
    let id = bill.measured.request_id in
    let* found = read ledger.Ledger.store ~id in
    match found with
    | Error _ as error -> Lwt.return error
    | Ok None -> Lwt.return_error "compute deposit missing"
    | Ok (Some deposit) ->
      match payment ~chain_id ~epoch bill deposit with
      | Error _ as error -> Lwt.return error
      | Ok (deposit, credits) ->
        let amount = Z.of_int64 deposit.permission.terms.amount in
        match Ledger.debit_amount_only ledger escrow amount with
        | Error _ as error -> Lwt.return error
        | Ok () ->
          Chaos.inject "compute_pay_after_debit";
          let rec credit = function
            | [] -> Ok ()
            | (node, value) :: rest ->
              match Ledger.credit ledger node value with
              | Error _ as error -> error
              | Ok () -> Chaos.inject "compute_pay_after_credit"; credit rest in
          match credit credits with
          | Error _ as error -> Lwt.return error
          | Ok () ->
            let* () = Store_irmin.write ledger.store (deposit_path id) (encode_deposit deposit) in
            Chaos.inject "compute_pay_after_record";
            Lwt.return_ok (bill.charged, bill.refund))