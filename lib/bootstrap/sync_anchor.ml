(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Checkpoint = State_sync_checkpoint
module C_codec = Octra_consensus.C_codec
module C_config = Octra_consensus.C_config
module C_hash = Octra_consensus.C_hash
module C_qc = Octra_consensus.C_qc
module C_types = Octra_consensus.C_types
module Eic = Octra_core.Epoch_index_commitment
module Irmin = Octra_core.Store_irmin
module O = Octra_net.Oce1
module Update = Octra_core.Validator_set_update

type source =
  | Pending
  | Active

type step = {
  source : source;
  finalize : C_types.finalize;
  ledger_state_root : string;
  epoch_index_root : string;
  update : string;
  proof : string;
}

type t = {
  steps : step list;
  finalize : C_types.finalize;
  validator_set : C_types.validator_set;
}

let max_steps = 1_024
let max_finalize_bytes = 4_000_000
let max_set_bytes = 1_000_000
let max_update_bytes = 1_000_000
let max_proof_bytes = 4_000_000
let max_root_bytes = 256

let make ~steps ~finalize ~validator_set =
  { steps; finalize; validator_set }

let steps t =
  t.steps

let validator_set t =
  t.validator_set

let finality t =
  t.finalize

let ( let* ) value f =
  match value with
  | Ok item -> f item
  | Error _ as error -> error

let protect f =
  try Ok (f ()) with exn -> Error (Printexc.to_string exn)

let source_to_u8 = function
  | Pending -> 1
  | Active -> 2

let source_of_u8 = function
  | 1 -> Pending
  | 2 -> Active
  | _ -> failwith "validator transition source is invalid"

let put_step buffer (step : step) =
  O.put_u8 buffer (source_to_u8 step.source);
  O.put_bytes buffer (C_codec.encode_finalize step.finalize);
  O.put_string buffer step.ledger_state_root;
  O.put_string buffer step.epoch_index_root;
  O.put_bytes buffer step.update;
  O.put_bytes buffer step.proof

let get_step cursor =
  let source = source_of_u8 (O.get_u8 cursor) in
  let finalize =
    O.get_bytes_bounded ~max:max_finalize_bytes cursor
    |> C_codec.decode_finalize
  in
  let ledger_state_root = O.get_string_bounded ~max:max_root_bytes cursor in
  let epoch_index_root = O.get_string_bounded ~max:max_root_bytes cursor in
  let update = O.get_bytes_bounded ~max:max_update_bytes cursor in
  let proof = O.get_bytes_bounded ~max:max_proof_bytes cursor in
  { source; finalize; ledger_state_root; epoch_index_root; update; proof }

let raw t =
  O.encode (fun buffer ->
    O.put_list put_step buffer t.steps;
    O.put_bytes buffer (C_codec.encode_finalize t.finalize);
    O.put_bytes buffer (C_codec.encode_validator_set t.validator_set))

let encode t =
  raw t |> Base64.encode_exn

let decode encoded =
  let* bytes = protect (fun () -> Base64.decode_exn encoded) in
  if Base64.encode_exn bytes <> encoded then Error "finality encoding is not exact"
  else
    let* value =
      protect (fun () ->
        O.decode
          (fun cursor ->
            let steps =
              O.get_list_bounded ~max:max_steps get_step cursor
            in
            let finalize =
              O.get_bytes_bounded ~max:max_finalize_bytes cursor
              |> C_codec.decode_finalize
            in
            let validator_set =
              O.get_bytes_bounded ~max:max_set_bytes cursor
              |> C_codec.decode_validator_set
            in
            { steps; finalize; validator_set })
          bytes)
    in
    if raw value <> bytes then Error "finality encoding is not exact"
    else Ok value

let verify_vote validator_set (vote : C_types.vote) =
  match C_types.pubkey_of_addr validator_set vote.C_types.validator with
  | None -> false
  | Some pubkey -> C_hash.verify_vote ~pubkey_raw:pubkey vote

let raw_validator_set validator_set =
  let* entries =
    List.fold_left
      (fun state (validator : C_types.validator_info) ->
        let* items = state in
        let* pubkey = Checkpoint.raw_pubkey validator.pubkey in
        let* weight =
          match C_types.weight_of_addr validator_set validator.address with
          | Some value -> Ok value
          | None -> Error "trusted validator weight is missing"
        in
        Ok ((C_types.{ validator with pubkey }, weight) :: items))
      (Ok [])
      validator_set.C_types.validators
    |> Result.map List.rev
  in
  if validator_set.weighted then
    C_types.make_weighted_validator_set entries
  else
    Ok (C_types.make_validator_set (List.map fst entries))

let encoded_validator_set validator_set =
  let* validators =
    List.fold_left
      (fun state (validator : C_types.validator_info) ->
        let* items = state in
        if String.length validator.pubkey <> 32 then
          Error "finality validator public key is invalid"
        else
          let* _ =
            match C_types.weight_of_addr validator_set validator.address with
            | Some value -> Ok value
            | None -> Error "finality validator weight is missing"
          in
          let encoded =
            C_types.{ validator with pubkey = Base64.encode_exn validator.pubkey }
          in
          Ok (encoded :: items))
      (Ok [])
      validator_set.C_types.validators
    |> Result.map List.rev
  in
  if validators = [] then
    Error "finality validator set is empty"
  else
    Ok C_types.{ validator_set with validators }

let finalize_hash finalize =
  C_codec.encode_finalize finalize
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_raw_string
  |> Checkpoint.raw_to_hex

let same_set left right =
  C_config.validator_set_hash left = C_config.validator_set_hash right

let lower_hex lengths value =
  List.mem (String.length value) lengths
  && String.for_all
       (function
         | '0'..'9' | 'a'..'f' -> true
         | _ -> false)
       value

let verify_finalize ~chain_id ~validator_set finalize =
  if finalize.C_types.chain_id <> chain_id
     || finalize.header.chain_id <> chain_id then
    Error "finality chain mismatch"
  else if finalize.epoch_id <> finalize.header.epoch_id then
    Error "finality epoch mismatch"
  else
    match
      C_qc.validate_persisted_finalize_strict
        ~chain_id
        ~validator_set
        ~verify_vote:(verify_vote validator_set)
        finalize
    with
    | C_qc.Valid -> Ok ()
    | C_qc.Invalid reason -> Error ("finality qc " ^ reason)

let verify_step ~chain_id ~prior_epoch validator_set (step : step) =
  let epoch = step.finalize.C_types.epoch_id in
  if Option.fold
       ~none:false
       ~some:(fun prior -> Int64.compare epoch prior <= 0)
       prior_epoch then
    Error "validator transition order is invalid"
  else if Int64.compare epoch Int64.max_int = 0 then
    Error "validator transition epoch overflows"
  else if not (lower_hex [64; 128] step.ledger_state_root) then
    Error "validator transition ledger root is invalid"
  else if not (lower_hex [64] step.epoch_index_root) then
    Error "validator transition index root is invalid"
  else
    let* update = Update.of_string step.update in
    if Update.to_string update <> step.update then
      Error "validator transition update is not exact"
    else if not update.Update.weighted then
      Error "validator transition update is not weighted"
    else if update.source_epoch = None then
      Error "validator transition source epoch is missing"
    else
      let path, activation_valid =
        match step.source with
        | Pending ->
            ["meta"; Update.pending_meta_key],
            update.activate_epoch = Int64.succ epoch
        | Active ->
            ["meta"; Update.active_meta_key],
            Int64.compare update.activate_epoch epoch <= 0
      in
      if not activation_valid then
        Error "validator transition activation epoch mismatch"
      else
      let* () = verify_finalize ~chain_id ~validator_set step.finalize in
      let folded =
        Eic.folded_state_root
          ~ledger_state_root:step.ledger_state_root
          ~epoch_index_root:step.epoch_index_root
      in
      if Checkpoint.raw_to_hex step.finalize.header.proposed_state_root <> folded then
        Error "validator transition folded root mismatch"
      else
        let* value =
          Irmin.verify_merkle_proof_det
            ~ledger_state_root:step.ledger_state_root
            ~path
            ~proof:step.proof
        in
        if value <> Some step.update then
          Error "validator transition state proof mismatch"
        else
          let* next = Update.validator_set update in
          let next =
            C_types.validator_set_for_epoch
              ~chain_id
              ~epoch_id:update.activate_epoch
              next
          in
          Ok (Some epoch, next)

let derive ~validator_set:trusted value =
  let* trusted = raw_validator_set trusted in
  let rec loop prior_epoch current = function
    | [] ->
        if same_set current value.validator_set then Ok current
        else Error "finality validator transition is incomplete"
    | (step : step) :: rest ->
        if Int64.compare step.finalize.epoch_id value.finalize.epoch_id >= 0 then
          Error "validator transition is not before checkpoint"
        else
          let* prior_epoch, next =
            verify_step
              ~chain_id:value.finalize.chain_id
              ~prior_epoch
              current
              step
          in
          loop prior_epoch next rest
  in
  loop None trusted value.steps

let verify_checkpoint checkpoint value =
  let* () = Checkpoint.validate checkpoint in
  let finalize = value.finalize in
  let header = finalize.C_types.header in
  let set_hash =
    C_config.validator_set_hash value.validator_set
    |> Checkpoint.raw_to_hex
  in
  let* index_root =
    match checkpoint.Checkpoint.epoch_index_root with
    | Some value -> Ok value
    | None -> Error "finality checkpoint index root is missing"
  in
  let folded =
    Eic.folded_state_root
      ~ledger_state_root:checkpoint.ledger_state_root
      ~epoch_index_root:index_root
  in
  if folded <> checkpoint.state_root then
    Error "finality checkpoint folded root mismatch"
  else if set_hash <> checkpoint.Checkpoint.validator_set_hash then
    Error "finality validator set mismatch"
  else if finalize.chain_id <> checkpoint.chain_id then
    Error "finality chain mismatch"
  else if finalize.epoch_id <> checkpoint.epoch
       || header.epoch_id <> checkpoint.epoch then
    Error "finality epoch mismatch"
  else if Checkpoint.raw_to_hex header.proposed_state_root
          <> checkpoint.state_root then
    Error "finality state root mismatch"
  else if header.txid_hi <> checkpoint.txid_hi then
    Error "finality transaction high-water mismatch"
  else if
    Option.fold
      ~none:false
      ~some:(fun expected -> finalize_hash finalize <> expected)
      checkpoint.quorum_cert_hash
  then
    Error "finality certificate hash mismatch"
  else
    verify_finalize
      ~chain_id:checkpoint.chain_id
      ~validator_set:value.validator_set
      finalize

let verify ~validator_set checkpoint encoded =
  let* value = decode encoded in
  let* derived = derive ~validator_set value in
  if not (same_set derived value.validator_set) then
    Error "finality validator transition mismatch"
  else
    let* () = verify_checkpoint checkpoint value in
    Ok value