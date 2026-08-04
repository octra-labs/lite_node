(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C_config = Octra_consensus.C_config
module C_hash = Octra_consensus.C_hash
module C_parent = Octra_consensus.C_parent_commit
module C_types = Octra_consensus.C_types
module O = Octra_net.Oce1

type t = {
  chain_id : string;
  epoch : int;
  state_root : string;
  ledger_state_root : string;
  txid_hi : int64;
  config_hash : string;
  validator_set_hash : string;
  epoch_index_hash : string;
  epoch_index_root : string;
  parent_commit : C_types.parent_commit;
}

let max_chain_id_bytes = 128
let max_root_bytes = 256
let max_parent_bytes = 8_000_000

let ( let* ) value f =
  match value with
  | Ok item -> f item
  | Error _ as error -> error

let protect f =
  try Ok (f ()) with exn -> Error (Printexc.to_string exn)

let lower_hex length value =
  String.length value = length
  && String.for_all
       (function
         | '0'..'9' | 'a'..'f' -> true
         | _ -> false)
       value

let hex_of_raw value =
  let output = Bytes.create (String.length value * 2) in
  let alphabet = "0123456789abcdef" in
  String.iteri
    (fun index byte ->
      let value = Char.code byte in
      Bytes.set output (index * 2) alphabet.[value lsr 4];
      Bytes.set output ((index * 2) + 1) alphabet.[value land 15])
    value;
  Bytes.unsafe_to_string output

let create ~chain_id ~epoch ~state_root ~ledger_state_root ~txid_hi
    ~config_hash ~validator_set_hash ~epoch_index_hash ~epoch_index_root
    ~parent_commit =
  if chain_id = "" || String.length chain_id > max_chain_id_bytes then
    Error "history floor chain id is invalid"
  else if epoch < 0 then
    Error "history floor epoch is negative"
  else if Int64.compare txid_hi 0L < 0 || txid_hi = Int64.max_int then
    Error "history floor transaction high-water is invalid"
  else if not (lower_hex 64 state_root) then
    Error "history floor state root is invalid"
  else if not (Octra_consensus.C_light_root.text_root_ok ledger_state_root) then
    Error "history floor ledger root is invalid"
  else if not (lower_hex 64 config_hash) then
    Error "history floor config hash is invalid"
  else if not (lower_hex 64 validator_set_hash) then
    Error "history floor validator set hash is invalid"
  else if not (lower_hex 64 epoch_index_hash) then
    Error "history floor epoch hash is invalid"
  else if not (lower_hex 64 epoch_index_root) then
    Error "history floor epoch root is invalid"
  else
    let certificate = parent_commit.C_types.certificate in
    let header = certificate.header in
    let folded =
      Epoch_index_commitment.folded_state_root
        ~ledger_state_root
        ~epoch_index_root
    in
    let expected = C_parent.{
      chain_id;
      epoch_id = Int64.of_int epoch;
      proposal_id = certificate.proposal_id;
      state_root = header.proposed_state_root;
      validator_set_hash = C_config.validator_set_hash parent_commit.validator_set;
    } in
    if folded <> state_root then
      Error "history floor folded root differs"
    else if hex_of_raw header.proposed_state_root <> state_root then
      Error "history floor parent state root differs"
    else if header.txid_hi <> txid_hi then
      Error "history floor parent transaction high-water differs"
    else if C_hash.proposal_id header <> certificate.proposal_id then
      Error "history floor proposal id differs"
    else if
      hex_of_raw (C_config.validator_set_hash parent_commit.validator_set)
      <> validator_set_hash
    then
      Error "history floor validator set differs"
    else
      match C_parent.validate expected parent_commit with
      | C_parent.Invalid reason ->
          Error ("history floor parent commit is invalid: " ^ reason)
      | C_parent.Valid ->
          Ok {
            chain_id;
            epoch;
            state_root;
            ledger_state_root;
            txid_hi;
            config_hash;
            validator_set_hash;
            epoch_index_hash;
            epoch_index_root;
            parent_commit = C_parent.decode (C_parent.encode parent_commit);
          }

let chain_id t = t.chain_id
let epoch t = t.epoch
let state_root t = t.state_root
let ledger_state_root t = t.ledger_state_root
let txid_hi t = t.txid_hi
let next_txid t = Int64.succ t.txid_hi
let config_hash t = t.config_hash
let validator_set_hash t = t.validator_set_hash
let epoch_index_hash t = t.epoch_index_hash
let epoch_index_root t = t.epoch_index_root
let parent_commit t = t.parent_commit

let raw t =
  O.encode
    (fun buffer ->
      O.put_string buffer t.chain_id;
      O.put_u64 buffer (Int64.of_int t.epoch);
      O.put_hash32 buffer t.state_root;
      O.put_string buffer t.ledger_state_root;
      O.put_u64 buffer t.txid_hi;
      O.put_hash32 buffer t.config_hash;
      O.put_hash32 buffer t.validator_set_hash;
      O.put_hash32 buffer t.epoch_index_hash;
      O.put_hash32 buffer t.epoch_index_root;
      O.put_bytes buffer (C_parent.encode t.parent_commit))

let to_string t =
  Base64.encode_exn (raw t)

let of_string encoded =
  let* bytes = protect (fun () -> Base64.decode_exn encoded) in
  if Base64.encode_exn bytes <> encoded then
    Error "history floor encoding is not exact"
  else
    let* fields =
      protect
        (fun () ->
          O.decode
            (fun cursor ->
              let chain_id =
                O.get_string_bounded ~max:max_chain_id_bytes cursor
              in
              let epoch = O.get_u64 cursor in
              let state_root = O.get_hash32 cursor |> hex_of_raw in
              let ledger_state_root =
                O.get_string_bounded ~max:max_root_bytes cursor
              in
              let txid_hi = O.get_u64 cursor in
              let config_hash = O.get_hash32 cursor |> hex_of_raw in
              let validator_set_hash = O.get_hash32 cursor |> hex_of_raw in
              let epoch_index_hash = O.get_hash32 cursor |> hex_of_raw in
              let epoch_index_root = O.get_hash32 cursor |> hex_of_raw in
              let parent_commit =
                O.get_bytes_bounded ~max:max_parent_bytes cursor
                |> C_parent.decode
              in
              chain_id,
              epoch,
              state_root,
              ledger_state_root,
              txid_hi,
              config_hash,
              validator_set_hash,
              epoch_index_hash,
              epoch_index_root,
              parent_commit)
            bytes)
    in
    let chain_id, epoch, state_root, ledger_state_root, txid_hi,
      config_hash, validator_set_hash, epoch_index_hash, epoch_index_root,
      parent_commit = fields
    in
    if Int64.compare epoch (Int64.of_int max_int) > 0 then
      Error "history floor epoch exceeds platform limit"
    else
      let* value =
        create
          ~chain_id
          ~epoch:(Int64.to_int epoch)
          ~state_root
          ~ledger_state_root
          ~txid_hi
          ~config_hash
          ~validator_set_hash
          ~epoch_index_hash
          ~epoch_index_root
          ~parent_commit
      in
      if raw value <> bytes then
        Error "history floor encoding is not exact"
      else
        Ok value