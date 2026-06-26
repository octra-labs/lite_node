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


module Ledger = Octra_core.Ledger
module Rpc = Octra_core.Rpc

let ok_lwt value =
  Lwt.return (Ok value)

let err_lwt err =
  Lwt.return (Error err)

let raw32_of_hex name s =
  if String.length s <> 64 || not (Text.is_hex_string s) then
    Error ("invalid " ^ name)
  else
    Ok (Text.hex_to_raw32_lossy s)

let opt_raw32_of_hex = function
  | None -> Ok None
  | Some s ->
    match raw32_of_hex "optional root" s with
    | Error e -> Error e
    | Ok raw -> Ok (Some raw)

type signer = {
  chain_id : string;
  validator_addr : string;
  validator_pubkey : string;
  validator_priv_b64 : string;
  config_hash : string;
}

let sign_head signer h =
  match raw32_of_hex "state_root" h.Octra_core.Head_manifest.state_root with
  | Error e -> Error e
  | Ok state_root ->
    match opt_raw32_of_hex h.epoch_index_root with
    | Error e -> Error e
    | Ok epoch_index_root ->
      let priv_raw_full = Base64.decode_exn signer.validator_priv_b64 in
      let priv_raw =
        if String.length priv_raw_full >= 32 then String.sub priv_raw_full 0 32
        else priv_raw_full in
      let sign_fn msg =
        Octra_consensus.C_hash.sign_ed25519 ~priv_raw ~msg in
      let att = Octra_consensus.C_light_root.{
        chain_id = signer.chain_id;
        validator_addr = signer.validator_addr;
        head_epoch = Int64.of_int h.Octra_core.Head_manifest.epoch_id;
        state_root;
        ledger_state_root = h.ledger_state_root;
        epoch_index_root;
        txid_hi = h.txid_hi;
        config_hash = signer.config_hash;
        signature = "";
      } in
      Octra_consensus.C_light_root.sign ~sign_fn att

let signed_root signer h =
  match sign_head signer h with
  | Error e -> Error e
  | Ok signed ->
    Ok (Rpc_view.signed_root
      ~validator_pubkey:signer.validator_pubkey
      h
      signed)

let account_proof_addr params =
  Rpc.require_address params 0 "address"

let epoch_proof_id params =
  Rpc.require_int params 0 "epoch_id"

let tx_inclusion_hash params =
  Rpc.require_hash params 0 "hash"

let account_witness ~chain_id ~addr h value =
  match value with
  | None -> Ok `Null
  | Some raw ->
    match Yojson.Safe.from_string raw with
    | exception _ -> Error "invalid account witness"
    | json ->
      match Octra_core.Ledger_types.account_of_yojson json with
      | Error _ -> Error "invalid account witness"
      | Ok a ->
        match raw32_of_hex "state_root" h.Octra_core.Head_manifest.state_root with
        | Error e -> Error e
        | Ok state_root ->
          let proof =
            Octra_consensus.C_light_account.make
              ~chain_id
              ~address:addr
              ~head_epoch:(Int64.of_int h.Octra_core.Head_manifest.epoch_id)
              ~state_root
              ~balance:(Z.to_string a.Ledger.balance)
              ~nonce:a.Ledger.nonce
              ~public_key:a.Ledger.public_key
              ~encrypted_balance:a.Ledger.encrypted_balance
              ~decrypt_allowance:(Z.to_string a.Ledger.decrypt_allowance) in
          if Octra_consensus.C_light_account.verify proof then
            Ok (Rpc_view.account_witness ~inclusion:true proof)
          else
            Error "invalid account witness"

let account_proof signer ~store ~addr h =
  let open Lwt.Syntax in
  match sign_head signer h with
  | Error e -> Lwt.return (Error e)
  | Ok signed ->
    let* merkle = Octra_core.Store_irmin.account_merkle_proof store addr in
    match merkle with
    | Error e -> Lwt.return (Error e)
    | Ok merkle ->
      let ledger_root = Octra_core.Head_manifest.ledger_state_root h in
      if merkle.ledger_state_root <> ledger_root then
        Lwt.return (Error "account proof root mismatch")
      else
        let* verified =
          Octra_core.Store_irmin.verify_account_merkle_proof_lwt
            ~ledger_state_root:ledger_root
            ~addr
            ~proof:merkle.proof in
        match verified with
        | Error e -> Lwt.return (Error e)
        | Ok verified_value ->
          if verified_value <> merkle.value then
            Lwt.return (Error "account proof value mismatch")
          else
            match account_witness ~chain_id:signer.chain_id ~addr h merkle.value with
            | Error e -> Lwt.return (Error e)
            | Ok account ->
              let exists = merkle.value <> None in
              Lwt.return (Ok (Rpc_view.account_proof
                ~root:(Rpc_view.signed_root
                  ~validator_pubkey:signer.validator_pubkey
                  h
                  signed)
                ~exists
                ~account
                ~irmin_proof:(Rpc_view.account_irmin_proof ~addr merkle ~exists)))

let account_proof_params signer ~store ~head params =
  match account_proof_addr params with
  | Error e ->
    err_lwt e
  | Ok addr ->
    match head with
    | None ->
      err_lwt (Rpc.not_found "HEAD not available")
    | Some h ->
      let open Lwt.Syntax in
      let* result = account_proof signer ~store ~addr h in
      match result with
      | Error e ->
        err_lwt (Rpc.err (-32000) e None)
      | Ok proof ->
        ok_lwt proof

let rec collect_epoch_tx_hashes chaindata start_txid tx_count i acc =
  if i >= tx_count then Some (List.rev acc)
  else
    let txid = Int64.add start_txid (Int64.of_int i) in
    match Octra_core.Store_chaindata.get_tx_by_txid chaindata txid with
    | None -> None
    | Some (tx_hash, _) ->
      collect_epoch_tx_hashes chaindata start_txid tx_count (i + 1)
        (String.lowercase_ascii tx_hash :: acc)

let epoch_proof_of_header ~chain_id h tx_hashes =
  let proof = Octra_consensus.C_light_epoch.{
    chain_id;
    epoch_id = Int64.of_int h.Octra_core.Epochlog.id;
    prev_state_root = String.lowercase_ascii h.prev_state_root;
    state_root = String.lowercase_ascii h.state_root;
    tx_list_hash = Octra_consensus.C_light_epoch.tx_list_hash_hex tx_hashes;
    tx_hashes;
    start_txid = h.start_txid;
    tx_count = h.tx_count;
    proposer = h.proposer.creator_addr;
    commit_round = h.proposer.commit_round;
  } in
  if Octra_consensus.C_light_epoch.verify proof then Ok proof
  else Error "epoch proof verification failed"

let epoch_proof ~chain_id chaindata eid =
  match Octra_core.Store_chaindata.get_epoch_header chaindata eid with
  | None -> Error "epoch not found"
  | Some h ->
    match collect_epoch_tx_hashes chaindata h.start_txid h.tx_count 0 [] with
    | None -> Error "epoch tx list incomplete"
    | Some tx_hashes -> epoch_proof_of_header ~chain_id h tx_hashes

let epoch_proof_params ~chain_id chaindata params =
  match epoch_proof_id params with
  | Error e ->
    err_lwt e
  | Ok eid ->
    match epoch_proof ~chain_id chaindata eid with
    | Error e ->
      err_lwt (Rpc.err (-32000) e None)
    | Ok proof ->
      ok_lwt (Rpc_view.epoch_proof proof)

let find_tx_index tx_hash tx_hashes =
  let rec go i = function
    | [] -> None
    | h :: rest ->
      if h = tx_hash then Some i else go (i + 1) rest in
  go 0 tx_hashes

let tx_inclusion_of_epoch ~tx_hash epoch =
  match find_tx_index tx_hash epoch.Octra_consensus.C_light_epoch.tx_hashes with
  | None -> Error "transaction not present in epoch proof"
  | Some index ->
    let proof = Octra_consensus.C_light_epoch.{ epoch; tx_hash; index } in
    if Octra_consensus.C_light_epoch.verify_tx_inclusion proof then Ok proof
    else Error "tx inclusion verification failed"

let tx_inclusion_proof ~chain_id chaindata tx_hash =
  let tx_hash = String.lowercase_ascii tx_hash in
  match Octra_core.Store_chaindata.get_tx_by_hash chaindata tx_hash with
  | None -> Error "transaction not found"
  | Some (epoch_id, tx_json) ->
    match epoch_proof ~chain_id chaindata epoch_id with
    | Error e -> Error e
    | Ok epoch ->
      match tx_inclusion_of_epoch ~tx_hash epoch with
      | Error e -> Error e
      | Ok proof -> Ok (proof, tx_json)

let tx_inclusion_proof_params ~chain_id chaindata params =
  match tx_inclusion_hash params with
  | Error e ->
    err_lwt e
  | Ok tx_hash ->
    match tx_inclusion_proof ~chain_id chaindata tx_hash with
    | Error "transaction not found" ->
      err_lwt (Rpc.not_found "transaction not found")
    | Error e ->
      err_lwt (Rpc.err (-32000) e None)
    | Ok (proof, tx_json) ->
      ok_lwt (Rpc_view.tx_inclusion_proof proof ~tx_json)