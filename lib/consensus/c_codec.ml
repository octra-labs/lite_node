(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open C_types
module O = Octra_net.Oce1

let max_chain_id_bytes = 128
let max_addr_bytes = 128
let max_request_id_bytes = 128
let max_status_bytes = 64
let max_error_bytes = 256
let max_signature_bytes = 64
let max_round = 1_000_000
let max_proposal_txs = 2_048
let max_validators = 4_096
let max_catchup_records = 16
let max_json_bytes = 8 * 1024 * 1024

type round_sync = {
  chain_id : string;
  epoch_id : int64;
  round : int;
  step : round_step;
  request : bool;
  validator : string;
  signature : string;
}

let get_string max c = O.get_string_bounded ~max c

let get_exact_bytes len c =
  let value = O.get_bytes_bounded ~max:len c in
  if String.length value <> len then failwith "consensus codec: bad byte length";
  value

let get_exact_string len c =
  let value = O.get_string_bounded ~max:len c in
  if String.length value <> len then failwith "consensus codec: bad string length";
  value

let get_round c =
  let round = O.get_u32_int c in
  if round < 0 || round > max_round then failwith "consensus codec: round out of range";
  round

let get_epoch_ts c =
  let epoch_ts = Int64.float_of_bits (O.get_u64 c) in
  match Epoch_time.of_seconds epoch_ts with
  | Ok _ -> epoch_ts
  | Error error -> failwith ("consensus codec: " ^ error)

let get_tx_hashes c = O.get_list_bounded ~max:max_proposal_txs O.get_hash32 c
let get_json_list c =
  O.get_list_bounded ~max:max_proposal_txs (get_string max_json_bytes) c

let encode_epoch_header buf (h : epoch_header) =
  O.put_u16 buf h.proto_version;
  O.put_string buf h.chain_id;
  O.put_u64 buf h.epoch_id;
  O.put_hash32 buf h.prev_state_root;
  O.put_hash32 buf h.tx_list_hash;
  O.put_hash32 buf h.receipt_root;
  O.put_hash32 buf h.proposed_state_root;
  begin
    match h.proto_version with
    | version when version = proto_version_parent_legacy ->
      if not (Octra_net.Hash_domain.is_nil h.parent_commit_hash) then
        invalid_arg "legacy epoch header has parent commit"
    | version when version = proto_version_current ->
      O.put_hash32 buf h.parent_commit_hash
    | _ -> invalid_arg "unsupported epoch header protocol"
  end;
  O.put_string buf h.creator_addr;
  O.put_u64 buf h.txid_hi;
  O.put_u64 buf (Int64.bits_of_float h.ts)

let decode_epoch_header c =
  let proto_version = O.get_u16 c in
  if
    proto_version <> proto_version_parent_legacy
    && proto_version <> proto_version_current
  then
    failwith "consensus codec: unsupported epoch header protocol";
  let chain_id = get_string max_chain_id_bytes c in
  let epoch_id = O.get_u64 c in
  let prev_state_root = O.get_hash32 c in
  let tx_list_hash = O.get_hash32 c in
  let receipt_root = O.get_hash32 c in
  let proposed_state_root = O.get_hash32 c in
  let parent_commit_hash =
    if proto_version = proto_version_parent_legacy then
      Octra_net.Hash_domain.nil_hash
    else O.get_hash32 c
  in
  let creator_addr = get_string max_addr_bytes c in
  let txid_hi = O.get_u64 c in
  let ts = Int64.float_of_bits (O.get_u64 c) in
  (match classify_float ts with
   | FP_normal | FP_subnormal | FP_zero -> ()
   | FP_infinite | FP_nan -> failwith "epoch_header timestamp is not finite");
  {
    proto_version;
    chain_id;
    epoch_id;
    prev_state_root;
    tx_list_hash;
    receipt_root;
    proposed_state_root;
    parent_commit_hash;
    creator_addr;
    txid_hi;
    ts;
  }

let encode_vote (v : vote) =
  O.encode (fun buf ->
    O.put_string buf v.chain_id;
    O.put_u64 buf v.epoch_id;
    O.put_u32_int buf v.round;
    O.put_u8 buf (vote_type_to_u8 v.vote_type);
    O.put_hash32 buf v.proposal_id;
    O.put_string buf v.validator;
    O.put_bytes buf v.signature)

let decode_vote payload =
  O.decode (fun c ->
    let chain_id = get_string max_chain_id_bytes c in
    let epoch_id = O.get_u64 c in
    let round = get_round c in
    let vote_type = vote_type_of_u8 (O.get_u8 c) in
    let proposal_id = O.get_hash32 c in
    let validator = get_string max_addr_bytes c in
    let signature = get_exact_bytes max_signature_bytes c in
    { chain_id; epoch_id; round; vote_type; proposal_id; validator; signature }) payload

let encode_round_sync (sync : round_sync) =
  O.encode (fun buf ->
    O.put_string buf sync.chain_id;
    O.put_u64 buf sync.epoch_id;
    O.put_u32_int buf sync.round;
    O.put_u8 buf (round_step_to_u8 sync.step);
    O.put_bool buf sync.request;
    O.put_string buf sync.validator;
    O.put_bytes buf sync.signature)

let decode_round_sync payload =
  O.decode (fun c ->
    let chain_id = get_string max_chain_id_bytes c in
    let epoch_id = O.get_u64 c in
    let round = get_round c in
    let step = round_step_of_u8 (O.get_u8 c) in
    let request = O.get_bool c in
    let validator = get_string max_addr_bytes c in
    let signature = get_exact_bytes max_signature_bytes c in
    { chain_id; epoch_id; round; step; request; validator; signature }) payload

let encode_validator_set validator_set =
  O.encode (fun buf ->
    O.put_bool buf validator_set.weighted;
    O.put_list
      (fun buf (validator : validator_info) ->
        O.put_string buf validator.address;
        O.put_bytes buf validator.pubkey;
        match weight_of_addr validator_set validator.address with
        | Some weight -> O.put_string buf (Z.to_string weight)
        | None -> failwith "consensus codec: validator weight missing")
      buf
      validator_set.validators)

let decode_validator_set payload =
  let validator_set =
    O.decode
      (fun c ->
        let weighted = O.get_bool c in
        let entries =
          O.get_list_bounded
            ~max:max_validators
            (fun c ->
              let address = get_string max_addr_bytes c in
              let pubkey = get_exact_bytes 32 c in
              let weight =
                try get_string 256 c |> Z.of_string
                with _ -> failwith "consensus codec: invalid validator weight"
              in
              { address; pubkey }, weight)
            c
        in
        match make_weighted_validator_set entries with
        | Error error -> failwith ("consensus codec: " ^ error)
        | Ok validator_set when weighted -> validator_set
        | Ok validator_set ->
          if
            List.exists
              (fun (_, weight) -> not (Z.equal weight Z.one))
              entries
          then
            failwith "consensus codec: unweighted validator has non-unit weight"
          else
            make_validator_set validator_set.validators)
      payload
  in
  if encode_validator_set validator_set <> payload then
    failwith "consensus codec: validator set is not canonical";
  validator_set

let put_commit_vote buf (vote : vote) =
  O.put_string buf vote.chain_id;
  O.put_u64 buf vote.epoch_id;
  O.put_u32_int buf vote.round;
  O.put_u8 buf (vote_type_to_u8 vote.vote_type);
  O.put_hash32 buf vote.proposal_id;
  O.put_string buf vote.validator;
  O.put_bytes buf vote.signature

let get_commit_vote c =
  let chain_id = get_string max_chain_id_bytes c in
  let epoch_id = O.get_u64 c in
  let round = get_round c in
  let vote_type = vote_type_of_u8 (O.get_u8 c) in
  let proposal_id = O.get_hash32 c in
  let validator = get_string max_addr_bytes c in
  let signature = get_exact_bytes max_signature_bytes c in
  { chain_id; epoch_id; round; vote_type; proposal_id; validator; signature }

let encode_commit_certificate (certificate : commit_certificate) =
  O.encode (fun buf ->
    O.put_string buf certificate.chain_id;
    O.put_u64 buf certificate.epoch_id;
    O.put_u32_int buf certificate.commit_round;
    encode_epoch_header buf certificate.header;
    O.put_hash32 buf certificate.proposal_id;
    O.put_list put_commit_vote buf certificate.precommits)

let decode_commit_certificate payload =
  O.decode
    (fun c ->
      let chain_id = get_string max_chain_id_bytes c in
      let epoch_id = O.get_u64 c in
      let commit_round = get_round c in
      let header = decode_epoch_header c in
      let proposal_id = O.get_hash32 c in
      let precommits =
        O.get_list_bounded ~max:max_validators get_commit_vote c
      in
      { chain_id; epoch_id; commit_round; header; proposal_id; precommits })
    payload

let canonical_parent_commit (parent : parent_commit) =
  let certificate = parent.certificate in
  {
    parent with
    certificate = {
      certificate with
      precommits =
        List.sort
          (fun (left : vote) (right : vote) ->
            String.compare left.validator right.validator)
          certificate.precommits;
    };
  }

let put_parent_commit_fields buf (parent : parent_commit) =
  let parent = canonical_parent_commit parent in
  O.put_bytes buf (encode_commit_certificate parent.certificate);
  O.put_bytes buf (encode_validator_set parent.validator_set)

let get_parent_commit_fields c =
  let certificate =
    O.get_bytes_bounded ~max:(16 * 1024 * 1024) c
    |> decode_commit_certificate
  in
  let validator_set =
    O.get_bytes_bounded ~max:(4 * 1024 * 1024) c
    |> decode_validator_set
  in
  { certificate; validator_set }

let encode_parent_commit parent =
  O.encode (fun buf -> put_parent_commit_fields buf parent)

let decode_parent_commit payload =
  let parent = O.decode get_parent_commit_fields payload in
  if encode_parent_commit parent <> payload then
    failwith "consensus codec: parent commit is not canonical";
  parent

let put_parent_commit buf parent =
  O.put_bytes buf (encode_parent_commit parent)

let get_parent_commit c =
  O.get_bytes_bounded ~max:(20 * 1024 * 1024) c
  |> decode_parent_commit

let encode_propose (p : propose) =
  if not (C_protocol.supported_version p.header.proto_version) then
    invalid_arg "consensus codec: proposal protocol mismatch";
  O.encode (fun buf ->
    O.put_string buf p.chain_id;
    O.put_u64 buf p.epoch_id;
    O.put_u32_int buf p.round;
    O.put_option O.put_u32_int buf p.valid_round;
    encode_epoch_header buf p.header;
    O.put_list O.put_hash32 buf p.tx_hashes;
    begin
      match p.header.proto_version with
      | version when version = proto_version_parent_legacy ->
        if Option.is_some p.parent_commit then
          invalid_arg "legacy proposal has parent commit"
      | version when version = proto_version_current ->
        O.put_option put_parent_commit buf p.parent_commit
      | _ -> invalid_arg "unsupported proposal protocol"
    end;
    O.put_string buf p.proposer;
    O.put_bytes buf p.signature)

let decode_propose payload =
  O.decode (fun c ->
    let chain_id = get_string max_chain_id_bytes c in
    let epoch_id = O.get_u64 c in
    let round = get_round c in
    let valid_round = O.get_option get_round c in
    let header = decode_epoch_header c in
    let tx_hashes = get_tx_hashes c in
    let parent_commit =
      if header.proto_version = proto_version_parent_legacy then None
      else O.get_option get_parent_commit c
    in
    let proposer = get_string max_addr_bytes c in
    let signature = get_exact_bytes max_signature_bytes c in
    {
      chain_id;
      epoch_id;
      round;
      valid_round;
      header;
      tx_hashes;
      parent_commit;
      proposer;
      signature;
    }) payload

let encode_finalize (f : finalize) =
  O.encode (fun buf ->
    O.put_string buf f.chain_id;
    O.put_u64 buf f.epoch_id;
    O.put_u32_int buf f.commit_round;
    encode_epoch_header buf f.header;
    O.put_hash32 buf f.proposal_id;
    O.put_list put_commit_vote buf f.precommits;
    match f.header.proto_version with
    | version when version = proto_version_parent_legacy ->
      if Option.is_some f.parent_commit then
        invalid_arg "legacy finalize has parent commit"
    | version when version = proto_version_current ->
      O.put_option put_parent_commit buf f.parent_commit
    | _ -> invalid_arg "unsupported finalize protocol")

let decode_finalize payload =
  O.decode (fun c ->
    let chain_id = get_string max_chain_id_bytes c in
    let epoch_id = O.get_u64 c in
    let commit_round = get_round c in
    let header = decode_epoch_header c in
    let proposal_id = O.get_hash32 c in
    let precommits =
      O.get_list_bounded ~max:max_validators get_commit_vote c
    in
    let parent_commit =
      if header.proto_version = proto_version_parent_legacy then None
      else O.get_option get_parent_commit c
    in
    {
      chain_id;
      epoch_id;
      commit_round;
      header;
      proposal_id;
      precommits;
      parent_commit;
    }) payload

type epoch_root_query = {
  chain_id : string;
  epoch_id : int64;
}

type epoch_root_response = {
  chain_id : string;
  epoch_id : int64;
  state_root : string option;
  responder_addr : string;
  responder_head_epoch : int64;
  signature : string;
}

let encode_epoch_root_query (q : epoch_root_query) =
  O.encode (fun buf ->
    O.put_string buf q.chain_id;
    O.put_u64 buf q.epoch_id)

let decode_epoch_root_query payload =
  O.decode (fun c ->
    let chain_id = get_string max_chain_id_bytes c in
    let epoch_id = O.get_u64 c in
    { chain_id; epoch_id }) payload

let encode_epoch_root_response (r : epoch_root_response) =
  O.encode (fun buf ->
    O.put_string buf r.chain_id;
    O.put_u64 buf r.epoch_id;
    O.put_option O.put_hash32 buf r.state_root;
    O.put_string buf r.responder_addr;
    O.put_u64 buf r.responder_head_epoch;
    O.put_string buf r.signature)

let decode_epoch_root_response payload =
  O.decode (fun c ->
    let chain_id = get_string max_chain_id_bytes c in
    let epoch_id = O.get_u64 c in
    let state_root = O.get_option O.get_hash32 c in
    let responder_addr = get_string max_addr_bytes c in
    let responder_head_epoch = O.get_u64 c in
    let signature = get_exact_string max_signature_bytes c in
    { chain_id; epoch_id; state_root; responder_addr; responder_head_epoch; signature }) payload

type bundle_query = {
  chain_id : string;
  epoch_id : int64;
  proposal_id : string;
}

type bundle_response = {
  chain_id : string;
  epoch_id : int64;
  proposal_id : string;
  responder_addr : string;
  tx_hashes : string list;
  txs_json : string list;
  receipts_json : string list;
  signature : string;
}

let encode_bundle_query (q : bundle_query) =
  O.encode (fun buf ->
    O.put_string buf q.chain_id;
    O.put_u64 buf q.epoch_id;
    O.put_hash32 buf q.proposal_id)

let decode_bundle_query payload =
  O.decode (fun c ->
    let chain_id = get_string max_chain_id_bytes c in
    let epoch_id = O.get_u64 c in
    let proposal_id = O.get_hash32 c in
    { chain_id; epoch_id; proposal_id }) payload

let encode_bundle_response (r : bundle_response) =
  O.encode (fun buf ->
    O.put_string buf r.chain_id;
    O.put_u64 buf r.epoch_id;
    O.put_hash32 buf r.proposal_id;
    O.put_string buf r.responder_addr;
    O.put_list O.put_hash32 buf r.tx_hashes;
    O.put_list O.put_string buf r.txs_json;
    O.put_list O.put_string buf r.receipts_json;
    O.put_string buf r.signature)

let decode_bundle_response payload =
  O.decode (fun c ->
    let chain_id = get_string max_chain_id_bytes c in
    let epoch_id = O.get_u64 c in
    let proposal_id = O.get_hash32 c in
    let responder_addr = get_string max_addr_bytes c in
    let tx_hashes = get_tx_hashes c in
    let txs_json = get_json_list c in
    let receipts_json = get_json_list c in
    let signature = get_exact_string max_signature_bytes c in
    { chain_id; epoch_id; proposal_id; responder_addr; tx_hashes; txs_json;
      receipts_json; signature }) payload

type catchup_epoch_record = {
  epoch_id : int64;
  prev_state_root : string;
  state_root : string;
  tx_list_hash : string;
  tx_hashes : string list;
  txs_json : string list;
  receipt_root : string;
  receipts_json : string list;
  epoch_ts : float;
  creator_addr : string;
  commit_round : int;
  reward_source : reward_source option;
  finality : catchup_finality option;
}

and catchup_finality = {
  finalize : finalize;
  validator_set : validator_set;
}

type catchup_query_range = {
  chain_id : string;
  request_id : string;
  from_epoch : int64;
  max_epochs : int;
}

type catchup_range_response = {
  chain_id : string;
  request_id : string;
  status : string;
  error_code : string option;
  records : catchup_epoch_record list;
  next_epoch : int64 option;
  responder_addr : string;
  signature : string;
}

let encode_catchup_epoch_record_v1 buf (r : catchup_epoch_record) =
  O.put_u64 buf r.epoch_id;
  O.put_hash32 buf r.prev_state_root;
  O.put_hash32 buf r.state_root;
  O.put_hash32 buf r.tx_list_hash;
  O.put_list O.put_hash32 buf r.tx_hashes;
  O.put_list O.put_string buf r.txs_json

let empty_receipt_root =
  Octra_net.Hash_domain.hash_encoded "octra:preverify_receipt_root:v1" (fun buf ->
    O.put_list O.put_string buf [])

let decode_catchup_epoch_record_v1 c =
  let epoch_id = O.get_u64 c in
  let prev_state_root = O.get_hash32 c in
  let state_root = O.get_hash32 c in
  let tx_list_hash = O.get_hash32 c in
  let tx_hashes = get_tx_hashes c in
  let txs_json = get_json_list c in
  { epoch_id; prev_state_root; state_root; tx_list_hash; tx_hashes; txs_json;
    receipt_root = empty_receipt_root; receipts_json = [];
    epoch_ts = Int64.to_float epoch_id *. 10.;
    creator_addr = ""; commit_round = 0; reward_source = None;
    finality = None }

let encode_catchup_epoch_record_v2 buf (r : catchup_epoch_record) =
  O.put_u64 buf r.epoch_id;
  O.put_hash32 buf r.prev_state_root;
  O.put_hash32 buf r.state_root;
  O.put_hash32 buf r.tx_list_hash;
  O.put_list O.put_hash32 buf r.tx_hashes;
  O.put_list O.put_string buf r.txs_json;
  O.put_hash32 buf r.receipt_root;
  O.put_list O.put_string buf r.receipts_json;
  O.put_u64 buf (Int64.bits_of_float r.epoch_ts);
  O.put_string buf r.creator_addr;
  O.put_u32_int buf r.commit_round;
  begin
    match r.reward_source with
    | Some source -> C_reward_source.encode_into buf source
    | None -> invalid_arg "catchup reward source is missing"
  end;
  match r.finality with
  | Some finality ->
    O.put_bytes buf (encode_finalize finality.finalize);
    O.put_bytes buf (encode_validator_set finality.validator_set)
  | None -> invalid_arg "catchup finality is missing"

let decode_catchup_epoch_record_v2 c =
  let epoch_id = O.get_u64 c in
  let prev_state_root = O.get_hash32 c in
  let state_root = O.get_hash32 c in
  let tx_list_hash = O.get_hash32 c in
  let tx_hashes = get_tx_hashes c in
  let txs_json = get_json_list c in
  let receipt_root = O.get_hash32 c in
  let receipts_json = get_json_list c in
  let epoch_ts = get_epoch_ts c in
  let creator_addr = get_string max_addr_bytes c in
  let commit_round = get_round c in
  let reward_source = Some (C_reward_source.decode_from c) in
  let finalize =
    O.get_bytes_bounded ~max:(32 * 1024 * 1024) c
    |> decode_finalize
  in
  let validator_set =
    O.get_bytes_bounded ~max:(4 * 1024 * 1024) c
    |> decode_validator_set
  in
  { epoch_id; prev_state_root; state_root; tx_list_hash; tx_hashes; txs_json;
    receipt_root; receipts_json; epoch_ts; creator_addr; commit_round;
    reward_source; finality = Some { finalize; validator_set } }

let encode_catchup_query_range (q : catchup_query_range) =
  O.encode (fun buf ->
    O.put_string buf q.chain_id;
    O.put_string buf q.request_id;
    O.put_u64 buf q.from_epoch;
    O.put_u32_int buf q.max_epochs)

let decode_catchup_query_range payload =
  O.decode (fun c ->
    let chain_id = get_string max_chain_id_bytes c in
    let request_id = get_string max_request_id_bytes c in
    let from_epoch = O.get_u64 c in
    let max_epochs = O.get_u32_int c in
    if max_epochs <= 0 || max_epochs > max_catchup_records then
      failwith "consensus codec: catchup range out of bounds";
    { chain_id; request_id; from_epoch; max_epochs }) payload

let encode_catchup_range_response_v1 (r : catchup_range_response) =
  O.encode (fun buf ->
    O.put_string buf r.chain_id;
    O.put_string buf r.request_id;
    O.put_string buf r.status;
    O.put_option O.put_string buf r.error_code;
    O.put_list encode_catchup_epoch_record_v1 buf r.records;
    O.put_option O.put_u64 buf r.next_epoch;
    O.put_string buf r.responder_addr;
    O.put_string buf r.signature)

let decode_catchup_range_response_v1 payload =
  O.decode (fun c ->
    let chain_id = get_string max_chain_id_bytes c in
    let request_id = get_string max_request_id_bytes c in
    let status = get_string max_status_bytes c in
    let error_code = O.get_option (get_string max_error_bytes) c in
    let records = O.get_list_bounded ~max:max_catchup_records decode_catchup_epoch_record_v1 c in
    let next_epoch = O.get_option O.get_u64 c in
    let responder_addr = get_string max_addr_bytes c in
    let signature = get_exact_string max_signature_bytes c in
    { chain_id; request_id; status; error_code; records; next_epoch; responder_addr; signature }) payload

let encode_catchup_range_response_v2 (r : catchup_range_response) =
  O.encode (fun buf ->
    O.put_string buf r.chain_id;
    O.put_string buf r.request_id;
    O.put_string buf r.status;
    O.put_option O.put_string buf r.error_code;
    O.put_list encode_catchup_epoch_record_v2 buf r.records;
    O.put_option O.put_u64 buf r.next_epoch;
    O.put_string buf r.responder_addr;
    O.put_string buf r.signature)

let decode_catchup_range_response_v2 payload =
  O.decode (fun c ->
    let chain_id = get_string max_chain_id_bytes c in
    let request_id = get_string max_request_id_bytes c in
    let status = get_string max_status_bytes c in
    let error_code = O.get_option (get_string max_error_bytes) c in
    let records = O.get_list_bounded ~max:max_catchup_records decode_catchup_epoch_record_v2 c in
    let next_epoch = O.get_option O.get_u64 c in
    let responder_addr = get_string max_addr_bytes c in
    let signature = get_exact_string max_signature_bytes c in
    { chain_id; request_id; status; error_code; records; next_epoch; responder_addr; signature }) payload