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


open C_types
module O = Octra_net.Oce1

let encode_epoch_header buf (h : epoch_header) =
  O.put_u16 buf h.proto_version;
  O.put_string buf h.chain_id;
  O.put_u64 buf h.epoch_id;
  O.put_hash32 buf h.prev_state_root;
  O.put_hash32 buf h.tx_list_hash;
  O.put_hash32 buf h.receipt_root;
  O.put_hash32 buf h.proposed_state_root;
  O.put_string buf h.creator_addr;
  O.put_u64 buf h.txid_hi;
  O.put_u64 buf (Int64.bits_of_float h.ts)

let decode_epoch_header c =
  let proto_version = O.get_u16 c in
  if proto_version <> proto_version_current then
    failwith (Printf.sprintf "epoch_header proto_version mismatch: got %d expected %d"
                proto_version proto_version_current);
  let chain_id = O.get_string c in
  let epoch_id = O.get_u64 c in
  let prev_state_root = O.get_hash32 c in
  let tx_list_hash = O.get_hash32 c in
  let receipt_root = O.get_hash32 c in
  let proposed_state_root = O.get_hash32 c in
  let creator_addr = O.get_string c in
  let txid_hi = O.get_u64 c in
  let ts = Int64.float_of_bits (O.get_u64 c) in
  { proto_version; chain_id; epoch_id; prev_state_root; tx_list_hash; receipt_root;
    proposed_state_root; creator_addr; txid_hi; ts }

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
  let c = O.make_cursor payload in
  let chain_id = O.get_string c in
  let epoch_id = O.get_u64 c in
  let round = O.get_u32_int c in
  let vote_type = vote_type_of_u8 (O.get_u8 c) in
  let proposal_id = O.get_hash32 c in
  let validator = O.get_string c in
  let signature = O.get_bytes c in
  { chain_id; epoch_id; round; vote_type; proposal_id; validator; signature }

let encode_propose (p : propose) =
  O.encode (fun buf ->
    O.put_string buf p.chain_id;
    O.put_u64 buf p.epoch_id;
    O.put_u32_int buf p.round;
    O.put_option O.put_u32_int buf p.valid_round;
    encode_epoch_header buf p.header;
    O.put_list O.put_hash32 buf p.tx_hashes;
    O.put_string buf p.proposer;
    O.put_bytes buf p.signature)

let decode_propose payload =
  let c = O.make_cursor payload in
  let chain_id = O.get_string c in
  let epoch_id = O.get_u64 c in
  let round = O.get_u32_int c in
  let valid_round = O.get_option O.get_u32_int c in
  let header = decode_epoch_header c in
  let tx_hashes = O.get_list O.get_hash32 c in
  let proposer = O.get_string c in
  let signature = O.get_bytes c in
  { chain_id; epoch_id; round; valid_round; header; tx_hashes; proposer; signature }

let encode_finalize (f : finalize) =
  O.encode (fun buf ->
    O.put_string buf f.chain_id;
    O.put_u64 buf f.epoch_id;
    O.put_u32_int buf f.commit_round;
    encode_epoch_header buf f.header;
    O.put_hash32 buf f.proposal_id;
    O.put_list (fun buf (v : vote) ->
      O.put_string buf v.chain_id;
      O.put_u64 buf v.epoch_id;
      O.put_u32_int buf v.round;
      O.put_u8 buf (vote_type_to_u8 v.vote_type);
      O.put_hash32 buf v.proposal_id;
      O.put_string buf v.validator;
      O.put_bytes buf v.signature
    ) buf f.precommits)

let decode_finalize payload =
  let c = O.make_cursor payload in
  let chain_id = O.get_string c in
  let epoch_id = O.get_u64 c in
  let commit_round = O.get_u32_int c in
  let header = decode_epoch_header c in
  let proposal_id = O.get_hash32 c in
  let precommits = O.get_list (fun c ->
    let chain_id = O.get_string c in
    let epoch_id = O.get_u64 c in
    let round = O.get_u32_int c in
    let vote_type = vote_type_of_u8 (O.get_u8 c) in
    let proposal_id = O.get_hash32 c in
    let validator = O.get_string c in
    let signature = O.get_bytes c in
    { chain_id; epoch_id; round; vote_type; proposal_id; validator; signature }
  ) c in
  { chain_id; epoch_id; commit_round; header; proposal_id; precommits }

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
  let c = O.make_cursor payload in
  let chain_id = O.get_string c in
  let epoch_id = O.get_u64 c in
  { chain_id; epoch_id }

let encode_epoch_root_response (r : epoch_root_response) =
  O.encode (fun buf ->
    O.put_string buf r.chain_id;
    O.put_u64 buf r.epoch_id;
    O.put_option O.put_hash32 buf r.state_root;
    O.put_string buf r.responder_addr;
    O.put_u64 buf r.responder_head_epoch;
    O.put_string buf r.signature)

let decode_epoch_root_response payload =
  let c = O.make_cursor payload in
  let chain_id = O.get_string c in
  let epoch_id = O.get_u64 c in
  let state_root = O.get_option O.get_hash32 c in
  let responder_addr = O.get_string c in
  let responder_head_epoch = O.get_u64 c in
  let signature = O.get_string c in
  { chain_id; epoch_id; state_root; responder_addr; responder_head_epoch; signature }

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
}

let encode_bundle_query (q : bundle_query) =
  O.encode (fun buf ->
    O.put_string buf q.chain_id;
    O.put_u64 buf q.epoch_id;
    O.put_hash32 buf q.proposal_id)

let decode_bundle_query payload =
  let c = O.make_cursor payload in
  let chain_id = O.get_string c in
  let epoch_id = O.get_u64 c in
  let proposal_id = O.get_hash32 c in
  { chain_id; epoch_id; proposal_id }

let encode_bundle_response (r : bundle_response) =
  O.encode (fun buf ->
    O.put_string buf r.chain_id;
    O.put_u64 buf r.epoch_id;
    O.put_hash32 buf r.proposal_id;
    O.put_string buf r.responder_addr;
    O.put_list O.put_hash32 buf r.tx_hashes;
    O.put_list O.put_string buf r.txs_json;
    O.put_list O.put_string buf r.receipts_json)

let decode_bundle_response payload =
  let c = O.make_cursor payload in
  let chain_id = O.get_string c in
  let epoch_id = O.get_u64 c in
  let proposal_id = O.get_hash32 c in
  let responder_addr = O.get_string c in
  let tx_hashes = O.get_list O.get_hash32 c in
  let txs_json = O.get_list O.get_string c in
  let receipts_json = O.get_list O.get_string c in
  { chain_id; epoch_id; proposal_id; responder_addr; tx_hashes; txs_json; receipts_json }

type catchup_epoch_record = {
  epoch_id : int64;
  prev_state_root : string;
  state_root : string;
  tx_list_hash : string;
  tx_hashes : string list;
  txs_json : string list;
  receipt_root : string;
  receipts_json : string list;
  creator_addr : string;
  commit_round : int;
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
  let tx_hashes = O.get_list O.get_hash32 c in
  let txs_json = O.get_list O.get_string c in
  { epoch_id; prev_state_root; state_root; tx_list_hash; tx_hashes; txs_json;
    receipt_root = empty_receipt_root; receipts_json = [];
    creator_addr = ""; commit_round = 0 }

let encode_catchup_epoch_record_v2 buf (r : catchup_epoch_record) =
  O.put_u64 buf r.epoch_id;
  O.put_hash32 buf r.prev_state_root;
  O.put_hash32 buf r.state_root;
  O.put_hash32 buf r.tx_list_hash;
  O.put_list O.put_hash32 buf r.tx_hashes;
  O.put_list O.put_string buf r.txs_json;
  O.put_hash32 buf r.receipt_root;
  O.put_list O.put_string buf r.receipts_json;
  O.put_string buf r.creator_addr;
  O.put_u32_int buf r.commit_round

let decode_catchup_epoch_record_v2 c =
  let epoch_id = O.get_u64 c in
  let prev_state_root = O.get_hash32 c in
  let state_root = O.get_hash32 c in
  let tx_list_hash = O.get_hash32 c in
  let tx_hashes = O.get_list O.get_hash32 c in
  let txs_json = O.get_list O.get_string c in
  let receipt_root = O.get_hash32 c in
  let receipts_json = O.get_list O.get_string c in
  let creator_addr = O.get_string c in
  let commit_round = O.get_u32_int c in
  { epoch_id; prev_state_root; state_root; tx_list_hash; tx_hashes; txs_json;
    receipt_root; receipts_json; creator_addr; commit_round }

let encode_catchup_query_range (q : catchup_query_range) =
  O.encode (fun buf ->
    O.put_string buf q.chain_id;
    O.put_string buf q.request_id;
    O.put_u64 buf q.from_epoch;
    O.put_u32_int buf q.max_epochs)

let decode_catchup_query_range payload =
  let c = O.make_cursor payload in
  let chain_id = O.get_string c in
  let request_id = O.get_string c in
  let from_epoch = O.get_u64 c in
  let max_epochs = O.get_u32_int c in
  { chain_id; request_id; from_epoch; max_epochs }

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
  let c = O.make_cursor payload in
  let chain_id = O.get_string c in
  let request_id = O.get_string c in
  let status = O.get_string c in
  let error_code = O.get_option O.get_string c in
  let records = O.get_list decode_catchup_epoch_record_v1 c in
  let next_epoch = O.get_option O.get_u64 c in
  let responder_addr = O.get_string c in
  let signature = O.get_string c in
  { chain_id; request_id; status; error_code; records; next_epoch; responder_addr; signature }

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
  let c = O.make_cursor payload in
  let chain_id = O.get_string c in
  let request_id = O.get_string c in
  let status = O.get_string c in
  let error_code = O.get_option O.get_string c in
  let records = O.get_list decode_catchup_epoch_record_v2 c in
  let next_epoch = O.get_option O.get_u64 c in
  let responder_addr = O.get_string c in
  let signature = O.get_string c in
  { chain_id; request_id; status; error_code; records; next_epoch; responder_addr; signature }