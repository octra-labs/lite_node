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


let proto_version_current = 3

type epoch_header = {
  proto_version : int;
  chain_id : string;
  epoch_id : int64;
  prev_state_root : string;
  tx_list_hash : string;
  receipt_root : string;
  proposed_state_root : string;
  creator_addr : string;
  txid_hi : int64;
  ts : float;
}

type vote_type = Prevote | Precommit

type vote = {
  chain_id : string;
  epoch_id : int64;
  round : int;
  vote_type : vote_type;
  proposal_id : string;
  validator : string;
  signature : string;
}

type propose = {
  chain_id : string;
  epoch_id : int64;
  round : int;
  valid_round : int option;
  header : epoch_header;
  tx_hashes : string list;
  proposer : string;
  signature : string;
}

type finalize = {
  chain_id : string;
  epoch_id : int64;
  commit_round : int;
  header : epoch_header;
  proposal_id : string;
  precommits : vote list;
}

type proof_kind = Zero | Bound | Range

type proof_cert = {
  task_id : string;
  result : bool;
  validator : string;
  signature : string;
}

type proof_qc = {
  task_id : string;
  result : bool;
  certs : (string * string) list;
}

type round_step = ProposeStep | PrevoteStep | PrecommitStep

type engine_state = {
  height : int64;
  round : int;
  step : round_step;
  locked_round : int;
  locked_value : epoch_header option;
  valid_round : int;
  valid_value : epoch_header option;
}

let initial_engine_state height = {
  height;
  round = 0;
  step = ProposeStep;
  locked_round = -1;
  locked_value = None;
  valid_round = -1;
  valid_value = None;
}

let vote_type_to_u8 = function Prevote -> 1 | Precommit -> 2
let vote_type_of_u8 = function 1 -> Prevote | 2 -> Precommit | _ -> failwith "bad vote_type"

let proof_kind_to_u8 = function Zero -> 1 | Bound -> 2 | Range -> 3
let proof_kind_of_u8 = function 1 -> Zero | 2 -> Bound | 3 -> Range | _ -> failwith "bad proof_kind"

let round_step_to_u8 = function ProposeStep -> 1 | PrevoteStep -> 2 | PrecommitStep -> 3

type validator_info = {
  address : string;
  pubkey : string;
}

type validator_set = {
  validators : validator_info list;
  n : int;
  f : int;
  quorum : int;
}

let make_validator_set (vals : validator_info list) =
  let sorted = List.sort (fun a b -> String.compare a.address b.address) vals in
  let n = List.length sorted in
  let f = (n - 1) / 3 in
  let quorum =
    if n <= 0 then 0
    else if n < 4 then n
    else 2 * f + 1 in
  { validators = sorted; n; f; quorum }

let pubkey_of_addr (vs : validator_set) (addr : string) : string option =
  match List.find_opt (fun v -> v.address = addr) vs.validators with
  | Some v -> Some v.pubkey
  | None -> None

let is_validator (vs : validator_set) (addr : string) : bool =
  List.exists (fun v -> v.address = addr) vs.validators

let leader_of (vs : validator_set) ~epoch_id ~round =
  let idx = Int64.to_int (Int64.rem epoch_id (Int64.of_int vs.n)) in
  let idx = (idx + round) mod vs.n in
  List.nth vs.validators idx