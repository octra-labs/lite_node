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


type prev_root_check =
  | Prev_root_ok
  | Prev_root_mismatch of {
      local_raw : string;
      expected_prev : string;
    }

type post_root_check =
  | Post_root_ok
  | Post_root_missing
  | Post_root_mismatch of {
      actual_raw : string;
      expected_root : string;
    }

type account_diag = {
  addr : string;
  balance : string;
  nonce : int;
  pub : string;
  enc : string;
  decrypt_allowance : string;
}

type account_view = {
  balance : string;
  nonce : int;
  pub : string option;
  enc : string option;
  decrypt_allowance : string;
}

type account_diag_row =
  | Account_missing of string
  | Account_present of account_diag

type post_root_diag = {
  epoch_id : int;
  proposer_source : string;
  proposer : string;
  round : int;
  validators : int;
  validators_sha : string;
  tx_count : int;
  base_reward : string;
  total_reward : string;
  proposer_total : string;
  each_validator : string;
  remainder : string;
  prev_supply : string;
  emission_remaining : string;
  next_supply : string;
  next_emission : string;
  fees : string;
  accounts_hash : string;
  meta_hash : string;
  root : string;
  expected_root : string;
  actual_root : string;
  current_epoch_meta : string;
  last_epoch_meta : string;
  total_supply_meta : string;
  emission_remaining_meta : string;
  accounts : account_diag_row list;
}

type post_root_action =
  | Post_root_continue of string option
  | Post_root_fail of {
      diag_lines : string list;
      fatal_lines : string list;
    }

let zero32 = String.make 32 '\x00'

let raw32_of_hex64 hex =
  String.init 32 (fun i ->
    Char.chr (int_of_string ("0x" ^ String.sub hex (i * 2) 2)))

let raw32_of_pre_root root =
  if String.length root >= 64 then raw32_of_hex64 root
  else if String.length root = 32 then root
  else zero32

let raw_hex8 raw =
  String.concat ""
    (List.init
       (min 8 (String.length raw))
       (fun i -> Printf.sprintf "%02x" (Char.code raw.[i])))

let assoc_or_none key values =
  match List.assoc_opt key values with
  | Some value -> value
  | None -> "<none>"

let short_field n value =
  if String.length value > n then String.sub value 0 n else value

let account_diag ~short ~find_account raw_addr =
  let addr = short raw_addr in
  match find_account raw_addr with
  | None ->
    Account_missing addr
  | Some a ->
    Account_present {
      addr;
      balance = a.balance;
      nonce = a.nonce;
      pub = Option.fold
        ~none:"<none>"
        ~some:(short_field 24)
        a.pub;
      enc = Option.fold
        ~none:"<none>"
        ~some:(short_field 24)
        a.enc;
      decrypt_allowance = a.decrypt_allowance;
    }

let check_prev_root ~local_pre_root ~expected_prev =
  if String.length expected_prev <> 32 || expected_prev = zero32 then
    Prev_root_ok
  else
    let local_raw = raw32_of_pre_root local_pre_root in
    if local_raw = expected_prev then
      Prev_root_ok
    else
      Prev_root_mismatch { local_raw; expected_prev }

let check_post_root ~actual_root ~expected_root =
  if String.length expected_root <> 32 || expected_root = zero32 then
    Post_root_missing
  else
    let actual_raw = raw32_of_pre_root actual_root in
    if actual_raw = expected_root then
      Post_root_ok
    else
      Post_root_mismatch { actual_raw; expected_root }

let mismatch_line ~epoch_id ~local_raw ~expected_prev =
  Printf.sprintf
    "layer_a_guard = pre_root_mismatch epoch = %d local_pre = %s header_prev = %s action = exit reason = wrong_base_state"
    epoch_id
    (raw_hex8 local_raw)
    (raw_hex8 expected_prev)

let post_root_missing_line epoch_id =
  Printf.sprintf
    "event = pending_expected_roots status = miss epoch = %d armed = false reason = bootstrap_or_upstream_catchup"
    epoch_id

let level_trace_line ~epoch_id ~txs_in ~confirmed ~fees ~base =
  Printf.sprintf
    "event = level_trace epoch = %d txs_in = %d confirmed = %d fees = %s base = %s"
    epoch_id
    txs_in
    confirmed
    fees
    base

let replay_post_line ~epoch_id ~base ~fees ~accounts ~meta ~root =
  Printf.sprintf
    "event = replay_post epoch = %d base = %s fees = %s accounts = %s meta = %s root = %s"
    epoch_id
    base
    fees
    accounts
    meta
    root

let missing_batch_tree_line =
  "event = state_hash fallback = head reason = missing_batch_tree"

let account_diag_line = function
  | Account_missing addr ->
    Printf.sprintf "event = layera_diag_account addr = %s status = missing" addr
  | Account_present a ->
    Printf.sprintf
      "event = layera_diag_account addr = %s balance = %s nonce = %d pub = %s enc = %s decrypt_allowance = %s"
      a.addr
      a.balance
      a.nonce
      a.pub
      a.enc
      a.decrypt_allowance

let post_root_diag_lines d =
  [
    Printf.sprintf
      "event = layera_diag epoch = %d proposer_source = %s proposer = %s round = %d validators = %d validators_sha = %s tx_count = %d"
      d.epoch_id
      d.proposer_source
      d.proposer
      d.round
      d.validators
      d.validators_sha
      d.tx_count;
    Printf.sprintf
      "event = layera_diag_reward base = %s total = %s proposer_total = %s each = %s remainder = %s prev_supply = %s emission_remaining = %s next_supply = %s next_emission = %s fees = %s"
      d.base_reward
      d.total_reward
      d.proposer_total
      d.each_validator
      d.remainder
      d.prev_supply
      d.emission_remaining
      d.next_supply
      d.next_emission
      d.fees;
    Printf.sprintf
      "event = layera_diag_subtrees accounts = %s meta = %s root = %s expected = %s actual = %s"
      d.accounts_hash
      d.meta_hash
      d.root
      d.expected_root
      d.actual_root;
    Printf.sprintf
      "event = layera_diag_meta current_epoch = %s last_epoch = %s total_supply = %s emission_remaining = %s"
      d.current_epoch_meta
      d.last_epoch_meta
      d.total_supply_meta
      d.emission_remaining_meta;
  ] @ List.map account_diag_line d.accounts

let post_root_mismatch_fatal_lines ~epoch_id ~expected_root ~actual_root
    ~proposer ~tx_count =
  [
    Printf.sprintf
      "event = state_root_mismatch epoch = %d expected = %s actual = %s creator = %s tx_count = %d"
      epoch_id
      expected_root
      actual_root
      proposer
      tx_count;
    "event = commit_refused reason = divergent_state";
    "event = recovery action = restart_and_catchup";
  ]

let post_root_action ~consensus_mode ~layera_diag ~epoch_id ~actual_root
    ~expected_root ~diag_lines ~fatal_lines =
  match expected_root with
  | Some expected_root ->
    (match check_post_root ~actual_root ~expected_root with
     | Post_root_ok ->
       Post_root_continue None
     | Post_root_missing ->
       Post_root_continue (
         if consensus_mode then Some (post_root_missing_line epoch_id) else None)
     | Post_root_mismatch { actual_raw; expected_root } ->
       let expected_root = raw_hex8 expected_root in
       let actual_root = raw_hex8 actual_raw in
       Post_root_fail {
         diag_lines =
           if layera_diag then diag_lines ~expected_root ~actual_root else [];
         fatal_lines = fatal_lines ~expected_root ~actual_root;
       })
  | None ->
    Post_root_continue (
      if consensus_mode then Some (post_root_missing_line epoch_id) else None)