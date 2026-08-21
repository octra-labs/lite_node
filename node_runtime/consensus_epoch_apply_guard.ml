(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Ledger = Octra_core.Ledger
module Store_irmin = Octra_core.Store_irmin

type prev_root_check =
  | Prev_root_ok
  | Prev_root_mismatch of {
      local_raw : string;
      expected_prev : string;
    }

type prev_root_result =
  | Prev_root_checked
  | Prev_root_exited

type prev_root_effects = {
  fatal : string -> unit;
  require_sync : Sync_need.t -> unit;
  exit : unit -> unit;
}

type prev_root_context = {
  epoch_id : int;
  local_pre_root : string;
  expected_prev : string option;
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

type post_root_result =
  | Post_root_checked
  | Post_root_exited

type post_root_effects = {
  warn : string -> unit;
  error : string -> unit;
  fatal : string -> unit;
  require_sync : Sync_need.t -> unit;
  remove_expected_root : int -> unit;
  prune_expected_roots_before : int -> unit;
  exit : unit -> unit;
}

type post_root_context = {
  consensus_mode : bool;
  layera_diag : bool;
  epoch_id : int;
  current_epoch : int;
  actual_root : string;
  expected_root : string option;
  proposer_source : string;
  proposer : string;
  proposer_short : string;
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
  current_epoch_meta : string;
  last_epoch_meta : string;
  total_supply_meta : string;
  emission_remaining_meta : string;
  account_addrs : string list;
  short : string -> string;
  find_account : string -> account_view option;
}

type post_root_context_input = {
  consensus_mode : bool;
  layera_diag : bool;
  epoch_id : int;
  current_epoch : int;
  actual_root : string;
  expected_root : string option;
  proposer_source : string;
  proposer : string;
  round : int;
  validators : int;
  validators_sha : string;
  tx_count : int;
  plan : Octra_core.Epoch_exec.reward_plan;
  prev_supply : Z.t;
  emission_remaining : Z.t;
  confirmed_fees : Z.t;
  replay_subtrees : (string * string) list;
  layera_meta_pairs : (string * string) list;
  root : string;
  account_addrs : string list;
  short : string -> string;
  find_account : string -> account_view option;
}

type stage_batch_effects = {
  trace : string -> unit;
  warn : string -> unit;
  level_output : string -> unit;
  replay_output : string -> unit;
  write_marker : int -> string -> unit;
  set_meta : key:string -> value:string -> unit Lwt.t;
  flush_dirty : unit -> unit Lwt.t;
  dump_subtrees : unit -> (string * string) list Lwt.t;
  dump_meta : unit -> (string * string) list Lwt.t;
  batch_root : unit -> string option Lwt.t;
  fallback_root : unit -> string Lwt.t;
}

type stage_batch_live_effects = {
  data_dir : string;
  store : Store_irmin.t;
  ledger : Ledger.t;
  trace : string -> unit;
  warn : string -> unit;
  level_output : string -> unit;
  replay_output : string -> unit;
}

type stage_batch_context = {
  epoch_id : int;
  current_epoch : int;
  replay_trace : bool;
  layera_diag : bool;
  txs_in : int;
  confirmed : int;
  fees : string;
  base : string;
}

type stage_batch_result = {
  post_state_hash : string;
  replay_subtrees : (string * string) list;
  layera_meta_pairs : (string * string) list;
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

let account_view_of_ledger_account a =
  {
    balance = Z.to_string a.Ledger.balance;
    nonce = a.Ledger.nonce;
    pub = a.Ledger.public_key;
    enc = a.Ledger.encrypted_balance;
    decrypt_allowance = Z.to_string a.Ledger.decrypt_allowance;
  }

let post_root_context_of_input i =
  let plan = i.plan in
  {
    consensus_mode = i.consensus_mode;
    layera_diag = i.layera_diag;
    epoch_id = i.epoch_id;
    current_epoch = i.current_epoch;
    actual_root = i.actual_root;
    expected_root = i.expected_root;
    proposer_source = i.proposer_source;
    proposer = i.proposer;
    proposer_short = i.short i.proposer;
    round = i.round;
    validators = i.validators;
    validators_sha = i.validators_sha;
    tx_count = i.tx_count;
    base_reward = Z.to_string plan.Octra_core.Epoch_exec.base_reward;
    total_reward = Z.to_string plan.total_reward;
    proposer_total = Z.to_string plan.proposer_total;
    each_validator = Z.to_string plan.each_validator;
    remainder = Z.to_string plan.remainder;
    prev_supply = Z.to_string i.prev_supply;
    emission_remaining = Z.to_string i.emission_remaining;
    next_supply = Z.to_string plan.new_total_supply;
    next_emission = Z.to_string plan.new_emission_remaining;
    fees = Z.to_string i.confirmed_fees;
    accounts_hash = assoc_or_none "accounts" i.replay_subtrees;
    meta_hash = assoc_or_none "meta" i.replay_subtrees;
    root = i.root;
    current_epoch_meta = assoc_or_none "current_epoch" i.layera_meta_pairs;
    last_epoch_meta = assoc_or_none "last_epoch" i.layera_meta_pairs;
    total_supply_meta = assoc_or_none "total_supply" i.layera_meta_pairs;
    emission_remaining_meta =
      assoc_or_none "emission_remaining" i.layera_meta_pairs;
    account_addrs = i.account_addrs;
    short = i.short;
    find_account = i.find_account;
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

let run_prev_root_check (effects : prev_root_effects) context =
  match context.expected_prev with
  | None -> Prev_root_checked
  | Some expected_prev ->
    match check_prev_root
            ~local_pre_root:context.local_pre_root
            ~expected_prev with
    | Prev_root_ok -> Prev_root_checked
    | Prev_root_mismatch { local_raw; expected_prev } ->
      effects.fatal
        (mismatch_line
           ~epoch_id:context.epoch_id
           ~local_raw
           ~expected_prev);
      effects.require_sync
        (Sync_need.root
           ~epoch:context.epoch_id
           ~head:(max 0 (context.epoch_id - 1)));
      effects.exit ();
      Prev_root_exited

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

let live_stage_batch_effects (live : stage_batch_live_effects) =
  {
    trace = live.trace;
    warn = live.warn;
    level_output = live.level_output;
    replay_output = live.replay_output;
    write_marker = Octra_core.Epoch_commit_marker.write_marker live.data_dir;
    set_meta = (fun ~key ~value -> Store_irmin.set_meta live.store key value);
    flush_dirty = (fun () -> Ledger.flush_dirty_lwt live.ledger);
    dump_subtrees = (fun () -> Store_irmin.dump_batch_subtree_hashes live.store);
    dump_meta = (fun () -> Store_irmin.dump_batch_meta_pairs live.store);
    batch_root = (fun () -> Store_irmin.get_batch_tree_hash live.store);
    fallback_root = (fun () -> Ledger.hash live.ledger);
  }

let run_stage_batch (effects : stage_batch_effects) context =
  let open Lwt.Syntax in
  effects.trace "event = stage_batch_set_meta";
  effects.write_marker context.epoch_id "stage_batch_begin";
  let* () =
    effects.set_meta
      ~key:"last_epoch"
      ~value:(string_of_int context.epoch_id)
  in
  let* () =
    effects.set_meta
      ~key:"current_epoch"
      ~value:(string_of_int context.current_epoch)
  in
  effects.trace "event = stage_batch_flush_dirty";
  let* () = effects.flush_dirty () in
  let* replay_subtrees =
    if context.replay_trace || context.layera_diag then
      effects.dump_subtrees ()
    else
      Lwt.return []
  in
  let* layera_meta_pairs =
    if context.layera_diag then
      effects.dump_meta ()
    else
      Lwt.return []
  in
  if context.confirmed > 0 || context.txs_in > 0 then
    effects.level_output
      (level_trace_line
         ~epoch_id:context.epoch_id
         ~txs_in:context.txs_in
         ~confirmed:context.confirmed
         ~fees:context.fees
         ~base:context.base);
  let* post_state_hash =
    let* batch_opt = effects.batch_root () in
    match batch_opt with
    | Some root -> Lwt.return root
    | None ->
      effects.warn missing_batch_tree_line;
      effects.fallback_root ()
  in
  if context.replay_trace then
    effects.replay_output
      (replay_post_line
         ~epoch_id:context.epoch_id
         ~base:context.base
         ~fees:context.fees
         ~accounts:(assoc_or_none "accounts" replay_subtrees)
         ~meta:(assoc_or_none "meta" replay_subtrees)
         ~root:post_state_hash);
  Lwt.return {
    post_state_hash;
    replay_subtrees;
    layera_meta_pairs;
  }

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

let post_root_diag_lines (d : post_root_diag) =
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
    "event = recovery action = signed_snapshot";
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

let post_root_diag_of_context (ctx : post_root_context) ~expected_root
    ~actual_root =
  post_root_diag_lines {
    epoch_id = ctx.epoch_id;
    proposer_source = ctx.proposer_source;
    proposer = ctx.proposer_short;
    round = ctx.round;
    validators = ctx.validators;
    validators_sha = ctx.validators_sha;
    tx_count = ctx.tx_count;
    base_reward = ctx.base_reward;
    total_reward = ctx.total_reward;
    proposer_total = ctx.proposer_total;
    each_validator = ctx.each_validator;
    remainder = ctx.remainder;
    prev_supply = ctx.prev_supply;
    emission_remaining = ctx.emission_remaining;
    next_supply = ctx.next_supply;
    next_emission = ctx.next_emission;
    fees = ctx.fees;
    accounts_hash = ctx.accounts_hash;
    meta_hash = ctx.meta_hash;
    root = ctx.root;
    expected_root;
    actual_root;
    current_epoch_meta = ctx.current_epoch_meta;
    last_epoch_meta = ctx.last_epoch_meta;
    total_supply_meta = ctx.total_supply_meta;
    emission_remaining_meta = ctx.emission_remaining_meta;
    accounts =
      List.map
        (account_diag ~short:ctx.short ~find_account:ctx.find_account)
        ctx.account_addrs;
  }

let post_root_fatal_of_context (ctx : post_root_context) ~expected_root
    ~actual_root =
  post_root_mismatch_fatal_lines
    ~epoch_id:ctx.epoch_id
    ~expected_root
    ~actual_root
    ~proposer:ctx.proposer_short
    ~tx_count:ctx.tx_count

let run_post_root_check effects (ctx : post_root_context) =
  let action =
    post_root_action
      ~consensus_mode:ctx.consensus_mode
      ~layera_diag:ctx.layera_diag
      ~epoch_id:ctx.epoch_id
      ~actual_root:ctx.actual_root
      ~expected_root:ctx.expected_root
      ~diag_lines:(post_root_diag_of_context ctx)
      ~fatal_lines:(post_root_fatal_of_context ctx)
  in
  match action with
  | Post_root_continue None ->
    effects.remove_expected_root ctx.epoch_id;
    effects.prune_expected_roots_before (ctx.current_epoch - 32);
    Post_root_checked
  | Post_root_continue (Some line) ->
    effects.warn line;
    effects.remove_expected_root ctx.epoch_id;
    effects.prune_expected_roots_before (ctx.current_epoch - 32);
    Post_root_checked
  | Post_root_fail { diag_lines; fatal_lines } ->
    List.iter effects.error diag_lines;
    List.iter effects.fatal fatal_lines;
    effects.require_sync
      (Sync_need.root
         ~epoch:ctx.epoch_id
         ~head:(max 0 (ctx.epoch_id - 1)));
    effects.exit ();
    Post_root_exited

let run_post_root_input effects input =
  run_post_root_check effects (post_root_context_of_input input)