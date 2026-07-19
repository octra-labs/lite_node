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


module Private_gate = Consensus_epoch_apply_private_gate
module Private_ledger = Octra_core.Private_ledger
module Transaction = Octra_core.Transaction

type deps = {
  fee : Z.t;
  nonce : int;
  stealth_count : int;
  max_stealth_per_epoch : int;
  max_stealth_defer : int;
  inline_verify_allowed : bool;
  gate_reject : unit -> Private_gate.reject option;
  tx_hash : unit -> string;
  short_hash : string -> string;
  defer_count : string -> int;
  set_defer_count : string -> int -> unit;
  clear_defer_count : string -> unit;
  preverify_state : string -> Preverify_cache.state;
  preverify_remove : string -> unit;
  preverify_ready :
    string ->
    sender_enc_snapshot:string ->
    Preverify_cache.result option;
  log_cap_defer : count:int -> max:int -> tx:string -> unit;
  log_defer : count:int -> max:int -> status:string -> tx:string -> unit;
  log_cache_hit : unit -> unit;
  log_cache_miss_verify : unit -> unit;
  log_cache_miss_disabled : tx:string -> unit;
  defer : unit -> unit Lwt.t;
  reject : string -> string -> unit Lwt.t;
  plan : unit -> (Private_ledger.stealth_plan, Private_ledger.failure) result Lwt.t;
  trace : Private_ledger.stealth_plan -> unit;
  inline_range :
    Private_ledger.stealth_plan ->
    (Private_ledger.stealth_range, Private_ledger.failure) result Lwt.t;
  accept_range : Private_ledger.stealth_range -> (unit, Private_ledger.failure) result;
  binding :
    Private_ledger.stealth_plan ->
    (unit, Private_ledger.failure) result Lwt.t;
  debit : Z.t -> int -> (unit, string) result;
  update : string -> (unit, string) result;
  create_output :
    tx_hash:string ->
    Private_ledger.stealth_plan ->
    (int64, string) result Lwt.t;
  accept : int64 -> Private_ledger.stealth_plan -> unit Lwt.t;
}

type tx_deps = {
  stealth_count : int;
  max_stealth_per_epoch : int;
  max_stealth_defer : int;
  inline_verify_allowed : bool;
  gate_reject : Transaction.t -> Private_gate.reject option;
  short_hash : string -> string;
  defer_count : string -> int;
  set_defer_count : string -> int -> unit;
  clear_defer_count : string -> unit;
  preverify_state : string -> Preverify_cache.state;
  preverify_remove : string -> unit;
  preverify_ready :
    string ->
    sender_enc_snapshot:string ->
    Preverify_cache.result option;
  log_cap_defer : count:int -> max:int -> tx:string -> unit;
  log_defer : count:int -> max:int -> status:string -> tx:string -> unit;
  log_cache_hit : unit -> unit;
  log_cache_miss_verify : unit -> unit;
  log_cache_miss_disabled : tx:string -> unit;
  defer_tx : Transaction.t -> unit Lwt.t;
  reject : string -> string -> unit Lwt.t;
  plan :
    Transaction.t ->
    (Private_ledger.stealth_plan, Private_ledger.failure) result Lwt.t;
  trace : Transaction.t -> Private_ledger.stealth_plan -> unit;
  inline_range :
    Transaction.t ->
    Private_ledger.stealth_plan ->
    (Private_ledger.stealth_range, Private_ledger.failure) result Lwt.t;
  accept_range : Private_ledger.stealth_range -> (unit, Private_ledger.failure) result;
  binding :
    Transaction.t ->
    Private_ledger.stealth_plan ->
    (unit, Private_ledger.failure) result Lwt.t;
  debit : Transaction.t -> Z.t -> int -> (unit, string) result;
  update : Transaction.t -> string -> (unit, string) result;
  create_output :
    tx_hash:string ->
    Transaction.t ->
    Private_ledger.stealth_plan ->
    (int64, string) result Lwt.t;
  accept :
    Transaction.t ->
    int64 ->
    Private_ledger.stealth_plan ->
    unit Lwt.t;
}

type gate_deps = {
  expected_kat : unit -> string;
  stored_kat : string -> string option;
  set_kat : string -> string -> unit;
  log_backfill : string -> unit;
  debit_gate : unit -> Private_gate.reject option;
  fhe_gate : unit -> Private_gate.reject option;
}

type live_tx_args = {
  stealth_count : int;
  max_stealth_per_epoch : int;
  max_stealth_defer : int;
  inline_verify_allowed : bool;
  expected_kat : unit -> string;
  stored_kat : string -> string option;
  set_kat : string -> string -> unit;
  log_backfill : string -> unit;
  debit_gate : unit -> Private_gate.reject option;
  fhe_gate : unit -> Private_gate.reject option;
  defer_count : string -> int;
  set_defer_count : string -> int -> unit;
  clear_defer_count : string -> unit;
  defer_tx : Transaction.t -> unit Lwt.t;
  reject : string -> string -> unit Lwt.t;
  plan :
    Transaction.t ->
    (Private_ledger.stealth_plan, Private_ledger.failure) result Lwt.t;
  trace_cipher : string -> string -> string -> string -> unit;
  inline_range :
    Transaction.t ->
    Private_ledger.stealth_plan ->
    (Private_ledger.stealth_range, Private_ledger.failure) result Lwt.t;
  accept_range : Private_ledger.stealth_range -> (unit, Private_ledger.failure) result;
  binding :
    Transaction.t ->
    Private_ledger.stealth_plan ->
    (unit, Private_ledger.failure) result Lwt.t;
  debit : Transaction.t -> Z.t -> int -> (unit, string) result;
  update : Transaction.t -> string -> (unit, string) result;
  create_output :
    tx_hash:string ->
    Transaction.t ->
    Private_ledger.stealth_plan ->
    (int64, string) result Lwt.t;
  accept :
    Transaction.t ->
    int64 ->
    Private_ledger.stealth_plan ->
    unit Lwt.t;
}

type live_ledger_tx_args = {
  ledger : Octra_core.Ledger.t;
  current_epoch : unit -> int;
  stealth_count : int;
  max_stealth_per_epoch : int;
  max_stealth_defer : int;
  inline_verify_allowed : bool;
  debit_gate : unit -> Private_gate.reject option;
  fhe_gate : unit -> Private_gate.reject option;
  defer_count : string -> int;
  set_defer_count : string -> int -> unit;
  clear_defer_count : string -> unit;
  defer_tx : Transaction.t -> unit Lwt.t;
  reject : string -> string -> unit Lwt.t;
  trace_cipher : string -> string -> string -> string -> unit;
  short_addr : string -> string;
  mark_debit : unit -> unit;
  incr_stealth : unit -> unit;
  incr_fhe : unit -> unit;
  confirm : unit -> unit Lwt.t;
}

let failure_tag (e : Private_ledger.failure) =
  e.tag

let failure_reason (e : Private_ledger.failure) =
  e.reason

let cached_range (pv : Preverify_cache.result) : Private_ledger.stealth_range =
  {
    delta_ok = pv.delta_ok;
    balance_ok = pv.balance_ok;
  }

let reject_failure (deps : deps) e =
  deps.reject (failure_tag e) (failure_reason e)

let clear_preverify (deps : deps) tx_hash =
  deps.preverify_remove tx_hash;
  deps.clear_defer_count tx_hash

let kat_ok (deps : gate_deps) addr =
  let expected_kat = deps.expected_kat () in
  match deps.stored_kat addr with
  | None ->
    deps.log_backfill addr;
    deps.set_kat addr expected_kat;
    true
  | Some stored_kat ->
    String.equal stored_kat expected_kat

let gate_reject_plan (deps : gate_deps) tx =
  if not (kat_ok deps tx.Transaction.from) then
    Some (Private_gate.aes_kat_mismatch ~op:"stealth")
  else
    Private_gate.first_reject [
      deps.debit_gate ();
      deps.fhe_gate ();
      Private_gate.stealth_target ~target:tx.to_;
    ]

let log_cap_defer ~count ~max ~tx =
  Log.warn "epoch"
    "event = stealth_deferred reason = cap count = %d max = %d tx = %s"
    count max tx

let log_defer ~count ~max ~status ~tx =
  Log.info "epoch"
    "event = stealth_deferred count = %d max = %d status = %s tx = %s"
    count max status tx

let log_cache_hit () =
  Log.info "epoch" "event = stealth_preverify_cache status = hit"

let log_cache_miss_verify () =
  Log.info "epoch" "event = stealth_preverify_cache status = miss inline = verify"

let log_cache_miss_disabled ~tx =
  Log.warn "epoch"
    "event = stealth_preverify_cache status = miss inline = disabled action = defer tx = %s"
    tx

let gate_reject deps tx =
  gate_reject_plan deps tx

let live_tx_deps (args : live_tx_args) : tx_deps =
  let gate : gate_deps = {
    expected_kat = args.expected_kat;
    stored_kat = args.stored_kat;
    set_kat = args.set_kat;
    log_backfill = args.log_backfill;
    debit_gate = args.debit_gate;
    fhe_gate = args.fhe_gate;
  } in
  {
    stealth_count = args.stealth_count;
    max_stealth_per_epoch = args.max_stealth_per_epoch;
    max_stealth_defer = args.max_stealth_defer;
    inline_verify_allowed = args.inline_verify_allowed;
    gate_reject = gate_reject_plan gate;
    short_hash = Consensus_epoch_apply_sender.short_hash;
    defer_count = args.defer_count;
    set_defer_count = args.set_defer_count;
    clear_defer_count = args.clear_defer_count;
    preverify_state = Preverify_cache.state;
    preverify_remove = Preverify_cache.remove;
    preverify_ready = Preverify_cache.ready_result;
    log_cap_defer;
    log_defer;
    log_cache_hit;
    log_cache_miss_verify;
    log_cache_miss_disabled;
    defer_tx = args.defer_tx;
    reject = args.reject;
    plan = args.plan;
    trace = (fun tx plan ->
      args.trace_cipher
        "stealth_withdraw"
        tx.Transaction.from
        plan.Private_ledger.stealth_current_cipher
        plan.stealth_next_cipher);
    inline_range = args.inline_range;
    accept_range = args.accept_range;
    binding = args.binding;
    debit = args.debit;
    update = args.update;
    create_output = args.create_output;
    accept = args.accept;
  }

let live_ledger_tx_deps (args : live_ledger_tx_args) : tx_deps =
  live_tx_deps {
    stealth_count = args.stealth_count;
    max_stealth_per_epoch = args.max_stealth_per_epoch;
    max_stealth_defer = args.max_stealth_defer;
    inline_verify_allowed = args.inline_verify_allowed;
    expected_kat = Octra_core.Pvac_registry.expected_kat;
    stored_kat = Octra_core.Ledger.get_pvac_kat args.ledger;
    set_kat = Octra_core.Ledger.set_pvac_kat args.ledger;
    log_backfill = (fun addr ->
      Log.info "pvac" "event = kat_backfill addr = %s op = stealth"
        (args.short_addr addr));
    debit_gate = args.debit_gate;
    fhe_gate = args.fhe_gate;
    defer_count = args.defer_count;
    set_defer_count = args.set_defer_count;
    clear_defer_count = args.clear_defer_count;
    defer_tx = args.defer_tx;
    reject = args.reject;
    plan = Octra_core.Private_ledger.stealth_plan args.ledger;
    trace_cipher = args.trace_cipher;
    inline_range = Octra_core.Private_ledger.stealth_inline_range args.ledger;
    accept_range = Octra_core.Private_ledger.stealth_accept_range;
    binding = Octra_core.Private_ledger.stealth_binding args.ledger;
    debit = (fun tx fee nonce ->
      Octra_core.Ledger.debit args.ledger tx.Transaction.from fee nonce);
    update = (fun tx cipher ->
      Octra_core.Ledger.update_enc_balance args.ledger tx.Transaction.from cipher);
    create_output = (fun ~tx_hash tx plan ->
      Octra_core.Ledger.create_stealth_output args.ledger
        ~stealth_tag:plan.stealth_tag
        ~eph_pub:plan.stealth_eph_pub
        ~enc_amount:plan.stealth_enc_amount
        ~amount:Z.zero
        ~epoch_id:(args.current_epoch ())
        ~tx_hash
        ~sender_addr:tx.Transaction.from
        ~claim_pub:plan.stealth_claim_pub
        ~delta_cipher_stored:plan.stealth_delta_cipher
        ~amount_hash:Octra_core.Private_ledger.key_bound_stealth_output_marker
        ~amount_commitment:plan.stealth_amount_commitment
        ());
    accept = (fun tx output_id plan ->
      Log.info "epoch"
        "event = stealth_v5 sender = %s tag = %s output_id = %Ld"
        (args.short_addr tx.Transaction.from)
        plan.stealth_tag
        output_id;
      args.mark_debit ();
      args.incr_stealth ();
      args.incr_fhe ();
      args.confirm ());
  }

let run_ready (deps : deps) =
  let open Lwt.Syntax in
  let* plan_result = deps.plan () in
  match plan_result with
  | Error e ->
    reject_failure deps e
  | Ok plan ->
    let tx_hash = deps.tx_hash () in
    deps.trace plan;
    let cached =
      deps.preverify_ready
        tx_hash
        ~sender_enc_snapshot:plan.stealth_current_cipher
    in
    if cached = None && not deps.inline_verify_allowed then begin
      deps.log_cache_miss_disabled ~tx:(deps.short_hash tx_hash);
      deps.defer ()
    end else
      let* range_result =
        match cached with
        | Some pv ->
          deps.log_cache_hit ();
          Lwt.return (Ok (cached_range pv))
        | None ->
          deps.log_cache_miss_verify ();
          deps.inline_range plan
      in
      clear_preverify deps tx_hash;
      match range_result with
      | Error e ->
        reject_failure deps e
      | Ok range ->
        match deps.accept_range range with
        | Error e ->
          reject_failure deps e
        | Ok () ->
          let* binding = deps.binding plan in
          match binding with
          | Error e ->
            reject_failure deps e
          | Ok () ->
            match deps.debit deps.fee deps.nonce with
            | Error err ->
              deps.reject "insufficient_balance" err
            | Ok () ->
              match deps.update plan.stealth_next_cipher with
              | Error e ->
                failwith (Printf.sprintf "stealth_update_failed reason = %s" e)
              | Ok () ->
                let tx_hash = deps.tx_hash () in
                let* created = deps.create_output ~tx_hash plan in
                match created with
                | Error e ->
                  failwith ("stealth create_stealth_output after balance update: " ^ e)
                | Ok output_id ->
                  deps.accept output_id plan

let run (deps : deps) =
  match deps.gate_reject () with
  | Some r ->
    deps.reject r.tag r.reason
  | None ->
    if deps.stealth_count >= deps.max_stealth_per_epoch then begin
      let tx_hash = deps.tx_hash () in
      deps.log_cap_defer
        ~count:deps.stealth_count
        ~max:deps.max_stealth_per_epoch
        ~tx:(deps.short_hash tx_hash);
      deps.defer ()
    end else
      let tx_hash = deps.tx_hash () in
      let defer_count = deps.defer_count tx_hash in
      match
        Preverify_cache.gate
          ~state:(deps.preverify_state tx_hash)
          ~defer_count
          ~max_defer:deps.max_stealth_defer
      with
      | Failed_gate reason ->
        clear_preverify deps tx_hash;
        deps.reject "pre_verify_failed" (Printf.sprintf "reason = %s" reason)
      | Timeout_gate reason ->
        deps.clear_defer_count tx_hash;
        deps.reject
          "pre_verify_timeout"
          (Printf.sprintf
             "max_epochs = %d reason = %s"
             deps.max_stealth_defer
             reason)
      | Defer_gate defer ->
        deps.set_defer_count tx_hash defer.next_count;
        deps.log_defer
          ~count:defer.next_count
          ~max:deps.max_stealth_defer
          ~status:defer.status
          ~tx:(deps.short_hash tx_hash);
        deps.defer ()
      | Ready_gate ->
        run_ready deps

let run_tx (deps : tx_deps) tx =
  run {
    fee = tx.Transaction.ou;
    nonce = tx.nonce;
    stealth_count = deps.stealth_count;
    max_stealth_per_epoch = deps.max_stealth_per_epoch;
    max_stealth_defer = deps.max_stealth_defer;
    inline_verify_allowed = deps.inline_verify_allowed;
    gate_reject = (fun () -> deps.gate_reject tx);
    tx_hash = (fun () -> Transaction.hash tx);
    short_hash = deps.short_hash;
    defer_count = deps.defer_count;
    set_defer_count = deps.set_defer_count;
    clear_defer_count = deps.clear_defer_count;
    preverify_state = deps.preverify_state;
    preverify_remove = deps.preverify_remove;
    preverify_ready = deps.preverify_ready;
    log_cap_defer = deps.log_cap_defer;
    log_defer = deps.log_defer;
    log_cache_hit = deps.log_cache_hit;
    log_cache_miss_verify = deps.log_cache_miss_verify;
    log_cache_miss_disabled = deps.log_cache_miss_disabled;
    defer = (fun () -> deps.defer_tx tx);
    reject = deps.reject;
    plan = (fun () -> deps.plan tx);
    trace = deps.trace tx;
    inline_range = deps.inline_range tx;
    accept_range = deps.accept_range;
    binding = deps.binding tx;
    debit = deps.debit tx;
    update = deps.update tx;
    create_output = (fun ~tx_hash plan -> deps.create_output ~tx_hash tx plan);
    accept = deps.accept tx;
  }