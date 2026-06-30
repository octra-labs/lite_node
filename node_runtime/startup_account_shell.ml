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


type validator = string * string

type plan =
  | Genesis_bootstrap
  | Observer_existing
  | Create_joining_account
  | Skip_consensus_account
  | Account_ready

type deps = {
  ledger_empty : unit -> bool;
  observer_mode : bool;
  wallet_addr : string;
  wallet_pub : string;
  wallet_present : unit -> bool;
  consensus_port_configured : unit -> bool;
  validators : unit -> validator list;
  add_account :
    addr:string ->
    pub:string ->
    amount:Z.t ->
    (unit, string) result;
  set_meta : key:string -> value:string -> unit;
  flush_dirty : unit -> unit;
}

type genesis_amounts = {
  circulating : Z.t;
  emission_pool : Z.t;
  supply : Z.t;
  per_validator : Z.t;
}

let oct_units n =
  Z.mul (Z.of_int n) Octra_core.Denomination.units_per_oct

let plan ~ledger_empty ~observer_mode ~wallet_present ~consensus_port_configured =
  if ledger_empty then
    Genesis_bootstrap
  else if observer_mode then
    Observer_existing
  else if not wallet_present then
    if consensus_port_configured then Skip_consensus_account else Create_joining_account
  else
    Account_ready

let genesis_amounts ~validator_count =
  let circulating = oct_units 100_000_000 in
  {
    circulating;
    emission_pool = oct_units 900_000_000;
    supply = circulating;
    per_validator = Z.div circulating (Z.of_int validator_count);
  }

let log_genesis_account addr amount = function
  | Ok () ->
    Octra_log.info "init" "genesis account = %s balance = %s"
      addr (Z.to_string amount)
  | Error e ->
    Octra_log.warn "init" "genesis account = %s error = %s" addr e

let run_genesis deps =
  let validators = deps.validators () in
  let amounts = genesis_amounts ~validator_count:(List.length validators) in
  List.iter
    (fun (addr, pub) ->
      deps.add_account ~addr ~pub ~amount:amounts.per_validator
      |> log_genesis_account addr amounts.per_validator)
    validators;
  Octra_log.info "init"
    "genesis validators = %d per_validator = %s total = %s"
    (List.length validators)
    (Z.to_string amounts.per_validator)
    (Z.to_string amounts.circulating);
  deps.set_meta ~key:"emission_remaining" ~value:(Z.to_string amounts.emission_pool);
  deps.set_meta ~key:"total_supply" ~value:(Z.to_string amounts.supply);
  Octra_log.info "init" "emission remaining = %s supply = %s"
    (Z.to_string amounts.emission_pool)
    (Z.to_string amounts.supply);
  if deps.observer_mode then
    Octra_log.info "init"
      "observer mode = true genesis = deterministic local_observer_account = skipped";
  deps.flush_dirty ()

let run_create_joining deps =
  match deps.add_account ~addr:deps.wallet_addr ~pub:deps.wallet_pub ~amount:Z.zero with
  | Ok () ->
    Octra_log.info "init"
      "validator account = %s balance = 0 action = created"
      deps.wallet_addr;
    deps.flush_dirty ()
  | Error e ->
    Octra_log.warn "init"
      "validator account = %s action = create_failed error = %s"
      deps.wallet_addr e

let run deps =
  match plan
          ~ledger_empty:(deps.ledger_empty ())
          ~observer_mode:deps.observer_mode
          ~wallet_present:(deps.wallet_present ())
          ~consensus_port_configured:(deps.consensus_port_configured ()) with
  | Genesis_bootstrap -> run_genesis deps
  | Observer_existing ->
    Octra_log.info "init"
      "observer mode = true local_account_creation = skipped epoch_production = skipped"
  | Create_joining_account -> run_create_joining deps
  | Skip_consensus_account ->
    Octra_log.info "init"
      "consensus mode = true local_account_creation = skipped reason = fund_with_transfer"
  | Account_ready -> ()