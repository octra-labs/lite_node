(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Infix

type gc_config = {
  max_overhead : int;
  space_overhead : int;
  minor_heap_size : int;
}

type network_config = {
  api_port : int;
  p2p_port : int;
  consensus_port : int;
  consensus_peers : string list;
  chain_id : string;
}

type local_wallet_admission =
  | Wallet_admitted
  | Missing_voting_validator
  | Missing_non_voting_validator
  | Pubkey_mismatch_voting
  | Pubkey_mismatch_non_voting

let parse_int ~default raw =
  match int_of_string_opt raw with
  | Some value -> value
  | None -> default

let parse_int64 ~default raw =
  match Int64.of_string_opt raw with
  | Some value -> value
  | None -> default

let parse_float ~default raw =
  match float_of_string_opt raw with
  | Some value -> value
  | None -> default

let int_env env name fallback =
  match env name with
  | Some raw -> parse_int ~default:fallback raw
  | None -> fallback

let string_env env name fallback =
  match env name with
  | Some raw -> raw
  | None -> fallback

let csv_env env name =
  match env name with
  | Some raw -> String.split_on_char ',' raw
  | None -> []

let gc_config ~env =
  {
    max_overhead = 1_000_000;
    space_overhead = int_env env "OCTRA_GC_SPACE_OVERHEAD" 80;
    minor_heap_size = int_env env "OCTRA_GC_MINOR_HEAP" 2_097_152;
  }

let network_config ~env =
  {
    api_port = int_env env "OCTRA_API_PORT" 8080;
    p2p_port = int_env env "OCTRA_P2P_PORT" 9000;
    consensus_port = int_env env "OCTRA_CONSENSUS_PORT" 0;
    consensus_peers = csv_env env "OCTRA_PEERS";
    chain_id = string_env env "OCTRA_CHAIN_ID" "octra-mainnet";
  }

let epoch_duration ~env =
  Epoch_cadence.minimum_seconds_of env

let node_start_messages ~version ~validator ~storage ~store_path
    ~epoch_duration config =
  [
    Printf.sprintf
      "node version = %s validator = %s storage = %s"
      version
      validator
      storage;
    Printf.sprintf
      "api = http://localhost:%d p2p = localhost:%d consensus = %d"
      config.api_port
      config.p2p_port
      config.consensus_port;
    Printf.sprintf
      "path = %s epoch_duration = %.0fs"
      store_path
      epoch_duration;
  ]

let log_node_start ~info ~version ~validator ~storage ~store_path
    ~epoch_duration config =
  node_start_messages
    ~version
    ~validator
    ~storage
    ~store_path
    ~epoch_duration
    config
  |> List.iter info

let data_dir ~env =
  string_env env "OCTRA_DATA_DIR" "data"

let ensure_data_dir ~init_mode ~data_dir ~exit_fatal =
  if not (Sys.file_exists data_dir) then begin
    if not init_mode then begin
      Octra_log.fatal "init" "%s/ not found - run with --init cwd = %s"
        data_dir
        (Sys.getcwd ());
      exit_fatal ()
    end;
    Unix.mkdir data_dir 0o755
  end

let load_wallet ~path ~ensure ~address ~exit_fatal =
  try
    let wallet = ensure path in
    Octra_log.info "init" "wallet loaded addr = %s" (address wallet);
    wallet
  with e ->
    Octra_log.fatal "init" "wallet_load = failed reason = %s"
      (Printexc.to_string e);
    exit_fatal ();
    raise e

let validator_pubkeys_boot ~current ~next ~fallback =
  match current @ next with
  | [] -> fallback
  | validators -> validators

let local_wallet_admission ~permissionless ~validator_env_present
    ~validators ~wallet_addr ~wallet_pub ~voting_consensus_mode
    ~consensus_mode =
  if not validator_env_present then
    Wallet_admitted
  else
    match List.assoc_opt wallet_addr validators with
  | None ->
      if voting_consensus_mode && not permissionless then
        Missing_voting_validator
      else if consensus_mode then Missing_non_voting_validator
      else Wallet_admitted
    | Some expected_pub when expected_pub <> wallet_pub ->
      if voting_consensus_mode then Pubkey_mismatch_voting
      else Pubkey_mismatch_non_voting
    | Some _ -> Wallet_admitted

let admit_local_wallet ~permissionless ~validator_env_present
    ~validators ~wallet_addr ~wallet_pub ~voting_consensus_mode
    ~consensus_mode ~role_label ~exit_fatal =
  match
    local_wallet_admission
      ~permissionless
      ~validator_env_present
      ~validators
      ~wallet_addr
      ~wallet_pub
      ~voting_consensus_mode
      ~consensus_mode
  with
  | Wallet_admitted -> ()
  | Missing_voting_validator ->
    Octra_log.fatal "init"
      "local wallet addr = %s missing_from = OCTRA_VALIDATORS/OCTRA_VALIDATORS_NEXT action = refuse_consensus_start"
      wallet_addr;
    exit_fatal ()
  | Missing_non_voting_validator ->
    Octra_log.info "init"
      "local wallet addr = %s validator_set = missing role = %s"
      wallet_addr
      role_label
  | Pubkey_mismatch_voting ->
    Octra_log.fatal "init"
      "local wallet addr = %s pubkey = mismatch action = refuse_consensus_start"
      wallet_addr;
    exit_fatal ()
  | Pubkey_mismatch_non_voting ->
    Octra_log.warn "init"
      "local wallet addr = %s validator_set_pubkey = mismatch role = %s"
      wallet_addr
      role_label

let admit_wallet_validators ~permissionless ~validator_env_present
    ~current ~next ~fallback ~wallet_addr ~wallet_pub
    ~voting_consensus_mode ~consensus_mode ~role_label ~exit_fatal =
  let validators = validator_pubkeys_boot ~current ~next ~fallback in
  admit_local_wallet
    ~permissionless
    ~validator_env_present
    ~validators
    ~wallet_addr
    ~wallet_pub
    ~voting_consensus_mode
    ~consensus_mode
    ~role_label
    ~exit_fatal;
  validators

let configure_lwt_engine () =
  try
    Lwt_engine.set ~transfer:true ~destroy:true (new Lwt_engine.libev ());
    Octra_log.info "init" "lwt_engine = libev limit = no_fd_limit"
  with e ->
    Octra_log.warn "init" "lwt_engine = select limit = 1024_fd reason = %s"
      (Printexc.to_string e)

let initialize_crypto exit_fatal =
  try
    Mirage_crypto_rng_unix.use_default ();
    Octra_log.info "init" "crypto = initialized"
  with e ->
    Octra_log.fatal "init" "crypto = failed error = %s" (Printexc.to_string e);
    exit_fatal ()

let configure_process ~exit_fatal =
  Octra_log.init_from_env ();
  Octra_log.info "init" "starting_node backend = irmin-pack";
  configure_lwt_engine ();
  initialize_crypto exit_fatal;
  Random.self_init ()

let apply_gc_config config =
  let current = Gc.get () in
  Gc.set {
    current with
    max_overhead = config.max_overhead;
    space_overhead = config.space_overhead;
    minor_heap_size = config.minor_heap_size;
  };
  Octra_log.info "init"
    "gc_config max_overhead = %d space_overhead = %d minor_heap_words = %d"
    config.max_overhead
    config.space_overhead
    config.minor_heap_size

let log_gc_snapshot () =
  let s = Gc.quick_stat () in
  let live_mb = (s.live_words * (Sys.word_size / 8)) / 1_048_576 in
  let heap_mb = (s.heap_words * (Sys.word_size / 8)) / 1_048_576 in
  Octra_log.info "gc"
    "live_mb = %d heap_mb = %d minor_collections = %d major_collections = %d compactions = %d"
    live_mb
    heap_mb
    s.minor_collections
    s.major_collections
    s.compactions

let start_gc_reporter ~prune =
  Lwt.async (fun () ->
    let rec loop () =
      Lwt_unix.sleep 60.0 >>= fun () ->
      log_gc_snapshot ();
      prune ();
      loop ()
    in
    loop ())