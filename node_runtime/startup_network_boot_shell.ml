(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Store_chaindata = Octra_core.Store_chaindata

type wallet = {
  address : string;
  pub : string;
  priv : string;
}

type refs = {
  consensus_config_hash : string ref;
  consensus_validator_set : Octra_consensus.C_types.validator_set ref;
  scheduled_validator_set : Octra_consensus.C_config.scheduled option ref;
}

type deps = {
  env : string -> string option;
  read_active_validator_meta : unit -> string option;
  read_pending_validator_meta : unit -> string option;
  data_dir : string;
  store_path : string;
  epoch_duration : float;
  current_epoch : int ref;
  chaindata : Store_chaindata.t;
  finality : Consensus_finality_state.callbacks;
  set_proposal : Octra_core.Transaction.t list -> string list -> unit;
  apply_finalized : Consensus_startup_sync.apply_finalized;
  consensus_role : string;
  wallet : wallet;
  exit_error : unit -> unit;
  exit_success : unit -> unit;
}

type t = {
  api_port : int;
  p2p_port : int;
  consensus_port : int;
  consensus_peers : string list;
  chain_id : string;
  consensus_config_hash_ref : string ref;
  consensus_validator_set_ref : Octra_consensus.C_types.validator_set ref;
  scheduled_validator_set_ref : Octra_consensus.C_config.scheduled option ref;
}

let create_refs () =
  {
    consensus_config_hash = ref (String.make 32 '\x00');
    consensus_validator_set =
      ref (Octra_consensus.C_engine.make_validator_set []);
    scheduled_validator_set = ref None;
  }

let run deps =
  let network_config = Startup_process_shell.network_config ~env:deps.env in
  let Startup_process_shell.{
    api_port;
    p2p_port;
    consensus_port;
    consensus_peers;
    chain_id;
  } = network_config in
  let refs = create_refs () in
  let validator_anchor = Consensus_validator_anchor.{
    getenv = deps.env;
    chain_id;
    current_height = (fun () ->
      Int64.of_int (max (-1) (!(deps.current_epoch) - 1)));
    active_raw = deps.read_active_validator_meta;
    pending_raw = deps.read_pending_validator_meta;
  } in
  Startup_process_shell.log_node_start
    ~info:(Log.info "init" "%s")
    ~version:"3.0.0"
    ~validator:deps.wallet.address
    ~storage:"irmin+chaindata"
    ~store_path:deps.store_path
    ~epoch_duration:deps.epoch_duration
    network_config;
  Lwt_main.run (
    Consensus_startup_sync.run_node
      Consensus_startup_sync.{
        env = deps.env;
        expected_validator_set_hash = (fun epoch ->
          Result.map
            Octra_consensus.C_config.validator_set_hash
            (Consensus_validator_anchor.expected_set
               validator_anchor
               ~epoch));
        fetch_json = Consensus_join_rpc.http_get_json;
        current_epoch = (fun () -> !(deps.current_epoch));
        head = Octra_core.Head_manifest.get_cached;
        next_txid = (fun () -> Store_chaindata.next_txid deps.chaindata);
        finality = deps.finality;
        set_proposal = deps.set_proposal;
        write_entry = Octra_consensus.Finality_log.write deps.data_dir;
        apply_finalized = deps.apply_finalized;
        sleep = Lwt_unix.sleep;
        now = Unix.gettimeofday;
        data_dir = deps.data_dir;
        consensus_role = deps.consensus_role;
        chain_id;
        validator = deps.wallet.address;
        validator_pubkey = deps.wallet.pub;
        priv_b64 = deps.wallet.priv;
        exit_error = deps.exit_error;
        exit_success = deps.exit_success;
      });
  {
    api_port;
    p2p_port;
    consensus_port;
    consensus_peers;
    chain_id;
    consensus_config_hash_ref = refs.consensus_config_hash;
    consensus_validator_set_ref = refs.consensus_validator_set;
    scheduled_validator_set_ref = refs.scheduled_validator_set;
  }