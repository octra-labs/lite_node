(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module P = Circle_proposal
module Core = Octra_core
module N = Octra_node_runtime
module C = N.Consensus_proposal
module Preview = N.Consensus_proposal_preview_shell
module T = Core.Transaction
module S = Core.Store_irmin
module L = Core.Ledger
module G = Core.Rule_graph
module R = Recovery_case
module CT = Octra_consensus.C_types
module H = Octra_consensus.C_hash

let epoch = 1_737_349
let epoch_ts = 99.
let initial = Z.of_int 10_000_000_000
let circle = "oct" ^ String.make 44 '3'
let response = "OCWS1\003\000\000\000\0017"
let monotonic_clock () = Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e9

let code () =
  let request = Core.Circle_wasm_codec.encode_request ~method_name:"call_value" [] |> P.unwrap in
  let byte value = String.make 1 (Char.chr value) in
  let rec number limit value =
    if value < limit then byte value
    else byte ((value land 127) lor 128) ^ number limit (value lsr 7) in
  let size = number 128 in
  let string value = size (String.length value) ^ value in
  let vector values = size (List.length values) ^ String.concat "" values in
  let section tag value = byte tag ^ string value in
  let const value = "\065" ^ number 64 value in
  let export name kind index = string name ^ byte kind ^ size index in
  let data = request ^ "paid" in
  let update = const 0 ^ const (String.length request) ^ const 1024 ^ const 64
    ^ "\016\000\033\002"
    ^ const (String.length request) ^ const 4 ^ const 1024 ^ "\032\002\016\001\026"
    ^ const 1024 ^ "\032\002\016\002\026" ^ const 0 in
  String.concat "" [
    "\000asm\001\000\000\000";
    section 1 (vector ["\096\004\127\127\127\127\001\127";
      "\096\002\127\127\001\127"; "\096\001\127\001\127"]);
    section 2 (vector [string "octra" ^ string "host_circle_invoke" ^ "\000\000";
      string "octra" ^ string "host_kv_put" ^ "\000\000";
      string "octra" ^ string "host_response_write" ^ "\000\001"]);
    section 3 (vector [size 2; size 1]);
    section 5 (vector ["\001\001\001"]);
    section 7 (vector [export "memory" 2 0; export "octra_alloc" 0 3;
      export "octra_update" 0 4]);
    section 10 (vector [string ("\000" ^ const 4096 ^ "\011");
      string ("\001\001\127" ^ update ^ "\011")]);
    section 11 (vector ["\000\065\000\011" ^ string data]);
  ]

let measure action =
  let before = Core.Compute_memory.stats Core.Compute_memory.shared in
  let started = monotonic_clock () in
  let result = action () in
  let elapsed = monotonic_clock () -. started in
  P.expect "host block retained memory tickets"
    (Core.Compute_memory.stats Core.Compute_memory.shared = before);
  P.expect "host block retained child"
    (List.sort compare (Core.Proof_frame.children (Unix.getpid ()))
      = List.sort compare (Core.Proof_keeper.processes ()));
  List.iter (fun pid -> P.expect "host block retained proof child"
    (Core.Proof_frame.children pid = [])) (Core.Proof_keeper.processes ());
  result, elapsed

let proof_cut ~hash ~clock ~armed_clock ~label dir action =
  let path = Filename.concat dir label in
  let worker = Sys.getenv "OCTRA_PVAC_VERIFY_WORKER" in
  let owner = Core.Proof_keeper.current worker |> Option.get in
  let before = Core.Proof_keeper.stats owner in
  let phase = Core.Proof_keeper.phase owner in
  let memory = Core.Circle_wasm_worker.memory_stats () in
  let inbox = Core.Circle_wasm_worker.inbox_stats () in
  let queue () =
    let stats = Core.Pvac_verify_worker.channel_stats () in
    stats.active, stats.required_waiting, stats.speculative_waiting in
  let queued = queue () in
  let program = Unix.realpath Sys.executable_name in
  P.expect "proof cut shares warm owner" (Core.Proof_keeper.current program = None);
  P.expect "proof cut window differs"
    (Octra_consensus.C_engine.timeout_ms ~round:0 ~step:CT.ProposeStep = 15_000);
  let settings = ["OCTRA_PVAC_VERIFY_WORKER", program;
    "OCTRA_HOST_CUT_FILE", path; "OCTRA_HOST_CUT_HASH", hash] in
  let previous = List.map (fun (name, _) -> name, Option.value (Sys.getenv_opt name) ~default:"") settings in
  let child () =
    if not (Sys.file_exists (path ^ ".pid")) then None else
    Some (In_channel.with_open_text (path ^ ".pid") In_channel.input_all |> int_of_string) in
  Fun.protect ~finally:(fun () ->
    List.iter (fun (name, value) -> Unix.putenv name value) previous;
    Option.iter (fun pid ->
      try match Unix.waitpid [Unix.WNOHANG] pid with
      | 0, _ -> Core.Proof_frame.signal pid; ignore (Core.Proof_frame.reap pid)
      | _ -> ()
      with Unix.Unix_error (Unix.ECHILD, _, _) -> ()) (child ())) (fun () ->
    List.iter (fun (name, value) -> Unix.putenv name value) settings;
    let result = action (fun () ->
      if Sys.file_exists (path ^ ".verified") then clock
      else if Sys.file_exists (path ^ ".armed") then armed_clock else 0.) in
    P.expect "proof cut lacks verified result" (Sys.file_exists (path ^ ".verified"));
    let response = In_channel.with_open_bin (path ^ ".verified") In_channel.input_all
      |> Core.Pvac_verify_protocol.response_of_string |> P.unwrap in
    P.expect "proof cut response differs"
      (response.request_hash = hash && response.accepted && response.reason = "");
    P.expect "proof cut did not cancel verified worker" (not (Sys.file_exists (path ^ ".expired")));
    let pid = child () |> Option.get in
    P.expect "proof cut child survived" (try Unix.kill pid 0; false
      with Unix.Unix_error (Unix.ESRCH, _, _) -> true);
    P.expect "proof cut child not reaped" (try ignore (Unix.waitpid [Unix.WNOHANG] pid); false
      with Unix.Unix_error (Unix.ECHILD, _, _) -> true);
    P.expect "proof cut retained memory"
      (Core.Circle_wasm_worker.memory_stats () = memory && Core.Circle_wasm_worker.inbox_stats () = inbox);
    P.expect "proof cut retained proof slot" (queue () = queued);
    P.expect "proof cut changed warm owner"
      (Core.Proof_keeper.stats owner = before && Core.Proof_keeper.phase owner = phase);
    Printf.printf "event = proof_cut hash = %s slice_clock = %.2f verified = true reaped = true\n%!" hash clock;
    result)

let signed_parent ~state_root height validators =
  let parent = P.make_parent ~state_root height validators in
  let validator_set = CT.make_weighted_validator_set
    (List.map (fun member -> member, Z.one) parent.validator_set.validators) |> P.unwrap in
  let parent = {parent with CT.validator_set = validator_set} in
  ignore (Core.Set_fold.read_parent ~marks:true ~chain_id:P.chain parent |> P.unwrap);
  parent

let seed ?wasm ?(epoch = epoch) ?(fee = 100_000) ?(values = [])
    ?(method_name = "octra_update")
    ?(proof_policy = Core.Circle_hfhe_policy.verify_zero_mode_key) dir chaindata store =
  let people = List.init 7 (fun i -> P.identity (i + 1)) in
  let caller = List.nth people 0 and payer = List.nth people 5 in
  let validators = List.init 4 (fun i -> List.nth people (i + 1)) in
  let proposer = List.hd validators in
  let ledger = L.create store in
  List.iter (fun item -> P.unwrap (L.add_account ledger item.P.address initial)) people;
  Lwt_main.run (L.flush_dirty_lwt ledger);
  List.iter (fun (key, value) -> Lwt_main.run (S.set_meta store key value)) [
    "last_epoch", string_of_int (epoch - 1); "current_epoch", string_of_int epoch;
    "total_supply", Z.to_string (L.get_total_supply ledger); "emission_remaining", "0";
  ];
  P.seed_execution store (epoch - 1);
  ignore (P.install store caller.address circle Core.Circles.Wasm_v1
    (Option.value wasm ~default:(code ())));
  if Option.is_some wasm then begin
    let storage = Lwt_main.run (S.load_circle_stable_storage store circle) |> P.unwrap in
    List.iter (fun key -> Hashtbl.replace storage key "any_registered")
      Core.Circle_hfhe_policy.[proof_policy; cipher_serde_mode_key; pubkey_serde_mode_key];
    List.iter (fun (key, value) -> Hashtbl.replace storage key value) values;
    ignore (Core.Circles.validate_stable_storage Core.Circles.default_limits storage |> P.unwrap);
    ignore (Lwt_main.run (S.save_circle_stable_storage store circle storage))
  end;
  let info = Lwt_main.run (S.get_circle_info store circle) |> Option.get in
  Lwt_main.run (Core.Circle_deploy.allow_calls store info);
  P.expect "host block calls disabled" (Lwt_main.run (Core.Circle_deploy.calls_enabled store info));
  let prior = signed_parent ~state_root:(P.root_raw (Lwt_main.run (L.hash ledger)))
    (Int64.of_int (epoch - 2)) validators in
  let at = Int64.of_int (epoch - 1) in
  let duty, reason, changed = Core.Set_fold.advance ~marks:true Core.Set_fold.participating
    ~chain_id:P.chain ~start:at ~at ~parent:(Some prior) Core.Set_fold.empty |> P.unwrap in
  P.expect "host block duty initialization refused" (changed && reason = None);
  Lwt_main.run (S.set_meta store Core.Set_fold.meta_key (Core.Set_fold.to_string duty));
  let root = Lwt_main.run (L.hash ledger) in
  let epoch_index_hash, epoch_index_root = R.Eic.next_root
    ~prev:R.Eic.genesis_root ~epoch_id:(epoch - 1) [] in
  let state_root = R.Eic.folded_state_root ~ledger_state_root:root ~epoch_index_root in
  let parent = signed_parent ~state_root:(P.root_raw state_root)
    (Int64.of_int (epoch - 1)) validators in
  let floor = Core.History_floor.create ~chain_id:P.chain ~epoch:(epoch - 1)
    ~state_root ~ledger_state_root:root ~txid_hi:0L ~config_hash:P.config
    ~validator_set_hash:(N.Text.raw_to_hex
      (Octra_consensus.C_config.validator_set_hash parent.validator_set))
    ~epoch_index_hash ~epoch_index_root ~parent_commit:parent |> P.unwrap in
  P.unwrap (R.SC.seed_history_floor chaindata floor);
  Lwt_main.run (S.tag_epoch store (epoch - 1));
  let txlog_seg, txlog_off = R.SC.txlog_position chaindata in
  let head = R.HM.{schema_version; generation = epoch - 1; epoch_id = epoch - 1;
    state_root; ledger_state_root = Some root; irmin_commit = Lwt_main.run (S.get_commit_hash store);
    txid_hi = 0L; txlog_seg = Some txlog_seg; txlog_off = Some txlog_off;
    epochlog_off = Some (R.SC.epochlog_offset chaindata); commit_id = "circle-host";
    ts = 1.; quorum_cert_hash = None;
    epoch_index_hash = Some epoch_index_hash; epoch_index_root = Some epoch_index_root} in
  R.HM.atomic_write dir head;
  let rules = G.create_ready ~chain_id:P.chain ~ready_config_hash:P.config
    ~root_at:(fun at -> match G.root_after_floor ~chain_id:P.chain ~floor_epoch:epoch ~epoch:at with
      | Some value -> G.Root value | None -> G.Missing) in
  P.expect "host block proof key is not active" (G.proof_key rules ~epoch = Ok G.Active);
  P.expect "host block calls are not active" (G.proof_exec rules ~epoch = Ok G.Active);
  let env = Core.Epoch_exec.{chain_id = P.chain; epoch_id = epoch; proposer_addr = proposer.address;
    validator_addrs = List.map (fun item -> item.P.address) validators;
    validator_pubkeys = List.map (fun item -> item.P.address, item.public) validators;
    prev_state_root = state_root; epoch_ts;
    ready_state_root_at = Some Circle_store.ready_root; ready_max_lag = 0} in
  let call = P.signed ~ou:fee caller ~op_type:T.CircleCall ~to_:circle ~nonce:1
    ~message:"[]" ~method_:(Some method_name) in
  let signing = Core.Tx_signature.at ~chain_id:P.chain ~epoch:(Int64.of_int epoch) in
  let call = Core.Tx_signature.sign signing {call with amount = Z.of_int 7}
    (Mirage_crypto_ec.Ed25519.priv_to_octets caller.secret |> Base64.encode_exn) in
  let payment = P.signed ~ou:1000 payer ~op_type:T.Standard ~to_:(List.nth people 6).address
    ~nonce:1 ~message:"" ~method_:None in
  let txs = T.consensus_order [call; payment] in
  List.iter (fun tx -> P.unwrap (Core.Tx_signature.check signing tx)) txs;
  head, env, rules, parent, ledger, txs

let cost ~request ~accepted dir =
  let prepared, preview_ms = R.with_stores dir (fun chaindata store ->
    let wasm = P.host_code ~request ~checked:true ~stored:true () in
    let head, env, rules, parent, ledger, txs = seed ~wasm ~epoch:2_000_000 ~fee:20_000_000
      ~values:(P.host_inputs request) ~method_name:"fhe_verify_bound"
      ~proof_policy:Core.Circle_hfhe_policy.verify_bound_mode_key dir chaindata store in
    let txs = List.filter (fun tx -> tx.T.op_type = T.CircleCall) txs in
    P.expect "cost call count differs" (List.length txs = 1);
    let root = R.HM.ledger_state_root head in
    let reward = N.Consensus_reward_attribution.of_parent_commit parent |> P.unwrap in
    let backend = Preview.node_backend ~program_trust:Octra_vm.Program_trust.empty ~rules
      ~legacy_replay:Circle_store.legacy ~private_result_policy:Circle_store.result_policy
      ~max_fhe:1 ~max_stealth:1 store ledger in
    let prepared, seconds = measure (fun () -> Lwt_main.run (backend.prepare
      ~epoch_id:env.epoch_id ~proposal_id:"circle-cost" ~expected_prev_root:(Some root)
      ~preverify:(Core.Preverify_commit.create []) ~parent_commit:(Some parent) ~reward ~env ~txs)
      |> P.unwrap) in
    let transcript = match prepared.preverify.receipts with
      | [{Core.Preverify_receipt.circle = Some {transcript; _}; _}] -> transcript
      | _ -> failwith "cost circle receipt missing" in
    let expected = Core.Circle_hfhe_transcript.{method_name = "fhe_verify_bound";
      request_hash = Digestif.SHA256.(to_hex (digest_string
        ("octra:circle_hfhe_request:proof_key\000" ^ request)));
      response_hash = Digestif.SHA256.(to_hex (digest_string
        ("octra:circle_hfhe_response:standard:v1\000OCWS1"
          ^ (if accepted then "\002" else "\001") ^ "\000\000\000\000")));
      result = Some accepted} in
    P.expect "cost verifier transcript differs" (transcript = [expected]);
    P.expect "cost preview changed base root" (Lwt_main.run (L.hash ledger) = root);
    P.expect "cost preview changed base commit" (Lwt_main.run (S.get_commit_hash store) = head.irmin_commit);
    let storage = Lwt_main.run (S.load_circle_stable_storage store circle) |> P.unwrap in
    P.expect "cost preview retained write" (not (Hashtbl.mem storage "paid"));
    let artifacts = prepared.execution.artifacts in
    if accepted then begin
      P.expect "valid proof call refused"
        (List.map fst artifacts.confirmed = txs && artifacts.rejected = []);
      P.expect "valid proof fee differs" (Z.equal artifacts.confirmed_fees (Z.of_int 20_000_000))
    end else begin
      P.expect "invalid proof call confirmed" (artifacts.confirmed = []);
      P.expect "invalid proof refusal differs" (match artifacts.rejected with
        | [{tx; error_type; reason; charged}] -> tx = List.hd txs && error_type = "circle_call_failed"
          && reason = "wasm `unreachable` instruction executed" && charged
        | _ -> false);
      P.expect "invalid proof fee differs" (Z.equal artifacts.confirmed_fees (Z.of_int 20_000_000))
    end;
    Circle_store.{head; env; rules; parent; reward; txs; prepared}, seconds *. 1000.) in
  let applied =
    let (), seconds = measure (fun () -> Circle_store.apply dir prepared "complete" "success") in
    R.with_stores dir (fun chaindata store ->
      let tx = List.hd prepared.txs in
      let caller = Lwt_main.run (S.get_account store tx.from) |> Option.get in
      P.expect "cost applied account differs"
        (Z.equal caller.balance (Z.sub initial
          (Z.of_int (if accepted then 20_000_007 else 20_000_000))) && caller.nonce = 1);
      let payee = Lwt_main.run (S.get_account store circle) in
      P.expect "cost applied value differs" (if accepted then
        Option.fold ~none:false ~some:(fun (account : Core.Ledger_types.account) ->
          Z.equal account.balance (Z.of_int 7)) payee
        else Option.fold ~none:true ~some:(fun (account : Core.Ledger_types.account) ->
          Z.equal account.balance Z.zero) payee);
      let storage = Lwt_main.run (S.load_circle_stable_storage store circle) |> P.unwrap in
      P.expect "cost applied proof verdict differs"
        (Hashtbl.find_opt storage "paid" =
          if accepted then Some "OCWS1\002\000\000\000\000" else None);
      P.expect "cost applied transaction visibility differs"
        (Option.is_some (R.SC.get_tx_by_hash chaindata (T.hash tx)) = accepted);
      let stored = Core.Preverify_receipt_store.read dir ~epoch_id:prepared.env.epoch_id in
      P.expect "cost applied receipts differ" (stored = Some (Circle_store.receipts prepared));
      let head = R.HM.load dir |> Option.get in
      P.expect "cost applied head differs"
        (head.epoch_id = prepared.env.epoch_id
         && head.txid_hi = (if accepted then 1L else 0L)
         && R.HM.ledger_state_root head = prepared.prepared.execution.post_state_root);
      P.expect "cost applied root differs"
        (Lwt_main.run (S.state_hash store) = prepared.prepared.execution.post_state_root));
    seconds *. 1000. in
  `Assoc ["preview_ms", `Float preview_ms;
    "apply_ms", `Float applied;
    "reopen_checked", `Bool true;
    "accepted", `Bool accepted; "whole_circle", `Bool true; "whole_private_certify", `Bool false;
    "leader_window", `Bool false; "network_ingress", `Bool false;
    "fee", `Int 20_000_000; "epoch", `Int prepared.env.epoch_id;
    "request_sha256", `String (Digestif.SHA256.(to_hex (digest_string request)));
    "root", `String prepared.prepared.execution.post_state_root; "passed", `Bool true]

let prepare ?request ?resume ?cut_hash ?(cut_clock = 4.) ?(cut_armed = 0.) ?(cut_label = "proof_cut")
    ?(range = false) ?(cold = false) ?(accepted = true) ?(queued = false)
    ?(waiting = false) ?(following = []) ?(seed_epoch = 2_000_000)
    ~make_deps ~verify_deps dir = R.with_stores dir (fun chaindata store ->
  let fee = if request = None then 100_000 else 20_000_000 in
  let head, env, rules, parent, ledger, txs = match resume with
    | Some (previous : Circle_store.sample) ->
      let head = R.HM.load dir |> Option.get in
      let finalized = Circle_store.signed previous head in
      let parent = CT.{validator_set = previous.parent.validator_set;
        certificate = certificate_of_finalize finalized} in
      ignore (Core.Set_fold.read_parent ~marks:true ~chain_id:P.chain parent |> P.unwrap);
      let env = {previous.env with epoch_id = head.epoch_id + 1;
        prev_state_root = head.state_root; epoch_ts = previous.env.epoch_ts +. 10.} in
      head, env, previous.rules, parent, L.create store,
        Core.Tx_staging.get_epoch_txs ~capacity:Core.Tx_staging.max_ou
    | None -> match request with
      | None -> seed dir chaindata store
      | Some request -> seed ~wasm:(P.host_code ~request ~checked:true ~stored:true ~following ())
        ~epoch:seed_epoch ~fee ~values:(P.host_inputs (String.concat "" (request :: following)))
        ~method_name:(if range then "fhe_verify_range" else "fhe_verify_bound")
        ~proof_policy:(if range then Core.Circle_hfhe_policy.verify_range_mode_key
          else Core.Circle_hfhe_policy.verify_bound_mode_key) dir chaindata store in
  let epoch = env.Core.Epoch_exec.epoch_id in
  let root = R.HM.ledger_state_root head in
  let head_epoch = Int64.of_int head.epoch_id in
  let account address = Lwt_main.run (S.get_account store address) in
  if queued && resume = None then
    List.iter (fun tx -> ignore (Core.Tx_staging.add_smart ~head:head_epoch
      ~lookup:(fun address -> Option.map (fun (item : Core.Ledger_types.account) ->
        item.balance, item.nonce) (account address)) tx |> P.unwrap)) txs;
  let pending = List.filter (fun tx -> tx.T.op_type <> T.Standard) txs
    |> List.map (fun tx ->
      let hash = T.hash tx in
      tx, account tx.from, Core.Tx_staging.queue_state ~confirmed:0 hash,
        R.SC.get_tx_by_hash chaindata hash, R.SC.get_contract_receipt chaindata ~tx_hash:hash) in
  let caps = N.Startup_runtime_limits.proposal_limits
    {int_value = (fun _ value -> value); opt = (fun _ -> None)}
    ~default_max_ou:Core.Tx_staging.max_ou_per_epoch in
  let runtime = N.Consensus_circle_preverify.{store; ledger;
    program_trust = Octra_vm.Program_trust.empty; rules;
    env = (fun ~pre_state_root -> {env with prev_state_root = pre_state_root})} in
  let checked ~state_root ~tx_hashes inputs =
    P.expect "host block preverify inputs differ" (tx_hashes = List.map T.hash inputs);
    P.expect "host block preverify root differs" (state_root = root || state_root = P.root_raw root);
    let open Lwt.Syntax in
    let+ result = Core.Preverify_worker.run_many ~registered:true ~keyed:true ~strict:true
      ~field_policy:Core.Private_ledger.Unique_fields ~ledger
      ~circle_preverify:(N.Consensus_circle_preverify.run runtime) inputs in
    Option.iter (fun _ ->
      let path = Filename.concat dir (cut_label ^ ".armed") in
      if not (Sys.file_exists path) then
        Out_channel.with_open_gen [Open_wronly; Open_creat; Open_excl] 0o600 path
          (fun stream -> output_string stream "execution")) cut_hash;
    result in
  let backend = Preview.node_backend ~program_trust:Octra_vm.Program_trust.empty ~rules
    ~legacy_replay:Circle_store.legacy ~private_result_policy:Circle_store.result_policy
    ~max_fhe:1 ~max_stealth:1 store ledger in
  let runtime = Preview.{chain_id = P.chain; program_trust = Octra_vm.Program_trust.empty; backend;
    ready_state_root_at = Circle_store.ready_root; ready_max_lag = 0; warn = failwith} in
  let captures = ref [] in
  let captured_request = ref None in
  let prepare request =
    let open Lwt.Syntax in
    let started = monotonic_clock () in
    let+ result = Preview.prepare runtime request in
    (match result with
    | Ok value -> captured_request := Some request; captures := value :: !captures
    | Error _ -> ());
    if queued then Printf.printf "event = proof_preview epoch = %d seconds = %.6f result = %s\n%!"
      epoch (monotonic_clock () -. started)
      (match result with Ok _ -> "accepted" | Error error -> error);
    result in
  let unchanged () =
    P.expect "host block preview changed ledger" (Lwt_main.run (L.hash ledger) = root);
    P.expect "host block preview changed commit" (Lwt_main.run (S.get_commit_hash store) = head.irmin_commit);
    let storage = Lwt_main.run (S.load_circle_stable_storage store circle) |> P.unwrap in
    P.expect "host block preview retained callback value" (not (Hashtbl.mem storage "paid")) in
  let saved = ref None in
  let held = ref [] in
  let deps : C.make_proposal_deps = make_deps ~epoch ~staging:txs ~root in
  let deps = C.{deps with cached_head = (fun () -> Some head); current_round = (fun () -> 0);
    now = (fun () -> env.epoch_ts);
    previous_epoch_ts = (fun _ -> Some (env.epoch_ts -. 10.));
    parent_commit = (fun ~epoch_id:_ -> Ok (Some parent));
    proposer = (fun () -> env.proposer_addr); validator_pubkeys = (fun _ -> env.validator_pubkeys);
    build_preverify_once = checked; preview = Preview.run runtime;
    prev_eic_root = (fun () -> Option.get head.epoch_index_root);
    next_txid = (fun () -> Int64.succ head.txid_hi); head_txid_hi = (fun () -> Some head.txid_hi);
    staging_total = (if queued then Core.Tx_staging.staging_size else deps.staging_total);
    staging_txs = (if not queued then deps.staging_txs else fun ?(circles = true) () ->
      Core.Tx_staging.ready_epoch_txs ~capacity:Core.Tx_staging.max_ou
        ~confirmed_nonce:(fun address -> Option.map (fun (item : Core.Ledger_types.account) ->
          item.nonce) (account address))
        ~accept:(fun tx -> Result.is_ok (Core.Tx_staging.Preview.send
          (Ready (Some head_epoch, T.hash tx))))
      |> (if circles then Fun.id else N.Circle_refill.without));
    hold_preview = (fun ~epoch tx ->
      if not queued then failwith "host block deferred transaction";
      P.unwrap (Core.Tx_staging.Preview.send (Hold {head = head_epoch; epoch; hash = T.hash tx}));
      held := T.hash tx :: !held);
    store_bundle = (fun ~proposal_id:_ ~tx_hashes ~txs ~receipts_json ->
      saved := Some (tx_hashes, txs, receipts_json))} in
  let propose clock = Lwt_main.run
    (C.make_proposal ~clock ~prepare deps ~chain_id:P.chain ~root_to_raw32:P.root_raw
      ~limits:caps ~epoch_id:(Int64.of_int epoch)) in
  let proposal, proposed = measure (fun () -> match cut_hash with
    | None -> propose monotonic_clock
    | Some hash -> proof_cut ~hash ~clock:cut_clock ~armed_clock:cut_armed ~label:cut_label dir propose) in
  let proposal = match proposal with Some value -> value | None ->
    failwith (Printf.sprintf "host block missing epoch = %d seconds = %.6f captures = %d held = %d"
      epoch proposed (List.length !captures) (List.length !held)) in
  let hashes, stored, receipts = Option.get !saved in
  let executions = List.length !captures in
  P.expect "host block repeated leader execution" (executions = 1);
  let prepared = List.hd !captures in
  let processed = List.map fst prepared.execution.artifacts.confirmed
    @ List.map (fun (row : Core.Epoch_exec.tx_reject) -> row.tx) prepared.execution.artifacts.rejected
    |> T.consensus_order in
  let renewed = List.exists (fun (tx, _, queue, _, _) ->
    Core.Tx_staging.queue_state ~confirmed:0 (T.hash tx) <> queue) pending in
  let outcome = if waiting then `Waiting else if !held <> [] then `Hold
    else if renewed then `Renew
    else if processed = txs then `Complete else `Window in
  let ordinary = List.filter (fun tx -> tx.T.op_type = T.Standard) txs in
  let expected = match outcome with
    | `Complete -> txs
    | `Waiting -> P.expect "waiting input executed" (!held = []); []
    | `Hold | `Renew | `Window ->
      P.expect "host block omitted unsupported input"
        (queued && pending <> [] && List.for_all (fun tx ->
          tx.T.op_type = T.Standard || tx.T.op_type = T.CircleCall) txs);
      ordinary in
  if outcome <> `Complete then begin
    List.iter (fun (tx, prior, queue, history, receipt) ->
      let hash = T.hash tx in
      P.expect "omitted proof changed account" (account tx.from = prior);
      P.expect "omitted proof changed history" (R.SC.get_tx_by_hash chaindata hash = history);
      P.expect "omitted proof changed receipt"
        (R.SC.get_contract_receipt chaindata ~tx_hash:hash = receipt);
      let current = Core.Tx_staging.queue_state ~confirmed:0 hash in
      if outcome = `Renew then P.expect "proof renewal changed allowance"
        (match queue, current with
        | Some (Core.Tx_staging.Wait_preview prior), Some (Core.Tx_staging.Wait_preview next) ->
          next.attempts = prior.attempts && next.expires_at = prior.expires_at
          && next.after = Int64.add head_epoch (Int64.of_int (1 lsl (prior.attempts - 1)))
        | _ -> false)
      else if outcome <> `Hold then P.expect "omitted proof changed hold" (current = queue)) pending;
    let request : C.build_preview_request = Option.get !captured_request in
    let replay = Lwt_main.run (Preview.prepare runtime
      {request with txs = expected; preverify = Core.Preverify_commit.create []}) |> P.unwrap in
    P.expect "omitted proof differs from ordinary replay"
      (prepared.execution = replay.execution
        && prepared.preverify.receipts = replay.preverify.receipts)
  end;
  let confirmed, refused = List.partition (fun tx -> accepted || tx.T.op_type <> T.CircleCall) expected in
  P.expect "host block selected another input" (proposal.tx_hashes = List.map T.hash confirmed);
  P.expect "host block bundle differs" (hashes = proposal.tx_hashes && stored = confirmed);
  P.expect "host block lost staged input" (not queued
    || List.for_all (fun tx -> Core.Tx_staging.find_by_hash (T.hash tx) = Some tx) txs);
  let offered = txs in
  let txs = expected in
  P.expect "host block execution differs" (List.map fst prepared.execution.artifacts.confirmed = confirmed
    && List.map (fun (row : Core.Epoch_exec.tx_reject) -> row.tx) prepared.execution.artifacts.rejected = refused
    && List.for_all (fun (row : Core.Epoch_exec.tx_reject) -> row.charged
      && row.error_type = "circle_call_failed" && row.reason = "wasm `unreachable` instruction executed")
      prepared.execution.artifacts.rejected
    && prepared.execution.artifacts.tx_count = List.length txs
    && Z.equal prepared.execution.artifacts.confirmed_fees
      (List.fold_left (fun sum tx -> Z.add sum tx.T.ou) Z.zero txs));
  P.expect "host block receipt commitment differs"
    ((receipts <> [] || List.for_all (fun tx -> tx.T.op_type = T.Standard) txs)
      && proposal.header.receipt_root = H.receipt_root receipts);
  unchanged ();
  let deps = verify_deps ~staging:confirmed ~root:(P.root_raw head.state_root) ~ledger_root:root in
  let deps = C.{deps with limits = caps; root_to_raw32 = P.root_raw;
    now = (fun () -> env.epoch_ts);
    previous_epoch_ts = (fun _ -> Some (env.epoch_ts -. 10.));
    validator_pubkeys = (fun _ -> env.validator_pubkeys);
    validate_preverify_once = checked; preview = Preview.run runtime;
    cached_bundle = (fun ~proposal_id:_ -> if accepted then None else
      let outcomes = Core.Tx_outcome.split receipts |> P.unwrap in
      Some N.Consensus_bundle_fetch.{txs = confirmed; receipts_json = receipts; rejections = outcomes.rejections});
    query_bundle = (fun ~epoch_id:_ ~proposal_id:_ ~validate:_ -> failwith "host block requested remote bundle");
    verify_address_pubkey = (fun ~addr ~pubkey -> Core.Crypto.Address.verify_address_pubkey addr pubkey);
    verify_tx_signature = (fun ~signing tx ~pubkey -> Core.Tx_signature.verify signing tx pubkey);
    verify_parent_commit = (fun ~epoch_id:_ value ->
      if value <> Some parent then Error "host block parent differs"
      else Core.Set_fold.read_parent ~chain_id:P.chain parent |> Result.map (fun _ -> ()));
    prev_eic_root = (fun () -> Option.get head.epoch_index_root);
    next_txid = (fun () -> Int64.succ head.txid_hi); head_txid_hi = (fun () -> Some head.txid_hi)} in
  let wire = CT.{chain_id = P.chain; epoch_id = Int64.of_int epoch; round = 0;
    header = proposal.header; tx_hashes = proposal.tx_hashes; parent_commit = Some parent;
    valid_round = None; proposer = env.proposer_addr; signature = ""} in
  let verify wire = Lwt_main.run (C.verify_proposal ~prepare deps ~chain_id:P.chain wire) in
  let worker = Sys.getenv "OCTRA_PVAC_VERIFY_WORKER" in
  let warm = if cold then Some (Core.Proof_keeper.current worker |> Option.get) else None in
  let before = Option.map Core.Proof_keeper.stats warm in
  let verdict, verified = measure (fun () ->
    if not cold then verify wire else
    let isolated = Filename.concat dir "cold-worker.exe" in
    let target = Unix.realpath worker in
    if not (Sys.file_exists isolated) then Unix.symlink target isolated;
    P.expect "cold worker path differs" (Unix.readlink isolated = target);
    Fun.protect ~finally:(fun () -> Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" worker) (fun () ->
      Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" isolated;
      P.expect "cold verifier shares proof owner" (Core.Proof_keeper.current isolated = None);
      verify wire)) in
  P.expect "cold verifier used leader proof cache" (Option.map Core.Proof_keeper.stats warm = before);
  P.expect "host block validator refused" (verdict = Octra_consensus.C_driver.Proposal_accept);
  P.expect "host block validator did not execute once" (List.length !captures = executions + 1);
  let replayed = List.hd !captures in
  P.expect "host block validator execution differs" (replayed.execution = prepared.execution
    && replayed.preverify.receipts = prepared.preverify.receipts);
  unchanged ();
  let checks : C.verification_deps = {public_key_for_tx = deps.public_key_for_tx;
    verify_address_pubkey = deps.verify_address_pubkey; verify_tx_signature = deps.verify_tx_signature} in
  let bundle header inputs = C.verify_bundle checks ~limits:caps ~header ~expected_tx_count:(List.length confirmed) inputs receipts in
  P.expect "host block bundle refused" (Result.is_ok (bundle wire.header confirmed));
  (match confirmed with
  | [] ->
    let injected = match offered with tx :: _ -> tx | [] -> failwith "host block has no offered input" in
    P.expect "empty block accepted injected transaction"
      (bundle wire.header [injected] = Error (C.Missing_txs {have = 1; need = 0}))
  | first :: rest ->
    let tx = {first with T.signature = Base64.encode_exn (String.make 64 '\000')} in
    P.expect "host block signature check accepted alteration"
      (not (Core.Tx_signature.verify
       (Core.Tx_signature.at ~chain_id:P.chain ~epoch:(Int64.of_int epoch)) tx
       (Option.get (checks.public_key_for_tx tx))));
    P.expect "host block bundle accepted altered signature"
      (Result.is_error (bundle wire.header (tx :: rest))));
  P.expect "host block accepted altered receipt commitment"
    (bundle {wire.header with receipt_root = H.receipt_root ("altered" :: receipts)} confirmed
      = Error C.Receipt_root_mismatch);
  let verdict, _ = measure (fun () -> verify {wire with header =
    {wire.header with proposed_state_root = String.make 32 '\000'}}) in
  P.expect "host block accepted altered state" (verdict = Octra_consensus.C_driver.Proposal_reject);
  let verdict, _ = measure (fun () ->
    Fun.protect ~finally:(fun () -> Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" worker) (fun () ->
      Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" (Filename.concat dir "absent-worker");
      verify wire)) in
  P.expect "host block worker availability differs"
    (verdict = if List.exists (fun tx -> tx.T.op_type = T.CircleCall) txs then
      Octra_consensus.C_driver.Proposal_wait else Octra_consensus.C_driver.Proposal_accept);
  unchanged ();
  let verdict, _ = measure (fun () -> verify wire) in
  P.expect "host block worker restore refused" (verdict = Octra_consensus.C_driver.Proposal_accept);
  unchanged ();
  let reward = N.Consensus_reward_attribution.of_parent_commit parent |> P.unwrap in
  Circle_store.{head; env; rules; parent; reward; txs; prepared}, proposal.header, proposed, verified, outcome)

let inspect dir (sample : Circle_store.sample) header =
  let head = Circle_store.inspect dir sample true in
  P.expect "host block applied another root" (P.root_raw head.state_root = header.CT.proposed_state_root);
  P.expect "host block applied another receipt commitment"
    (H.receipt_root (Circle_store.receipts sample) = header.receipt_root);
  R.with_stores dir (fun chaindata store ->
    let storage = Lwt_main.run (S.load_circle_stable_storage store circle) |> P.unwrap in
    P.expect "host block lost callback bytes" (Hashtbl.find_opt storage "paid" = Some response);
    List.iteri (fun index item ->
      let account = Lwt_main.run (S.get_account store item.P.address) |> Option.get in
      let delta, nonce = match index with
        | 0 -> -100_007, 1 | 1 -> 78_275, 0 | 2 | 3 | 4 -> 7_575, 0
        | 5 -> -1_001, 1 | _ -> 1, 0 in
      P.expect "host block funds or nonce differ"
        (Z.equal account.balance (Z.add initial (Z.of_int delta)) && account.nonce = nonce))
      (List.init 7 (fun i -> P.identity (i + 1)));
    let account = Lwt_main.run (S.get_account store circle) |> Option.get in
    P.expect "host block value transfer differs" (Z.equal account.balance (Z.of_int 7) && account.nonce = 0);
    P.expect "host block supply differs"
      (Lwt_main.run (S.get_meta store "total_supply") = Some (Z.to_string (Z.mul initial (Z.of_int 7))));
    let call = List.find (fun tx -> tx.T.op_type = T.CircleCall) sample.txs in
    let receipt = R.SC.get_contract_receipt chaindata ~tx_hash:(T.hash call) |> Option.get in
    let effort = Yojson.Safe.Util.(receipt |> member "effort" |> to_int) in
    P.expect "host block receipt effort invalid" (effort > 0 && effort <= Core.Resource_lanes.vm_limit call.ou);
    P.expect "host block receipt differs" (receipt = `Assoc [
      "contract", `String circle; "method", `String "octra_update"; "success", `Bool true;
      "effort", `Int effort; "events", `List []; "error", `Null;
      "epoch", `Int epoch; "ts", `Float sample.env.epoch_ts;
    ]))

let cut ~held ~make_deps dir = R.with_stores dir (fun chaindata store ->
  let module F = Pvac_ffi in
  let module B = Core.Crypto.FheBalance in
  let seed_value = Bytes.make 32 '\041' in
  let key, secret = F.keygen_from_seed (F.default_params ()) seed_value in
  let cipher = F.enc_value_seeded key secret 0L seed_value in
  let math = G.math_at ~chain_id:P.chain ~epoch = G.Active in
  let proof = F.make_zero_proof ~math ~keyed:true key secret cipher |> B.encode_zero_proof in
  let pubkey = F.serialize_pubkey key |> Bytes.to_string in
  let ciphertext = B.encode_cipher cipher in
  let proof_input = Core.Pvac_verify_worker.with_rules ~keyed:true ~math
    (Core.Pvac_verify_protocol.Zero {pubkey; cipher = ciphertext; proof}) in
  P.expect "incident proof input refused"
    (Core.Pvac_verify_direct.response ~keyed:false proof_input).accepted;
  let request = Core.Circle_wasm_codec.encode_request ~method_name:"fhe_verify_zero"
    [`String (Base64.encode_exn pubkey); `String ciphertext; `String proof] |> P.unwrap in
  let head, env, rules, parent, ledger, _ = seed ~wasm:(P.host_code ~request ()) dir chaindata store in
  let root = R.HM.ledger_state_root head in
  let people = List.init 7 (fun i -> P.identity (i + 1))
    |> List.sort (fun a b -> String.compare a.P.address b.P.address) in
  let calls = List.init 2 (fun i -> P.signed ~ou:20_000_000 (List.nth people i)
    ~op_type:T.CircleCall ~to_:circle ~nonce:1 ~message:"[]" ~method_:(Some "fhe_verify_zero")) in
  let pay = P.signed ~ou:1000 (List.nth people 2) ~op_type:T.Standard
    ~to_:(List.nth people 3).address ~nonce:1 ~message:"" ~method_:None in
  let inputs = calls @ [pay] in
  P.expect "incident order differs" (T.consensus_order inputs = inputs);
  let context = N.Consensus_circle_preverify.{store; ledger;
    program_trust = Octra_vm.Program_trust.empty; rules;
    env = (fun ~pre_state_root -> {env with prev_state_root = pre_state_root})} in
  let checked ~state_root:_ ~tx_hashes:_ inputs =
    Core.Preverify_worker.run_many ~registered:true ~keyed:true ~strict:true
      ~field_policy:Core.Private_ledger.Unique_fields ~ledger
      ~circle_preverify:(N.Consensus_circle_preverify.run context) inputs in
  let backend = Preview.node_backend ~program_trust:Octra_vm.Program_trust.empty ~rules
    ~legacy_replay:Circle_store.legacy ~private_result_policy:Circle_store.result_policy
    ~max_fhe:1 ~max_stealth:1 store ledger in
  let runtime = Preview.{chain_id = P.chain; program_trust = Octra_vm.Program_trust.empty; backend;
    ready_state_root_at = Circle_store.ready_root; ready_max_lag = 0; warn = failwith} in
  let captured = ref [] in
  let prepare request =
    let open Lwt.Syntax in
    let+ result = Preview.prepare runtime request in
    (match result with Ok item -> captured := (request, item) :: !captured | Error _ -> ());
    result in
  let staged = Core.Tx_staging.hash_index in
  let saved = ref None in
  let published = ref None in
  let deps = make_deps ~epoch ~staging:inputs ~root in
  let deps = C.{deps with cached_head = (fun () -> Some head); current_round = (fun () -> 0);
    now = (fun () -> epoch_ts); parent_commit = (fun ~epoch_id:_ -> Ok (Some parent));
    proposer = (fun () -> env.proposer_addr); validator_pubkeys = (fun _ -> env.validator_pubkeys);
    build_preverify_once = checked; preview = Preview.run runtime;
    prev_eic_root = (fun () -> Option.get head.epoch_index_root);
    next_txid = (fun () -> 1L); head_txid_hi = (fun () -> Some 0L);
    hold_preview = (fun ~epoch tx -> P.unwrap (Core.Tx_staging.Preview.send
      (Hold {head = Int64.of_int head.epoch_id; epoch; hash = T.hash tx})));
    set_proposal = (fun txs hashes -> published := Some (txs, hashes));
    store_bundle = (fun ~proposal_id:_ ~tx_hashes ~txs ~receipts_json ->
      saved := Some (tx_hashes, txs, receipts_json))} in
  let caps = N.Startup_runtime_limits.proposal_limits
    {int_value = (fun _ value -> value); opt = (fun _ -> None)}
    ~default_max_ou:Core.Tx_staging.max_ou_per_epoch in
  let worker = Sys.getenv "OCTRA_PVAC_VERIFY_WORKER" in
  let marker = Filename.concat dir "proof-read" in
  Core.Tx_staging.clear ();
  Fun.protect ~finally:(fun () ->
    Core.Tx_staging.clear ();
    Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" worker;
    Unix.putenv "OCTRA_HOST_DELAY_FILE" "";
    Unix.putenv "OCTRA_HOST_DELAY_REQUEST" "") (fun () ->
    List.iter (fun tx -> P.unwrap (Core.Tx_staging.add_smart
      ~lookup:(fun _ -> Some (initial, 0)) tx) |> ignore) inputs;
    if held then begin
      let entry = Hashtbl.find staged (T.hash (List.hd calls)) in
      entry.preview <- Some (Core.Tx_staging.hold_after
        (Int64.of_int (head.epoch_id - 1)) None)
    end;
    Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" Sys.executable_name;
    Unix.putenv "OCTRA_HOST_DELAY_FILE" marker;
    Unix.putenv "OCTRA_HOST_DELAY_REQUEST" (Core.Pvac_verify_protocol.request_hash proof_input);
    let plan, elapsed = measure (fun () -> Lwt_main.run
      (C.make_proposal ~budget:15. ~prepare deps ~chain_id:P.chain ~root_to_raw32:P.root_raw
        ~limits:caps ~epoch_id:(Int64.of_int epoch)) |> Option.get) in
    P.expect "incident omitted payment" (plan.tx_hashes = [T.hash pay]);
    P.expect "incident missed publish window" (elapsed < 10.);
    P.expect "incident bundle absent" (!published = Some ([pay], [T.hash pay])
      && Option.fold ~none:false ~some:(fun (hashes, txs, _) -> hashes = [T.hash pay] && txs = [pay]) !saved);
    P.expect "incident bypassed proof verifier" (Sys.file_exists marker);
    let channel = open_in marker in
    let proof_count = Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
      let rec count n = match input_line channel with
        | "received" -> count (n + 1)
        | "verified" -> count n
        | _ -> failwith "proof record differs"
        | exception End_of_file -> n in count 0) in
    P.expect "incident started second heavy call" (proof_count = 1);
    P.expect "incident lost staged work" (Core.Tx_staging.staging_size () = 3);
    P.expect "incident held wrong hash"
      ((Hashtbl.find staged (T.hash (List.hd calls))).preview <> None
       && (Hashtbl.find staged (T.hash (List.nth calls 1))).preview = None
       && (Hashtbl.find staged (T.hash pay)).preview = None);
    P.expect "incident changed root" (Lwt_main.run (L.hash ledger) = root);
    P.expect "incident changed commit" (Lwt_main.run (S.get_commit_hash store) = head.irmin_commit);
    let storage = Lwt_main.run (S.load_circle_stable_storage store circle) |> P.unwrap in
    P.expect "incident retained write" (not (Hashtbl.mem storage "counter"));
    let request, prepared = List.hd !captured in
    Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" worker;
    let replay = Lwt_main.run (Preview.prepare runtime {request with txs = [pay]}) |> P.unwrap in
    P.expect "incident state differs from replay"
      (prepared.execution = replay.execution && prepared.execution.artifacts.rejected = []);
    Printf.printf "event = active_cut repeat = %b proofs = %d held = 1 payments = 1 elapsed_ms = %.3f status = pass\n%!"
      held proof_count (elapsed *. 1000.)))

let worker () =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--circle-session" then begin
    Pvac_ffi.isolate_worker ();
    Core.Circle_wasm_worker.serve ();
    exit 0
  end;
  (match Sys.getenv_opt "OCTRA_HOST_CUT_FILE" with
  | Some path when path <> "" ->
    Pvac_ffi.isolate_worker ();
    let publish name value =
      let file = path ^ name in
      P.expect "proof cut repeated worker" (not (Sys.file_exists file));
      Out_channel.with_open_gen [Open_wronly; Open_creat; Open_excl; Open_binary] 0o600
        (file ^ ".next") (fun stream -> output_string stream value);
      Unix.rename (file ^ ".next") file in
    let input = Buffer.create 1024 in
    (try while true do Buffer.add_channel input stdin 1024 done with End_of_file -> ());
    let request = Core.Pvac_verify_protocol.request_of_string (Buffer.contents input) |> P.unwrap in
    let response = Core.Pvac_verify_direct.response ~keyed:false request in
    let raw = Core.Pvac_verify_protocol.response_bytes response in
    if not (Sys.file_exists (path ^ ".armed")) then begin
      output_string stdout raw;
      flush stdout;
      exit 0
    end;
    publish ".pid" (string_of_int (Unix.getpid ()));
    let hash = Sys.getenv "OCTRA_HOST_CUT_HASH" in
    P.expect "proof cut request differs" (Core.Pvac_verify_protocol.request_hash request = hash);
    P.expect "proof cut verification refused"
      (response.request_hash = hash && response.accepted && response.reason = "");
    publish ".verified" raw;
    let expires = monotonic_clock () +. 5. in
    let rec wait () =
      if monotonic_clock () < expires then (Thread.delay 0.01; wait ()) in
    wait ();
    publish ".expired" hash;
    print_endline raw;
    exit 0
  | _ -> ());
  match Sys.getenv_opt "OCTRA_HOST_DELAY_FILE" with
  | Some path when path <> "" ->
    Pvac_ffi.isolate_worker ();
    let input = Buffer.create 1024 in
    (try while true do Buffer.add_channel input stdin 1024 done with End_of_file -> ());
    let request = Core.Pvac_verify_protocol.request_of_string (Buffer.contents input) |> P.unwrap in
    P.expect "incident proof request differs"
      (Core.Pvac_verify_protocol.request_hash request = Sys.getenv "OCTRA_HOST_DELAY_REQUEST");
    let channel = open_out_gen [Open_wronly; Open_creat; Open_append] 0o600 path in
    Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () -> output_string channel "received\n");
    let result = Core.Pvac_verify_direct.response ~keyed:false request in
    P.expect "incident proof refused" result.accepted;
    let channel = open_out_gen [Open_wronly; Open_creat; Open_append] 0o600 path in
    Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () -> output_string channel "verified\n");
    Unix.sleep 15;
    print_endline (Core.Pvac_verify_protocol.response_bytes result);
    exit 0
  | _ -> ()

let after_refusal ~make_deps ~verify_deps dir (sample : Circle_store.sample) =
  let head = R.HM.load dir |> Option.get in
  let finalize = Circle_store.signed sample head in
  Circle_store.stage dir sample finalize;
  N.Consensus_finality_journal.promote_applied dir ~epoch:finalize.epoch_id
    ~state_root:finalize.header.proposed_state_root;
  let record = Circle_store.read_range dir sample in
  let cursor = N.Consensus_join_rpc.{epoch = finalize.epoch_id;
    prev_root = sample.head.state_root; eic = Option.get sample.head.epoch_index_root;
    txid = Int64.succ sample.head.txid_hi} in
  let imported = N.Consensus_join_rpc.prepare_record ~chain_id:P.chain
    ~expected_validator_set_hash:(Octra_consensus.C_config.validator_set_hash
      sample.parent.validator_set) ~cursor record in
  let confirmed = List.map fst sample.prepared.execution.artifacts.confirmed in
  P.expect "refused proof entered exported transactions" (imported.txs = confirmed);
  P.expect "refused proof receipts missing" (record.receipts_json = Circle_store.receipts sample);
  let call = List.find (fun tx -> tx.T.op_type = T.CircleCall) sample.txs in
  let sender = List.init 7 (fun i -> P.identity (i + 1))
    |> List.find (fun item -> item.P.address = call.from) in
  let payment = P.signed ~ou:1000 sender ~op_type:T.Standard ~to_:sample.env.proposer_addr
    ~nonce:2 ~message:"" ~method_:None in
  Core.Tx_staging.remove_processed (List.map T.hash sample.txs);
  let root = R.with_stores dir (fun chaindata store ->
    ignore (Lwt_main.run (R.Recovery.recover ~data_dir:dir ~chaindata ~store));
    let root = Lwt_main.run (S.state_hash store) in
    P.expect "refused proof recovery changed root" (root = sample.prepared.execution.post_state_root);
    P.expect "refused proof recovery changed head" (R.HM.load dir = Some head);
    P.expect "refused proof recovery wrote history" (R.SC.get_tx_by_hash chaindata (T.hash call) = None);
    P.unwrap (Core.Tx_staging.add_smart ~head:(Int64.of_int head.epoch_id)
      ~lookup:(fun address -> Lwt_main.run (S.get_account store address)
        |> Option.map (fun (account : Core.Ledger_types.account) -> account.balance, account.nonce)) payment)
      |> ignore;
    root) in
  let next, header, _, _, outcome = prepare ~resume:sample ~queued:true ~make_deps ~verify_deps dir in
  P.expect "payment after refusal was omitted" (outcome = `Complete);
  P.expect "refused proof prevented next payment" (next.txs = [payment]);
  Circle_store.apply dir next "complete" "success";
  let current = Circle_store.inspect dir next true in
  P.expect "payment after refusal root differs" (P.root_raw current.state_root = header.CT.proposed_state_root);
  R.with_stores dir (fun chaindata store ->
    List.iter (fun _ -> ignore (Lwt_main.run (R.Recovery.recover ~data_dir:dir ~chaindata ~store))) [(); ()];
    P.expect "payment after refusal recovery changed head" (R.HM.load dir = Some current);
    P.expect "payment after refusal recovery changed root"
      (Lwt_main.run (S.state_hash store) = next.prepared.execution.post_state_root
        && next.prepared.execution.post_state_root <> root);
    let account = Lwt_main.run (S.get_account store sender.address) |> Option.get in
    P.expect "payment after refusal debit differs"
      (account.nonce = 2 && Z.equal account.balance (Z.sub initial (Z.of_int 20_001_001)));
    P.expect "payment after refusal history differs"
      (R.SC.get_tx_by_hash chaindata (T.hash call) = None
       && Option.is_some (R.SC.get_tx_by_hash chaindata (T.hash payment))
       && current.txid_hi = Int64.succ head.txid_hi));
  Core.Tx_staging.remove_processed [T.hash payment];
  P.expect "payment after refusal remained pending" (Core.Tx_staging.staging_size () = 0);
  `Assoc ["hash", `String (T.hash payment); "epoch", `Int current.epoch_id;
    "txid_hi", `String (Int64.to_string current.txid_hi); "state_root", `String current.state_root]

let range_inputs input =
  let raw = In_channel.with_open_bin input In_channel.input_all in
  P.expect "range input digest differs" (Digestif.SHA256.(to_hex (digest_string raw))
    = "57fa6b9809d67f78c3414ac4f24807616951ef0e6b6b506ec9573a20485d35de");
  In_channel.with_open_bin input (fun stream ->
    let fields = List.map (fun name ->
      let size = input_binary_int stream in
      P.expect "range input too large" (size > 0 && size <= 8 * 1024 * 1024);
      name, really_input_string stream size) ["key"; "cipher"; "proof"; "point"; "digest"] in
    P.expect "range input has extra bytes" (match input_char stream with
      | _ -> false | exception End_of_file -> true);
    fields)

let change_inner proof =
  P.expect "range proof encoding differs" (String.length proof >= 366
    && String.sub proof 0 6 = "PVAC\003\007");
  let rounds = String.get_int64_le proof 358 in
  P.expect "range proof rounds differ" (rounds = 19L);
  let offset = 366 + (64 * Int64.to_int rounds) in
  P.expect "range proof scalar missing" (String.length proof >= offset + 32);
  let zero = String.make 32 '\000' in
  let scalar = if String.sub proof offset 32 = zero then "\001" ^ String.sub zero 1 31 else zero in
  String.sub proof 0 offset ^ scalar ^ String.sub proof (offset + 32) (String.length proof - offset - 32)

let proof_path ?(retry = false) ?(calls = 1) ?(range = false) ?(cold = false) ?(accepted = true) ~make_deps ~verify_deps ~input ~output () =
  let open Yojson.Safe.Util in
  let controlled = retry && range && cold && accepted in
  let seed_base = if controlled then 2_000_100 else 2_000_000 in
  let large = if range then range_inputs input else [] in
  let samples = if range then [`Null] else begin
    let samples = Yojson.Safe.from_file input |> member "samples" |> to_list in
    P.expect "proof samples missing" (List.length samples = 16);
    if accepted then begin
      let samples = List.filter (fun row -> member "accepted" row |> to_bool) samples in
      P.expect "positive proof samples missing" (List.length samples = 8);
      samples
    end else begin
      let sample = List.hd samples in
      P.expect "late proof sample accepted" (not (member "accepted" sample |> to_bool));
      [sample]
    end
  end in
  let read sample name = if range then List.assoc name large else
    let path = Filename.concat (Filename.dirname input) (member name sample |> to_string) in
    let raw = In_channel.with_open_bin path In_channel.input_all in
    P.expect "proof input digest differs"
      (Digestif.SHA256.(to_hex (digest_string raw)) = (member (name ^ "_sha256") sample |> to_string));
    raw in
  P.unwrap (Core.Pvac_verify_worker.ready_sync ());
  let owner = Core.Proof_keeper.current (Sys.getenv "OCTRA_PVAC_VERIFY_WORKER") |> Option.get in
  let deadline = monotonic_clock () +. 120. in
  let rec ready () = match Core.Proof_keeper.phase owner with
    | Core.Proof_warm.Ready _ -> ()
    | Core.Proof_warm.Closed -> failwith "proof session closed"
    | _ when monotonic_clock () >= deadline -> failwith "proof session not ready"
    | _ -> Thread.delay 0.01; ready () in
  if not retry || controlled then ready ();
  let settings = [Core.Validator_policy.env_name, "100";
    "OCTRA_BFT_RELEASE_PROFILE", "devnet_full_v1";
    "OCTRA_EMISSION_PROFILE", ""; "OCTRA_EMISSION_ACTIVATION_EPOCH", ""] in
  let previous = List.map (fun (name, _) -> name, Option.value (Sys.getenv_opt name) ~default:"") settings in
  let head = !(R.HM.cached) in
  Fun.protect ~finally:(fun () ->
    List.iter (fun (name, value) -> Unix.putenv name value) previous;
    if retry then Core.Tx_staging.clear ();
    R.HM.cached := head) (fun () ->
  List.iter (fun (name, value) -> Unix.putenv name value) settings;
  let cuts = ref 0 in
  let encode sample =
    let key = read sample "key" and cipher = read sample "cipher" in
    let proof = read sample "proof" and point = read sample "point" in
    let proof = if range && not accepted then change_inner proof else proof in
    Core.Circle_wasm_codec.encode_request ~method_name:(if range then "fhe_verify_range" else "fhe_verify_bound")
      [`String (Base64.encode_exn key);
       `String (Core.Crypto.FheBalance.prefix ^ Base64.encode_exn cipher);
       `String ((if range then Core.Crypto.FheBalance.range_proof_prefix
          else Core.Crypto.FheBalance.zero_proof_prefix) ^ Base64.encode_exn proof);
       `String (Base64.encode_exn point)] |> P.unwrap in
  let following = if calls = 1 then [] else [encode (List.nth samples 1)] in
  let samples = if calls = 1 then samples else [List.hd samples] in
  let samples = List.concat_map (fun sample ->
    if controlled then [sample, false; sample, true] else [sample, false]) samples in
  List.iteri (fun index (sample, first_window) ->
    let request = encode sample in
    let dir = Filename.concat output ("state_" ^ string_of_int index) in
    Unix.mkdir dir 0o700;
    if retry then Core.Tx_staging.clear ();
    let before = Core.Proof_keeper.stats owner in
    let cut_hash = if not controlled then None else
      let proof = Core.Pvac_verify_protocol.Range_bound {
        pubkey = read sample "key";
        cipher = Core.Crypto.FheBalance.prefix ^ Base64.encode_exn (read sample "cipher");
        proof = Core.Crypto.FheBalance.range_proof_prefix ^ Base64.encode_exn (read sample "proof");
        commitment = Base64.encode_exn (read sample "point"); strict = true} in
      Some (Core.Pvac_verify_worker.with_rules ~keyed:true
        ~math:(G.math_at ~chain_id:P.chain ~epoch:(seed_base + (4 * index)) = G.Active) proof
        |> Core.Pvac_verify_protocol.request_hash) in
    let cut_clock, cut_armed, cut_label = if first_window then 8.25, 4.5, "proof_first_window"
      else 4., 0., "proof_cut" in
    let sample, header, proposed, verified, outcome =
      prepare ~request ?cut_hash ~cut_clock ~cut_armed ~cut_label ~range ~cold ~accepted
        ~queued:retry ~following ~seed_epoch:(seed_base + (4 * index)) ~make_deps ~verify_deps dir in
    let cut = not (List.exists (fun tx -> tx.T.op_type = T.CircleCall) sample.txs) in
    P.expect "verified proof missed first cut" (not controlled || cut);
    if cut then begin
      incr cuts;
      P.expect "first proof omission differs" (outcome = `Hold || outcome = `Window);
      if controlled then P.expect "controlled first proof outcome differs"
        (outcome = if first_window then `Window else `Hold);
      P.expect "proof cut lost payment" (List.length sample.txs = 1);
      P.expect "proof cut lost staging" (Core.Tx_staging.staging_size () = 2);
      let calls = Core.Tx_staging.get_epoch_txs ~capacity:Core.Tx_staging.max_ou
        |> List.filter (fun tx -> tx.T.op_type = T.CircleCall) in
      let call = match calls with [call] -> call | _ -> failwith "proof cut lost call" in
      let hash = T.hash call in
      let prior = Int64.of_int sample.head.epoch_id in
      (match outcome with
      | `Hold ->
        P.expect "proof cut did not hold input"
          (match Core.Tx_staging.queue_state ~head:prior ~confirmed:0 hash with
          | Some (Core.Tx_staging.Wait_preview {after; attempts = 1; _}) -> after = Int64.succ prior
          | _ -> false);
        P.expect "proof cut changed retry budget" (Core.Tx_staging.preview_limit hash 3.75 = 7.5);
        P.expect "proof cut allowed same head"
          (Result.is_error (Core.Tx_staging.Preview.send (Ready (Some prior, hash))))
      | `Window ->
        P.expect "first window changed queue"
          (match Core.Tx_staging.queue_state ~head:prior ~confirmed:0 hash with
          | Some (Core.Tx_staging.Ready _) -> true | _ -> false);
        P.expect "first window changed retry budget" (Core.Tx_staging.preview_limit hash 3.75 = 3.75);
        P.expect "first window blocked same head"
          (Result.is_ok (Core.Tx_staging.Preview.send (Ready (Some prior, hash))))
      | `Complete | `Renew | `Waiting -> assert false);
      R.with_stores dir (fun chaindata store ->
        let caller = Lwt_main.run (S.get_account store call.from) |> Option.get in
        P.expect "proof cut charged caller" (caller.nonce = 0 && Z.equal caller.balance initial);
        P.expect "proof cut wrote transaction" (R.SC.get_tx_by_hash chaindata hash = None);
        P.expect "proof cut wrote receipt" (R.SC.get_contract_receipt chaindata ~tx_hash:hash = None))
    end;
    let (), applied = measure (fun () -> Circle_store.apply dir sample "complete" "success") in
    let head = Circle_store.inspect dir sample true in
    P.expect "proof proposal root differs" (P.root_raw head.state_root = header.CT.proposed_state_root);
    P.expect "proof proposal receipts differ" (H.receipt_root (Circle_store.receipts sample) = header.receipt_root);
    let retry_result, last = if not cut then `Null, sample else begin
      Core.Tx_staging.remove_processed (List.map T.hash sample.txs);
      P.expect "proof commit lost retained call" (Core.Tx_staging.staging_size () = 1);
      let renewals = ref 0 in
      let rec include_call prior head attempts holds after windows =
        let call = match Core.Tx_staging.get_epoch_txs ~capacity:Core.Tx_staging.max_ou with
          | [call] -> call | _ -> failwith "proof retry lost call" in
        let queue = Core.Tx_staging.queue_state ~confirmed:0 (T.hash call) in
        P.expect "proof retry changed hold"
          (match queue with
          | Some (Core.Tx_staging.Wait_preview {after = actual; attempts; _}) -> actual = after && attempts = holds
          | Some (Core.Tx_staging.Ready _) -> holds = 0
          | _ -> false);
        let head_epoch = Int64.of_int head.R.HM.epoch_id in
        let waiting = head_epoch < after in
        P.expect "proof retry did not include call" (waiting || attempts < 3);
        ready ();
        let retry_cut = if controlled && not first_window && not waiting && attempts < 2
          then cut_hash else None in
        let cut_clock, cut_label = if attempts = 0 then 7.6, "proof_hold"
          else 8.25, "proof_window" in
        let next, header, proposed, verified, outcome =
          prepare ~request ?cut_hash:retry_cut ~cut_clock ~cut_label ~range ~cold ~accepted
            ~resume:prior ~following ~queued:true ~waiting ~make_deps ~verify_deps dir in
        if retry_cut <> None then P.expect "controlled proof outcome differs"
          (outcome = if attempts = 0 then `Hold else `Renew);
        let attempts = attempts + (if waiting then 0 else 1) in
        P.expect "proof retry did not advance head" (next.head = head);
        let (), applied = measure (fun () -> Circle_store.apply dir next "complete" "success") in
        let final = Circle_store.inspect dir next true in
        P.expect "proof retry root differs" (P.root_raw final.state_root = header.CT.proposed_state_root);
        if outcome = `Window || outcome = `Waiting then P.expect "proof commit changed hold"
          (Core.Tx_staging.queue_state ~confirmed:0 (T.hash call) = queue);
        match outcome with
        | `Complete ->
          P.expect "proof retry selected another input" (next.txs = [call]);
          call, next, final, proposed, verified, applied, attempts, windows
        | `Hold | `Renew | `Window | `Waiting ->
          P.expect "deferred proof selected another input" (next.txs = []);
          P.expect "deferred proof advanced cursor" (final.txid_hi = 1L);
          P.expect "deferred proof lost staging" (Core.Tx_staging.staging_size () = 1);
          let call = match Core.Tx_staging.get_epoch_txs ~capacity:Core.Tx_staging.max_ou with
            | [call] -> call | _ -> failwith "deferred proof missing" in
          R.with_stores dir (fun chaindata store ->
            let account = Lwt_main.run (S.get_account store call.from) |> Option.get in
            P.expect "deferred proof charged caller" (account.nonce = 0 && Z.equal account.balance initial);
            P.expect "deferred proof wrote history" (R.SC.get_tx_by_hash chaindata (T.hash call) = None);
            P.expect "deferred proof wrote receipt"
              (R.SC.get_contract_receipt chaindata ~tx_hash:(T.hash call) = None));
          let holds, after, windows = match outcome with
            | `Hold ->
              let holds = holds + 1 in
              holds, Int64.add head_epoch (Int64.of_int (1 lsl (holds - 1))), windows
            | `Renew ->
              incr renewals;
              holds, Int64.add head_epoch (Int64.of_int (1 lsl (holds - 1))), windows
            | `Window ->
              P.expect "proof repeated completion omission" (windows = 0);
              holds, after, windows + 1
            | `Waiting -> holds, after, windows
            | `Complete -> assert false in
          include_call next final attempts holds after windows in
      let prior = Int64.of_int sample.head.epoch_id in
      let holds, after, windows = if outcome = `Hold then 1, Int64.succ prior, 0
        else 0, prior, 1 in
      let call, last, final, proposed, verified, applied, eligible, windows =
        include_call sample head 0 holds after windows in
      P.expect "controlled proof cooldown differs"
        (not controlled || if first_window then windows = 1 else windows = 0 && !renewals = 1);
      P.expect "proof retry cursor differs" (final.txid_hi = if accepted then 2L else 1L);
      R.with_stores dir (fun chaindata store ->
        let caller = Lwt_main.run (S.get_account store call.from) |> Option.get in
        P.expect "proof retry fee or nonce differs"
          (caller.nonce = 1 && Z.equal caller.balance
            (Z.sub initial (Z.of_int (if accepted then 20_000_007 else 20_000_000))));
        let payer = (List.hd sample.txs).from in
        let payer = Lwt_main.run (S.get_account store payer) |> Option.get in
        P.expect "proof retry charged payment twice"
          (payer.nonce = 1 && Z.equal payer.balance (Z.sub initial (Z.of_int 1001)));
        P.expect "proof retry lost first transaction"
          (Option.is_some (R.SC.get_tx_by_hash chaindata (T.hash (List.hd sample.txs))));
        let storage = Lwt_main.run (S.load_circle_stable_storage store circle) |> P.unwrap in
        P.expect "proof retry lost verdict"
          (Hashtbl.find_opt storage "paid" = if accepted then Some "OCWS1\002\000\000\000\000" else None));
      Core.Tx_staging.remove_processed [T.hash call];
      P.expect "proof retry retained confirmed call" (Core.Tx_staging.staging_size () = 0);
      `Assoc ["proposal_ms", `Float (proposed *. 1000.);
        "verify_ms", `Float (verified *. 1000.); "apply_ms", `Float (applied *. 1000.);
        "head", `Int final.epoch_id; "deferred", `Int (eligible - 1);
        "eligible_attempts", `Int eligible; "windows", `Int windows; "renewals", `Int !renewals;
        "txid_hi", `String (Int64.to_string final.txid_hi)], last
    end in
    let continuation = if accepted then `Null else after_refusal ~make_deps ~verify_deps dir last in
    let after = Core.Proof_keeper.stats owner in
    P.expect "proof retry skipped verification"
      (not controlled || after.prepared > before.prepared
        || (first_window && after.reused > before.reused));
    let checks = (if cold then 3 else 4) * calls in
    P.expect "proof proposal session unused" (if accepted then
      after.prepared + after.reused >= before.prepared + before.reused + checks
      else after.refused >= before.refused + checks);
    let result = `Assoc ["proposal_ms", `Float (proposed *. 1000.);
      "verify_ms", `Float (verified *. 1000.); "apply_ms", `Float (applied *. 1000.);
      "first_cut", `Bool cut; "first_window", `Bool (outcome = `Window);
      "retry", retry_result; "accepted", `Bool accepted; "cold_verifier", `Bool cold;
      "payment_after_refusal", continuation;
      "request_sha256", `String (Digestif.SHA256.(to_hex (digest_string request)));
      "accepted_checks", `Int (after.prepared - before.prepared);
      "reused_checks", `Int (after.reused - before.reused); "refused_checks", `Int (after.refused - before.refused);
      "calls", `Int calls; "passed", `Bool true] in
    Out_channel.with_open_gen [Open_wronly; Open_creat; Open_excl; Open_binary] 0o600
      (Filename.concat output ("sample_" ^ string_of_int index ^ ".json"))
      (fun stream -> output_string stream (Yojson.Safe.pretty_to_string result ^ "\n"))) samples;
  P.expect "proof retry never crossed first slice"
    (not retry || not accepted || (range && not controlled) || !cuts > 0))

let run ?(cut_only = false) ~make_deps ~verify_deps () =
  let settings = [Core.Validator_policy.env_name, "100";
    "OCTRA_BFT_RELEASE_PROFILE", "devnet_full_v1";
    "OCTRA_EMISSION_PROFILE", ""; "OCTRA_EMISSION_ACTIVATION_EPOCH", ""] in
  let previous = List.map (fun (name, _) -> name, Option.value (Sys.getenv_opt name) ~default:"") settings in
  let head = !(R.HM.cached) in
  Fun.protect ~finally:(fun () ->
    List.iter (fun (name, value) -> Unix.putenv name value) previous;
    R.HM.cached := head) (fun () ->
    List.iter (fun (name, value) -> Unix.putenv name value) settings;
    if cut_only then List.iter (fun held ->
      Test_workspace.with_dir "active_cut" (cut ~held ~make_deps)) [false; true]
    else begin
    let pid = Unix.create_process Sys.executable_name [|Sys.executable_name; "--active-cut"|]
      Unix.stdin Unix.stdout Unix.stderr in
    P.expect "active cut child failed" (Test_workspace.wait pid = Unix.WEXITED 0);
    Test_workspace.with_dir "circle_host" (fun dir ->
      let sample, header, proposed, verified, outcome = prepare ~make_deps ~verify_deps dir in
      P.expect "host block omitted input" (outcome = `Complete);
      let (), applied = measure (fun () -> Circle_store.apply dir sample "complete" "success") in
      inspect dir sample header;
      Printf.printf "event = circle_host transactions = 2 calls = 1 proposal_ms = %.3f verify_ms = %.3f apply_ms = %.3f status = pass\n%!"
        (proposed *. 1000.) (verified *. 1000.) (applied *. 1000.))
    end)