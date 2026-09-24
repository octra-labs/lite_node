(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Exit_case
open Lwt.Syntax

module Service = Exit_service
module Checkpoint = Octra_bootstrap.State_sync_checkpoint
module Manifest = Octra_bootstrap.State_sync_manifest
module Capture = Octra_bootstrap.Sync_capture
module Anchor = Octra_bootstrap.Sync_anchor
module Client = Octra_bootstrap.State_sync_client
module Source = Octra_bootstrap.State_sync_source
module Sync = Octra_bootstrap.State_sync
module Roots = Octra_bootstrap.Root_win
module Head = Octra_core.Head_manifest
module Http = Octra_node_runtime.State_sync_http

let hash value = Digestif.SHA256.(digest_string value |> to_hex)

let local_file path =
  if Sys.file_exists path then
    Some (In_channel.with_open_bin path In_channel.input_all, (Unix.stat path).st_perm)
  else None

let wallet (key : key) = Octra_core.Crypto.Wallet.{
  priv = Base64.encode_exn key.secret;
  pub = Base64.encode_exn key.public;
  address = key.address;
}

let validator_set =
  List.map (fun (key : key) -> C.{ address = key.address; pubkey = key.public })
    peers
  |> C.make_validator_set

let finalize ~epoch ~root ~txid prior =
  let parent_commit = Option.map (fun value -> C.{
    certificate = C.certificate_of_finalize value; validator_set;
  }) prior in
  let header = C.{
    proto_version = proto_version_current; chain_id; epoch_id = Int64.of_int epoch;
    prev_state_root = Option.fold ~none:(root_bytes (hash "snapshot start"))
      ~some:(fun value -> value.C.header.proposed_state_root) prior;
    tx_list_hash = H.tx_list_hash []; receipt_root = H.receipt_root [];
    proposed_state_root = root_bytes root;
    parent_commit_hash = H.parent_commit_hash_opt parent_commit;
    creator_addr = (List.hd peers).address; txid_hi = txid;
    ts = float_of_int epoch *. 10.;
  } in
  let proposal_id = H.proposal_id header in
  let precommits = List.map (fun (key : key) ->
    let vote = C.{
      chain_id; epoch_id = header.epoch_id; round = 0; vote_type = Precommit;
      proposal_id; validator = key.address; signature = String.make 64 '\000';
    } in
    { vote with signature = H.sign_ed25519 ~priv_raw:key.secret
        ~msg:(H.vote_sign_bytes vote) }) peers in
  C.{ chain_id; epoch_id = header.epoch_id; commit_round = 0; header;
      proposal_id; precommits; parent_commit }

let capture (node : Service.t) publisher =
  let* selected = S.capture_read_snapshot node.store in
  let selected = get "snapshot source" selected in
  expect "snapshot selected ledger differs" (selected.state_root = node.view.root);
  let index = hash "exit snapshot index" in
  let index_root = hash "exit snapshot index root" in
  let root = Octra_core.Epoch_index_commitment.folded_state_root
    ~ledger_state_root:selected.state_root ~epoch_index_root:index_root in
  let txid = Int64.of_int node.view.account.nonce in
  let history = List.init (Roots.width + 1) (fun offset ->
    node.head - Roots.width + offset)
    |> List.fold_left (fun history epoch ->
      let prior = match history with [] -> None | head :: _ -> Some head in
      finalize ~epoch ~root ~txid prior :: history) [] in
  let final = List.hd history in
  let roots = List.tl history in
  ignore (Roots.verify ~anchor:final roots |> get "snapshot root chain");
  let signers = Anchor.encoded_validator_set validator_set |> get "snapshot signer encoding" in
  let config_hash = hash "exit snapshot config" in
  let head = Head.{
    schema_version = Head.schema_version; generation = node.head; epoch_id = node.head;
    state_root = root; ledger_state_root = Some selected.state_root;
    irmin_commit = Some selected.commit_hash; txid_hi = txid;
    txlog_seg = Some 0; txlog_off = Some Octra_core.Txlog.header_size;
    epochlog_off = Some Octra_core.Epochlog.header_size;
    commit_id = "exit-snapshot"; ts = float_of_int node.head *. 10.;
    quorum_cert_hash = None; epoch_index_hash = Some index;
    epoch_index_root = Some index_root;
  } in
  let now = Int64.of_float (Unix.gettimeofday ()) in
  let checkpoint = Checkpoint.of_head ~chain_id ~config_hash
    ~validator_set_hash:(Manifest.set_hash validator_set)
    ~created_at:(Int64.pred now) ~valid_until:(Int64.add now 3600L) head in
  let id = Checkpoint.hash checkpoint |> get "snapshot checkpoint" in
  let target = Sync.snapshot_dir publisher id in
  let* built = Capture.build
    Capture.{ data_dir = node.data_dir; head; store = node.store; roots } ~target in
  ignore (get "snapshot capture" built);
  let draft = Manifest.build ~checkpoint ~source_dir:target
    ~chunk_size:Manifest.chunk_size_min |> get "snapshot manifest" in
  let certificate = Manifest.{
    checkpoint; checkpoint_hash = draft.checkpoint_hash;
    authority = Finalized (Anchor.make ~steps:[] ~finalize:final ~validator_set
      |> Anchor.encode);
    manifest = draft.manifest; manifest_hash = draft.manifest_hash;
    exporter_signatures = [Manifest.make_exporter_signature
      ~wallet:(wallet (List.hd peers)) draft.manifest |> get "snapshot signature"];
  } in
  ignore (Manifest.verify_reference_certificate ~validator_set:signers
    ~exporter_set:signers certificate |> get "snapshot certificate");
  let invalid = { certificate with exporter_signatures =
    List.map (fun signature -> { signature with Checkpoint.signature =
      Base64.encode_exn (String.make 64 '\000') }) certificate.exporter_signatures }
  in
  expect "invalid snapshot signature accepted"
    (Manifest.verify_reference_certificate ~validator_set:signers
      ~exporter_set:signers invalid = Error "invalid checkpoint signature");
  expect "private identity entered snapshot"
    (not (List.exists (fun (file : Manifest.file) ->
      String.split_on_char '/' file.path
      |> List.exists (fun part -> List.mem part
        [".keys"; "validator-control"; "wallet.json"; "node.env"; "enrollment.json"]))
      draft.manifest.files));
  Manifest.write_json (Sync.snapshot_certificate_path target)
    (Manifest.certificate_json certificate);
  Lwt.return (certificate, signers)

let download publisher target certificate signers =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) in
  Lwt_unix.listen socket 8;
  let port = match Lwt_unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> failwith "snapshot listener differs" in
  let checkpoint = certificate.Manifest.checkpoint in
  let config_hash = checkpoint.config_hash in
  let entries = List.map (fun (key : key) ->
    key.address ^ ":" ^ Base64.encode_exn key.public) peers |> String.concat "," in
  let settings = [
    "OCTRA_STATE_SYNC_ENABLE", "1";
    "OCTRA_STATE_SYNC_CERT", Sync.snapshot_certificate_path
      (Sync.snapshot_dir publisher certificate.manifest.snapshot_id);
    "OCTRA_STATE_SYNC_EXPORTERS", entries;
    "OCTRA_VALIDATORS", entries;
    "OCTRA_CONSENSUS_CONFIG_HASH", config_hash;
  ] in
  let prior = List.map (fun (name, _) -> name, Sys.getenv_opt name) settings in
  let old_chain = !Client.chain_id and old_config = !Client.config_hash in
  let old_epoch = !Client.min_epoch in
  List.iter (fun (name, value) -> Unix.putenv name value) settings;
  Client.chain_id := chain_id;
  Client.config_hash := config_hash;
  Client.min_epoch := checkpoint.epoch;
  let requests = ref 0 in
  let corrupt = ref true in
  let callback _ request _ =
    let uri = Cohttp.Request.uri request in
    match Uri.path uri with
    | "/state-sync/manifest" ->
      Http.handle_manifest ~data_dir:publisher ~chain_id ~config_hash
        ~validator_set:signers ~current_epoch:(ref (Int64.to_int checkpoint.epoch))
    | "/state-sync/chunk" ->
      incr requests;
      let* response, body = Http.handle_chunk ~data_dir:publisher ~chain_id ~config_hash
        ~validator_set:signers (Uri.query uri) in
      if not !corrupt then Lwt.return (response, body)
      else
        let* payload = Cohttp_lwt.Body.to_string body in
        expect "test chunk body is empty" (payload <> "");
        let altered = Bytes.of_string payload in
        Bytes.set altered 0 (Char.chr (Char.code (Bytes.get altered 0) lxor 1));
        Lwt.return (response, Cohttp_lwt.Body.of_string (Bytes.to_string altered))
    | _ -> Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body:"" ()
  in
  let server = Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Socket socket))
    (Cohttp_lwt_unix.Server.make ~callback ()) in
  Lwt.finalize (fun () ->
    let source = Source.create ~allow_private_http:false
      (Printf.sprintf "http://127.0.0.1:%d" port) |> get "snapshot source URL" in
    let* source, fetched = Client.fetch_manifest signers signers source in
    expect "fetched certificate differs" (fetched = certificate);
    let file = List.find (fun (file : Manifest.file) -> file.path = "ledger.dat")
      fetched.manifest.files in
    let task = Client.{ file; partial = ""; chunk = List.hd file.chunks } in
    let* rejected = Lwt.catch
      (fun () -> let* _ = Client.fetch_chunk source fetched task in Lwt.return_false)
      (function
        | Failure reason when reason = "chunk hash mismatch" -> Lwt.return_true
        | exn -> Lwt.fail exn) in
    expect "corrupt snapshot chunk accepted" rejected;
    corrupt := false;
    let* () = Client.run_sync fetched [source] target in
    expect "snapshot did not download all chunks"
      (!requests >= fetched.manifest.chunk_count);
    expect "snapshot verification marker absent"
      (Sys.file_exists (Filename.concat target "snapshot_verified.json"));
    Lwt.return_unit)
    (fun () ->
      Lwt.cancel server;
      Client.chain_id := old_chain;
      Client.config_hash := old_config;
      Client.min_epoch := old_epoch;
      List.iter (fun (name, value) ->
        Unix.putenv name (Option.value ~default:"" value)) prior;
      Lwt.return_unit)

let restore (node : Service.t) =
  let* () = Service.stop_duty node in
  let before = node.view in
  let control = Filename.concat node.data_dir "validator-control" in
  let private_paths = ["wallet.json"; "node.env"; "enrollment.json"] @
    (Sys.readdir control |> Array.to_list
      |> List.filter (fun name -> Filename.check_suffix name ".json")
      |> List.map (Filename.concat "validator-control")) in
  let identity = List.map (fun relative ->
    let path = Filename.concat node.data_dir relative in
    path, local_file path) private_paths in
  let publisher = node.database ^ ".snapshot" in
  let target = node.database ^ ".download" in
  let* certificate, signers = capture node publisher in
  let* () = download publisher target certificate signers in
  let data = Filename.concat target "data" in
  let database = Filename.concat data "irmin_store" in
  let* store = S.open_store ~readonly:true database in
  let* () = Lwt.finalize (fun () ->
    let ledger = L.create store in
    let* restored = view_lwt store ledger in
    expect "snapshot changed exited account or escrow" (restored = before);
    Lwt.return_unit) (fun () -> S.close store) in
  let* () = S.close node.store in
  Service.History.close node.history;
  Unix.rename node.database (node.database ^ ".before");
  Unix.rename database node.database;
  Unix.rename (node.database ^ ".history") (node.database ^ ".history.before");
  Unix.rename (Filename.concat data "chaindata") (node.database ^ ".history");
  let* store = S.open_store node.database in
  node.store <- store;
  node.ledger <- L.create store;
  node.history <- Service.History.open_chaindata (node.database ^ ".history");
  node.control <- Service.Control.create ~data_dir:node.data_dir;
  node.duty <- Some (Service.actor node);
  List.iter (fun (path, prior) ->
    expect "snapshot changed local identity or signed intent"
      (local_file path = prior)) identity;
  let* restored = view_lwt node.store node.ledger in
  expect "installed snapshot changed account" (restored = before);
  List.iter (fun tx ->
    expect "snapshot retained a pruned transaction receipt"
      (Service.History.get_tx_by_hash node.history (T.hash tx) = None)
  ) node.submitted;
  Printf.printf "status = pass test = exit_snapshot epoch = %d chunks = %d\n%!"
    node.head certificate.manifest.chunk_count;
  Lwt.return_unit