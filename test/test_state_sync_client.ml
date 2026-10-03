(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Infix

module Checkpoint = Octra_bootstrap.State_sync_checkpoint
module Client = Octra_bootstrap.State_sync_client
module Manifest = Octra_bootstrap.State_sync_manifest
module Journal = Octra_bootstrap.State_sync_journal
module Source = Octra_bootstrap.State_sync_source
module State_sync = Octra_bootstrap.State_sync
module Http = Octra_node_runtime.State_sync_http
module Head = Octra_core.Head_manifest

let fail message =
  failwith ("test_state_sync_client: " ^ message)

let expect_ok = function
  | Ok value -> value
  | Error reason -> fail reason

let expect_error = function
  | Error _ -> ()
  | Ok _ -> fail "expected error"

let sha value =
  Digestif.SHA256.(digest_string value |> to_hex)

let mkdir_p path =
  let rec loop current =
    if current = "" || current = "." || Sys.file_exists current then ()
    else begin
      loop (Filename.dirname current);
      Unix.mkdir current 0o755
    end
  in
  loop path

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Sys.readdir path
      |> Array.iter (fun name ->
        if name <> "." && name <> ".." then
          remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let write_file path value =
  mkdir_p (Filename.dirname path);
  let output = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr output)
    (fun () -> output_string output value)

let wallet seed =
  let private_raw = String.make 32 (Char.chr seed) in
  let private_key =
    match Mirage_crypto_ec.Ed25519.priv_of_octets private_raw with
    | Ok key -> key
    | Error _ -> fail "private key creation failed"
  in
  let public_key =
    Mirage_crypto_ec.Ed25519.pub_of_priv private_key
    |> Mirage_crypto_ec.Ed25519.pub_to_octets
    |> Base64.encode_exn
  in
  Octra_core.Crypto.Wallet.{
    priv = Base64.encode_exn private_raw;
    pub = public_key;
    address = Octra_core.Crypto.Address.address_from_pubkey public_key;
  }

let signer_set wallets =
  wallets
  |> List.map (fun wallet ->
    Octra_consensus.C_types.{
      address = wallet.Octra_core.Crypto.Wallet.address;
      pubkey = wallet.pub;
    })
  |> Octra_consensus.C_types.make_validator_set

let runtime_set wallets =
  wallets
  |> List.map (fun wallet ->
    Octra_consensus.C_types.{
      address = wallet.Octra_core.Crypto.Wallet.address;
      pubkey = Base64.decode_exn wallet.pub;
    })
  |> Octra_consensus.C_types.make_validator_set

let chunks payload chunk_size =
  let rec loop index offset items =
    if offset >= String.length payload then List.rev items
    else
      let size = min chunk_size (String.length payload - offset) in
      let part = String.sub payload offset size in
      let chunk = Manifest.{
        index;
        offset = Int64.of_int offset;
        size;
        sha256 = sha part;
      } in
      loop (index + 1) (offset + size) (chunk :: items)
  in
  loop 0 0 []

let manifest_file path payload chunk_size =
  Manifest.{
    path;
    size = Int64.of_int (String.length payload);
    sha256 = sha payload;
    chunks = chunks payload chunk_size;
  }

let sample ?(valid_for = 3_600L) wallets payload state_label =
  let validators = signer_set wallets in
  let exporters = signer_set [List.hd wallets] in
  let state_root = sha state_label in
  let ledger_root = String.make 128 'a' in
  let config_hash = sha "config" in
  let head = Octra_core.Head_manifest.{
    schema_version = 3;
    generation = 777;
    epoch_id = 777;
    state_root;
    ledger_state_root = Some ledger_root;
    irmin_commit = Some (String.make 128 'b');
    txid_hi = 400L;
    txlog_seg = Some 0;
    txlog_off = Some 10;
    epochlog_off = Some 20;
    commit_id = "ep777-test";
    ts = 10.0;
    quorum_cert_hash = Some (sha "qc");
    epoch_index_hash = Some (sha "index");
    epoch_index_root = Some (sha "index-root");
  } in
  let now = Int64.of_float (Unix.gettimeofday ()) in
  let checkpoint =
    Checkpoint.of_head
      ~chain_id:"octra-devnet-bft"
      ~config_hash
      ~validator_set_hash:(Manifest.set_hash validators)
      ~created_at:(Int64.sub now 10L)
      ~valid_until:(Int64.add now valid_for)
      head
  in
  let checkpoint_hash = expect_ok (Checkpoint.hash checkpoint) in
  let chunk_size = Manifest.chunk_size_min in
  let head_json = Octra_core.Head_manifest.to_json head in
  let files = [
    manifest_file "HEAD.json" head_json chunk_size;
    manifest_file "chaindata/blob" payload chunk_size;
  ] in
  let manifest = Manifest.{
    checkpoint_hash;
    snapshot_id = checkpoint_hash;
    irmin_commit = head.irmin_commit;
    chunk_size;
    total_size =
      List.fold_left (fun total file -> Int64.add total file.Manifest.size) 0L files;
    file_count = List.length files;
    chunk_count =
      List.fold_left (fun total file -> total + List.length file.Manifest.chunks) 0 files;
    chunks_root = Manifest.chunks_root files;
    files;
  } in
  let quorum_signatures =
    wallets
    |> List.filteri (fun index _ -> index < 4)
    |> List.map (fun wallet ->
      expect_ok (Manifest.make_checkpoint_signature ~wallet checkpoint))
    |> List.sort (fun left right ->
      String.compare left.Checkpoint.signer right.signer)
  in
  let certificate = Manifest.{
    checkpoint;
    checkpoint_hash;
    authority = Checkpoint_quorum quorum_signatures;
    manifest;
    manifest_hash = expect_ok (Manifest.manifest_hash manifest);
    exporter_signatures = [
      expect_ok (Manifest.make_exporter_signature ~wallet:(List.hd wallets) manifest);
    ];
  } in
  validators, exporters, config_hash, head_json, certificate

let listen () =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) >>= fun () ->
  Lwt_unix.listen socket 8;
  match Lwt_unix.getsockname socket with
  | Unix.ADDR_INET (_, port) -> Lwt.return (socket, port)
  | _ -> fail "listener address differs"

let server ?(reply = fun _ -> Lwt.return_none)
    ~socket ~data_dir ~chain_id ~config_hash ~validator_set ~requests () =
  let callback _connection request _body =
    let uri = Cohttp.Request.uri request in
    match Uri.path uri with
    | "/state-sync/manifest" ->
        Http.handle_manifest
          ~data_dir
          ~chain_id
          ~config_hash
          ~validator_set
          ~current_epoch:(ref 777)
    | "/state-sync/chunk" ->
        requests := uri :: !requests;
        reply uri >>= (function
          | Some response -> Lwt.return response
          | None ->
              Http.handle_chunk
                ~data_dir
                ~chain_id
                ~config_hash
                ~validator_set
                (Uri.query uri))
    | _ ->
        Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body:"" ()
  in
  Cohttp_lwt_unix.Server.create
    ~mode:(`TCP (`Socket socket))
    (Cohttp_lwt_unix.Server.make ~callback ())

let expired_server ~socket requests =
  let callback _connection request _body =
    if Uri.path (Cohttp.Request.uri request) = "/state-sync/chunk" then
      incr requests;
    Cohttp_lwt_unix.Server.respond_string
      ~status:`Gone
      ~body:"{\"status\":\"error\",\"error\":{\"type\":\"state_sync_snapshot_expired\",\"reason\":\"snapshot retired\"}}"
      ()
  in
  Cohttp_lwt_unix.Server.create
    ~mode:(`TCP (`Socket socket))
    (Cohttp_lwt_unix.Server.make ~callback ())

let source_url port =
  Printf.sprintf "http://127.0.0.1:%d" port

let test_cancel root start =
  listen () >>= fun (socket, port) ->
  let started, signal = Lwt.wait () in
  let blocked, _ = Lwt.task () in
  let requests = ref 0 in
  let callback _connection _request _body =
    incr requests;
    if Lwt.is_sleeping started then Lwt.wakeup_later signal ();
    blocked
  in
  let service = Cohttp_lwt_unix.Server.create
    ~mode:(`TCP (`Socket socket))
    (Cohttp_lwt_unix.Server.make ~callback ()) in
  let source = expect_ok (Source.create ~allow_private_http:false (source_url port)) in
  let work = start source root in
  Lwt.finalize
    (fun () ->
      Lwt_unix.with_timeout 2.0 (fun () -> started) >>= fun () ->
      Lwt.cancel work;
      Lwt.pause () >>= fun () ->
      if source.failures <> 0 then fail "cancelled request counted as source failure";
      begin match Lwt.state work with
      | Lwt.Fail Lwt.Canceled -> ()
      | _ -> fail "cancelled download did not stop"
      end;
      Lwt_unix.sleep 1.1 >>= fun () ->
      if !requests <> 1 then fail "cancelled download sent another request";
      if Sys.file_exists (Filename.concat root "snapshot_verified.json") then
        fail "cancelled download installed a verified marker";
      Lwt.return_unit)
    (fun () ->
      Lwt.cancel work;
      Lwt.cancel blocked;
      Lwt.cancel service;
      remove_tree root;
      Lwt.return_unit)

let test_choice_cancel root certificate =
  let blocked, _ = Lwt.task () in
  let attempts = ref 0 in
  let work = Client.sync_manifests
    ~max_bytes:Int64.max_int ~stage:root
    ~check:(fun _ _ -> Lwt.return_unit)
    ~sync:(fun _ _ _ -> incr attempts; blocked)
    [certificate, []; certificate, []] in
  Lwt.cancel work;
  Lwt.pause () >>= fun () ->
  if !attempts <> 1 then fail "cancelled selection started another manifest";
  match Lwt.state work with
  | Lwt.Fail Lwt.Canceled -> Lwt.return_unit
  | _ -> fail "cancelled selection did not stop"

let test_chunk_resume root certificate data_dir validators ~delay =
  listen () >>= fun (socket, port) ->
  let file = List.find (fun file -> file.Manifest.path = "chaindata/blob")
    certificate.Manifest.manifest.files in
  let last = List.hd (List.rev file.chunks) in
  let unavailable = ref true in
  let requests = ref [] in
  let reply uri =
    if !unavailable && Uri.get_query_param uri "path" = Some file.path
       && Uri.get_query_param uri "index" = Some (string_of_int last.index) then
      Lwt_unix.sleep delay >>= fun () ->
      Cohttp_lwt_unix.Server.respond_string
        ~status:`Service_unavailable ~body:"not ready" () >|= Option.some
    else Lwt.return_none
  in
  let service = server ~reply ~socket ~data_dir
    ~chain_id:certificate.checkpoint.chain_id
    ~config_hash:certificate.checkpoint.config_hash
    ~validator_set:validators ~requests () in
  let source () = expect_ok (Source.create ~allow_private_http:false (source_url port)) in
  let verified = ref 0 in
  let sync () = Client.run_sync certificate [source ()] root
    ~verify_state:(fun checkpoint data_dir ->
      incr verified;
      match Head.load_result data_dir with
      | Head.Present head when Checkpoint.matches_head checkpoint head -> Lwt.return_ok ()
      | _ -> Lwt.return_error "restored head differs") in
  Lwt.finalize
    (fun () ->
      Client.retries := 2;
      Lwt.try_bind sync
        (fun () -> Lwt.return_false)
        (function
          | Failure reason when reason = Printf.sprintf
              "chunk retry budget exhausted path = %s index = %d" file.path last.index ->
              Lwt.return_true
          | exn -> Lwt.fail exn) >>= fun refused ->
      if not refused then fail "exhausted chunk attempts were accepted";
      if !verified <> 0 then fail "incomplete snapshot reached state verification";
      let count path index = List.length (List.filter (fun uri ->
        Uri.get_query_param uri "path" = Some path
        && Uri.get_query_param uri "index" = Some (string_of_int index)) !requests) in
      if count file.path last.index <> 2 then fail "chunk attempts differ from policy";
      let journal_path = Filename.concat root "journal.jsonl" in
      let journal = expect_ok (Journal.open_journal
        ~path:journal_path ~manifest_hash:certificate.manifest_hash) in
      let first = List.hd file.chunks in
      let damaged = List.nth file.chunks 1 in
      if not (Journal.is_completed journal file.path first)
         || not (Journal.is_completed journal file.path damaged)
         || Journal.is_completed journal file.path last then
        fail "failed download lost its verified prefix";
      let _, partial, _ = expect_ok (Journal.prepare_file
        ~stage:(Filename.concat root "data") file) in
      let output = open_out_gen [Open_wronly; Open_binary] 0o600 partial in
      Fun.protect ~finally:(fun () -> close_out_noerr output) (fun () ->
        seek_out output (Int64.to_int damaged.offset);
        output_char output '\255');
      requests := [];
      unavailable := false;
      sync () >>= fun () ->
      if count "HEAD.json" 0 <> 0 || count file.path first.index <> 0 then
        fail "resumed download fetched verified bytes";
      if count file.path damaged.index <> 1 || count file.path last.index <> 1 then
        fail "resumed download did not repair only missing or damaged chunks";
      if !verified <> 1 then fail "completed snapshot was not verified once";
      let destination = Filename.concat (Filename.concat root "data") file.path in
      if expect_ok (Journal.hash_file destination) <> file.sha256 then
        fail "repaired file hash differs";
      let journal = expect_ok (Journal.open_journal
        ~path:journal_path ~manifest_hash:certificate.manifest_hash) in
      if not (List.for_all (Journal.is_completed journal file.path) file.chunks) then
        fail "resumed journal is incomplete";
      Lwt.return_unit)
    (fun () -> Lwt.cancel service; remove_tree root; Lwt.return_unit)

let test_rotation root wallets payload certificate data_dir validators =
  let lock = expect_ok (Journal.stage_lock root) in
  let test_lock blocked =
    match Unix.fork () with
    | 0 ->
        let acquired = match Journal.stage_lock root with
          | Ok fd -> Unix.close fd; true
          | Error _ -> false in
        Unix._exit (if acquired = not blocked then 0 else 1)
    | pid ->
        let rec wait () =
          try snd (Unix.waitpid [] pid) with
          | Unix.Unix_error (Unix.EINTR, _, _) -> wait () in
        if wait () <> Unix.WEXITED 0 then fail "stage lock did not isolate processes"
  in
  test_lock true;
  Unix.close lock;
  test_lock false;
  let changed = Bytes.of_string payload in
  Bytes.set changed Manifest.chunk_size_min '\255';
  let _, _, _, _, prior = sample wallets (Bytes.to_string changed) "previous" in
  let snapshots = Filename.concat root "snapshots" in
  let old = Filename.concat snapshots prior.Manifest.manifest_hash in
  let current = Filename.concat snapshots certificate.Manifest.manifest_hash in
  let file = List.find (fun file -> file.Manifest.path = "chaindata/blob")
    prior.manifest.files in
  let _, partial, _ = expect_ok (Journal.prepare_file ~stage:(Filename.concat old "data") file) in
  let journal = expect_ok (Journal.open_journal ~path:(Filename.concat old "journal.jsonl")
    ~manifest_hash:prior.manifest_hash) in
  List.iter (fun chunk ->
    if chunk.Manifest.index < 2 then begin
      let body = Bytes.sub_string changed (Int64.to_int chunk.offset) chunk.size in
      ignore (expect_ok (Journal.write_chunk ~partial chunk body));
      Journal.record_completed journal file.path chunk
    end) file.chunks;
  Unix.LargeFile.truncate partial (Int64.of_int (2 * Manifest.chunk_size_min));
  let discarded = Filename.concat snapshots (sha "discarded") in
  write_file (Filename.concat discarded "data/unused") "unused";
  let outside = Filename.concat root "outside" in
  write_file (Filename.concat outside "keep") "preserve";
  Unix.symlink (Unix.realpath outside) (Filename.concat discarded "link");
  let unknown = Filename.concat snapshots "notes" in
  write_file (Filename.concat unknown "keep") "preserve";
  let donor = expect_ok (Journal.select_donor ~stage:root ~current) in
  if donor <> Some (Filename.concat old "data") then fail "download donor selection differs";
  if Sys.file_exists discarded || not (Sys.file_exists (Filename.concat outside "keep"))
     || not (Sys.file_exists unknown) then fail "stage cleanup selected unrelated data";
  listen () >>= fun (socket, port) ->
  let requests = ref [] in
  let unavailable = ref true in
  let reply uri =
    if !unavailable && Uri.get_query_param uri "path" = Some file.path
       && Uri.get_query_param uri "index" = Some "2" then
      Cohttp_lwt_unix.Server.respond_string ~status:`Service_unavailable ~body:"wait" ()
      >|= Option.some
    else Lwt.return_none
  in
  let service = server ~reply ~socket ~data_dir
    ~chain_id:certificate.checkpoint.chain_id ~config_hash:certificate.checkpoint.config_hash
    ~validator_set:validators ~requests () in
  let verified = ref 0 in
  let sync () =
    let source = expect_ok (Source.create ~allow_private_http:false (source_url port)) in
    Client.run_sync ~stage:root certificate [source] current
      ~verify_state:(fun checkpoint data_dir ->
        incr verified;
        match Head.load_result data_dir with
        | Head.Present head when Checkpoint.matches_head checkpoint head -> Lwt.return_ok ()
        | _ -> Lwt.return_error "restored head differs")
  in
  let count index = List.length (List.filter (fun uri ->
    Uri.get_query_param uri "path" = Some file.path
    && Uri.get_query_param uri "index" = Some (string_of_int index)) !requests) in
  Lwt.finalize (fun () ->
    Client.retries := 1;
    Lwt.try_bind sync (fun () -> fail "missing suffix was accepted")
      (function
        | Failure reason when reason =
            "chunk retry budget exhausted path = chaindata/blob index = 2" -> Lwt.return_unit
        | exn -> Lwt.fail exn) >>= fun () ->
    if count 0 <> 0 || count 1 <> 1 || count 2 <> 1 || !verified <> 0 then
      fail "rotated download did not reuse only matching chunks";
    let journal = expect_ok (Journal.open_journal
      ~path:(Filename.concat current "journal.jsonl") ~manifest_hash:certificate.manifest_hash) in
    let first = List.hd file.chunks in
    if not (Journal.is_completed journal file.path first) then fail "local chunk was not journaled";
    requests := [];
    unavailable := false;
    sync () >>= fun () ->
    if count 0 <> 0 || count 1 <> 0 || count 2 <> 1 || !verified <> 1 then
      fail "restart lost rotated download progress";
    let target = Filename.concat current "data/chaindata/blob" in
    if expect_ok (Journal.hash_file target) <> sha payload then fail "rotated payload differs";
    let donor = Filename.concat old "data" in
    let output = Filename.concat current "probe" in
    write_file output (String.make first.size '\000');
    let reuse path = expect_ok (Journal.reuse_chunk ~donor ~path ~partial:output first) in
    write_file (Filename.concat donor "final") (String.sub payload 0 first.size);
    if not (reuse "final") then fail "completed donor file was not reused";
    write_file (Filename.concat donor "final") "short";
    if reuse "final" then fail "short donor chunk was accepted";
    write_file (Filename.concat donor "final") (String.make first.size '\255');
    if reuse "final" then fail "damaged donor chunk was accepted";
    Unix.symlink (Unix.realpath target) (Filename.concat donor "file-link");
    Unix.symlink (Unix.realpath (Filename.dirname target)) (Filename.concat donor "dir-link");
    if reuse "file-link" || reuse "dir-link/blob" || reuse "../probe" then
      fail "local chunk read followed an indirect path";
    Lwt.return_unit)
    (fun () -> Lwt.cancel service; remove_tree root; Lwt.return_unit)

let test_conflicting_quorum () =
  let wallets = List.init 5 (fun index -> wallet (index + 1)) in
  let _, _, _, _, left = sample wallets "payload" "state-left" in
  let _, _, _, _, right = sample wallets "payload" "state-right" in
  let first =
    expect_ok (Source.create ~allow_private_http:false "http://127.0.0.1:44001")
  in
  let second =
    expect_ok (Source.create ~allow_private_http:false "http://127.0.0.1:44002")
  in
  try
    ignore (Client.select_manifest [first, left; second, right]);
    fail "conflicting quorum checkpoints were accepted"
  with Client.Sync_error _ -> ()

let test_equivalent_quorum_proofs () =
  let wallets = List.init 5 (fun index -> wallet (index + 1)) in
  let _, _, _, _, left = sample wallets "payload" "state" in
  let checkpoint = {
    left.Manifest.checkpoint with
    quorum_cert_hash = Some (sha "other-qc");
  } in
  let checkpoint_hash = expect_ok (Checkpoint.hash checkpoint) in
  let manifest = {
    left.manifest with
    checkpoint_hash;
    snapshot_id = checkpoint_hash;
  } in
  let right = {
    left with
    checkpoint;
    checkpoint_hash;
    manifest;
    manifest_hash = expect_ok (Manifest.manifest_hash manifest);
  } in
  let first =
    expect_ok (Source.create ~allow_private_http:false "http://127.0.0.1:44001")
  in
  let second =
    expect_ok (Source.create ~allow_private_http:false "http://127.0.0.1:44002")
  in
  let selected = Client.select_manifests [first, left; second, right] in
  if List.length selected <> 2 then
    fail "equivalent quorum proofs were collapsed or rejected";
  if List.exists (fun (_, sources) -> List.length sources <> 1) selected then
    fail "equivalent quorum proof sources were mixed"

let test_distinct_byte_manifests () =
  let wallets = List.init 5 (fun index -> wallet (index + 1)) in
  let _, _, _, _, left = sample wallets "payload" "state" in
  let right = {
    left with
    manifest_hash = sha "other-manifest";
  } in
  let first =
    expect_ok (Source.create ~allow_private_http:false "http://127.0.0.1:44001")
  in
  let second =
    expect_ok (Source.create ~allow_private_http:false "http://127.0.0.1:44002")
  in
  let selected = Client.select_manifests [first, left; second, right] in
  if List.length selected <> 2 then fail "distinct byte manifests were collapsed";
  if List.exists (fun (_, sources) -> List.length sources <> 1) selected then
    fail "byte manifest sources were mixed"

let test_distinct_checkpoints () =
  let wallets = List.init 5 (fun index -> wallet (index + 1)) in
  let _, _, _, _, newer = sample wallets "newer" "state-newer" in
  let _, _, _, _, older = sample wallets "older" "state-older" in
  let older_checkpoint = {
    older.Manifest.checkpoint with
    epoch = Int64.pred newer.checkpoint.epoch;
  } in
  let older_hash = sha "older-checkpoint" in
  let older = {
    older with
    checkpoint = older_checkpoint;
    checkpoint_hash = older_hash;
    manifest = {
      older.manifest with
      checkpoint_hash = older_hash;
      snapshot_id = older_hash;
    };
    manifest_hash = sha "older-manifest";
  } in
  let first =
    expect_ok (Source.create ~allow_private_http:false "http://127.0.0.1:44001")
  in
  let second =
    expect_ok (Source.create ~allow_private_http:false "http://127.0.0.1:44002")
  in
  let selected = Client.select_manifests [second, older; first, newer] in
  begin
    match selected with
    | [(selected_newer, [newer_source]); (selected_older, [older_source])]
      when selected_newer.checkpoint_hash = newer.checkpoint_hash
           && selected_older.checkpoint_hash = older.checkpoint_hash
           && newer_source.Source.url = first.url
           && older_source.Source.url = second.url ->
        ()
    | _ -> fail "older quorum checkpoint is not available after a byte failure"
  end;
  let attempts = ref [] in
  Lwt_main.run
    (Client.sync_manifests
       ~max_bytes:Int64.max_int
       ~stage:"runtime_data/state_sync_selection"
       ~check:(fun _ _ -> Lwt.return_unit)
       ~sync:(fun certificate _ _ ->
         attempts := certificate.Manifest.checkpoint_hash :: !attempts;
         if certificate.checkpoint_hash = newer.checkpoint_hash then
           Lwt.fail_with "newer source unavailable"
         else
           Lwt.return_unit)
       selected);
  if List.rev !attempts <> [newer.checkpoint_hash; older.checkpoint_hash] then
    fail "client did not retry the older whole checkpoint";
  let oversized = {
    newer with
    manifest = { newer.manifest with total_size = Int64.max_int };
  } in
  let checks = ref [] in
  let selected_hashes = ref [] in
  Lwt_main.run
    (Client.sync_manifests
       ~max_bytes:older.manifest.total_size
       ~stage:"runtime_data/state_sync_selection"
       ~check:(fun certificate _ ->
         checks := certificate.Manifest.checkpoint_hash :: !checks;
         Lwt.return_unit)
       ~sync:(fun certificate _ _ ->
         selected_hashes := certificate.Manifest.checkpoint_hash :: !selected_hashes;
         Lwt.return_unit)
       [oversized, [first]; older, [second]]);
  if List.rev !checks <> [newer.checkpoint_hash] then
    fail "oversized manifest chunk was not checked";
  if List.rev !selected_hashes <> [older.checkpoint_hash] then
    fail "oversized manifest blocked an older checkpoint";
  let oversized_result =
    try
      Lwt_main.run
        (Client.sync_manifests
           ~max_bytes:older.manifest.total_size
           ~stage:"runtime_data/state_sync_selection"
           ~check:(fun _ _ -> Lwt.return_unit)
           ~sync:(fun _ _ _ -> Lwt.return_unit)
           [oversized, [first]]);
      Ok ()
    with exn -> Error exn
  in
  begin
    match oversized_result with
    | Error (Client.Sync_error "snapshot exceeds configured byte limit") -> ()
    | Error exn -> fail ("oversized manifest returned " ^ Printexc.to_string exn)
    | Ok () -> fail "oversized manifest was accepted"
  end

let test_transport_guard () =
  let https =
    expect_ok (Source.create ~allow_private_http:false "https://state.example")
  in
  let loopback =
    expect_ok (Source.create ~allow_private_http:false "http://127.0.0.1:44001")
  in
  begin
    try
      Client.require_transport ~tls_available:false [https];
      fail "HTTPS accepted without TLS"
    with Client.Sync_error _ -> ()
  end;
  Client.require_transport ~tls_available:false [loopback];
  Client.require_transport ~tls_available:true [https]

let prepare_partial stage file payload =
  let destination, partial, _ =
    expect_ok (Journal.prepare_file ~stage file)
  in
  write_file partial payload;
  destination, partial

let test_finalize_batch root =
  let data_dir = Filename.concat root "batch" in
  let files =
    List.init 24 (fun index ->
      let payload = Printf.sprintf "receipt-%d" index in
      manifest_file
        (Printf.sprintf "preverify_receipts/epoch_%d.json" index)
        payload
        Manifest.chunk_size_min,
      payload)
  in
  let head = manifest_file "HEAD.json" "head" Manifest.chunk_size_min in
  let prepared =
    files
    |> List.map (fun (file, payload) ->
      let destination, partial = prepare_partial data_dir file payload in
      file, destination, partial)
  in
  let head_destination, head_partial = prepare_partial data_dir head "head" in
  Client.finalize_files
    ~parallelism:8
    ~data_dir
    (head :: List.map (fun (file, _, _) -> file) prepared) >>= fun () ->
  List.iter (fun (_, destination, partial) ->
    if not (Sys.file_exists destination) || Sys.file_exists partial then
      fail "parallel finalization left an invalid file state"
  ) prepared;
  if not (Sys.file_exists head_destination) || Sys.file_exists head_partial then
    fail "head finalization left an invalid file state";
  Lwt.return_unit

let test_finalize_head_last root =
  let data_dir = Filename.concat root "head-last" in
  let broken =
    manifest_file "preverify_receipts/broken.json" "expected" Manifest.chunk_size_min
  in
  let head = manifest_file "HEAD.json" "head" Manifest.chunk_size_min in
  let _, _ = prepare_partial data_dir broken "rejected" in
  let head_destination, head_partial = prepare_partial data_dir head "head" in
  Lwt.try_bind
    (fun () ->
      Client.finalize_files ~parallelism:8 ~data_dir [head; broken] >>= fun () ->
      Lwt.return_false)
    Lwt.return
    (fun _ -> Lwt.return_true) >>= fun rejected ->
  if not rejected then fail "invalid file passed finalization";
  if Sys.file_exists head_destination || not (Sys.file_exists head_partial) then
    fail "head finalized before regular files";
  Lwt.return_unit

let test_reference_head_resume root certificate head_json =
  let data_dir = Filename.concat root "data" in
  let file =
    List.find
      (fun file -> file.Manifest.path = "HEAD.json")
      certificate.Manifest.manifest.files
  in
  let target = Filename.concat data_dir "HEAD.json" in
  write_file target head_json;
  ignore (expect_ok (Client.retain_head ~root ~data_dir file));
  let head = Head.of_json head_json in
  Head.atomic_write
    data_dir
    { head with Head.irmin_commit = Some (String.make 128 'c') };
  ignore (expect_ok (Client.restore_head ~root ~data_dir file));
  begin
    match Journal.hash_file target with
    | Ok hash when hash = file.sha256 -> ()
    | _ -> fail "reference HEAD was not restored"
  end;
  write_file (Client.retained_head root) "corrupt";
  expect_error (Client.restore_head ~root ~data_dir file)

let salt_for_first left right chunk =
  let rec loop index =
    let salt = sha (string_of_int index) in
    match Source.rank
      ~salt
      ~chunk_key:(Journal.chunk_key "chaindata/blob" chunk)
      ~now:0.0
      ~attempted:[]
      [left; right] with
    | first :: _ when first.Source.url = left.url -> salt
    | _ -> loop (index + 1)
  in
  loop 0

let test_cert_model trust certificate =
  let module Cache = Octra_node_runtime.Sync_cert in
  let query = Cache.{ path = "first"; trust; trust_hash = trust_hash trust } in
  let bytes = Cache.{ raw = "bytes"; digest = sha "bytes" } in
  let state, effects = Cache.delta Cache.empty (Ask (1L, query, 0.)) in
  begin match effects with
  | [Cache.Read_file job] when job.id = 1L && job.generation = 0L -> ()
  | _ -> fail "certificate read admission differs"
  end;
  let same, duplicate = Cache.delta state (Ask (1L, query, 0.)) in
  if Cache.stats same <> Cache.stats state || duplicate <> [] then
    fail "duplicate certificate request changed state";
  let full = List.fold_left (fun state id ->
    let next, effects = Cache.delta state (Ask (Int64.of_int id, query, 0.)) in
    if effects <> [] then fail "queued certificate started early";
    next) state (List.init (Cache.capacity - 1) (fun index -> index + 2)) in
  let full, effects = Cache.delta full (Ask (100L, query, 0.)) in
  begin match effects with
  | [Cache.Reply (100L, Error Cache.Busy)] -> ()
  | _ -> fail "certificate queue exceeded capacity"
  end;
  if (Cache.stats full).active + (Cache.stats full).queued <> Cache.capacity then
    fail "certificate queue size differs";
  let expired, _ = Cache.delta full (Tick (Cache.lifetime +. 1.)) in
  let expired, effects = Cache.delta expired (Read (1L, 0L, Cache.lifetime +. 1., Ok bytes)) in
  if effects <> [] || (Cache.stats expired).active <> 0 || (Cache.stats expired).queued <> 0 then
    fail "expired certificate work was reused";
  let stopped, effects = Cache.delta full Stop in
  let replies = List.filter (function Cache.Reply _ -> true | _ -> false) effects in
  if List.length replies <> Cache.capacity then fail "stop lost certificate requests";
  let stopped, effects = Cache.delta stopped (Read (1L, 0L, 1., Ok bytes)) in
  if effects <> [] || (Cache.stats stopped).cached <> 0 then fail "old read entered stopped actor";
  let checked, effects = Cache.delta state (Read (1L, 0L, 1., Ok bytes)) in
  begin match effects with
  | [Cache.Verify (job, "bytes")] when job.id = 1L -> ()
  | _ -> fail "certificate verification was skipped"
  end;
  let stopped, _ = Cache.delta checked Stop in
  let stopped, effects = Cache.delta stopped (Checked (1L, 0L, 2., Ok certificate)) in
  if effects <> [] || (Cache.stats stopped).cached <> 0 then fail "old verification entered stopped actor";
  let checked, _ = Cache.delta checked (Cancel 1L) in
  let checked, effects = Cache.delta checked (Checked (1L, 0L, 2., Ok certificate)) in
  if effects <> [] || (Cache.stats checked).cached <> 0 then fail "cancelled verification was published";
  let checking, _ = Cache.delta state (Read (1L, 0L, 1., Ok bytes)) in
  let checked, effects = Cache.delta checking (Checked (1L, 0L, Cache.lifetime +. 1., Ok certificate)) in
  begin match effects with
  | [Cache.Reply (1L, Error Cache.Expired)] -> ()
  | _ -> fail "late verification did not expire its waiter"
  end;
  if (Cache.stats checked).cached <> 1 then fail "late valid verification was discarded";
  let checked, _ = Cache.delta checked (Ask (2L, query, Cache.lifetime +. 2.)) in
  let _, effects = Cache.delta checked (Read (2L, 0L, Cache.lifetime +. 3., Ok bytes)) in
  begin match effects with
  | [Cache.Parse (_, "bytes")] -> ()
  | _ -> fail "late valid verification was repeated"
  end

let test_cert_actor trust certificate =
  let module Cache = Octra_node_runtime.Sync_cert in
  let raw = Yojson.Safe.to_string (Manifest.certificate_json certificate) in
  let reads = ref 0 in
  let verifies = ref 0 in
  let body = ref raw in
  let now = ref 1. in
  let deps = Cache.{
    now = (fun () -> !now);
    read = (fun _ -> incr reads; Lwt.return_ok !body);
    verify = (fun ~cancelled trust raw ->
      incr verifies;
      match Manifest.parse_certificate_string raw with
      | Error reason -> Lwt.return_error reason
      | Ok certificate -> Manifest.verify_certificate_lwt
          ~cancelled ~validator_set:trust.validators ~exporter_set:trust.exporters certificate);
  } in
  let actor = Cache.create deps in
  Lwt.finalize (fun () ->
    Lwt_list.iter_s (fun path ->
      Cache.load actor ~path trust >|= fun result ->
      let loaded = match result with Ok loaded -> loaded | Error error -> fail (Cache.reason error) in
      if loaded.raw <> raw then fail "certificate bytes changed")
      ["current"; "archive"; "current"; "archive"] >>= fun () ->
    if !reads <> 4 || !verifies <> 1 then fail "same certificate was verified repeatedly";
    Lwt_list.iter_p (fun path -> Cache.load actor ~path trust >|= fun result ->
      if Result.is_error result then fail "concurrent certificate read failed")
      ["current"; "archive"; "current"; "archive"] >>= fun () ->
    if !reads <> 8 || !verifies <> 1 then fail "concurrent certificate checks were repeated";
    let changed = Cache.{ trust with config = sha "changed" } in
    Cache.load actor ~path:"current" changed >>= fun result ->
    if Result.is_error result then fail "changed trust read failed";
    if !verifies <> 2 then fail "trust change reused certificate verification";
    body := "{\"invalid\":true}";
    Cache.load actor ~path:"current" trust >>= fun result ->
    begin match result with
    | Error _ -> ()
    | Ok _ -> fail "changed certificate reused old success"
    end;
    let calls = !verifies in
    Cache.load actor ~path:"archive" trust >>= fun result ->
    begin match result with
    | Error _ when !verifies = calls -> ()
    | _ -> fail "invalid certificate was not cached"
    end;
    now := !now +. Cache.negative_seconds +. 1.;
    Cache.load actor ~path:"archive" trust >>= fun result ->
    begin match result with
    | Error _ when !verifies = calls + 1 -> ()
    | _ -> fail "expired failure was reused"
    end;
    body := raw;
    Cache.load actor ~path:"current" trust >>= fun result ->
    if Result.is_error result then fail "restored certificate read failed";
    let count = 10 in
    let bodies = List.init count (fun index -> raw ^ String.make (index + 1) ' ') in
    let before = !verifies in
    Lwt_list.iter_s (fun raw ->
      body := raw;
      Cache.load actor ~path:"retained" trust >|= fun result ->
      if Result.is_error result then fail "retained certificate read failed") (bodies @ bodies) >|= fun () ->
    if !verifies <> before + count then fail "retained certificates evicted one another")
    (fun () -> Cache.shutdown actor)

let test_cert_cache trust certificate =
  let module Cache = Octra_node_runtime.Sync_cert in
  let query = Cache.{ path = "current"; trust; trust_hash = trust_hash trust } in
  let save state index =
    let id = Int64.of_int index in
    let raw = string_of_int index in
    let state, _ = Cache.delta state (Ask (id, query, 0.)) in
    let state, _ = Cache.delta state (Read (id, 0L, 1., Ok Cache.{ raw; digest = sha raw })) in
    let state, _ = Cache.delta state (Checked (id, 0L, 2., Ok certificate)) in
    if (Cache.stats state).cached > Cache.cache_capacity then fail "certificate cache exceeds capacity";
    state
  in
  let state = List.fold_left save Cache.empty (List.init (Cache.cache_capacity + 2) (fun i -> i + 1)) in
  if (Cache.stats state).cached <> Cache.cache_capacity then fail "certificate cache size differs";
  let state, _ = Cache.delta state (Ask (100L, query, 3.)) in
  let _, effects = Cache.delta state (Read (100L, 0L, 4., Ok Cache.{ raw = "1"; digest = sha "1" })) in
  begin match effects with
  | [Cache.Verify (_, "1")] -> ()
  | _ -> fail "evicted certificate was reused"
  end

let test_http_cert root wallets certificate validators =
  let path = Filename.concat root "http-certificate.json" in
  let keys = ["OCTRA_STATE_SYNC_ENABLE"; "OCTRA_STATE_SYNC_CERT";
    "OCTRA_VALIDATORS"; "OCTRA_STATE_SYNC_EXPORTERS"; "OCTRA_CONSENSUS_CONFIG_HASH"] in
  let saved = List.map (fun key -> key, Option.value ~default:"" (Sys.getenv_opt key)) keys in
  let config_hash = certificate.Manifest.checkpoint.config_hash in
  let chain_id = certificate.checkpoint.chain_id in
  let member wallet = wallet.Octra_core.Crypto.Wallet.address ^ ":" ^ wallet.pub in
  let configured = String.concat "," (List.map member wallets) in
  let exporter = member (List.hd wallets) in
  let write certificate = Manifest.write_json path (Manifest.certificate_json certificate) in
  let snapshot = Int64.to_int certificate.checkpoint.epoch in
  let read ?(chain = chain_id) ?(epoch = snapshot + 1_061) status =
    Http.handle_manifest ~data_dir:root ~chain_id:chain ~config_hash
      ~validator_set:validators ~current_epoch:(ref epoch) >>= fun (response, body) ->
    Cohttp_lwt.Body.to_string body >|= fun raw ->
    if Cohttp.Response.status response <> status then
      fail (Printf.sprintf "certificate HTTP status differs lag = %d expected = %d actual = %d"
        (epoch - snapshot - 1) (Cohttp.Code.code_of_status status)
        (Cohttp.Code.code_of_status (Cohttp.Response.status response)));
    if status = `OK && Manifest.parse_certificate_string raw <> Ok certificate then
      fail "certificate HTTP bytes differ"
  in
  let read_lag lag =
    let status, expected = if lag <= 3_000 then `OK, "ready"
      else `Service_unavailable, "old_epoch" in
    let epoch = snapshot + lag + 1 in
    read ~epoch status >>= fun () ->
    Http.handle_head ~data_dir:root ~chain_id ~config_hash
      ~validator_set:validators ~current_epoch:(ref epoch) >>= fun (response, body) ->
    Cohttp_lwt.Body.to_string body >|= fun raw ->
    if Cohttp.Response.status response <> `OK then fail "snapshot head is unavailable";
    let json = Yojson.Safe.from_string raw in
    let field name = Yojson.Safe.Util.member name json in
    if field "snapshot_status" <> `String expected
       || field "snapshot_lag" <> `Int lag
       || field "snapshot_lag_limit" <> `Int 3_000 then
      fail "snapshot head and manifest decisions differ"
  in
  Unix.putenv "OCTRA_STATE_SYNC_ENABLE" "1";
  Unix.putenv "OCTRA_STATE_SYNC_CERT" path;
  Unix.putenv "OCTRA_VALIDATORS" configured;
  Unix.putenv "OCTRA_STATE_SYNC_EXPORTERS" exporter;
  Unix.putenv "OCTRA_CONSENSUS_CONFIG_HASH" config_hash;
  mkdir_p root;
  write certificate;
  Lwt.finalize (fun () ->
    let active = List.init Octra_node_runtime.Sync_cert.capacity (fun _ ->
      Http.handle_manifest ~data_dir:root ~chain_id ~config_hash
        ~validator_set:validators ~current_epoch:(ref 778)) in
    let file = List.hd certificate.manifest.files in
    let chunk = List.hd file.chunks in
    let snapshot_id = certificate.manifest.snapshot_id in
    mkdir_p (State_sync.snapshot_dir root snapshot_id);
    Http.handle_chunk ~data_dir:root ~chain_id ~config_hash ~validator_set:validators
      ["snapshot", [snapshot_id]; "path", [file.path]; "index", [string_of_int chunk.index];
       "sha256", [chunk.sha256]] >>= fun (response, body) ->
    Cohttp_lwt.Body.drain_body body >>= fun () ->
    if Cohttp.Response.status response <> `Too_many_requests
       || Cohttp.Header.get (Cohttp.Response.headers response) "Retry-After" <> Some "1" then
      fail "certificate overload did not apply HTTP backpressure";
    Http.handle_head ~data_dir:root ~chain_id ~config_hash ~validator_set:validators
      ~current_epoch:(ref 778) >>= fun (response, body) ->
    Cohttp_lwt.Body.drain_body body >>= fun () ->
    if Cohttp.Response.status response <> `Too_many_requests then
      fail "certificate overload reported a missing snapshot";
    Lwt_list.iter_s (fun work -> work >>= fun (_, body) -> Cohttp_lwt.Body.drain_body body) active >>= fun () ->
    read `OK >>= fun () ->
    read ~chain:"other-chain" `Service_unavailable >>= fun () ->
    read `OK >>= fun () ->
    Unix.putenv "OCTRA_CONSENSUS_CONFIG_HASH" (sha "other-config");
    read `Service_unavailable >>= fun () ->
    Unix.putenv "OCTRA_CONSENSUS_CONFIG_HASH" config_hash;
    read `OK >>= fun () ->
    Unix.putenv "OCTRA_STATE_SYNC_EXPORTERS" (member (wallet 21));
    read `Service_unavailable >>= fun () ->
    Unix.putenv "OCTRA_STATE_SYNC_EXPORTERS" exporter;
    Unix.putenv "OCTRA_VALIDATORS" (member (wallet 22));
    read `Service_unavailable >>= fun () ->
    Unix.putenv "OCTRA_VALIDATORS" configured;
    read `OK >>= fun () ->
    Lwt_list.iter_s read_lag [0; 720; 721; 1_060; 3_000; 3_001; 4_096; 5_000] >>= fun () ->
    write { certificate with exporter_signatures = List.map (fun item ->
      Checkpoint.{ item with signature = Base64.encode_exn (String.make 64 '\000') })
      certificate.exporter_signatures };
    read `Service_unavailable >>= fun () ->
    write { certificate with authority = Manifest.Checkpoint_quorum [] };
    read `Service_unavailable >>= fun () ->
    let _, _, _, _, expired = sample ~valid_for:(-1L) wallets "payload" "expired" in
    write expired;
    read `Service_unavailable >>= fun () ->
    write certificate;
    read `OK >>= fun () ->
    Unix.unlink path;
    Unix.mkfifo path 0o600;
    Lwt_unix.with_timeout 1. (fun () -> read `Service_unavailable) >>= fun () ->
    Unix.unlink path;
    write certificate;
    let output = Unix.openfile path [Unix.O_WRONLY] 0o600 in
    Unix.ftruncate output (Manifest.manifest_limit + 1);
    Unix.close output;
    read `Service_unavailable >>= fun () ->
    write certificate;
    read `OK)
    (fun () -> List.iter (fun (key, value) -> Unix.putenv key value) saved; Lwt.return_unit)

let test_cert_cancel trust certificate =
  let module Cache = Octra_node_runtime.Sync_cert in
  let pending, finish = Lwt.wait () in
  let reads = ref 0 in
  let verifies = ref 0 in
  let actor = Cache.create {
    now = (fun () -> 0.);
    read = (fun _ -> incr reads; pending);
    verify = (fun ~cancelled:_ _ _ -> incr verifies; Lwt.return_ok certificate);
  } in
  let first = Cache.load actor ~path:"first" trust in
  Lwt.cancel first;
  let second = Cache.load actor ~path:"second" trust in
  if !reads <> 1 then fail "cancel released a running certificate read";
  let stopped = Cache.shutdown actor in
  if not (Lwt.is_sleeping stopped) then fail "shutdown did not wait for the running read";
  second >>= fun result ->
  begin match result with
  | Error Cache.Stopped -> ()
  | _ -> fail "queued request survived stop"
  end;
  Lwt.wakeup_later finish (Ok "bytes");
  stopped >>= fun () ->
  if !reads <> 1 || !verifies <> 0 then fail "stopped actor launched more work";
  Cache.load actor ~path:"after" trust >|= function
  | Error Cache.Stopped -> ()
  | _ -> fail "stopped actor accepted work"

let test_verify_cancel trust certificate =
  let module Cache = Octra_node_runtime.Sync_cert in
  let entered, enter = Lwt.wait () in
  let pending, finish = Lwt.wait () in
  let later = ref 0 in
  let actor = Cache.create {
    now = (fun () -> 0.);
    read = (fun _ -> Lwt.return_ok "bytes");
    verify = (fun ~cancelled _ _ ->
      Lwt.wakeup_later enter ();
      pending >|= fun () ->
      if cancelled () then Error "cancelled"
      else begin incr later; Ok certificate end);
  } in
  let request = Cache.load actor ~path:"first" trust in
  entered >>= fun () ->
  Lwt.cancel request;
  let stopped = Cache.shutdown actor in
  if not (Lwt.is_sleeping stopped) then fail "shutdown left verification running";
  Lwt.wakeup_later finish ();
  stopped >|= fun () ->
  if !later <> 0 then fail "cancelled verification launched another stage"

let run () =
  let root =
    Filename.concat
      "runtime_data"
      (Printf.sprintf "test_state_sync_client_%d" (Unix.getpid ()))
  in
  remove_tree root;
  let good_data = Filename.concat root "good" in
  let bad_data = Filename.concat root "bad" in
  let output_root = Filename.concat root "output" in
  let payload =
    String.init ((2 * Manifest.chunk_size_min) + 91) (fun index ->
      Char.chr (index land 255))
  in
  let wallets = List.init 5 (fun index -> wallet (index + 1)) in
  let validators, exporters, config_hash, head_json, certificate =
    sample wallets payload "state"
  in
  let trust = Octra_node_runtime.Sync_cert.{
    validators; exporters; config = config_hash; chain = certificate.checkpoint.chain_id;
  } in
  test_cert_model trust certificate;
  test_cert_cache trust certificate;
  test_cert_actor trust certificate >>= fun () ->
  test_cert_cancel trust certificate >>= fun () ->
  test_verify_cancel trust certificate >>= fun () ->
  test_http_cert root wallets certificate validators >>= fun () ->
  let runtime_validators = runtime_set wallets in
  let snapshot_id = certificate.Manifest.manifest.snapshot_id in
  let good_snapshot = State_sync.snapshot_dir good_data snapshot_id in
  let bad_snapshot = State_sync.snapshot_dir bad_data snapshot_id in
  write_file (Filename.concat good_snapshot "HEAD.json") head_json;
  write_file (Filename.concat bad_snapshot "HEAD.json") head_json;
  write_file (Filename.concat good_snapshot "chaindata/blob") payload;
  write_file
    (Filename.concat bad_snapshot "chaindata/blob")
    (String.make (String.length payload) 'x');
  write_file (State_sync.snapshot_ready_path good_snapshot) "{}";
  write_file (State_sync.snapshot_ready_path bad_snapshot) "{}";
  let certificate_path = Filename.concat root "certificate.json" in
  Manifest.write_json certificate_path (Manifest.certificate_json certificate);
  let exporter = List.hd wallets in
  Unix.putenv "OCTRA_STATE_SYNC_ENABLE" "1";
  Unix.putenv "OCTRA_STATE_SYNC_CERT" certificate_path;
  Unix.putenv
    "OCTRA_STATE_SYNC_EXPORTERS"
    (exporter.Octra_core.Crypto.Wallet.address ^ ":" ^ exporter.pub);
  Unix.putenv
    "OCTRA_VALIDATORS"
    (wallets
     |> List.map (fun wallet ->
       wallet.Octra_core.Crypto.Wallet.address ^ ":" ^ wallet.pub)
     |> String.concat ",");
  Unix.putenv "OCTRA_CONSENSUS_CONFIG_HASH" config_hash;
  listen () >>= fun (left_socket, left_port) ->
  listen () >>= fun (right_socket, right_port) ->
  listen () >>= fun (expired_socket, expired_port) ->
  let left =
    expect_ok (Source.create ~allow_private_http:false (source_url left_port))
  in
  let right =
    expect_ok (Source.create ~allow_private_http:false (source_url right_port))
  in
  let expired =
    expect_ok (Source.create ~allow_private_http:false (source_url expired_port))
  in
  let file =
    List.find (fun file -> file.Manifest.path = "chaindata/blob")
      certificate.manifest.files
  in
  let first_chunk = List.hd file.chunks in
  let second_chunk = List.nth file.chunks 1 in
  let salt = salt_for_first left right second_chunk in
  mkdir_p output_root;
  Manifest.write_json (Filename.concat output_root "source_salt.json") (`String salt);
  let output_data = Filename.concat output_root "data" in
  let _, partial, _ =
    expect_ok (Journal.prepare_file ~stage:output_data file)
  in
  let first_payload =
    String.sub payload (Int64.to_int first_chunk.offset) first_chunk.size
  in
  ignore (expect_ok (Journal.write_chunk ~partial first_chunk first_payload));
  let journal =
    expect_ok (Journal.open_journal
      ~path:(Filename.concat output_root "journal.jsonl")
      ~manifest_hash:certificate.manifest_hash)
  in
  Journal.record_completed journal file.path first_chunk;
  let output = open_out_gen [Open_wronly; Open_append; Open_binary] 0o600 journal.path in
  Fun.protect ~finally:(fun () -> close_out_noerr output)
    (fun () -> output_string output "{\"version\":");
  Client.chain_id := certificate.checkpoint.chain_id;
  Client.config_hash := config_hash;
  Client.timeout_seconds := 5.0;
  Client.concurrency := 3;
  Client.source_concurrency := 2;
  Client.retries := 6;
  let requests = ref [] in
  let bad_server =
    server
      ~socket:left_socket
      ~data_dir:bad_data
      ~chain_id:certificate.checkpoint.chain_id
      ~config_hash
      ~validator_set:runtime_validators
      ~requests
      ()
  in
  let good_server =
    server
      ~socket:right_socket
      ~data_dir:good_data
      ~chain_id:certificate.checkpoint.chain_id
      ~config_hash
      ~validator_set:runtime_validators
      ~requests
      ()
  in
  let expired_requests = ref 0 in
  let expired_server =
    expired_server ~socket:expired_socket expired_requests
  in
  test_finalize_batch (Filename.concat root "finalize") >>= fun () ->
  test_finalize_head_last (Filename.concat root "finalize") >>= fun () ->
  test_reference_head_resume
    (Filename.concat root "reference-resume")
    certificate
    head_json;
  Lwt.async (fun () -> bad_server);
  Lwt.async (fun () -> good_server);
  Lwt.async (fun () -> expired_server);
  Lwt_unix.sleep 0.1 >>= fun () ->
  Client.concurrency := 1;
  Client.source_concurrency := 1;
  Lwt.try_bind
    (fun () ->
      Client.run_sync
        ~verify_state:(fun _ _ -> Lwt.return_ok ())
        certificate
        [expired]
        (Filename.concat root "expired"))
    (fun () -> Lwt.return_false)
    (function
      | Client.Snapshot_expired "snapshot retired" -> Lwt.return_true
      | exn -> Lwt.fail exn) >>= fun expired_rejected ->
  if not expired_rejected then fail "expired snapshot was accepted";
  if !expired_requests <> 1 then fail "expired snapshot exhausted retry budget";
  test_cancel (Filename.concat root "cancel")
    (fun source root -> Client.run_sync certificate [source] root) >>= fun () ->
  test_cancel (Filename.concat root "manifest-cancel")
    (fun source _ -> Client.fetch_manifest validators exporters source >|= fun _ -> ()) >>= fun () ->
  test_cancel (Filename.concat root "probe-cancel")
    (fun source _ -> Client.check_manifest_chunk certificate [source; source]) >>= fun () ->
  test_choice_cancel (Filename.concat root "choice-cancel") certificate >>= fun () ->
  test_chunk_resume (Filename.concat root "chunk-resume") certificate good_data
    runtime_validators ~delay:0.0 >>= fun () ->
  Client.timeout_seconds := 1.0;
  test_chunk_resume (Filename.concat root "chunk-timeout") certificate good_data
    runtime_validators ~delay:1.5 >>= fun () ->
  Client.timeout_seconds := 5.0;
  test_rotation (Filename.concat root "rotation") wallets payload certificate good_data
    runtime_validators >>= fun () ->
  Client.retries := 6;
  Client.concurrency := 3;
  Client.source_concurrency := 2;
  Client.check_manifest_chunk certificate [right] >>= fun () ->
  Client.run_sync
    ~verify_state:(fun checkpoint data_dir ->
      match Octra_core.Head_manifest.load_result data_dir with
      | Octra_core.Head_manifest.Present head
        when Checkpoint.matches_head checkpoint head ->
          Lwt.return_ok ()
      | _ -> Lwt.return_error "restored HEAD.json does not match checkpoint")
    certificate
    [left; right]
    output_root >>= fun () ->
  let journal = expect_ok (Journal.open_journal
    ~path:journal.path ~manifest_hash:certificate.manifest_hash) in
  if not (List.for_all (Journal.is_completed journal file.path) file.chunks) then
    fail "client journal lost completed chunks after repair";
  if List.exists (fun uri ->
    Uri.get_query_param uri "path" = Some file.path
    && Uri.get_query_param uri "index" = Some "0"
  ) !requests then fail "client fetched a verified saved chunk";
  let destination = Filename.concat output_root "data/chaindata/blob" in
  begin
    match Journal.hash_file destination with
    | Ok hash when hash = file.sha256 -> ()
    | _ -> fail "restored file hash mismatch"
  end;
  let manifest_source =
    expect_ok (Source.create ~allow_private_http:false (source_url left_port))
  in
  Lwt.try_bind
    (fun () -> Client.fetch_manifest validators exporters manifest_source)
    (fun _ -> Lwt.return_false)
    (function
      | Failure reason
        when String.equal reason "reference checkpoint finality is required" ->
          Lwt.return_true
      | exn -> Lwt.fail exn) >>= fun legacy_rejected ->
  if not legacy_rejected then fail "legacy checkpoint authority was accepted";
  Lwt.cancel bad_server;
  Lwt.cancel good_server;
  Lwt.cancel expired_server;
  remove_tree root;
  Lwt.return_unit

let () =
  if not Conduit_lwt_tls.available then fail "native TLS backend unavailable";
  test_conflicting_quorum ();
  test_equivalent_quorum_proofs ();
  test_distinct_byte_manifests ();
  test_distinct_checkpoints ();
  test_transport_guard ();
  Lwt_main.run (run ());
  print_endline "status = pass test = state_sync_client"