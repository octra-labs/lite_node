(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Infix

module Manifest = State_sync_manifest
module Checkpoint = State_sync_checkpoint
module Journal = State_sync_journal
module Source = State_sync_source
module State_sync = State_sync
module Verify = State_sync_verify
module Store = Octra_core.Store_chaindata
module Ledger_store = Octra_core.Store_irmin
module Head = Octra_core.Head_manifest
module Image = Octra_core.Ledger_image
module String_map = Map.Make (String)
module Epoch_map = Map.Make (struct
  type t = int64
  let compare = Int64.compare
end)

type task = {
  file : Manifest.file;
  partial : string;
  chunk : Manifest.chunk;
}

let source_values = ref []
let validator_entries = ref []
let exporter_entries = ref []
let stage_dir = ref ""
let chain_id = ref ""
let config_hash = ref ""
let concurrency = ref 8
let source_concurrency = ref 2
let retries = ref 8
let timeout_seconds = ref 30.0
let min_epoch = ref 0L
let max_bytes = ref 2_000_000_000_000L
let allow_private_http = ref false

let usage =
  "octra_state_sync_client --source URL --validator ADDRESS:PUBKEY --exporter ADDRESS:PUBKEY --stage DIR --chain-id ID --config-hash HASH"

let options = [
  "--source", Arg.String (fun value -> source_values := value :: !source_values),
    "state sync source";
  "--validator", Arg.String (fun value -> validator_entries := value :: !validator_entries),
    "trusted address:pubkey";
  "--exporter", Arg.String (fun value -> exporter_entries := value :: !exporter_entries),
    "trusted exporter address:pubkey";
  "--stage", Arg.Set_string stage_dir, "staging root";
  "--chain-id", Arg.Set_string chain_id, "expected chain identifier";
  "--config-hash", Arg.Set_string config_hash, "expected consensus config hash";
  "--concurrency", Arg.Set_int concurrency, "global concurrent chunks";
  "--source-concurrency", Arg.Set_int source_concurrency, "concurrent chunks per source";
  "--retries", Arg.Set_int retries, "chunk attempts";
  "--timeout", Arg.Set_float timeout_seconds, "request timeout seconds";
  "--min-epoch", Arg.String (fun value -> min_epoch := Int64.of_string value),
    "minimum snapshot epoch";
  "--max-bytes", Arg.String (fun value -> max_bytes := Int64.of_string value),
    "maximum accepted snapshot bytes";
  "--allow-private-http", Arg.Set allow_private_http, "allow explicit private HTTP transport";
]

exception Sync_error of string

let fail message =
  raise (Sync_error message)

let require name value =
  if String.trim value = "" then fail (name ^ " is required") else String.trim value

let require_transport ~tls_available sources =
  let requires_tls =
    List.exists (fun source ->
      Uri.scheme (Uri.of_string source.Source.url) = Some "https"
    ) sources
  in
  if requires_tls && not tls_available then
    fail "HTTPS state sync requires the native TLS backend"

let mkdir_p path =
  let rec loop current =
    if current = "" || current = "." || Sys.file_exists current then ()
    else begin
      loop (Filename.dirname current);
      Unix.mkdir current 0o755
    end
  in
  loop path

let read_body_limited body limit =
  let stream = Cohttp_lwt.Body.to_stream body in
  let buffer = Buffer.create (min limit 65_536) in
  let rec loop total =
    Lwt_stream.get stream >>= function
    | None -> Lwt.return (Buffer.contents buffer)
    | Some chunk ->
        let size = String.length chunk in
        if size > limit - total then Lwt.fail_with "response body exceeds size limit"
        else begin
          Buffer.add_string buffer chunk;
          loop (total + size)
        end
  in
  loop 0

let get_limited ~timeout ~limit url =
  Lwt_unix.with_timeout timeout (fun () ->
    Cohttp_lwt_unix.Client.get (Uri.of_string url) >>= fun (response, body) ->
    let code = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
    let body_limit = if code >= 200 && code < 300 then limit else 65_536 in
    read_body_limited body body_limit >>= fun payload ->
    if code < 200 || code >= 300 then
      Lwt.fail_with (Printf.sprintf "HTTP %d" code)
    else
      Lwt.return (response, payload))

let fetch_manifest validator_set exporter_set source =
  let url = source.Source.url ^ "/state-sync/manifest" in
  let rec loop attempt =
    let started = Unix.gettimeofday () in
    Lwt.catch
      (fun () ->
        get_limited
          ~timeout:!timeout_seconds
          ~limit:Manifest.manifest_limit
          url >>= fun (response, raw) ->
        let header =
          Cohttp.Response.headers response
          |> fun headers -> Cohttp.Header.get headers "x-octra-manifest-sha256"
        in
        begin
          match Manifest.parse_certificate_string raw with
          | Error reason -> Lwt.fail_with reason
          | Ok certificate ->
              begin
                match Manifest.verify_certificate
                  ~validator_set
                  ~exporter_set
                  certificate with
                | Error reason -> Lwt.fail_with reason
                | Ok certificate ->
                    let checkpoint = certificate.checkpoint in
                    if checkpoint.chain_id <> !chain_id then
                      Lwt.fail_with "manifest chain id mismatch"
                    else if checkpoint.config_hash <> !config_hash then
                      Lwt.fail_with "manifest config hash mismatch"
                    else if Int64.compare checkpoint.epoch !min_epoch < 0 then
                      Lwt.fail_with "manifest snapshot epoch below minimum"
                    else if not (Manifest.fresh
                      ~now:(Int64.of_float (Unix.gettimeofday ()))
                      certificate) then
                      Lwt.fail_with "manifest certificate is outside its validity window"
                    else if header <> Some certificate.manifest_hash then
                      Lwt.fail_with "manifest response hash header mismatch"
                    else
                      let elapsed = (Unix.gettimeofday () -. started) *. 1000.0 in
                      Source.record_success source elapsed;
                      Lwt.return (source, certificate)
              end
        end)
      (fun exn ->
        Source.record_failure source (Unix.gettimeofday ());
        if attempt >= 2 then Lwt.fail exn
        else
          Lwt_unix.sleep (float_of_int (attempt + 1)) >>= fun () ->
          loop (attempt + 1))
  in
  loop 0

let select_manifests (results : (Source.t * Manifest.certificate) list) =
  let ordered =
    List.sort (fun (_, left) (_, right) ->
      let epoch_order =
        Int64.compare right.Manifest.checkpoint.epoch left.checkpoint.epoch in
      if epoch_order <> 0 then epoch_order
      else
        let checkpoint_order =
          String.compare left.checkpoint_hash right.checkpoint_hash in
        if checkpoint_order <> 0 then checkpoint_order
        else String.compare left.manifest_hash right.manifest_hash
    ) results
  in
  let _, conflicting =
    List.fold_left
      (fun (epochs, conflicting) (_, (certificate : Manifest.certificate)) ->
        let epoch = certificate.checkpoint.epoch in
        match Epoch_map.find_opt epoch epochs with
        | None ->
            Epoch_map.add epoch certificate.checkpoint_hash epochs, conflicting
        | Some checkpoint_hash ->
            epochs, conflicting || checkpoint_hash <> certificate.checkpoint_hash)
      (Epoch_map.empty, false)
      ordered
  in
  if conflicting then
    fail "conflicting quorum checkpoints at the same snapshot epoch";
  let manifest_groups candidates =
    candidates
    |> List.fold_left
         (fun groups (source, (certificate : Manifest.certificate)) ->
           String_map.update
             certificate.manifest_hash
             (function
               | None -> Some (certificate, [source])
               | Some (selected, sources) -> Some (selected, source :: sources))
             groups)
         String_map.empty
    |> String_map.bindings
    |> List.map (fun (manifest_hash, (certificate, sources)) ->
         manifest_hash, certificate, List.rev sources)
    |> List.sort (fun (left_hash, _, left_sources) (right_hash, _, right_sources) ->
         let count_order = compare (List.length right_sources) (List.length left_sources) in
         if count_order <> 0 then count_order else String.compare left_hash right_hash)
    |> List.map (fun (_, certificate, sources) -> certificate, sources)
  in
  let rec take_checkpoint checkpoint_hash selected = function
    | ((_, (certificate : Manifest.certificate)) as item) :: rest
      when certificate.checkpoint_hash = checkpoint_hash ->
        take_checkpoint checkpoint_hash (item :: selected) rest
    | rest -> List.rev selected, rest
  in
  let rec collect = function
    | [] -> []
    | (_, (certificate : Manifest.certificate)) :: _ as remaining ->
        let selected, rest =
          take_checkpoint certificate.checkpoint_hash [] remaining
        in
        manifest_groups selected @ collect rest
  in
  match collect ordered with
  | [] -> fail "no source returned a valid quorum certificate"
  | candidates -> candidates

let select_manifest results =
  match select_manifests results with
  | selected :: _ -> selected
  | [] -> fail "checkpoint has no byte manifest"

let random_salt path =
  if Sys.file_exists path then
    let value =
      match Yojson.Safe.from_file path with
      | `String value -> value
      | _ -> fail "source salt is corrupt"
    in
    if String.length value <> 64
       || not (String.for_all (function
         | '0'..'9' | 'a'..'f' -> true
         | _ -> false
       ) value) then fail "source salt is corrupt";
    value
  else
    let input = open_in_bin "/dev/urandom" in
    let raw =
      Fun.protect
        ~finally:(fun () -> close_in_noerr input)
        (fun () -> really_input_string input 32)
    in
    let salt = Manifest.raw_to_hex raw in
    Manifest.write_json path (`String salt);
    let input = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr input)
      (fun () ->
        match Yojson.Safe.from_channel input with
        | `String value -> value
        | _ -> fail "source salt write failed")

let chunk_url source certificate task =
  let manifest = certificate.Manifest.manifest in
  let query =
    Uri.encoded_of_query [
      "snapshot", [manifest.snapshot_id];
      "path", [task.file.path];
      "index", [string_of_int task.chunk.index];
      "sha256", [task.chunk.sha256];
    ]
  in
  source.Source.url ^ "/state-sync/chunk?" ^ query

let require_header response name expected =
  let actual =
    Cohttp.Response.headers response
    |> fun headers -> Cohttp.Header.get headers name
  in
  if actual <> Some expected then
    Lwt.fail_with ("response header mismatch: " ^ name)
  else
    Lwt.return_unit

let fetch_chunk source certificate task =
  get_limited
    ~timeout:!timeout_seconds
    ~limit:task.chunk.size
    (chunk_url source certificate task) >>= fun (response, payload) ->
  require_header response "x-octra-chunk-sha256" task.chunk.sha256 >>= fun () ->
  require_header response "x-octra-manifest-sha256" certificate.Manifest.manifest_hash >>= fun () ->
  require_header response "x-octra-snapshot" certificate.manifest.snapshot_id >>= fun () ->
  require_header response "x-octra-chunk-index" (string_of_int task.chunk.index) >>= fun () ->
  if String.length payload <> task.chunk.size then
    Lwt.fail_with "chunk length mismatch"
  else
    let actual = Digestif.SHA256.(digest_string payload |> to_hex) in
    if actual <> task.chunk.sha256 then Lwt.fail_with "chunk hash mismatch"
    else Lwt.return payload

let check_task certificate =
  let choose selected file chunk =
    match selected with
    | None -> Some { file; partial = ""; chunk }
    | Some task ->
        let size_order = compare chunk.Manifest.size task.chunk.size in
        let path_order = String.compare file.Manifest.path task.file.path in
        let index_order = compare chunk.index task.chunk.index in
        if size_order < 0
           || (size_order = 0 && path_order < 0)
           || (size_order = 0 && path_order = 0 && index_order < 0)
        then Some { file; partial = ""; chunk }
        else selected
  in
  certificate.Manifest.manifest.files
  |> List.fold_left
       (fun selected file ->
          List.fold_left
            (fun selected chunk -> choose selected file chunk)
            selected
            file.Manifest.chunks)
       None

let check_manifest_chunk certificate sources =
  match check_task certificate with
  | None -> Lwt.fail_with "state sync manifest has no chunk"
  | Some task ->
      let rec loop last_error = function
        | [] ->
            begin
              match last_error with
              | Some exn -> Lwt.fail exn
              | None -> Lwt.fail_with "state sync manifest has no source"
            end
        | source :: rest ->
            Lwt.catch
              (fun () -> fetch_chunk source certificate task >|= fun _ -> ())
              (fun exn -> loop (Some exn) rest)
      in
      loop None sources

let prepare_sources values =
  let rec loop sources = function
    | [] -> List.rev sources
    | value :: rest ->
        begin
          match Source.create ~allow_private_http:!allow_private_http value with
          | Ok source -> loop (source :: sources) rest
          | Error reason -> fail reason
        end
  in
  let sources = loop [] values in
  let urls = List.map (fun source -> source.Source.url) sources in
  if urls <> List.sort_uniq String.compare urls then fail "duplicate state sync source";
  sources

let source_pools sources =
  let pools = Hashtbl.create (List.length sources) in
  List.iter (fun source ->
    Hashtbl.add pools source.Source.url (Lwt_pool.create !source_concurrency (fun () -> Lwt.return_unit))
  ) sources;
  pools

let write_verified_marker root certificate sources =
  let checkpoint = certificate.Manifest.checkpoint in
  let manifest = certificate.manifest in
  let path = Filename.concat root "snapshot_verified.json" in
  Manifest.write_json path (`Assoc [
    "version", `String "octra-state-sync-verified";
    "status", `String "snapshot_verified";
    "voting", `Bool false;
    "chain_id", `String checkpoint.chain_id;
    "config_hash", `String checkpoint.config_hash;
    "checkpoint_hash", `String certificate.checkpoint_hash;
    "manifest_hash", `String certificate.manifest_hash;
    "snapshot_id", `String manifest.snapshot_id;
    "snapshot_epoch", `String (Int64.to_string checkpoint.epoch);
    "state_root", `String checkpoint.state_root;
    "ledger_state_root", `String checkpoint.ledger_state_root;
    "sources", `List (List.map (fun source -> `String source.Source.url) sources);
    "verified_at", `String (Int64.of_float (Unix.gettimeofday ()) |> Int64.to_string);
  ])

let finalize_one ~data_dir file =
  match Journal.prepare_file ~stage:data_dir file with
  | Error reason -> Lwt.fail_with reason
  | Ok (destination, partial, _) ->
      Lwt_preemptive.detach
        (fun () -> Journal.finalize_file ~destination ~partial file)
        () >>= function
      | Ok () -> Lwt.return_unit
      | Error reason -> Lwt.fail_with (file.Manifest.path ^ ": " ^ reason)

let finalize_group ~parallelism ~data_dir files =
  let pending = Queue.create () in
  List.iter (fun file -> Queue.add file pending) files;
  let lock = Lwt_mutex.create () in
  let take () =
    Lwt_mutex.with_lock lock (fun () ->
      Lwt.return (Queue.take_opt pending))
  in
  let rec worker () =
    take () >>= function
    | None -> Lwt.return_unit
    | Some file -> finalize_one ~data_dir file >>= worker
  in
  Lwt.join (List.init (max 1 (min 8 parallelism)) (fun _ -> worker ()))

let finalize_files ~parallelism ~data_dir files =
  let regular, head =
    files
    |> List.sort (fun left right -> String.compare left.Manifest.path right.path)
    |> List.partition (fun file -> file.Manifest.path <> "HEAD.json")
  in
  finalize_group ~parallelism ~data_dir regular >>= fun () ->
  Lwt_list.iter_s (finalize_one ~data_dir) head

let retained_head root =
  Filename.concat root "HEAD.source.json"

let regular_file path =
  try (Unix.lstat path).Unix.st_kind = Unix.S_REG with _ -> false

let sync_dir path =
  let descriptor = Unix.openfile path [Unix.O_RDONLY] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close descriptor)
    (fun () -> Unix.fsync descriptor)

let link_replace source target =
  let temporary = target ^ ".source" in
  if Sys.file_exists temporary then Unix.unlink temporary;
  Unix.link source temporary;
  Unix.rename temporary target;
  sync_dir (Filename.dirname target)

let head_file certificate =
  List.find_opt
    (fun file -> file.Manifest.path = "HEAD.json")
    certificate.Manifest.manifest.files

let retain_head ~root ~data_dir file =
  let source = Filename.concat data_dir file.Manifest.path in
  let retained = retained_head root in
  if Sys.file_exists retained && not (regular_file retained) then
    Error "retained HEAD is not a regular file"
  else if not (regular_file source) then
    Error "source HEAD is not a regular file"
  else match Journal.hash_file retained with
  | Ok hash when hash = file.sha256 -> Ok ()
  | Ok _ -> Error "retained HEAD hash mismatch"
  | Error _ when not (Sys.file_exists retained) ->
      begin
        match Journal.hash_file source with
        | Ok hash when hash = file.sha256 ->
            begin
              try
                Unix.link source retained;
                sync_dir root;
                Ok ()
              with exn -> Error (Printexc.to_string exn)
            end
        | Ok _ -> Error "source HEAD hash mismatch"
        | Error reason -> Error reason
      end
  | Error reason -> Error reason

let restore_head ~root ~data_dir file =
  let retained = retained_head root in
  if not (Sys.file_exists retained) then Ok ()
  else if not (regular_file retained) then
    Error "retained HEAD is not a regular file"
  else
    match Journal.hash_file retained with
    | Ok hash when hash = file.Manifest.sha256 ->
        let target = Filename.concat data_dir file.path in
        begin
          match Journal.hash_file target with
          | Ok current when current = hash -> Ok ()
          | Ok _ | Error _ ->
              begin
                try
                  link_replace retained target;
                  Ok ()
                with exn -> Error (Printexc.to_string exn)
              end
        end
    | Ok _ -> Error "retained HEAD hash mismatch"
    | Error reason -> Error reason

let validate_rebuild checkpoint (report : Store.rebuild_report) =
  if Int64.compare checkpoint.Checkpoint.epoch (Int64.of_int max_int) >= 0 then
    Error "checkpoint epoch exceeds platform limit"
  else if report.transactions <> Int64.succ checkpoint.txid_hi then
    Error "rebuilt transaction high-water does not match checkpoint"
  else if report.last_epoch <> Int64.to_int checkpoint.epoch then
    Error "rebuilt epoch high-water does not match checkpoint"
  else if report.epochs <> report.last_epoch + 1 then
    Error "rebuilt epoch sequence is incomplete"
  else
    match checkpoint.epoch_index_root with
    | Some expected when report.epoch_index_root = expected -> Ok ()
    | Some _ -> Error "rebuilt epoch index root does not match checkpoint"
    | None -> Error "checkpoint epoch index root is missing"

let rebuild_public_keys data_dir =
  let path = Filename.concat data_dir "irmin_store" in
  Ledger_store.open_store ~readonly:true path >>= fun ledger_store ->
  Lwt.finalize
    (fun () ->
      Ledger_store.load_all_accounts ledger_store >|= fun accounts ->
      List.fold_left
        (fun keys (address, account) ->
          match account.Octra_core.Ledger_types.public_key with
          | Some public_key -> String_map.add address public_key keys
          | None -> keys)
        String_map.empty
        accounts)
    (fun () -> Ledger_store.close ledger_store)

let rebuild_state checkpoint data_dir =
  rebuild_public_keys data_dir >>= fun public_keys ->
  Lwt_preemptive.detach
    (fun () ->
      try
        let store =
          Store.open_chaindata (Filename.concat data_dir "chaindata")
        in
        Fun.protect
          ~finally:(fun () -> Store.close store)
          (fun () ->
            match
              Store.rebuild_index
                ~public_key_of:(fun address ->
                  String_map.find_opt address public_keys)
                store
            with
            | Error _ as error -> error
            | Ok report -> validate_rebuild checkpoint report)
      with exn -> Error (Printexc.to_string exn))
    ()

let restore_ledger checkpoint data_dir =
  let source = Filename.concat data_dir "ledger.dat" in
  let target = Filename.concat data_dir "irmin_store" in
  Image.restore
    ~source
    ~target
    ~expected_root:checkpoint.Checkpoint.ledger_state_root
  >>= function
  | Error _ as error -> Lwt.return error
  | Ok report ->
      begin
        match Head.load_result data_dir with
        | Head.Missing -> Lwt.return_error "restored HEAD.json is missing"
        | Head.Corrupt reason ->
            Lwt.return_error ("restored HEAD.json is corrupt: " ^ reason)
        | Head.Present head
          when not (Checkpoint.matches_head checkpoint head) ->
            Lwt.return_error "restored HEAD.json does not match checkpoint"
        | Head.Present head ->
            Head.atomic_write
              data_dir
              { head with Head.irmin_commit = Some report.commit };
            Lwt.return_ok report
      end

let remove_ledger data_dir =
  let path = Filename.concat data_dir "ledger.dat" in
  if Sys.file_exists path then begin
    Unix.unlink path;
    let descriptor = Unix.openfile data_dir [Unix.O_RDONLY] 0 in
    Fun.protect
      ~finally:(fun () -> Unix.close descriptor)
      (fun () -> Unix.fsync descriptor)
  end

let run_sync ?(verify_state = Verify.verify) certificate sources root =
  let body = certificate.Manifest.manifest in
  let checkpoint = certificate.checkpoint in
  let data_dir = Filename.concat root "data" in
  let journal_path = Filename.concat root "journal.jsonl" in
  let certificate_file = Filename.concat root "certificate.json" in
  mkdir_p root;
  mkdir_p data_dir;
  if Manifest.is_reference certificate then begin
    match head_file certificate with
    | Some file ->
        begin
          match restore_head ~root ~data_dir file with
          | Ok () -> ()
          | Error reason -> fail reason
        end
    | None -> fail "reference snapshot has no HEAD"
  end;
  Manifest.write_json certificate_file (Manifest.certificate_json certificate);
  let journal =
    match Journal.open_journal ~path:journal_path ~manifest_hash:certificate.manifest_hash with
    | Ok journal -> journal
    | Error reason -> fail reason
  in
  let pending = Queue.create () in
  let completed_bytes = ref 0L in
  List.iter (fun file ->
    match Journal.prepare_file ~stage:data_dir file with
    | Error reason -> fail reason
    | Ok (destination, _, `Final) ->
        begin
          match Journal.hash_file destination with
          | Ok hash when hash = file.sha256 ->
              completed_bytes := Int64.add !completed_bytes file.size
          | Ok _ -> fail ("existing final file hash mismatch: " ^ file.path)
          | Error reason -> fail reason
        end
    | Ok (_, partial, `Partial) ->
        List.iter (fun chunk ->
          if Journal.is_completed journal file.path chunk then
            match Journal.verify_completed_chunk ~partial chunk with
            | Ok true ->
                completed_bytes :=
                  Int64.add !completed_bytes (Int64.of_int chunk.size)
            | Ok false ->
                Journal.record_invalid journal file.path chunk;
                Queue.add { file; partial; chunk } pending
            | Error reason -> fail reason
          else
            Queue.add { file; partial; chunk } pending
        ) file.chunks
  ) body.files;
  let source_pools = source_pools sources in
  let journal_lock = Lwt_mutex.create () in
  let progress_lock = Lwt_mutex.create () in
  let downloaded = ref !completed_bytes in
  let last_log = ref 0.0 in
  let salt = random_salt (Filename.concat root "source_salt.json") in
  let report_progress task source =
    Lwt_mutex.with_lock progress_lock (fun () ->
      downloaded := Int64.add !downloaded (Int64.of_int task.chunk.size);
      let now = Unix.gettimeofday () in
      if now -. !last_log >= 5.0 || !downloaded = body.total_size then begin
        last_log := now;
        let percent =
          if body.total_size = 0L then 100.0
          else Int64.to_float !downloaded /. Int64.to_float body.total_size *. 100.0
        in
        Printf.printf
          "event = sync_progress done = %Ld total = %Ld pct = %.2f source = %s path = %s index = %d\n%!"
          !downloaded
          body.total_size
          percent
          source.Source.url
          task.file.path
          task.chunk.index
      end;
      Lwt.return_unit)
  in
  let rec choose_source task attempted =
    let now = Unix.gettimeofday () in
    let key = Journal.chunk_key task.file.path task.chunk in
    match Source.rank ~salt ~chunk_key:key ~now ~attempted sources with
    | source :: _ -> Lwt.return (source, attempted)
    | [] ->
        let attempted =
          if List.length attempted >= List.length sources then [] else attempted in
        let wake =
          match Source.earliest_cooldown ~attempted sources with
          | Some value -> max 0.05 (value -. now)
          | None -> 0.05
        in
        Lwt_unix.sleep (min 5.0 wake) >>= fun () ->
        choose_source task attempted
  in
  let rec download_task task attempt attempted =
    if attempt >= !retries then
      Lwt.fail_with
        (Printf.sprintf "chunk retry budget exhausted path = %s index = %d"
          task.file.path task.chunk.index)
    else
      choose_source task attempted >>= fun (source, attempted) ->
      let pool = Hashtbl.find source_pools source.Source.url in
      let started = Unix.gettimeofday () in
      Lwt.catch
        (fun () ->
          Lwt_pool.use pool (fun () -> fetch_chunk source certificate task) >>= fun payload ->
          Lwt_preemptive.detach
            (fun () -> Journal.write_chunk ~partial:task.partial task.chunk payload)
            () >>= function
          | Error reason -> Lwt.fail_with reason
          | Ok () ->
              Lwt_mutex.with_lock journal_lock (fun () ->
                Journal.record_completed journal task.file.path task.chunk;
                Lwt.return_unit) >>= fun () ->
              Source.record_success source ((Unix.gettimeofday () -. started) *. 1000.0);
              report_progress task source)
        (fun exn ->
          Source.record_failure source (Unix.gettimeofday ());
          Printf.eprintf
            "event = chunk_retry source = %s path = %s index = %d attempt = %d error = %s\n%!"
            source.url
            task.file.path
            task.chunk.index
            (attempt + 1)
            (Printexc.to_string exn);
          download_task task (attempt + 1) (source.url :: attempted))
  in
  let rec worker () =
    if Queue.is_empty pending then Lwt.return_unit
    else
      let task = Queue.take pending in
      download_task task 0 [] >>= worker
  in
  Printf.printf
    "event = sync_start hash = %s epoch = %Ld files = %d chunks = %d sources = %d pending = %d bytes = %Ld resumed = %Ld\n%!"
    certificate.manifest_hash
    checkpoint.epoch
    body.file_count
    body.chunk_count
    (List.length sources)
    (Queue.length pending)
    body.total_size
    !completed_bytes;
  Lwt.join (List.init !concurrency (fun _ -> worker ())) >>= fun () ->
  Printf.printf
    "event = sync_download_complete hash = %s bytes = %Ld\n%!"
    certificate.manifest_hash
    body.total_size;
  Printf.printf
    "event = sync_finalize_start hash = %s files = %d\n%!"
    certificate.manifest_hash
    body.file_count;
  finalize_files ~parallelism:!concurrency ~data_dir body.files >>= fun () ->
  Printf.printf
    "event = sync_finalize_complete hash = %s files = %d\n%!"
    certificate.manifest_hash
    body.file_count;
  begin
    if Manifest.is_reference certificate then begin
      begin
        match head_file certificate with
        | Some file ->
            begin
              match retain_head ~root ~data_dir file with
              | Ok () -> ()
              | Error reason -> fail reason
            end
        | None -> fail "reference snapshot has no HEAD"
      end;
      Printf.printf
        "event = sync_ledger_start hash = %s epoch = %Ld\n%!"
        certificate.manifest_hash
        checkpoint.epoch;
      restore_ledger checkpoint data_dir >>= function
      | Error reason -> Lwt.fail_with reason
      | Ok report ->
          Printf.printf
            "event = sync_ledger_complete hash = %s epoch = %Ld records = %d root = %s\n%!"
            certificate.manifest_hash
            checkpoint.epoch
            report.Image.records
            report.root;
      Printf.printf
        "event = sync_index_start hash = %s epoch = %Ld\n%!"
        certificate.manifest_hash
        checkpoint.epoch;
      rebuild_state checkpoint data_dir >>= function
      | Error reason -> Lwt.fail_with reason
      | Ok () ->
          Printf.printf
            "event = sync_index_complete hash = %s epoch = %Ld\n%!"
            certificate.manifest_hash
            checkpoint.epoch;
          Lwt.return_unit
    end else Lwt.return_unit
  end >>= fun () ->
  Printf.printf
    "event = sync_verify_start hash = %s epoch = %Ld\n%!"
    certificate.manifest_hash
    checkpoint.epoch;
  verify_state checkpoint data_dir >>= function
  | Error reason -> Lwt.fail_with reason
  | Ok () ->
      if Manifest.is_reference certificate then remove_ledger data_dir;
      Manifest.write_json
        (State_sync.anchor_path data_dir)
        (Manifest.certificate_json certificate);
      write_verified_marker root certificate sources;
      Printf.printf
        "event = sync_verified hash = %s epoch = %Ld state_root = %s data = %s voting = false\n%!"
        certificate.manifest_hash
        checkpoint.epoch
        checkpoint.state_root
        data_dir;
      Lwt.return_unit

let sync_manifests ~max_bytes ~stage ~check ~sync candidates =
  let rec loop = function
    | [] -> Lwt.fail (Sync_error "all state sync byte manifests failed")
    | (certificate, matching_sources) :: rest ->
        let attempt () =
          if Int64.compare certificate.Manifest.manifest.total_size max_bytes > 0 then
            check certificate matching_sources >>= fun () ->
            Lwt.fail (Sync_error "snapshot exceeds configured byte limit")
          else
            let root =
              Filename.concat
                (Filename.concat stage "snapshots")
                certificate.Manifest.manifest_hash
            in
            sync certificate matching_sources root
        in
        Lwt.catch
          attempt
          (fun exn ->
            Printf.eprintf
              "event = sync_manifest_failed hash = %s error = %s\n%!"
              certificate.manifest_hash
              (Printexc.to_string exn);
            match rest with
            | [] -> Lwt.fail exn
            | _ -> loop rest)
  in
  loop candidates

let run () =
  Arg.parse options (fun value -> fail ("unexpected argument: " ^ value)) usage;
  let stage = require "stage" !stage_dir in
  let _ = require "chain-id" !chain_id in
  let _ = require "config-hash" !config_hash in
  if not (Checkpoint.lower_hex 64 !config_hash) then fail "config hash must be lowercase hex64";
  if !concurrency < 1 || !concurrency > 32 then fail "concurrency must be in 1..32";
  if !source_concurrency < 1 || !source_concurrency > 8 then
    fail "source concurrency must be in 1..8";
  if !retries < 1 || !retries > 32 then fail "retries must be in 1..32";
  if !timeout_seconds < 1.0 || !timeout_seconds > 300.0 then
    fail "timeout must be in 1..300 seconds";
  if Int64.compare !max_bytes 1L < 0 then fail "max bytes must be positive";
  let sources = prepare_sources (List.rev !source_values) in
  if sources = [] then fail "at least one source is required";
  require_transport ~tls_available:Conduit_lwt_tls.available sources;
  let validator_set =
    match Manifest.validator_set_of_entries (List.rev !validator_entries) with
    | Ok validator_set -> validator_set
    | Error reason -> fail reason
  in
  let exporter_set =
    match Manifest.validator_set_of_entries (List.rev !exporter_entries) with
    | Ok exporter_set -> exporter_set
    | Error reason -> fail reason
  in
  mkdir_p stage;
  Lwt_list.map_p
    (fun source ->
      Lwt.catch
        (fun () ->
          fetch_manifest validator_set exporter_set source >|= fun result -> Some result)
        (fun exn ->
          Printf.eprintf
            "event = manifest_source_rejected source = %s error = %s\n%!"
            source.Source.url
            (Printexc.to_string exn);
          Lwt.return_none))
    sources >>= fun results ->
  let candidates =
    results |> List.filter_map Fun.id |> select_manifests
  in
  sync_manifests
    ~max_bytes:!max_bytes
    ~stage
    ~check:check_manifest_chunk
    ~sync:run_sync
    candidates

let main () =
  try Lwt_main.run (run ()) with
  | Sync_error message ->
      Printf.eprintf "error = %s\n%!" message;
      exit 1
  | exn ->
      Printf.eprintf "error = %s\n%!" (Printexc.to_string exn);
      exit 1