(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Publish = Octra_node_runtime.Sync_publish
module Manifest = Octra_bootstrap.State_sync_manifest
module Sync = Octra_bootstrap.State_sync
module Head = Octra_core.Head_manifest

let get = function
  | Ok value -> value
  | Error reason -> failwith reason

let check reason value =
  if not value then failwith reason

let read path = In_channel.with_open_bin path In_channel.input_all

let write path body =
  Out_channel.with_open_bin path (fun channel -> output_string channel body)

let publication ~run ~validators (deps : Publish.deps) certificate =
  Test_workspace.with_dir "sync_publish_phase" (fun root ->
    let path = Filename.concat root "certificate.json" in
    let id = certificate.Manifest.manifest.snapshot_id in
    let target = Sync.snapshot_dir root id in
    let image dir = Filename.concat dir "ledger.dat" in
    let deps = Publish.{ deps with
      data_dir = root;
      certificate_path = (fun () -> path);
    } in
    let stopped = Publish.{ deps with exporter_set = (fun () ->
      if Sys.file_exists (image target) || Sys.file_exists (image (target ^ ".next"))
      then Error "publisher permission changed" else Ok validators) } in
    run stopped;
    check "unsigned image became visible" (not (Sys.file_exists target));
    check "refused publisher advertised a certificate" (not (Sys.file_exists path));
    run deps;
    let published = Manifest.load_certificate path |> get in
    ignore (Manifest.verify_certificate ~validator_set:validators ~exporter_set:validators published |> get);
    check "publication changed manifest" (published.manifest = certificate.manifest);
    check "published image has no certificate"
      (Manifest.load_certificate (Sync.snapshot_certificate_path target) |> get = published))

let stages ~run (deps : Publish.deps) certificate =
  let module Archive = Octra_bootstrap.Sync_archive in
  List.iter (fun mode ->
    Test_workspace.with_dir "sync_publish_retry" (fun root ->
      let path = Filename.concat root "certificate.json" in
      let id = certificate.Manifest.manifest.snapshot_id in
      let target = Sync.snapshot_dir root id in
      let stage = target ^ ".next" in
      let image = Filename.concat target "ledger.dat" in
      let pinned = Filename.concat root "ledger.saved" in
      let deps = Publish.{ deps with
        data_dir = root;
        certificate_path = (fun () -> path);
      } in
      run deps;
      Unix.link image pinned;
      let inode = (Unix.stat pinned).Unix.st_ino in
      let body = read image in
      Unix.unlink path;
      get (Archive.run (Sync.snapshot_root root) (fun owner ->
        Unix.rename target stage;
        Archive.mark_stage owner id;
        if mode = "moved" then begin
          let parent = Unix.stat (Sync.snapshot_root root) in
          let sync fd =
            let stat = Unix.fstat fd in
            if stat.st_dev = parent.st_dev && stat.st_ino = parent.st_ino then
              raise (Unix.Unix_error (Unix.EIO, "fsync", "archive"));
            Unix.fsync fd in
          let refused = try Archive.publish_stage ~sync owner id; false with
            Unix.Unix_error (Unix.EIO, "fsync", "archive") -> true in
          check "publication missed directory sync failure" refused;
          check "publication failure did not follow rename" (Sys.file_exists target)
        end));
      if mode = "staged" then begin
        let archived = Sync.snapshot_certificate_path stage in
        let saved = read archived in
        write archived "{";
        run deps;
        check "producer trusted damaged stage certificate" (not (Sys.file_exists path));
        check "producer rewrote refused signed stage"
          ((Unix.stat (Filename.concat stage "ledger.dat")).Unix.st_ino = inode);
        write archived saved;
        write (Filename.concat stage "ledger.dat") (body ^ "invalid");
        run deps;
        check "producer trusted changed signed stage" (not (Sys.file_exists path));
        check "producer removed changed signed stage" (Sys.file_exists stage);
        write (Filename.concat stage "ledger.dat") body
      end;
      run deps;
      check "producer recaptured signed stage" ((Unix.stat image).Unix.st_ino = inode);
      check "producer changed retried image" (read image = body);
      check "producer retry left ownership marker"
        (not (Sys.file_exists (Filename.concat target ".writer")));
      check "producer retry left signed stage" (not (Sys.file_exists stage));
      check "producer retry changed certificate"
        (Manifest.load_certificate path |> get = certificate))) ["staged"; "moved"]

let links ~run (deps : Publish.deps) certificate =
  Test_workspace.with_dir "sync_publish_link" (fun root ->
    let path = Filename.concat root "certificate.json" in
    let target = Sync.snapshot_dir root certificate.Manifest.manifest.snapshot_id in
    let outside = Filename.concat root "outside" in
    let deps = Publish.{ deps with data_dir = root; certificate_path = (fun () -> path) } in
    run deps;
    Unix.unlink path;
    Unix.rename target outside;
    Unix.symlink outside target;
    let archived = Sync.snapshot_certificate_path outside in
    let inode = (Unix.stat archived).Unix.st_ino in
    run deps;
    check "producer advertised linked image" (not (Sys.file_exists path));
    check "producer wrote through snapshot link" ((Unix.stat archived).Unix.st_ino = inode);
    Unix.unlink target;
    Unix.rename outside target)

let archived_links ~run (deps : Publish.deps) prior =
  Test_workspace.with_dir "sync_archive_link" (fun root ->
    let path = Filename.concat root "certificate.json" in
    let outside = Filename.concat root "outside" in
    let target = Sync.snapshot_dir root prior.Manifest.manifest.snapshot_id in
    let deps = Publish.{ deps with data_dir = root; certificate_path = (fun () -> path) } in
    run deps;
    let current = Manifest.load_certificate path |> get in
    check "archive test needs distinct images" (current.checkpoint_hash <> prior.checkpoint_hash);
    Unix.mkdir outside 0o750;
    let archived = Sync.snapshot_certificate_path outside in
    write archived "preserve";
    let pinned = Filename.concat root "certificate.saved" in
    Unix.link archived pinned;
    let inode = (Unix.stat pinned).Unix.st_ino in
    Unix.symlink outside target;
    Manifest.write_json path (Manifest.certificate_json prior);
    run deps;
    check "archive wrote through previous image link" ((Unix.stat archived).Unix.st_ino = inode);
    check "archive changed linked certificate" (read archived = "preserve");
    check "archive link refusal changed publication" (Manifest.load_certificate path |> get = prior);
    Unix.unlink target;
    Unix.rename outside target;
    run deps;
    check "archive link recovery changed publication" (Manifest.load_certificate path |> get = current);
    check "archive did not save prior certificate" (Manifest.load_certificate
      (Sync.snapshot_certificate_path target) |> get = prior))

let retry ~run ~validators (deps : Publish.deps) certificate =
  let path = deps.certificate_path () in
  let target = Sync.snapshot_dir deps.data_dir certificate.Manifest.manifest.snapshot_id in
  let archived = Sync.snapshot_certificate_path target in
  let image = Filename.concat target "ledger.dat" in
  let pinned = Filename.concat deps.data_dir "ledger.saved" in
  Unix.link image pinned;
  let inode = (Unix.stat pinned).Unix.st_ino in
  let bytes = read pinned in
  let preserved () =
    check "producer replaced published image" ((Unix.stat image).Unix.st_ino = inode);
    check "producer changed published image" (read image = bytes);
    check "producer left retry stage" (not (Sys.file_exists (target ^ ".next"))) in
  let accepted () =
    let current = Manifest.load_certificate path |> get in
    ignore (Manifest.verify_certificate ~validator_set:validators ~exporter_set:validators current |> get);
    check "producer retry changed manifest" (current.manifest = certificate.manifest);
    check "producer retry changed checkpoint" (current.checkpoint = certificate.checkpoint);
    preserved () in
  write path "{";
  run deps;
  accepted ();
  Unix.unlink path;
  run deps;
  accepted ();
  let invalid = Manifest.{ certificate with authority = Finalized "invalid" } in
  Manifest.write_json path (Manifest.certificate_json invalid);
  run deps;
  accepted ();
  let bad_exporter = Manifest.{ certificate with exporter_signatures = [] } in
  List.iter (fun broken ->
    let body = Manifest.certificate_json broken in
    Manifest.write_json archived body;
    Manifest.write_json path body;
    let before = read path in
    run deps;
    check "producer signed unauthenticated image" (read path = before);
    check "producer changed refused certificate" (read archived = before);
    preserved ())
    [invalid; bad_exporter;
     { certificate with manifest_hash = String.make 64 '0' }];
  Unix.unlink path;
  Unix.unlink archived;
  run deps;
  check "producer signed image without certificate" (not (Sys.file_exists path));
  preserved ();
  Manifest.write_json archived (Manifest.certificate_json certificate);
  write path "{";
  write image (bytes ^ "invalid");
  run deps;
  check "producer signed changed image" (read path = "{");
  check "producer deleted changed image" ((Unix.stat image).Unix.st_ino = inode);
  check "producer rewrote changed image" (read image = bytes ^ "invalid");
  write image bytes;
  run deps;
  accepted ()

let retention ~run (deps : Publish.deps) certificate =
  let path = deps.certificate_path () in
  let target = Sync.snapshot_dir deps.data_dir certificate.Manifest.manifest.snapshot_id in
  let image = Filename.concat target "ledger.dat" in
  let body = read image in
  let head = match Head.load_result target with
    | Head.Present value -> value
    | _ -> failwith "producer image head is missing" in
  let extra = List.map (fun offset ->
    let id = "unpublished" ^ string_of_int offset in
    let dir = Sync.snapshot_dir deps.data_dir id in
    Unix.mkdir dir 0o750;
    write (Filename.concat dir "HEAD.json")
      (Head.to_json { head with epoch_id = head.epoch_id + offset });
    dir) [1; 2; 3] in
  List.iter (fun mode ->
    let observed = ref false in
    let warned = ref false in
    let deps = Publish.{ deps with
      info = (fun event ->
        if String.starts_with ~prefix:"event = sync_published " event then begin
          observed := true;
          match mode with
          | "read" -> write path "{"
          | "missing" -> Unix.unlink path
          | "authority" -> Manifest.write_json path
              (Manifest.certificate_json { certificate with authority = Finalized "invalid" })
          | "signature" -> Manifest.write_json path
              (Manifest.certificate_json { certificate with exporter_signatures = [] })
          | _ -> failwith "unknown retention case"
        end);
      warn = (fun event ->
        if String.starts_with ~prefix:"event = sync_retention_failed " event then
          warned := true);
    } in
    write path "{";
    run deps;
    check "retention test missed publication" !observed;
    check "retention deleted published image on read failure" (Sys.file_exists image);
    check "retention changed published image" (read image = body);
    check "retention hid certificate failure" !warned;
    List.iter (fun dir ->
      check "retention deleted archive during read failure" (Sys.file_exists dir)) extra)
    ["read"; "missing"; "authority"; "signature"];
  write path "{";
  run deps;
  check "retention did not resume after recovery" (not (Sys.file_exists (List.hd extra)));
  check "retention removed recovered publication" (read image = body);
  List.iter (fun dir ->
    check "retention removed recent image" (Sys.file_exists dir)) (List.tl extra);
  check "retention recovery changed certificate"
    (Manifest.load_certificate path |> get = certificate)