(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module S = Octra_core.Store_irmin
module F = Octra_core.Set_fold
module T = Octra_core.Ledger_types

let fail reason =
  failwith ("test_store_growth: " ^ reason)

let expect reason value =
  if not value then fail reason

let expect_ok reason = function
  | Ok value -> value
  | Error error -> fail (reason ^ ": " ^ error)

let rec remove path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | stat when stat.Unix.st_kind = Unix.S_DIR ->
    Sys.readdir path
    |> Array.iter (fun name -> remove (Filename.concat path name));
    Unix.rmdir path
  | _ -> Unix.unlink path

let ensure path =
  if Sys.file_exists path then
    expect "work path is not a directory" (Sys.is_directory path)
  else
    Unix.mkdir path 0o700

let work_dir () =
  let data = Filename.concat (Sys.getcwd ()) "runtime_data" in
  let scope = Filename.concat data "store-growth-tests" in
  ensure data;
  ensure scope;
  let name =
    Printf.sprintf "run-%d-%.0f"
      (Unix.getpid ())
      (Unix.gettimeofday () *. 1_000_000.)
  in
  let path = Filename.concat scope name in
  Unix.mkdir path 0o700;
  path

let rec disk_bytes path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> 0L
  | stat when stat.Unix.st_kind = Unix.S_DIR ->
    Sys.readdir path
    |> Array.fold_left
         (fun sum name ->
           Int64.add sum (disk_bytes (Filename.concat path name)))
         0L
  | stat when stat.Unix.st_kind = Unix.S_REG ->
    Int64.of_int stat.Unix.st_size
  | _ -> 0L

let rec suffix_bytes path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> 0L
  | stat when stat.Unix.st_kind = Unix.S_DIR ->
    Sys.readdir path
    |> Array.fold_left
         (fun sum name ->
           Int64.add sum (suffix_bytes (Filename.concat path name)))
         0L
  | stat when stat.Unix.st_kind = Unix.S_REG
              && Filename.check_suffix path ".suffix" ->
    Int64.of_int stat.Unix.st_size
  | _ -> 0L

let wait_disk path before =
  let rec sample () =
    let bytes = disk_bytes path in
    if bytes < before then Lwt.return bytes
    else
      let* () = Lwt_unix.sleep 0.01 in
      sample ()
  in
  Lwt_main.run (Lwt_unix.with_timeout 5.0 sample)

let address id =
  let hash =
    Digestif.SHA256.digest_string (string_of_int id)
    |> Digestif.SHA256.to_hex
  in
  "oct" ^ String.sub hash 0 44

let account ?cipher balance = T.{
  balance = Z.of_int balance;
  nonce = 0;
  public_key = None;
  encrypted_balance = cipher;
  decrypt_allowance = Z.zero;
}

let cipher size id =
  let alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  in
  let width = String.length alphabet in
  String.init size (fun index ->
    alphabet.[(id * 7 + index * 13 + (index / 65_521) * 17) mod width])

let rec seed store count id =
  if id = count then Lwt.return_unit
  else
    let* () = S.set_account store (address id) (account 1) in
    let* () =
      if id mod 512 = 0 then Lwt.pause () else Lwt.return_unit
    in
    seed store count (id + 1)

let write_epoch store mode ciphers epoch =
  let* () = S.begin_epoch_batch ~mode store in
  let rec reward id =
    if id = Array.length ciphers then Lwt.return_unit
    else
      let* () =
        S.set_account
          store
          (address id)
          (account ~cipher:ciphers.(id) (epoch + id + 2))
      in
      reward (id + 1)
  in
  let* () = reward 0 in
  let* () = S.set_meta store "last_epoch" (string_of_int epoch) in
  let* () = S.set_meta store "current_epoch" (string_of_int (epoch + 1)) in
  let* () = S.commit_epoch_batch store "epoch" in
  S.tag_epoch store epoch

let rec write_epochs store mode ciphers epoch count =
  if count = 0 then Lwt.return_unit
  else
    let* () = write_epoch store mode ciphers epoch in
    write_epochs store mode ciphers (epoch + 1) (count - 1)

let rec write_changed store size accounts epoch count =
  if count = 0 then Lwt.return_unit
  else
    let ciphers =
      Array.init accounts (fun id ->
        cipher size (1_000_000 + epoch * accounts + id))
    in
    let* () = write_epoch store Octra_core.Rule_graph.Active ciphers epoch in
    write_changed store size accounts (epoch + 1) (count - 1)

let head store =
  match Lwt_main.run (S.get_head_hash store) with
  | Some hash -> hash
  | None -> fail "store head is absent"

let commit store =
  match Lwt_main.run (S.get_commit_hash store) with
  | Some hash -> hash
  | None -> fail "store commit is absent"

let clone store source path =
  Lwt_main.run (S.create_compact_store store ~expected_commit:source ~target:path)
  |> expect_ok "compact store"

let with_store path run =
  let store = Lwt_main.run (S.open_store path) in
  Fun.protect
    ~finally:(fun () -> Lwt_main.run (S.close store))
    (fun () -> run store)

let grow path run =
  let disk_before = disk_bytes path in
  let suffix_before = suffix_bytes path in
  run ();
  Int64.sub (disk_bytes path) disk_before,
  Int64.sub (suffix_bytes path) suffix_before

let average bytes epochs =
  Int64.div bytes (Int64.of_int epochs)

let seed_store path accounts ciphers =
  let store = Lwt_main.run (S.open_store ~fresh:true path) in
  Lwt_main.run (S.begin_epoch_batch ~mode:Octra_core.Rule_graph.Prior store);
  Lwt_main.run (seed store accounts 0);
  Array.iteri
    (fun id raw ->
      Lwt_main.run (S.set_account store (address id) (account ~cipher:raw 1)))
    ciphers;
  Lwt_main.run (S.commit_epoch_batch store "seed");
  store

let rec wait_child pid =
  match Unix.waitpid [] pid with
  | value -> value
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait_child pid

let fork_run name run =
  match Unix.fork () with
  | 0 ->
    begin
      match run () with
      | () -> Unix._exit 0
      | exception _ -> Unix._exit 1
    end
  | pid ->
    let _, status = wait_child pid in
    expect name (status = Unix.WEXITED 0)

let crash_batch path cipher =
  fork_run "precommit crash child failed" (fun () ->
    let store = Lwt_main.run (S.open_store path) in
    Lwt_main.run (S.begin_epoch_batch ~mode:Octra_core.Rule_graph.Active store);
    Lwt_main.run
      (S.set_account store (address 0) (account ~cipher 999_999)))

let crash_commit path ciphers =
  fork_run "committed crash child failed" (fun () ->
    let store = Lwt_main.run (S.open_store path) in
    Lwt_main.run (write_epoch store Octra_core.Rule_graph.Active ciphers 1))

let gc_case root =
  let path = Filename.concat root "gc" in
  let size = 65_536 in
  let store = seed_store path 1 [|cipher size 0|] in
  Fun.protect
    ~finally:(fun () -> Lwt_main.run (S.close store))
    (fun () ->
      Lwt_main.run (write_changed store size 1 1 64);
      begin
        match Lwt_main.run (S.collect_pack_at store ~keep:1 64) with
        | S.Gc_split 64 -> ()
        | _ -> fail "pack split was not created"
      end;
      Lwt_main.run (write_changed store size 1 65 1);
      let before = disk_bytes path in
      let need =
        match Lwt_main.run (S.collect_pack_at ~free:0L store ~keep:1 65) with
        | S.Gc_space value -> value.need
        | _ -> fail "pack collection ignored zero free space"
      in
      let _, _, _, reported_need = Lwt_main.run (S.pack_gc_status store) in
      expect "pack estimate was not reported" (reported_need = Some need);
      expect "pack estimate did not improve historical size"
        (need < Int64.add before S.gc_reserve);
      let expected_root = head store in
      begin
        match Lwt_main.run (S.collect_pack_at ~free:need store ~keep:1 65) with
        | S.Gc_started _ -> ()
        | _ -> fail "pack collection did not restart"
      end;
      Lwt_main.run (S.wait_pack_gc store) |> expect_ok "pack collection";
      let _, _, _, reported_need = Lwt_main.run (S.pack_gc_status store) in
      expect "completed pack estimate was retained" (reported_need = None);
      expect "pack collection changed root"
        (String.equal expected_root (head store));
      let after = wait_disk path before in
      expect "pack collection did not reduce disk" (after < before);
      Printf.printf
        "event = store_growth mode = gc before = %Ld after = %Ld need = %Ld\n%!"
        before after need)

let run () =
  let accounts = 147_000 in
  let epochs = 256 in
  let cipher_size = 240_000 in
  let prior_epochs = 16 in
  let changed_epochs = 16 in
  let changed_accounts = 16 in
  let limit = 3_000_000L in
  let ciphers = Array.init F.standard.max_members (cipher cipher_size) in
  let prior_ciphers = Array.sub ciphers 0 21 in
  let root = work_dir () in
  let seed_path = Filename.concat root "seed" in
  let prior_path = Filename.concat root "prior" in
  let replay_path = Filename.concat root "replay" in
  let changed_path = Filename.concat root "changed" in
  let crash_path = Filename.concat root "crash" in
  let durable_path = Filename.concat root "durable" in
  let snapshot_path = Filename.concat root "snapshot" in
  Fun.protect
    ~finally:(fun () -> remove root)
    (fun () ->
      gc_case root;
      let seed = seed_store seed_path accounts ciphers in
      let seed_root, migration_root, migration_disk, migration_suffix, disk,
          suffix, disk_each, expected_root, expected_commit =
        Fun.protect
          ~finally:(fun () -> Lwt_main.run (S.close seed))
          (fun () ->
            let seed_root = head seed in
            let seed_commit = commit seed in
            let prior = clone seed seed_commit prior_path in
            let replay = clone seed seed_commit replay_path in
            let changed = clone seed seed_commit changed_path in
            let crash = clone seed seed_commit crash_path in
            let durable = clone seed seed_commit durable_path in
            expect "prior clone commit differs"
              (String.equal prior.commit_hash seed_commit);
            expect "replay clone commit differs"
              (String.equal replay.commit_hash seed_commit);
            expect "changed clone commit differs"
              (String.equal changed.commit_hash seed_commit);
            expect "crash clone commit differs"
              (String.equal crash.commit_hash seed_commit);
            expect "durable clone commit differs"
              (String.equal durable.commit_hash seed_commit);
            expect "prior clone tree differs"
              (String.equal prior.tree_hash seed_root);
            expect "replay clone tree differs"
              (String.equal replay.tree_hash seed_root);
            expect "changed clone tree differs"
              (String.equal changed.tree_hash seed_root);
            expect "crash clone tree differs"
              (String.equal crash.tree_hash seed_root);
            expect "durable clone tree differs"
              (String.equal durable.tree_hash seed_root);
            let migration_disk, migration_suffix =
              grow seed_path (fun () ->
                Lwt_main.run
                  (write_epoch seed Octra_core.Rule_graph.Active ciphers 1))
            in
            let migration_root = head seed in
            let disk, suffix =
              grow seed_path (fun () ->
                Lwt_main.run
                  (write_epochs
                     seed
                     Octra_core.Rule_graph.Active
                     ciphers
                     2
                     epochs))
            in
            let disk_each = average disk epochs in
            let suffix_each = average suffix epochs in
            expect "active growth crossed limit" (disk_each < limit);
            expect "active suffix crossed limit" (suffix_each < limit);
            let expected_root = head seed in
            let expected_commit = commit seed in
            let snapshot = clone seed expected_commit snapshot_path in
            expect "snapshot commit differs"
              (String.equal snapshot.commit_hash expected_commit);
            expect "snapshot tree differs"
              (String.equal snapshot.tree_hash expected_root);
            seed_root, migration_root, migration_disk, migration_suffix, disk,
            suffix, disk_each, expected_root, expected_commit)
      in
      crash_batch crash_path (cipher cipher_size 99);
      with_store crash_path (fun store ->
        expect "precommit crash changed root"
          (String.equal (head store) seed_root));
      crash_commit durable_path ciphers;
      with_store durable_path (fun store ->
        expect "committed crash lost root"
          (String.equal (head store) migration_root));
      with_store prior_path (fun store ->
        expect "prior clone head differs" (String.equal (head store) seed_root);
        let disk, suffix =
          grow prior_path (fun () ->
            Lwt_main.run
              (write_epochs
                 store
                 Octra_core.Rule_graph.Prior
                 prior_ciphers
                 1
                 prior_epochs))
        in
        let disk_each = average disk prior_epochs in
        let suffix_each = average suffix prior_epochs in
        expect "prior growth did not cross limit" (disk_each > limit);
        Printf.printf
          "event = store_growth mode = prior epochs = %d accounts = %d rewarded = %d cipher_bytes = %d disk = %Ld suffix = %Ld bytes_each = %Ld suffix_each = %Ld\n%!"
          prior_epochs accounts (Array.length prior_ciphers) cipher_size disk
          suffix disk_each suffix_each);
      with_store changed_path (fun store ->
        expect "changed clone head differs"
          (String.equal (head store) seed_root);
        let disk, suffix =
          grow changed_path (fun () ->
            Lwt_main.run
              (write_changed
                 store
                 cipher_size
                 changed_accounts
                 1
                 changed_epochs))
        in
        let payload =
          Int64.of_int (cipher_size * changed_accounts * changed_epochs)
        in
        let overhead = Int64.max 0L (Int64.sub disk payload) in
        let suffix_overhead = Int64.max 0L (Int64.sub suffix payload) in
        let overhead_each = average overhead changed_epochs in
        let suffix_overhead_each = average suffix_overhead changed_epochs in
        expect "changed growth amplification crossed limit"
          (overhead_each < limit);
        expect "changed suffix amplification crossed limit"
          (suffix_overhead_each < limit);
        Printf.printf
          "event = store_growth mode = changed epochs = %d accounts = %d cipher_bytes = %d payload = %Ld disk = %Ld suffix = %Ld overhead_each = %Ld suffix_overhead_each = %Ld limit = %Ld\n%!"
          changed_epochs changed_accounts cipher_size payload disk suffix
          overhead_each suffix_overhead_each limit);
      let replay_root =
        with_store replay_path (fun store ->
          expect "replay clone head differs"
            (String.equal (head store) seed_root);
          Lwt_main.run
            (write_epoch store Octra_core.Rule_graph.Active ciphers 1);
          let first = epochs / 2 in
          Lwt_main.run
            (write_epochs store Octra_core.Rule_graph.Active ciphers 2 first);
          let stable = head store in
          Lwt_main.run
            (S.begin_epoch_batch ~mode:Octra_core.Rule_graph.Active store);
          Lwt_main.run
            (S.set_account
               store
               (address 0)
               (account ~cipher:(cipher cipher_size 99) 999_999));
          S.abort_epoch_batch store;
          expect "aborted batch changed root" (String.equal (head store) stable);
          stable)
      in
      with_store replay_path (fun store ->
        expect "aborted batch survived reopen"
          (String.equal (head store) replay_root);
        let first = epochs / 2 in
        Lwt_main.run
          (write_epochs
             store
             Octra_core.Rule_graph.Active
             ciphers
             (2 + first)
             (epochs - first));
        expect "replay root differs" (String.equal (head store) expected_root));
      with_store snapshot_path (fun store ->
        expect "snapshot head differs" (String.equal (head store) expected_root);
        expect "snapshot commit differs"
          (String.equal (commit store) expected_commit);
        let packed = Lwt_main.run (S.get_account store (address 0)) in
        expect "snapshot packed account differs"
          (match packed with
           | Some value ->
             Z.equal value.balance (Z.of_int (epochs + 3))
             && value.encrypted_balance = Some ciphers.(0)
           | None -> false);
        let old = Lwt_main.run (S.get_account store (address 1_000)) in
        expect "snapshot old account differs"
          (match old with
           | Some value ->
             Z.equal value.balance Z.one
             && value.encrypted_balance = None
           | None -> false));
      Printf.printf
        "event = store_growth mode = migration epochs = 1 disk = %Ld suffix = %Ld\n%!"
        migration_disk migration_suffix;
      Printf.printf
        "event = store_growth mode = active epochs = %d accounts = %d rewarded = %d cipher_bytes = %d disk = %Ld suffix = %Ld bytes_each = %Ld suffix_each = %Ld limit = %Ld root = %s\n%!"
        epochs accounts (Array.length ciphers) cipher_size disk suffix disk_each
        (average suffix epochs) limit expected_root)

let () = run ()