(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module S = Octra_core.Store_irmin
module W = Test_workspace

let count = 16_384

let payload index =
  let prefix = Printf.sprintf "%08d" index in
  prefix ^ String.make (8192 - String.length prefix) 'x'

let key index = ["samples"; string_of_int index]

let populate path =
  let* store = S.open_store ~fresh:true path in
  Lwt.finalize (fun () ->
    let* () = S.begin_epoch_batch store in
    let rec loop index =
      if index = count then S.commit_epoch_batch store "heap sample"
      else
        let* () = S.write store (key index) (payload index) in
        loop (index + 1)
    in
    loop 0
  ) (fun () -> S.close store)

let inspect store read =
  let started = Unix.gettimeofday () in
  let rec loop index =
    if index = count then Lwt.return_unit
    else
      let* value = read (key index) in
      if value <> Some (payload index) then failwith "store value differs";
      loop (index + 1)
  in
  let* () = loop 0 in
  let* head = S.Store.Head.get store in
  Gc.full_major ();
  let stats = Gc.stat () in
  ignore (Sys.opaque_identity store);
  Lwt.return (Irmin.Type.to_string S.Store.Hash.t (S.Store.Commit.hash head),
    stats.live_words * (Sys.word_size / 8),
    Unix.gettimeofday () -. started)

let measure path limited =
  Gc.full_major ();
  if limited then
    let* store = S.open_store ~readonly:true path in
    Lwt.finalize (fun () -> inspect store.store (S.read store)) (fun () -> S.close store)
  else
    let config = Irmin_pack.Conf.init ~readonly:true ~lru_size:100_000
      ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal path in
    let* repo = S.Store.Repo.v config in
    Lwt.finalize (fun () ->
      let* store = S.Store.main repo in
      inspect store (S.Store.find store)
    ) (fun () -> S.Store.Repo.close repo)

let check_reads path =
  let* store = S.open_store path in
  let check read expected =
    let* value = read ["check"] in
    if value <> expected then failwith "cached read differs";
    Lwt.return_unit
  in
  Lwt.finalize (fun () ->
    let* () = S.begin_epoch_batch store in
    let* () = S.write store ["check"] "first" in
    let* () = check (S.read store) (Some "first") in
    let* () = check (S.read store) (Some "first") in
    let save = match S.save_batch store with
      | Ok save -> save
      | Error error -> failwith error in
    let* () = S.write store ["check"] "second" in
    let* () = check (S.read store) (Some "second") in
    (match S.restore_batch store save with
     | Ok () -> ()
     | Error error -> failwith error);
    let* () = check (S.read store) (Some "first") in
    let* () = S.commit_epoch_batch store "first" in
    let* tree = S.Store.tree store.store in
    let* () = S.begin_epoch_batch store in
    let* () = S.remove_path store ["check"] in
    let* () = check (S.read store) None in
    let* () = check (S.read_value tree) (Some "first") in
    let* () = S.commit_epoch_batch store "removed" in
    let* () = check (S.read store) None in
    check (S.read_value tree) (Some "first")
  ) (fun () -> S.close store)

let read_measure path limited =
  let process = Unix.open_process_args_in Sys.executable_name
    [|Sys.executable_name; "measure"; path; string_of_bool limited|] in
  let value : string * int * float = Marshal.from_channel process in
  match Unix.close_process_in process with
  | Unix.WEXITED 0 -> value
  | _ -> failwith "store measure failed"

let rec remove path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | stat when stat.Unix.st_kind = Unix.S_DIR ->
    Sys.readdir path
    |> Array.iter (fun name -> remove (Filename.concat path name));
    Unix.rmdir path
  | _ -> Unix.unlink path

let run () =
  let path = W.unique_dir "store-heap" in
  Fun.protect ~finally:(fun () -> remove path) (fun () ->
    Lwt_main.run (populate path);
    let before_root, before, before_s = read_measure path false in
    let after_root, after, after_s = read_measure path true in
    if before_root <> after_root then failwith "store root differs";
    Printf.printf
      "event = store_heap entries = %d before = %d after = %d before_s = %.3f after_s = %.3f\n%!"
      count before after before_s after_s;
    if after + 16_777_216 >= before then failwith "store cache did not release values";
    Lwt_main.run (check_reads path))

let () =
  match Array.to_list Sys.argv with
  | [_; "measure"; path; limited] ->
    let value = Lwt_main.run (measure path (bool_of_string limited)) in
    Marshal.to_channel stdout value [];
    flush stdout
  | [_] -> run ()
  | _ -> failwith "invalid store measure arguments"