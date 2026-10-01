(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Case = Recovery_case
module Store = Case.SI

let snapshot path =
  let store = Lwt_main.run (Store.open_store ~readonly:true path) in
  Fun.protect ~finally:(fun () -> Lwt_main.run (Store.close store)) (fun () ->
    let names = Lwt_main.run (Store.Store.Branch.list store.Store.repo) |> List.sort String.compare in
    let branches = List.map (fun name ->
      let value = Lwt_main.run (Store.Store.Branch.find store.repo name) in
      name, Option.map (fun commit ->
        Irmin.Type.to_string Store.Store.Hash.t (Store.Store.Commit.hash commit)) value) names in
    branches, Lwt_main.run (Store.get_commit_hash store))

let setup root =
  let dir = Filename.concat root "split_open" in
  Unix.mkdir dir 0o700;
  ignore (Case.prepare dir);
  Case.with_stores dir (fun _ store ->
    let prior = Option.get (Lwt_main.run (Store.Store.Head.find store.Store.store)) in
    Lwt_main.run (Store.set_meta store "detached" "branch");
    let other = Option.get (Lwt_main.run (Store.Store.Head.find store.store)) in
    Lwt_main.run (Store.Store.Branch.set store.repo "pack_split_1" other);
    Lwt_main.run (Store.Store.Head.set store.store prior);
    Store.Store.flush store.repo;
    Store.sync_branches store.store_path);
  Filename.concat dir "irmin_store"

let run root =
  let path = setup root in
  let before = snapshot path in
  let store = Lwt_main.run (Store.open_store path) in
  Lwt_main.run (Store.close store);
  let after = snapshot path in
  let prior, prior_head = before and current, current_head = after in
  Printf.printf "event = store_open branches_before = %d branches_after = %d head_same = %b\n%!"
    (List.length prior) (List.length current) (prior_head = current_head);
  List.iter (fun (name, value) ->
    if List.assoc_opt name current <> Some value then
      Printf.printf "event = branch_changed name = %s\n%!" name) prior;
  Case.expect "opening the store changed saved branches" (before = after)

let () = Test_workspace.with_dir "store_open" run