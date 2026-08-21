(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

let root_hex value =
  if String.length value = 32 then
    String.concat "" (List.init 32 (fun i ->
      Printf.sprintf "%02x" (Char.code value.[i])))
  else
    value

let root_matches expected actual =
  let expected = root_hex expected in
  let n = String.length expected in
  String.length actual >= n
  && String.equal expected (String.sub actual 0 n)

let view base store =
  {
    Store_irmin.repo = base.Store_irmin.repo;
    store;
    batch_tree = None;
    stealth_counter = ref !(base.stealth_counter);
    tags = base.tags;
    tag_lock = Lwt_mutex.create ();
    pvac_dir = base.pvac_dir;
    state_root_file = base.state_root_file;
  }

let with_state ~(base_store : Store_irmin.t) ?base_ledger ~epoch_id:_ ~proposal_id:_
    ?(expected_prev_root : string option) f =
  let* head = Store_irmin.Store.Head.find base_store.store in
  match head with
  | None -> Lwt.return_error "preview_head_missing"
  | Some head ->
    let tree = Store_irmin.Store.Commit.tree head in
    let root =
      Store_irmin.Store.Tree.hash tree
      |> Irmin.Type.to_string Store_irmin.Store.Hash.t
    in
    let matches =
      match expected_prev_root with
      | Some expected -> root_matches expected root
      | None -> true
    in
    if not matches then
      Lwt.return_error "preview_state_changed"
    else
      let snap =
        match base_ledger with
        | Some ledger -> Result.map Option.some (Ledger.freeze ledger)
        | None -> Ok None
      in
      match snap with
      | Error error -> Lwt.return_error error
      | Ok snap ->
        let base =
          Store_irmin.Store.Commit.hash head
          |> Irmin.Type.to_string Store_irmin.Store.Hash.t
        in
        let* store = Store_irmin.Store.of_commit head in
        let store = view base_store store in
        let ledger =
          match snap with
          | Some snap -> Ledger.thaw store snap
          | None -> Ledger.create store
        in
        let* result = f store ledger in
        let* live = Store_irmin.Store.Head.find base_store.store in
        let unchanged =
          match live with
          | Some live ->
            Store_irmin.Store.Commit.hash live
            |> Irmin.Type.to_string Store_irmin.Store.Hash.t
            |> String.equal base
          | None -> false
        in
        if unchanged then Lwt.return result
        else Lwt.return_error "preview_state_changed"

let with_preview ~(base_store : Store_irmin.t) ?base_ledger ~epoch_id ~proposal_id
    ?(expected_prev_root : string option) f =
  with_state
    ~base_store
    ?base_ledger
    ~epoch_id
    ~proposal_id
    ?expected_prev_root
    (fun store ledger ->
      let backend = Epoch_exec.{
        store;
        ledger;
        ops = Epoch_exec.ledger_ops ledger;
        emission_policy = Emission_policy.of_env Sys.getenv_opt;
        emission_schedule = Emission_schedule.of_env_exn Sys.getenv_opt;
        legacy_total_supply = Emission_policy.legacy_total Sys.getenv_opt;
        sender_key_activation_epoch =
          Sender_key_policy.activation_epoch_exn Sys.getenv_opt;
        validator_policy = Validator_policy.of_env_exn Sys.getenv_opt;
        fold = Epoch_exec.prior_fold;
        begin_batch = (fun () -> Store_irmin.begin_epoch_batch store);
        commit_batch = (fun () -> Store_irmin.commit_epoch_batch store "preview");
        flush_dirty = (fun () -> Ledger.flush_dirty_lwt ledger);
        get_head_hash = (fun () -> Store_irmin.get_head_hash store);
        set_meta = (fun key value -> Store_irmin.set_meta store key value);
      } in
      f backend)

let cleanup_stale_previews ~(base_store : Store_irmin.t) =
  let* branches = Store_irmin.Store.Branch.list base_store.repo in
  let preview_branches = List.filter (fun b ->
    String.length b > 8 && String.sub b 0 8 = "preview_"
  ) branches in
  Lwt_list.iter_s (fun b ->
    Store_irmin.Store.Branch.remove base_store.repo b
  ) preview_branches