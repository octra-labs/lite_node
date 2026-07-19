(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


open Lwt.Syntax

let preview_counter = ref 0

let preview_branch_name ~epoch_id ~proposal_id =
  incr preview_counter;
  let pid_short =
    if String.length proposal_id > 12 then String.sub proposal_id 0 12 else proposal_id in
  Printf.sprintf "preview_%d_%s_%d_%d" epoch_id pid_short (Unix.getpid ()) !preview_counter

let with_preview ~(base_store : Store_irmin.t) ~epoch_id ~proposal_id
    ?(expected_prev_root : string option) f =
  let branch_name = preview_branch_name ~epoch_id ~proposal_id in


  let* head_opt = Store_irmin.Store.Head.find base_store.store in
  match head_opt with
  | None -> Lwt.return (Error "no head commit for preview")
  | Some head_commit ->


  let head_tree = Store_irmin.Store.Commit.tree head_commit in
  let head_hash = Irmin.Type.to_string Store_irmin.Store.Hash.t
    (Store_irmin.Store.Tree.hash head_tree) in
  let race_ok = match expected_prev_root with
    | Some expected ->
      let expected_hex =
        if String.length expected = 32 then
          String.concat "" (List.init 32 (fun i ->
            Printf.sprintf "%02x" (Char.code expected.[i])))
        else expected in
      let head_hash_truncated =
        let n_expected = String.length expected_hex in
        if String.length head_hash >= n_expected
        then String.sub head_hash 0 n_expected
        else head_hash in
      head_hash_truncated = expected_hex
    | None -> true
  in
  if not race_ok then
    Lwt.return (Error (Printf.sprintf "preview race: HEAD moved (head=%s)" (String.sub head_hash 0 16)))
  else


  let* () = Store_irmin.Store.Branch.set base_store.repo branch_name head_commit in


  let* preview_store = Store_irmin.Store.of_branch base_store.repo branch_name in


  let preview_t = {
    Store_irmin.repo = base_store.repo;
    store = preview_store;
    batch_tree = None;
    stealth_counter = ref !(base_store.stealth_counter);
    pvac_dir = base_store.pvac_dir;
  } in


  let preview_ledger = Ledger.create preview_t in


  let backend = Epoch_exec.{
    store = preview_t;
    ledger = preview_ledger;
    ops = Epoch_exec.ledger_ops preview_ledger;
    begin_batch = (fun () -> Store_irmin.begin_epoch_batch preview_t);
    commit_batch = (fun () -> Store_irmin.commit_epoch_batch preview_t "preview");
    flush_dirty = (fun () -> Ledger.flush_dirty_lwt preview_ledger);
    get_head_hash = (fun () -> Store_irmin.get_head_hash preview_t);
    set_meta = (fun k v -> Store_irmin.set_meta preview_t k v);
  } in


  Lwt.finalize
    (fun () -> f backend)
    (fun () ->

      let* () = Store_irmin.Store.Branch.remove base_store.repo branch_name in
      Lwt.return_unit)


let cleanup_stale_previews ~(base_store : Store_irmin.t) =
  let* branches = Store_irmin.Store.Branch.list base_store.repo in
  let preview_branches = List.filter (fun b ->
    String.length b > 8 && String.sub b 0 8 = "preview_"
  ) branches in
  Lwt_list.iter_s (fun b ->
    Store_irmin.Store.Branch.remove base_store.repo b
  ) preview_branches