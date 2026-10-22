(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Store = Octra_core.Store_irmin
module Data = Octra_core.Store_chaindata
module Origin = Octra_core.Store_origin

type t = {
  origin : Origin.t;
  chain : string;
  store : Store.t;
  chaindata : Data.t;
  root : string;
  commit : string;
  mutable used : bool;
}

let history_clear data_dir =
  Array.for_all (fun name ->
    let kind = (Unix.lstat (Filename.concat data_dir name)).Unix.st_kind in
    match name with
    | "wallet.json" -> kind = Unix.S_REG || kind = Unix.S_LNK
    | "chaindata" | "irmin_store" | "local_drops" | "compute_history" ->
      kind = Unix.S_DIR
    | _ -> false) (Sys.readdir data_dir)

let empty_chain data =
  not data.Data.closed && not data.index.readonly
  && Data.next_txid data = 0L
  && Data.last_epoch_id data = Ok None
  && Data.history_floor data = Ok None

let state store =
  Lwt_main.run (let open Lwt.Syntax in
    let* root = Store.get_head_hash store in
    let* commit = Store.get_commit_hash store in
    let* epoch = Store.get_meta store "current_epoch" in
    let* previous = Store.get_meta store "last_epoch" in
    Lwt.return (root, commit, epoch, previous))

let finish ~created ~data_dir ~chain ~epoch ~store ~chaindata =
  match chaindata.Data.origin with
  | None -> None
  | Some origin ->
    let claimed = Origin.claim origin ~data_dir in
    if not (claimed && created && epoch = 0 && empty_chain chaindata) then None
    else match state store with
      | Some root, Some commit, Some "0", None when history_clear data_dir ->
        Some {origin; chain; store; chaindata; root; commit; used = false}
      | _ -> None

let consume value ~data_dir ~chain ~epoch ~head =
  let unused = not value.used in
  value.used <- true;
  try
    unused && value.chain = chain
    && epoch = 0 && head = -1 && Origin.active value.origin ~data_dir
    && empty_chain value.chaindata
    && state value.store = (Some value.root, Some value.commit, Some "0", None)
    && history_clear data_dir
  with Unix.Unix_error _ | Sys_error _ -> false