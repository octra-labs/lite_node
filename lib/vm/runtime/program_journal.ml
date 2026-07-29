(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type deploy = {
  address : string;
  code_hash : string;
  bytecode_b64 : string;
  owner : string;
  ctype : string;
  admission : string;
  storage : (string, string) Hashtbl.t;
}

type upgrade = {
  address : string;
  expected_code_hash : string;
  code_hash : string;
  bytecode_b64 : string;
  owner : string;
  ctype : string;
  admission : string;
  version : string;
}

type snapshot = {
  deploys : deploy list;
  upgrades : upgrade list;
  storage : (string, (string, string) Hashtbl.t) Hashtbl.t;
}

type t = {
  deploys : deploy list ref;
  upgrades : upgrade list ref;
  storage : (string, (string, string) Hashtbl.t) Hashtbl.t;
}

let copy_deploy (deploy : deploy) : deploy =
  { deploy with storage = Hashtbl.copy deploy.storage }

let copy_storage storage =
  let copy = Hashtbl.create (Hashtbl.length storage) in
  Hashtbl.iter
    (fun address values -> Hashtbl.replace copy address (Hashtbl.copy values))
    storage;
  copy

let create () : t =
  {
    deploys = ref [];
    upgrades = ref [];
    storage = Hashtbl.create 16;
  }

let snapshot (journal : t) : snapshot =
  {
    deploys = List.map copy_deploy !(journal.deploys);
    upgrades = !(journal.upgrades);
    storage = copy_storage journal.storage;
  }

let restore_table target source =
  Hashtbl.reset target;
  Hashtbl.iter (Hashtbl.replace target) source

let restore (journal : t) (snapshot : snapshot) =
  journal.deploys := List.map copy_deploy snapshot.deploys;
  journal.upgrades := snapshot.upgrades;
  let addresses =
    Hashtbl.fold (fun address _ items -> address :: items) journal.storage []
  in
  List.iter
    (fun address ->
      match
        Hashtbl.find_opt journal.storage address,
        Hashtbl.find_opt snapshot.storage address
      with
      | Some target, Some source -> restore_table target source
      | Some target, None ->
        Hashtbl.reset target;
        Hashtbl.remove journal.storage address
      | None, _ -> ())
    addresses;
  Hashtbl.iter
    (fun address values ->
      if not (Hashtbl.mem journal.storage address) then
        Hashtbl.replace journal.storage address (Hashtbl.copy values))
    snapshot.storage

let discard (journal : t) =
  journal.deploys := [];
  journal.upgrades := [];
  Hashtbl.reset journal.storage

let add_deploy (journal : t) (deploy : deploy) =
  let deploy = copy_deploy deploy in
  journal.deploys := deploy :: !(journal.deploys);
  Hashtbl.replace journal.storage deploy.address (Hashtbl.copy deploy.storage)

let find_deploy (journal : t) address =
  !(journal.deploys)
  |> List.find_opt (fun (deploy : deploy) -> String.equal deploy.address address)
  |> Option.map copy_deploy

let has_deploy (journal : t) address =
  List.exists
    (fun (deploy : deploy) -> String.equal deploy.address address)
    !(journal.deploys)

let add_upgrade (journal : t) (upgrade : upgrade) =
  journal.upgrades := upgrade :: !(journal.upgrades)

let find_upgrade (journal : t) address =
  !(journal.upgrades)
  |> List.find_opt (fun (upgrade : upgrade) -> String.equal upgrade.address address)

let has_upgrade (journal : t) address =
  Option.is_some (find_upgrade journal address)

let load_storage (journal : t) address =
  Option.map Hashtbl.copy (Hashtbl.find_opt journal.storage address)

let checkout_storage (journal : t) address ~fallback =
  match Hashtbl.find_opt journal.storage address with
  | Some storage -> storage
  | None ->
    let storage = fallback () in
    Hashtbl.replace journal.storage address storage;
    storage

let deploys (journal : t) =
  List.rev_map copy_deploy !(journal.deploys)

let upgrades (journal : t) =
  List.rev !(journal.upgrades)

let storage_entries (journal : t) =
  Hashtbl.fold
    (fun address storage entries ->
      (address, Hashtbl.copy storage) :: entries)
    journal.storage
    []
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)