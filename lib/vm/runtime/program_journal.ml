(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type deploy = {
  address : string;
  code_hash : string;
  bytecode_b64 : string;
  owner : string;
  ctype : string;
  admission : string;
  version : string;
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
  circles : (string, (string, string) Hashtbl.t) Hashtbl.t;
}

type t = {
  deploys : deploy list ref;
  upgrades : upgrade list ref;
  storage : (string, (string, string) Hashtbl.t) Hashtbl.t;
  circles : (string, (string, string) Hashtbl.t) Hashtbl.t;
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
    circles = Hashtbl.create 8;
  }

let storage_effort storage =
  let count = Hashtbl.length storage in
  let levels = Z.numbits (Z.of_int count) in
  let keys, values = Hashtbl.fold (fun key value (keys, values) ->
    Z.add keys (Z.of_int (String.length key)),
    Z.add values (Z.of_int (String.length value))) storage (Z.zero, Z.zero) in
  let bytes = Z.add values (Z.mul keys (Z.of_int (1 + levels))) in
  Z.add (Z.cdiv bytes (Z.of_int 16))
    (Z.mul (Z.of_int count) (Z.of_int (16 + levels)))

let snapshot_effort (journal : t) =
  let copy values = Z.of_int (Hashtbl.length values) in
  let tables storage = Hashtbl.fold (fun _ values total ->
    Z.add total (Z.succ (copy values))) storage Z.zero in
  let deploys = List.fold_left (fun total (entry : deploy) ->
    Z.add total (Z.succ (copy entry.storage))) Z.zero !(journal.deploys) in
  Z.mul (Z.of_int 32) Z.(add deploys (add (tables journal.storage) (tables journal.circles)))

let inline_overhead =
  Octra_core.Circles.make_stable_entry "" (Octra_core.Circles.Inline "")
  |> Octra_core.Circles.yojson_of_stable_entry
  |> Yojson.Safe.to_string
  |> String.length

let escaped_extra text =
  String.fold_left (fun total -> function
    | '"' | '\\' | '\b' | '\012' | '\n' | '\r' | '\t' -> total + 1
    | '\x00' .. '\x1f' | '\x7f' -> total + 5
    | _ -> total) 0 text

let write_effort storage =
  let extra = Hashtbl.fold (fun key value total ->
    Z.add total (Z.of_int (inline_overhead + escaped_extra key + escaped_extra value)))
    storage Z.zero in
  Z.add (storage_effort storage) (Z.cdiv extra (Z.of_int 16))

let snapshot (journal : t) : snapshot =
  {
    deploys = List.map copy_deploy !(journal.deploys);
    upgrades = !(journal.upgrades);
    storage = copy_storage journal.storage;
    circles = copy_storage journal.circles;
  }

let restore_table target source =
  Hashtbl.reset target;
  Hashtbl.iter (Hashtbl.replace target) source

let restore_storage storage saved =
  let addresses =
    Hashtbl.fold (fun address _ items -> address :: items) storage []
  in
  List.iter
    (fun address ->
      match
        Hashtbl.find_opt storage address,
        Hashtbl.find_opt saved address
      with
      | Some target, Some source -> restore_table target source
      | Some target, None ->
        Hashtbl.reset target;
        Hashtbl.remove storage address
      | None, _ -> ())
    addresses;
  Hashtbl.iter
    (fun address values ->
      if not (Hashtbl.mem storage address) then
        Hashtbl.replace storage address (Hashtbl.copy values))
    saved

let restore (journal : t) (snapshot : snapshot) =
  journal.deploys := List.map copy_deploy snapshot.deploys;
  journal.upgrades := snapshot.upgrades;
  restore_storage journal.storage snapshot.storage;
  restore_storage journal.circles snapshot.circles

let discard (journal : t) =
  journal.deploys := [];
  journal.upgrades := [];
  Hashtbl.reset journal.storage;
  Hashtbl.reset journal.circles

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

let circle_storage (journal : t) address values =
  match Hashtbl.find_opt journal.circles address with
  | Some storage -> storage
  | None ->
    Hashtbl.replace journal.circles address values;
    values

let find_circle (journal : t) address =
  Hashtbl.find_opt journal.circles address

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