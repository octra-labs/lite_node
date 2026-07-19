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


type deploy = {
  address : string;
  code_hash : string;
  bytecode_b64 : string;
  owner : string;
  ctype : string;
  storage : (string, string) Hashtbl.t;
}

type snapshot = {
  deploys : deploy list;
  storage : (string, (string, string) Hashtbl.t) Hashtbl.t;
}

type t = {
  deploys : deploy list ref;
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
  { deploys = ref []; storage = Hashtbl.create 16 }

let snapshot (journal : t) : snapshot =
  {
    deploys = List.map copy_deploy !(journal.deploys);
    storage = copy_storage journal.storage;
  }

let restore_table target source =
  Hashtbl.reset target;
  Hashtbl.iter (Hashtbl.replace target) source

let restore (journal : t) (snapshot : snapshot) =
  journal.deploys := List.map copy_deploy snapshot.deploys;
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
  Hashtbl.reset journal.storage

let add_deploy (journal : t) (deploy : deploy) =
  let deploy = copy_deploy deploy in
  journal.deploys := deploy :: !(journal.deploys);
  Hashtbl.replace journal.storage deploy.address (Hashtbl.copy deploy.storage)

let find_deploy (journal : t) address =
  !(journal.deploys)
  |> List.find_opt (fun deploy -> String.equal deploy.address address)
  |> Option.map copy_deploy

let has_deploy (journal : t) address =
  List.exists
    (fun deploy -> String.equal deploy.address address)
    !(journal.deploys)

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

let storage_entries (journal : t) =
  Hashtbl.fold
    (fun address storage entries ->
      (address, Hashtbl.copy storage) :: entries)
    journal.storage
    []
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)