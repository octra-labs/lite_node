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


type deps = {
  get_epoch_json : int -> string option;
  get_tx_by_txid : int64 -> (string * string) option;
  read_receipts : int -> string list;
  root_to_raw32 : string -> string;
  head_epoch : unit -> int option;
  lookup_bundle : string -> Consensus_bundle_cache.encoded option;
}

type node_readers = {
  get_epoch_json : int -> string option;
  get_tx_by_txid : int64 -> (string * string) option;
  read_receipts_opt : int -> string list option;
  root_to_raw32 : string -> string;
  cached_head : unit -> Octra_core.Head_manifest.t option;
  lookup_bundle : string -> Consensus_bundle_cache.encoded option;
}

type cached_root = {
  root : string;
  eic : string option;
}

type node_root_readers = {
  current_epoch : unit -> int;
  root_to_raw32 : string -> string;
  read_head_hash : unit -> string option Lwt.t;
  cached_head : unit -> Octra_core.Head_manifest.t option;
  get_epoch_json : int -> string option;
}

type node_root_deps = {
  read_local_ledger_root_raw : unit -> string Lwt.t;
  read_local_root_raw : unit -> string Lwt.t;
  committed_head_epoch : unit -> int;
  committed_epoch_root_raw : int -> string option;
  cached_root : unit -> cached_root;
}

let raw_zero = String.make 32 '\x00'

let root_to_raw32_or_zero ~root_to_raw32 root =
  try root_to_raw32 root with _ -> raw_zero

let root_to_raw32_opt ~root_to_raw32 root =
  try Some (root_to_raw32 root) with _ -> None

let ledger_root_or_zero ~root_to_raw32 = function
  | Some root when String.length root > 0 ->
    root_to_raw32_or_zero ~root_to_raw32 root
  | _ ->
    raw_zero

let cached_state_root ~root_to_raw32 = function
  | Some head ->
    let root = head.Octra_core.Head_manifest.state_root in
    if String.length root > 0 then
      Some (root_to_raw32_or_zero ~root_to_raw32 root)
    else
      None
  | None ->
    None

let committed_head_epoch ~current_epoch = function
  | Some head -> head.Octra_core.Head_manifest.epoch_id
  | None -> current_epoch - 1

let epoch_root_from_json ~root_to_raw32 = function
  | None ->
    None
  | Some json ->
    match Octra_core.Epochlog.epoch_of_json json with
    | Some h ->
      root_to_raw32_opt
        ~root_to_raw32
        h.Octra_core.Epochlog.state_root
    | None ->
      None

let cached_root ~root_to_raw32 = function
  | Some head ->
    {
      root =
        root_to_raw32_or_zero
          ~root_to_raw32
          head.Octra_core.Head_manifest.state_root;
      eic = head.Octra_core.Head_manifest.epoch_index_root;
    }
  | None ->
    {
      root = raw_zero;
      eic = None;
    }

let epoch_root (deps : deps) epoch_id =
  epoch_root_from_json
    ~root_to_raw32:deps.root_to_raw32
    (deps.get_epoch_json (Int64.to_int epoch_id))

let local_head_epoch (deps : deps) =
  match deps.head_epoch () with
  | Some epoch -> Int64.of_int epoch
  | None -> -1L

let bundle (deps : deps) proposal_id =
  deps.lookup_bundle proposal_id

let catchup_range (deps : deps) ~from_epoch ~max_epochs =
  let read_deps = Consensus_catchup_read.{
    get_epoch_json = deps.get_epoch_json;
    get_tx_by_txid = deps.get_tx_by_txid;
    read_receipts = deps.read_receipts;
    root_to_raw32 = deps.root_to_raw32;
  } in
  Consensus_catchup_read.range read_deps ~from_epoch ~max_epochs

let node_deps (readers : node_readers) =
  {
    get_epoch_json = readers.get_epoch_json;
    get_tx_by_txid = readers.get_tx_by_txid;
    read_receipts = (fun epoch ->
      match readers.read_receipts_opt epoch with
      | Some receipts -> receipts
      | None -> []);
    root_to_raw32 = readers.root_to_raw32;
    head_epoch = (fun () ->
      match readers.cached_head () with
      | Some h -> Some h.Octra_core.Head_manifest.epoch_id
      | None -> None);
    lookup_bundle = readers.lookup_bundle;
  }

let node_store_deps ~chaindata ~data_dir ~root_to_raw32 ~cached_head
    ~lookup_bundle =
  node_deps
    {
      get_epoch_json = (fun epoch ->
        Octra_core.Chaindata_index.get_epoch
          (Octra_core.Store_chaindata.index chaindata)
          epoch);
      get_tx_by_txid = Octra_core.Store_chaindata.get_tx_by_txid chaindata;
      read_receipts_opt = (fun epoch ->
        Octra_core.Preverify_receipt_store.read data_dir ~epoch_id:epoch);
      root_to_raw32;
      cached_head;
      lookup_bundle;
    }

let node_root_deps readers =
  let read_local_ledger_root_raw () =
    let open Lwt.Syntax in
    let* opt = readers.read_head_hash () in
    Lwt.return
      (ledger_root_or_zero
         ~root_to_raw32:readers.root_to_raw32
         opt)
  in
  let read_local_root_raw () =
    match
      cached_state_root
        ~root_to_raw32:readers.root_to_raw32
        (readers.cached_head ())
    with
    | Some root -> Lwt.return root
    | None -> read_local_ledger_root_raw ()
  in
  {
    read_local_ledger_root_raw;
    read_local_root_raw;
    committed_head_epoch = (fun () ->
      committed_head_epoch
        ~current_epoch:(readers.current_epoch ())
        (readers.cached_head ()));
    committed_epoch_root_raw = (fun epoch_id ->
      epoch_root_from_json
        ~root_to_raw32:readers.root_to_raw32
        (readers.get_epoch_json epoch_id));
    cached_root = (fun () ->
      cached_root
        ~root_to_raw32:readers.root_to_raw32
        (readers.cached_head ()));
  }