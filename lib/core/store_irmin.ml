(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module Conf = struct
  let entries = 32
  let stable_hash = 256
  let contents_length_header = Some `Varint
  let inode_child_order = `Hash_bits
  let forbid_empty_dir_persistence = false
end

module KV = Irmin_pack_unix.KV(Conf)
module Store = KV.Make(Irmin.Contents.String)

type t = {
  repo : Store.repo;
  mutable store : Store.t;
  mutable batch_tree : Store.tree option;
  stealth_counter : int64 ref;
  pvac_dir : string;
  state_root_file : string;
}

type batch_savepoint = {
  tree : Store.tree;
  stealth_counter : int64;
}

let rec strip_trailing_slash path =
  let n = String.length path in
  if n > 1 && path.[n - 1] = Filename.dir_sep.[0] then
    strip_trailing_slash (String.sub path 0 (n - 1))
  else
    path

let absolute_path path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let pvac_dir_of_store_path path =
  let path = strip_trailing_slash path in
  if String.equal (Filename.basename path) "irmin_store" then
    Filename.concat (Filename.dirname path) "pvac"
  else
    path ^ "_pvac"

let state_root_file_of_store_path path =
  path
  |> strip_trailing_slash
  |> Filename.dirname
  |> fun base -> Filename.concat base "state_root"

let make_info msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  fun () -> Store.Info.v ~author:"octra" ~message:msg date

let open_store ?(fresh=false) ?(readonly=false) path =
  let path = absolute_path path in
  let config = Irmin_pack.Conf.init
    ~fresh
    ~readonly
    ~lru_size:100_000
    ~index_log_size:2_500_000
    ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal
    ~use_fsync:true
    path
  in
  let* repo = Store.Repo.v config in
  let* store = Store.main repo in
  let counter = ref 0L in
  let* v = Store.find store ["index"; "stealth_counter"] in
  (match v with
   | Some s ->
     (try counter := Int64.of_string s
      with e ->

        let msg = Printf.sprintf "FATAL: stealth_counter corrupt value=%S: %s"
          s (Printexc.to_string e) in
        Octra_log.fatal "irmin"
          "event = stealth_counter_corrupt value = %S error = %s"
          s (Printexc.to_string e);
        failwith msg)
   | None -> ());
  Lwt.return {
    repo;
    store;
    batch_tree = None;
    stealth_counter = counter;
    pvac_dir = pvac_dir_of_store_path path;
    state_root_file = state_root_file_of_store_path path;
  }

let close t =
  Store.Repo.close t.repo

exception Irmin_write_failed of string

let write t path value =
  match t.batch_tree with
  | Some tree ->
    let* tree' = Store.Tree.add tree path value in
    t.batch_tree <- Some tree';
    Lwt.return_unit
  | None ->
    let info = make_info "w" in
    let* result = Store.set t.store ~info path value in
    (match result with
     | Ok () -> Lwt.return_unit
     | Error e ->
       let msg = Printf.sprintf "Irmin write failed path=%s: %s"
         (String.concat "/" path)
         (Fmt.to_to_string (Irmin.Type.pp_json Store.write_error_t) e) in
       Octra_log.fatal "irmin" "event = write_failed path = %s error = %s"
         (String.concat "/" path)
         (Fmt.to_to_string (Irmin.Type.pp_json Store.write_error_t) e);
       Lwt.fail (Irmin_write_failed msg))

let read t path =
  match t.batch_tree with
  | Some tree -> Store.Tree.find tree path
  | None -> Store.find t.store path

let read_tree t path =
  match t.batch_tree with
  | Some tree -> Store.Tree.find_tree tree path
  | None -> Store.find_tree t.store path

exception Irmin_remove_failed of string

let remove_path t path =
  match t.batch_tree with
  | Some tree ->
    let* tree' = Store.Tree.remove tree path in
    t.batch_tree <- Some tree';
    Lwt.return_unit
  | None ->
    let info = make_info "rm" in
    let* result = Store.remove t.store ~info path in
    (match result with
     | Ok () -> Lwt.return_unit
     | Error e ->
       let errmsg = Printf.sprintf "Irmin remove failed path=%s: %s"
         (String.concat "/" path)
         (Fmt.to_to_string (Irmin.Type.pp_json Store.write_error_t) e) in
       Octra_log.fatal "irmin" "event = remove_failed path = %s error = %s"
         (String.concat "/" path)
         (Fmt.to_to_string (Irmin.Type.pp_json Store.write_error_t) e);
       Lwt.fail (Irmin_remove_failed errmsg))

let begin_epoch_batch t =
  let* tree = Store.get_tree t.store [] in
  t.batch_tree <- Some tree;
  Lwt.return_unit

let commit_epoch_batch t msg =
  match t.batch_tree with
  | None -> Lwt.return_unit
  | Some tree ->
    let info = make_info msg in
    let* result = Store.set_tree t.store ~info [] tree in
    (match result with
     | Ok () ->
       t.batch_tree <- None;
       Lwt.return_unit
     | Error e ->
       let errmsg = Printf.sprintf "Irmin commit_epoch_batch failed msg=%s: %s" msg
         (Fmt.to_to_string (Irmin.Type.pp_json Store.write_error_t) e) in
       Octra_log.fatal "irmin"
         "event = epoch_commit_failed message = %s error = %s"
         msg
         (Fmt.to_to_string (Irmin.Type.pp_json Store.write_error_t) e);

       Lwt.fail (Irmin_write_failed errmsg))

let abort_epoch_batch t =
  t.batch_tree <- None

let save_batch t =
  match t.batch_tree with
  | Some tree ->
    Ok {
      tree;
      stealth_counter = !(t.stealth_counter);
    }
  | None -> Error "store batch is not active"

let restore_batch t savepoint =
  match t.batch_tree with
  | Some _ ->
    t.batch_tree <- Some savepoint.tree;
    t.stealth_counter := savepoint.stealth_counter;
    Ok ()
  | None -> Error "store batch is not active"

let json_of_account (a : Ledger_types.account) =
  Yojson.Safe.to_string (Ledger_types.account_to_yojson a)

let account_of_json s =
  match Yojson.Safe.from_string s with
  | json -> (match Ledger_types.account_of_yojson json with Ok a -> Some a | Error _ -> None)
  | exception _ -> None

let get_account t addr =
  let* v = read t ["accounts"; addr; "data"] in
  match v with
  | Some s -> Lwt.return (account_of_json s)
  | None -> Lwt.return_none

type account_merkle_proof = {
  ledger_state_root : string;
  proof : string;
  value : string option;
}

let proof_path addr =
  ["accounts"; addr; "data"]

let proof_bin proof =
  let enc = Irmin.Type.unstage (Irmin.Type.to_bin_string Store.Tree.Proof.t) in
  enc proof

let proof_of_bin raw =
  let dec = Irmin.Type.unstage (Irmin.Type.of_bin_string Store.Tree.Proof.t) in
  dec raw

let proof_root = function
  | `Node h -> Some (Irmin.Type.to_string Store.Hash.t h)
  | `Contents _ -> None

let head_tree_key t =
  let* head = Store.Head.find t.store in
  match head with
  | None -> Lwt.return_none
  | Some commit ->
    let tree = Store.Commit.tree commit in
    match Store.Tree.key tree with
    | Some key -> Lwt.return_some (tree, key)
    | None ->
      let* key_opt = Store.Tree.find_key t.repo tree in
      Lwt.return (Option.map (fun key -> tree, key) key_opt)

let account_merkle_proof t addr =
  let* key_opt = head_tree_key t in
  match key_opt with
  | None -> Lwt.return_error "missing store head"
  | Some (tree, key) ->
    let ledger_state_root = Irmin.Type.to_string Store.Hash.t (Store.Tree.hash tree) in
    let path = proof_path addr in
    let* proof, value =
      Store.Tree.produce_proof t.repo key (fun tree ->
        let* value = Store.Tree.find tree path in
        Lwt.return (tree, value)) in
    Lwt.return_ok {
      ledger_state_root;
      proof = Base64.encode_exn (proof_bin proof);
      value;
    }

let verify_account_merkle_proof_lwt ~ledger_state_root ~addr ~proof =
  match Base64.decode proof with
  | Error (`Msg e) -> Lwt.return_error ("invalid proof base64: " ^ e)
  | Ok raw ->
    match proof_of_bin raw with
    | Error (`Msg e) -> Lwt.return_error ("invalid proof encoding: " ^ e)
    | Ok proof ->
      match proof_root (Store.Tree.Proof.before proof), proof_root (Store.Tree.Proof.after proof) with
      | Some before_root, Some after_root
        when before_root = ledger_state_root && after_root = ledger_state_root ->
        let path = proof_path addr in
        let* result =
          Store.Tree.verify_proof proof (fun tree ->
            let* value = Store.Tree.find tree path in
            Lwt.return (tree, value)) in
        begin match result with
        | Error (`Proof_mismatch e) -> Lwt.return_error ("proof mismatch: " ^ e)
        | Ok (_, value) -> Lwt.return_ok value
        end
      | Some before_root, _ -> Lwt.return_error ("proof root mismatch: " ^ before_root)
      | None, _ -> Lwt.return_error "proof root is not a node"

let verify_account_merkle_proof ~ledger_state_root ~addr ~proof =
  Lwt_main.run (verify_account_merkle_proof_lwt ~ledger_state_root ~addr ~proof)

let load_all_accounts t =
  let result = ref [] in
  let* tree_opt = read_tree t ["accounts"] in
  let* () = match tree_opt with
   | None -> Lwt.return ()
   | Some tree ->
     Store.Tree.fold
       ~depth:(`Eq 2)
       ~contents:(fun path value () ->
         let leaf = List.nth_opt path (List.length path - 1) in
         if leaf = Some "data" then begin
           let addr = (match path with a :: _ -> a | _ -> "") in
           if addr <> "" then
             match account_of_json value with
             | Some acc -> result := (addr, acc) :: !result
             | None -> ()
         end;
         Lwt.return_unit)
       tree () in
  Lwt.return !result

let set_account t addr (a : Ledger_types.account) =
  write t ["accounts"; addr; "data"] (json_of_account a)

let sum_balances t =
  let sum = ref Z.zero in
  let* tree_opt = read_tree t ["accounts"] in
  match tree_opt with
  | None -> Lwt.return Z.zero
  | Some tree ->
    let* () = Store.Tree.fold
      ~depth:(`Eq 2)
      ~contents:(fun path value () ->
        let leaf = List.nth_opt path (List.length path - 1) in
        if leaf = Some "data" then begin
          match account_of_json value with
          | Some a -> sum := Z.add !sum a.Ledger_types.balance
          | None -> ()
        end;
        Lwt.return_unit)
      tree () in
    Lwt.return !sum

let get_encrypted_balance t addr =
  let* v = read t ["accounts"; addr; "data"] in
  match v with
  | Some s ->
    (match account_of_json s with
     | Some a -> Lwt.return (Option.value ~default:"0" a.Ledger_types.encrypted_balance)
     | None -> Lwt.return "0")
  | None -> Lwt.return "0"

let set_encrypted_balance t addr cipher =
  let* acc = get_account t addr in
  let a = match acc with Some x -> x | None -> Ledger_types.empty_account in
  set_account t addr { a with encrypted_balance = Some cipher }

let get_decrypt_allowance t addr =
  let* acc = get_account t addr in
  match acc with
  | Some a -> Lwt.return a.Ledger_types.decrypt_allowance
  | None -> Lwt.return Z.zero

let set_decrypt_allowance t addr v =
  let* acc = get_account t addr in
  let a = match acc with Some x -> x | None -> Ledger_types.empty_account in
  set_account t addr { a with decrypt_allowance = v }

let ensure_pvac_dir t =
  try Unix.mkdir t.pvac_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let ensure_dir path =
  try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let pvac_hash blob =
  Digestif.SHA256.digest_string blob
  |> Digestif.SHA256.to_hex

let pvac_hash_path addr =
  ["pvac_hashes"; addr]

let pvac_legacy_path t addr =
  Filename.concat t.pvac_dir (addr ^ ".pk")

let pvac_blob_dir t =
  Filename.concat t.pvac_dir "blobs"

let pvac_blob_path t hash =
  Filename.concat (pvac_blob_dir t) (hash ^ ".pk")

let read_file path =
  if Sys.file_exists path then
    try
      let ic = open_in_bin path in
      let n = in_channel_length ic in
      let raw = Bytes.create n in
      really_input ic raw 0 n;
      close_in ic;
      Some (Bytes.to_string raw)
    with _ -> None
  else
    None

let pvac_write_id = ref 0

let fsync_dir path =
  let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () -> Unix.fsync fd)

let write_blob t hash blob =
  ensure_pvac_dir t;
  let dir = pvac_blob_dir t in
  ensure_dir dir;
  let path = pvac_blob_path t hash in
  match read_file path with
  | Some current when String.equal (pvac_hash current) hash -> ()
  | Some _ -> failwith "pvac blob hash collision"
  | None ->
    incr pvac_write_id;
    let tmp =
      Printf.sprintf "%s.%d.%d.tmp" path (Unix.getpid ()) !pvac_write_id
    in
    let cleanup () =
      try Unix.unlink tmp with Unix.Unix_error _ -> ()
    in
    try
      let oc = open_out_bin tmp in
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () ->
          output_string oc blob;
          flush oc;
          Unix.fsync (Unix.descr_of_out_channel oc));
      Unix.rename tmp path;
      fsync_dir dir;
      fsync_dir t.pvac_dir;
      fsync_dir (Filename.dirname t.pvac_dir)
    with e ->
      cleanup ();
      raise e

let get_pvac_hash t addr =
  let* value = read t (pvac_hash_path addr) in
  match value with
  | Some "none"
  | None -> Lwt.return_none
  | Some hash -> Lwt.return_some hash

let pvac_is_bound t addr =
  let* value = read t (pvac_hash_path addr) in
  match value with
  | Some hash when hash <> "none" -> Lwt.return true
  | Some _
  | None -> Lwt.return false

let get_pvac_pubkey t addr =
  let* bound = read t (pvac_hash_path addr) in
  match bound with
  | Some "none" -> Lwt.return_none
  | Some hash ->
    begin
      match read_file (pvac_blob_path t hash) with
      | Some blob when String.equal (pvac_hash blob) hash ->
        Lwt.return_some blob
      | Some _ ->
        Lwt.fail_with "pvac blob hash mismatch"
      | None ->
        Lwt.fail_with "pvac blob unavailable"
    end
  | None ->
    Lwt.return (read_file (pvac_legacy_path t addr))

let set_pvac_pubkey t addr pk_blob =
  let hash = pvac_hash pk_blob in
  write_blob t hash pk_blob;
  write t (pvac_hash_path addr) hash

let kat_path t addr = Filename.concat t.pvac_dir (addr ^ ".kat")

let set_pvac_kat t addr kat_hex =
  ensure_pvac_dir t;
  let p = kat_path t addr in
  let oc = open_out p in
  output_string oc kat_hex;
  close_out oc

let get_pvac_kat t addr =
  let p = kat_path t addr in
  if Sys.file_exists p then
    try
      let ic = open_in p in
      let s = input_line ic in
      close_in ic;
      Some s
    with _ -> None
  else
    None

let delete_pvac_pubkey t addr =
  write t (pvac_hash_path addr) "none"

let get_circle_balance t addr =
  let* v = read t ["accounts"; addr; "circle_balance"] in
  Lwt.return (Option.value ~default:"0" v)

let set_circle_balance t addr cipher =
  write t ["accounts"; addr; "circle_balance"] cipher

let list_epoch_ids t =
  let* tree_opt = read_tree t ["epochs"] in
  match tree_opt with
  | None -> Lwt.return []
  | Some tree ->
    let ids = ref [] in
    let* () = Store.Tree.fold
      ~depth:(`Eq 1)
      ~contents:(fun path _value () ->
        (match path with
         | [id_str] -> (try ids := int_of_string id_str :: !ids with _ -> ())
         | _ -> ());
        Lwt.return_unit)
      tree () in
    Lwt.return (List.sort (fun a b -> compare b a) !ids)

let next_stealth_id t =
  let* stored = read t ["index"; "stealth_counter"] in
  let id =
    match stored with
    | Some value -> Int64.of_string value
    | None -> 0L
  in
  let next = Int64.succ id in
  t.stealth_counter := next;
  let* () = write t ["index"; "stealth_counter"] (Int64.to_string next) in
  Lwt.return id

let insert_stealth_output t ~stealth_tag ~eph_pub ~enc_amount ~amount
    ~epoch_id ~tx_hash ~sender_addr ~claim_pub ~delta_cipher_stored
    ~amount_hash ~amount_commitment =
  let* id = next_stealth_id t in
  let so_json = Yojson.Safe.to_string (`Assoc [
    "id", `Intlit (Int64.to_string id);
    "stealth_tag", `String stealth_tag;
    "eph_pub", `String eph_pub;
    "enc_amount", `String enc_amount;
    "amount", `String amount;
    "epoch_id", `Int epoch_id;
    "tx_hash", `String tx_hash;
    "sender_addr", `String sender_addr;
    "claimed", `Int 0;
    "claim_pub", `String claim_pub;
    "delta_cipher_stored", `String delta_cipher_stored;
    "amount_hash", `String amount_hash;
    "amount_commitment", `String amount_commitment;
  ]) in
  let* () = write t ["stealth"; Int64.to_string id] so_json in
  Lwt.return (Ok id)

let stealth_output_of_string ~fallback_id s =
  try
    let j = Yojson.Safe.from_string s in
    let open Yojson.Safe.Util in
    Some Ledger_types.{
      id = (try j |> member "id" |> to_int with _ -> fallback_id);
      stealth_tag = j |> member "stealth_tag" |> to_string;
      eph_pub = j |> member "eph_pub" |> to_string;
      enc_amount = j |> member "enc_amount" |> to_string;
      amount = j |> member "amount" |> to_string;
      epoch_id = j |> member "epoch_id" |> to_int;
      tx_hash = j |> member "tx_hash" |> to_string;
      sender_addr = j |> member "sender_addr" |> to_string;
      claimed = j |> member "claimed" |> to_int;
      claim_pub = (try j |> member "claim_pub" |> to_string with _ -> "");
      delta_cipher_stored = (try j |> member "delta_cipher_stored" |> to_string with _ -> "");
      amount_hash = (try j |> member "amount_hash" |> to_string with _ -> "");
      amount_commitment = (try j |> member "amount_commitment" |> to_string with _ -> "");
    }
  with _ ->
    None

let get_stealth_output_by_id t output_id =
  let* v = read t ["stealth"; string_of_int output_id] in
  match v with
  | None -> Lwt.return_none
  | Some s -> Lwt.return (stealth_output_of_string ~fallback_id:output_id s)

type stealth_page = {
  outputs : Ledger_types.stealth_output list;
  next_before_id : int64 option;
  has_more : bool;
  scanned : int;
}

let get_stealth_outputs_page t ~from_epoch ~before_id ~limit =
  let limit = max 1 (min limit 256) in
  let scan_limit = max 256 (min 2048 (limit * 8)) in
  let* tree = Store.get_tree t.store [] in
  let* counter = Store.Tree.find tree ["index"; "stealth_counter"] in
  let latest =
    match counter with
    | Some value -> Int64.of_string value
    | None -> 0L
  in
  let start =
    Option.value ~default:latest before_id
    |> Int64.max 0L
    |> Int64.min latest
  in
  let page cursor outputs scanned has_more =
    let next_before_id = if has_more then Some cursor else None in
    Lwt.return {outputs; next_before_id; has_more; scanned}
  in
  let rec scan cursor outputs found scanned =
    if Int64.compare cursor 0L <= 0 then
      page cursor outputs scanned false
    else if found >= limit || scanned >= scan_limit then
      page cursor outputs scanned true
    else
      let id = Int64.pred cursor in
      let* raw = Store.Tree.find tree ["stealth"; Int64.to_string id] in
      let scanned = scanned + 1 in
      let* () =
        if scanned mod 32 = 0 then Lwt.pause () else Lwt.return_unit
      in
      match raw with
      | None ->
        scan id outputs found scanned
      | Some raw ->
        match stealth_output_of_string ~fallback_id:(Int64.to_int id) raw with
        | None ->
          scan id outputs found scanned
        | Some output when output.Ledger_types.epoch_id < from_epoch ->
          page id outputs scanned false
        | Some output ->
          scan id (output :: outputs) (found + 1) scanned
  in
  scan start [] 0 0

let get_stealth_outputs_by_ids t ids =
  let* tree = Store.get_tree t.store [] in
  let rec read_ids outputs = function
    | [] -> Lwt.return (List.rev outputs)
    | id :: rest ->
      let* raw = Store.Tree.find tree ["stealth"; string_of_int id] in
      let outputs =
        match raw with
        | Some value ->
          begin
            match stealth_output_of_string ~fallback_id:id value with
            | Some output -> output :: outputs
            | None -> outputs
          end
        | None ->
          outputs
      in
      read_ids outputs rest
  in
  read_ids [] ids

let get_stealth_outputs_since t from_epoch =
  let results = ref [] in
  let* tree_opt = read_tree t ["stealth"] in
  let* () = match tree_opt with
   | None -> Lwt.return ()
   | Some tree ->
     Store.Tree.fold
       ~depth:(`Eq 1)
       ~contents:(fun _path value () ->
         (try
            let j = Yojson.Safe.from_string value in
            let eid = Yojson.Safe.Util.(j |> member "epoch_id" |> to_int) in
            if eid >= from_epoch then begin
              let claim_pub_val = try Yojson.Safe.Util.(`String (j |> member "claim_pub" |> to_string))
                with _ -> `Null in
              let m s = Yojson.Safe.Util.(j |> member s |> to_string) in
              let row = `Assoc [
                "id", (try Yojson.Safe.Util.(j |> member "id") with _ -> `Int 0);
                "stealth_tag", `String (m "stealth_tag");
                "eph_pub", `String (m "eph_pub");
                "enc_amount", `String (m "enc_amount");
                "epoch_id", `Int eid;
                "tx_hash", `String (m "tx_hash");
                "sender_addr", `String (m "sender_addr");
                "claimed", (try Yojson.Safe.Util.(j |> member "claimed") with _ -> `Int 0);
                "claim_pub", claim_pub_val;
                "delta_cipher_stored", `String (try m "delta_cipher_stored" with _ -> "");
                "amount_hash", `String (try m "amount_hash" with _ -> "");
                "amount_commitment", `String (try m "amount_commitment" with _ -> "");
              ] in
              results := row :: !results
            end
          with _ -> ());
         Lwt.return_unit)
       tree () in
  Lwt.return (List.rev !results)

let get_claimed_stealth_claim_tx_hashes t =
  let results = ref [] in
  let* tree_opt = read_tree t ["stealth"] in
  let* () = match tree_opt with
   | None -> Lwt.return ()
   | Some tree ->
     Store.Tree.fold
       ~depth:(`Eq 1)
       ~contents:(fun _path value () ->
         (try
            let j = Yojson.Safe.from_string value in
            let open Yojson.Safe.Util in
            let claimed = try j |> member "claimed" |> to_int with _ -> 0 in
            let claim_tx_hash = try j |> member "claim_tx_hash" |> to_string with _ -> "" in
            if claimed <> 0 && String.length claim_tx_hash = 64 then
              results := claim_tx_hash :: !results
          with _ -> ());
         Lwt.return_unit)
       tree () in
  Lwt.return (List.rev !results)

let mark_stealth_claimed t output_id claim_tx_hash =
  let key = ["stealth"; string_of_int output_id] in
  let* v = read t key in
  match v with
  | None -> Lwt.return (Error "output not found")
  | Some s ->
    (try
       let j = Yojson.Safe.from_string s in
       let open Yojson.Safe.Util in
       let claimed = j |> member "claimed" |> to_int in
       if claimed <> 0 then
         Lwt.return (Error "output already claimed or not found")
       else begin
         let j' = match j with
           | `Assoc fields ->
             `Assoc (List.map (fun (k, v) ->
               if k = "claimed" then (k, `Int 1)
               else if k = "claim_tx_hash" then (k, `String claim_tx_hash)
               else (k, v)) fields
             |> (fun fs ->
               if List.exists (fun (k, _) -> k = "claim_tx_hash") fs then fs
               else fs @ ["claim_tx_hash", `String claim_tx_hash]))
           | _ -> j in
         let* () = write t key (Yojson.Safe.to_string j') in
         Lwt.return (Ok ())
       end
     with _ -> Lwt.return (Error "parse error"))

let get_unclaimed_stealth_amount t =
  let sum = ref Z.zero in
  let* tree_opt = read_tree t ["stealth"] in
  let* () = match tree_opt with
   | None -> Lwt.return ()
   | Some tree ->
     Store.Tree.fold
       ~depth:(`Eq 1)
       ~contents:(fun _path value () ->
         (try
            let j = Yojson.Safe.from_string value in
            let claimed = Yojson.Safe.Util.(j |> member "claimed" |> to_int) in
            if claimed = 0 then begin
              let amt = Yojson.Safe.Util.(j |> member "amount" |> to_string) |> Z.of_string in
              sum := Z.add !sum amt
            end
          with _ -> ());
         Lwt.return_unit)
       tree () in
  Lwt.return !sum

let load_bytecode t addr =
  read t ["contracts"; addr; "bytecode"]

let load_contract_storage t addr =
  let tbl = Hashtbl.create 100 in
  let* tree_opt = read_tree t ["contracts"; addr; "storage"] in
  let* () = match tree_opt with
   | None -> Lwt.return ()
   | Some tree ->
     Store.Tree.fold
       ~depth:(`Eq 1)
       ~contents:(fun kpath value () ->
         (match kpath with
          | [k] -> Hashtbl.add tbl k value
          | _ -> ());
         Lwt.return_unit)
       tree () in
  Lwt.return tbl

let sorted_storage_pairs storage_tbl =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) storage_tbl []
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)

let save_contract_storage t addr storage_tbl =
  let* current_tbl = load_contract_storage t addr in
  let removals =
    Hashtbl.fold
      (fun key _ acc ->
        if Hashtbl.mem storage_tbl key then acc else key :: acc)
      current_tbl
      []
    |> List.sort String.compare
  in
  let writes =
    sorted_storage_pairs storage_tbl
    |> List.filter (fun (key, value) ->
      match Hashtbl.find_opt current_tbl key with
      | Some current -> not (String.equal current value)
      | None -> true)
  in
  let* () =
    Lwt_list.iter_s
      (fun key -> remove_path t ["contracts"; addr; "storage"; key])
      removals
  in
  Lwt_list.iter_s
    (fun (key, value) -> write t ["contracts"; addr; "storage"; key] value)
    writes

type contract_meta = {
  address : string;
  code_hash : string;
  version : string;
  owner : string;
  ctype : string;
  admission : string;
}

let contract_meta_json meta =
  Yojson.Safe.to_string (`Assoc [
    "address", `String meta.address;
    "code_hash", `String meta.code_hash;
    "version", `String meta.version;
    "owner", `String meta.owner;
    "ctype", `String meta.ctype;
    "admission", `String meta.admission;
  ])

let deploy_contract t ~address ~code_hash ~version ~owner ~ctype ~admission
    ~bytecode_b64 =
  let meta =
    contract_meta_json { address; code_hash; version; owner; ctype; admission } in
  let* () = write t ["contracts"; address; "meta"] meta in
  write t ["contracts"; address; "bytecode"] bytecode_b64

let contract_exists t addr =
  let* v = read t ["contracts"; addr; "meta"] in
  Lwt.return (v <> None)

let save_contract_abi t addr abi_json =
  write t ["contracts"; addr; "abi"] abi_json

let get_contract_abi t addr =
  read t ["contracts"; addr; "abi"]

let save_contract_source t addr source =
  write t ["contracts"; addr; "source"] source

let get_contract_source t addr =
  read t ["contracts"; addr; "source"]

let save_contract_verification t addr verification_json =
  write t ["contracts"; addr; "verification"] verification_json

let get_contract_verification t addr =
  read t ["contracts"; addr; "verification"]

let save_contract_certificate t addr certificate_json =
  write t ["contracts"; addr; "certificate"] certificate_json

let get_contract_certificate t addr =
  read t ["contracts"; addr; "certificate"]

let get_contract_meta t addr =
  let* v = read t ["contracts"; addr; "meta"] in
  match v with
  | None -> Lwt.return_none
  | Some s ->
    (try
       let j = Yojson.Safe.from_string s in
       let open Yojson.Safe.Util in
       Lwt.return_some {
         address = j |> member "address" |> to_string;
         code_hash = j |> member "code_hash" |> to_string;
         version = j |> member "version" |> to_string;
         owner = j |> member "owner" |> to_string;
         ctype =
           (match j |> member "ctype" with
            | `String value -> value
            | _ -> "CUSTOM");
         admission =
           (match j |> member "admission" with
            | `String value -> value
            | _ -> "binary");
       }
     with _ -> Lwt.return_none)

let get_contract_info t addr =
  let* meta = get_contract_meta t addr in
  Lwt.return
    (Option.map
       (fun meta ->
         meta.address,
         meta.code_hash,
         meta.version,
         meta.owner)
       meta)

let upgrade_contract t ~address ~expected_code_hash ~code_hash ~version
    ~owner ~ctype ~admission ~bytecode_b64 =
  let* current = get_contract_meta t address in
  match current with
  | None ->
    Lwt.return_error "program not found"
  | Some current when not (String.equal current.owner owner) ->
    Lwt.return_error "program owner changed"
  | Some current when not (String.equal current.code_hash expected_code_hash) ->
    Lwt.return_error "program code hash changed"
  | Some _ ->
    let meta =
      contract_meta_json
        { address; code_hash; version; owner; ctype; admission }
    in
    let* () = write t ["contracts"; address; "meta"] meta in
    let* () = write t ["contracts"; address; "bytecode"] bytecode_b64 in
    Lwt.return_ok ()

let list_contracts t =
  let* tree_opt = read_tree t ["contracts"] in
  match tree_opt with
  | None -> Lwt.return []
  | Some tree ->
    let* entries = Store.Tree.list tree [] in
    let addrs = List.filter_map (fun (name, _) ->
      if Crypto.is_octra_address name then Some name
      else None
    ) entries in
    Lwt.return addrs

let read_contract_storage_key t addr key =
  read t ["contracts"; addr; "storage"; key]

let list_contract_storage t addr =
  let* tree_opt = read_tree t ["contracts"; addr; "storage"] in
  match tree_opt with
  | None -> Lwt.return []
  | Some tree ->
    let* entries = Store.Tree.list tree [] in
    let* pairs = Lwt_list.filter_map_s (fun (key, _) ->
      let* v = read t ["contracts"; addr; "storage"; key] in
      match v with
      | Some value -> Lwt.return (Some (key, value))
      | None -> Lwt.return_none
    ) entries in
    Lwt.return pairs

let set_optional_string t path value_opt =
  match value_opt with
  | Some value -> write t path value
  | None -> remove_path t path

let deploy_circle t (info : Circles.circle_info) =
  let root = ["circles"; info.circle_id] in
  let* () = write t (root @ ["runtime"]) (Circles.string_of_runtime info.runtime) in
  let* () = write t (root @ ["version"]) (Int64.to_string info.version) in
  let* () = write t (root @ ["owner"]) info.owner in
  let* () = write t (root @ ["code_hash"]) info.code_hash in
  let* () = write t (root @ ["stable_root"]) info.stable_root in
  let* () = write t (root @ ["assets_root"]) info.assets_root in
  let* () = write t (root @ ["privacy"; "class"]) (Circles.string_of_privacy_class info.privacy_class) in
  let* () = write t (root @ ["browser"; "mode"]) (Circles.string_of_browser_mode info.browser_mode) in
  let* () = write t (root @ ["resources"; "mode"]) (Circles.string_of_resource_mode info.resource_mode) in
  let* () = set_optional_string t (root @ ["privacy"; "policy_hash"]) info.policy_hash in
  let* () = set_optional_string t (root @ ["privacy"; "members_root"]) info.members_root in
  let* () = set_optional_string t (root @ ["privacy"; "export_policy"]) info.export_policy in
  let* () = write t (root @ ["limits"; "max_stable_bytes"]) (Int64.to_string info.limits.max_stable_bytes) in
  let* () = write t (root @ ["limits"; "max_assets_bytes"]) (Int64.to_string info.limits.max_assets_bytes) in
  let* () = write t (root @ ["limits"; "max_inline_value"]) (Int64.to_string info.limits.max_inline_value) in
  write t (root @ ["limits"; "max_wasm_bytes"]) (Int64.to_string info.limits.max_wasm_bytes)

let circle_exists t circle_id =
  let* v = read t ["circles"; circle_id; "runtime"] in
  Lwt.return (v <> None)

let get_circle_info t circle_id =
  let root = ["circles"; circle_id] in
  let* runtime_opt = read t (root @ ["runtime"]) in
  let* version_opt = read t (root @ ["version"]) in
  let* owner_opt = read t (root @ ["owner"]) in
  let* code_hash_opt = read t (root @ ["code_hash"]) in
  let* stable_root_opt = read t (root @ ["stable_root"]) in
  let* assets_root_opt = read t (root @ ["assets_root"]) in
  let* privacy_class_opt = read t (root @ ["privacy"; "class"]) in
  let* browser_mode_opt = read t (root @ ["browser"; "mode"]) in
  let* resource_mode_opt = read t (root @ ["resources"; "mode"]) in
  let* policy_hash = read t (root @ ["privacy"; "policy_hash"]) in
  let* members_root = read t (root @ ["privacy"; "members_root"]) in
  let* export_policy = read t (root @ ["privacy"; "export_policy"]) in
  let* max_stable_bytes_opt = read t (root @ ["limits"; "max_stable_bytes"]) in
  let* max_assets_bytes_opt = read t (root @ ["limits"; "max_assets_bytes"]) in
  let* max_inline_value_opt = read t (root @ ["limits"; "max_inline_value"]) in
  let* max_wasm_bytes_opt = read t (root @ ["limits"; "max_wasm_bytes"]) in
  match runtime_opt, version_opt, owner_opt, code_hash_opt, stable_root_opt, assets_root_opt,
    privacy_class_opt, browser_mode_opt, resource_mode_opt,
    max_stable_bytes_opt, max_assets_bytes_opt, max_inline_value_opt, max_wasm_bytes_opt with
  | Some runtime_s, Some version_s, Some owner, Some code_hash, Some stable_root, Some assets_root,
    Some privacy_class_s, Some browser_mode_s, Some resource_mode_s, Some max_stable_bytes_s, Some max_assets_bytes_s,
    Some max_inline_value_s, Some max_wasm_bytes_s ->
    begin
      match Circles.runtime_of_string runtime_s, Circles.privacy_class_of_string privacy_class_s,
        Circles.browser_mode_of_string browser_mode_s, Circles.resource_mode_of_string resource_mode_s with
      | Ok runtime, Ok privacy_class, Ok browser_mode, Ok resource_mode ->
        Lwt.return (Some {
          Circles.circle_id;
          runtime;
          version = Int64.of_string version_s;
          owner;
          code_hash;
          stable_root;
          assets_root;
          privacy_class;
          browser_mode;
          resource_mode;
          policy_hash;
          members_root;
          export_policy;
          limits = {
            Circles.max_stable_bytes = Int64.of_string max_stable_bytes_s;
            max_assets_bytes = Int64.of_string max_assets_bytes_s;
            max_inline_value = Int64.of_string max_inline_value_s;
            max_wasm_bytes = Int64.of_string max_wasm_bytes_s;
          };
        })
      | Error _, _, _, _ | _, Error _, _, _ | _, _, Error _, _ | _, _, _, Error _ ->
        Lwt.return_none
    end
  | _ -> Lwt.return_none

let save_circle_stable_entry t circle_id (entry : Circles.stable_entry) =
  let json = Yojson.Safe.to_string (Circles.yojson_of_stable_entry entry) in
  write t ["circles"; circle_id; "stable"; "by_hash"; entry.key_hash] json

let get_circle_stable_entry t circle_id key_hash =
  let* v = read t ["circles"; circle_id; "stable"; "by_hash"; key_hash] in
  match v with
  | None -> Lwt.return_none
  | Some s ->
    begin
      match Circles.stable_entry_of_yojson (Yojson.Safe.from_string s) with
      | Ok entry -> Lwt.return (Some entry)
      | Error _ -> Lwt.return_none
    end

let list_circle_stable_entries t circle_id =
  let* tree_opt = read_tree t ["circles"; circle_id; "stable"; "by_hash"] in
  match tree_opt with
  | None -> Lwt.return []
  | Some tree ->
    let* entries = Store.Tree.list tree [] in
    let* pairs = Lwt_list.filter_map_s (fun (key_hash, _) ->
      let* v = read t ["circles"; circle_id; "stable"; "by_hash"; key_hash] in
      match v with
      | None -> Lwt.return_none
      | Some value ->
        begin
          match Circles.stable_entry_of_yojson (Yojson.Safe.from_string value) with
          | Ok entry -> Lwt.return (Some entry)
          | Error _ -> Lwt.return_none
        end
    ) entries in
    Lwt.return pairs

type circle_stable_page = {
  entries : Circles.stable_entry list;
  total : int;
}

let list_circle_stable_entries_page t circle_id ~limit =
  let* tree_opt = read_tree t ["circles"; circle_id; "stable"; "by_hash"] in
  match tree_opt with
  | None -> Lwt.return { entries = []; total = 0 }
  | Some tree ->
    let* total = Store.Tree.length tree [] in
    let* rows = Store.Tree.list tree ~length:(max 0 limit) [] in
    let* entries =
      Lwt_list.filter_map_s
        (fun (key_hash, _) ->
          let* value_opt =
            read t ["circles"; circle_id; "stable"; "by_hash"; key_hash] in
          match value_opt with
          | None -> Lwt.return_none
          | Some value ->
            begin
              match Circles.stable_entry_of_yojson (Yojson.Safe.from_string value) with
              | Ok entry -> Lwt.return (Some entry)
              | Error _ -> Lwt.return_none
            end)
        rows in
    Lwt.return { entries; total }

let read_circle_stable_key t circle_id raw_key =
  let* entry_opt = get_circle_stable_entry t circle_id (Circles.stable_key_hash raw_key) in
  match entry_opt with
  | Some entry when entry.raw_key = raw_key -> Lwt.return (Some entry)
  | _ -> Lwt.return_none

let read_circle_stable_inline_value t circle_id raw_key =
  let* entry_opt = read_circle_stable_key t circle_id raw_key in
  match entry_opt with
  | Some { Circles.value = Circles.Inline value; _ } -> Lwt.return (Some value)
  | _ -> Lwt.return_none

let load_circle_stable_storage t circle_id =
  let tbl = Hashtbl.create 100 in
  let* entries = list_circle_stable_entries t circle_id in
  let inline_only = ref true in
  List.iter (fun entry ->
    match entry.Circles.value with
    | Circles.Inline value -> Hashtbl.replace tbl entry.raw_key value
    | Circles.Blob_ref _ -> inline_only := false
  ) entries;
  if !inline_only then Lwt.return (Ok tbl)
  else Lwt.return (Error "circle stable storage contains non-inline values")

let load_circle_stable_storage_page t circle_id ~limit =
  let* page = list_circle_stable_entries_page t circle_id ~limit in
  let rec inline_pairs acc = function
    | [] -> Ok (List.rev acc)
    | entry :: rest ->
      begin
        match entry.Circles.value with
        | Circles.Inline value ->
          inline_pairs ((entry.raw_key, value) :: acc) rest
        | Circles.Blob_ref _ ->
          Error "circle stable storage contains non-inline values"
      end
  in
  match inline_pairs [] page.entries with
  | Error e -> Lwt.return (Error e)
  | Ok pairs ->
    Lwt.return (Ok (pairs, page.total))

let save_circle_stable_storage t circle_id storage_tbl =
  let* () = remove_path t ["circles"; circle_id; "stable"; "by_hash"] in
  let pairs =
    Hashtbl.fold (fun raw_key value acc -> (raw_key, value) :: acc) storage_tbl []
    |> List.sort (fun (left, _) (right, _) -> String.compare left right)
  in
  let* () = Lwt_list.iter_s (fun (raw_key, value) ->
    save_circle_stable_entry t circle_id (Circles.make_stable_entry raw_key (Circles.Inline value))
  ) pairs in
  let* tree_opt = read_tree t ["circles"; circle_id; "stable"] in
  let stable_root =
    match tree_opt with
    | Some tree -> Irmin.Type.to_string Store.Hash.t (Store.Tree.hash tree)
    | None -> Circles.zero_hash_hex
  in
  let* () = write t ["circles"; circle_id; "stable_root"] stable_root in
  Lwt.return stable_root

let save_circle_stable_storage_checked t circle_id limits storage_tbl =
  match Circles.validate_stable_storage limits storage_tbl with
  | Error e -> Lwt.return (Error e)
  | Ok _ ->
    let* stable_root = save_circle_stable_storage t circle_id storage_tbl in
    Lwt.return (Ok stable_root)

let save_circle_asset_meta t circle_id (meta : Circles.asset_meta) =
  let json = Yojson.Safe.to_string (Circles.yojson_of_asset_meta meta) in
  write t ["circles"; circle_id; "assets"; "by_hash"; meta.path_key; "meta"] json

let get_circle_asset_meta t circle_id path_key =
  let* v = read t ["circles"; circle_id; "assets"; "by_hash"; path_key; "meta"] in
  match v with
  | None -> Lwt.return_none
  | Some s ->
    begin
      match Circles.asset_meta_of_yojson (Yojson.Safe.from_string s) with
      | Ok meta -> Lwt.return (Some meta)
      | Error _ -> Lwt.return_none
    end

let save_circle_asset_body_b64 t circle_id path_key body_b64 =
  write t ["circles"; circle_id; "assets"; "by_hash"; path_key; "body_b64"] body_b64

let get_circle_asset_body_b64 t circle_id path_key =
  read t ["circles"; circle_id; "assets"; "by_hash"; path_key; "body_b64"]

let save_circle_asset_ciphertext_b64 t circle_id path_key ciphertext_b64 =
  write t ["circles"; circle_id; "assets"; "by_hash"; path_key; "ciphertext_b64"] ciphertext_b64

let get_circle_asset_ciphertext_b64 t circle_id path_key =
  read t ["circles"; circle_id; "assets"; "by_hash"; path_key; "ciphertext_b64"]

let save_circle_asset_resource_index t circle_id resource_key path_key =
  write t ["circles"; circle_id; "assets"; "resource_keys"; resource_key] path_key

let get_circle_asset_path_key_by_resource_key t circle_id resource_key =
  read t ["circles"; circle_id; "assets"; "resource_keys"; resource_key]

let set_circle_assets_root t circle_id assets_root =
  write t ["circles"; circle_id; "assets_root"] assets_root

let set_circle_stable_root t circle_id stable_root =
  write t ["circles"; circle_id; "stable_root"] stable_root

let set_circle_asset_usage_bytes t circle_id usage_bytes =
  write t ["circles"; circle_id; "assets"; "usage_bytes"] (Int64.to_string usage_bytes)

let get_circle_asset_usage_bytes t circle_id =
  let* v = read t ["circles"; circle_id; "assets"; "usage_bytes"] in
  match v with
  | Some s ->
    (try Lwt.return (Int64.of_string s) with _ -> Lwt.return 0L)
  | None -> Lwt.return 0L

let save_circle_program_code_b64 t circle_id code_b64 =
  write t ["circles"; circle_id; "program"; "code_b64"] code_b64

let get_circle_program_code_b64 t circle_id =
  read t ["circles"; circle_id; "program"; "code_b64"]

let update_circle_program t circle_id ~version ~code_hash ~code_b64 =
  let* () = write t ["circles"; circle_id; "version"] (Int64.to_string version) in
  let* () = write t ["circles"; circle_id; "code_hash"] code_hash in
  save_circle_program_code_b64 t circle_id code_b64

let save_circle_outbox_intent t circle_id (intent : Circles.outbox_intent) =
  let root = ["circles"; circle_id; "egress"; "outbox"; intent.intent_id] in
  let json = Yojson.Safe.to_string (Circles.yojson_of_outbox_intent intent) in
  let* () = write t (root @ ["meta"]) json in
  let* () = write t (root @ ["status"]) (Circles.string_of_outbox_status Circles.Open) in
  match intent.delivery_key_id with
  | Some key_id ->
    write t ["circles"; circle_id; "egress"; "by_delivery_key"; key_id; "intents"; intent.intent_id] "1"
  | None ->
    Lwt.return_unit

let circle_outbox_claim_root circle_id intent_id relay_id =
  ["circles"; circle_id; "egress"; "outbox"; intent_id; "claims"; relay_id]

let circle_outbox_claim_resolution_root circle_id intent_id relay_id =
  circle_outbox_claim_root circle_id intent_id relay_id @ ["resolution"]

let save_circle_outbox_claim_for_relay t circle_id (claim : Circles.relay_claim) =
  let root = circle_outbox_claim_root circle_id claim.intent_id claim.relay_id in
  let json = Yojson.Safe.to_string (Circles.yojson_of_relay_claim claim) in
  write t (root @ ["meta"]) json

let clear_circle_outbox_claim_resolution t circle_id intent_id relay_id =
  remove_path t (circle_outbox_claim_resolution_root circle_id intent_id relay_id)

let save_circle_outbox_claim t circle_id (claim : Circles.relay_claim) =
  let root = ["circles"; circle_id; "egress"; "outbox"; claim.intent_id; "claim"] in
  let json = Yojson.Safe.to_string (Circles.yojson_of_relay_claim claim) in
  let* () = save_circle_outbox_claim_for_relay t circle_id claim in
  let* () = clear_circle_outbox_claim_resolution t circle_id claim.intent_id claim.relay_id in
  write t (root @ ["meta"]) json

let save_circle_outbox_claim_resolution t circle_id relay_id (resolution : Circles.outbox_resolution) =
  let root = circle_outbox_claim_resolution_root circle_id resolution.intent_id relay_id in
  let json = Yojson.Safe.to_string (Circles.yojson_of_outbox_resolution resolution) in
  write t (root @ ["meta"]) json

let save_circle_outbox_resolution t circle_id (resolution : Circles.outbox_resolution) =
  let root = ["circles"; circle_id; "egress"; "outbox"; resolution.intent_id; "resolution"] in
  let json = Yojson.Safe.to_string (Circles.yojson_of_outbox_resolution resolution) in
  write t (root @ ["meta"]) json

let get_circle_outbox_intent t circle_id intent_id =
  let* v = read t ["circles"; circle_id; "egress"; "outbox"; intent_id; "meta"] in
  match v with
  | None -> Lwt.return_none
  | Some s ->
    begin
      match Circles.outbox_intent_of_yojson (Yojson.Safe.from_string s) with
      | Ok intent -> Lwt.return (Some intent)
      | Error _ -> Lwt.return_none
    end

let get_circle_outbox_claim t circle_id intent_id =
  let* v = read t ["circles"; circle_id; "egress"; "outbox"; intent_id; "claim"; "meta"] in
  match v with
  | None -> Lwt.return_none
  | Some s ->
    begin
      match Circles.relay_claim_of_yojson (Yojson.Safe.from_string s) with
      | Ok claim -> Lwt.return (Some claim)
      | Error _ -> Lwt.return_none
    end

let get_circle_outbox_claim_for_relay t circle_id intent_id relay_id =
  let* v = read t (circle_outbox_claim_root circle_id intent_id relay_id @ ["meta"]) in
  match v with
  | None -> Lwt.return_none
  | Some s ->
    begin
      match Circles.relay_claim_of_yojson (Yojson.Safe.from_string s) with
      | Ok claim -> Lwt.return (Some claim)
      | Error _ -> Lwt.return_none
    end

let list_circle_outbox_claims t circle_id intent_id =
  let* tree_opt = read_tree t ["circles"; circle_id; "egress"; "outbox"; intent_id; "claims"] in
  match tree_opt with
  | None -> Lwt.return []
  | Some tree ->
    let* entries = Store.Tree.list tree [] in
    let* claims =
      Lwt_list.filter_map_s
        (fun (relay_id, _) -> get_circle_outbox_claim_for_relay t circle_id intent_id relay_id)
        entries in
    Lwt.return claims

let get_circle_outbox_claim_resolution t circle_id intent_id relay_id =
  let* v = read t (circle_outbox_claim_resolution_root circle_id intent_id relay_id @ ["meta"]) in
  match v with
  | None -> Lwt.return_none
  | Some s ->
    begin
      match Circles.outbox_resolution_of_yojson (Yojson.Safe.from_string s) with
      | Ok resolution -> Lwt.return (Some resolution)
      | Error _ -> Lwt.return_none
    end

let list_circle_outbox_claim_resolutions t circle_id intent_id =
  let* tree_opt = read_tree t ["circles"; circle_id; "egress"; "outbox"; intent_id; "claims"] in
  match tree_opt with
  | None -> Lwt.return []
  | Some tree ->
    let* entries = Store.Tree.list tree [] in
    let* resolutions =
      Lwt_list.filter_map_s
        (fun (relay_id, _) -> get_circle_outbox_claim_resolution t circle_id intent_id relay_id)
        entries in
    Lwt.return resolutions

let get_circle_outbox_resolution t circle_id intent_id =
  let* v = read t ["circles"; circle_id; "egress"; "outbox"; intent_id; "resolution"; "meta"] in
  match v with
  | None -> Lwt.return_none
  | Some s ->
    begin
      match Circles.outbox_resolution_of_yojson (Yojson.Safe.from_string s) with
      | Ok resolution -> Lwt.return (Some resolution)
      | Error _ -> Lwt.return_none
    end

let get_circle_outbox_status t circle_id intent_id =
  let* v = read t ["circles"; circle_id; "egress"; "outbox"; intent_id; "status"] in
  match v with
  | None -> Lwt.return_none
  | Some s ->
    begin
      match Circles.outbox_status_of_string s with
      | Ok status -> Lwt.return (Some status)
      | Error _ -> Lwt.return_none
    end

let set_circle_outbox_status t circle_id intent_id status =
  write t ["circles"; circle_id; "egress"; "outbox"; intent_id; "status"]
    (Circles.string_of_outbox_status status)

let list_circle_outbox_intents_by_delivery_key t circle_id key_id =
  let* tree_opt = read_tree t ["circles"; circle_id; "egress"; "by_delivery_key"; key_id; "intents"] in
  match tree_opt with
  | None -> Lwt.return []
  | Some tree ->
    let* entries = Store.Tree.list tree [] in
    Lwt.return (entries |> List.map fst |> List.sort_uniq String.compare)

let save_circle_ingress_policy_hash t circle_id policy_hash =
  write t ["circles"; circle_id; "ingress"; "policy_hash"] policy_hash

let get_circle_ingress_policy_hash t circle_id =
  read t ["circles"; circle_id; "ingress"; "policy_hash"]

let save_circle_ingress_packet t circle_id (packet : Circles.ingress_packet) =
  let root = ["circles"; circle_id; "ingress"; "packets"; packet.intent_id] in
  let json = Yojson.Safe.to_string (Circles.yojson_of_ingress_packet packet) in
  write t (root @ ["meta"]) json

let get_circle_ingress_packet t circle_id intent_id =
  let* v = read t ["circles"; circle_id; "ingress"; "packets"; intent_id; "meta"] in
  match v with
  | None -> Lwt.return_none
  | Some s ->
    begin
      match Circles.ingress_packet_of_yojson (Yojson.Safe.from_string s) with
      | Ok (packet : Circles.ingress_packet) -> Lwt.return (Some packet)
      | Error _ -> Lwt.return_none
    end

let get_meta t key =
  read t ["meta"; key]

let set_meta t key value =
  write t ["meta"; key] value

let state_hash t =
  let* head = Store.Head.find t.store in
  match head with
  | Some commit ->
    let tree = Store.Commit.tree commit in
    let hash = Store.Tree.hash tree in
    Lwt.return (Irmin.Type.to_string Store.Hash.t hash)
  | None -> Lwt.return "empty"

let get_head_hash t =
  let* head = Store.Head.find t.store in
  match head with
  | Some commit ->
    let tree = Store.Commit.tree commit in
    Lwt.return (Some (Irmin.Type.to_string Store.Hash.t (Store.Tree.hash tree)))
  | None -> Lwt.return_none

let get_batch_tree_hash t =
  match t.batch_tree with
  | Some tree ->
    Lwt.return (Some (Irmin.Type.to_string Store.Hash.t (Store.Tree.hash tree)))
  | None -> Lwt.return_none

let get_tree_hash_at_path t path =
  let* tree_opt = read_tree t path in
  match tree_opt with
  | Some tree ->
    Lwt.return (Some (Irmin.Type.to_string Store.Hash.t (Store.Tree.hash tree)))
  | None -> Lwt.return_none

let dump_batch_subtree_hashes t =
  match t.batch_tree with
  | None -> Lwt.return []
  | Some tree ->
    let* entries = Store.Tree.list tree [] in
    Lwt_list.map_s (fun (name, _) ->
      let* sub_opt = Store.Tree.find_tree tree [name] in
      match sub_opt with
      | Some sub ->
        let h = Irmin.Type.to_string Store.Hash.t (Store.Tree.hash sub) in
        Lwt.return (name, h)
      | None -> Lwt.return (name, "<none>")
    ) entries

let dump_batch_meta_pairs t =
  match t.batch_tree with
  | None -> Lwt.return []
  | Some tree ->
    let* meta_opt = Store.Tree.find_tree tree ["meta"] in
    match meta_opt with
    | None -> Lwt.return []
    | Some meta_tree ->
      let* entries = Store.Tree.list meta_tree [] in
      Lwt_list.map_s (fun (name, _) ->
        let* v = Store.Tree.find meta_tree [name] in
        Lwt.return (name, Option.value ~default:"<none>" v)
      ) entries

let get_commit_hash t =
  let* head = Store.Head.find t.store in
  match head with
  | Some commit ->
    Lwt.return (Some (Irmin.Type.to_string Store.Hash.t (Store.Commit.hash commit)))
  | None -> Lwt.return_none

type integrity_result = {
  ok : bool;
  head_hash : string;
  accounts_sampled : int;
  accounts_ok : int;
  errors : string list;
}

let verify_integrity t =
  let errors = ref [] in
  let err msg = errors := msg :: !errors in
  let* head_opt = Store.Head.find t.store in
  match head_opt with
  | None ->
    Lwt.return { ok = false; head_hash = ""; accounts_sampled = 0;
                 accounts_ok = 0; errors = ["no head commit — store empty or corrupted"] }
  | Some commit ->
    let head_hash = Irmin.Type.to_string Store.Hash.t (Store.Commit.hash commit) in
    let* tree_exists = read_tree t ["accounts"] in
    (if tree_exists = None then err "accounts subtree missing");
    let sampled = ref 0 in
    let ok_count = ref 0 in
    let* () = match tree_exists with
      | None -> Lwt.return_unit
      | Some tree ->
        Store.Tree.fold
          ~depth:(`Eq 2)
          ~contents:(fun path value () ->
            let leaf = List.nth_opt path (List.length path - 1) in
            if leaf = Some "data" then begin
              incr sampled;
              (try
                 let j = Yojson.Safe.from_string value in
                 match Ledger_types.account_of_yojson j with
                 | Ok _a -> incr ok_count
                 | Error e ->
                   let addr = if List.length path >= 2 then List.nth path (List.length path - 2) else "?" in
                   err (Printf.sprintf "account %s: json decode failed: %s" addr e)
               with exn ->
                 let addr = if List.length path >= 2 then List.nth path (List.length path - 2) else "?" in
                 err (Printf.sprintf "account %s: parse exception: %s" addr (Printexc.to_string exn)))
            end;
            Lwt.return_unit)
          tree () in
    let* meta_ok =
      let* v = read t ["meta"; "last_epoch"] in
      match v with
      | Some s ->
        (try ignore (int_of_string s); Lwt.return true
         with _ -> err (Printf.sprintf "meta/last_epoch corrupt: %s" s); Lwt.return false)
      | None -> err "meta/last_epoch missing"; Lwt.return false
    in
    ignore meta_ok;
    let saved_root =
      if Sys.file_exists t.state_root_file then
        let ic = open_in_bin t.state_root_file in
        let s = input_line ic in
        close_in ic; Some s
      else
        None
    in
    (match saved_root with
     | Some expected when expected <> head_hash ->
       err (Printf.sprintf "STATE ROOT MISMATCH: saved=%s actual=%s — possible data corruption!" expected head_hash)
     | _ -> ());
    let final_errors = List.rev !errors in
    Lwt.return { ok = (final_errors = []); head_hash;
                 accounts_sampled = !sampled; accounts_ok = !ok_count;
                 errors = final_errors }

let save_state_root t =
  let* h = state_hash t in
  if h <> "empty" then begin
    let oc = open_out_bin t.state_root_file in
    output_string oc h;
    close_out oc;
    Lwt.return_unit
  end else Lwt.return_unit

let account_count t =
  let count = ref 0 in
  let* tree_opt = read_tree t ["accounts"] in
  let* () = match tree_opt with
   | None -> Lwt.return ()
   | Some tree ->
     Store.Tree.fold
       ~depth:(`Eq 1)
       ~tree:(fun _path _subtree () ->
         incr count; Lwt.return_unit)
       tree () in
  Lwt.return !count

let txs_by_epoch t epoch_id =
  let hashes = ref [] in
  let* tree_opt = read_tree t ["index"; "by_epoch"; string_of_int epoch_id] in
  let* () = match tree_opt with
   | None -> Lwt.return ()
   | Some tree ->
     Store.Tree.fold
       ~depth:(`Eq 1)
       ~contents:(fun kpath _value () ->
         (match kpath with
          | [h] -> hashes := h :: !hashes
          | _ -> ());
         Lwt.return_unit)
       tree () in
  Lwt.return !hashes

let txs_by_epoch_full t epoch_id =
  let* hashes = txs_by_epoch t epoch_id in
  Lwt_list.filter_map_s (fun hash ->
    let* v = read t ["txs"; hash] in
    match v with
    | Some tj -> Lwt.return (Some (hash, tj))
    | None -> Lwt.return_none
  ) hashes

let iter_subtree t path f =
  let* tree_opt = read_tree t path in
  match tree_opt with
  | None -> Lwt.return_unit
  | Some tree ->
    Store.Tree.fold
      ~depth:(`Eq 1)
      ~contents:(fun kpath value () ->
        (match kpath with
         | [key] -> f key value
         | _ -> ());
        Lwt.return_unit)
      tree ()

type pvac_blob_check = {
  bound : int;
  missing : string list;
  corrupt : string list;
}

let inspect_pvac_blobs t =
  let bound = ref 0 in
  let missing = ref [] in
  let corrupt = ref [] in
  let* () =
    iter_subtree t ["pvac_hashes"] (fun addr hash ->
      if hash <> "none" then begin
        incr bound;
        match read_file (pvac_blob_path t hash) with
        | None -> missing := addr :: !missing
        | Some blob when String.equal (pvac_hash blob) hash -> ()
        | Some _ -> corrupt := addr :: !corrupt
      end)
  in
  Lwt.return {
    bound = !bound;
    missing = List.sort String.compare !missing;
    corrupt = List.sort String.compare !corrupt;
  }

type bulk_tree = Store.tree

let begin_bulk t =
  Store.get_tree t.store []

let bulk_add tree path value =
  Store.Tree.add tree path value

let commit_bulk t tree msg =
  let info = make_info msg in
  let* result = Store.set_tree t.store ~info [] tree in
  (match result with
   | Ok () -> Lwt.return_unit
   | Error e ->
     let errmsg = Printf.sprintf "Irmin commit_bulk failed msg=%s: %s" msg
       (Fmt.to_to_string (Irmin.Type.pp_json Store.write_error_t) e) in
     Octra_log.fatal "irmin"
       "event = bulk_commit_failed message = %s error = %s"
       msg
       (Fmt.to_to_string (Irmin.Type.pp_json Store.write_error_t) e);
     Lwt.fail (Irmin_write_failed errmsg))

type compact_store_result = {
  commit_hash : string;
  tree_hash : string;
}

let create_compact_store t ~expected_commit ~target =
  let* head = Store.Head.find t.store in
  match head with
  | None -> Lwt.return (Error "source Irmin store has no head")
  | Some commit ->
    let commit_hash =
      Irmin.Type.to_string Store.Hash.t (Store.Commit.hash commit)
    in
    if commit_hash <> expected_commit then
      Lwt.return (Error "source Irmin commit does not match checkpoint")
    else if Sys.file_exists target then
      Lwt.return (Error "compact Irmin target already exists")
    else
      let tree_hash =
        Irmin.Type.to_string Store.Hash.t
          (Store.Tree.hash (Store.Commit.tree commit))
      in
      Lwt.catch
        (fun () ->
          let* () =
            Store.create_one_commit_store
              t.repo
              (Store.Commit.key commit)
              target
          in
          let* compact = open_store target in
          Lwt.finalize
            (fun () ->
              let* restored =
                Store.Commit.of_hash compact.repo (Store.Commit.hash commit)
              in
              match restored with
              | None ->
                Lwt.return (Error "compact Irmin commit is unavailable")
              | Some restored ->
                let restored_tree_hash =
                  Irmin.Type.to_string Store.Hash.t
                    (Store.Tree.hash (Store.Commit.tree restored))
                in
                if restored_tree_hash <> tree_hash then
                  Lwt.return (Error "compact Irmin tree hash mismatch")
                else
                  let* () = Store.Head.set compact.store restored in
                  Store.flush compact.repo;
                  Lwt.return (Ok { commit_hash; tree_hash }))
            (fun () -> close compact))
        (fun exn ->
          Lwt.return
            (Error ("compact Irmin export failed: " ^ Printexc.to_string exn)))

let tag_epoch t epoch_id =
  let* head = Store.Head.find t.store in
  match head with
  | Some commit ->
    let branch = Printf.sprintf "epoch_%d" epoch_id in
    let* () = Store.Branch.set t.repo branch commit in
    Lwt.return_unit
  | None -> Lwt.return_unit

let rollback_to_epoch t epoch_id =
  let branch = Printf.sprintf "epoch_%d" epoch_id in
  let* commit_opt = Store.Branch.find t.repo branch in
  match commit_opt with
  | None -> Lwt.return (Error (Printf.sprintf "epoch %d tag not found" epoch_id))
  | Some commit ->
    let* () = Store.Head.set t.store commit in
    Lwt.return (Ok ())

let read_at_epoch t epoch_id path =
  let branch = Printf.sprintf "epoch_%d" epoch_id in
  let* commit_opt = Store.Branch.find t.repo branch in
  match commit_opt with
  | None -> Lwt.return_none
  | Some commit ->
    let tree = Store.Commit.tree commit in
    Store.Tree.find tree path

let list_epoch_tags t =
  let* branches = Store.Branch.list t.repo in
  let epochs = List.filter_map (fun b ->
    if String.length b > 6 && String.sub b 0 6 = "epoch_" then
      (try Some (int_of_string (String.sub b 6 (String.length b - 6)))
       with _ -> None)
    else None
  ) branches in
  Lwt.return (List.sort compare epochs)

let gc_keep_epochs = 50000

let cleanup_old_tags t current_epoch =
  let cutoff = current_epoch - gc_keep_epochs in
  if cutoff <= 0 then Lwt.return_unit
  else
    let* tags = list_epoch_tags t in
    Lwt_list.iter_s (fun eid ->
      if eid < cutoff then
        Store.Branch.remove t.repo (Printf.sprintf "epoch_%d" eid)
      else Lwt.return_unit
    ) tags