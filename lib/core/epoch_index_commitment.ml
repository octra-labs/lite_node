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


type item = {
  txid : int64;
  hash : string;
}

let tag_epoch = "octra:eic:epoch:v1"

let tag_root = "octra:eic:root:v1"

let tag_genesis = "octra:eic:genesis:v1"

let tag_state_root = "octra:eic:state-root:v1"

let tag_storage_root = "octra:eic:storage-root-string:v1"

let sha s =
  Digestif.SHA256.(digest_string s |> to_hex)

let put b s =
  Buffer.add_string b (string_of_int (String.length s));
  Buffer.add_char b ':';
  Buffer.add_string b s;
  Buffer.add_char b '\n'

let is_hex c =
  match c with
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let raw_hex s =
  String.concat "" (List.init (String.length s) (fun i ->
    Printf.sprintf "%02x" (Char.code s.[i])))

let norm_hash hash =
  let h =
    if String.length hash = 32 then raw_hex hash
    else String.lowercase_ascii hash
  in
  if String.length h <> 64 || not (String.for_all is_hex h) then
    invalid_arg "epoch index commitment: invalid tx hash";
  h

let root_hex root =
  if String.length root >= 64 && String.for_all is_hex root then
    String.sub (String.lowercase_ascii root) 0 64
  else if String.length root = 32 then
    raw_hex root
  else if String.length root > 0 then begin
    let b = Buffer.create (String.length root + 64) in
    put b tag_storage_root;
    put b root;
    sha (Buffer.contents b)
  end
  else
    invalid_arg "epoch index commitment: invalid root"

let item ~txid ~hash =
  { txid; hash = norm_hash hash }

let put_i b n =
  Buffer.add_string b (Int64.to_string n);
  Buffer.add_char b '\n'

let order items =
  let sorted = List.sort (fun a b -> Int64.compare a.txid b.txid) items in
  let rec scan prev = function
    | [] -> ()
    | x :: xs ->
        (match prev with
         | Some p when p = x.txid ->
             invalid_arg "epoch index commitment: duplicate txid"
         | _ -> ());
        ignore (norm_hash x.hash);
        scan (Some x.txid) xs
  in
  scan None sorted;
  sorted

let epoch_hash ~epoch_id items =
  let items = order items in
  let b = Buffer.create (64 + List.length items * 96) in
  put b tag_epoch;
  put_i b (Int64.of_int epoch_id);
  put_i b (Int64.of_int (List.length items));
  List.iter (fun x ->
    put_i b x.txid;
    put b (norm_hash x.hash)
  ) items;
  sha (Buffer.contents b)

let genesis_root =
  sha tag_genesis

let root_hash ~prev ~epoch_hash =
  let b = Buffer.create 160 in
  put b tag_root;
  put b prev;
  put b (norm_hash epoch_hash);
  sha (Buffer.contents b)

let next_root ~prev ~epoch_id items =
  let epoch_hash = epoch_hash ~epoch_id items in
  let epoch_index_root = root_hash ~prev ~epoch_hash in
  (epoch_hash, epoch_index_root)

let items_from_hashes ~start_txid hashes =
  List.mapi (fun i hash ->
    item ~txid:(Int64.add start_txid (Int64.of_int i)) ~hash
  ) hashes

let next_root_from_hashes ~prev ~epoch_id ~start_txid hashes =
  next_root ~prev ~epoch_id (items_from_hashes ~start_txid hashes)

let folded_state_root ~ledger_state_root ~epoch_index_root =
  let b = Buffer.create 160 in
  put b tag_state_root;
  put b (root_hex ledger_state_root);
  put b (root_hex epoch_index_root);
  sha (Buffer.contents b)