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


type decision =
  | Already_applied
  | Apply_now
  | Need_catchup

type entry = {
  height : int;
  round : int;
  proposal_id : string;
  tx_list_hash : string;
  state_root : string;
  creator_addr : string;
  txid_hi : int64;
  qc_hash : string option;
  ts : float;
}

let decide ~head height =
  if height <= head then Already_applied
  else if height = head + 1 then Apply_now
  else Need_catchup

let hex s =
  String.concat "" (List.init (String.length s) (fun i ->
    Printf.sprintf "%02x" (Char.code s.[i])))

let esc s =
  let b = Buffer.create (String.length s + 8) in
  String.iter (function
    | '"' -> Buffer.add_string b "\\\""
    | '\\' -> Buffer.add_string b "\\\\"
    | '\b' -> Buffer.add_string b "\\b"
    | '\012' -> Buffer.add_string b "\\f"
    | '\n' -> Buffer.add_string b "\\n"
    | '\r' -> Buffer.add_string b "\\r"
    | '\t' -> Buffer.add_string b "\\t"
    | c when Char.code c < 0x20 ->
      Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
    | c -> Buffer.add_char b c) s;
  Buffer.contents b

let str s =
  Printf.sprintf "\"%s\"" (esc s)

let opt = function
  | Some s -> str s
  | None -> "null"

let line e =
  Printf.sprintf
    "{\"height\":%d,\"round\":%d,\"proposal_id\":%s,\"tx_list_hash\":%s,\"state_root\":%s,\"creator_addr\":%s,\"txid_hi\":%Ld,\"qc_hash\":%s,\"ts\":%.6f}"
    e.height
    e.round
    (str e.proposal_id)
    (str e.tx_list_hash)
    (str e.state_root)
    (str e.creator_addr)
    e.txid_hi
    (opt e.qc_hash)
    e.ts

let dir base =
  Filename.concat base "finality"

let path base =
  Filename.concat (dir base) "finality.log"

let ensure_dir base =
  let d = dir base in
  if Sys.file_exists d then begin
    if not (Sys.is_directory d) then
      failwith ("finality path is not a directory: " ^ d)
  end else
    Unix.mkdir d 0o750

let rec write_all fd s pos =
  if pos < String.length s then begin
    let n = Unix.write_substring fd s pos (String.length s - pos) in
    if n <= 0 then failwith "finality log write returned zero";
    write_all fd s (pos + n)
  end

let write base e =
  ensure_dir base;
  let p = path base in
  let fd = Unix.openfile p [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] 0o640 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      write_all fd (line e ^ "\n") 0;
      Unix.fsync fd)

let replace base entries =
  ensure_dir base;
  let p = path base in
  let tmp = p ^ ".tmp" in
  let fd = Unix.openfile tmp [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o640 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      List.iter (fun e -> write_all fd (line e ^ "\n") 0) entries;
      Unix.fsync fd);
  Unix.rename tmp p;
  (try
    let dfd = Unix.openfile (dir base) [Unix.O_RDONLY] 0 in
    Fun.protect
      ~finally:(fun () -> Unix.close dfd)
      (fun () -> Unix.fsync dfd)
   with _ -> ())

let entry_of_json j =
  let open Yojson.Safe.Util in
  let int64 name =
    match member name j with
    | `Int i -> Int64.of_int i
    | `Intlit s -> Int64.of_string s
    | `String s -> Int64.of_string s
    | _ -> failwith ("finality log bad int64: " ^ name)
  in
  {
    height = j |> member "height" |> to_int;
    round = j |> member "round" |> to_int;
    proposal_id = j |> member "proposal_id" |> to_string;
    tx_list_hash = j |> member "tx_list_hash" |> to_string;
    state_root = j |> member "state_root" |> to_string;
    creator_addr = j |> member "creator_addr" |> to_string;
    txid_hi = int64 "txid_hi";
    qc_hash = j |> member "qc_hash" |> to_string_option;
    ts = j |> member "ts" |> to_float;
  }

let read base =
  let p = path base in
  if not (Sys.file_exists p) then []
  else
    let ic = open_in p in
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () ->
        let rec loop acc =
          match input_line ic with
          | line ->
            let line = String.trim line in
            if line = "" then loop acc
            else loop (entry_of_json (Yojson.Safe.from_string line) :: acc)
          | exception End_of_file -> List.rev acc
        in
        loop [])

let drop_after base head =
  let entries = read base in
  let kept = List.filter (fun e -> e.height <= head) entries in
  if List.length kept = List.length entries then 0
  else begin
    replace base kept;
    List.length entries - List.length kept
  end

let last base =
  match List.rev (read base) with
  | x :: _ -> Some x
  | [] -> None

let make ~height ~round ~proposal_id ~tx_list_hash ~state_root
    ~creator_addr ~txid_hi ?qc_hash ~ts () =
  { height; round; proposal_id; tx_list_hash; state_root; creator_addr;
    txid_hi; qc_hash; ts }

let of_header ?qc_hash ~round (h : C_types.epoch_header) =
  make
    ~height:(Int64.to_int h.epoch_id)
    ~round
    ~proposal_id:(hex (C_hash.proposal_id h))
    ~tx_list_hash:(hex h.tx_list_hash)
    ~state_root:(hex h.proposed_state_root)
    ~creator_addr:h.creator_addr
    ~txid_hi:h.txid_hi
    ?qc_hash
    ~ts:h.ts
    ()

let hash_finalize f =
  C_codec.encode_finalize f
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_raw_string
  |> hex

let of_finalize f =
  make
    ~height:(Int64.to_int f.C_types.epoch_id)
    ~round:f.C_types.commit_round
    ~proposal_id:(hex f.C_types.proposal_id)
    ~tx_list_hash:(hex f.C_types.header.tx_list_hash)
    ~state_root:(hex f.C_types.header.proposed_state_root)
    ~creator_addr:f.C_types.header.creator_addr
    ~txid_hi:f.C_types.header.txid_hi
    ~qc_hash:(hash_finalize f)
    ~ts:f.C_types.header.ts
    ()

let id_of_parts ~height ~prev_state_root ~tx_list_hash ~state_root =
  Digestif.SHA256.digest_string
    (string_of_int height ^ prev_state_root ^ tx_list_hash ^ state_root)
  |> Digestif.SHA256.to_raw_string
  |> hex

let catchup_id (r : C_codec.catchup_epoch_record) =
  id_of_parts
    ~height:(Int64.to_int r.epoch_id)
    ~prev_state_root:r.prev_state_root
    ~tx_list_hash:r.tx_list_hash
    ~state_root:r.state_root

let of_catchup ?qc_hash (r : C_codec.catchup_epoch_record) =
  make
    ~height:(Int64.to_int r.epoch_id)
    ~round:r.commit_round
    ~proposal_id:(catchup_id r)
    ~tx_list_hash:(hex r.tx_list_hash)
    ~state_root:(hex r.state_root)
    ~creator_addr:r.creator_addr
    ~txid_hi:(-1L)
    ?qc_hash
    ~ts:(Unix.gettimeofday ())
    ()