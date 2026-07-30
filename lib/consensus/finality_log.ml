(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

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

let last_entry_fast base =
  let p = path base in
  if not (Sys.file_exists p) then None
  else
    let ic = open_in_bin p in
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () ->
        let length = in_channel_length ic in
        if length = 0 then None
        else
          let window = min length 16384 in
          let offset = length - window in
          seek_in ic offset;
          let tail = really_input_string ic window in
          let lines = String.split_on_char '\n' tail in
          let complete =
            if offset = 0 then lines
            else
              match lines with
              | _ :: rest -> rest
              | [] -> []
          in
          match
            complete
            |> List.filter (fun value -> String.trim value <> "")
            |> List.rev
          with
          | value :: _ ->
            Some (entry_of_json (Yojson.Safe.from_string value))
          | [] ->
            failwith "finality log tail exceeds read window")

let same_commitment a b =
  a.height = b.height
  && a.proposal_id = b.proposal_id
  && a.tx_list_hash = b.tx_list_hash
  && a.state_root = b.state_root
  && a.creator_addr = b.creator_addr
  && a.txid_hi = b.txid_hi

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

let find_height base height =
  let target = path base in
  if not (Sys.file_exists target) then None
  else
    let input = open_in target in
    Fun.protect
      ~finally:(fun () -> close_in input)
      (fun () ->
        let rec loop () =
          match input_line input with
          | value ->
            let value = String.trim value in
            if value = "" then
              loop ()
            else
              let entry =
                entry_of_json (Yojson.Safe.from_string value)
              in
              if entry.height < height then
                loop ()
              else if entry.height = height then
                Some entry
              else
                None
          | exception End_of_file ->
            None
        in
        loop ())

let can_upgrade_catchup_placeholder prior entry =
  prior.height = entry.height
  && prior.round = entry.round
  && prior.txid_hi = -1L
  && prior.qc_hash = None
  && entry.creator_addr <> ""
  && (entry.txid_hi >= 0L || prior.creator_addr = "")
  && prior.tx_list_hash = entry.tx_list_hash
  && prior.state_root = entry.state_root

let replace_last base prior entry =
  match List.rev (read base) with
  | last :: rest when last = prior ->
    replace base (List.rev (entry :: rest))
  | _ ->
    failwith "finality log changed during replacement"

let check_write base entry =
  match last_entry_fast base with
  | Some prior when entry.height < prior.height ->
    begin
      match find_height base entry.height with
      | Some historical when same_commitment historical entry -> ()
      | Some _
      | None ->
        failwith "finality log height regression"
    end
  | Some prior when entry.height = prior.height ->
    if not
         (same_commitment prior entry
          || can_upgrade_catchup_placeholder prior entry)
    then
      failwith "conflicting finality at committed height"
  | Some _
  | None ->
    ()

let write base entry =
  ensure_dir base;
  check_write base entry;
  match last_entry_fast base with
  | Some prior when entry.height < prior.height ->
    ()
  | Some prior when same_commitment prior entry ->
    ()
  | Some prior when can_upgrade_catchup_placeholder prior entry ->
    replace_last base prior entry
  | Some prior when entry.height = prior.height ->
    failwith "conflicting finality at committed height"
  | Some _
  | None ->
    let p = path base in
    let fd =
      Unix.openfile p [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] 0o640
    in
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () ->
        write_all fd (line entry ^ "\n") 0;
        Unix.fsync fd)

let index base =
  let entries = Hashtbl.create 256 in
  List.iter (fun entry -> Hashtbl.replace entries entry.height entry) (read base);
  entries

let find entries height =
  Hashtbl.find_opt entries height

let drop_after base head =
  let entries = read base in
  let kept = List.filter (fun e -> e.height <= head) entries in
  if List.length kept = List.length entries then 0
  else begin
    replace base kept;
    List.length entries - List.length kept
  end

let drop_uncommitted_after base head =
  let entries = read base in
  let tail = List.filter (fun e -> e.height > head) entries in
  match List.find_opt (fun e -> Option.is_some e.qc_hash) tail with
  | Some entry ->
    Error
      (Printf.sprintf
         "certified finality above head height = %d head = %d"
         entry.height
         head)
  | None ->
    let kept = List.filter (fun e -> e.height <= head) entries in
    if List.length kept = List.length entries then Ok 0
    else begin
      replace base kept;
      Ok (List.length entries - List.length kept)
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

let of_catchup ?qc_hash ~chain_id ~txid_hi
    (record : C_codec.catchup_epoch_record) =
  if record.creator_addr = "" then
    failwith "catchup finality creator is missing";
  let header : C_types.epoch_header = {
    proto_version = C_types.proto_version_current;
    chain_id;
    epoch_id = record.epoch_id;
    prev_state_root = record.prev_state_root;
    tx_list_hash = record.tx_list_hash;
    receipt_root = record.receipt_root;
    proposed_state_root = record.state_root;
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = record.creator_addr;
    txid_hi;
    ts = record.epoch_ts;
  } in
  of_header ?qc_hash ~round:record.commit_round header