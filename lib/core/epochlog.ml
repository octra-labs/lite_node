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


let magic = "OEPL"
let version = 2
let header_size = 16

type proposer_info = {
  creator_addr : string;
  commit_round : int;
}

let empty_proposer_info = { creator_addr = ""; commit_round = 0 }

type reward_recipient = {
  reward_addr : string;
  reward_role : string;
  reward_amount : string;
}

type epoch_header = {
  id : int;
  state_root : string;
  prev_state_root : string;
  parent_commit : string;
  start_txid : int64;
  tx_count : int;
  finalized_by : string;
  finalized_at : float;
  proposer : proposer_info;
  fees_total : string;
  base_reward : string;
  total_reward : string;
  proposer_reward : string;
  validator_reward_each : string;
  reward_recipients : reward_recipient list;
}

let empty_epoch_header = {
  id = 0;
  state_root = "";
  prev_state_root = "";
  parent_commit = "";
  start_txid = 0L;
  tx_count = 0;
  finalized_by = "";
  finalized_at = 0.0;
  proposer = empty_proposer_info;
  fees_total = "0";
  base_reward = "0";
  total_reward = "0";
  proposer_reward = "0";
  validator_reward_each = "0";
  reward_recipients = [];
}

type t = {
  fd : Unix.file_descr;
  mutable offsets : (int, int) Hashtbl.t;
  mutable tip : epoch_header option;
}

let write_u32_le buf off v =
  Bytes.set_uint8 buf off (v land 0xFF);
  Bytes.set_uint8 buf (off+1) ((v lsr 8) land 0xFF);
  Bytes.set_uint8 buf (off+2) ((v lsr 16) land 0xFF);
  Bytes.set_uint8 buf (off+3) ((v lsr 24) land 0xFF)

let read_u32_le buf off =
  Bytes.get_uint8 buf off
  lor (Bytes.get_uint8 buf (off+1) lsl 8)
  lor (Bytes.get_uint8 buf (off+2) lsl 16)
  lor (Bytes.get_uint8 buf (off+3) lsl 24)

let checksum payload =
  let d = Digestif.SHA256.digest_string payload in
  String.sub (Digestif.SHA256.to_raw_string d) 0 4

let epoch_to_json h =
  let rewards_json =
    `List (List.map (fun r ->
      `Assoc [
        "address", `String r.reward_addr;
        "role", `String r.reward_role;
        "amount", `String r.reward_amount;
      ]) h.reward_recipients)
  in
  Yojson.Safe.to_string (`Assoc [
    "id", `Int h.id;
    "state_root", `String h.state_root;
    "prev_state_root", `String h.prev_state_root;
    "parent_commit", `String h.parent_commit;
    "start_txid", `String (Int64.to_string h.start_txid);
    "tx_count", `Int h.tx_count;
    "finalized_by", `String h.finalized_by;
    "finalized_at", `Float h.finalized_at;
    "creator_addr", `String h.proposer.creator_addr;
    "commit_round", `Int h.proposer.commit_round;
    "fees_total", `String h.fees_total;
    "base_reward", `String h.base_reward;
    "total_reward", `String h.total_reward;
    "proposer_reward", `String h.proposer_reward;
    "validator_reward_each", `String h.validator_reward_each;
    "reward_recipients", rewards_json;
  ])

let epoch_of_json s =
  try
    let j = Yojson.Safe.from_string s in
    let open Yojson.Safe.Util in
    let opt_string k =
      try j |> member k |> to_string with _ -> "" in
    let opt_amount k =
      try j |> member k |> to_string with _ -> "0" in
    let opt_int k =
      try j |> member k |> to_int with _ -> 0 in
    let reward_recipients =
      try
        j |> member "reward_recipients" |> to_list
        |> List.filter_map (fun row ->
          try
            Some {
              reward_addr = row |> member "address" |> to_string;
              reward_role = row |> member "role" |> to_string;
              reward_amount = row |> member "amount" |> to_string;
            }
          with _ -> None)
      with _ -> []
    in
    Some {
      id = j |> member "id" |> to_int;
      state_root = j |> member "state_root" |> to_string;
      prev_state_root = j |> member "prev_state_root" |> to_string;
      parent_commit = j |> member "parent_commit" |> to_string;
      start_txid = Int64.of_string (j |> member "start_txid" |> to_string);
      tx_count = j |> member "tx_count" |> to_int;
      finalized_by = j |> member "finalized_by" |> to_string;
      finalized_at = j |> member "finalized_at" |> to_number;
      proposer = {
        creator_addr = opt_string "creator_addr";
        commit_round = opt_int "commit_round";
      };
      fees_total = opt_amount "fees_total";
      base_reward = opt_amount "base_reward";
      total_reward = opt_amount "total_reward";
      proposer_reward = opt_amount "proposer_reward";
      validator_reward_each = opt_amount "validator_reward_each";
      reward_recipients;
    }
  with _ -> None

let write_file_header fd =
  let buf = Bytes.make header_size '\000' in
  Bytes.blit_string magic 0 buf 0 4;
  Bytes.set_uint8 buf 4 (version land 0xFF);
  Bytes.set_uint8 buf 5 ((version lsr 8) land 0xFF);
  let _ = Unix.write fd buf 0 header_size in
  ()

let validate_file_header fd =
  let buf = Bytes.create header_size in
  let _ = Unix.lseek fd 0 Unix.SEEK_SET in
  let n = Unix.read fd buf 0 header_size in
  if n < header_size then failwith "epochlog: truncated header";
  if Bytes.sub_string buf 0 4 <> magic then failwith "epochlog: bad magic";
  let v = Bytes.get_uint8 buf 4 lor (Bytes.get_uint8 buf 5 lsl 8) in
  if v <> 1 && v <> version then failwith (Printf.sprintf "epochlog: version %d not in [1,%d]" v version)

let scan_records fd =
  let offsets = Hashtbl.create 1024 in
  let tip = ref None in
  let file_size = Unix.lseek fd 0 Unix.SEEK_END in
  let _ = Unix.lseek fd header_size Unix.SEEK_SET in
  let pos = ref header_size in
  (try while !pos < file_size do
    let hdr = Bytes.create 4 in
    let n = Unix.read fd hdr 0 4 in
    if n < 4 then raise Exit;
    let record_len = read_u32_le hdr 0 in
    let payload_len = record_len - 4 in
    let record_buf = Bytes.create record_len in
    let rd = ref 0 in
    (try while !rd < record_len do
      let n = Unix.read fd record_buf !rd (record_len - !rd) in
      if n = 0 then raise Exit;
      rd := !rd + n
    done with Exit -> raise Exit);
    let payload = Bytes.sub_string record_buf 0 payload_len in
    let stored_chk = Bytes.sub_string record_buf payload_len 4 in
    let expected_chk = checksum payload in
    if stored_chk = expected_chk then begin
      match epoch_of_json payload with
      | Some h ->
        Hashtbl.replace offsets h.id !pos;
        tip := Some h
      | None -> ()
    end;
    pos := !pos + 4 + record_len
  done with Exit -> ());
  (offsets, !tip)

let open_log path =
  let dir = Filename.dirname path in
  if not (Sys.file_exists dir) then
    Unix.mkdir dir 0o755;
  if Sys.file_exists path then begin
    let fd = Unix.openfile path [Unix.O_RDWR; Unix.O_APPEND] 0o644 in
    validate_file_header fd;
    let (offsets, tip) = scan_records fd in
    { fd; offsets; tip }
  end else begin
    let fd = Unix.openfile path [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
    write_file_header fd;
    { fd; offsets = Hashtbl.create 64; tip = None }
  end

let close t =
  Unix.close t.fd

let append t h =
  let payload = epoch_to_json h in
  let payload_len = String.length payload in
  let record_len = payload_len + 4 in
  let chk = checksum payload in
  let buf = Bytes.create (4 + record_len) in
  write_u32_le buf 0 record_len;
  Bytes.blit_string payload 0 buf 4 payload_len;
  Bytes.blit_string chk 0 buf (4 + payload_len) 4;
  let pos = Unix.lseek t.fd 0 Unix.SEEK_END in
  let total = 4 + record_len in
  let written = ref 0 in
  while !written < total do
    let n = Unix.write t.fd buf !written (total - !written) in
    written := !written + n
  done;
  Hashtbl.replace t.offsets h.id pos;
  t.tip <- Some h

let read_all t =
  let _ = Unix.lseek t.fd 0 Unix.SEEK_END in
  let file_size = Unix.lseek t.fd 0 Unix.SEEK_END in
  let _ = Unix.lseek t.fd header_size Unix.SEEK_SET in
  let pos = ref header_size in
  let results = ref [] in
  (try while !pos < file_size do
    let hdr = Bytes.create 4 in
    let n = Unix.read t.fd hdr 0 4 in
    if n < 4 then raise Exit;
    let record_len = read_u32_le hdr 0 in
    let record_buf = Bytes.create record_len in
    let rd = ref 0 in
    (try while !rd < record_len do
      let n = Unix.read t.fd record_buf !rd (record_len - !rd) in
      if n = 0 then raise Exit;
      rd := !rd + n
    done with Exit -> raise Exit);
    let payload_len = record_len - 4 in
    let payload = Bytes.sub_string record_buf 0 payload_len in
    (match epoch_of_json payload with
     | Some h -> results := h :: !results
     | None -> ());
    pos := !pos + 4 + record_len
  done with Exit -> ());
  List.rev !results

let last t = t.tip

let current_offset t =
  try Unix.lseek t.fd 0 Unix.SEEK_END
  with _ -> 0

let offset_after t epoch_id =
  if epoch_id < 0 then Some header_size
  else if Hashtbl.mem t.offsets epoch_id then begin
    let off = Hashtbl.find t.offsets epoch_id in
    let _ = Unix.lseek t.fd off Unix.SEEK_SET in
    let hdr = Bytes.create 4 in
    let n = Unix.read t.fd hdr 0 4 in
    if n < 4 then None
    else
      let record_len = read_u32_le hdr 0 in
      Some (off + 4 + record_len)
  end else
    None

let fsync t =
  Unix.fsync t.fd

let truncate_to t ~offset =
  Unix.ftruncate t.fd offset;
  Unix.fsync t.fd;
  let (offsets, tip) = scan_records t.fd in
  t.offsets <- offsets;
  t.tip <- tip

let get t epoch_id =
  if Hashtbl.mem t.offsets epoch_id then begin
    let off = Hashtbl.find t.offsets epoch_id in
    let _ = Unix.lseek t.fd off Unix.SEEK_SET in
    let hdr = Bytes.create 4 in
    let n = Unix.read t.fd hdr 0 4 in
    if n < 4 then None
    else begin
      let record_len = read_u32_le hdr 0 in
      let payload_len = record_len - 4 in
      let record_buf = Bytes.create record_len in
      let rd = ref 0 in
      (try while !rd < record_len do
        let n = Unix.read t.fd record_buf !rd (record_len - !rd) in
        if n = 0 then raise Exit;
        rd := !rd + n
      done with Exit -> ());
      if !rd < record_len then None
      else
        let payload = Bytes.sub_string record_buf 0 payload_len in
        epoch_of_json payload
    end
  end else
    None