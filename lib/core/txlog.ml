(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let magic = "OTXL"
let version = 1
let header_size = 16
let max_segment_size = 1_073_741_824

type t = {
  dir : string;
  readonly : bool;
  mutable current_seg : int;
  mutable current_fd : Unix.file_descr;
  mutable current_offset : int;
}

let seg_path dir seg_id =
  Filename.concat dir (Printf.sprintf "seg%06d.dat" seg_id)

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

let checksum epoch_bytes payload =
  let d = Digestif.SHA256.digest_string (epoch_bytes ^ payload) in
  String.sub (Digestif.SHA256.to_raw_string d) 0 4

let write_header fd seg_id =
  let buf = Bytes.make header_size '\000' in
  Bytes.blit_string magic 0 buf 0 4;
  Bytes.set_uint8 buf 4 (version land 0xFF);
  Bytes.set_uint8 buf 5 ((version lsr 8) land 0xFF);
  write_u32_le buf 6 seg_id;
  let _ = Unix.write fd buf 0 header_size in
  ()

let validate_header fd =
  let buf = Bytes.create header_size in
  let _ = Unix.lseek fd 0 Unix.SEEK_SET in
  let n = Unix.read fd buf 0 header_size in
  if n < header_size then failwith "txlog: truncated header";
  if Bytes.sub_string buf 0 4 <> magic then failwith "txlog: bad magic";
  let v = Bytes.get_uint8 buf 4 lor (Bytes.get_uint8 buf 5 lsl 8) in
  if v <> version then failwith (Printf.sprintf "txlog: version %d != %d" v version);
  read_u32_le buf 6

let open_segment ?(readonly=false) dir seg_id =
  let path = seg_path dir seg_id in
  if Sys.file_exists path then begin
    let flags = if readonly then [Unix.O_RDONLY] else [Unix.O_RDWR] in
    let fd = Unix.openfile path flags 0o644 in
    let _seg = validate_header fd in
    let off = Unix.lseek fd 0 Unix.SEEK_END in
    (fd, off)
  end else if readonly then begin
    failwith "txlog: read-only segment is missing"
  end else begin
    let fd = Unix.openfile path [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
    write_header fd seg_id;
    (fd, header_size)
  end

let parse_segment_id filename =
  let len = String.length filename in
  if len <= 7 || String.sub filename 0 3 <> "seg" || String.sub filename (len - 4) 4 <> ".dat"
  then None
  else
    let digits_len = len - 7 in
    let rec digits_only i =
      if i >= digits_len then true
      else
        match filename.[3 + i] with
        | '0' .. '9' -> digits_only (i + 1)
        | _ -> false
    in
    if not (digits_only 0) then None
    else
      try Some (int_of_string (String.sub filename 3 digits_len))
      with _ -> None

let looks_like_segment_filename filename =
  let len = String.length filename in
  len > 7
  && String.sub filename 0 3 = "seg"
  && String.sub filename (len - 4) 4 = ".dat"

let find_latest_segment ?(readonly=false) dir =
  if not (Sys.file_exists dir) then begin
    if readonly then failwith "txlog: read-only directory is missing"
    else begin
      Unix.mkdir dir 0o755;
      0
    end
  end else
    let files = Sys.readdir dir in
    let max_seg = ref (-1) in
    let malformed = ref [] in
    Array.iter (fun f ->
      match parse_segment_id f with
      | Some n when n > !max_seg -> max_seg := n
      | Some _ -> ()
      | None when looks_like_segment_filename f -> malformed := f :: !malformed
      | None -> ()
    ) files;
    if !malformed <> [] then
      failwith
        (Printf.sprintf "txlog: malformed segment filename(s): %s"
           (String.concat ", " (List.rev !malformed)));
    if !max_seg < 0 then 0 else !max_seg

let open_log ?(readonly=false) dir =
  let seg_id = find_latest_segment ~readonly dir in
  let (fd, off) = open_segment ~readonly dir seg_id in
  { dir; readonly; current_seg = seg_id; current_fd = fd; current_offset = off }

let close t =
  Unix.close t.current_fd

let rotate t =
  Unix.close t.current_fd;
  let new_seg = t.current_seg + 1 in
  let (fd, off) = open_segment t.dir new_seg in
  t.current_seg <- new_seg;
  t.current_fd <- fd;
  t.current_offset <- off

let ensure_physical_eof_matches t =
  let actual = Unix.lseek t.current_fd 0 Unix.SEEK_END in
  if actual <> t.current_offset then
    failwith (Printf.sprintf
      "txlog: physical EOF drift seg=%d expected=%d actual=%d"
      t.current_seg t.current_offset actual)

let rec append t ~epoch_id ~payload =
  if t.readonly then failwith "txlog: append on read-only log";
  ensure_physical_eof_matches t;
  if t.current_offset >= max_segment_size then rotate t;
  ensure_physical_eof_matches t;
  let payload_len = String.length payload in
  let record_len = 4 + payload_len + 4 in
  let epoch_bytes = Bytes.create 4 in
  write_u32_le epoch_bytes 0 epoch_id;
  let epoch_str = Bytes.to_string epoch_bytes in
  let chk = checksum epoch_str payload in
  let buf = Bytes.create (4 + record_len) in
  write_u32_le buf 0 record_len;
  Bytes.blit_string epoch_str 0 buf 4 4;
  Bytes.blit_string payload 0 buf 8 payload_len;
  Bytes.blit_string chk 0 buf (8 + payload_len) 4;
  let seg_id = t.current_seg in
  let offset = t.current_offset in
  let total = 4 + record_len in
  let actual = Unix.lseek t.current_fd 0 Unix.SEEK_END in
  if actual <> offset then
    failwith (Printf.sprintf
      "txlog: append offset drift seg=%d expected=%d actual=%d"
      seg_id offset actual);
  ignore (Unix.lseek t.current_fd offset Unix.SEEK_SET);
  let written = ref 0 in
  while !written < total do
    let n = Unix.write t.current_fd buf !written (total - !written) in
    if n = 0 then failwith "txlog: short write";
    written := !written + n
  done;
  let expected_end = offset + total in
  let actual_end = Unix.lseek t.current_fd 0 Unix.SEEK_END in
  if actual_end <> expected_end then
    failwith (Printf.sprintf
      "txlog: append end drift seg=%d expected_end=%d actual_end=%d"
      seg_id expected_end actual_end);
  t.current_offset <- expected_end;
  let (stored_epoch, stored_payload) = read_record t ~seg_id ~offset ~len:record_len in
  if stored_epoch <> epoch_id || stored_payload <> payload then
    failwith (Printf.sprintf
      "txlog: append readback mismatch seg=%d offset=%d epoch=%d stored_epoch=%d"
      seg_id offset epoch_id stored_epoch);
  (seg_id, offset, record_len)

and read_record t ~seg_id ~offset ~len =
  let fd =
    if seg_id = t.current_seg then t.current_fd
    else Unix.openfile (seg_path t.dir seg_id) [Unix.O_RDONLY] 0
  in
  let total = 4 + len in
  let buf = Bytes.create total in
  let _ = Unix.lseek fd offset Unix.SEEK_SET in
  let rd = ref 0 in
  while !rd < total do
    let n = Unix.read fd buf !rd (total - !rd) in
    if n = 0 then failwith "txlog: unexpected EOF";
    rd := !rd + n
  done;
  if seg_id <> t.current_seg then Unix.close fd;
  let stored_len = read_u32_le buf 0 in
  if stored_len <> len then failwith "txlog: record_len mismatch";
  let epoch_id = read_u32_le buf 4 in
  let payload_len = len - 4 - 4 in
  let payload = Bytes.sub_string buf 8 payload_len in
  let epoch_bytes = Bytes.sub_string buf 4 4 in
  let stored_chk = Bytes.sub_string buf (8 + payload_len) 4 in
  let expected_chk = checksum epoch_bytes payload in
  if stored_chk <> expected_chk then failwith "txlog: checksum mismatch";
  (epoch_id, payload)

let read_record_prefix t ~seg_id ~offset ~len ~prefix_len =
  let fd =
    if seg_id = t.current_seg then t.current_fd
    else Unix.openfile (seg_path t.dir seg_id) [Unix.O_RDONLY] 0
  in
  let payload_len = max 0 (len - 8) in
  let prefix_len = min payload_len (max 0 prefix_len) in
  let total = 8 + prefix_len in
  let buf = Bytes.create total in
  let close_needed = seg_id <> t.current_seg in
  try
    let _ = Unix.lseek fd offset Unix.SEEK_SET in
    let rd = ref 0 in
    while !rd < total do
      let n = Unix.read fd buf !rd (total - !rd) in
      if n = 0 then failwith "txlog: unexpected EOF";
      rd := !rd + n
    done;
    if close_needed then Unix.close fd;
    let stored_len = read_u32_le buf 0 in
    if stored_len <> len then failwith "txlog: record_len mismatch";
    let epoch_id = read_u32_le buf 4 in
    let payload_prefix = Bytes.sub_string buf 8 prefix_len in
    (epoch_id, payload_prefix)
  with e ->
    if close_needed then (try Unix.close fd with _ -> ());
    raise e

let fsync t =
  if not t.readonly then Unix.fsync t.current_fd

let scan_all t f =
  let seg_id = ref 0 in
  while Sys.file_exists (seg_path t.dir !seg_id) do
    let fd = Unix.openfile (seg_path t.dir !seg_id) [Unix.O_RDONLY] 0 in
    let _seg = validate_header fd in
    let file_size = Unix.lseek fd 0 Unix.SEEK_END in
    let pos = ref header_size in
    let _ = Unix.lseek fd header_size Unix.SEEK_SET in
    (try while !pos < file_size do
      let hdr = Bytes.create 4 in
      let n = Unix.read fd hdr 0 4 in
      if n < 4 then raise Exit;
      let record_len = read_u32_le hdr 0 in
      let record_buf = Bytes.create record_len in
      let rd = ref 0 in
      (try while !rd < record_len do
        let n = Unix.read fd record_buf !rd (record_len - !rd) in
        if n = 0 then raise Exit;
        rd := !rd + n
      done with Exit -> raise Exit);
      let epoch_id = read_u32_le record_buf 0 in
      let payload_len = record_len - 4 - 4 in
      let payload = Bytes.sub_string record_buf 4 payload_len in
      f !seg_id !pos record_len epoch_id payload;
      pos := !pos + 4 + record_len
    done with Exit -> ());
    Unix.close fd;
    incr seg_id
  done

let current_position t =
  (t.current_seg, t.current_offset)

let truncate_to t ~seg_id ~offset =
  if t.readonly then failwith "txlog: truncate on read-only log";
  Unix.close t.current_fd;
  let later = ref (seg_id + 1) in
  while Sys.file_exists (seg_path t.dir !later) do
    Sys.remove (seg_path t.dir !later);
    incr later
  done;
  let path = seg_path t.dir seg_id in
  if Sys.file_exists path then
    Unix.truncate path offset;
  let (fd, off) = open_segment t.dir seg_id in
  t.current_seg <- seg_id;
  t.current_fd <- fd;
  t.current_offset <- off