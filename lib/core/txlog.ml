(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let magic = "OTXL"
let version = 1
let header_size = 16
let max_segment_size = 1_073_741_824
let max_record_len = 128_000_000

type scan_record = {
  segment : int;
  offset : int;
  length : int;
  epoch : int;
  payload : string;
}

type scan_error =
  | Segment_missing of int
  | Header_truncated of int
  | Header_marker of int
  | Header_version of int * int
  | Header_segment of int * int
  | Prefix_truncated of int * int
  | Length_invalid of int * int * int
  | Record_truncated of int * int
  | Checksum_mismatch of int * int
  | Record_rejected of int * int * string
  | Scan_io of string

type t = {
  dir : string;
  readonly : bool;
  mutable current_seg : int;
  mutable current_fd : Unix.file_descr;
  mutable current_offset : int;
  mutable cut : (int * int) option;
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

let scan_error_message = function
  | Segment_missing segment ->
      Printf.sprintf "txlog segment is missing: %d" segment
  | Header_truncated segment ->
      Printf.sprintf "txlog header is truncated: %d" segment
  | Header_marker segment ->
      Printf.sprintf "txlog header marker is invalid: segment = %d" segment
  | Header_version (segment, actual) ->
      Printf.sprintf
        "txlog header format is invalid: segment = %d value = %d"
        segment
        actual
  | Header_segment (expected, actual) ->
      Printf.sprintf
        "txlog header segment mismatch: expected = %d actual = %d"
        expected
        actual
  | Prefix_truncated (segment, offset) ->
      Printf.sprintf
        "txlog record prefix is truncated: segment = %d offset = %d"
        segment
        offset
  | Length_invalid (segment, offset, length) ->
      Printf.sprintf
        "txlog record length is invalid: segment = %d offset = %d length = %d"
        segment offset length
  | Record_truncated (segment, offset) ->
      Printf.sprintf
        "txlog record is truncated: segment = %d offset = %d"
        segment
        offset
  | Checksum_mismatch (segment, offset) ->
      Printf.sprintf
        "txlog checksum mismatch: segment = %d offset = %d"
        segment
        offset
  | Record_rejected (segment, offset, reason) ->
      Printf.sprintf
        "txlog record rejected: segment = %d offset = %d reason = %s"
        segment offset reason
  | Scan_io reason -> "txlog scan failed: " ^ reason

let read_exact fd buffer offset length =
  let rec loop position remaining =
    if remaining = 0 then true
    else
      let count = Unix.read fd buffer position remaining in
      count > 0 && loop (position + count) (remaining - count)
  in
  loop offset length

let record_length_valid ~remaining length =
  length >= 8 && length <= max_record_len && length <= remaining

let read_scan_record fd ~segment ~offset ~size ~check_checksum =
  if size - offset < 4 then Error (Prefix_truncated (segment, offset))
  else
    let prefix = Bytes.create 4 in
    ignore (Unix.lseek fd offset Unix.SEEK_SET);
    if not (read_exact fd prefix 0 4) then
      Error (Prefix_truncated (segment, offset))
    else
      let length = read_u32_le prefix 0 in
      if not (record_length_valid ~remaining:(size - offset - 4) length) then
        Error (Length_invalid (segment, offset, length))
      else
        let bytes = Bytes.create length in
        if not (read_exact fd bytes 0 length) then
          Error (Record_truncated (segment, offset))
        else
          let epoch = read_u32_le bytes 0 in
          let payload_length = length - 8 in
          let payload = Bytes.sub_string bytes 4 payload_length in
          if check_checksum
             && Bytes.sub_string bytes (4 + payload_length) 4
                <> checksum (Bytes.sub_string bytes 0 4) payload then
            Error (Checksum_mismatch (segment, offset))
          else
            Ok { segment; offset; length; epoch; payload }

let write_header ?(write = Unix.write) fd seg_id =
  let buf = Bytes.make header_size '\000' in
  Bytes.blit_string magic 0 buf 0 4;
  Bytes.set_uint8 buf 4 (version land 0xFF);
  Bytes.set_uint8 buf 5 ((version lsr 8) land 0xFF);
  write_u32_le buf 6 seg_id;
  let rec loop offset =
    if offset < header_size then
      match write fd buf offset (header_size - offset) with
      | 0 -> failwith "txlog: header write made no progress"
      | count -> loop (offset + count)
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop offset
  in
  loop 0

let validate_header fd =
  let buf = Bytes.create header_size in
  let _ = Unix.lseek fd 0 Unix.SEEK_SET in
  let n = Unix.read fd buf 0 header_size in
  if n < header_size then failwith "txlog: truncated header";
  if Bytes.sub_string buf 0 4 <> magic then failwith "txlog: bad magic";
  let v = Bytes.get_uint8 buf 4 lor (Bytes.get_uint8 buf 5 lsl 8) in
  if v <> version then failwith (Printf.sprintf "txlog: version %d != %d" v version);
  read_u32_le buf 6

let create_segment ?write ?(sync = Unix.fsync) dir seg_id =
  let path = seg_path dir seg_id in
  let rec reserve attempt =
    let staged = Printf.sprintf "%s.staged.%d.%d" path (Unix.getpid ()) attempt in
    match Unix.openfile staged [Unix.O_RDWR; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC] 0o644 with
    | fd -> staged, fd
    | exception Unix.Unix_error (Unix.EEXIST, _, _) -> reserve (attempt + 1)
  in
  let staged, fd = reserve 0 in
  Store_scope.protect ~close:(fun () -> Unix.close fd) (fun () ->
    Fun.protect ~finally:(fun () ->
      try Unix.unlink staged with Unix.Unix_error (Unix.ENOENT, _, _) -> ()) (fun () ->
      write_header ?write fd seg_id;
      sync fd;
      Unix.link staged path;
      Unix.unlink staged;
      let parent = Unix.openfile dir [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
      Fun.protect ~finally:(fun () -> Unix.close parent) (fun () -> sync parent);
      fd, header_size))

let open_segment ?(readonly = false) dir seg_id =
  let path = seg_path dir seg_id in
  let exists = Sys.file_exists path in
  if not exists && readonly then failwith "txlog: read-only segment is missing";
  if not exists then create_segment dir seg_id
  else
    let flags = if readonly then [Unix.O_RDONLY] else [Unix.O_RDWR] in
    let fd = Unix.openfile path (Unix.O_CLOEXEC :: flags) 0o644 in
    Store_scope.protect ~close:(fun () -> Unix.close fd) (fun () ->
      if validate_header fd <> seg_id then failwith "txlog: segment header identity differs";
      fd, Unix.lseek fd 0 Unix.SEEK_END)

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

let open_log ?retained ?(readonly=false) dir =
  let seg_id = find_latest_segment ~readonly dir in
  let cut = match retained with
    | Some (segment, offset) when segment >= 0 && segment < seg_id && offset >= header_size ->
      let stat = Unix.lstat (seg_path dir seg_id) in
      if stat.Unix.st_kind = Unix.S_REG && stat.st_size < header_size then retained else None
    | _ -> None in
  let fd, off = match cut with
    | None -> open_segment ~readonly dir seg_id
    | Some _ ->
      let flags = if readonly then [Unix.O_RDONLY; Unix.O_CLOEXEC] else [Unix.O_RDWR; Unix.O_CLOEXEC] in
      let fd = Unix.openfile (seg_path dir seg_id) flags 0 in
      Store_scope.protect ~close:(fun () -> Unix.close fd) (fun () ->
        let stat = Unix.fstat fd in
        if stat.Unix.st_kind <> Unix.S_REG || stat.st_size >= header_size then
          failwith "txlog: incomplete segment changed during open";
        fd, stat.st_size) in
  { dir; readonly; current_seg = seg_id; current_fd = fd; current_offset = off; cut }

let close t =
  Unix.close t.current_fd

let rotate ?(sync = Unix.fsync) t =
  if t.readonly then failwith "txlog: rotate on read-only log";
  if t.cut <> None then failwith "txlog: cut is incomplete";
  sync t.current_fd;
  let new_seg = t.current_seg + 1 in
  let (fd, off) = open_segment t.dir new_seg in
  (match Unix.close t.current_fd with
   | () -> ()
   | exception error ->
     let trace = Printexc.get_raw_backtrace () in
     Unix.close fd;
     Printexc.raise_with_backtrace error trace);
  t.current_seg <- new_seg;
  t.current_fd <- fd;
  t.current_offset <- off

let ensure_physical_eof_matches t =
  let actual = Unix.lseek t.current_fd 0 Unix.SEEK_END in
  if actual <> t.current_offset then
    failwith (Printf.sprintf
      "txlog: physical EOF drift seg = %d expected = %d actual = %d"
      t.current_seg t.current_offset actual)

let read_at t ~seg_id ~offset length =
  let read fd =
    let bytes = Bytes.create length in
    ignore (Unix.lseek fd offset Unix.SEEK_SET);
    if not (read_exact fd bytes 0 length) then failwith "txlog: unexpected EOF";
    bytes
  in
  if seg_id = t.current_seg then read t.current_fd
  else
    let fd = Unix.openfile (seg_path t.dir seg_id) [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> read fd)

let read_location ~dir ~seg_id ~offset ~len =
  if seg_id < 0 || offset < header_size || len < 8 || len > max_record_len then
    failwith "txlog: invalid read location";
  let fd = Unix.openfile (seg_path dir seg_id)
    [Unix.O_RDONLY; Unix.O_CLOEXEC; Unix.O_NONBLOCK] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    let stat = Unix.fstat fd in
    if stat.Unix.st_kind <> Unix.S_REG then failwith "txlog: segment is not regular";
    if validate_header fd <> seg_id then failwith "txlog: segment header identity differs";
    if offset > stat.st_size || stat.st_size - offset < 4
       || len > stat.st_size - offset - 4 then
      failwith "txlog: read location exceeds segment";
    let prefix = Bytes.create 4 in
    ignore (Unix.lseek fd offset Unix.SEEK_SET);
    if not (read_exact fd prefix 0 4) || read_u32_le prefix 0 <> len then
      failwith "txlog: record_len mismatch";
    match read_scan_record fd ~segment:seg_id ~offset ~size:stat.st_size
      ~check_checksum:true with
    | Ok record when record.length = len -> record.epoch, record.payload
    | Ok _ -> failwith "txlog: record_len mismatch"
    | Error error -> failwith (scan_error_message error))

let rec append t ~epoch_id ~payload =
  if t.readonly then failwith "txlog: append on read-only log";
  if t.cut <> None then failwith "txlog: cut is incomplete";
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
      "txlog: append offset drift seg = %d expected = %d actual = %d"
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
      "txlog: append end drift seg = %d expected_end = %d actual_end = %d"
      seg_id expected_end actual_end);
  t.current_offset <- expected_end;
  let (stored_epoch, stored_payload) = read_record t ~seg_id ~offset ~len:record_len in
  if stored_epoch <> epoch_id || stored_payload <> payload then
    failwith (Printf.sprintf
      "txlog: append readback mismatch seg = %d offset = %d epoch = %d stored_epoch = %d"
      seg_id offset epoch_id stored_epoch);
  (seg_id, offset, record_len)

and read_record t ~seg_id ~offset ~len =
  let buf = read_at t ~seg_id ~offset (4 + len) in
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
  let payload_len = max 0 (len - 8) in
  let prefix_len = min payload_len (max 0 prefix_len) in
  let buf = read_at t ~seg_id ~offset (8 + prefix_len) in
  let stored_len = read_u32_le buf 0 in
  if stored_len <> len then failwith "txlog: record_len mismatch";
  let epoch_id = read_u32_le buf 4 in
  let payload_prefix = Bytes.sub_string buf 8 prefix_len in
  (epoch_id, payload_prefix)

let fsync ?(sync = Unix.fsync) t =
  if not t.readonly then begin
    sync t.current_fd;
    List.iter (fun path ->
      let fd = Unix.openfile path [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
      Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> sync fd))
      [t.dir; Filename.dirname t.dir]
  end

let scan_all t f =
  let seg_id = ref 0 in
  while Sys.file_exists (seg_path t.dir !seg_id) do
    let fd = Unix.openfile (seg_path t.dir !seg_id) [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      let _seg = validate_header fd in
      let file_size = Unix.lseek fd 0 Unix.SEEK_END in
      let pos = ref header_size in
      let _ = Unix.lseek fd header_size Unix.SEEK_SET in
      try while !pos < file_size do
        match read_scan_record fd ~segment:!seg_id ~offset:!pos ~size:file_size
                ~check_checksum:false with
        | Error (Prefix_truncated _ | Record_truncated _) -> raise Exit
        | Error (Length_invalid (_, _, length))
          when length >= 8 && length <= max_record_len -> raise Exit
        | Error error -> failwith (scan_error_message error)
        | Ok record ->
          f record.segment record.offset record.length record.epoch record.payload;
          pos := !pos + 4 + record.length
      done with Exit -> ());
    incr seg_id
  done

let fold_strict ?end_at t ~init ~f =
  let last = match end_at with None -> t.current_seg | Some (segment, _) -> segment in
  let inspect_segment state segment =
    let path = seg_path t.dir segment in
    if not (Sys.file_exists path) then Error (Segment_missing segment)
    else
      let fd = Unix.openfile path [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          let physical = (Unix.fstat fd).Unix.st_size in
          let size = match end_at with
            | Some (last, offset) when segment = last -> offset
            | _ -> physical in
          let header = Bytes.create header_size in
          ignore (Unix.lseek fd 0 Unix.SEEK_SET);
          if size > physical then Error (Scan_io "transaction cut exceeds file length")
          else if size < header_size || not (read_exact fd header 0 header_size) then
            Error (Header_truncated segment)
          else if Bytes.sub_string header 0 4 <> magic then
            Error (Header_marker segment)
          else
            let actual_version =
              Bytes.get_uint8 header 4 lor (Bytes.get_uint8 header 5 lsl 8)
            in
            if actual_version <> version then
              Error (Header_version (segment, actual_version))
            else
              let actual_segment = read_u32_le header 6 in
              if actual_segment <> segment then
                Error (Header_segment (segment, actual_segment))
              else
                let rec records state offset =
                  if offset = size then Ok state
                  else
                    match read_scan_record fd ~segment ~offset ~size
                            ~check_checksum:true with
                    | Error _ as error -> error
                    | Ok record ->
                      match f state record with
                      | Error reason -> Error (Record_rejected (segment, offset, reason))
                      | Ok state -> records state (offset + 4 + record.length)
                in
                records state header_size)
  in
  let rec segments state segment =
    if segment > last then Ok state
    else
      match inspect_segment state segment with
      | Error _ as error -> error
      | Ok state -> segments state (segment + 1)
  in
  try
    if last < 0 || last > t.current_seg then Error (Segment_missing last)
    else segments init 0
  with
  | Unix.Unix_error (error, call, path) ->
      Error (Scan_io (Printf.sprintf "%s: %s: %s" call path (Unix.error_message error)))
  | Sys_error reason -> Error (Scan_io reason)

let current_position t =
  (t.current_seg, t.current_offset)

let truncate_to ?(sync = Unix.fsync) ?(remove = Unix.unlink) t ~seg_id ~offset =
  if t.readonly then failwith "txlog: truncate on read-only log";
  let target = seg_id, offset in
  if Option.fold ~none:false ~some:((<>) target) t.cut then
    failwith "txlog: a different cut is incomplete";
  (match fold_strict ~end_at:target t ~init:() ~f:(fun () _ -> Ok ()) with
   | Ok () -> ()
   | Error reason -> failwith (scan_error_message reason));
  let later = Sys.readdir t.dir |> Array.to_list |> List.filter_map (fun name ->
    match parse_segment_id name with
    | Some segment when segment > seg_id ->
      let path = seg_path t.dir segment in
      if Filename.basename path <> name || (Unix.lstat path).Unix.st_kind <> Unix.S_REG then
        failwith "txlog: invalid segment path";
      Some (segment, path)
    | _ -> None) |> List.sort (fun (left, _) (right, _) -> compare right left) in
  let fd = Unix.openfile (seg_path t.dir seg_id) [Unix.O_RDWR; Unix.O_CLOEXEC] 0 in
  let owned = ref false in
  Fun.protect ~finally:(fun () -> if not !owned then Unix.close fd) (fun () ->
    t.cut <- Some target;
    Unix.ftruncate fd offset;
    sync fd;
    List.iter (fun (_, path) -> remove path) later;
    List.iter (fun path ->
      let dir = Unix.openfile path [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
      Fun.protect ~finally:(fun () -> Unix.close dir) (fun () -> sync dir))
      [t.dir; Filename.dirname t.dir];
    let previous = t.current_fd in
    t.current_fd <- fd;
    t.current_seg <- seg_id;
    t.current_offset <- offset;
    owned := true;
    Unix.close previous;
    t.cut <- None)