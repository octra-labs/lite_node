(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module T = Octra_core.Txlog

let expect name value = if not value then failwith name

let descriptors () =
  let path = if Sys.file_exists "/proc/self/fd" then "/proc/self/fd" else "/dev/fd" in
  Array.length (Sys.readdir path)

let failure run =
  match run () with
  | _ -> None
  | exception Failure text -> Some (`Failure text)
  | exception Invalid_argument _ -> Some `Argument
  | exception Unix.Unix_error (code, _, _) -> Some (`Unix code)
  | exception Not_found -> Some `Missing

let released name expected run =
  let before = descriptors () in
  List.iter (fun _ -> expect (name ^ " error") (failure run = expected))
    (List.init 8 Fun.id);
  expect (name ^ " descriptor count") (descriptors () = before)

let with_log name run =
  let parent = "runtime_data" in
  (try Unix.mkdir parent 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let dir = Filename.concat parent (Printf.sprintf "txlog-%d-%s" (Unix.getpid ()) name) in
  Unix.mkdir dir 0o700;
  Fun.protect
    ~finally:(fun () ->
      Array.iter (fun file -> Unix.unlink (Filename.concat dir file)) (Sys.readdir dir);
      Unix.rmdir dir)
    (fun () ->
      let log = T.open_log dir in
      Fun.protect ~finally:(fun () -> T.close log) (fun () -> run dir log))

let test_reads () =
  with_log "reads" (fun dir log ->
    let first = T.append log ~epoch_id:7 ~payload:"first record" in
    T.rotate log;
    let last = T.append log ~epoch_id:8 ~payload:"last record" in
    let readers = [
      "full", (fun ~seg_id ~offset ~len -> T.read_record log ~seg_id ~offset ~len);
      "prefix", (fun ~seg_id ~offset ~len ->
        T.read_record_prefix log ~seg_id ~offset ~len ~prefix_len:4096);
    ] in
    List.iter (fun ((seg_id, offset, len), epoch, payload) ->
      let path = T.seg_path dir seg_id in
      let bytes = Digest.file path in
      List.iter (fun (name, read) ->
        expect (name ^ " valid record") (read ~seg_id ~offset ~len = (epoch, payload));
        released (name ^ " eof") (Some (`Failure "txlog: unexpected EOF"))
          (fun () -> read ~seg_id ~offset:(offset + 4 + len) ~len);
        released (name ^ " seek") (Some (`Unix Unix.EINVAL))
          (fun () -> read ~seg_id ~offset:(-1) ~len);
        released (name ^ " length") (Some (`Failure "txlog: record_len mismatch"))
          (fun () -> read ~seg_id ~offset ~len:(len - 1));
        expect (name ^ " read after error") (read ~seg_id ~offset ~len = (epoch, payload));
        expect (name ^ " content preserved") (Digest.file path = bytes)) readers;
      released "negative allocation" (Some `Argument)
        (fun () -> T.read_record log ~seg_id ~offset ~len:(-5))) [
      first, 7, "first record";
      last, 8, "last record";
    ];
    let seg_id, offset, len = T.append log ~epoch_id:9 ~payload:"after errors" in
    expect "current descriptor retained"
      (T.read_record log ~seg_id ~offset ~len = (9, "after errors")))

let test_open () =
  with_log "open" (fun dir log ->
    let path = T.seg_path dir 1 in
    let write bytes =
      let output = open_out_bin path in
      Fun.protect ~finally:(fun () -> close_out_noerr output)
        (fun () -> output_string output bytes)
    in
    List.iter (fun (bytes, message) ->
      write bytes;
      let stored = Digest.file path in
      List.iter (fun readonly ->
        released "invalid header" (Some (`Failure message))
          (fun () -> T.open_segment ~readonly dir 1);
        released "invalid log header" (Some (`Failure message))
          (fun () -> T.open_log ~readonly dir)) [false; true];
      expect "invalid header unchanged" (Digest.file path = stored)) [
      "short", "txlog: truncated header";
      String.make T.header_size '\000', "txlog: bad magic";
      "OTXL\002\000" ^ String.make 10 '\000', "txlog: version 2 != 1";
    ];
    Unix.unlink path;
    let fd, offset = T.open_segment dir 1 in
    Fun.protect ~finally:(fun () -> Unix.close fd)
      (fun () -> expect "new segment header"
        (offset = T.header_size && (Unix.fstat fd).Unix.st_kind = Unix.S_REG));
    let fd, offset = T.open_segment ~readonly:true dir 1 in
    Fun.protect ~finally:(fun () -> Unix.close fd)
      (fun () -> expect "existing segment header"
        (offset = T.header_size && (Unix.fstat fd).Unix.st_kind = Unix.S_REG));
    ignore (T.append log ~epoch_id:1 ~payload:"current"))

let test_scan () =
  with_log "scan" (fun dir log ->
    ignore (T.append log ~epoch_id:1 ~payload:"one");
    let bytes = Digest.file (T.seg_path dir 0) in
    released "scan callback" (Some `Missing)
      (fun () -> T.scan_all log (fun _ _ _ _ _ -> raise Not_found));
    expect "scan bytes preserved" (Digest.file (T.seg_path dir 0) = bytes);
    let records = ref [] in
    T.scan_all log (fun segment offset length epoch payload ->
      records := (segment, offset, length, epoch, payload) :: !records);
    expect "scan still reads" (!records = [0, T.header_size, 11, 1, "one"]);
    let fd = Unix.openfile (T.seg_path dir 0) [Unix.O_RDWR] 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      expect "write changed header" (Unix.write_substring fd "NONE" 0 4 = 4);
      Fun.protect ~finally:(fun () ->
        ignore (Unix.lseek fd 0 Unix.SEEK_SET);
        expect "restore header" (Unix.write_substring fd T.magic 0 4 = 4))
        (fun () -> released "scan header" (Some (`Failure "txlog: bad magic"))
          (fun () -> T.scan_all log (fun _ _ _ _ _ -> failwith "unexpected record"))));
    expect "header restored" (Digest.file (T.seg_path dir 0) = bytes);
    ignore (T.append log ~epoch_id:2 ~payload:"after scan"))

let test_lengths () =
  expect "minimum record" (T.record_length_valid ~remaining:8 8);
  expect "remaining file limit" (not (T.record_length_valid ~remaining:8 9));
  expect "existing record cap"
    (T.record_length_valid ~remaining:T.max_record_len T.max_record_len);
  expect "file-backed record cap"
    (not (T.record_length_valid ~remaining:(T.max_record_len + 1)
      (T.max_record_len + 1)));
  with_log "lengths" (fun dir log ->
    ignore (T.append log ~epoch_id:1 ~payload:"");
    let segment, offset, length = T.append log ~epoch_id:2 ~payload:"two" in
    T.rotate log;
    ignore (T.append log ~epoch_id:3 ~payload:"three");
    let scan () =
      let rows = ref [] in
      T.scan_all log (fun segment offset length epoch payload ->
        rows := (segment, offset, length, epoch, payload) :: !rows);
      List.rev !rows
    in
    let strict () =
      T.fold_strict log ~init:[] ~f:(fun rows record ->
        Ok (rows @ [record.T.segment, record.offset, record.length,
          record.epoch, record.payload]))
    in
    let first = 0, T.header_size, 8, 1, "" in
    let last = 1, T.header_size, 13, 3, "three" in
    expect "scan record parity"
      (scan () = [first; segment, offset, length, 2, "two"; last]);
    expect "strict record parity" (strict () = Ok (scan ()));
    let path = T.seg_path dir segment in
    let fd = Unix.openfile path [Unix.O_RDWR] 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      let prefix = Bytes.create 4 in
      List.iter (fun (invalid, remaining) ->
        Unix.ftruncate fd (offset + 4 + remaining);
        T.write_u32_le prefix 0 invalid;
        ignore (Unix.lseek fd offset Unix.SEEK_SET);
        expect "write record length" (Unix.write fd prefix 0 4 = 4);
        let before = Gc.allocated_bytes () in
        let error = T.Length_invalid (segment, offset, invalid) in
        released "scan rejects length" (Some (`Failure (T.scan_error_message error))) scan;
        expect "strict rejects length"
          (strict () = Error error);
        expect "invalid length allocation" (Gc.allocated_bytes () -. before < 100_000.))
        (List.map (fun invalid -> invalid, length)
           (List.init 8 Fun.id @ [T.max_record_len + 1; 0xFFFF_FFFF])
         @ [T.max_record_len + 1, T.max_record_len + 1]);
      Unix.ftruncate fd (offset + 4 + length);
      List.iter (fun declared ->
        T.write_u32_le prefix 0 declared;
        ignore (Unix.lseek fd offset Unix.SEEK_SET);
        expect "write incomplete length" (Unix.write fd prefix 0 4 = 4);
        expect "scan skips incomplete tail" (scan () = [first; last]);
        expect "strict rejects incomplete tail"
          (strict () = Error (T.Length_invalid (segment, offset, declared))))
        [length + 1; T.max_record_len];
      T.write_u32_le prefix 0 length;
      ignore (Unix.lseek fd offset Unix.SEEK_SET);
      expect "restore record length" (Unix.write fd prefix 0 4 = 4);
      ignore (Unix.lseek fd (offset + length) Unix.SEEK_SET);
      let byte = Bytes.create 1 in
      expect "read checksum" (Unix.read fd byte 0 1 = 1);
      Bytes.set_uint8 byte 0 (Bytes.get_uint8 byte 0 lxor 1);
      ignore (Unix.lseek fd (offset + length) Unix.SEEK_SET);
      expect "write checksum" (Unix.write fd byte 0 1 = 1);
      expect "scan checksum policy" (List.length (scan ()) = 3);
      expect "strict checksum policy"
        (strict () = Error (T.Checksum_mismatch (segment, offset)));
      List.iter (fun count ->
        Unix.ftruncate fd (offset + count);
        expect "scan skips short prefix" (scan () = [first; last]);
        expect "strict rejects short prefix"
          (strict () = Error (T.Prefix_truncated (segment, offset)))) [3; 2; 1]))

let () =
  let tests = ["reads", test_reads; "open", test_open; "scan", test_scan;
    "lengths", test_lengths] in
  let selected = match Array.to_list Sys.argv with
    | [_] -> tests
    | [_; name] -> [name, List.assoc name tests]
    | _ -> failwith "usage: test_txlog [reads|open|scan|lengths]"
  in
  List.iter (fun (_, test) -> test ()) selected;
  Printf.printf "event = txlog status = pass\n%!"