(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module I = Image_codec
module W = Test_workspace

let expect name value = if not value then failwith name

let test_count () =
  W.with_dir "image-count" (fun dir ->
    let path = Filename.concat dir "records.dat" in
    let channel = open_out_bin path in
    let sink : I.sink = {
      channel; buffer = Buffer.create 128; records = 16_777_215L;
      bytes = 0L; prior = None; pvac_hashes = [];
    } in
    Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
      Lwt_main.run (I.emit sink (I.Value (["a"], "one")));
      Lwt_main.run (I.emit sink (I.Value (["b"], "two")));
      I.put_u32 sink.buffer 0;
      Lwt_main.run (I.drain sink));
    expect "writer record count was capped" (sink.records = 16_777_217L);
    let channel = open_in_bin path in
    Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
      let reader : I.reader = {
        channel; size = LargeFile.in_channel_length channel; format = I.Full64;
        records = 16_777_215L; prior = None;
      } in
      expect "first record differs" (I.read_record reader = Some (I.Value (["a"], "one")));
      expect "second record differs" (I.read_record reader = Some (I.Value (["b"], "two")));
      expect "reader record count was capped" (reader.records = 16_777_217L);
      expect "image end differs" (I.read_record reader = None);
      I.exact_end reader);
    List.iter (fun format ->
      let channel = open_in_bin path in
      Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
        let reader : I.reader = {
          channel; size = LargeFile.in_channel_length channel; format;
          records = 16_777_216L; prior = None;
        } in
        match I.read_record reader with
        | _ -> failwith "old record policy changed"
        | exception Failure reason ->
            expect "old record refusal differs" (reason = "ledger image record count exceeds limit")))
      [I.Prior; I.Path64]);
  print_endline "status = pass test = image_count"

let () = test_count ()