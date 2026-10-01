(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Recovery_case

let () =
  if Array.length Sys.argv <> 2 then failwith "check_store executable required";
  let program = Sys.argv.(1) in
  Test_workspace.with_dir "store_check" (fun root ->
    List.iter (fun (name, alter, expected) ->
      let dir = Filename.concat root name in
      Unix.mkdir dir 0o700;
      let head, _ = prepare dir in
      alter dir head;
      let before = files dir in
      let child = Unix.create_process program [|program; dir|] Unix.stdin Unix.stdout Unix.stderr in
      let status = wait child in
      expect (name ^ ": exit differs") (status = Unix.WEXITED expected);
      expect (name ^ ": files changed") (files dir = before);
      Printf.printf "event = test name = store_check case = %s status = passed\n%!" name)
      ["clean", (fun _ _ -> ()), 0;
       "suffix", (fun dir _ -> with_stores dir (fun chain _ ->
         ignore (Octra_core.Txlog.append chain.SC.txlog ~epoch_id:1 ~payload:(hash 'b' ^ "{}")))), 0;
       "corrupt", (fun dir head -> HM.atomic_write dir {head with HM.state_root = hash 'f'}), 2;
       "short_committed", (fun dir _ ->
         Unix.truncate (Filename.concat dir "chaindata/txlog/seg000000.dat") 3), 2;
       "short_suffix", (fun dir _ ->
         let channel = open_out_bin (Filename.concat dir "chaindata/txlog/seg000001.dat") in
         Fun.protect ~finally:(fun () -> close_out_noerr channel)
           (fun () -> output_string channel "OTX")), 0;
       "missing", (fun dir _ -> Unix.unlink (HM.path dir)), 2])