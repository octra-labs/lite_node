(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Ready = Octra_core.Proof_ready

let need reason value = if not value then failwith reason

let () =
  need "ready table exists before build" (Ready.gates () = 0 && not (Ready.check ()));
  let child = Unix.fork () in
  if child = 0 then begin
    Ready.prepare ();
    need "prepared capacity differs" (Ready.gates () = Ready.capacity ());
    need "prepared digest differs" (Ready.check ());
    Unix._exit 0
  end;
  need "prepared child failed" (snd (Unix.waitpid [] child) = Unix.WEXITED 0);
  List.iter (fun n ->
    let refused = try Ready.build n; false with
      | Invalid_argument reason -> reason = "ready size must be positive" && n <= 0
      | Failure reason -> reason = "pvac: ready size rejected" && n > (1 lsl 20) in
    need "invalid ready size accepted" refused;
    need "invalid ready build changed state" (Ready.gates () = 0)) [-1; 0; (1 lsl 20) + 1];
  Ready.build 4096;
  need "ready size differs" (Ready.gates () = 4096);
  need "small ready image accepted" (not (Ready.check ()));
  let module S = Octra_core.Proof_serve in
  let response = Octra_core.Pvac_verify_protocol.{request_hash = String.make 64 'a';
    accepted = false; reason = "proof rejected"} in
  let refused = try ignore (S.answer 7 response 1 1); false with
    | Failure reason -> reason = "proof result image" in
  need "unchecked negative image accepted" refused;
  need "early refusal changed" (S.answer 8 response 0 0 = Octra_core.Proof_frame.Answer
    (8, 0, 0, Octra_core.Pvac_verify_protocol.response_bytes response));
  need "ready build counted as sum" (Ready.sums () = 0 && Ready.rejected () = 0);
  let refused = try Ready.build 4096; false with
    | Failure reason -> reason = "pvac: ready table already exists" in
  need "second ready build accepted" refused;
  need "ready snapshot changed" (Ready.gates () = 4096 && Ready.sums () = 0 && Ready.rejected () = 0);
  print_endline "event = proof_ready status = passed"