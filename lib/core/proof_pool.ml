(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let capacity =
  match Sys.getenv_opt "OCTRA_VERIFY_WORKERS" with
  | None -> 2
  | Some raw ->
    begin
      try
        let value = int_of_string raw in
        if value < 1 || value > 8 then 2 else value
      with _ -> 2
    end

let slots =
  Lwt_pool.create capacity (fun () -> Lwt.return_unit)

let run task =
  Lwt_pool.use slots (fun () -> Lwt_preemptive.detach task ())