(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Q = Fhe_queue
module M = Queue_model

let ticket id = Proof_wait.{
  request = string_of_int (id / 4);
  generation = string_of_int (id mod 4);
}

let identity (entry : Q.entry) =
  4 * int_of_string entry.ticket.request + int_of_string entry.ticket.generation

let model_entry (entry : Q.entry) = M.{
  identity = Z.of_int (identity entry);
  deadline = Z.of_int64 entry.deadline;
  abandoned = entry.abandoned;
  urgent = entry.urgent;
}

let model_message = function
  | Q.Submit entry -> M.Submit (model_entry entry)
  | Q.Complete value ->
    M.Complete (Z.of_int (4 * int_of_string value.request + int_of_string value.generation))
  | Q.Cancel value ->
    M.Cancel (Z.of_int (4 * int_of_string value.request + int_of_string value.generation))
  | Q.Tick -> M.Tick
  | Q.Stop -> M.Stop

let model_state (state : Q.state) = M.{
  active = Option.map model_entry state.active;
  pending = List.map model_entry state.pending;
  closed = state.closed;
}

let checked = ref 0

let step (actual, proved) (now, message) =
  let next, _ = Q.delta actual (now, message) in
  let expected = M.delta proved (Z.of_int64 now) (model_message message) in
  incr checked;
  if model_state next <> expected then
    failwith ("queue model differs step = " ^ string_of_int !checked);
  next, expected

let submit ?(urgent = true) ?(abandoned = false) id deadline =
  Q.Submit Q.{ticket = ticket id; deadline; urgent; abandoned}

let trace messages =
  ignore (List.fold_left step (Q.empty, M.empty) messages)

let directed () =
  trace ([0L, submit 0 100L; 0L, submit ~urgent:false 4 100L;
    0L, submit ~urgent:false 8 100L; 0L, submit ~urgent:false 12 100L]
    @ List.init 10 (fun index -> 0L, submit (index + 16) 100L)
    @ [0L, Q.Complete (ticket 1); 0L, Q.Cancel (ticket 0);
       0L, Q.Complete (ticket 0); 100L, Q.Tick; 100L, Q.Stop]);
  trace [0L, submit ~abandoned:true 0 10L; 0L, submit 0 10L;
    0L, submit 1 10L; 0L, submit 2 10L; 0L, Q.Cancel (ticket 1);
    0L, Q.Cancel (ticket 0); 0L, Q.Cancel (ticket 0);
    0L, Q.Complete (ticket 0); 0L, Q.Complete (ticket 0);
    0L, Q.Complete (ticket 2); 0L, submit 0 10L];
  List.iter (fun time ->
    trace [time, submit 0 time; time, submit 1 Int64.max_int;
      time, submit ~urgent:false 2 Int64.max_int;
      Int64.max_int, Q.Tick; Int64.max_int, Q.Complete (ticket 1);
      Int64.max_int, Q.Stop; Int64.max_int, submit 3 Int64.max_int])
    [0L; 1L; 4_611_686_018_427_387_903L; Int64.pred Int64.max_int];
  trace [0L, submit 0 10L; 0L, submit 1 10L; 0L, Q.Stop;
    0L, Q.Complete (ticket 0); 0L, submit 0 10L; 0L, Q.Stop]

let generated () =
  let random = Random.State.make [|97; 1_640_000|] in
  for _ = 1 to 512 do
    let rec run count now state =
      if count > 0 then begin
        let id = Random.State.int random 32 in
        let deadline = Int64.max 0L (Int64.add now
          (Int64.of_int (Random.State.int random 24 - 2))) in
        let message = match Random.State.int random 128 with
          | 0 -> Q.Stop
          | choice when choice < 72 ->
            submit ~urgent:(Random.State.bool random)
              ~abandoned:(Random.State.bool random) id deadline
          | choice when choice < 100 -> Q.Complete (ticket id)
          | choice when choice < 120 -> Q.Cancel (ticket id)
          | _ -> Q.Tick in
        let next = step state (now, message) in
        let now = Int64.add now (Int64.of_int (Random.State.int random 3)) in
        run (count - 1) now next
      end in
    run 256 0L (Q.empty, M.empty)
  done

let () =
  if model_state Q.empty <> M.empty then failwith "queue model initial state differs";
  directed ();
  generated ();
  Printf.printf "event = queue_model steps = %d status = passed\n%!" !checked