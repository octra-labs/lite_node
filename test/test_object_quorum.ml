(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_vm.Contract_vm

module Policy = Octra_core.Circle_object_policy
module Binding = Octra_core.Circle_object_binding
module Member = Octra_core.Circle_object_member
module Apply = Octra_core.Circle_object_apply

let check name condition =
  if not condition then failwith ("object_quorum: " ^ name)

let object_ref = String.make 64 'a'
let state_ref = String.make 64 'b'
let transition_ref = String.make 64 'c'
let intent_id = String.make 64 'd'
let digest value = Digestif.SHA256.(digest_string value |> to_hex)

let entries table =
  Hashtbl.fold (fun key value result -> (key, value) :: result) table []
  |> List.sort compare

let storage quorum bootstrap members =
  let table = Hashtbl.create 16 in
  let policy = Policy.{empty with
    transition_mode = Some Octra_core.Circle_object_transition_mode.Open;
    member_quorum = Some quorum;
    allow_detach = Some false;
    allow_root_state_rotation = Some false} in
  Policy.write_snapshot table object_ref policy;
  Hashtbl.iter (fun key value ->
    check "policy input invalid" (Result.is_ok (Policy.validate_runtime_key key value))) table;
  if not bootstrap then begin
    Binding.write_snapshot table object_ref Binding.{empty with
      current_state_ref = Some state_ref; version = Some 7L};
    List.iter (fun name ->
      Member.write_snapshot table object_ref name Member.{
        state_ref = Some state_ref; member_kind = Some "member";
        state_class = Some "public"; codec = Some "raw"; status = Some "active"}) members
  end;
  table

let run_case mode quorum bootstrap count =
  let members = List.init count (fun index -> "member" ^ string_of_int index) in
  let table = storage quorum bootstrap members in
  let before = entries table in
  let bundle = if not bootstrap then "" else
    List.map (fun name -> String.concat "|"
      ["attach"; name; state_ref; "member"; "public"; "raw"; "active"]) members
    |> String.concat ";" in
  let state = create_state ~ctx:{default_ctx with object_cost = true; object_quorum = mode}
    ~caller:"" ~origin:"" ~address:"" ~value:Z.zero ~storage:table () in
  state.regs.(0) <- VString "unchanged";
  List.iteri (fun index value -> state.regs.(index + 1) <- VString value)
    [transition_ref; object_ref; state_ref; state_ref; bundle; digest bundle;
     "none"; ""; "active"; intent_id];
  let accepted = exec_one state (OBJECT_TRANSITION_APPLY (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) in
  let expected = match mode with
    | Apply.Machine -> count >= Int64.to_int quorum
    | Apply.Exact -> Int64.compare (Int64.of_int count) quorum >= 0 in
  check (Printf.sprintf "quorum = %Ld members = %d bootstrap = %b accepted = %b"
    quorum count bootstrap accepted) (accepted = expected);
  if expected then begin
    check "version differs" (state.regs.(0) = VInt (Z.of_int (if bootstrap then 1 else 8)));
    check "state ref differs"
      (Hashtbl.find_opt table (Binding.current_state_ref_key object_ref) = Some state_ref)
  end else begin
    check "refusal did not revert" state.reverted;
    check "refusal changed storage" (entries table = before);
    check "refusal changed output" (state.regs.(0) = VString "unchanged")
  end

let pure_cases () =
  let counts = [-1; 0; 1; 2; 17; max_int] in
  let quorums = [-1L; 0L; 1L; 2L; 17L; Int64.of_int max_int;
    Int64.succ (Int64.of_int max_int); Int64.max_int] in
  List.iter (fun bootstrap ->
    List.iter (fun quorum ->
      let policy = Policy.{empty with member_quorum = Some quorum} in
      List.iter (fun count ->
        let expected = count >= 0 && quorum >= 0L
          && Z.compare (Z.of_int count) (Z.of_int64 quorum) >= 0 in
        let active, next = if bootstrap then 0, count else count, 0 in
        let actual = Apply.validate_quorum ~mode:Apply.Exact policy ~bootstrap
          ~active_member_count:active ~next_active_member_count:next in
        check "exact count differs" (Result.is_ok actual = expected)) counts) quorums)
    [true; false]

let () =
  pure_cases ();
  check "default changed history" (default_ctx.object_quorum = Apply.Machine);
  List.iter (fun mode ->
    List.iter (fun bootstrap ->
      List.iter (fun (quorum, count) -> run_case mode quorum bootstrap count)
        [0L, 0; 1L, 0; 1L, 1; 2L, 1; 2L, 2;
         1_073_741_824L, 0; 2_147_483_648L, 0;
         Int64.of_int max_int, 2; Int64.succ (Int64.of_int max_int), 0;
         Int64.max_int, 0; Int64.max_int, 2]) [true; false]) [Apply.Machine; Apply.Exact];
  print_endline "status = pass test = object_quorum"