(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Input = Octra_vm.Aml_input
module Local = Octra_vm.Local_vm
module Mach = Octra_vm.C_mach
module Octb = Octra_vm.C_octb
module Prior = Octra_vm.Prior_compile
module Program = Octra_vm.Aml_program
module Sess = Octra_vm.C_sess
module Source = Octra_vm.Aml_source
module Vm = Octra_vm.Contract_vm

let fail name = failwith name

let need value name =
  match value with
  | Ok value -> value
  | Error reason -> fail (name ^ " reason = " ^ reason)

let compile name source = need (Program.compile source) name

let compile_prior name source =
  let resolver path = if path = "main.aml" then Some source else None in
  let result = Prior.compile_program_multi_first resolver "main.aml" in
  match result.error with
  | None -> result
  | Some reason -> fail (name ^ " reason = " ^ reason)

let image name program =
  match Octb.decode program.Program.octb with
  | Ok value -> value
  | Error error -> fail (name ^ " reason = " ^ Octb.decode_text error)

let wrapper_label target =
  target = Octb.dispatch_label 0
  || target = Octb.dispatch_label 1
  || target >= Octb.check_label 0 0
    && target < Octb.check_label Octb.input_limit 0
  || target >= Octb.data_label 0
    && target < Octb.data_label Octb.label_limit
  || target >= Octb.guard_label 0
    && target < Octb.guard_label Octb.label_limit
  || target >= Octb.body_label 0

let wrapper_labels name program =
  let code =
    match Octra_vm.Bytecode.decode_image program.Program.octb with
    | Ok value -> value.code
    | Error reason -> fail (name ^ " reason = " ^ reason)
  in
  let marks =
    Array.fold_left
      (fun out -> function Vm.JDEST target -> target :: out | _ -> out)
      [] code
  in
  if List.length marks <> List.length (List.sort_uniq Int.compare marks) then
    fail (name ^ " duplicate");
  Array.iter
    (function
      | Vm.JDEST target ->
        if not (wrapper_label target) then fail (name ^ " mark")
      | Vm.JMP target | Vm.JIF (_, target) ->
        if not (wrapper_label target) || not (List.mem target marks) then
          fail (name ^ " target")
      | _ -> ())
    code

let values name image raws =
  match Input.core_octb image.Octb.inputs raws with
  | Ok value -> value
  | Error error -> fail (name ^ " reason = " ^ Input.error_text error)

let args values =
  List.concat_map (fun (value : Input.core_value) -> value.vms) values

let lits values =
  List.concat_map (fun (value : Input.core_value) -> value.lits) values

let run ?(view = true) ?(grants = []) name program values =
  let config =
    Local.config
      ~view
      ~byte_result:Vm.Typed_bytes
      ~grants
      ~method_name:"main"
      ~args:(args values)
      ()
  in
  match Local.run ~trace:true config program.Program.code with
  | Ok value when value.Local.stop = Local.Returned -> value
  | Ok value -> fail (name ^ " stop = " ^ Local.stop_text value.stop)
  | Error error -> fail (name ^ " reason = " ^ Local.error_text error)

let integers name regs result wanted =
  let found =
    Array.to_list result
    |> List.map (fun reg ->
      match regs.(reg) with
      | Vm.VInt value -> Z.to_int value
      | _ -> fail (name ^ " result type"))
  in
  if found <> wanted then fail (name ^ " result")

let checked name image values outcome =
  let state =
    match Octra_vm.C_vm.make_in ~activate:(Some Z.zero) (lits values) with
    | Ok value -> value
    | Error error -> fail (name ^ " reason = " ^ Octra_vm.C_vm.text error)
  in
  begin
    match Octra_vm.C_vm.run state image.Octb.code with
    | Ok () -> ()
    | Error error -> fail (name ^ " reason = " ^ Octra_vm.C_vm.text error)
  end;
  Array.iter
    (fun reg ->
      match Octra_vm.C_vm.value state reg, outcome.Local.regs.(reg) with
      | Octra_vm.C_emit.Int left, Vm.VInt right when Z.equal left right -> ()
      | Octra_vm.C_emit.Bool left, Vm.VBool right when Bool.equal left right -> ()
      | Octra_vm.C_emit.Bytes left, Vm.VBytes right when String.equal left right -> ()
      | _, _ -> fail (name ^ " machine result"))
    image.results

let legacy = {|
program ArithmeticContract {
  pure fn arithmetic(left: int, right: int): int {
    return (left + right) * 3
  }
}
|}

let legacy_state = {|
contract LegacyState {
  state { owner: address }
  public fn same(): bool {
    self.owner = caller
    return caller == self.owner
  }
}
|}

let mixed = {|
program Compose {
  state { balance: int }

  private pure fn bump(value: int): int {
    return value + 1
  }

  form total [many base: int] (many value: int) ->[many] int marks {}
    under {steps[1000], depth[1000], work[1000]} =
    bump(base) + value

  public fn add(value: int): int {
    self.balance += total(value, value)
    return self.balance
  }

  public view fn read(): int { return self.balance }
}
|}

let sort4 = {|
program Sort4 {
  input many a: int
  input many b: int
  input many c: int
  input many d: int
  form order [many x: int] (many y: int) ->[many] int * int marks {} =
    if x <= y then (x, y) else (y, x)
  term
    let many p: int * int = order(a, b) in
    split p as many p0: int, many p1: int in
    let many q: int * int = order(c, d) in
    split q as many p2: int, many p3: int in
    let many r: int * int = order(p0, p2) in
    split r as many q0: int, many q2: int in
    let many s: int * int = order(p1, p3) in
    split s as many q1: int, many q3: int in
    let many t: int * int = order(q2, q1) in
    split t as many r1: int, many r2: int in
      vec[int](q0, r1, r2, q3)
}
|}

let sequence = {|
program Batch {
  input once amounts: seq[32, uint[64]]
  term
    fold amounts from (0, 0) with
      many amount: uint[64], once state: int * int =>
        split state as once n: int, once sum: int in
          (n + 1, wide(amount) + sum)
}
|}

let range = {|
program Amount {
  input many amount: uint[8]
  term wide(amount) + 1
}
|}

let unequal = {|
program Unequal {
  input many left: int
  input many right: int
  term if left != right then 1 else 0
}
|}

let close = {|
program OpenClose {
  permit cap[7] = close
  input once gate: cap[7]
  term close(gate)
}
|}

let service_result =
  let inputs =
    List.init 60 (fun index ->
      Printf.sprintf "input many value%d: int" index)
    |> String.concat " "
  in
  "program ServiceResult { " ^ inputs ^ " term (1, 2) }"

let legacy_check () =
  let program = compile "legacy" legacy in
  let current = Octra_vm.Oct_compile.compile_program_checked legacy in
  let prior = compile_prior "legacy prior" legacy in
  begin
    match current.error with
    | Some reason -> fail ("legacy prior reason = " ^ reason)
    | None -> ()
  end;
  if not (String.equal current.bytecode prior.bytecode) then
    fail "legacy frozen bytecode";
  if not (String.equal program.octb current.bytecode) then
    fail "legacy bytecode";
  let hash = Digestif.SHA256.(digest_string program.octb |> to_hex) in
  if not (String.equal hash
      "11eea44626b52bae92fbc5db878a12018658f6a1ba8d3ff052330f9910003a66")
  then fail "legacy golden"

let legacy_state_check () =
  let artifact = need (Source.compile legacy_state) "legacy state" in
  if Input.storage_kinds artifact.ast <> [] then fail "legacy state schema";
  let run strict code =
    let config =
      Local.config
        ~strict_values:strict
        ~method_name:"same"
        ~args:[]
        ()
    in
    match Local.run ~trace:false config code with
    | Ok value -> value
    | Error error -> fail ("legacy state reason = " ^ Local.error_text error)
  in
  let source = run false artifact.code in
  let image =
    match Octra_vm.Bytecode.decode_image artifact.octb with
    | Ok value -> value
    | Error reason -> fail ("legacy state OCTB reason = " ^ reason)
  in
  let octb = run false image.code in
  let wrong = run true artifact.code in
  if source.stop <> Local.Returned || source.result <> Vm.VBool true then
    fail "legacy state source";
  if octb.stop <> Local.Returned || octb.result <> Vm.VBool true then
    fail "legacy state OCTB";
  if wrong.stop <> Local.Reverted then fail "legacy state strict"

let mixed_check () =
  let program = compile "mixed" mixed in
  let hash = Digestif.SHA256.(digest_string program.octb |> to_hex) in
  if not (String.equal hash
      "67f8f96b0cf244162a8ea152a55cf46f923fd2ee143f2b0f207cddd57d9798a8")
  then fail "mixed golden";
  let config =
    Local.config
      ~storage_kinds:["balance", Vm.StorageInt]
      ~method_name:"add"
      ~args:[Vm.VInt (Z.of_int 5)]
      ()
  in
  match Local.run ~trace:false config program.code with
  | Ok value when value.stop = Local.Returned
      && value.result = Vm.VInt (Z.of_int 11) -> ()
  | Ok _ | Error _ -> fail "mixed execution"

let sort_check () =
  let program = compile "sort" sort4 in
  wrapper_labels "sort labels" program;
  let image = image "sort" program in
  let cases = [
    ["7"; "3"; "5"; "1"], [1; 3; 5; 7];
    ["9"; "9"; "1"; "9"], [1; 9; 9; 9];
  ] in
  List.iter
    (fun (raws, wanted) ->
      let values = values "sort input" image raws in
      let outcome = run "sort run" program values in
      integers "sort" outcome.regs image.results wanted;
      if outcome.effort <> 193 || outcome.steps <> 124 then
        fail "sort cost";
      checked "sort" image values outcome)
    cases

let sequence_check () =
  let program = compile "sequence" sequence in
  wrapper_labels "sequence labels" program;
  let image = image "sequence" program in
  let full =
    List.init 32 (fun index -> string_of_int (index + 1))
    |> String.concat ","
    |> Printf.sprintf "seq(%s)"
  in
  let cases = [
    "seq()", [0; 0], 1714, 825;
    full, [32; 528], 2194, 1273;
  ] in
  List.iter
    (fun (raw, wanted, effort, steps) ->
      let values = values "sequence input" image [raw] in
      let outcome = run "sequence run" program values in
      integers "sequence" outcome.regs image.results wanted;
      if outcome.effort <> effort || outcome.steps <> steps then
        fail "sequence cost";
      checked "sequence" image values outcome)
    cases

let range_check () =
  let program = compile "range" range in
  wrapper_labels "range labels" program;
  let image = image "range" program in
  let values = values "range input" image ["255"] in
  let outcome = run "range run" program values in
  integers "range" outcome.regs image.results [256];
  begin
    match Input.core_octb image.inputs ["256"],
        Input.core_octb image.inputs ["-1"] with
    | Error _, Error _ -> ()
    | _, _ -> fail "range admission"
  end

let unequal_check () =
  let program = compile "unequal" unequal in
  wrapper_labels "unequal labels" program;
  let image = image "unequal" program in
  if not (Array.exists (function Vm.NEQ _ -> true | _ -> false) program.code)
  then fail "unequal opcode";
  let test raws wanted =
    let values = values "unequal input" image raws in
    let outcome = run "unequal run" program values in
    integers "unequal" outcome.regs image.results [wanted];
    checked "unequal" image values outcome;
    outcome.effort, outcome.steps
  in
  let left = test ["3"; "5"] 1 in
  let right = test ["5"; "5"] 0 in
  if left <> right then fail "unequal cost"

let close_check () =
  let program = compile "close" close in
  wrapper_labels "close labels" program;
  let image = image "close" program in
  let values = values "close input" image ["cap[7](9)"] in
  let cap =
    match args values with
    | [Vm.VCap value] -> value
    | _ -> fail "close capability"
  in
  let outcome = run ~view:false ~grants:[cap] "close run" program values in
  if outcome.closes <> [cap] || outcome.storage <> [] || outcome.events <> [] then
    fail "close effects";
  let direct ?(view = false) ?(grants = [cap]) code =
    let config =
      Local.config
        ~view
        ~grants
        ~method_name:"main"
        ~args:[Vm.VCap cap]
        ()
    in
    match Local.run_at ~trace:false config ~entry:0 code with
    | Ok value -> value
    | Error error -> fail ("close direct reason = " ^ Local.error_text error)
  in
  let base = [|
    Vm.MLOAD (1, 1001);
    Vm.CAP_CHECK (Z.of_int 7, 1);
    Vm.CAP_CLOSE (Z.of_int 7, 1);
    Vm.STOP;
  |] in
  let exact = direct base in
  if exact.stop <> Local.Returned || exact.closes <> [cap]
      || exact.effort <> 74 || exact.steps <> 4 then
    fail "close direct";
  let raw = Octra_vm.Bytecode.encode base in
  let decoded =
    match Octra_vm.Bytecode.decode_image raw with
    | Ok value when value.code = base -> value
    | Ok _ | Error _ -> fail "close wire"
  in
  let cap_pc =
    let rec find pc =
      if pc >= Array.length decoded.code then fail "close opcode absent"
      else
        match decoded.code.(pc) with
        | Vm.CAP_CHECK _ -> pc
        | _ -> find (pc + 1)
    in
    find 0
  in
  let cap_at = decoded.cells.(cap_pc).at in
  let cap_error = Printf.sprintf "unknown opcode 0x89 at %d" cap_at in
  begin
    match Octra_vm.Bytecode.decode_image ~active:false raw with
    | Error reason when String.equal reason cap_error -> ()
    | Error reason -> fail ("close prior reason = " ^ reason)
    | Ok _ -> fail "close prior admitted"
  end;
  begin
    match Octra_vm.Admission.decode raw with
    | Error (Octra_vm.Admission.Decode_error reason)
        when String.equal reason cap_error -> ()
    | Error error -> fail (Octra_vm.Admission.error_message error)
    | Ok _ -> fail "close prior admission"
  end;
  begin
    match Octra_vm.Admission.decode ~point_ops:true raw with
    | Ok _ -> ()
    | Error error -> fail (Octra_vm.Admission.error_message error)
  end;
  begin
    match Vm.Verifier.verify [|Vm.LDI (0, Vm.VCap cap); Vm.STOP|] with
    | Error (Vm.Verifier.CapabilityLiteral 0) -> ()
    | Error _ | Ok () -> fail "close literal"
  end;
  begin
    match Vm.Verifier.verify [|Vm.CAP_CLOSE (Z.minus_one, 0); Vm.STOP|] with
    | Error (Vm.Verifier.CapabilityKind (0, kind))
        when Z.equal kind Z.minus_one -> ()
    | Error _ | Ok () -> fail "close kind"
  end;
  let absent = direct ~grants:[] base in
  let visible = direct ~view:true base in
  if absent.stop <> Local.Reverted || visible.stop <> Local.Reverted then
    fail "close authority";
  let rolled = direct [|
    Vm.MLOAD (1, 1001);
    Vm.CAP_CHECK (Z.of_int 7, 1);
    Vm.CHECKPOINT;
    Vm.CAP_CLOSE (Z.of_int 7, 1);
    Vm.ROLLBACK;
    Vm.STOP;
  |] in
  if rolled.stop <> Local.Returned || rolled.closes <> [] then
    fail "close rollback";
  let nested = direct [|
    Vm.MLOAD (1, 1001);
    Vm.CAP_CHECK (Z.of_int 7, 1);
    Vm.CHECKPOINT;
    Vm.CHECKPOINT;
    Vm.CAP_CLOSE (Z.of_int 7, 1);
    Vm.COMMIT;
    Vm.ROLLBACK;
    Vm.STOP;
  |] in
  if nested.stop <> Local.Returned || nested.closes <> [] then
    fail "close nested rollback";
  let nested_write = direct [|
    Vm.CHECKPOINT;
    Vm.CHECKPOINT;
    Vm.LDI (1, Vm.VInt (Z.of_int 2));
    Vm.SSTORE ("value", 1);
    Vm.COMMIT;
    Vm.ROLLBACK;
    Vm.STOP;
  |] in
  if nested_write.stop <> Local.Returned || nested_write.storage <> [] then
    fail "write nested rollback";
  let repeated = direct [|
    Vm.MLOAD (1, 1001);
    Vm.CAP_CHECK (Z.of_int 7, 1);
    Vm.CAP_CLOSE (Z.of_int 7, 1);
    Vm.CAP_CLOSE (Z.of_int 7, 1);
    Vm.STOP;
  |] in
  if repeated.stop <> Local.Reverted || repeated.closes <> [] then
    fail "close replay";
  let reject name regs code =
    let state =
      Vm.create_state
        ~strict_values:true
        ~caller:"caller"
        ~origin:"origin"
        ~address:"program"
        ~value:Z.zero
        ~storage:(Hashtbl.create 0)
        ()
    in
    List.iter (fun (reg, value) -> state.Vm.regs.(reg) <- value) regs;
    ignore (Vm.run state code);
    if not state.reverted || state.closes <> [] then fail name
  in
  reject "close memory" [1, Vm.VCap cap]
    [|Vm.MSTORE (0, 1); Vm.STOP|];
  reject "close event" [1, Vm.VCap cap]
    [|Vm.EMIT ("Leak", [1]); Vm.STOP|];
  reject "close storage" [1, Vm.VCap cap]
    [|Vm.SSTORE ("value", 1); Vm.STOP|];
  reject "close call"
    [1, Vm.VAddr "target"; 2, Vm.VString "method"; 3, Vm.VCap cap]
    [|Vm.XCALL (0, 1, 2, 3, 1); Vm.STOP|];
  let scope =
    match
      Sess.scope
        ~chain:"chain"
        ~prog:"program"
        ~root:(String.make 64 '2')
    with
    | Ok value -> value
    | Error error -> fail ("close scope reason = " ^ Sess.text error)
  in
  let state, token =
    match Sess.issue (Sess.empty scope) ~kind:(Z.of_int 7) ~id:(Z.of_int 9) with
    | Ok value -> value
    | Error error -> fail ("close issue reason = " ^ Sess.text error)
  in
  let auth =
    match Local.grant state token with
    | Some (Vm.VCap value) -> value
    | Some _ | None -> fail "close grant"
  in
  let config =
    Local.config
      ~view:false
      ~grants:[auth]
      ~method_name:"main"
      ~args:[Vm.VCap auth]
      ()
  in
  let success =
    match Local.run_at ~trace:false config ~entry:0 base with
    | Ok value when value.stop = Local.Returned -> value
    | Ok _ | Error _ -> fail "close session"
  in
  let closed =
    match Local.settle state [token] success with
    | Ok (value, []) -> value
    | Ok _ | Error _ -> fail "close settlement"
  in
  if Sess.current closed token || Option.is_some (Local.grant closed token) then
    fail "close current";
  begin
    match Local.settle closed [token] success with
    | Error (Local.Session (Sess.Stale _)) -> ()
    | Error _ | Ok _ -> fail "close settlement replay"
  end

let register_check () =
  if not (Octb.result_regs [0; 60]) then fail "result register edge";
  if Octb.result_regs [61] then fail "result service register";
  if Octb.result_regs [1; 1] then fail "result repeated register";
  match Program.compile service_result with
  | Error reason when String.equal reason
      "OCTB result registers must be distinct and below 61" -> ()
  | Error reason -> fail ("result register reason = " ^ reason)
  | Ok _ -> fail "result register accepted"

let label_check () =
  if Octb.label_limit <> 1_000_000 then fail "label limit";
  if Octb.dispatch_label 1 <> 200 then fail "dispatch label";
  if Octb.check_label 59 1 <> 1119 then fail "check label";
  if Octb.data_label 999_999 <> 1_999_999 then fail "data label";
  if Octb.guard_label 999_999 <> 2_999_999 then fail "guard label";
  if Octb.body_label 1_048_575 <> 11_048_575 then fail "body label";
  if Octb.label_count (-1) || not (Octb.label_count 1_000_000)
      || Octb.label_count 1_000_001 then
    fail "label count";
  if Octb.body_target 2 (Octb.body_label 1) <> Some 1
      || Octb.body_target 1 (Octb.body_label 1) <> None
      || Octb.body_target 1 (Octb.body_label 0 - 1) <> None then
    fail "body target"

let () =
  legacy_check ();
  legacy_state_check ();
  mixed_check ();
  sort_check ();
  sequence_check ();
  range_check ();
  unequal_check ();
  close_check ();
  register_check ();
  label_check ();
  print_endline "aml_program = pass legacy = exact sort = checked sequence = checked range = checked unequal = checked close = checked register = checked labels = checked"