(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module V = Octra_vm.Contract_vm
module L = Octra_vm.Local_vm

let check yes reason = if not yes then failwith reason
let number value = V.VInt (Z.of_int value)
let owner = "oct" ^ String.make 44 '1'
let spender = "oct" ^ String.make 44 '2'
let payee = "oct" ^ String.make 44 '3'
let balance address = "balances:" ^ address
let grant = "grants:" ^ owner ^ ":" ^ spender
let code = Octra_vm.Ocs01.make_full_bytecode
  {name = "Token"; symbol = "TKN"; decimals = 6; supply = Z.of_int 120}
  |> Octra_vm.Bytecode.encode
  |> Octra_vm.Bytecode.decode_exn

let initial = List.sort compare [
  "name", "Token"; "symbol", "TKN"; "decimals", "6"; "owner", owner;
  "total_supply", "120"; balance owner, "100"; balance payee, "20"; grant, "30"]

let execute ?(caller = owner) ?(storage = initial) ?(strict = false) method_name args =
  let storage_kinds = if strict then ["owner", V.StorageAddr] else [] in
  let config = L.config ~strict_values:strict ~storage ~storage_kinds
    ~caller ~origin:caller ~address:"oct_token"
    ~limit:1_000_000 ~step_cap:1000 ~method_name ~args () in
  match L.run ~trace:true config code with
  | Ok value -> value
  | Error error -> failwith (L.error_text error)

let arguments name recipient amount =
  match name with
  | "pull" -> [V.VString owner; V.VString recipient; amount]
  | "mint" | "transfer" -> [V.VString recipient; amount]
  | "burn" -> [amount]
  | _ -> invalid_arg "token method"

let changes rows =
  List.fold_left (fun entries (name, value) ->
    (name, string_of_int value) :: List.remove_assoc name entries) initial rows
  |> List.sort compare

let call name recipient amount =
  execute ~caller:(if name = "pull" then spender else owner)
    name (arguments name recipient amount)

let negatives name =
  let values = [V.VInt Z.minus_one; V.VInt (Z.neg (Z.shift_left Z.one 120));
    V.VU64 Z.minus_one; V.VU128 Z.minus_one; V.VU256 Z.minus_one;
    V.VString "-1"; V.VString "-001"; V.VString "-0x1"] in
  List.iter (fun value ->
    let result = call name payee value in
    check (result.stop = L.Reverted) ("negative token amount accepted: " ^ name);
    check (result.storage = initial && result.events = [] && result.closes = [])
      ("negative token amount changed state: " ^ name);
    check (not (List.exists (fun (frame : L.frame) -> match frame.op with
      | V.SSTORE _ | V.SSTOREK _ | V.EMIT _ -> true | _ -> false) result.frames))
      ("negative token amount reached write: " ^ name)) values

let positives name =
  List.iter (fun amount ->
    List.iter (fun recipient ->
      let result = call name recipient (number amount) in
      let expected = match name with
        | "pull" -> changes ([grant, 30 - amount] @
          if recipient = owner then [] else [balance owner, 100 - amount; balance payee, 20 + amount])
        | "mint" -> changes ["total_supply", 120 + amount;
          balance recipient, (if recipient = owner then 100 else 20) + amount]
        | "burn" -> changes ["total_supply", 120 - amount; balance owner, 100 - amount]
        | _ -> assert false in
      check (result.stop = L.Returned && result.result = V.VBool true)
        ("nonnegative token amount refused: " ^ name);
      let event, values = match name with
        | "pull" -> "Transfer", [V.VString owner; V.VString recipient; number amount]
        | "mint" -> "Mint", [V.VString recipient; number amount]
        | "burn" -> "Burn", [V.VAddr owner; number amount]
        | _ -> assert false in
      check (result.storage = expected
        && result.events = [V.{contract = "oct_token"; depth = 0; event; values}]
        && result.closes = [])
        ("token balances or event differ: " ^ name))
      [owner; payee]) [0; 1; 30]

let refusals name =
  let result = if name = "mint" then
    execute ~caller:spender name (arguments name payee (number 1))
    else call name payee (number 121) in
  check (result.stop = L.Reverted && result.storage = initial && result.events = [])
    ("token authority or funds bypassed: " ^ name);
  if name = "pull" then begin
    let storage = changes [grant, 0] in
    let result = execute ~storage ~caller:spender name (arguments name payee (number 1)) in
    check (result.stop = L.Reverted && result.storage = storage)
      "pull without grant accepted"
  end

let modes name =
  List.iter (fun strict ->
    List.iter (fun amount ->
      let result = execute ~strict ~caller:(if name = "pull" then spender else owner)
        name (arguments name payee (number amount)) in
      check (result.stop = if amount < 0 then L.Reverted else L.Returned)
        ("typed token amount differs: " ^ name)) [-1; 0; 1]) [false; true];
  let storage = ["owner", owner] in
  let result = execute ~storage ~caller:(if name = "pull" then spender else owner)
    name (arguments name payee (number 0)) in
  let rows = match name with
    | "pull" -> [grant, "0"; balance owner, "0"; balance payee, "0"]
    | "mint" -> ["total_supply", "0"; balance payee, "0"]
    | "burn" -> ["total_supply", "0"; balance owner, "0"]
    | _ -> assert false in
  check (result.stop = L.Returned && result.storage = List.sort compare (rows @ storage))
    ("zero token key creation differs: " ^ name);
  List.iter (fun amount ->
    let result = call name payee amount in
    let zero = call name payee (number 0) in
    check (result.stop = zero.stop && result.storage = zero.storage)
      ("legacy token conversion differs: " ^ name))
    [V.VBytes "-1"; V.VBytes32 (String.make 32 '-'); V.VString "-5,3"; V.VBool false];
  if name = "mint" then begin
    let result = execute ~strict:true ~storage
      name (arguments name payee (V.VString "0")) in
    check (result.stop = L.Returned) "typed numeric text zero refused"
  end

let compatibility () =
  List.iter (fun amount ->
    let result = execute "grant" [V.VString spender; number amount] in
    check (result.stop = L.Returned && result.storage = changes [grant, amount])
      "grant semantics changed") [-1; 0; 30];
  let zero = call "transfer" payee (number 0) in
  check (zero.stop = L.Reverted && zero.storage = initial) "zero transfer changed";
  let positive = call "transfer" payee (number 1) in
  check (positive.stop = L.Returned
    && positive.storage = changes [balance owner, 99; balance payee, 21])
    "positive transfer changed";
  let signed = Octra_vm.Aml_source.compile ~syntax:Octra_vm.Oct_gen.Source
    "program Signed { fn value(amount: int): int { return amount + 2 } }"
    |> Result.get_ok in
  let cfg = L.config ~method_name:"value" ~args:[number (-5)] () in
  let result = L.run ~trace:false cfg (Octra_vm.Bytecode.decode_exn signed.octb) in
  check (match result with Ok value -> value.stop = L.Returned
    && value.result = number (-3) | Error _ -> false) "signed arithmetic refused"

let native_checks () =
  let module Money = Octra_vm.Value_journal in
  List.iter (fun async -> List.iter (fun strict -> List.iter (fun amount ->
    let funds = Money.create ~balance:(fun address ->
      if address = owner then Z.of_int 100 else Z.zero) in
    let calls = ref 0 in
    let ctx = {V.default_ctx with scalar_work = true;
      byte_work = Some (Octra_vm.Byte_work.create Octra_vm.Byte_work.consensus_limits);
      do_transfer = (fun from_addr to_addr amount ->
        incr calls;
        Money.transfer funds ~from_addr ~to_addr ~amount)} in
    let state = V.create_state ~ctx ~strict_values:strict
      ~caller:owner ~origin:owner ~address:owner ~value:Z.zero ~storage:(Hashtbl.create 1) () in
    let program = V.[|LDI (0, VAddr payee); LDI (1, number amount);
      TRANSFER (2, 0, 1); STOP|] in
    let accepted = if async then Lwt_main.run (V.run_async state program) else V.run state program in
    check (accepted && V.getr state 2 = V.VBool (amount >= 0 && amount <= 100))
      "native transfer decision differs";
    check (!calls = if amount <= 0 then 0 else 1) "native transfer reached host";
    let sent = if amount > 0 && amount <= 100 then amount else 0 in
    check (Z.equal (Money.effective funds owner) (Z.of_int (100 - sent))
      && Z.equal (Money.effective funds payee) (Z.of_int sent))
      "native transfer changed funds") [-1; 0; 1; 100; 101]) [false; true]) [false; true]

let () =
  List.iter (fun (name, arity) ->
    check (Octra_vm.Contract.extract_method_arity (Octra_vm.Contract.fix_jumps code) name = Some arity)
      ("token network arity differs: " ^ name)) ["pull", 3; "mint", 2; "burn", 1];
  let methods = match Array.to_list Sys.argv with
    | [_] -> ["pull"; "mint"; "burn"]
    | [_; name] when List.mem name ["pull"; "mint"; "burn"] -> [name]
    | _ -> invalid_arg "token test arguments" in
  List.iter (fun name ->
    positives name;
    refusals name;
    modes name;
    negatives name) methods;
  compatibility ();
  native_checks ();
  Printf.printf "event = token_amount status = pass methods = %d\n" (List.length methods)