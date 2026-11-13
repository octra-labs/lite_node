(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_core
module V = Octra_vm
module S = C.Store_irmin
module VM = V.Contract_vm
module E = V.Tx_effects

let run = Lwt_main.run
let need label value = if not value then failwith label
let ok = function Ok value -> value | Error reason -> failwith reason
let z = Z.of_int
let addr digit = "oct" ^ String.make 44 digit
let owner = addr '1'
let guardian = addr '2'
let user = addr '3'
let operator = addr '4'
let recipient = addr '5'
let eth = "0x1234567890123456789012345678901234567890"
let text value = `String value
let rows storage = Hashtbl.fold (fun key value all -> (key, value) :: all) storage [] |> List.sort compare
let sha raw = Digestif.SHA256.(digest_string raw |> to_hex)

let check source historical active async_exec =
  let raw, version, error = if historical then
    let value = V.Prior_compile.compile source in value.bytecode, value.version, value.error
  else let value = V.Oct_compile.compile source in value.bytecode, value.version, value.error in
  Option.iter failwith error;
  Test_workspace.with_dir "bridge_compat" (fun path ->
    let store = run (S.open_store (Filename.concat path "store")) in
    Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
      let ledger = C.Ledger.create store in
      List.iter (fun who -> ok (C.Ledger.add_account ledger who (Z.of_string "100000000000000")))
        [owner; guardian; user; operator; recipient];
      run (C.Ledger.flush_dirty_lwt ledger);
      run (S.begin_epoch_batch store);
      let effect = E.create ~ledger ~store in
      let address, created = V.Contract.deploy ~journal:(E.program effect) ~params:[text guardian]
        store owner "CUSTOM" [||] raw 1 in
      need "bridge constructor refused" created.success;
      ok (E.ensure_account effect address);
      E.commit_exn effect;
      let baseline = VM.default_ctx in
      let profile = if not active then baseline else {baseline with
        point_ops = true; math = true; object_cost = true; int_work = V.Int_work.Active;
        scalar_work = true; fhe_work = C.Rule_graph.Active; proof_exec = C.Rule_graph.Active;
        proof_key = C.Rule_graph.Active; wasm_float = C.Rule_graph.Active} in
      let balance who = (Option.get (C.Ledger.find_opt ledger who)).balance in
      let storage () = run (S.load_contract_storage store address) |> rows in
      let calls = ref 0 in
      let max_effort = ref 0 in
      let invoke ?(caller = user) ?(value = Z.zero) ?(epoch = 100) ?(transfer = true)
          success method_name params =
        let before = storage () in
        let balances = List.map (fun who -> who, balance who) [owner; guardian; user; operator; recipient; address] in
        let effect = E.create ~ledger ~store in
        let move from_addr target amount = transfer && E.apply effect
          (V.Call_plan.value_effect ~from_addr ~target ~amount ~balance:(E.balance effect from_addr)) in
        need "bridge value input refused" (Z.equal value Z.zero || move caller address value);
        let ctx = {profile with current_epoch = epoch;
          byte_work = (if active then Some (V.Byte_work.create V.Byte_work.consensus_limits) else None);
          get_balance = E.balance effect; do_transfer = move} in
        let call = if async_exec then
          fun () -> run (V.Contract.execute_call_async ~ctx ~journal:(E.program effect)
            store address method_name params caller value)
        else fun () -> V.Contract.execute_call ~ctx ~journal:(E.program effect)
            store address method_name params caller value in
        let receipt = call () in
        incr calls;
        max_effort := max !max_effort receipt.effort_used;
        if receipt.success then E.commit_exn effect else E.discard effect;
        need ("bridge outcome differs: " ^ method_name) (receipt.success = success);
        if not success then begin
          need ("bridge storage changed on refusal: " ^ method_name) (storage () = before);
          need ("bridge balance changed on refusal: " ^ method_name)
            (List.for_all (fun (who, amount) -> Z.equal amount (balance who)) balances)
        end;
        receipt in
      let accepted ?caller ?value ?epoch method_name params =
        invoke ?caller ?value ?epoch true method_name params in
      let refused ?caller ?value ?epoch ?transfer method_name params =
        ignore (invoke ?caller ?value ?epoch ?transfer false method_name params) in
      let read name params = Option.get (accepted name params).return_value in
      let release = VM.to_string (read "version" []) in
      need "bridge version differs" (release = "3.1.0" || release = "3.0.0");
      let limit, daily = if release = "3.1.0" then
        Z.of_string "115000000000000", Z.of_string "115000000000000"
        else Z.of_string "100000000000", Z.of_string "1000000000000" in
      need "bridge initial reserve differs" (read "get_total_locked" [] = VM.VInt Z.zero);
      need "bridge initial operator differs" (VM.to_string (read "get_unlock_operator" []) = owner);
      let locked = accepted ~value:(z 3_000_000) "lock_to_eth" [text eth] in
      need "bridge lock event missing" (List.length locked.events = 1);
      let lock_event = List.hd locked.events in
      need "bridge lock event changed" (lock_event.contract = address && lock_event.depth = 0
        && lock_event.event = "Locked" && List.map VM.to_string lock_event.values
        = [user; "3000000"; eth; "1"]);
      need "bridge locked balance differs" (balance address = z 3_000_000);
      need "bridge lock nonce differs" (read "get_lock_nonce" [] = VM.VInt Z.one);
      need "bridge daily lock differs" (read "daily_lock_remaining" [] = VM.VInt (Z.sub daily (z 3_000_000)));
      need "bridge pause state differs" (read "is_paused" [] = VM.VInt Z.zero);
      need "bridge lock history differs"
        (read "get_lock_info" [text "1"] = VM.VString (user ^ "|3000000|" ^ eth));
      refused ~value:(z 1_000_000) "lock_to_eth" [text eth];
      refused ~value:Z.one ~epoch:106 "lock_to_eth" [text eth];
      refused ~value:(z 1_000_000) ~epoch:106 "lock_to_eth" [text "invalid"];
      refused "unlock_trusted" [text recipient; text "1000000"; text "burn-a"];
      refused ~caller:owner "unlock_trusted" [text recipient; text (Z.to_string (Z.succ limit)); text "burn-limit"];
      refused ~caller:owner "unlock_trusted" [text recipient; text "0"; text "burn-zero"];
      refused ~caller:owner "unlock_trusted" [text "invalid"; text "1000000"; text "burn-address"];
      refused "set_unlock_operator" [text operator];
      ignore (accepted ~caller:owner "set_unlock_operator" [text operator]);
      refused ~caller:owner "unlock_trusted" [text recipient; text "1000000"; text "burn-a"];
      let before = balance recipient in
      let unlocked = accepted ~caller:operator "unlock_trusted"
        [text recipient; text "1000000"; text "burn-a"] in
      need "bridge unlock event missing" (List.length unlocked.events = 1);
      let unlock_event = List.hd unlocked.events in
      need "bridge unlock event changed" (unlock_event.contract = address && unlock_event.depth = 0
        && unlock_event.event = "Unlocked" && List.map VM.to_string unlock_event.values
        = [recipient; "1000000"; "burn-a"]);
      need "bridge unlock balance differs" (balance recipient = Z.add before (z 1_000_000));
      need "bridge unlock reserve differs" (balance address = z 2_000_000);
      need "bridge unlock count differs" (read "get_unlock_count" [] = VM.VInt Z.one);
      need "bridge daily unlock differs" (read "daily_unlock_remaining" [] = VM.VInt (Z.sub daily (z 1_000_000)));
      need "bridge replay record missing" (read "is_unlock_processed" [text "burn-a"] = VM.VInt Z.one);
      refused ~caller:operator "unlock_trusted" [text recipient; text "1000000"; text "burn-a"];
      refused ~caller:operator "unlock_trusted" [text recipient; text "3000000"; text "burn-b"];
      refused ~caller:operator ~transfer:false "unlock_trusted"
        [text recipient; text "1000000"; text "burn-c"];
      need "bridge failed transfer retained burn" (read "is_unlock_processed" [text "burn-c"] = VM.VInt Z.zero);
      refused "pause" [];
      ignore (accepted ~caller:guardian "pause" []);
      need "bridge pause state missing" (read "is_paused" [] = VM.VInt Z.one);
      refused ~value:(z 1_000_000) ~epoch:106 "lock_to_eth" [text eth];
      refused ~caller:operator "unlock_trusted" [text recipient; text "1000000"; text "burn-c"];
      refused ~caller:guardian "unpause" [];
      ignore (accepted ~caller:owner "unpause" []);
      ignore (accepted ~epoch:106 ~value:(z 1_000_000) "lock_to_eth" [text eth]);
      ignore (accepted ~caller:operator "unlock_trusted" [text recipient; text "1000000"; text "burn-c"]);
      need "bridge supply differs" (read "supply_invariant" [] = VM.VInt (balance address));
      need "bridge total unlocked differs" (read "get_total_unlocked" [] = VM.VInt (z 2_000_000));
      refused "update_guardian" [text recipient];
      ignore (accepted ~caller:owner "update_guardian" [text recipient]);
      refused ~caller:guardian "pause" [];
      ignore (accepted ~caller:recipient "pause" []);
      ignore (accepted ~caller:owner "unpause" []);
      refused "transfer_ownership" [text recipient];
      ignore (accepted ~caller:owner "transfer_ownership" [text recipient]);
      refused ~caller:owner "set_unlock_operator" [text owner];
      ignore (accepted ~caller:recipient "set_unlock_operator" [text owner]);
      let saved = storage () in
      run (S.commit_epoch_batch store "bridge compat");
      need "bridge persisted storage differs" (storage () = saved);
      Printf.printf "event = bridge_compat historical = %b active = %b async = %b calls = %d max_effort = %d code = %s version = %s\n"
        historical active async_exec !calls !max_effort (sha raw) version))

module Deployed = struct
  module Fields = Map.Make (String)
  module J = Yojson.Safe.Util

  type mode = Safety | Compat

  let roles = ["owner"; "guardian"; "unlock_operator"]
  let numbers = ["paused"; "total_locked"; "lock_nonce"; "daily_locked";
    "last_lock_day"; "reentrancy_lock"; "total_unlocked"; "unlock_count";
    "daily_unlocked"; "last_unlock_day"]
  let maps = ["user_last_lock_epoch"; "lock_history"; "lock_amounts";
    "lock_eth_recipients"; "processed_unlocks"]

  type input = {
    network : string;
    address : string;
    code_hash : string;
    encoded : string;
    metadata : string;
    version : string;
    balance : Z.t;
    storage : string Fields.t;
    scope : string;
    epoch : int;
  }

  type trace = string * bool * string option * string option
    * (string * int * string * string list) list * string * (string * Z.t) list
    * (string * string * Z.t) list

  type session = {
    input : input;
    store : S.t;
    ledger : C.Ledger.t;
    active : bool;
    async_exec : bool;
    accounts : string list;
    trace : trace list ref;
    max_effort : int ref;
  }

  let field name value = J.member name value
  let string name value = field name value |> J.to_string
  let integer name value = field name value |> J.to_int

  let assoc value =
    J.to_assoc value |> List.fold_left (fun all (key, value) ->
      need ("duplicate RPC field: " ^ key) (not (Fields.mem key all));
      Fields.add key value all) Fields.empty

  let load folder file =
    let path = Filename.concat folder file in
    let raw = In_channel.with_open_bin path In_channel.input_all in
    Printf.printf "event = bridge_input file = %S sha256 = %s\n" path (sha raw);
    let json = Yojson.Safe.from_string raw in
    ignore (assoc json);
    json

  let result json =
    need "RPC error in deployed inputs" (field "error" json = `Null);
    let value = field "result" json in
    ignore (assoc value);
    value

  let rpc folder network name =
    load folder (network ^ "_" ^ name ^ ".json") |> result

  let stored storage key =
    match Fields.find_opt key storage with
    | Some value -> value
    | None -> failwith ("missing bridge state: " ^ key)

  let amount storage key = Z.of_string (stored storage key)

  let heads folder network =
    let before = rpc folder network "head_before" in
    let after = rpc folder network "head_after" in
    let identity value =
      integer "head_epoch" value, string "state_root" value,
      string "irmin_commit" value, string "txid_hi" value in
    need "RPC heads moved backwards"
      (integer "head_epoch" before <= integer "head_epoch" after);
    Printf.printf
      "event = bridge_heads network = %s folder = %S before = %d after = %d same_head = %b root_verified = false\n"
      network folder (integer "head_epoch" before) (integer "head_epoch" after)
      (identity before = identity after);
    integer "epoch" after, identity before, identity after

  let uncut value =
    List.iter (fun name -> match field name value with
      | `Null | `Bool false -> ()
      | _ -> failwith ("incomplete RPC storage: " ^ name)) ["truncated"; "has_more"]

  let dump folder network address =
    let file = network ^ "_dump.json" in
    if not (Sys.file_exists (Filename.concat folder file)) then None else
    let json = load folder file in
    match field "error" json with
    | `Assoc _ as error when integer "code" error = -32601 -> None
    | _ ->
      let value = result json in
      need "dump address differs" (string "address" value = address);
      uncut value;
      let storage = assoc (field "storage" value) in
      let sizes = assoc (field "storage_sizes" value) in
      need "dump count differs" (integer "count" value = Fields.cardinal storage);
      need "dump size keys differ"
        (List.map fst (Fields.bindings storage) = List.map fst (Fields.bindings sizes));
      Some (Fields.mapi (fun key value ->
        let raw = J.to_string value in
        need ("dump value truncated: " ^ key)
          (J.to_int (Fields.find key sizes) = String.length raw);
        raw) storage)

  let selected folder network kind =
    Sys.readdir folder |> Array.to_list |> List.sort String.compare
    |> List.filter (fun name -> String.starts_with ~prefix:(network ^ "_" ^ kind ^ "_") name
      && Filename.check_suffix name ".json")
    |> List.map (fun name -> load folder name |> result)

  let partial codefolder dumpfolder network =
    selected codefolder network "state" @ selected dumpfolder network "map"
    |> List.fold_left (fun storage value ->
      let key = string "key" value in
      need "storage truncation flag missing" (field "truncated" value = `Bool false);
      match field "value" value with
      | `Null ->
        need "missing storage size differs" (integer "size" value = 0);
        storage
      | `String raw ->
        need ("selected storage truncated: " ^ key) (integer "size" value = String.length raw);
        need ("selected storage differs: " ^ key)
          (match Fields.find_opt key storage with None -> true | Some old -> old = raw);
        Fields.add key raw storage
      | _ -> failwith "selected storage value is not a string") Fields.empty

  let validate_storage storage =
    List.iter (fun key -> need ("invalid bridge role: " ^ key)
      (C.Crypto.is_octra_address (stored storage key))) roles;
    List.iter (fun key -> need ("negative bridge state: " ^ key)
      (Z.sign (amount storage key) >= 0)) numbers;
    need "saved bridge is paused" (amount storage "paused" = Z.zero);
    need "saved bridge has an open call" (amount storage "reentrancy_lock" = Z.zero);
    List.iter (fun key -> need "map root cannot establish map contents"
      (not (Fields.mem key storage))) maps

  let load_input codefolder dumpfolder (network, address, expected) =
    let info = rpc codefolder network "info" in
    let code = rpc codefolder network "code" in
    let encoded = string "bytecode" code in
    let raw = Base64.decode_exn encoded in
    need "deployed address differs"
      (string "address" info = address && string "address" code = address);
    need "deployed raw hash differs"
      (sha raw = expected && string "code_hash" info = expected && string "code_hash" code = expected);
    need "bytecode encoded size differs" (integer "size" code = String.length encoded);
    need "invalid metadata owner" (C.Crypto.is_octra_address (string "owner" info));
    let _, code_before, code_after = heads codefolder network in
    let epoch, dump_before, dump_after = heads dumpfolder network in
    let storage, scope = match dump dumpfolder network address with
      | Some storage -> storage, "rpc_full_storage"
      | None -> partial codefolder dumpfolder network, "partial_state" in
    validate_storage storage;
    let balance = Z.of_string (string "balance" info) in
    need "negative saved balance" (Z.sign balance >= 0);
    need "invalid replay epoch" (epoch >= 6);
    Printf.printf
      "event = bridge_dataset network = %s scope = %s entries = %d reads_same_head = %b metadata = rpc_projection balance = earlier_info root_verified = false total_compatibility = false\n"
      network scope (Fields.cardinal storage)
      (code_before = code_after && code_after = dump_before && dump_before = dump_after);
    {network; address; code_hash = expected; encoded; metadata = Yojson.Safe.to_string info;
      version = string "version" info; balance; storage; scope; epoch}

  let state session = run (S.load_contract_storage session.store session.input.address) |> rows
  let balance session who = (Option.get (C.Ledger.find_opt session.ledger who)).balance
  let snapshot session = state session, List.map (fun who -> who, balance session who) session.accounts
  let digest state = `Assoc (List.map (fun (key, value) -> key, text value) state)
    |> Yojson.Safe.to_string |> sha

  let profile session =
    let prior = VM.default_ctx in
    let ctx = if not session.active then prior else {prior with
      point_ops = true; math = true; object_cost = true; int_work = V.Int_work.Active;
      scalar_work = true; fhe_work = C.Rule_graph.Active; proof_exec = C.Rule_graph.Active;
      proof_key = C.Rule_graph.Active; wasm_float = C.Rule_graph.Active} in
    {ctx with current_epoch = session.input.epoch; node_id = session.input.address;
      byte_work = (if session.active then Some (V.Byte_work.create V.Byte_work.consensus_limits) else None)}

  let record session ?(transfers = []) label (receipt : V.Contract.exec_result) =
    session.max_effort := max !(session.max_effort) receipt.effort_used;
    let storage, balances = snapshot session in
    let events = List.map (fun (event : VM.event_record) ->
      event.contract, event.depth, event.event, List.map VM.to_string event.values) receipt.events in
    session.trace := (label, receipt.success, receipt.error,
      Option.map VM.to_string receipt.return_value, events,
      digest storage, balances, transfers) :: !(session.trace)

  let with_call session ~caller ~value ~transfer name params inspect =
    let effect = E.create ~ledger:session.ledger ~store:session.store in
    Fun.protect ~finally:(fun () -> E.discard effect) (fun () ->
      let transfers = ref [] in
      let move from_addr target amount =
        transfers := (from_addr, target, amount) :: !transfers;
        transfer && E.apply effect
          (V.Call_plan.value_effect ~from_addr ~target ~amount ~balance:(E.balance effect from_addr)) in
      need "deployed value input refused" (value = Z.zero || move caller session.input.address value);
      let ctx = { (profile session) with get_balance = E.balance effect; do_transfer = move } in
      let receipt = if session.async_exec then
        run (V.Contract.execute_call_async ~ctx ~journal:(E.program effect)
          session.store session.input.address name params caller value)
        else V.Contract.execute_call ~ctx ~journal:(E.program effect)
          session.store session.input.address name params caller value in
      inspect effect receipt (List.rev !transfers))

  let invoke session ?(caller = user) ?(value = Z.zero) ?(transfer = true) success name params =
    let before = snapshot session in
    with_call session ~caller ~value ~transfer name params (fun effect receipt transfers ->
      if not transfer then begin
        need "deployed transfer refusal not reached" (List.length transfers = 1);
        need "deployed transfer refusal accepted" (not receipt.success)
      end;
      if receipt.success then E.commit_exn effect else E.discard effect;
      need ("deployed outcome differs: " ^ name ^ " effort = " ^ string_of_int receipt.effort_used
        ^ " error = " ^ Option.value receipt.error ~default:"none") (receipt.success = success);
      if not success then need ("deployed refusal changed state: " ^ name) (snapshot session = before);
      record session ~transfers name receipt;
      receipt)

  let view session name params =
    let before = snapshot session in
    let ctx = { (profile session) with get_balance = balance session } in
    let receipt = if session.async_exec then
      run (V.Contract.view_async ~ctx ~limit:1_000_000 session.store session.input.address name params user)
      else V.Contract.execute_view_call ~ctx ~limit:1_000_000
        session.store session.input.address name params user in
    need ("deployed view refused: " ^ name ^ " effort = " ^ string_of_int receipt.effort_used
      ^ " error = " ^ Option.value receipt.error ~default:"none") receipt.success;
    need ("deployed view changed state: " ^ name) (snapshot session = before);
    record session name receipt;
    Option.get receipt.return_value

  let expect_view session name params expected =
    let actual = VM.to_string (view session name params) in
    need ("deployed view differs: " ^ name ^ " expected = " ^ expected ^ " actual = " ^ actual)
      (actual = expected)

  let map_entries storage name =
    let prefix = name ^ ":" in
    Fields.bindings storage |> List.filter_map (fun (key, value) ->
      if String.starts_with ~prefix key then
        Some (String.sub key (String.length prefix) (String.length key - String.length prefix), value)
      else None)

  let sampled_views session =
    let storage = session.input.storage in
    let locks = map_entries storage "lock_history" |> List.filter (fun (nonce, _) ->
      Fields.mem ("lock_amounts:" ^ nonce) storage && Fields.mem ("lock_eth_recipients:" ^ nonce) storage) in
    let chosen = match locks with [] -> [] | first :: _ -> List.sort_uniq compare [first; List.hd (List.rev locks)] in
    List.iter (fun (nonce, who) -> expect_view session "get_lock_info" [text nonce]
      (who ^ "|" ^ stored storage ("lock_amounts:" ^ nonce) ^ "|"
        ^ stored storage ("lock_eth_recipients:" ^ nonce))) chosen;
    match List.find_opt (fun (_, value) -> value <> "0") (map_entries storage "processed_unlocks") with
    | None -> ()
    | Some (burn, value) ->
      expect_view session "is_unlock_processed" [text burn] value;
      ignore (invoke session ~caller:(stored storage "unlock_operator") false "unlock_trusted"
        [text recipient; text "1000000"; text burn])

  let daily_bytecode code name label key cap =
    let pc = match V.Contract.extract_method_target code name with
      | Some pc -> pc
      | None -> failwith ("deployed daily method missing: " ^ name) in
    let expected = [|VM.JDEST label; VM.LDI (1, VM.VInt cap); VM.SLOAD (2, key);
      VM.LDI (3, VM.VInt Z.zero); VM.ADD (2, 3, 2); VM.SUB (4, 1, 2);
      VM.MOV (0, 4); VM.STOP|] in
    need ("deployed daily bytecode differs: " ^ name)
      (pc >= 0 && pc <= Array.length code - Array.length expected
        && Array.sub code pc (Array.length expected) = expected)

  let daily_caps input =
    let lock_cap, unlock_cap = match input.code_hash with
      | "1479b1e69a6fa011fa423a861f91ed52e1441f31ffb2831b9e29c51d870c9793" ->
        "250000000000000", "115000000000000"
      | "2e7ec4136cc7031b95f8ae82ada933f788c3c309f81983569f13444f40e68ebd" ->
        "1000000000000", "1000000000000"
      | _ -> failwith "deployed daily cap hash unknown" in
    let raw = Base64.decode_exn input.encoded in
    need "deployed daily raw hash differs" (sha raw = input.code_hash);
    let code = V.Bytecode.decode_exn raw |> V.Contract.fix_jumps in
    let caps = ["daily_lock_remaining", 2700, "daily_locked", Z.of_string lock_cap;
      "daily_unlock_remaining", 2800, "daily_unlocked", Z.of_string unlock_cap] in
    List.iter (fun (name, label, key, cap) -> daily_bytecode code name label key cap) caps;
    caps

  let code_slice code pc expected =
    need "deployed transfer bytecode differs"
      (pc >= 0 && pc <= Array.length code - Array.length expected
        && Array.sub code pc (Array.length expected) = expected)

  let known_transfer input =
    need "deployed transfer hash unknown" (List.mem input.code_hash
      ["1479b1e69a6fa011fa423a861f91ed52e1441f31ffb2831b9e29c51d870c9793";
       "2e7ec4136cc7031b95f8ae82ada933f788c3c309f81983569f13444f40e68ebd"]);
    let raw = Base64.decode_exn input.encoded in
    need "deployed transfer raw hash differs" (sha raw = input.code_hash);
    let code = V.Bytecode.decode_exn raw in
    let fixed = V.Contract.fix_jumps code in
    need "deployed unlock target differs"
      (V.Contract.extract_method_target fixed "unlock_trusted" = Some 334);
    code_slice code 334 [|VM.JDEST 1200|];
    code_slice code 425 [|VM.TRANSFER (4, 1, 2); VM.EMIT ("Unlocked", [1; 2; 3]);
      VM.CALL_INT (4, 700); VM.LDI (4, VM.VBool true); VM.MOV (0, 4); VM.STOP; VM.STOP|];
    let pc = match fixed.(427) with
      | VM.CALL_INT (4, pc) -> pc
      | _ -> failwith "deployed unlock cleanup differs" in
    code_slice code pc [|VM.JDEST 700; VM.LDI (1, VM.VInt Z.zero);
      VM.SSTORE ("reentrancy_lock", 1); VM.STOP|]

  let initial_views session =
    let release = VM.to_string (view session "version" []) in
    need "deployed bridge version unknown" (release = "3.1.0" || release = "3.0.0");
    let caps = daily_caps session.input in
    List.iter (fun (name, key) -> expect_view session name [] (stored session.input.storage key))
      ["get_total_locked", "total_locked"; "get_total_unlocked", "total_unlocked";
       "get_lock_nonce", "lock_nonce"; "get_unlock_count", "unlock_count";
       "is_paused", "paused"; "get_unlock_operator", "unlock_operator"; "supply_invariant", "total_locked"];
    List.iter (fun (name, _, key, cap) -> expect_view session name []
      (Z.to_string (Z.sub cap (amount session.input.storage key)))) caps;
    sampled_views session

  let expect_delta session before updates =
    let expected = List.fold_left (fun state (key, value) -> Fields.add key value state)
      (Fields.of_seq (List.to_seq before)) updates |> Fields.bindings in
    need "deployed state delta differs" (state session = expected)

  let check_event session (receipt : V.Contract.exec_result) name values =
    match receipt.events with
    | [event] -> need "deployed event differs" (event.contract = session.input.address
        && event.depth = 0 && event.event = name && List.map VM.to_string event.values = values)
    | _ -> failwith "deployed event count differs"

  let daily storage epoch day_key total_key value =
    let previous = if amount storage day_key = z (epoch / 8640) then amount storage total_key else Z.zero in
    Z.to_string (Z.add previous value)

  let lock session =
    let input = session.input in
    let nonce = Z.to_string (Z.succ (amount input.storage "lock_nonce")) in
    List.iter (fun key -> need "local lock key already exists" (not (Fields.mem key input.storage)))
      ["lock_history:" ^ nonce; "lock_amounts:" ^ nonce; "lock_eth_recipients:" ^ nonce;
       "user_last_lock_epoch:" ^ user];
    let before = state session in
    let sender = balance session user in
    let receipt = invoke session ~value:(z 3_000_000) true "lock_to_eth" [text eth] in
    check_event session receipt "Locked" [user; "3000000"; eth; nonce];
    expect_delta session before ["total_locked", Z.to_string (Z.add (amount input.storage "total_locked") (z 3_000_000));
      "lock_nonce", nonce; "daily_locked", daily input.storage input.epoch "last_lock_day" "daily_locked" (z 3_000_000);
      "last_lock_day", string_of_int (input.epoch / 8640); "user_last_lock_epoch:" ^ user, string_of_int input.epoch;
      "lock_history:" ^ nonce, user; "lock_amounts:" ^ nonce, "3000000"; "lock_eth_recipients:" ^ nonce, eth];
    need "deployed lock balances differ" (balance session input.address = Z.add input.balance (z 3_000_000)
      && balance session user = Z.sub sender (z 3_000_000));
    expect_view session "get_lock_info" [text nonce] (user ^ "|3000000|" ^ eth);
    ignore (invoke session ~value:(z 1_000_000) false "lock_to_eth" [text eth])

  let unlock session burn =
    let input = session.input in
    let before = state session in
    let receiver = balance session recipient in
    let receipt = invoke session ~caller:(stored input.storage "unlock_operator") true
      "unlock_trusted" [text recipient; text "1000000"; text burn] in
    check_event session receipt "Unlocked" [recipient; "1000000"; burn];
    expect_delta session before ["total_locked", Z.to_string (Z.add (amount input.storage "total_locked") (z 2_000_000));
      "total_unlocked", Z.to_string (Z.add (amount input.storage "total_unlocked") (z 1_000_000));
      "unlock_count", Z.to_string (Z.succ (amount input.storage "unlock_count"));
      "daily_unlocked", daily input.storage input.epoch "last_unlock_day" "daily_unlocked" (z 1_000_000);
      "last_unlock_day", string_of_int (input.epoch / 8640); "processed_unlocks:" ^ burn, "1"];
    need "deployed unlock balances differ" (balance session input.address = Z.add input.balance (z 2_000_000)
      && balance session recipient = Z.add receiver (z 1_000_000));
    expect_view session "is_unlock_processed" [text burn] "1";
    expect_view session "supply_invariant" [] (Z.to_string (Z.add (amount input.storage "total_locked") (z 2_000_000)))

  let role_refusals session failed =
    List.iter (fun (name, params) -> ignore (invoke session false name params))
      ["pause", []; "unpause", []; "set_unlock_operator", [text recipient];
       "update_guardian", [text recipient]; "transfer_ownership", [text recipient];
       "unlock_trusted", [text recipient; text "1000000"; text failed]]

  let replay_refusal session burn =
    let caller = stored session.input.storage "unlock_operator" in
    ignore (invoke session ~caller false "unlock_trusted" [text recipient; text "1000000"; text burn])

  let transfer_required session failed =
    let caller = stored session.input.storage "unlock_operator" in
    ignore (invoke session ~caller ~transfer:false false "unlock_trusted"
      [text recipient; text "1000000"; text failed]);
    expect_view session "is_unlock_processed" [text failed] "0"

  let transfer_delta session before burn =
    let input = session.input in
    let value = z 1_000_000 in
    let locked = Z.sub (amount input.storage "total_locked") value in
    expect_delta session (fst before) ["total_locked", Z.to_string locked;
      "total_unlocked", Z.to_string (Z.add (amount input.storage "total_unlocked") value);
      "unlock_count", Z.to_string (Z.succ (amount input.storage "unlock_count"));
      "daily_unlocked", daily input.storage input.epoch "last_unlock_day" "daily_unlocked" value;
      "last_unlock_day", string_of_int (input.epoch / 8640); "processed_unlocks:" ^ burn, "1"];
    need "known transfer refusal moved balance" (snd (snapshot session) = snd before);
    need "known transfer refusal surplus differs" (Z.sub (balance session input.address) locked
      = Z.add (Z.sub input.balance (amount input.storage "total_locked")) value)

  let transfer_known session burn =
    known_transfer session.input;
    need "known transfer burn already exists"
      (not (Fields.mem ("processed_unlocks:" ^ burn) session.input.storage));
    let before = snapshot session in
    let caller = stored session.input.storage "unlock_operator" in
    with_call session ~caller ~value:Z.zero ~transfer:false "unlock_trusted"
      [text recipient; text "1000000"; text burn] (fun effect receipt transfers ->
        need "known transfer refusal arguments differ"
          (transfers = [session.input.address, recipient, z 1_000_000]);
        need "known transfer refusal outcome differs"
          (receipt.success && receipt.error = None && receipt.return_value = Some (VM.VBool true));
        check_event session receipt "Unlocked" [recipient; "1000000"; burn];
        E.commit_exn effect;
        transfer_delta session before burn;
        record session ~transfers "transfer_ignored" receipt);
    expect_view session "is_unlock_processed" [text burn] "1";
    replay_refusal session burn;
    transfer_delta session before burn

  let install input store ledger =
    let actors = List.map (stored input.storage) roles |> List.sort_uniq String.compare in
    List.iter (fun who -> need "synthetic actor overlaps saved identity"
      (who <> input.address && not (List.mem who actors))) [user; recipient];
    need "vault is also a role account" (not (List.mem input.address actors));
    List.iter (fun who -> ok (C.Ledger.add_account ledger who Z.zero)) actors;
    List.iter (fun who -> ok (C.Ledger.add_account ledger who (z 10_000_000))) [user; recipient];
    ok (C.Ledger.add_account ledger input.address input.balance);
    run (C.Ledger.flush_dirty_lwt ledger);
    run (S.begin_epoch_batch store);
    run (S.write store ["contracts"; input.address; "meta"] input.metadata);
    run (S.write store ["contracts"; input.address; "bytecode"] input.encoded);
    let storage = Hashtbl.create (Fields.cardinal input.storage) in
    Fields.iter (Hashtbl.add storage) input.storage;
    run (S.save_contract_storage store input.address storage);
    List.sort_uniq String.compare (input.address :: user :: recipient :: actors)

  let with_session scenario input active async_exec inspect =
    Printf.printf
      "event = bridge_replay scenario = %s network = %s scope = %s active = %b async = %b execution = vm limit = 1000000\n%!"
      scenario input.network input.scope active async_exec;
    Test_workspace.with_dir "bridge_deployed" (fun path ->
      let store = run (S.open_store (Filename.concat path "store")) in
      Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
        let ledger = C.Ledger.create store in
        let accounts = install input store ledger in
        let session = {input; store; ledger; active; async_exec; accounts; trace = ref []; max_effort = ref 0} in
        need "imported storage differs" (state session = Fields.bindings input.storage);
        inspect session))

  let normal_case session =
    let burn = "local-" ^ session.input.code_hash in
    let failed = burn ^ "-refused" in
    List.iter (fun id -> need "local burn key already exists"
      (not (Fields.mem ("processed_unlocks:" ^ id) session.input.storage))) [burn; failed];
    initial_views session;
    lock session;
    unlock session burn;
    role_refusals session failed;
    replay_refusal session burn;
    expect_view session "is_unlock_processed" [text failed] "0";
    let locked = Z.of_string (List.assoc "total_locked" (state session)) in
    need "deployed reserve surplus changed" (Z.sub (balance session session.input.address) locked
      = Z.sub session.input.balance (amount session.input.storage "total_locked"));
    failed

  let save_session session scenario =
    let input = session.input in
    let final = snapshot session in
    run (C.Ledger.flush_dirty_lwt session.ledger);
    run (S.commit_epoch_batch session.store "deployed bridge local replay");
    need "deployed saved state differs" (snapshot session = final);
    need "deployed metadata changed"
      (run (S.read session.store ["contracts"; input.address; "meta"]) = Some input.metadata);
    need "deployed bytecode changed" (run (S.load_bytecode session.store input.address) = Some input.encoded);
    Printf.printf
      "event = bridge_deployed scenario = %s network = %s address = %s code = %s version = %S scope = %s active = %b async = %b calls = %d max_effort = %d state = %s surplus = %s root_verified = false total_compatibility = false\n%!"
      scenario input.network input.address input.code_hash input.version input.scope session.active session.async_exec
      (List.length !(session.trace)) !(session.max_effort) (digest (fst final))
      (Z.to_string (Z.sub (balance session input.address) (Z.of_string (List.assoc "total_locked" (fst final)))));
    List.rev !(session.trace), final

  let check input active async_exec =
    with_session "safety" input active async_exec (fun session ->
      let failed = normal_case session in
      transfer_required session failed;
      save_session session "safety")

  let compat input active async_exec =
    known_transfer input;
    let normal = with_session "compat" input active async_exec (fun session ->
      ignore (normal_case session);
      save_session session "compat") in
    let deficient = with_session "transfer_ignored" input active async_exec (fun session ->
      transfer_known session ("local-" ^ input.code_hash ^ "-ignored");
      save_session session "transfer_ignored") in
    Printf.printf
      "event = bridge_deployed_compat network = %s active = %b async = %b status = characterized transfer_check = false deployment_ready = false root_verified = false total_compatibility = false\n%!"
      input.network active async_exec;
    normal, deficient

  let compare_contexts check input =
    let prior = check input false false in
    List.iter (fun (active, async_exec) -> need "deployed context results differ"
      (check input active async_exec = prior)) [false, true; true, false; true, true]

  let run_all mode codefolder dumpfolder =
    List.iter (fun pin ->
      let input = load_input codefolder dumpfolder pin in
      match mode with
      | Safety -> compare_contexts check input
      | Compat -> compare_contexts compat input)
      ["mainnet", "oct5MrNfjiXFNRDLwsodn8Zm9hDKNGAYt3eQDCQ52bSpCHq",
        "1479b1e69a6fa011fa423a861f91ed52e1441f31ffb2831b9e29c51d870c9793";
       "devnet", "octBpYDyxxdmc8EqjTbcW2gds9Y3K6yakP6noyjWcWqjYNH",
        "2e7ec4136cc7031b95f8ae82ada933f788c3c309f81983569f13444f40e68ebd"];
    match mode with
    | Safety ->
      print_endline "event = bridge_deployed status = passed root_verified = false total_compatibility = false"
    | Compat -> print_endline
      "event = bridge_deployed_compat status = characterized combinations = 8 transfer_check = false deployment_ready = false root_verified = false total_compatibility = false"
end

let source_main () =
  need "bridge source inputs required" (Array.length Sys.argv = 3);
  Array.sub Sys.argv 1 2 |> Array.iter (fun path ->
    let source = In_channel.with_open_bin path In_channel.input_all in
    List.iter (fun historical -> List.iter (fun active -> List.iter
      (check source historical active) [false; true]) [false; true]) [true; false]);
  print_endline "event = bridge_compat status = passed"

let () =
  match if Array.length Sys.argv > 1 then Sys.argv.(1) else "" with
  | "--deployed" ->
    need "usage: --deployed codefolder dumpfolder" (Array.length Sys.argv = 4);
    Deployed.run_all Deployed.Safety Sys.argv.(2) Sys.argv.(3)
  | "--deployed-compat" ->
    need "usage: --deployed-compat codefolder dumpfolder" (Array.length Sys.argv = 4);
    Deployed.run_all Deployed.Compat Sys.argv.(2) Sys.argv.(3)
  | _ -> source_main ()