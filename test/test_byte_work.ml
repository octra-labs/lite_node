(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_vm
open Contract_vm

let require condition reason = if not condition then failwith reason

let limits ?(write = 128) ?(alloc = 1024) ?(unit_bytes = 32) () =
  Option.get (Byte_work.limits ~key_bytes:16 ~value_bytes:128
    ~copy_bytes:256 ~write_bytes:write ~alloc_bytes:alloc ~unit_bytes)

let state ?(limit = 1000) ?budget () =
  create_state ~limit ~ctx:{default_ctx with byte_work = budget}
    ~caller:"" ~origin:"" ~address:"" ~value:Z.zero
    ~storage:(Hashtbl.create 8) ()

let batch st entries =
  st.regs.(0) <- VInt (Z.of_int 10);
  st.regs.(1) <- VInt (Z.of_int 1010);
  st.regs.(2) <- VInt (Z.of_int (List.length entries));
  List.iteri (fun index (key, value) ->
    Hashtbl.replace st.memory.data (10 + index) (VString key);
    Hashtbl.replace st.memory.data (1010 + index) (VString value)) entries;
  exec_one st (SSTOREN (0, 1, 2))

let check_plan () =
  let rules = limits () in
  let plan requests =
    Byte_work.plan rules ~remaining:128 ~available:1024 ~used:10 ~limit:100 ~base:5 requests in
  require (plan [Byte_work.Write (1, 32)] = Some (17, 95, 1024)) "write price";
  require (plan [Byte_work.Copy (32, 32)] = Some (17, 128, 960)) "copy price";
  require (plan [Byte_work.Erase 64] = Some (17, 64, 1024)) "delete long key";
  List.iter (fun requests ->
    require (plan requests = None) "invalid byte request accepted")
    [[Byte_work.Write (-1, 1)]; [Byte_work.Write (0, 1)]; [Byte_work.Write (17, 1)];
     [Byte_work.Write (1, 129)]; [Byte_work.Copy (256, 1)];
     [Byte_work.Copy (max_int, max_int)]; [Byte_work.Write (1, 128)];
     [Byte_work.Write (1, 64); Byte_work.Write (1, 64)]];
  require (Byte_work.limits ~key_bytes:1 ~value_bytes:1 ~copy_bytes:1
    ~write_bytes:1 ~alloc_bytes:1 ~unit_bytes:0 = None) "zero unit accepted";
  let budget = Byte_work.create rules in
  require (Byte_work.charge budget ~used:0 ~limit:1 ~base:0
    [Byte_work.Write (1, 64)] = None) "effort cap ignored";
  require (Byte_work.remaining budget = 128) "failed plan consumed bytes"

let check_price_grid () =
  let rules = limits ~write:1024 () in
  for key = 1 to 16 do
    for value = 0 to 128 do
      let bytes = key + value in
      let cost = (bytes + 31) / 32 in
      List.iter (fun available ->
        let actual = Byte_work.plan rules ~remaining:1024 ~available:1024 ~used:7
          ~limit:(7 + available) ~base:3 [Byte_work.Write (key, value)] in
        let expected = if available < 3 + cost then None
          else Some (10 + cost, 1024 - bytes, 1024) in
        require (actual = expected) "byte price grid") [0; 3 + cost - 1; 3 + cost]
    done
  done;
  let huge = Option.get (Byte_work.limits ~key_bytes:Sys.max_string_length
    ~value_bytes:Sys.max_string_length ~copy_bytes:Sys.max_string_length
    ~write_bytes:max_int ~alloc_bytes:max_int ~unit_bytes:1) in
  require (Byte_work.plan huge ~remaining:max_int ~available:max_int ~used:1 ~limit:max_int
    ~base:max_int [] = None) "effort overflow accepted";
  require (Byte_work.plan huge ~remaining:max_int ~available:max_int ~used:0 ~limit:max_int
    ~base:0 [Byte_work.Erase max_int; Byte_work.Erase 1] = None)
    "volume overflow accepted";
  require (Byte_work.plan huge ~remaining:max_int ~available:max_int ~used:0 ~limit:max_int
    ~base:0 [Byte_work.Erase max_int] = Some (max_int, 0, max_int))
    "exact maximum refused"

let check_batch_atomic () =
  let budget = Byte_work.create (limits ()) in
  let st = state ~budget () in
  require (not (batch st ["a", String.make 64 'x'; "b", String.make 64 'y']))
    "batch write budget ignored";
  require (st.reverted && Hashtbl.length st.storage = 0 && st.undo_stack = [])
    "refused batch changed storage";
  require (Byte_work.remaining budget = 128) "refused batch consumed bytes"

let check_tariff () =
  List.iter (fun size ->
    let value = String.make size 'x' in
    let price = (size + 1 + 31) / 32 in
    List.iter (fun indexed ->
      let budget = Byte_work.create (limits ~write:1024 ()) in
      let st = state ~budget () in
      st.regs.(0) <- VString "a";
      st.regs.(1) <- VString value;
      let op = if indexed then SSTOREK (0, 1) else SSTORE ("a", 1) in
      require (exec_one st op) "single write refused";
      require (st.effort_used = 100 + price) "single byte price";
      require (Byte_work.remaining budget = 1023 - size) "single byte volume";
      require (Hashtbl.find st.storage "a" = value) "single bytes changed") [false; true];
    let budget = Byte_work.create (limits ~write:1024 ()) in
    let st = state ~budget () in
    require (batch st ["a", value]) "priced batch refused";
    require (st.effort_used = 180 + price) "batch byte price";
    require (Byte_work.remaining budget = 1023 - size) "batch byte volume";
    let prior = state () in
    require (batch prior ["a", value]) "prior batch refused";
    require (prior.effort_used = 180) "prior batch price changed")
    [0; 1; 30; 31; 32; 63; 64; 127; 128]

let check_invalid_batch () =
  List.iter (fun entries ->
    let budget = Byte_work.create (limits ()) in
    let st = state ~budget () in
    Hashtbl.add st.storage "old" "value";
    require (not (batch st entries)) "invalid batch accepted";
    require (Hashtbl.length st.storage = 1 && Hashtbl.find st.storage "old" = "value")
      "invalid batch changed state";
    require (Byte_work.remaining budget = 128) "invalid batch charged volume")
    [["a", "ok"; "", "no"]; ["a", "ok"; String.make 17 'k', "no"];
     ["a", "ok"; "\000spawn_nonce", "no"]; ["a", String.make 129 'x']];
  let budget = Byte_work.create (limits ()) in
  let st = state ~budget ~limit:181 () in
  require (not (batch st ["a", "x"; "b", "y"])) "batch effort cap ignored";
  require (Hashtbl.length st.storage = 0 && Byte_work.remaining budget = 128)
    "effort refusal changed storage";
  List.iter (fun count ->
    let st = state ~budget () in
    st.regs.(2) <- VInt count;
    require (not (exec_one st (SSTOREN (0, 1, 2)))) "invalid batch span accepted")
    [Z.minus_one; Z.of_int 1001; Z.shift_left Z.one 100]

let check_rollback () =
  let budget = Byte_work.create (limits ()) in
  let st = state ~budget () in
  st.regs.(1) <- VString (String.make 63 'x');
  require (exec_one st CHECKPOINT) "checkpoint refused";
  require (exec_one st (SSTORE ("a", 1))) "initial write refused";
  require (exec_one st ROLLBACK) "rollback refused";
  require (Hashtbl.length st.storage = 0 && Byte_work.remaining budget = 64)
    "rollback refunded work";
  require (exec_one st (SSTORE ("b", 1))) "second write refused";
  require (Byte_work.remaining budget = 0) "gross write volume changed";
  require (not (exec_one st (SSTORE ("c", 1)))) "write budget exceeded";
  require (not (Hashtbl.mem st.storage "c")) "refused write persisted"

let check_concat () =
  List.iter (fun (left, right, accepted) ->
    let budget = Byte_work.create (limits ()) in
    let st = state ~budget () in
    st.regs.(0) <- VString (String.make left 'a');
    st.regs.(1) <- VString (String.make right 'b');
    st.regs.(2) <- VString "unchanged";
    require (exec_one st (CONCAT (2, 0, 1)) = accepted) "concat limit";
    if accepted then begin
      require (st.effort_used = 3 + (left + right + 31) / 32) "concat price";
      require (to_string st.regs.(2) = String.make left 'a' ^ String.make right 'b')
        "concat bytes changed"
    end else
      require (st.regs.(2) = VString "unchanged") "refused concat changed register";
    require (Byte_work.remaining budget = 128) "concat spent storage budget")
    [0, 0, true; 1, 31, true; 128, 128, true; 128, 129, false];
  let budget = Byte_work.create (limits ()) in
  let st = state ~budget ~limit:3 () in
  st.regs.(0) <- VBytes "a";
  st.regs.(1) <- VBytes "b";
  require (not (exec_one st (CONCAT (2, 0, 1)))) "concat effort cap ignored";
  let st = state ~budget () in
  st.regs.(0) <- VInt (Z.shift_left Z.one 5000);
  require (not (exec_one st (CONCAT (2, 0, 1)))) "decimal conversion cap ignored"

let check_copy_volume () =
  let budget = Byte_work.create (limits ()) in
  let st = state ~budget ~limit:1_000_000 () in
  st.regs.(0) <- VString (String.make 128 'a');
  st.regs.(1) <- VString (String.make 128 'b');
  let rec copies index =
    if index = 5000 then failwith "copy volume not limited"
    else if exec_one st (CONCAT (2, 0, 1)) then begin
      Hashtbl.replace st.memory.data index st.regs.(2);
      copies (index + 1)
    end
  in
  copies 0

let check_delete () =
  List.iter (fun indexed ->
    let budget = Byte_work.create (limits ()) in
    let st = state ~budget () in
    let key = String.make 17 'k' in
    Hashtbl.add st.storage key "historic";
    st.regs.(0) <- VString key;
    let op = if indexed then SDELK 0 else SDEL key in
    require (exec_one st op) "historic key deletion refused";
    require (not (Hashtbl.mem st.storage key)) "key not deleted";
    require (Byte_work.remaining budget = 111) "delete volume";
    require (st.effort_used = effort_cost op + 1) "delete price")
    [false; true]

let check_blob () =
  let rules = Option.get (Byte_work.limits ~key_bytes:64 ~value_bytes:128
    ~copy_bytes:256 ~write_bytes:128 ~alloc_bytes:1024 ~unit_bytes:32) in
  List.iter (fun (size, accepted) ->
    let budget = Byte_work.create rules in
    let st = state ~budget () in
    st.regs.(0) <- VString (String.make size 'x');
    require (exec_one st (FSTORE (1, 0)) = accepted) "blob volume limit";
    require (Hashtbl.length st.blobs = if accepted then 1 else 0) "blob mutation";
    require (Byte_work.remaining budget = if accepted then 128 - 64 - size else 128)
      "blob charged volume") [64, true; 65, false]

let check_view () =
  let budget = Byte_work.create (limits ()) in
  let st = create_state ~ctx:{default_ctx with byte_work = Some budget} ~is_view:true
    ~caller:"" ~origin:"" ~address:"" ~value:Z.zero ~storage:(Hashtbl.create 1) () in
  require (not (batch st ["a", "x"])) "view write accepted";
  require (Byte_work.remaining budget = 128 && Hashtbl.length st.storage = 0)
    "view changed storage budget"

let check_nested () =
  List.iter (fun success ->
    let budget = Byte_work.create (limits ()) in
    let ctx = {default_ctx with byte_work = Some budget;
      call_contract = (fun _ _ _ _ scope ->
        require (Option.equal (==) scope.bytes (Some budget) && scope.depth = 1)
          "child budget lost";
        require (scope.limit <> None) "child effort absent";
        let child = state ?budget:scope.bytes ?limit:scope.limit () in
        child.regs.(0) <- VString (String.make 63 'x');
        require (exec_one child (SSTORE ("a", 0))) "child write refused";
        if success then
          Ok {return_value = VInt Z.one; effort_used = child.effort_used; events = []}
        else Error "child refused")} in
    let st = create_state ~ctx ~limit:2000 ~caller:"" ~origin:"" ~address:""
      ~value:Z.zero ~storage:(Hashtbl.create 1) () in
    require (exec_one st (XCALL (0, 1, 2, 3, 0)) = success) "nested call result";
    require (Byte_work.remaining budget = 64) "child work lost";
    if success then begin
      st.regs.(1) <- VString (String.make 64 'x');
      require (not (exec_one st (SSTORE ("b", 1)))) "child and parent budgets separated"
    end) [false; true]

let check_spawn () =
  List.iter (fun version ->
    let budget = Byte_work.create (limits ~write:1024 ()) in
    let called = ref false in
    let ctx = {default_ctx with byte_work = Some budget;
      deploy_contract = (fun _ _ _ scope _ ->
        called := true;
        require (Option.equal (==) scope.bytes (Some budget)
          && scope.limit <> None && scope.depth = 1)
          "constructor scope lost";
        let child = state ?budget:scope.bytes ?limit:scope.limit () in
        child.regs.(0) <- VString "x";
        require (exec_one child (SSTORE ("a", 0))) "constructor write refused";
        Ok {spawned_addr = "program"; effort_used = child.effort_used; events = []})} in
    let st = create_state ~ctx ~limit:20000 ~caller:"" ~origin:"" ~address:""
      ~value:Z.zero ~storage:(Hashtbl.create 1) () in
    st.regs.(0) <- VString "OCTB12345678";
    let op = if version = 1 then SPAWN (1, 0) else SPAWN2 (1, 0, 2, 0) in
    require (exec_one st op && !called) "constructor not called";
    let written = String.length "\000spawn_nonce" + 1 + 2 in
    require (Byte_work.remaining budget = 1024 - written) "constructor volume")
    [1; 2]

let check_numeric () =
  let budget = Byte_work.create (limits ()) in
  let st = state ~budget () in
  st.regs.(0) <- VInt (Z.of_int (-123));
  require (exec_one st (SSTORE ("a", 0))) "numeric write refused";
  require (Hashtbl.find st.storage "a" = "-123") "numeric bytes changed";
  require (Byte_work.remaining budget = 123) "numeric volume";
  let st = state ~budget () in
  ignore (batch st []);
  st.regs.(2) <- VInt Z.one;
  Hashtbl.replace st.memory.data 10 (VString "a");
  Hashtbl.replace st.memory.data 1010 (VInt (Z.shift_left Z.one 5000));
  require (not (exec_one st (SSTOREN (0, 1, 2)))) "batch decimal cap ignored";
  require (Hashtbl.length st.storage = 0) "oversized number stored"

let check_alloc_plan () =
  let rules = limits ~alloc:128 () in
  for size = 0 to 128 do
    for free = 0 to 128 do
      let cost = (size + 31) / 32 in
      let actual = Byte_work.plan rules ~remaining:128 ~available:free
        ~used:3 ~limit:100 ~base:2 [Byte_work.Copy (size, 0)] in
      let expected =
        if size > free then None else Some (5 + cost, 128, free - size) in
      require (actual = expected) "allocation grid"
    done
  done;
  let budget = Byte_work.create rules in
  require (Byte_work.charge budget ~used:0 ~limit:100 ~base:0
    [Byte_work.Write (1, 1); Byte_work.Allocate 129] = None) "mixed plan accepted";
  require (Byte_work.remaining budget = 128 && Byte_work.available budget = 128)
    "mixed plan partially charged";
  require (Byte_work.plan rules ~remaining:128 ~available:128 ~used:0
    ~limit:max_int ~base:0 [Byte_work.Allocate max_int; Byte_work.Allocate 1] = None)
    "allocation overflow accepted"

let check_copy_rollback () =
  let budget = Byte_work.create (limits ~alloc:64 ()) in
  let st = state ~budget () in
  st.regs.(0) <- VString (String.make 32 'x');
  st.regs.(1) <- VString "";
  require (exec_one st CHECKPOINT) "copy checkpoint";
  require (exec_one st (CONCAT (2, 0, 1))) "copy before rollback";
  require (exec_one st ROLLBACK) "copy rollback";
  require (Byte_work.available budget = 32) "copy refunded";
  let child = state ~budget () in
  child.regs.(0) <- st.regs.(0);
  child.regs.(1) <- VString "";
  require (exec_one child (CONCAT (2, 0, 1))) "shared copy";
  require (Byte_work.available budget = 0) "shared allocation lost";
  require (not (exec_one st (CONCAT (2, 0, 1)))) "shared allocation exceeded"

let check_slices () =
  List.iter (fun (start, length) ->
    List.iter (fun typed ->
      let prior = state () in
      let budget = Byte_work.create (limits ~alloc:32 ()) in
      let active = state ~budget () in
      List.iter (fun st ->
        st.regs.(0) <- if typed then VBytes "abc\000de" else VString "abc\000de";
        st.regs.(1) <- VInt (Z.of_int start);
        st.regs.(2) <- VInt (Z.of_int length)) [prior; active];
      let op = SUBSTR (3, 0, 1, 2) in
      require (exec_one prior op && exec_one active op) "slice refused";
      require (prior.regs.(3) = active.regs.(3)) "slice bytes changed";
      require (Byte_work.available budget = 32 - String.length (to_string active.regs.(3)))
        "slice allocation missing") [false; true])
    [-1, 5; 0, 0; 0, 6; 2, 3; 5, 9; 6, 1; 7, 1; 2, -1];
  let budget = Byte_work.create (limits ~alloc:2 ()) in
  let st = state ~budget () in
  st.regs.(0) <- VString "abc";
  st.regs.(1) <- VInt Z.zero;
  st.regs.(2) <- VInt (Z.of_int 3);
  st.regs.(3) <- VString "unchanged";
  require (not (exec_one st (SUBSTR (3, 0, 1, 2)))) "slice volume ignored";
  require (st.regs.(3) = VString "unchanged") "refused slice published";
  List.iter (fun op ->
    let budget = Byte_work.create (limits ~alloc:64 ()) in
    let st = state ~budget () in
    st.regs.(0) <- VString "abc";
    require (exec_one st op) "hash refused";
    require (Byte_work.available budget = 0) "hash allocation missing";
    require (not (exec_one st op)) "hash allocation exceeded")
    [SHA256 (1, 0); KECCAK256 (1, 0)]

let check_find () =
  let rec words count =
    if count = 0 then [""]
    else
      let shorter = words (count - 1) in
      "" :: List.concat_map (fun word ->
        List.map (fun byte -> String.make 1 byte ^ word) ['a'; 'b'; '\000']) shorter in
  let reference text pattern =
    let width = String.length pattern in
    let rec scan index =
      if index + width > String.length text then -1
      else if String.sub text index width = pattern then index else scan (index + 1) in
    scan 0 in
  List.iter (fun text ->
    List.iter (fun pattern ->
      require (Text_work.find text pattern = reference text pattern) "search result")
      (words 3)) (words 5);
  let text = String.make 100_000 'a' in
  let pattern = String.make 255 'a' ^ "b" in
  let before = Gc.allocated_bytes () in
  let found = Text_work.find text pattern in
  let allocated = Gc.allocated_bytes () -. before in
  require (found = -1 && allocated < 50_000.) "search copies input";
  List.iter (fun (text, pattern) ->
    let prior = state () in
    let active = state ~budget:(Byte_work.create (limits ())) () in
    List.iter (fun st ->
      st.regs.(0) <- VString text;
      st.regs.(1) <- VString pattern) [prior; active];
    let op = INDEXOF (2, 0, 1) in
    require (exec_one prior op && exec_one active op) "vm search refused";
    require (prior.regs.(2) = active.regs.(2)) "vm search result")
    ["aabb", "bb"; "aaaa", "ab"; "abc", ""; "", "abc"; "ababab", "abab"];
  let budget = Byte_work.create (limits ~alloc:15 ()) in
  let st = state ~budget () in
  st.regs.(0) <- VString "aabb";
  st.regs.(1) <- VString "bb";
  require (not (exec_one st (INDEXOF (2, 0, 1)))) "search scratch ignored"

let check_key_page () =
  let keys = List.init 2103 (fun index -> "p/" ^ string_of_int index) in
  let keys = "\000private" :: "other" :: "p/\000" :: "p/\255" :: keys in
  List.iter (fun after ->
    List.iter (fun capacity ->
      let expected = List.filter (fun key ->
        String.length key >= 2 && String.sub key 0 2 = "p/"
        && (after = "" || String.compare (String.sub key 2 (String.length key - 2)) after > 0))
        keys |> List.sort String.compare
        |> List.filteri (fun index _ -> index < capacity) in
      List.iter (fun inputs ->
        let actual = Text_work.page ~prefix:"p/" ~after ~capacity ~iter:(fun visit ->
          List.iter visit inputs) in
        require (actual = expected) "key page order";
        List.iter (fun value ->
          require (List.exists ((==) value) inputs) "key page copied string") actual)
        [keys; List.rev keys]) [0; 1; 17; 1001]) [""; "1"; "1999"; "z"; "\255"];
  let budget = Byte_work.create (limits ~alloc:100_000 ()) in
  let prior = state ~limit:100_000 () in
  let active = state ~budget ~limit:100_000 () in
  List.iter (fun st ->
    List.iter (fun key -> Hashtbl.add st.storage key "value") keys;
    st.regs.(0) <- VString "p/";
    st.regs.(1) <- VString "";
    st.regs.(2) <- VInt (Z.of_int 10)) [prior; active];
  let op = SKEYS_PAGE (3, 4, 0, 1, 2) in
  require (exec_one prior op && exec_one active op) "key page refused";
  require (prior.regs.(3) = active.regs.(3) && prior.regs.(4) = active.regs.(4))
    "key cursor changed";
  Hashtbl.iter (fun index value ->
    require (Hashtbl.find_opt active.memory.data index = Some value) "key value changed")
    prior.memory.data;
  let budget = Byte_work.create (limits ~alloc:128 ()) in
  let st = state ~budget ~limit:100_000 () in
  Hashtbl.add st.storage "p/abc" "value";
  st.regs.(0) <- VString "p/";
  st.regs.(1) <- VInt Z.zero;
  st.regs.(2) <- VString "unchanged";
  require (not (exec_one st (SKEYS (2, 0, 1)))) "key copy quota ignored";
  require (Hashtbl.length st.memory.data = 0 && st.regs.(2) = VString "unchanged")
    "key page partially published";
  let long_keys = List.init 10_000 (fun index ->
    string_of_int index ^ String.make 1024 'k') in
  let before = Gc.allocated_bytes () in
  let selected = Text_work.page ~prefix:"" ~after:"" ~capacity:1001
    ~iter:(fun visit -> List.iter visit long_keys) in
  let allocated = Gc.allocated_bytes () -. before in
  require (List.length selected = 1001 && allocated < 1_000_000.)
    "key page copies all matches"

let check_scan_limit () =
  let seen = ref 0 in
  let rec keys () =
    incr seen;
    Seq.Cons ("key", keys) in
  require (Text_work.measure ~fits:(fun bytes -> bytes <= 70) keys = None)
    "key scan did not stop";
  require (!seen = 3) "key scan exceeded work";
  require (Text_work.measure ~fits:(fun _ -> false) keys = None && !seen = 3)
    "refused scan visited storage";
  List.iter (fun values ->
    require (Text_work.measure ~fits:(fun bytes -> bytes <= 71)
      (List.to_seq values) = Some 71) "key work order") [["a"; "bcdefg"]; ["bcdefg"; "a"]]

let () =
  check_alloc_plan ();
  check_copy_rollback ();
  check_slices ();
  check_find ();
  check_key_page ();
  check_scan_limit ();
  check_plan ();
  check_price_grid ();
  check_batch_atomic ();
  check_tariff ();
  check_invalid_batch ();
  check_rollback ();
  check_concat ();
  check_copy_volume ();
  check_delete ();
  check_blob ();
  check_view ();
  check_nested ();
  check_spawn ();
  check_numeric ();
  Printf.printf "byte_work = pass\n"