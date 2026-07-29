(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let trim s = String.trim s

let parse_reg s =
  let s = trim s in
  if String.length s >= 2 && s.[0] = 'r' then
    int_of_string (String.sub s 1 (String.length s - 1))
  else failwith (Printf.sprintf "bad register: %s" s)

let parse_int s =
  let s = trim s in
  Z.of_string s

let parse_value s =
  let s = trim s in
  if String.length s >= 2 && s.[0] = '"' && s.[String.length s - 1] = '"' then
    Contract_vm.VString (String.sub s 1 (String.length s - 2))
  else if s = "true" then Contract_vm.VBool true
  else if s = "false" then Contract_vm.VBool false
  else Contract_vm.VInt (Z.of_string s)

let split_args s =
  let buf = Buffer.create 16 in
  let results = ref [] in
  let in_str = ref false in
  String.iter (fun c ->
    if c = '"' then (in_str := not !in_str; Buffer.add_char buf c)
    else if c = ',' && not !in_str then (
      results := Buffer.contents buf :: !results;
      Buffer.clear buf)
    else Buffer.add_char buf c
  ) s;
  let last = Buffer.contents buf in
  if String.length (trim last) > 0 then results := last :: !results;
  List.rev !results |> List.map trim

let is_quoted s =
  String.length s >= 2 && s.[0] = '"' && s.[String.length s - 1] = '"'

let unquote s =
  String.sub s 1 (String.length s - 2)

let parse_line line =
  let line = trim line in
  if String.length line = 0 || line.[0] = ';' then None
  else
    let space = try String.index line ' ' with Not_found -> String.length line in
    let op = String.uppercase_ascii (String.sub line 0 space) in
    let rest = if space < String.length line then
      trim (String.sub line (space + 1) (String.length line - space - 1))
    else "" in
    let args = if rest = "" then [] else split_args rest in
    let r i = parse_reg (List.nth args i) in
    let instr = match op, args with
    | "ADD", [_;_;_] -> Contract_vm.ADD (r 0, r 1, r 2)
    | "SUB", [_;_;_] -> Contract_vm.SUB (r 0, r 1, r 2)
    | "MUL", [_;_;_] -> Contract_vm.MUL (r 0, r 1, r 2)
    | "DIV", [_;_;_] -> Contract_vm.DIV (r 0, r 1, r 2)
    | "MOD", [_;_;_] -> Contract_vm.MOD (r 0, r 1, r 2)
    | "NEG", [_;_] -> Contract_vm.NEG (r 0, r 1)
    | "ABS", [_;_] -> Contract_vm.ABS (r 0, r 1)
    | "EQ", [_;_;_] -> Contract_vm.EQ (r 0, r 1, r 2)
    | "LT", [_;_;_] -> Contract_vm.LT (r 0, r 1, r 2)
    | "GT", [_;_;_] -> Contract_vm.GT (r 0, r 1, r 2)
    | "NEQ", [_;_;_] -> Contract_vm.NEQ (r 0, r 1, r 2)
    | "LDI", [_;v] -> Contract_vm.LDI (r 0, parse_value v)
    | "MOV", [_;_] -> Contract_vm.MOV (r 0, r 1)
    | "SLOAD", [d;k] when is_quoted k -> Contract_vm.SLOAD (parse_reg d, unquote k)
    | "SSTORE", [k;s] when is_quoted k -> Contract_vm.SSTORE (unquote k, parse_reg s)
    | "SDEL", [k] when is_quoted k -> Contract_vm.SDEL (unquote k)
    | "SLOADK", [_;_] -> Contract_vm.SLOADK (r 0, r 1)
    | "SSTOREK", [_;_] -> Contract_vm.SSTOREK (r 0, r 1)
    | "SDELK", [_] -> Contract_vm.SDELK (r 0)
    | "MLOAD", [d;idx] -> Contract_vm.MLOAD (parse_reg d, int_of_string (trim idx))
    | "MSTORE", [idx;s] -> Contract_vm.MSTORE (int_of_string (trim idx), parse_reg s)
    | "JMP", [a] -> Contract_vm.JMP (int_of_string (trim a))
    | "JIF", [_;a] -> Contract_vm.JIF (r 0, int_of_string (trim a))
    | "JDEST", [a] -> Contract_vm.JDEST (int_of_string (trim a))
    | "STOP", [] -> Contract_vm.STOP
    | "REVERT", [] -> Contract_vm.REVERT
    | "CALLER", [_] -> Contract_vm.CALLER (r 0)
    | "ORIGIN", [_] -> Contract_vm.ORIGIN (r 0)
    | "SELF", [_] -> Contract_vm.SELF (r 0)
    | "EPOCH", [_] -> Contract_vm.EPOCH (r 0)
    | "EPOCH_TIME", [_] -> Contract_vm.EPOCH_TIME (r 0)
    | "VALUE", [_] -> Contract_vm.VALUE (r 0)
    | "BALANCE", [_;_] -> Contract_vm.BALANCE (r 0, r 1)
    | "TREEHASH", [_] -> Contract_vm.TREEHASH (r 0)
    | "NODEID", [_] -> Contract_vm.NODEID (r 0)
    | "TXHASH", [_] -> Contract_vm.TXHASH (r 0)
    | "XCALL", [_;_;_;_;n] -> Contract_vm.XCALL (r 0, r 1, r 2, r 3, int_of_string (trim n))
    | "SPAWN", [_;_] -> Contract_vm.SPAWN (r 0, r 1)
    | "SPAWN2", [_;_;_;n] -> Contract_vm.SPAWN2 (r 0, r 1, r 2, int_of_string (trim n))
    | "TRANSFER", [_;_;_] -> Contract_vm.TRANSFER (r 0, r 1, r 2)
    | "CHECKPOINT", [] -> Contract_vm.CHECKPOINT
    | "ROLLBACK", [] -> Contract_vm.ROLLBACK
    | "COMMIT", [] -> Contract_vm.COMMIT
    | "EMIT", name :: _regs when is_quoted name ->
      Contract_vm.EMIT (unquote name, List.map parse_reg (List.tl args))
    | "CONCAT", [_;_;_] -> Contract_vm.CONCAT (r 0, r 1, r 2)
    | "STRLEN", [_;_] -> Contract_vm.STRLEN (r 0, r 1)
    | "CALL_INT", [_;a] -> Contract_vm.CALL_INT (r 0, int_of_string (trim a))
    | "MLOADR", [_;_] -> Contract_vm.MLOADR (r 0, r 1)
    | "MSTORER", [_;_] -> Contract_vm.MSTORER (r 0, r 1)
    | "PARSE_INTS", [_;_;_] -> Contract_vm.PARSE_INTS (r 0, r 1, r 2)
    | "ISADDR", [_;_] -> Contract_vm.ISADDR (r 0, r 1)
    | "ISHEX", [_;_] -> Contract_vm.ISHEX (r 0, r 1)
    | "STATE_PATH_KEY", [_;_] -> Contract_vm.STATE_PATH_KEY (r 0, r 1)
    | "OBJECT_MEMBER_COUNT", [_;_] -> Contract_vm.OBJECT_MEMBER_COUNT (r 0, r 1)
    | "OBJECT_HAS_MEMBER", [_;_;_] -> Contract_vm.OBJECT_HAS_MEMBER (r 0, r 1, r 2)
    | "OBJECT_MEMBER_REF_AT", [_;_;_] -> Contract_vm.OBJECT_MEMBER_REF_AT (r 0, r 1, r 2)
    | "OBJECT_TRANSITION_APPLY", [_;_;_;_;_;_;_;_;_;_;_] ->
      Contract_vm.OBJECT_TRANSITION_APPLY (r 0, r 1, r 2, r 3, r 4, r 5, r 6, r 7, r 8, r 9, r 10)
    | "ASSERT", [_] -> Contract_vm.ASSERT (r 0)
    | "ASSERT_ADDR", [_] -> Contract_vm.ASSERT_ADDR (r 0)
    | "EFFORT", [_] -> Contract_vm.EFFORT (r 0)
    | "NOP", [] -> Contract_vm.NOP
    | "SUBSTR", [_;_;_;_] -> Contract_vm.SUBSTR (r 0, r 1, r 2, r 3)
    | "INDEXOF", [_;_;_] -> Contract_vm.INDEXOF (r 0, r 1, r 2)
    | "SHA256", [_;_] -> Contract_vm.SHA256 (r 0, r 1)
    | "KECCAK256", [_;_] -> Contract_vm.KECCAK256 (r 0, r 1)
    | "ED25519_OK", [_;_;_;_] -> Contract_vm.ED25519_OK (r 0, r 1, r 2, r 3)
    | "BITAND", [_;_;_] -> Contract_vm.BITAND (r 0, r 1, r 2)
    | "BITOR", [_;_;_] -> Contract_vm.BITOR (r 0, r 1, r 2)
    | "BITXOR", [_;_;_] -> Contract_vm.BITXOR (r 0, r 1, r 2)
    | "BITSHL", [_;_;_] -> Contract_vm.BITSHL (r 0, r 1, r 2)
    | "BITSHR", [_;_;_] -> Contract_vm.BITSHR (r 0, r 1, r 2)
    | "SKEYS", [_;_;_] -> Contract_vm.SKEYS (r 0, r 1, r 2)
    | "SKEYS_PAGE", [_;_;_;_;_] -> Contract_vm.SKEYS_PAGE (r 0, r 1, r 2, r 3, r 4)
    | "SLOADN", [_;_;_] -> Contract_vm.SLOADN (r 0, r 1, r 2)
    | "SSTOREN", [_;_;_] -> Contract_vm.SSTOREN (r 0, r 1, r 2)
    | "FSTORE", [_;_] -> Contract_vm.FSTORE (r 0, r 1)
    | "FLOAD", [_;_] -> Contract_vm.FLOAD (r 0, r 1)
    | "EXP_Q16", [_;_] -> Contract_vm.EXP_Q16 (r 0, r 1)
    | "SOFTMAX_Q16_INPLACE", [_;_] -> Contract_vm.SOFTMAX_Q16_INPLACE (r 0, r 1)
    | "LAYERNORM_Q16_INPLACE", [_;_;_;_] ->
      Contract_vm.LAYERNORM_Q16_INPLACE (r 0, r 1, r 2, r 3)
    | "RMSNORM_Q16_INPLACE", [_;_;_] ->
      Contract_vm.RMSNORM_Q16_INPLACE (r 0, r 1, r 2)
    | "SILU_Q16_INPLACE", [_;_] -> Contract_vm.SILU_Q16_INPLACE (r 0, r 1)
    | "ROPE_APPLY_Q16", [_;_;_;_] -> Contract_vm.ROPE_APPLY_Q16 (r 0, r 1, r 2, r 3)
    | "ATTENTION_KV_Q16", [_;_;_;_;_;_;_;_] ->
      Contract_vm.ATTENTION_KV_Q16 (r 0, r 1, r 2, r 3, r 4, r 5, r 6, r 7)
    | "VECDOT_Q16", [_;_;_;_] -> Contract_vm.VECDOT_Q16 (r 0, r 1, r 2, r 3)
    | "ELEMWISE_MUL_Q16", [_;_;_] -> Contract_vm.ELEMWISE_MUL_Q16 (r 0, r 1, r 2)
    | "RESIDUAL_ADD_Q16", [_;_;_] -> Contract_vm.RESIDUAL_ADD_Q16 (r 0, r 1, r 2)
    | "LOAD_INT8_Q16", [_;_;_;_;_] -> Contract_vm.LOAD_INT8_Q16 (r 0, r 1, r 2, r 3, r 4)
    | "APPEND_VEC_Q16", [_;_;_;_] -> Contract_vm.APPEND_VEC_Q16 (r 0, r 1, r 2, r 3)
    | "ARGMAX_Q16", [_;_;_] -> Contract_vm.ARGMAX_Q16 (r 0, r 1, r 2)
    | "FHE_LOAD_PK", [_;_] -> Contract_vm.FHE_LOAD_PK (r 0, r 1)
    | "FHE_ADD", [_;_;_;_] -> Contract_vm.FHE_ADD (r 0, r 1, r 2, r 3)
    | "FHE_SUB", [_;_;_;_] -> Contract_vm.FHE_SUB (r 0, r 1, r 2, r 3)
    | "FHE_MUL", [_;_;_;_] -> Contract_vm.FHE_MUL (r 0, r 1, r 2, r 3)
    | "FHE_SCALE", [_;_;_;_] -> Contract_vm.FHE_SCALE (r 0, r 1, r 2, r 3)
    | "FHE_DIV_CONST", [_;_;_;_] -> Contract_vm.FHE_DIV_CONST (r 0, r 1, r 2, r 3)
    | "FHE_ADD_CONST", [_;_;_;_] -> Contract_vm.FHE_ADD_CONST (r 0, r 1, r 2, r 3)
    | "FHE_SUB_CONST", [_;_;_;_] -> Contract_vm.FHE_SUB_CONST (r 0, r 1, r 2, r 3)
    | "FHE_VERIFY_ZERO", [_;_;_;_] -> Contract_vm.FHE_VERIFY_ZERO (r 0, r 1, r 2, r 3)
    | "FHE_VERIFY_RANGE", [_;_;_;_] -> Contract_vm.FHE_VERIFY_RANGE (r 0, r 1, r 2, r 3)
    | "GROTH16_VERIFY_BN254", [_;_;_;_] -> Contract_vm.GROTH16_VERIFY_BN254 (r 0, r 1, r 2, r 3)
    | "FHE_VERIFY_BOUND", [_;_;_;_;_] -> Contract_vm.FHE_VERIFY_BOUND (r 0, r 1, r 2, r 3, r 4)
    | "FHE_COMMIT", [_;_;_] -> Contract_vm.FHE_COMMIT (r 0, r 1, r 2)
    | "FHE_PEDERSEN", [_;_;_] -> Contract_vm.FHE_PEDERSEN (r 0, r 1, r 2)
    | "FHE_SER", [_;_] -> Contract_vm.FHE_SER (r 0, r 1)
    | "FHE_DESER", [_;_] -> Contract_vm.FHE_DESER (r 0, r 1)
    | "FHE_SER_PK", [_;_] -> Contract_vm.FHE_SER_PK (r 0, r 1)
    | "FHE_DESER_PK", [_;_] -> Contract_vm.FHE_DESER_PK (r 0, r 1)
    | _ -> failwith (Printf.sprintf "unknown instruction: %s" line)
    in
    Some instr

let parse text =
  let lines = String.split_on_char '\n' text in
  let instrs = List.filter_map parse_line lines in
  Array.of_list instrs

let emit_v = function
  | Contract_vm.VInt z -> Z.to_string z
  | Contract_vm.VBool true -> "true"
  | Contract_vm.VBool false -> "false"
  | Contract_vm.VString s -> Printf.sprintf "\"%s\"" s
  | Contract_vm.VBytes b -> Printf.sprintf "\"%s\"" b
  | Contract_vm.VBytes32 b -> Printf.sprintf "\"%s\"" b
  | Contract_vm.VU64 z -> Z.to_string z
  | Contract_vm.VU128 z -> Z.to_string z
  | Contract_vm.VU256 z -> Z.to_string z
  | Contract_vm.VAddr a -> Printf.sprintf "\"%s\"" a
  | Contract_vm.VCipher _ -> "\"<cipher>\""
  | Contract_vm.VPubKey _ -> "\"<pubkey>\""

let emit_reg r = Printf.sprintf "r%d" r

let emit_instr = function
  | Contract_vm.ADD (d,a,b) -> Printf.sprintf "ADD %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b)
  | Contract_vm.SUB (d,a,b) -> Printf.sprintf "SUB %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b)
  | Contract_vm.MUL (d,a,b) -> Printf.sprintf "MUL %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b)
  | Contract_vm.DIV (d,a,b) -> Printf.sprintf "DIV %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b)
  | Contract_vm.MOD (d,a,b) -> Printf.sprintf "MOD %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b)
  | Contract_vm.NEG (d,s) -> Printf.sprintf "NEG %s, %s" (emit_reg d) (emit_reg s)
  | Contract_vm.ABS (d,s) -> Printf.sprintf "ABS %s, %s" (emit_reg d) (emit_reg s)
  | Contract_vm.EQ (d,a,b) -> Printf.sprintf "EQ %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b)
  | Contract_vm.LT (d,a,b) -> Printf.sprintf "LT %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b)
  | Contract_vm.GT (d,a,b) -> Printf.sprintf "GT %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b)
  | Contract_vm.NEQ (d,a,b) -> Printf.sprintf "NEQ %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b)
  | Contract_vm.LDI (d,v) -> Printf.sprintf "LDI %s, %s" (emit_reg d) (emit_v v)
  | Contract_vm.MOV (d,s) -> Printf.sprintf "MOV %s, %s" (emit_reg d) (emit_reg s)
  | Contract_vm.SLOAD (d,k) -> Printf.sprintf "SLOAD %s, \"%s\"" (emit_reg d) k
  | Contract_vm.SSTORE (k,s) -> Printf.sprintf "SSTORE \"%s\", %s" k (emit_reg s)
  | Contract_vm.SDEL k -> Printf.sprintf "SDEL \"%s\"" k
  | Contract_vm.SLOADK (d,s) -> Printf.sprintf "SLOADK %s, %s" (emit_reg d) (emit_reg s)
  | Contract_vm.SSTOREK (d,s) -> Printf.sprintf "SSTOREK %s, %s" (emit_reg d) (emit_reg s)
  | Contract_vm.SDELK r -> Printf.sprintf "SDELK %s" (emit_reg r)
  | Contract_vm.MLOAD (d,i) -> Printf.sprintf "MLOAD %s, %d" (emit_reg d) i
  | Contract_vm.MSTORE (i,s) -> Printf.sprintf "MSTORE %d, %s" i (emit_reg s)
  | Contract_vm.JMP a -> Printf.sprintf "JMP %d" a
  | Contract_vm.JIF (r,a) -> Printf.sprintf "JIF %s, %d" (emit_reg r) a
  | Contract_vm.JDEST a -> Printf.sprintf "JDEST %d" a
  | Contract_vm.STOP -> "STOP"
  | Contract_vm.REVERT -> "REVERT"
  | Contract_vm.CALLER d -> Printf.sprintf "CALLER %s" (emit_reg d)
  | Contract_vm.ORIGIN d -> Printf.sprintf "ORIGIN %s" (emit_reg d)
  | Contract_vm.SELF d -> Printf.sprintf "SELF %s" (emit_reg d)
  | Contract_vm.EPOCH d -> Printf.sprintf "EPOCH %s" (emit_reg d)
  | Contract_vm.EPOCH_TIME d -> Printf.sprintf "EPOCH_TIME %s" (emit_reg d)
  | Contract_vm.VALUE d -> Printf.sprintf "VALUE %s" (emit_reg d)
  | Contract_vm.BALANCE (d,s) -> Printf.sprintf "BALANCE %s, %s" (emit_reg d) (emit_reg s)
  | Contract_vm.TREEHASH d -> Printf.sprintf "TREEHASH %s" (emit_reg d)
  | Contract_vm.NODEID d -> Printf.sprintf "NODEID %s" (emit_reg d)
  | Contract_vm.TXHASH d -> Printf.sprintf "TXHASH %s" (emit_reg d)
  | Contract_vm.XCALL (d,t,m,a,n) ->
    Printf.sprintf "XCALL %s, %s, %s, %s, %d" (emit_reg d) (emit_reg t) (emit_reg m) (emit_reg a) n
  | Contract_vm.SPAWN (d,s) -> Printf.sprintf "SPAWN %s, %s" (emit_reg d) (emit_reg s)
  | Contract_vm.SPAWN2 (d,s,a,n) -> Printf.sprintf "SPAWN2 %s, %s, %s, %d" (emit_reg d) (emit_reg s) (emit_reg a) n
  | Contract_vm.TRANSFER (d,a,v) ->
    Printf.sprintf "TRANSFER %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg v)
  | Contract_vm.CHECKPOINT -> "CHECKPOINT"
  | Contract_vm.ROLLBACK -> "ROLLBACK"
  | Contract_vm.COMMIT -> "COMMIT"
  | Contract_vm.EMIT (name, regs) ->
    Printf.sprintf "EMIT \"%s\", %s" name (String.concat ", " (List.map emit_reg regs))
  | Contract_vm.CONCAT (d,a,b) -> Printf.sprintf "CONCAT %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b)
  | Contract_vm.STRLEN (d,s) -> Printf.sprintf "STRLEN %s, %s" (emit_reg d) (emit_reg s)
  | Contract_vm.CALL_INT (d,addr) -> Printf.sprintf "CALL_INT %s, %d" (emit_reg d) addr
  | Contract_vm.MLOADR (d,s) -> Printf.sprintf "MLOADR %s, %s" (emit_reg d) (emit_reg s)
  | Contract_vm.MSTORER (d,s) -> Printf.sprintf "MSTORER %s, %s" (emit_reg d) (emit_reg s)
  | Contract_vm.PARSE_INTS (d,a,b) -> Printf.sprintf "PARSE_INTS %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b)
  | Contract_vm.ISADDR (d,s) -> Printf.sprintf "ISADDR %s, %s" (emit_reg d) (emit_reg s)
  | Contract_vm.ISHEX (d,s) -> Printf.sprintf "ISHEX %s, %s" (emit_reg d) (emit_reg s)
  | Contract_vm.STATE_PATH_KEY (d,s) -> Printf.sprintf "STATE_PATH_KEY %s, %s" (emit_reg d) (emit_reg s)
  | Contract_vm.OBJECT_MEMBER_COUNT (d,s) ->
    Printf.sprintf "OBJECT_MEMBER_COUNT %s, %s" (emit_reg d) (emit_reg s)
  | Contract_vm.OBJECT_HAS_MEMBER (d,a,b) ->
    Printf.sprintf "OBJECT_HAS_MEMBER %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b)
  | Contract_vm.OBJECT_MEMBER_REF_AT (d,a,b) ->
    Printf.sprintf "OBJECT_MEMBER_REF_AT %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b)
  | Contract_vm.OBJECT_TRANSITION_APPLY (d,a,b,c,e,f,g,h,i,j,k) ->
    Printf.sprintf
      "OBJECT_TRANSITION_APPLY %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s"
      (emit_reg d)
      (emit_reg a)
      (emit_reg b)
      (emit_reg c)
      (emit_reg e)
      (emit_reg f)
      (emit_reg g)
      (emit_reg h)
      (emit_reg i)
      (emit_reg j)
      (emit_reg k)
  | Contract_vm.ASSERT d -> Printf.sprintf "ASSERT %s" (emit_reg d)
  | Contract_vm.ASSERT_ADDR d -> Printf.sprintf "ASSERT_ADDR %s" (emit_reg d)
  | Contract_vm.EFFORT d -> Printf.sprintf "EFFORT %s" (emit_reg d)
  | Contract_vm.NOP -> "NOP"
  | Contract_vm.SUBSTR (d,s,start,len) ->
    Printf.sprintf "SUBSTR %s, %s, %s, %s" (emit_reg d) (emit_reg s) (emit_reg start) (emit_reg len)
  | Contract_vm.INDEXOF (d,s,search) ->
    Printf.sprintf "INDEXOF %s, %s, %s" (emit_reg d) (emit_reg s) (emit_reg search)
  | Contract_vm.SHA256 (d,s) -> Printf.sprintf "SHA256 %s, %s" (emit_reg d) (emit_reg s)
  | Contract_vm.KECCAK256 (d,s) -> Printf.sprintf "KECCAK256 %s, %s" (emit_reg d) (emit_reg s)
  | Contract_vm.ED25519_OK (d,pk,msg,sig_) ->
    Printf.sprintf "ED25519_OK %s, %s, %s, %s" (emit_reg d) (emit_reg pk) (emit_reg msg) (emit_reg sig_)
  | Contract_vm.BITAND (d,a,b) -> Printf.sprintf "BITAND %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b)
  | Contract_vm.BITOR (d,a,b) -> Printf.sprintf "BITOR %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b)
  | Contract_vm.BITXOR (d,a,b) -> Printf.sprintf "BITXOR %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b)
  | Contract_vm.BITSHL (d,a,b) -> Printf.sprintf "BITSHL %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b)
  | Contract_vm.BITSHR (d,a,b) -> Printf.sprintf "BITSHR %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b)
  | Contract_vm.SKEYS (d,prefix,base) ->
    Printf.sprintf "SKEYS %s, %s, %s" (emit_reg d) (emit_reg prefix) (emit_reg base)
  | Contract_vm.SKEYS_PAGE (d,n,prefix,after,base) ->
    Printf.sprintf "SKEYS_PAGE %s, %s, %s, %s, %s" (emit_reg d) (emit_reg n) (emit_reg prefix) (emit_reg after) (emit_reg base)
  | Contract_vm.SLOADN (a,b,c) -> Printf.sprintf "SLOADN %s, %s, %s" (emit_reg a) (emit_reg b) (emit_reg c)
  | Contract_vm.SSTOREN (a,b,c) -> Printf.sprintf "SSTOREN %s, %s, %s" (emit_reg a) (emit_reg b) (emit_reg c)
  | Contract_vm.FSTORE (d,s) -> Printf.sprintf "FSTORE %s, %s" (emit_reg d) (emit_reg s)
  | Contract_vm.FLOAD (d,s) -> Printf.sprintf "FLOAD %s, %s" (emit_reg d) (emit_reg s)
  | Contract_vm.FHE_LOAD_PK (d,s) -> Printf.sprintf "FHE_LOAD_PK %s, %s" (emit_reg d) (emit_reg s)
  | Contract_vm.FHE_ADD (d,pk,a,b) ->
    Printf.sprintf "FHE_ADD %s, %s, %s, %s" (emit_reg d) (emit_reg pk) (emit_reg a) (emit_reg b)
  | Contract_vm.FHE_SUB (d,pk,a,b) ->
    Printf.sprintf "FHE_SUB %s, %s, %s, %s" (emit_reg d) (emit_reg pk) (emit_reg a) (emit_reg b)
  | Contract_vm.FHE_MUL (d,pk,a,b) ->
    Printf.sprintf "FHE_MUL %s, %s, %s, %s" (emit_reg d) (emit_reg pk) (emit_reg a) (emit_reg b)
  | Contract_vm.FHE_SCALE (d,pk,ct,sc) ->
    Printf.sprintf "FHE_SCALE %s, %s, %s, %s" (emit_reg d) (emit_reg pk) (emit_reg ct) (emit_reg sc)
  | Contract_vm.FHE_DIV_CONST (d,pk,ct,divisor) ->
    Printf.sprintf "FHE_DIV_CONST %s, %s, %s, %s" (emit_reg d) (emit_reg pk) (emit_reg ct) (emit_reg divisor)
  | Contract_vm.FHE_ADD_CONST (d,pk,ct,c) ->
    Printf.sprintf "FHE_ADD_CONST %s, %s, %s, %s" (emit_reg d) (emit_reg pk) (emit_reg ct) (emit_reg c)
  | Contract_vm.FHE_SUB_CONST (d,pk,ct,c) ->
    Printf.sprintf "FHE_SUB_CONST %s, %s, %s, %s" (emit_reg d) (emit_reg pk) (emit_reg ct) (emit_reg c)
  | Contract_vm.FHE_VERIFY_ZERO (d,pk,ct,pf) ->
    Printf.sprintf "FHE_VERIFY_ZERO %s, %s, %s, %s" (emit_reg d) (emit_reg pk) (emit_reg ct) (emit_reg pf)
  | Contract_vm.FHE_VERIFY_RANGE (d,pk,ct,pf) ->
    Printf.sprintf "FHE_VERIFY_RANGE %s, %s, %s, %s" (emit_reg d) (emit_reg pk) (emit_reg ct) (emit_reg pf)
  | Contract_vm.GROTH16_VERIFY_BN254 (d,vk,pf,inp) ->
    Printf.sprintf "GROTH16_VERIFY_BN254 %s, %s, %s, %s" (emit_reg d) (emit_reg vk) (emit_reg pf) (emit_reg inp)
  | Contract_vm.FHE_VERIFY_BOUND (d,pk,ct,pf,cm) ->
    Printf.sprintf "FHE_VERIFY_BOUND %s, %s, %s, %s, %s" (emit_reg d) (emit_reg pk) (emit_reg ct) (emit_reg pf) (emit_reg cm)
  | Contract_vm.FHE_COMMIT (d,pk,ct) ->
    Printf.sprintf "FHE_COMMIT %s, %s, %s" (emit_reg d) (emit_reg pk) (emit_reg ct)
  | Contract_vm.FHE_PEDERSEN (d,a,bl) ->
    Printf.sprintf "FHE_PEDERSEN %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg bl)
  | Contract_vm.FHE_SER (d,ct) -> Printf.sprintf "FHE_SER %s, %s" (emit_reg d) (emit_reg ct)
  | Contract_vm.FHE_DESER (d,b) -> Printf.sprintf "FHE_DESER %s, %s" (emit_reg d) (emit_reg b)
  | Contract_vm.FHE_SER_PK (d,pk) -> Printf.sprintf "FHE_SER_PK %s, %s" (emit_reg d) (emit_reg pk)
  | Contract_vm.FHE_DESER_PK (d,b) -> Printf.sprintf "FHE_DESER_PK %s, %s" (emit_reg d) (emit_reg b)
  | Contract_vm.MATMUL (d,l,r,m,k,n) -> Printf.sprintf "MATMUL %s, %s, %s, %s, %s, %s" (emit_reg d) (emit_reg l) (emit_reg r) (emit_reg m) (emit_reg k) (emit_reg n)
  | Contract_vm.VECDOT (d,a,b,n) -> Printf.sprintf "VECDOT %s, %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b) (emit_reg n)
  | Contract_vm.EXP_LUT (d,x) -> Printf.sprintf "EXP_LUT %s, %s" (emit_reg d) (emit_reg x)
  | Contract_vm.EXP_Q16 (d,x) -> Printf.sprintf "EXP_Q16 %s, %s" (emit_reg d) (emit_reg x)
  | Contract_vm.SOFTMAX_INPLACE (a,n) -> Printf.sprintf "SOFTMAX_INPLACE %s, %s" (emit_reg a) (emit_reg n)
  | Contract_vm.SOFTMAX_Q16_INPLACE (a,n) -> Printf.sprintf "SOFTMAX_Q16_INPLACE %s, %s" (emit_reg a) (emit_reg n)
  | Contract_vm.LAYERNORM_INPLACE (a,n,g,b) -> Printf.sprintf "LAYERNORM_INPLACE %s, %s, %s, %s" (emit_reg a) (emit_reg n) (emit_reg g) (emit_reg b)
  | Contract_vm.LAYERNORM_Q16_INPLACE (a,n,g,b) -> Printf.sprintf "LAYERNORM_Q16_INPLACE %s, %s, %s, %s" (emit_reg a) (emit_reg n) (emit_reg g) (emit_reg b)
  | Contract_vm.RELU_INPLACE (a,n) -> Printf.sprintf "RELU_INPLACE %s, %s" (emit_reg a) (emit_reg n)
  | Contract_vm.RMSNORM_INPLACE (a,n,g) -> Printf.sprintf "RMSNORM_INPLACE %s, %s, %s" (emit_reg a) (emit_reg n) (emit_reg g)
  | Contract_vm.RMSNORM_Q16_INPLACE (a,n,g) -> Printf.sprintf "RMSNORM_Q16_INPLACE %s, %s, %s" (emit_reg a) (emit_reg n) (emit_reg g)
  | Contract_vm.SILU_Q16_INPLACE (a,n) -> Printf.sprintf "SILU_Q16_INPLACE %s, %s" (emit_reg a) (emit_reg n)
  | Contract_vm.SILU_INPLACE (a,n) -> Printf.sprintf "SILU_INPLACE %s, %s" (emit_reg a) (emit_reg n)
  | Contract_vm.ELEMWISE_MUL_INPLACE (d,s,n) -> Printf.sprintf "ELEMWISE_MUL_INPLACE %s, %s, %s" (emit_reg d) (emit_reg s) (emit_reg n)
  | Contract_vm.LOAD_INT8_BYTES_TO_MEM (d,s,o,n,sc) -> Printf.sprintf "LOAD_INT8_BYTES_TO_MEM %s, %s, %s, %s, %s" (emit_reg d) (emit_reg s) (emit_reg o) (emit_reg n) (emit_reg sc)
  | Contract_vm.RESIDUAL_ADD (d,s,n) -> Printf.sprintf "RESIDUAL_ADD %s, %s, %s" (emit_reg d) (emit_reg s) (emit_reg n)
  | Contract_vm.ROPE_APPLY (a,n,p,b) -> Printf.sprintf "ROPE_APPLY %s, %s, %s, %s" (emit_reg a) (emit_reg n) (emit_reg p) (emit_reg b)
  | Contract_vm.ROPE_APPLY_Q16 (a,n,p,b) -> Printf.sprintf "ROPE_APPLY_Q16 %s, %s, %s, %s" (emit_reg a) (emit_reg n) (emit_reg p) (emit_reg b)
  | Contract_vm.LOAD_INT8_B64_TO_MEM (d,s,o,n,sc) -> Printf.sprintf "LOAD_INT8_B64_TO_MEM %s, %s, %s, %s, %s" (emit_reg d) (emit_reg s) (emit_reg o) (emit_reg n) (emit_reg sc)
  | Contract_vm.MATMUL_Q16 (d,l,r,m,k,n) -> Printf.sprintf "MATMUL_Q16 %s, %s, %s, %s, %s, %s" (emit_reg d) (emit_reg l) (emit_reg r) (emit_reg m) (emit_reg k) (emit_reg n)
  | Contract_vm.SHIFT_ROUND_INPLACE (a,n,b) -> Printf.sprintf "SHIFT_ROUND_INPLACE %s, %s, %s" (emit_reg a) (emit_reg n) (emit_reg b)
  | Contract_vm.MATMUL_FP (d,l,r,m,k,n) -> Printf.sprintf "MATMUL_FP %s, %s, %s, %s, %s, %s" (emit_reg d) (emit_reg l) (emit_reg r) (emit_reg m) (emit_reg k) (emit_reg n)
  | Contract_vm.RMSNORM_FP (a,n,g) -> Printf.sprintf "RMSNORM_FP %s, %s, %s" (emit_reg a) (emit_reg n) (emit_reg g)
  | Contract_vm.SILU_FP (a,n) -> Printf.sprintf "SILU_FP %s, %s" (emit_reg a) (emit_reg n)
  | Contract_vm.ELEMWISE_MUL_FP (d,s,n) -> Printf.sprintf "ELEMWISE_MUL_FP %s, %s, %s" (emit_reg d) (emit_reg s) (emit_reg n)
  | Contract_vm.RESIDUAL_ADD_FP (d,s,n) -> Printf.sprintf "RESIDUAL_ADD_FP %s, %s, %s" (emit_reg d) (emit_reg s) (emit_reg n)
  | Contract_vm.ROPE_APPLY_FP (a,n,p,b) -> Printf.sprintf "ROPE_APPLY_FP %s, %s, %s, %s" (emit_reg a) (emit_reg n) (emit_reg p) (emit_reg b)
  | Contract_vm.LOAD_INT8_FP (d,s,o,n,sc) -> Printf.sprintf "LOAD_INT8_FP %s, %s, %s, %s, %s" (emit_reg d) (emit_reg s) (emit_reg o) (emit_reg n) (emit_reg sc)
  | Contract_vm.VECDOT_FP (d,a,b,n) -> Printf.sprintf "VECDOT_FP %s, %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b) (emit_reg n)
  | Contract_vm.ARGMAX_FP (d,a,n) -> Printf.sprintf "ARGMAX_FP %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg n)
  | Contract_vm.ATTENTION_KV_FP (q,k,v,c,t,nq,nk,hd) -> Printf.sprintf "ATTENTION_KV_FP %s, %s, %s, %s, %s, %s, %s, %s" (emit_reg q) (emit_reg k) (emit_reg v) (emit_reg c) (emit_reg t) (emit_reg nq) (emit_reg nk) (emit_reg hd)
  | Contract_vm.ATTENTION_KV_Q16 (q,k,v,c,t,nq,nk,hd) -> Printf.sprintf "ATTENTION_KV_Q16 %s, %s, %s, %s, %s, %s, %s, %s" (emit_reg q) (emit_reg k) (emit_reg v) (emit_reg c) (emit_reg t) (emit_reg nq) (emit_reg nk) (emit_reg hd)
  | Contract_vm.VECDOT_Q16 (d,a,b,n) -> Printf.sprintf "VECDOT_Q16 %s, %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg b) (emit_reg n)
  | Contract_vm.ELEMWISE_MUL_Q16 (d,s,n) -> Printf.sprintf "ELEMWISE_MUL_Q16 %s, %s, %s" (emit_reg d) (emit_reg s) (emit_reg n)
  | Contract_vm.RESIDUAL_ADD_Q16 (d,s,n) -> Printf.sprintf "RESIDUAL_ADD_Q16 %s, %s, %s" (emit_reg d) (emit_reg s) (emit_reg n)
  | Contract_vm.LOAD_INT8_Q16 (d,s,o,n,sc) -> Printf.sprintf "LOAD_INT8_Q16 %s, %s, %s, %s, %s" (emit_reg d) (emit_reg s) (emit_reg o) (emit_reg n) (emit_reg sc)
  | Contract_vm.APPEND_VEC_Q16 (d,p,s,n) -> Printf.sprintf "APPEND_VEC_Q16 %s, %s, %s, %s" (emit_reg d) (emit_reg p) (emit_reg s) (emit_reg n)
  | Contract_vm.ARGMAX_Q16 (d,a,n) -> Printf.sprintf "ARGMAX_Q16 %s, %s, %s" (emit_reg d) (emit_reg a) (emit_reg n)
  | Contract_vm.APPEND_VEC_FP (d,p,s,n) -> Printf.sprintf "APPEND_VEC_FP %s, %s, %s, %s" (emit_reg d) (emit_reg p) (emit_reg s) (emit_reg n)

let emit instrs =
  Array.to_list instrs
  |> List.map emit_instr
  |> String.concat "\n"