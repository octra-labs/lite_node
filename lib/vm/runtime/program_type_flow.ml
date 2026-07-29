(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type kind =
  | Int
  | Bool
  | String
  | Bytes
  | Bytes32
  | U64
  | U128
  | U256
  | Addr
  | Cipher
  | PubKey
  | Unknown

type error =
  | Invalid_reg of int * int
  | Uninitialized of int * int
  | Invalid_target of int * int
  | Expected of int * int * string * kind
  | Xcall_abi of int * string
  | Unsupported of int
  | Flow_limit of int

let kind_name = function
  | Int -> "int"
  | Bool -> "bool"
  | String -> "string"
  | Bytes -> "bytes"
  | Bytes32 -> "bytes32"
  | U64 -> "u64"
  | U128 -> "u128"
  | U256 -> "u256"
  | Addr -> "address"
  | Cipher -> "cipher"
  | PubKey -> "pubkey"
  | Unknown -> "unknown"

let error_message = function
  | Invalid_reg (pc, reg) -> Printf.sprintf "invalid register r%d at pc %d" reg pc
  | Uninitialized (pc, reg) -> Printf.sprintf "uninitialized register r%d at pc %d" reg pc
  | Invalid_target (pc, target) -> Printf.sprintf "invalid target %d at pc %d" target pc
  | Expected (pc, reg, expected, actual) ->
    Printf.sprintf "expected %s in r%d at pc %d, got %s" expected reg pc (kind_name actual)
  | Xcall_abi (pc, message) -> Printf.sprintf "external call ABI mismatch at pc %d: %s" pc message
  | Unsupported pc -> Printf.sprintf "instruction at pc %d is outside the typed Program profile" pc
  | Flow_limit steps -> Printf.sprintf "Program type flow exceeds %d steps" steps

let valid_reg reg = reg >= 0 && reg < 64

type slot = Unset | Set of kind

type constant =
  | Text of string
  | Zero

module IntMap = Map.Make (Int)

type state = {
  regs : slot array;
  mem : slot IntMap.t;
  consts : constant option array;
}

type entry = {
  target : int;
  mem : (int * kind) list;
  effects : string list;
}

type call = {
  owner : int;
  pc : int;
  target : int;
  kind : kind;
}

type capability =
  | View
  | Storage_read
  | Storage_write
  | Transfer
  | Deploy
  | Fhe

type xcall = {
  pc : int;
  method_name : string;
  inputs : kind list;
  output : kind;
  capabilities : capability list;
}

type facts = {
  root : (int * kind) list;
  storage : (string * kind) list;
  entries : entry list;
  calls : call list;
  xcalls : xcall list;
}

let empty_facts = {
  root = [];
  storage = [];
  entries = [];
  calls = [];
  xcalls = [];
}

let capability_name = function
  | View -> "view"
  | Storage_read -> "storage_read"
  | Storage_write -> "storage_write"
  | Transfer -> "transfer"
  | Deploy -> "deploy"
  | Fhe -> "fhe"

let capability_of_name = function
  | "view" -> Some View
  | "storage_read" -> Some Storage_read
  | "storage_write" -> Some Storage_write
  | "transfer" -> Some Transfer
  | "deploy" -> Some Deploy
  | "fhe" -> Some Fhe
  | _ -> None

let kind_of_name = function
  | "int" -> Some Int
  | "bool" -> Some Bool
  | "string" -> Some String
  | "bytes" -> Some Bytes
  | "bytes32" -> Some Bytes32
  | "u64" -> Some U64
  | "u128" -> Some U128
  | "u256" -> Some U256
  | "address" -> Some Addr
  | "cipher" -> Some Cipher
  | "pubkey" -> Some PubKey
  | "unknown" -> Some Unknown
  | _ -> None

let kind_of_value = function
  | Contract_vm.VInt _ -> Int
  | Contract_vm.VBool _ -> Bool
  | Contract_vm.VString _ -> String
  | Contract_vm.VBytes _ -> Bytes
  | Contract_vm.VBytes32 _ -> Bytes32
  | Contract_vm.VU64 _ -> U64
  | Contract_vm.VU128 _ -> U128
  | Contract_vm.VU256 _ -> U256
  | Contract_vm.VAddr _ -> Addr
  | Contract_vm.VCipher _ -> Cipher
  | Contract_vm.VPubKey _ -> PubKey

let numeric = function
  | Int | U64 | U128 | U256 -> true
  | _ -> false

let same left right =
  left <> Unknown && right <> Unknown &&
  (left = right || numeric left && numeric right)

let text = function
  | String | Bytes | Bytes32 -> true
  | _ -> false

let address = function
  | String | Addr -> true
  | _ -> false

let scalar_value kind =
  text kind || address kind || numeric kind || kind = Bool

let read pc env reg =
  if not (valid_reg reg) then Error (Invalid_reg (pc, reg))
  else
    match env.regs.(reg) with
    | Unset -> Error (Uninitialized (pc, reg))
    | Set kind -> Ok kind

let expect pc env reg predicate name =
  match read pc env reg with
  | Error error -> Error error
  | Ok kind when predicate kind -> Ok kind
  | Ok kind -> Error (Expected (pc, reg, name, kind))

let expect_num pc env reg = expect pc env reg numeric "number"
let expect_bool pc env reg = expect pc env reg (( = ) Bool) "bool"
let expect_text pc env reg = expect pc env reg text "text"
let expect_address pc env reg = expect pc env reg address "address"
let expect_cipher pc env reg = expect pc env reg (( = ) Cipher) "cipher"
let expect_pubkey pc env reg = expect pc env reg (( = ) PubKey) "pubkey"

let expect_nums pc env regs =
  let rec loop = function
    | [] -> Ok env
    | reg :: rest ->
      (match expect_num pc env reg with
       | Ok _ -> loop rest
       | Error error -> Error error)
  in
  loop regs

let read_span pc env base count =
  if base < 0 || count < 0 || count > 64 || base > 64 - count then
    Error (Invalid_reg (pc, base))
  else
    let rec loop index =
      if index = count then Ok ()
      else
        match read pc env (base + index) with
        | Error error -> Error error
        | Ok _ -> loop (index + 1)
    in
    loop 0

let expect_same pc env left right =
  match read pc env left, read pc env right with
  | Error error, _
  | _, Error error -> Error error
  | Ok left_kind, Ok right_kind when same left_kind right_kind -> Ok ()
  | Ok left_kind, Ok right_kind -> Error (Expected (pc, right, kind_name left_kind, right_kind))

let write pc env reg kind =
  if not (valid_reg reg) then Error (Invalid_reg (pc, reg))
  else begin
    env.regs.(reg) <- Set kind;
    env.consts.(reg) <- None;
    Ok env
  end

let write_const pc env reg kind value =
  match write pc env reg kind with
  | Error error -> Error error
  | Ok env ->
    env.consts.(reg) <- Some value;
    Ok env

let read_const pc env reg =
  match read pc env reg with
  | Error error -> Error error
  | Ok _ -> Ok env.consts.(reg)

let mem_facts facts =
  List.fold_left (fun mem (slot, kind) -> IntMap.add slot (Set kind) mem)
    IntMap.empty facts

let seed facts target (env : state) =
  match List.find_opt (fun (entry : entry) -> entry.target = target) facts.entries with
  | None -> env
  | Some entry ->
    let env = { regs = Array.make 64 Unset; mem = env.mem; consts = Array.make 64 None } in
    List.fold_left
      (fun (env : state) (slot, kind) ->
        { env with mem = IntMap.add slot (Set kind) env.mem })
      env entry.mem

let call_kind facts pc =
  match List.find_opt (fun (call : call) -> call.pc = pc) facts.calls with
  | Some call -> call.kind
  | None -> Unknown

let xcall_fact facts pc =
  List.find_opt (fun call -> call.pc = pc) facts.xcalls

let storage_kind facts key =
  List.assoc_opt key facts.storage

let binary_num pc env dest left right =
  match expect_num pc env left, expect_num pc env right with
  | Error error, _
  | _, Error error -> Error error
  | Ok _, Ok _ -> write pc env dest Int

let binary_add pc env dest left right =
  match
    read pc env left,
    read_const pc env left,
    read pc env right,
    read_const pc env right
  with
  | Error error, _, _, _
  | _, Error error, _, _
  | _, _, Error error, _
  | _, _, _, Error error -> Error error
  | Ok left_kind, Ok left_const, Ok right_kind, Ok right_const
    when numeric left_kind && numeric right_kind
         || numeric left_kind && left_const = Some Zero && right_kind = String
         || left_kind = String && numeric right_kind && right_const = Some Zero ->
    write pc env dest Int
  | Ok left_kind, _, Ok right_kind, _ ->
    Error (Expected (pc, right, kind_name left_kind, right_kind))

let binary_cmp pc env dest left right =
  match expect_same pc env left right with
  | Error error -> Error error
  | Ok () -> write pc env dest Bool

let binary_order pc env dest left right =
  match expect_num pc env left, expect_num pc env right with
  | Error error, _
  | _, Error error -> Error error
  | Ok _, Ok _ -> write pc env dest Bool

let successors labels pc op len =
  let next = if pc + 1 < len then [pc + 1] else [] in
  let target label =
    match Hashtbl.find_opt labels label with
    | Some pc -> pc
    | None -> label
  in
  match op with
  | Contract_vm.STOP
  | Contract_vm.REVERT -> []
  | Contract_vm.JMP label -> [target label]
  | Contract_vm.JIF (_, label) -> target label :: next
  | Contract_vm.CALL_INT (_, label) -> target label :: next
  | _ -> next

let step facts pc env = function
  | Contract_vm.LDI (dest, Contract_vm.VString value) ->
    write_const pc env dest String (Text value)
  | Contract_vm.LDI (dest, Contract_vm.VInt value) when Z.equal value Z.zero ->
    write_const pc env dest Int Zero
  | Contract_vm.LDI (dest, value) -> write pc env dest (kind_of_value value)
  | Contract_vm.MOV (dest, source) ->
    (match read pc env source, read_const pc env source with
     | Error error, _
     | _, Error error -> Error error
     | Ok kind, Ok (Some value) -> write_const pc env dest kind value
     | Ok kind, Ok None -> write pc env dest kind)
  | Contract_vm.ADD (dest, left, right) ->
    binary_add pc env dest left right
  | Contract_vm.SUB (dest, left, right)
  | Contract_vm.MUL (dest, left, right)
  | Contract_vm.DIV (dest, left, right)
  | Contract_vm.MOD (dest, left, right) -> binary_num pc env dest left right
  | Contract_vm.NEG (dest, source)
  | Contract_vm.ABS (dest, source) ->
    (match expect_num pc env source with
     | Error error -> Error error
     | Ok _ -> write pc env dest Int)
  | Contract_vm.EQ (dest, left, right)
  | Contract_vm.NEQ (dest, left, right) -> binary_cmp pc env dest left right
  | Contract_vm.LT (dest, left, right)
  | Contract_vm.GT (dest, left, right) -> binary_order pc env dest left right
  | Contract_vm.CONCAT (dest, left, right) ->
    (match expect pc env left scalar_value "scalar value" with
     | Error error -> Error error
     | Ok _ ->
       (match expect pc env right scalar_value "scalar value" with
        | Error error -> Error error
        | Ok _ -> write pc env dest String))
  | Contract_vm.STRLEN (dest, source) ->
    (match expect_text pc env source with
     | Error error -> Error error
     | Ok _ -> write pc env dest Int)
  | Contract_vm.EXP_Q16 (dest, source) ->
    (match expect_num pc env source with
     | Error error -> Error error
     | Ok _ -> write pc env dest Int)
  | Contract_vm.SOFTMAX_Q16_INPLACE (addr, count) ->
    (match expect_num pc env addr, expect_num pc env count with
     | Ok _, Ok _ -> Ok env
     | Error error, _
     | _, Error error -> Error error)
  | Contract_vm.LAYERNORM_Q16_INPLACE (addr, count, gamma, beta) ->
    (match expect_num pc env addr, expect_num pc env count,
           expect_num pc env gamma, expect_num pc env beta with
     | Ok _, Ok _, Ok _, Ok _ -> Ok env
     | Error error, _, _, _
     | _, Error error, _, _
     | _, _, Error error, _
     | _, _, _, Error error -> Error error)
  | Contract_vm.RMSNORM_Q16_INPLACE (addr, count, gamma) ->
    (match expect_num pc env addr, expect_num pc env count,
           expect_num pc env gamma with
     | Ok _, Ok _, Ok _ -> Ok env
     | Error error, _, _
     | _, Error error, _
     | _, _, Error error -> Error error)
  | Contract_vm.SILU_Q16_INPLACE (addr, count) ->
    (match expect_num pc env addr, expect_num pc env count with
     | Ok _, Ok _ -> Ok env
     | Error error, _
     | _, Error error -> Error error)
  | Contract_vm.ROPE_APPLY_Q16 (addr, count, position, base) ->
    (match expect_num pc env addr, expect_num pc env count,
           expect_num pc env position, expect_num pc env base with
     | Ok _, Ok _, Ok _, Ok _ -> Ok env
     | Error error, _, _, _
     | _, Error error, _, _
     | _, _, Error error, _
     | _, _, _, Error error -> Error error)
  | Contract_vm.ATTENTION_KV_Q16 (query, key, value, context, total,
                                  query_heads, key_heads, head_dim) ->
    expect_nums pc env
      [query; key; value; context; total; query_heads; key_heads; head_dim]
  | Contract_vm.LOAD_INT8_Q16 (dest, source, offset, count, scale) ->
    (match expect pc env source (function String | Bytes | Bytes32 -> true | _ -> false) "bytes" with
     | Error error -> Error error
     | Ok _ -> expect_nums pc env [dest; offset; count; scale])
  | Contract_vm.APPEND_VEC_Q16 (dest, position, source, count) ->
    expect_nums pc env [dest; position; source; count]
  | Contract_vm.ARGMAX_Q16 (dest, addr, count) ->
    (match expect_nums pc env [addr; count] with
     | Error error -> Error error
     | Ok _ -> write pc env dest Int)
  | Contract_vm.VECDOT_Q16 (dest, left, right, count) ->
    (match expect_nums pc env [left; right; count] with
     | Error error -> Error error
     | Ok _ -> write pc env dest Int)
  | Contract_vm.ELEMWISE_MUL_Q16 (dest, source, count)
  | Contract_vm.RESIDUAL_ADD_Q16 (dest, source, count) ->
    expect_nums pc env [dest; source; count]
  | Contract_vm.CALLER dest
  | Contract_vm.ORIGIN dest
  | Contract_vm.SELF dest -> write pc env dest Addr
  | Contract_vm.EPOCH dest
  | Contract_vm.EPOCH_TIME dest
  | Contract_vm.VALUE dest -> write pc env dest Int
  | Contract_vm.BALANCE (dest, source) ->
    (match expect pc env source (function String | Addr -> true | _ -> false) "address" with
     | Error error -> Error error
     | Ok _ -> write pc env dest Int)
  | Contract_vm.SLOAD (dest, key) ->
    write pc env dest
      (match storage_kind facts key with
       | Some kind -> kind
       | None -> String)
  | Contract_vm.TREEHASH dest
  | Contract_vm.NODEID dest
  | Contract_vm.TXHASH dest -> write pc env dest String
  | Contract_vm.SLOADK (dest, key) ->
    (match expect_address pc env key with
     | Error error -> Error error
     | Ok _ -> write pc env dest String)
  | Contract_vm.SSTORE (key, source) ->
    (match storage_kind facts key with
     | Some expected ->
       (match read pc env source with
        | Ok actual when same expected actual -> Ok env
        | Ok actual -> Error (Expected (pc, source, kind_name expected, actual))
        | Error error -> Error error)
     | None ->
       (match expect pc env source scalar_value "scalar value" with
        | Error error -> Error error
        | Ok _ -> Ok env))
  | Contract_vm.SSTOREK (key, source) ->
    (match expect_address pc env key with
     | Error error -> Error error
     | Ok _ ->
       (match expect pc env source scalar_value "scalar value" with
        | Error error -> Error error
        | Ok _ -> Ok env))
  | Contract_vm.SDELK key ->
    (match expect_address pc env key with
     | Error error -> Error error
     | Ok _ -> Ok env)
  | Contract_vm.MLOAD (dest, index) ->
    let kind =
      match IntMap.find_opt index env.mem with
      | Some (Set kind) -> kind
      | Some Unset -> Unknown
      | None -> if index = 999 || index = 1000 then String else Unknown
    in
    write pc env dest kind
  | Contract_vm.MSTORE (index, source) ->
    (match read pc env source with
     | Error error -> Error error
     | Ok kind -> Ok { env with mem = IntMap.add index (Set kind) env.mem })
  | Contract_vm.MLOADR (dest, index) ->
    (match expect_num pc env index with
     | Error error -> Error error
     | Ok _ -> write pc env dest Unknown)
  | Contract_vm.MSTORER (index, source) ->
    (match expect_num pc env index with
     | Error error -> Error error
     | Ok _ ->
       (match read pc env source with
        | Error error -> Error error
        | Ok _ -> Ok { env with mem = IntMap.empty }))
  | Contract_vm.XCALL (dest, target, method_name, base, count) ->
    (match expect_address pc env target with
    | Error error -> Error error
    | Ok _ ->
      match expect pc env method_name (( = ) String) "method" with
      | Error error -> Error error
      | Ok _ ->
        match read_span pc env base count with
        | Error error -> Error error
        | Ok () ->
          match xcall_fact facts pc with
          | None -> write pc env dest Unknown
          | Some abi ->
            match read_const pc env method_name with
            | Error error -> Error error
            | Ok (Some (Text method_name))
              when String.equal method_name abi.method_name ->
              if List.length abi.inputs <> count then
                Error (Xcall_abi (pc, "argument count mismatch"))
              else
                let rec check_args index =
                  if index = count then Ok ()
                  else
                    match read pc env (base + index) with
                    | Error error -> Error error
                    | Ok actual ->
                      let expected = List.nth abi.inputs index in
                      if same actual expected then check_args (index + 1)
                      else Error (Xcall_abi (pc, "argument type mismatch"))
                in
                (match check_args 0 with
                 | Error error -> Error error
                 | Ok () -> write pc env dest abi.output)
            | Ok (Some _) -> Error (Xcall_abi (pc, "method mismatch"))
            | Ok None -> Error (Xcall_abi (pc, "method is not constant")))
  | Contract_vm.CALL_INT (dest, target) ->
    if target < 0 then Error (Invalid_target (pc, target))
    else
      (match write pc env dest (call_kind facts pc) with
       | Error error -> Error error
       | Ok env -> Ok { env with mem = IntMap.empty })
  | Contract_vm.SPAWN (dest, source) ->
    (match expect_text pc env source with
     | Error error -> Error error
     | Ok _ -> write pc env dest Addr)
  | Contract_vm.SPAWN2 (dest, source, base, count) ->
    (match expect_text pc env source with
     | Error error -> Error error
     | Ok _ ->
       (match read_span pc env base count with
        | Error error -> Error error
        | Ok () -> write pc env dest Addr))
  | Contract_vm.TRANSFER (dest, target, value) ->
    (match expect_address pc env target with
     | Error error -> Error error
     | Ok _ ->
       (match expect_num pc env value with
        | Error error -> Error error
        | Ok _ -> write pc env dest Bool))
  | Contract_vm.EMIT (_, regs) ->
    let rec read_all = function
      | [] -> Ok env
      | reg :: rest ->
        (match read pc env reg with
         | Error error -> Error error
         | Ok _ -> read_all rest)
    in
    read_all regs
  | Contract_vm.EFFORT dest -> write pc env dest Int
  | Contract_vm.CHECKPOINT
  | Contract_vm.ROLLBACK
  | Contract_vm.COMMIT -> Ok env
  | Contract_vm.JIF (source, _) ->
    (match expect_bool pc env source with
     | Error error -> Error error
     | Ok _ -> Ok env)
  | Contract_vm.ASSERT source ->
    (match expect_bool pc env source with
     | Error error -> Error error
     | Ok _ -> Ok env)
  | Contract_vm.ISADDR (dest, source)
  | Contract_vm.ISHEX (dest, source) ->
    (match expect_address pc env source with
     | Error error -> Error error
     | Ok _ -> write pc env dest Bool)
  | Contract_vm.BITAND (dest, left, right)
  | Contract_vm.BITOR (dest, left, right)
  | Contract_vm.BITXOR (dest, left, right)
  | Contract_vm.BITSHL (dest, left, right)
  | Contract_vm.BITSHR (dest, left, right) -> binary_num pc env dest left right
  | Contract_vm.FHE_LOAD_PK (dest, source) ->
    (match expect_address pc env source with
     | Error error -> Error error
     | Ok _ -> write pc env dest PubKey)
  | Contract_vm.FHE_ADD (dest, pubkey, left, right)
  | Contract_vm.FHE_SUB (dest, pubkey, left, right)
  | Contract_vm.FHE_MUL (dest, pubkey, left, right) ->
    (match expect_pubkey pc env pubkey, expect_cipher pc env left, expect_cipher pc env right with
     | Ok _, Ok _, Ok _ -> write pc env dest Cipher
     | Error error, _, _
     | _, Error error, _
     | _, _, Error error -> Error error)
  | Contract_vm.FHE_SCALE (dest, pubkey, cipher, scalar)
  | Contract_vm.FHE_DIV_CONST (dest, pubkey, cipher, scalar)
  | Contract_vm.FHE_ADD_CONST (dest, pubkey, cipher, scalar)
  | Contract_vm.FHE_SUB_CONST (dest, pubkey, cipher, scalar) ->
    (match expect_pubkey pc env pubkey, expect_cipher pc env cipher, expect_num pc env scalar with
     | Ok _, Ok _, Ok _ -> write pc env dest Cipher
     | Error error, _, _
     | _, Error error, _
     | _, _, Error error -> Error error)
  | Contract_vm.FHE_VERIFY_ZERO (dest, pubkey, cipher, proof)
  | Contract_vm.FHE_VERIFY_RANGE (dest, pubkey, cipher, proof) ->
    (match expect_pubkey pc env pubkey, expect_cipher pc env cipher, expect_text pc env proof with
     | Ok _, Ok _, Ok _ -> write pc env dest Bool
     | Error error, _, _
     | _, Error error, _
     | _, _, Error error -> Error error)
  | Contract_vm.FHE_VERIFY_BOUND (dest, pubkey, cipher, proof, commit) ->
    (match expect_pubkey pc env pubkey, expect_cipher pc env cipher,
           expect_text pc env proof, expect_text pc env commit with
     | Ok _, Ok _, Ok _, Ok _ -> write pc env dest Bool
     | Error error, _, _, _
     | _, Error error, _, _
     | _, _, Error error, _
     | _, _, _, Error error -> Error error)
  | Contract_vm.GROTH16_VERIFY_BN254 (dest, key, proof, inputs) ->
    (match expect_text pc env key, expect_text pc env proof, expect_text pc env inputs with
     | Ok _, Ok _, Ok _ -> write pc env dest Bool
     | Error error, _, _
     | _, Error error, _
     | _, _, Error error -> Error error)
  | Contract_vm.FHE_COMMIT (dest, pubkey, cipher) ->
    (match expect_pubkey pc env pubkey, expect_cipher pc env cipher with
     | Ok _, Ok _ -> write pc env dest String
     | Error error, _
     | _, Error error -> Error error)
  | Contract_vm.FHE_PEDERSEN (dest, amount, blinding) ->
    (match expect_num pc env amount, expect_text pc env blinding with
     | Ok _, Ok _ -> write pc env dest String
     | Error error, _
     | _, Error error -> Error error)
  | Contract_vm.FHE_SER (dest, cipher) ->
    (match expect_cipher pc env cipher with
     | Error error -> Error error
     | Ok _ -> write pc env dest String)
  | Contract_vm.FHE_DESER (dest, bytes) ->
    (match expect_text pc env bytes with
     | Error error -> Error error
     | Ok _ -> write pc env dest Cipher)
  | Contract_vm.FHE_SER_PK (dest, pubkey) ->
    (match expect_pubkey pc env pubkey with
     | Error error -> Error error
     | Ok _ -> write pc env dest String)
  | Contract_vm.FHE_DESER_PK (dest, bytes) ->
    (match expect_text pc env bytes with
     | Error error -> Error error
     | Ok _ -> write pc env dest PubKey)
  | Contract_vm.JDEST _
  | Contract_vm.JMP _
  | Contract_vm.STOP
  | Contract_vm.REVERT
  | Contract_vm.NOP -> Ok env
  | Contract_vm.ASSERT_ADDR source ->
    (match expect_address pc env source with
     | Error error -> Error error
     | Ok _ -> Ok env)
  | Contract_vm.SUBSTR (dest, source, start, length) ->
    (match expect_text pc env source, expect_num pc env start, expect_num pc env length with
     | Ok _, Ok _, Ok _ -> write pc env dest String
     | Error error, _, _
     | _, Error error, _
     | _, _, Error error -> Error error)
  | Contract_vm.INDEXOF (dest, source, needle) ->
    (match expect_text pc env source, expect_text pc env needle with
     | Ok _, Ok _ -> write pc env dest Int
     | Error error, _
     | _, Error error -> Error error)
  | Contract_vm.SHA256 (dest, source)
  | Contract_vm.KECCAK256 (dest, source) ->
    (match expect_text pc env source with
     | Error error -> Error error
     | Ok _ -> write pc env dest String)
  | _ -> Error (Unsupported pc)

let merge_regs _pc left right =
  let result = Array.copy left in
  let rec loop reg =
    if reg = 64 then Ok result
    else
      match left.(reg), right.(reg) with
      | Unset, Unset -> loop (reg + 1)
      | Unset, Set _
      | Set _, Unset ->
        result.(reg) <- Unset;
        loop (reg + 1)
      | Set left_kind, Set right_kind when left_kind = right_kind -> loop (reg + 1)
      | Set left_kind, Set right_kind when numeric left_kind && numeric right_kind ->
        result.(reg) <- Set Int;
        loop (reg + 1)
      | Set _, Set _ ->
        result.(reg) <- Unset;
        loop (reg + 1)
  in
  loop 0

let merge_consts left right =
  Array.mapi (fun index value ->
    match value, right.(index) with
    | Some left, Some right when left = right -> Some left
    | _ -> None)
    left

let merge_mem left right =
  let join _ left right =
    match left, right with
    | None, None -> None
    | None, Some _
    | Some _, None -> Some (Set Unknown)
    | Some Unset, Some Unset -> None
    | Some (Set left_kind), Some (Set right_kind)
      when left_kind = right_kind -> Some (Set left_kind)
    | Some (Set left_kind), Some (Set right_kind)
      when numeric left_kind && numeric right_kind -> Some (Set Int)
    | Some _, Some _ -> Some (Set Unknown)
  in
  IntMap.merge join left right

let merge pc left right =
  match merge_regs pc left.regs right.regs with
  | Error error -> Error error
  | Ok regs -> Ok {
      regs;
      mem = merge_mem left.mem right.mem;
      consts = merge_consts left.consts right.consts;
    }

let valid_fact (slot, _) = slot >= 0 && slot <= 16_777_216

let persistable_kind = function
  | Int
  | Bool
  | String
  | Bytes
  | Bytes32
  | U64
  | U128
  | U256
  | Addr -> true
  | Cipher
  | PubKey
  | Unknown -> false

let valid_effect effect =
  List.mem effect [
    "memory_read";
    "memory_write";
    "storage_read";
    "storage_write";
    "call";
    "deploy";
    "transfer";
    "emit";
    "fhe";
    "journal";
  ]

let unique key values =
  let seen = Hashtbl.create (List.length values) in
  List.for_all
    (fun value ->
      let item = key value in
      if Hashtbl.mem seen item then false
      else begin
        Hashtbl.add seen item ();
        true
      end)
    values

let call_pcs code =
  let pcs = ref [] in
  Array.iteri
    (fun pc op ->
      match op with
      | Contract_vm.CALL_INT _ -> pcs := pc :: !pcs
      | _ -> ())
    code;
  List.rev !pcs

let acyclic calls =
  let edges = Hashtbl.create (List.length calls) in
  List.iter
    (fun call ->
      let current = Option.value (Hashtbl.find_opt edges call.owner) ~default:[] in
      Hashtbl.replace edges call.owner (call.target :: current))
    calls;
  let active = Hashtbl.create (Hashtbl.length edges) in
  let complete = Hashtbl.create (Hashtbl.length edges) in
  let rec visit node =
    match Hashtbl.find_opt complete node with
    | Some depth -> Some depth
    | None when Hashtbl.mem active node -> None
    | None ->
      Hashtbl.add active node ();
      let children =
        Hashtbl.find_opt edges node
        |> Option.value ~default:[]
        |> List.rev
      in
      let rec deepest depth = function
        | [] -> Some depth
        | child :: rest ->
          match visit child with
          | None -> None
          | Some child_depth -> deepest (max depth child_depth) rest
      in
      let result =
        match deepest 0 children with
        | None -> None
        | Some child_depth ->
          let depth = child_depth + 1 in
          if depth > Program_limits.max_call_depth then None
          else begin
            Hashtbl.replace complete node depth;
            Some depth
          end
      in
      Hashtbl.remove active node;
      result
  in
  Hashtbl.to_seq_keys edges
  |> List.of_seq
  |> List.sort compare
  |> List.for_all (fun node -> Option.is_some (visit node))

let valid_facts len code facts =
  let entries = facts.entries in
  let labels = Hashtbl.create (List.length entries) in
  Array.iteri
    (fun pc op ->
      match op with
      | Contract_vm.JDEST label -> Hashtbl.replace labels label pc
      | _ -> ())
    code;
  let entry_targets = Hashtbl.create (List.length entries) in
  List.iter
    (fun (entry : entry) -> Hashtbl.replace entry_targets entry.target ())
    entries;
  let valid_memory memory =
    List.for_all valid_fact memory
    && unique (fun (slot, _) -> slot) memory
  in
  let valid_entry (entry : entry) =
    entry.target >= 0
    && entry.target < len
    && unique (fun (slot, _) -> slot) entry.mem
    && valid_memory entry.mem
    && List.for_all valid_effect entry.effects
    && unique Fun.id entry.effects
    &&
    match code.(entry.target) with
    | Contract_vm.JDEST _ -> true
    | _ -> false
  in
  let valid_call (call : call) =
    call.owner >= 0
    && call.owner < len
    && call.pc >= call.owner
    && call.pc < len
    && call.target >= 0
    && call.target < len
    && Hashtbl.mem entry_targets call.owner
    && Hashtbl.mem entry_targets call.target
    &&
    match code.(call.pc) with
    | Contract_vm.CALL_INT (_, label) ->
      Hashtbl.find_opt labels label = Some call.target
    | _ -> false
  in
  let valid_xcall (call : xcall) =
    call.pc >= 0
    && call.pc < len
    && call.method_name <> ""
    && List.for_all (fun kind -> kind <> Unknown) call.inputs
    && call.output <> Unknown
    && unique (fun value -> capability_name value) call.capabilities
    &&
    match code.(call.pc) with
    | Contract_vm.XCALL _ -> true
    | _ -> false
  in
  let expected = call_pcs code in
  let actual = List.map (fun (call : call) -> call.pc) facts.calls in
  len <= Program_limits.max_instructions
  && List.length entries <= Program_limits.max_functions
  && List.length facts.calls <= Program_limits.max_internal_calls
  && List.length facts.xcalls <= Program_limits.max_external_calls
  && List.length facts.root <= Program_limits.max_facts
  && List.length facts.storage <= Program_limits.max_facts
  && List.for_all
       (fun (entry : entry) ->
         List.length entry.mem <= Program_limits.max_facts)
       entries
  && List.for_all valid_fact facts.root
  && unique (fun (slot, _) -> slot) facts.root
  && List.for_all
       (fun (key, kind) -> key <> "" && persistable_kind kind)
       facts.storage
  && unique fst facts.storage
  && unique (fun (entry : entry) -> entry.target) entries
  && unique (fun (call : call) -> call.pc) facts.calls
  && List.for_all valid_entry entries
  && List.for_all valid_call facts.calls
  && List.sort compare expected = List.sort compare actual
  && unique (fun (call : xcall) -> call.pc) facts.xcalls
  && List.for_all valid_xcall facts.xcalls
  && acyclic facts.calls

let canonical_facts facts =
  let pair_json key (slot, kind) =
    `Assoc [key, `Int slot; "kind", `String (kind_name kind)]
  in
  let root =
    facts.root
    |> List.sort (fun (left, _) (right, _) -> compare left right)
    |> List.map (pair_json "slot")
  in
  let storage =
    facts.storage
    |> List.sort (fun (left, _) (right, _) -> String.compare left right)
    |> List.map (fun (key, kind) ->
      `Assoc ["key", `String key; "kind", `String (kind_name kind)])
  in
  let entries =
    facts.entries
    |> List.sort (fun (left : entry) (right : entry) -> compare left.target right.target)
    |> List.map (fun (entry : entry) ->
      let mem =
        entry.mem
        |> List.sort (fun (left, _) (right, _) -> compare left right)
        |> List.map (pair_json "slot")
      in
      `Assoc [
        "target", `Int entry.target;
        "memory", `List mem;
        "effects", `List (List.map (fun value -> `String value) entry.effects)
      ])
  in
  let calls =
    facts.calls
    |> List.sort (fun (left : call) (right : call) -> compare left.pc right.pc)
    |> List.map (fun call ->
      `Assoc [
        "owner", `Int call.owner;
        "pc", `Int call.pc;
        "target", `Int call.target;
        "kind", `String (kind_name call.kind)
      ])
  in
  let xcalls =
    facts.xcalls
    |> List.sort (fun (left : xcall) (right : xcall) -> compare left.pc right.pc)
    |> List.map (fun call ->
      let kind_json kind = `String (kind_name kind) in
      `Assoc [
        "pc", `Int call.pc;
        "method", `String call.method_name;
        "inputs", `List (List.map kind_json call.inputs);
        "output", kind_json call.output;
        "capabilities", `List (List.map (fun value -> `String (capability_name value)) call.capabilities)
      ])
  in
  let fields = [
    "root", `List root;
    "entries", `List entries;
    "calls", `List calls;
    "xcalls", `List xcalls;
  ] in
  let fields =
    if storage = [] then fields
    else ("storage", `List storage) :: fields
  in
  Yojson.Safe.to_string (`Assoc fields)

let facts_hash facts =
  Digestif.SHA256.(digest_string (canonical_facts facts) |> to_hex)

let runtime_entry code =
  let rec find pc =
    if pc = Array.length code then 0
    else
      match code.(pc) with
      | Contract_vm.JDEST 100 -> pc
      | _ -> find (pc + 1)
  in
  find 0

let check
    ?(max_steps = Program_limits.max_flow_steps)
    ?(facts = empty_facts)
    code =
  let len = Array.length code in
  if len = 0 then Error (Unsupported 0)
  else if max_steps <= 0 then Error (Flow_limit max_steps)
  else if not (valid_facts len code facts) then Error (Unsupported 0)
  else
    let labels = Hashtbl.create 32 in
    Array.iteri (fun pc op ->
      match op with
      | Contract_vm.JDEST label -> Hashtbl.replace labels label pc
      | _ -> ()) code;
    let states = Array.make len None in
    let entry = runtime_entry code in
    let steps = ref 0 in
    states.(entry) <- Some {
      regs = Array.make 64 Unset;
      mem = mem_facts facts.root;
      consts = Array.make 64 None;
    };
    let rec visit pending =
      match pending with
      | [] -> Ok ()
      | pc :: rest ->
        if !steps >= max_steps then Error (Flow_limit max_steps)
        else begin
          incr steps;
          match states.(pc) with
          | None -> visit rest
          | Some env ->
            match step facts pc {
              env with
              regs = Array.copy env.regs;
              consts = Array.copy env.consts;
            } code.(pc) with
            | Error error -> Error error
            | Ok next_env ->
              let rec add_targets queue = function
                | [] -> visit queue
                | target :: targets ->
                  if target < 0 || target >= len then
                    Error (Invalid_target (pc, target))
                  else
                    let target_env = seed facts target next_env in
                    match states.(target) with
                    | None ->
                      states.(target) <- Some {
                        target_env with
                        regs = Array.copy target_env.regs;
                        consts = Array.copy target_env.consts;
                      };
                      add_targets (target :: queue) targets
                    | Some old ->
                      match merge target old target_env with
                      | Error error -> Error error
                      | Ok joined when joined = old ->
                        add_targets queue targets
                      | Ok joined ->
                        states.(target) <- Some joined;
                        add_targets (target :: queue) targets
              in
              add_targets rest (successors labels pc code.(pc) len)
        end
    in
    visit [entry]