(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


type v =
  | VInt of Z.t
  | VBool of bool
  | VString of string
  | VBytes of string
  | VBytes32 of string
  | VU64 of int64
  | VU128 of Z.t
  | VU256 of Z.t
  | VAddr of string
  | VCipher of Pvac_ffi.cipher
  | VPubKey of Pvac_ffi.pubkey

type reg = int

type instr =
  | ADD of reg * reg * reg
  | SUB of reg * reg * reg
  | MUL of reg * reg * reg
  | DIV of reg * reg * reg
  | MOD of reg * reg * reg
  | NEG of reg * reg
  | ABS of reg * reg
  | EQ of reg * reg * reg
  | LT of reg * reg * reg
  | GT of reg * reg * reg
  | NEQ of reg * reg * reg
  | LDI of reg * v
  | MOV of reg * reg
  | SLOAD of reg * string
  | SSTORE of string * reg
  | SDEL of string
  | SLOADK of reg * reg
  | SSTOREK of reg * reg
  | SDELK of reg
  | MLOAD of reg * int
  | MSTORE of int * reg
  | JMP of int
  | JIF of reg * int
  | JDEST of int
  | STOP
  | REVERT
  | CALLER of reg
  | ORIGIN of reg
  | SELF of reg
  | EPOCH of reg
  | VALUE of reg
  | BALANCE of reg * reg
  | TREEHASH of reg
  | NODEID of reg
  | TXHASH of reg
  | XCALL of reg * reg * reg * reg * int
  | SPAWN of reg * reg
  | SPAWN2 of reg * reg * reg * int
  | TRANSFER of reg * reg * reg
  | CHECKPOINT
  | ROLLBACK
  | COMMIT
  | EMIT of string * reg list
  | CONCAT of reg * reg * reg
  | STRLEN of reg * reg
  | ASSERT of reg
  | EFFORT of reg
  | NOP
  | FHE_LOAD_PK of reg * reg
  | FHE_ADD of reg * reg * reg * reg
  | FHE_SUB of reg * reg * reg * reg
  | FHE_MUL of reg * reg * reg * reg
  | FHE_SCALE of reg * reg * reg * reg
  | FHE_ADD_CONST of reg * reg * reg * reg
  | FHE_SUB_CONST of reg * reg * reg * reg
  | FHE_VERIFY_ZERO of reg * reg * reg * reg
  | FHE_VERIFY_RANGE of reg * reg * reg * reg
  | FHE_VERIFY_BOUND of reg * reg * reg * reg * reg
  | GROTH16_VERIFY_BN254 of reg * reg * reg * reg
  | FHE_COMMIT of reg * reg * reg
  | FHE_PEDERSEN of reg * reg * reg
  | FHE_SER of reg * reg
  | FHE_DESER of reg * reg
  | FHE_SER_PK of reg * reg
  | FHE_DESER_PK of reg * reg
  | CALL_INT of reg * int
  | MLOADR of reg * reg
  | MSTORER of reg * reg
  | PARSE_INTS of reg * reg * reg
  | ISADDR of reg * reg
  | ISHEX of reg * reg
  | STATE_PATH_KEY of reg * reg
  | OBJECT_MEMBER_COUNT of reg * reg
  | OBJECT_HAS_MEMBER of reg * reg * reg
  | OBJECT_MEMBER_REF_AT of reg * reg * reg
  | OBJECT_TRANSITION_APPLY of reg * reg * reg * reg * reg * reg * reg * reg * reg * reg * reg
  | ASSERT_ADDR of reg
  | SUBSTR of reg * reg * reg * reg
  | INDEXOF of reg * reg * reg
  | SHA256 of reg * reg
  | KECCAK256 of reg * reg
  | ED25519_OK of reg * reg * reg * reg
  | BITAND of reg * reg * reg
  | BITOR of reg * reg * reg
  | BITXOR of reg * reg * reg
  | BITSHL of reg * reg * reg
  | BITSHR of reg * reg * reg
  | SKEYS of reg * reg * reg
  | SKEYS_PAGE of reg * reg * reg * reg * reg
  | SLOADN of reg * reg * reg
  | SSTOREN of reg * reg * reg
  | FSTORE of reg * reg
  | FLOAD of reg * reg
  | MATMUL of reg * reg * reg * reg * reg * reg
  | VECDOT of reg * reg * reg * reg
  | EXP_LUT of reg * reg
  | SOFTMAX_INPLACE of reg * reg
  | LAYERNORM_INPLACE of reg * reg * reg * reg
  | RELU_INPLACE of reg * reg
  | RMSNORM_INPLACE of reg * reg * reg
  | SILU_INPLACE of reg * reg
  | ELEMWISE_MUL_INPLACE of reg * reg * reg
  | LOAD_INT8_BYTES_TO_MEM of reg * reg * reg * reg * reg
  | RESIDUAL_ADD of reg * reg * reg
  | ROPE_APPLY of reg * reg * reg * reg
  | LOAD_INT8_B64_TO_MEM of reg * reg * reg * reg * reg
  | MATMUL_Q16 of reg * reg * reg * reg * reg * reg
  | SHIFT_ROUND_INPLACE of reg * reg * reg
  | MATMUL_FP of reg * reg * reg * reg * reg * reg
  | RMSNORM_FP of reg * reg * reg
  | SILU_FP of reg * reg
  | ELEMWISE_MUL_FP of reg * reg * reg
  | RESIDUAL_ADD_FP of reg * reg * reg
  | ROPE_APPLY_FP of reg * reg * reg * reg
  | LOAD_INT8_FP of reg * reg * reg * reg * reg
  | VECDOT_FP of reg * reg * reg * reg
  | ARGMAX_FP of reg * reg * reg
  | ATTENTION_KV_FP of reg * reg * reg * reg * reg * reg * reg * reg
  | APPEND_VEC_FP of reg * reg * reg * reg

type mem = { mutable data : (int, v) Hashtbl.t; mutable size : int }

type undo_entry =
  | UndoMarker of int
  | UndoWrite of string * string option

type event_record = {
  contract : string;
  depth : int;
  event : string;
  values : v list;
}

type subcall_result = {
  return_value : v;
  effort_used : int;
  events : event_record list;
}

type spawn_result = {
  spawned_addr : string;
  effort_used : int;
  events : event_record list;
}

type fhe_capability =
  | Fhe_load_pk_cap
  | Fhe_encrypt_cap
  | Fhe_decrypt_cap
  | Fhe_cipher_arithmetic_cap
  | Fhe_verify_zero_cap
  | Fhe_verify_range_cap
  | Fhe_verify_bound_cap
  | Fhe_commit_cap
  | Fhe_pedersen_cap
  | Fhe_cipher_serde_cap
  | Fhe_pubkey_serde_cap

type exec_ctx = {
  get_balance : string -> Z.t;
  do_transfer : string -> string -> Z.t -> bool;
  call_contract : string -> string -> string -> v list -> int -> (subcall_result, string) result;
  deploy_contract : string -> string -> int -> int -> v list -> (spawn_result, string) result;
  get_fhe_pubkey : string -> Pvac_ffi.pubkey option;
  get_fhe_keypair : string -> (Pvac_ffi.pubkey * Pvac_ffi.seckey) option;
  allow_fhe_capability : fhe_capability -> bool;
  circle_hfhe_key_id : string option;
  circle_hfhe_intent_id : string option;
  circle_hfhe_active_relay_id : string option;
  current_epoch : int;
  tree_hash : string;
  node_id : string;
  tx_hash : string;
}

let default_ctx = {
  tx_hash = String.make 64 '0';
  get_balance = (fun _ -> Z.zero);
  do_transfer = (fun _ _ _ -> false);
  call_contract = (fun _ _ _ _ _ -> Error "not implemented");
  deploy_contract = (fun _ _ _ _ _ -> Error "not implemented");
  get_fhe_pubkey = (fun _ -> None);
  get_fhe_keypair = (fun _ -> None);
  allow_fhe_capability = (fun _ -> true);
  circle_hfhe_key_id = None;
  circle_hfhe_intent_id = None;
  circle_hfhe_active_relay_id = None;
  current_epoch = 0;
  tree_hash = String.make 64 '0';
  node_id = "node_001";
}

type s = {
  regs : v array;
  mutable memory : mem;
  storage : (string, string) Hashtbl.t;
  mutable effort_used : int;
  effort_limit : int;
  mutable reverted : bool;
  mutable pc : int;
  caller : string;
  origin : string;
  address : string;
  value : Z.t;
  logs : event_record list ref;
  ctx : exec_ctx;
  mutable undo_stack : undo_entry list;
  mutable undo_id : int;
  mutable call_depth : int;
  mutable return_stack : (int * int * v array) list;
  blobs : (string, string) Hashtbl.t;
  mutable is_view : bool;
  decoded_chunk_cache : (int, string) Hashtbl.t;
}

let max_storage_value_len = 4_194_304

let is_reserved_key k =
  String.length k > 0 && Char.code k.[0] = 0

let effort_cost = function
  | LDI _ | MOV _ | JMP _ | JDEST _ | STOP | REVERT | NOP | EFFORT _ | CALL_INT _ -> 1
  | EQ _ | LT _ | GT _ | NEQ _ -> 2
  | CALLER _ | ORIGIN _ | SELF _ | EPOCH _ | VALUE _ -> 2
  | ADD _ | SUB _ | MUL _ | MLOAD _ | MSTORE _ | MLOADR _ | MSTORER _ | CONCAT _ | STRLEN _ | ISADDR _ | ISHEX _ | STATE_PATH_KEY _ -> 3
  | DIV _ | MOD _ | NEG _ | ABS _ -> 5
  | OBJECT_HAS_MEMBER _ -> 30
  | OBJECT_MEMBER_COUNT _ | OBJECT_MEMBER_REF_AT _ -> 50
  | JIF _ | ASSERT _ | ASSERT_ADDR _ | TREEHASH _ | NODEID _ | TXHASH _ -> 5
  | SLOAD _ | SLOADK _ | BALANCE _ -> 20
  | EMIT _ -> 30
  | SDEL _ | SDELK _ | TRANSFER _ -> 50
  | SSTORE _ | SSTOREK _ | XCALL _ | CHECKPOINT -> 100
  | ROLLBACK -> 200
  | SPAWN _ | SPAWN2 _ -> 5000
  | COMMIT -> 10
  | FHE_SER_PK _ | FHE_DESER_PK _ -> 50
  | FHE_LOAD_PK _ | FHE_SER _ | FHE_DESER _ -> 100
  | FHE_COMMIT _ | FHE_PEDERSEN _ -> 200
  | FHE_ADD _ | FHE_SUB _ | FHE_ADD_CONST _ | FHE_SUB_CONST _ -> 500
  | FHE_SCALE _ -> 1000
  | FHE_MUL _ -> 10000
  | FHE_VERIFY_ZERO _ | FHE_VERIFY_BOUND _ -> 50000
  | PARSE_INTS _ -> 10
  | SUBSTR _ -> 5
  | INDEXOF _ -> 10
  | SHA256 _ | KECCAK256 _ -> 20
  | ED25519_OK _ -> 2000
  | BITAND _ | BITOR _ | BITXOR _ | BITSHL _ | BITSHR _ -> 3
  | SKEYS _ -> 50
  | SKEYS_PAGE _ -> 50
  | SLOADN _ -> 50
  | SSTOREN _ -> 100
  | FSTORE _ -> 100
  | FLOAD _ -> 100
  | FHE_VERIFY_RANGE _ -> 500000
  | GROTH16_VERIFY_BN254 _ -> 50000
  | OBJECT_TRANSITION_APPLY _ -> 300
  | MATMUL _ -> 100
  | VECDOT _ -> 10
  | EXP_LUT _ -> 5
  | SOFTMAX_INPLACE _ -> 20
  | LAYERNORM_INPLACE _ -> 30
  | RELU_INPLACE _ -> 5
  | RMSNORM_INPLACE _ -> 25
  | SILU_INPLACE _ -> 10
  | ELEMWISE_MUL_INPLACE _ -> 5
  | LOAD_INT8_BYTES_TO_MEM _ -> 10
  | RESIDUAL_ADD _ -> 5
  | ROPE_APPLY _ -> 50
  | LOAD_INT8_B64_TO_MEM _ -> 30
  | MATMUL_Q16 _ -> 100
  | SHIFT_ROUND_INPLACE _ -> 5
  | MATMUL_FP _ -> 200
  | RMSNORM_FP _ -> 50
  | SILU_FP _ -> 20
  | ELEMWISE_MUL_FP _ -> 10
  | RESIDUAL_ADD_FP _ -> 10
  | ROPE_APPLY_FP _ -> 100
  | LOAD_INT8_FP _ -> 30
  | VECDOT_FP _ -> 20
  | ARGMAX_FP _ -> 5
  | ATTENTION_KV_FP _ -> 200
  | APPEND_VEC_FP _ -> 5

let is_valid_addr s =
  let len = String.length s in
  if len <> 47 then false
  else if not (String.length s >= 3 && String.sub s 0 3 = "oct") then false
  else
    let base58 = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" in
    let rec check i =
      if i >= len then true
      else if String.contains base58 (String.get s i) then check (i + 1)
      else false
    in check 3

let create_state ?(limit=1_000_000) ?(ctx=default_ctx) ?(depth=0) ?(is_view=false) ~caller ~origin ~address ~value ~storage () =
  {
    regs = Array.make 64 (VInt Z.zero);
    memory = { data = Hashtbl.create 1024; size = 0 };
    storage;
    effort_used = 0;
    effort_limit = limit;
    reverted = false;
    pc = 0;
    caller; origin; address; value;
    logs = ref [];
    ctx;
    undo_stack = [];
    undo_id = 0;
    call_depth = depth;
    return_stack = [];
    blobs = Hashtbl.create 16;
    is_view;
    decoded_chunk_cache = Hashtbl.create 512;
  }

let to_z = function
  | VInt z -> z
  | VU64 n -> Z.of_int64 n
  | VU128 z -> z
  | VU256 z -> z
  | VBool b -> if b then Z.one else Z.zero
  | VString s -> (try Z.of_string s with _ -> Z.zero)
  | _ -> Z.zero

let to_bool = function
  | VBool b -> b
  | VInt z -> not (Z.equal z Z.zero)
  | VString s -> s <> ""
  | _ -> false

let to_string = function
  | VString s -> s
  | VInt z -> Z.to_string z
  | VBool b -> if b then "true" else "false"
  | VBytes b -> b
  | VBytes32 b -> b
  | VU64 n -> Int64.to_string n
  | VU128 z -> Z.to_string z
  | VU256 z -> Z.to_string z
  | VAddr a -> a
  | VCipher _ -> "<cipher>"
  | VPubKey _ -> "<pubkey>"

let to_cipher = function VCipher c -> Some c | _ -> None
let to_pubkey = function VPubKey pk -> Some pk | _ -> None

let max_u64 = Z.of_string "18446744073709551615"
let max_u128 = Z.sub (Z.shift_left Z.one 128) Z.one
let max_u256 = Z.sub (Z.shift_left Z.one 256) Z.one

let validate_u64 z =
  Z.sign z >= 0 && Z.compare z max_u64 <= 0

let validate_u128 z =
  Z.sign z >= 0 && Z.compare z max_u128 <= 0

let validate_u256 z =
  Z.sign z >= 0 && Z.compare z max_u256 <= 0

let validate_bytes32 s = String.length s = 32

let round_float_to_int f =
  if Float.is_nan f || Float.is_integer f then int_of_float f
  else if f >= 0.0 then int_of_float (f +. 0.5)
  else -int_of_float (-. f +. 0.5)

let fp64_to_z f =
  Z.of_int64 (Int64.bits_of_float f)

let z_to_fp64 z =
  if Z.fits_int64 z then Int64.float_of_bits (Z.to_int64 z) else 0.0

let mem_get_fp64 mem a =
  match Hashtbl.find_opt mem a with
  | Some (VInt z) -> z_to_fp64 z
  | _ -> 0.0

let mem_set_fp64 mem a f =
  Hashtbl.replace mem a (VInt (fp64_to_z f))

let make_u64 z =
  if validate_u64 z then Some (VU64 (Z.to_int64 z)) else None

let make_u128 z =
  if validate_u128 z then Some (VU128 z) else None

let make_u256 z =
  if validate_u256 z then Some (VU256 z) else None

let make_bytes32 s =
  if validate_bytes32 s then Some (VBytes32 s) else None
let to_bytes = function VBytes b -> Some b | VBytes32 b -> Some b | VString s -> Some s | _ -> None

let max_fhe_proof_bytes = 1_048_576

let max_zk_vk_bytes = 131_072
let max_zk_proof_bytes = 1_024
let max_zk_inputs_bytes = 32_768

let deser_bytes f s =
  match Base64.decode s with
  | Ok raw -> (try Some (f (Bytes.of_string raw)) with _ ->
    (try Some (f (Bytes.of_string s)) with _ -> None))
  | Error _ -> (try Some (f (Bytes.of_string s)) with _ -> None)

let decode_raw_or_b64_len expected s =
  if String.length s = expected then Some s
  else
    match Base64.decode s with
    | Ok raw when String.length raw = expected -> Some raw
    | _ -> None

let deterministic_seed parts =
  Bytes.of_string (Digestif.SHA256.(digest_string (String.concat "\000" parts) |> to_raw_string))

let revert st = st.reverted <- true; false

let revert_with_reason st reason =
  st.logs := { contract = st.address; depth = st.call_depth;
               event = "Require"; values = [VString reason] }
             :: !(st.logs);
  revert st

let view_guard st =
  if st.is_view then begin
    st.logs := { contract = st.address; depth = st.call_depth;
                 event = "Require"; values = [VString "write in view context"] }
               :: !(st.logs);
    ignore (revert st); false
  end else true

let getr st r = st.regs.(r)
let setr st r v = st.regs.(r) <- v

let add_dyn_effort st cost =
  st.effort_used <- st.effort_used + cost;
  st.effort_used <= st.effort_limit

let object_apply_dyn_cost writes =
  List.fold_left
    (fun acc -> function
      | Octra_core.Circle_object_apply.Set (_key, value) ->
        acc + 10 + (String.length value / 32)
      | Octra_core.Circle_object_apply.Del _ ->
        acc + 5)
    0
    writes

let apply_object_write st = function
  | Octra_core.Circle_object_apply.Set (key, value) ->
    let old_val = Hashtbl.find_opt st.storage key in
    st.undo_stack <- UndoWrite (key, old_val) :: st.undo_stack;
    Hashtbl.replace st.storage key value;
    true
  | Octra_core.Circle_object_apply.Del key ->
    let old_val = Hashtbl.find_opt st.storage key in
    st.undo_stack <- UndoWrite (key, old_val) :: st.undo_stack;
    Hashtbl.remove st.storage key;
    true

let rec apply_object_writes st = function
  | [] ->
    true
  | write :: rest ->
    if apply_object_write st write then
      apply_object_writes st rest
    else
      false

let exec_one st op =
  st.effort_used <- st.effort_used + effort_cost op;
  if st.effort_used > st.effort_limit then revert st
  else match op with
  | ADD (rd, rs1, rs2) ->
    setr st rd (VInt (Z.add (to_z (getr st rs1)) (to_z (getr st rs2)))); true
  | SUB (rd, rs1, rs2) ->
    setr st rd (VInt (Z.sub (to_z (getr st rs1)) (to_z (getr st rs2)))); true
  | MUL (rd, rs1, rs2) ->
    setr st rd (VInt (Z.mul (to_z (getr st rs1)) (to_z (getr st rs2)))); true
  | DIV (rd, rs1, rs2) ->
    let d = to_z (getr st rs2) in
    if Z.equal d Z.zero then revert st
    else (setr st rd (VInt (Z.div (to_z (getr st rs1)) d)); true)
  | MOD (rd, rs1, rs2) ->
    let d = to_z (getr st rs2) in
    if Z.equal d Z.zero then revert st
    else (setr st rd (VInt (Z.rem (to_z (getr st rs1)) d)); true)
  | NEG (rd, rs) ->
    setr st rd (VInt (Z.neg (to_z (getr st rs)))); true
  | ABS (rd, rs) ->
    setr st rd (VInt (Z.abs (to_z (getr st rs)))); true
  | EQ (rd, rs1, rs2) ->
    let a = getr st rs1 and b = getr st rs2 in
    (match a, b with

     | (VCipher _ | VPubKey _), VInt z when Z.equal z Z.zero ->
       setr st rd (VBool false); true
     | VInt z, (VCipher _ | VPubKey _) when Z.equal z Z.zero ->
       setr st rd (VBool false); true

     | VCipher a, VCipher b -> setr st rd (VBool (a == b)); true
     | VPubKey a, VPubKey b -> setr st rd (VBool (a == b)); true

     | VCipher _, _ | _, VCipher _ | VPubKey _, _ | _, VPubKey _ ->
       setr st rd (VBool false); true
     | _ -> setr st rd (VBool (to_string a = to_string b)); true)
  | LT (rd, rs1, rs2) ->
    let a = getr st rs1 and b = getr st rs2 in
    (match a, b with
     | VCipher _, _ | _, VCipher _ | VPubKey _, _ | _, VPubKey _ ->
       setr st rd (VBool false); true
     | _ -> setr st rd (VBool (Z.lt (to_z a) (to_z b))); true)
  | GT (rd, rs1, rs2) ->
    let a = getr st rs1 and b = getr st rs2 in
    (match a, b with
     | VCipher _, _ | _, VCipher _ | VPubKey _, _ | _, VPubKey _ ->
       setr st rd (VBool false); true
     | _ -> setr st rd (VBool (Z.gt (to_z a) (to_z b))); true)
  | NEQ (rd, rs1, rs2) ->
    let a = getr st rs1 and b = getr st rs2 in
    (match a, b with
     | (VCipher _ | VPubKey _), VInt z when Z.equal z Z.zero ->
       setr st rd (VBool true); true
     | VInt z, (VCipher _ | VPubKey _) when Z.equal z Z.zero ->
       setr st rd (VBool true); true
     | VCipher a, VCipher b -> setr st rd (VBool (a != b)); true
     | VPubKey a, VPubKey b -> setr st rd (VBool (a != b)); true
     | VCipher _, _ | _, VCipher _ | VPubKey _, _ | _, VPubKey _ ->
       setr st rd (VBool true); true
     | _ -> setr st rd (VBool (to_string a <> to_string b)); true)
  | LDI (rd, v) ->
    setr st rd v; true
  | MOV (rd, rs) ->
    setr st rd (getr st rs); true
  | SLOAD (rd, key) ->
    let v = match Hashtbl.find_opt st.storage key with
      | Some s -> VString s | None -> VString "0" in
    setr st rd v; true
  | SSTORE (key, rs) ->
    if not (view_guard st) then false
    else if is_reserved_key key then revert st
    else
    (match getr st rs with
     | VCipher _ | VPubKey _ -> revert st
     | v ->
       let s = to_string v in
       let len = String.length s in
       if len > max_storage_value_len then revert st
       else begin
         st.effort_used <- st.effort_used + len / 32;
         if st.effort_used > st.effort_limit then revert st
         else begin
           let old_val = Hashtbl.find_opt st.storage key in
           st.undo_stack <- UndoWrite (key, old_val) :: st.undo_stack;
           Hashtbl.replace st.storage key s; true
         end
       end)
  | SDEL key ->
    if not (view_guard st) then false
    else if is_reserved_key key then revert st
    else begin
      let old_val = Hashtbl.find_opt st.storage key in
      st.undo_stack <- UndoWrite (key, old_val) :: st.undo_stack;
      Hashtbl.remove st.storage key; true
    end
  | SDELK rk ->
    if not (view_guard st) then false
    else
      let key = to_string (getr st rk) in
      if is_reserved_key key then revert st
      else begin
        let old_val = Hashtbl.find_opt st.storage key in
        st.undo_stack <- UndoWrite (key, old_val) :: st.undo_stack;
        Hashtbl.remove st.storage key; true
      end
  | SLOADK (rd, rs) ->
    let key = to_string (getr st rs) in
    let v = match Hashtbl.find_opt st.storage key with
      | Some s -> VString s | None -> VString "0" in
    setr st rd v; true
  | SSTOREK (rk, rv) ->
    if not (view_guard st) then false
    else
    (match getr st rv with
     | VCipher _ | VPubKey _ -> revert st
     | v ->
       let key = to_string (getr st rk) in
       if is_reserved_key key then revert st
       else
       let s = to_string v in
       let len = String.length s in
       if len > max_storage_value_len then revert st
       else begin
         st.effort_used <- st.effort_used + len / 32;
         if st.effort_used > st.effort_limit then revert st
         else begin
           let old_val = Hashtbl.find_opt st.storage key in
           st.undo_stack <- UndoWrite (key, old_val) :: st.undo_stack;
           Hashtbl.replace st.storage key s; true
         end
       end)
  | MLOAD (rd, idx) ->
    let v = match Hashtbl.find_opt st.memory.data idx with
      | Some x -> x | None -> VInt Z.zero in
    setr st rd v; true
  | MSTORE (idx, rs) ->
    Hashtbl.replace st.memory.data idx (getr st rs);
    if idx >= st.memory.size then st.memory.size <- idx + 1;
    true
  | MLOADR (rd, rs_idx) ->
    let idx = Z.to_int (to_z (getr st rs_idx)) in
    if idx < 0 || idx > 16_777_216 then revert st
    else begin
      let v = match Hashtbl.find_opt st.memory.data idx with
        | Some x -> x | None -> VInt Z.zero in
      setr st rd v; true
    end
  | MSTORER (rs_idx, rs_val) ->
    let idx = Z.to_int (to_z (getr st rs_idx)) in
    if idx < 0 || idx > 16_777_216 then revert st
    else begin
      Hashtbl.replace st.memory.data idx (getr st rs_val);
      if idx >= st.memory.size then st.memory.size <- idx + 1;
      true
    end
  | PARSE_INTS (rd_count, rs_string, rs_base) ->
    let s = to_string (getr st rs_string) in
    let base = Z.to_int (to_z (getr st rs_base)) in
    if base < 0 || base > 16_777_216 then revert st
    else begin
      let parts = String.split_on_char ',' s in
      let count = ref 0 in
      let ok = ref true in
      List.iter (fun part ->
        if !ok then begin
          let trimmed = String.trim part in
          if trimmed <> "" then begin
            let idx = base + !count in
            if idx > 16_777_216 then ok := false
            else begin
              (try
                Hashtbl.replace st.memory.data idx (VInt (Z.of_string trimmed))
              with _ ->
                Hashtbl.replace st.memory.data idx (VInt Z.zero));
              if idx >= st.memory.size then st.memory.size <- idx + 1;
              if not (add_dyn_effort st 2) then ok := false
              else count := !count + 1
            end
          end
        end
      ) parts;
      if not !ok then revert st
      else begin
        setr st rd_count (VInt (Z.of_int !count)); true
      end
    end
  | ISADDR (rd, rs) ->
    setr st rd (VBool (is_valid_addr (to_string (getr st rs)))); true
  | ISHEX (rd, rs) ->
    let s = to_string (getr st rs) in
    let is_hex = String.length s > 0 && try
      String.iter (fun c -> match c with
        | '0'..'9' | 'a'..'f' | 'A'..'F' -> ()
        | _ -> raise Exit
      ) s; true
    with Exit -> false in
    setr st rd (VBool is_hex); true
  | STATE_PATH_KEY (rd, rs) ->
    begin
      match Octra_core.Circles.path_key_of_state_ref (to_string (getr st rs)) with
      | Ok (_, _, path_key) ->
        setr st rd (VString path_key);
        true
      | Error _ ->
        revert st
    end
  | OBJECT_MEMBER_COUNT (rd, robject_ref) ->
    let object_ref = to_string (getr st robject_ref) in
    let count =
      Octra_core.Circle_object_member_query.member_count_in_storage_tbl
        st.storage
        object_ref in
    setr st rd (VInt (Z.of_int count));
    true
  | OBJECT_HAS_MEMBER (rd, robject_ref, rmember_ref) ->
    let object_ref = to_string (getr st robject_ref) in
    let member_ref = to_string (getr st rmember_ref) in
    let present =
      Octra_core.Circle_object_member_query.has_member_in_storage_tbl
        st.storage
        object_ref
        member_ref in
    setr st rd (VBool present);
    true
  | OBJECT_MEMBER_REF_AT (rd, robject_ref, rindex) ->
    let object_ref = to_string (getr st robject_ref) in
    let index = Z.to_int (to_z (getr st rindex)) in
    begin
      match
        Octra_core.Circle_object_member_query.member_ref_at_in_storage_tbl
          st.storage
          object_ref
          index
      with
      | Some member_ref ->
        setr st rd (VString member_ref);
        true
      | None ->
        setr st rd (VString "");
        true
    end
  | OBJECT_TRANSITION_APPLY
      ( rd,
        rtransition_ref,
        robject_ref,
        rprevious_state_ref,
        rnext_state_ref,
        rmember_bundle,
        rtouched_members_hash,
        rproof_kind,
        rproof_receipt_hash,
        rstatus,
        rintent_id ) ->
    if not (view_guard st) then false
    else
      begin
        match
          Octra_core.Circle_object_apply.apply
            ~current_epoch:st.ctx.current_epoch
            ~storage_tbl:st.storage
            ~transition_ref:(to_string (getr st rtransition_ref))
            ~object_ref:(to_string (getr st robject_ref))
            ~previous_state_ref:(to_string (getr st rprevious_state_ref))
            ~next_state_ref:(to_string (getr st rnext_state_ref))
            ~member_bundle:(to_string (getr st rmember_bundle))
            ~touched_members_hash:(to_string (getr st rtouched_members_hash))
            ~proof_kind_raw:(to_string (getr st rproof_kind))
            ~proof_receipt_hash_raw:(to_string (getr st rproof_receipt_hash))
            ~status:(to_string (getr st rstatus))
            ~intent_id:(to_string (getr st rintent_id))
        with
        | Error _ ->
          revert st
        | Ok result ->
          if not (add_dyn_effort st (object_apply_dyn_cost result.writes)) then
            revert st
          else if
            List.exists
              (function
                | Octra_core.Circle_object_apply.Set (_key, value) ->
                  String.length value > max_storage_value_len
                | Octra_core.Circle_object_apply.Del _ ->
                  false)
              result.writes
          then
            revert st
          else if apply_object_writes st result.writes then begin
            setr st rd (VInt (Z.of_int64 result.version));
            true
          end else
            revert st
      end
  | ASSERT_ADDR rs ->
    let s = to_string (getr st rs) in
    if is_valid_addr s then true
    else begin
      st.logs := { contract = st.address; depth = st.call_depth;
                   event = "Require"; values = [VString "invalid address"] }
                 :: !(st.logs);
      revert st
    end
  | SUBSTR (rd, rs, rstart, rlen) ->
    let s = to_string (getr st rs) in
    let slen = String.length s in
    if not (add_dyn_effort st (slen / 256)) then revert st
    else
    let start = Z.to_int (to_z (getr st rstart)) in
    let len = Z.to_int (to_z (getr st rlen)) in
    if start < 0 || start > slen || len < 0 then setr st rd (VString "")
    else begin
      let actual_len = min len (slen - start) in
      setr st rd (VString (String.sub s start actual_len))
    end; true
  | INDEXOF (rd, rs, rsearch) ->
    let s = to_string (getr st rs) in
    let search = to_string (getr st rsearch) in
    let slen = String.length s in
    let plen = String.length search in

    if not (add_dyn_effort st ((slen * (max plen 1)) / 1024)) then revert st
    else if plen = 0 then (setr st rd (VInt Z.zero); true)
    else if plen > slen then (setr st rd (VInt (Z.of_int (-1))); true)
    else begin
      let found = ref (-1) in
      for i = 0 to slen - plen do
        if !found = -1 && String.sub s i plen = search then found := i
      done;
      setr st rd (VInt (Z.of_int !found)); true
    end
  | SHA256 (rd, rs) ->
    let s = to_string (getr st rs) in
    if not (add_dyn_effort st ((String.length s) / 64)) then revert st
    else begin
      let h = Digestif.SHA256.digest_string s in
      setr st rd (VString (Digestif.SHA256.to_hex h)); true
    end
  | KECCAK256 (rd, rs) ->
    let s = to_string (getr st rs) in
    if not (add_dyn_effort st ((String.length s) / 64)) then revert st
    else begin
      let h = Digestif.KECCAK_256.digest_string s in
      setr st rd (VString (Digestif.KECCAK_256.to_hex h)); true
    end
  | ED25519_OK (rd, rpk, rmsg, rsig) ->
    (match to_bytes (getr st rpk), to_bytes (getr st rmsg), to_bytes (getr st rsig) with
     | Some pk_in, Some msg, Some sig_in ->
       if not (add_dyn_effort st ((String.length msg) / 64)) then revert st
       else
         let ok =
           match decode_raw_or_b64_len 32 pk_in, decode_raw_or_b64_len 64 sig_in with
           | Some pk_raw, Some sig_raw ->
             let pk_b64 = Base64.encode_exn pk_raw in
             let sig_b64 = Base64.encode_exn sig_raw in
             Octra_core.Peer_auth.verify msg sig_b64 pk_b64
           | _ -> false
         in
         setr st rd (VBool ok); true
     | _ -> revert st)
  | BITAND (rd, ra, rb) ->
    let mask64 = Z.sub (Z.shift_left Z.one 64) Z.one in
    let a = Z.logand (to_z (getr st ra)) mask64 in
    let b = Z.logand (to_z (getr st rb)) mask64 in
    setr st rd (VInt (Z.logand a b)); true
  | BITOR (rd, ra, rb) ->
    let mask64 = Z.sub (Z.shift_left Z.one 64) Z.one in
    let a = Z.logand (to_z (getr st ra)) mask64 in
    let b = Z.logand (to_z (getr st rb)) mask64 in
    setr st rd (VInt (Z.logor a b)); true
  | BITXOR (rd, ra, rb) ->
    let mask64 = Z.sub (Z.shift_left Z.one 64) Z.one in
    let a = Z.logand (to_z (getr st ra)) mask64 in
    let b = Z.logand (to_z (getr st rb)) mask64 in
    setr st rd (VInt (Z.logxor a b)); true
  | BITSHL (rd, ra, rb) ->
    let mask64 = Z.sub (Z.shift_left Z.one 64) Z.one in
    let a = Z.logand (to_z (getr st ra)) mask64 in
    let n = Z.to_int (to_z (getr st rb)) in
    if n < 0 || n > 63 then (setr st rd (VInt Z.zero); true)
    else (setr st rd (VInt (Z.logand (Z.shift_left a n) mask64)); true)
  | BITSHR (rd, ra, rb) ->
    let mask64 = Z.sub (Z.shift_left Z.one 64) Z.one in
    let a = Z.logand (to_z (getr st ra)) mask64 in
    let n = Z.to_int (to_z (getr st rb)) in
    if n < 0 || n > 63 then (setr st rd (VInt Z.zero); true)
    else (setr st rd (VInt (Z.shift_right a n)); true)
  | SKEYS (rd_count, rs_prefix, rs_base) ->
    let prefix = to_string (getr st rs_prefix) in
    let base = Z.to_int (to_z (getr st rs_base)) in
    let plen = String.length prefix in
    let max_keys = 1000 in

    let suffixes = ref [] in
    Hashtbl.iter (fun k _v ->
      if not (is_reserved_key k)
         && String.length k >= plen && String.sub k 0 plen = prefix then
        suffixes := String.sub k plen (String.length k - plen) :: !suffixes
    ) st.storage;
    let sorted = List.sort String.compare !suffixes in
    let count = ref 0 in
    let ok = ref true in
    List.iter (fun suffix ->
      if !ok && !count < max_keys then begin
        let idx = base + !count in
        if idx >= 0 && idx < 16_777_216 then begin
          if not (add_dyn_effort st 5) then ok := false
          else begin
            Hashtbl.replace st.memory.data idx (VString suffix);
            if idx >= st.memory.size then st.memory.size <- idx + 1;
            count := !count + 1
          end
        end
      end
    ) sorted;
    if not !ok then revert st
    else (setr st rd_count (VInt (Z.of_int !count)); true)
  | SKEYS_PAGE (rd_count, rd_next, rs_prefix, rs_after, rs_base) ->
    let prefix = to_string (getr st rs_prefix) in
    let after = to_string (getr st rs_after) in
    let base = Z.to_int (to_z (getr st rs_base)) in
    let plen = String.length prefix in
    let max_keys = 1000 in

    let suffixes = ref [] in
    Hashtbl.iter (fun k _v ->
      if not (is_reserved_key k)
         && String.length k >= plen && String.sub k 0 plen = prefix then begin
        let suffix = String.sub k plen (String.length k - plen) in

        if after = "" || String.compare suffix after > 0 then
          suffixes := suffix :: !suffixes
      end
    ) st.storage;

    let sorted = List.sort String.compare !suffixes in

    let count = ref 0 in
    let last_suffix = ref "" in
    let ok = ref true in
    List.iter (fun suffix ->
      if !ok && !count < max_keys then begin
        let idx = base + !count in
        if idx >= 0 && idx < 16_777_216 then begin
          if not (add_dyn_effort st 5) then ok := false
          else begin
            Hashtbl.replace st.memory.data idx (VString suffix);
            if idx >= st.memory.size then st.memory.size <- idx + 1;
            last_suffix := suffix;
            count := !count + 1
          end
        end
      end
    ) sorted;
    if not !ok then revert st
    else begin
    setr st rd_count (VInt (Z.of_int !count));

    let has_more = List.length sorted > !count in
    setr st rd_next (VString (if has_more then !last_suffix else ""));
    true
    end
  | SLOADN (rs_base_key, rs_base_val, rs_count) ->
    let base_key = Z.to_int (to_z (getr st rs_base_key)) in
    let base_val = Z.to_int (to_z (getr st rs_base_val)) in
    let count = Z.to_int (to_z (getr st rs_count)) in
    let count = min count 1000 in
    let ok = ref true in
    let i = ref 0 in
    while !ok && !i < count do
      let key_idx = base_key + !i in
      let val_idx = base_val + !i in
      let key = match Hashtbl.find_opt st.memory.data key_idx with
        | Some v -> to_string v | None -> "" in
      let v = match Hashtbl.find_opt st.storage key with
        | Some s -> VString s | None -> VString "" in
      Hashtbl.replace st.memory.data val_idx v;
      if val_idx >= st.memory.size then st.memory.size <- val_idx + 1;
      if not (add_dyn_effort st 15) then ok := false;
      incr i
    done;
    if not !ok then revert st else true
  | SSTOREN (rs_base_key, rs_base_val, rs_count) ->
    if not (view_guard st) then false
    else begin
      let base_key = Z.to_int (to_z (getr st rs_base_key)) in
      let base_val = Z.to_int (to_z (getr st rs_base_val)) in
      let count = Z.to_int (to_z (getr st rs_count)) in
      let count = min count 1000 in
      let ok = ref true in
      let i = ref 0 in
      while !ok && !i < count do
        let key_idx = base_key + !i in
        let val_idx = base_val + !i in
        let key = match Hashtbl.find_opt st.memory.data key_idx with
          | Some v -> to_string v | None -> "" in
        let value = match Hashtbl.find_opt st.memory.data val_idx with
          | Some v -> to_string v | None -> "" in
        if String.length key > 0 && String.length value <= max_storage_value_len
           && not (is_reserved_key key) then begin
          let old_val = Hashtbl.find_opt st.storage key in
          st.undo_stack <- UndoWrite (key, old_val) :: st.undo_stack;
          Hashtbl.replace st.storage key value
        end;
        if not (add_dyn_effort st 80) then ok := false;
        incr i
      done;
      if not !ok then revert st else true
    end
  | FSTORE (rd_hash, rs_data) ->
    if not (view_guard st) then false
    else begin
      let data = to_string (getr st rs_data) in
      let max_blob = 10_485_760 in
      if String.length data > max_blob then (st.reverted <- true; false)
      else begin
        let hash = Digestif.SHA256.(to_hex (digest_string data)) in
        Hashtbl.replace st.blobs hash data;
        if not (add_dyn_effort st (String.length data / 1024)) then revert st
        else (setr st rd_hash (VString hash); true)
      end
    end
  | FLOAD (rd_data, rs_hash) ->
    let hash = to_string (getr st rs_hash) in
    (match Hashtbl.find_opt st.blobs hash with
     | Some data ->
       if not (add_dyn_effort st (String.length data / 1024)) then revert st
       else (setr st rd_data (VString data); true)
     | None -> setr st rd_data (VString ""); true)
  | MATMUL (rd_addr, rs_lhs, rs_rhs, rs_m, rs_k, rs_n) ->
    let dst_addr = Z.to_int (to_z (getr st rd_addr)) in
    let lhs_addr = Z.to_int (to_z (getr st rs_lhs)) in
    let rhs_addr = Z.to_int (to_z (getr st rs_rhs)) in
    let m = Z.to_int (to_z (getr st rs_m)) in
    let k = Z.to_int (to_z (getr st rs_k)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    if m <= 0 || k <= 0 || n <= 0 || m > 32768 || k > 32768 || n > 32768 then revert st
    else begin
      let dyn_cost = (m * n * k) / 1024 in
      if not (add_dyn_effort st dyn_cost) then revert st
      else begin
        let mem_get a =
          match Hashtbl.find_opt st.memory.data a with
          | Some v -> to_z v
          | None -> Z.zero
        in
        for r = 0 to m - 1 do
          for c = 0 to n - 1 do
            let acc = ref Z.zero in
            for i = 0 to k - 1 do
              let lv = mem_get (lhs_addr + r * k + i) in
              let rv = mem_get (rhs_addr + i * n + c) in
              acc := Z.add !acc (Z.mul lv rv)
            done;
            let cell = dst_addr + r * n + c in
            Hashtbl.replace st.memory.data cell (VInt !acc);
            if cell >= st.memory.size then st.memory.size <- cell + 1
          done
        done;
        true
      end
    end
  | VECDOT (rd, rs_a, rs_b, rs_n) ->
    let a_addr = Z.to_int (to_z (getr st rs_a)) in
    let b_addr = Z.to_int (to_z (getr st rs_b)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    if n <= 0 || n > 131072 then revert st
    else begin
      if not (add_dyn_effort st (n / 16)) then revert st
      else begin
        let mem_get a =
          match Hashtbl.find_opt st.memory.data a with
          | Some v -> to_z v
          | None -> Z.zero
        in
        let acc = ref Z.zero in
        for i = 0 to n - 1 do
          let av = mem_get (a_addr + i) in
          let bv = mem_get (b_addr + i) in
          acc := Z.add !acc (Z.mul av bv)
        done;
        setr st rd (VInt !acc);
        true
      end
    end
  | EXP_LUT (rd, rs_x) ->
    let x = to_z (getr st rs_x) in
    let q_one = 65536 in
    let lut_range_min = -8 * q_one in
    let lut_range_max = 8 * q_one in
    let xi = if Z.fits_int x then Z.to_int x else 0 in
    let xi = max lut_range_min (min lut_range_max xi) in
    let f = float_of_int xi /. float_of_int q_one in
    let e = exp f in
    let result = round_float_to_int (e *. float_of_int q_one) in
    setr st rd (VInt (Z.of_int result));
    true
  | SOFTMAX_INPLACE (rs_addr, rs_n) ->
    let addr = Z.to_int (to_z (getr st rs_addr)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    if n <= 0 || n > 131072 then revert st
    else begin
      if not (add_dyn_effort st (n * 8)) then revert st
      else begin
        let q_one = 65536.0 in
        let mem_get a =
          match Hashtbl.find_opt st.memory.data a with
          | Some v ->
            let z = to_z v in
            if Z.fits_int z then Z.to_int z else 0
          | None -> 0
        in
        let arr = Array.init n (fun i -> mem_get (addr + i)) in
        let max_v = Array.fold_left max arr.(0) arr in
        let exps = Array.map (fun v ->
          let f = float_of_int (v - max_v) /. q_one in
          exp f
        ) arr in
        let sum = Array.fold_left (+.) 0.0 exps in
        if sum <= 0.0 then revert st
        else begin
          for i = 0 to n - 1 do
            let prob = exps.(i) /. sum in
            let qv = round_float_to_int (prob *. q_one) in
            Hashtbl.replace st.memory.data (addr + i) (VInt (Z.of_int qv))
          done;
          true
        end
      end
    end
  | LAYERNORM_INPLACE (rs_addr, rs_n, rs_gamma, rs_beta) ->
    let addr = Z.to_int (to_z (getr st rs_addr)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    let gamma_addr = Z.to_int (to_z (getr st rs_gamma)) in
    let beta_addr = Z.to_int (to_z (getr st rs_beta)) in
    if n <= 0 || n > 131072 then revert st
    else begin
      if not (add_dyn_effort st (n * 4)) then revert st
      else begin
        let q_one = 65536.0 in
        let mem_get_f a =
          match Hashtbl.find_opt st.memory.data a with
          | Some v ->
            let z = to_z v in
            if Z.fits_int z then float_of_int (Z.to_int z) /. q_one else 0.0
          | None -> 0.0
        in
        let arr = Array.init n (fun i -> mem_get_f (addr + i)) in
        let mean = Array.fold_left (+.) 0.0 arr /. float_of_int n in
        let var = Array.fold_left (fun acc v -> acc +. (v -. mean) ** 2.0) 0.0 arr /. float_of_int n in
        let inv_std = 1.0 /. sqrt (var +. 1e-5) in
        for i = 0 to n - 1 do
          let g = mem_get_f (gamma_addr + i) in
          let b = mem_get_f (beta_addr + i) in
          let normalized = (arr.(i) -. mean) *. inv_std in
          let result = g *. normalized +. b in
          let qv = round_float_to_int (result *. q_one) in
          Hashtbl.replace st.memory.data (addr + i) (VInt (Z.of_int qv))
        done;
        true
      end
    end
  | RELU_INPLACE (rs_addr, rs_n) ->
    let addr = Z.to_int (to_z (getr st rs_addr)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    if n <= 0 || n > 1048576 then revert st
    else begin
      if not (add_dyn_effort st (n / 4)) then revert st
      else begin
        for i = 0 to n - 1 do
          let cell = addr + i in
          match Hashtbl.find_opt st.memory.data cell with
          | Some v ->
            let z = to_z v in
            if Z.sign z < 0 then
              Hashtbl.replace st.memory.data cell (VInt Z.zero)
          | None -> ()
        done;
        true
      end
    end
  | RMSNORM_INPLACE (rs_addr, rs_n, rs_gamma) ->
    let addr = Z.to_int (to_z (getr st rs_addr)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    let gamma_addr = Z.to_int (to_z (getr st rs_gamma)) in
    if n <= 0 || n > 131072 then revert st
    else begin
      if not (add_dyn_effort st (n * 3)) then revert st
      else begin
        let q_one = 65536.0 in
        let mem_get_f a =
          match Hashtbl.find_opt st.memory.data a with
          | Some v ->
            let z = to_z v in
            if Z.fits_int z then float_of_int (Z.to_int z) /. q_one else 0.0
          | None -> 0.0
        in
        let arr = Array.init n (fun i -> mem_get_f (addr + i)) in
        let sum_sq = Array.fold_left (fun acc v -> acc +. v *. v) 0.0 arr in
        let mean_sq = sum_sq /. float_of_int n in
        let inv_rms = 1.0 /. sqrt (mean_sq +. 1e-6) in
        for i = 0 to n - 1 do
          let g = mem_get_f (gamma_addr + i) in
          let result = g *. arr.(i) *. inv_rms in
          let qv = round_float_to_int (result *. q_one) in
          Hashtbl.replace st.memory.data (addr + i) (VInt (Z.of_int qv))
        done;
        true
      end
    end
  | SILU_INPLACE (rs_addr, rs_n) ->
    let addr = Z.to_int (to_z (getr st rs_addr)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    if n <= 0 || n > 131072 then revert st
    else begin
      if not (add_dyn_effort st (n * 2)) then revert st
      else begin
        let q_one = 65536.0 in
        for i = 0 to n - 1 do
          let cell = addr + i in
          match Hashtbl.find_opt st.memory.data cell with
          | Some v ->
            let z = to_z v in
            if Z.fits_int z then begin
              let x = float_of_int (Z.to_int z) /. q_one in
              let sigmoid = 1.0 /. (1.0 +. exp (-. x)) in
              let result = x *. sigmoid in
              let qv = round_float_to_int (result *. q_one) in
              Hashtbl.replace st.memory.data cell (VInt (Z.of_int qv))
            end
          | None -> ()
        done;
        true
      end
    end
  | ELEMWISE_MUL_INPLACE (rs_dst, rs_src, rs_n) ->
    let dst = Z.to_int (to_z (getr st rs_dst)) in
    let src = Z.to_int (to_z (getr st rs_src)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    if n <= 0 || n > 1048576 then revert st
    else begin
      if not (add_dyn_effort st (n / 2)) then revert st
      else begin
        let q_shift = 16 in
        for i = 0 to n - 1 do
          let a_z = match Hashtbl.find_opt st.memory.data (dst + i) with
            | Some v -> to_z v | None -> Z.zero in
          let b_z = match Hashtbl.find_opt st.memory.data (src + i) with
            | Some v -> to_z v | None -> Z.zero in
          let prod = Z.mul a_z b_z in
          let scaled = Z.shift_right prod q_shift in
          Hashtbl.replace st.memory.data (dst + i) (VInt scaled)
        done;
        true
      end
    end
  | LOAD_INT8_BYTES_TO_MEM (rs_dst, rs_src, rs_off, rs_n, rs_scale) ->
    let dst = Z.to_int (to_z (getr st rs_dst)) in
    let src_str = to_string (getr st rs_src) in
    let off = Z.to_int (to_z (getr st rs_off)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    let scale_z = to_z (getr st rs_scale) in
    let slen = String.length src_str in
    if n <= 0 || n > 1_048_576 then revert st
    else if off < 0 || off + n > slen then revert st
    else begin
      if not (add_dyn_effort st (n / 2)) then revert st
      else begin
        for i = 0 to n - 1 do
          let b = Char.code src_str.[off + i] in
          let signed = if b >= 128 then b - 256 else b in
          let v = Z.mul (Z.of_int signed) scale_z in
          Hashtbl.replace st.memory.data (dst + i) (VInt v)
        done;
        true
      end
    end
  | RESIDUAL_ADD (rs_dst, rs_src, rs_n) ->
    let dst = Z.to_int (to_z (getr st rs_dst)) in
    let src = Z.to_int (to_z (getr st rs_src)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    if n <= 0 || n > 1_048_576 then revert st
    else begin
      if not (add_dyn_effort st (n / 4)) then revert st
      else begin
        for i = 0 to n - 1 do
          let a_z = match Hashtbl.find_opt st.memory.data (dst + i) with
            | Some v -> to_z v | None -> Z.zero in
          let b_z = match Hashtbl.find_opt st.memory.data (src + i) with
            | Some v -> to_z v | None -> Z.zero in
          Hashtbl.replace st.memory.data (dst + i) (VInt (Z.add a_z b_z))
        done;
        true
      end
    end
  | MATMUL_Q16 (rd_addr, rs_lhs, rs_rhs, rs_m, rs_k, rs_n) ->
    let dst_addr = Z.to_int (to_z (getr st rd_addr)) in
    let lhs_addr = Z.to_int (to_z (getr st rs_lhs)) in
    let rhs_addr = Z.to_int (to_z (getr st rs_rhs)) in
    let m = Z.to_int (to_z (getr st rs_m)) in
    let k = Z.to_int (to_z (getr st rs_k)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    if m <= 0 || k <= 0 || n <= 0 || m > 32768 || k > 32768 || n > 32768 then revert st
    else begin
      let dyn_cost = (m * n * k) / 1024 in
      if not (add_dyn_effort st dyn_cost) then revert st
      else begin
        let mem_get a =
          match Hashtbl.find_opt st.memory.data a with
          | Some v -> to_z v
          | None -> Z.zero
        in
        let half = Z.of_int 32768 in
        let shift = 16 in
        for r = 0 to m - 1 do
          for c = 0 to n - 1 do
            let acc = ref Z.zero in
            for i = 0 to k - 1 do
              let lv = mem_get (lhs_addr + r * k + i) in
              let rv = mem_get (rhs_addr + i * n + c) in
              acc := Z.add !acc (Z.mul lv rv)
            done;
            let rounded =
              if Z.sign !acc >= 0 then
                Z.shift_right (Z.add !acc half) shift
              else
                Z.neg (Z.shift_right (Z.add (Z.neg !acc) half) shift)
            in
            let cell = dst_addr + r * n + c in
            Hashtbl.replace st.memory.data cell (VInt rounded);
            if cell >= st.memory.size then st.memory.size <- cell + 1
          done
        done;
        true
      end
    end
  | SHIFT_ROUND_INPLACE (rs_addr, rs_n, rs_bits) ->
    let addr = Z.to_int (to_z (getr st rs_addr)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    let bits = Z.to_int (to_z (getr st rs_bits)) in
    if n <= 0 || n > 1_048_576 || bits <= 0 || bits > 62 then revert st
    else begin
      if not (add_dyn_effort st (n / 4)) then revert st
      else begin
        let half = Z.shift_left Z.one (bits - 1) in
        for i = 0 to n - 1 do
          let v = match Hashtbl.find_opt st.memory.data (addr + i) with
            | Some x -> to_z x | None -> Z.zero in
          let rounded =
            if Z.sign v >= 0 then
              Z.shift_right (Z.add v half) bits
            else
              Z.neg (Z.shift_right (Z.add (Z.neg v) half) bits)
          in
          Hashtbl.replace st.memory.data (addr + i) (VInt rounded)
        done;
        true
      end
    end
  | LOAD_INT8_B64_TO_MEM (rs_dst, rs_src, rs_off, rs_n, rs_scale) ->
    let dst = Z.to_int (to_z (getr st rs_dst)) in
    let src_b64 = to_string (getr st rs_src) in
    let off = Z.to_int (to_z (getr st rs_off)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    let scale_z = to_z (getr st rs_scale) in
    if n <= 0 || n > 1_048_576 then revert st
    else begin
      match Base64.decode src_b64 with
      | Error _ -> revert st
      | Ok decoded ->
        let dlen = String.length decoded in
        if off < 0 || off + n > dlen then revert st
        else begin
          if not (add_dyn_effort st (n + dlen / 4)) then revert st
          else begin
            for i = 0 to n - 1 do
              let b = Char.code decoded.[off + i] in
              let signed = if b >= 128 then b - 256 else b in
              let v = Z.mul (Z.of_int signed) scale_z in
              Hashtbl.replace st.memory.data (dst + i) (VInt v)
            done;
            true
          end
        end
    end
  | ROPE_APPLY (rs_addr, rs_n_dim, rs_pos, rs_base) ->
    let addr = Z.to_int (to_z (getr st rs_addr)) in
    let n_dim = Z.to_int (to_z (getr st rs_n_dim)) in
    let pos = Z.to_int (to_z (getr st rs_pos)) in
    let base_q = to_z (getr st rs_base) in
    if n_dim <= 0 || n_dim > 131072 || (n_dim land 1) <> 0 then revert st
    else if pos < 0 then revert st
    else begin
      if not (add_dyn_effort st (n_dim * 4)) then revert st
      else begin
        let q_one = 65536.0 in
        let base_f =
          if Z.fits_int base_q then float_of_int (Z.to_int base_q) /. q_one
          else 10000.0 in
        let half = n_dim / 2 in
        let mem_get_f a =
          match Hashtbl.find_opt st.memory.data a with
          | Some v ->
            let z = to_z v in
            if Z.fits_int z then float_of_int (Z.to_int z) /. q_one else 0.0
          | None -> 0.0
        in
        let pf = float_of_int pos in
        let nf = float_of_int n_dim in
        for i = 0 to half - 1 do
          let exp_term = (2.0 *. float_of_int i) /. nf in
          let inv_freq = 1.0 /. (base_f ** exp_term) in
          let angle = pf *. inv_freq in
          let c = cos angle in
          let s = sin angle in
          let x_re = mem_get_f (addr + i) in
          let x_im = mem_get_f (addr + i + half) in
          let new_re = x_re *. c -. x_im *. s in
          let new_im = x_re *. s +. x_im *. c in
          Hashtbl.replace st.memory.data (addr + i)
            (VInt (Z.of_int (round_float_to_int (new_re *. q_one))));
          Hashtbl.replace st.memory.data (addr + i + half)
            (VInt (Z.of_int (round_float_to_int (new_im *. q_one))))
        done;
        true
      end
    end
  | MATMUL_FP (rd_addr, rs_lhs, rs_rhs, rs_m, rs_k, rs_n) ->
    let dst = Z.to_int (to_z (getr st rd_addr)) in
    let lhs = Z.to_int (to_z (getr st rs_lhs)) in
    let rhs = Z.to_int (to_z (getr st rs_rhs)) in
    let m = Z.to_int (to_z (getr st rs_m)) in
    let k = Z.to_int (to_z (getr st rs_k)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    if m <= 0 || k <= 0 || n <= 0 || m > 32768 || k > 32768 || n > 32768 then revert st
    else begin
      if not (add_dyn_effort st (m * n * k / 512)) then revert st
      else begin
        let lhs_arr = Array.make (m * k) 0.0 in
        for i = 0 to m * k - 1 do
          lhs_arr.(i) <- mem_get_fp64 st.memory.data (lhs + i)
        done;
        let rhs_arr = Array.make (k * n) 0.0 in
        for i = 0 to k * n - 1 do
          rhs_arr.(i) <- mem_get_fp64 st.memory.data (rhs + i)
        done;
        let dst_arr = Array.make (m * n) 0.0 in
        for r = 0 to m - 1 do
          let r_k = r * k in
          let r_n = r * n in
          for c = 0 to n - 1 do
            let acc = ref 0.0 in
            for i = 0 to k - 1 do
              acc := !acc +. (Array.unsafe_get lhs_arr (r_k + i)
                              *. Array.unsafe_get rhs_arr (i * n + c))
            done;
            Array.unsafe_set dst_arr (r_n + c) !acc
          done
        done;
        for i = 0 to m * n - 1 do
          mem_set_fp64 st.memory.data (dst + i) (Array.unsafe_get dst_arr i)
        done;
        true
      end
    end
  | RMSNORM_FP (rs_addr, rs_n, rs_gamma) ->
    let addr = Z.to_int (to_z (getr st rs_addr)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    let gamma = Z.to_int (to_z (getr st rs_gamma)) in
    if n <= 0 || n > 131072 then revert st
    else begin
      if not (add_dyn_effort st (n * 4)) then revert st
      else begin
        let arr = Array.init n (fun i -> mem_get_fp64 st.memory.data (addr + i)) in
        let sum_sq = Array.fold_left (fun acc v -> acc +. v *. v) 0.0 arr in
        let mean_sq = sum_sq /. float_of_int n in
        let inv_rms = 1.0 /. sqrt (mean_sq +. 1e-5) in
        for i = 0 to n - 1 do
          let g = mem_get_fp64 st.memory.data (gamma + i) in
          mem_set_fp64 st.memory.data (addr + i) (g *. arr.(i) *. inv_rms)
        done;
        true
      end
    end
  | SILU_FP (rs_addr, rs_n) ->
    let addr = Z.to_int (to_z (getr st rs_addr)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    if n <= 0 || n > 131072 then revert st
    else begin
      if not (add_dyn_effort st (n * 3)) then revert st
      else begin
        for i = 0 to n - 1 do
          let x = mem_get_fp64 st.memory.data (addr + i) in
          let s = 1.0 /. (1.0 +. exp (-. x)) in
          mem_set_fp64 st.memory.data (addr + i) (x *. s)
        done;
        true
      end
    end
  | ELEMWISE_MUL_FP (rs_dst, rs_src, rs_n) ->
    let dst = Z.to_int (to_z (getr st rs_dst)) in
    let src = Z.to_int (to_z (getr st rs_src)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    if n <= 0 || n > 1_048_576 then revert st
    else begin
      if not (add_dyn_effort st n) then revert st
      else begin
        for i = 0 to n - 1 do
          let a = mem_get_fp64 st.memory.data (dst + i) in
          let b = mem_get_fp64 st.memory.data (src + i) in
          mem_set_fp64 st.memory.data (dst + i) (a *. b)
        done;
        true
      end
    end
  | RESIDUAL_ADD_FP (rs_dst, rs_src, rs_n) ->
    let dst = Z.to_int (to_z (getr st rs_dst)) in
    let src = Z.to_int (to_z (getr st rs_src)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    if n <= 0 || n > 1_048_576 then revert st
    else begin
      if not (add_dyn_effort st (n / 2)) then revert st
      else begin
        for i = 0 to n - 1 do
          let a = mem_get_fp64 st.memory.data (dst + i) in
          let b = mem_get_fp64 st.memory.data (src + i) in
          mem_set_fp64 st.memory.data (dst + i) (a +. b)
        done;
        true
      end
    end
  | ROPE_APPLY_FP (rs_addr, rs_n_dim, rs_pos, rs_base) ->
    let addr = Z.to_int (to_z (getr st rs_addr)) in
    let n_dim = Z.to_int (to_z (getr st rs_n_dim)) in
    let pos = Z.to_int (to_z (getr st rs_pos)) in
    let base_z = to_z (getr st rs_base) in
    if n_dim <= 0 || n_dim > 131072 || (n_dim land 1) <> 0 then revert st
    else if pos < 0 then revert st
    else begin
      if not (add_dyn_effort st (n_dim * 8)) then revert st
      else begin
        let base_f = z_to_fp64 base_z in
        let half = n_dim / 2 in
        let pf = float_of_int pos in
        let nf = float_of_int n_dim in
        for i = 0 to half - 1 do
          let exp_term = (2.0 *. float_of_int i) /. nf in
          let inv_freq = 1.0 /. (base_f ** exp_term) in
          let angle = pf *. inv_freq in
          let c = cos angle in
          let s = sin angle in
          let x_re = mem_get_fp64 st.memory.data (addr + i) in
          let x_im = mem_get_fp64 st.memory.data (addr + i + half) in
          mem_set_fp64 st.memory.data (addr + i) (x_re *. c -. x_im *. s);
          mem_set_fp64 st.memory.data (addr + i + half) (x_re *. s +. x_im *. c)
        done;
        true
      end
    end
  | LOAD_INT8_FP (rs_dst, rs_src, rs_off, rs_n, rs_scale) ->
    let dst = Z.to_int (to_z (getr st rs_dst)) in
    let src_b64 = to_string (getr st rs_src) in
    let off = Z.to_int (to_z (getr st rs_off)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    let scale = z_to_fp64 (to_z (getr st rs_scale)) in
    if n <= 0 || n > 1_048_576 then revert st
    else begin
      let cache_key =
        let len = String.length src_b64 in
        let prefix_len = min 32 len in
        let prefix_hash = ref 0 in
        for i = 0 to prefix_len - 1 do
          prefix_hash := (!prefix_hash * 31 + Char.code src_b64.[i]) land 0x7fffffff
        done;
        len * 1000003 + !prefix_hash
      in
      let decoded_opt = Hashtbl.find_opt st.decoded_chunk_cache cache_key in
      let decoded = match decoded_opt with
        | Some d -> Some d
        | None ->
          (match Base64.decode src_b64 with
           | Ok d ->
             Hashtbl.replace st.decoded_chunk_cache cache_key d;
             Some d
           | Error _ -> None)
      in
      match decoded with
      | None -> revert st
      | Some decoded ->
        let dlen = String.length decoded in
        if off < 0 || off + n > dlen then revert st
        else begin
          let was_cached = decoded_opt <> None in
          let cost = if was_cached then n / 2 else n + dlen / 4 in
          if not (add_dyn_effort st cost) then revert st
          else begin
            for i = 0 to n - 1 do
              let b = Char.code decoded.[off + i] in
              let signed = if b >= 128 then b - 256 else b in
              mem_set_fp64 st.memory.data (dst + i) (float_of_int signed *. scale)
            done;
            true
          end
        end
    end
  | VECDOT_FP (rd, rs_a, rs_b, rs_n) ->
    let a = Z.to_int (to_z (getr st rs_a)) in
    let b = Z.to_int (to_z (getr st rs_b)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    if n <= 0 || n > 1_048_576 then revert st
    else begin
      if not (add_dyn_effort st (n / 8)) then revert st
      else begin
        let a_arr = Array.make n 0.0 in
        let b_arr = Array.make n 0.0 in
        for i = 0 to n - 1 do
          a_arr.(i) <- mem_get_fp64 st.memory.data (a + i);
          b_arr.(i) <- mem_get_fp64 st.memory.data (b + i)
        done;
        let acc = ref 0.0 in
        for i = 0 to n - 1 do
          acc := !acc +. (Array.unsafe_get a_arr i *. Array.unsafe_get b_arr i)
        done;
        setr st rd (VInt (fp64_to_z !acc));
        true
      end
    end
  | ATTENTION_KV_FP (rs_q, rs_k, rs_v, rs_ctx, rs_T, rs_n_q_heads, rs_n_kv_heads, rs_head_dim) ->
    let q_addr = Z.to_int (to_z (getr st rs_q)) in
    let k_addr = Z.to_int (to_z (getr st rs_k)) in
    let v_addr = Z.to_int (to_z (getr st rs_v)) in
    let ctx_addr = Z.to_int (to_z (getr st rs_ctx)) in
    let t_total = Z.to_int (to_z (getr st rs_T)) in
    let n_q_heads = Z.to_int (to_z (getr st rs_n_q_heads)) in
    let n_kv_heads = Z.to_int (to_z (getr st rs_n_kv_heads)) in
    let head_dim = Z.to_int (to_z (getr st rs_head_dim)) in
    if t_total <= 0 || t_total > 8192 || n_q_heads <= 0 || n_q_heads > 256
       || n_kv_heads <= 0 || n_kv_heads > 256 || head_dim <= 0 || head_dim > 1024
       || n_q_heads mod n_kv_heads <> 0 then revert st
    else begin
      let cost = n_q_heads * t_total * head_dim * 4 in
      if not (add_dyn_effort st cost) then revert st
      else begin
        let kv_dim = n_kv_heads * head_dim in
        let group = n_q_heads / n_kv_heads in
        let inv_sqrt_d = 1.0 /. sqrt (float_of_int head_dim) in
        let scores = Array.make t_total 0.0 in
        for q_head = 0 to n_q_heads - 1 do
          let kv_head = q_head / group in
          let q_off = q_head * head_dim in
          for t = 0 to t_total - 1 do
            let k_off = t * kv_dim + kv_head * head_dim in
            let acc = ref 0.0 in
            for d = 0 to head_dim - 1 do
              let qv = mem_get_fp64 st.memory.data (q_addr + q_off + d) in
              let kv = mem_get_fp64 st.memory.data (k_addr + k_off + d) in
              acc := !acc +. (qv *. kv)
            done;
            scores.(t) <- !acc *. inv_sqrt_d
          done;
          let max_score = ref neg_infinity in
          for t = 0 to t_total - 1 do
            if scores.(t) > !max_score then max_score := scores.(t)
          done;
          let sum_exp = ref 0.0 in
          for t = 0 to t_total - 1 do
            let e = exp (scores.(t) -. !max_score) in
            scores.(t) <- e;
            sum_exp := !sum_exp +. e
          done;
          let inv_sum = 1.0 /. !sum_exp in
          for t = 0 to t_total - 1 do
            scores.(t) <- scores.(t) *. inv_sum
          done;
          for d = 0 to head_dim - 1 do
            let acc = ref 0.0 in
            for t = 0 to t_total - 1 do
              let v_off = t * kv_dim + kv_head * head_dim + d in
              acc := !acc +. (scores.(t) *. mem_get_fp64 st.memory.data (v_addr + v_off))
            done;
            mem_set_fp64 st.memory.data (ctx_addr + q_off + d) !acc
          done
        done;
        true
      end
    end
  | APPEND_VEC_FP (rs_dst, rs_pos, rs_src, rs_n) ->
    let dst = Z.to_int (to_z (getr st rs_dst)) in
    let pos = Z.to_int (to_z (getr st rs_pos)) in
    let src = Z.to_int (to_z (getr st rs_src)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    if n <= 0 || n > 1_048_576 || pos < 0 || pos > 16_777_216 then revert st
    else begin
      if not (add_dyn_effort st n) then revert st
      else begin
        for i = 0 to n - 1 do
          let v = mem_get_fp64 st.memory.data (src + i) in
          mem_set_fp64 st.memory.data (dst + pos * n + i) v
        done;
        true
      end
    end
  | ARGMAX_FP (rd, rs_addr, rs_n) ->
    let addr = Z.to_int (to_z (getr st rs_addr)) in
    let n = Z.to_int (to_z (getr st rs_n)) in
    if n <= 0 || n > 1_048_576 then revert st
    else begin
      if not (add_dyn_effort st (n / 2)) then revert st
      else begin
        let best_id = ref 0 in
        let best_val = ref neg_infinity in
        for i = 0 to n - 1 do
          let v = mem_get_fp64 st.memory.data (addr + i) in
          if v > !best_val then begin
            best_val := v;
            best_id := i
          end
        done;
        setr st rd (VInt (Z.of_int !best_id));
        true
      end
    end
  | JMP addr ->
    st.pc <- addr; true
  | JIF (rs, addr) ->
    if to_bool (getr st rs) then st.pc <- addr; true
  | JDEST _ -> true
  | STOP ->
    (match st.return_stack with
     | [] -> false
     | (ret_pc, dest_r, saved_regs) :: rest ->
       let result = st.regs.(0) in
       Array.blit saved_regs 0 st.regs 0 64;
       st.regs.(dest_r) <- result;
       st.return_stack <- rest;
       st.pc <- ret_pc;
       true)
  | REVERT -> revert st
  | CALLER rd -> setr st rd (VAddr st.caller); true
  | ORIGIN rd -> setr st rd (VAddr st.origin); true
  | SELF rd -> setr st rd (VAddr st.address); true
  | EPOCH rd -> setr st rd (VInt (Z.of_int st.ctx.current_epoch)); true
  | VALUE rd -> setr st rd (VInt st.value); true
  | BALANCE (rd, rs) ->
    (match getr st rs with
     | VAddr addr | VString addr -> setr st rd (VInt (st.ctx.get_balance addr)); true
     | _ -> setr st rd (VInt Z.zero); true)
  | TREEHASH rd -> setr st rd (VString st.ctx.tree_hash); true
  | NODEID rd -> setr st rd (VString st.ctx.node_id); true
  | TXHASH rd -> setr st rd (VString st.ctx.tx_hash); true
  | XCALL (rd, rt, rm, ra, nargs) ->
    if st.call_depth >= 8 then revert st
    else
      let target = to_string (getr st rt) in
      let method_name = to_string (getr st rm) in
      let args = List.init nargs (fun i -> getr st (ra + i)) in
      (match st.ctx.call_contract st.address target method_name args (st.call_depth + 1) with
       | Ok sub ->
         if not (add_dyn_effort st sub.effort_used) then revert st
         else begin
           st.logs := List.rev_append sub.events !(st.logs);
           setr st rd sub.return_value;
           true
         end
       | Error _ -> revert st)
  | SPAWN (rd, rs) ->
    if not (view_guard st) then false
    else if st.call_depth >= 8 then revert st
    else
      let input = to_string (getr st rs) in

      let bytecode_raw =
        if String.length input >= 4 && String.sub input 0 4 = "OCTB" then input
        else (try Base64.decode_exn input with _ -> input) in
      if String.length bytecode_raw < 12 then revert st
      else
        let nonce_key = "\x00spawn_nonce" in
        let nonce = match Hashtbl.find_opt st.storage nonce_key with
          | Some s -> (try int_of_string s with _ -> 0) | None -> 0 in
        Hashtbl.replace st.storage nonce_key (string_of_int (nonce + 1));

        let spawn_effort = 5000 + (String.length bytecode_raw / 100) in
        if not (add_dyn_effort st spawn_effort) then revert st
        else
        (match st.ctx.deploy_contract st.address bytecode_raw nonce (st.call_depth + 1) [] with
         | Ok sp ->
           if not (add_dyn_effort st sp.effort_used) then revert st
           else begin
             st.logs := List.rev_append sp.events !(st.logs);
             setr st rd (VAddr sp.spawned_addr);
             true
           end
         | Error e ->
           Octra_log.stdout "SPAWN revert: %s (bytecode %d bytes)\n%!" e (String.length bytecode_raw);
           Hashtbl.replace st.storage nonce_key (string_of_int nonce); revert st)
  | SPAWN2 (rd, rs, base, nargs) ->
    if not (view_guard st) then false
    else if st.call_depth >= 8 then revert st
    else
      let input = to_string (getr st rs) in
      let bytecode_raw =
        if String.length input >= 4 && String.sub input 0 4 = "OCTB" then input
        else (try Base64.decode_exn input with _ -> input) in
      if String.length bytecode_raw < 12 then revert st
      else
        let params = List.init nargs (fun i -> getr st (base + i)) in
        let nonce_key = "\x00spawn_nonce" in
        let nonce = match Hashtbl.find_opt st.storage nonce_key with
          | Some s -> (try int_of_string s with _ -> 0) | None -> 0 in
        Hashtbl.replace st.storage nonce_key (string_of_int (nonce + 1));
        let spawn_effort = 5000 + (String.length bytecode_raw / 100) in
        if not (add_dyn_effort st spawn_effort) then revert st
        else
        (match st.ctx.deploy_contract st.address bytecode_raw nonce (st.call_depth + 1) params with
         | Ok sp ->
           if not (add_dyn_effort st sp.effort_used) then revert st
           else begin
             st.logs := List.rev_append sp.events !(st.logs);
             setr st rd (VAddr sp.spawned_addr);
             true
           end
         | Error e ->
           Octra_log.stdout "SPAWN2 revert: %s (bytecode %d bytes, %d params)\n%!" e (String.length bytecode_raw) nargs;
           Hashtbl.replace st.storage nonce_key (string_of_int nonce); revert st)
  | TRANSFER (rd, ra, rv) ->
    if not (view_guard st) then false
    else
      let to_addr = to_string (getr st ra) in
      if not (is_valid_addr to_addr) then (setr st rd (VBool false); true)
      else
        let amount = to_z (getr st rv) in

        if Z.sign amount < 0 then (setr st rd (VBool false); true)
        else if Z.equal amount Z.zero then (setr st rd (VBool true); true)
        else
          let ok = st.ctx.do_transfer st.address to_addr amount in
          setr st rd (VBool ok); true
  | CHECKPOINT ->
    st.undo_id <- st.undo_id + 1;
    st.undo_stack <- UndoMarker st.undo_id :: st.undo_stack;
    true
  | ROLLBACK ->
    let rec restore = function
      | [] -> []
      | UndoMarker _ :: rest -> rest
      | UndoWrite (k, Some v) :: rest ->
        Hashtbl.replace st.storage k v; restore rest
      | UndoWrite (k, None) :: rest ->
        Hashtbl.remove st.storage k; restore rest
    in
    st.undo_stack <- restore st.undo_stack;
    true
  | COMMIT ->
    let rec discard = function
      | [] -> []
      | UndoMarker _ :: rest -> rest
      | _ :: rest -> discard rest
    in
    st.undo_stack <- discard st.undo_stack;
    true
  | EMIT (event, regs_list) ->
    if List.length !(st.logs) >= 256 then revert st
    else
    let vals = List.map (fun r -> getr st r) regs_list in
    st.logs := { contract = st.address; depth = st.call_depth; event; values = vals }
               :: !(st.logs);
    true
  | CONCAT (rd, rs1, rs2) ->
    setr st rd (VString (to_string (getr st rs1) ^ to_string (getr st rs2))); true
  | STRLEN (rd, rs) ->
    setr st rd (VInt (Z.of_int (String.length (to_string (getr st rs))))); true
  | ASSERT rs ->
    if not (to_bool (getr st rs)) then revert st else true
  | EFFORT rd ->
    setr st rd (VInt (Z.of_int st.effort_used)); true
  | NOP -> true
  | FHE_LOAD_PK (rd, rs) ->
    if not (st.ctx.allow_fhe_capability Fhe_load_pk_cap) then
      revert_with_reason st "fhe_load_pk not allowed"
    else
      let addr = to_string (getr st rs) in
      (match st.ctx.get_fhe_pubkey addr with
       | Some pk -> setr st rd (VPubKey pk); true
       | None -> revert_with_reason st ("fhe pubkey not available: " ^ addr))
  | FHE_ADD (rd, rpk, ra, rb) ->
    if not (st.ctx.allow_fhe_capability Fhe_cipher_arithmetic_cap) then
      revert st
    else
      (match to_pubkey (getr st rpk), to_cipher (getr st ra), to_cipher (getr st rb) with
       | Some pk, Some a, Some b ->
         (try setr st rd (VCipher (Pvac_ffi.ct_add pk a b)); true
          with _ -> revert st)
       | _ -> revert st)
  | FHE_SUB (rd, rpk, ra, rb) ->
    if not (st.ctx.allow_fhe_capability Fhe_cipher_arithmetic_cap) then
      revert st
    else
      (match to_pubkey (getr st rpk), to_cipher (getr st ra), to_cipher (getr st rb) with
       | Some pk, Some a, Some b ->
         (try setr st rd (VCipher (Pvac_ffi.ct_sub pk a b)); true
          with _ -> revert st)
       | _ -> revert st)
  | FHE_MUL (rd, rpk, ra, rb) ->
    if not (st.ctx.allow_fhe_capability Fhe_cipher_arithmetic_cap) then
      revert st
    else
      (match to_pubkey (getr st rpk), to_cipher (getr st ra), to_cipher (getr st rb) with
       | Some pk, Some a, Some b ->
         (try
            let seed = deterministic_seed [
              "aml.fhe_mul";
              st.ctx.tx_hash;
              st.address;
              string_of_int st.pc;
              string_of_int rd;
            ] in
            setr st rd (VCipher (Pvac_ffi.ct_mul_seeded pk a b seed)); true
          with _ -> revert st)
       | _ -> revert st)
  | FHE_SCALE (rd, rpk, rct, rscalar) ->
    if not (st.ctx.allow_fhe_capability Fhe_cipher_arithmetic_cap) then
      revert st
    else
      (match to_pubkey (getr st rpk), to_cipher (getr st rct) with
       | Some pk, Some ct ->
         (try
            let s = Z.to_int64 (to_z (getr st rscalar)) in
            setr st rd (VCipher (Pvac_ffi.ct_scale pk ct s)); true
          with _ -> revert st)
       | _ -> revert st)
  | FHE_ADD_CONST (rd, rpk, rct, rconst) ->
    if not (st.ctx.allow_fhe_capability Fhe_cipher_arithmetic_cap) then
      revert st
    else
      (match to_pubkey (getr st rpk), to_cipher (getr st rct) with
       | Some pk, Some ct ->
         (try
            let c = Z.to_int64 (to_z (getr st rconst)) in
            setr st rd (VCipher (Pvac_ffi.ct_add_const pk ct c 0L)); true
          with _ -> revert st)
       | _ -> revert st)
  | FHE_SUB_CONST (rd, rpk, rct, rconst) ->
    if not (st.ctx.allow_fhe_capability Fhe_cipher_arithmetic_cap) then
      revert st
    else
      (match to_pubkey (getr st rpk), to_cipher (getr st rct) with
       | Some pk, Some ct ->
         (try
            let c = Z.to_int64 (to_z (getr st rconst)) in
            setr st rd (VCipher (Pvac_ffi.ct_sub_const pk ct c)); true
          with _ -> revert st)
       | _ -> revert st)
  | FHE_VERIFY_ZERO (rd, rpk, rct, rproof) ->
    if not (st.ctx.allow_fhe_capability Fhe_verify_zero_cap) then
      revert st
    else
      (match to_pubkey (getr st rpk), to_cipher (getr st rct), to_bytes (getr st rproof) with
       | Some pk, Some ct, Some proof_bytes ->
         let raw_len = match Base64.decode proof_bytes with
           | Ok r -> String.length r | Error _ -> String.length proof_bytes in
         if raw_len > max_fhe_proof_bytes then
           (setr st rd (VBool false); true)
         else
           (match deser_bytes Pvac_ffi.deserialize_zero_proof proof_bytes with
            | Some proof ->
              setr st rd (VBool (Pvac_ffi.verify_zero pk ct proof)); true
            | None -> setr st rd (VBool false); true)
       | _ -> revert st)
  | FHE_VERIFY_RANGE (rd, rpk, rct, rproof) ->
    if not (st.ctx.allow_fhe_capability Fhe_verify_range_cap) then
      revert st
    else if not st.is_view then

      revert st
    else
    (match to_pubkey (getr st rpk), to_cipher (getr st rct), to_bytes (getr st rproof) with
     | Some pk, Some ct, Some proof_bytes ->
       let raw_len = match Base64.decode proof_bytes with
         | Ok r -> String.length r | Error _ -> String.length proof_bytes in
       if raw_len > max_fhe_proof_bytes then
         (setr st rd (VBool false); true)
       else
         let raw = match Base64.decode proof_bytes with
           | Ok r -> Bytes.of_string r | Error _ -> Bytes.of_string proof_bytes in
         (try
            let ok = Pvac_ffi.verify_range_any pk ct raw in
            setr st rd (VBool ok); true
          with _ -> setr st rd (VBool false); true)
     | _ -> revert st)
  | GROTH16_VERIFY_BN254 (rd, rvk, rproof, rinputs) ->
    if not st.is_view then
      revert st
    else
    (match to_bytes (getr st rvk), to_bytes (getr st rproof), to_bytes (getr st rinputs) with
     | Some vk_in, Some proof_in, Some inputs_in ->
       let decode_b64 b =
         match Base64.decode b with Ok r -> r | Error _ -> b
       in
       let vk_raw = decode_b64 vk_in in
       let proof_raw = decode_b64 proof_in in
       let inputs_raw = decode_b64 inputs_in in
       if String.length vk_raw > max_zk_vk_bytes
          || String.length proof_raw > max_zk_proof_bytes
          || String.length inputs_raw > max_zk_inputs_bytes then
         (setr st rd (VBool false); true)
       else
         (try
            let ok = Zk_ffi.groth16_verify_bn254
              (Bytes.of_string vk_raw)
              (Bytes.of_string proof_raw)
              (Bytes.of_string inputs_raw) in
            setr st rd (VBool ok); true
          with _ -> setr st rd (VBool false); true)
     | _ -> revert st)
  | FHE_VERIFY_BOUND (rd, rpk, rct, rproof, rcommit) ->
    if not (st.ctx.allow_fhe_capability Fhe_verify_bound_cap) then
      revert st
    else
      (match to_pubkey (getr st rpk), to_cipher (getr st rct),
             to_bytes (getr st rproof), to_bytes (getr st rcommit) with
       | Some pk, Some ct, Some proof_bytes, Some commit_bytes ->
         let raw_len = match Base64.decode proof_bytes with
           | Ok r -> String.length r | Error _ -> String.length proof_bytes in
         if raw_len > max_fhe_proof_bytes then
           (setr st rd (VBool false); true)
         else
           let commit_raw = match Base64.decode commit_bytes with
             | Ok r -> Bytes.of_string r | Error _ -> Bytes.of_string commit_bytes in
           (match deser_bytes Pvac_ffi.deserialize_zero_proof proof_bytes with
            | Some proof ->
              (try
                let ok = Pvac_ffi.verify_zero_bound pk ct proof commit_raw in
                setr st rd (VBool ok); true
               with _ -> setr st rd (VBool false); true)
            | None -> setr st rd (VBool false); true)
       | _ -> revert st)
  | FHE_COMMIT (rd, rpk, rct) ->
    if not (st.ctx.allow_fhe_capability Fhe_commit_cap) then
      revert st
    else
      (match to_pubkey (getr st rpk), to_cipher (getr st rct) with
       | Some pk, Some ct ->
         (try
            let raw = Bytes.to_string (Pvac_ffi.commit_ct pk ct) in
            setr st rd (VString (Base64.encode_exn raw)); true
          with _ -> revert st)
       | _ -> revert st)
  | FHE_PEDERSEN (rd, ramount, rblinding) ->
    if not (st.ctx.allow_fhe_capability Fhe_pedersen_cap) then
      revert st
    else
      (match to_bytes (getr st rblinding) with
       | Some blinding ->
         (try
            let amount = Z.to_int64 (to_z (getr st ramount)) in
            let result = Pvac_ffi.pedersen_commit_amount amount (Bytes.of_string blinding) in
            setr st rd (VString (Base64.encode_exn (Bytes.to_string result))); true
          with _ -> revert st)
       | _ -> revert st)
  | FHE_SER (rd, rct) ->
    if not (st.ctx.allow_fhe_capability Fhe_cipher_serde_cap) then
      revert st
    else
      (match to_cipher (getr st rct) with
       | Some ct ->
         (try
            let raw = Bytes.to_string (Pvac_ffi.serialize_cipher ct) in
            setr st rd (VString (Base64.encode_exn raw)); true
          with _ -> revert st)
       | None -> revert st)
  | FHE_DESER (rd, rbytes) ->
    if not (st.ctx.allow_fhe_capability Fhe_cipher_serde_cap) then
      revert st
    else
      (match to_bytes (getr st rbytes) with
       | Some b ->
         (match deser_bytes Pvac_ffi.deserialize_cipher b with
          | Some ct -> setr st rd (VCipher ct); true
          | None -> revert st)
       | None -> revert st)
  | FHE_SER_PK (rd, rpk) ->
    if not (st.ctx.allow_fhe_capability Fhe_pubkey_serde_cap) then
      revert st
    else
      (match to_pubkey (getr st rpk) with
       | Some pk ->
         (try
            let raw = Bytes.to_string (Pvac_ffi.serialize_pubkey pk) in
            setr st rd (VString (Base64.encode_exn raw)); true
          with _ -> revert st)
       | None -> revert st)
  | FHE_DESER_PK (rd, rbytes) ->
    if not (st.ctx.allow_fhe_capability Fhe_pubkey_serde_cap) then
      revert st
    else
      (match to_bytes (getr st rbytes) with
       | Some b ->
         (match deser_bytes Pvac_ffi.deserialize_pubkey b with
          | Some pk -> setr st rd (VPubKey pk); true
          | None -> revert st)
       | None -> revert st)
  | CALL_INT (rd, label) ->
    if List.length st.return_stack >= 8 then revert st
    else begin
      let saved = Array.copy st.regs in
      st.return_stack <- (st.pc, rd, saved) :: st.return_stack;
      st.pc <- label;
      true
    end

let run state program =
  let len = Array.length program in
  while state.pc < len && not state.reverted do
    let op = program.(state.pc) in
    state.pc <- state.pc + 1;
    if not (exec_one state op) then state.pc <- len
  done;
  not state.reverted

module Verifier = struct
  type err =
    | InvalidReg of int * int
    | InvalidJumpDest of int
    | DuplicateJDest of int
    | CodeTooLarge of int
    | EmptyCode
    | ReservedKey of int * string

  let max_size = 33_554_432

  let check_reg pc r =
    if r < 0 || r > 63 then Some (InvalidReg (pc, r)) else None

  let check_regs pc regs =
    List.find_map (check_reg pc) regs

  let verify code =
    if Array.length code = 0 then Error EmptyCode
    else if Array.length code * 32 > max_size then Error (CodeTooLarge (Array.length code))
    else
      let dests = Hashtbl.create 32 in
      let dup = ref None in
      Array.iter (fun op -> match op with
        | JDEST n ->
          if Hashtbl.mem dests n then dup := Some n;
          Hashtbl.replace dests n true
        | _ -> ()
      ) code;
      match !dup with
      | Some n -> Error (DuplicateJDest n)
      | None ->
      let rec check pc =
        if pc >= Array.length code then Ok ()
        else
          let err = match code.(pc) with
            | ADD (d,a,b) | SUB (d,a,b) | MUL (d,a,b)
            | DIV (d,a,b) | MOD (d,a,b)
            | EQ (d,a,b) | LT (d,a,b) | GT (d,a,b) | NEQ (d,a,b)
            | CONCAT (d,a,b) -> check_regs pc [d;a;b]
            | NEG (d,s) | ABS (d,s) | MOV (d,s) | BALANCE (d,s)
            | SLOADK (d,s) | SSTOREK (d,s) | SPAWN (d,s)
            | STRLEN (d,s) -> check_regs pc [d;s]
            | SDELK s -> check_reg pc s
            | LDI (d,_) | SLOAD (d,_) | MLOAD (d,_)
            | CALLER d | ORIGIN d | SELF d | EPOCH d | VALUE d
            | TREEHASH d | NODEID d | TXHASH d | EFFORT d -> check_reg pc d
            | SSTORE (k,s) ->
              if is_reserved_key k then Some (ReservedKey (pc, k))
              else check_reg pc s
            | SDEL k ->
              if is_reserved_key k then Some (ReservedKey (pc, k))
              else None
            | MSTORE (_,s) | ASSERT s -> check_reg pc s
            | TRANSFER (d,a,v) -> check_regs pc [d;a;v]
            | XCALL (d,t,m,a,n) -> check_regs pc [d;t;m] |> (function
              | Some e -> Some e
              | None -> check_regs pc (List.init n (fun i -> a + i)))
            | SPAWN2 (d,s,a,n) -> check_regs pc [d;s] |> (function
              | Some e -> Some e
              | None -> check_regs pc (List.init n (fun i -> a + i)))
            | FHE_ADD (d,pk,a,b) | FHE_SUB (d,pk,a,b) | FHE_MUL (d,pk,a,b)
            | FHE_SCALE (d,pk,a,b) | FHE_ADD_CONST (d,pk,a,b)
            | FHE_SUB_CONST (d,pk,a,b)
            | FHE_VERIFY_ZERO (d,pk,a,b) | FHE_VERIFY_RANGE (d,pk,a,b) ->
              check_regs pc [d;pk;a;b]
            | GROTH16_VERIFY_BN254 (d,vk,pf,inp) -> check_regs pc [d;vk;pf;inp]
            | FHE_VERIFY_BOUND (d,pk,ct,pf,cm) -> check_regs pc [d;pk;ct;pf;cm]
            | FHE_COMMIT (d,pk,ct) | FHE_PEDERSEN (d,pk,ct) -> check_regs pc [d;pk;ct]
            | FHE_LOAD_PK (d,s) | FHE_SER (d,s) | FHE_DESER (d,s)
            | FHE_SER_PK (d,s) | FHE_DESER_PK (d,s)
            | MLOADR (d,s) | MSTORER (d,s) -> check_regs pc [d;s]
            | PARSE_INTS (d,a,b) -> check_regs pc [d;a;b]
            | ISADDR (d,s) -> check_regs pc [d;s]
            | ISHEX (d,s) -> check_regs pc [d;s]
            | STATE_PATH_KEY (d,s) -> check_regs pc [d;s]
            | OBJECT_MEMBER_COUNT (d,s) -> check_regs pc [d;s]
            | OBJECT_HAS_MEMBER (d,a,b)
            | OBJECT_MEMBER_REF_AT (d,a,b) -> check_regs pc [d;a;b]
            | OBJECT_TRANSITION_APPLY (d,a,b,c,e,f,g,h,i,j,k) ->
              check_regs pc [d;a;b;c;e;f;g;h;i;j;k]
            | ASSERT_ADDR s -> check_reg pc s
            | SUBSTR (d,s,a,b) -> check_regs pc [d;s;a;b]
            | INDEXOF (d,s,p) -> check_regs pc [d;s;p]
            | SHA256 (d,s) | KECCAK256 (d,s) -> check_regs pc [d;s]
            | ED25519_OK (d,pk,msg,sig_) -> check_regs pc [d;pk;msg;sig_]
            | BITAND (d,a,b) | BITOR (d,a,b) | BITXOR (d,a,b)
            | BITSHL (d,a,b) | BITSHR (d,a,b) -> check_regs pc [d;a;b]
            | SKEYS (d,p,b) -> check_regs pc [d;p;b]
            | SKEYS_PAGE (d,n,p,a,b) -> check_regs pc [d;n;p;a;b]
            | SLOADN (a,b,c) -> check_regs pc [a;b;c]
            | SSTOREN (a,b,c) -> check_regs pc [a;b;c]
            | FSTORE (d,s) -> check_regs pc [d;s]
            | FLOAD (d,s) -> check_regs pc [d;s]
            | MATMUL (d,l,r,m,k,n) -> check_regs pc [d;l;r;m;k;n]
            | VECDOT (d,a,b,n) -> check_regs pc [d;a;b;n]
            | EXP_LUT (d,x) -> check_regs pc [d;x]
            | SOFTMAX_INPLACE (a,n) -> check_regs pc [a;n]
            | LAYERNORM_INPLACE (a,n,g,b) -> check_regs pc [a;n;g;b]
            | RELU_INPLACE (a,n) -> check_regs pc [a;n]
            | RMSNORM_INPLACE (a,n,g) -> check_regs pc [a;n;g]
            | SILU_INPLACE (a,n) -> check_regs pc [a;n]
            | ELEMWISE_MUL_INPLACE (d,s,n) -> check_regs pc [d;s;n]
            | LOAD_INT8_BYTES_TO_MEM (d,s,o,n,sc) -> check_regs pc [d;s;o;n;sc]
            | RESIDUAL_ADD (d,s,n) -> check_regs pc [d;s;n]
            | ROPE_APPLY (a,n,p,b) -> check_regs pc [a;n;p;b]
            | LOAD_INT8_B64_TO_MEM (d,s,o,n,sc) -> check_regs pc [d;s;o;n;sc]
            | MATMUL_Q16 (d,l,r,m,k,n) -> check_regs pc [d;l;r;m;k;n]
            | SHIFT_ROUND_INPLACE (a,n,b) -> check_regs pc [a;n;b]
            | MATMUL_FP (d,l,r,m,k,n) -> check_regs pc [d;l;r;m;k;n]
            | RMSNORM_FP (a,n,g) -> check_regs pc [a;n;g]
            | SILU_FP (a,n) -> check_regs pc [a;n]
            | ELEMWISE_MUL_FP (d,s,n) -> check_regs pc [d;s;n]
            | RESIDUAL_ADD_FP (d,s,n) -> check_regs pc [d;s;n]
            | ROPE_APPLY_FP (a,n,p,b) -> check_regs pc [a;n;p;b]
            | LOAD_INT8_FP (d,s,o,n,sc) -> check_regs pc [d;s;o;n;sc]
            | VECDOT_FP (d,a,b,n) -> check_regs pc [d;a;b;n]
            | ARGMAX_FP (d,a,n) -> check_regs pc [d;a;n]
            | ATTENTION_KV_FP (q,k,v,c,t,nq,nk,hd) -> check_regs pc [q;k;v;c;t;nq;nk;hd]
            | APPEND_VEC_FP (d,p,s,n) -> check_regs pc [d;p;s;n]
            | EMIT (_,rs) -> check_regs pc rs
            | JIF (s, dest) ->
              (match check_reg pc s with
               | Some e -> Some e
               | None ->
                 if not (Hashtbl.mem dests dest) then Some (InvalidJumpDest dest)
                 else None)
            | JMP dest ->
              if not (Hashtbl.mem dests dest) then Some (InvalidJumpDest dest)
              else None
            | CALL_INT (d, dest) ->
              (match check_reg pc d with
               | Some e -> Some e
               | None ->
                 if not (Hashtbl.mem dests dest) then Some (InvalidJumpDest dest)
                 else None)
            | JDEST _ -> None
            | _ -> None
          in
          match err with Some e -> Error e | None -> check (pc + 1)
      in
      check 0
end