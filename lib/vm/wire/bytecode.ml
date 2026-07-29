(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let magic = "OCTB"
let version = 1

type const =
  | CInt of string
  | CBool of bool
  | CStr of string
  | CBytes of string
  | CAddr of string

let const_of_v = function
  | Contract_vm.VInt z -> CInt (Z.to_string z)
  | Contract_vm.VBool b -> CBool b
  | Contract_vm.VString s -> CStr s
  | Contract_vm.VBytes b -> CBytes b
  | Contract_vm.VBytes32 b -> CBytes b
  | Contract_vm.VU64 z -> CInt (Z.to_string z)
  | Contract_vm.VU128 z -> CInt (Z.to_string z)
  | Contract_vm.VU256 z -> CInt (Z.to_string z)
  | Contract_vm.VAddr a -> CAddr a
  | Contract_vm.VCipher ct -> CBytes (Bytes.to_string (Pvac_ffi.serialize_cipher ct))
  | Contract_vm.VPubKey pk -> CBytes (Bytes.to_string (Pvac_ffi.serialize_pubkey pk))

let v_of_const = function
  | CInt s -> Contract_vm.VInt (Z.of_string s)
  | CBool b -> Contract_vm.VBool b
  | CStr s -> Contract_vm.VString s
  | CBytes b -> Contract_vm.VBytes b
  | CAddr a -> Contract_vm.VAddr a

let const_tag = function
  | CInt _ -> 0 | CBool _ -> 1 | CStr _ -> 2 | CBytes _ -> 3 | CAddr _ -> 4

let const_data = function
  | CInt s -> s | CStr s -> s | CBytes s -> s | CAddr s -> s
  | CBool b -> if b then "1" else "0"

module Pool = struct
  type t = {
    mutable entries : const list;
    index : (string, int) Hashtbl.t;
  }

  let create () = { entries = []; index = Hashtbl.create 64 }

  let key_of_const c =
    Printf.sprintf "%d:%s" (const_tag c) (const_data c)

  let intern pool c =
    let k = key_of_const c in
    match Hashtbl.find_opt pool.index k with
    | Some idx -> idx
    | None ->
      let idx = List.length pool.entries in
      pool.entries <- pool.entries @ [c];
      Hashtbl.replace pool.index k idx;
      idx

  let to_list pool = pool.entries
end

let put_u8 buf v = Buffer.add_char buf (Char.chr (v land 0xff))
let put_u16le buf v =
  Buffer.add_char buf (Char.chr (v land 0xff));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xff))
let put_u32le buf v =
  Buffer.add_char buf (Char.chr (v land 0xff));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr ((v lsr 16) land 0xff));
  Buffer.add_char buf (Char.chr ((v lsr 24) land 0xff))

let get_u8 s pos = Char.code (Bytes.get s pos)
let get_u16le s pos =
  (Char.code (Bytes.get s pos)) lor
  ((Char.code (Bytes.get s (pos + 1))) lsl 8)
let get_u32le s pos =
  (Char.code (Bytes.get s pos)) lor
  ((Char.code (Bytes.get s (pos + 1))) lsl 8) lor
  ((Char.code (Bytes.get s (pos + 2))) lsl 16) lor
  ((Char.code (Bytes.get s (pos + 3))) lsl 24)

let op_tag = function
  | Contract_vm.ADD _ -> 0x00  | Contract_vm.SUB _ -> 0x01
  | Contract_vm.MUL _ -> 0x02  | Contract_vm.DIV _ -> 0x03
  | Contract_vm.MOD _ -> 0x04  | Contract_vm.NEG _ -> 0x05
  | Contract_vm.ABS _ -> 0x06  | Contract_vm.EQ _ -> 0x07
  | Contract_vm.LT _ -> 0x08   | Contract_vm.GT _ -> 0x09
  | Contract_vm.NEQ _ -> 0x0A  | Contract_vm.LDI _ -> 0x0B
  | Contract_vm.MOV _ -> 0x0C  | Contract_vm.SLOAD _ -> 0x0D
  | Contract_vm.SSTORE _ -> 0x0E | Contract_vm.SDEL _ -> 0x0F
  | Contract_vm.SLOADK _ -> 0x10 | Contract_vm.SSTOREK _ -> 0x11
  | Contract_vm.SDELK _ -> 0x54
  | Contract_vm.MLOAD _ -> 0x12  | Contract_vm.MSTORE _ -> 0x13
  | Contract_vm.JMP _ -> 0x14    | Contract_vm.JIF _ -> 0x15
  | Contract_vm.JDEST _ -> 0x16  | Contract_vm.STOP -> 0x17
  | Contract_vm.REVERT -> 0x18   | Contract_vm.CALLER _ -> 0x19
  | Contract_vm.ORIGIN _ -> 0x1A | Contract_vm.SELF _ -> 0x1B
  | Contract_vm.EPOCH _ -> 0x1C  | Contract_vm.VALUE _ -> 0x1D
  | Contract_vm.EPOCH_TIME _ -> 0x7B
  | Contract_vm.BALANCE _ -> 0x1E | Contract_vm.TREEHASH _ -> 0x1F
  | Contract_vm.NODEID _ -> 0x20  | Contract_vm.XCALL _ -> 0x21
  | Contract_vm.TXHASH _ -> 0x7A
  | Contract_vm.SPAWN _ -> 0x22   | Contract_vm.TRANSFER _ -> 0x23
  | Contract_vm.CHECKPOINT -> 0x24 | Contract_vm.ROLLBACK -> 0x25
  | Contract_vm.COMMIT -> 0x26    | Contract_vm.EMIT _ -> 0x27
  | Contract_vm.CONCAT _ -> 0x28  | Contract_vm.ASSERT _ -> 0x29
  | Contract_vm.EFFORT _ -> 0x2A  | Contract_vm.NOP -> 0x2B
  | Contract_vm.STRLEN _ -> 0x2C
  | Contract_vm.CALL_INT _ -> 0x2D
  | Contract_vm.MLOADR _ -> 0x2E
  | Contract_vm.MSTORER _ -> 0x2F
  | Contract_vm.PARSE_INTS _ -> 0x40
  | Contract_vm.ISADDR _ -> 0x41
  | Contract_vm.STATE_PATH_KEY _ -> 0x56
  | Contract_vm.OBJECT_TRANSITION_APPLY _ -> 0x57
  | Contract_vm.OBJECT_MEMBER_COUNT _ -> 0x58
  | Contract_vm.OBJECT_HAS_MEMBER _ -> 0x59
  | Contract_vm.OBJECT_MEMBER_REF_AT _ -> 0x5A
  | Contract_vm.ISHEX _ -> 0x52
  | Contract_vm.SPAWN2 _ -> 0x53
  | Contract_vm.ASSERT_ADDR _ -> 0x42
  | Contract_vm.SUBSTR _ -> 0x43
  | Contract_vm.INDEXOF _ -> 0x44
  | Contract_vm.SHA256 _ -> 0x45
  | Contract_vm.KECCAK256 _ -> 0x46
  | Contract_vm.ED25519_OK _ -> 0x55
  | Contract_vm.BITAND _ -> 0x47
  | Contract_vm.BITOR _ -> 0x48
  | Contract_vm.BITXOR _ -> 0x49
  | Contract_vm.BITSHL _ -> 0x4A
  | Contract_vm.BITSHR _ -> 0x4B
  | Contract_vm.SKEYS _ -> 0x4C
  | Contract_vm.SKEYS_PAGE _ -> 0x51
  | Contract_vm.SLOADN _ -> 0x4D
  | Contract_vm.SSTOREN _ -> 0x4E
  | Contract_vm.FSTORE _ -> 0x4F
  | Contract_vm.FLOAD _ -> 0x50
  | Contract_vm.FHE_LOAD_PK _ -> 0x30 | Contract_vm.FHE_ADD _ -> 0x31
  | Contract_vm.FHE_SUB _ -> 0x32     | Contract_vm.FHE_SCALE _ -> 0x33
  | Contract_vm.FHE_ADD_CONST _ -> 0x34 | Contract_vm.FHE_SUB_CONST _ -> 0x35
  | Contract_vm.FHE_VERIFY_ZERO _ -> 0x36 | Contract_vm.FHE_VERIFY_RANGE _ -> 0x37
  | Contract_vm.FHE_VERIFY_BOUND _ -> 0x38 | Contract_vm.FHE_COMMIT _ -> 0x39
  | Contract_vm.FHE_PEDERSEN _ -> 0x3A | Contract_vm.FHE_SER _ -> 0x3B
  | Contract_vm.FHE_DESER _ -> 0x3C   | Contract_vm.FHE_SER_PK _ -> 0x3D
  | Contract_vm.FHE_DESER_PK _ -> 0x3E
  | Contract_vm.GROTH16_VERIFY_BN254 _ -> 0x3F
  | Contract_vm.FHE_MUL _ -> 0x5B
  | Contract_vm.FHE_DIV_CONST _ -> 0x5C
  | Contract_vm.MATMUL _ -> 0x60
  | Contract_vm.VECDOT _ -> 0x61
  | Contract_vm.EXP_LUT _ -> 0x62
  | Contract_vm.EXP_Q16 _ -> 0x7C
  | Contract_vm.SOFTMAX_INPLACE _ -> 0x63
  | Contract_vm.SOFTMAX_Q16_INPLACE _ -> 0x7D
  | Contract_vm.LAYERNORM_INPLACE _ -> 0x64
  | Contract_vm.LAYERNORM_Q16_INPLACE _ -> 0x7E
  | Contract_vm.RELU_INPLACE _ -> 0x65
  | Contract_vm.RMSNORM_INPLACE _ -> 0x66
  | Contract_vm.RMSNORM_Q16_INPLACE _ -> 0x7F
  | Contract_vm.SILU_Q16_INPLACE _ -> 0x80
  | Contract_vm.ROPE_APPLY_Q16 _ -> 0x81
  | Contract_vm.ATTENTION_KV_Q16 _ -> 0x82
  | Contract_vm.VECDOT_Q16 _ -> 0x83
  | Contract_vm.ELEMWISE_MUL_Q16 _ -> 0x84
  | Contract_vm.RESIDUAL_ADD_Q16 _ -> 0x85
  | Contract_vm.LOAD_INT8_Q16 _ -> 0x86
  | Contract_vm.APPEND_VEC_Q16 _ -> 0x87
  | Contract_vm.ARGMAX_Q16 _ -> 0x88
  | Contract_vm.SILU_INPLACE _ -> 0x67
  | Contract_vm.ELEMWISE_MUL_INPLACE _ -> 0x68
  | Contract_vm.LOAD_INT8_BYTES_TO_MEM _ -> 0x69
  | Contract_vm.RESIDUAL_ADD _ -> 0x6A
  | Contract_vm.ROPE_APPLY _ -> 0x6B
  | Contract_vm.LOAD_INT8_B64_TO_MEM _ -> 0x6C
  | Contract_vm.MATMUL_Q16 _ -> 0x6D
  | Contract_vm.SHIFT_ROUND_INPLACE _ -> 0x6E
  | Contract_vm.MATMUL_FP _ -> 0x6F
  | Contract_vm.RMSNORM_FP _ -> 0x70
  | Contract_vm.SILU_FP _ -> 0x71
  | Contract_vm.ELEMWISE_MUL_FP _ -> 0x72
  | Contract_vm.RESIDUAL_ADD_FP _ -> 0x73
  | Contract_vm.ROPE_APPLY_FP _ -> 0x74
  | Contract_vm.LOAD_INT8_FP _ -> 0x75
  | Contract_vm.VECDOT_FP _ -> 0x76
  | Contract_vm.ARGMAX_FP _ -> 0x77
  | Contract_vm.ATTENTION_KV_FP _ -> 0x78
  | Contract_vm.APPEND_VEC_FP _ -> 0x79

let encode_instr buf pool instr =
  put_u8 buf (op_tag instr);
  match instr with
  | Contract_vm.ADD (d,a,b) | Contract_vm.SUB (d,a,b)
  | Contract_vm.MUL (d,a,b) | Contract_vm.DIV (d,a,b)
  | Contract_vm.MOD (d,a,b)
  | Contract_vm.EQ (d,a,b) | Contract_vm.LT (d,a,b)
  | Contract_vm.GT (d,a,b) | Contract_vm.NEQ (d,a,b)
  | Contract_vm.CONCAT (d,a,b) ->
    put_u8 buf d; put_u8 buf a; put_u8 buf b
  | Contract_vm.NEG (d,s) | Contract_vm.ABS (d,s)
  | Contract_vm.MOV (d,s) | Contract_vm.BALANCE (d,s)
  | Contract_vm.SLOADK (d,s) | Contract_vm.SSTOREK (d,s)
  | Contract_vm.SPAWN (d,s) | Contract_vm.STRLEN (d,s)
  | Contract_vm.MLOADR (d,s) | Contract_vm.MSTORER (d,s)
  | Contract_vm.ISADDR (d,s) ->
    put_u8 buf d; put_u8 buf s
  | Contract_vm.ISHEX (d,s) ->
    put_u8 buf d; put_u8 buf s
  | Contract_vm.SPAWN2 (d,s,base,n) ->
    put_u8 buf d; put_u8 buf s; put_u8 buf base; put_u8 buf n
  | Contract_vm.PARSE_INTS (d,a,b) ->
    put_u8 buf d; put_u8 buf a; put_u8 buf b
  | Contract_vm.TRANSFER (d,a,v) ->
    put_u8 buf d; put_u8 buf a; put_u8 buf v
  | Contract_vm.LDI (d,v) ->
    put_u8 buf d; put_u16le buf (Pool.intern pool (const_of_v v))
  | Contract_vm.SLOAD (d,k) ->
    put_u8 buf d; put_u16le buf (Pool.intern pool (CStr k))
  | Contract_vm.SSTORE (k,s) ->
    put_u16le buf (Pool.intern pool (CStr k)); put_u8 buf s
  | Contract_vm.SDEL k ->
    put_u16le buf (Pool.intern pool (CStr k))
  | Contract_vm.SDELK r ->
    put_u8 buf r
  | Contract_vm.MLOAD (d,idx) ->
    put_u8 buf d; put_u16le buf idx
  | Contract_vm.MSTORE (idx,s) ->
    put_u16le buf idx; put_u8 buf s
  | Contract_vm.JMP addr -> put_u32le buf addr
  | Contract_vm.JIF (r,addr) -> put_u8 buf r; put_u32le buf addr
  | Contract_vm.JDEST addr -> put_u32le buf addr
  | Contract_vm.CALLER d | Contract_vm.ORIGIN d | Contract_vm.SELF d
  | Contract_vm.EPOCH d | Contract_vm.EPOCH_TIME d | Contract_vm.VALUE d | Contract_vm.TREEHASH d
  | Contract_vm.NODEID d | Contract_vm.TXHASH d | Contract_vm.EFFORT d | Contract_vm.ASSERT d
  | Contract_vm.ASSERT_ADDR d ->
    put_u8 buf d
  | Contract_vm.XCALL (d,t,m,a,n) ->
    put_u8 buf d; put_u8 buf t; put_u8 buf m; put_u8 buf a; put_u8 buf n
  | Contract_vm.EMIT (name, regs) ->
    put_u16le buf (Pool.intern pool (CStr name));
    put_u8 buf (List.length regs);
    List.iter (put_u8 buf) regs
  | Contract_vm.FHE_ADD (d,pk,a,b) | Contract_vm.FHE_SUB (d,pk,a,b)
  | Contract_vm.FHE_MUL (d,pk,a,b)
  | Contract_vm.FHE_SCALE (d,pk,a,b) | Contract_vm.FHE_DIV_CONST (d,pk,a,b)
  | Contract_vm.FHE_ADD_CONST (d,pk,a,b)
  | Contract_vm.FHE_SUB_CONST (d,pk,a,b)
  | Contract_vm.FHE_VERIFY_ZERO (d,pk,a,b) | Contract_vm.FHE_VERIFY_RANGE (d,pk,a,b)
  | Contract_vm.GROTH16_VERIFY_BN254 (d,pk,a,b)
  | Contract_vm.ED25519_OK (d,pk,a,b) ->
    put_u8 buf d; put_u8 buf pk; put_u8 buf a; put_u8 buf b
  | Contract_vm.FHE_VERIFY_BOUND (d,pk,ct,pf,cm) ->
    put_u8 buf d; put_u8 buf pk; put_u8 buf ct; put_u8 buf pf; put_u8 buf cm
  | Contract_vm.FHE_COMMIT (d,pk,ct) | Contract_vm.FHE_PEDERSEN (d,pk,ct) ->
    put_u8 buf d; put_u8 buf pk; put_u8 buf ct
  | Contract_vm.FHE_LOAD_PK (d,s) | Contract_vm.FHE_SER (d,s)
  | Contract_vm.FHE_DESER (d,s) | Contract_vm.FHE_SER_PK (d,s)
  | Contract_vm.FHE_DESER_PK (d,s) ->
    put_u8 buf d; put_u8 buf s
  | Contract_vm.CALL_INT (d, addr) ->
    put_u8 buf d; put_u32le buf addr
  | Contract_vm.SUBSTR (d,s,start,len) ->
    put_u8 buf d; put_u8 buf s; put_u8 buf start; put_u8 buf len
  | Contract_vm.INDEXOF (d,s,search) ->
    put_u8 buf d; put_u8 buf s; put_u8 buf search
  | Contract_vm.SHA256 (d,s) | Contract_vm.KECCAK256 (d,s)
  | Contract_vm.STATE_PATH_KEY (d,s) ->
    put_u8 buf d; put_u8 buf s
  | Contract_vm.OBJECT_MEMBER_COUNT (d,s) ->
    put_u8 buf d; put_u8 buf s
  | Contract_vm.OBJECT_HAS_MEMBER (d,a,b)
  | Contract_vm.OBJECT_MEMBER_REF_AT (d,a,b) ->
    put_u8 buf d; put_u8 buf a; put_u8 buf b
  | Contract_vm.OBJECT_TRANSITION_APPLY (d,a,b,c,e,f,g,h,i,j,k) ->
    put_u8 buf d;
    put_u8 buf a;
    put_u8 buf b;
    put_u8 buf c;
    put_u8 buf e;
    put_u8 buf f;
    put_u8 buf g;
    put_u8 buf h;
    put_u8 buf i;
    put_u8 buf j;
    put_u8 buf k
  | Contract_vm.BITAND (d,a,b) | Contract_vm.BITOR (d,a,b)
  | Contract_vm.BITXOR (d,a,b) | Contract_vm.BITSHL (d,a,b)
  | Contract_vm.BITSHR (d,a,b) ->
    put_u8 buf d; put_u8 buf a; put_u8 buf b
  | Contract_vm.SKEYS (d,prefix,base) ->
    put_u8 buf d; put_u8 buf prefix; put_u8 buf base
  | Contract_vm.SKEYS_PAGE (d,n,prefix,after,base) ->
    put_u8 buf d; put_u8 buf n; put_u8 buf prefix; put_u8 buf after; put_u8 buf base
  | Contract_vm.SLOADN (a,b,c) | Contract_vm.SSTOREN (a,b,c) ->
    put_u8 buf a; put_u8 buf b; put_u8 buf c
  | Contract_vm.FSTORE (d,s) | Contract_vm.FLOAD (d,s) ->
    put_u8 buf d; put_u8 buf s
  | Contract_vm.MATMUL (d,l,r,m,k,n) ->
    put_u8 buf d; put_u8 buf l; put_u8 buf r; put_u8 buf m; put_u8 buf k; put_u8 buf n
  | Contract_vm.VECDOT (d,a,b,n) ->
    put_u8 buf d; put_u8 buf a; put_u8 buf b; put_u8 buf n
  | Contract_vm.EXP_LUT (d,x) ->
    put_u8 buf d; put_u8 buf x
  | Contract_vm.EXP_Q16 (d,x) ->
    put_u8 buf d; put_u8 buf x
  | Contract_vm.SOFTMAX_INPLACE (a,n) ->
    put_u8 buf a; put_u8 buf n
  | Contract_vm.SOFTMAX_Q16_INPLACE (a,n) ->
    put_u8 buf a; put_u8 buf n
  | Contract_vm.LAYERNORM_INPLACE (a,n,g,b) ->
    put_u8 buf a; put_u8 buf n; put_u8 buf g; put_u8 buf b
  | Contract_vm.LAYERNORM_Q16_INPLACE (a,n,g,b) ->
    put_u8 buf a; put_u8 buf n; put_u8 buf g; put_u8 buf b
  | Contract_vm.RELU_INPLACE (a,n) ->
    put_u8 buf a; put_u8 buf n
  | Contract_vm.RMSNORM_INPLACE (a,n,g) ->
    put_u8 buf a; put_u8 buf n; put_u8 buf g
  | Contract_vm.RMSNORM_Q16_INPLACE (a,n,g) ->
    put_u8 buf a; put_u8 buf n; put_u8 buf g
  | Contract_vm.SILU_Q16_INPLACE (a,n) ->
    put_u8 buf a; put_u8 buf n
  | Contract_vm.ROPE_APPLY_Q16 (a,n,p,b) ->
    put_u8 buf a; put_u8 buf n; put_u8 buf p; put_u8 buf b
  | Contract_vm.ATTENTION_KV_Q16 (q,k,v,c,t,nq,nk,hd) ->
    put_u8 buf q; put_u8 buf k; put_u8 buf v; put_u8 buf c;
    put_u8 buf t; put_u8 buf nq; put_u8 buf nk; put_u8 buf hd
  | Contract_vm.VECDOT_Q16 (d,a,b,n) ->
    put_u8 buf d; put_u8 buf a; put_u8 buf b; put_u8 buf n
  | Contract_vm.ELEMWISE_MUL_Q16 (d,s,n) | Contract_vm.RESIDUAL_ADD_Q16 (d,s,n) ->
    put_u8 buf d; put_u8 buf s; put_u8 buf n
  | Contract_vm.LOAD_INT8_Q16 (d,s,o,n,sc) ->
    put_u8 buf d; put_u8 buf s; put_u8 buf o; put_u8 buf n; put_u8 buf sc
  | Contract_vm.APPEND_VEC_Q16 (d,p,s,n) ->
    put_u8 buf d; put_u8 buf p; put_u8 buf s; put_u8 buf n
  | Contract_vm.ARGMAX_Q16 (d,a,n) ->
    put_u8 buf d; put_u8 buf a; put_u8 buf n
  | Contract_vm.SILU_INPLACE (a,n) ->
    put_u8 buf a; put_u8 buf n
  | Contract_vm.ELEMWISE_MUL_INPLACE (d,s,n) ->
    put_u8 buf d; put_u8 buf s; put_u8 buf n
  | Contract_vm.LOAD_INT8_BYTES_TO_MEM (d,s,o,n,sc) ->
    put_u8 buf d; put_u8 buf s; put_u8 buf o; put_u8 buf n; put_u8 buf sc
  | Contract_vm.RESIDUAL_ADD (d,s,n) ->
    put_u8 buf d; put_u8 buf s; put_u8 buf n
  | Contract_vm.ROPE_APPLY (a,n,p,b) ->
    put_u8 buf a; put_u8 buf n; put_u8 buf p; put_u8 buf b
  | Contract_vm.LOAD_INT8_B64_TO_MEM (d,s,o,n,sc) ->
    put_u8 buf d; put_u8 buf s; put_u8 buf o; put_u8 buf n; put_u8 buf sc
  | Contract_vm.MATMUL_Q16 (d,l,r,m,k,n) ->
    put_u8 buf d; put_u8 buf l; put_u8 buf r; put_u8 buf m; put_u8 buf k; put_u8 buf n
  | Contract_vm.SHIFT_ROUND_INPLACE (a,n,b) ->
    put_u8 buf a; put_u8 buf n; put_u8 buf b
  | Contract_vm.MATMUL_FP (d,l,r,m,k,n) ->
    put_u8 buf d; put_u8 buf l; put_u8 buf r; put_u8 buf m; put_u8 buf k; put_u8 buf n
  | Contract_vm.RMSNORM_FP (a,n,g) ->
    put_u8 buf a; put_u8 buf n; put_u8 buf g
  | Contract_vm.SILU_FP (a,n) ->
    put_u8 buf a; put_u8 buf n
  | Contract_vm.ELEMWISE_MUL_FP (d,s,n) ->
    put_u8 buf d; put_u8 buf s; put_u8 buf n
  | Contract_vm.RESIDUAL_ADD_FP (d,s,n) ->
    put_u8 buf d; put_u8 buf s; put_u8 buf n
  | Contract_vm.ROPE_APPLY_FP (a,n,p,b) ->
    put_u8 buf a; put_u8 buf n; put_u8 buf p; put_u8 buf b
  | Contract_vm.LOAD_INT8_FP (d,s,o,n,sc) ->
    put_u8 buf d; put_u8 buf s; put_u8 buf o; put_u8 buf n; put_u8 buf sc
  | Contract_vm.VECDOT_FP (d,a,b,n) ->
    put_u8 buf d; put_u8 buf a; put_u8 buf b; put_u8 buf n
  | Contract_vm.ARGMAX_FP (d,a,n) ->
    put_u8 buf d; put_u8 buf a; put_u8 buf n
  | Contract_vm.ATTENTION_KV_FP (q,k,v,c,t,nq,nk,hd) ->
    put_u8 buf q; put_u8 buf k; put_u8 buf v; put_u8 buf c;
    put_u8 buf t; put_u8 buf nq; put_u8 buf nk; put_u8 buf hd
  | Contract_vm.APPEND_VEC_FP (d,p,s,n) ->
    put_u8 buf d; put_u8 buf p; put_u8 buf s; put_u8 buf n
  | Contract_vm.STOP | Contract_vm.REVERT
  | Contract_vm.CHECKPOINT | Contract_vm.ROLLBACK
  | Contract_vm.COMMIT | Contract_vm.NOP -> ()

let encode instrs =
  let pool = Pool.create () in
  let instr_buf = Buffer.create 1024 in
  Array.iter (encode_instr instr_buf pool) instrs;
  let consts = Pool.to_list pool in
  let buf = Buffer.create 2048 in
  Buffer.add_string buf magic;
  put_u16le buf version;
  put_u16le buf (List.length consts);
  put_u32le buf (Array.length instrs);
  List.iter (fun c ->
    put_u8 buf (const_tag c);
    let d = const_data c in
    put_u32le buf (String.length d);
    Buffer.add_string buf d
  ) consts;
  Buffer.add_buffer buf instr_buf;
  Buffer.contents buf

let max_consts = 32768
let max_instrs = 1_048_576
let max_const_len = 16_777_216
let max_octb_bytes = 67_108_864

let decode_const s pos total_len =
  if pos + 5 > total_len then failwith "OCTB truncated constant header";
  let tag = get_u8 s pos in
  let len = get_u32le s (pos + 1) in
  if len > max_const_len then failwith (Printf.sprintf "OCTB constant too large: %d" len);
  if pos + 5 + len > total_len then failwith "OCTB truncated constant data";
  let data = Bytes.sub_string s (pos + 5) len in
  let c = match tag with
    | 0 -> CInt data | 1 -> CBool (data = "1")
    | 2 -> CStr data | 3 -> CBytes data
    | 4 -> CAddr data | _ -> failwith "bad const tag"
  in
  (c, pos + 5 + len)

let decode_instr s pos consts =
  let tag = get_u8 s pos in
  let p = pos + 1 in
  match tag with
  | 0x00 -> (Contract_vm.ADD (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x01 -> (Contract_vm.SUB (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x02 -> (Contract_vm.MUL (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x03 -> (Contract_vm.DIV (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x04 -> (Contract_vm.MOD (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x05 -> (Contract_vm.NEG (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x06 -> (Contract_vm.ABS (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x07 -> (Contract_vm.EQ (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x08 -> (Contract_vm.LT (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x09 -> (Contract_vm.GT (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x0A -> (Contract_vm.NEQ (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x0B ->
    let d = get_u8 s p in
    let ci = get_u16le s (p+1) in
    (Contract_vm.LDI (d, v_of_const consts.(ci)), p+3)
  | 0x0C -> (Contract_vm.MOV (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x0D ->
    let d = get_u8 s p in
    let ci = get_u16le s (p+1) in
    let k = match consts.(ci) with CStr s -> s | _ -> "" in
    (Contract_vm.SLOAD (d, k), p+3)
  | 0x0E ->
    let ci = get_u16le s p in
    let r = get_u8 s (p+2) in
    let k = match consts.(ci) with CStr s -> s | _ -> "" in
    (Contract_vm.SSTORE (k, r), p+3)
  | 0x0F ->
    let ci = get_u16le s p in
    let k = match consts.(ci) with CStr s -> s | _ -> "" in
    (Contract_vm.SDEL k, p+2)
  | 0x10 -> (Contract_vm.SLOADK (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x11 -> (Contract_vm.SSTOREK (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x54 -> (Contract_vm.SDELK (get_u8 s p), p+1)
  | 0x12 -> (Contract_vm.MLOAD (get_u8 s p, get_u16le s (p+1)), p+3)
  | 0x13 -> (Contract_vm.MSTORE (get_u16le s p, get_u8 s (p+2)), p+3)
  | 0x14 -> (Contract_vm.JMP (get_u32le s p), p+4)
  | 0x15 -> (Contract_vm.JIF (get_u8 s p, get_u32le s (p+1)), p+5)
  | 0x16 -> (Contract_vm.JDEST (get_u32le s p), p+4)
  | 0x17 -> (Contract_vm.STOP, p)
  | 0x18 -> (Contract_vm.REVERT, p)
  | 0x19 -> (Contract_vm.CALLER (get_u8 s p), p+1)
  | 0x1A -> (Contract_vm.ORIGIN (get_u8 s p), p+1)
  | 0x1B -> (Contract_vm.SELF (get_u8 s p), p+1)
  | 0x1C -> (Contract_vm.EPOCH (get_u8 s p), p+1)
  | 0x1D -> (Contract_vm.VALUE (get_u8 s p), p+1)
  | 0x7B -> (Contract_vm.EPOCH_TIME (get_u8 s p), p+1)
  | 0x1E -> (Contract_vm.BALANCE (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x1F -> (Contract_vm.TREEHASH (get_u8 s p), p+1)
  | 0x20 -> (Contract_vm.NODEID (get_u8 s p), p+1)
  | 0x21 ->
    (Contract_vm.XCALL (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3), get_u8 s (p+4)), p+5)
  | 0x22 -> (Contract_vm.SPAWN (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x23 -> (Contract_vm.TRANSFER (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x24 -> (Contract_vm.CHECKPOINT, p)
  | 0x25 -> (Contract_vm.ROLLBACK, p)
  | 0x26 -> (Contract_vm.COMMIT, p)
  | 0x27 ->
    let ci = get_u16le s p in
    let name = match consts.(ci) with CStr s -> s | _ -> "" in
    let nregs = get_u8 s (p+2) in
    let regs = List.init nregs (fun i -> get_u8 s (p+3+i)) in
    (Contract_vm.EMIT (name, regs), p+3+nregs)
  | 0x28 -> (Contract_vm.CONCAT (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x29 -> (Contract_vm.ASSERT (get_u8 s p), p+1)
  | 0x2A -> (Contract_vm.EFFORT (get_u8 s p), p+1)
  | 0x2B -> (Contract_vm.NOP, p)
  | 0x2C -> (Contract_vm.STRLEN (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x2D -> (Contract_vm.CALL_INT (get_u8 s p, get_u32le s (p+1)), p+5)
  | 0x2E -> (Contract_vm.MLOADR (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x2F -> (Contract_vm.MSTORER (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x30 -> (Contract_vm.FHE_LOAD_PK (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x31 -> (Contract_vm.FHE_ADD (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x32 -> (Contract_vm.FHE_SUB (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x33 -> (Contract_vm.FHE_SCALE (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x34 -> (Contract_vm.FHE_ADD_CONST (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x35 -> (Contract_vm.FHE_SUB_CONST (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x36 -> (Contract_vm.FHE_VERIFY_ZERO (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x37 -> (Contract_vm.FHE_VERIFY_RANGE (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x38 -> (Contract_vm.FHE_VERIFY_BOUND (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3), get_u8 s (p+4)), p+5)
  | 0x39 -> (Contract_vm.FHE_COMMIT (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x3A -> (Contract_vm.FHE_PEDERSEN (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x3B -> (Contract_vm.FHE_SER (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x3C -> (Contract_vm.FHE_DESER (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x3D -> (Contract_vm.FHE_SER_PK (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x3E -> (Contract_vm.FHE_DESER_PK (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x3F -> (Contract_vm.GROTH16_VERIFY_BN254 (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x5B -> (Contract_vm.FHE_MUL (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x5C -> (Contract_vm.FHE_DIV_CONST (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x40 -> (Contract_vm.PARSE_INTS (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x41 -> (Contract_vm.ISADDR (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x56 -> (Contract_vm.STATE_PATH_KEY (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x57 ->
    ( Contract_vm.OBJECT_TRANSITION_APPLY
        ( get_u8 s p,
          get_u8 s (p+1),
          get_u8 s (p+2),
          get_u8 s (p+3),
          get_u8 s (p+4),
          get_u8 s (p+5),
          get_u8 s (p+6),
          get_u8 s (p+7),
          get_u8 s (p+8),
          get_u8 s (p+9),
          get_u8 s (p+10) ),
      p+11 )
  | 0x58 -> (Contract_vm.OBJECT_MEMBER_COUNT (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x59 ->
    ( Contract_vm.OBJECT_HAS_MEMBER
        (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)),
      p+3 )
  | 0x5A ->
    ( Contract_vm.OBJECT_MEMBER_REF_AT
        (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)),
      p+3 )
  | 0x52 -> (Contract_vm.ISHEX (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x53 -> (Contract_vm.SPAWN2 (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x42 -> (Contract_vm.ASSERT_ADDR (get_u8 s p), p+1)
  | 0x43 -> (Contract_vm.SUBSTR (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x44 -> (Contract_vm.INDEXOF (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x45 -> (Contract_vm.SHA256 (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x46 -> (Contract_vm.KECCAK256 (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x55 -> (Contract_vm.ED25519_OK (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x47 -> (Contract_vm.BITAND (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x48 -> (Contract_vm.BITOR (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x49 -> (Contract_vm.BITXOR (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x4A -> (Contract_vm.BITSHL (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x4B -> (Contract_vm.BITSHR (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x4C -> (Contract_vm.SKEYS (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x4D -> (Contract_vm.SLOADN (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x4E -> (Contract_vm.SSTOREN (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x4F -> (Contract_vm.FSTORE (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x50 -> (Contract_vm.FLOAD (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x51 -> (Contract_vm.SKEYS_PAGE (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3), get_u8 s (p+4)), p+5)
  | 0x60 -> (Contract_vm.MATMUL (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3), get_u8 s (p+4), get_u8 s (p+5)), p+6)
  | 0x61 -> (Contract_vm.VECDOT (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x62 -> (Contract_vm.EXP_LUT (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x7C -> (Contract_vm.EXP_Q16 (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x63 -> (Contract_vm.SOFTMAX_INPLACE (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x7D -> (Contract_vm.SOFTMAX_Q16_INPLACE (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x64 -> (Contract_vm.LAYERNORM_INPLACE (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x7E -> (Contract_vm.LAYERNORM_Q16_INPLACE (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x65 -> (Contract_vm.RELU_INPLACE (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x66 -> (Contract_vm.RMSNORM_INPLACE (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x7F -> (Contract_vm.RMSNORM_Q16_INPLACE (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x80 -> (Contract_vm.SILU_Q16_INPLACE (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x81 -> (Contract_vm.ROPE_APPLY_Q16 (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x82 -> (Contract_vm.ATTENTION_KV_Q16 (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3), get_u8 s (p+4), get_u8 s (p+5), get_u8 s (p+6), get_u8 s (p+7)), p+8)
  | 0x83 -> (Contract_vm.VECDOT_Q16 (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x84 -> (Contract_vm.ELEMWISE_MUL_Q16 (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x85 -> (Contract_vm.RESIDUAL_ADD_Q16 (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x86 -> (Contract_vm.LOAD_INT8_Q16 (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3), get_u8 s (p+4)), p+5)
  | 0x87 -> (Contract_vm.APPEND_VEC_Q16 (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x88 -> (Contract_vm.ARGMAX_Q16 (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x67 -> (Contract_vm.SILU_INPLACE (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x68 -> (Contract_vm.ELEMWISE_MUL_INPLACE (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x69 -> (Contract_vm.LOAD_INT8_BYTES_TO_MEM (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3), get_u8 s (p+4)), p+5)
  | 0x6A -> (Contract_vm.RESIDUAL_ADD (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x6B -> (Contract_vm.ROPE_APPLY (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x6C -> (Contract_vm.LOAD_INT8_B64_TO_MEM (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3), get_u8 s (p+4)), p+5)
  | 0x6D -> (Contract_vm.MATMUL_Q16 (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3), get_u8 s (p+4), get_u8 s (p+5)), p+6)
  | 0x6E -> (Contract_vm.SHIFT_ROUND_INPLACE (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x6F -> (Contract_vm.MATMUL_FP (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3), get_u8 s (p+4), get_u8 s (p+5)), p+6)
  | 0x70 -> (Contract_vm.RMSNORM_FP (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x71 -> (Contract_vm.SILU_FP (get_u8 s p, get_u8 s (p+1)), p+2)
  | 0x72 -> (Contract_vm.ELEMWISE_MUL_FP (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x73 -> (Contract_vm.RESIDUAL_ADD_FP (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x74 -> (Contract_vm.ROPE_APPLY_FP (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x75 -> (Contract_vm.LOAD_INT8_FP (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3), get_u8 s (p+4)), p+5)
  | 0x76 -> (Contract_vm.VECDOT_FP (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x77 -> (Contract_vm.ARGMAX_FP (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2)), p+3)
  | 0x78 -> (Contract_vm.ATTENTION_KV_FP (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3), get_u8 s (p+4), get_u8 s (p+5), get_u8 s (p+6), get_u8 s (p+7)), p+8)
  | 0x79 -> (Contract_vm.APPEND_VEC_FP (get_u8 s p, get_u8 s (p+1), get_u8 s (p+2), get_u8 s (p+3)), p+4)
  | 0x7A -> (Contract_vm.TXHASH (get_u8 s p), p+1)
  | _ -> failwith (Printf.sprintf "unknown opcode 0x%02x at %d" tag pos)

let trim_error msg =
  if String.length msg <= 256 then msg
  else String.sub msg 0 256

let decode raw =
  try
    let s = Bytes.of_string raw in
    let len = Bytes.length s in
    if len > max_octb_bytes then failwith (Printf.sprintf "OCTB too large: %d bytes (max %d)" len max_octb_bytes);
    if len < 12 then failwith "OCTB too short";
    let m = Bytes.sub_string s 0 4 in
    if m <> magic then failwith "bad OCTB magic";
    let ver = get_u16le s 4 in
    if ver <> version then failwith (Printf.sprintf "unsupported OCTB version: %d" ver);
    let n_consts = get_u16le s 6 in
    let n_instrs = get_u32le s 8 in
    if n_consts > max_consts then failwith (Printf.sprintf "OCTB too many constants: %d" n_consts);
    if n_instrs > max_instrs then failwith (Printf.sprintf "OCTB too many instructions: %d" n_instrs);
    let pos = ref 12 in
    let consts = Array.init n_consts (fun _ ->
      let (c, next) = decode_const s !pos len in
      pos := next; c
    ) in
    let code = Array.init n_instrs (fun _ ->
      if !pos >= len then failwith "OCTB truncated instruction stream";
      let (instr, next) = decode_instr s !pos consts in
      pos := next; instr
    ) in
    if !pos <> len then failwith "OCTB trailing bytes";
    Ok code
  with Failure msg -> Error (trim_error msg)
    | exn -> Error (trim_error (Printexc.to_string exn))

let decode_exn raw =
  match decode raw with
  | Ok code -> code
  | Error msg -> failwith msg