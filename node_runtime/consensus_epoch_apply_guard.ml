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


type prev_root_check =
  | Prev_root_ok
  | Prev_root_mismatch of {
      local_raw : string;
      expected_prev : string;
    }

let zero32 = String.make 32 '\x00'

let raw32_of_hex64 hex =
  String.init 32 (fun i ->
    Char.chr (int_of_string ("0x" ^ String.sub hex (i * 2) 2)))

let raw32_of_pre_root root =
  if String.length root >= 64 then raw32_of_hex64 root
  else if String.length root = 32 then root
  else zero32

let raw_hex8 raw =
  String.concat ""
    (List.init
       (min 8 (String.length raw))
       (fun i -> Printf.sprintf "%02x" (Char.code raw.[i])))

let check_prev_root ~local_pre_root ~expected_prev =
  if String.length expected_prev <> 32 || expected_prev = zero32 then
    Prev_root_ok
  else
    let local_raw = raw32_of_pre_root local_pre_root in
    if local_raw = expected_prev then
      Prev_root_ok
    else
      Prev_root_mismatch { local_raw; expected_prev }

let mismatch_line ~epoch_id ~local_raw ~expected_prev =
  Printf.sprintf
    "layer_a_guard = pre_root_mismatch epoch = %d local_pre = %s header_prev = %s action = exit reason = wrong_base_state"
    epoch_id
    (raw_hex8 local_raw)
    (raw_hex8 expected_prev)