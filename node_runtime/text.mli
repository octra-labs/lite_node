(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val addr_short : string -> string

val prefix_or_unknown : int -> string -> string

val hash_short : string -> string

val peer_short : string -> string

val addr14 : string -> string

val root_short : string -> string

val msg_hex : int -> string

val frame_len : Octra_net.P2p_frame.frame -> int

val is_hex_string : string -> bool

val is_hex_len : int -> string -> bool

val hex_to_string : string -> string

val decode_message_if_hex : string -> string

val raw_to_hex : string -> string

val hash32_hex : string -> string

val hex_to_raw32_lossy : string -> string