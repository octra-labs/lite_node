(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  chain_id : string;
  node_id : string;
  caller : string;
  caller_public_key : string;
  job_id : string;
  epoch_id : int64;
  expires_epoch : int64;
  caller_signature : string;
}

val max_epochs : int64
val shape : t -> bool
val sign_bytes : t -> string
val verify : chain_id:string -> node_id:string -> epoch:int64 -> t -> bool