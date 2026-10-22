(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  data_dir : string;
  data_id : int * int;
  chain_id : int * int;
  process : int;
  mutable open_ : bool;
  mutable claimed : bool;
}

let identity path =
  let stat = Unix.lstat path in
  if stat.Unix.st_kind <> Unix.S_DIR then invalid_arg "store path is not a directory";
  stat.Unix.st_dev, stat.Unix.st_ino

let capture chain_dir =
  try
    let data_dir = Filename.dirname chain_dir in
    let data_id = identity data_dir in
    let chain_id = identity chain_dir in
    let permitted = function
      | "chaindata" -> Sys.readdir chain_dir = [||]
      | "wallet.json" ->
        let kind = (Unix.lstat (Filename.concat data_dir "wallet.json")).Unix.st_kind in
        kind = Unix.S_REG || kind = Unix.S_LNK
      | _ -> false
    in
    if Filename.basename chain_dir <> "chaindata"
       || not (Array.for_all permitted (Sys.readdir data_dir)) then None
    else Some {data_dir = Unix.realpath data_dir; data_id; chain_id; process = Unix.getpid ();
      open_ = true; claimed = false}
  with Unix.Unix_error _ | Sys_error _ | Invalid_argument _ -> None

let close value = value.open_ <- false

let active value ~data_dir =
  value.open_ && value.process = Unix.getpid ()
  && try
    value.data_dir = Unix.realpath data_dir && identity data_dir = value.data_id
    && identity (Filename.concat data_dir "chaindata") = value.chain_id
  with Unix.Unix_error _ | Sys_error _ | Invalid_argument _ -> false

let claim value ~data_dir =
  let accepted = not value.claimed && active value ~data_dir in
  value.claimed <- true;
  accepted