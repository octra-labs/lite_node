(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type slot = { token : unit ref; host : string; net : string; grant : string option }

let limit = 136
let reserve = 8
let per_host = 4
let per_net = 16
let wait_s = 5.0

let boot = function
  | Some grant -> String.starts_with ~prefix:"boot:" grant
  | None -> false

let select grant slots =
  if boot grant && List.length (List.filter (fun slot -> boot slot.grant) slots) >= reserve
  then None else grant

let address = function
  | Unix.ADDR_UNIX path -> path, path
  | Unix.ADDR_INET (ip, _) ->
    let host = Unix.string_of_inet_addr ip in
    let net = match String.split_on_char '.' host with
      | [a; b; c; _] -> String.concat "." [a; b; c]
      | _ -> "ipv6" in
    host, net

let room ~grant ~host ~net slots =
  let known = Option.is_some grant in
  let count field value =
    List.fold_left (fun count slot ->
      if Option.is_some slot.grant = known && field slot = value then count + 1 else count) 0 slots in
  List.length slots < (if not known then limit - 2 * reserve else if boot grant then limit - reserve else limit)
  && count (fun slot -> slot.host) host < per_host
  && count (fun slot -> slot.net) net < per_net
  && (not (boot grant) || List.length (List.filter (fun slot -> boot slot.grant) slots) < reserve)
  && (not known || List.length (List.filter (fun slot -> slot.grant = grant) slots) < per_host)

let release token slots = List.filter (fun slot -> slot.token != token) slots