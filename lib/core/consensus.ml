(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Validator = struct
  type t = {
    address : string;
    score : float;
    online : bool;
    last_seen : float;
  }

  let create addr score =
    { address = addr; score; online = true; last_seen = 0.0 }
end

let select_leader ?(epoch_id=0) validators =
  let online = List.filter (fun v -> v.Validator.online) validators in
  let sorted = List.sort (fun a b -> String.compare a.Validator.address b.Validator.address) online in
  match sorted with
  | [] -> failwith "no online validators"
  | _ ->
    let idx = epoch_id mod List.length sorted in
    List.nth sorted idx