(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type abi_type = Bytes | Text | Credits | Flag | Account

type arg = abi_type

type fn_sig = {
  inputs : abi_type list;
  outputs : abi_type list;
  effort : Z.t;
  pure : bool;
  payable : bool;
}

type event_field = string * abi_type

type ev_sig = event_field list

type abi = {
  fns : (string * fn_sig) list;
  events : (string * ev_sig) list;
}

type token_info = { name : string; symbol : string; decimals : int; supply : Z.t }

let sig_ ?(pure = false) ?(payable = false) name inputs outputs effort =
  name, {
    inputs;
    outputs;
    effort = Z.of_int effort;
    pure;
    payable;
  }

let view name inputs outputs effort =
  sig_ ~pure:true name inputs outputs effort

let write name inputs outputs effort =
  sig_ name inputs outputs effort

let event name fields =
  name, fields

let ocs01_abi : abi =
  { fns = [
      view "name" [] [Text] 8;
      view "symbol" [] [Text] 8;
      view "total_supply" [] [Credits] 8;
      view "decimals" [] [Credits] 8;
      view "balance_of" [Account] [Credits] 16;
      view "limit" [Account; Account] [Credits] 16;
      write "transfer" [Account; Credits] [Flag] 45;
      write "grant" [Account; Credits] [Flag] 35;
      write "pull" [Account; Account; Credits] [Flag] 60;
      write "mint" [Account; Credits] [Flag] 50;
      write "burn" [Credits] [Flag] 40;
    ];
    events = [
      event "Transfer" ["from", Account; "to", Account; "amount", Credits];
      event "Grant" ["owner", Account; "spender", Account; "amount", Credits];
      event "Mint" ["to", Account; "amount", Credits];
      event "Burn" ["from", Account; "amount", Credits];
    ] }