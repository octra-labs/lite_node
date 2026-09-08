(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Ocs01_model

let constructor_code ti =
  let open Contract_vm in
  [|
    MLOAD (0, 999);
    LDI (1, VString "constructor");
    NEQ (2, 0, 1);
    JIF (2, 100);
    LDI (0, VString ti.name);
    SSTORE ("name", 0);
    LDI (0, VString ti.symbol);
    SSTORE ("symbol", 0);
    LDI (0, VInt (Z.of_int ti.decimals));
    SSTORE ("decimals", 0);
    LDI (0, VInt ti.supply);
    SSTORE ("total_supply", 0);
    ORIGIN 1;
    SSTORE ("owner", 1);
    LDI (2, VString "balances:");
    CONCAT (3, 2, 1);
    LDI (4, VInt ti.supply);
    SSTOREK (3, 4);
    EMIT ("Transfer", [1; 1; 4]);
    STOP;
  |]

let dispatch_code : Contract_vm.instr array =
  let open Contract_vm in
  [|
    JDEST 100;
    MLOAD (0, 1000);
    LDI (1, VString "name");
    EQ (2, 0, 1);
    JIF (2, 200);
    LDI (1, VString "symbol");
    EQ (2, 0, 1);
    JIF (2, 210);
    LDI (1, VString "total_supply");
    EQ (2, 0, 1);
    JIF (2, 220);
    LDI (1, VString "decimals");
    EQ (2, 0, 1);
    JIF (2, 225);
    LDI (1, VString "balance_of");
    EQ (2, 0, 1);
    JIF (2, 230);
    LDI (1, VString "limit");
    EQ (2, 0, 1);
    JIF (2, 240);
    LDI (1, VString "transfer");
    EQ (2, 0, 1);
    JIF (2, 300);
    LDI (1, VString "grant");
    EQ (2, 0, 1);
    JIF (2, 400);
    LDI (1, VString "pull");
    EQ (2, 0, 1);
    JIF (2, 500);
    LDI (1, VString "mint");
    EQ (2, 0, 1);
    JIF (2, 600);
    LDI (1, VString "burn");
    EQ (2, 0, 1);
    JIF (2, 700);
    REVERT;
  |]

let view_code : Contract_vm.instr array =
  let open Contract_vm in
  [|
    JDEST 200;
    SLOAD (0, "name");
    STOP;
    JDEST 210;
    SLOAD (0, "symbol");
    STOP;
    JDEST 220;
    SLOAD (0, "total_supply");
    STOP;
    JDEST 225;
    SLOAD (0, "decimals");
    STOP;
    JDEST 230;
    MLOAD (1, 1001);
    LDI (2, VString "balances:");
    CONCAT (3, 2, 1);
    SLOADK (0, 3);
    STOP;
    JDEST 240;
    MLOAD (1, 1001);
    MLOAD (2, 1002);
    LDI (3, VString "grants:");
    CONCAT (4, 3, 1);
    LDI (5, VString ":");
    CONCAT (4, 4, 5);
    CONCAT (4, 4, 2);
    SLOADK (0, 4);
    STOP;
  |]

let transfer_code : Contract_vm.instr array =
  let open Contract_vm in
  [|
    JDEST 300;
    MLOAD (10, 1001);
    MLOAD (11, 1002);
    LDI (12, VInt Z.zero);
    GT (13, 11, 12);
    ASSERT 13;
    LDI (14, VString "balances:");
    CALLER 15;
    CONCAT (16, 14, 15);
    SLOADK (17, 16);
    LDI (18, VInt Z.zero);
    ADD (18, 18, 17);
    LT (19, 18, 11);
    LDI (20, VBool true);
    NEQ (21, 19, 20);
    ASSERT 21;
    SUB (22, 18, 11);
    SSTOREK (16, 22);
    CONCAT (23, 14, 10);
    SLOADK (24, 23);
    LDI (25, VInt Z.zero);
    ADD (25, 25, 24);
    ADD (26, 25, 11);
    SSTOREK (23, 26);
    EMIT ("Transfer", [15; 10; 11]);
    LDI (0, VBool true);
    STOP;
  |]

let grant_code : Contract_vm.instr array =
  let open Contract_vm in
  [|
    JDEST 400;
    MLOAD (10, 1001);
    MLOAD (11, 1002);
    LDI (12, VString "grants:");
    CALLER 13;
    CONCAT (14, 12, 13);
    LDI (15, VString ":");
    CONCAT (14, 14, 15);
    CONCAT (14, 14, 10);
    SSTOREK (14, 11);
    EMIT ("Grant", [13; 10; 11]);
    LDI (0, VBool true);
    STOP;
  |]

let pull_code : Contract_vm.instr array =
  let open Contract_vm in
  [|
    JDEST 500;
    MLOAD (10, 1001);
    MLOAD (11, 1002);
    MLOAD (12, 1003);
    LDI (13, VString "grants:");
    CONCAT (14, 13, 10);
    LDI (15, VString ":");
    CONCAT (14, 14, 15);
    CALLER 16;
    CONCAT (14, 14, 16);
    SLOADK (17, 14);
    LDI (18, VInt Z.zero);
    ADD (18, 18, 17);
    LT (19, 18, 12);
    LDI (20, VBool true);
    NEQ (21, 19, 20);
    ASSERT 21;
    SUB (22, 18, 12);
    SSTOREK (14, 22);
    LDI (23, VString "balances:");
    CONCAT (24, 23, 10);
    SLOADK (25, 24);
    LDI (26, VInt Z.zero);
    ADD (26, 26, 25);
    LT (27, 26, 12);
    LDI (28, VBool true);
    NEQ (29, 27, 28);
    ASSERT 29;
    SUB (30, 26, 12);
    SSTOREK (24, 30);
    CONCAT (24, 23, 11);
    SLOADK (25, 24);
    LDI (26, VInt Z.zero);
    ADD (26, 26, 25);
    ADD (30, 26, 12);
    SSTOREK (24, 30);
    EMIT ("Transfer", [10; 11; 12]);
    LDI (0, VBool true);
    STOP;
  |]

let mint_code : Contract_vm.instr array =
  let open Contract_vm in
  [|
    JDEST 600;
    CALLER 10;
    SLOAD (11, "owner");
    EQ (12, 10, 11);
    ASSERT 12;
    MLOAD (13, 1001);
    MLOAD (14, 1002);
    SLOAD (15, "total_supply");
    LDI (16, VInt Z.zero);
    ADD (16, 16, 15);
    ADD (17, 16, 14);
    SSTORE ("total_supply", 17);
    LDI (18, VString "balances:");
    CONCAT (19, 18, 13);
    SLOADK (20, 19);
    LDI (21, VInt Z.zero);
    ADD (21, 21, 20);
    ADD (22, 21, 14);
    SSTOREK (19, 22);
    EMIT ("Mint", [13; 14]);
    LDI (0, VBool true);
    STOP;
  |]

let burn_code : Contract_vm.instr array =
  let open Contract_vm in
  [|
    JDEST 700;
    MLOAD (10, 1001);
    LDI (11, VString "balances:");
    CALLER 12;
    CONCAT (13, 11, 12);
    SLOADK (14, 13);
    LDI (15, VInt Z.zero);
    ADD (15, 15, 14);
    LT (16, 15, 10);
    LDI (17, VBool true);
    NEQ (18, 16, 17);
    ASSERT 18;
    SUB (19, 15, 10);
    SSTOREK (13, 19);
    SLOAD (20, "total_supply");
    LDI (21, VInt Z.zero);
    ADD (21, 21, 20);
    SUB (22, 21, 10);
    SSTORE ("total_supply", 22);
    EMIT ("Burn", [12; 10]);
    LDI (0, VBool true);
    STOP;
  |]

let make ti =
  Array.concat [
    constructor_code ti;
    dispatch_code;
    view_code;
    transfer_code;
    grant_code;
    pull_code;
    mint_code;
    burn_code;
  ]