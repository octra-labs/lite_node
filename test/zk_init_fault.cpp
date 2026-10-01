// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <mcl/bn.h>

static int failed_init(int, int) { return -1; }

#define mclBn_init failed_init
#define caml_zk_initialize caml_zk_failed_initialize
#define caml_zk_groth16_verify_bn254 caml_zk_failed_verify
#include "zk_stubs.cpp"