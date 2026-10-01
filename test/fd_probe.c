// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <caml/mlvalues.h>

CAMLprim value octra_test_fd(value descriptor)
{
  return descriptor;
}