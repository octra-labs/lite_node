// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <errno.h>

CAMLprim value octra_accept_codes(value unit)
{
  CAMLparam1(unit);
  CAMLlocal2(result, entry);
  result = Val_emptylist;
#ifdef EPROTO
  entry = caml_alloc_small(2, 0);
  Field(entry, 0) = Val_int(EPROTO);
  Field(entry, 1) = result;
  result = entry;
#endif
#ifdef ENONET
  entry = caml_alloc_small(2, 0);
  Field(entry, 0) = Val_int(ENONET);
  Field(entry, 1) = result;
  result = entry;
#endif
  CAMLreturn(result);
}