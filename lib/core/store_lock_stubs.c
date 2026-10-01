// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <sys/file.h>

CAMLprim value octra_store_lock(value descriptor)
{
  CAMLparam1(descriptor);
  int result;
  do {
    result = flock(Int_val(descriptor), LOCK_EX | LOCK_NB);
  } while (result < 0 && errno == EINTR);
  if (result < 0) uerror("flock", Nothing);
  CAMLreturn(Val_unit);
}