// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#include <stdint.h>
#include <sys/statvfs.h>

CAMLprim value octra_disk_free(value path)
{
  CAMLparam1(path);
  CAMLlocal1(result);
  struct statvfs info;

  if (statvfs(String_val(path), &info) != 0) {
    uerror("statvfs", path);
  }

  uint64_t blocks = (uint64_t)info.f_bavail;
  uint64_t width = (uint64_t)info.f_frsize;
  uint64_t bytes =
    width != 0 && blocks > INT64_MAX / width
      ? INT64_MAX
      : blocks * width;

  result = caml_copy_int64((int64_t)bytes);
  CAMLreturn(result);
}