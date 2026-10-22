// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#include <errno.h>
#include <stdint.h>
#include <string.h>
#include <sys/statvfs.h>
#include <unistd.h>

CAMLprim value octra_disk_read_at(value file, value position, value bytes,
                                value start, value length)
{
  CAMLparam5(file, position, bytes, start, length);
  intnat offset = Long_val(start);
  intnat count = Long_val(length);
  intnat at = Long_val(position);
  char buffer[65536];

  if (offset < 0 || count < 0 || at < 0 ||
      (uintnat)offset > caml_string_length(bytes) ||
      (uintnat)count > caml_string_length(bytes) - (uintnat)offset) {
    caml_invalid_argument("disk read range");
  }
  if (count > (intnat)sizeof(buffer)) count = sizeof(buffer);
  int fd = Int_val(file);
  ssize_t got;
  int error;
  caml_enter_blocking_section();
  do {
    got = pread(fd, buffer, (size_t)count, (off_t)at);
  } while (got < 0 && errno == EINTR);
  error = errno;
  caml_leave_blocking_section();
  if (got < 0) {
    errno = error;
    uerror("pread", Nothing);
  }
  memcpy(Bytes_val(bytes) + offset, buffer, (size_t)got);
  CAMLreturn(Val_long(got));
}

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