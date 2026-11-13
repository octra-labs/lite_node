// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#if defined(__linux__) && !defined(_GNU_SOURCE)
#define _GNU_SOURCE
#endif

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdint.h>
#include <string.h>
#include <sys/statvfs.h>
#include <unistd.h>

#ifdef __linux__
#include <sys/prctl.h>
#include <sys/syscall.h>
#endif

static void octra_pack_close(int fd)
{
  int result;
  do {
    result = close(fd);
  } while (result < 0 && errno == EINTR);
  if (result < 0 && errno != EBADF) uerror("close", Nothing);
}

CAMLprim value octra_pack_child_fd(value output, value parent, value timeout)
{
  CAMLparam3(output, parent, timeout);
  intnat file = Long_val(output);
  intnat owner = Long_val(parent);
  intnat seconds = Long_val(timeout);
  if (file < 0 || file > INT_MAX || owner <= 0 || owner > INT_MAX ||
    seconds <= 0 || seconds > INT_MAX || getpid() == owner) {
    caml_invalid_argument("GC child isolation arguments are invalid");
  }

  if (signal(SIGALRM, SIG_DFL) == SIG_ERR) uerror("signal", Nothing);
  sigset_t signals;
  if (sigemptyset(&signals) != 0 || sigaddset(&signals, SIGALRM) != 0 ||
    sigprocmask(SIG_UNBLOCK, &signals, NULL) != 0) {
    uerror("sigprocmask", Nothing);
  }
  alarm((unsigned int)seconds);
#ifdef __linux__
  if (prctl(PR_SET_PDEATHSIG, SIGKILL, 0UL, 0UL, 0UL) != 0) {
    uerror("prctl", Nothing);
  }
#endif
  if (getppid() != owner) caml_failwith("GC child parent changed");

  int result;
  do {
    result = dup2((int)file, 3);
  } while (result < 0 && errno == EINTR);
  if (result < 0) uerror("dup2", Nothing);
  do {
    result = fcntl(3, F_SETFD, FD_CLOEXEC);
  } while (result < 0 && errno == EINTR);
  if (result < 0) uerror("fcntl", Nothing);
  if (file != 3) octra_pack_close((int)file);
  for (int fd = 0; fd < 3; ++fd) octra_pack_close(fd);

  int closed = 0;
#if defined(__linux__) && defined(SYS_close_range)
  closed = syscall(SYS_close_range, 4U, UINT_MAX, 0U) == 0;
#endif
  if (!closed) {
#ifdef __linux__
    DIR *directory = opendir("/proc/self/fd");
#else
    DIR *directory = opendir("/dev/fd");
#endif
    if (directory == NULL) uerror("opendir", Nothing);
    int maximum = 3;
    int error = 0;
    for (;;) {
      errno = 0;
      struct dirent *entry = readdir(directory);
      if (entry == NULL) {
        error = errno;
        break;
      }
      const char *name = entry->d_name;
      if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0) continue;
      int fd = 0;
      if (*name == '\0') error = EINVAL;
      for (; *name != '\0' && error == 0; ++name) {
        if (*name < '0' || *name > '9') {
          error = EINVAL;
        } else {
          int digit = *name - '0';
          if (fd > (INT_MAX - digit) / 10) error = ERANGE;
          else fd = fd * 10 + digit;
        }
      }
      if (error != 0) break;
      if (fd > maximum) maximum = fd;
    }
    if (closedir(directory) != 0 && error == 0) error = errno;
    if (error != 0) {
      errno = error;
      uerror("GC child descriptor listing", Nothing);
    }
    for (int fd = maximum; fd >= 4; --fd) octra_pack_close(fd);
  }
  if (getppid() != owner) caml_failwith("GC child parent changed");
  CAMLreturn(Val_int(3));
}

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