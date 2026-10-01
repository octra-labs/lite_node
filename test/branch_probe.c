// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#if defined(__linux__) && !defined(_GNU_SOURCE)
#define _GNU_SOURCE
#endif

#include <caml/mlvalues.h>
#include <dlfcn.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static int descriptor_path(int fd, char *path, size_t size) {
#if defined(__APPLE__)
  (void)size;
  return fcntl(fd, F_GETPATH, path);
#elif defined(__linux__)
  char link[64];
  snprintf(link, sizeof(link), "/proc/self/fd/%d", fd);
  ssize_t length = readlink(link, path, size - 1);
  if (length < 0) return -1;
  path[length] = '\0';
  return 0;
#else
#error unsupported descriptor inspection
#endif
}

CAMLprim value octra_sync_count(value unused) {
  (void)unused;
  const char *count = getenv("OCTRA_TEST_SYNC_COUNT");
  return Val_int(count == NULL ? 0 : atoi(count));
}

int fsync(int fd) {
  int (*sync_real)(int) = dlsym(RTLD_NEXT, "fsync");
  if (sync_real == NULL) {
    errno = ENOSYS;
    return -1;
  }
  char phase[32];
  char target[32];
  const char *value = getenv("OCTRA_TEST_SYNC_PHASE");
  snprintf(phase, sizeof(phase), "%s", value == NULL ? "" : value);
  value = getenv("OCTRA_TEST_SYNC_TARGET");
  snprintf(target, sizeof(target), "%s", value == NULL ? "" : value);
  char path[PATH_MAX];
  if (phase[0] != '\0' && target[0] != '\0'
      && descriptor_path(fd, path, sizeof(path)) == 0) {
    const char *name = strrchr(path, '/');
    const char *expected = strcmp(target, "directory") == 0 ? "irmin_store" : "store.branches";
    if (name != NULL && strcmp(name + 1, expected) == 0) {
      const char *prior = getenv("OCTRA_TEST_SYNC_COUNT");
      char count[32];
      snprintf(count, sizeof(count), "%d", prior == NULL ? 1 : atoi(prior) + 1);
      setenv("OCTRA_TEST_SYNC_COUNT", count, 1);
      fprintf(stderr, "event = branch_sync phase = %s target = %s count = %s\n", phase, target, count);
      if (strcmp(phase, "error") == 0) {
        errno = EIO;
        return -1;
      }
      if (strcmp(phase, "kill_before") == 0) {
        kill(getpid(), SIGKILL);
        _exit(99);
      }
      if (strcmp(phase, "error_after") == 0 || strcmp(phase, "kill_after") == 0) {
        int result = sync_real(fd);
        if (result != 0) return result;
        if (strcmp(phase, "kill_after") == 0) {
          kill(getpid(), SIGKILL);
          _exit(99);
        }
        errno = EIO;
        return -1;
      }
    }
  }
  return sync_real(fd);
}