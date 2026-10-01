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
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

CAMLprim value octra_root_count(value unused) {
  (void)unused;
  const char *count = getenv("OCTRA_TEST_ROOT_COUNT");
  return Val_int(count == NULL ? 0 : atoi(count));
}

CAMLprim value octra_root_syncs(value unused) {
  (void)unused;
  const char *count = getenv("OCTRA_TEST_ROOT_SYNCS");
  return Val_int(count == NULL ? 0 : atoi(count));
}

CAMLprim value octra_root_writes(value unused) {
  (void)unused;
  const char *count = getenv("OCTRA_TEST_ROOT_WRITES");
  return Val_int(count == NULL ? 0 : atoi(count));
}

CAMLprim value octra_root_renames(value unused) {
  (void)unused;
  const char *count = getenv("OCTRA_TEST_ROOT_RENAMES");
  return Val_int(count == NULL ? 0 : atoi(count));
}

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

int fsync(int fd) {
  int (*sync_real)(int) = dlsym(RTLD_NEXT, "fsync");
  if (sync_real == NULL) { errno = ENOSYS; return -1; }
  char phase[64];
  char directory[PATH_MAX];
  const char *value = getenv("OCTRA_TEST_ROOT_PHASE");
  snprintf(phase, sizeof(phase), "%s", value == NULL ? "" : value);
  value = getenv("OCTRA_TEST_ROOT_DIR");
  snprintf(directory, sizeof(directory), "%s", value == NULL ? "" : value);
  char path[PATH_MAX];
  if (phase[0] != '\0' && directory[0] != '\0'
      && descriptor_path(fd, path, sizeof(path)) == 0) {
    const char *name = strrchr(path, '/');
    int is_directory = strcmp(path, directory) == 0;
    int is_file = name != NULL && (strcmp(name + 1, "state_root") == 0
      || strcmp(name + 1, "state_root.staged") == 0);
    if (is_directory || is_file) {
      const char *prior = getenv("OCTRA_TEST_ROOT_SYNCS");
      char count[32];
      snprintf(count, sizeof(count), "%d", prior == NULL ? 1 : atoi(prior) + 1);
      setenv("OCTRA_TEST_ROOT_SYNCS", count, 1);
      const char *kind = is_directory ? "dir" : "file";
      fprintf(stderr, "event = root_sync phase = %s kind = %s count = %s\n", phase, kind, count);
      const char *renamed = getenv("OCTRA_TEST_ROOT_RENAMES");
      int renames = renamed == NULL ? 0 : atoi(renamed);
      if ((is_directory && (renames != 1 || atoi(count) != 2))
          || (is_file && (renames != 0 || atoi(count) != 1))) {
        errno = EPROTO;
        return -1;
      }
      size_t length = strlen(kind);
      if (strncmp(phase, kind, length) == 0 && phase[length] == '_') {
        const char *action = phase + length + 1;
        if (strstr(action, "after") != NULL && sync_real(fd) != 0) return -1;
        if (strncmp(action, "kill", 4) == 0) {
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

int open(const char *path, int flags, ...) {
  mode_t mode = 0;
  if (flags & O_CREAT) {
    va_list args;
    va_start(args, flags);
    mode = (mode_t)va_arg(args, int);
    va_end(args);
  }
  int (*open_real)(const char *, int, ...) = dlsym(RTLD_NEXT, "open");
  if (open_real == NULL) { errno = ENOSYS; return -1; }
  int fd = open_real(path, flags, mode);
  char phase[32];
  const char *value = getenv("OCTRA_TEST_ROOT_PHASE");
  snprintf(phase, sizeof(phase), "%s", value == NULL ? "" : value);
  const char *name = strrchr(path, '/');
  name = name == NULL ? path : name + 1;
  if (fd >= 0 && (flags & O_TRUNC) && phase[0] != '\0'
      && (strcmp(name, "state_root") == 0 || strcmp(name, "state_root.staged") == 0)) {
    const char *prior = getenv("OCTRA_TEST_ROOT_COUNT");
    char count[32];
    snprintf(count, sizeof(count), "%d", prior == NULL ? 1 : atoi(prior) + 1);
    setenv("OCTRA_TEST_ROOT_COUNT", count, 1);
    fprintf(stderr, "event = root_open phase = %s file = %s count = %s\n", phase, name, count);
    if (strcmp(phase, "kill") == 0) {
      kill(getpid(), SIGKILL);
      _exit(99);
    }
    if (strcmp(phase, "error") == 0) {
      close(fd);
      errno = EIO;
      return -1;
    }
  }
  return fd;
}

ssize_t write(int fd, const void *bytes, size_t size) {
  ssize_t (*write_real)(int, const void *, size_t) = dlsym(RTLD_NEXT, "write");
  if (write_real == NULL) { errno = ENOSYS; return -1; }
  char phase[64];
  const char *value = getenv("OCTRA_TEST_ROOT_PHASE");
  snprintf(phase, sizeof(phase), "%s", value == NULL ? "" : value);
  char path[PATH_MAX];
  if (phase[0] != '\0' && descriptor_path(fd, path, sizeof(path)) == 0) {
    const char *name = strrchr(path, '/');
    if (name != NULL && (strcmp(name + 1, "state_root") == 0
        || strcmp(name + 1, "state_root.staged") == 0)) {
      const char *prior = getenv("OCTRA_TEST_ROOT_WRITES");
      char count[32];
      snprintf(count, sizeof(count), "%d", prior == NULL ? 1 : atoi(prior) + 1);
      setenv("OCTRA_TEST_ROOT_WRITES", count, 1);
      if (strcmp(phase, "write_error") == 0 || strcmp(phase, "write_kill") == 0) {
        size_t part = size / 2;
        if (part == 0) { errno = EINVAL; return -1; }
        if (write_real(fd, bytes, part) != (ssize_t)part) return -1;
        fprintf(stderr, "event = root_write phase = %s bytes = %zu\n", phase, part);
        if (strcmp(phase, "write_kill") == 0) {
          kill(getpid(), SIGKILL);
          _exit(99);
        }
        errno = EIO;
        return -1;
      }
    }
  }
  return write_real(fd, bytes, size);
}

int rename(const char *source, const char *target) {
  int (*rename_real)(const char *, const char *) = dlsym(RTLD_NEXT, "rename");
  if (rename_real == NULL) { errno = ENOSYS; return -1; }
  char phase[64];
  const char *value = getenv("OCTRA_TEST_ROOT_PHASE");
  snprintf(phase, sizeof(phase), "%s", value == NULL ? "" : value);
  const char *name = strrchr(target, '/');
  if (phase[0] != '\0' && name != NULL && strcmp(name + 1, "state_root") == 0) {
    const char *synced = getenv("OCTRA_TEST_ROOT_SYNCS");
    int syncs = synced == NULL ? 0 : atoi(synced);
    const char *written = getenv("OCTRA_TEST_ROOT_WRITES");
    int writes = written == NULL ? 0 : atoi(written);
    if (syncs != 1 || writes != 1) {
      fprintf(stderr, "event = root_rename status = rejected syncs = %d writes = %d\n", syncs, writes);
      errno = EPROTO;
      return -1;
    }
    const char *prior = getenv("OCTRA_TEST_ROOT_RENAMES");
    char count[32];
    snprintf(count, sizeof(count), "%d", prior == NULL ? 1 : atoi(prior) + 1);
    setenv("OCTRA_TEST_ROOT_RENAMES", count, 1);
    if (strncmp(phase, "rename_", 7) == 0) {
      if (strstr(phase, "after") != NULL && rename_real(source, target) != 0) return -1;
      fprintf(stderr, "event = root_rename phase = %s\n", phase);
      if (strncmp(phase + 7, "kill", 4) == 0) {
        kill(getpid(), SIGKILL);
        _exit(99);
      }
      errno = EIO;
      return -1;
    }
  }
  return rename_real(source, target);
}