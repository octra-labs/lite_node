// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <dirent.h>
#include <limits.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#ifdef __APPLE__
#include <libproc.h>
#endif

CAMLprim value octra_proof_prefer_oom(value unit)
{
  CAMLparam1(unit);
#ifdef __linux__
  FILE *file = fopen("/proc/self/oom_score_adj", "w");
  if (file == NULL) uerror("proof_oom_priority", Nothing);
  int error = fputs("1000\n", file) < 0 ? (errno ? errno : EIO) : 0;
  if (fclose(file) != 0 && error == 0) error = errno ? errno : EIO;
  if (error != 0) {
    errno = error;
    uerror("proof_oom_priority", Nothing);
  }
#endif
  CAMLreturn(Val_unit);
}

CAMLprim value octra_proof_rss(value process)
{
  CAMLparam1(process);
  const pid_t pid = Int_val(process);
  intnat megabytes = -1;
#ifdef __APPLE__
  struct rusage_info_v2 usage;
  if (proc_pid_rusage(pid, RUSAGE_INFO_V2, (rusage_info_t *)&usage) == 0)
    megabytes = (usage.ri_resident_size + 1048575) / 1048576;
#elif defined(__linux__)
  char path[64];
  snprintf(path, sizeof(path), "/proc/%d/statm", pid);
  FILE *file = fopen(path, "r");
  if (file != NULL) {
    unsigned long total, pages;
    const long page_size = sysconf(_SC_PAGESIZE);
    if (page_size > 0 && fscanf(file, "%lu %lu", &total, &pages) == 2
      && pages <= ULONG_MAX / (unsigned long)page_size)
      megabytes = (pages * (unsigned long)page_size + 1048575) / 1048576;
    fclose(file);
  }
#endif
  CAMLreturn(Val_long(megabytes));
}

CAMLprim value octra_proof_shed(value unit)
{
  CAMLparam1(unit);
#ifdef __linux__
  DIR *directory = opendir("/proc/self/fd");
#else
  DIR *directory = opendir("/dev/fd");
#endif
  if (directory == NULL) uerror("proof_descriptors", Nothing);
  const int listing = dirfd(directory);
  int error = 0;
  for (;;) {
    errno = 0;
    struct dirent *entry = readdir(directory);
    if (entry == NULL) {
      error = errno;
      break;
    }
    char *end;
    long fd = strtol(entry->d_name, &end, 10);
    if (*entry->d_name != '\0' && *end == '\0' && fd > 2 && fd <= INT_MAX && fd != listing)
      if (close((int)fd) < 0 && errno != EBADF && errno != EINTR) {
        error = errno;
        break;
      }
  }
  if (closedir(directory) != 0 && error == 0) error = errno;
  if (error != 0) {
    errno = error;
    uerror("proof_descriptors", Nothing);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value octra_proof_children(value process)
{
  CAMLparam1(process);
  CAMLlocal2(result, cell);
  const pid_t pid = Int_val(process);
  pid_t children[64];
  size_t count = 0;
#ifdef __APPLE__
  errno = 0;
  const int found = proc_listchildpids(pid, children, sizeof(children));
  if (found < 0 || (found == 0 && errno != 0)) uerror("proof_children", Nothing);
  if (found >= 64) caml_failwith("proof children limit");
  count = found;
#elif defined(__linux__)
  char path[96];
  snprintf(path, sizeof(path), "/proc/%d/task", pid);
  DIR *tasks = opendir(path);
  if (tasks == NULL) uerror("proof_children", Nothing);
  int error = 0;
  for (;;) {
    errno = 0;
    struct dirent *entry = readdir(tasks);
    if (entry == NULL) { error = errno; break; }
    char *end;
    const long tid = strtol(entry->d_name, &end, 10);
    if (tid <= 0 || tid > INT_MAX || *end != '\0') continue;
    snprintf(path, sizeof(path), "/proc/%d/task/%ld/children", pid, tid);
    FILE *file = fopen(path, "r");
    if (file == NULL) {
      if (errno == ENOENT) continue;
      error = errno;
      break;
    }
    int child;
    while (fscanf(file, "%d", &child) == 1 && count < 64) children[count++] = child;
    if (ferror(file)) error = EIO;
    fclose(file);
    if (error || count == 64) break;
  }
  closedir(tasks);
  if (error) caml_failwith("proof children read");
  if (count == 64) caml_failwith("proof children limit");
#else
  caml_failwith("proof children unavailable");
#endif
  result = Val_emptylist;
  for (size_t index = count; index > 0; --index) {
    cell = caml_alloc_small(2, 0);
    Field(cell, 0) = Val_long(children[index - 1]);
    Field(cell, 1) = result;
    result = cell;
  }
  CAMLreturn(result);
}