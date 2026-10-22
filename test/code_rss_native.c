// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#ifdef __APPLE__
#include <mach/mach.h>
#endif

CAMLprim value octra_test_rss(value unit) {
  (void)unit;
  uint64_t bytes = 0;
#ifdef __APPLE__
  mach_task_basic_info_data_t info;
  mach_msg_type_number_t count = MACH_TASK_BASIC_INFO_COUNT;
  if (task_info(mach_task_self(), MACH_TASK_BASIC_INFO, (task_info_t)&info, &count) != KERN_SUCCESS)
    caml_failwith("RSS read failed");
  bytes = info.resident_size;
#elif defined(__linux__)
  FILE *file = fopen("/proc/self/statm", "r");
  unsigned long pages = 0;
  unsigned long resident = 0;
  if (file == NULL) caml_failwith("RSS open failed");
  int read = fscanf(file, "%lu %lu", &pages, &resident);
  fclose(file);
  long page_size = sysconf(_SC_PAGESIZE);
  if (read != 2 || page_size <= 0) caml_failwith("RSS read failed");
  bytes = (uint64_t)resident * (uint64_t)page_size;
#else
  caml_failwith("RSS platform unsupported");
#endif
  return caml_copy_int64((int64_t)bytes);
}