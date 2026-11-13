// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <sys/resource.h>
#include <unistd.h>

static volatile sig_atomic_t gc_halt = 0;
static volatile sig_atomic_t gc_limit = 0;

static void configure_gc_child(void) {
  if (gc_limit) {
    struct rlimit limit = {16, 16};
    gc_limit = 0;
    if (setrlimit(RLIMIT_NOFILE, &limit) != 0) _exit(125);
  }
  if (gc_halt) {
    gc_halt = 0;
    kill(getpid(), SIGSTOP);
  }
}

static void install_gc_hook(void) {
  static int installed = 0;
  if (!installed) {
    if (pthread_atfork(NULL, NULL, configure_gc_child) != 0)
      caml_failwith("GC fork hook failed");
    installed = 1;
  }
}

CAMLprim value octra_test_gc_halt(value armed) {
  install_gc_hook();
  gc_halt = Bool_val(armed);
  return Val_unit;
}

CAMLprim value octra_test_gc_limit(value armed) {
  install_gc_hook();
  gc_limit = Bool_val(armed);
  return Val_unit;
}

CAMLprim value octra_test_gc_limits(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(result, soft, hard);
  struct rlimit limit;
  if (getrlimit(RLIMIT_NOFILE, &limit) != 0)
    caml_failwith("GC descriptor limits unavailable");
  soft = caml_copy_int64((int64_t)limit.rlim_cur);
  hard = caml_copy_int64((int64_t)limit.rlim_max);
  result = caml_alloc_tuple(2);
  Store_field(result, 0, soft);
  Store_field(result, 1, hard);
  CAMLreturn(result);
}

CAMLprim value octra_test_gc_high_fd(value file) {
  return Val_bool(Long_val(file) >= 16);
}