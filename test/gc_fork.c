// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <pthread.h>
#include <signal.h>
#include <unistd.h>

static volatile sig_atomic_t gc_halt = 0;

static void halt_gc_child(void) {
  if (gc_halt) {
    gc_halt = 0;
    kill(getpid(), SIGSTOP);
  }
}

CAMLprim value octra_test_gc_halt(value armed) {
  static int installed = 0;
  if (!installed) {
    if (pthread_atfork(NULL, NULL, halt_gc_child) != 0)
      caml_failwith("GC fork hook failed");
    installed = 1;
  }
  gc_halt = Bool_val(armed);
  return Val_unit;
}