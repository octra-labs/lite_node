// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <poll.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <signal.h>
#include <unistd.h>

CAMLprim value octra_worker_stop(value process)
{
  CAMLparam1(process);
  const pid_t pid = Int_val(process);
  if (pid <= 1) caml_invalid_argument("worker pid");
  if (kill(pid, SIGKILL) < 0 && errno != ESRCH) uerror("worker_kill", Nothing);
  if (kill(-pid, SIGKILL) < 0 && errno != ESRCH) {
    const int saved = errno;
    const pid_t group = getpgid(pid);
    if (saved != EPERM || group == pid || (group < 0 && errno != ESRCH)) {
      errno = saved;
      uerror("worker_group_kill", Nothing);
    }
  }
  CAMLreturn(Val_unit);
}

CAMLprim value octra_io_wait(value input, value output, value error, value mask)
{
  CAMLparam4(input, output, error, mask);
  const int active = Int_val(mask);
  struct pollfd fds[3] = {
    {active & 1 ? Int_val(output) : -1, POLLIN, 0},
    {active & 2 ? Int_val(error) : -1, POLLIN, 0},
    {active & 4 ? Int_val(input) : -1, POLLOUT, 0}
  };
  caml_enter_blocking_section();
  const int result = poll(fds, 3, 100);
  const int saved = errno;
  caml_leave_blocking_section();
  if (result < 0 && saved != EINTR) {
    errno = saved;
    uerror("poll", Nothing);
  }
  int ready = 0;
  for (int i = 0; i < 3; ++i) {
    if (fds[i].revents & POLLNVAL) {
      errno = EBADF;
      uerror("poll", Nothing);
    }
    if (fds[i].revents & (fds[i].events | POLLHUP | POLLERR))
      ready |= 1 << i;
  }
  CAMLreturn(Val_int(ready));
}