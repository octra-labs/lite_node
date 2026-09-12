// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <poll.h>

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