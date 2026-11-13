// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

extern "C" {
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/threads.h>
}

#include <cstdio>
#include "pvac/crypto/bulletproofs/ready_points.hpp"

enum class ReadyFault { None, Memory, Failed };

template<class F>
static intnat ready_run(F run) {
    intnat result = 0;
    ReadyFault fault = ReadyFault::None;
    char error[256]{};
    caml_enter_blocking_section();
    try { result = run(); }
    catch (const std::bad_alloc&) { fault = ReadyFault::Memory; }
    catch (const std::exception& value) {
        fault = ReadyFault::Failed;
        std::snprintf(error, sizeof(error), "%s", value.what());
    }
    catch (...) { fault = ReadyFault::Failed; }
    caml_leave_blocking_section();
    switch (fault) {
        case ReadyFault::None: break;
        case ReadyFault::Memory: caml_raise_out_of_memory();
        case ReadyFault::Failed: caml_failwith(error[0] ? error : "ready operation failed");
    }
    return result;
}

extern "C" CAMLprim value octra_ready_build(value size) {
    CAMLparam1(size);
    const intnat n = Long_val(size);
    if (n <= 0) caml_invalid_argument("ready size must be positive");
    ready_run([=] { pvac::bp::ready_table().build(static_cast<size_t>(n)); return intnat{0}; });
    CAMLreturn(Val_unit);
}

extern "C" CAMLprim value octra_ready_check(value unit) {
    CAMLparam1(unit);
    const auto result = ready_run([] { return pvac::bp::ready_check() ? 1 : 0; });
    CAMLreturn(Val_bool(result));
}

extern "C" CAMLprim value octra_ready_capacity(value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_long(pvac::bp::ready_capacity));
}

extern "C" CAMLprim value octra_ready_gates(value unit) {
    CAMLparam1(unit);
    const auto result = ready_run([] { return pvac::bp::ready_table().size(); });
    CAMLreturn(Val_long(result));
}

extern "C" CAMLprim value octra_ready_sums(value unit) {
    CAMLparam1(unit);
    const auto result = ready_run([] { return pvac::bp::ready_table().sums(); });
    CAMLreturn(Val_long(result));
}

extern "C" CAMLprim value octra_ready_rejected(value unit) {
    CAMLparam1(unit);
    const auto result = ready_run([] { return pvac::bp::ready_table().rejected(); });
    CAMLreturn(Val_long(result));
}