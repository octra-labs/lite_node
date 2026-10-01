// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <atomic>
#include <cstddef>
#include <cstdlib>
#include <new>
#include <vector>
extern "C" {
#include <caml/mlvalues.h>
}

static std::atomic<intnat> remaining{-1};

void* operator new(std::size_t size) {
    auto count = remaining.load();
    while (count >= 0) {
        if (remaining.compare_exchange_weak(count, count - 1)) {
            if (count == 0) throw std::bad_alloc();
            break;
        }
    }
    if (void* ptr = std::malloc(size == 0 ? 1 : size)) return ptr;
    throw std::bad_alloc();
}

void* operator new[](std::size_t size) { return ::operator new(size); }
void operator delete(void* ptr) noexcept { std::free(ptr); }
void operator delete[](void* ptr) noexcept { std::free(ptr); }
void operator delete(void* ptr, std::size_t) noexcept { std::free(ptr); }
void operator delete[](void* ptr, std::size_t) noexcept { std::free(ptr); }

extern "C" CAMLprim value octra_alloc_fault(value count) {
    remaining = Long_val(count);
    return Val_unit;
}

extern "C" CAMLprim value octra_alloc_probe(value unit) {
    remaining = 0;
    bool caught = false;
    try {
        std::vector<char> bytes(64, 0);
        caught = bytes.at(0) != 0;
    } catch (const std::bad_alloc&) {
        caught = true;
    }
    remaining = -1;
    return Val_bool(caught);
}