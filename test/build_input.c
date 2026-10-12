// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <caml/signals.h>
#include <caml/threads.h>
#include <signal.h>
#include <stdatomic.h>
#include <stdlib.h>
#include <time.h>

#undef caml_release_runtime_system
#define caml_release_runtime_system input_pause
#define caml_enter_blocking_section_no_pending input_safe_release
#define caml_alloc_string input_string
#define octra_compute_input_new input_new
#define octra_compute_input_drop input_drop
#define octra_compute_build_retain input_retain
#define octra_compute_build_release input_release
#define octra_compute_build_step input_step
#define octra_circle_call_new input_call_new
#define octra_circle_call_retain input_call_retain
#define octra_circle_call_drop input_call_drop
#define octra_circle_call_step input_call_step
#define octra_circle_wasm_host_run_json input_run
#define octra_circle_wasm_host_free_bytes input_free
#define malloc input_malloc
#define free input_raw_free
#define caml_octra_compute_build_new probe_build_new
#define caml_octra_compute_build_begin probe_build_begin
#define caml_octra_compute_build_close probe_build_close
#define caml_octra_compute_build_step probe_build_step
#define caml_octra_compute_lease_new probe_lease_new
#define caml_octra_compute_lease_hold probe_lease_hold
#define caml_octra_compute_lease_close probe_lease_close
#define caml_octra_circle_call_new probe_call_new
#define caml_octra_circle_call_close probe_call_close
#define caml_octra_circle_call_step probe_call_step
#define caml_octra_circle_wasm_host_run_json probe_run_json
#define caml_octra_circle_hfhe_bridge probe_hfhe_bridge
#define octra_circle_wasm_host_hfhe_call_json probe_hfhe_json

static void input_pause(void);
static void input_safe_release(void);
static void* input_malloc(size_t size);
static void input_raw_free(void* bytes);
#include "circle_wasm_native_stubs.c"

#undef caml_alloc_string
#undef octra_compute_input_new
#undef octra_compute_input_drop
#undef octra_compute_build_retain
#undef octra_compute_build_release
#undef octra_compute_build_step
#undef octra_circle_call_new
#undef octra_circle_call_retain
#undef octra_circle_call_drop
#undef octra_circle_call_step
#undef octra_circle_wasm_host_run_json
#undef octra_circle_wasm_host_free_bytes
#undef malloc
#undef free
#undef caml_enter_blocking_section_no_pending

extern int octra_compute_input_new(const uint8_t*, size_t, void**, const uint8_t**);
extern void octra_compute_input_drop(void*);
extern void* octra_compute_build_retain(void*);
extern void octra_compute_build_release(void*);
extern int octra_compute_build_step(void*, const uint8_t*, size_t, uint8_t**, size_t*);
extern void* octra_circle_call_new(void);
extern void* octra_circle_call_retain(void*);
extern void octra_circle_call_drop(void*);
extern int octra_circle_call_step(void*, const uint8_t*, size_t, uint8_t**, size_t*);
extern int octra_circle_wasm_host_run_json(
  const uint8_t*, size_t, uint8_t**, size_t*, uint8_t**, size_t*, void**
);
extern void octra_circle_wasm_host_free_bytes(uint8_t*, size_t);
extern value caml_alloc_string(mlsize_t);
extern void caml_record_signal(int);

static atomic_int mode = 0;
static atomic_int waiting = 0;
static atomic_int resume_work = 0;
static atomic_int call_refs = 0;
static atomic_int bridge_depth = 0;
static atomic_int callback_returned = 0;
static void* held_input = NULL;
static void* held_port = NULL;
static void* held_reply = NULL;
static uint8_t* held_output = NULL;
static uint8_t* held_error = NULL;
static void* raw_blocks[64] = {0};
static int raw_invalid = 0;

static int raw_live(void) {
  int count = raw_invalid;
  for (size_t index = 0; index < 64; ++index) count += raw_blocks[index] != NULL;
  return count;
}

static void* input_malloc(size_t size) {
  if (atomic_load(&mode) == 10) {
    void* owner = NULL;
    int code = octra_circle_input_take(128 * 1024 * 1024, &owner);
    octra_circle_input_drop(owner);
    if (code != 2) caml_failwith("native input not reserved");
  }
  if (atomic_load(&mode) == 6 ||
      (atomic_load(&mode) == 8 && atomic_load(&bridge_depth) > 0)) {
    atomic_store(&mode, 0);
    return NULL;
  }
  void* bytes = malloc(size);
  if (bytes != NULL) {
    for (size_t index = 0; index < 64; ++index) {
      if (raw_blocks[index] == NULL) {
        raw_blocks[index] = bytes;
        return bytes;
      }
    }
    free(bytes);
    caml_failwith("native allocation tracking full");
  }
  return bytes;
}

static void input_raw_free(void* bytes) {
  if (bytes == NULL) return;
  for (size_t index = 0; index < 64; ++index) {
    if (raw_blocks[index] == bytes) {
      if (atomic_load(&mode) == 10) {
        void* owner = NULL;
        int code = octra_circle_input_take(128 * 1024 * 1024, &owner);
        octra_circle_input_drop(owner);
        if (code != 2) caml_failwith("native input released before buffer");
      }
      raw_blocks[index] = NULL;
      free(bytes);
      return;
    }
  }
  raw_invalid = 1;
}

void* input_call_new(void) {
  void* port = octra_circle_call_new();
  if (port != NULL) atomic_fetch_add(&call_refs, 1);
  return port;
}

void* input_call_retain(void* port) {
  void* retained = octra_circle_call_retain(port);
  if (retained != NULL) atomic_fetch_add(&call_refs, 1);
  return retained;
}

void input_call_drop(void* port) {
  if (port != NULL) atomic_fetch_sub(&call_refs, 1);
  octra_circle_call_drop(port);
}

int input_call_step(void* port, const uint8_t* bytes, size_t length,
  uint8_t** output, size_t* size) {
  int code = octra_circle_call_step(port, bytes, length, output, size);
  held_output = *output;
  return code;
}

int input_run(const uint8_t* bytes, size_t length,
  uint8_t** output, size_t* size, uint8_t** error, size_t* error_size, void** owner) {
  int code = octra_circle_wasm_host_run_json(bytes, length, output, size, error, error_size, owner);
  held_reply = *owner;
  held_output = *output;
  held_error = *error;
  return code;
}

int input_new(const uint8_t* input, size_t length, void** owner, const uint8_t** bytes) {
  int selected = atomic_load(&mode);
  if (selected == 4 || selected == 5) {
    atomic_store(&mode, 0);
    *owner = NULL;
    *bytes = NULL;
    return selected == 4 ? 2 : 4;
  }
  int code = octra_compute_input_new(input, length, owner, bytes);
  if (code == 0) held_input = *owner;
  return code;
}

void input_drop(void* owner) {
  if (owner != NULL && held_input == owner) held_input = NULL;
  if (owner != NULL && held_reply == owner) {
    held_reply = NULL;
    held_output = NULL;
  }
  octra_compute_input_drop(owner);
}

void* input_retain(void* port) {
  held_port = octra_compute_build_retain(port);
  return held_port;
}

void input_release(void* port) {
  if (port != NULL && held_port == port) held_port = NULL;
  octra_compute_build_release(port);
}

int input_step(void* port, const uint8_t* bytes, size_t length, uint8_t** output, size_t* size) {
  int code = octra_compute_build_step(port, bytes, length, output, size);
  held_output = *output;
  return code;
}

void input_free(uint8_t* bytes, size_t size) {
  if (bytes != NULL && held_output == bytes) held_output = NULL;
  if (bytes != NULL && held_error == bytes) held_error = NULL;
  octra_circle_wasm_host_free_bytes(bytes, size);
}

value input_string(mlsize_t size) {
  if (atomic_load(&mode) == 2 ||
      (atomic_load(&mode) == 7 && atomic_load(&bridge_depth) > 0)) {
    atomic_store(&mode, 0);
    caml_raise_out_of_memory();
  }
  return caml_alloc_string(size);
}

static void input_pause(void) {
  if (atomic_load(&bridge_depth) == 0 &&
      (held_input == NULL || held_port == NULL) && raw_live() == 0
      && atomic_load(&call_refs) < 2) caml_failwith("native ownership missing");
  int selected = atomic_load(&mode);
  if (selected == 1 || (selected == 9 && atomic_load(&bridge_depth) > 0)) {
    atomic_store(&mode, 0);
    caml_record_signal(SIGUSR1);
  }
  caml_enter_blocking_section();
  if (selected == 3) {
    atomic_store(&waiting, 1);
    struct timespec delay = {0, 1000000};
    for (int tick = 0; tick < 10000 && !atomic_load(&resume_work); ++tick) {
      nanosleep(&delay, NULL);
    }
    atomic_store(&waiting, 0);
    atomic_store(&mode, 0);
  }
}

static void input_safe_release(void) {
  if (atomic_load(&mode) == 9) {
    atomic_store(&mode, 0);
    caml_record_signal(SIGUSR1);
  }
  caml_enter_blocking_section_no_pending();
}

CAMLprim value probe_hfhe_call(value unit) {
  CAMLparam1(unit);
  const uint8_t input[] = "{}";
  uint8_t* output = NULL;
  size_t size = 0;
  uint8_t* error = NULL;
  size_t error_size = 0;
  atomic_store(&callback_returned, 0);
  atomic_fetch_add(&bridge_depth, 1);
  caml_enter_blocking_section_no_pending();
  int code = probe_hfhe_json(input, 2, &output, &size, &error, &error_size);
  caml_acquire_runtime_system();
  input_raw_free(output);
  input_raw_free(error);
  atomic_fetch_sub(&bridge_depth, 1);
  atomic_store(&callback_returned, 1);
  CAMLreturn(Val_int(code));
}

CAMLprim value probe_hfhe_returned(value unit) {
  return Val_bool(atomic_load(&callback_returned));
}

CAMLprim value probe_hfhe_signal(value unit) {
  caml_record_signal(SIGUSR1);
  return Val_unit;
}

CAMLprim value probe_input_mode(value selected) {
  atomic_store(&resume_work, 0);
  atomic_store(&mode, Int_val(selected));
  return Val_unit;
}

CAMLprim value probe_input_wait(value unit) {
  return Val_bool(atomic_load(&waiting));
}

CAMLprim value probe_input_resume(value unit) {
  atomic_store(&resume_work, 1);
  return Val_unit;
}

CAMLprim value probe_input_live(value unit) {
  return Val_int((held_input != NULL) + (held_port != NULL) + (held_output != NULL)
    + (held_error != NULL) + (held_reply != NULL) + raw_live() + atomic_load(&call_refs));
}

CAMLprim value probe_call_refs(value unit) {
  return Val_int(atomic_load(&call_refs));
}

static void quota_release(value block) {
  void** owner = Data_custom_val(block);
  octra_circle_input_drop(*owner);
  *owner = NULL;
}

static struct custom_operations quota_ops = {
  "octra.test.quota",
  quota_release,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value probe_input_hold(value size) {
  CAMLparam1(size);
  CAMLlocal1(owner);
  owner = caml_alloc_custom(&quota_ops, sizeof(void*), 0, 1);
  *((void**)Data_custom_val(owner)) = NULL;
  int code = octra_circle_input_take(Long_val(size), Data_custom_val(owner));
  if (code != 0) caml_failwith("input quota unavailable");
  CAMLreturn(owner);
}

CAMLprim value probe_input_drop(value owner) {
  quota_release(owner);
  return Val_unit;
}

CAMLprim value probe_input_room(value size) {
  void* owner = NULL;
  int code = octra_circle_input_take(Long_val(size), &owner);
  octra_circle_input_drop(owner);
  return Val_int(code);
}