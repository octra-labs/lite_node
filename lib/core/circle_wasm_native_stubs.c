// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/threads.h>

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

extern int octra_circle_wasm_host_run_json(
  const uint8_t* input_ptr,
  size_t input_len,
  uint8_t** out_ptr,
  size_t* out_len,
  uint8_t** err_ptr,
  size_t* err_len
);

extern void octra_circle_wasm_host_free_bytes(uint8_t* ptr, size_t len);

static void copy_to_malloc_string(value v_string, uint8_t** out_ptr, size_t* out_len) {
  size_t len = caml_string_length(v_string);
  uint8_t* buf = NULL;
  if (len > 0) {
    buf = malloc(len);
    if (buf == NULL) {
      caml_failwith("octra_circle_wasm_host: alloc failed");
    }
    memcpy(buf, String_val(v_string), len);
  }
  if (out_ptr != NULL) {
    *out_ptr = buf;
  }
  if (out_len != NULL) {
    *out_len = len;
  }
}

CAMLprim value caml_octra_circle_wasm_host_run_json(value v_input) {
  CAMLparam1(v_input);
  CAMLlocal1(v_output);

  uint8_t* input_ptr = NULL;
  size_t input_len = 0;
  uint8_t* out_ptr = NULL;
  size_t out_len = 0;
  uint8_t* err_ptr = NULL;
  size_t err_len = 0;

  copy_to_malloc_string(v_input, &input_ptr, &input_len);
  if (input_ptr == NULL) {
    input_ptr = malloc(1);
    if (input_ptr == NULL) {
      caml_failwith("octra_circle_wasm_host: alloc failed");
    }
  }
  caml_release_runtime_system();
  int rc =
    octra_circle_wasm_host_run_json(
      input_ptr,
      input_len,
      &out_ptr,
      &out_len,
      &err_ptr,
      &err_len
    );
  caml_acquire_runtime_system();
  free(input_ptr);

  if (rc != 0) {
    if (err_ptr != NULL && err_len > 0) {
      char* message = malloc(err_len + 1);
      if (message == NULL) {
        octra_circle_wasm_host_free_bytes(err_ptr, err_len);
        caml_failwith("octra_circle_wasm_host: alloc failed");
      }
      memcpy(message, err_ptr, err_len);
      message[err_len] = '\0';
      octra_circle_wasm_host_free_bytes(err_ptr, err_len);
      caml_failwith(message);
    }
    caml_failwith("octra_circle_wasm_host: unknown error");
  }

  v_output = caml_alloc_string(out_len);
  if (out_len > 0 && out_ptr != NULL) {
    memcpy(Bytes_val(v_output), out_ptr, out_len);
    octra_circle_wasm_host_free_bytes(out_ptr, out_len);
  }

  CAMLreturn(v_output);
}

static int hfhe_call_json_locked(
  const uint8_t* input_ptr,
  size_t input_len,
  uint8_t** out_ptr,
  size_t* out_len,
  uint8_t** err_ptr,
  size_t* err_len
) {
  static const value* registered_cb = NULL;
  CAMLparam0();
  CAMLlocal2(v_input, v_output);

  if (registered_cb == NULL) {
    registered_cb = caml_named_value("octra_circle_wasm_host_hfhe_call_json");
  }
  if (registered_cb == NULL) {
    const char* message = "octra_circle_wasm_host: hfhe callback not registered";
    size_t len = strlen(message);
    uint8_t* buf = malloc(len);
    if (buf != NULL) {
      memcpy(buf, message, len);
    }
    if (err_ptr != NULL) {
      *err_ptr = buf;
    }
    if (err_len != NULL) {
      *err_len = len;
    }
    if (out_ptr != NULL) {
      *out_ptr = NULL;
    }
    if (out_len != NULL) {
      *out_len = 0;
    }
    CAMLreturnT(int, 1);
  }

  v_input = caml_alloc_string(input_len);
  if (input_len > 0 && input_ptr != NULL) {
    memcpy(Bytes_val(v_input), input_ptr, input_len);
  }

  v_output = caml_callback_exn(*registered_cb, v_input);
  if (Is_exception_result(v_output)) {
    const char* message = "octra_circle_wasm_host: hfhe callback exception";
    size_t len = strlen(message);
    uint8_t* buf = malloc(len);
    if (buf != NULL) {
      memcpy(buf, message, len);
    }
    if (err_ptr != NULL) {
      *err_ptr = buf;
    }
    if (err_len != NULL) {
      *err_len = len;
    }
    if (out_ptr != NULL) {
      *out_ptr = NULL;
    }
    if (out_len != NULL) {
      *out_len = 0;
    }
    CAMLreturnT(int, 1);
  }

  copy_to_malloc_string(v_output, out_ptr, out_len);
  if (err_ptr != NULL) {
    *err_ptr = NULL;
  }
  if (err_len != NULL) {
    *err_len = 0;
  }
  CAMLreturnT(int, 0);
}

int octra_circle_wasm_host_hfhe_call_json(
  const uint8_t* input_ptr,
  size_t input_len,
  uint8_t** out_ptr,
  size_t* out_len,
  uint8_t** err_ptr,
  size_t* err_len
) {
  caml_acquire_runtime_system();
  int rc =
    hfhe_call_json_locked(
      input_ptr,
      input_len,
      out_ptr,
      out_len,
      err_ptr,
      err_len
    );
  caml_release_runtime_system();
  return rc;
}