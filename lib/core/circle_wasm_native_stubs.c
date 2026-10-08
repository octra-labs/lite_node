// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/custom.h>

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

extern void* octra_circle_call_new(void);
extern void octra_circle_call_drop(void* port);
extern int octra_circle_call_step(
  void* port,
  const uint8_t* input,
  size_t length,
  uint8_t** output,
  size_t* size
);

static void call_release(value session) {
  void** port = (void**)Data_custom_val(session);
  octra_circle_call_drop(*port);
  *port = NULL;
}

static struct custom_operations call_ops = {
  "octra.circle.call",
  call_release,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value caml_octra_circle_call_new(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(session);
  session = caml_alloc_custom(&call_ops, sizeof(void*), 0, 1);
  *((void**)Data_custom_val(session)) = octra_circle_call_new();
  CAMLreturn(session);
}

CAMLprim value caml_octra_circle_call_close(value session) {
  CAMLparam1(session);
  call_release(session);
  CAMLreturn(Val_unit);
}

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

CAMLprim value caml_octra_circle_call_step(value session, value input) {
  CAMLparam2(session, input);
  CAMLlocal2(body, result);
  void* port = *((void**)Data_custom_val(session));
  uint8_t* bytes = NULL;
  size_t length = 0;
  uint8_t* output = NULL;
  size_t size = 0;
  copy_to_malloc_string(input, &bytes, &length);
  caml_release_runtime_system();
  int code = octra_circle_call_step(port, bytes, length, &output, &size);
  caml_acquire_runtime_system();
  free(bytes);
  body = caml_alloc_string(size);
  if (size > 0 && output != NULL) {
    memcpy(Bytes_val(body), output, size);
  }
  octra_circle_wasm_host_free_bytes(output, size);
  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(code));
  Store_field(result, 1, body);
  CAMLreturn(result);
}

CAMLprim value caml_octra_circle_wasm_host_run_json(value v_input) {
  CAMLparam1(v_input);
  CAMLlocal2(v_payload, v_result);

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

  if (rc == 0) {
    v_payload = caml_alloc_string(out_len);
    if (out_len > 0 && out_ptr != NULL) {
      memcpy(Bytes_val(v_payload), out_ptr, out_len);
      octra_circle_wasm_host_free_bytes(out_ptr, out_len);
    }
  } else {
    v_payload = caml_alloc_string(err_len);
    if (err_len > 0 && err_ptr != NULL) {
      memcpy(Bytes_val(v_payload), err_ptr, err_len);
      octra_circle_wasm_host_free_bytes(err_ptr, err_len);
    }
  }

  v_result = caml_alloc_tuple(2);
  Store_field(v_result, 0, Val_int(rc));
  Store_field(v_result, 1, v_payload);
  CAMLreturn(v_result);
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