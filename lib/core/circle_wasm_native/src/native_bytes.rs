// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

pub unsafe fn write_owned_bytes(bytes: Vec<u8>, ptr_out: *mut *mut u8, len_out: *mut usize) {
    if !ptr_out.is_null() {
        *ptr_out = std::ptr::null_mut();
    }
    if !len_out.is_null() {
        *len_out = 0;
    }
    if ptr_out.is_null() || len_out.is_null() || bytes.is_empty() {
        return;
    }
    let bytes = bytes.into_boxed_slice();
    *len_out = bytes.len();
    *ptr_out = Box::into_raw(bytes).cast::<u8>();
}

pub unsafe fn free_bytes(ptr: *mut u8, len: usize) {
    if !ptr.is_null() && len != 0 {
        drop(Box::from_raw(std::ptr::slice_from_raw_parts_mut(ptr, len)));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::alloc::{GlobalAlloc, Layout, System};
    use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicUsize, Ordering::SeqCst};

    struct Checked;

    static TRACKED: AtomicPtr<u8> = AtomicPtr::new(std::ptr::null_mut());
    static SIZE: AtomicUsize = AtomicUsize::new(0);
    static INVALID: AtomicBool = AtomicBool::new(false);

    #[global_allocator]
    static ALLOCATOR: Checked = Checked;

    unsafe impl GlobalAlloc for Checked {
        unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
            System.alloc(layout)
        }

        unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
            let layout = if TRACKED.load(SeqCst) == ptr {
                let size = SIZE.load(SeqCst);
                INVALID.fetch_or(layout.size() != size || layout.align() != 1, SeqCst);
                TRACKED.store(std::ptr::null_mut(), SeqCst);
                Layout::from_size_align_unchecked(size, 1)
            } else {
                layout
            };
            System.dealloc(ptr, layout);
        }

        unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, size: usize) -> *mut u8 {
            let tracked = TRACKED.load(SeqCst) == ptr;
            let layout = if tracked {
                let expected = SIZE.load(SeqCst);
                INVALID.fetch_or(layout.size() != expected || layout.align() != 1, SeqCst);
                Layout::from_size_align_unchecked(expected, 1)
            } else {
                layout
            };
            let result = System.realloc(ptr, layout, size);
            if tracked && !result.is_null() {
                SIZE.store(size, SeqCst);
                TRACKED.store(result, SeqCst);
            }
            result
        }
    }

    fn bytes(capacity: usize, contents: &[u8]) -> Vec<u8> {
        assert!(TRACKED.load(SeqCst).is_null());
        let mut bytes = Vec::with_capacity(capacity);
        bytes.extend_from_slice(contents);
        SIZE.store(bytes.capacity(), SeqCst);
        INVALID.store(false, SeqCst);
        if bytes.capacity() != 0 {
            TRACKED.store(bytes.as_mut_ptr(), SeqCst);
        }
        bytes
    }

    fn released() {
        assert!(TRACKED.load(SeqCst).is_null(), "native bytes leaked");
        assert!(!INVALID.load(SeqCst), "native bytes allocation layout changed");
    }

    #[test]
    fn ownership() {
        for capacity in [3, 4096] {
            let bytes = bytes(capacity, b"abc");
            let mut ptr = std::ptr::null_mut();
            let mut len = 0;
            unsafe {
                write_owned_bytes(bytes, &mut ptr, &mut len);
                assert_eq!(std::slice::from_raw_parts(ptr, len), b"abc");
                free_bytes(ptr, len);
            }
            released();
        }
        for capacity in [0, 4096] {
            let bytes = bytes(capacity, b"");
            let mut ptr = std::ptr::null_mut();
            let mut len = 99;
            unsafe {
                write_owned_bytes(bytes, &mut ptr, &mut len);
                assert!(ptr.is_null());
                assert_eq!(len, 0);
                free_bytes(ptr, len);
            }
            released();
        }
        let mut len = 99;
        unsafe { write_owned_bytes(bytes(4096, b"abc"), std::ptr::null_mut(), &mut len); }
        assert_eq!(len, 0);
        released();
        let mut ptr = std::ptr::NonNull::<u8>::dangling().as_ptr();
        unsafe { write_owned_bytes(bytes(4096, b"abc"), &mut ptr, std::ptr::null_mut()); }
        assert!(ptr.is_null());
        released();
    }
}