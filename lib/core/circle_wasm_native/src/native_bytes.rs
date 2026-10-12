// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

use std::sync::{Arc, Mutex};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Usage {
    used: usize,
    limit: usize,
    failed: bool,
}

#[derive(Clone, Copy)]
enum Charge {
    Take(usize),
    Release(usize),
}

impl Usage {
    fn delta(self, event: Charge) -> Option<Self> {
        let used = match event {
            Charge::Take(size) if !self.failed => self.used.checked_add(size)?,
            Charge::Release(size) => self.used.checked_sub(size)?,
            _ => return None,
        };
        (used <= self.limit).then_some(Self { used, ..self })
    }
}

#[derive(Debug)]
pub(crate) struct Pool {
    usage: Mutex<Usage>,
    parent: Option<Arc<Pool>>,
}

#[derive(Debug)]
pub(crate) struct Budget {
    pool: Arc<Pool>,
}

impl Budget {
    pub(crate) fn new(limit: usize) -> Arc<Self> {
        Arc::new(Self { pool: Pool::new(limit) })
    }

    pub(crate) fn take(&self, size: usize) -> Result<Lease, &'static str> {
        self.pool.take(size)
    }

    #[cfg(test)]
    pub(crate) fn used(&self) -> usize { self.pool.used() }
}

impl Pool {
    pub(crate) fn new(limit: usize) -> Arc<Self> {
        Arc::new(Self {
            usage: Mutex::new(Usage { used: 0, limit, failed: false }),
            parent: None,
        })
    }

    pub(crate) fn child(self: &Arc<Self>, limit: usize) -> Arc<Self> {
        Arc::new(Self {
            usage: Mutex::new(Usage { used: 0, limit, failed: false }),
            parent: Some(self.clone()),
        })
    }

    fn charge(&self, event: Charge) -> Result<(), &'static str> {
        let mut usage = self.usage.lock().map_err(|_| "compute memory owner failed")?;
        let next = usage.delta(event).ok_or("compute memory busy")?;
        if let Some(parent) = &self.parent {
            parent.charge(event)?;
        }
        *usage = next;
        Ok(())
    }

    pub(crate) fn take(self: &Arc<Self>, size: usize) -> Result<Lease, &'static str> {
        self.take_in(size, None)
    }

    pub(crate) fn take_in(self: &Arc<Self>, size: usize, quota: Option<&Arc<Budget>>)
        -> Result<Lease, &'static str>
    {
        self.charge_in(Charge::Take(size), quota)?;
        Ok(Lease { pool: self.clone(), quota: quota.cloned(), size })
    }

    fn charge_in(&self, event: Charge, quota: Option<&Arc<Budget>>) -> Result<(), &'static str> {
        let mut usage = match quota {
            Some(quota) => Some(quota.pool.usage.lock().map_err(|_| "job memory owner failed")?),
            None => None,
        };
        let next = match usage.as_ref() {
            Some(usage) => Some(usage.delta(event).ok_or("job memory limit exceeded")?),
            None => None,
        };
        self.charge(event)?;
        if let (Some(usage), Some(next)) = (usage.as_mut(), next) { **usage = next; }
        Ok(())
    }

    fn release(&self, size: usize) {
        let mut usage = match self.usage.lock() {
            Ok(usage) => usage,
            Err(error) => {
                let mut usage = error.into_inner();
                usage.failed = true;
                usage
            }
        };
        match usage.delta(Charge::Release(size)) {
            Some(next) => {
                if let Some(parent) = &self.parent {
                    parent.release(size);
                }
                *usage = next;
            }
            None => usage.failed = true,
        }
    }

    #[cfg(test)]
    pub(crate) fn used(&self) -> usize {
        self.usage.lock().unwrap_or_else(|error| error.into_inner()).used
    }
}

#[derive(Debug)]
pub(crate) struct Lease {
    pool: Arc<Pool>,
    quota: Option<Arc<Budget>>,
    size: usize,
}

impl Lease {
    pub(crate) fn size(&self) -> usize {
        self.size
    }

    pub(crate) fn counted(&self, quota: &Arc<Budget>) -> bool {
        Arc::ptr_eq(&self.pool, &quota.pool)
            || self.quota.as_ref().is_some_and(|owner| Arc::ptr_eq(owner, quota))
    }

    pub(crate) fn has_owner(&self, quota: Option<&Arc<Budget>>) -> bool {
        match (self.quota.as_ref(), quota) {
            (None, None) => true,
            (Some(left), Some(right)) => Arc::ptr_eq(left, right),
            _ => false,
        }
    }

    pub(crate) fn borrow(quota: Option<&Arc<Budget>>, resident: Option<&Self>, size: usize)
        -> Result<Option<Self>, &'static str>
    {
        match quota {
            Some(quota) if !resident.is_some_and(|lease| lease.counted(quota)) =>
                quota.take(resident.map_or(size, Self::size)).map(Some),
            _ => Ok(None),
        }
    }

    pub(crate) fn resize(&mut self, size: usize) -> Result<(), &'static str> {
        let event = if size >= self.size {
            Charge::Take(size - self.size)
        } else {
            Charge::Release(self.size - size)
        };
        self.pool.charge_in(event, self.quota.as_ref())?;
        self.size = size;
        Ok(())
    }

    pub(crate) fn join(&mut self, other: &mut Self) -> Result<(), &'static str> {
        if !Arc::ptr_eq(&self.pool, &other.pool) || !self.has_owner(other.quota.as_ref()) {
            return Err("compute memory owners differ");
        }
        self.size = self.size.checked_add(other.size)
            .ok_or("compute memory size overflow")?;
        other.size = 0;
        Ok(())
    }
}

impl Drop for Lease {
    fn drop(&mut self) {
        let mut quota = self.quota.as_ref().map(|quota| match quota.pool.usage.lock() {
            Ok(usage) => usage,
            Err(error) => {
                let mut usage = error.into_inner();
                usage.failed = true;
                usage
            }
        });
        if let Some(usage) = quota.as_mut() {
            match usage.delta(Charge::Release(self.size)) {
                Some(next) => **usage = next,
                None => { usage.failed = true; return; }
            }
        }
        self.pool.release(self.size);
    }
}

#[derive(Debug)]
pub(crate) struct Value<T = u8> {
    data: Vec<T>,
    _lease: Option<Lease>,
}

impl<T> Default for Value<T> {
    fn default() -> Self {
        Self { data: Vec::new(), _lease: None }
    }
}

impl Value {
    pub(crate) fn clear(&mut self) {
        if self._lease.is_some() {
            *self = Self::default();
        } else {
            self.data.clear();
        }
    }

    pub(crate) fn append(&mut self, pool: Option<&Arc<Pool>>, input: &[u8], limit: usize)
        -> Result<(), &'static str>
    {
        self.append_in(pool, None, input, limit)
    }

    pub(crate) fn append_in(&mut self, pool: Option<&Arc<Pool>>, quota: Option<&Arc<Budget>>,
        input: &[u8], limit: usize) -> Result<(), &'static str>
    {
        let size = self.data.len().checked_add(input.len())
            .filter(|size| *size <= limit).ok_or("response exceeds limit")?;
        if input.is_empty() {
            return Ok(());
        }
        let Some(pool) = pool else {
            if self._lease.is_some() {
                return Err("response memory owner missing");
            }
            self.data.extend_from_slice(input);
            return Ok(());
        };
        if size <= self.data.capacity()
            && self._lease.as_ref().is_some_and(|lease| Arc::ptr_eq(&lease.pool, pool))
            && quota.is_none_or(|quota| self._lease.as_ref().is_some_and(|lease| lease.counted(quota)))
        {
            self.data.extend_from_slice(input);
            return Ok(());
        }
        let capacity = size.max(self.data.capacity().saturating_mul(2).max(256)).min(limit);
        let lease = pool.take_in(capacity, quota)?;
        let mut data = Vec::new();
        data.try_reserve_exact(capacity).map_err(|_| "response allocation failed")?;
        let mut next = Self::charged(data, lease)?;
        next.data.extend_from_slice(&self.data);
        next.data.extend_from_slice(input);
        *self = next;
        Ok(())
    }
}

impl<T> Value<T> {
    pub(crate) fn uncounted(&self, quota: &Arc<Budget>) -> Result<usize, &'static str> {
        if self._lease.as_ref().is_some_and(|lease| lease.counted(quota)) { return Ok(0); }
        match &self._lease {
            Some(lease) => Ok(lease.size()),
            None => self.data.capacity().checked_mul(std::mem::size_of::<T>())
                .ok_or("value capacity overflow"),
        }
    }

    pub(crate) fn borrow(&self, quota: Option<&Arc<Budget>>) -> Result<Option<Lease>, &'static str> {
        let size = self.data.capacity().checked_mul(std::mem::size_of::<T>())
            .ok_or("value capacity overflow")?;
        Lease::borrow(quota, self._lease.as_ref(), size)
    }

    pub(crate) fn charged(data: Vec<T>, lease: Lease) -> Result<Self, &'static str> {
        if data.capacity().checked_mul(std::mem::size_of::<T>())
            .filter(|size| *size <= lease.size).is_none()
        {
            drop(data);
            drop(lease);
            return Err("compute memory reservation differs");
        }
        Ok(Self { data, _lease: Some(lease) })
    }

    pub(crate) fn as_slice(&self) -> &[T] {
        &self.data
    }

    pub(crate) fn into_plain(self) -> Vec<T> {
        assert!(self._lease.is_none());
        self.data
    }

    pub(crate) fn is_charged(&self) -> bool {
        self._lease.is_some()
    }
}

impl Value {
    pub(crate) fn into_foreign(mut self)
        -> Result<(*mut std::ffi::c_void, *mut u8, usize), &'static str>
    {
        let len = self.data.len();
        let lease = self._lease.as_mut().ok_or("reply owner missing")?;
        let copy = len.checked_add(2 * std::mem::size_of::<usize>())
            .ok_or("reply size overflow")?;
        let size = lease.size().checked_add(copy)
            .and_then(|size| size.checked_add(std::mem::size_of::<Self>()))
            .ok_or("reply size overflow")?;
        lease.resize(size)?;
        let layout = std::alloc::Layout::new::<Self>();
        let owner = unsafe { std::alloc::alloc(layout).cast::<Self>() };
        if owner.is_null() { return Err("reply owner allocation failed"); }
        let bytes = self.data.as_mut_ptr();
        unsafe { owner.write(self); }
        Ok((owner.cast(), bytes, len))
    }
}

impl<T> From<Vec<T>> for Value<T> {
    fn from(data: Vec<T>) -> Self {
        Self { data, _lease: None }
    }
}

impl<T> std::ops::Deref for Value<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> std::ops::DerefMut for Value<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl<T> AsMut<[T]> for Value<T> {
    fn as_mut(&mut self) -> &mut [T] {
        &mut self.data
    }
}

impl<T: PartialEq> PartialEq for Value<T> {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl<T: Eq> Eq for Value<T> {}

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
pub(crate) mod tests {
    use super::*;
    use std::alloc::{GlobalAlloc, Layout, System};
    use std::cell::Cell;
    use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicUsize, Ordering::SeqCst};

    #[test]
    fn job_limits() {
        let pool = Pool::new(1024);
        let child = pool.child(768);
        let job = Budget::new(512);
        let mut lease = child.take_in(400, Some(&job)).unwrap();
        assert_eq!((pool.used(), child.used(), job.used()), (400, 400, 400));
        assert!(child.take_in(113, Some(&job)).is_err());
        assert!(lease.resize(513).is_err());
        assert_eq!((pool.used(), child.used(), job.used()), (400, 400, 400));
        lease.resize(512).unwrap();
        lease.resize(100).unwrap();
        let full = pool.take(900).unwrap();
        assert!(child.take_in(25, Some(&job)).is_err());
        assert_eq!((pool.used(), child.used(), job.used()), (1000, 100, 100));
        drop(full);
        drop(lease);
        assert_eq!((pool.used(), child.used(), job.used()), (0, 0, 0));
    }

    #[test]
    fn job_borrow() {
        let pool = Pool::new(1024);
        let first = Budget::new(512);
        let next = Budget::new(255);
        let value = Value::charged(vec![7_u8; 256], pool.take_in(256, Some(&first)).unwrap()).unwrap();
        assert!(value.borrow(Some(&first)).unwrap().is_none());
        assert!(value.borrow(Some(&next)).is_err());
        assert_eq!((pool.used(), first.used(), next.used()), (256, 256, 0));
        let next = Budget::new(256);
        let held = value.borrow(Some(&next)).unwrap().unwrap();
        assert_eq!((pool.used(), first.used(), next.used()), (256, 256, 256));
        drop(held);
        assert_eq!(next.used(), 0);
        drop(value);
        assert_eq!((pool.used(), first.used()), (0, 0));
    }

    #[test]
    fn job_join() {
        let pool = Pool::new(1024);
        let job = Budget::new(512);
        let other = Budget::new(512);
        let mut first = pool.take_in(200, Some(&job)).unwrap();
        let mut second = pool.take_in(100, Some(&job)).unwrap();
        let mut foreign = pool.take_in(100, Some(&other)).unwrap();
        assert!(first.join(&mut foreign).is_err());
        first.join(&mut second).unwrap();
        drop(second);
        assert_eq!((pool.used(), job.used(), other.used()), (400, 300, 100));
        drop(first);
        drop(foreign);
        assert_eq!((pool.used(), job.used(), other.used()), (0, 0, 0));
    }

    #[test]
    fn job_parallel() {
        let pool = Pool::new(256);
        let job = Budget::new(192);
        let barrier = Arc::new(std::sync::Barrier::new(8));
        let threads: Vec<_> = (0..8).map(|_| {
            let pool = pool.clone();
            let job = job.clone();
            let barrier = barrier.clone();
            std::thread::spawn(move || {
                let lease = pool.take_in(64, Some(&job)).ok();
                barrier.wait();
                let accepted = lease.is_some();
                barrier.wait();
                accepted
            })
        }).collect();
        let count = threads.into_iter().map(|thread| usize::from(thread.join().unwrap())).sum::<usize>();
        assert_eq!(count, 3);
        assert_eq!((pool.used(), job.used()), (0, 0));
    }

    struct Checked;

    static TRACKED: AtomicPtr<u8> = AtomicPtr::new(std::ptr::null_mut());
    static SIZE: AtomicUsize = AtomicUsize::new(0);
    static INVALID: AtomicBool = AtomicBool::new(false);
    std::thread_local! {
        static MIN_SIZE: Cell<usize> = const { Cell::new(usize::MAX) };
        static LARGE: Cell<usize> = const { Cell::new(0) };
        static REFUSED: Cell<usize> = const { Cell::new(usize::MAX) };
        static SKIP: Cell<usize> = const { Cell::new(0) };
        static PAUSE: Cell<*const AllocPause> = const { Cell::new(std::ptr::null()) };
        static PEAK: Cell<Option<(usize, usize, bool)>> = const { Cell::new(None) };
    }

    struct PeakWatch;

    impl Drop for PeakWatch {
        fn drop(&mut self) {
            let _ = PEAK.try_with(|slot| slot.set(None));
        }
    }

    fn count_bytes(added: usize, removed: usize) {
        let _ = PEAK.try_with(|slot| {
            if let Some((used, peak, valid)) = slot.get() {
                let next = used.checked_add(added).and_then(|size| size.checked_sub(removed));
                let overlap = used.checked_add(added);
                slot.set(Some((next.unwrap_or(0), peak.max(overlap.unwrap_or(usize::MAX)),
                    valid && next.is_some() && overlap.is_some())));
            }
        });
    }

    pub(crate) fn allocation_peak(run: impl FnOnce()) -> usize {
        PEAK.with(|slot| {
            assert!(slot.get().is_none());
            slot.set(Some((0, 0, true)));
        });
        let watch = PeakWatch;
        run();
        let (used, peak, valid) = PEAK.with(|slot| slot.get().unwrap());
        drop(watch);
        assert!(valid, "allocation accounting differs");
        assert_eq!(used, 0, "measured allocation escaped scope");
        peak
    }

    #[derive(Clone, Copy, PartialEq, Eq)]
    pub(crate) enum AllocPhase {
        Allocated = 1,
        Releasing = 2,
    }

    struct AllocPause {
        size: usize,
        align: usize,
        ptr: AtomicPtr<u8>,
        phase: AtomicUsize,
        resume: AtomicUsize,
        expired: AtomicBool,
    }

    impl AllocPause {
        fn stop(&self, phase: AllocPhase) {
            let phase = phase as usize;
            self.phase.store(phase, SeqCst);
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
            while self.resume.load(SeqCst) < phase {
                if std::time::Instant::now() >= deadline {
                    self.expired.store(true, SeqCst);
                    break;
                }
                std::thread::sleep(std::time::Duration::from_millis(1));
            }
        }
    }

    fn with_alloc_pause(run: impl FnOnce(&AllocPause)) {
        let _ = PAUSE.try_with(|slot| {
            if let Some(pause) = unsafe { slot.get().as_ref() } {
                run(pause);
            }
        });
    }

    pub(crate) fn paused_alloc<T: Send + 'static>(
        size: usize,
        align: usize,
        run: impl FnOnce() -> T + Send + 'static,
        mut inspect: impl FnMut(AllocPhase),
    ) -> (T, bool) {
        let pause = Arc::new(AllocPause {
            size,
            align,
            ptr: AtomicPtr::new(std::ptr::null_mut()),
            phase: AtomicUsize::new(0),
            resume: AtomicUsize::new(0),
            expired: AtomicBool::new(false),
        });
        let state = pause.clone();
        let worker = std::thread::spawn(move || {
            PAUSE.with(|slot| slot.set(Arc::as_ptr(&state)));
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(run));
            PAUSE.with(|slot| slot.set(std::ptr::null()));
            result
        });
        let checked = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let mut freeing = false;
            for phase in [AllocPhase::Allocated, AllocPhase::Releasing] {
                let deadline = std::time::Instant::now() + std::time::Duration::from_secs(3);
                while pause.phase.load(SeqCst) < phase as usize && !worker.is_finished() {
                    assert!(std::time::Instant::now() < deadline, "allocation phase not reached");
                    std::thread::sleep(std::time::Duration::from_millis(1));
                }
                let reached = pause.phase.load(SeqCst) == phase as usize;
                if phase == AllocPhase::Allocated {
                    assert!(reached, "allocation was not observed");
                } else {
                    freeing = reached;
                }
                if reached {
                    inspect(phase);
                }
                pause.resume.store(phase as usize, SeqCst);
            }
            freeing
        }));
        pause.resume.store(AllocPhase::Releasing as usize, SeqCst);
        let result = worker.join().unwrap();
        let freeing = checked.unwrap();
        assert!(!pause.expired.load(SeqCst), "allocation pause expired");
        (result.unwrap(), freeing)
    }

    struct AllocationWatch;

    struct AllocationRefusal;

    impl Drop for AllocationRefusal {
        fn drop(&mut self) {
            let _ = REFUSED.try_with(|size| size.set(usize::MAX));
        }
    }

    pub(crate) fn refused_alloc<T>(size: usize, run: impl FnOnce() -> T) -> T {
        refused_alloc_after(size, 0, run)
    }

    pub(crate) fn refused_alloc_after<T>(size: usize, skip: usize, run: impl FnOnce() -> T) -> T {
        REFUSED.with(|current| {
            assert_eq!(current.get(), usize::MAX);
            current.set(size);
        });
        SKIP.with(|current| current.set(skip));
        let guard = AllocationRefusal;
        let result = run();
        drop(guard);
        result
    }

    fn refuse(size: usize) -> bool {
        REFUSED.try_with(|value| value.get() == size).unwrap_or(false)
            && SKIP.try_with(|value| {
                let skip = value.get();
                value.set(skip.saturating_sub(1));
                skip == 0
            }).unwrap_or(false)
    }

    impl Drop for AllocationWatch {
        fn drop(&mut self) {
            let _ = MIN_SIZE.try_with(|minimum| minimum.set(usize::MAX));
        }
    }

    pub(crate) fn large_allocs<T>(minimum: usize, run: impl FnOnce() -> T) -> (T, usize) {
        MIN_SIZE.with(|current| assert_eq!(current.get(), usize::MAX, "allocation watch active"));
        LARGE.with(|count| count.set(0));
        MIN_SIZE.with(|current| current.set(minimum));
        let watch = AllocationWatch;
        let result = run();
        let count = LARGE.with(Cell::get);
        drop(watch);
        (result, count)
    }

    fn count_large(size: usize) {
        if MIN_SIZE.try_with(|minimum| size >= minimum.get()).unwrap_or(false) {
            let _ = LARGE.try_with(|count| count.set(count.get().saturating_add(1)));
        }
    }

    #[global_allocator]
    static ALLOCATOR: Checked = Checked;

    unsafe impl GlobalAlloc for Checked {
        unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
            count_large(layout.size());
            if refuse(layout.size()) {
                return std::ptr::null_mut();
            }
            let ptr = System.alloc(layout);
            if !ptr.is_null() { count_bytes(layout.size(), 0); }
            with_alloc_pause(|pause| {
                if layout.size() == pause.size && layout.align() == pause.align && !ptr.is_null()
                    && pause.phase.load(SeqCst) == 0
                {
                    pause.ptr.store(ptr, SeqCst);
                    pause.stop(AllocPhase::Allocated);
                }
            });
            ptr
        }

        unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
            count_bytes(0, layout.size());
            with_alloc_pause(|pause| {
                if pause.ptr.compare_exchange(ptr, std::ptr::null_mut(), SeqCst, SeqCst).is_ok() {
                    pause.stop(AllocPhase::Releasing);
                }
            });
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
            count_large(size);
            if refuse(size) {
                return std::ptr::null_mut();
            }
            let tracked = TRACKED.load(SeqCst) == ptr;
            let layout = if tracked {
                let expected = SIZE.load(SeqCst);
                INVALID.fetch_or(layout.size() != expected || layout.align() != 1, SeqCst);
                Layout::from_size_align_unchecked(expected, 1)
            } else {
                layout
            };
            let result = System.realloc(ptr, layout, size);
            if !result.is_null() { count_bytes(size, layout.size()); }
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
    fn allocation_pause() {
        let mut phases = Vec::new();
        let run = || {
            let value = std::hint::black_box(vec![7_u8; 4097]);
            assert_eq!(value.len(), 4097);
            drop(value);
            17
        };
        let (result, freeing) = paused_alloc(4097, 1, run, |phase| phases.push(phase));
        assert_eq!(result, 17);
        assert!(freeing);
        assert!(phases == [AllocPhase::Allocated, AllocPhase::Releasing]);
        let failed = std::panic::catch_unwind(|| {
            paused_alloc(4097, 1, run, |_| panic!("inspection interrupted"));
        });
        assert!(failed.is_err());
        let (result, freeing) = paused_alloc(4097, 1, run, |_| ());
        assert_eq!(result, 17);
        assert!(freeing);
    }

    #[test]
    fn allocation_count() {
        let (value, count) = large_allocs(4096, || std::hint::black_box(vec![7; 8192]));
        assert_eq!(value.len(), 8192);
        assert!(count > 0, "large allocation was not observed");
        drop(value);
        let (_, count) = large_allocs(4096, || std::hint::black_box(vec![7; 128]));
        assert_eq!(count, 0);
        let failed = std::panic::catch_unwind(|| large_allocs(4096, || panic!("watch interrupted")));
        assert!(failed.is_err());
        assert_eq!(MIN_SIZE.with(Cell::get), usize::MAX);
        let (_, count) = large_allocs(4096, || ());
        assert_eq!(count, 0);
    }

    #[test]
    fn allocation_peer() {
        let (_, count) = large_allocs(4096, || {
            std::thread::spawn(|| std::hint::black_box(vec![7; 8192])).join().unwrap()
        });
        assert_eq!(count, 0, "peer allocation changed observation");
    }

    #[test]
    fn memory_lifetime() {
        let pool = Pool::new(6);
        let value = Arc::new(Value::charged(b"abc".to_vec(), pool.take(3).unwrap()).unwrap());
        let reader = value.clone();
        assert_eq!(pool.used(), 3);
        drop(value);
        assert_eq!(pool.used(), 3);
        let other = Value::charged(b"def".to_vec(), pool.take(3).unwrap()).unwrap();
        assert!(pool.take(1).is_err());
        assert_eq!(reader.as_slice(), b"abc");
        drop(reader);
        assert_eq!(pool.used(), 3);
        drop(other);
        assert_eq!(pool.used(), 0);
        assert!(Value::charged(vec![0; 4], pool.take(3).unwrap()).is_err());
        assert_eq!(pool.used(), 0);
        let pool = Pool::new(usize::MAX);
        let full = pool.take(usize::MAX).unwrap();
        assert!(pool.take(1).is_err());
        drop(full);
        assert_eq!(pool.used(), 0);
    }

    #[test]
    fn tree_limits() {
        let root = Pool::new(10);
        let first = root.child(8);
        let second = root.child(8);
        let leaf = first.child(7);
        let held = leaf.take(6).unwrap();
        let peer = second.take(4).unwrap();
        assert_eq!((root.used(), first.used(), leaf.used(), second.used()), (10, 6, 6, 4));
        assert!(leaf.take(1).is_err());
        assert!(first.take(3).is_err());
        assert!(root.take(1).is_err());
        assert_eq!((root.used(), first.used(), leaf.used(), second.used()), (10, 6, 6, 4));
        drop(peer);
        assert!(leaf.take(2).is_err());
        assert_eq!((root.used(), first.used(), leaf.used()), (6, 6, 6));
        drop(held);
        assert_eq!((root.used(), first.used(), leaf.used()), (0, 0, 0));
        let root = Pool::new(usize::MAX);
        let leaf = root.child(usize::MAX);
        let held = root.take(usize::MAX).unwrap();
        assert!(leaf.take(1).is_err());
        assert_eq!(leaf.used(), 0);
        drop(held);
        assert_eq!(root.used(), 0);
    }

    #[test]
    fn tree_resize() {
        let root = Pool::new(12);
        let child = root.child(10);
        let mut first = child.take(4).unwrap();
        let mut second = child.take(3).unwrap();
        let peer = root.take(5).unwrap();
        assert!(first.resize(5).is_err());
        assert_eq!((first.size(), child.used(), root.used()), (4, 7, 12));
        first.resize(2).unwrap();
        assert_eq!((child.used(), root.used()), (5, 10));
        first.join(&mut second).unwrap();
        assert_eq!((first.size(), second.size(), child.used(), root.used()), (5, 0, 5, 10));
        drop(second);
        first.resize(7).unwrap();
        assert_eq!((child.used(), root.used()), (7, 12));
        drop(peer);
        first.resize(10).unwrap();
        assert!(first.resize(11).is_err());
        assert_eq!((child.used(), root.used()), (10, 10));
        let mut other = root.take(1).unwrap();
        assert!(first.join(&mut other).is_err());
        assert_eq!((child.used(), root.used()), (10, 11));
        drop(other);
        drop(first);
        assert_eq!((child.used(), root.used()), (0, 0));
    }

    #[test]
    fn tree_parallel() {
        let root = Pool::new(9);
        let ready = Arc::new(std::sync::Barrier::new(4));
        let held = Arc::new(std::sync::Barrier::new(4));
        let release = Arc::new(std::sync::Barrier::new(4));
        let workers: Vec<_> = (0..3).map(|_| {
            let child = root.child(9);
            let ready = ready.clone();
            let held = held.clone();
            let release = release.clone();
            std::thread::spawn(move || {
                ready.wait();
                let lease = child.take(6).ok();
                held.wait();
                release.wait();
                let accepted = lease.is_some();
                drop(lease);
                assert_eq!(child.used(), 0);
                accepted
            })
        }).collect();
        ready.wait();
        held.wait();
        let used = root.used();
        release.wait();
        let count = workers.into_iter()
            .map(|worker| usize::from(worker.join().unwrap())).sum::<usize>();
        assert_eq!((used, count), (6, 1));
        assert_eq!(root.used(), 0);
    }

    #[test]
    fn tree_poison() {
        for parent in [false, true] {
            let root = Pool::new(10);
            let child = root.child(8);
            let lease = child.take(5).unwrap();
            let target = if parent { &root } else { &child };
            let result = std::panic::catch_unwind(|| {
                let _guard = target.usage.lock().unwrap();
                panic!("owner interrupted");
            });
            assert!(result.is_err());
            assert!(child.take(1).is_err());
            assert_eq!((root.used(), child.used()), (5, 5));
            drop(lease);
            assert_eq!((root.used(), child.used()), (0, 0));
            assert!(child.take(1).is_err());
            assert_eq!(root.take(1).is_err(), parent);
            assert_eq!((root.used(), child.used()), (0, 0));
        }
    }

    #[test]
    fn tree_lifetime() {
        let root = Pool::new(8194);
        let child = root.child(4097);
        let source = child.clone();
        let (_, freed) = paused_alloc(4097, 1, move || {
            let lease = source.take(4097).unwrap();
            let value = Arc::new(Value::charged(vec![7_u8; 4097], lease).unwrap());
            let reader = value.clone();
            drop(value);
            assert_eq!(reader.len(), 4097);
            drop(reader);
        }, |_| {
            assert_eq!((root.used(), child.used()), (4097, 4097));
        });
        assert!(freed);
        assert_eq!((root.used(), child.used()), (0, 0));
    }

    #[test]
    fn append_peak() {
        let pool = Pool::new(20_000);
        let source = pool.clone();
        let ((), freeing) = paused_alloc(8194, 1, move || {
            let mut value = Value::default();
            value.append(Some(&source), &[7; 4097], 16_384).unwrap();
            assert_eq!(source.used(), 4097);
            value.append(Some(&source), &[9; 4097], 16_384).unwrap();
            assert_eq!(&value[..4097], &[7; 4097]);
            assert_eq!(&value[4097..], &[9; 4097]);
            assert_eq!(source.used(), 8194);
            drop(value);
        }, |phase| {
            let expected = if phase == AllocPhase::Allocated { 12_291 } else { 8194 };
            assert_eq!(pool.used(), expected, "response reservation lifetime differs");
        });
        assert!(freeing);
        assert_eq!(pool.used(), 0);
    }

    #[test]
    fn append_failures() {
        let pool = Pool::new(12_000);
        let mut value = Value::default();
        value.append(Some(&pool), &[7; 4097], 16_384).unwrap();
        let (result, count) = large_allocs(4097,
            || value.append(Some(&pool), &[9; 4097], 16_384));
        assert_eq!(result, Err("compute memory busy"));
        assert_eq!(count, 0);
        assert_eq!(value.as_slice(), &[7; 4097]);
        assert_eq!(pool.used(), 4097);
        assert_eq!(value.append(None, &[9], 16_384), Err("response memory owner missing"));
        value.clear();
        assert_eq!(pool.used(), 0);
        assert_eq!(value.append(Some(&pool), &[7], 0), Err("response exceeds limit"));
        assert!(value.is_empty());
        value.append(Some(&pool), &[], 0).unwrap();
        assert_eq!(pool.used(), 0);
        let result = refused_alloc(4097,
            || value.append(Some(&pool), &[7; 4097], 16_384));
        assert_eq!(result, Err("response allocation failed"));
        assert_eq!(pool.used(), 0);
        assert!(value.is_empty());
        let pool = Pool::new(20_000);
        value.append(Some(&pool), &[7; 4097], 16_384).unwrap();
        let result = refused_alloc(8194,
            || value.append(Some(&pool), &[9; 4097], 16_384));
        assert_eq!(result, Err("response allocation failed"));
        assert_eq!(value.as_slice(), &[7; 4097]);
        assert_eq!(pool.used(), 4097);
        drop(value);
        assert_eq!(pool.used(), 0);
    }

    #[test]
    fn append_owners() {
        let first = Pool::new(4096);
        let second = Pool::new(4096);
        let mut value: Value = b"a".to_vec().into();
        value.append(Some(&first), b"b", 4096).unwrap();
        assert_eq!(value.as_slice(), b"ab");
        assert!(first.used() >= value.len());
        value.append(Some(&second), b"c", 4096).unwrap();
        assert_eq!(value.as_slice(), b"abc");
        assert_eq!(first.used(), 0);
        assert!(second.used() >= value.len());
        value.clear();
        assert_eq!(second.used(), 0);
    }

    #[test]
    fn append_growth() {
        let pool = Pool::new(4096);
        let mut value = Value::default();
        let mut capacities = Vec::new();
        for _ in 0..1024 {
            let before = pool.used();
            value.append(Some(&pool), b"x", 1024).unwrap();
            if pool.used() != before { capacities.push(pool.used()); }
        }
        assert_eq!(capacities, [256, 512, 1024]);
        assert_eq!(value.len(), 1024);
        assert_eq!(value.append(Some(&pool), b"x", 1024), Err("response exceeds limit"));
        assert_eq!(pool.used(), 1024);
        value.clear();
        assert_eq!(pool.used(), 0);
    }

    #[test]
    fn memory_parallel() {
        let pool = Pool::new(12);
        let ready = Arc::new(std::sync::Barrier::new(9));
        let release = Arc::new(std::sync::Barrier::new(9));
        let workers: Vec<_> = (0..8).map(|_| {
            let pool = pool.clone();
            let ready = ready.clone();
            let release = release.clone();
            std::thread::spawn(move || {
                let lease = pool.take(3);
                ready.wait();
                release.wait();
                lease.is_ok()
            })
        }).collect();
        ready.wait();
        assert_eq!(pool.used(), 12);
        release.wait();
        assert_eq!(workers.into_iter().map(|worker| worker.join().unwrap()).filter(|held| *held).count(), 4);
        assert_eq!(pool.used(), 0);
    }

    #[test]
    fn typed_memory() {
        let pool = Pool::new(24);
        let mut value = Value::charged(vec![1_f64, 2.0], pool.take(16).unwrap()).unwrap();
        value[1] = 3.0;
        assert_eq!(value.as_slice(), &[1.0, 3.0]);
        assert_eq!(pool.used(), 16);
        assert!(Value::charged(vec![1_f32, 2.0], pool.take(4).unwrap()).is_err());
        assert_eq!(pool.used(), 16);
        drop(value);
        assert_eq!(pool.used(), 0);
    }

    #[test]
    fn memory_resize() {
        let pool = Pool::new(12);
        let other = pool.take(4).unwrap();
        let mut lease = pool.take(2).unwrap();
        lease.resize(8).unwrap();
        assert_eq!(pool.used(), 12);
        assert_eq!(lease.size(), 8);
        assert!(lease.resize(9).is_err());
        assert_eq!(pool.used(), 12);
        assert_eq!(lease.size(), 8);
        lease.resize(3).unwrap();
        assert_eq!(pool.used(), 7);
        drop(lease);
        assert_eq!(pool.used(), 4);
        drop(other);
        assert_eq!(pool.used(), 0);
        let pool = Pool::new(usize::MAX);
        let held = pool.take(1).unwrap();
        let mut lease = pool.take(0).unwrap();
        assert!(lease.resize(usize::MAX).is_err());
        assert_eq!(lease.size(), 0);
        assert_eq!(pool.used(), 1);
        drop(held);
        lease.resize(usize::MAX).unwrap();
        lease.resize(0).unwrap();
        assert_eq!(pool.used(), 0);
    }

    #[test]
    fn memory_join() {
        let pool = Pool::new(12);
        let mut first = pool.take(4).unwrap();
        let mut second = pool.take(8).unwrap();
        first.join(&mut second).unwrap();
        assert_eq!(second.size(), 0);
        assert_eq!(first.size(), 12);
        assert_eq!(pool.used(), 12);
        assert!(pool.take(1).is_err());
        let other = Pool::new(7);
        let mut third = other.take(7).unwrap();
        assert_eq!(first.join(&mut third).unwrap_err(),
            "compute memory owners differ");
        assert_eq!(first.size(), 12);
        assert_eq!(pool.used(), 12);
        assert_eq!(other.used(), 7);
        drop(third);
        assert_eq!(other.used(), 0);
        first.resize(3).unwrap();
        assert_eq!(pool.used(), 3);
        drop(first);
        assert_eq!(pool.used(), 0);
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