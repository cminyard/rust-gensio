// Copyright 2023 Corey Minyard
//
// SPDX-License-Identifier: Apache-2.0

use std::sync::Arc;
use std::sync::Mutex;
use std::time::Duration;
use std::ffi;
pub mod raw;

/// Used to refcount gensio_os_funcs.
pub struct IOsFuncs {
    log_data: *mut GensioLogHandlerData,
    pub o: *const raw::gensio_os_funcs
}

impl Drop for IOsFuncs {
    fn drop(&mut self) {
	unsafe {
	    if self.log_data != std::ptr::null_mut() {
		drop(Box::from_raw(self.log_data));
	    }
	    raw::gensio_rust_cleanup(self.o);
	    raw::gensio_os_funcs_free(self.o);
	}
    }
}

/// Os Handling functions for gensio You need one of these to do
/// pretty much anything with gensio.
pub struct OsFuncs {
    o: Arc<IOsFuncs>,
    proc_data: *const raw::gensio_os_proc_data,
}

/// Allocate an OsFuncs structure.  This takes a log handler for
/// handling internal logs from gensios and osfuncs.
pub fn new(log_func: Arc<dyn GensioLogHandler>) -> Result<Arc<OsFuncs>, i32> {
    let err;
    let o: *const raw::gensio_os_funcs = std::ptr::null();

    unsafe {
	err = raw::gensio_alloc_os_funcs(-198234, &o);
    }
    match err {
	0 => {
	    let d = Box::new(GensioLogHandlerData { cb: log_func });
	    let d = Box::into_raw(d);
	    unsafe {
		raw::gensio_rust_set_log(o, log_handler,
					 d as *mut ffi::c_void);
	    }
		let rv = Arc::new(
		    OsFuncs { o: Arc::new(IOsFuncs {log_data: d, o: o}),
			      proc_data: std::ptr::null()});
	    Ok(rv)
	}
	_ => Err(err)
    }
}

/// Used for OsFuncs to handle logs.
pub trait GensioLogHandler {
    /// Used to report internal logs from the system that couldn't be
    /// propagated back other ways.
    fn log(&self, s: String); // FIXME = make this &mut
}

struct GensioLogHandlerData {
    cb: Arc<dyn GensioLogHandler>
}

extern "C" fn log_handler(log: *const ffi::c_char,
			  data: *mut ffi::c_void) {
    let d = data as *mut GensioLogHandlerData;
    let s = unsafe { ffi::CStr::from_ptr(log) };
    let s = s.to_str().expect("Invalid log string").to_string();

    unsafe { (*d).cb.log(s); }
}

impl OsFuncs {
    /// Called to setup the task (signals, shutdown handling, etc.)
    /// for a process.  This should be called on the first OsFuncs
    /// (and only the first one, this should only be called once
    /// unless all OsFucns have been freed and a new one allocated)
    /// and that OsFuncs should be kept around until you are done with
    /// all other OsFuncs.  You almost certainly should call this.
    /// The cleanup function is called automatically as part of the
    /// OsFuncs automatic cleanup.
    pub fn proc_setup(&self) -> Result<(), i32> {
	let err = unsafe { raw::gensio_os_proc_setup(self.o.o,
						     &self.proc_data) };
	match err {
	    0 => Ok(()),
	    _ => Err(err)
	}
    }

    /// Allocate a new Waiter object for the OsFuncs.
    pub fn new_waiter(&self) -> Option<Waiter> {
	let w;

	unsafe {
	    w = raw::gensio_os_funcs_alloc_waiter(self.o.o);
	}
	if w == std::ptr::null() {
	    None
	} else {
	    Some(Waiter { o: self.o.clone() , w: w })
	}
    }

    /// Allocate a new Timer object for the OsFuncs.
    pub fn new_timer(&self, handler: Arc<dyn TimeoutHandler>)
		     -> Option<Timer> {
	let w;
	unsafe {
	    w = raw::gensio_os_funcs_alloc_waiter(self.o.o);
	}
	let d = Box::new(TimerData { o: self.o.clone(),
				     handler: handler, freed: false,
				     stopping: false,
				     w: w, m: Mutex::new(0) });
	let d = Box::into_raw(d);
	let t;
	unsafe {
	    t = raw::gensio_os_funcs_alloc_timer(self.o.o, timeout_handler,
						 d as *mut ffi::c_void);
	}

	if t == std::ptr::null() {
	    unsafe { drop(Box::from_raw(d)); }
	    return None
	}
	Some(Timer { t: t, d: d })
    }

    /// Get a reference to the os_funcs that we can keep and use.  For
    /// internal use only.
    pub fn raw(&self) -> Arc<IOsFuncs> {
	self.o.clone()
    }
}

impl Drop for OsFuncs {
    fn drop(&mut self) {
	unsafe {
	    if self.proc_data != std::ptr::null() {
		raw::gensio_os_proc_cleanup(self.proc_data);
	    }
	    // o will be freed by the Arc<>
	}
    }
}

/// A type used to wait for things to complete.  The wait call will do
/// gensio background processing as you would expect.
pub struct Waiter {
    o: Arc<IOsFuncs>,
    w: *const raw::gensio_waiter
}

impl Waiter {
    /// Decrement the wakeup count on a wait() call.  When the count
    /// reaches 0, that function will return success.
    pub fn wake(&self) -> Result<(), i32> {
	let err = unsafe { raw::gensio_os_funcs_wake(self.o.o, self.w) };
	match err {
	    0 => Ok(()),
	    _ => Err(err)
	}
    }

    /// Wait for a given number of wake calls to occur, or a timeout.
    /// If that many wake calls occur, this returns success with a
    /// duration of how much time is left.  On a timeout it returns a
    /// GE_TIMEDOUT error.  Other errors may occur.
    pub fn wait(&self, count: u32, timeout: &Duration)
		-> Result<Duration, i32> {
	let t = raw::gensio_time{ secs: timeout.as_secs() as i64,
				  nsecs: timeout.subsec_nanos() as i32 };
	let err = unsafe { raw::gensio_os_funcs_wait(self.o.o, self.w,
						     count, &t) };
	match err {
	    0 => Ok(Duration::new(t.secs as u64, t.nsecs as u32)),
	    _ => Err(err)
	}
    }

    /// Like wait, but if a signal is received, this will return a
    /// GE_INTERRUPTED error.
    pub fn wait_intr(&self, count: u32, timeout: &Duration)
		-> Result<Duration, i32> {
	let t = raw::gensio_time{ secs: timeout.as_secs() as i64,
				  nsecs: timeout.subsec_nanos() as i32 };
	let err = unsafe { raw::gensio_os_funcs_wait_intr(self.o.o, self.w,
							  count, &t) };
	match err {
	    0 => Ok(Duration::new(t.secs as u64, t.nsecs as u32)),
	    _ => Err(err)
	}
    }
}

impl Drop for Waiter {
    fn drop(&mut self) {
	unsafe {
	    raw::gensio_os_funcs_free_waiter(self.o.o, self.w);
	}
    }
}

/// Timer callbacks will need to implement this trait.
pub trait TimeoutHandler {
    /// Report that the operation (close) has completed.
    fn timeout(&self); // FIXME = make this &mut
}

/// Timer stop done callbacks will need to implement this trait.
pub trait TimerStopDoneHandler {
    /// Report that the operation (close) has completed.
    fn timer_stopped(&mut self);
}

struct TimerData {
    o: Arc<IOsFuncs>,
    handler: Arc<dyn TimeoutHandler>,
    stopping: bool,
    freed: bool,
    w: *const raw::gensio_waiter, // Waits for the timer to stop
    m: Mutex<u32>
}

impl Drop for TimerData {
    fn drop(&mut self) {
	unsafe {
	    raw::gensio_os_funcs_free_waiter(self.o.o, self.w);
	}
    }
}

pub struct Timer {
    t: *const raw::gensio_timer,
    d: *mut TimerData
}

extern "C" fn timeout_handler(_t: *const raw::gensio_timer,
			      cb_data: *mut ffi::c_void) {
    let d = cb_data as *mut TimerData;
    unsafe { (*d).handler.timeout() };
}

struct TimerStopData<'a> {
    cb: Arc<&'a mut dyn TimerStopDoneHandler>,
    d: *mut TimerData
}

extern "C" fn timer_stopped_handler(_t: *const raw::gensio_timer,
				    cb_data: *mut ffi::c_void) {
    let d = cb_data as *mut TimerStopData;
    unsafe {
	let d2 = (*d).d;
	{
	    let _guard = (*d2).m.lock().unwrap();
	    (*d2).stopping = false;
	}

	// FIXME - This is a big hack
	//let cb = Arc::get_mut_unchecked(&mut (*d).cb);
	let cb = Arc::as_ptr(&mut (*d).cb);
	let cb = cb as *mut &mut dyn TimerStopDoneHandler;
	(*cb).timer_stopped();

	let _guard = (*d2).m.lock().unwrap();
	if (*d2).freed {
	    raw::gensio_os_funcs_wake((*d2).o.o, (*d2).w);
	}
	drop(Box::from_raw(d));
    }
}

extern "C" fn timer_freed_handler(_t: *const raw::gensio_timer,
				  cb_data: *mut ffi::c_void) {
    let d = cb_data as *mut TimerData;

    unsafe {
	raw::gensio_os_funcs_wake((*d).o.o, (*d).w);
    }
}

impl Timer {
    /// Start timer relative
    pub fn start(&self, timeout: &Duration) -> Result<(), i32> {
	let t = raw::gensio_time{ secs: timeout.as_secs() as i64,
				  nsecs: timeout.subsec_nanos() as i32 };
	let err = unsafe {
	    raw::gensio_os_funcs_start_timer((*self.d).o.o, self.t, &t)
	};
	match err {
	    0 => Ok(()),
	    _ => Err(err)
	}
    }

    /// Stop a timer
    pub fn stop(&self) -> Result<(), i32> {
	let err = unsafe {
	    raw::gensio_os_funcs_stop_timer((*self.d).o.o, self.t)
	};
	match err {
	    0 => Ok(()),
	    _ => Err(err)
	}
    }

    /// Stop a timer
    pub fn stop_with_done<'a>(&self, cb: Arc<&'a mut dyn TimerStopDoneHandler>)
			  -> Result<(), i32> {
	unsafe {
	    let _guard = (*self.d).m.lock().unwrap();

	    if (*self.d).freed {
		return Err(crate::GE_NOTREADY)
	    }
	    if (*self.d).stopping {
		return Err(crate::GE_INUSE)
	    }
	    let d = Box::new(TimerStopData { cb : cb, d: self.d });
	    let d = Box::into_raw(d);
	    let err = raw::gensio_os_funcs_stop_timer_with_done(
		(*self.d).o.o, self.t,
		timer_stopped_handler, d as *mut ffi::c_void);
	    match err {
		0 => {
		    (*self.d).stopping = true;
		    Ok(())
		}
		_ => {
		    drop(Box::from_raw(d));
		    Err(err)
		}
	    }
	}
    }
}

impl Drop for Timer {
    // The working of freeing a timer is subtle.  The timer can be in
    // a number of states when this is happening.
    //
    // If the timer is running or in the timer callback,
    // stop_timer_with_done() will return 0, meaning that the done
    // function will be called.
    //
    // If the timer is not running but there is already a stop pending,
    // stop_timer_with_done() will return we still want to do that stop, 
    fn drop(&mut self) {
	unsafe {
	    let mut do_wait = false;
	    {
		let _guard = (*self.d).m.lock().unwrap();

		let err = raw::gensio_os_funcs_stop_timer_with_done
		    ((*self.d).o.o, self.t,
		     timer_freed_handler, self.d as *mut ffi::c_void);
		match err {
		    crate::GE_TIMEDOUT => { // Timer is not running
			if (*self.d).stopping {
			    // Stop done handler is pending and still
			    // hasn't run, so make sure it does the
			    // wake.
			    (*self.d).freed = true;
			    do_wait = true;
			}
		    }
		    0 => {
			// Timer was running and is now stopped, the
			// stop done handler will be called.
			do_wait = true;
		    }
		    crate::GE_INUSE => {
			// Timer has a stop done handler pending, set
			// it up to wake us when done.
			(*self.d).freed = true;
			do_wait = true;
		    },
		    _ => () // Shouldn't happen...
		}
	    }
	    if do_wait {
		raw::gensio_os_funcs_wait((*self.d).o.o, (*self.d).w,
					  1, std::ptr::null());
	    }
	    raw::gensio_os_funcs_free_timer((*self.d).o.o, self.t);
	    drop(Box::from_raw(self.d));
	}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serial_test::serial;

    struct LogHandler;

    impl GensioLogHandler for LogHandler {
	fn log(&self, _logstr: String) {
	    
	}
    }

    #[test]
    #[serial]
    fn wait_test() {
	let o = new(Arc::new(LogHandler)).expect("Couldn't allocate OsFuncs");
	o.proc_setup().expect("Couldn't set up OsFuncs");
	let w = o.new_waiter().expect("Couldn't allocate Waiter");

	drop(o);
	assert_eq!(w.wait(1, &Duration::new(0, 0)), Err(crate::GE_TIMEDOUT));
    }

    struct HandleTimeout1 {
	w: Waiter,
    }
    impl TimeoutHandler for HandleTimeout1 {
	fn timeout(&self) {
	    self.w.wake().expect("Wake failed");
	}
    }

    // Normal timer operation, wait for it to time out.
    #[test]
    #[serial]
    fn timer_test() {
	let o = new(Arc::new(LogHandler)).expect("Couldn't allocate OsFuncs");
	o.proc_setup().expect("Couldn't set up OsFuncs");
	let h = Arc::new(HandleTimeout1 {
	    w: o.new_waiter().expect("Couldn't allocate Waiter"),
	});
	let t = o.new_timer(h.clone()).expect("Couldn't allocate Timer");

	t.start(&Duration::new(0, 1000)).expect("Couldn't start timer");

	match h.w.wait(1, &Duration::new(1, 0)) {
	    Ok(_) => (),
	    Err(e) => assert!(e != 0)
	}
    }

    // See that the cleanup happens on a running timer
    #[test]
    #[serial]
    fn timer_test2() {
	let o = new(Arc::new(LogHandler)).expect("Couldn't allocate OsFuncs");
	o.proc_setup().expect("Couldn't set up OsFuncs");
	let h = Arc::new(HandleTimeout1 {
	    w: o.new_waiter().expect("Couldn't allocate Waiter"),
	});
	let t = o.new_timer(h.clone()).expect("Couldn't allocate Timer");

	t.start(&Duration::new(100, 0)).expect("Couldn't start timer");
    }

    struct StopTimer1 {
	w: Waiter,
    }
    impl TimerStopDoneHandler for StopTimer1 {
	fn timer_stopped(&mut self) {
	    self.w.wake().expect("Wake failed");
	}
    }

    // Stop the timer and wait for it
    #[test]
    #[serial]
    fn timer_test3() {
	let o = new(Arc::new(LogHandler)).expect("Couldn't allocate OsFuncs");
	o.proc_setup().expect("Couldn't set up OsFuncs");
	let h = Arc::new(HandleTimeout1 {
	    w: o.new_waiter().expect("Couldn't allocate Waiter"),
	});
	let mut s1 = StopTimer1 {
	    w: o.new_waiter().expect("Couldn't allocate Waiter"),
	};
	let s: Arc<&mut dyn TimerStopDoneHandler> = Arc::new(&mut s1);
	let t = o.new_timer(h.clone()).expect("Couldn't allocate Timer");

	t.start(&Duration::new(100, 0)).expect("Couldn't start timer");
	t.stop_with_done(s.clone()).expect("Couldn't stop timer");
	match s1.w.wait(1, &Duration::new(1, 0)) {
	    Ok(_) => (),
	    Err(e) => assert!(e != 0)
	}
    }

    // See that the cleanup works properly on a timer that is being
    // stopped.
    #[test]
    #[serial]
    fn timer_test4() {
	let o = new(Arc::new(LogHandler)).expect("Couldn't allocate OsFuncs");
	o.proc_setup().expect("Couldn't set up OsFuncs");
	let h = Arc::new(HandleTimeout1 {
	    w: o.new_waiter().expect("Couldn't allocate Waiter"),
	});
	let mut s1 = StopTimer1 {
	    w: o.new_waiter().expect("Couldn't allocate Waiter"),
	};
	let s: Arc<&mut dyn TimerStopDoneHandler> = Arc::new(&mut s1);
	let t = o.new_timer(h.clone()).expect("Couldn't allocate Timer");

	t.start(&Duration::new(100, 0)).expect("Couldn't start timer");
	t.stop_with_done(s.clone()).expect("Couldn't stop timer");
    }
}
