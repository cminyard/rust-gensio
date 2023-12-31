// Copyright 2023 Corey Minyard
//
// SPDX-License-Identifier: Apache-2.0

use std::sync::Arc;
use std::sync::Mutex;
use std::sync::Weak;
use std::time::Duration;
use std::ffi;
use std::panic;
use crate::Error;
use crate::val_to_error;
pub mod raw;

// Used to refcount gensio_os_funcs.
struct IOsFuncs {
    log_data: *mut GensioLogHandlerData,
    pub o: *const raw::gensio_os_funcs,
    proc_data: Mutex<*const raw::gensio_os_proc_data>,
    term_handler: Arc<GensioTermHandlerData>,
    hup_handler: Arc<GensioHupHandlerData>,
    winsize_handler: Arc<GensioWinsizeHandlerData>,
}

// This is thread safe.
unsafe impl Sync for IOsFuncs {}
unsafe impl Send for IOsFuncs {}

impl Drop for IOsFuncs {
    fn drop(&mut self) {
        let proc_data = self.proc_data.lock().unwrap();
        let proc_data = *proc_data;
        if !proc_data.is_null() {
            unsafe {
		raw::gensio_os_proc_cleanup(proc_data);
	    }
        }

        unsafe {
	    if !self.log_data.is_null() {
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
}

/// For flags on the new function, allocate mutexes (internal an
/// external) with priority inheritance.  Not supported on all
/// platforms.
pub const GENSIO_OS_FUNCS_FLAG_PRIO_INHERIT: u32 = 1 << 0;

/// Used for OsFuncs to handle logs.
/// Note: You must keep this object around because it is stored as Weak.
/// If you allow this object to be done, the callbacks will stop working.
pub trait GensioLogHandler {
    /// Used to report internal logs from the system that couldn't be
    /// propagated back other ways.
    fn log(&self, s: String);
}

struct GensioLogHandlerData {
    cb: Weak<dyn GensioLogHandler>
}

fn i_log_handler(log: *const ffi::c_char,
		 data: *mut ffi::c_void) {
    let d = data as *mut GensioLogHandlerData;
    let cb = unsafe { (*d).cb.upgrade() };
    let cb = match cb {
	None => return,
	Some(cb) => cb
    };
    let s = unsafe { ffi::CStr::from_ptr(log) };
    let s = s.to_str().expect("Invalid log string").to_string();

    cb.log(s)
}

extern "C" fn log_handler(log: *const ffi::c_char,
			  data: *mut ffi::c_void) {
    let _r = panic::catch_unwind(|| {
	i_log_handler(log, data)
    });
}

/// Used to report a termination signal, Windows or Unix
/// Note: You must keep this object around because it is stored as Weak.
/// If you allow this object to be done, the callbacks will stop working.
pub trait GensioTermHandler {
    fn term_sig(&self);
}

struct GensioTermHandlerData {
    cb: Mutex<Option<Weak<dyn GensioTermHandler>>>
}

// FIXME - clippy complains about this type not implementing Send and
// Sync, and says to wrap the inner part with a Mutex.  Huh?
unsafe impl Send for GensioTermHandlerData {}
unsafe impl Sync for GensioTermHandlerData {}

fn i_term_handler(data: *mut ffi::c_void) {
    let d = data as *mut GensioTermHandlerData;
    let cb = match *unsafe {(*d).cb.lock().unwrap() } {
        None => return,
        Some(ref icb) => icb.upgrade()
    };
    match cb {
        None => (),
        Some(cb) => cb.term_sig()
    }
}

extern "C" fn term_handler(data: *mut ffi::c_void) {
    let _r = panic::catch_unwind(|| {
	i_term_handler(data)
    });
}

/// Used to report a hangup signal, Unix
/// Note: You must keep this object around because it is stored as Weak.
/// If you allow this object to be done, the callbacks will stop working.
pub trait GensioHupHandler {
    fn hup_sig(&self);
}

struct GensioHupHandlerData {
    cb: Mutex<Option<Weak<dyn GensioHupHandler>>>
}

// FIXME - clippy complains about this type not implementing Send and
// Sync, and says to wrap the inner part with a Mutex.  Huh?
unsafe impl Send for GensioHupHandlerData {}
unsafe impl Sync for GensioHupHandlerData {}

fn i_hup_handler(data: *mut ffi::c_void) {
    let d = data as *mut GensioHupHandlerData;
    let cb = match *unsafe {(*d).cb.lock().unwrap() } {
        None => return,
        Some(ref icb) => icb.upgrade()
    };
    match cb {
        None => (),
        Some(cb) => cb.hup_sig()
    }
}

extern "C" fn hup_handler(data: *mut ffi::c_void) {
    let _r = panic::catch_unwind(|| {
	i_hup_handler(data)
    });
}

/// Used to report a window size signal, Unix and Windows
/// Note: You must keep this object around because it is stored as Weak.
/// If you allow this object to be done, the callbacks will stop working.
pub trait GensioWinsizeHandler {
    fn winsize_sig(&self, x_chrs: i32, y_chrs: i32, x_bits: i32, y_bits: i32);
}

struct GensioWinsizeHandlerData {
    cb: Mutex<Option<Weak<dyn GensioWinsizeHandler>>>
}

// FIXME - clippy complains about this type not implementing Send and
// Sync, and says to wrap the inner part with a Mutex.  Huh?
unsafe impl Send for GensioWinsizeHandlerData {}
unsafe impl Sync for GensioWinsizeHandlerData {}

fn i_winsize_handler(x_chrs: i32, y_chrs: i32, x_bits: i32, y_bits: i32,
                     data: *mut ffi::c_void) {
    let d = data as *mut GensioWinsizeHandlerData;
    let cb = match *unsafe {(*d).cb.lock().unwrap() } {
        None => return,
        Some(ref icb) => icb.upgrade()
    };
    match cb {
        None => (),
        Some(cb) => cb.winsize_sig(x_chrs, y_chrs, x_bits, y_bits)
    }
}

extern "C" fn winsize_handler(x_chrs: i32, y_chrs: i32,
			      x_bits: i32, y_bits: i32,
                              data: *mut ffi::c_void) {
    let _r = panic::catch_unwind(|| {
	i_winsize_handler(x_chrs, y_chrs, x_bits, y_bits, data)
    });
}

impl OsFuncs {
    /// Allocate an OsFuncs structure.  This takes a log handler for
    /// handling internal logs from gensios and osfuncs.  Set the flags to
    /// 0, generally.  There is a priority inheritance flag if you need
    /// that.
    pub fn new_flags(log_func: Weak<dyn GensioLogHandler>,
		     flags: u32) -> Result<Self, Error> {
        let err;
        let o: *const raw::gensio_os_funcs = std::ptr::null();

        unsafe {
	    err = raw::gensio_alloc_os_funcs(-198234, &o, flags as ffi::c_uint);
        }
        match err {
	    0 => {
	        let d = Box::new(GensioLogHandlerData { cb: log_func });
	        let proc_data = Mutex::new(std::ptr::null());
	        let term_handler = Arc::new(GensioTermHandlerData
					    {cb: Mutex::new(None)});
	        let winsize_handler = Arc::new(GensioWinsizeHandlerData
                                               {cb: Mutex::new(None)});
	        let hup_handler = Arc::new(GensioHupHandlerData
                                           {cb: Mutex::new(None)});
	        let d = Box::into_raw(d);
	        // Nothing below here can panic, so it's safe to convert
	        // the Box to raw here.
	        unsafe {
		    raw::gensio_rust_set_log(o, log_handler,
					     d as *mut ffi::c_void);
	        }
                let ios = IOsFuncs {
                    log_data: d, o,
		    proc_data,
                    term_handler,
                    winsize_handler,
                    hup_handler,
                };
                Ok(OsFuncs { o: Arc::new(ios) })
	    }
	    _ => Err(val_to_error(err))
        }
    }

    /// Allocate an OsFuncs structure.  This takes a log handler for
    /// handling internal logs from gensios and osfuncs.
    pub fn new(log_func: Weak<dyn GensioLogHandler>) -> Result<Self, Error> {
        Self::new_flags(log_func, 0)
    }

    /// Called to setup the task (signals, shutdown handling, etc.)
    /// for a process.  This should be called on the first OsFuncs
    /// (and only the first one, this should only be called once
    /// unless all OsFucns have been freed and a new one allocated)
    /// and that OsFuncs should be kept around until you are done with
    /// all other OsFuncs.  You almost certainly should call this.
    /// The cleanup function is called automatically as part of the
    /// OsFuncs automatic cleanup.
    pub fn proc_setup(&self) -> Result<(), Error> {
        let mut proc_data = self.o.proc_data.lock().unwrap();
        let new_proc_data: *const raw::gensio_os_proc_data = std::ptr::null();
	let err = unsafe { raw::gensio_os_proc_setup(self.o.o,
						     &new_proc_data) };
	match err {
	    0 => {
                *proc_data = new_proc_data;
                Ok(())
            }
	    _ => Err(val_to_error(err))
	}
    }

    /// Call this to setup a thread with the proper signal handlers
    /// and such to work inside the gensio library.  Any threads you
    /// create through the gensio library will already be correct, and
    /// the proc_setup function does this processing, too.  In general,
    /// and threads you create from a gensio thread using OS operations
    /// will also be set up correctly.
    pub fn thread_setup(&self) -> Result<(), Error> {
	let err = unsafe { raw::gensio_os_thread_setup(self.o.o) };
	match err {
	    0 => Ok(()),
	    _ => Err(val_to_error(err))
	}
    }

    /// Register a callback to be called when a process termination
    /// signal is received.
    pub fn register_term_handler(&self, handler: Weak<dyn GensioTermHandler>)
                                 -> Result<(), Error> {
        let proc_data;
        {
            let l_proc_data = self.o.proc_data.lock().unwrap();
            proc_data = *l_proc_data;
        }
        if proc_data.is_null() {
            return Err(Error::NotReady);
        }

        let mut d = self.o.term_handler.cb.lock().unwrap();
        match *d {
            None => (),
            Some(_) => return Err(Error::InUse)
        }

        *d = Some(handler);
        let err = unsafe {
            raw::gensio_os_proc_register_term_handler(
                proc_data, term_handler,
                Arc::as_ptr(&self.o.term_handler) as *mut ffi::c_void)
        };
        match err {
            0 => Ok(()),
            _ => Err(val_to_error(err))
        }
    }

    /// Register a callback to be called when a SIGHUP is received.
    pub fn register_hup_handler(&self, handler: Weak<dyn GensioHupHandler>)
                                -> Result<(), Error> {
        let proc_data;
        {
            let l_proc_data = self.o.proc_data.lock().unwrap();
            proc_data = *l_proc_data;
        }
        if proc_data.is_null() {
            return Err(Error::NotReady);
        }

        let mut d = self.o.hup_handler.cb.lock().unwrap();
        match *d {
            None => (),
            Some(_) => return Err(Error::InUse)
        }

        *d = Some(handler);
        let err;
        unsafe {
            err = raw::gensio_os_proc_register_reload_handler(
                proc_data, hup_handler,
                Arc::as_ptr(&self.o.hup_handler) as *mut ffi::c_void);
        }
        match err {
            0 => Ok(()),
            _ => Err(val_to_error(err))
        }
    }

    /// # Safety
    /// Register a callback to be called when the console window gets
    /// resized.  The console_iod must be valid.
    pub unsafe fn register_winsize_handler(&self,
				    console_iod: *const raw::gensio_iod,
                                    handler: Weak<dyn GensioWinsizeHandler>)
                                    -> Result<(), Error> {
        let proc_data;
        {
            let l_proc_data = self.o.proc_data.lock().unwrap();
            proc_data = *l_proc_data;
        }
        if proc_data.is_null() {
            return Err(Error::NotReady);
        }

        let mut d = self.o.winsize_handler.cb.lock().unwrap();
        match *d {
            None => (),
            Some(_) => return Err(Error::InUse)
        }

        *d = Some(handler);
        let err;
        unsafe {
            err = raw::gensio_os_proc_register_winsize_handler(
                proc_data, console_iod, winsize_handler,
                Arc::as_ptr(&self.o.winsize_handler) as *mut ffi::c_void);
        }
        match err {
            0 => Ok(()),
            _ => Err(val_to_error(err))
        }
    }

    /// Run the service loop once to handle events.
    pub fn service(&self, timeout: Option<&Duration>)
		   -> Result<Option<Duration>, Error> {
	let t: *const raw::gensio_time;
        let mut to = raw::gensio_time { secs: 0, nsecs: 0 };
        match timeout {
            None => t = std::ptr::null(),
            Some(d) => {
                to.secs = d.as_secs() as i64;
		to.nsecs = d.subsec_nanos() as i32;
                t = &to;
            }
        }
	let err = unsafe { raw::gensio_os_funcs_service(self.o.o, t) };
	match err {
	    0 => {
                if t.is_null() {
                    Ok(None)
                } else {
                    Ok(Some(unsafe {Duration::new((*t).secs as u64,
                                                  (*t).nsecs as u32)}))
                }
            }
	    _ => Err(val_to_error(err))
	}
    }

    /// Get a reference to the os_funcs that we can keep and use.  For
    /// internal use only.
    pub fn raw(&self) -> *const raw::gensio_os_funcs {
	self.o.o
    }

    /// Call gensio_os_funcs_zalloc().  This is really for internal
    /// use only.
    pub fn zalloc(&self, size: usize) -> *mut ffi::c_void {
        unsafe { raw::gensio_os_funcs_zalloc(self.o.o, size as raw::gensiods) }
    }
}

impl Clone for OsFuncs {
    fn clone(&self) -> Self {
        OsFuncs { o: self.o.clone() }
    }
}

/// A type used to wait for things to complete.  The wait call will do
/// gensio background processing as you would expect.
pub struct Waiter {
    o: Arc<IOsFuncs>,
    w: *const raw::gensio_waiter
}

impl Waiter {
    /// Allocate a new Waiter object for the OsFuncs.
    pub fn new(o: &OsFuncs) -> Result<Self, Error> {
	let w;

	unsafe {
	    w = raw::gensio_os_funcs_alloc_waiter(o.raw());
	}
	if w.is_null() {
	    Err(Error::NoMem)
	} else {
	    Ok(Waiter { o: o.o.clone() , w })
	}
    }

    /// Decrement the wakeup count on a wait() call.  When the count
    /// reaches 0, that function will return success.
    pub fn wake(&self) {
	unsafe { raw::gensio_os_funcs_wake(self.o.o, self.w) };
    }

    /// Wait for a given number of wake calls to occur, or a timeout.
    /// If that many wake calls occur, this returns success with a
    /// duration of how much time is left.  On a timeout it returns a
    /// TimedOut error.  Other errors may occur.
    pub fn wait(&self, count: u32, timeout: Option<&Duration>)
		-> Result<Option<Duration>, Error> {
	let t: *const raw::gensio_time;
        let mut to = raw::gensio_time { secs: 0, nsecs: 0 };
        match timeout {
            None => t = std::ptr::null(),
            Some(d) => {
                to.secs = d.as_secs() as i64;
		to.nsecs = d.subsec_nanos() as i32;
                t = &to;
            }
        }
	let err = unsafe { raw::gensio_os_funcs_wait(self.o.o, self.w,
						     count, t) };
	match err {
	    0 => {
                if t.is_null() {
                    Ok(None)
                } else {
                    Ok(Some(unsafe {Duration::new((*t).secs as u64,
                                                  (*t).nsecs as u32)}))
                }
            }
	    _ => Err(val_to_error(err))
	}
    }

    /// Like wait, but if a signal is received, this will return a
    /// Interrupted error.
    pub fn wait_intr(&self, count: u32, timeout: Option<&Duration>)
		     -> Result<Option<Duration>, Error> {
	let t: *const raw::gensio_time;
        let mut to = raw::gensio_time { secs: 0, nsecs: 0 };
        match timeout {
            None => t = std::ptr::null(),
            Some(d) => {
                to.secs = d.as_secs() as i64;
		to.nsecs = d.subsec_nanos() as i32;
                t = &to;
            }
        }
	let err = unsafe { raw::gensio_os_funcs_wait_intr(self.o.o, self.w,
							  count, t) };
	match err {
	    0 => {
                if t.is_null() {
                    Ok(None)
                } else {
                    Ok(Some(unsafe {Duration::new((*t).secs as u64,
                                                  (*t).nsecs as u32)}))
                }
            }
	    _ => Err(val_to_error(err))
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
/// Note: You must keep this object around because it is stored as Weak.
/// If you allow this object to be done, the callbacks will stop working.
pub trait TimeoutHandler {
    /// Report that the timeout has occurred.
    fn timeout(&self);
}

/// Timer stop done callbacks will need to implement this trait.
/// Note: You must keep this object around because it is stored as Weak.
/// If you allow this object to be done, the callbacks will stop working.
pub trait TimerStopDoneHandler {
    /// Report that the operation (timer stop) has completed.
    fn timer_stopped(&self);
}

struct TimerData {
    o: Arc<IOsFuncs>,
    cb: Weak<dyn TimeoutHandler>,
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

/// A timer for waiting for things to happen.
pub struct Timer {
    t: *const raw::gensio_timer,
    d: *mut TimerData
}

fn i_timeout_handler(_t: *const raw::gensio_timer,
		     cb_data: *mut ffi::c_void) {
    let d = cb_data as *mut TimerData;
    let cb = unsafe { (*d).cb.upgrade() };
    let cb = match cb {
	None => return,
	Some(cb) => cb
    };
    cb.timeout();
}

extern "C" fn timeout_handler(t: *const raw::gensio_timer,
			      cb_data: *mut ffi::c_void) {
    let _r = panic::catch_unwind(|| {
	i_timeout_handler(t, cb_data)
    });
}

struct TimerStopData {
    cb: Weak<dyn TimerStopDoneHandler>,
    d: *mut TimerData
}

fn i_timer_stopped_handler(_t: *const raw::gensio_timer,
			   cb_data: *mut ffi::c_void) {
    let d = cb_data as *mut TimerStopData;
    let d = unsafe { Box::from_raw(d) }; // Covert it to Box so it will free.
    unsafe {
	let d2 = d.d;
	{
	    let _guard = (*d2).m.lock().unwrap();
	    (*d2).stopping = false;
	}

	let cb = d.cb.upgrade();
	match cb {
	    None => (),
	    Some(cb) => cb.timer_stopped()
	};

	let _guard = (*d2).m.lock().unwrap();
	if (*d2).freed {
	    raw::gensio_os_funcs_wake((*d2).o.o, (*d2).w);
	}
    }
}

extern "C" fn timer_stopped_handler(t: *const raw::gensio_timer,
				    cb_data: *mut ffi::c_void) {
    let _r = panic::catch_unwind(|| {
	i_timer_stopped_handler(t, cb_data)
    });
}

extern "C" fn i_timer_freed_handler(_t: *const raw::gensio_timer,
				    cb_data: *mut ffi::c_void) {
    let d = cb_data as *mut TimerData;

    unsafe {
	raw::gensio_os_funcs_wake((*d).o.o, (*d).w);
    }
}

extern "C" fn timer_freed_handler(t: *const raw::gensio_timer,
				  cb_data: *mut ffi::c_void) {
    let _r = panic::catch_unwind(|| {
	i_timer_freed_handler(t, cb_data)
    });
}

impl Timer {
    /// Allocate a new Timer object for the OsFuncs.
    pub fn new(o: &OsFuncs, cb: Weak<dyn TimeoutHandler>)
		     -> Result<Self, Error> {
	let w;
	unsafe {
	    w = raw::gensio_os_funcs_alloc_waiter(o.raw());
	}
	let d = Box::new(TimerData { o: o.o.clone(),
				     cb,
				     freed: false,
				     stopping: false,
				     w, m: Mutex::new(0) });
	let d = Box::into_raw(d);
	let t;
	// Nothing below here can panic, so it's safe to convert the
	// box to raw.
	unsafe {
	    t = raw::gensio_os_funcs_alloc_timer(o.raw(), timeout_handler,
						 d as *mut ffi::c_void);
	}

	if t.is_null() {
	    unsafe { drop(Box::from_raw(d)); }
	    return Err(Error::NoMem);
	}
	Ok(Timer { t, d })
    }

    /// Start timer relative
    pub fn start(&self, timeout: &Duration) -> Result<(), Error> {
	let t = raw::gensio_time{ secs: timeout.as_secs() as i64,
				  nsecs: timeout.subsec_nanos() as i32 };
	let err = unsafe {
	    raw::gensio_os_funcs_start_timer((*self.d).o.o, self.t, &t)
	};
	match err {
	    0 => Ok(()),
	    _ => Err(val_to_error(err))
	}
    }

    // FIXME - there is no absolute time.  It may not be necessary,
    // but could be added.

    /// Stop a timer.
    pub fn stop(&self) -> Result<(), Error> {
	let err = unsafe {
	    raw::gensio_os_funcs_stop_timer((*self.d).o.o, self.t)
	};
	match err {
	    0 => Ok(()),
	    _ => Err(val_to_error(err))
	}
    }

    /// Stop a timer and call a callback when the stop completes.
    pub fn stop_with_done(&self, cb: Weak<dyn TimerStopDoneHandler>)
			  -> Result<(), Error> {
	unsafe {
	    let _guard = (*self.d).m.lock().unwrap();

	    if (*self.d).freed {
		return Err(Error::NotReady)
	    }
	    if (*self.d).stopping {
		return Err(Error::InUse)
	    }
	    let d = Box::new(TimerStopData { cb, d: self.d });
	    let d = Box::into_raw(d);
	    // Nothing below here can panic, so it's safe to convert the
	    // box to raw.
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
		    Err(val_to_error(err))
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
		match val_to_error(err) {
		    Error::TimedOut => { // Timer is not running
			if (*self.d).stopping {
			    // Stop done handler is pending and still
			    // hasn't run, so make sure it does the
			    // wake.
			    (*self.d).freed = true;
			    do_wait = true;
			}
		    }
		    Error::NoErr => {
			// Timer was running and is now stopped, the
			// stop done handler will be called.
			do_wait = true;
		    }
		    Error::InUse => {
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

/// Runner callbacks will need to implement this trait.
/// Note: You must keep this object around because it is stored as Weak.
/// If you allow this object to be done, the callbacks will stop working.
pub trait RunnerHandler {
    /// The runner callback calls this.
    fn runner(&self);
}

struct RunnerData {
    o: Arc<IOsFuncs>,
    handler: Weak<dyn RunnerHandler>,
}

/// A type for running callbacks in base context.
pub struct Runner {
    r: *const raw::gensio_runner,
    d: *mut RunnerData
}

fn i_runner_handler(_r: *const raw::gensio_runner,
		    cb_data: *mut ffi::c_void) {
    let d = cb_data as *mut RunnerData;
    let cb = unsafe { (*d).handler.upgrade() };
    match cb {
	None => (),
	Some(cb) => cb.runner()
    };
}

extern "C" fn runner_handler(r: *const raw::gensio_runner,
			     cb_data: *mut ffi::c_void) {
    let _r = panic::catch_unwind(|| {
	i_runner_handler(r, cb_data)
    });
}

impl Runner {
    /// Allocate a new Runner object for the OsFuncs.
    pub fn new(o: &OsFuncs, handler: Weak<dyn RunnerHandler>)
	       -> Result<Self, Error> {
	let d = Box::new(RunnerData { o: o.o.clone(), handler });
	let d = Box::into_raw(d);
	let r;
	// Nothing below here can panic, so it's safe to convert the
	// box to raw.
	unsafe {
	    r = raw::gensio_os_funcs_alloc_runner(o.raw(), runner_handler,
						  d as *mut ffi::c_void);
	}

	if r.is_null() {
	    unsafe { drop(Box::from_raw(d)); }
	    return Err(Error::NoMem);
	}
	Ok(Runner { r, d })
    }

    /// Run the runner
    pub fn run(&self) -> Result<(), Error> {
	let err = unsafe {
	    raw::gensio_os_funcs_run((*self.d).o.o, self.r)
	};
	match err {
	    0 => Ok(()),
	    _ => Err(val_to_error(err))
	}
    }
}

impl Drop for Runner {
    fn drop(&mut self) {
	unsafe {
	    raw::gensio_os_funcs_free_runner((*self.d).o.o, self.r);
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
	    // What to fill in here?
	}
    }

    #[test]
    fn wait_test() {
	let logh = Arc::new(LogHandler);
	let o = OsFuncs::new(Arc::downgrade(&logh) as _)
	    .expect("Couldn't allocate OsFuncs");
	o.thread_setup().expect("Couldn't setup thread");
	let w = Waiter::new(&o).expect("Couldn't allocate Waiter");

	drop(o);
	assert_eq!(w.wait(1, Some(&Duration::new(0, 0))), Err(Error::TimedOut));
    }

    struct HandleTimeout1 {
	w: Waiter,
    }
    impl TimeoutHandler for HandleTimeout1 {
	fn timeout(&self) {
	    self.w.wake();
	}
    }

    // Normal timer operation, wait for it to time out.
    #[test]
    fn timer_test() {
	let logh = Arc::new(LogHandler);
	let o = OsFuncs::new(Arc::downgrade(&logh) as _)
	    .expect("Couldn't allocate OsFuncs");
	o.thread_setup().expect("Couldn't setup thread");

	let h = Arc::new(HandleTimeout1 {
	    w: Waiter::new(&o).expect("Couldn't allocate Waiter"),
	});
	let t = Timer::new(&o, Arc::downgrade(&h) as _)
	    .expect("Couldn't allocate Timer");

	t.start(&Duration::new(0, 1000)).expect("Couldn't start timer");

	match h.w.wait(1, Some(&Duration::new(1, 0))) {
	    Ok(_) => (),
	    Err(e) => panic!("Wait failed: {}", e)
	}
    }

    // See that the cleanup happens on a running timer
    #[test]
    fn timer_test2() {
	let logh = Arc::new(LogHandler);
	let o = OsFuncs::new(Arc::downgrade(&logh) as _)
	    .expect("Couldn't allocate OsFuncs");
	o.thread_setup().expect("Couldn't setup thread");

	let h = Arc::new(HandleTimeout1 {
	    w: Waiter::new(&o).expect("Couldn't allocate Waiter"),
	});
	let t = Timer::new(&o, Arc::downgrade(&h) as _)
	    .expect("Couldn't allocate Timer");

	t.start(&Duration::new(100, 0)).expect("Couldn't start timer");
    }

    struct StopTimer3 {
	w: Waiter,
        v: Mutex<u32>,
    }

    impl TimerStopDoneHandler for StopTimer3 {
	fn timer_stopped(&self) {
            let mut v = self.v.lock().unwrap();
            let v2 = TESTVAL3.lock().unwrap();
            assert_eq!(*v, *v2);
            *v = 10;
	    self.w.wake();
	}
    }

    static TESTVAL3: Mutex<u32> = Mutex::new(0);

    // Stop the timer and wait for it
    #[test]
    fn timer_test3() {
	let logh = Arc::new(LogHandler);
	let o = OsFuncs::new(Arc::downgrade(&logh) as _)
	    .expect("Couldn't allocate OsFuncs");
	o.thread_setup().expect("Couldn't setup thread");

	let h = Arc::new(HandleTimeout1 {
	    w: Waiter::new(&o).expect("Couldn't allocate Waiter"),
	});
	let s = Arc::new(StopTimer3 {
	    w: Waiter::new(&o).expect("Couldn't allocate Waiter"),
            v: Mutex::new(1),
	});
	let t = Timer::new(&o, Arc::downgrade(&h) as _)
	    .expect("Couldn't allocate Timer");

        {
            let mut v2 = TESTVAL3.lock().unwrap();
            *v2 = 1;
        }
	t.start(&Duration::new(100, 0)).expect("Couldn't start timer");
	t.stop_with_done(Arc::downgrade(&s) as _).expect("Couldn't stop timer");
	match s.w.wait(1, Some(&Duration::new(1, 0))) {
	    Ok(_) => (),
	    Err(e) => panic!("Wait failed: {}", e)
	}
        let vv = s.v.lock().unwrap();
        assert_eq!(*vv, 10);
    }

    struct StopTimer4 {
	w: Waiter,
        v: Mutex<u32>,
    }

    impl TimerStopDoneHandler for StopTimer4 {
	fn timer_stopped(&self) {
            let mut v = self.v.lock().unwrap();
            let v2 = TESTVAL4.lock().unwrap();
            assert_eq!(*v, *v2);
            *v = 10;
	    self.w.wake();
	}
    }

    static TESTVAL4: Mutex<u32> = Mutex::new(0);

    // See that the cleanup works properly on a timer that is being
    // stopped.
    #[test]
    fn timer_test4() {
	let logh = Arc::new(LogHandler);
	let o = OsFuncs::new(Arc::downgrade(&logh) as _)
	    .expect("Couldn't allocate OsFuncs");
	o.thread_setup().expect("Couldn't setup thread");

	let h = Arc::new(HandleTimeout1 {
	    w: Waiter::new(&o).expect("Couldn't allocate Waiter"),
	});
	let s = Arc::new(StopTimer4 {
	    w: Waiter::new(&o).expect("Couldn't allocate Waiter"),
            v: Mutex::new(2),
	});
	let t = Timer::new(&o, Arc::downgrade(&h) as _)
	    .expect("Couldn't allocate Timer");
        {
            let mut v2 = TESTVAL4.lock().unwrap();
            *v2 = 2;
        }

	t.start(&Duration::new(100, 0)).expect("Couldn't start timer");
	t.stop_with_done(Arc::downgrade(&s) as _).expect("Couldn't stop timer");
    }

    struct TermHnd {
        w: Waiter,
    }

    impl GensioTermHandler for TermHnd {
        fn term_sig(&self) {
            self.w.wake();
        }
    }

    // Note that all the signal handler test functions are required to be
    // serial because they use proc_setup().

    #[link(name = "gensiooshelpers")]
    extern "C" {
        fn send_term_self() -> ffi::c_int;
        fn send_hup_self() -> ffi::c_int;
        fn send_winch_self() -> ffi::c_int;
    }
    #[test]
    #[serial]
    fn term_test() {
	let logh = Arc::new(LogHandler);
	let o = OsFuncs::new(Arc::downgrade(&logh) as _)
	    .expect("Couldn't allocate OsFuncs");
	o.proc_setup().expect("Couldn't setup proc");
	let h = Arc::new(TermHnd {
	    w: Waiter::new(&o).expect("Couldn't allocate Waiter"),
	});
        o.register_term_handler(Arc::downgrade(&h) as _)
	    .expect("Couldn't register term handler");
        let err = unsafe { send_term_self() };
	if err == -2 {
	    // Not supported on platform.
	    return;
	}

        // There are other threads running in the process, if those
        // threads handle the signal, it won't necessarily wake up the
        // waiter.  So we loop twice to catch the signal setting the
        // second time around.  Same with the other signal handler
        // functions.
        let mut count = 0;
        loop {
	    let rv = h.w.wait(1, Some(&Duration::new(10, 0)));
            if rv == Err(Error::TimedOut) && count == 0 {
                count += 1;
                continue;
            }
            match rv {
	        Ok(_) => (), 
	        Err(e) => panic!("Wait failed {}", e)
            }
            break;
	};
    }

    struct HupHnd {
        w: Waiter,
    }

    impl GensioHupHandler for HupHnd {
        fn hup_sig(&self) {
            self.w.wake();
        }
    }

    #[test]
    #[serial]
    fn hup_test() {
	let logh = Arc::new(LogHandler);
	let o = OsFuncs::new(Arc::downgrade(&logh) as _)
	    .expect("Couldn't allocate OsFuncs");
	o.proc_setup().expect("Couldn't setup proc");
	let h = Arc::new(HupHnd {
	    w: Waiter::new(&o).expect("Couldn't allocate Waiter"),
	});
	match o.register_hup_handler(Arc::downgrade(&h) as _) {
	    Ok(_) => (),
	    Err(e) => {
	        if e == Error::NotSup {
	            // Not supported, just quit.
	            return;
		}
	        panic!("Couldn't register hup handler");
	    }
	}
        let err = unsafe { send_hup_self() };
	if err == -2 {
	    // Not supported on platform.
	    return;
	}

        // See note in term_test on this loop.
        let mut count = 0;
        loop {
	    let rv = h.w.wait(1, Some(&Duration::new(1, 0)));
            if rv == Err(Error::TimedOut) && count == 0 {
                count += 1;
                continue;
            }
            match rv {
	        Ok(_) => (),
	        Err(e) => panic!("Wait failed {}", e)
            }
            break;
	};
    }

    struct WinsizeHnd {
        w: Waiter,
    }

    impl GensioWinsizeHandler for WinsizeHnd {
        fn winsize_sig(&self, _x_chrs: i32, _y_chrs: i32, _x_bits: i32, _y_bits: i32) {
            self.w.wake();
        }
    }

    #[test]
    #[serial]
    fn winsize_test() {
	let logh = Arc::new(LogHandler);
	let o = OsFuncs::new(Arc::downgrade(&logh) as _)
	    .expect("Couldn't allocate OsFuncs");
	o.proc_setup().expect("Couldn't setup proc");
	let h = Arc::new(WinsizeHnd {
	    w: Waiter::new(&o).expect("Couldn't allocate Waiter"),
	});
        let iod;
        let o2;
        unsafe {
            o2 = o.raw();
            iod = raw::gensio_add_iod(o2, raw::GENSIO_IOD_CONSOLE, 0);
            assert!(iod != std::ptr::null());
            o.register_winsize_handler(iod, Arc::downgrade(&h) as _)
                .expect("Couldn't register winsize handler");
        }
        // Note that the winsize handler will automatically send the
        // proper signal to cause the callback, so no need to send it.
	// Temporarily send it anyway, for MacOS.
        unsafe { send_winch_self(); }

        // See note in term_test on this loop.
        let mut count = 0;
        loop {
	    let rv = h.w.wait(1, Some(&Duration::new(1, 0)));
            if rv == Err(Error::TimedOut) && count == 0 {
                count += 1;
                continue;
            }
            match rv {
	        Ok(_) => (),
	        Err(e) => panic!("Wait failed {}", e)
            }
            break;
	};
        unsafe {
            raw::gensio_release_iod(o2, iod);
        }
    }

    struct HandleRunner1 {
	w: Waiter,
    }
    impl RunnerHandler for HandleRunner1 {
	fn runner(&self) {
	    self.w.wake();
	}
    }

    #[test]
    fn runner_test() {
	let logh = Arc::new(LogHandler);
	let o = OsFuncs::new(Arc::downgrade(&logh) as _)
	    .expect("Couldn't allocate OsFuncs");
	o.thread_setup().expect("Couldn't setup thread");

	let h = Arc::new(HandleRunner1 {
	    w: Waiter::new(&o).expect("Couldn't allocate Waiter"),
	});
	let t = Runner::new(&o, Arc::downgrade(&h) as _)
	    .expect("Couldn't allocate Timer");

	t.run().expect("Couldn't start timer");

	match h.w.wait(1, Some(&Duration::new(1, 0))) {
	    Ok(_) => (),
	    Err(e) => panic!("Wait failed: {}", e)
	}
    }
}
