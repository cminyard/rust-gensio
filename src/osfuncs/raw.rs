// Copyright 2023 Corey Minyard
//
// SPDX-License-Identifier: Apache-2.0

use std::ffi;

#[repr(C)]
pub struct gensio_time {
    pub secs: i64,
    pub nsecs: i32
}

#[allow(non_camel_case_types)]
pub type gensiods = ffi::c_ulong;

#[repr(C)]
pub struct gensio_os_funcs;

#[repr(C)]
pub struct gensio_os_proc_data;

#[repr(C)]
pub struct gensio_waiter;

#[repr(C)]
pub struct gensio_lock;

#[repr(C)]
pub struct gensio_timer;

#[repr(C)]
pub struct gensio_runner;

#[repr(C)]
pub struct gensio_iod;

#[allow(non_camel_case_types)]
pub type gensio_timer_cb = extern "C" fn (t: *const gensio_timer,
					  data: *mut ffi::c_void);

#[allow(non_camel_case_types)]
pub type gensio_runner_cb = extern "C" fn (r: *const gensio_runner,
					   data: *mut ffi::c_void);

#[allow(non_camel_case_types)]
pub type gensio_sig_cb = extern "C" fn (data: *mut ffi::c_void);

#[allow(non_camel_case_types)]
pub type gensio_winsize_cb = extern "C" fn (x_chrs: ffi::c_int,
					    y_chrs: ffi::c_int,
					    x_bits: ffi::c_int,
					    y_bits: ffi::c_int,
					    data: *mut ffi::c_void);

#[allow(non_camel_case_types)]
pub type gensio_rust_log_func = extern "C" fn (log: *const ffi::c_char,
					       data: *mut ffi::c_void);

#[link(name = "gensioosh")]
extern "C" {
    // Note: This must be passed a static variable.  It is not copied,
    // the pointer is used.
    pub fn gensio_set_progname(o: *const ffi::c_char) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_default_os_hnd(wake_sig: ffi::c_int,
				 o: *const *const gensio_os_funcs)
				 -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_alloc_os_funcs(wake_sig: ffi::c_int,
				 o: *const *const gensio_os_funcs,
				 flags: ffi::c_uint)
			       -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_os_proc_setup(o: *const gensio_os_funcs,
				data: *const *const gensio_os_proc_data)
				-> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_os_thread_setup(o: *const gensio_os_funcs)
				  -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_os_proc_cleanup(data: *const gensio_os_proc_data);

    #[allow(improper_ctypes)]
    pub fn gensio_os_proc_register_term_handler(data: *const gensio_os_proc_data,
					        cb: gensio_sig_cb,
					        data: *mut ffi::c_void) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_os_proc_register_reload_handler(data: *const gensio_os_proc_data,
					          cb: gensio_sig_cb,
					          data: *mut ffi::c_void) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_os_proc_register_winsize_handler(data: *const gensio_os_proc_data,
                                                   console_iod: *const gensio_iod,
					           cb: gensio_winsize_cb,
					           data: *mut ffi::c_void) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_alloc_lock(o: *const gensio_os_funcs)
				      -> *const gensio_lock;

    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_free_lock(o: *const gensio_os_funcs,
				     lock: *const gensio_lock);

    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_lock(o: *const gensio_os_funcs,
				lock: *const gensio_lock);

    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_unlock(o: *const gensio_os_funcs,
				  lock: *const gensio_lock);

    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_alloc_timer(o: *const gensio_os_funcs,
				       cb: gensio_timer_cb,
				       data: *mut ffi::c_void)
				       -> *const gensio_timer;

    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_free_timer(o: *const gensio_os_funcs,
				      t: *const gensio_timer);

    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_start_timer(o: *const gensio_os_funcs,
				       t: *const gensio_timer,
				       timeout: &gensio_time) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_start_timer_abs(o: *const gensio_os_funcs,
					   t: *const gensio_timer,
					   timeout: &gensio_time) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_stop_timer(o: *const gensio_os_funcs,
				      t: *const gensio_timer) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_stop_timer_with_done
	(o: *const gensio_os_funcs,
	 t: *const gensio_timer,
	 cb: gensio_timer_cb,
	 data: *mut ffi::c_void) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_alloc_runner(o: *const gensio_os_funcs,
					cb: gensio_runner_cb,
					data: *mut ffi::c_void)
					-> *const gensio_runner;

    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_free_runner(o: *const gensio_os_funcs,
				       w: *const gensio_runner);

    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_run(o: *const gensio_os_funcs,
			       w: *const gensio_runner)
			       -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_service(o: *const gensio_os_funcs,
				   timeout: *const gensio_time) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_alloc_waiter(o: *const gensio_os_funcs)
					-> *const gensio_waiter;

    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_free_waiter(o: *const gensio_os_funcs,
				       w: *const gensio_waiter);

    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_wait(o: *const gensio_os_funcs,
				w: *const gensio_waiter, count: ffi::c_uint,
				timeout: *const gensio_time) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_wait_intr(o: *const gensio_os_funcs,
				     w: *const gensio_waiter,
				     count: ffi::c_uint,
				     timeout: *const gensio_time) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_wake(o: *const gensio_os_funcs,
				w: *const gensio_waiter);

    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_free(o: *const gensio_os_funcs);

    /// Allocate some gensio data.  For internal use only, subject to change.
    #[allow(improper_ctypes)]
    pub fn gensio_os_funcs_zalloc(o: *const gensio_os_funcs, len: gensiods)
				  -> *mut ffi::c_void;
}

#[link(name = "gensiooshelpers")]
extern "C" {
    /// Set the log handler function.  This is rust-specific, since
    /// rust can't take va_list and we need a helper function to
    /// generate the log string.
    #[allow(improper_ctypes)]
    pub fn gensio_rust_set_log(o: *const gensio_os_funcs,
			       cb: gensio_rust_log_func,
			       data: *mut ffi::c_void);

    /// Must be called before freeing the os funcs, it will clean up
    /// any data allocated.
    #[allow(improper_ctypes)]
    pub fn gensio_rust_cleanup(o: *const gensio_os_funcs);

    /// Allocate an IOD.  This is for internal use and subject to change.
    #[allow(improper_ctypes)]
    pub fn gensio_add_iod(o: *const gensio_os_funcs, kind: ffi::c_int, fd: ffi::c_int)
                          -> *const gensio_iod;

    /// Release an allocated IOD.
    #[allow(improper_ctypes)]
    pub fn gensio_release_iod(o: *const gensio_os_funcs, iod: *const gensio_iod);
}

pub const GESNIO_IOD_INVALID: i32       = 0;
pub const GENSIO_IOD_SOCKET: i32        = 1;
pub const GENSIO_IOD_PIPE: i32          = 2;
pub const GENSIO_IOD_DEV: i32           = 3;
pub const GENSIO_IOD_FILE: i32          = 4;
pub const GENSIO_IOD_SIGNAL: i32        = 5;
pub const GENSIO_IOD_STDIO: i32         = 6;
pub const GENSIO_IOD_CONSOLE: i32       = 7;
pub const GENSIO_IOD_PTY: i32           = 8;
