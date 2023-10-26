// Copyright 2023 Corey Minyard
//
// SPDX-License-Identifier: Apache-2.0

use std::ffi;
use crate::osfuncs::raw::gensiods;
use crate::osfuncs::raw::gensio_os_funcs;

#[repr(C)]
pub struct gensio;

pub const GENSIO_EVENT_READ:		ffi::c_int = 1;
pub const GENSIO_EVENT_WRITE_READY:	ffi::c_int = 2;
pub const GENSIO_EVENT_NEW_CHANNEL:	ffi::c_int = 3;
pub const GENSIO_EVENT_SEND_BREAK:	ffi::c_int = 4;
pub const GENSIO_EVENT_AUTH_BEGIN:	ffi::c_int = 5;
pub const GENSIO_EVENT_PRECERT_VERIFY:	ffi::c_int = 6;
pub const GENSIO_EVENT_POSTCERT_VERIFY:	ffi::c_int = 7;
pub const GENSIO_EVENT_PASSWORD_VERIFY:	ffi::c_int = 8;
pub const GENSIO_EVENT_REQUEST_PASSWORD: ffi::c_int = 9;
pub const GENSIO_EVENT_REQUEST_2FA:	ffi::c_int = 10;
pub const GENSIO_EVENT_2FA_VERIFY:	ffi::c_int = 11;
pub const GENSIO_EVENT_PARMLOG:	ffi::c_int = 12; // struct gensio_parm_data
pub const GENSIO_EVENT_WIN_SIZE: ffi::c_int = 13;
pub const GENSIO_EVENT_LOG: ffi::c_int = 14; // struct gensio_log_data

// FIXME - gensio log mask

#[allow(non_camel_case_types)]
pub type gensio_event = extern "C" fn (io: *const gensio,
				       user_data: *const ffi::c_void,
				       event: ffi::c_int, err: ffi::c_int,
				       buf: *const ffi::c_void,
				       buflen: *mut gensiods,
				       auxdata: *const *const ffi::c_char)
				       -> ffi::c_int;

#[allow(non_camel_case_types)]
pub type gensio_done_err = extern "C" fn (io: *const gensio,
					  err: ffi::c_int,
					  user_data: *mut ffi::c_void);

#[allow(non_camel_case_types)]
pub type gensio_done = extern "C" fn (io: *const gensio,
				      user_data: *mut ffi::c_void);

pub const GENSIO_ACC_EVENT_NEW_CONNECTION: ffi::c_int =		1;
pub const GENSIO_ACC_EVENT_LOG: ffi::c_int =			2;
// event logs come in as this, we use a C helper to convert
// it to a string.
//struct gensio_loginfo {
//    enum gensio_log_levels level;
//    const char *str;
//    va_list args;
//};

pub const GENSIO_ACC_EVENT_PRECERT_VERIFY: ffi::c_int =			3;
pub const GENSIO_ACC_EVENT_AUTH_BEGIN: ffi::c_int =			4;

pub const GENSIO_ACC_EVENT_PASSWORD_VERIFY: ffi::c_int =		5;
pub const GENSIO_ACC_EVENT_REQUEST_PASSWORD: ffi::c_int =		6;
#[repr(C)]
pub struct gensio_acc_password_verify_data {
    pub io: *const gensio,
    pub password: *const ffi::c_char,
    pub password_len: gensiods,
}

pub const GENSIO_ACC_EVENT_POSTCERT_VERIFY: ffi::c_int =		7;
#[repr(C)]
pub struct gensio_acc_postcert_verify_data {
    pub io: *const gensio,
    pub err: ffi::c_int,
    pub errstr: *const ffi::c_char,
}

pub const GENSIO_ACC_EVENT_2FA_VERIFY: ffi::c_int =			8;
pub const GENSIO_ACC_EVENT_REQUEST_2FA: ffi::c_int =			9;
/* Uses struct gensio_acc_password_verify_data */

pub const GENSIO_ACC_EVENT_PARMLOG: ffi::c_int =			10;
// event logs come in as this, we use a C helper to convert
// it to a string.
//struct gensio_parmlog_data {
//    const char *log;
//    va_list args;
//};

#[repr(C)]
pub struct gensio_accepter;

#[allow(non_camel_case_types)]
pub type gensio_accepter_event = extern "C" fn (acc: *const gensio_accepter,
						user_data: *const ffi::c_void,
						event: ffi::c_int,
						data: *const ffi::c_void)
						-> ffi::c_int;

#[allow(non_camel_case_types)]
pub type gensio_acc_done = extern "C" fn (acc: *const gensio_accepter,
					  user_data: *mut ffi::c_void);

#[link(name = "gensio")]
#[link(name = "gensiohelpers")]
extern "C" {
    #[allow(improper_ctypes)]
    pub fn str_to_gensio(s: *const ffi::c_char,
			 o: *const gensio_os_funcs,
			 cb: gensio_event,
			 user_data: *const ffi::c_void,
			 rgensio: *const *const gensio
    ) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_set_user_data(g: *const gensio, data: *const ffi::c_void);

    #[allow(improper_ctypes)]
    pub fn gensio_set_callback(g: *const gensio, cb: gensio_event,
			       data: *const ffi::c_void);

    #[allow(improper_ctypes)]
    pub fn gensio_open(io: *const gensio, open_done: gensio_done_err,
		       open_data: *mut ffi::c_void) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_open_s(io: *const gensio) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_close(io: *const gensio, close_done: gensio_done,
			close_data: *mut ffi::c_void) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_close_s(io: *const gensio) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_free(io: *const gensio);

    #[allow(improper_ctypes)]
    pub fn gensio_write(io: *const gensio, count: &mut gensiods,
			buf: *const ffi::c_void, buflen: gensiods,
			auxdata: *const *const ffi::c_char) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_set_read_callback_enable(g: *const gensio,
					   enabled: ffi::c_int);

    #[allow(improper_ctypes)]
    pub fn str_to_gensio_accepter(s: *const ffi::c_char,
				  o: *const gensio_os_funcs,
				  cb: gensio_accepter_event,
				  user_data: *const ffi::c_void,
				  racc: *const *const gensio_accepter)
				  -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_acc_set_user_data(a: *const gensio_accepter,
				    user_data: *const ffi::c_void);

    #[allow(improper_ctypes)]
    pub fn gensio_acc_startup(a: *const gensio_accepter) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_acc_shutdown(a: *const gensio_accepter,
			       cb: gensio_acc_done,
			       shutdown_data: *const ffi::c_void)
			       -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_acc_shutdown_s(a: *const gensio_accepter) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_acc_free(a: *const gensio_accepter) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_loginfo_to_str(vloginfo: *const ffi::c_void)
				 -> *mut ffi::c_char;

    #[allow(improper_ctypes)]
    pub fn gensio_parmlog_to_str(vloginfo: *const ffi::c_void)
				 -> *mut ffi::c_char;

    #[allow(improper_ctypes)]
    pub fn gensio_free_loginfo_str(str: *mut ffi::c_char);
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use serial_test::serial;
    use crate::osfuncs::raw::gensio_time;

    use crate::osfuncs::raw::gensio_alloc_os_funcs;
    use crate::osfuncs::raw::gensio_os_funcs_free;

    use crate::osfuncs::raw::gensio_os_proc_data;
    use crate::osfuncs::raw::gensio_os_proc_setup;
    use crate::osfuncs::raw::gensio_os_proc_cleanup;

    use crate::osfuncs::raw::gensio_waiter;
    use crate::osfuncs::raw::gensio_os_funcs_alloc_waiter;
    use crate::osfuncs::raw::gensio_os_funcs_free_waiter;
    use crate::osfuncs::raw::gensio_os_funcs_wake;
    use crate::osfuncs::raw::gensio_os_funcs_wait;
    use super::*;

    struct GData {
	o: *const gensio_os_funcs,
 	w: *const gensio_waiter,
	g: *const gensio
    }

    extern "C" fn evhndl(_io: *const gensio, user_data: *const ffi::c_void,
			 _event: ffi::c_int, _err: ffi::c_int,
			 buf: *const ffi::c_void, buflen: *mut gensiods,
			 _auxdata: *const *const ffi::c_char) -> ffi::c_int
    {
	// Convert the buffer into a slice.  You can't use it directly as
	// a pointer to create a CString with from_raw() because then Rust
	// takes over ownership of the data, and will free it when this
	// function exits.
	let b =
	    unsafe {
		std::slice::from_raw_parts(buf as *mut u8, *buflen as usize)
	    };

	let s;
	unsafe {
	    assert_eq!(*buflen, 7);
	    s = ffi::CString::from_vec_unchecked(b.to_vec());
	}
	assert_eq!(s.into_string().expect("into_string() call failed"),
		   "teststr");
	unsafe {
	    let d = user_data as *const GData;

	    let err = gensio_os_funcs_wake((*d).o, (*d).w);
	    assert_eq!(err, 0);
	}
	0
    }

    extern "C" fn opened(_io: *const gensio, err: ffi::c_int,
			 open_data: *mut ffi::c_void)
    {
	assert_eq!(err, 0);
	unsafe {
	    let d = open_data as *const GData;

	    let err = gensio_os_funcs_wake((*d).o, (*d).w);
	    assert_eq!(err, 0);
	}
    }

    extern "C" fn closed(_io: *const gensio,close_data: *mut ffi::c_void)
    {
	unsafe {
	    let d = close_data as *const GData;

	    let err = gensio_os_funcs_wake((*d).o, (*d).w);
	    assert_eq!(err, 0);
	}
    }

    #[test]
    #[serial]
    fn basic_gensio() {
	let mut err: ffi::c_int;

	let o: *const gensio_os_funcs = std::ptr::null();
	unsafe {
	    err = gensio_alloc_os_funcs(-198234, &o);
	}
	let p: *const gensio_os_proc_data = std::ptr::null();
	assert_eq!(err, 0);
	unsafe {
	    err = gensio_os_proc_setup(o, &p);
	}
	assert_eq!(err, 0);
	let w;
	unsafe {
	    w = gensio_os_funcs_alloc_waiter(o);
	}
	assert_eq!(err, 0);
	let g: *const gensio = std::ptr::null();
	unsafe {
	    let s = ffi::CString::new("echo").expect("CString::new failed");
	    err = str_to_gensio(s.as_ptr(), o, evhndl,
				std::ptr::null(), &g);
	}
	assert_eq!(err, 0);
	let d = Rc::new(GData { o: o, w: w, g: g });
	unsafe {
	    gensio_set_user_data(g, Rc::as_ptr(&d) as *mut ffi::c_void);
	}

	unsafe {
	    err = gensio_open(d.g, opened, Rc::as_ptr(&d) as *mut ffi::c_void);
	}
	assert_eq!(err, 0);
	unsafe {
	    err = gensio_os_funcs_wait(d.o, d.w, 1,
				       &mut gensio_time{secs: 1, nsecs: 0});
	}
	assert_eq!(err, 0);
	unsafe {
	    let s = ffi::CString::new("teststr").expect("CString::new failed");
	    let mut count = 0;
	    err = gensio_write(d.g, &mut count,
			       s.as_ptr() as *const ffi::c_void, 7,
			       std::ptr::null());
	    assert_eq!(err, 0);
	    assert_eq!(count, 7);
	}
	unsafe {
	    gensio_set_read_callback_enable(d.g, 1);
	    err = gensio_os_funcs_wait(d.o, d.w, 1,
				       &mut gensio_time{secs: 1, nsecs: 0});
	}
	assert_eq!(err, 0);

	unsafe {
	    err = gensio_close(d.g, closed, Rc::as_ptr(&d) as *mut ffi::c_void);
	}
	assert_eq!(err, 0);
	unsafe {
	    err = gensio_os_funcs_wait(d.o, d.w, 1,
				       &mut gensio_time{secs: 1, nsecs: 0});
	}
	assert_eq!(err, 0);
	unsafe {
	    gensio_free(d.g);
	    gensio_os_funcs_free_waiter(d.o, d.w);
	    gensio_os_proc_cleanup(p);
	    gensio_os_funcs_free(d.o);
	}
    }
}
