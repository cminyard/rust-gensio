//! # gensio
//!
//! `gensio` is a tool for handling all sort of I/O.
//!
//! Note that all the callbacks in gensio are not mutable.  You must
//! use a Mutex or something like that to put mutable data in them.

// Copyright 2023 Corey Minyard
//
// SPDX-License-Identifier: Apache-2.0

use std::ffi;
use std::sync::Arc;
use std::time::Duration;

pub mod osfuncs;
pub mod raw;

pub const GE_NOERR:			i32 = 0;
pub const GE_NOMEM:			i32 = 1;
pub const GE_NOTSUP:			i32 = 2;
pub const GE_INVAL:			i32 = 3;
pub const GE_NOTFOUND:			i32 = 4;
pub const GE_EXISTS:			i32 = 5;
pub const GE_OUTOFRANGE:		i32 = 6;
pub const GE_INCONSISTENT:		i32 = 7;
pub const GE_NODATA:			i32 = 8;
pub const GE_OSERR:			i32 = 9;
pub const GE_INUSE:			i32 = 10;
pub const GE_INPROGRESS:		i32 = 11;
pub const GE_NOTREADY:			i32 = 12;
pub const GE_TOOBIG:			i32 = 13;
pub const GE_TIMEDOUT:			i32 = 14;
pub const GE_RETRY:			i32 = 15;
pub const GE_KEYNOTFOUND:		i32 = 17;
pub const GE_CERTREVOKED:		i32 = 18;
pub const GE_CERTEXPIRED:		i32 = 19;
pub const GE_KEYINVALID:		i32 = 20;
pub const GE_NOCERT:			i32 = 21;
pub const GE_CERTINVALID:		i32 = 22;
pub const GE_PROTOERR:			i32 = 23;
pub const GE_COMMERR:			i32 = 24;
pub const GE_IOERR:			i32 = 25;
pub const GE_REMCLOSE:			i32 = 26;
pub const GE_HOSTDOWN:			i32 = 27;
pub const GE_CONNREFUSE:		i32 = 28;
pub const GE_DATAMISSING:		i32 = 29;
pub const GE_CERTNOTFOUND:		i32 = 30;
pub const GE_AUTHREJECT:		i32 = 31;
pub const GE_ADDRINUSE:			i32 = 32;
pub const GE_INTERRUPTED:		i32 = 33;
pub const GE_SHUTDOWN:			i32 = 34;
pub const GE_LOCALCLOSED:		i32 = 35;
pub const GE_PERM:			i32 = 36;
pub const GE_APPERR:			i32 = 37;
pub const GE_UNKNOWN_NAME_ERROR:	i32 = 38;
pub const GE_NAME_ERROR:		i32 = 39;
pub const GE_NAME_SERVER_FAILURE:	i32 = 40;
pub const GE_NAME_INVALID:		i32 = 41;
pub const GE_NAME_NET_NOT_UP:		i32 = 42;

pub const GENSIO_CONTROL_DEPTH_ALL: i32 =	-1;
pub const GENSIO_CONTROL_DEPTH_FIRST: i32 =	-2;

pub const GENSIO_CONTROL_GET: bool =	true;
pub const GENSIO_CONTROL_SET: bool =	false;

pub const GENSIO_CONTROL_NODELAY: u32 =			1;
pub const GENSIO_CONTROL_STREAMS: u32 =			2;
pub const GENSIO_CONTROL_SEND_BREAK: u32 =		3;
pub const GENSIO_CONTROL_GET_PEER_CERT_NAME: u32 =	4;
pub const GENSIO_CONTROL_CERT_AUTH: u32 =		5;
pub const GENSIO_CONTROL_USERNAME: u32 =		6;
pub const GENSIO_CONTROL_SERVICE: u32 =			7;
pub const GENSIO_CONTROL_CERT: u32 =			8;
pub const GENSIO_CONTROL_CERT_FINGERPRINT: u32 =	9;
pub const GENSIO_CONTROL_ENVIRONMENT: u32 =		10;
pub const GENSIO_CONTROL_MAX_WRITE_PACKET: u32 =	11;
pub const GENSIO_CONTROL_ARGS: u32 =			12;
pub const GENSIO_CONTROL_EXIT_CODE: u32 =		13;
pub const GENSIO_CONTROL_WAIT_TASK: u32 =		14;
pub const GENSIO_CONTROL_ADD_MCAST: u32 =		15;
pub const GENSIO_CONTROL_DEL_MCAST: u32 =		16;
pub const GENSIO_CONTROL_LADDR: u32 =			17;
pub const GENSIO_CONTROL_LPORT: u32 =			18;
pub const GENSIO_CONTROL_CLOSE_OUTPUT: u32 =		19;
pub const GENSIO_CONTROL_CONNECT_ADDR_STR: u32 =	20;
pub const GENSIO_CONTROL_RADDR: u32 =			21;
pub const GENSIO_CONTROL_RADDR_BIN: u32 =		22;
pub const GENSIO_CONTROL_REMOTE_ID: u32 =		23;
pub const GENSIO_CONTROL_KILL_TASK: u32 =		24;
pub const GENSIO_CONTROL_MCAST_LOOP: u32 =		25;
pub const GENSIO_CONTROL_MCAST_TTL: u32 =		26;
pub const GENSIO_CONTROL_PASSWORD: u32 =		27;
pub const GENSIO_CONTROL_2FA: u32 =			28;
pub const GENSIO_CONTROL_AUX_DATA: u32 =		29;
pub const GENSIO_CONTROL_REM_AUX_DATA: u32 =		30;
pub const GENSIO_CONTROL_IOD: u32 =			31;
pub const GENSIO_CONTROL_EXTRAINFO: u32 =		32;
pub const GENSIO_CONTROL_ENABLE_OOB: u32 =		33;
pub const GENSIO_CONTROL_WIN_SIZE: u32 =		34;
pub const GENSIO_CONTROL_START_DIRECTORY: u32 =		35;
pub const GENSIO_CONTROL_IN_RATE: u32 =			36;
pub const GENSIO_CONTROL_OUT_RATE: u32 =		37;
pub const GENSIO_CONTROL_IN_BUFSIZE: u32 =		38;
pub const GENSIO_CONTROL_OUT_BUFSIZE: u32 =		39;
pub const GENSIO_CONTROL_IN_NR_CHANS: u32 =		40;
pub const GENSIO_CONTROL_OUT_NR_CHANS: u32 =		41;
pub const GENSIO_CONTROL_IN_FORMAT: u32 =		42;
pub const GENSIO_CONTROL_OUT_FORMAT: u32 =		43;
pub const GENSIO_CONTROL_DRAIN_COUNT: u32 =		44;
pub const GENSIO_CONTROL_SER_MODEMSTATE: u32 =		45;
pub const GENSIO_CONTROL_SER_FLOWCONTROL_STATE: u32 =	46;
pub const GENSIO_CONTROL_SER_FLUSH: u32 =		47;
pub const GENSIO_CONTROL_SER_SEND_BREAK: u32 =		48;
pub const GENSIO_CONTROL_SER_LINESTATE: u32 =		49;

pub const GENSIO_ACONTROL_SER_BAUD: u32 =		1000;
pub const GENSIO_ACONTROL_SER_DATASIZE: u32 =		1001;
pub const GENSIO_ACONTROL_SER_PARITY: u32 =		1002;
pub const GENSIO_ACONTROL_SER_STOPBITS: u32 =		1003;
pub const GENSIO_ACONTROL_SER_FLOWCONTROL: u32 =	1004;
pub const GENSIO_ACONTROL_SER_IFLOWCONTROL: u32 =	1005;
pub const GENSIO_ACONTROL_SER_SBREAK: u32 =		1006;
pub const GENSIO_ACONTROL_SER_DTR: u32 =		1007;
pub const GENSIO_ACONTROL_SER_RTS: u32 =		1008;
pub const GENSIO_ACONTROL_SER_CTS: u32 =		1009;
pub const GENSIO_ACONTROL_SER_DCD_DSR: u32 =		1010;
pub const GENSIO_ACONTROL_SER_RI: u32 =			1011;
pub const GENSIO_ACONTROL_SER_SIGNATURE: u32 =		1012;

type GensioDS = osfuncs::raw::gensiods;

extern "C" {
    fn printf(s: *const ffi::c_char);
}

pub fn printfit(s: &str) {
    let s1 = ffi::CString::new(s).expect("CString::new failed");
    unsafe {
	printf(s1.as_ptr());
    }
}

/// Open callbacks will need to implement this trait.
pub trait OpDoneErr {
    /// Report an error on the operation.  Unlike most other gensio
    /// interfaces, which pass the error in the done() method, the
    /// error report is done separately here.
    fn done_err(&self, err: i32);

    /// Report that the operation (open) has completed.
    fn done(&self);
}

struct OpDoneErrData {
    cb: Arc<dyn OpDoneErr>
}

/// Acontrol callbacks will need to implement this trait.
pub trait ControlDone {
    /// Report an error on the operation.  Unlike most other gensio
    /// interfaces, which pass the error in the done() method, the
    /// error report is done separately here.
    fn done_err(&self, err: i32);

    /// Report that the operation (Acontrol) has completed.
    fn done(&self, buf: &[u8]);
}

struct ControlDoneData {
    cb: Arc<dyn ControlDone>
}

extern "C" fn control_done(_io: *const raw::gensio,
			   err: ffi::c_int,
			   buf: *const ffi::c_void,
			   len: GensioDS,
			   user_data: *mut ffi::c_void) {
    let d = user_data as *mut ControlDoneData;
    let d = unsafe { Box::from_raw(d) }; // Use from_raw so it will be freed
    let data = buf as *const u8;
    let data = unsafe {
	std::slice::from_raw_parts(data, len as usize)
    };
    match err {
	0 => d.cb.done_err(err),
	_ => d.cb.done(data)
    }
}

extern "C" fn op_done_err(_io: *const raw::gensio, err: ffi::c_int,
			  user_data: *mut ffi::c_void) {
    let d = user_data as *mut OpDoneErrData;
    let d = unsafe { Box::from_raw(d) }; // Use from_raw so it will be freed
    let cb = d.cb;
    if err == 0 {
	cb.done();
    } else {
	cb.done_err(err);
    }
}

/// Close callbacks will need to implement this trait.
pub trait OpDone {
    /// Report that the operation (close) has completed.
    fn done(&self);
}

struct OpDoneData {
    cb: Arc<dyn OpDone>
}

extern "C" fn op_done(_io: *const raw::gensio,
		      user_data: *mut ffi::c_void) {
    let d = user_data as *mut OpDoneData;
    let d = unsafe { Box::from_raw(d) }; // Use from_raw so it will be freed
    d.cb.done();
}

/// The struct that gets callbacks from a gensio will need to
/// implement this trait.
pub trait Event {
    /// A log was reported dealing with the gensio.
    fn log(&self, _s: String) {}

    /// Called when parameter parsing is incorrect in str_to_gensio().
    fn parmlog(&self, _s: String) {}

    /// Report a read error.  Unlike most other gensio interfaces,
    /// which combine the error with the read() method, the error
    /// report is done separately here.
    fn err(&self, err: i32) -> i32;

    /// Report some received data.  The i32 return (first value in
    /// tuble) return is the error return, normally 0, and the u64
    /// (second value) is the number of bytes consumed.
    fn read(&self, buf: &[u8], auxdata: &Option<Vec<String>>) -> (i32, u64);

    fn write_ready(&self) -> i32 {
	GE_NOTSUP
    }

    fn new_channel(&self, _g: Arc<Gensio>, _auxdata: &Option<Vec<String>>)
		   -> i32 {
	GE_NOTSUP
    }

    fn send_break(&self) -> i32 {
	GE_NOTSUP
    }

    fn auth_begin(&self) -> i32 {
	GE_NOTSUP
    }

    fn precert_verify(&self) -> i32 {
	GE_NOTSUP
    }

    fn postcert_verify(&self, _err: i32, _errstr: &Option<String>) -> i32 {
	GE_NOTSUP
    }

    fn password_verify(&self, _passwd: &String) -> i32 {
	GE_NOTSUP
    }

    fn request_password(&self, _maxsize: u64) -> (i32, Option<String>) {
	(GE_NOTSUP, None)
    }

    fn verify_2fa(&self, _data: &[u8]) -> i32 {
	GE_NOTSUP
    }

    fn request_2fa(&self) -> (i32, Option<&[u8]>) {
	(GE_NOTSUP, None)
    }

    fn win_size(&self, _height: u32, _width: u32) { }

    fn modemstate(&self, _state: u32) { }
    fn linestate(&self, _state: u32) { }
    fn flow_state(&self, _state: bool) { }
    fn sync(&self) { }
    fn signature(&self, _data: &[u8]) { }
    fn flush(&self, _val: u32) { }
    fn baud(&self, _speed: u32) { }
    fn datasize(&self, _size: u32) { }
    fn parity(&self, _par: u32) { }
    fn stopbits(&self, _bits: u32) { }
    fn flowcontrol(&self, _flow: u32) { }
    fn iflowcontrol(&self, _flow: u32) { }
    fn sbreak(&self, _sbreak: u32) { }
    fn dtr(&self, _dtr: u32) { }
    fn rts(&self, _rts: u32) { }
    fn modemstate_mask(&self, _state: u32) { }
    fn linestate_mask(&self, _state: u32) { }
}

/// Values for linestate and linestate_mask callbacks.
pub const GENSIO_SER_LINESTATE_DATA_READY: u32 = 	1 << 0;
pub const GENSIO_SER_LINESTATE_OVERRUN_ERR: u32 =	1 << 1;
pub const GENSIO_SER_LINESTATE_PARITY_ERR: u32 = 	1 << 2;
pub const GENSIO_SER_LINESTATE_FRAMING_ERR: u32 =	1 << 3;
pub const GENSIO_SER_LINESTATE_BREAK: u32 =		1 << 4;
pub const GENSIO_SER_LINESTATE_XMIT_HOLD_EMPTY: u32 =	1 << 5;
pub const GENSIO_SER_LINESTATE_XMIT_SHIFT_EMPTY: u32 =	1 << 6;
pub const GENSIO_SER_LINESTATE_TIMEOUT_ERR: u32 =	1 << 7;

/// Values for modemstate and modemstate_mask callbacks.
pub const GENSIO_SER_MODEMSTATE_CTS_CHANGED: u32 =	1 << 0;
pub const GENSIO_SER_MODEMSTATE_DSR_CHANGED: u32 =	1 << 1;
pub const GENSIO_SER_MODEMSTATE_RI_CHANGED: u32 =	1 << 2;
pub const GENSIO_SER_MODEMSTATE_CD_CHANGED: u32 =	1 << 3;
pub const GENSIO_SER_MODEMSTATE_CTS: u32 =		1 << 4;
pub const GENSIO_SER_MODEMSTATE_DSR: u32 =		1 << 5;
pub const GENSIO_SER_MODEMSTATE_RI: u32 =		1 << 6;
pub const GENSIO_SER_MODEMSTATE_CD: u32 =		1 << 7;

/// Values for parity callback.
pub const GENSIO_SER_PARITY_NONE: u32 = 1;
pub const GENSIO_SER_PARITY_ODD: u32 = 2;
pub const GENSIO_SER_PARITY_EVEN: u32 = 3;
pub const GENSIO_SER_PARITY_MARK: u32 = 4;
pub const GENSIO_SER_PARITY_SPACE: u32 = 5;

/// Values for flowcontrol and iflowcontrol callbacks.
pub const GENSIO_SER_FLOWCONTROL_NONE: u32 = 1;
pub const GENSIO_SER_FLOWCONTROL_XON_XOFF: u32 = 2;
pub const GENSIO_SER_FLOWCONTROL_RTS_CTS: u32 = 3;
pub const GENSIO_SER_FLOWCONTROL_DCD: u32 = 4;
pub const GENSIO_SER_FLOWCONTROL_DTR: u32 = 5;
pub const GENSIO_SER_FLOWCONTROL_DSR: u32 = 6;

/// Values for sbreak, dtr, and rts callbacks.
pub const GENSIO_SER_ON: u32 = 1;
pub const GENSIO_SER_OFF: u32 = 2;

/// A gensio
pub struct Gensio {
    o: Arc<osfuncs::IOsFuncs>, // Used to keep the os funcs alive.
    g: *const raw::gensio,
    cb: Arc<dyn Event>,

    // Points to the structure that is passed to the callback, which
    // is different than what is returned to the user.
    myptr: *mut Gensio
}

impl std::fmt::Debug for Gensio {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	write!(f, "gensio {:?}", self.g)
    }
}

// Convert an auxdata, like from a read call, to a vector of strings.
fn auxtovec(auxdata: *const *const ffi::c_char) -> Option<Vec<String>> {
    if auxdata == std::ptr::null() {
	None
    } else {
	let sl = unsafe { std::slice::from_raw_parts(auxdata, 10000) };
	let mut i = 0;
	let mut v: Vec<String> = Vec::new();
	while sl[i] != std::ptr::null() {
	    let cs = unsafe { ffi::CStr::from_ptr(sl[i]) };
	    v.push(cs.to_str().expect("Invalid string").to_string());
	    i += 1;
	}
	if i == 0 {
	    None
	} else {
	    Some(v)
	}
    }
}

// Convert a vector of strings to a vector of pointers to CString raw
// values.  You use as_ptr() to get a pointer to the array for
// something to pass into a C function that takes char **.  You must
// call auxfree() with the returned value, which will consume it and
// free the data.
fn vectoaux(vi: &[String]) -> Result<Vec<*mut ffi::c_char>, i32> {
    let mut vo: Vec<*mut ffi::c_char> = Vec::new();
    for i in vi {
	let cs = match ffi::CString::new(i.clone()) {
	    Ok(v) => v,
	    Err(_) => return Err(GE_INVAL)
	};
	vo.push(ffi::CString::into_raw(cs));
    }
    return Ok(vo);
}

// Free the value returned by vectoaux().
fn auxfree(v: Option<Vec<*mut ffi::c_char>>) {
    match v {
	None => (),
	Some(x) => {
	    for i in x {
		let cs = unsafe { ffi::CString::from_raw(i) };
		drop(cs);
	    }
	}
    }
}

struct DummyEvHndl {
}

impl Event for DummyEvHndl {
    fn err(&self, _err: i32) -> i32 {
	GE_NOTSUP
    }

    fn read(&self, _buf: &[u8], _auxdata: &Option<Vec<String>>) -> (i32, u64) {
	(GE_NOTSUP, 0)
    }
}

extern "C" fn evhndl(_io: *const raw::gensio, user_data: *const ffi::c_void,
		     event: ffi::c_int, ierr: ffi::c_int,
		     buf: *const ffi::c_void, buflen: *mut GensioDS,
		     auxdata: *const *const ffi::c_char) -> ffi::c_int
{
    let g = user_data as *mut Gensio;

    let mut err = 0;
    match event {
	raw::GENSIO_EVENT_LOG => {
	    let s = unsafe { raw::gensio_loginfo_to_str(buf) };
	    if s != std::ptr::null_mut() {
		let cs = unsafe { ffi::CStr::from_ptr(s) };
		let logstr = cs.to_str().expect("Invalid string").to_string();
		unsafe { (*g).cb.log(logstr); }
		unsafe { raw::gensio_free_loginfo_str(s); }
	    }
	    err = 0;
	}
	raw::GENSIO_EVENT_PARMLOG => {
	    let s = unsafe { raw::gensio_parmlog_to_str(buf) };
	    if s != std::ptr::null_mut() {
		let cs = unsafe { ffi::CStr::from_ptr(s) };
		let logstr = cs.to_str().expect("Invalid string").to_string();
		unsafe { (*g).cb.parmlog(logstr); }
		unsafe { raw::gensio_free_loginfo_str(s); }
	    }
	    err = 0;
	}
	raw::GENSIO_EVENT_READ => {
	    if ierr != 0 {
		return unsafe {(*g).cb.err(ierr)};
	    }

	    // Convert the buffer into a slice.  You can't use it directly as
	    // a pointer to create a CString with from_raw() because then Rust
	    // takes over ownership of the data, and will free it when this
	    // function exits.
	    let b = unsafe {
		std::slice::from_raw_parts(buf as *mut u8, *buflen as usize)
	    };
	    let a = auxtovec(auxdata);
	    let count;
	    (err, count) = unsafe { (*g).cb.read(b, &a) };
	    unsafe { *buflen = count as GensioDS; }
	}
	raw::GENSIO_EVENT_WRITE_READY => {
	    err = unsafe { (*g).cb.write_ready() };
	}
	raw::GENSIO_EVENT_NEW_CHANNEL => {
	    let g2 = buf as *const raw::gensio;
	    let cb = Arc::new(DummyEvHndl{ });
	    let d = Box::new(Gensio { o: unsafe {(*g).o.clone() }, g: g2,
				      cb: cb.clone(),
				      myptr: std::ptr::null_mut() });
	    let d = Box::into_raw(d);
	    unsafe {
		raw::gensio_set_callback((*d).g, evhndl, d as *mut ffi::c_void);
	    }
	    err = unsafe {
		(*g).cb.new_channel(Arc::new(Gensio
					     { o: (*g).o.clone(),
					       g: g2, cb: cb, myptr: d }),
				    &auxtovec(auxdata))
	    };
	}
	raw::GENSIO_EVENT_SEND_BREAK => {
	    err = unsafe { (*g).cb.send_break() };
	}
	raw::GENSIO_EVENT_AUTH_BEGIN => {
	    err = unsafe { (*g).cb.auth_begin() };
	}
	raw::GENSIO_EVENT_PRECERT_VERIFY => {
	    err = unsafe { (*g).cb.precert_verify() };
	}
	raw::GENSIO_EVENT_POSTCERT_VERIFY => {
	    let errstr = {
		if auxdata == std::ptr::null() {
		    None
		} else {
		    let sl = unsafe { std::slice::from_raw_parts(auxdata,
								 10000) };
		    let cs = unsafe { ffi::CStr::from_ptr(sl[0]) };
		    Some(cs.to_str().expect("Invalid string").to_string())
		}
	    };
	    err = unsafe { (*g).cb.postcert_verify(ierr, &errstr) };
	}
	raw::GENSIO_EVENT_PASSWORD_VERIFY => {
	    let cs = unsafe { ffi::CStr::from_ptr(buf as *const i8) };
	    let s = cs.to_str().expect("Invalid string").to_string();
	    err = unsafe { (*g).cb.password_verify(&s) };
	}
	raw::GENSIO_EVENT_REQUEST_PASSWORD => {
	    let s;
	    let maxlen = unsafe {*buflen as usize };
	    (err, s) = unsafe { (*g).cb.request_password(maxlen as u64) };
	    if err == 0 {
		match s {
		    None => return GE_INVAL,
		    Some(s) => {
			let len = s.len();
			if len > maxlen {
			    return GE_TOOBIG;
			}
			let cs = match ffi::CString::new(s) {
			    Ok(v) => v,
			    Err(_) => return GE_INVAL
			};
			let src = cs.to_bytes();
			let dst = buf as *mut u8;
			let dst = unsafe {
			    std::slice::from_raw_parts_mut(dst, maxlen)
			};
			for i in 0 .. len - 1 {
			    dst[i] = src[i];
			}
			unsafe { *buflen = len as GensioDS; }
		    }
		}
	    }
	}
	raw::GENSIO_EVENT_REQUEST_2FA => {
	    let src;
	    (err, src) = unsafe { (*g).cb.request_2fa() };
	    if err != 0 {
		return err;
	    }
	    let src = match src {
		None => return GE_INVAL,
		Some(v) => v
	    };
	    let len = src.len();
	    let dst = unsafe {
		osfuncs::raw::gensio_os_funcs_zalloc((*g).o.o, len as GensioDS)
	    };
	    let dst = dst as *mut u8;
	    let dst = unsafe {std::slice::from_raw_parts_mut(dst, len) };
	    for i in 0 .. len - 1 {
		dst[i] = src[i];
	    }
	    unsafe { *buflen = len as GensioDS; }
	}
	raw::GENSIO_EVENT_2FA_VERIFY => {
	    let data = buf as *const u8;
	    let data = unsafe {
		std::slice::from_raw_parts(data, *buflen as usize)
	    };
	    err = unsafe { (*g).cb.verify_2fa(data) };
	}
	raw::GENSIO_EVENT_WIN_SIZE => {
	    let data = buf as *mut u8;
	    let data = unsafe {
		Vec::from_raw_parts(data, *buflen as usize, *buflen as usize)
	    };
	    let str = String::from_utf8(data).unwrap();
	    let str: Vec<&str> = str.split(":").collect();
	    if str.len() >= 2 {
		let height: u32 = str[0].parse().unwrap();
		let width: u32 = str[1].parse().unwrap();
		unsafe { (*g).cb.win_size(height, width); }
	    }
	}
	raw::GENSIO_EVENT_SER_MODEMSTATE => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data } as u32;
	    unsafe { (*g).cb.modemstate(data); }
	}
	raw::GENSIO_EVENT_SER_LINESTATE => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data } as u32;
	    unsafe { (*g).cb.linestate(data); }
	}
	raw::GENSIO_EVENT_SER_MODEMSTATE_MASK => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data } as u32;
	    unsafe { (*g).cb.modemstate_mask(data); }
	}
	raw::GENSIO_EVENT_SER_LINESTATE_MASK => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data } as u32;
	    unsafe { (*g).cb.linestate_mask(data); }
	}
	raw::GENSIO_EVENT_SER_SIGNATURE => {
	    let data = buf as *const u8;
	    let data = unsafe {
		std::slice::from_raw_parts(data, *buflen as usize)
	    };
	    unsafe { (*g).cb.signature(data); }
	}
	raw::GENSIO_EVENT_SER_FLOW_STATE => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data } as u32;
	    let data = data == 1;
	    unsafe { (*g).cb.flow_state(data); }
	}
	raw::GENSIO_EVENT_SER_FLUSH => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data } as u32;
	    unsafe { (*g).cb.flush(data); }
	}
	raw::GENSIO_EVENT_SER_SYNC => {
	    unsafe { (*g).cb.sync(); }
	}
	raw::GENSIO_EVENT_SER_BAUD => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data } as u32;
	    unsafe { (*g).cb.baud(data); }
	}
	raw::GENSIO_EVENT_SER_DATASIZE => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data } as u32;
	    unsafe { (*g).cb.datasize(data); }
	}
	raw::GENSIO_EVENT_SER_PARITY => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data } as u32;
	    unsafe { (*g).cb.parity(data); }
	}
	raw::GENSIO_EVENT_SER_STOPBITS => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data } as u32;
	    unsafe { (*g).cb.stopbits(data); }
	}
	raw::GENSIO_EVENT_SER_FLOWCONTROL => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data } as u32;
	    unsafe { (*g).cb.flowcontrol(data); }
	}
	raw::GENSIO_EVENT_SER_IFLOWCONTROL => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data } as u32;
	    unsafe { (*g).cb.iflowcontrol(data); }
	}
	raw::GENSIO_EVENT_SER_SBREAK => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data } as u32;
	    unsafe { (*g).cb.sbreak(data); }
	}
	raw::GENSIO_EVENT_SER_DTR => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data } as u32;
	    unsafe { (*g).cb.dtr(data); }
	}
	raw::GENSIO_EVENT_SER_RTS => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data } as u32;
	    unsafe { (*g).cb.rts(data); }
	}
	_ => err = GE_NOTSUP
    }
    err
}

/// Allocate a new gensio based upon the given string.  We pass in an
/// Arc holding the reference to the event handler.  This function
/// clones it so it can make sure the data stays around until the
/// gensio is closed.
pub fn new(s: String, o: &osfuncs::OsFuncs, cb: Arc<dyn Event>)
	   -> Result<Gensio, i32>
{
    let or = o.raw().clone();
    let g: *const raw::gensio = std::ptr::null();
    let s = match ffi::CString::new(s) {
	Ok(s) => s,
	Err(_) => return Err(GE_INVAL)
    };
    // Create a temporary data item so str_to_gensio can report parmlogs.
    let dt = Box::new(Gensio { o: or.clone(), g: std::ptr::null(),
			       cb: cb.clone(),
			       myptr: std::ptr::null_mut() });
    let dt = Box::into_raw(dt);
    let err = unsafe {
	raw::str_to_gensio(s.as_ptr(), or.o, evhndl,
			   dt as *mut ffi::c_void, &g)
    };
    let rv = match err {
	0 => {
	    let d = Box::new(Gensio { o: or.clone(), g: g, cb: cb.clone(),
				      myptr: std::ptr::null_mut() });
	    let d = Box::into_raw(d);
	    unsafe {
		raw::gensio_set_user_data((*d).g, d as *mut ffi::c_void);
	    }
	    Ok(Gensio { o: or, g: g, cb: cb, myptr: d })
	}
	_ => Err(GE_INVAL)
    };
    let _dt = unsafe {Box::from_raw(dt) }; // Free our original box
    rv
}

fn duration_to_gensio_time(t: &mut osfuncs::raw::gensio_time,
			   time: Option<&Duration>)
			   -> *const osfuncs::raw::gensio_time {
    match time {
	None => {
	    std::ptr::null()
	}
	Some(gt) => {
	    *t = osfuncs::raw::gensio_time{
		secs: gt.as_secs() as i64,
		nsecs: gt.subsec_nanos() as i32 };
	    &*t
	}
    }
}

impl Gensio {
    /// Open the gensio.  The cb will be called when the operation
    /// completes.  Note that the Arc holding the callback is done so
    /// the callback data can be kept around until the callback is
    /// complete.
    ///
    /// Note that the gensio is not open until the callback is called.
    pub fn open(&self, cb: Arc<dyn OpDoneErr>) -> Result<(), i32> {
	let d = Box::new(OpDoneErrData { cb : cb });
	let d = Box::into_raw(d);
	let err = unsafe {
	    raw::gensio_open(self.g, op_done_err, d as *mut ffi::c_void)
	};
	match err {
	    0 => Ok(()),
	    _ => {
		unsafe { drop(Box::from_raw(d)); } // Free the data
		Err(err)
	    }
	}
    }

    pub fn set_handler(&self, cb: Arc<dyn Event>) {
	let g = self.myptr as *mut Gensio;

	unsafe { (*g).cb = cb; }
    }

    // Open the gensio synchronously.  Wait until the open completes
    // before returning.
    pub fn open_s(&self) -> Result<(), i32> {
	let err = unsafe {
	    raw::gensio_open_s(self.g)
	};
	match err {
	    0 => Ok(()),
	    _ => Err(err)
	}
    }

    /// Close the gensio.  The cb will be called when the operation
    /// completes.  Note that the Arc holding the callback is done so
    /// the callback data can be kept around until the callback is
    /// complete.
    ///
    /// Note that the gensio is not closed until the callback is called.
    pub fn close(&self, cb: Arc<dyn OpDone>) -> Result<(), i32> {
	let d = Box::new(OpDoneData { cb : cb });
	let d = Box::into_raw(d);
	let err = unsafe {
	    raw::gensio_close(self.g, op_done, d as *mut ffi::c_void)
	};
	match err {
	    0 => Ok(()),
	    _ => {
		unsafe { drop(Box::from_raw(d)); } // Free the data
		Err(err)
	    }
	}
    }

    // Close the gensio synchronously.  Wait until the close completes
    // before returning.
    pub fn close_s(&self) -> Result<(), i32> {
	let err = unsafe {
	    raw::gensio_close_s(self.g)
	};
	match err {
	    0 => Ok(()),
	    _ => Err(err)
	}
    }

    /// Write some data to the gensio.  On success, the number of
    /// bytes written is returned.  On failure an error code is
    /// returned.
    pub fn write(&self, data: &[u8], auxdata: Option<&[String]>)
		 -> Result<u64, i32> {
	let mut count: GensioDS = 0;
	let a1 = match auxdata {
	    None => None,
	    Some(ref v) => Some(vectoaux(v)?)
	};
	let a2: *mut *mut ffi::c_char = match a1 {
	    None => std::ptr::null_mut(),
	    Some(ref v) => v.as_ptr() as *mut *mut ffi::c_char
	};

	let err = unsafe {
	    raw::gensio_write(self.g, &mut count,
			      data.as_ptr() as *const ffi::c_void,
			      data.len() as GensioDS,
			      a2 as *const *const ffi::c_char)
	};
	auxfree(a1);
	match err {
	    0 => Ok(count),
	    _ => Err(err)
	}
    }

    /// Write some data to the gensio.  On success, the number of
    /// bytes written is returned.  On failure an error code is
    /// returned.
    pub fn write_s(&self, data: &[u8], timeout: Option<&Duration>)
		   -> Result<u64, i32> {
	let mut t = osfuncs::raw::gensio_time { secs: 0, nsecs: 0 };
	let tptr: *const osfuncs::raw::gensio_time
	    = duration_to_gensio_time(&mut t, timeout);
	let mut count: GensioDS = 0;

	let err = unsafe {
	    raw::gensio_write_s(self.g, &mut count,
				data.as_ptr() as *const ffi::c_void,
				data.len() as GensioDS, tptr)
	};
	match err {
	    0 => Ok(count),
	    _ => Err(err)
	}
    }

    /// Write some data to the gensio.  Allow signals to interrupt.
    /// On success, the number of bytes written is returned.  On
    /// failure an error code is returned.
    pub fn write_s_intr(&self, data: &[u8], timeout: Option<&Duration>)
			-> Result<u64, i32> {
	let mut t = osfuncs::raw::gensio_time { secs: 0, nsecs: 0 };
	let tptr: *const osfuncs::raw::gensio_time
	    = duration_to_gensio_time(&mut t, timeout);
	let mut count: GensioDS = 0;

	let err = unsafe {
	    raw::gensio_write_s_intr(self.g, &mut count,
				     data.as_ptr() as *const ffi::c_void,
				     data.len() as GensioDS, tptr)
	};
	match err {
	    0 => Ok(count),
	    _ => Err(err)
	}
    }

    /// Read some data from the gensio.  On success, the number of
    /// bytes read is returned.  On failure an error code is
    /// returned.
    pub fn read_s(&self, data: &mut Vec<u8>, timeout: Option<&Duration>)
		  -> Result<u64, i32> {
	let mut t = osfuncs::raw::gensio_time { secs: 0, nsecs: 0 };
	let tptr: *const osfuncs::raw::gensio_time
	    = duration_to_gensio_time(&mut t, timeout);
	let mut count: GensioDS = 0;

	let err = unsafe {
	    raw::gensio_read_s(self.g, &mut count,
			       data.as_mut_ptr() as *mut ffi::c_void,
			       data.capacity() as GensioDS, tptr)
	};
	match err {
	    0 => {
		unsafe { data.set_len(count as usize); }
		Ok(count)
	    }
	    _ => Err(err)
	}
    }

    /// Read some data from the gensio.  On success, the number of
    /// bytes read is returned.  On failure an error code is
    /// returned.
    pub fn read_s_intr(&self, data: &mut Vec<u8>, timeout: Option<&Duration>)
		  -> Result<u64, i32> {
	let mut t = osfuncs::raw::gensio_time { secs: 0, nsecs: 0 };
	let tptr: *const osfuncs::raw::gensio_time
	    = duration_to_gensio_time(&mut t, timeout);
	let mut count: GensioDS = 0;

	let err = unsafe {
	    raw::gensio_read_s_intr(self.g, &mut count,
				    data.as_mut_ptr() as *mut ffi::c_void,
				    data.capacity() as GensioDS, tptr)
	};
	match err {
	    0 => {
		unsafe { data.set_len(count as usize); }
		Ok(count)
	    }
	    _ => Err(err)
	}
    }

    /// Enable or disable the read callback.
    pub fn read_enable(&self, enable: bool) {
	let enable = match enable { true => 1, false => 0 };
	unsafe {
	    raw::gensio_set_read_callback_enable(self.g, enable);
	}
    }

    /// Enable or disable the write callback.
    pub fn write_enable(&self, enable: bool) {
	let enable = match enable { true => 1, false => 0 };
	unsafe {
	    raw::gensio_set_write_callback_enable(self.g, enable);
	}
    }


    /// Call gensio_control() with the given options.  As much data as
    /// can be held is stored in data upon return and the required
    /// size to return all data is returned in Ok().
    pub fn control(&self, depth: i32, get: bool, option: u32,
		   data: &mut Vec<u8>) -> Result<usize, i32> {
	let err;
	let mut len: GensioDS;
	unsafe {
	    len = data.capacity() as GensioDS;
	    if len == 0 {
		// If we don't have at least one byte, as_mut_ptr()
		// will return null.
		data.push(0);
		len = 1;
	    }
	    err = raw::gensio_control(self.g, depth, get as i32, option,
				      data.as_mut_ptr() as *mut ffi::c_void,
				      &mut len);
	}
	if err == 0 {
	    if len < data.capacity() as GensioDS {
		unsafe {data.set_len(len as usize); }
	    }
	    Ok(len as usize)
	} else {
	    Err(err)
	}
    }

    /// Call gensio_acontrol with the given options.
    pub fn acontrol(&self, depth: i32, get: bool, option: u32,
		    data: &mut Vec<u8>, done: Arc<dyn ControlDone>,
		    timeout: Option<&Duration>)
		    -> Result<(), i32> {
	let d = Box::new(ControlDoneData { cb : done });
	let d = Box::into_raw(d);
	let mut t = osfuncs::raw::gensio_time { secs: 0, nsecs: 0 };
	let tptr: *const osfuncs::raw::gensio_time
	    = duration_to_gensio_time(&mut t, timeout);
	let get = match get { false => 0, true => 1 };
	let err = unsafe {
	    raw::gensio_acontrol(self.g, depth, get, option,
				 data.as_ptr() as *const ffi::c_void,
				 data.capacity() as GensioDS,
				 control_done, d as *mut ffi::c_void, tptr)
	};
	match err {
	    0 => Ok(()),
	    _ => {
		unsafe { drop(Box::from_raw(d)); } // Free the data
		Err(err)
	    }
	}
    }

    /// Call gensio_acontrol_s() with the given options.  As much data as
    /// can be held is stored in data upon return and the required
    /// size to return all data is returned in Ok().
    pub fn acontrol_s(&self, depth: i32, get: bool, option: u32,
		      data: &mut Vec<u8>,
		      timeout: Option<&Duration>) -> Result<usize, i32> {
	let err;
	let mut len: GensioDS;
	let mut t = osfuncs::raw::gensio_time { secs: 0, nsecs: 0 };
	let tptr: *const osfuncs::raw::gensio_time
	    = duration_to_gensio_time(&mut t, timeout);
	unsafe {
	    len = data.capacity() as GensioDS;
	    if len == 0 {
		// If we don't have at least one byte, as_mut_ptr()
		// will return null.
		data.push(0);
		len = 1;
	    }
	    err = raw::gensio_acontrol_s(self.g, depth, get as i32, option,
					 data.as_mut_ptr() as *mut ffi::c_void,
					 &mut len, tptr);
	}
	if err == 0 {
	    if len < data.capacity() as GensioDS {
		unsafe {data.set_len(len as usize); }
	    }
	    Ok(len as usize)
	} else {
	    Err(err)
	}
    }

    /// Call gensio_acontrol_s() with the given options.  As much data as
    /// can be held is stored in data upon return and the required
    /// size to return all data is returned in Ok().
    pub fn acontrol_s_intr(&self, depth: i32, get: bool, option: u32,
			   data: &mut Vec<u8>,
			   timeout: Option<&Duration>) -> Result<usize, i32> {
	let err;
	let mut len: GensioDS;
	let mut t = osfuncs::raw::gensio_time { secs: 0, nsecs: 0 };
	let tptr: *const osfuncs::raw::gensio_time
	    = duration_to_gensio_time(&mut t, timeout);
	unsafe {
	    len = data.capacity() as GensioDS;
	    if len == 0 {
		// If we don't have at least one byte, as_mut_ptr()
		// will return null.
		data.push(0);
		len = 1;
	    }
	    err = raw::gensio_acontrol_s_intr(self.g, depth, get as i32, option,
					 data.as_mut_ptr() as *mut ffi::c_void,
					 &mut len, tptr);
	}
	if err == 0 {
	    if len < data.capacity() as GensioDS {
		unsafe {data.set_len(len as usize); }
	    }
	    Ok(len as usize)
	} else {
	    Err(err)
	}
    }

    /// Call gensio_control() and return a vector holding the result.
    pub fn control_resize(&self, depth: i32, get: bool, option: u32,
			  data: &Vec<u8>) -> Result<Vec<u8>, i32> {
	let mut len: usize;
	let mut data2 = data.clone();
	match self.control(depth, get, option, &mut data2) {
	    Ok(ilen) => len = ilen,
	    Err(err) => return Err(err)
	}
	if len <= data2.capacity() {
	    return Ok(data2);
	}
	let mut data2 = data.clone();
	data2.reserve(len + 1); // Add 1 for C string terminator
	match self.control(depth, get, option, &mut data2) {
	    Ok(ilen) => len = ilen,
	    Err(err) => return Err(err)
	}
	if len >= data2.capacity() {
	    Err(GE_TOOBIG)
	} else {
	    Ok(data2)
	}
    }

    /// Call gensio_control(), passing in the given string.  The result
    /// string is returned in Ok().
    pub fn control_str(&self, depth: i32, get: bool, option: u32, val: &str)
		       -> Result<String, i32> {
	let mut valv = val.as_bytes().to_vec();
	match self.control_resize(depth, get, option, &mut valv) {
	    Ok(newv) => Ok(String::from_utf8(newv).unwrap()),
	    Err(err) => Err(err)
	}
    }

    pub fn get_type(&self, depth: u32) -> String {
	let s;
	unsafe {
	    let cs = raw::gensio_get_type(self.g, depth as ffi::c_uint);
	    let cs = ffi::CStr::from_ptr(cs);
	    s = cs.to_str().expect("Invalid string").to_string();
	}
	s
    }

    pub fn is_client(&self) -> bool {
	unsafe {
	    raw::gensio_is_client(self.g) != 0
	}
    }

    pub fn is_reliable(&self) -> bool {
	unsafe {
	    raw::gensio_is_reliable(self.g) != 0
	}
    }

    pub fn is_packet(&self) -> bool {
	unsafe {
	    raw::gensio_is_packet(self.g) != 0
	}
    }

    pub fn is_authenticated(&self) -> bool {
	unsafe {
	    raw::gensio_is_authenticated(self.g) != 0
	}
    }

    pub fn is_encrypted(&self) -> bool {
	unsafe {
	    raw::gensio_is_encrypted(self.g) != 0
	}
    }

    pub fn is_message(&self) -> bool {
	unsafe {
	    raw::gensio_is_message(self.g) != 0
	}
    }

    pub fn is_mux(&self) -> bool {
	unsafe {
	    raw::gensio_is_mux(self.g) != 0
	}
    }

    pub fn is_serial(&self) -> bool {
	unsafe {
	    raw::gensio_is_serial(self.g) != 0
	}
    }

    pub fn set_sync(&self) -> Result<(), i32> {
	let err = unsafe { raw::gensio_set_sync(self.g) };
	match err {
	    0 => Ok(()),
	    _ => Err(err)
	}
    }

    pub fn clear_sync(&self) -> Result<(), i32> {
	let err = unsafe { raw::gensio_clear_sync(self.g) };
	match err {
	    0 => Ok(()),
	    _ => Err(err)
	}
    }
}

impl Drop for Gensio {
    fn drop(&mut self) {
	unsafe {
	    // Only the Gensio given to the user has a pointer set in
	    // myptr, so we clean when the main gensio is freed then
	    // free the one passed to the callbacks.
	    if self.myptr != std::ptr::null_mut() {
		raw::gensio_close_s(self.g);
		raw::gensio_free(self.g);
		drop(Box::from_raw(self.myptr));
	    }
	}
    }
}

/// The struct that gets callbacks from a gensio will need to
/// implement this trait.
pub trait AccepterEvent {
    /// A log was reported dealing with the gensio.
    fn log(&self, _s: String) {}

    /// Called when parameter parsing is incorrect in str_to_gensio().
    fn parmlog(&self, _s: String) {}

    fn new_connection(&self, g: Arc<Gensio>) -> i32;

    fn auth_begin(&self) -> i32 {
	GE_NOTSUP
    }

    fn precert_verify(&self) -> i32 {
	GE_NOTSUP
    }

    fn postcert_verify(&self, _err: i32, _errstr: &Option<String>) -> i32 {
	GE_NOTSUP
    }

    fn password_verify(&self, _passwd: &String) -> i32 {
	GE_NOTSUP
    }

    fn request_password(&self, _maxsize: u64) -> (i32, Option<String>) {
	(GE_NOTSUP, None)
    }

    fn verify_2fa(&self, _data: &[u8]) -> i32 {
	GE_NOTSUP
    }

    fn request_2fa(&self) -> (i32, Option<&[u8]>) {
	(GE_NOTSUP, None)
    }
}

pub const GENSIO_ACC_CONTROL_LADDR: u32 = 1;
pub const GENSIO_ACC_CONTROL_LPORT: u32 = 2;
pub const GENSIO_ACC_CONTROL_TCPDNAME: u32 = 3;

pub struct Accepter {
    o: Arc<osfuncs::IOsFuncs>, // Used to keep the os funcs alive.
    a: *const raw::gensio_accepter,
    cb: Arc<dyn AccepterEvent>,

    // Points to the structure that is passed to the callback, which
    // is different than what is returned to the user.
    myptr: *mut Accepter
}

extern "C" fn acc_evhndl(_acc: *const raw::gensio_accepter,
			 user_data: *const ffi::c_void,
			 event: ffi::c_int,
			 data: *const ffi::c_void)
			 -> ffi::c_int {
    let a = user_data as *mut Accepter;

    let err: i32;
    match event {
	raw::GENSIO_ACC_EVENT_LOG => {
	    let s = unsafe { raw::gensio_loginfo_to_str(data) };
	    if s != std::ptr::null_mut() {
		let cs = unsafe { ffi::CStr::from_ptr(s) };
		let logstr = cs.to_str().expect("Invalid string").to_string();
		unsafe { (*a).cb.log(logstr); }
		unsafe { raw::gensio_free_loginfo_str(s); }
	    }
	    err = 0;
	}
	raw::GENSIO_ACC_EVENT_PARMLOG => {
	    let s = unsafe { raw::gensio_parmlog_to_str(data) };
	    if s != std::ptr::null_mut() {
		let cs = unsafe { ffi::CStr::from_ptr(s) };
		let logstr = cs.to_str().expect("Invalid string").to_string();
		unsafe { (*a).cb.parmlog(logstr); }
		unsafe { raw::gensio_free_loginfo_str(s); }
	    }
	    err = 0;
	}
	raw::GENSIO_ACC_EVENT_NEW_CONNECTION => {
	    let g = data as *const raw::gensio;
	    let cb = Arc::new(DummyEvHndl{ });
	    let d = Box::new(Gensio { o: unsafe {(*a).o.clone() }, g: g,
				      cb: cb.clone(),
				      myptr: std::ptr::null_mut() });
	    let d = Box::into_raw(d);
	    unsafe {
		raw::gensio_set_callback((*d).g, evhndl, d as *mut ffi::c_void);
	    }
	    err = unsafe {
		(*a).cb.new_connection(Arc::new(Gensio
						{ o: (*a).o.clone(),
						  g: g, cb: cb, myptr: d }))
	    };
	}
	raw::GENSIO_ACC_EVENT_AUTH_BEGIN => {
	    err = unsafe { (*a).cb.auth_begin() };
	}
	raw::GENSIO_ACC_EVENT_PRECERT_VERIFY => {
	    err = unsafe { (*a).cb.precert_verify() };
	}
	raw::GENSIO_ACC_EVENT_POSTCERT_VERIFY => {
	    let vd = data as *const raw::gensio_acc_postcert_verify_data;
	    let errstr = {
		if unsafe {(*vd).errstr } == std::ptr::null() {
		    None
		} else {
		    let cs = unsafe { ffi::CStr::from_ptr((*vd).errstr) };
		    Some(cs.to_str().expect("Invalid string").to_string())
		}
	    };
	    err = unsafe { (*a).cb.postcert_verify((*vd).err, &errstr) };
	}
	raw::GENSIO_ACC_EVENT_PASSWORD_VERIFY => {
	    let vd = data as *const raw::gensio_acc_password_verify_data;
	    let cs = unsafe { ffi::CStr::from_ptr((*vd).password) };
	    let s = cs.to_str().expect("Invalid string").to_string();
	    err = unsafe { (*a).cb.password_verify(&s) };
	}
	raw::GENSIO_ACC_EVENT_REQUEST_PASSWORD => {
	    let mut vd = data as *mut raw::gensio_acc_password_verify_data;
	    let s;
	    let maxlen = unsafe { (*vd).password_len } as usize;
	    (err, s) = unsafe { (*a).cb.request_password(maxlen as u64) };
	    if err == 0 {
		match s {
		    None => return GE_INVAL,
		    Some(s) => {
			let len = s.len();
			if len > maxlen {
			    return GE_TOOBIG;
			}
			let cs = match ffi::CString::new(s) {
			    Ok(v) => v,
			    Err(_) => return GE_INVAL
			};
			let src = cs.to_bytes();
			let dst = unsafe { (*vd).password as *mut u8 };
			let dst = unsafe {
			    std::slice::from_raw_parts_mut(dst, maxlen)
			};
			for i in 0 .. len - 1 {
			    dst[i] = src[i];
			}
			unsafe { (*vd).password_len = len as GensioDS; }
		    }
		}
	    }
	}
	raw::GENSIO_ACC_EVENT_2FA_VERIFY => {
	    let vd = data as *mut raw::gensio_acc_password_verify_data;
	    let data = unsafe { (*vd).password } as *const u8;
	    let data = unsafe {
		std::slice::from_raw_parts(data, (*vd).password_len as usize)
	    };
	    err = unsafe { (*a).cb.verify_2fa(data) };
	}
	raw::GENSIO_ACC_EVENT_REQUEST_2FA => {
	    let mut vd = data as *mut raw::gensio_acc_password_verify_data;
	    let src;
	    (err, src) = unsafe { (*a).cb.request_2fa() };
	    if err != 0 {
		return err;
	    }
	    let src = match src {
		None => return GE_INVAL,
		Some(v) => v
	    };
	    let len = src.len();
	    if len > unsafe { (*vd).password_len } as usize {
		return GE_TOOBIG;
	    }
	    let dst = unsafe { (*vd).password as *mut u8 };
	    let dst = unsafe {std::slice::from_raw_parts_mut(dst, len) };
	    for i in 0 .. len - 1 {
		dst[i] = src[i];
	    }
	    unsafe {(*vd).password_len = len as GensioDS; }
	}
	_ => { err = GE_NOTSUP; }
    }
    err
}

pub fn new_accepter(s: String, o: &osfuncs::OsFuncs,
		    cb: Arc<dyn AccepterEvent>)
		    -> Result<Accepter, i32> {
    let or = o.raw().clone();
    let a: *const raw::gensio_accepter = std::ptr::null();
    let s = match ffi::CString::new(s) {
	Ok(s) => s,
	Err(_) => return Err(GE_INVAL)
    };
    // Create a temporary data item so str_to_gensio_accepter can
    // report parmlogs.
    let dt = Box::new(Accepter { o: or.clone(), a: std::ptr::null(),
				 cb: cb.clone(),
				 myptr: std::ptr::null_mut() });
    let dt = Box::into_raw(dt);
    let err = unsafe {
	raw::str_to_gensio_accepter(s.as_ptr(), or.o, acc_evhndl,
				    dt as *mut ffi::c_void, &a)
    };
    let rv = match err {
	0 => {
	    let d = Box::new(Accepter { o: or.clone(), a: a, cb: cb.clone(),
					myptr: std::ptr::null_mut() });
	    let d = Box::into_raw(d);
	    unsafe {
		raw::gensio_acc_set_user_data((*d).a, d as *mut ffi::c_void);
	    }
	    Ok(Accepter { o: or, a: a, cb: cb, myptr: d })
	}
	_ => Err(GE_INVAL)
    };
    let _dt = unsafe {Box::from_raw(dt) }; // Free our original box
    rv
}

/// Shutdown callbacks will need to implement this trait.
pub trait AccepterOpDone {
    /// Report that the operation (close) has completed.
    fn done(&self);
}

struct AccepterOpDoneData {
    cb: Arc<dyn AccepterOpDone>
}

extern "C" fn acc_op_done(_io: *const raw::gensio_accepter,
			  user_data: *mut ffi::c_void) {
    let d = user_data as *mut AccepterOpDoneData;
    let d = unsafe { Box::from_raw(d) }; // Use from_raw so it will be freed
    d.cb.done();
}

impl Accepter {
    pub fn startup(&self) -> Result<(), i32> {
	let err = unsafe {
	    raw::gensio_acc_startup(self.a)
	};
	match err {
	    0 => Ok(()),
	    _ => Err(err)
	}
    }

    /// Note that the accepter is not shut down until the callback is called.
    pub fn shutdown(&self, cb: Arc<dyn AccepterOpDone>) -> Result<(), i32> {
	let d = Box::new(AccepterOpDoneData { cb : cb });
	let d = Box::into_raw(d);
	let err = unsafe {
	    raw::gensio_acc_shutdown(self.a, acc_op_done, d as *mut ffi::c_void)
	};
	match err {
	    0 => Ok(()),
	    _ => {
		unsafe { drop(Box::from_raw(d)); } // Free the data
		Err(err)
	    }
	}
    }

    /// Call gensio_acc_control() with the given options.  As much
    /// data as can be held is stored in data upon return and the
    /// required size to return all data is returned in Ok().
    pub fn control(&self, depth: i32, get: bool, option: u32,
		   data: &mut Vec<u8>) -> Result<usize, i32> {
	let err;
	let mut len: GensioDS;
	unsafe {
	    len = data.capacity() as GensioDS;
	    if len == 0 {
		// If we don't have at least one byte, as_mut_ptr()
		// will return null.
		data.push(0);
		len = data.capacity() as GensioDS;
	    }
	    err = raw::gensio_acc_control(self.a, depth, get as i32, option,
					  data.as_mut_ptr() as *mut ffi::c_void,
					  &mut len);
	}
	if err == 0 {
	    if len < data.capacity() as GensioDS {
		unsafe {data.set_len(len as usize); }
	    }
	    Ok(len as usize)
	} else {
	    Err(err)
	}
    }

    /// Call gensio_acc_control() and return a vector holding the result.
    pub fn control_resize(&self, depth: i32, get: bool, option: u32,
			  data: &Vec<u8>) -> Result<Vec<u8>, i32> {
	let mut len: usize;
	let mut data2 = data.clone();
	match self.control(depth, get, option, &mut data2) {
	    Ok(ilen) => len = ilen,
	    Err(err) => return Err(err)
	}
	if len <= data2.capacity() {
	    return Ok(data2);
	}
	let mut data2 = data.clone();
	data2.reserve(len + 1);
	match self.control(depth, get, option, &mut data2) {
	    Ok(ilen) => len = ilen,
	    Err(err) => return Err(err)
	}
	if len >= data2.capacity() {
	    Err(GE_TOOBIG)
	} else {
	    Ok(data2)
	}
    }

    /// Call gensio_acc_control(), passing in the given string.  The result
    /// string is returned in Ok().
    pub fn control_str(&self, depth: i32, get: bool, option: u32, val: &str)
		       -> Result<String, i32> {
	let mut valv = val.as_bytes().to_vec();
	match self.control_resize(depth, get, option, &mut valv) {
	    Ok(newv) => Ok(String::from_utf8(newv).unwrap()),
	    Err(err) => Err(err)
	}
    }

    pub fn is_reliable(&self) -> bool {
	unsafe {
	    raw::gensio_acc_is_reliable(self.a) != 0
	}
    }

    pub fn is_packet(&self) -> bool {
	unsafe {
	    raw::gensio_acc_is_packet(self.a) != 0
	}
    }

    pub fn is_message(&self) -> bool {
	unsafe {
	    raw::gensio_acc_is_message(self.a) != 0
	}
    }

    pub fn is_mux(&self) -> bool {
	unsafe {
	    raw::gensio_acc_is_mux(self.a) != 0
	}
    }

    pub fn is_serial(&self) -> bool {
	unsafe {
	    raw::gensio_acc_is_serial(self.a) != 0
	}
    }
}

impl Drop for Accepter {
    fn drop(&mut self) {
	unsafe {
	    // Only the Gensio given to the user has a pointer set in
	    // myptr, so we clean when the main gensio is freed then
	    // free the one passed to the callbacks.
	    if self.myptr != std::ptr::null_mut() {
		raw::gensio_acc_shutdown_s(self.a);
		raw::gensio_acc_free(self.a);
		drop(Box::from_raw(self.myptr));
	    }
	}
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;
    use std::sync::Mutex;
    use super::*;

    struct EvStruct {
	w: osfuncs::Waiter
    }

    impl Event for EvStruct {
	fn err(&self, err: i32) -> i32 {
	    assert_eq!(err, 0);
	    0
	}

	fn read(&self, buf: &[u8], _auxdata: &Option<Vec<String>>)
		-> (i32, u64) {
	    assert_eq!(buf.len(), 7);
	    let s = unsafe { std::str::from_utf8_unchecked(buf) };
	    assert_eq!(s, "teststr");
	    self.w.wake().expect("Wake open done failed");
	    (0, buf.len() as u64)
	}
    }

    impl OpDoneErr for EvStruct {
	fn done_err(&self, err: i32) {
	    assert_eq!(err, 0);
	}

	fn done(&self) {
	    self.w.wake().expect("Wake open done failed");
	}
    }

    impl OpDone for EvStruct {
	fn done(&self) {
	    self.w.wake().expect("Wake close done failed");
	}
    }

    struct LogHandler;

    impl osfuncs::GensioLogHandler for LogHandler {
	fn log(&self, _logstr: String) {
	    // What to fill in here?
	}
    }

    #[test]
    fn basic_gensio() {
	let o = osfuncs::new(Arc::new(LogHandler))
	    .expect("Couldn't allocate os funcs");
	let w = o.new_waiter().expect("Couldn't allocate waiter");
	let e = Arc::new(EvStruct { w: w });
	let g = new("echo".to_string(), &o, e.clone())
	    .expect("Couldn't alloc gensio");
	g.open(e.clone()).expect("Couldn't open genio");
	e.w.wait(1, &Duration::new(1, 0)).expect("Wait failed");
	g.read_enable(true);
	let v1 = vec!["t1".to_string(), "t2".to_string()];
	let count = g.write(&b"teststr".to_vec()[..], Some(&v1))
			    .expect("Write failed");
	assert_eq!(count, 7);
	e.w.wait(1, &Duration::new(1, 0)).expect("Wait failed");
	g.close(e.clone()).expect("Couldn't close gensio");
	e.w.wait(1, &Duration::new(1, 0)).expect("Wait failed");
    }

    struct AccMutData {
	logstr: Option<String>,
	ag: Option<Arc<Gensio>>,
    }

    struct AccEvent {
	w: osfuncs::Waiter,
	d: Mutex<AccMutData>,
    }

    impl AccepterEvent for AccEvent {
	fn parmlog(&self, s: String) {
	    let mut d = self.d.lock().unwrap();
	    let chks = match &d.logstr {
		None => {
		    printfit(&format!("Unexpected log: {s}\n").to_string());
		    assert!(false);
		    return;
		}
		Some(s) => s
	    };
	    assert_eq!(&s, chks);
	    d.logstr = None;
	    self.w.wake().expect("Wake failed");
	}

	fn new_connection(&self, g: Arc<Gensio>) -> i32 {
	    let mut d = self.d.lock().unwrap();
	    d.ag = Some(g);
	    self.w.wake().expect("Wake failed");
	    0
	}
    }

    impl AccepterOpDone for AccEvent {
	fn done(&self) {
	    self.w.wake().expect("Wake close done failed");
	}
    }

    struct GenMutData {
	logstr: Option<String>,
	experr: i32,
    }

    struct GenEvent {
	w: osfuncs::Waiter,
	_g: Option<Arc<Gensio>>,
	d: Mutex<GenMutData>,
    }

    impl Event for GenEvent {
	fn parmlog(&self, s: String) {
	    let mut d = self.d.lock().unwrap();
	    let chks = match &d.logstr {
		None => {
		    printfit(&format!("Unexpected log: {s}\n").to_string());
		    assert!(false);
		    return;
		}
		Some(s) => s
	    };
	    assert_eq!(&s, chks);
	    d.logstr = None;
	    self.w.wake().expect("Wake failed");
	}

	fn err(&self, err: i32) -> i32 {
	    let d = self.d.lock().unwrap();
	    assert_eq!(d.experr, err);
	    self.w.wake().expect("Wake failed");
	    0
	}

	fn read(&self, _buf: &[u8], _auxdata: &Option<Vec<String>>)
		-> (i32, u64) {
	    (0, 0)
	}
    }

    #[test]
    fn parmerr() {
	let o = osfuncs::new(Arc::new(LogHandler))
	    .expect("Couldn't allocate os funcs");

	let w = o.new_waiter().expect("Couldn't allocate waiter");
	let d = Mutex::new(AccMutData { logstr: None, ag: None });
	let e = Arc::new(AccEvent { w: w, d: d });

	{
	    let mut d = e.d.lock().unwrap();
	    d.logstr = Some(
		"accepter base: Unknown gensio type: asdf,127.0.0.1:1234"
		    .to_string());
	}
	let a = new_accepter("asdf,127.0.0.1:1234".to_string(), &o, e.clone());
	match a {
	    Ok(_a) => assert!(false),
	    Err(e) => assert_eq!(e, GE_INVAL)
	};
    }

    #[test]
    fn acc_conn1() {
	let o = osfuncs::new(Arc::new(LogHandler))
	    .expect("Couldn't allocate os funcs");

	let w = o.new_waiter().expect("Couldn't allocate waiter");
	let d = Mutex::new(AccMutData { logstr: None, ag: None });
	let e1 = Arc::new(AccEvent { w: w, d: d });
	let a = new_accepter("tcp,127.0.0.1,0".to_string(), &o, e1.clone())
	    .expect("Couldn't allocate accepter");
	a.startup().expect("Couldn't start accepter");
	let port = match a.control_str(0, GENSIO_CONTROL_GET,
				       GENSIO_ACC_CONTROL_LPORT, "") {
	    Ok(s) => s,
	    Err(err) => panic!("Error getting acc laddr {err}")
	};

	let w = o.new_waiter().expect("Couldn't allocate waiter");
	let d = Mutex::new(GenMutData { logstr: None, experr: 0 });
	let e2 = Arc::new(GenEvent { w: w, _g: None, d: d });
	let g = new(format!("tcp,127.0.0.1,{port}").to_string(),
		    &o, e2.clone())
	    .expect("Couldn't alocate gensio");
	g.open_s().expect("Couldn't open gensio");

	e1.w.wait(1, &Duration::new(1, 0)).expect("Wait failed");
	// Let automatic cleanup happen.
    }

    #[test]
    fn acc_conn2() {
	let o = osfuncs::new(Arc::new(LogHandler))
	    .expect("Couldn't allocate os funcs");

	let w = o.new_waiter().expect("Couldn't allocate waiter");
	let d = Mutex::new(AccMutData { logstr: None, ag: None });
	let e1 = Arc::new(AccEvent { w: w, d: d });
	let a = new_accepter("tcp,127.0.0.1,0".to_string(), &o, e1.clone())
	    .expect("Couldn't allocate accepter");
	a.startup().expect("Couldn't start accepter");
	let port = match a.control_str(0, GENSIO_CONTROL_GET,
				       GENSIO_ACC_CONTROL_LPORT, "") {
	    Ok(s) => s,
	    Err(err) => panic!("Error getting acc laddr {err}")
	};

	let w = o.new_waiter().expect("Couldn't allocate waiter");
	let d = Mutex::new(GenMutData { logstr: None, experr: 0 });
	let e2 = Arc::new(GenEvent { w: w, _g: None, d: d });
	let g = new(format!("tcp,127.0.0.1,{port}").to_string(),
		    &o, e2.clone()).
	    expect("Couldn't alocate gensio");
	g.open_s().expect("Couldn't open gensio");

	e1.w.wait(1, &Duration::new(1, 0)).expect("Wait failed");

	// Assign a handler for the new gensio
	let e3;
	{
	    let w = o.new_waiter().expect("Couldn't allocate waiter");
	    let d = Mutex::new(GenMutData { logstr: None,
					    experr: GE_REMCLOSE });
	    e3 = Arc::new(GenEvent { w: w, _g: None, d: d });
	    let d1 = e1.d.lock().unwrap();
	    match &d1.ag {
		None => assert!(false),
		Some(d2) => {
		    d2.set_handler(e3.clone());
		}
	    }

	}
	assert_eq!(g.get_type(0), "tcp".to_string());

	{
	    let d1 = e1.d.lock().unwrap();
	    let ag;
	    match &d1.ag {
		None => panic!("No gensio"),
		Some(d2) => ag = d2
	    }

	    g.set_sync().expect("Set sync 1 failed");
	    ag.set_sync().expect("Set sync 2 failed");
	    let v: [u8; 4] = [ 10, 20, 30, 40 ];
	    let l = g.write_s(&v, None).expect("Write failed");
	    assert_eq!(l, 4);

	    let mut data: Vec<u8> = Vec::new();
	    data.reserve(10);
	    let l = ag.read_s(&mut data, None).expect("Read failed");
	    assert_eq!(l, 4);
	    for i in 0 .. 4 {
		assert_eq!(v[i], data[i]);
	    }
	    ag.clear_sync().expect("Clear sync failed");
	    ag.read_enable(true);
	}

	g.close_s().expect("Close failed");
	// Wait for the error from the other end.
	e3.w.wait(1, &Duration::new(1, 0)).expect("Wait failed");

	a.shutdown(e1.clone()).expect("Shutdown failed");
	e1.w.wait(1, &Duration::new(1, 0)).expect("Wait failed");
    }
}
