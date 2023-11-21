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
use std::fmt;
use std::sync::Arc;
use std::sync::Weak;
use std::sync::Mutex;
use std::time::Duration;
use std::panic;

pub mod osfuncs;
pub mod raw;
pub mod addr;
pub mod mdns;
pub mod netifs;

use crate::osfuncs::OsFuncs;
use crate::osfuncs::Waiter;

/// gensio error values.  See gensio_err.3
#[repr(i32)]
#[derive(Copy, Clone, PartialEq)]
pub enum Error {
    NoErr		= 0,
    NoMem		= 1,
    NotSup		= 2,
    Inval		= 3,
    NotFound		= 4,
    Exists		= 5,
    OutOfRange		= 6,
    Inconsistent	= 7,
    NoData		= 8,
    OsErr		= 9,
    InUse		= 10,
    InProgress		= 11,
    NotReady		= 12,
    TooBig		= 13,
    TimedOut		= 14,
    Retry		= 15,
    KeyNotFound		= 17,
    CertRevoked		= 18,
    CertExpired		= 19,
    KeyInvalid		= 20,
    NoCert		= 21,
    CertInvalid		= 22,
    ProtoErr		= 23,
    CommErr		= 24,
    IOErr		= 25,
    RemClose		= 26,
    HostDown		= 27,
    ConnRefuse		= 28,
    DataMissing		= 29,
    CertNotFound	= 30,
    AuthReject		= 31,
    AddrInUse		= 32,
    Interrupted		= 33,
    Shutdown		= 34,
    LocalClosed		= 35,
    Perm		= 36,
    AppErr		= 37,
    UnknownNameErr	= 38,
    NameErr		= 39,
    NameServerFailure	= 40,
    NameInvalid		= 41,
    NameNetNotUp	= 42,
}
const MAX_ERR_VAL:i32 = 42;

fn val_to_error(val: ffi::c_int) -> Error {
    if (0..MAX_ERR_VAL + 1).contains(&val) {
	unsafe { std::mem::transmute(val) }
    } else {
	Error::Inval
    }
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
	write!(f, "gensio error: {}", err_to_str(*self))
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
	write!(f, "gensio error: {}", err_to_str(*self))
    }
}

/// Values for the first parameter of control functions.
pub const CONTROL_DEPTH_ALL: i32 =	-1;
pub const CONTROL_DEPTH_FIRST: i32 =	-2;

/// Values for the second parameter of control functions.
#[derive(Copy, Clone, PartialEq)]
pub enum ControlDir { Get, Set, }

fn control_dir_to_cbool(dir: ControlDir) -> ffi::c_int {
    match dir { ControlDir::Get => 1, ControlDir::Set => 0 }
}

/// Values for the third parameter of control functions.  See gensio_control.3
#[derive(Copy, Clone, PartialEq)]
pub enum ControlOp {
    NoDelay		= 1,
    Streams		= 2,
    SendBreak		= 3,
    GetPeerCertName	= 4,
    CertAuth		= 5,
    Username		= 6,
    Service		= 7,
    Cert		= 8,
    CertFingerprint	= 9,
    Environment		= 10,
    MaxWritePacket	= 11,
    Args		= 12,
    ExitCode		= 13,
    WaitTask		= 14,
    AddMCast		= 15,
    DelMCast		= 16,
    LAddr		= 17,
    LPort		= 18,
    CloseOutput		= 19,
    ConnectAddrStr	= 20,
    Raddr		= 21,
    RaddrBin		= 22,
    RemoteId		= 23,
    KillTask		= 24,
    McastLoop		= 25,
    McastTTL		= 26,
    Password		= 27,
    Get2FA		= 28,
    AuxData		= 29,
    RemAuxData		= 30,
    IOD			= 31,
    ExtraInfo		= 32,
    EnableOOB		= 33,
    WinSize		= 34,
    StartDirectory	= 35,
    InRate		= 36,
    OutRate		= 37,
    InBufsize		= 38,
    OutBufsize		= 39,
    InNrChans		= 40,
    OutNrChans		= 41,
    InFormat		= 42,
    OutFormat		= 43,
    DrainCount		= 44,
    SerModemState	= 45,
    SerFlowcontrolState	= 46,
    SerFlush		= 47,
    SerSendBreak	= 48,
    SerLinestate	= 49,
}

fn control_op_to_val(op: ControlOp) -> ffi::c_uint {
    op as ffi::c_uint
}

/// Values for the third parameter of acontrol() and friends
#[derive(Copy, Clone, PartialEq)]
pub enum AControlOp {
    SerBaud		= 1000,
    SerDatasize		= 1001,
    SerParity		= 1002,
    SerStopbits		= 1003,
    SerFlowcontrol	= 1004,
    SerIFlowcontrol	= 1005,
    SerSBreak		= 1006,
    SerDtr		= 1007,
    SerRts		= 1008,
    SerCts		= 1009,
    SerDcdDsr		= 1010,
    SerRI		= 1011,
    SerSignature	= 1012,
}

fn acontrol_op_to_val(op: AControlOp) -> ffi::c_uint {
    op as ffi::c_uint
}

pub type GensioDS = osfuncs::raw::gensiods;

/// Open callbacks will need to implement this trait.
/// Note: You must keep this object around because it is stored as Weak.
/// If you allow this object to be done, the callbacks will stop working.
pub trait OpDoneErr {
    /// Report an error on the operation.  Unlike most other gensio
    /// interfaces, which pass the error in the done() method, the
    /// error report is done separately here.
    fn done_err(&self, err: Error);

    /// Report that the operation (open) has completed.
    fn done(&self);
}

struct OpDoneErrData {
    cb: Weak<dyn OpDoneErr>
}

/// Acontrol callbacks will need to implement this trait.
/// Note: You must keep this object around because it is stored as Weak.
/// If you allow this object to be done, the callbacks will stop working.
pub trait ControlDone {
    /// Report an error on the operation.  Unlike most other gensio
    /// interfaces, which pass the error in the done() method, the
    /// error report is done separately here.
    fn done_err(&self, err: Error);

    /// Report that the operation (Acontrol) has completed.
    fn done(&self, buf: &[u8]);
}

struct ControlDoneData {
    cb: Weak<dyn ControlDone>
}

fn i_control_done(_io: *const raw::gensio,
		  err: ffi::c_int,
		  buf: *const ffi::c_void,
		  len: GensioDS,
		  user_data: *mut ffi::c_void)
{
    let d = user_data as *mut ControlDoneData;
    let d = unsafe { Box::from_raw(d) }; // Use from_raw so it will be freed
    let data = buf as *const u8;
    let cb = d.cb.upgrade();
    let cb = match cb {
	None => return,
	Some(cb) => cb
    };
    let data = unsafe {
	std::slice::from_raw_parts(data, len as usize)
    };
    match err {
	0 => cb.done(data),
	_ => cb.done_err(val_to_error(err))
    }
}

extern "C" fn control_done(io: *const raw::gensio,
			   err: ffi::c_int,
			   buf: *const ffi::c_void,
			   len: GensioDS,
			   user_data: *mut ffi::c_void) {
    let _r = panic::catch_unwind(|| {
	i_control_done(io, err, buf, len, user_data);
    });
}

fn i_op_done_err(_io: *const raw::gensio, err: ffi::c_int,
		 user_data: *mut ffi::c_void) {
    let d = user_data as *mut OpDoneErrData;
    let d = unsafe { Box::from_raw(d) }; // Use from_raw so it will be freed
    let cb = d.cb.upgrade();
    let cb = match cb {
	None => return,
	Some(cb) => cb
    };
    match err {
	0 => cb.done(),
	_ => cb.done_err(val_to_error(err))
    }
}

extern "C" fn op_done_err(io: *const raw::gensio, err: ffi::c_int,
			  user_data: *mut ffi::c_void) {
    let _r = panic::catch_unwind(|| {
	i_op_done_err(io, err, user_data);
    });
}

/// Close callbacks will need to implement this trait.
/// Note: You must keep this object around because it is stored as Weak.
/// If you allow this object to be done, the callbacks will stop working.
pub trait OpDone {
    /// Report that the operation (close) has completed.
    fn done(&self);
}

extern "C" {
    fn printf(s: *const ffi::c_char, s2: *const ffi::c_char);
}

pub fn puts(s: &str) {
    let s1 = ffi::CString::new(s).expect("CString::new failed");
    let fmt = ffi::CString::new("%s").expect("CString::fmt failed");
    unsafe {
       printf(fmt.as_ptr(), s1.as_ptr());
    }
}

struct CloseDoneData {
    cb: Option<Weak<dyn OpDone>>,
    d: *mut GensioData,
}

fn i_close_done(_io: *const raw::gensio, user_data: *mut ffi::c_void) {
    let d = user_data as *mut CloseDoneData;
    let d = unsafe { Box::from_raw(d) }; // Use from_raw so it will be freed
    match d.cb {
	None => (),
	Some(cb) => {
	    let cb = cb.upgrade();
	    match cb {
		None => (),
		Some(cb) => cb.done()
	    };
	}
    }
    let mut state = unsafe {(*d.d).state.lock().unwrap() };
    match *state {
	GensioState::WaitClose => {
	    // The Gensio is being dropped and waiting for us to complete.
	    unsafe { _ = (*d.d).close_waiter.wake(); }
	}
	_ => *state = GensioState::Closed,
    }
}

extern "C" fn close_done(io: *const raw::gensio, user_data: *mut ffi::c_void) {
    let _r = panic::catch_unwind(|| {
	i_close_done(io, user_data);
    });
}

/// The struct that gets callbacks from a gensio will need to
/// implement this trait.
/// Note: You must keep this object around because it is stored as Weak.
/// If you allow this object to be done, the callbacks will stop working.
pub trait Event {
    /// A log was reported dealing with the gensio.
    fn log(&self, _s: String) {}

    /// Called when parameter parsing is incorrect in str_to_gensio().
    fn parmlog(&self, _s: String) {}

    /// Report a read error.  Unlike most other gensio interfaces,
    /// which combine the error with the read() method, the error
    /// report is done separately here.
    fn err(&self, err: Error) -> Error;

    /// Report some received data.  The i32 return (first value in
    /// tuble) return is the error return, normally 0, and the u64
    /// (second value) is the number of bytes consumed.
    fn read(&self, buf: &[u8], auxdata: Option<&[&str]>) -> (Error, usize);

    fn write_ready(&self) -> Error {
	Error::NotSup
    }

    fn new_channel(&self, _g: Gensio, _auxdata: Option<&[&str]>)
		   -> Error {
	Error::NotSup
    }

    fn send_break(&self) -> Error {
	Error::NotSup
    }

    fn auth_begin(&self) -> Error {
	Error::NotSup
    }

    fn precert_verify(&self) -> Error {
	Error::NotSup
    }

    fn postcert_verify(&self, _err: Error, _errstr: Option<&str>) -> Error {
	Error::NotSup
    }

    fn password_verify(&self, _passwd: &str) -> Error {
	Error::NotSup
    }

    fn request_password(&self, _maxsize: usize) -> (Error, Option<String>) {
	(Error::NotSup, None)
    }

    fn verify_2fa(&self, _data: &[u8]) -> Error {
	Error::NotSup
    }

    fn request_2fa(&self) -> (Error, Option<&[u8]>) {
	(Error::NotSup, None)
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

// There is an issue with close and shutdown.  If those are called
// then the Gensio/Accepter is dropped before the callback is called,
// then there is a race.  We use this to mark if the gensio/accepter
// is in close and wait for the close/shutdown to complete.
enum GensioState {
    Open,
    InClose,
    WaitClose,
    Closed,
}

struct GensioData {
    o: OsFuncs,
    cb: Weak<dyn Event>,
    state: Mutex<GensioState>,
    close_waiter: Waiter,
}

impl GensioData {
    fn new(o: &OsFuncs, cb: Weak<dyn Event>,
	   state: GensioState) -> Result<GensioData, Error> {
        Ok(GensioData {
            o: o.clone(), cb, state: Mutex::new(state),
            close_waiter: Waiter::new(o)?,
        })
    }
}


/// A gensio
pub struct Gensio {
    g: *const raw::gensio,

    // Points to the structure that is passed to the callback.  It's allocated
    // in a Box.
    d: *mut GensioData
}

impl std::fmt::Debug for Gensio {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	write!(f, "gensio {:?}", self.g)
    }
}

/// # Safety
///
/// Convert an auxdata, like from a read callback, to a vector of strings.
/// You must make sure that auxdata is valid, though it may be null.
pub unsafe fn aux_to_stringvec(auxdata: *const *const ffi::c_char)
			       -> Option<Vec<String>> {
    if auxdata.is_null() {
	None
    } else {
	let sl = unsafe { std::slice::from_raw_parts(auxdata, 10000) };
	let mut i = 0;
	let mut v: Vec<String> = Vec::new();
	while !sl[i].is_null() {
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

/// Convert an option vector of strings into an option vector of str.
/// You need this separate from auxtovec because of the lifetime of
/// the strings.
pub fn stringvec_to_strvec(a: &Option<Vec<String>>) -> Option<Vec<&str>> {
    match a {
	None => None,
	Some(a) =>  {
	    let mut r: Vec<&str> = Vec::new();
	    for i in a {
		r.push(i)
	    }
	    Some(r)
	}
    }
}

/// Holder for a vector of pointers to C strings.  Here so the data
/// will be properly freed on an unwind.
pub struct CAuxVec {
    v: Vec<*mut ffi::c_char>,
}

/// Convert a slice of strings to a vector of pointers to CString raw
/// values.  You use as_ptr() to get a pointer to the array for
/// something to pass into a C function that takes char **.
pub fn strslice_to_auxvec(vi: &[&str]) -> Result<CAuxVec, Error> {
    let mut cauxvec = CAuxVec { v: Vec::new() };
    for i in vi {
	let cs = match ffi::CString::new(i.to_string()) {
	    Ok(v) => v,
	    Err(_) => return Err(Error::Inval)
	};
	cauxvec.v.push(ffi::CString::into_raw(cs));
    }
    Ok(cauxvec)
}

impl CAuxVec {
    fn as_ptr(&self) -> *mut *mut ffi::c_char {
	self.v.as_ptr() as *mut *mut ffi::c_char
    }
}

impl Drop for CAuxVec {
    fn drop(&mut self) {
	for i in &self.v {
	    let cs = unsafe { ffi::CString::from_raw(*i) };
	    drop(cs);
	}
    }
}

struct DummyEvHndl {
}

impl Event for DummyEvHndl {
    fn err(&self, _err: Error) -> Error {
	Error::NotSup
    }

    fn read(&self, _buf: &[u8], _auxdata: Option<&[&str]>)
	    -> (Error, usize) {
	(Error::NotSup, 0)
    }
}

fn new_gensio(o: &OsFuncs, g: *const raw::gensio,
	      cb: Weak<dyn Event>, state: GensioState,
	      d: Option<*mut GensioData>) -> Result<Gensio, Error>
{
    let d = match d {
	Some(d) => d,
	None => Box::into_raw(Box::new(GensioData::new(o, cb, state)?))
    };
    Ok(Gensio { g, d })
}

fn i_evhndl(_io: *const raw::gensio, user_data: *const ffi::c_void,
	    event: ffi::c_int, ierr: ffi::c_int,
	    buf: *const ffi::c_void, buflen: *mut GensioDS,
	    auxdata: *const *const ffi::c_char) -> ffi::c_int
{
    let g = user_data as *mut GensioData;
    let cb = unsafe { (*g).cb.upgrade() };
    let cb = match cb {
	None => return Error::NotReady as ffi::c_int,
	Some(cb) => cb
    };

    let mut err = Error::NoErr;
    match event {
	raw::GENSIO_EVENT_LOG => {
	    let s = unsafe { raw::gensio_loginfo_to_str(buf) };
	    if !s.is_null() {
		let cs = unsafe { ffi::CStr::from_ptr(s) };
		let logstr = cs.to_str().expect("Invalid string").to_string();
		cb.log(logstr);
		unsafe { raw::gensio_free_loginfo_str(s); }
	    }
	}
	raw::GENSIO_EVENT_PARMLOG => {
	    let s = unsafe { raw::gensio_parmlog_to_str(buf) };
	    if !s.is_null() {
		let cs = unsafe { ffi::CStr::from_ptr(s) };
		let logstr = cs.to_str().expect("Invalid string").to_string();
		cb.parmlog(logstr);
		unsafe { raw::gensio_free_loginfo_str(s); }
	    }
	}
	raw::GENSIO_EVENT_READ => {
	    if ierr != 0 {
		return cb.err(val_to_error(ierr)) as ffi::c_int;
	    }

	    // Convert the buffer into a slice.  You can't use it directly as
	    // a pointer to create a CString with from_raw() because then Rust
	    // takes over ownership of the data, and will free it when this
	    // function exits.
	    let b = unsafe {
		std::slice::from_raw_parts(buf as *mut u8, *buflen as usize)
	    };
	    let a = unsafe { aux_to_stringvec(auxdata) };
	    let a2 = stringvec_to_strvec(&a);
	    let a3 = a2.as_deref();
	    let count;
	    (err, count) = cb.read(b, a3);
	    unsafe { *buflen = count as GensioDS; }
	}
	raw::GENSIO_EVENT_WRITE_READY => {
	    err = cb.write_ready();
	}
	raw::GENSIO_EVENT_NEW_CHANNEL => {
	    let g2 = buf as *const raw::gensio;
	    let tmpcb = Arc::new(DummyEvHndl{ });
	    // tmpcb will go away in this function, but that's ok, as
	    // it should never get called, and if it does the code
	    // will return an error.
	    let o = unsafe {(*g).o.clone() };
	    let new_g = match new_gensio(&o, g2, Arc::downgrade(&tmpcb) as _,
					 GensioState::Open, None) {
		Ok(g) => g,
		Err(e) => return e as ffi::c_int
	    };
	    unsafe {
		raw::gensio_set_callback(new_g.g, evhndl,
                                         new_g.d as *mut ffi::c_void);
	    }
	    let a = unsafe { aux_to_stringvec(auxdata) };
	    let a2 = stringvec_to_strvec(&a);
	    let a3 = a2.as_deref();
	    err = cb.new_channel(new_g, a3);
	}
	raw::GENSIO_EVENT_SEND_BREAK => {
	    err = cb.send_break();
	}
	raw::GENSIO_EVENT_AUTH_BEGIN => {
	    err = cb.auth_begin();
	}
	raw::GENSIO_EVENT_PRECERT_VERIFY => {
	    err = cb.precert_verify();
	}
	raw::GENSIO_EVENT_POSTCERT_VERIFY => {
	    let errstr = {
		if auxdata.is_null() {
		    None
		} else {
		    let sl = unsafe { std::slice::from_raw_parts(auxdata,
								 10000) };
		    let cs = unsafe { ffi::CStr::from_ptr(sl[0]) };
		    Some(cs.to_str().expect("Invalid string"))
		}
	    };
	    err = cb.postcert_verify(val_to_error(ierr), errstr);
	}
	raw::GENSIO_EVENT_PASSWORD_VERIFY => {
	    let cs = unsafe { ffi::CStr::from_ptr(buf as *const ffi::c_char) };
	    let s = cs.to_str().expect("Invalid string");
	    err = cb.password_verify(s);
	}
	raw::GENSIO_EVENT_REQUEST_PASSWORD => {
	    let s;
	    let maxlen = unsafe {*buflen as usize };
	    (err, s) = cb.request_password(maxlen);
	    if err == Error::NoErr {
		match s {
		    None => return Error::Inval as ffi::c_int,
		    Some(s) => {
			let len = s.len();
			if len > maxlen {
			    return Error::TooBig as ffi::c_int;
			}
			let cs = match ffi::CString::new(s) {
			    Ok(v) => v,
			    Err(_) => return Error::Inval as ffi::c_int
			};
			let src = cs.to_bytes();
			let dst = buf as *mut u8;
			let dst = unsafe {
			    std::slice::from_raw_parts_mut(dst, maxlen)
			};
			dst[..len].copy_from_slice(&src[..len]);
			unsafe { *buflen = len as GensioDS; }
		    }
		}
	    }
	}
	raw::GENSIO_EVENT_REQUEST_2FA => {
	    let src;
	    (err, src) = cb.request_2fa();
	    if err != Error::NoErr {
		return err as ffi::c_int;
	    }
	    let src = match src {
		None => return Error::Inval as ffi::c_int,
		Some(v) => v
	    };
	    let len = src.len();
	    let dst = unsafe { (*g).o.zalloc(len) };
            unsafe {
                let bufptr = buf as *mut *mut ffi::c_void;
                *bufptr = dst;
            }
	    let dst = dst as *mut u8;
	    let dst = unsafe {std::slice::from_raw_parts_mut(dst, len) };
	    dst[..len].copy_from_slice(&src[..len]);
	    unsafe {
                *buflen = len as GensioDS;
            }
	}
	raw::GENSIO_EVENT_2FA_VERIFY => {
	    let data = buf as *const u8;
	    let data = unsafe {
		std::slice::from_raw_parts(data, *buflen as usize)
	    };
	    err = cb.verify_2fa(data);
	}
	raw::GENSIO_EVENT_WIN_SIZE => {
	    let data = buf as *mut u8;
	    let data = unsafe {
		Vec::from_raw_parts(data, *buflen as usize, *buflen as usize)
	    };
	    let str = String::from_utf8_lossy(&data);
	    let str: Vec<&str> = str.split(':').collect();
	    if str.len() >= 2 {
		let height: u32 = str[0].parse().unwrap();
		let width: u32 = str[1].parse().unwrap();
		cb.win_size(height, width);
	    }
	}
	raw::GENSIO_EVENT_SER_MODEMSTATE => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data };
	    cb.modemstate(data);
	}
	raw::GENSIO_EVENT_SER_LINESTATE => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data };
	    cb.linestate(data);
	}
	raw::GENSIO_EVENT_SER_MODEMSTATE_MASK => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data };
	    cb.modemstate_mask(data);
	}
	raw::GENSIO_EVENT_SER_LINESTATE_MASK => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data };
	    cb.linestate_mask(data);
	}
	raw::GENSIO_EVENT_SER_SIGNATURE => {
	    let data = buf as *const u8;
	    let data = unsafe {
		std::slice::from_raw_parts(data, *buflen as usize)
	    };
	    cb.signature(data);
	}
	raw::GENSIO_EVENT_SER_FLOW_STATE => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data };
	    let data = data == 1;
	    cb.flow_state(data);
	}
	raw::GENSIO_EVENT_SER_FLUSH => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data };
	    cb.flush(data);
	}
	raw::GENSIO_EVENT_SER_SYNC => {
	    cb.sync();
	}
	raw::GENSIO_EVENT_SER_BAUD => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data };
	    cb.baud(data);
	}
	raw::GENSIO_EVENT_SER_DATASIZE => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data };
	    cb.datasize(data);
	}
	raw::GENSIO_EVENT_SER_PARITY => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data };
	    cb.parity(data);
	}
	raw::GENSIO_EVENT_SER_STOPBITS => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data };
	    cb.stopbits(data);
	}
	raw::GENSIO_EVENT_SER_FLOWCONTROL => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data };
	    cb.flowcontrol(data);
	}
	raw::GENSIO_EVENT_SER_IFLOWCONTROL => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data };
	    cb.iflowcontrol(data);
	}
	raw::GENSIO_EVENT_SER_SBREAK => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data };
	    cb.sbreak(data);
	}
	raw::GENSIO_EVENT_SER_DTR => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data };
	    cb.dtr(data);
	}
	raw::GENSIO_EVENT_SER_RTS => {
	    let data = buf as *const ffi::c_uint;
	    let data = unsafe { *data };
	    cb.rts(data);
	}
	_ => err = Error::NotSup
    }
    err as ffi::c_int
}

extern "C" fn evhndl(io: *const raw::gensio, user_data: *const ffi::c_void,
		     event: ffi::c_int, ierr: ffi::c_int,
		     buf: *const ffi::c_void, buflen: *mut GensioDS,
		     auxdata: *const *const ffi::c_char) -> ffi::c_int
{
    let r = panic::catch_unwind(|| {
	i_evhndl(io, user_data, event, ierr, buf, buflen, auxdata)
    });
    match r {
	Ok(v) => v,
	Err(_) => Error::OsErr as ffi::c_int
    }
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
    /// Allocate a new gensio based upon the given string.  We pass in an
    /// Arc holding the reference to the event handler.  This function
    /// clones it so it can make sure the data stays around until the
    /// gensio is closed.  See str_to_gensio() for details.
    pub fn new(s: &str, o: &OsFuncs, cb: Weak<dyn Event>)
	       -> Result<Self, Error>
    {
        let g: *const raw::gensio = std::ptr::null();
        let s = match ffi::CString::new(s) {
	    Ok(s) => s,
	    Err(_) => return Err(Error::Inval)
        };
        // Create a temporary data item so str_to_gensio can report parmlogs.
        let gd = Box::new(GensioData::new(&o.clone(), cb.clone(),
                                          GensioState::Closed)?);
        let gd = Box::into_raw(gd);

        // There is nothing in here that can panic or otherwise unwind, so
        // the drop of "gd" later is safe.
        let err = unsafe {
	    raw::str_to_gensio(s.as_ptr(), o.raw(), evhndl,
			       gd as *mut ffi::c_void, &g)
        };
        match err {
	    0 => Ok(Gensio { g, d: gd }),
	    _ => {
	        unsafe { drop(Box::from_raw(gd)) }; // Free our original box
	        Err(val_to_error(err))
	    }
        }
    }

    /// Open the gensio.  The cb will be called when the operation
    /// completes.  Note that the Arc holding the callback is done so
    /// the callback data can be kept around until the callback is
    /// complete.
    ///
    /// Note that the gensio is not open until the callback is called.
    pub fn open(&self, cb: Weak<dyn OpDoneErr>) -> Result<(), Error> {
	let d = Box::new(OpDoneErrData { cb });
	let mut state = unsafe { (*self.d).state.lock().unwrap() };
	let d = Box::into_raw(d);
	// Everything from here to the end of the function shouldn't
	// panic, so there is no need to worry about unwinding not
	// recovering the Box.
	let err = unsafe {
	    raw::gensio_open(self.g, op_done_err, d as *mut ffi::c_void)
	};
	match err {
	    0 => {
		*state = GensioState::Open;
		Ok(())
	    }
	    _ => {
		unsafe { drop(Box::from_raw(d)); } // Free the data
		Err(val_to_error(err))
	    }
	}
    }

    /// Set a new event handler for the gensio.
    pub fn set_handler(&self, cb: Weak<dyn Event>) {
	unsafe { (*self.d).cb = cb; }
    }

    // Open the gensio synchronously.  Wait until the open completes
    // before returning.
    pub fn open_s(&self) -> Result<(), Error> {
	let err = unsafe { raw::gensio_open_s(self.g) };
	match err {
	    0 => Ok(()),
	    _ => Err(val_to_error(err))
	}
    }

    /// Close the gensio.  The cb will be called when the operation
    /// completes.  Note that the Arc holding the callback is done so
    /// the callback data can be kept around until the callback is
    /// complete.  You can pass in None to the callback if you don't
    /// care.
    ///
    /// Note that the gensio is not closed until the callback is called.
    pub fn close(&self, cb: Option<Weak<dyn OpDone>>) -> Result<(), Error> {
	let d = Box::new(CloseDoneData { cb, d: self.d });
	let mut state = unsafe { (*self.d).state.lock().unwrap() };
	let d = Box::into_raw(d);
	// Everything from here to the end of the function shouldn't
	// panic, so there is no need to worry about unwinding not
	// recovering the Box.
	let err = unsafe {
	    raw::gensio_close(self.g, close_done, d as *mut ffi::c_void)
	};
	match err {
	    0 => {
		*state = GensioState::InClose;
		Ok(())
	    }
	    _ => {
		unsafe { drop(Box::from_raw(d)); } // Free the data
		Err(val_to_error(err))
	    }
	}
    }

    // Close the gensio synchronously.  Wait until the close completes
    // before returning.
    pub fn close_s(&self) -> Result<(), Error> {
	let err = unsafe {
	    raw::gensio_close_s(self.g)
	};
	match err {
	    0 => {
		let mut state = unsafe { (*self.d).state.lock().unwrap() };
		*state = GensioState::Closed;
		Ok(())
	    }
	    _ => Err(val_to_error(err))
	}
    }

    /// Write some data to the gensio.  On success, the number of
    /// bytes written is returned.  On failure an error code is
    /// returned.
    pub fn write(&self, data: &[u8], auxdata: Option<&[&str]>)
		 -> Result<u64, Error> {
	let mut count: GensioDS = 0;
	let a1 = match auxdata {
	    None => None,
	    Some(v) => Some(strslice_to_auxvec(v)?)
	};
	let a2: *mut *mut ffi::c_char = match a1 {
	    None => std::ptr::null_mut(),
	    Some(ref v) => v.as_ptr()
	};

	let err = unsafe {
	    raw::gensio_write(self.g, &mut count,
			      data.as_ptr() as *const ffi::c_void,
			      data.len() as GensioDS,
			      a2 as *const *const ffi::c_char)
	};
	match err {
	    0 => Ok(count),
	    _ => Err(val_to_error(err))
	}
    }

    /// Write some data to the gensio.  On success, the number of
    /// bytes written is returned.  On failure an error code is
    /// returned.
    pub fn write_s(&self, data: &[u8], timeout: Option<&Duration>)
		   -> Result<u64, Error> {
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
	    _ => Err(val_to_error(err))
	}
    }

    /// Write some data to the gensio.  Allow signals to interrupt.
    /// On success, the number of bytes written is returned.  On
    /// failure an error code is returned.
    pub fn write_s_intr(&self, data: &[u8], timeout: Option<&Duration>)
			-> Result<u64, Error> {
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
	    _ => Err(val_to_error(err))
	}
    }

    /// Read some data from the gensio.  On success, the number of
    /// bytes read is returned.  On failure an error code is
    /// returned.  The data vector is filled in with data.
    pub fn read_s(&self, data: &mut Vec<u8>, timeout: Option<&Duration>)
		  -> Result<u64, Error> {
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
	    _ => Err(val_to_error(err))
	}
    }

    /// Like read_s, but if a signal comes in this will return an
    /// interrupted error.
    pub fn read_s_intr(&self, data: &mut Vec<u8>, timeout: Option<&Duration>)
		  -> Result<u64, Error> {
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
	    _ => Err(val_to_error(err))
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
    pub fn control(&self, depth: i32, dir: ControlDir, option: ControlOp,
		   data: &mut Vec<u8>) -> Result<usize, Error> {
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
	    err = raw::gensio_control(self.g, depth, control_dir_to_cbool(dir),
				      control_op_to_val(option),
				      data.as_mut_ptr() as *mut ffi::c_void,
				      &mut len);
	}
	if err == 0 {
	    if len < data.capacity() as GensioDS {
		unsafe {data.set_len(len as usize); }
	    }
	    Ok(len as usize)
	} else {
	    Err(val_to_error(err))
	}
    }

    /// Call gensio_control() and return a vector holding the result.
    pub fn control_resize(&self, depth: i32, dir: ControlDir, option: ControlOp,
			  data: &[u8]) -> Result<Vec<u8>, Error> {
	let mut len: usize;
	let mut data2 = data.to_vec();
	len = self.control(depth, dir, option, &mut data2)?;
	if len <= data2.capacity() {
	    return Ok(data2);
	}
	let mut data2 = data.to_vec();
	data2.reserve(len + 1); // Add 1 for C string terminator
	len = self.control(depth, dir, option, &mut data2)?;
	if len >= data2.capacity() {
	    Err(Error::TooBig)
	} else {
	    Ok(data2)
	}
    }

    /// Call gensio_control(), passing in the given string.  The result
    /// string is returned in Ok().
    pub fn control_str(&self, depth: i32, dir: ControlDir, option: ControlOp,
		       val: &str)
		       -> Result<String, Error> {
	let valv = to_term_str_bytes(val);
	let newv = self.control_resize(depth, dir, option, &valv)?;
	Ok(String::from_utf8_lossy(&newv).to_string())
    }

    /// Call gensio_acontrol with the given options.
    pub fn acontrol(&self, depth: i32, dir: ControlDir, option: AControlOp,
		    data: &[u8], done: Option<Weak<dyn ControlDone>>,
		    timeout: Option<&Duration>)
		    -> Result<(), Error> {
	let mut t = osfuncs::raw::gensio_time { secs: 0, nsecs: 0 };
	let tptr: *const osfuncs::raw::gensio_time
	    = duration_to_gensio_time(&mut t, timeout);
	let d = match done {
	    Some(cb) => Box::into_raw(Box::new(ControlDoneData { cb })),
	    None => std::ptr::null_mut()
	};
	// Everything from here to the end of the function shouldn't
	// panic, so there is no need to worry about unwinding not
	// recovering the Box.
	let err = unsafe {
	    raw::gensio_acontrol(self.g, depth, control_dir_to_cbool(dir),
				 acontrol_op_to_val(option),
				 data.as_ptr() as *const ffi::c_void,
				 data.len() as GensioDS,
				 control_done, d as *mut ffi::c_void, tptr)
	};
	match err {
	    0 => Ok(()),
	    _ => {
		if !d.is_null() {
		    unsafe { drop(Box::from_raw(d)); } // Free the data
		}
		Err(val_to_error(err))
	    }
	}
    }

    /// Like acontrol, but taks a string and converts it to nil
    /// terminated bytes for the caller.
    pub fn acontrol_str(&self, depth: i32, dir: ControlDir, option: AControlOp,
			data: &str, done: Option<Weak<dyn ControlDone>>,
			timeout: Option<&Duration>)
			-> Result<(), Error> {
	
	let datav = to_term_str_bytes(data);
	self.acontrol(depth, dir, option, datav.as_slice(), done, timeout)
    }

    /// Call gensio_acontrol_s() with the given options.  As much data as
    /// can be held is stored in data upon return and the required
    /// size to return all data is returned in Ok().
    pub fn acontrol_s(&self, depth: i32, dir: ControlDir, option: AControlOp,
		      data: &mut Vec<u8>,
		      timeout: Option<&Duration>) -> Result<usize, Error> {
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
	    err = raw::gensio_acontrol_s(self.g, depth,
					 control_dir_to_cbool(dir),
					 acontrol_op_to_val(option),
					 data.as_mut_ptr() as *mut ffi::c_void,
					 &mut len, tptr);
	}
	if err == 0 {
	    if len < data.capacity() as GensioDS {
		unsafe {data.set_len(len as usize); }
	    }
	    Ok(len as usize)
	} else {
	    Err(val_to_error(err))
	}
    }

    /// Like acontrol_s, but resize return the result in a new vector.
    pub fn acontrol_resize_s(&self, depth: i32, dir: ControlDir,
			     option: AControlOp, data: &[u8],
			     timeout: Option<&Duration>)
			     -> Result<Vec<u8>, Error> {
	let mut len: usize;
	let mut data2 = data.to_vec();
	len = self.acontrol_s(depth, dir, option, &mut data2, timeout)?;
	if len <= data2.capacity() {
	    return Ok(data2);
	}
	let mut data2 = data.to_vec();
	data2.reserve(len + 1); // Add 1 for C string terminator
	len = self.acontrol_s(depth, dir, option, &mut data2, timeout)?;
	if len >= data2.capacity() {
	    Err(Error::TooBig)
	} else {
	    Ok(data2)
	}
    }

    /// Like acontrol_resize_s, but takes and returns a string that is
    /// converted to/from bytes.
    pub fn acontrol_str_s(&self, depth: i32, dir: ControlDir,
			  option: AControlOp,
			  val: &str, timeout: Option<&Duration>)
			  -> Result<String, Error> {
	let valv = to_term_str_bytes(val);
	let newv = self.acontrol_resize_s(depth, dir, option, &valv, timeout)?;
	Ok(String::from_utf8_lossy(&newv).to_string())
    }

    /// Like acontrol_s() but this version is interruptible on Unix
    /// like systems.
    pub fn acontrol_s_intr(&self, depth: i32, dir: ControlDir,
			   option: AControlOp, data: &mut Vec<u8>,
			   timeout: Option<&Duration>) -> Result<usize, Error> {
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
	    err = raw::gensio_acontrol_s_intr(self.g, depth,
					 control_dir_to_cbool(dir),
					 acontrol_op_to_val(option),
					 data.as_mut_ptr() as *mut ffi::c_void,
					 &mut len, tptr);
	}
	if err == 0 {
	    if len < data.capacity() as GensioDS {
		unsafe {data.set_len(len as usize); }
	    }
	    Ok(len as usize)
	} else {
	    Err(val_to_error(err))
	}
    }

    /// Like acontrol_intr_s, but resize return the result in a new vector.
    pub fn acontrol_resize_s_intr(&self, depth: i32, dir: ControlDir,
				  option: AControlOp, data: &[u8],
				  timeout: Option<&Duration>)
				  -> Result<Vec<u8>, Error> {
	let mut len: usize;
	let mut data2 = data.to_vec();
	len = self.acontrol_s_intr(depth, dir, option, &mut data2, timeout)?;
	if len <= data2.capacity() {
	    return Ok(data2);
	}
	let mut data2 = data.to_vec();
	data2.reserve(len + 1); // Add 1 for C string terminator
	len = self.acontrol_s_intr(depth, dir, option, &mut data2, timeout)?;
	if len >= data2.capacity() {
	    Err(Error::TooBig)
	} else {
	    Ok(data2)
	}
    }

    /// Like acontrol_resize_s_intr, but takes and returns a string that is
    /// converted to/from bytes.
    pub fn acontrol_str_s_intr(&self, depth: i32, dir: ControlDir,
			       option: AControlOp,
			       val: &str, timeout: Option<&Duration>)
			       -> Result<String, Error> {
	let valv = to_term_str_bytes(val);
	let newv = self.acontrol_resize_s_intr(depth, dir, option, &valv,
					       timeout)?;
	Ok(String::from_utf8_lossy(&newv).to_string())
    }

    /// Return a string for the gensio type at the given depth.
    pub fn get_type(&self, depth: u32) -> String {
	unsafe {
	    let cs = raw::gensio_get_type(self.g, depth as ffi::c_uint);
	    let cs = ffi::CStr::from_ptr(cs);
	    cs.to_str().expect("Invalid string").to_string()
	}
    }

    /// Return if the gensio is client side or server side.
    pub fn is_client(&self) -> bool {
	unsafe {
	    raw::gensio_is_client(self.g) != 0
	}
    }

    /// Return if the gensio transfers data reliable (no loss and flow
    /// control).
    pub fn is_reliable(&self) -> bool {
	unsafe {
	    raw::gensio_is_reliable(self.g) != 0
	}
    }

    /// Return if the gensio is packet oriented.  See gensio_is_packet.3.
    pub fn is_packet(&self) -> bool {
	unsafe {
	    raw::gensio_is_packet(self.g) != 0
	}
    }

    /// Returns if the connection is authenticated.
    pub fn is_authenticated(&self) -> bool {
	unsafe {
	    raw::gensio_is_authenticated(self.g) != 0
	}
    }

    /// Returns if the connection is encrypted.
    pub fn is_encrypted(&self) -> bool {
	unsafe {
	    raw::gensio_is_encrypted(self.g) != 0
	}
    }

    /// Returns if the connection is message oriented.  See
    /// gensio_is_message.3.
    pub fn is_message(&self) -> bool {
	unsafe {
	    raw::gensio_is_message(self.g) != 0
	}
    }

    /// Returns if the gensio is a mux.
    pub fn is_mux(&self) -> bool {
	unsafe {
	    raw::gensio_is_mux(self.g) != 0
	}
    }

    /// Returns if the gensio acts as a serial port.  Note that this
    /// means that something in the gensio stack is a serial port.
    pub fn is_serial(&self) -> bool {
	unsafe {
	    raw::gensio_is_serial(self.g) != 0
	}
    }

    /// Enable synchronous mode on the gensio, so read_s and write_s
    /// work.  See gensio_set_sync.3.
    pub fn set_sync(&self) -> Result<(), Error> {
	let err = unsafe { raw::gensio_set_sync(self.g) };
	match err {
	    0 => Ok(()),
	    _ => Err(val_to_error(err))
	}
    }

    /// Disable synchronous mode on a gensio.
    pub fn clear_sync(&self) -> Result<(), Error> {
	let err = unsafe { raw::gensio_clear_sync(self.g) };
	match err {
	    0 => Ok(()),
	    _ => Err(val_to_error(err))
	}
    }
}

impl Drop for Gensio {
    fn drop(&mut self) {
	unsafe {
	    let mut do_wait = false;
	    {
		let mut state = (*self.d).state.lock().unwrap();
		match *state {
		    GensioState::Closed => (),
		    GensioState::WaitClose => (), // Shouldn't happen
		    GensioState::InClose => {
			// Gensio is in the close process, wait for it.
			*state = GensioState::WaitClose;
			do_wait = true;
		    }
		    GensioState::Open => {
			raw::gensio_close_s(self.g);
		    }
		}
	    }
	    if do_wait {
                _ = (*self.d).close_waiter.wait(1, None);
	    }
	    raw::gensio_free(self.g);
	    drop(Box::from_raw(self.d));
	}
    }
}

/// The struct that gets callbacks from a gensio will need to
/// implement this trait.
/// Note: You must keep this object around because it is stored as Weak.
/// If you allow this object to be done, the callbacks will stop working.
pub trait AccepterEvent {
    /// A log was reported dealing with the gensio.
    fn log(&self, _s: String) {}

    /// Called when parameter parsing is incorrect in str_to_gensio().
    fn parmlog(&self, _s: String) {}

    /// A gensio has come in on a connection.
    fn new_connection(&self, g: Gensio) -> Error;

    /// See gensio_event.3, GENSIO_EVENT_AUTH_BEGIN.
    fn auth_begin(&self) -> Error {
	Error::NotSup
    }

    /// See gensio_event.3, GENSIO_EVENT_PRECERT_VERIFY.
    fn precert_verify(&self) -> Error {
	Error::NotSup
    }

    /// See gensio_event.3, GENSIO_EVENT_POSTCERT_VERIFY.
    fn postcert_verify(&self, _err: Error, _errstr: Option<&str>) -> Error {
	Error::NotSup
    }

    /// See gensio_event.3, GENSIO_EVENT_PASSWORD_VERIFY.
    fn password_verify(&self, _passwd: &str) -> Error {
	Error::NotSup
    }

    /// See gensio_event.3, GENSIO_EVENT_PASSWORD_REQUEST_PASSWORD.
    fn request_password(&self, _maxsize: u64) -> (Error, Option<String>) {
	(Error::NotSup, None)
    }

    /// See gensio_event.3, GENSIO_EVENT_VERIFY_2FA.
    fn verify_2fa(&self, _data: &[u8]) -> Error {
	Error::NotSup
    }

    /// See gensio_event.3, GENSIO_EVENT_REQUEST_2FA.
    fn request_2fa(&self) -> (Error, Option<&[u8]>) {
	(Error::NotSup, None)
    }
}

/// See gensio_acc_control.3 for these.
#[derive(Copy, Clone, PartialEq)]
pub enum AccControlOp {
    LAddr = 1,
    LPort = 2,
    TCPDNName = 3,
}

fn acc_control_op_to_val(op: AccControlOp) -> ffi::c_uint {
    op as ffi::c_uint
}

struct AccepterData {
    o: OsFuncs, // Used to keep the os funcs alive.
    cb: Weak<dyn AccepterEvent>,
    state: Mutex<GensioState>,
    close_waiter: Waiter,
}

/// An accepter gensio for receiving connections.
pub struct Accepter {
    a: *const raw::gensio_accepter,

    // Points to the structure that is passed to the callback.  It is from
    // a Box.
    d: *mut AccepterData,
}

fn i_acc_evhndl(_acc: *const raw::gensio_accepter,
		user_data: *const ffi::c_void,
		event: ffi::c_int,
		data: *const ffi::c_void)
		-> ffi::c_int {
    let a = user_data as *mut AccepterData;
    let cb = match unsafe { (*a).cb.upgrade() } {
	None => return Error::NotReady as ffi::c_int,
	Some(cb) => cb
    };

    let mut err: Error = Error::NoErr;
    match event {
	raw::GENSIO_ACC_EVENT_LOG => {
	    let s = unsafe { raw::gensio_loginfo_to_str(data) };
	    if !s.is_null() {
		let cs = unsafe { ffi::CStr::from_ptr(s) };
		let logstr = cs.to_str().expect("Invalid string").to_string();
		cb.log(logstr);
		unsafe { raw::gensio_free_loginfo_str(s); }
	    }
	}
	raw::GENSIO_ACC_EVENT_PARMLOG => {
	    let s = unsafe { raw::gensio_parmlog_to_str(data) };
	    if !s.is_null() {
		let cs = unsafe { ffi::CStr::from_ptr(s) };
		let logstr = cs.to_str().expect("Invalid string").to_string();
		cb.parmlog(logstr);
		unsafe { raw::gensio_free_loginfo_str(s); }
	    }
	}
	raw::GENSIO_ACC_EVENT_NEW_CONNECTION => {
	    let g = data as *const raw::gensio;
	    let tmpcb = Arc::new(DummyEvHndl{ });
	    // tmpcb will go away in this function, but that's ok, as
	    // it should never get called, and if it does the code
	    // will return an error.
	    let o = unsafe { (*a).o.clone() };
	    let new_g = match new_gensio(&o, g, Arc::downgrade(&tmpcb) as _,
					 GensioState::Open, None) {
		Ok(g) => g,
		Err(e) => return e as ffi::c_int
	    };
	    unsafe {
		raw::gensio_set_callback(new_g.g, evhndl,
                                         new_g.d as *mut ffi::c_void);
	    }
	    err = cb.new_connection(new_g);
	}
	raw::GENSIO_ACC_EVENT_AUTH_BEGIN => {
	    err = cb.auth_begin();
	}
	raw::GENSIO_ACC_EVENT_PRECERT_VERIFY => {
	    err = cb.precert_verify();
	}
	raw::GENSIO_ACC_EVENT_POSTCERT_VERIFY => {
	    let vd = data as *const raw::gensio_acc_postcert_verify_data;
	    let errstr = {
		if unsafe {(*vd).errstr }.is_null() {
		    None
		} else {
		    let cs = unsafe { ffi::CStr::from_ptr((*vd).errstr) };
		    Some(cs.to_str().expect("Invalid string"))
		}
	    };
	    err = cb.postcert_verify(unsafe { val_to_error((*vd).err) },
				     errstr);
	}
	raw::GENSIO_ACC_EVENT_PASSWORD_VERIFY => {
	    let vd = data as *const raw::gensio_acc_password_verify_data;
	    let cs = unsafe {
		ffi::CStr::from_ptr((*vd).password as *const ffi::c_char)
	    };
	    let s = cs.to_str().expect("Invalid string");
	    err = cb.password_verify(s);
	}
	raw::GENSIO_ACC_EVENT_REQUEST_PASSWORD => {
	    let vd = data as *mut raw::gensio_acc_password_verify_data;
	    let s;
	    let maxlen = unsafe { (*vd).password_len } as usize;
	    (err, s) = cb.request_password(maxlen as u64);
	    if err == Error::NoErr {
		match s {
		    None => return err as ffi::c_int,
		    Some(s) => {
			let len = s.len();
			if len > maxlen {
			    return Error::TooBig as ffi::c_int;
			}
			let cs = match ffi::CString::new(s) {
			    Ok(v) => v,
			    Err(_) => return Error::Inval as ffi::c_int
			};
			let src = cs.to_bytes();
			let dst = unsafe { (*vd).password as *mut u8 };
			let dst = unsafe {
			    std::slice::from_raw_parts_mut(dst, maxlen)
			};
			dst[..len].copy_from_slice(&src[..len]);
			unsafe { (*vd).password_len = len as GensioDS; }
		    }
		}
	    }
	}
	raw::GENSIO_ACC_EVENT_2FA_VERIFY => {
	    let vd = data as *mut raw::gensio_acc_password_verify_data;
	    let data = unsafe {
		std::slice::from_raw_parts((*vd).password as *const u8,
                                           (*vd).password_len as usize)
	    };
	    err = cb.verify_2fa(data);
	}
	raw::GENSIO_ACC_EVENT_REQUEST_2FA => {
	    let vd = data as *mut raw::gensio_acc_password_verify_data;
	    let src;
	    (err, src) = cb.request_2fa();
	    if err != Error::NoErr {
		return err as ffi::c_int;
	    }
	    let src = match src {
		None => return Error::Inval as ffi::c_int,
		Some(v) => v
	    };
	    let len = src.len();
	    if len > unsafe { (*vd).password_len } as usize {
		return Error::TooBig as ffi::c_int;
	    }
	    let dst = unsafe { (*vd).password as *mut u8 };
	    let dst = unsafe {std::slice::from_raw_parts_mut(dst, len) };
	    dst[..len].copy_from_slice(&src[..len]);
	    unsafe {(*vd).password_len = len as GensioDS; }
	}
	_ => { err = Error::NotSup; }
    }
    err as ffi::c_int
}

extern "C" fn acc_evhndl(acc: *const raw::gensio_accepter,
			 user_data: *const ffi::c_void,
			 event: ffi::c_int,
			 data: *const ffi::c_void)
			 -> ffi::c_int {
    let r = panic::catch_unwind(|| {
	i_acc_evhndl(acc, user_data, event, data)
    });
    match r {
	Ok(v) => v,
	Err(_) => Error::OsErr as ffi::c_int
    }
}

/// Shutdown callbacks will need to implement this trait.
/// Note: You must keep this object around because it is stored as Weak.
/// If you allow this object to be done, the callbacks will stop working.
pub trait AccepterShutdownDone {
    /// Report that the operation (close) has completed.
    fn done(&self);
}

struct AccepterShutdownDoneData {
    cb: Option<Weak<dyn AccepterShutdownDone>>,
    d: *mut AccepterData,
}

fn i_acc_shutdown_done(_io: *const raw::gensio_accepter,
	       user_data: *mut ffi::c_void) {
    let d = user_data as *mut AccepterShutdownDoneData;
    let d = unsafe { Box::from_raw(d) }; // Use from_raw so it will be freed
    match d.cb {
	None => (),
	Some(cb) => {
	    let cb = cb.upgrade();
	    match cb {
		None => (),
		Some(cb) => cb.done()
	    }
	}
    }
    let mut state = unsafe {(*d.d).state.lock().unwrap() };
    match *state {
	GensioState::WaitClose => {
	    // The Gensio is being dropped and waiting for us to complete.
	    unsafe {
                _ = (*d.d).close_waiter.wake()
	    }
	}
	_ => *state = GensioState::Closed,
    }
}

extern "C" fn acc_shutdown_done(io: *const raw::gensio_accepter,
			  user_data: *mut ffi::c_void) {
    let _r = panic::catch_unwind(|| {
	i_acc_shutdown_done(io, user_data)
    });
}

impl Accepter {
    /// Allocate a new accepter gensio.  We pass in an Arc holding the
    /// reference to the event handler.  This function clones it so it can
    /// make sure the data stays around until the gensio is closed.  See
    /// str_to_gensio_accepter.3
    pub fn new(s: &str, o: &OsFuncs, cb: Weak<dyn AccepterEvent>)
	       -> Result<Self, Error> {
        let a: *const raw::gensio_accepter = std::ptr::null();
        let s = match ffi::CString::new(s) {
	    Ok(s) => s,
	    Err(_) => return Err(Error::Inval)
        };
        // Create the callback data.
        let dt = Box::new(AccepterData { o: o.clone(),
				         cb,
				         state: Mutex::new(GensioState::Closed),
				         close_waiter: Waiter::new(o)?, });
        let dt = Box::into_raw(dt);
        // Everything from here to the end of the function shouldn't
        // panic, so there is no need to worry about unwinding not
        // recovering the Box.
        let err = unsafe {
	    raw::str_to_gensio_accepter(s.as_ptr(), o.raw(), acc_evhndl,
				        dt as *mut ffi::c_void, &a)
        };
        match err {
	    0 => {
	        Ok(Accepter { a, d: dt })
	    }
	    _ => {
                let _dt = unsafe {Box::from_raw(dt) }; // Free our original box
                Err(val_to_error(err))
            }
        }
    }

    /// Set a new event handler for the accepter.
    pub fn set_handler(&self, cb: Weak<dyn AccepterEvent>) {
	unsafe { (*self.d).cb = cb; }
    }

    /// Start the accepter running.
    pub fn startup(&self) -> Result<(), Error> {
	let mut state = unsafe { (*self.d).state.lock().unwrap() };
	let err = unsafe {
	    raw::gensio_acc_startup(self.a)
	};
	match err {
	    0 => {
		*state = GensioState::Open;
		Ok(())
	    }
	    _ => Err(val_to_error(err))
	}
    }

    /// Stop the accepter.  Note that the accepter is not shut down
    /// until the callback is called.  You can pass in None to the
    /// callback if you don't care.
    pub fn shutdown(&self, cb: Option<Weak<dyn AccepterShutdownDone>>)
		    -> Result<(), Error> {
	let d = Box::new(
	    AccepterShutdownDoneData { cb, d: self.d });
	let mut state = unsafe { (*self.d).state.lock().unwrap() };
	let d = Box::into_raw(d);
	// Everything from here to the end of the function shouldn't
	// panic, so there is no need to worry about unwinding not
	// recovering the Box.
	let err = unsafe {
	    raw::gensio_acc_shutdown(self.a, acc_shutdown_done,
				     d as *mut ffi::c_void)
	};
	match err {
	    0 => {
		*state = GensioState::InClose;
		Ok(())
	    }
	    _ => {
		unsafe { drop(Box::from_raw(d)); } // Free the data
		Err(val_to_error(err))
	    }
	}
    }

    /// Stop the accepter.  Wait until the shutdown completes before
    /// returning.
    pub fn shutdown_s(&self)
		      -> Result<(), Error> {
	let err = unsafe {
	    raw::gensio_acc_shutdown_s(self.a)
	};
	match err {
	    0 => {
		let mut state = unsafe { (*self.d).state.lock().unwrap() };
		*state = GensioState::Closed;
		Ok(())
	    }
	    _ => Err(val_to_error(err))
	}
    }	

    /// Call gensio_acc_control() with the given options.  As much
    /// data as can be held is stored in data upon return and the
    /// required size to return all data is returned in Ok().
    pub fn control(&self, depth: i32, dir: ControlDir, option: AccControlOp,
		   data: &mut Vec<u8>) -> Result<usize, Error> {
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
	    err = raw::gensio_acc_control(self.a, depth,
					  control_dir_to_cbool(dir),
					  acc_control_op_to_val(option),
					  data.as_mut_ptr() as *mut ffi::c_void,
					  &mut len);
	}
	if err == 0 {
	    if len < data.capacity() as GensioDS {
		unsafe {data.set_len(len as usize); }
	    }
	    Ok(len as usize)
	} else {
	    Err(val_to_error(err))
	}
    }

    /// Call gensio_acc_control() and return a vector holding the result.
    pub fn control_resize(&self, depth: i32, dir: ControlDir,
			  option: AccControlOp,
			  data: &[u8]) -> Result<Vec<u8>, Error> {
	let mut len: usize;
	let mut data2 = data.to_vec();
	len = self.control(depth, dir, option, &mut data2)?;
	if len <= data2.capacity() {
	    return Ok(data2);
	}
	let mut data2 = data.to_vec();
	data2.reserve(len + 1);
	len = self.control(depth, dir, option, &mut data2)?;
	if len >= data2.capacity() {
	    Err(Error::TooBig)
	} else {
	    Ok(data2)
	}
    }

    /// Call gensio_acc_control(), passing in the given string.  The result
    /// string is returned in Ok().
    pub fn control_str(&self, depth: i32, dir: ControlDir, option: AccControlOp,
		       val: &str)
		       -> Result<String, Error> {
	let valv = val.as_bytes().to_vec();
	let rv = self.control_resize(depth, dir, option, &valv)?;
	Ok(String::from_utf8_lossy(&rv).to_string())
    }

    /// Will gensios the accepter creates be reliable?
    pub fn is_reliable(&self) -> bool {
	unsafe {
	    raw::gensio_acc_is_reliable(self.a) != 0
	}
    }

    /// Will gensios the accepter creates be packet oriented?
    pub fn is_packet(&self) -> bool {
	unsafe {
	    raw::gensio_acc_is_packet(self.a) != 0
	}
    }

    /// Will gensios the accepter creates be message oriented?
    pub fn is_message(&self) -> bool {
	unsafe {
	    raw::gensio_acc_is_message(self.a) != 0
	}
    }

    /// Will gensios the accepter creates be muxes?
    pub fn is_mux(&self) -> bool {
	unsafe {
	    raw::gensio_acc_is_mux(self.a) != 0
	}
    }

    /// Will gensios the accepter creates be serial?
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
	    let mut do_wait = false;
	    {
		let mut state = (*self.d).state.lock().unwrap();
		match *state {
		    GensioState::Closed => (),
		    GensioState::WaitClose => (), // Shouldn't happen
		    GensioState::InClose => {
			// Gensio is in the close process, wait for it.
			*state = GensioState::WaitClose;
			do_wait = true;
		    }
		    GensioState::Open => {
			raw::gensio_acc_shutdown_s(self.a);
		    }
		}
	    }
	    if do_wait {
                _ = (*self.d).close_waiter.wait(1, None);
	    }
	    raw::gensio_acc_shutdown_s(self.a);
	    raw::gensio_acc_free(self.a);
	    drop(Box::from_raw(self.d));
	}
    }
}

/// A helper function to take a string and convert it to a nil terminated
/// vector.  This is useful for passing strings to control() and friends.
pub fn to_term_str_bytes(s: &str) -> Vec<u8> {
    let mut v = s.as_bytes().to_vec();
    v.push(0);
    v
}

/// Convert an integer parity value to a string.
pub fn parity_to_str(val: u32) -> String {
    let cs = unsafe { raw::gensio_parity_to_str(val as ffi::c_uint) };
    let cs = unsafe { ffi::CStr::from_ptr(cs) };
    String::from_utf8_lossy(cs.to_bytes()).to_string()
}

/// Convert an integer parity string to a numeric value.
pub fn str_to_parity(sval: &str) -> Result<u32, Error> {
    let cs = match ffi::CString::new(sval) {
	Ok(s) => s,
	Err(_) => return Err(Error::Inval)
    };
    let val = unsafe { raw::gensio_str_to_parity(cs.as_ptr()) };
    if val < 0 {
	Err(Error::Inval)
    } else {
	Ok(val as u32)
    }
}

/// Convert an integer flowcontrol value to a string.
pub fn flowcontrol_to_str(val: u32) -> String {
    let cs = unsafe { raw::gensio_flowcontrol_to_str(val as ffi::c_uint) };
    let cs = unsafe { ffi::CStr::from_ptr(cs) };
    String::from_utf8_lossy(cs.to_bytes()).to_string()
}

/// Convert an integer flowcontrol string to a numeric value.
pub fn str_to_flowcontrol(sval: &str) -> Result<u32, Error> {
    let cs = match ffi::CString::new(sval) {
	Ok(s) => s,
	Err(_) => return Err(Error::Inval)
    };
    let val = unsafe { raw::gensio_str_to_flowcontrol(cs.as_ptr()) };
    if val < 0 {
	Err(Error::Inval)
    } else {
	Ok(val as u32)
    }
}

/// Convert an integer onoff value to a string.
pub fn onoff_to_str(val: u32) -> String {
    let cs = unsafe { raw::gensio_onoff_to_str(val as ffi::c_uint) };
    let cs = unsafe { ffi::CStr::from_ptr(cs) };
    String::from_utf8_lossy(cs.to_bytes()).to_string()
}

/// Convert an integer onoff string to a numeric value.
pub fn str_to_onoff(sval: &str) -> Result<u32, Error> {
    let cs = match ffi::CString::new(sval) {
	Ok(s) => s,
	Err(_) => return Err(Error::Inval)
    };
    let val = unsafe { raw::gensio_str_to_onoff(cs.as_ptr()) };
    if val < 0 {
	Err(Error::Inval)
    } else {
	Ok(val as u32)
    }
}

pub fn err_to_str(err: Error) -> String {
    let cs = unsafe { raw::gensio_err_to_str(err as ffi::c_int) };
    let cs = unsafe { ffi::CStr::from_ptr(cs).to_bytes() };
    String::from_utf8_lossy(cs).to_string()
}

#[cfg(test)]
mod tests {
    use std::time::Duration;
    use std::sync::Mutex;
    use super::*;
    use log::{ error };

    static LOGGER_INITIALIZED: Mutex<bool> = Mutex::new(false);

    fn init_logger() {
	let mut b = LOGGER_INITIALIZED.lock().unwrap();
	if !*b {
	    *b = true;
	    env_logger::init();
	}
    }

    struct EvStruct {
	w: Waiter
    }

    impl Event for EvStruct {
	fn err(&self, err: Error) -> Error {
	    panic!("Got error: {}", err);
	}

	fn read(&self, buf: &[u8], _auxdata: Option<&[&str]>)
		-> (Error, usize) {
	    assert_eq!(buf.len(), 7);
	    let s = String::from_utf8_lossy(buf);
	    assert_eq!(s, "teststr");
	    self.w.wake().expect("Wake open done failed");
	    (Error::NoErr, buf.len())
	}
    }

    impl OpDoneErr for EvStruct {
	fn done_err(&self, err: Error) {
	    panic!("Got done error: {}", err);
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
	init_logger();
	let logh = Arc::new(LogHandler);
	let o = OsFuncs::new(Arc::downgrade(&logh) as _)
	    .expect("Couldn't allocate os funcs");
	o.thread_setup().expect("Couldn't setup thread");

	let w = Waiter::new(&o).expect("Couldn't allocate waiter");
	let e = Arc::new(EvStruct { w: w });
	let ew = Arc::downgrade(&e);
	let g = Gensio::new("echo", &o, ew.clone())
            .expect("Couldn't alloc gensio");
	g.open(ew.clone()).expect("Couldn't open genio");
	e.w.wait(1, Some(&Duration::new(1, 0))).expect("Wait failed");
	g.read_enable(true);
	let v1 = vec!["t1", "t2"];
	let count = g.write(&b"teststr".to_vec()[..], Some(&v1))
			    .expect("Write failed");
	assert_eq!(count, 7);
	e.w.wait(1, Some(&Duration::new(1, 0))).expect("Wait failed");
	g.close(Some(ew)).expect("Couldn't close gensio");
	e.w.wait(1, Some(&Duration::new(1, 0))).expect("Wait failed");
    }

    struct AccMutData {
	logstr: Option<String>,
	ag: Option<Gensio>,
    }

    struct AccEvent {
	w: Waiter,
	d: Mutex<AccMutData>,
    }

    impl AccepterEvent for AccEvent {
	fn parmlog(&self, s: String) {
	    let mut d = self.d.lock().unwrap();
	    let chks = match &d.logstr {
		None => {
		    error!("{}", &format!("Unexpected log: {s}\n").to_string());
		    assert!(false);
		    return;
		}
		Some(s) => s
	    };
	    assert_eq!(&s, chks);
	    d.logstr = None;
	    self.w.wake().expect("Wake failed");
	}

	fn new_connection(&self, g: Gensio) -> Error {
	    let mut d = self.d.lock().unwrap();
	    d.ag = Some(g);
	    self.w.wake().expect("Wake failed");
	    Error::NoErr
	}
    }

    impl AccepterShutdownDone for AccEvent {
	fn done(&self) {
	    self.w.wake().expect("Wake close done failed");
	}
    }

    struct GenMutData {
	logstr: Option<String>,
	experr: Error,
    }

    struct GenEvent {
	w: Waiter,
	_g: Option<Arc<Gensio>>,
	d: Mutex<GenMutData>,
    }

    impl Event for GenEvent {
	fn parmlog(&self, s: String) {
	    let mut d = self.d.lock().unwrap();
	    let chks = match &d.logstr {
		None => {
		    error!("{}", &format!("Unexpected log: {s}\n").to_string());
		    assert!(false);
		    return;
		}
		Some(s) => s
	    };
	    assert_eq!(&s, chks);
	    d.logstr = None;
	    self.w.wake().expect("Wake failed");
	}

	fn err(&self, err: Error) -> Error {
	    let d = self.d.lock().unwrap();
	    assert_eq!(d.experr, err);
	    self.w.wake().expect("Wake failed");
	    Error::NoErr
	}

	fn read(&self, _buf: &[u8], _auxdata: Option<&[&str]>)
		-> (Error, usize) {
	    (Error::NoErr, 0)
	}
    }

    #[test]
    fn parmerr() {
	init_logger();
	let logh = Arc::new(LogHandler);
	let o = OsFuncs::new(Arc::downgrade(&logh) as _)
	    .expect("Couldn't allocate os funcs");
	o.thread_setup().expect("Couldn't setup thread");

	let w = Waiter::new(&o).expect("Couldn't allocate waiter");
	let d = Mutex::new(AccMutData { logstr: None, ag: None });
	let e = Arc::new(AccEvent { w: w, d });

	{
	    let mut d = e.d.lock().unwrap();
	    d.logstr = Some(
		"accepter base: Unknown gensio type: asdf,127.0.0.1:1234"
		    .to_string());
	}
	let a = Accepter::new("asdf,127.0.0.1:1234", &o,
			     Arc::downgrade(&e) as _);
	match a {
	    Ok(_a) => assert!(false),
	    Err(e) => assert_eq!(e, Error::UnknownNameErr)
	};
    }

    #[test]
    fn acc_conn1() {
	init_logger();
	let logh = Arc::new(LogHandler);
	let o = OsFuncs::new(Arc::downgrade(&logh) as _)
	    .expect("Couldn't allocate os funcs");
	o.thread_setup().expect("Couldn't setup thread");

	let w = Waiter::new(&o).expect("Couldn't allocate waiter");
	let d = Mutex::new(AccMutData { logstr: None, ag: None });
	let e1 = Arc::new(AccEvent { w: w, d });
	let a = Accepter::new("tcp,127.0.0.1,0", &o,
			     Arc::downgrade(&e1) as _)
	    .expect("Couldn't allocate accepter");
	a.startup().expect("Couldn't start accepter");
	let port = match a.control_str(0, ControlDir::Get,
				       AccControlOp::LPort, "") {
	    Ok(s) => s,
	    Err(err) => panic!("Error getting acc laddr {err}")
	};

	let w = Waiter::new(&o).expect("Couldn't allocate waiter");
	let d = Mutex::new(GenMutData { logstr: None, experr: Error::NoErr });
	let e2 = Arc::new(GenEvent { w: w, _g: None, d });
	let g = Gensio::new(&format!("tcp,127.0.0.1,{port}"), &o,
		    Arc::downgrade(&e2) as _)
	    .expect("Couldn't alocate gensio");
	g.open_s().expect("Couldn't open gensio");

	e1.w.wait(1, Some(&Duration::new(1, 0))).expect("Wait failed");
	// Let automatic cleanup happen.
    }

    #[test]
    fn acc_conn2() {
	init_logger();
	let logh = Arc::new(LogHandler);
	let o = OsFuncs::new(Arc::downgrade(&logh) as _)
	    .expect("Couldn't allocate os funcs");
	o.thread_setup().expect("Couldn't setup thread");

	let w = Waiter::new(&o).expect("Couldn't allocate waiter");
	let d = Mutex::new(AccMutData { logstr: None, ag: None });
	let e1 = Arc::new(AccEvent { w: w, d });
	let e1w = Arc::downgrade(&e1);
	let a = Accepter::new("tcp,127.0.0.1,0", &o, e1w.clone())
	    .expect("Couldn't allocate accepter");
	a.startup().expect("Couldn't start accepter");
	let port = match a.control_str(0, ControlDir::Get,
				       AccControlOp::LPort, "") {
	    Ok(s) => s,
	    Err(err) => panic!("Error getting acc laddr {err}")
	};

	let w = Waiter::new(&o).expect("Couldn't allocate waiter");
	let d = Mutex::new(GenMutData { logstr: None, experr: Error::NoErr });
	let e2 = Arc::new(GenEvent { w: w, _g: None, d });
	let g = Gensio::new(&format!("tcp,127.0.0.1,{port}"), &o,
		    Arc::downgrade(&e2) as _).
	    expect("Couldn't alocate gensio");
	g.open_s().expect("Couldn't open gensio");

	e1.w.wait(1, Some(&Duration::new(1, 0))).expect("Wait failed");

	// Assign a handler for the new gensio
	let e3;
	{
	    let w = Waiter::new(&o).expect("Couldn't allocate waiter");
	    let d = Mutex::new(GenMutData { logstr: None,
					    experr: Error::RemClose });
	    e3 = Arc::new(GenEvent { w: w, _g: None, d });
	    let d1 = e1.d.lock().unwrap();
	    match &d1.ag {
		None => assert!(false),
		Some(d2) => d2.set_handler(Arc::downgrade(&e3) as _)
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
	e3.w.wait(1, Some(&Duration::new(1, 0))).expect("Wait failed");

	a.shutdown(Some(e1w)).expect("Shutdown failed");
	e1.w.wait(1, Some(&Duration::new(1, 0))).expect("Wait failed");
    }

    struct TelnetReflectorInstList {
	list: Mutex<Vec<Arc<TelnetReflectorInst>>>,
    }

    struct TelnetReflectorInstData {
	baud: u32,
	datasize: u32,
	stopbits: u32,
	parity: u32,
	flowcontrol: u32,
	iflowcontrol: u32,
	sbreak: u32,
	rts: u32,
	dtr: u32,
	signature: Vec<u8>,
    }
    impl Default for TelnetReflectorInstData {
	fn default() -> TelnetReflectorInstData {
	    TelnetReflectorInstData {
		baud: 9600,
		datasize: 8,
		stopbits: 1,
		parity: GENSIO_SER_PARITY_NONE,
		flowcontrol: GENSIO_SER_FLOWCONTROL_NONE,
		iflowcontrol: GENSIO_SER_FLOWCONTROL_NONE,
		sbreak: GENSIO_SER_OFF,
		dtr: GENSIO_SER_OFF,
		rts: GENSIO_SER_OFF,
		signature: "mysig".as_bytes().to_vec(),
	    }
	}
    }

    struct TelnetReflectorInst {
        g: Gensio,
	list: Arc<TelnetReflectorInstList>,
        d: Mutex<TelnetReflectorInstData>,
    }

    impl TelnetReflectorInst {
	fn shutdown(&self) {
	    let mut list = self.list.list.lock().unwrap();
	    // Removing ourself from the list will drop the reference.
	    list.retain(|x| std::ptr::eq(x.as_ref(), self));
	}
    }

    impl Event for TelnetReflectorInst {
        fn err(&self, err: Error) -> Error {
	    error!("{}", &format!("Error: {err}\n").to_string());
	    self.shutdown();
            Error::NoErr
        }

        fn read(&self, buf: &[u8], _auxdata: Option<&[&str]>)
                -> (Error, usize) {
	    match self.g.write(buf, None) {
		Ok(len) => {
		    if (len as usize) < buf.len() {
			self.g.read_enable(false);
			self.g.write_enable(true);
		    }
		    (Error::NoErr, (len as u64).try_into().unwrap())
		}
		Err(err) => {
		    self.shutdown();
		    (err, 0)
		}
	    }
        }

        fn write_ready(&self) -> Error {
            self.g.read_enable(true);
            self.g.write_enable(false);
            Error::NoErr
        }

        fn baud(&self, baud: u32) {
	    let mut baud = baud;
	    {
		let mut d = self.d.lock().unwrap();
		if baud != 0 {
		    d.baud = baud;
		} else {
		    baud = d.baud;
		}
	    }
	    let baud = baud.to_string();
	    _ = self.g.acontrol_str(CONTROL_DEPTH_FIRST,
				    ControlDir::Set,
				    AControlOp::SerBaud,
				    &baud.to_string(), None, None);
        }

        fn datasize(&self, val: u32) {
	    let mut val = val;
	    {
		let mut d = self.d.lock().unwrap();
		if val != 0 {
		    d.datasize = val;
		} else {
		    val = d.datasize;
		}
	    }
	    _ = self.g.acontrol_str(CONTROL_DEPTH_FIRST,
				    ControlDir::Set,
				    AControlOp::SerDatasize,
				    &val.to_string(), None, None);
        }

        fn stopbits(&self, val: u32) {
	    let mut val = val;
	    {
		let mut d = self.d.lock().unwrap();
		if val != 0 {
		    d.stopbits = val;
		} else {
		    val = d.stopbits;
		}
	    }
	    let val = val.to_string();
	    _ = self.g.acontrol_str(CONTROL_DEPTH_FIRST,
				    ControlDir::Set,
				    AControlOp::SerStopbits,
				    &val.to_string(), None, None);
        }

        fn parity(&self, val: u32) {
	    let mut val = val;
	    {
		let mut d = self.d.lock().unwrap();
		if val != 0 {
		    d.parity = val;
		} else {
		    val = d.parity;
		}
	    }
	    let val = parity_to_str(val);
	    _ = self.g.acontrol_str(CONTROL_DEPTH_FIRST,
				    ControlDir::Set,
				    AControlOp::SerParity,
				    &val.to_string(), None, None);
        }

        fn flowcontrol(&self, val: u32) {
	    let mut val = val;
	    {
		let mut d = self.d.lock().unwrap();
		if val != 0 {
		    d.flowcontrol = val;
		} else {
		    val = d.flowcontrol;
		}
	    }
	    let val = flowcontrol_to_str(val);
	    _ = self.g.acontrol_str(CONTROL_DEPTH_FIRST,
				    ControlDir::Set,
				    AControlOp::SerFlowcontrol,
				    &val.to_string(), None, None);
        }

        fn iflowcontrol(&self, val: u32) {
	    let mut val = val;
	    {
		let mut d = self.d.lock().unwrap();
		if val != 0 {
		    d.iflowcontrol = val;
		} else {
		    val = d.iflowcontrol;
		}
	    }
	    let val = flowcontrol_to_str(val);
	    _ = self.g.acontrol_str(CONTROL_DEPTH_FIRST,
				    ControlDir::Set,
				    AControlOp::SerIFlowcontrol,
				    &val.to_string(), None, None);
        }

        fn sbreak(&self, val: u32) {
	    let mut val = val;
	    {
		let mut d = self.d.lock().unwrap();
		if val != 0 {
		    d.sbreak = val;
		} else {
		    val = d.sbreak;
		}
	    }
	    let val = onoff_to_str(val);
	    _ = self.g.acontrol_str(CONTROL_DEPTH_FIRST,
				    ControlDir::Set,
				    AControlOp::SerSBreak,
				    &val.to_string(), None, None);
        }

        fn dtr(&self, val: u32) {
	    let mut val = val;
	    {
		let mut d = self.d.lock().unwrap();
		if val != 0 {
		    d.dtr = val;
		} else {
		    val = d.dtr;
		}
	    }
	    let val = onoff_to_str(val);
	    _ = self.g.acontrol_str(CONTROL_DEPTH_FIRST,
				    ControlDir::Set,
				    AControlOp::SerDtr,
				    &val.to_string(), None, None);
        }

        fn rts(&self, val: u32) {
	    let mut val = val;
	    {
		let mut d = self.d.lock().unwrap();
		if val != 0 {
		    d.rts = val;
		} else {
		    val = d.rts;
		}
	    }
	    let val = onoff_to_str(val);
	    _ = self.g.acontrol_str(CONTROL_DEPTH_FIRST,
				    ControlDir::Set,
				    AControlOp::SerRts,
				    &val.to_string(), None, None);
        }

        fn signature(&self, _val: &[u8]) {
	    let d = self.d.lock().unwrap();
	    // Signature value cannot be changed.
	    _ = self.g.acontrol(CONTROL_DEPTH_FIRST,
				ControlDir::Set,
				AControlOp::SerSignature,
				d.signature.as_slice(),
				None, None);
        }
    }

    struct TelnetReflector {
        a: Arc<Accepter>,
        port: String,
	list: Arc<TelnetReflectorInstList>,
    }

    impl AccepterEvent for TelnetReflector {
	// No need for parmlog, InitialTelnetReflectorEv handled that.

        fn new_connection(&self, g: Gensio) -> Error {
	    let mut list = self.list.list.lock().unwrap();

	    let d = TelnetReflectorInstData { ..Default::default() };
	    let inst = Arc::new(TelnetReflectorInst { g: g,
						      list: self.list.clone(),
						      d: Mutex::new(d) });
	    inst.g.set_handler(Arc::downgrade(&inst) as _);
	    inst.g.read_enable(true);
	    list.push(inst);
	    Error::NoErr
        }
    }

    // Used as the initial handler when creating a new accepter.  Will be
    // replaced after the accepter is up.
    struct InitialTelnetReflectorEv {
    }

    impl AccepterEvent for InitialTelnetReflectorEv {
        fn parmlog(&self, s: String) {
            error!("{}", &format!("Unexpected parmlog: {s}\n").to_string());
        }

	// Refuse connections until we are ready.
        fn new_connection(&self, _g: Gensio) -> Error {
            Error::NotSup
        }
    }

    fn new_telnet_reflector(o: &osfuncs::OsFuncs)
			    -> Result<Arc<TelnetReflector>, Error> {
	let h = Arc::new(InitialTelnetReflectorEv{});
        let a = Accepter::new("telnet(rfc2217),tcp,localhost,0", o,
			     Arc::downgrade(&h) as _)?;
        a.startup()?;
	let port = a.control_str(CONTROL_DEPTH_FIRST, ControlDir::Get,
				 AccControlOp::LPort, "")?;
	let list = TelnetReflectorInstList {  list: Mutex::new(Vec::new()) };
	let refl = Arc::new(TelnetReflector { a: Arc::new(a), port: port,
					      list: Arc::new(list) });
	refl.a.set_handler(Arc::downgrade(&refl) as _);
	Ok(refl)
    }

    struct SerialEvInst {
	w: Waiter,
	expect_val: Mutex<Option<String>>,
    }

    impl Event for SerialEvInst {
	fn parmlog(&self, _s: String) {
	    panic!("Unexpected parm log");
	}

	fn err(&self, _err: Error) -> Error {
	    panic!("Unexpected err");
	}

	fn read(&self, buf: &[u8], _auxdata: Option<&[&str]>)
		-> (Error, usize) {
	    (Error::NoErr, buf.len())
	}
    }

    impl ControlDone for SerialEvInst {
	fn done_err(&self, _err: Error) {
	    panic!("Unexpected err");
	}

	fn done(&self, buf: &[u8]) {
	    let mut v = self.expect_val.lock().unwrap();
	    match &*v {
		None => panic!("No value"),
		Some(s2) => {
		    let v = buf.to_vec();
		    let s = String::from_utf8_lossy(&v);
		    assert_eq!(*s2, s);
		}
	    };
	    *v = None;
	    self.w.wake().expect("Wake control done failed");
	}
    }

    // Get the current value of "option" synchronously, which should
    // match osval, then set the value to sval and validate that it
    // matches.
    fn test_acontrol(e: &Arc<SerialEvInst>, g: &Gensio,
		     name: &str, osval: &str, sval: &str, option: AControlOp)
    {
	let mut v = Vec::with_capacity(16);
	v.extend(to_term_str_bytes("0").as_slice());
	g.acontrol_s(0, ControlDir::Get, option,
		     &mut v, None)
	    .expect(&format!("Acontrol get {name} failed"));
	assert_eq!(v.as_slice(), osval.as_bytes());

	{
	    let mut v = e.expect_val.lock().unwrap();
	    *v = Some(sval.to_string());
	}
	g.acontrol_str(0, ControlDir::Set, option, sval,
		       Some(Arc::downgrade(&e) as _), None)
	    .expect("Acontrol failed");
	e.w.wait(1, Some(&Duration::new(1, 0))).expect("Wait failed");
    }

    #[test]
    fn serial() {
	init_logger();
	let logh = Arc::new(LogHandler);
	let o = OsFuncs::new(Arc::downgrade(&logh) as _)
	    .expect("Couldn't allocate os funcs");
	o.thread_setup().expect("Couldn't setup thread");
	let w = Waiter::new(&o).expect("Couldn't allocate waiter");
        let r = new_telnet_reflector(&o).expect("Allocate reflector failed");

	let e = Arc::new(SerialEvInst{
	    w,
	    expect_val: Mutex::new(None),
	});
	let fs = format!("telnet(rfc2217),tcp,localhost,{}", r.port);
	let g = Gensio::new(&fs, &o, Arc::downgrade(&e) as _)
	    .expect("Gensio allocation failed");
	g.open_s().expect("Open failed");
	g.read_enable(true);

	test_acontrol(&e, &g, "baud", "9600", "19200",
		      AControlOp::SerBaud);
	test_acontrol(&e, &g, "datasize", "8", "7",
		      AControlOp::SerDatasize);
	test_acontrol(&e, &g, "stopbits", "1", "2",
		      AControlOp::SerStopbits);
	test_acontrol(&e, &g, "parity", "none", "odd",
		      AControlOp::SerParity);
	test_acontrol(&e, &g, "flowcontrol", "none", "xonxoff",
		      AControlOp::SerFlowcontrol);
	test_acontrol(&e, &g, "iflowcontrol", "none", "dtr",
		      AControlOp::SerIFlowcontrol);
	test_acontrol(&e, &g, "sbreak", "off", "on",
		      AControlOp::SerSBreak);
	test_acontrol(&e, &g, "dtr", "off", "on",
		      AControlOp::SerDtr);
	test_acontrol(&e, &g, "rts", "off", "on",
		      AControlOp::SerRts);
	test_acontrol(&e, &g, "signature", "mysig", "mysig",
		      AControlOp::SerSignature);

	g.close_s().expect("Close failed");
    }
}
