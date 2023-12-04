// Copyright 2023 Corey Minyard
//
// SPDX-License-Identifier: Apache-2.0

use std::sync::Weak;
use std::sync::Mutex;
use std::ffi;
use std::ffi::CString;
use crate::osfuncs;
use crate::osfuncs::OsFuncs;
use crate::osfuncs::Waiter;
use crate::addr;
pub mod raw;
use std::panic;
use crate::Error;
use crate::val_to_error;

pub struct MDNS {
    o: OsFuncs,
    m: *const raw::gensio_mdns,
    closed: Mutex<bool>,
}

struct MDNSDoneData {
    cb: Weak<dyn MDNSDone>,
}

/// Used to report when an MDNS object has finished being freed.
/// Note: You must keep this object around because it is stored as Weak.
/// If you allow this object to be done, the callbacks will stop working.
pub trait MDNSDone {
    fn done(&self);
}

fn i_mdns_done(_m: *const raw::gensio_mdns, cb_data: *mut ffi::c_void) {
    let d = unsafe { Box::from_raw(cb_data as *mut MDNSDoneData) };
    let cb = match d.cb.upgrade() {
	None => return,
	Some(cb) => cb
    };
    cb.done();
}

extern "C" fn mdns_done(m: *const raw::gensio_mdns,
			cb_data: *mut ffi::c_void) {
    let _r = panic::catch_unwind(|| {
	i_mdns_done(m, cb_data);
    });
}

/// Specify information about an MDNS watch or service, all the names
/// and such.
pub struct MDNSSpec {
    iface: i32,
    ipdomain: i32,
    name: Option<String>,
    mtype: Option<String>,
    domain: Option<String>,
    host: Option<String>,
}

fn optstr_to_cstr(s: &Option<String>)
		  -> Result<(*const ffi::c_char, Option<CString>), Error> {
    match s {
	None => Ok((std::ptr::null(), None)),
	Some(s) => {
	    match ffi::CString::new(s.as_str()) {
		Ok(s) => Ok((s.as_ptr(), Some(s))),
		Err(_) => Err(crate::Error::Inval)
	    }
	}
    }
}

impl MDNS {
    pub fn new(o: &OsFuncs) -> Result<Self, i32> {
        let m: *const raw::gensio_mdns = std::ptr::null();
        let rv = unsafe { raw::gensio_alloc_mdns(o.raw(), &m) };
        if rv != 0 {
	    return Err(rv)
        }
        Ok(MDNS { o: o.clone(), m, closed: Mutex::new(false) })
    }

    pub fn shutdown(&self, cb: Weak<dyn MDNSDone>) -> Result<(), Error> {
	let mut closed = self.closed.lock().unwrap();
	if *closed {
	    return Err(crate::Error::NotReady);
	}
	*closed = true;

	let d = Box::new(MDNSDoneData { cb });
	let d = Box::into_raw(d);
	// Nothing below here can panic, so it's safe to convert the
	// box to raw.
	let err = unsafe { raw::gensio_free_mdns(self.m, mdns_done,
						 d as *mut ffi::c_void) };
	if err != 0 {
	    unsafe { drop(Box::from_raw(d)); }
	    return Err(val_to_error(err));
	}
	Ok(())
    }
}

fn cstrptr_to_optstring(cstr: *const ffi::c_char) -> Option<String> {
    if cstr.is_null() {
	None
    } else {
	let cs = unsafe { ffi::CStr::from_ptr(cstr) };
	let s = cs.to_str().expect("Invalid service info string").to_string();
	Some(s)
    }
}

fn optstring_to_optstr(s: &Option<String>) -> Option<&str> {
    match s {
	None => None,
	Some(s) => Some(s.as_str())
    }
}

/// For the event value of ServiceEvent.done.
pub const GENSIO_MDNS_SERVICE_ERROR: i32 = 0;
pub const GENSIO_MDNS_SERVICE_READY: i32 = 1;
pub const GENSIO_MDNS_SERVICE_READY_NEW_NAME: i32 = 2;
pub const GENSIO_MDNS_SERVICE_REMOVED: i32 = 3;

/// Used to report that the service is ready or not.
/// Note: You must keep this object around because it is stored as Weak.
/// If you allow this object to be done, the callbacks will stop working.
pub trait ServiceEvent {
    fn event(&self, event: i32, info: Option<&str>);
}

struct ServiceData {
    _o: OsFuncs, // Keep osfuncs around
    cb: Weak<dyn ServiceEvent>,
}

pub struct Service {
    s: *const raw::gensio_mdns_service,
    _d: *mut ServiceData,
}

fn i_service_event(_s: *const raw::gensio_mdns_service,
		   event: ffi::c_int,
		   info: *const ffi::c_char,
		   cb_data: *mut ffi::c_void) {
    let d = cb_data as *mut ServiceData;
    let infosstr = cstrptr_to_optstring(info);
    let infos = optstring_to_optstr(&infosstr);
    match unsafe { (*d).cb.upgrade() } {
	None => (),
	Some(cb) => cb.event(event, infos)
    }
    if event == GENSIO_MDNS_SERVICE_REMOVED {
	unsafe { drop(Box::from_raw(d)) };
    }
}

extern "C" fn service_event(s: *const raw::gensio_mdns_service,
			    event: ffi::c_int,
			    info: *const ffi::c_char,
			    cb_data: *mut ffi::c_void) {
    let _r = panic::catch_unwind(|| {
	i_service_event(s, event, info, cb_data);
    });
}

impl Service {
    pub fn new(m: &MDNS, spec: &MDNSSpec,
	       port: i32, txt: Option<&[&str]>,
	       cb: Weak<dyn ServiceEvent>)
	       -> Result<Service, Error> {
	let closed = m.closed.lock().unwrap();
	if *closed {
	    return Err(crate::Error::NotReady);
	}

	let sd = Box::new(ServiceData { _o: m.o.clone(), cb });
	let s: *const raw::gensio_mdns_service = std::ptr::null();
	let (namep, _nameh) = optstr_to_cstr(&spec.name)?;
	let (typep, _typeh) = optstr_to_cstr(&spec.mtype)?;
	let (domainp, _domainh) = optstr_to_cstr(&spec.domain)?;
	let (hostp, _hosth) = optstr_to_cstr(&spec.host)?;
	let txt1 = match txt {
	    None => None,
	    Some(v) => Some(crate::strslice_to_auxvec(v)?)
	};
	let txt2: *mut *mut ffi::c_char = match txt1 {
	    None => std::ptr::null_mut(),
	    Some(ref v) => v.as_ptr()
	};
	let sd = Box::into_raw(sd);
	// Nothing below here can panic, so it's safe to convert the
	// box to raw.
	let err = unsafe {
	    raw::gensio_mdns_add_service2(m.m, spec.iface as ffi::c_int,
					  spec.ipdomain as ffi::c_int,
					  namep, typep, domainp, hostp,
					  port as ffi::c_int,
					  txt2 as *const *const ffi::c_char,
					  service_event, sd as *mut ffi::c_void,
					  &s)
	};
	if err != 0 {
	    unsafe { drop(Box::from_raw(sd)) };
	    return Err(val_to_error(err));
	}
	Ok(Service { s, _d: sd })
    }

    pub fn shutdown(&self) -> Result<(), Error> {
	let err = unsafe { raw::gensio_mdns_remove_service(self.s) };
	match err {
	    0 => Ok(()),
	    _ => Err(val_to_error(err))
	}
    }
}

impl Drop for Service {
    fn drop(&mut self) {
	unsafe { raw::gensio_mdns_remove_service(self.s) };
    }
}

struct WatchDoneData {
    cb: Option<Weak<dyn WatchDone>>,
    d: *mut WatchData,
}

/// Used to report that a watch has finished shutdown.
/// Note: You must keep this object around because it is stored as Weak.
/// If you allow this object to be done, the callbacks will stop working.
pub trait WatchDone {
    fn done(&self);
}

fn i_watch_done(_w: *const raw::gensio_mdns_watch,
		cb_data: *mut ffi::c_void) {
    let d = unsafe { Box::from_raw(cb_data as *mut WatchDoneData) };
    let cb = match d.cb {
	None => None,
	Some(cb) => cb.upgrade()
    };
    {
	let mut state = unsafe { (*d.d).state.lock().unwrap() };
	match *state {
	    CloseState::WaitClose => {
		// The Gensio is being dropped and waiting for us to complete.
		unsafe { (*d.d).close_waiter.wake(); }
	    }
	    _ => *state = CloseState::Closed,
	}
    }
    match cb {
	None => (),
	Some(cb) => cb.done()
    }
}

extern "C" fn watch_done(s: *const raw::gensio_mdns_watch,
			 cb_data: *mut ffi::c_void) {
    let _r = panic::catch_unwind(|| {
	i_watch_done(s, cb_data);
    });
}

/// For the state value of watch
pub const GENSIO_MDNS_WATCH_NEW_DATA: i32 = 0;
pub const GENSIO_MDNS_WATCH_DATA_GONE: i32 = 1;
pub const GENSIO_MDNS_WATCH_ALL_FOR_NOW: i32 = 2;

/// Handles events from the watch.
/// Note: You must keep this object around because it is stored as Weak.
/// If you allow this object to be done, the callbacks will stop working.
pub trait WatchEvent {
    fn watch(&self,
	     state: i32,
	     spec: &MDNSSpec,
	     addr: Option<addr::Addr>,
	     txt: Option<&[&str]>);
}

enum CloseState {
    Open,
    InClose,
    WaitClose,
    Closed,
}

struct WatchData {
    _o: OsFuncs, // Keep osfuncs around
    cb: Weak<dyn WatchEvent>,
    state: Mutex<CloseState>,
    close_waiter: osfuncs::Waiter,
}

pub struct Watch {
    w: *const raw::gensio_mdns_watch,
    d: *mut WatchData,
}

struct IMDNSSpec {
    iface: ffi::c_int,
    ipdomain: ffi::c_int,
    name: *const ffi::c_char,
    mtype: *const ffi::c_char,
    domain: *const ffi::c_char,
    host: *const ffi::c_char,
}

fn i_watch_event(_w: *const raw::gensio_mdns_watch,
		 state: ffi::c_int,
		 spec: &IMDNSSpec,
		 addr: *const crate::addr::raw::gensio_addr,
		 txt: *const *const ffi::c_char,
		 cb_data: *mut ffi::c_void) {
    let d = cb_data as *mut WatchData;
    let cb = unsafe { (*d).cb.upgrade() };
    let cb = match cb {
	None => return,
	Some(cb) => cb
    };

    let namesstr = cstrptr_to_optstring(spec.name);
    let typesstr = cstrptr_to_optstring(spec.mtype);
    let domainsstr = cstrptr_to_optstring(spec.domain);
    let hostsstr = cstrptr_to_optstring(spec.host);
    let addro: Option<addr::Addr> =
	if addr.is_null() {
	    None
	} else {
	    Some(match unsafe { addr::new(addr) } {
		Ok(a) => a,
		Err(_) => return
	    })
	};
    let txt = unsafe { crate::aux_to_stringvec(txt) };
    let txt2 = crate::stringvec_to_strvec(&txt);
    let txt3 = txt2.as_deref();

    cb.watch(state, &MDNSSpec { iface: spec.iface,
				ipdomain: spec.ipdomain,
				name: namesstr, mtype: typesstr,
				domain: domainsstr, host: hostsstr },
	     addro, txt3);
}

extern "C" fn watch_event(w: *const raw::gensio_mdns_watch,
			  state: ffi::c_int,
			  iface: ffi::c_int,
			  ipdomain: ffi::c_int,
			  name: *const ffi::c_char,
			  mtype: *const ffi::c_char,
			  domain: *const ffi::c_char,
			  host: *const ffi::c_char,
			  addr: *const crate::addr::raw::gensio_addr,
			  txt: *const *const ffi::c_char,
			  cb_data: *mut ffi::c_void) {
    let _r = panic::catch_unwind(|| {
	i_watch_event(w, state, &IMDNSSpec{ iface, ipdomain,
					    name, mtype, domain, host },
		      addr, txt, cb_data);
    });
}

impl Watch {
    pub fn new(m: &MDNS, spec: &MDNSSpec, cb: Weak<dyn WatchEvent>)
		     -> Result<Watch, Error> {
	let closed = m.closed.lock().unwrap();
	if *closed {
	    return Err(crate::Error::NotReady);
	}

	let wd = Box::new(WatchData {
	    _o: m.o.clone(),
	    cb,
	    state: Mutex::new(CloseState::Open),
	    close_waiter: Waiter::new(&m.o)?,
	});
	let w: *const raw::gensio_mdns_watch = std::ptr::null();
	let (namep, _nameh) = optstr_to_cstr(&spec.name)?;
	let (typep, _typeh) = optstr_to_cstr(&spec.mtype)?;
	let (domainp, _domainh) = optstr_to_cstr(&spec.domain)?;
	let (hostp, _hosth) = optstr_to_cstr(&spec.host)?;
	let wd = Box::into_raw(wd);
	// Nothing below here can panic, so it's safe to convert the
	// box to raw.
	let err = unsafe {
	    raw::gensio_mdns_add_watch(m.m, spec.iface as ffi::c_int,
				       spec.ipdomain as ffi::c_int,
				       namep, typep, domainp, hostp,
				       watch_event, wd as *mut ffi::c_void,
				       &w)
	};
	if err != 0 {
	    unsafe { drop(Box::from_raw(wd)) };
	    return Err(val_to_error(err));
	}
	Ok(Watch { w, d: wd })
    }

    fn i_shutdown(&self, cb: Option<Weak<dyn WatchDone>>)
		  -> Result<(), Error> {
	let d = Box::new(WatchDoneData { cb, d: self.d });
	let d = Box::into_raw(d);
	// Nothing below here can panic, so it's safe to convert the
	// box to raw.
	let err = unsafe { raw::gensio_mdns_remove_watch(
	    self.w, watch_done, d as *mut ffi::c_void) };
	if err != 0 {
	    unsafe { drop(Box::from_raw(d)); }
	    return Err(val_to_error(err));
	}
	Ok(())
    }

    pub fn shutdown(&self, done: Option<Weak<dyn WatchDone>>)
		    -> Result<(), Error> {
	let mut state = unsafe { (*self.d).state.lock().unwrap() };
	match *state {
	    CloseState::Open => (),
	    _ => return Err(crate::Error::InUse)
	}
	*state = CloseState::InClose;

	let rv = self.i_shutdown(done);
	if rv.is_ok() {
	    *state = CloseState::InClose;
	}
	rv
    }
}

impl Drop for Watch {
    fn drop(&mut self) {
	unsafe {
	    let mut do_wait = false;
	    {
		let mut state = (*self.d).state.lock().unwrap();
		match *state {
		    CloseState::Closed => (),
		    CloseState::WaitClose => (), // Shouldn't happen
		    CloseState::InClose => {
			*state = CloseState::WaitClose;
			do_wait = true;
		    }
		    CloseState::Open => {
			*state = CloseState::WaitClose;
			let err = self.i_shutdown(None);
			// FIXME - what to do on error?
			if err.is_ok() { do_wait = true };
		    }
		}
	    }
	    if do_wait {
                _ = (*self.d).close_waiter.wait(1, None);
	    }
	    drop(Box::from_raw(self.d));
	}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use std::time::Duration;
    use serial_test::serial;

    struct LogHandler;

    impl osfuncs::GensioLogHandler for LogHandler {
	fn log(&self, _logstr: String) {
	    // What to fill in here?
	}
    }

    struct IWatchEventData {
	do_check: bool,
	waiting_gone: bool,
	name: String,
	mtype: String,
	txt: Vec<String>,
    }

    struct IWatchEvent {
	w: osfuncs::Waiter,
	d: Mutex<IWatchEventData>,
    }

    impl WatchEvent for IWatchEvent {
	fn watch(&self,
		 state: i32,
		 spec: &MDNSSpec,
		 _addr: Option<addr::Addr>,
		 txt: Option<&[&str]>) {
	    let wake;
	    let d = self.d.lock().unwrap();
	    if !d.do_check {
		return;
	    }
	    match state {
		GENSIO_MDNS_WATCH_ALL_FOR_NOW => {
		    return;
		}
		GENSIO_MDNS_WATCH_NEW_DATA => {
		    wake = !d.waiting_gone;
		}
		GENSIO_MDNS_WATCH_DATA_GONE => {
		    wake = d.waiting_gone;
		}
		_ => return,
	    }

	    let a = _addr.expect("No address").to_string();
	    println!("{a}");

	    assert_eq!(spec.name, Some(d.name.to_string()));
	    assert_eq!(spec.mtype, Some(d.mtype.to_string()));
	    match txt {
		None => panic!("txt should be present"),
		Some(t) => {
		    assert_eq!(t.len(), d.txt.len());
		    for i in 0..t.len() {
			assert_eq!(t[i], d.txt[i].as_str());
		    }
		}
	    }
	    if wake {
		_ = self.w.wake();
	    }
	}
    }

    struct IWatchDone {
	w: osfuncs::Waiter,
    }

    impl WatchDone for IWatchDone {
	fn done(&self) {
	    _ = self.w.wake();
	}
    }

    struct IServiceEvent {
	w: osfuncs::Waiter,
    }

    impl ServiceEvent for IServiceEvent {
	fn event(&self, _event: i32, _info: Option<&str>) {
	    _ = self.w.wake();
	}
    }

    #[test]
    #[serial] // FIXME - figure out why these crash when not serial.
    fn mdns1() {
	let logh = Arc::new(LogHandler);
	let o = OsFuncs::new(Arc::downgrade(&logh) as _)
	    .expect("Couldn't allocate os funcs");
	o.thread_setup().expect("Couldn't setup thread");
	let m = MDNS::new(&o).expect("Failed to allocate MDNS");
	let we = Arc::new(IWatchEvent {
	    w: Waiter::new(&o).expect("Unable to allocate waiter"),
	    d: Mutex::new(IWatchEventData {
		do_check: true,
		waiting_gone: false,
		name: String::from("gensio1"),
		mtype: String::from("_gensio_rusttest._tcp"),
		txt: vec!["A=1".to_string(), "B=2".to_string()],
	    }),
	});
	let w = Watch::new(&m,
	    &MDNSSpec {
		iface: -1,
		ipdomain: addr::GENSIO_NETTYPE_UNSPEC,
		name: None,
		mtype: Some("=_gensio_rusttest._tcp".to_string()),
		domain: None, host: None },
	    Arc::downgrade(&we) as _)
	    .expect("Unable to allocate watch");
	let se = Arc::new(IServiceEvent {
	    w: Waiter::new(&o).expect("Unable to allocate waiter")
	});
	let s = Service::new(&m,
	    &MDNSSpec {
		iface: -1,
		ipdomain: addr::GENSIO_NETTYPE_UNSPEC,
		name: Some("gensio1".to_string()),
		mtype: Some("_gensio_rusttest._tcp".to_string()),
		domain: None, host: None },
	    5001, Some(&["A=1", "B=2"]),
	    Arc::downgrade(&se) as _)
	    .expect("Unable to allocate watch");
	se.w.wait(1, Some(&Duration::new(1, 0))).expect("Wait failed");
	we.w.wait(1, Some(&Duration::new(5, 0))).expect("Wait failed");
	{
	    let mut d = we.d.lock().unwrap();
	    d.waiting_gone = true;
	}
	drop(s);
	we.w.wait(1, Some(&Duration::new(5, 0))).expect("Wait failed");
	let wd = Arc::new(IWatchDone {
	    w: Waiter::new(&o).expect("Unable to allocate waiter")
	});
	w.shutdown(Some(Arc::downgrade(&wd) as _)).expect("Shutdown failed");
	wd.w.wait(1, Some(&Duration::new(5, 0))).expect("Wait failed");
    }

    #[test]
    #[serial] // FIXME - figure out why these crash when not serial.
    fn mdns2() {
	let logh = Arc::new(LogHandler);
	let o = OsFuncs::new(Arc::downgrade(&logh) as _)
	    .expect("Couldn't allocate os funcs");
	o.thread_setup().expect("Couldn't setup thread");
	let m = MDNS::new(&o).expect("Failed to allocate MDNS");
	let we = Arc::new(IWatchEvent {
	    w: Waiter::new(&o).expect("Unable to allocate waiter"),
	    d: Mutex::new(IWatchEventData {
		do_check: false,
		waiting_gone: false,
		name: String::from(""),
		mtype: String::from(""),
		txt: vec![],
	    }),
	});
	let _w = Watch::new(&m,
	    &MDNSSpec {
		iface: -1,
		ipdomain: addr::GENSIO_NETTYPE_UNSPEC,
		name: None,
		mtype: Some("=_gensio_rusttest._tcp".to_string()),
		domain: None, host: None },
	    Arc::downgrade(&we) as _)
	    .expect("Unable to allocate watch");
	let se = Arc::new(IServiceEvent {
	    w: Waiter::new(&o).expect("Unable to allocate waiter")
	});
	let _s = Service::new(&m,
	    &MDNSSpec {
		iface: -1,
		ipdomain: addr::GENSIO_NETTYPE_UNSPEC,
		name: Some("gensio1".to_string()),
		mtype: Some("_gensio_rusttest._tcp".to_string()),
		domain: None, host: None },
	    5001, Some(&["A=1", "B=2"]),
	    Arc::downgrade(&se) as _)
	    .expect("Unable to allocate watch");
	// Test automatic cleanup
    }
}
