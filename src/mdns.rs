// Copyright 2023 Corey Minyard
//
// SPDX-License-Identifier: Apache-2.0

use std::sync::Arc;
use std::sync::Mutex;
use std::ffi;
use std::ffi::CString;
use crate::osfuncs;
use crate::addr;
pub mod raw;
use std::panic;

pub struct MDNS {
    o: osfuncs::OsFuncs,
    m: *const raw::gensio_mdns,
    closed: Mutex<bool>,
}

pub fn new(o: &osfuncs::OsFuncs) -> Result<MDNS, i32> {
    let m: *const raw::gensio_mdns = std::ptr::null();
    let rv = unsafe { raw::gensio_alloc_mdns(o.raw(), &m) };
    if rv != 0 {
	return Err(rv)
    }
    Ok(MDNS { o: o.clone(), m: m, closed: Mutex::new(false) })
}

struct MDNSDoneData {
    cb: Arc<dyn MDNSDone>,
}

pub trait MDNSDone {
    fn done(&self);
}

fn i_mdns_done(_m: *const raw::gensio_mdns, cb_data: *mut ffi::c_void) {
    let d = unsafe { Box::from_raw(cb_data as *mut MDNSDoneData) };
    d.cb.done();
}

extern "C" fn mdns_done(m: *const raw::gensio_mdns,
			cb_data: *mut ffi::c_void) {
    let _r = panic::catch_unwind(|| {
	i_mdns_done(m, cb_data);
    });
}

fn optstr_to_cstr(s: Option<&str>)
		  -> Result<(*const ffi::c_char, Option<CString>), i32> {
    match s {
	None => Ok((std::ptr::null(), None)),
	Some(s) => {
	    match ffi::CString::new(s) {
		Ok(s) => Ok((s.as_ptr(), Some(s))),
		Err(_) => Err(crate::GE_INVAL)
	    }
	}
    }
}

impl MDNS {
    pub fn shutdown(&self, done: Arc<dyn MDNSDone>) -> Result<(), i32> {
	let mut closed = self.closed.lock().unwrap();
	if *closed {
	    return Err(crate::GE_NOTREADY);
	}
	*closed = true;

	let d = Box::new(MDNSDoneData { cb: done.clone() });
	let d = Box::into_raw(d);
	let err = unsafe { raw::gensio_free_mdns(self.m, mdns_done,
						 d as *mut ffi::c_void) };
	if err != 0 {
	    unsafe { drop(Box::from_raw(d)); }
	    return Err(err);
	}
	Ok(())
    }

    pub fn new_service(&self, iface: i32, ipdomain: i32,
		       name: Option<&str>, mtype: Option<&str>,
		       domain: Option<&str>, host: Option<&str>,
		       port: i32, txt: Option<&[&str]>,
		       done: Arc<dyn ServiceEvent>)
		       -> Result<Service, i32> {
	let closed = self.closed.lock().unwrap();
	if *closed {
	    return Err(crate::GE_NOTREADY);
	}

	let sd = Box::new(ServiceData { _o: self.o.clone(), cb: done });
	let sd = Box::into_raw(sd);
	let s: *const raw::gensio_mdns_service = std::ptr::null();
	let (namep, _nameh) = optstr_to_cstr(name)?;
	let (typep, _typeh) = optstr_to_cstr(mtype)?;
	let (domainp, _domainh) = optstr_to_cstr(domain)?;
	let (hostp, _hosth) = optstr_to_cstr(host)?;
	let txt1 = match txt {
	    None => None,
	    Some(ref v) => Some(crate::vectoaux(v)?)
	};
	let txt2: *mut *mut ffi::c_char = match txt1 {
	    None => std::ptr::null_mut(),
	    Some(ref v) => v.as_ptr() as *mut *mut ffi::c_char
	};
	let err = unsafe {
	    raw::gensio_mdns_add_service2(self.m, iface as ffi::c_int,
					  ipdomain as ffi::c_int,
					  namep, typep, domainp, hostp,
					  port as ffi::c_int,
					  txt2 as *const *const ffi::c_char,
					  service_event, sd as *mut ffi::c_void,
					  &s)
	};
	crate::auxfree(txt1);
	if err != 0 {
	    unsafe { drop(Box::from_raw(sd)) };
	    return Err(err);
	}
	Ok(Service { s: s, _d: sd })
    }

    pub fn new_watch(&self, iface: i32, ipdomain: i32,
		     name: Option<&str>, mtype: Option<&str>,
		     domain: Option<&str>, host: Option<&str>,
		     cb: Arc<dyn WatchEvent>) -> Result<Watch, i32> {
	let closed = self.closed.lock().unwrap();
	if *closed {
	    return Err(crate::GE_NOTREADY);
	}

	let wd = Box::new(WatchData {
	    _o: self.o.clone(),
	    cb: cb,
	    state: Mutex::new(CloseState::Open),
	    close_waiter: self.o.new_waiter()?,
	});
	let wd = Box::into_raw(wd);
	let w: *const raw::gensio_mdns_watch = std::ptr::null();
	let (namep, _nameh) = optstr_to_cstr(name)?;
	let (typep, _typeh) = optstr_to_cstr(mtype)?;
	let (domainp, _domainh) = optstr_to_cstr(domain)?;
	let (hostp, _hosth) = optstr_to_cstr(host)?;
	let err = unsafe {
	    raw::gensio_mdns_add_watch(self.m, iface as ffi::c_int,
				       ipdomain as ffi::c_int,
				       namep, typep, domainp, hostp,
				       watch_event, wd as *mut ffi::c_void,
				       &w)
	};
	if err != 0 {
	    unsafe { drop(Box::from_raw(wd)) };
	    return Err(err);
	}
	Ok(Watch { w: w, d: wd })
    }
}

fn cstrptr_to_optstring(cstr: *const ffi::c_char) -> Option<String> {
    if cstr == std::ptr::null() {
	None
    } else {
	let cs = unsafe { ffi::CStr::from_ptr(cstr) };
	let s = cs.to_str().expect("Invalid service info string").to_string();
	Some(s)
    }
}

fn optstring_to_optstr<'a>(s: &'a Option<String>) -> Option<&'a str> {
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

pub trait ServiceEvent {
    fn event(&self, event: i32, info: Option<&str>);
}

struct ServiceData {
    _o: osfuncs::OsFuncs, // Keep osfuncs around
    cb: Arc<dyn ServiceEvent>,
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
    unsafe { (*d).cb.event(event as i32, infos); }
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
    pub fn shutdown(&self) -> Result<(), i32> {
	let err = unsafe { raw::gensio_mdns_remove_service(self.s) };
	match err {
	    0 => Ok(()),
	    _ => Err(err)
	}
    }
}

impl Drop for Service {
    fn drop(&mut self) {
	unsafe { raw::gensio_mdns_remove_service(self.s) };
    }
}

struct WatchDoneData {
    cb: Option<Arc<dyn WatchDone>>,
    d: *mut WatchData,
}

pub trait WatchDone {
    fn done(&self);
}

fn i_watch_done(_w: *const raw::gensio_mdns_watch,
		cb_data: *mut ffi::c_void) {
    let d = unsafe { Box::from_raw(cb_data as *mut WatchDoneData) };
    let cb = (*d).cb.clone();
    {
	let mut state = unsafe { (*d.d).state.lock().unwrap() };
	match *state {
	    CloseState::WaitClose => {
		// The Gensio is being dropped and waiting for us to complete.
		unsafe { _ = (*d.d).close_waiter.wake(); }
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

pub trait WatchEvent {
    fn watch(&self,
	     state: i32,
	     iface: i32,
	     ipdomain: i32,
	     name: Option<&str>,
	     mtype: Option<&str>,
	     domain: Option<&str>,
	     host:  Option<&str>,
	     addr: Option<addr::Addr>,
	     txt: Option<Vec<String>>);
}

enum CloseState {
    Open,
    InClose,
    WaitClose,
    Closed,
}

struct WatchData {
    _o: osfuncs::OsFuncs, // Keep osfuncs around
    cb: Arc<dyn WatchEvent>,
    state: Mutex<CloseState>,
    close_waiter: osfuncs::Waiter,
}

pub struct Watch {
    w: *const raw::gensio_mdns_watch,
    d: *mut WatchData,
}

fn i_watch_event(_w: *const raw::gensio_mdns_watch,
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
    let d = cb_data as *mut WatchData;
    let namesstr = cstrptr_to_optstring(name);
    let names = optstring_to_optstr(&namesstr);
    let typesstr = cstrptr_to_optstring(mtype);
    let types = optstring_to_optstr(&typesstr);
    let domainsstr = cstrptr_to_optstring(domain);
    let domains = optstring_to_optstr(&domainsstr);
    let hostsstr = cstrptr_to_optstring(host);
    let hosts = optstring_to_optstr(&hostsstr);
    let addro: Option<addr::Addr>;
    if addr == std::ptr::null() {
	addro = None;
    } else {
	addro = Some(addr::new(addr));
    }
    let txt = crate::auxtovec(txt);
    unsafe {
	(*d).cb.watch(state as i32, iface as i32, ipdomain as i32,
		      names, types, domains, hosts,
		      addro, txt);
    }
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
	i_watch_event(w, state, iface, ipdomain,
		      name, mtype, domain, host,
		      addr, txt, cb_data);
    });
}

impl Watch {
    fn i_shutdown(&self, done: Option<Arc<dyn WatchDone>>)
		  -> Result<(), i32> {
	let d = Box::new(WatchDoneData { cb: done.clone(), d: self.d });
	let d = Box::into_raw(d);
	let err = unsafe { raw::gensio_mdns_remove_watch(
	    self.w, watch_done, d as *mut ffi::c_void) };
	if err != 0 {
	    unsafe { drop(Box::from_raw(d)); }
	    return Err(err);
	}
	Ok(())
    }

    pub fn shutdown(&self, done: Option<Arc<dyn WatchDone>>)
		    -> Result<(), i32> {
	let mut state = unsafe { (*self.d).state.lock().unwrap() };
	match *state {
	    CloseState::Open => (),
	    _ => return Err(crate::GE_INUSE)
	}
	*state = CloseState::InClose;

	let rv = self.i_shutdown(done);
	if rv == Ok(()) {
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
			match err {
			    Ok(_) => do_wait = true,
			    _ => () // FIXME - what to do on error?
			}
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
    use std::time::Duration;
    use serial_test::serial;

    struct LogHandler;

    impl osfuncs::GensioLogHandler for LogHandler {
	fn log(&self, _logstr: String) {
	    // What to fill in here?
	}
    }

    struct IWatchEvent {
	w: osfuncs::Waiter,
    }

    impl WatchEvent for IWatchEvent {
	fn watch(&self,
		 _state: i32,
		 _iface: i32,
		 _ipdomain: i32,
		 _name: Option<&str>,
		 _mtype: Option<&str>,
		 _domain: Option<&str>,
		 _host:  Option<&str>,
		 _addr: Option<addr::Addr>,
		 _txt: Option<Vec<String>>) {
	    _ = self.w.wake();
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
	let o = osfuncs::new(Arc::new(LogHandler))
	    .expect("Couldn't allocate os funcs");
	o.thread_setup().expect("Couldn't setup thread");
	let m = new(&o).expect("Failed to allocate MDNS");
	let we = Arc::new(IWatchEvent {
	    w: o.new_waiter().expect("Unable to allocate waiter")
	});
	let w = m.new_watch(-1, addr::GENSIO_NETTYPE_UNSPEC, None,
			    Some("=_gensio_rusttest._tcp"), None, None,
			    we.clone()).expect("Unable to allocate watch");
	let se = Arc::new(IServiceEvent {
	    w: o.new_waiter().expect("Unable to allocate waiter")
	});
	let s = m.new_service(-1, addr::GENSIO_NETTYPE_UNSPEC, Some("gensio1"),
			      Some("=_gensio_rusttest._tcp"), None, None,
			      5001, Some(&["A=1", "B=2"]), se.clone())
	    .expect("Unable to allocate watch");
	se.w.wait(1, Some(&Duration::new(1, 0))).expect("Wait failed");
	we.w.wait(1, Some(&Duration::new(5, 0))).expect("Wait failed");
	drop(s);
	let wd = Arc::new(IWatchDone {
	    w: o.new_waiter().expect("Unable to allocate waiter")
	});
	w.shutdown(Some(wd.clone())).expect("Shutdown failed");
	wd.w.wait(1, Some(&Duration::new(5, 0))).expect("Wait failed");
    }

    #[test]
    #[serial] // FIXME - figure out why these crash when not serial.
    fn mdns2() {
	let o = osfuncs::new(Arc::new(LogHandler))
	    .expect("Couldn't allocate os funcs");
	o.thread_setup().expect("Couldn't setup thread");
	let m = new(&o).expect("Failed to allocate MDNS");
	let we = Arc::new(IWatchEvent {
	    w: o.new_waiter().expect("Unable to allocate waiter")
	});
	let _w = m.new_watch(-1, addr::GENSIO_NETTYPE_UNSPEC, None,
			     Some("=_gensio_rusttest2._tcp"), None, None,
			     we.clone()).expect("Unable to allocate watch");
	let se = Arc::new(IServiceEvent {
	    w: o.new_waiter().expect("Unable to allocate waiter")
	});
	let _s = m.new_service(-1, addr::GENSIO_NETTYPE_UNSPEC, Some("gensio2"),
			       Some("=_gensio_rusttest2._tcp"), None, None,
			       5001, Some(&["A=1", "B=2"]), se.clone())
	    .expect("Unable to allocate watch");
	// Test automatic cleanup
    }
}
