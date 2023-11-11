// Copyright 2023 Corey Minyard
//
// SPDX-License-Identifier: Apache-2.0

use std::sync::Arc;
use crate::osfuncs;
use crate::addr;
pub mod raw;

pub struct MDNS {
    _m: *const raw::gensio_mdns,
}

pub fn new(_o: &osfuncs::OsFuncs) -> Result<MDNS, i32> {
    Err(crate::GE_NOTSUP)
}

pub trait MDNSDone {
    fn done(&self);
}

/// For the event value of ServiceDone.done.
pub const GENSIO_MDNS_SERVICE_ERROR: i32 = 0;
pub const GENSIO_MDNS_SERVICE_READY: i32 = 1;
pub const GENSIO_MDNS_SERVICE_READY_NEW_NAME: i32 = 2;
pub const GENSIO_MDNS_SERVICE_REMOVED: i32 = 3;

pub trait ServiceDone {
    fn done(&self, event: i32, info: &Option<String>);
}

pub trait WatchDone {
    fn done(&self);
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
	     txt: Option<&[String]>);
}

impl MDNS {
    pub fn shutdown(&self, _done: Arc<dyn MDNSDone>) -> Result<(), i32> {
	Err(crate::GE_NOTSUP)
    }

    pub fn add_service(_iface: i32, _ipdomain: i32,
		       _name: Option<&str>, _mtype: Option<&str>,
		       _domain: Option<&str>, _host: Option<&str>,
		       _port: i32, _txt: Option<&[String]>,
		       _done: Arc<dyn ServiceDone>)
		       -> Result<Service, i32> {
	Err(crate::GE_NOTSUP)
    }
		       
    pub fn add_watch(_iface: i32, _ipdomain: i32,
		     _name: Option<&str>, _mtype: Option<&str>,
		     _domain: Option<&str>, _host: Option<&str>,
		     _cb: Arc<dyn WatchEvent>) -> Result<Watch, i32> {
	Err(crate::GE_NOTSUP)
    }
}

impl Drop for MDNS {
    fn drop(&mut self) {
    }
}

pub struct Service {
    _s: *const raw::gensio_mdns_service,
}

impl Service {
    pub fn shutdown(&self, _done: Arc<dyn ServiceDone>) -> Result<(), i32> {
	Err(crate::GE_NOTSUP)
    }
}

impl Drop for Service {
    fn drop(&mut self) {
    }
}

pub struct Watch {
    _w: *const raw::gensio_mdns_watch,
}

impl Watch {
    pub fn shutdown(&self, _done: Arc<dyn WatchDone>) -> Result<(), i32> {
	Err(crate::GE_NOTSUP)
    }
}

impl Drop for Watch {
    fn drop(&mut self) {
    }
}
