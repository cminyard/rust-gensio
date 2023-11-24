//! # netifs
//!
//! `netifs` is a tool for fetching network intefaces and information about
//! them.

// Copyright 2023 Corey Minyard
//
// SPDX-License-Identifier: Apache-2.0


use std::ffi;
use std::slice;
use crate::osfuncs::OsFuncs;
use crate::Error;
use crate::val_to_error;

mod raw;

pub const GENSIO_NET_IF_UP: u32 = 1 << 0;
pub const GENSIO_NET_IF_LOOPBACK: u32 = 1 << 1;
pub const GENSIO_NET_IF_MULTICAST: u32 = 1 << 2;

/// A structure holding information about network interfaces.  It
/// holds an array of interfaces that you must fetch with the index in
/// the get() functions.  Each interface holds a list of addresses you
/// fetch with the index and the address index.
pub struct NetIfs {
    o: OsFuncs,
    ni: *const *const raw::gensio_net_if,
    len: ffi::c_uint,
}

impl NetIfs {
    /// Allocate a new NetIfs structure and fill it in with the network
    /// interfaces.
    pub fn new(o: &OsFuncs) -> Result<Self, Error> {
	let ni: *const *const raw::gensio_net_if = std::ptr::null();
	let mut len: ffi::c_uint = 0;
	let rv = unsafe {
	    raw::gensio_os_get_net_ifs(o.raw(), &ni, &mut len)
	};
	match rv {
	    0 => (),
	    _ => return Err(val_to_error(rv))
	}
	Ok(NetIfs { o: o.clone(), ni, len })
    }

    fn get_ni(&self, idx: usize) -> Result<*const raw::gensio_net_if, Error> {
	if idx >= self.len as usize {
	    return Err(Error::OutOfRange);
	}
	let nis = unsafe { slice::from_raw_parts(self.ni, self.len as usize) };
	Ok(nis[idx])
    }

    /// How many network interfaces are there?
    pub fn len(&self) -> usize {
	self.len as usize
    }

    /// Do we have any network interfaces?
    pub fn is_empty(&self) -> bool {
	self.len == 0
    }

    /// Get the OS name for the network interface.
    pub fn get_name(&self, idx: usize) -> Result<String, Error> {
	let ni = self.get_ni(idx)?;
	let cs = unsafe { ffi::CStr::from_ptr((*ni).name) };
	Ok(String::from_utf8_lossy(cs.to_bytes()).to_string())
    }

    /// Is the network interface operational?
    pub fn is_up(&self, idx: usize) -> Result<bool, Error> {
	let ni = self.get_ni(idx)?;
	Ok(unsafe { (*ni).flags } & GENSIO_NET_IF_UP != 0)
    }

    /// Is the network interface a loopback?
    pub fn is_loopback(&self, idx: usize) -> Result<bool, Error> {
	let ni = self.get_ni(idx)?;
	Ok(unsafe { (*ni).flags } & GENSIO_NET_IF_LOOPBACK != 0)
    }

    /// Does the network interface support multicast?
    pub fn is_multicast(&self, idx: usize) -> Result<bool, Error> {
	let ni = self.get_ni(idx)?;
	Ok(unsafe { (*ni).flags } & GENSIO_NET_IF_MULTICAST != 0)
    }

    /// Get the network interface number on the system.
    pub fn get_ifindex(&self, idx: usize) -> Result<usize, Error> {
	let ni = self.get_ni(idx)?;
	Ok(unsafe { (*ni).ifindex } as usize )
    }

    /// Get the number of addresses in the given network interface.
    pub fn get_num_addrs(&self, idx: usize) -> Result<usize, Error> {
	let ni = self.get_ni(idx)?;
	Ok(unsafe { (*ni).naddrs } as usize )
    }

    fn get_addr(&self, idx: usize, aidx: usize)
		-> Result<&raw::gensio_net_addr, Error> {
	let ni = self.get_ni(idx)?;
	if aidx >= unsafe { (*ni).naddrs } as usize {
	    return Err(crate::Error::OutOfRange);
	}
	let addrs = unsafe {
	    slice::from_raw_parts((*ni).addrs, (*ni).naddrs as usize)
	};
	Ok(&addrs[aidx])
    }

    /// Return the number of network bits in the address prefix.
    pub fn get_addr_netbits(&self, idx: usize, aidx: usize)
			    -> Result<u32, Error> {
	let addr = self.get_addr(idx, aidx)?;
	Ok(addr.netbits as u32)
    }
    /// Return an address string for the network interface.
    pub fn get_addr_str(&self, idx: usize, aidx: usize)
			-> Result<String, Error> {
	let addr = self.get_addr(idx, aidx)?;
	let cs = unsafe { ffi::CStr::from_ptr(addr.addrstr) };
	Ok(String::from_utf8_lossy(cs.to_bytes()).to_string())
    }
}

impl Drop for NetIfs {
    fn drop(&mut self) {
	unsafe { raw::gensio_os_free_net_ifs(self.o.raw(), self.ni, self.len); }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use crate::osfuncs;

    struct LogHandler;

    impl osfuncs::GensioLogHandler for LogHandler {
	fn log(&self, _logstr: String) {
	    // What to fill in here?
	}
    }

    #[test]
    fn netifs() {
	let logh = Arc::new(LogHandler);
	let o = OsFuncs::new(Arc::downgrade(&logh) as _)
	    .expect("Couldn't allocate os funcs");
	o.thread_setup().expect("Couldn't setup thread");

	let nis = NetIfs::new(&o).expect("Unable to get network interfaces");
	for i in 0..nis.len() {
	    crate::puts(&nis.get_name(i).expect("get name failed"));
	    crate::puts(":");
	    if nis.is_up(i).expect("is_up failed") {
		crate::puts(" up");
	    }
	    if nis.is_loopback(i).expect("is_loopback failed") {
		crate::puts(" loopback");
	    }
	    if nis.is_multicast(i).expect("is_multicast failed") {
		crate::puts(" multicast");
	    }
	    crate::puts("\n");
	    for j in 0..nis.get_num_addrs(i).expect("num_addrs") {
		crate::puts("  ");
		crate::puts(&nis.get_addr_str(i, j).expect("get addr failed"));
		crate::puts("/");
		crate::puts(&nis.get_addr_netbits(i, j)
			    .expect("get addr failed").to_string());
		crate::puts("\n");
	    }
	}
    }
}
