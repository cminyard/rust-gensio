//! # addr
//!
//! This holds code for manipulating gensio addresses.
//!

// Copyright 2023 Corey Minyard
//
// SPDX-License-Identifier: Apache-2.0

use std::ffi;
use std::ffi::CString;
use crate::GensioDS;
use crate::osfuncs;
use crate::Error;
use crate::val_to_error;

pub mod raw;

/// Structure representing a gensio address.
pub struct Addr {
    ai: *const raw::gensio_addr,
}

pub const GENSIO_NET_PROTOCOL_TCP: i32 = 1;
pub const GENSIO_NET_PROTOCOL_UDP: i32 = 2;
pub const GENSIO_NET_PROTOCOL_SCTP: i32 = 3;
pub const GENSIO_NET_PROTOCOL_UNIX: i32 = 4;

pub const GENSIO_NETTYPE_UNSPEC: i32 = 0;
pub const GENSIO_NETTYPE_IPV4: i32 = 1;
pub const GENSIO_NETTYPE_IPV6: i32 = 2;
pub const GENSIO_NETTYPE_UNIX: i32 = 3;
pub const GENSIO_NETTYPE_AX25: i32 = 4;

/// # Safety
///
/// Create a new address from a raw gensio address.  This is mostly for
/// internal use, the ai value must be valid.
pub unsafe fn new(ai: *const raw::gensio_addr) -> Result<Addr, Error> {
    let naddr = unsafe { raw::gensio_addr_dup(ai) };
    if ai.is_null() {
	return Err(crate::Error::NoMem);
    }
    Ok(Addr { ai: naddr })
}

/// Create an address from the nettype, port, and a buffer with the
/// address info, either the ipv4 address, ipv6 address, or the unix
/// path.  Doesn't work for AX25.
pub fn from_bytes(o: &osfuncs::OsFuncs, nettype: i32, buf: &[u8], port: u32)
		  -> Result<Addr, Error> {
    let ai: *const raw::gensio_addr = std::ptr::null();
    let rv = unsafe {
	raw::gensio_addr_create(o.raw(), nettype as ffi::c_int,
				buf.as_ptr() as *const ffi::c_void,
				buf.len() as GensioDS,
				port as ffi::c_uint, &ai)
    };
    if rv != 0 {
	return Err(val_to_error(rv));
    }
    Ok(Addr { ai })
}

/// Create an address from an address string.  Doesn't work for AX25.
pub fn from_str(o: &osfuncs::OsFuncs, s: &str, protocol: i32, listen: bool)
		-> Result<Addr, Error> {
    let ai: *const raw::gensio_addr = std::ptr::null();
    let cstr = match CString::new(s) {
	Ok(s) => s,
	Err(_) => return Err(crate::Error::Inval)
    };
    let listeni = match listen { true => 1, false => 0 };
    let rv = unsafe {
	raw::gensio_os_scan_netaddr(o.raw(),
				    cstr.as_ptr() as *const ffi::c_char,
				    listeni as ffi::c_int,
				    protocol as ffi::c_int,
				    &ai)
    };
    if rv != 0 {
	return Err(val_to_error(rv));
    }
    Ok(Addr { ai })
}

impl Addr {
    /// Move the current working address to the next address.  Returns
    /// false if at the last address.
    pub fn next(&self) -> bool {
	unsafe { raw::gensio_addr_next(self.ai) != 0 }
    }

    /// Move the current working address to the first address.
    pub fn rewind(&self) {
	unsafe { raw::gensio_addr_rewind(self.ai) };
    }

    /// Get the nettype from the address.
    pub fn nettype(&self) -> i32 {
	unsafe { raw::gensio_addr_get_nettype(self.ai) }
    }

    /// Get the port from the address.  Returns -1 if the address doesn't
    /// use a port.
    pub fn port(&self) -> i32 {
	unsafe { raw::gensio_addr_get_port(self.ai) }
    }

    /// Compare two addresses for equality.  cmp_port sets whether the
    /// ports are compared.  If cmp_all is set, all addresses in the
    /// address are compared.
    pub fn equal(&self, a2: &Addr, cmp_port: bool, cmp_all: bool) -> bool {
	let p = match cmp_port { true => 1, false => 0 };
	let a = match cmp_all { true => 1, false => 0 };
	unsafe { raw::gensio_addr_equal(self.ai, a2.ai, p, a) != 0 }
    }

    /// Get the raw addres data for the address, either the ipv4 address,
    /// ipv6 address, or the unix path.
    pub fn to_bytes(&self) -> Vec<u8> {
	let mut buf: Vec<u8> = Vec::new();
	buf.push(0); // Ensure some data is there.
	let mut len: GensioDS = 0;

	unsafe {
	    raw::gensio_addr_get_data(self.ai,
				      buf.as_mut_ptr() as *mut ffi::c_void,
				      &mut len)
	};
	buf.reserve(len as usize);
	unsafe {
	    raw::gensio_addr_get_data(self.ai,
				      buf.as_mut_ptr() as *mut ffi::c_void,
				      &mut len);
	};
	unsafe { buf.set_len(len as usize); }
	buf
    }
}

impl Clone for Addr {
    fn clone(&self) -> Self {
	let naddr = unsafe { raw::gensio_addr_dup(self.ai) };
	if naddr.is_null() {
	    panic!("Address clone failed");
	}
	Addr { ai: naddr }
    }
}

impl ToString for Addr {
    fn to_string(&self) -> String {
	let mut buf: Vec<u8> = Vec::new();
	// We have to allocate something to have a pointer to pass to
	// gensio_addr_to_string(), this should be enough for any
	// address.
	buf.reserve(100);
	let mut pos: GensioDS = 0;

	let rv = unsafe {
	    raw::gensio_addr_to_str(self.ai,
				    buf.as_mut_ptr() as *mut ffi::c_char,
				    &mut pos, buf.capacity() as GensioDS)
	};
	if rv != 0 {
	    panic!("Unable to convert address to string");
	}
	if pos as usize > buf.capacity() + 1 {
	    // Just in case the address is too big for the first allocation.
	    buf.reserve(pos as usize + 1);
	    pos = 0;
	    let rv = unsafe {
		raw::gensio_addr_to_str(self.ai,
					buf.as_mut_ptr() as *mut ffi::c_char,
					&mut pos, buf.capacity() as GensioDS)
	    };
	    if rv != 0 {
		panic!("Unable to convert address to string");
	    }
	}
	unsafe { buf.set_len(pos as usize); }
	let cstr = ffi::CString::new(buf).expect("Invalid address string");
	cstr.into_string().expect("Invalid address utf8")
    }
}

impl PartialEq for Addr {
    fn eq(&self, a2: &Addr) -> bool {
	self.equal(a2, true, false)
    }
}

impl Drop for Addr {
    fn drop(&mut self) {
	unsafe { raw::gensio_addr_free(self.ai); }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    struct LogHandler;

    impl osfuncs::GensioLogHandler for LogHandler {
	fn log(&self, _logstr: String) {
	    // What to fill in here?
	}
    }

    #[test]
    fn addr() {
	let logh = Arc::new(LogHandler);
	let loghw = Arc::downgrade(&logh);
	let o = osfuncs::new(loghw)
	    .expect("Couldn't allocate os funcs");
	o.thread_setup().expect("Couldn't setup thread");

	let a1 = from_str(&o, "ipv4,127.0.0.1,3000", GENSIO_NET_PROTOCOL_TCP,
			  false).expect("Error in address creation");
	assert_eq!(a1.to_string(), "ipv4,127.0.0.1,3000".to_string());
	assert_eq!(a1.port(), 3000);
	assert_eq!(a1.nettype(), GENSIO_NETTYPE_IPV4);
	let a2 = from_bytes(&o, a1.nettype(), &a1.to_bytes(), a1.port() as u32).
	    expect("Couldn't create address 2");
	assert!(a1.eq(&a2));
    }
}
