// Copyright 2023 Corey Minyard
//
// SPDX-License-Identifier: Apache-2.0

use std::ffi;
use crate::GensioDS;
use crate::osfuncs;

#[repr(C)]
pub struct gensio_addr;

#[link(name = "gensioosh")]
#[link(name = "gensio")]
extern "C" {
    #[allow(improper_ctypes)]
    pub fn gensio_addr_free(ai: *const gensio_addr);

    #[allow(improper_ctypes)]
    pub fn gensio_addr_dup(ai: *const gensio_addr) -> *const gensio_addr;

    #[allow(improper_ctypes)]
    pub fn gensio_addr_equal(a1: *const gensio_addr, ad: *const gensio_addr,
			     compare_ports: ffi::c_int, compare_all: ffi::c_int)
			     -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_addr_to_str(a: *const gensio_addr, buf: *mut ffi::c_char,
			      epos: &mut GensioDS, buflen: GensioDS)
			      -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_addr_get_nettype(ai: *const gensio_addr) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_addr_get_port(ai: *const gensio_addr) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_addr_next(ai: *const gensio_addr) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_addr_rewind(ai: *const gensio_addr);

    #[allow(improper_ctypes)]
    pub fn gensio_addr_get_data(a: *const gensio_addr, buf: *mut ffi::c_void,
				len: &mut GensioDS);

    #[allow(improper_ctypes)]
    pub fn gensio_addr_create(o: *const osfuncs::raw::gensio_os_funcs,
			      nettype: ffi::c_int,
			      buf: *const ffi::c_void, len: GensioDS,
			      port: ffi::c_uint,
			      newaddr: *const *const gensio_addr) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_os_scan_netaddr(o: *const osfuncs::raw::gensio_os_funcs,
				  str: *const ffi::c_char,
				  listen: ffi::c_int,
				  protocol: ffi::c_int,
				  newaddr: *const *const gensio_addr)
				  -> ffi::c_int;
}
