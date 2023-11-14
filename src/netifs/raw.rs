// Copyright 2023 Corey Minyard
//
// SPDX-License-Identifier: Apache-2.0

use std::ffi;
use crate::osfuncs;

#[repr(C)]
pub struct gensio_net_addr {
    pub family: ffi::c_uint,
    pub flags: ffi::c_uint,
    pub netbits: ffi::c_uchar,
    pub addrlen: ffi::c_uchar,
    pub addr: [ffi::c_uchar; 16],
    pub addrstr: *const ffi::c_char,
}

#[repr(C)]
pub struct gensio_net_if {
    pub name: *const ffi::c_char,
    pub flags: ffi::c_uint,
    pub ifindex: ffi::c_uint,
    pub naddrs: ffi::c_uint,
    pub addrs: *const gensio_net_addr,
}

#[link(name = "gensioosh")]
extern "C" {
    #[allow(improper_ctypes)]
    pub fn gensio_os_get_net_ifs(o: *const osfuncs::raw::gensio_os_funcs,
				 rifs: *const *const *const gensio_net_if,
				 rnifs: *mut ffi::c_uint) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_os_free_net_ifs(o: *const osfuncs::raw::gensio_os_funcs,
				  rifs: *const *const gensio_net_if,
				  rnifs: ffi::c_uint);
}
