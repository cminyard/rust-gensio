// Copyright 2023 Corey Minyard
//
// SPDX-License-Identifier: Apache-2.0

use std::ffi;
use crate::addr::raw::gensio_addr;
use crate::osfuncs;

#[repr(C)]
pub struct gensio_mdns;

#[repr(C)]
pub struct gensio_mdns_service;

#[repr(C)]
pub struct gensio_mdns_watch;

#[allow(non_camel_case_types)]
pub type gensio_mdns_done = extern "C" fn (m: *const gensio_mdns,
					   data: *mut ffi::c_void);

#[allow(non_camel_case_types)]
pub type gensio_mdns_service_cb = extern "C" fn (s: *const gensio_mdns_service,
						 event: ffi::c_int,
						 info: *const ffi::c_char,
						 data: *mut ffi::c_void);

#[allow(non_camel_case_types)]
pub type gensio_mdns_watch_cb = extern "C" fn (w: *const gensio_mdns_watch,
					       state: ffi::c_int,
					       iface: ffi::c_int,
					       ipdomain: ffi::c_int,
					       name: *const ffi::c_char,
					       mtype: *const ffi::c_char,
					       domain: *const ffi::c_char,
					       host: *const ffi::c_char,
					       addr: *const gensio_addr,
					       txt: *const *const ffi::c_char,
					       data: *mut ffi::c_void);

#[allow(non_camel_case_types)]
pub type gensio_mdns_watch_done = extern "C" fn (w: *const gensio_mdns_watch,
						 data: *mut ffi::c_void);

#[link(name = "gensioosh")]
#[link(name = "gensiomdns")]
extern "C" {
    #[allow(improper_ctypes)]
    pub fn gensio_alloc_mdns(o: *const osfuncs::raw::gensio_os_funcs,
			     m: *const *const gensio_mdns) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_free_mdns(m: *const gensio_mdns,
			    done: gensio_mdns_done,
			    cb_data: *mut ffi::c_void) -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_mdns_add_service2(m: *const gensio_mdns,
				    iface: ffi::c_int,
				    ipdomain: ffi::c_int,
				    name: *const ffi::c_char,
				    mtype: *const ffi::c_char,
				    domain: *const ffi::c_char,
				    host: *const ffi::c_char,
				    port: ffi::c_int,
				    txt: *const *const ffi::c_char,
				    done: gensio_mdns_service_cb,
				    cb_data: *mut ffi::c_void,
				    s: *const *const gensio_mdns_service)
				    -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_mdns_remove_service(s: *const gensio_mdns_service)
				      -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_mdns_add_watch(m: *const gensio_mdns,
				 iface: ffi::c_int,
				 ipdomain: ffi::c_int,
				 name: *const ffi::c_char,
				 mtype: *const ffi::c_char,
				 domain: *const ffi::c_char,
				 host: *const ffi::c_char,
				 done: gensio_mdns_watch_cb,
				 cb_data: *mut ffi::c_void,
				 w: *const *const gensio_mdns_watch)
				 -> ffi::c_int;

    #[allow(improper_ctypes)]
    pub fn gensio_mdns_remove_watch(s: *const gensio_mdns_watch,
				    done: gensio_mdns_watch_done,
    				    cb_data: *mut ffi::c_void)
				    -> ffi::c_int;
}

