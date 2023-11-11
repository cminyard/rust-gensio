// Copyright 2023 Corey Minyard
//
// SPDX-License-Identifier: Apache-2.0

//use std::ffi;

#[repr(C)]
pub struct gensio_addr;

#[link(name = "gensioosh")]
#[link(name = "gensio")]
extern "C" {
    #[allow(improper_ctypes)]
    pub fn gensio_addr_free(ai: *const gensio_addr);
}
