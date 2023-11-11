// Copyright 2023 Corey Minyard
//
// SPDX-License-Identifier: Apache-2.0

pub mod raw;

pub struct Addr {
    ai: *const raw::gensio_addr,
}

pub fn new(ai: *const raw::gensio_addr) -> Addr {
    Addr { ai: ai }
}

impl Drop for Addr {
    fn drop(&mut self) {
	unsafe { raw::gensio_addr_free(self.ai); }
    }
}
