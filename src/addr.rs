// Copyright 2023 Corey Minyard
//
// SPDX-License-Identifier: Apache-2.0

pub mod raw;

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

pub fn new(ai: *const raw::gensio_addr) -> Addr {
    Addr { ai: ai }
}

impl Drop for Addr {
    fn drop(&mut self) {
	unsafe { raw::gensio_addr_free(self.ai); }
    }
}
