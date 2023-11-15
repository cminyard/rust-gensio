//
// Copyright 2021 Corey Minyard
//
// SPDX-License-Identifier: Apache-2.0

// This is a basic telnet client, it makes a telnet connection and in
// line mode sends anything typed on stdin to the telnet server and
// prints anything that comes back to stdout.

use std::env;
use std::sync::Arc;
use gensio;
use gensio::osfuncs;

// An event handler, data received on io is written to otherio.
struct ClientEvent {
    w: Arc<osfuncs::Waiter>,
    io: Arc<gensio::Gensio>,
    otherio: Arc<gensio::Gensio>,
}

impl ClientEvent {
    fn shutdown(&self) {
        self.io.read_enable(false);
        self.io.write_enable(false);
	_ = self.w.wake();
    }
}

impl gensio::Event for ClientEvent {
    fn err(&self, err: i32) -> i32 {
	println!("Error from gensio: {}", gensio::err_to_str(err));
	self.shutdown();
        0
    }

    fn read(&self, buf: &[u8], _auxdata: Option<&[&str]>)
            -> (i32, usize) {
	match self.otherio.write(buf, None) {
	    Ok(len) => {
		if (len as usize) < buf.len() {
		    self.io.read_enable(false);
		    self.otherio.write_enable(true);
		}
		(0, (len as u64).try_into().unwrap())
	    }
	    Err(err) => {
		self.shutdown();
		println!("Error writing to gensio: {}",
			 gensio::err_to_str(err));
		(err, 0)
	    }
	}
    }

    fn write_ready(&self) -> i32 {
        self.io.write_enable(false);
        self.otherio.read_enable(true);
        0
    }
}

// Use for catching parm logs at allocation.
struct StartupEvent;

impl gensio::Event for StartupEvent {
    fn parmlog(&self, s: String) {
	println!("Parameter error: {}", s);
    }

    fn err(&self, _err: i32) -> i32 {
	gensio::GE_NOTSUP
    }

    fn read(&self, _buf: &[u8], _auxdata: Option<&[&str]>)
            -> (i32, usize) {
	(gensio::GE_NOTSUP, 0)
    }

    fn write_ready(&self) -> i32 {
	gensio::GE_NOTSUP
    }
}

struct LogHandler;

impl osfuncs::GensioLogHandler for LogHandler {
    fn log(&self, logstr: String) {
	println!("{}", &logstr);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
	println!("No ip specification given");
	println!("try: tcp,<ipaddress>,<port>");
	return;
    }
    let mut telnetstr = String::from("telnet,");
    telnetstr.push_str(&args[1]);

    let logh = Arc::new(LogHandler);
    let loghw = Arc::downgrade(&logh);
    let o = osfuncs::new(loghw)
	.expect("Couldn't allocate os funcs");
    o.proc_setup().expect("Couldn't setup thread");

    let start_e = Arc::new(StartupEvent{});
    let start_ew = Arc::downgrade(&start_e);
    let io1 = Arc::new(gensio::new("stdio(self)", &o, start_ew.clone())
		       .expect("Unable to allocate stdio gensio"));
    let io2 = Arc::new(gensio::new(&telnetstr, &o, start_ew)
		       .expect("Unable to allocate telnet gensio"));
    let w = Arc::new(o.new_waiter().expect("Unable to allocate waiter 1"));
    let ev1 = Arc::new(ClientEvent { w: w.clone(), io: io1.clone(),
				     otherio: io2.clone() });
    let ev2 = Arc::new(ClientEvent { w: w.clone(), io: io2, otherio: io1 });
    let ev1w = Arc::downgrade(&ev1);
    ev1.io.set_handler(ev1w);
    let ev2w = Arc::downgrade(&ev2);
    ev2.io.set_handler(ev2w);

    ev1.io.open_s().expect("Open of stdio failed");
    ev2.io.open_s().expect("Open of telnet failed");
    ev1.io.read_enable(true);
    ev2.io.read_enable(true);

    _ = w.wait(1, None);
}
