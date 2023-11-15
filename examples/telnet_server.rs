//
// Copyright 2021 Corey Minyard
//
// SPDX-License-Identifier: Apache-2.0

// This is a basic telnet server, it accepts one connection and echos
// back everything it receives.

use std::env;
use std::sync::Mutex;
use std::sync::Arc;
use gensio;
use gensio::osfuncs;

struct TelnetReflectorInstList {
    list: Mutex<Vec<Arc<TelnetReflectorInst>>>,
}

struct TelnetReflectorInstData {
    baud: u32,
    datasize: u32,
    stopbits: u32,
    parity: u32,
    flowcontrol: u32,
    iflowcontrol: u32,
    sbreak: u32,
    rts: u32,
    dtr: u32,
    signature: Vec<u8>,
}
impl Default for TelnetReflectorInstData {
    fn default() -> TelnetReflectorInstData {
	TelnetReflectorInstData {
	    baud: 9600,
	    datasize: 8,
	    stopbits: 1,
	    parity: gensio::GENSIO_SER_PARITY_NONE,
	    flowcontrol: gensio::GENSIO_SER_FLOWCONTROL_NONE,
	    iflowcontrol: gensio::GENSIO_SER_FLOWCONTROL_NONE,
	    sbreak: gensio::GENSIO_SER_OFF,
	    dtr: gensio::GENSIO_SER_OFF,
	    rts: gensio::GENSIO_SER_OFF,
	    signature: "mysig".as_bytes().to_vec(),
	}
    }
}

struct TelnetReflectorInst {
    g: gensio::Gensio,
    list: Arc<TelnetReflectorInstList>,
    d: Mutex<TelnetReflectorInstData>,
}

impl TelnetReflectorInst {
    fn shutdown(&self) {
	let mut list = self.list.list.lock().unwrap();
	// Removing ourself from the list will drop the reference.
	list.retain(|x| std::ptr::eq(x.as_ref(), self));
    }
}

impl gensio::Event for TelnetReflectorInst {
    fn err(&self, err: i32) -> i32 {
	println!("Error writing to gensio: {}",
		 gensio::err_to_str(err));
	self.shutdown();
        0
    }

    fn read(&self, buf: &[u8], _auxdata: Option<&[&str]>)
            -> (i32, usize) {
	match self.g.write(buf, None) {
	    Ok(len) => {
		if (len as usize) < buf.len() {
		    self.g.read_enable(false);
		    self.g.write_enable(true);
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
        self.g.read_enable(true);
        self.g.write_enable(false);
        0
    }

    fn baud(&self, baud: u32) {
	let mut baud = baud;
	{
	    let mut d = self.d.lock().unwrap();
	    if baud != 0 {
		d.baud = baud;
	    } else {
		baud = d.baud;
	    }
	}
	let baud = baud.to_string();
	println!("Baud set to {}", baud);
	_ = self.g.acontrol_str(gensio::GENSIO_CONTROL_DEPTH_FIRST,
				gensio::GENSIO_CONTROL_SET,
				gensio::GENSIO_ACONTROL_SER_BAUD,
				&baud.to_string(), None, None);
    }

    fn datasize(&self, val: u32) {
	let mut val = val;
	{
	    let mut d = self.d.lock().unwrap();
	    if val != 0 {
		d.datasize = val;
	    } else {
		val = d.datasize;
	    }
	}
	println!("Datasize set to {}", val);
	_ = self.g.acontrol_str(gensio::GENSIO_CONTROL_DEPTH_FIRST,
				gensio::GENSIO_CONTROL_SET,
				gensio::GENSIO_ACONTROL_SER_DATASIZE,
				&val.to_string(), None, None);
    }

    fn stopbits(&self, val: u32) {
	let mut val = val;
	{
	    let mut d = self.d.lock().unwrap();
	    if val != 0 {
		d.stopbits = val;
	    } else {
		val = d.stopbits;
	    }
	}
	println!("Stopbits set to {}", val);
	_ = self.g.acontrol_str(gensio::GENSIO_CONTROL_DEPTH_FIRST,
				gensio::GENSIO_CONTROL_SET,
				gensio::GENSIO_ACONTROL_SER_STOPBITS,
				&val.to_string(), None, None);
    }

    fn parity(&self, val: u32) {
	let mut val = val;
	{
	    let mut d = self.d.lock().unwrap();
	    if val != 0 {
		d.parity = val;
	    } else {
		val = d.parity;
	    }
	}
	let val = gensio::parity_to_str(val);
	println!("Parity set to {}", val);
	_ = self.g.acontrol_str(gensio::GENSIO_CONTROL_DEPTH_FIRST,
				gensio::GENSIO_CONTROL_SET,
				gensio::GENSIO_ACONTROL_SER_PARITY,
				&val.to_string(), None, None);
    }

    fn flowcontrol(&self, val: u32) {
	let mut val = val;
	{
	    let mut d = self.d.lock().unwrap();
	    if val != 0 {
		d.flowcontrol = val;
	    } else {
		val = d.flowcontrol;
	    }
	}
	let val = gensio::flowcontrol_to_str(val);
	println!("Flowcontrol set to {}", val);
	_ = self.g.acontrol_str(gensio::GENSIO_CONTROL_DEPTH_FIRST,
				gensio::GENSIO_CONTROL_SET,
				gensio::GENSIO_ACONTROL_SER_FLOWCONTROL,
				&val.to_string(), None, None);
    }

    fn iflowcontrol(&self, val: u32) {
	let mut val = val;
	{
	    let mut d = self.d.lock().unwrap();
	    if val != 0 {
		d.iflowcontrol = val;
	    } else {
		val = d.iflowcontrol;
	    }
	}
	let val = gensio::flowcontrol_to_str(val);
	println!("IFlowcontrol set to {}", val);
	_ = self.g.acontrol_str(gensio::GENSIO_CONTROL_DEPTH_FIRST,
				gensio::GENSIO_CONTROL_SET,
				gensio::GENSIO_ACONTROL_SER_IFLOWCONTROL,
				&val.to_string(), None, None);
    }

    fn sbreak(&self, val: u32) {
	let mut val = val;
	{
	    let mut d = self.d.lock().unwrap();
	    if val != 0 {
		d.sbreak = val;
	    } else {
		val = d.sbreak;
	    }
	}
	let val = gensio::onoff_to_str(val);
	println!("Break set to {}", val);
	_ = self.g.acontrol_str(gensio::GENSIO_CONTROL_DEPTH_FIRST,
				gensio::GENSIO_CONTROL_SET,
				gensio::GENSIO_ACONTROL_SER_SBREAK,
				&val.to_string(), None, None);
    }

    fn dtr(&self, val: u32) {
	let mut val = val;
	{
	    let mut d = self.d.lock().unwrap();
	    if val != 0 {
		d.dtr = val;
	    } else {
		val = d.dtr;
	    }
	}
	let val = gensio::onoff_to_str(val);
	println!("DTR set to {}", val);
	_ = self.g.acontrol_str(gensio::GENSIO_CONTROL_DEPTH_FIRST,
				gensio::GENSIO_CONTROL_SET,
				gensio::GENSIO_ACONTROL_SER_DTR,
				&val.to_string(), None, None);
    }

    fn rts(&self, val: u32) {
	let mut val = val;
	{
	    let mut d = self.d.lock().unwrap();
	    if val != 0 {
		d.rts = val;
	    } else {
		val = d.rts;
	    }
	}
	let val = gensio::onoff_to_str(val);
	println!("RTS set to {}", val);
	_ = self.g.acontrol_str(gensio::GENSIO_CONTROL_DEPTH_FIRST,
				gensio::GENSIO_CONTROL_SET,
				gensio::GENSIO_ACONTROL_SER_RTS,
				&val.to_string(), None, None);
    }

    fn signature(&self, _val: &[u8]) {
	let d = self.d.lock().unwrap();
	// Signature value cannot be changed.
	println!("Signature request for {}",
		 String::from_utf8_lossy(&d.signature));
	_ = self.g.acontrol(gensio::GENSIO_CONTROL_DEPTH_FIRST,
			    gensio::GENSIO_CONTROL_SET,
			    gensio::GENSIO_ACONTROL_SER_SIGNATURE,
			    d.signature.as_slice(),
			    None, None);
    }
}

struct TelnetReflector {
    a: Arc<gensio::Accepter>,
    port: String,
    list: Arc<TelnetReflectorInstList>,
}

impl gensio::AccepterEvent for TelnetReflector {
    // No need for parmlog, InitialTelnetReflectorEv handled that.

    fn new_connection(&self, g: gensio::Gensio) -> i32 {
	let mut list = self.list.list.lock().unwrap();

	let d = TelnetReflectorInstData { ..Default::default() };
	let inst = TelnetReflectorInst { g: g,
					 list: self.list.clone(),
					 d: Mutex::new(d) };
	let inst = Arc::new(inst);
	inst.g.set_handler(inst.clone());
	inst.g.read_enable(true);
	list.push(inst);
	0
    }
}

// Used as the initial handler when creating a new accepter.  Will be
// replaced after the accepter is up.
struct InitialTelnetReflectorEv {
}

impl gensio::AccepterEvent for InitialTelnetReflectorEv {
    fn parmlog(&self, s: String) {
        println!("{}", &format!("Unexpected parmlog: {s}\n").to_string());
    }

    // Refuse connections until we are ready.
    fn new_connection(&self, _g: gensio::Gensio) -> i32 {
        gensio::GE_NOTSUP
    }
}

fn new_telnet_reflector(o: &osfuncs::OsFuncs, port: &str)
			-> Result<Arc<TelnetReflector>, i32> {
    let mut astr = "telnet(rfc2217),tcp,localhost,0".to_string();
    astr.push_str(port);
    let a = gensio::new_accepter(&astr,
				 o, Arc::new(InitialTelnetReflectorEv{}))?;
    a.startup()?;
    let port = a.control_str(gensio::GENSIO_CONTROL_DEPTH_FIRST,
			     gensio::GENSIO_CONTROL_GET,
			     gensio::GENSIO_ACC_CONTROL_LPORT, "")?;
    let list = TelnetReflectorInstList {  list: Mutex::new(Vec::new()) };
    let refl = TelnetReflector {
	a: Arc::new(a), port: port,
	list: Arc::new(list),
    };
    let refl = Arc::new(refl);
    refl.a.set_handler(refl.clone());
    Ok(refl)
}

struct LogHandler;

impl osfuncs::GensioLogHandler for LogHandler {
    fn log(&self, logstr: String) {
	println!("{}", &logstr);
    }
}

fn main() {
    let mut port = "0";
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
	port = &args[1];
    }

    let o = osfuncs::new(Arc::new(LogHandler))
	.expect("Couldn't allocate os funcs");
    o.proc_setup().expect("Couldn't setup thread");
    let r = new_telnet_reflector(&o, port).expect("Allocate reflector failed");

    println!("Telnet port is {}", r.port);

    loop {
	_ = o.service(None);
    }
}
