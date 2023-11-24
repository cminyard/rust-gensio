// Copyright 2023 Corey Minyard
//
// SPDX-License-Identifier: Apache-2.0
//

use std::path::Path;
use std::env;

fn main() {
    let mut extra_includes = None;
    let target_os = env::var("CARGO_CFG_TARGET_OS");
    if target_os.is_ok() && target_os.unwrap() == "windows" {
        extra_includes = Some(Path::new("C:/Program Files (x86)/Gensio/include"));
        println!("cargo:rustc-link-search=C:/Program Files (x86)/Gensio/lib");
    }

    cc::Build::new()
	.file("src/oshelpers.c")
	.includes(extra_includes)
	.compile("gensiooshelpers");
    cc::Build::new()
	.file("src/ghelpers.c")
	.includes(extra_includes)
	.compile("gensiohelpers");
}
