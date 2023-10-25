// Copyright 2023 Corey Minyard
//
// SPDX-License-Identifier: Apache-2.0

fn main() {
    cc::Build::new()
	.file("src/oshelpers.c")
	.compile("gensiooshelpers");
    cc::Build::new()
	.file("src/ghelpers.c")
	.compile("gensiohelpers");
}
