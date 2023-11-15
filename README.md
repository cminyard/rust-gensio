# rust-gensio
Rust bindings to the gensio library, a library for general stream I/O

rust-gensio provides a fairly complete binding to the gensio library.

## What is gensio?

gensio provides consistent access to a number of different types of
stream (and packet) I/O, things like serial ports, TCP, UDP, stdio,
files, ptys, sound, and more.  It does this through a single
consistent API.

It works on Linux, BSD, MacOS, and Windows.

It also has filter gensios which can do processing on a stream and
stacked on top of other gensios.  Things like telnet, encryption,
authentication, and others.

See https://github.com/cminyard/gensio for details.  gensio also has a
fairly complete set of man pages.

## Things specific to rust-gensio

gensio works through a mostly event-driven interface.  You must
provide callbacks by implementing traits.

All callbacks and event handlers are stored as Weak values in the
library.  That means you must keep them around and not let them be
freed.  If you allow them to go away, the callbacks will stop working.
This is a little annoying, but not nearly as annoying as the circular
references that can be created by non-weak callbacks.