# rust-gensio
Rust bindings to the gensio library

This provides a fairly complete binding to the gensio library.

See the internal documentation for details.  Hopefully more will be
added here sometime soon.

Note: All callbacks and event handlers are stored as Weak values in
the library.  That means you must keep them around.  If you allow them
to go away, the callbacks will stop working.  This is a little
annoying, but not nearly as annoying as the circular references that
can be created by non-weak callbacks.