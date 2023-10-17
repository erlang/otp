### Vanilla Driver

The old previously documented support for opening a port to an external resource
by passing an atom (or a string) as first argument to
[`open_port()`](`erlang:open_port/2`), implemented by the vanilla driver, will
be removed in OTP 27. This functionality was marked as obsolete about two
decades ago and then a few years later the documentation for it was removed. If
this functionality is not used with care it might hang or crash the runtime
system.
