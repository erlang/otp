DTrace and Erlang/OTP
=====================

History
-------

The first implementation of DTrace probes for the Erlang virtual
machine was presented at the [2008 Erlang User Conference] [1].  That
work, based on the Erlang/OTP R12 release, was discontinued due to
what appears to be miscommunication with the original developers.

Several users have created Erlang port drivers, linked-in drivers, or
NIFs that allow Erlang code to try to activate a probe,
e.g. `foo_module:dtrace_probe("message goes here!")`.

Goals
-----

* Annotate as much of the Erlang VM as is practical.
   * The initial goal is to trace file I/O operations.
* Support all platforms that implement DTrace: OS X, Solaris,
  and (I hope) FreeBSD and NetBSD.
* To the extent that it's practical, support SystemTap on Linux
  via DTrace provider compatibility.
* Allow Erlang code to supply annotations.

Supported platforms
-------------------

* OS X 10.6.x / Snow Leopard, OS X 10.7.x / Lion and probably newer versions.
* Solaris 10.  I have done limited testing on Solaris 11 and
  OpenIndiana release 151a, and both appear to work.
* FreeBSD 9.0 and 10.0.
* Linux via SystemTap compatibility.  Please see
  [$ERL_TOP/HOWTO/SYSTEMTAP.md][] for more details.

Just add the `--with-dynamic-trace=dtrace` option to your command when you 
run the `configure` script. If you are using systemtap, the configure option 
is `--with-dynamic-trace=systemtap`

Status
------

As of R15B01, the dynamic trace code is included in the OTP source distribution,
although it's considered experimental. The main development of the dtrace code 
still happens outside of Ericsson, but there is no need to fetch a patched 
version of the OTP source to get the basic functionality.

DTrace probe specifications
---------------------------

Probe specifications can be found in `erts/emulator/beam/erlang_dtrace.d`, and
a few example scripts can be found under `lib/runtime_tools/examples/`.

   [1]: http://www.erlang.org/euc/08/
   [$ERL_TOP/HOWTO/SYSTEMTAP.md]: SYSTEMTAP.md
