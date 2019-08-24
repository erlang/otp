SystemTap and Erlang/OTP
========================

Introduction
------------

SystemTap is DTrace for Linux. In fact Erlang's SystemTap support
is build using SystemTap's DTrace compatibility's layer. For an
introduction to Erlang DTrace support read [$ERL_TOP/HOWTO/DTRACE.md][].

Requisites
----------

*   Linux Kernel with UTRACE support
    
    check for UTRACE support in your current kernel:

        # grep CONFIG_UTRACE /boot/config-`uname -r`
        CONFIG_UTRACE=y

    Fedora 16 is known to contain UTRACE, for most other Linux distributions
    a custom build kernel will be required.
    Check Fedora's SystemTap documentation for additional required packages
    (e.g. Kernel Debug Symbols)

*   SystemTap > 1.6
  
    A the time of writing this, the latest released version of SystemTap is
    version 1.6. Erlang's DTrace support requires a MACRO that was introduced
    after that release. So either get a newer release or build SystemTap from
    git yourself (see: http://sourceware.org/systemtap/getinvolved.html)

Building Erlang
---------------

Configure and build Erlang with SystemTap support:

    # ./configure --with-dynamic-trace=systemtap + whatever args you need
    # make

Testing
-------

SystemTap, unlike DTrace, needs to know what binary it is tracing and has to
be able to read that binary before it starts tracing. Your probe script
therefor has to reference the correct beam emulator and stap needs to be able
to find that binary.
The examples are written for "beam", but other versions such as "beam.smp" or
"beam.debug.smp" might exist (depending on your configuration). Make sure you
either specify the full the path of the binary in the probe or your "beam"
binary is in the search path.

All available probes can be listed like this:

    # stap -L 'process("beam").mark("*")'

or:

    # PATH=/path/to/beam:$PATH stap -L 'process("beam").mark("*")'


Probes in the dtrace.so NIF library like this:

    # PATH=/path/to/dtrace/priv/lib:$PATH stap -L 'process("dtrace.so").mark("*")'

Running SystemTap scripts
-------------------------

Adjust the process("beam") reference to your beam version and attach the script
to a running "beam" instance:

    # stap /path/to/probe/script/port1.systemtap -x <pid of beam>


   [$ERL_TOP/HOWTO/DTRACE.md]: DTRACE.md
