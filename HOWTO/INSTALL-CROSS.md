Cross Compiling Erlang/OTP
==========================

Introduction
------------

This document describes how to cross compile Erlang/OTP-%OTP-REL%. 
You are advised to read the whole document before attempting to cross
compile Erlang/OTP. However, before reading this document, you should read
the [$ERL_TOP/HOWTO/INSTALL.md][] document which describes building and installing
Erlang/OTP in general. `$ERL_TOP` is the top directory in the source tree.

### otp\_build Versus configure/make ###

Building Erlang/OTP can be done either by using the `$ERL_TOP/otp_build`
script, or by invoking `$ERL_TOP/configure` and `make` directly. Building using
`otp_build` is easier since it involves fewer steps, but the `otp_build` build
procedure is not as flexible as the `configure`/`make` build procedure. Note
that `otp_build configure` will produce a default configuration that differs
from what `configure` will produce by default. For example, currently
`--disable-dynamic-ssl-lib` is added to the `configure` command line arguments
unless `--enable-dynamic-ssl-lib` has been explicitly passed. The binary
releases that we deliver are built using `otp_build`.  The defaults used by
`otp_build configure` may change at any time without prior notice.

### Cross Configuration ###

The `$ERL_TOP/xcomp/erl-xcomp.conf.template` file contains all available cross
configuration variables and can be used as a template when creating a cross
compilation configuration. All [cross configuration variables][] are also
listed at the end of this document. For examples of working cross
configurations see the `$ERL_TOP/xcomp/erl-xcomp-TileraMDE2.0-tilepro.conf`
file and the `$ERL_TOP/xcomp/erl-xcomp-x86_64-saf-linux-gnu.conf` file. If the
default behavior of a variable is satisfactory, the variable does not need to
be set. However, the `configure` script will issue a warning when a default
value is used. When a variable has been set, no warning will be issued.

A cross configuration file can be passed to `otp_build configure` using the
`--xcomp-conf` command line argument. Note that `configure` does not accept
this command line argument. When using the `configure` script directly, pass
the configuration variables as arguments to `configure` using a
`<VARIABLE>=<VALUE>` syntax. Variables can also be passed as environment
variables to `configure`. However, if you pass the configuration in the
environment, make sure to unset all of these environment variables before
invoking `make`; otherwise, the environment variables might set make variables
in some applications, or parts of some applications, and you may end up with
an erroneously configured build. 

### What can be Cross Compiled? ###

All Erlang/OTP applications except the `wx` application can be cross compiled.
The build of the `wx` driver will currently be automatically disabled when
cross compiling.

### Compatibility ###

The build system, including cross compilation configuration variables used,
may be subject to non backward compatible changes without prior notice.
Current cross build system has been tested when cross compiling some Linux/GNU
systems, but has only been partly tested for more esoteric platforms. The
VxWorks example file is highly dependent on our environment and is here more
or less only for internal use.

### Patches ###

Please submit any patches for cross compiling in a way consistent with this
system. All input is welcome as we have a very limited set of cross compiling
environments to test with. If a new configuration variable is needed, add it
to `$ERL_TOP/xcomp/erl-xcomp.conf.template`, and use it in `configure.in`.
Other files that might need to be updated are:

- `$ERL_TOP/xcomp/erl-xcomp-vars.sh`
- `$ERL_TOP/erl-build-tool-vars.sh`
- `$ERL_TOP/erts/aclocal.m4`
- `$ERL_TOP/xcomp/README.md`
- `$ERL_TOP/xcomp/erl-xcomp-*.conf`

Note that this might be an incomplete list of files that need to be updated.

General information on how to submit patches can be found at:
  <http://wiki.github.com/erlang/otp/submitting-patches>

Build and Install Procedure
---------------------------

If you are building in Git, you want to read the [Building in Git][] section
of [$ERL_TOP/HOWTO/INSTALL.md][] before proceeding.

We will first go through the `configure`/`make` build procedure which people
probably are most familiar with.

### Building With configure/make Directly ###

  (1)

Change directory into the top directory of the Erlang/OTP source tree.

    $ cd $ERL_TOP

In order to compile Erlang code, a small Erlang bootstrap system has to be
built, or an Erlang/OTP system of the same release as the one being built
has to be provided in the `$PATH`. The Erlang/OTP for the target system will
be built using this Erlang system, together with the cross compilation tools
provided.

If you want to build using a compatible Erlang/OTP system in the `$PATH`,
jump to (3).

#### Building a Bootstrap System ####

  (2)

    $ ./configure --enable-bootstrap-only
    $ make

The `--enable-bootstrap-only` argument to `configure` isn't strictly necessary,
but will speed things up. It will only run `configure` in applications
necessary for the bootstrap, and will disable a lot of things not needed by
the bootstrap system. If you run `configure` without `--enable-boostrap-only`
you also have to run make as `make bootstrap`; otherwise, the whole system will
be built.

#### Cross Building the System ####

  (3)

    $ ./configure --host=<HOST> --build=<BUILD> [Other Config Args]
    $ make

`<HOST>` is the host/target system that you build for. It does not have to be
a full `CPU-VENDOR-OS` triplet, but can be. The full `CPU-VENDOR-OS` triplet
will be created by executing `$ERL_TOP/erts/autoconf/config.sub <HOST>`. If
`config.sub` fails, you need to be more specific.

`<BUILD>` should equal the `CPU-VENDOR-OS` triplet of the system that you
build on. If you execute `$ERL_TOP/erts/autoconf/config.guess`, it will in
most cases print the triplet you want to use for this.

Pass the cross compilation variables as command line arguments to `configure`
using a `<VARIABLE>=<VALUE>` syntax.

> *NOTE*: You can *not* pass a configuration file using the `--xcomp-conf`
> argument when you invoke `configure` directly. The `--xcomp-conf` argument
> can only be passed to `otp_build configure`.

`make` will verify that the Erlang/OTP system used when building is of the
same release as the system being built, and will fail if this is not the case.
It is possible, however not recommended, to force the cross compilation even
though the wrong Erlang/OTP system is used. This by invoking `make` like this:
`make ERL_XCOMP_FORCE_DIFFERENT_OTP=yes`.

> *WARNING*: Invoking `make ERL_XCOMP_FORCE_DIFFERENT_OTP=yes` might fail,
> silently produce suboptimal code, or silently produce erroneous code.

#### Installing ####

You can either install using the installation paths determined by `configure`
(4), or install manually using (5).

##### Installing Using Paths Determined by configure #####

  (4)

    $ make install DESTDIR=<TEMPORARY_PREFIX>

`make install` will install at a location specified when doing `configure`.
`configure` arguments specifying where the installation should reside are for
example: `--prefix`, `--exec-prefix`, `--libdir`, `--bindir`, etc. By default
it will install under `/usr/local`. You typically do not want to install your
cross build under `/usr/local` on your build machine. Using [DESTDIR][]
will cause the installation paths to be prefixed by `$DESTDIR`. This makes it
possible to install and package the installation on the build machine without
having to place the installation in the same directory on the build machine as
it should be executed from on the target machine.

When `make install` has finished, change directory into `$DESTDIR`, package
the system, move it to the target machine, and unpack it. Note that the
installation will only be working on the target machine at the location
determined by `configure`.

##### Installing Manually #####

  (5)

    $ make release RELEASE_ROOT=<RELEASE_DIR>

`make release` will copy what you have built for the target machine to
`<RELEASE_DIR>`. The `Install` script will not be run. The content of
`<RELEASE_DIR>` is what by default ends up in `/usr/local/lib/erlang`.

The `Install` script used when installing Erlang/OTP requires common Unix
tools such as `sed` to be present in your `$PATH`. If your target system
does not have such tools, you need to run the `Install` script on your
build machine before packaging Erlang/OTP. The `Install` script should
currently be invoked as follows in the directory where it resides
(the top directory):

    $ ./Install [-cross] [-minimal|-sasl] <ERL_ROOT>

where:

*   `-minimal` Creates an installation that starts up a minimal amount
    of applications, i.e., only `kernel` and `stdlib` are started. The
    minimal system is normally enough, and is what `make install` uses.
*   `-sasl` Creates an installation that also starts up the `sasl`
    application.
*   `-cross` For cross compilation. Informs the install script that it
    is run on the build machine.
*   `<ERL_ROOT>` - The absolute path to the Erlang installation to use
    at run time. This is often the same as the current working directory,
    but does not have to be. It can follow any other path through the file
    system to the same directory.

If neither `-minimal`, nor `-sasl` is passed as argument you will be
prompted.

You can now either do:

  (6)

*   Decide where the installation should be located on the target machine,
    run the `Install` script on the build machine, and package the installed
    installation. The installation just need to be unpacked at the right
    location on the target machine:

        $ cd <RELEASE_DIR>
        $ ./Install -cross [-minimal|-sasl] <ABSOLUTE_INSTALL_DIR_ON_TARGET>

or:

  (7)

*   Package the installation in `<RELEASE_DIR>`, place it wherever you want
    on your target machine, and run the `Install` script on your target
    machine:

        $ cd <ABSOLUTE_INSTALL_DIR_ON_TARGET>
        $ ./Install [-minimal|-sasl] <ABSOLUTE_INSTALL_DIR_ON_TARGET>

### Building With the otp\_build Script ###

  (8)

    $ cd $ERL_TOP

  (9)

    $ ./otp_build configure --xcomp-conf=<FILE> [Other Config Args]

alternatively:

    $ ./otp_build configure --host=<HOST> --build=<BUILD> [Other Config Args]

If you have your cross compilation configuration in a file, pass it using the
`--xcomp-conf=<FILE>` command line argument. If not, pass `--host=<HOST>`,
`--build=<BUILD>`, and the configuration variables using a `<VARIABLE>=<VALUE>`
syntax on the command line  (same as in (3)). Note that `<HOST>` and `<BUILD>`
have to be passed one way or the other; either by using `erl_xcomp_host=<HOST>`
and `erl_xcomp_build=<BUILD>` in the configuration file, or by using the
`--host=<HOST>`, and `--build=<BUILD>` command line arguments.

`otp_build configure` will configure both for the boostrap system on the
build machine and the cross host system.

  (10)

    $ ./otp_build boot -a

`otp_build boot -a` will first build a bootstrap system for the build machine
and then do the cross build of the system.

  (11)

    $ ./otp_build release -a <RELEASE_DIR>

`otp_build release -a` will do the same as (5), and you will after this have
to do a manual install either by doing (6), or (7).

Building and Installing the Documentation
-----------------------------------------

After the system has been cross built you can build and install the
documentation the same way as after a native build of the system. See the
[How to Build the Documentation][] section in the [$ERL_TOP/HOWTO/INSTALL.md][]
document for information on how to build the documentation.

Testing the cross compiled system
---------------------------------

Some of the tests that come with erlang use native code to test. This means
that when cross compiling erlang you also have to cross compile test suites
in order to run tests on the target host. To do this you first have to release
the tests as usual.

    $ make release_tests

or

    $ ./otp_build tests

The tests will be released into `$ERL_TOP/release/tests`. After releasing the
tests you have to install the tests on the build machine. You supply the same
xcomp file as to `./otp_build` in (9).

    $ cd $ERL_TOP/release/tests/test_server/
    $ $ERL_TOP/bootstrap/bin/erl -eval 'ts:install([{xcomp,"<FILE>"}])' -s ts compile_testcases -s init stop

You should get a lot of printouts as the testcases are compiled. Once done you
should copy the entire `$ERL_TOP/release/tests` folder to the cross host system.

Then go to the cross host system and setup the erlang installed in (4) or (5)
to be in your `$PATH`. Then go to what previously was
`$ERL_TOP/release/tests/test_server` and issue the following command.

    $ erl -s ts install -s ts run all_tests -s init stop

The configure should be skipped and all tests should hopefully pass. For more
details about how to use ts run `erl -s ts help -s init stop`

Currently Used Configuration Variables
--------------------------------------

Note that you cannot define arbitrary variables in a cross compilation
configuration file. Only the ones listed below will be guaranteed to be
visible throughout the whole execution of all `configure` scripts. Other
variables needs to be defined as arguments to `configure` or exported in
the environment.

### Variables for otp\_build Only ###

Variables in this section are only used, when configuring Erlang/OTP for
cross compilation using `$ERL_TOP/otp_build configure`.

> *NOTE*: These variables currently have *no* effect if you configure using
> the `configure` script directly.

*   `erl_xcomp_build` - The build system used. This value will be passed as
    `--build=$erl_xcomp_build` argument to the `configure` script. It does
    not have to be a full `CPU-VENDOR-OS` triplet, but can be. The full
    `CPU-VENDOR-OS` triplet will be created by
    `$ERL_TOP/erts/autoconf/config.sub $erl_xcomp_build`. If set to `guess`,
    the build system will be guessed using
    `$ERL_TOP/erts/autoconf/config.guess`.

*   `erl_xcomp_host` - Cross host/target system to build for. This value will
    be passed as `--host=$erl_xcomp_host` argument to the `configure` script.
    It does not have to be a full `CPU-VENDOR-OS` triplet, but can be. The
    full `CPU-VENDOR-OS` triplet will be created by
    `$ERL_TOP/erts/autoconf/config.sub $erl_xcomp_host`.

*   `erl_xcomp_configure_flags` - Extra configure flags to pass to the
    `configure` script.

### Cross Compiler and Other Tools ###

If the cross compilation tools are prefixed by `<HOST>-` you probably do
not need to set these variables (where `<HOST>` is what has been passed as
`--host=<HOST>` argument to `configure`).

All variables in this section can also be used when native compiling.

*   `CC` - C compiler.

*   `CFLAGS` - C compiler flags.

*   `STATIC_CFLAGS` - Static C compiler flags.

*   `CFLAG_RUNTIME_LIBRARY_PATH` - This flag should set runtime library
    search path for the shared libraries. Note that this actually is a
    linker flag, but it needs to be passed via the compiler.

*   `CPP` - C pre-processor.

*   `CPPFLAGS` - C pre-processor flags.

*   `CXX` - C++ compiler.

*   `CXXFLAGS` - C++ compiler flags.

*   `LD` - Linker.

*   `LDFLAGS` - Linker flags.

*   `LIBS` - Libraries.

#### Dynamic Erlang Driver Linking ####

> *NOTE*: Either set all or none of the `DED_LD*` variables.

*   `DED_LD` - Linker for Dynamically loaded Erlang Drivers.

*   `DED_LDFLAGS` - Linker flags to use with `DED_LD`.

*   `DED_LD_FLAG_RUNTIME_LIBRARY_PATH` - This flag should set runtime library
    search path for shared libraries when linking with `DED_LD`.

#### Large File Support ####

> *NOTE*: Either set all or none of the `LFS_*` variables.

*   `LFS_CFLAGS` - Large file support C compiler flags.

*   `LFS_LDFLAGS` - Large file support linker flags.

*   `LFS_LIBS` - Large file support libraries.

#### Other Tools ####

*   `RANLIB` - `ranlib` archive index tool.

*   `AR` - `ar` archiving tool.

*   `GETCONF` - `getconf` system configuration inspection tool. `getconf` is
    currently used for finding out large file support flags to use, and
    on Linux systems for finding out if we have an NPTL thread library or
    not.

### Cross System Root Locations ###

*   `erl_xcomp_sysroot` - The absolute path to the system root of the cross
    compilation environment. Currently, the `crypto`, `odbc`, `ssh` and
    `ssl` applications need the system root. These applications will be
    skipped if the system root has not been set. The system root might be
    needed for other things too. If this is the case and the system root
    has not been set, `configure` will fail and request you to set it.

*   `erl_xcomp_isysroot` - The absolute path to the system root for includes
    of the cross compilation environment. If not set, this value defaults
    to `$erl_xcomp_sysroot`, i.e., only set this value if the include system
    root path is not the same as the system root path.

### Optional Feature, and Bug Tests ###

These tests cannot (always) be done automatically when cross compiling. You
usually do not need to set these variables.

> *WARNING*: Setting these variables wrong may cause hard to detect
> runtime errors. If you need to change these values, *really* make sure
> that the values are correct.

> *NOTE*: Some of these values will override results of tests performed
> by `configure`, and some will not be used until `configure` is sure that
> it cannot figure the result out.

The `configure` script will issue a warning when a default value is used.
When a variable has been set, no warning will be issued.

*   `erl_xcomp_after_morecore_hook` - `yes|no`. Defaults to `no`. If `yes`,
    the target system must have a working `__after_morecore_hook` that can be
    used for tracking used `malloc()` implementations core memory usage.
    This is currently only used by unsupported features.

*   `erl_xcomp_bigendian` - `yes|no`. No default. If `yes`, the target system
    must be big endian. If `no`, little endian. This can often be
    automatically detected, but not always. If not automatically detected,
    `configure` will fail unless this variable is set. Since no default
    value is used, `configure` will try to figure this out automatically.
	
*   `erl_xcomp_double_middle` - `yes|no`. Defaults to `no`. 
	If `yes`, the target system must have doubles in "middle-endian" format. If
    `no`, it has "regular" endianness. 	

*   `erl_xcomp_clock_gettime_cpu_time` - `yes|no`. Defaults to `no`. If `yes`,
    the target system must have a working `clock_gettime()` implementation
    that can be used for retrieving process CPU time.

*   `erl_xcomp_getaddrinfo` - `yes|no`. Defaults to `no`. If `yes`, the target
    system must have a working `getaddrinfo()` implementation that can
    handle both IPv4 and IPv6.

*   `erl_xcomp_gethrvtime_procfs_ioctl` - `yes|no`. Defaults to `no`. If `yes`,
    the target system must have a working `gethrvtime()` implementation and
    is used with procfs `ioctl()`.

*   `erl_xcomp_dlsym_brk_wrappers` - `yes|no`. Defaults to `no`. If `yes`, the
    target system must have a working `dlsym(RTLD_NEXT, <S>)` implementation
    that can be used on `brk` and `sbrk` symbols used by the `malloc()`
    implementation in use, and by this track the `malloc()` implementations
    core memory usage. This is currently only used by unsupported features.

*   `erl_xcomp_kqueue` - `yes|no`. Defaults to `no`. If `yes`, the target
    system must have a working `kqueue()` implementation that returns a file
    descriptor which can be used by `poll()` and/or `select()`. If `no` and
    the target system has not got `epoll()` or `/dev/poll`, the kernel-poll
    feature will be disabled.

*   `erl_xcomp_linux_clock_gettime_correction` - `yes|no`. Defaults to `yes` on
    Linux; otherwise, `no`. If `yes`, `clock_gettime(CLOCK_MONOTONIC, _)` on
    the target system must work. This variable is recommended to be set to
    `no` on Linux systems with kernel versions less than 2.6.

*   `erl_xcomp_linux_nptl` - `yes|no`. Defaults to `yes` on Linux; otherwise,
    `no`. If `yes`, the target system must have NPTL (Native POSIX Thread
    Library). Older Linux systems have LinuxThreads instead of NPTL (Linux
    kernel versions typically less than 2.6).

*   `erl_xcomp_linux_usable_sigaltstack` - `yes|no`. Defaults to `yes` on Linux;
    otherwise, `no`. If `yes`, `sigaltstack()` must be usable on the target
    system. `sigaltstack()` on Linux kernel versions less than 2.4 are
    broken.

*   `erl_xcomp_linux_usable_sigusrx` - `yes|no`. Defaults to `yes`. If `yes`,
    the `SIGUSR1` and `SIGUSR2` signals must be usable by the ERTS. Old
    LinuxThreads thread libraries (Linux kernel versions typically less than
    2.2) used these signals and made them unusable by the ERTS.

*   `erl_xcomp_poll` - `yes|no`. Defaults to `no` on Darwin/MacOSX; otherwise,
    `yes`. If `yes`, the target system must have a working `poll()`
    implementation that also can handle devices. If `no`, `select()` will be
    used instead of `poll()`.

*   `erl_xcomp_putenv_copy` - `yes|no`. Defaults to `no`. If `yes`, the target
    system must have a `putenv()` implementation that stores a copy of the
    key/value pair.

*   `erl_xcomp_reliable_fpe` - `yes|no`. Defaults to `no`. If `yes`, the target
    system must have reliable floating point exceptions.

*   `erl_xcomp_posix_memalign` - `yes|no`. Defaults to `yes` if `posix_memalign`
    system call exists; otherwise `no`. If `yes`, the target system must have a
    `posix_memalign` implementation that accepts larger than page size
    alignment.

*   `erl_xcomp_code_model_small` - `yes|no`. Default to `no`. If `yes`, the target
    system must place the beam.smp executable in the lower 2 GB of memory. That is it
    should not use position independent executable.


   [$ERL_TOP/HOWTO/INSTALL.md]: INSTALL.md
   [Building in Git]: INSTALL.md#How-to-Build-and-Install-ErlangOTP
   [How to Build the Documentation]: INSTALL.md#How-to-Build-and-Install-ErlangOTP_How-to-Build-the-Documentation
   [cross configuration variables]: #Currently-Used-Configuration-Variables
   [DESTDIR]: http://www.gnu.org/prep/standards/html_node/DESTDIR.html
   [?TOC]: true
