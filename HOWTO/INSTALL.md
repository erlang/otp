Building and Installing Erlang/OTP
==================================

Introduction
------------

This document describes how to build and install Erlang/OTP-%OTP-REL%. You
are advised to read the whole document before attempting to build and install
Erlang/OTP. You can find more information about Open Source Erlang/OTP at:

   <http://www.erlang.org/>

The source code for Erlang/OTP can also be found in a Git repository:

   <http://github.com/erlang/otp>

Erlang/OTP should be possible to build from source on any Unix system,
including Mac OS X. This document describes how to native compile Erlang/OTP
on Unix. For detailed instructions on how to

*   cross compile Erlang/OTP, see the [$ERL_TOP/HOWTO/INSTALL-CROSS.md][]
    document.

*   build Erlang/OTP on Windows, see the [$ERL_TOP/HOWTO/INSTALL-WIN32.md][]
    document.

    Binary releases for Windows can be found at
    <http://www.erlang.org/download.html>.

Before reading the above mentioned documents you are in any case advised to
read this document first, since it covers building Erlang/OTP in general as
well as other important information.

Daily Build and Test
--------------------
At Ericsson we have a "Daily Build and Test" that runs on:

*   Solaris 8, 9
    *   Sparc32
    *   Sparc64
*   Solaris 10
    *   Sparc32
    *   Sparc64
    *   x86
*   SuSE Linux/GNU 9.4, 10.1
    *   x86
*   SuSE Linux/GNU 10.0, 10.1, 11.0
    *   x86
    *   x86\_64
*   openSuSE 11.4 (Celadon)
    *   x86\_64 (valgrind)
*   Fedora 7
    *   PowerPC
*   Fedora 14
    * x86\_64
*   Gentoo Linux/GNU 1.12.11.1
    *   x86
*   Ubuntu Linux/GNU 7.04, 10.04, 10.10, 11.0
    *   x86\_64
*   MontaVista Linux/GNU 4.0.1
    *   PowerPC
*   FreeBSD 8.2
    *   x86
*   OpenBSD 5.0
    *   x86\_64
*   Mac OS X 10.5.8 (Leopard), 10.7.3 (Lion), 10.9 (Mavericks)
    *   x86
*   Windows XP SP3, 2003, Vista, 7
    *   x86
*   Windows 7
    *   x86\_64

We also have the following "Daily Cross Builds":

*   SuSE Linux/GNU 10.1 x86 -> SuSE Linux/GNU 10.1 x86\_64
*   SuSE Linux/GNU 10.1 x86\_64 -> Linux/GNU TILEPro64

and the following "Daily Cross Build Tests":

*   SuSE Linux/GNU 10.1 x86\_64

Versions Known NOT to Work
--------------------------

*   Suse linux 9.1 is shipped with a patched GCC version 3.3.3, having the
    rpm named `gcc-3.3.3-41`. That version has a serious optimization bug
    that makes it unusable for building the Erlang emulator. Please
    upgrade GCC to a newer version before building on Suse 9.1. Suse Linux
    Enterprise edition 9 (SLES9) has `gcc-3.3.3-43` and is not affected.

*   `gcc-4.3.0` has a serious optimizer bug. It produces an Erlang emulator
    that will crash immediately. The bug is supposed to be fixed in
    `gcc-4.3.1`.

*   FreeBSD had a bug which caused `kqueue`/`poll`/`select` to fail to detect
    that a `writev()` on a pipe has been made. This bug should have been fixed
    in FreeBSD 6.3 and FreeBSD 7.0. NetBSD and DragonFlyBSD probably have or
    have had the same bug. More information can be found at:

    *   <http://www.freebsd.org/cgi/cvsweb.cgi/src/sys/kern/sys_pipe.c>
    *   <http://lists.freebsd.org/pipermail/freebsd-arch/2007-September/006790.html>

*   `getcwd()` on Solaris 9 can cause an emulator crash. If you have
    async-threads enabled you can increase the stack size of the
    async-threads as a temporary workaround. See the `+a` command-line
    argument in the documentation of `erl(1)`. Without async-threads the
    emulator is not as vulnerable to this bug, but if you hit it without
    async-threads the only workaround available is to enable async-threads
    and increase the stack size of the async-threads. Sun has however
    released patches that fixes the issue:

    > Problem Description: 6448300 large mnttab can cause stack overrun
    > during Solaris 9 getcwd

    More information can be found at:

    *   <http://sunsolve.sun.com/search/document.do?assetkey=1-21-112874-40-1&searchclause=6448300>
    *   <http://sunsolve.sun.com/search/document.do?assetkey=1-21-114432-29-1&searchclause=6448300>

Required Utilities
------------------

These are the tools you will need in order to unpack and build Erlang/OTP.

### Unpacking ###

*   GNU unzip, or a modern uncompress.
*   A TAR program that understands the GNU TAR format for long filenames
    (such as GNU TAR).

### Building ###

*   GNU `make`
*   `gcc` -- GNU C compiler
*   Perl 5
*   GNU `m4` -- If HiPE (native code) support is enabled. HiPE can be
    disabled using `--disable-hipe`
*   `ncurses`, `termcap`, or `termlib` -- The development headers and
    libraries are needed, often known as `ncurses-devel`. Use
    `--without-termcap` to build without any of these libraries. Note that
    in this case only the old shell (without any line editing) can be used.
*   OpenSSL -- Optional, but needed for building the Erlang/OTP applications
    `ssl` and `crypto`. You need the "development package" of OpenSSL, i.e.
    including the header files. For building the application `ssl` the OpenSSL
    binary command program `openssl` is also needed. At least version 0.9.8
    of OpenSSL is required. Can be downloaded from <http://www.openssl.org>.
*   Sun Java jdk-1.5.0 or higher -- Optional but needed for building the
    Erlang/OTP application `jinterface` and parts of `ic` and `orber`. Can
    be downloaded from <http://java.sun.com>. We have also tested IBM's
    JDK 1.5.0.
*   X Windows -- Optional, but development headers and libraries are needed
    to build the Erlang/OTP application `gs` on Unix/Linux.
*  `sed` -- There seem to be some problems with some of the `sed` version on
    Solaris. Make sure `/bin/sed` or `/usr/bin/sed` is used on the Solaris
    platform.
*   `flex` -- Optional, headers and libraries are needed to build the `flex`
    scanner for the `megaco` application on Unix/Linux.

#### Building Documentation ####

*   `xsltproc` -- XSLT processor. A tool for applying XSLT stylesheets
    to XML documents. Can be downloaded from
    <http://xmlsoft.org/XSLT/xsltproc2.html>.
*   `fop` -- Apache FOP print formatter (requires Java). Can be downloaded
    from <http://xmlgraphics.apache.org/fop>.

#### Building in Git ####

*   GNU `autoconf` of at least version 2.59. Note that `autoconf` is not
    needed when building an unmodified version of the released source.

### Installing ###

*   An `install` program that can take multiple file names.

How to Build and Install Erlang/OTP
-----------------------------------

The following instructions are for building [the released source tar ball][].

The variable `$ERL_TOP` will be mentioned a lot of times. It refers to
the top directory in the source tree. More information about `$ERL_TOP`
can be found in the [make and $ERL_TOP][] section below. If you are
building in git you probably want to take a look at the [Building in Git][]
section below before proceeding.

### Unpacking ###

Step 1: Start by unpacking the Erlang/OTP distribution file with your GNU
compatible TAR program.

    $ gunzip -c otp_src_%OTP-REL%.tar.gz | tar xf -

alternatively:

    $ zcat otp_src_%OTP-REL%.tar.gz | tar xf -


Step 2: Now cd into the base directory (`$ERL_TOP`).

    $ cd otp_src_%OTP-REL%

### Configuring ###

Step 3: On some platforms Perl may behave strangely if certain locales are
set, so optionally you may need to set the LANG variable:

    # Bourne shell
    $ LANG=C; export LANG

or

    # C-Shell
    $ setenv LANG C

Step 4: Run the following commands to configure the build:

    $ ./configure  [ options ]

If you are building it from git you will need to run `autoconf` to generate configure file.
By default, Erlang/OTP will be installed in `/usr/local/{bin,lib/erlang}`.
To instead install in `<BaseDir>/{bin,lib/erlang}`, use the
`--prefix=<BaseDir>` option.

If you upgraded the source with some patch you may need to clean up
from previous builds before the new build. Before doing a `make clean`,
be sure to read the [Pre-built Source Release][] section below.

### Building ###

Step 5: Build the Erlang/OTP package.

    $ make

### Installing ###

Step 6: Install then Erlang/OTP package

    $ make install

### A Closer Look at the individual Steps ###

Let us go through them in some detail.

#### Configuring ####

Step 4 runs a configuration script created by the GNU autoconf utility, which
checks for system specific features and then creates a number of makefiles.

The configure script allows you to customize a number of parameters;
type `./configure --help` or `./configure --help=recursive` for details.
`./configure --help=recursive` will give help for all `configure` scripts in
all applications.

One of the things you can specify is where Erlang/OTP should be installed. By
default Erlang/OTP will be installed in `/usr/local/{bin,lib/erlang}`.
To keep the same structure but install in a different place, `<Dir>` say,
use the `--prefix` argument like this: `./configure --prefix=<Dir>`.

Some of the available `configure` options are:

*   `--prefix=PATH` - Specify installation prefix.
*   `--{enable,disable}-threads` - Thread support (enabled by default if
    possible)
*   `--{enable,disable}-smp-support` - SMP support (enabled by default if
    possible)
*   `--{enable,disable}-kernel-poll` - Kernel poll support (enabled by
    default if possible)
*   `--{enable,disable}-hipe` - HiPE support (enabled by default on supported
    platforms)
*   `--enable-darwin-universal` - Build universal binaries on darwin i386.
*   `--enable-darwin-64bit` - Build 64-bit binaries on darwin
*   `--enable-m64-build` - Build 64-bit binaries using the `-m64` flag to
    `(g)cc`
*   `--enable-m32-build` - Build 32-bit binaries using the `-m32` flag to
    `(g)cc`
*   `--{with,without}-termcap` - termcap (without implies that only the old
    Erlang shell can be used)
*   `--with-javac=JAVAC` - Specify Java compiler to use
*   `--{with,without}-javac` - Java compiler (without implies that the
    `jinterface` application won't be built)
*   `--{enable,disable}-dynamic-ssl-lib` - Dynamic OpenSSL libraries
*   `--{enable,disable}-shared-zlib` - Shared zlib library
*   `--with-ssl=PATH` - Specify location of OpenSSL include and lib
*   `--{with,without}-ssl` - OpenSSL (without implies that the `crypto`,
    `ssh`, and `ssl` won't be built)
*   `--with-libatomic_ops=PATH` - Use the `libatomic_ops` library for atomic
    memory accesses. If `configure` should inform you about no native atomic
    implementation available, you typically want to try using the
    `libatomic_ops` library. It can be downloaded from
    <http://www.hpl.hp.com/research/linux/atomic_ops/>.
*   `--disable-smp-require-native-atomics` - By default `configure` will
    fail if an SMP runtime system is about to be built, and no implementation
    for native atomic memory accesses can be found. If this happens, you are
    encouraged to find a native atomic implementation that can be used, e.g.,
    using `libatomic_ops`, but by passing `--disable-smp-require-native-atomics`
    you can build using a fallback implementation based on mutexes or spinlocks.
    Performance of the SMP runtime system will however suffer immensely without
    an implementation for native atomic memory accesses.
*   `--without-$app` - By default all applications in Erlang/OTP will be included
	in a release. If this is not wanted it is possible to specify that Erlang/OTP
	should be compiled without that applications, i.e. `--without-wx`. There is
	no automatic dependency handling inbetween applications. So if you disable
	an application that another depends on, you also have to disable the
	dependant application.

If you or your system has special requirements please read the `Makefile` for
additional configuration information.

#### Building ####

Step 5 builds the Erlang/OTP system. On a fast computer, this will take about
5 minutes. After completion of this step, you should have a working
Erlang/OTP system which you can try by typing `bin/erl`. This should start
up Erlang/OTP and give you a prompt:

    $ bin/erl
    Erlang %OTP-REL% (erts-%ERTS-VSN%) [source] [smp:4:4] [rq:4] [async-threads:0] [kernel-poll:false]

    Eshell V%ERTS-VSN%  (abort with ^G)
    1> _

#### Installing ####

Step 6 is optional. It installs Erlang/OTP at a standardized location (if you
change your mind about where you wish to install you can rerun step 4,
without having to do step 5 again).

##### Alternative Installation Procedures #####

*   Staged install using [DESTDIR][]. You can perform the install
    phase in a temporary directory and later move the installation into
    its correct location by use of the `DESTDIR` variable:

        $ make DESTDIR=<tmp install dir> install

    The installation will be created in a location prefixed by `$DESTDIR`.
    It can, however, not be run from there. It needs to be moved into the
    correct location before it can be run. If `DESTDIR` have not been set
    but `INSTALL_PREFIX` has been set, `DESTDIR` will be set to
    `INSTALL_PREFIX`. Note that `INSTALL_PREFIX` in pre R13B04 was buggy
    and behaved as `EXTRA_PREFIX` (see below). There are lots of areas of
    use for an installation procedure using `DESTDIR`, e.g. when creating
    a package, cross compiling, etc. Here is an example where the
    installation should be located under `/opt/local`:

        $ ./configure --prefix=/opt/local
        $ make
        $ make DESTDIR=/tmp/erlang-build install
        $ cd /tmp/erlang-build/opt/local
        $     # gnu-tar is used in this example
        $ tar -zcf /home/me/my-erlang-build.tgz *
        $ su -
        Password: *****
        $ cd /opt/local
        $ tar -zxf /home/me/my-erlang-build.tgz

*   Install using the `release` target. Instead of doing `make install` you
    can create the installation in whatever directory you like using the
    `release` target and run the `Install` script yourself. `RELEASE_ROOT`
    is used for specifying the directory where the installation should be
    created. This is what by default ends up under `/usr/local/lib/erlang`
    if you do the install using `make install`. All installation paths
    provided in the `configure` phase are ignored, as well as `DESTDIR`,
    and `INSTALL_PREFIX`. If you want links from a specific `bin` directory
    to the installation you have to set those up yourself. An example where
    Erlang/OTP should be located at `/home/me/OTP`:

        $ ./configure
        $ make
        $ make RELEASE_ROOT=/home/me/OTP release
        $ cd /home/me/OTP
        $ ./Install -minimal /home/me/OTP
        $ mkdir -p /home/me/bin
        $ cd /home/me/bin
        $ ln -s /home/me/OTP/bin/erl erl
        $ ln -s /home/me/OTP/bin/erlc erlc
        $ ln -s /home/me/OTP/bin/escript escript
        ...

    The `Install` script should currently be invoked as follows in the
    directory where it resides (the top directory):

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
        but does not have to be. It can follow any other path through the
        file system to the same directory.

    If neither `-minimal`, nor `-sasl` is passed as argument you will be
    prompted.

*   Test install using `EXTRA_PREFIX`. The content of the `EXTRA_PREFIX`
    variable will prefix all installation paths when doing `make install`.
    Note that `EXTRA_PREFIX` is similar to `DESTDIR`, but it does *not* have
    the same effect as `DESTDIR`. The installation can and have to be run
    from the location specified by `EXTRA_PREFIX`. That is, it can be useful
    if you want to try the system out, running test suites, etc, before doing
    the real install without `EXTRA_PREFIX`.

### Symbolic Links in --bindir ###

When doing `make install` and the default installation prefix is used,
relative symbolic links will be created from `/usr/local/bin` to all public
Erlang/OTP executables in `/usr/local/lib/erlang/bin`. The installation phase
will try to create relative symbolic links as long as `--bindir` and the
Erlang bin directory, located under `--libdir`, both have `--exec-prefix` as
prefix. Where `--exec-prefix` defaults to `--prefix`. `--prefix`,
`--exec-prefix`, `--bindir`, and `--libdir` are all arguments that can be
passed to `configure`. One can force relative, or absolute links by passing
`BINDIR_SYMLINKS=relative|absolute` as arguments to `make` during the install
phase. Note that such a request might cause a failure if the request cannot
be satisfied.

### Pre-built Source Release ###

The source release is delivered with a lot of platform independent
build results already pre-built. If you want to remove these pre-built
files, invoke `./otp_build remove_prebuilt_files` from the `$ERL_TOP`
directory. After you have done this, you can build exactly the same way
as before, but the build process will take a much longer time.

> *WARNING*: Doing `make clean` in an arbitrary directory of the source
> tree, may remove files needed for bootstrapping the build.
>
> Doing `./otp_build save_bootstrap` from the `$ERL_TOP` directory before
> doing `make clean` will ensure that it will be possible to build after
> doing `make clean`. `./otp_build save_bootstrap` will be invoked
> automatically when `make` is invoked from `$ERL_TOP` with either the
> `clean` target, or the default target. It is also automatically invoked
> if `./otp_build remove_prebuilt_files` is invoked.

### Building in Git ###

When building in a Git working directory you also have to have a GNU `autoconf`
of at least version 2.59 on your system, because you need to generate the
`configure` scripts before you can start building.

The `configure` scripts are generated by invoking `./otp_build autoconf` in
the `$ERL_TOP` directory. The `configure` scripts also have to be regenerated
when a `configure.in` or `aclocal.m4` file has been modified. Note that when
checking out a branch a `configure.in` or `aclocal.m4` file may change
content, and you may therefore have to regenerate the `configure` scripts
when checking out a branch. Regenerated `configure` scripts imply that you
have to run `configure` and build again.

> *NOTE*: Running `./otp_build autoconf` is **not** needed when building
> an unmodified version of the released source.

Other useful information can be found at our github wiki:
<http://wiki.github.com/erlang/otp>

### make and $ERL\_TOP ###

All the makefiles in the entire directory tree use the environment
variable `ERL_TOP` to find the absolute path of the installation. The
`configure` script will figure this out and set it in the top level
Makefile (which, when building, it will pass on). However, when
developing it is sometimes convenient to be able to run make in a
subdirectory. To do this you must set the `ERL_TOP` variable
before you run make.

For example, assume your GNU make program is called `make` and you
want to rebuild the application `STDLIB`, then you could do:

    $ cd lib/stdlib; env ERL_TOP=<Dir> make

where `<Dir>` would be what you find `ERL_TOP` is set to in the top level
Makefile.

The Erlang/OTP Documentation
----------------------------

### How to Build the Documentation ###

    $ cd $ERL_TOP

If you have just built Erlang/OTP in the current source tree, you have
already ran `configure` and do not need to do this again; otherwise, run
`configure`.

    $ ./configure [Configure Args]

When building the documentation you need a full Erlang/OTP-%OTP-REL% system in
the `$PATH`.

    $ export PATH=<Erlang/OTP-%OTP-REL% bin dir>:$PATH     # Assuming bash/sh

Build the documentation.

    $ make docs

The documentation can be installed either using the `install-docs` target,
or using the `release_docs` target.

*   If you have installed Erlang/OTP using the `install` target, install
    the documentation using the `install-docs` target. Install locations
    determined by `configure` will be used. `$DESTDIR` can be used the
    same way as when doing `make install`.

        $ make install-docs

*   If you have installed Erlang/OTP using the `release` target, install
    the documentation using the `release_docs` target. You typically want
    to use the same `RELEASE_ROOT` as when invoking `make release`.

        $ make release_docs RELEASE_ROOT=<release dir>

#### Build Issues ####

We have sometimes experienced problems with Sun's `java` running out of
memory when running `fop`. Increasing the amount of memory available
as follows has in our case solved the problem.

    $ export FOP_OPTS="-Xmx<Installed amount of RAM in MB>m"

More information can be found at
<http://xmlgraphics.apache.org/fop/0.95/running.html#memory>.

### How to Install the Pre-formatted Documentation ###

Pre-formatted [html documentation][] and [man pages][] can be downloaded at
<http://www.erlang.org/download.html>.

For some graphical tools to find the on-line help you have to install
the HTML documentation on top of the installed OTP applications, i.e.

    $ cd <ReleaseDir>
    $ gunzip -c otp_html_%OTP-REL%.tar.gz | tar xf -

For `erl -man <page>` to work the Unix manual pages have to be
installed in the same way, i.e.

    $ cd <ReleaseDir>
    $ gunzip -c otp_man_%OTP-REL%.tar.gz | tar xf -

Where `<ReleaseDir>` is

*   `<PrefixDir>/lib/erlang` if you have installed Erlang/OTP using
    `make install`.
*   `$DESTDIR<PrefixDir>/lib/erlang` if you have installed Erlang/OTP
    using `make install DESTDIR=<TmpInstallDir>`.
*   `RELEASE_ROOT` if you have installed using
    `make release RELEASE_ROOT=<ReleaseDir>`.

Support for SMP (Symmetric Multi Processing)
--------------------------------------------

An emulator with SMP support will be built by default on most platforms
if a usable POSIX thread library or native Windows threads is found.

You can force building of an SMP emulator, by using
`./configure --enable-smp-support`. However, if configure does not
automatically enable SMP support, the build is very likely to fail.

Use `./configure --disable-smp-support` if you for some reason do not
want to have the emulator with SMP support built.

If SMP support is enabled, support for threaded I/O will also be turned on
(also in the emulator without SMP support).

The `erl` command will automatically start the SMP emulator if the
computer has more than one logical processor. You can force a start
of the emulator with SMP support by passing `-smp enable` as
command line arguments to erl, and you can force a start of the
emulator without SMP support by passing `-smp disable`.

GS (Graphic System)
-------------------

GS now Tcl/Tk 8.4. It will be searched for when starting GS.

Using HiPE
----------

HiPE supports the following system configurations:

*   x86: All 32-bit and 64-bit mode processors should work.

    *   Linux: Fedora Core is supported. Both 32-bit and 64-bit modes are
        supported.

        NPTL glibc is strongly preferred, or a LinuxThreads
        glibc configured for "floating stacks". Old non-floating
        stacks glibcs have a fundamental problem that makes HiPE
        support and threads support mutually exclusive.

    *   Solaris: Solaris 10 (32-bit and 64-bit) and 9 (32-bit) are supported.
        The build requires a version of the GNU C compiler (gcc)
        that has been configured to use the GNU assembler (gas).
        Sun's x86 assembler is emphatically **not** supported.

    *   FreeBSD: FreeBSD 6.1 and 6.2 in 32-bit and 64-bit modes should work.

    *   MacOSX/Darwin: Darwin 9.8.0 in 32-bit mode should work.

*   PowerPC: All 32-bit 6xx/7xx(G3)/74xx(G4) processors should work. 32-bit
    mode on 970 (G5) and POWER5 processors should work.

    * Linux (Yellow Dog) and Mac OSX 10.4 are supported.

*   SPARC: All UltraSPARC processors running 32-bit user code should work.

    *   Solaris 9 is supported. The build requires a `gcc` that has been
        configured to use Sun's assembler and linker. Using the GNU assembler
        but Sun's linker has been known to cause problems.

    *   Linux (Aurora) is supported.

*   ARM: ARMv5TE (i.e. XScale) processors should work. Both big-endian and
    little-endian modes are supported.

    * Linux is supported.

HiPE is automatically enabled on the following systems:

*   x86 in 32-bit mode: Linux, Solaris, FreeBSD
*   x86 in 64-bit mode: Linux, Solaris, FreeBSD
*   PowerPC: Linux, MacOSX
*   SPARC: Linux
*   ARM: Linux

On other supported systems you need to `./configure --enable-hipe`.

If you are running on a platform supporting HiPE and if you have not disabled
HiPE, you can compile a module into native code like this from the Erlang
shell:

    1> c(Module, native).

or

    1> c(Module, [native|OtherOptions]).

Using the erlc program, write like this:

    $ erlc +native Module.erl

The native code will be placed into the beam file and automatically loaded
when the beam file is loaded.

To add hipe options, write like this from the Erlang shell:

    1> c(Module, [native,{hipe,HipeOptions}|MoreOptions]).

Use `hipe:help_options/0` to print out the available options.

    1> hipe:help_options().

Mac OS X (Darwin)
-----------------

Make sure that the command `hostname` returns a valid fully qualified host
name (this is configured in `/etc/hostconfig`).

If you develop linked-in drivers (shared library) you need to link using
`gcc` and the flags `-bundle -flat_namespace -undefined suppress`. You also
include `-fno-common` in `CFLAGS` when compiling. Use `.so` as the library
suffix.

Use the `--enable-darwin-64bit` configure flag to build a 64-bit
binaries on Mac OS X.

Building a fast Erlang VM on Mac OS Lion
----------------------------------------

Starting with Xcode 4.2, Apple no longer includes a "real" `gcc`
compiler (not based on the LLVM).  Building with `llvm-gcc` or `clang`
will work, but the performance of the Erlang run-time system will not
be the best possible.

Note that if you have `gcc-4.2` installed and included in `PATH`
(from a previous version of Xcode), `configure` will automatically
make sure that `gcc-4.2` will be used to compile `beam_emu.c`
(the source file most in need of `gcc`).

If you don't have `gcc-4.2.` and want to build a run-time system with
the best possible performance, do like this:

Install Xcode from the AppStore if it is not already installed.

If you have Xcode 4.3, or later, you will also need to download 
"Command Line Tools" via the Downloads preference pane in Xcode.

Some tools may still be lacking or out-of-date, we recommend using
[Homebrew](https://github.com/mxcl/homebrew/wiki/installation) or
Macports to update those tools.

Install MacPorts (<http://www.macports.org/>). Then:

    $ sudo port selfupdate
    $ sudo port install gcc45 +universal

### Building with wxErlang ###

If you want to build the `wx` application, you will need to get wxWidgets-3.0 (or later)
(`wxWidgets-3.0.0.tar.bz2` from <http://sourceforge.net/projects/wxwindows/files/3.0.0/>)
or get it from github:
    $ git clone git@github.com:wxWidgets/wxWidgets.git

Be aware that the wxWidgets-3.0 is a new release of wxWidgets, it is not as matured 
as the old releases and the MacOsX port still lags behind the other ports.

Configure and build wxWidgets:

    $ ./configure --with-cocoa --prefix=/usr/local 
      % Optional version and static libs: --with-macosx-version-min=10.9 --disable-shared
    $ make
    $ sudo make install
    $ export PATH=/usr/local/bin:$PATH

Check that you got the correct wx-config

    $ which wx-config

### Finish up ###

Build Erlang

    $ export PATH=/usr/local/bin:$PATH
    $ cd $ERL_TOP
    $ ./configure --enable-shared-zlib
    $ make
    $ sudo make install

How to Build a Debug Enabled Erlang RunTime System
--------------------------------------------------

After completing all the normal building steps described above a debug
enabled runtime system can be built. To do this you have to change
directory to `$ERL_TOP/erts/emulator`.

In this directory execute:

    $ make debug FLAVOR=$FLAVOR

where `$FLAVOR` is either `plain` or `smp`. The flavor options will
produce a beam.debug and beam.smp.debug executable respectively. The
files are installed along side with the normal (opt) versions `beam.smp`
and `beam`.

To start the debug enabled runtime system execute:

    $ $ERL_TOP/bin/cerl -debug

The debug enabled runtime system features lock violation checking,
assert checking and various sanity checks to help a developer ensure
correctness. Some of these features can be enabled on a normal beam
using appropriate configure options.

There are other types of runtime systems that can be built as well
using the similar steps just described.

    $ make $TYPE FLAVOR=$FLAVOR

where `$TYPE` is `opt`, `gcov`, `gprof`, `debug`, `valgrind`, or `lcnt`.
These different beam types are useful for debugging and profiling
purposes.

Authors
-------

Authors are mostly listed in the application's `AUTHORS` files,
that is `$ERL_TOP/lib/*/AUTHORS` and `$ERL_TOP/erts/AUTHORS`,
not in the individual source files.

Copyright and License
---------------------

%CopyrightBegin%

Copyright Ericsson AB 1998-2013. All Rights Reserved.

The contents of this file are subject to the Erlang Public License,
Version 1.1, (the "License"); you may not use this file except in
compliance with the License. You should have received a copy of the
Erlang Public License along with this software. If not, it can be
retrieved online at http://www.erlang.org/.

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
the License for the specific language governing rights and limitations
under the License.

%CopyrightEnd%

More Information
----------------

More information can be found at <http://www.erlang.org>.

Modifying This Document
-----------------------

Before modifying this document you need to have a look at the
[$ERL_TOP/HOWTO/MARKDOWN.md][] document.



   [$ERL_TOP/HOWTO/INSTALL-CROSS.md]: INSTALL-CROSS.md
   [$ERL_TOP/HOWTO/INSTALL-WIN32.md]: INSTALL-WIN32.md
   [DESTDIR]: http://www.gnu.org/prep/standards/html_node/DESTDIR.html
   [Building in Git]: #How-to-Build-and-Install-ErlangOTP_Building-in-Git
   [Pre-built Source Release]: #How-to-Build-and-Install-ErlangOTP_Prebuilt-Source-Release
   [make and $ERL_TOP]: #How-to-Build-and-Install-ErlangOTP_make-and-ERLTOP
   [html documentation]: http://www.erlang.org/download/otp_doc_html_%OTP-REL%.tar.gz
   [man pages]: http://www.erlang.org/download/otp_doc_man_%OTP-REL%.tar.gz
   [the released source tar ball]: http://www.erlang.org/download/otp_src_%OTP-REL%.tar.gz
   [$ERL_TOP/HOWTO/MARKDOWN.md]: MARKDOWN.md

   [?TOC]: true
