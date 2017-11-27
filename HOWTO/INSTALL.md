Building and Installing Erlang/OTP
==================================

Introduction
------------

This document describes how to build and install Erlang/OTP-%OTP-REL%.
Erlang/OTP should be possible to build from source on any Unix/Linux system,
including OS X. You are advised to read the whole document
before attempting to build and install Erlang/OTP.

The source code can be downloaded from the official site of Erlang/OTP or GitHub.
* <http://www.erlang.org>
* <https://github.com/erlang/otp>

Required Utilities
------------------

These are the tools you need in order to unpack and build Erlang/OTP.

### Unpacking ###

*   GNU unzip, or a modern uncompress.
*   A TAR program that understands the GNU TAR format for long filenames.

### Building ###

*   GNU `make`
*   Compiler -- GNU C Compiler, `gcc` or the C compiler frontend for LLVM, `clang`.
*   Perl 5
*   GNU `m4` -- If HiPE (native code) support is enabled. HiPE can be
    disabled using `--disable-hipe`
*   `ncurses`, `termcap`, or `termlib` -- The development headers and
    libraries are needed, often known as `ncurses-devel`. Use
    `--without-termcap` to build without any of these libraries. Note that
    in this case only the old shell (without any line editing) can be used.
*  `sed` -- Stream Editor for basic text transformation.

#### Building in Git ####

*   GNU `autoconf` of at least version 2.59. Note that `autoconf` is not
    needed when building an unmodified version of the released source.

#### Building on OS X ####

*   Xcode -- Download and install via the Mac App Store.
    Read about [Building on a Mac][] before proceeding.

### Installing ###

*   An `install` program that can take multiple file names.


Optional Utilities
------------------

Some applications are automatically skipped if the dependencies aren't met.
Here is a list of utilities needed for those applications. You will
also find the utilities needed for building the documentation.

### Building ###

*   OpenSSL -- The opensource toolkit for Secure Socket Layer
    and Transport Layer Security.
    Required for building the application `crypto`.
    Further, `ssl` and `ssh` require a working crypto application and
    will also be skipped if OpenSSL is missing. The `public_key`
    application is available without `crypto`, but the functionality
    will be very limited.

    The development package of OpenSSL including the header files are needed as well
    as the binary command program `openssl`. At least version 0.9.8 of OpenSSL is required.
    Read more and download from <http://www.openssl.org>.
*   Oracle Java SE JDK -- The Java Development Kit (Standard Edition).
    Required for building the application `jinterface` and parts of `ic` and `orber`.
    At least version 1.6.0 of the JDK is required.

    Download from <http://www.oracle.com/technetwork/java/javase/downloads>.
    We have also tested with IBM's JDK 1.6.0.
*   X Windows -- Development headers and libraries are needed
    to build the Erlang/OTP application `gs` on Unix/Linux.
*   `flex` -- Headers and libraries are needed to build the flex
    scanner for the `megaco` application on Unix/Linux.
*   wxWidgets -- Toolkit for GUI applications.
    Required for building the `wx` application. At least
    version 3.0 of wxWidgets is required.

    Download from <http://sourceforge.net/projects/wxwindows/files/3.0.0/>
    or get it from GitHub: <https://github.com/wxWidgets/wxWidgets>

    Further instructions on wxWidgets, read [Building with wxErlang][].



### Building Documentation ###

*   `xsltproc` -- A command line XSLT processor.

    A tool for applying XSLT stylesheets
    to XML documents. Download xsltproc from
    <http://xmlsoft.org/XSLT/xsltproc2.html>.

*   `fop` -- Apache FOP print formatter (requires Java). Can be downloaded
    from <http://xmlgraphics.apache.org/fop>.



How to Build and Install Erlang/OTP
-----------------------------------

The following instructions are for building [the released source tar ball][].

The variable `$ERL_TOP` will be mentioned a lot of times. It refers to
the top directory in the source tree. More information about `$ERL_TOP`
can be found in the [make and $ERL_TOP][] section below. If you are
building in git you probably want to take a look at the [Building in Git][]
section below before proceeding.

### Unpacking ###

Start by unpacking the Erlang/OTP distribution file with your GNU
compatible TAR program.

    $ tar -zxf otp_src_%OTP-VSN%.tar.gz    # Assuming bash/sh

Now change directory into the base directory and set the `$ERL_TOP` variable.

    $ cd otp_src_%OTP-VSN%
    $ export ERL_TOP=`pwd`    # Assuming bash/sh

### Configuring ###

Run the following commands to configure the build:

    $ ./configure [ options ]

> *NOTE*: If you are building Erlang/OTP from git you will need to run `./otp_build autoconf` to generate
> the configure scripts.

By default, Erlang/OTP release will be installed in `/usr/local/{bin,lib/erlang}`.
If you for instance don't have the permission to install in the standard location,
 you can install Erlang/OTP somewhere else. For example, to install in
`/opt/erlang/%OTP-VSN%/{bin,lib/erlang}`, use the `--prefix=/opt/erlang/%OTP-VSN%` option.

On some platforms Perl may behave strangely if certain locales are
set. If you get errors when building, try setting the LANG variable:

    $ export LANG=C   # Assuming bash/sh


### Building ###

Build the Erlang/OTP release.

    $ make


### Testing ###

Before installation you should test whether your build is working properly
by running our smoke test. The smoke test is a subset of the complete Erlang/OTP test suites.
First you will need to build and release the test suites.

    $ make release_tests

This creates an additional folder in `$ERL_TOP/release` called `tests`.
Now, it's time to start the smoke test.

    $ cd release/tests/test_server
    $ $ERL_TOP/bin/erl -s ts install -s ts smoke_test batch -s init stop

To verify that everything is ok you should open `$ERL_TOP/release/tests/test_server/index.html`
in your web browser and make sure that there are zero failed test cases.

> *NOTE*: On builds without `crypto`, `ssl` and `ssh` there is a failed test case
> for undefined functions. Verify that the failed test case log only shows calls
> to skipped applications.

### Installing ###

You are now ready to install the Erlang/OTP release!
The following command will install the release on your system.

    $ make install


### Running ###

You should now have a working release of Erlang/OTP!
Jump to [System Principles][] for instructions on running Erlang/OTP.


### How to Build the Documentation ###

Make sure you're in the top directory in the source tree.

    $ cd $ERL_TOP

If you have just built Erlang/OTP in the current source tree, you have
already ran `configure` and do not need to do this again; otherwise, run
`configure`.

    $ ./configure [Configure Args]

When building the documentation you need a full Erlang/OTP-%OTP-VSN% system in
the `$PATH`.

    $ export PATH=$ERL_TOP/bin:$PATH     # Assuming bash/sh

For the FOP print formatter, two steps must be taken:

*   Adding the location of your installation of `fop` in `$FOP_HOME`.

        $ export FOP_HOME=/path/to/fop/dir # Assuming bash/sh

*   Adding the `fop` script (in `$FOP_HOME`) to your `$PATH`, either by adding `$FOP_HOME` to `$PATH`, or by copying the `fop` script to a directory already in your `$PATH`.

Build the documentation.

    $ make docs

#### Build Issues ####

We have sometimes experienced problems with Oracle's `java` running out of
memory when running `fop`. Increasing the amount of memory available
as follows has in our case solved the problem.

    $ export FOP_OPTS="-Xmx<Installed amount of RAM in MB>m"

More information can be found at
*   <http://xmlgraphics.apache.org/fop/0.95/running.html#memory>.


### How to Install the Documentation ###

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


### Accessing the Documentation ###

After installation you can access the documentation by

*   Reading man pages. Make sure that `erl` is referring to the
    installed version. For example `/usr/local/bin/erl`.
    Try viewing at the man page for Mnesia

        $ erl -man mnesia

*   Browsing the html pages by loading the page `/usr/local/lib/erlang/doc/erlang/index.html`
    or `<BaseDir>/lib/erlang/doc/erlang/index.html` if the prefix option has been used.


### How to Install the Pre-formatted Documentation ###

Pre-formatted [html documentation][] and [man pages][] can be downloaded from
* <http://www.erlang.org/download.html>.

Extract the html archive in the installation directory.

    $ cd <ReleaseDir>
    $ tar -zxf otp_html_%OTP-VSN%.tar.gz

For `erl -man <page>` to work the Unix manual pages have to be
installed in the same way, i.e.

    $ cd <ReleaseDir>
    $ tar -zxf otp_man_%OTP-VSN%.tar.gz

Where `<ReleaseDir>` is

*   `<PrefixDir>/lib/erlang` if you have installed Erlang/OTP using
    `make install`.
*   `$DESTDIR<PrefixDir>/lib/erlang` if you have installed Erlang/OTP
    using `make install DESTDIR=<TmpInstallDir>`.
*   `RELEASE_ROOT` if you have installed using
    `make release RELEASE_ROOT=<ReleaseDir>`.


Advanced configuration and build of Erlang/OTP
----------------------------------------------

If you want to tailor your Erlang/OTP build and installation, please read
on for detailed information about the individual steps.

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

### otp\_build vs configure/make ###

Building Erlang/OTP can be done either by using the `$ERL_TOP/otp_build`
script, or by invoking `$ERL_TOP/configure` and `make` directly. Building using
`otp_build` is easier since it involves fewer steps, but the `otp_build` build
procedure is not as flexible as the `configure`/`make` build procedure. The binary
releases for Windows that we deliver are built using `otp_build`.

### Configuring ###

The configure script is created by the GNU autoconf utility, which
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
*   `--{enable,disable}-kernel-poll` - Kernel poll support (enabled by
    default if possible)
*   `--{enable,disable}-hipe` - HiPE support (enabled by default on supported
    platforms)
*   `--{enable,disable}-fp-exceptions` - Floating point exceptions (an
    optimization for floating point operations). The default differs
    depending on operating system and hardware platform. Note that by
    enabling this you might get a seemingly working system that sometimes
    fail on floating point operations.
*   `--enable-m64-build` - Build 64-bit binaries using the `-m64` flag to
    `(g)cc`
*   `--enable-m32-build` - Build 32-bit binaries using the `-m32` flag to
    `(g)cc`
*   `--with-assumed-cache-line-size=SIZE` - Set assumed cache-line size in
    bytes. Default is 64. Valid values are powers of two between and
    including 16 and 8192. The runtime system use this value in order to
    try to avoid false sharing. A too large value wastes memory. A to
    small value will increase the amount of false sharing.
*   `--{with,without}-termcap` - termcap (without implies that only the old
    Erlang shell can be used)
*   `--with-javac=JAVAC` - Specify Java compiler to use
*   `--{with,without}-javac` - Java compiler (without implies that the
    `jinterface` application won't be built)
*   `--{enable,disable}-dynamic-ssl-lib` - Dynamic OpenSSL libraries
*   `--{enable,disable}-builtin-zlib` - Use the built-in source for zlib.
*   `--{with,without}-ssl` - OpenSSL (without implies that the `crypto`,
    `ssh`, and `ssl` won't be built)
*   `--with-ssl=PATH` - Specify location of OpenSSL include and lib
*   `--with-ssl-incl=PATH` - Location of OpenSSL `include` directory,
    if different than specified by `--with-ssl=PATH`
*   `--with-ssl-rpath=yes|no|PATHS` - Runtime library path for OpenSSL.
    Default is `yes`, which equates to a number of standard locations. If
    `no`, then no runtime library paths will be used. Anything else should be
    a comma separated list of paths.
*   `--with-libatomic_ops=PATH` - Use the `libatomic_ops` library for atomic
    memory accesses. If `configure` should inform you about no native atomic
    implementation available, you typically want to try using the
    `libatomic_ops` library. It can be downloaded from
    <https://github.com/ivmai/libatomic_ops/>.
*   `--disable-smp-require-native-atomics` - By default `configure` will
    fail if an SMP runtime system is about to be built, and no implementation
    for native atomic memory accesses can be found. If this happens, you are
    encouraged to find a native atomic implementation that can be used, e.g.,
    using `libatomic_ops`, but by passing `--disable-smp-require-native-atomics`
    you can build using a fallback implementation based on mutexes or spinlocks.
    Performance of the SMP runtime system will however suffer immensely without
    an implementation for native atomic memory accesses.
*   `--enable-static-{nifs,drivers}` - To allow usage of nifs and drivers on OSs
    that do not support dynamic linking of libraries it is possible to statically
    link nifs and drivers with the main Erlang VM binary. This is done by passing
    a comma separated list to the archives that you want to statically link. e.g.
    `--enable-static-nifs=/home/$USER/my_nif.a`. The path has to be absolute and the
    name of the archive has to be the same as the module, i.e. `my_nif` in the
    example above. This is also true for drivers, but then it is the driver name
    that has to be the same as the filename. You also have to define
    `STATIC_ERLANG_{NIF,DRIVER}` when compiling the .o files for the nif/driver.
    If your nif/driver depends on some other dynamic library, you now have to link
    that to the Erlang VM binary. This is easily achieved by passing `LIBS=-llibname`
    to configure.
*   `--without-$app` - By default all applications in Erlang/OTP will be included
	in a release. If this is not wanted it is possible to specify that Erlang/OTP
	should be compiled without one or more applications, i.e. `--without-wx`. There is
	no automatic dependency handling between applications. If you disable
	an application that another application depends on, you also have to disable the
	dependant application.
*   `--enable-gettimeofday-as-os-system-time` - Force usage of `gettimeofday()` for
    OS system time.
*   `--enable-prefer-elapsed-monotonic-time-during-suspend` - Prefer an OS monotonic
    time source with elapsed time during suspend.
*   `--disable-prefer-elapsed-monotonic-time-during-suspend` - Do not prefer an OS
    monotonic time source with elapsed time during suspend.
*   `--with-clock-resolution=high|low` - Try to find clock sources for OS system
    time, and OS monotonic time with higher or lower resolution than chosen by
    default. Note that both alternatives may have a negative impact on the performance
    and scalability compared to the default clock sources chosen.
*   `--disable-saved-compile-time` - Disable saving of compile date and time
    in the emulator binary.

If you or your system has special requirements please read the `Makefile` for
additional configuration information.

#### Atomic Memory Operations and the VM ####

The VM with SMP support makes quite a heavy use of atomic memory operations.
An implementation providing native atomic memory operations is therefore very
important when building Erlang/OTP. By default the VM will refuse to build
if native atomic memory operations are not available.

Erlang/OTP itself provides implementations of native atomic memory operations
that can be used when compiling with a `gcc` compatible compiler for 32/64-bit
x86, 32/64-bit SPARC V9, 32-bit PowerPC, or 32-bit Tile. When compiling with
a `gcc` compatible compiler for other architectures, the VM may be able to make
use of native atomic operations using the `__atomic_*` builtins (may be
available when using a `gcc` of at least version 4.7) and/or using the
`__sync_*` builtins (may be available when using a `gcc` of at least version
4.1). If only the `gcc`'s `__sync_*` builtins are available, the performance
will suffer. Such a configuration should only be used as a last resort. When
compiling on Windows using a MicroSoft Visual C++ compiler native atomic
memory operations are provided by Windows APIs.

Native atomic implementation in the order preferred:
1.  The implementation provided by Erlang/OTP.
2.  The API provided by Windows.
3.  The implementation based on the `gcc` `__atomic_*` builtins.
4.  If none of the above are available for your architecture/compiler, you
    are recommended to build and install [libatomic_ops][] before building
    Erlang/OTP. The `libatomic_ops` library provides native atomic memory
    operations for a variety of architectures and compilers. When building
    Erlang/OTP you need to inform the build system of where the
    `libatomic_ops` library is installed using the
    `--with-libatomic_ops=PATH` `configure` switch.
5.  As a last resort, the implementation solely based on the `gcc`
    `__sync_*` builtins. This will however cause lots of expensive and
    unnecessary memory barrier instructions to be issued. That is,
    performance will suffer. The `configure` script will warn at the end
    of its execution if it cannot find any other alternative than this.

### Building ###

Building Erlang/OTP on a relatively fast computer takes approximately
5 minutes. To speed it up, you can utilize parallel make with the `-j<num_jobs>` option.

    $ export MAKEFLAGS=-j8    # Assuming bash/sh
    $ make

If you've upgraded the source with a patch you may need to clean up from previous
builds before the new build.
Make sure to read the [Pre-built Source Release][] section below before doing a `make clean`.

#### Within Git ####

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

Other useful information can be found at our GitHub wiki:
* <http://wiki.github.com/erlang/otp>

#### OS X (Darwin) ####

Make sure that the command `hostname` returns a valid fully qualified host
name (this is configured in `/etc/hostconfig`). Otherwise you might experience
problems when running distributed systems.

If you develop linked-in drivers (shared library) you need to link using
`gcc` and the flags `-bundle -flat_namespace -undefined suppress`. You also
include `-fno-common` in `CFLAGS` when compiling. Use `.so` as the library
suffix.

If you have Xcode 4.3, or later, you will also need to download
"Command Line Tools" via the Downloads preference pane in Xcode.

#### Building with wxErlang ####

If you want to build the `wx` application, you will need to get wxWidgets-3.0
(`wxWidgets-3.0.3.tar.bz2` from <https://github.com/wxWidgets/wxWidgets/releases/download/v3.0.3/wxWidgets-3.0.3.tar.bz2>) or get it from github with bug fixes:

    $ git clone --branch WX_3_0_BRANCH git@github.com:wxWidgets/wxWidgets.git

The wxWidgets-3.1 version should also work if 2.8 compatibility is enabled,
add `--enable-compat28` to configure commands below.

Configure and build wxWidgets (shared library on linux):

    $ ./configure --prefix=/usr/local
    $ make && sudo make install
    $ export PATH=/usr/local/bin:$PATH

Configure and build wxWidgets (static library on linux):

    $ export CFLAGS=-fPIC
    $ export CXXFLAGS=-fPIC
    $ ./configure --prefix=/usr/local --disable-shared
    $ make && sudo make install
    $ export PATH=/usr/local/bin:$PATH

Configure and build wxWidgets (on Mavericks - 10.9):

    $ ./configure --with-cocoa --prefix=/usr/local
    or without support for old versions and with static libs
    $ ./configure --with-cocoa --prefix=/usr/local --with-macosx-version-min=10.9 --disable-shared
    $ make
    $ sudo make install
    $ export PATH=/usr/local/bin:$PATH

Check that you got the correct wx-config

    $ which wx-config && wx-config --version-full

Build Erlang/OTP

    $ export PATH=/usr/local/bin:$PATH
    $ cd $ERL_TOP
    $ ./configure
    $ make
    $ sudo make install


#### Pre-built Source Release ####

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
>
> If you need to verify the bootstrap beam files match the provided
> source files, use `./otp_build update_primary` to create a new commit that
> contains differences, if any exist.

#### How to Build a Debug Enabled Erlang RunTime System ####

After completing all the normal building steps described above a debug
enabled runtime system can be built. To do this you have to change
directory to `$ERL_TOP/erts/emulator` and execute:

    $ (cd $ERL_TOP/erts/emulator && make debug)

This will produce a  beam.smp.debug executable. The
file are installed along side with the normal (opt) version `beam.smp`.

To start the debug enabled runtime system execute:

    $ $ERL_TOP/bin/cerl -debug

The debug enabled runtime system features lock violation checking,
assert checking and various sanity checks to help a developer ensure
correctness. Some of these features can be enabled on a normal beam
using appropriate configure options.

There are other types of runtime systems that can be built as well
using the similar steps just described.

    $ (cd $ERL_TOP/erts/emulator && make $TYPE)

where `$TYPE` is `opt`, `gcov`, `gprof`, `debug`, `valgrind`, or `lcnt`.
These different beam types are useful for debugging and profiling
purposes.


### Installing ###

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

#### Symbolic Links in --bindir ####

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


### Running ###

#### Using HiPE ####

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

    *   OS X/Darwin: Darwin 9.8.0 in 32-bit mode should work.

*   PowerPC: All 32-bit 6xx/7xx(G3)/74xx(G4) processors should work. 32-bit
    mode on 970 (G5) and POWER5 processors should work.

    * Linux (Yellow Dog) and OS X 10.4 are supported.

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
*   PowerPC: Linux, Mac OSX
*   SPARC: Linux
*   ARM: Linux

On other supported systems, see [Advanced Configure][] on how to enable HiPE.

If you are running on a platform supporting HiPE and if you have not disabled
HiPE, you can compile a module into native code like this from the Erlang
shell:

    1> c(Module, native).

or

    1> c(Module, [native|OtherOptions]).

Using the erlc program, write like this

    $ erlc +native Module.erl

The native code will be placed into the beam file and automatically loaded
when the beam file is loaded.

To add hipe options, write like this from the Erlang shell:

    1> c(Module, [native,{hipe,HipeOptions}|MoreOptions]).

Use `hipe:help_options/0` to print out the available options.

    1> hipe:help_options().




   [$ERL_TOP/HOWTO/INSTALL-CROSS.md]: INSTALL-CROSS.md
   [$ERL_TOP/HOWTO/INSTALL-WIN32.md]: INSTALL-WIN32.md
   [DESTDIR]: http://www.gnu.org/prep/standards/html_node/DESTDIR.html
   [Building in Git]: #Advanced-configuration-and-build-of-ErlangOTP_Building_Within-Git
   [Advanced Configure]: #Advanced-configuration-and-build-of-ErlangOTP_Configuring
   [Pre-built Source Release]: #Advanced-configuration-and-build-of-ErlangOTP_Building_Prebuilt-Source-Release
   [make and $ERL_TOP]: #Advanced-configuration-and-build-of-ErlangOTP_make-and-ERLTOP
   [html documentation]: http://www.erlang.org/download/otp_doc_html_%OTP-VSN%.tar.gz
   [man pages]: http://www.erlang.org/download/otp_doc_man_%OTP-VSN%.tar.gz
   [the released source tar ball]: http://www.erlang.org/download/otp_src_%OTP-VSN%.tar.gz
   [System Principles]: ../system_principles/system_principles
   [native build]: #How-to-Build-and-Install-ErlangOTP
   [cross build]: INSTALL-CROSS.md
   [Required Utilities]: #Required-Utilities
   [Optional Utilities]: #Optional-Utilities
   [Building on a Mac]: #Advanced-configuration-and-build-of-ErlangOTP_Building_OS-X-Darwin
   [Building with wxErlang]: #Advanced-configuration-and-build-of-ErlangOTP_Building_Building-with-wxErlang
   [libatomic_ops]: https://github.com/ivmai/libatomic_ops/
