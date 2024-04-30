Building and Installing Erlang/OTP
==================================

Introduction
------------

This document describes how to build and install Erlang/OTP-%OTP-REL%.
Erlang/OTP should be possible to build from source on any Unix/Linux system,
including macOS. You are advised to read the whole document
before attempting to build and install Erlang/OTP.

The source code can be downloaded from the official site of Erlang/OTP or GitHub.
* <http://www.erlang.org/downloads>
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
*   `ncurses`, `termcap`, or `termlib` -- The development headers and
    libraries are needed, often known as `ncurses-devel`. Use
    `--without-termcap` to build without any of these libraries. Note that
    in this case only the old shell (without any line editing) can be used.
*  `sed` -- Stream Editor for basic text transformation.

#### Building in Git ####

Build the same way as when building the unpacked tar file.

#### Building on macOS ####

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
    Required for building the application `jinterface`.
    At least version 1.6.0 of the JDK is required.

    Download from <http://www.oracle.com/technetwork/java/javase/downloads>.
    We have also tested with IBM's JDK 1.6.0.
*   `flex` -- Headers and libraries are needed to build the flex
    scanner for the `megaco` application on Unix/Linux.
*   wxWidgets -- Toolkit for GUI applications.
    Required for building the `wx` application. At least
    version 3.0 of wxWidgets is required.

    Download from <http://sourceforge.net/projects/wxwindows/files/3.0.0/>
    or get it from GitHub: <https://github.com/wxWidgets/wxWidgets>

    Further instructions on wxWidgets, read [Building with Wx][].

### Building Documentation ###

*   `ex_doc` -- [ExDoc](https://hexdocs.pm/ex_doc/readme.html) is a tool to
    generate html and epub documentation for Erlang and Elixir projects.
  
    Download as an [escript from github](https://github.com/elixir-lang/ex_doc/releases/latest)
    or get it from GitHub: <https://github.com/elixir-lang/ex_doc> and build
    your self.
    
    ExDoc %EX_DOC_VSN% was used to build the documentation for this release,
    but any version after that should work just as well.

    You can also use `./otp_build download_ex_doc` to download the correct version
    from github.

How to Build and Install Erlang/OTP
-----------------------------------

The following instructions are for building [the released source tar ball][]
or from a [git clone](https://github.com/erlang/otp).

The variable `$ERL_TOP` will be mentioned a lot of times. It refers to
the top directory in the source tree. More information about `$ERL_TOP`
can be found in the [make and $ERL_TOP][] section below.

### Unpacking ###

Start by unpacking the Erlang/OTP distribution file with your GNU
compatible TAR program.

    $ tar -zxf otp_src_%OTP-VSN%.tar.gz    # Assuming bash/sh

or clone from github:

    $ git clone https://github.com/erlang/otp otp_src_%OTP-VSN%

Now change directory into the base directory and set the `$ERL_TOP` variable.

    $ cd otp_src_%OTP-VSN%
    $ export ERL_TOP=`pwd`    # Assuming bash/sh

### Configuring ###

Run the following commands to configure the build:

    $ ./configure [ options ]

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

> #### Note {: .info }
>
> On builds without `crypto`, `ssl` and `ssh` there is a failed test case
> for undefined functions. Verify that the failed test case log only shows calls
> to skipped applications.

### Installing ###

You are now ready to install the Erlang/OTP release!
The following command will install the release on your system.

    $ make install

### Running ###

You should now have a working release of Erlang/OTP!
Jump to [System Principles][] for instructions on running Erlang/OTP.

[](){: #How-to-Build-and-Install-Erlang-OTP_How-to-Build-the-Documentation }

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

To build `html` and `epub` docs you need to have [ExDoc %EX_DOC_VSN%](https://github.com/elixir-lang/ex_doc).
See [Building Documentation](#building-documentation) for information on how to
install ExDoc.

Build the documentation using:

    $ make docs

It is possible to limit which types of documentation is build by passing the `DOC_TARGETS`
environment variable to `make docs`.

_Example_:

    $ make docs DOC_TARGETS=chunks

The currently available types are: `html` and `chunks`. Where:

* *chunks* - Build [EEP-48](`e:kernel:eep48_chapter.md`) documentation chunks.
* *html* - Build html and epub documentation.

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

It is possible to limit which types of documentation is released using the same `DOC_TARGETS`
environment variable as when building documentation.

### Accessing the Documentation ###

After installation you can access the documentation by

*   Browsing the html pages by loading the page `/usr/local/lib/erlang/doc/erlang/index.html`
    or `<BaseDir>/lib/erlang/doc/erlang/index.html` if the prefix option has been used.

*   Read the embedded documentation by using the built-in shell functions `h/1,2,3` or
    `ht/1,2,3`.

### How to Install the Pre-formatted Documentation ###

Pre-formatted [html documentation][] can be downloaded from <http://www.erlang.org/download.html>.

Extract the html archive in the installation directory.

    $ cd <ReleaseDir>
    $ tar -zxf otp_html_%OTP-VSN%.tar.gz

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

[](){: #advanced-configuration-and-build-of-erlang-otp_make-and-ERLTOP }

### make and $ERL_TOP ###

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

### otp_build vs configure/make ###

Building Erlang/OTP can be done either by using the `$ERL_TOP/otp_build`
script, or by invoking `$ERL_TOP/configure` and `make` directly. Building using
`otp_build` is easier since it involves fewer steps, but the `otp_build` build
procedure is not as flexible as the `configure`/`make` build procedure. The binary
releases for Windows that we deliver are built using `otp_build`.

[]() {: #advanced-configuration-and-build-of-erlang-otp_configuring }

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
*   `--disable-parallel-configure` - Disable parallel execution of
    `configure` scripts (parallel execution is enabled by default)
*   `--{enable,disable}-jit` - Force enabling or disabling of the JIT.
*   `--{enable,disable}-kernel-poll` - Kernel poll support (enabled by
    default if possible)
*   `--enable-m64-build` - Build 64-bit binaries using the `-m64` flag to
    `(g)cc`
*   `--enable-m32-build` - Build 32-bit binaries using the `-m32` flag to
    `(g)cc`
*   `--{enable,disable}-pie` - Build position independent executable binaries.
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
*   `--{enable,disable}-builtin-zlib` - Use the built-in source for zlib.
*   `--{enable,disable}-dynamic-ssl-lib` - Enable or disable dynamic OpenSSL
    libraries when linking the crypto NIF. By default dynamic linking is
    done unless it does not work or is if it is a Windows system.
*   `--{with,without}-ssl` - OpenSSL (without implies that the `crypto`,
    `ssh`, and `ssl` won't be built)
*   `--with-ssl=PATH` - Specify base location of OpenSSL include and lib
    directories.
*   `--with-ssl-incl=PATH` - Specify base location of OpenSSL `include`
    directory (if different than base location specified by --with-ssl=PATH).
*   `--with-ssl-zlib=PATH` - Path to static zlib library to link the
    crypto NIF with. This zlib library is most often not necessary but
    might be needed in order to link the NIF in some cases.
*   `--with-ssl-lib-subdir=RELATIVE_PATH` - Specify extra OpenSSL lib
    sub-directory to search in (relative to base directory).
*   `--with-ssl-rpath=yes|no|PATHS` - Runtime library path for OpenSSL.
    Default is `yes`, which equates to a number of standard locations. If
    `no`, then no runtime library paths will be used. Anything else should be
    a comma or colon separated list of paths.
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
    `--enable-static-nifs=/home/$USER/my_nif.a`. The paths have to be absolute.
    For drivers, the driver name has to be the same as the filename. You also
    have to define `STATIC_ERLANG_NIF_LIBNAME` (see `erl_nif` documentation) or
    `STATIC_ERLANG_DRIVER` when compiling the .o files for the nif/driver.
    If your nif/driver depends on some other dynamic library, you now have to link
    that to the Erlang VM binary. This is easily achieved by passing `LIBS=-llibname`
    to configure.
*   `--without-$app` - By default all applications in Erlang/OTP will be included
	in a release. If this is not wanted it is possible to specify that Erlang/OTP
	should be compiled without one or more applications, i.e. `--without-wx`. There is
	no automatic dependency handling between applications. If you disable
	an application that another application depends on, you also have to disable the
	dependent application.
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
*   `--enable-ensure-os-monotonic-time` - Enable functionality ensuring the
    monotonicity of monotonic timestamps delivered by the OS. When a
    non-monotonic timestamp is detected, it will be replaced by the last
    delivered monotonic timestamp before being used by Erlang's time
    functionality. Note that you do *not* want to enable this unless the OS
    monotonic time source on the system fails to produce monotonic timestamps.
    This since ensuring the monotonicity of OS monotonic timestamps will hurt
    scalability and performance of the system.
*   `--disable-saved-compile-time` - Disable saving of compile date and time
    in the emulator binary.
*   `--enable-ei-dynamic-lib` - Make erl_interface build a shared library in addition
    to the archive normally built.
*   `--disable-year2038` - Don't support timestamps after mid-January 2038. By
    default `configure` will try to enable support for timestamps after
    mid-January 2038. If it cannot figure out how to do that, it will fail and
    abort with an error. If you anyway want to build the system knowing that the
    system won't function properly after mid-January 2038, you can pass this
    option which will enable `configure` to continue without support for
    timestamps after mid-January 2038. This is typically only an issue on 32-bit
    platforms.

If you or your system has special requirements please read the `Makefile` for
additional configuration information.

[](){: #advanced-configuration-and-build-of-erlang-otp_Configuring_Important-Variables-Inspected-by-configure }

#### Important Variables Inspected by configure ####

##### Compiler and Linker #####

*   `CC` - C compiler.
*   `CFLAGS` - C compiler flags. Defaults to "-g -O2". If you set it,
    these will be removed.
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

##### Dynamic Erlang Driver Linking #####

> #### Note {: .info }
>
> Either set all or none of the `DED_LD*` variables (with the exception
> of `DED_LDFLAGS_CONFTEST`).

*   `DED_LD` - Linker for Dynamically loaded Erlang Drivers.
*   `DED_LDFLAGS` - Linker flags to use with `DED_LD`.
*   `DED_LDFLAGS_CONFTEST` - Linker flags to use with `DED_LD` in configure
    link tests if `DED_LDFLAGS` cannot be used in such tests. If not set,
    `DED_LDFLAGS` will be used in configure tests.
*   `DED_LD_FLAG_RUNTIME_LIBRARY_PATH` - This flag should set runtime library
    search path for shared libraries when linking with `DED_LD`.

##### Large File Support #####

> #### Note {: .info }
>
> Either set all or none of the `LFS_*` variables.

*   `LFS_CFLAGS` - Large file support C compiler flags.
*   `LFS_LDFLAGS` - Large file support linker flags.
*   `LFS_LIBS` - Large file support libraries.

##### Other Tools #####

*   `RANLIB` - `ranlib` archive index tool.
*   `AR` - `ar` archiving tool.
*   `GETCONF` - `getconf` system configuration inspection tool. `getconf` is
    currently used for finding out large file support flags to use, and
    on Linux systems for finding out if we have an NPTL thread library or
    not.

#### Updating configure Scripts ####

Generated `configure` scripts are nowadays included in the git repository.

If you modify any `configure.in` files or the `erts/aclocal.m4` file, you need
to regenerate `configure` scripts before the changes will take effect. First
ensure that you have GNU `autoconf` of version 2.69 in your path. Then execute
`./otp_build update_configure [--no-commit]` in the `$ERL_TOP` directory. The
`otp_build` script will verify that `autoconf` is of correct version and will
refuse to update the `configure` scripts if it is of any other version.

[](){: #advanced-configuration-and-build-of-erlang-otp_configuring_atomic-memory-operations-and-the-vm }

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

Other useful information can be found here:
* [Erlang/OTP GitHub wiki](https://github.com/erlang/otp/wiki)
* [Contributing to Erlang/OTP](https://github.com/erlang/otp/blob/master/CONTRIBUTING.md)
* [Developing Erlang/OTP](https://github.com/erlang/otp/blob/master/HOWTO/DEVELOPMENT.md)

[](){: #advanced-configuration-and-build-of-erlang-otp_Building_macOS-Darwin }

#### macOS (Darwin) ####

Make sure that the command `hostname` returns a valid fully qualified host
name (this is configured in `/etc/hostconfig`). Otherwise you might experience
problems when running distributed systems.

If you develop linked-in drivers (shared library) you need to link using
`gcc` and the flags `-bundle -flat_namespace -undefined suppress`. You also
include `-fno-common` in `CFLAGS` when compiling. Use `.so` as the library
suffix.

If you have Xcode 4.3, or later, you will also need to download
"Command Line Tools" via the Downloads preference pane in Xcode.

[](){: #advanced-configuration-and-build-of-erlang-otp_Building_Building-with-Wx }

#### Building with Wx ####

wxWidgets-3.2.x is recommended for building the `wx` application
(wxWidgets-3.0.x will also work). Download it from
<https://www.wxwidgets.org/downloads> or from
<https://github.com/wxWidgets/wxWidgets>. It is recommended to use the
latest release in the 3.2 series, which at the time of writing
is 3.2.2.1.

Note that the wxWidgets-3.3 versions are experimental, but they should
also work if 3.0 compatibility is enabled by adding
`--enable-compat30` to the `configure` commands below.

On all other platforms, a shared library is built as follows:

    $ ./configure --prefix=/usr/local
    $ make && sudo make install
    $ export PATH=/usr/local/bin:$PATH

On Linux, a static library is built as follows:

    $ export CFLAGS=-fPIC
    $ export CXXFLAGS=-fPIC
    $ ./configure --prefix=/usr/local --disable-shared
    $ make && sudo make install
    $ export PATH=/usr/local/bin:$PATH

On macOs, a static library compatible with macOS 13 (Ventura) and later is built
as follows:

    $ ./configure --prefix=/usr/local --with-macosx-version-min=13.0 --disable-shared
    $ make
    $ sudo make install
    $ export PATH=/usr/local/bin:$PATH

Verify that the build and installation succeeded:

    $ which wx-config && wx-config --version-full

Expected output is `/usr/local/bin/wx-config` on one line, followed by the full
version number. For example, if you built version 3.2.2.1, the expected output is:

    /usr/local/bin/wx-config
    3.2.2.1

Build Erlang/OTP in the usual way. To verify that `wx` application is
working run the following command:

    $ erl -run wx demo


[](){: #advanced-configuration-and-build-of-erlang-otp_Building_Prebuilt-Source-Release }

#### Pre-built Source Release ####

The source release is delivered with a lot of platform independent
build results already pre-built. If you want to remove these pre-built
files, invoke `./otp_build remove_prebuilt_files` from the `$ERL_TOP`
directory. After you have done this, you can build exactly the same way
as before, but the build process will take a much longer time.

> #### Warning {: .warning }
>
> Doing `make clean` in an arbitrary directory of the source
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

[](){: #advanced-configuration-and-build-of-erlang-otp_building_how-to-build-a-debug-enabled-erlang-runtime-system }

#### How to Build a Debug Enabled Erlang RunTime System ####

After completing all the normal building steps described above a debug
enabled runtime system can be built. To do this you have to change
directory to `$ERL_TOP/erts/emulator` and execute:

    $ (cd $ERL_TOP/erts/emulator && make debug)

This will produce a `beam.debug.smp` executable. The
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

where `$TYPE` is `opt`, `gcov`, `gprof`, `debug`, `valgrind`, `asan` or `lcnt`.
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

## Erlang/OTP test architectures ##

Erlang/OTP are currently tested on the following hardware and operating systems.
This is not an exhaustive list, but we try to keep it as up to date as possible.

Architecture

* x86, x86-64
* Aarch32, Aarch64
* powerpc, powerpc64le
* Apple M1, M2, M2 Pro

Operating system

* Fedora 31
* FreeBSD
* macOS 13 - 14
* MontaVista 4
* NetBSD
* OpenBSD
* SLES 10, 11, 12
* SunOS 5.11
* Ubuntu 10.04 - 22.04
* Windows 11, Windows 10, Windows Server 2019

[DESTDIR]: http://www.gnu.org/prep/standards/html_node/DESTDIR.html
[Building in Git]: #advanced-configuration-and-build-of-erlang-otp_Building_Within-Git
[Advanced Configure]: #advanced-configuration-and-build-of-erlang-otp_Configuring
[Pre-built Source Release]: #advanced-configuration-and-build-of-erlang-otp_Building_Prebuilt-Source-Release
[make and $ERL_TOP]: #advanced-configuration-and-build-of-erlang-otp_make-and-ERLTOP
[html documentation]: https://github.com/erlang/otp/releases/download/OTP-%OTP-VSN%/otp_doc_html_%OTP-VSN%.tar.gz
[the released source tar ball]: https://github.com/erlang/otp/releases/download/OTP-%OTP-VSN%/otp_src_%OTP-VSN%.tar.gz
[System Principles]: `e:system:system_principles.md`
[native build]: #how-to-build-and-install-erlang-otp
[cross build]: INSTALL-CROSS.md
[Required Utilities]: #Required-Utilities
[Optional Utilities]: #Optional-Utilities
[Building on a Mac]: #advanced-configuration-and-build-of-erlang-otp_Building_macOS-Darwin
[Building with Wx]: #advanced-configuration-and-build-of-erlang-otp_Building_Building-with-Wx
[libatomic_ops]: https://github.com/ivmai/libatomic_ops/
