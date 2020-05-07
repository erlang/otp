How to Build Erlang/OTP on Windows
==================================

Introduction
------------

This section describes how to build the Erlang emulator and the OTP
libraries on Windows. Note that the Windows binary releases are still
a preferred alternative if one does not have Microsoft’s development
tools and/or don’t want to install WSL.

The instructions apply to Windows 10 (v.1809 and later) supporting the
WSL.1 (Windows Subsystem for Linux v.1) and using Ubuntu 18.04 release.

The procedure described uses WSL as a build environment.  You run the
bash shell in WSL and use the gnu make/configure/autoconf etc to do
the build. The emulator C-source code is, however, mostly compiled
with Microsoft Visual C++™, producing a native Windows binary. This is
the same procedure as we use to build the pre-built binaries. Why we
use VC++ and not gcc is explained further in the FAQ section.

These instructions apply for both 32-bit and 64-bit Windows. Note that
even if you build a 64-bit version of Erlang, most of the directories
and files involved are still named win32. Some occurrences of the name
win64 are however present. The installation file for a 64-bit Windows
version of Erlang, for example, is `otp_win64_%OTP-REL%.exe`.

If you feel comfortable with the environment and build
system, and have all the necessary tools, you have a great opportunity
to make the Erlang/OTP distribution for Windows better. Please submit
any suggestions to our [JIRA] [1] and patches to our [git project] [2] to let
them find their way into the next version of Erlang. If making changes
to the build system (like makefiles etc) please bear in mind that the
same makefiles are used on Unix/VxWorks, so that your changes
don't break other platforms. That of course goes for C-code too; system
specific code resides in the `$ERL_TOP/erts/emulator/sys/win32` and
`$ERL_TOP/erts/etc/win32` directories mostly. The
`$ERL_TOP/erts/emulator/beam` directory is for common code.


Short Version
-------------

In the following sections, we've described as much as we could about
the installation of the tools needed. Once the tools are installed,
building is quite easy. We have also tried to make these instructions
understandable for people with limited Unix experience. WSL is a whole
new environment to some Windows users, why careful explanation of
environment variables etc seemed to be in place.

This is the short story though, for the experienced and impatient:

    *   Get and install complete WSL environment

    *   Install Visual Studio 2019

    *   Get and install windows JDK-8

    *   Get and install windows NSIS 3.05 or later (3.05 tried and working)

    *   Get, build and install OpenSSL v1.1.1d or later (up to 1.1.1d
        tried & working) with static libs.

    *   Get, build and install wxWidgets-3.1.3 or later (up to 3.1.3
        tried & working) with static libs.

    *   Get the Erlang source distribution (from
        <http://www.erlang.org/download.html>) and unpack with `tar`
        to the windows disk for example to: /mnt/c/src/

    *   Install mingw-gcc: `sudo apt install gcc-mingw-w64`

    *   `$ cd UNPACK_DIR`

    *   Modify PATH and other environment variables so that all these tools
        are runnable from a bash shell. Still standing in `$ERL_TOP`, issue
        the following commands (for 32-bit Windows, remove the x64 from the
        first row and change `otp_win64_%OTP-REL%` to `otp_win32_%OTP-REL%` on
        the last row):

            $ eval `./otp_build env_win32 x64`
            $ ./otp_build autoconf
            $ ./otp_build configure
            $ ./otp_build boot -a
            $ ./otp_build release -a
            $ ./otp_build installer_win32
            $ release/win32/otp_win64_%OTP-REL% /S

    Voila! `Start->Programs->Erlang OTP %OTP-REL%->Erlang` starts the Erlang
    Windows shell.


Tools you Need and Their Environment
------------------------------------

You need some tools to be able to build Erlang/OTP on Windows. Most
notably you'll need WSL (with ubuntu), Visual Studio and
Microsofts Windows SDK, but you might also want a Java compiler, the
NSIS install system, OpenSSL and wxWidgets. Well, here's some information about
the different tools:

*   WSL: Install WSL and Ubuntu in Windows 10
    <https://docs.microsoft.com/en-us/windows/wsl/install-win10>

    We have used and tested with WSL-1, WSL-2 was not available and may
    not be prefered when building Erlang/OTP since access to the windows
    disk is (currently) slower WSL-2.

*   Visual Studio 2019
    Download and run the installer from:
      <http://visualstudio.microsoft.com/downloads>
    Install C++ and SDK packages to the default installation directory.

*   Java JDK 8 or later  (optional)
    If you don't care about Java, you can skip this step. The
    result will be that jinterface is not built.

    Our Java code (jinterface, ic) is tested on windows with JDK 8.
    Get it for Windows and install it, the JRE is not enough.

    URL: <http://www.oracle.com/java/technologies/javase-downloads.html>

    Add javac to your path environment, in my case this means:

        `PATH="/mnt/c/Program\ Files/Java/jdk1.8.0_241/bin:$PATH`

    No `CLASSPATH` or anything is needed. Type `javac.exe` in the bash prompt
    and you should get a list of available Java options.

*   Nullsoft NSIS installer system (optional)
    You need this to build the self installing package.

    Download and run the installer from:
    URL: <http://nsis.sourceforge.net/download>

    Add 'makensis.exe' to your path environment:

         `PATH="/mnt/c/Program\ Files/NSIS/Bin:$PATH`

    Type `which makensis.exe` in the bash prompt and you should get the
    path to the program.

*   OpenSSL (optional)
    You need this to build crypto, ssh and ssl libs.

    We recommend v1.1.1d or later.
    There are prebuilt avaiable binaries, which you can just
    download and install, available here:
    URL: <http://wiki.openssl.org/index.php/Binaries>

    Install into `C:/OpenSSL-Win64` (or `C:/OpenSSL-Win32`)

*   wxWidgets (optional)
    You need this to build wx and use gui's in debugger and observer.

    We recommend v3.1.3 or later.
    Unpack into `c:/opt/local64/pgm/wxWidgets-3.1.3`

    If the `wxUSE_POSTSCRIPT` isn't enabled in  `c:/opt/local64/pgm/wxWidgets-3.1.3/include/wx/msw/setup.h`,
    enable it.

    Build with:

        C:\...\> cd c:\opt\local64\pgm\wxWidgets-3.1.3\build\msw
        C:\...\> nmake TARGET_CPU=amd64 BUILD=release SHARED=0 DIR_SUFFIX_CPU= -f makefile.vc

    Remove the `TARGET_CPU=amd64` for 32bit build.

*   Get the Erlang source distribution (from <http://www.erlang.org/download.html>).
    The same as for Unix platforms. Preferably use tar to
    unpack the source tar.gz (`tar zxf otp_src_%OTP-REL%.tar.gz`) to somewhere
    on the windows disk, `/mnt/c/path/to/otp_src`

    NOTE: It is important that source on the windows disk.

    Set the environment `ERL_TOP` to point to the root directory of the
    source distribution. Let's say I stood in `/mnt/c/src` and unpacked
    `otp_src_%OTP-REL%.tar.gz`, I then add the following to `.profile`:

        ERL_TOP=/mnt/c/src/otp_src_%OTP-REL%
        export ERL_TOP



The Shell Environment
---------------------

The path variable should now contain the windows paths to javac.exe and makensis.exe.

Setup the environment with:

    $ export PATH
    $ cd /mnt/c/path/to/otp_src/
    $ eval `./otp_build env_win32 x64`

This should setup the additional environment variables.

This should do the final touch to the environment and building should
be easy after this. You could run `./otp_build env_win32` without
`eval` just to see what it does, and to see that the environment it
sets seems OK. The path is cleaned of spaces if possible (using DOS
style short names instead), the variables `OVERRIDE_TARGET`, `CC`, `CXX`,
`AR` and `RANLIB` are set to their respective wrappers and the directories
`$ERL_TOP/erts/etc/win32/wsl_tools/vc` and
`$ERL_TOP/erts/etc/win32/wsl_tools` are added first in the PATH.

Now you can check which erlc you have by writing `type erlc` in your shell.
It should reside in `$ERL_TOP/erts/etc/win32/wsl_tools`.

And running `cl.exe` should print the Microsoft compiler usage message.

The needed compiler environment variables are setup inside `otp_build`
via `erts/etc/win32/wsl_tools/SetupWSLcross.bat`. It contains some
hardcoded paths, if your installation path is different it can be added
to that file.


Building and Installing
-----------------------

Building is easiest using the `otp_build` script:

    $ ./otp_build autoconf # Ignore the warning blob about versions of autoconf
    $ ./otp_build configure <optional configure options>
    $ ./otp_build boot -a
    $ ./otp_build release -a <installation directory>
    $ ./otp_build installer_win32 <installation directory> # optional

Now you will have a file called `otp_win32_%OTP-REL%.exe` or `otp_win64_%OTP-REL%.exe`
in the `<installation directory>`, i.e. `$ERL_TOP/release/win32`.

Lets get into more detail:

1.  `$ ./otp_build autoconf` - This step rebuilds the configure scripts
    to work correctly in your environment. In an ideal world, this
    would not be needed, but alas, we have encountered several
    incompatibilities between our distributed configure scripts (generated
    on a Linux platform) and the Cygwin/MSYS/MSYS2/WSL environment over the
    years. Running autoconf in WSL ensures that the configure
    scripts are generated in a compatible way and that they will work well
    in the next step.

2.  `$ ./otp_build configure` - This runs the newly generated configure
    scripts with options making configure behave nicely. The target machine
    type is plainly `win32`, so a lot of the configure-scripts recognize
    this awkward target name and behave accordingly. The CC variable also
    makes the compiler be `cc.sh`, which wraps MSVC++, so all configure
    tests regarding the C compiler gets to run the right compiler. A lot of
    the tests are not needed on Windows, but we thought it best to run the
    whole configure anyway.

3.  `$ ./otp_build boot -a` - This uses the bootstrap directory (shipped
    with the source, `$ERL_TOP/bootstrap`) to build a complete OTP
    system. When this is done you can run erl from within the source tree;
    just type `$ERL_TOP/bin/erl` and you should have the prompt.

4.  `$ ./otp_build release -a` - Builds a commercial release tree from the
    source tree. The default is to put it in `$ERL_TOP/release/win32`. You can
    give any directory as parameter, but it doesn't really
    matter if you're going to build a self extracting installer too.

5.  `$ ./otp_build installer_win32` - Creates the self extracting installer executable.
    The executable `otp_win32_%OTP-REL%.exe` or `otp_win64_%OTP-REL%.exe` will be placed
    in the top directory of the release created in the previous step. If
    no release directory is specified, the release is expected to have
    been built to `$ERL_TOP/release/win32`, which also will be the place
    where the installer executable will be placed. If you specified some
    other directory for the release (i.e. `./otp_build release -a
    /tmp/erl_release`), you're expected to give the same parameter here,
    (i.e. `./otp_build installer_win32 /tmp/erl_release`). You need to have
    a full NSIS installation and `makensis.exe` in your path for this to
    work. Once you have created the installer, you can run it to
    install Erlang/OTP in the regular way, just run the executable and
    follow the steps in the installation wizard. To get all default settings
    in the installation without any questions asked, you run the executable
    with the parameter `/S` (capital S) like in:

        $ cd $ERL_TOP
        $ release/win32/otp_win32_%OTP-REL% /S
        ...

    or

        $ cd $ERL_TOP
        $ release/win32/otp_win64_%OTP-REL% /S
        ...


    and after a while Erlang/OTP-%OTP-REL% will have been installed in
    `C:\Program Files\erl%ERTS-VSN%\`, with shortcuts in the menu etc.


Development
-----------

Once the system is built, you might want to change it. Having a test
release in some nice directory might be useful, but you can also run
Erlang from within the source tree. The target `local_setup`, makes
the program `$ERL_TOP/bin/erl.exe` usable and it also uses all the OTP
libraries in the source tree.

If you hack the emulator, you can build the emulator executable
by standing in `$ERL_TOP/erts/emulator` and do a simple

    $ make opt

Note that you need to have run ``(cd $ERL_TOP && eval `./otp_build env_win32`)``
in the particular shell before building anything on Windows. After
doing a make opt you can test your result by running `$ERL_TOP/bin/erl`.
If you want to copy the result to a release directory (say
`/tmp/erl_release`), you do this (still in  `$ERL_TOP/erts/emulator`)

    $ make TESTROOT=/tmp/erl_release release

That will copy the emulator executables.

To make a debug build of the emulator, you need to recompile both
`beam.dll` (the actual runtime system) and `erlexec.dll`. Do like this

    $ cd $ERL_TOP
    $ rm bin/win32/erlexec.dll
    $ cd erts/emulator
    $ make debug
    $ cd ../etc
    $ make debug

and sometimes

    $ cd $ERL_TOP
    $ make local_setup

So now when you run `$ERL_TOP/erl.exe`, you should have a debug compiled
emulator, which you will see if you do a:

    1> erlang:system_info(system_version).

in the erlang shell. If the returned string contains `[debug]`, you
got a debug compiled emulator.

To hack the erlang libraries, you simply do a `make opt` in the
specific "applications" directory, like:

    $ cd $ERL_TOP/lib/stdlib
    $ make opt

or even in the source directory...

    $ cd $ERL_TOP/lib/stdlib/src
    $ make opt

Note that you're expected to have a fresh Erlang in your path when
doing this, preferably the plain %OTP-REL% you have built in the previous
steps. You could also add `$ERL_TOP/bootstrap/bin` to your `PATH` before
rebuilding specific libraries. That would give you a good enough
Erlang system to compile any OTP erlang code.  Setting up the path
correctly is a little bit tricky. You still need to have
`$ERL_TOP/erts/etc/win32/wsl_tools/vc` and
`$ERL_TOP/erts/etc/win32/wsl_tools` *before* the actual emulator
in the path. A typical setting of the path for using the bootstrap
compiler would be:

    $ export PATH=$ERL_TOP/erts/etc/win32/wsl_tools/vc\
    :$ERL_TOP/erts/etc/win32/wsl_tools:$ERL_TOP/bootstrap/bin:$PATH

That should make it possible to rebuild any library without hassle...

If you want to copy a library (an application) newly built, to a
release area, you do like with the emulator:

    $ cd $ERL_TOP/lib/stdlib
    $ make TESTROOT=/tmp/erlang_release release

Remember that:

*   Windows specific C-code goes in the `$ERL_TOP/erts/emulator/sys/win32`,
    `$ERL_TOP/erts/emulator/drivers/win32` or `$ERL_TOP/erts/etc/win32`.

*   Windows specific erlang code should be used conditionally and the
    host OS tested in *runtime*, the exactly same beam files should be
    distributed for every platform! So write code like:

        case os:type() of
            {win32,_} ->
                do_windows_specific();
            Other ->
                do_fallback_or_exit()
        end,

That's basically all you need to get going.



Frequently Asked Questions
--------------------------

*   Q: So, now I can build Erlang using GCC on Windows?

    A: No, unfortunately not. You'll need Microsoft's Visual C++
    still. A Bourne-shell script (cc.sh) wraps the Visual C++ compiler
    and runs it from within the WSL environment. All other tools
    needed to build Erlang are free-ware/open source, but not the C
    compiler.

*   Q: Why haven't you got rid of VC++ then, you \*\*\*\*\*\*?

    A: Well, partly because it's a good compiler - really! Actually it's
    been possible in late R11-releases to build using mingw instead of
    visual C++ (you might see the remnants of that in some scripts and
    directories). Unfortunately the development of the SMP version for
    Windows broke the mingw build and we chose to focus on the VC++ build
    as the performance has been much better in the VC++ versions. The
    mingw build will possibly be back, but as long as VC++ gives better
    performance, the commercial build will be a VC++ one.

*   Q: OK, you need VC++, but now you've started to demand a quite recent
    (and expensive) version of Visual Studio. Why?

    A: Well, it's not expensive, it's free (as in free beer). Just
    download and install the latest Windows SDK from Microsoft and all
    the tools you need are there. The included debugger (WinDbg) is
    also quite usable. That's what I used when porting Erlang to 64bit
    Windows. Another reason to use later Microsoft compilers is
    DLL compatibility. DLL's using a new version of the standard
    library might not load if the VM is compiled with an old VC++
    version. So we should aim to use the latest freely available SDK
    and compiler.

*   Q: Hah, I saw you, you used GCC even though you said you didn't!

    A: OK, I admit, one of the files is compiled using
    MinGW's GCC and the resulting object code is then converted to MS
    VC++ compatible coff using a small C hack. It's because that
    particular file, `beam_emu.c` benefits immensely from being able
    to use the GCC labels-as-values extension, which boosts emulator
    performance by up to 50%. That does unfortunately not (yet) mean
    that all of OTP could be compiled using GCC. That particular
    source code does not do anything system specific and actually is
    adopted to the fact that GCC is used to compile it on Windows.

*   Q: So now there's a MS VC++ project file somewhere and I can build OTP
    using the nifty VC++ GUI?

    A: No, never. The hassle of keeping the project files up to date and
    do all the steps that constitute an OTP build from within the VC++ GUI
    is simply not worth it, maybe even impossible. A VC++ project
    file for Erlang/OTP will never happen.

*   Q: So how does it all work then?

    A: WSL/Ubuntu is the environment, it's almost like you had a
    virtual Unix machine inside Windows. Configure, given certain
    parameters, then creates makefiles that are used by the
    environment's gnu-make to built the system. Most of the actual
    compilers etc are not, however, WSL tools, so we've written
    a couple of wrappers (Bourne-shell scripts), which reside in
    `$ERL_TOP/etc/win32/wsl_tools`. They all do conversion of
    parameters and switches common in the Unix environment to fit the
    native Windows tools. Most notable is of course the paths, which
    in WSL are Unix-like paths with "forward slashes" (/) and
    no drive letters. The WSL specific command `wslpath` is used
    for most of the path conversions in a WSL environment.
    Luckily most compilers accept forward slashes instead
    of backslashes as path separators, but one still have to get the drive
    letters etc right, though. The wrapper scripts are not general in
    the sense that, for example, cc.sh would understand and translate
    every possible gcc option and pass correct options to
    cl.exe. The principle is that the scripts are powerful enough to
    allow building of Erlang/OTP, no more, no less. They might need
    extensions to cope with changes during the development of Erlang, and
    that's one of the reasons we made them into shell-scripts and not
    Perl-scripts. We believe they are easier to understand and change
    that way.

    In `$ERL_TOP`, there is a script called `otp_build`. That script handles
    the hassle of giving all the right parameters to `configure`/`make` and
    also helps you set up the correct environment variables to work with
    the Erlang source under WSL.

*   Q: Can I build something that looks exactly as the commercial release?

    A: Yes, we use the exact same build procedure.

*   Q: Which version of WSL and other tools do you use then?

    A:  We use WSL 1 with Ubuntu 18.04.
    The GCC we used for %OTP-REL% was version 7.3-win32.
    We used Visual studio 2019, Sun's JDK 1.8.0\_241,
    NSIS 3.05, Win32 OpenSSL 1.1.1d and wxWidgets-3.1.3.


   [1]: http://bugs.erlang.org
   [2]: https://github.com/erlang/otp

   [?TOC]: true
