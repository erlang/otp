How to Build Erlang/OTP on Windows
==================================

Introduction
------------

This file describes how to build the Erlang emulator and the OTP
libraries on Windows. The instructions apply to versions of Windows
supporting the Cygwin emulated gnuish environment for Windows. We've
built on the following platforms: Windows 2000 Professional, Windows
2003 server, Windows XP Home/Professional, and Windows Vista. Any
Windows95'ish platform will surely get you into trouble, what I'm not
sure of, but it certainly will...

The procedure described uses Cygwin as a build environment, you run
the bash shell in Cygwin and uses gnu make/configure/autoconf etc to
do the build. The emulator C-source code is, however, mostly compiled
with Microsoft Visual C++™, producing a native Windows binary. This
is the same procedure as we use to build the pre-built binaries. The
fact that we use VC++ and not gcc is explained further in the FAQ
section.

I describe the build procedure to make it possible for open source
customers to build the emulator, given that they have the needed
tools. The binary Windows releases is still a preferred alternative if
one does not have Microsoft's development tools and/or don't want to
install Cygwin.

To use Cygwin, one needs basic experience from a Unix environment, if
one does not know how to set environment variables, run programs etc
in a Unix environment, one will be quite lost in the Cygwin
ditto. I can unfortunately not teach all the world how to use
Cygwin and bash, neither how to install Cygwin nor perform basic tasks
on a computer. Please refer to other documentation on the net for
help, or use the binary release instead if you have problems using the
tools.

However, if you feel comfortable with the environment and build
system, and have all the necessary tools, you have a great opportunity
to make the Erlang/OTP distribution for Windows better. Please submit
any suggestions and patches to the appropriate [mailing lists] [1] to let
them find their way into the next version of Erlang. If making changes
to the build system (like makefiles etc) please bear in mind that the
same makefiles are used on Unix/VxWorks/OSEDelta, so that your changes
don't break other platforms. That of course goes for C-code too, system
specific code resides in the `$ERL_TOP/erts/emulator/sys/win32` and
`$ERL_TOP/erts/etc/win32` directories mostly. The
`$ERL_TOP/erts/emulator/beam directory` is for common code.

Before the R9C release of Erlang/OTP, the Windows release was built
partly on a Unix (Solaris) box and partly on a Windows box, using Perl
hacks to communicate and sync between the two machines. R9C was the
first release ever built solely on Windows, where no Unix machine is
needed at all. Now we've used this build procedure for a couple of
releases, and it has worked fine for us. Still, there might be all
sorts of troubles on different machines and with different
setups. I'll try to give hints wherever I've encountered difficulties,
but please share your experiences by using the [erlang-questions] [1]
mailing list. I cannot of course help everyone with all
their problems, please try to solve the problems and submit
solutions/workarounds. Remember, it's all about sharing, not about
demanding...

Lets go then, I'll start with a little FAQ, based on in house questions
and misunderstandings.


Frequently Asked Questions
--------------------------

*   Q: So, now I can build Erlang using GCC on Windows?

    A: No, unfortunately not. You'll need Microsoft's Visual C++ still, a
    Bourne-shell script (cc.sh) wraps the Visual C++ compiler and runs it
    from within the Cygwin environment. All other tools needed to build
    Erlang are free-ware/open source, but not the C compiler.

*   Q: Why haven't you got rid of VC++ then, you \*\*\*\*\*\*?

    A: Well, partly because it's a good compiler - really! Actually it's
    been possible in late R11-releases to build using mingw instead of
    visual C++ (you might see the remnants of that in some scripts and
    directories). Unfortunately the development of the SMP version for
    Windows broke the mingw build and we chose to focus on the VC++ build
    as the performance has been much better in the VC++ versions. The
    mingw build will be back, but as long as VC++ gives better
    performance, the commercial build will be a VC++ one.

*   Q: OK, VC++ you need, but now you've started to demand a very recent
    (and expensive) version of Visual studio, not the old and stable VC++
    6.0 that was used in earlier versions. Why?

    A: The SMP version of Erlang needs features in the Visual Studio 2005.
    Can't live without them. Besides the new compiler gives the Erlang
    emulator a ~40% performance boost(!). Alternatively you can build Erlang
    successfully using the free (proprietary) Visual Studio 2008 Express
    edition C++ compiler.

*   Q: Can/will I build a Cygwin binary with the procedure you describe?

    A: No, the result will be a pure Windows binary, and as far as I know,
    it's not possible to make a Cygwin binary yet. That is of course
    something desirable, but there are still some problems with the
    dynamic linking (dynamic Erlang driver loading) as well as the TCP/IP
    emulation in Cygwin, which, I'm sure of, will improve, but still has
    some problems. Fixing those problems might be easy or might be hard.
    I suggest you try yourself and share your experience. No one would be
    happier if a simple `./configure && make` would produce a fully fledged
    Cygwin binary. Ericsson does however not pay me to do a Cygwin port, so
    such a port would have to happen in spare time, which is a limited
    resource...

*   Q: Hah, I saw you, you used GCC even though you said you didn't!

    A: OK, I admit, one of the files is compiled using Cygwin's GCC and
    the resulting object code is then converted to MS VC++ compatible coff
    using a small C hack. It's because that particular file, `beam_emu.c`
    benefits immensely from being able to use the GCC labels-as-values
    extension, which boosts emulator performance by up to 50%. That does
    unfortunately not (yet) mean that all of OTP could be compiled using
    GCC, that particular source code does not do anything system specific
    and actually is adopted to the fact that GCC is used to compile it on
    Windows.

*   Q: So now there's a MS VC++ project file somewhere and I can build OTP
    using the nifty VC++ GUI?

    A: No, never. The hassle of keeping the project files up to date and
    do all the steps that constitute an OTP build from within the VC++ GUI
    is simply not worth it, maybe even impossible. A VC++ project
    file for Erlang/OTP will never happen, at least I will never make
    one. Clicking around in super-multi-tab'd dialogs to add a file or
    compiler option when it's so much easier in a makefile is simply not
    my style.

*   Q: So how does it all work then?

    A: Cygwin is the environment, which closely resembles the environments
    found on any Unix machine. It's almost like you had a virtual Unix
    machine inside Windows. Configure, given certain parameters, then
    creates makefiles that are used by the Cygwin gnu-make to built the
    system. Most of the actual compilers etc are not, however, Cygwin
    tools, so I've written a couple of wrappers (Bourne-shell scripts),
    which reside in `$ERL_TOP/etc/win32/cygwin_tools` and they all do
    conversion of parameters and switches common in the Unix environment
    to fit the native Windows tools. Most notable is of course the paths,
    which in Cygwin are Unix-like paths with "forward slashes" (/) and no
    drive letters, the Cygwin specific command `cygpath` is used for most
    of the path conversions. Luckily most compilers accept forward slashes
    instead of backslashes as path separators, one still have to get the
    drive letters etc right, though. The wrapper scripts are not general
    in the sense that, for example, cc.sh would understand and translates
    every possible gcc option and passes correct options to cl.exe. The
    principle is that the scripts are powerful enough to allow building of
    Erlang/OTP, no more, no less. They might need extensions to cope with
    changes during the development of Erlang, that's one of the reasons I
    made them into shell-scripts and not Perl-scripts, I believe they are
    easier to understand and change that way. I might be wrong though,
    cause another reason I didn't write them in Perl is because I've never
    liked Perl and my Perl code is no pleasant reading...

    In `$ERL_TOP`, there is a script called `otp_build`, that script handles
    the hassle of giving all the right parameters to `configure`/`make` and
    also helps you set up the correct environment variables to work with
    the Erlang source under Cygwin.

*   Q: You use and need Cygwin, but then you haven't taken the time to
    port Erlang to the Cygwin environment but instead focus on your
    commercial release, is that really ethical?

    A: No, not really, but see this as a step in the right direction. I'm
    aiming at GCC compiled emulators and a Cygwin version, but I really
    need to do other things as well... In time, but don't hold your
    breath...

*   Q: Can I build something that looks exactly as the commercial release?

    A: Yes, we use the exactly same build procedure.

*   Q: Which version of Cygwin and other tools do you use then?

    A: For Cygwin we try to use the latest releases available when
    building. What versions you use shouldn't really matter, I try to
    include workarounds for the bugs I've found in different Cygwin
    releases, please help me to add workarounds for new Cygwin-related
    bugs as soon as you encounter them. Also please do submit bug reports
    to the appropriate Cygwin developers. The Cygwin GCC we used for %OTP-REL%
    was version 3.4.4. We used VC++ 8.0 (i.e. Visual studio 2005 SP1),
    Sun's JDK 1.5.0\_17, NSIS 2.37, and Win32 OpenSSL 0.9.8e. Please read
    the next section for details on what you need.

*   Q: Can you help me setup X in Cygwin?

    A: No, unfortunately I haven't got time to help with Cygwin related
    user problems, please read Cygwin related web sites, newsgroups and
    mailing lists.

*   Q: Why is the instruction so long? Is it really that complicated?

    A: Partly it's long because I babble too much, partly because I've
    described as much as I could about the installation of the needed
    tools. Once the tools are installed, building is quite easy. I also
    have tried to make this instruction understandable for people with
    limited Unix experience. Cygwin is a whole new environment to some
    Windows users, why careful explanation of environment variables etc
    seemed to be in place. The short story, for the experienced and
    impatient is:

    *   Get and install complete Cygwin (latest)

    *   (Buy and) Install Microsoft Visual studio 2005 and SP1 (or higher)

    *   Alternatively install the free MS Visual Studio 2008 Express [msvc++]
    and the Windows SDK [32bit-SDK] or [64bit-SDK] depending on the Windows
    platform you are running.

    *   Get and install Sun's JDK 1.4.2

    *   Get and install NSIS 2.01 or higher (up to 2.46 tried and working)

    *   Get and install OpenSSL 0.9.7c or higher (up to 1.0.0a tried & working)

    *   Get the Erlang source distribution (from
        <http://www.erlang.org/download.html>) and unpack with Cygwin's `tar`.

    *   Set `ERL_TOP` to where you unpacked the source distribution

    *   `$ cd $ERL_TOP`

    *   Get (from <http://www.erlang.org/download/tcltk85_win32_bin.tar.gz>)
        and unpack the prebuilt TCL/TK binaries for windows with cygwin tar,
        standing in `$ERL_TOP`

    *   Modify PATH and other environment variables so that all these tools
        are runnable from a bash shell. Still standing in `$ERL_TOP`, issue
        the following commands:

            $ eval `./otp_build env_win32`
            $ ./otp_build autoconf
            $ ./otp_build configure
            $ ./otp_build boot -a
            $ ./otp_build release -a
            $ ./otp_build installer_win32
            $ release/win32/otp_win32_%OTP-REL% /S

    Voila! `Start->Programs->Erlang OTP %OTP-REL%->Erlang` starts the Erlang
    Windows shell.


Tools you Need and Their Environment
------------------------------------

You need some tools to be able to build Erlang/OTP on Windows. Most
notably you'll need Cygwin and Microsoft VC++, but you also might want
a Java compiler, the NSIS install system and OpenSSL. Only VC++ costs
money, but then again it costs a lot of money, I know...
Well' here's the list:

*   Cygwin, the very latest is usually best. Get all the development
    tools and of course all the basic ditto. In fact getting the complete
    package might be a good idea, as you'll start to love Cygwin after a
    while if you're accustomed to Unix. Make sure to get jar and also make
    sure *not* to install a Cygwin'ish Java... The Cygwin jar command is
    used but Sun's Java compiler and virtual machine...

    URL: <http://www.cygwin.com>

    Get the installer from the web site and use that to install
    Cygwin. Be sure to have fair privileges. If you're on a NT domain you
    should consider running `mkpasswd -d` and `mkgroup -d` after the
    installation to get the user databases correct. See their respective
    manual pages.

    When you start you first bash shell, you will get an awful prompt. You
    might also have a `PATH` environment variable that contains backslashes
    and such. Edit `$HOME/.profile` and `$HOME/.bashrc` to set fair prompts
    and set a correct PATH. Also do a `export SHELL` in `.profile`. For some
    non-obvious reason the environment variable `$SHELL` is not exported in
    bash. Also note that `.profile` is run at login time and `.bashrc` when
    sub shells are created. You'll need to explicitly source `.bashrc` from
    `.profile` if you want the commands there to be run at login time (like
    setting up aliases, shell functions and the like). I personally
    usually do like this at the end of `.profile`:

        ENV=$HOME/.bashrc
        export ENV
        . $ENV

    You might also, if you're a hard core type of person at least, want to
    setup X-windows (XFree86), that might be as easy as running startx
    from the command prompt and it might be much harder. Use Google to
    find help...

    If you don't use X-windows, you might want to setup the Windows
    console window by selecting properties in the console system menu
    (upper left corner of the window, the Cygwin icon in the title
    bar). Especially setting a larger screen buffer size (lines) is useful
    as it gets you a scrollbar so you can see whatever error messages
    that might appear...

    If you want to use (t)csh instead of bash you're on your own, I
    haven't tried and know of no one that has. I expect
    that you use bash in all shell examples.

*   Microsoft Visual Studio 2005 SP1. Please don't skip the service
    pack! The installer might update your environment so that you can run
    the `cl` command from the bash prompt, then again it might
    not... There is always a BAT file in VC\Bin under the installation
    directory (default `C:\Program Files\Microsoft Visual Studio 8`) called
    `VCVARS32.BAT`. Either add the environment settings in that file to the
    global environment settings in Windows or add the corresponding BASH
    environment settings to your `.profile`/`.bashrc`. For example, in my case
    I could add the following to `.profile`

        #Visual C++ Root directory as Cygwin style pathname
        VCROOT=/cygdrive/c/Program\ Files/Microsoft\ Visual\ Studio 8

        # Visual C++ Root directory as Windows style pathname
        WIN_VCROOT="C:\\Program Files\\Microsoft Visual Studio 8"

        # The PATH variable should be Cygwin'ish
        PATH=$VCROOT/Common7/IDE:$VCROOT/VC/BIN:$VCROOT/Common7/Tools:\
        $VCROOT/Common7/Tools/bin:$VCROOT/VC/PlatformSDK/bin:$VCROOT/SDK/v2.0/bin:\
        $VCROOT/VC/VCPackages:$PATH

        # Lib and INCLUDE should be Windows'ish
        # Note that semicolon (;) is used to separate Windows style paths but
        # colon (:) to separate Cygwin ditto!

        LIBPATH=$WIN_VCROOT\\VC\\ATLMFC\\LIB

        LIB=$WIN_VCROOT\\VC\\ATLMFC\\LIB\;$WIN_VCROOT\\VC\\LIB\;\
        $WIN_VCROOT\\VC\\PlatformSDK\\lib\;$WIN_VCROOT\\SDK\\v2.0\\lib

        INCLUDE=$WIN_VCROOT\\VC\\ATLMFC\\INCLUDE\;$WIN_VCROOT\\VC\\INCLUDE\;\
        $WIN_VCROOT\\VC\\PlatformSDK\\include

        export PATH LIB INCLUDE

    Make a simple hello world and try to compile it with the `cl` command
    from within bash. If that does not work, your environment needs
    fixing. Also remember to fix up the PATH environment, especially old
    Erlang installations might have inserted quoted paths that Cygwin does
    not understand. Remove or correct such paths. There should be no
    backslashes in your path environment variable in Cygwin bash, but LIB
    and INCLUDE should contain Windows style paths with semicolon,
    drive letters and backslashes.

    If you wish to use Visual Studio 2008, a couple things need to be tweaked,
    namely the fact that some of the SDK stuff is installed in (by default)
    `C:\Program Files\Microsoft SDKs\v6.0A` . Just ensure that that
    `C:\Program Files\Microsoft SDKs\v6.0A\Lib` is in `LIB` and
    `C:\Program Files\Microsoft SDKs\v6.0A\Include` is in `INCLUDE`. A symptom
    of not doing this is errors about finding kernel32.lib and windows.h.

    Additionally, if you encounter errors about mc.exe not being found, you must
    install the entire Windows SDK (the partial SDK included in visual studio
    apparently does not include it). After installing it you'll want to add
    something like: `/c/cygdrive/Program\ Files/Microsoft\ SDKs/v7.0/bin` to
    your `PATH` to allow the environment to find mc.exe. The next Visual Studio
    (2010) is expected to include this tool.

    Alternatively install the free MS Visual Studio 2008 Express [msvc++] and
    the Windows SDK [32bit-SDK] or [64bit-SDK] depending on the Windows
    platform you are running, which includes the missing mc.exe message
    compiler.

[msvc++]:	http://download.microsoft.com/download/E/8/E/E8EEB394-7F42-4963-A2D8-29559B738298/VS2008ExpressWithSP1ENUX1504728.iso
[32bit-SDK]:	http://download.microsoft.com/download/2/E/9/2E911956-F90F-4BFB-8231-E292A7B6F287/GRMSDK_EN_DVD.iso
[64bit-SDK]:	http://download.microsoft.com/download/2/E/9/2E911956-F90F-4BFB-8231-E292A7B6F287/GRMSDKX_EN_DVD.iso

*   Sun's Java JDK 1.5.0 or higher. Our Java code (jinterface, ic) is
    written for JDK 1.5.0. Get it for Windows and install it, the JRE is
    not enough. If you don't care about Java, you can skip this step, the
    result will be that jinterface is not built.

    URL: <http://java.sun.com>

    Add javac *LAST* to your path environment in bash, in my case this means:

        PATH="$PATH:/cygdrive/c/Program Files/Java/jdk1.5.0_17/bin"

    No `CLASSPATH` or anything is needed. Type `javac` at the bash prompt
    and you should get a list of available Java options. Make sure by
    typing `which java` that you use the Java you installed. Note however that
    Cygwin's `jar.exe` is used, that's why the JDK bin-directory should be
    added last in the `PATH`.

*   Nullsoft NSIS installer system. You need this to build the self
    installing package. It's a free open source installer that's much
    nicer to use than the commercial Wise and Install shield
    installers. This is the installer we use for commercial releases as
    well from R9C an on.

    URL: <http://www.nullsoft.com/free/nsis>

    Install the lot, especially the modern user interface components, as
    it's definitely needed. Put `makensis` in your path, in my case:

        PATH=/cygdrive/c/Program\ Files/NSIS:$PATH

    type makensis at the bash prompt and you should get a list of options
    if everything is OK.

*   OpenSSL for Windows. This is if you want the SSL and crypto
    applications to compile (and run). Go to <http://www.openssl.org>, click
    on the `Related` link and then on the `Binaries` link (upper right
    corner of the page last time I looked), you can then reach the
    "Shining Lights Productions" Web site for Windows binaries
    distributions. Get the latest 32-bit installer, or use 0.9.7c if you get
    trouble with the latest, and install to C:\OpenSSL which is where the
    Makefiles are expecting to find it. It's a nifty installer. The rest should
    be handled by `configure`, you needn't put anything in the path or anything.

    If you want to build openssl for windows yourself (which might be
    possible, as you wouldn't be reading this if you weren't a
    compile-it-yourself person), you either have to put the resulting
    DLL's in your path or in the windows system directory and either
    specify where you put the includes etc with the configure-parameter
    `--with-ssl=<cygwin path to the root>` or put your installation directly
    under `c:\OpenSSL`. The directory structure under the installation root
    for OpenSSL is expected to be one with subdirectories named `include`,
    `bin` and `lib`, possibly with a `VC` subdirectory of `lib` containing
    the actual `.lib` files. Note that the cygwin distributed OpenSSL cannot be
    used, it results in cygwin depending binaries and it has unix style
    archives (`.a`, not `.lib`).

*   Building with wxWidgets. Download wxWidgets-2.8.9 or higher patch
    release (2.9.\*  is a developer release which currently does not work
    with wxErlang).

    Install or unpack it to `DRIVE:/PATH/cygwin/opt/local/pgm`.
    Open from explorer (i.e. by double clicking the file)
    `C:\cygwin\opt\local\pgm\wxMSW-2.8.11\build\msw\wx.dsw`
    In Microsoft Visual Studio, click File/Open/File, locate and
    open:  `C:\cygwin\opt\local\pgm\wxMSW-2.8.11\include\wx\msw\setup.h`
    enable `wxUSE_GLCANVAS`, `wxUSE_POSTSCRIPT` and `wxUSE_GRAPHICS_CONTEXT`
    Build it by clicking Build/Batch Build and select all unicode release
    (and unicode debug) packages.

    Open `C:\cygwin\opt\local\pgm\wxMSW-2.8.11\contrib/build/stc/stc.dsw`
    and batch build all unicode packages.

    If you are using Visual C++ 9.0 or higher (Visual Studio 2008 onwards) you
    will also need to convert and re-create the project dependencies in the new
    .sln "Solution" format.

    * Open VSC++ & the project  `wxMSW-2.8.11\build\msw\wx.dsw`, accepting the
    automatic conversion to the newer VC++ format and save as
    `\wxMSW-2.8.11\build\msw\wx.sln`

    * right-click on the project, and set up the project dependencies for
    `wx.dsw` to achieve the below build order

            jpeg, png, tiff, zlib, regex, expat, base, net, odbc, core,
            gl, html, media, qa, adv, dbgrid, xrc, aui, richtext, xml

    Build all unicode release (and unicode debug) packages either from the
    GUI or alternatively launch a new prompt from somewhere like Start ->
    Programs -> Microsoft Visual C++ -> Visual Studio Tools -> VS2008 Cmd Prompt
    and cd to where you unpacked wxMSW

            pushd c:\wxMSW*\build\msw
            vcbuild /useenv /platform:Win32 /M4 wx.sln "Unicode Release|Win32"
            vcbuild /useenv /platform:Win32 /M4 wx.sln "Unicode Debug|Win32"

    Open VSC++ & convert `C:\wxMSW-2.8.11\contrib\build\stc\stc.dsw` to
    `C:\wxMSW-2.8.11\contrib\build\stc\stc.sln`

    * build the unicode release (and unicode debug) packages from the GUI or
    alternatively open a VS2008 Cmd Prompt and cd to where you unpacked wxMSW

            pushd c:\wxMSW*\contrib\build\stc
            vcbuild /useenv /platform:Win32 /M4 stc.sln "Unicode Release|Win32"
            vcbuild /useenv /platform:Win32 /M4 stc.sln "Unicode Debug|Win32"

*   The Erlang source distribution (from <http://www.erlang.org/download.html>).
    The same as for Unix platforms. Preferably use tar from within Cygwin to
    unpack the source tar.gz (`tar zxf otp_src_%OTP-REL%.tar.gz`).

    set the environment `ERL_TOP` to point to the root directory of the
    source distribution. Let's say I stood in `$HOME/src` and unpacked
    `otp_src_%OTP-REL%.tar.gz`, I then add the following to `.profile`:

        ERL_TOP=$HOME/src/otp_src_%OTP-REL%
        export $ERL_TOP

*   The TCL/TK binaries. You could compile Tcl/Tk for windows yourself,
    but you can get a stripped down version from our website which is
    suitable to include in the final binary package. If you want to supply
    tcl/tk yourself, read the instructions about how the tcl/tk tar file
    used in the build is constructed under `$ERL_TOP/lib/gs/tcl`. The easy
    way is to download <http://www.erlang.org/download/tcltk85_win32_bin.tar.gz>
    and unpack it standing in the `$ERL_TOP` directory. This will create the
    file `win32.tar.gz` in `$ERL_TOP/lib/gs/tcl/binaries`.

    One last alternative is to create a file named `SKIP` in the
    `$ERL_TOP/lib/gs/` after configure is run, but that will give you an
    erlang system without gs (which might be okay as you probably will use
    wx anyway).

The Shell Environment
---------------------

So, if you have followed the instructions above, when you start a bash
shell, you should have an INCLUDE environment with a Windows style
path, a LIB environment variable also in Windows style, and finally a
PATH that let's you reach cl, makensis, javac etc from the
command prompt (use `which cl` etc to verify from bash).

You should also have an `ERL_TOP` environment variable that is *Cygwin
style*, and points to a directory containing, among other files, the
script `otp_build`.

A final massage of the environment is needed, and that is done by
the script `$ERL_TOP/otp_build`. Start bash and do the following, note
the "back-ticks" (\`), can be quite hard to get on some keyboards, but
pressing the back-tick key followed by the space bar might do it...

    $ cd $ERL_TOP
    $ eval `./otp_build env_win32`

If you're unable to produce back-ticks on your keyboard, you can use
the ksh variant:

    $ cd $ERL_TOP
    $ eval $(./otp_build env_win32)

This should do the final touch to the environment and building should
be easy after this. You could run `./otp_build env_win32` without
`eval` just to see what it does, and to see that the environment it
sets seems OK. The path is cleaned of spaces if possible (using DOS
style short names instead), the variables `OVERRIDE_TARGET`, `CC`, `CXX`,
`AR` and `RANLIB` are set to their respective wrappers and the directories
`$ERL_TOP/erts/etc/win32/cygwin_tools/vc` and
`$ERL_TOP/erts/etc/win32/cygwin_tool` are added first in the PATH.

Try now a `which erlc`. That should result in the erlc wrapper script
(which does not have the .sh extension, for reasons best kept
untold...). It should reside in `$ERL_TOP/erts/etc/win32/cygwin_tools`.
You could also try `which cc.sh`, which `ar.sh` etc.

Now you're ready to build...


Building and Installing
-----------------------

Now it's assumed that you have executed `` eval `./otp_build env_win32` ``
for this particular shell...

Building is easiest using the `otp_build` script. That script takes care
of running configure, bootstrapping etc on Windows in a simple
way. The `otp_build` script is the utility we use ourselves to build on
different platforms and it therefore contains code for all sorts of
platforms. The principle is, however, that for non-Unix platforms, one
uses `./otp_build env_<target>` to set up environment and then the
script knows how to build on the platform "by itself". You've already
run `./otp_build env_win32` in the step above, so now it's mostly like
we build on any platform. OK, here are then steps; Assuming you will
want to build a full installation executable with NSIS, you can omit
`<installation directory>` and the release will be copied to
`$ERL_TOP/release/win32`: and there is where the packed self installing
executable will reside too.

    $ ./otp_build autoconf # Ignore the warning blob about versions of autoconf
    $ ./otp_build configure <optional configure options>
    $ ./otp_build boot -a
    $ ./otp_build release -a <installation directory>
    $ ./otp_build installer_win32 <installation directory> # optional

Now you will have a file called `otp_win32_R12B.exe` in the
`<installation directory>`, i.e. `$ERL_TOP/release/win32`.

Lets get into more detail:

1.  `$ ./otp_build autoconf` - This step rebuilds the configure scripts
    to work correctly in the cygwin environment. In an ideal world, this
    would not be needed, but alas, we have encountered several
    incompatibilities between our distributed configure scripts (generated
    on a Linux platform) and the cygwin environment over the
    years. Running autoconf on cygwin ensures that the configure scripts
    are generated in a cygwin-compatible way and that they will work well
    in the next step.

2.  `$ ./otp_build configure` - This runs the newly generated configure
    scripts with options making configure behave nicely. The target machine
    type is plainly `win32`, so a lot of the configure-scripts recognize
    this awkward target name and behave accordingly. The CC variable also
    makes the compiler be `cc.sh`, which wraps MSVC++, so all configure
    tests regarding the C compiler gets to run the right compiler. A lot of
    the tests are not needed on Windows, but I thought it best to run the
    whole configure anyway. The only configure option you might want to
    supply is `--with-ssl`, which might be needed if you have built your
    own OpenSSL distribution. The Shining Lights distribution should be
    found automatically by `configure`, if that fails, add a
    `--with-ssl=<dir>` that specifies the root directory of your OpenSSL
    installation.

3.  `$ ./otp_build boot -a` - This uses the bootstrap directory (shipped
    with the source, `$ERL_TOP/bootstrap`) to build a complete OTP
    system. It first builds an emulator and sets up a minimal OTP system
    under `$ERL_TOP/bootstrap`, then starts to compile the different OTP
    compilers to make the `$ERL_TOP/bootstrap` system potent enough to be
    able to compile all Erlang code in OTP. Then, all Erlang and C code
    under `$ERL_TOP/lib` is built using the bootstrap system, giving a
    complete OTP system (although not installed). When this is done, one
    can run Erlang from within the source tree, just type `$ERL_TOP/bin/erl`
    and you should have a prompt. If you omit the -a flag, you'll get a
    smaller system, that might be useful during development. Now
    exit from Erlang and start making a release of the thing:

4.  `$ ./otp_build release -a` - Builds a commercial release tree from the
    source tree, default is to put it in `$ERL_TOP/release/win32`, you can
    give any directory as parameter (Cygwin style), but it doesn't really
    matter if you're going to build a self extracting installer too. You
    could of course build release to the final directory and then run
    `./Install.exe` standing in the directory where the release was put,
    that will create a fully functional OTP installation. But let's make
    the nifty installer:

5.  `$ ./otp_build installer_win32` - Create the self extracting installer
    executable. The executable `otp_win32_%OTP-REL%.exe` will be placed
    in the top directory of the release created in the previous step. If
    no release directory is specified, the release is expected to have
    been built to `$ERL_TOP/release/win32`, which also will be the place
    where the installer executable will be placed. If you specified some
    other directory for the release (i.e. `./otp_build release -a
    /tmp/erl_release`), you're expected to give the same parameter here,
    (i.e. `./otp_build installer_win32 /tmp/erl_release`). You need to have
    a full NSIS installation and `makensis.exe` in your path for this to
    work of course. Once you have created the installer, you can run it to
    install Erlang/OTP in the regular way, just run the executable and
    follow the steps in the installation wizard. To get all default settings
    in the installation without any questions asked, you run the executable
    with the parameter `/S` (capital S) like in:

        $ cd $ERL_TOP
        $ release/win32/otp_win32_%OTP-REL% /S
        ...

    and after a while Erlang/OTP-%OTP-REL% will have been installed in
    `C:\Program Files\erl%ERTS-VSN%\`, with shortcuts in the menu etc.

    The necessary setup of an Erlang installation is actually done by the
    program `Install.exe`, which resides in the release top. That program
    creates `.ini`-files and copies the correct boot scripts. If one has
    the correct directory tree (like after a `./otp_build release -a`), only
    the running of `Install.exe` is necessary to get a fully functional
    OTP. What the self extracting installer adds is (of course) the
    possibility to distribute the binary easily, together with adding
    shortcuts to the Windows start menu. There is also some adding of
    entries in the registry, to associate `.erl` and `.beam` files with
    Erlang and get nifty icons, but that's not something you'll really need
    to run Erlang. The registry is also used to store uninstall information,
    but if one has not used the self extracting installer, one cannot
    (need not) do any uninstall, one just scratches the release directory
    and everything is gone. Erlang/OTP does not *need* to put anything
    in the Windows registry at all, and does not if you don't use the self
    extracting installer. In other words the installer is pure cosmetics.

> *NOTE*: Beginning with R9C, the Windows installer does *not* add Erlang
> to the system wide path. If one wants to have Erlang in the path, one
> has to add it by hand.

Development
-----------

Once the system is built, you might want to change it. Having a test
release in some nice directory might be useful, but you also can run
Erlang from within the source tree. The target `local_setup`, makes
the program `$ERL_TOP/bin/erl.exe` usable and it also uses all the OTP
libraries in the source tree.

If you hack the emulator, you can then build the emulator executable
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

Note that you're expected o have a fresh Erlang in your path when
doing this, preferably the plain %OTP-REL% you have built in the previous
steps. You could also add `$ERL_TOP/bootstrap/bin` to your `PATH` before
rebuilding specific libraries, that would give you a good enough
Erlang system to compile any OTP erlang code.  Setting up the path
correctly is a little bit tricky, you still need to have
`$ERL_TOP/erts/etc/win32/cygwin_tools/vc` and
`$ERL_TOP/erts/etc/win32/cygwin_tools` *before* the actual emulator
in the path. A typical setting of the path for using the bootstrap
compiler would be:

    $ export PATH=$ERL_TOP/erts/etc/win32/cygwin_tools/vc\
    :$ERL_TOP/erts/etc/win32/cygwin_tools:$ERL_TOP/bootstrap/bin:$PATH

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

Final Words
-----------
My hope is that the possibility to build the whole system on Windows
will open up for free development on this platform too. There are many
things one might want to do better in the Windows version, like the
window-style command prompt as well as pure Cygwin porting. Although i
realize it's a much larger step to start building on Windows (with all
the software you need) than for instance on Linux, I sincerely hope
that some of you will make the effort and start submitting Windows
friendly patches.

The first build system for Erlang using Cygwin on Windows was created
by Per Bergkvist. I haven't used his build system, but it's rumored to
be good. The idea to do this came from his work, so credit is well
deserved.

Of course this would have been completely impossible without the
excellent Cygwin package. The guys at Cygnus solutions and Redhat
deserves a huge THANKS! as well as all the other people in the free
software community who have helped in creating the magnificent
software that constitutes Cygwin.

Good luck and Happy Hacking,
Patrik, OTP

Copyright and License
---------------------

%CopyrightBegin%

Copyright Ericsson AB 2003-2010. All Rights Reserved.

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

Modifying This Document
-----------------------

Before modifying this document you need to have a look at the
`$ERL_TOP/README.md.txt` document.



   [1]: http://www.erlang.org/faq.html "mailing lists"

   [?TOC]: true
