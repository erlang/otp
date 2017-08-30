How to Build Erlang/OTP on Windows
==================================

Introduction
------------

This section describes how to build the Erlang emulator and the OTP 
libraries on Windows. Note that the Windows binary releases are still 
a preferred alternative if one does not have Microsoft’s development 
tools and/or don’t want to install Cygwin, MSYS or MSYS2.

The instructions apply to versions of Windows supporting the Cygwin 
emulated gnuish environment or the MSYS or MSYS2 ditto. We’ve built on 
the following platforms: Windows 2012, Windows 7, Windows 8 and Windows 10. 
It’s probably possible to build on older platforms too, but you might 
not be able to install the appropriate Microsoft SDK, Visual Studio or 
OpenSSL, in which case you will need to go back to earlier compilers etc.  

The procedure described uses either Cygwin, MSYS or MSYS2 as a build 
environment. You run the bash shell in Cygwin/MSYS/MSYS2 and use the gnu 
make/configure/autoconf etc to do the build. The emulator C-source code 
is, however, mostly compiled with Microsoft Visual C++™, producing a 
native Windows binary. This is the same procedure as we use to build the 
pre-built binaries. Why we use VC++ and not gcc is explained further in 
the FAQ section. 

If you are not familiar with Cygwin, MSYS, MSYS2 or a Unix environment, 
you’ll probably need to read up a bit on how that works. There are plenty of 
documentation about this online.

These instructions apply for both 32-bit and 64-bit Windows. Note that even 
if you build a 64-bit version of Erlang, most of the directories and files 
involved are still named win32. Some occurances of the name win64 are 
however present. The installation file for a 64-bit Windows version of 
Erlang, for example, is `otp_win64_%OTP-REL%.exe`.

If you feel comfortable with the environment and build
system, and have all the necessary tools, you have a great opportunity
to make the Erlang/OTP distribution for Windows better. Please submit
any suggestions to our [JIRA] [2] and patches to our [git project] [3] to let
them find their way into the next version of Erlang. If making changes
to the build system (like makefiles etc) please bear in mind that the
same makefiles are used on Unix/VxWorks, so that your changes
don't break other platforms. That of course goes for C-code too; system
specific code resides in the `$ERL_TOP/erts/emulator/sys/win32` and
`$ERL_TOP/erts/etc/win32` directories mostly. The
`$ERL_TOP/erts/emulator/beam` directory is for common code.

We've used this build procedure for a couple of
releases, and it has worked fine for us. Still, there might be all
sorts of troubles on different machines and with different
setups. We'll try to give hints wherever we've encountered difficulties,
but please share your experiences by using the [erlang-questions] [1]
mailing list. We cannot, of course, help everyone with all
their issues, so please try to solve such issues and submit
solutions/workarounds.

Lets go then! We’ll start with a short version of the setup procedure, 
followed by some FAQ, and then we’ll go into more details of the setup. 


Short Version
-------------

In the following sections, we've described as much as we could about the 
installation of the tools needed. Once the tools are installed, building 
is quite easy. We have also tried to make these instructions understandable 
for people with limited Unix experience. Cygwin/MSYS/MSYS2 is a whole new 
environment to some Windows users, why careful explanation of environment 
variables etc seemed to be in place. 

This is the short story though, for the experienced and impatient:

    *   Get and install complete Cygwin (latest), complete MinGW with MSYS or 
        complete MSYS2

    *   Install Visual Studio 12.0 (2013)

    *   Install Microsofts Windows SDK 8.1 

    *   Get and install Sun's JDK 1.6.0 or later

    *   Get and install NSIS 2.01 or later (up to 2.46 tried and working)

    *   Get, build and install OpenSSL 0.9.8r or later (up to 1.0.2d
        tried & working) with static libs.

    *   Get the Erlang source distribution (from
        <http://www.erlang.org/download.html>) and unpack with 
        Cygwin's/MSYS's/MSYS2's `tar`.

    *   Set `ERL_TOP` to where you unpacked the source distribution

    *   `$ cd $ERL_TOP`

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


Frequently Asked Questions
--------------------------

*   Q: So, now I can build Erlang using GCC on Windows?

    A: No, unfortunately not. You'll need Microsoft's Visual C++
    still. A Bourne-shell script (cc.sh) wraps the Visual C++ compiler
    and runs it from within the Cygwin environment. All other tools
    needed to build Erlang are free-ware/open source, but not the C
    compiler. The Windows SDK is however enough to build Erlang, you
    do not need to buy Visual C++, just download the SDK (SDK version
    8.1 == Visual studio 2013).

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

*   Q: Can/will I build a Cygwin binary with the procedure you describe?

    A: No, the result will be a pure Windows binary, and as far as I know,
    it's not possible to make a Cygwin binary yet. That is of course
    something desirable, but there are still some problems with the
    dynamic linking (dynamic Erlang driver loading) as well as the TCP/IP
    emulation in Cygwin, which, I'm sure of, will improve, but still has
    some problems. Fixing those problems might be easy or might be hard.
    I suggest you try yourself and share your experience. No one would be
    happier if a simple `./configure && make` would produce a fully fledged
    Cygwin binary. 

*   Q: Hah, I saw you, you used GCC even though you said you didn't!

    A: OK, I admit, one of the files is compiled using Cygwin's or
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

    A: Cygwin, MSYS or MSYS2 is the environment, which closely resembles the
    environment found on any Unix machine. It's almost like you had a
    virtual Unix machine inside Windows. Configure, given certain
    parameters, then creates makefiles that are used by the
    environment's gnu-make to built the system. Most of the actual
    compilers etc are not, however, Cygwin/MSYS/MSYS2 tools, so we've written
    a couple of wrappers (Bourne-shell scripts), which reside in
    `$ERL_TOP/etc/win32/cygwin_tools` and
    `$ERL_TOP/etc/win32/msys_tools`. They all do conversion of
    parameters and switches common in the Unix environment to fit the
    native Windows tools. Most notable is of course the paths, which
    in Cygwin/MSYS/MSYS2 are Unix-like paths with "forward slashes" (/) and
    no drive letters. The Cygwin specific command `cygpath` is used
    for most of the path conversions in a Cygwin environment. Other
    tools are used (when needed) in the corresponding MSYS and MSYS2
    environment. Luckily most compilers accept forward slashes instead
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
    the Erlang source under Cygwin/MSYS/MSYS2.

*   Q: You use and need Cygwin, but then you haven't taken the time to
    port Erlang to the Cygwin environment but instead focus on your
    commercial release, is that really ethical?

    A: No, not really, but see this as a step in the right direction. 

*   Q: Can I build something that looks exactly as the commercial release?

    A: Yes, we use the exact same build procedure.

*   Q: Which version of Cygwin/MSYS/MSYS2 and other tools do you use then?

    A: For Cygwin, MSYS and MSYS2 alike, we try to use the latest releases
    available when building. What versions you use shouldn't really
    matter. We try to include workarounds for the bugs we've found in
    different Cygwin/MSYS/MSYS2 releases. Please help us add workarounds
    for new Cygwin/MSYS/MSYS2-related bugs as soon as you encounter
    them. Also please do submit bug reports to the appropriate Cygwin, MSYS
    and/or MSYS2 developers. The GCC we used for %OTP-REL% was version
    4.8.1 (MinGW 32bit) and 4.8.5 (MSYS2 64bit). We used  VC++ 12.0 
    (i.e. Visual studio 2013), Sun's JDK 1.6.0\_45 (32bit) and Sun's
    JDK 1.7.0\_1 (64bit), NSIS 2.46, and Win32 OpenSSL 1.0.2d. Please
    read the next section for details on what you need.

*   Q: Can you help me setup X in Cygwin/MSYS/MSYS2?

    A: No, unfortunately we haven't got time to help with Cygwin/MSYS/MSYS2 
    related user problems, please read related websites, newsgroups and
    mailing lists.


Tools you Need and Their Environment
------------------------------------

You need some tools to be able to build Erlang/OTP on Windows. Most
notably you'll need Cygwin, MSYS or MSYS2, Visual Studio and Microsofts 
Windows SDK, but you might also want a Java compiler, the NSIS install 
system and OpenSSL. Well, here's some information about the different 
tools:

*   Cygwin, the very latest is usually best. Get all the development
    tools and of course all the basic ditto. Make sure to get jar and 
    also make sure *not* to install a Cygwin'ish Java, since the Cygwin 
    jar command is used but Sun's Java compiler and virtual machine.

    If you are going to build a 64bit Windows version, you should make
    sure to get MinGW's 64bit gcc installed with Cygwin. It's in one of
    the development packages.

    URL: <http://www.cygwin.com>

    Get the installer from the website and use it to install
    Cygwin. Be sure to have fair privileges. If you're on an NT domain you
    should consider running `mkpasswd -d` and `mkgroup -d` after the
    installation to get the user databases correct. See their respective
    manual pages.

    When you start your first bash shell, you will get an awful prompt. You
    might also have a `PATH` environment variable that contains backslashes
    and such. Edit `$HOME/.profile` and `$HOME/.bashrc` to set fair prompts
    and a correct PATH. Also do an `export SHELL` in `.profile`. For some
    non-obvious reason the environment variable `$SHELL` is not exported in
    bash. Also note that `.profile` is run at login time and `.bashrc` when
    sub shells are created. You'll need to explicitly source `.bashrc` from
    `.profile` if you want the commands there to be run at login time (like
    setting up aliases, shell functions and the like). You can for example 
    do like this at the end of `.profile`:

        ENV=$HOME/.bashrc
        export ENV
        . $ENV

    You might also want to setup X-windows (XFree86). That might be as easy 
    as running startx from the command prompt and it might be much harder. 
    Use Google to find help.

    If you don't use X-windows, you might want to setup the Windows
    console window by selecting properties in the console system menu
    (upper left corner of the window, the Cygwin icon in the title
    bar). Especially setting a larger screen buffer size (lines) is useful
    as it gets you a scrollbar so you can see whatever error messages
    that might appear.

    There are a few other shells available, but in all examples below we assume
    that you use bash.

*   Alternatively you download MinGW and MSYS. You'll find the latest
    installer at:

    URL: <http://sourceforge.net/projects/mingw/files/Installer/mingw-get-inst/>

    Make sure to install the basic dev tools, but avoid the MinGW autoconf and
    install the msys one instead.  

    To be able to build the 64bit VM, you will also need the 64bit
    MinGW compiler from:

    URL: <http://sourceforge.net/projects/mingw-w64/files/latest/download?source=files>

    We've tried up to 1.0, but the latest version should do. Make sure you 
    download the `mingw-w64-bin_i686-mingw_<something>.zip`, not a linux
    version. You unzip the package on top of your MinGW installation
    (`c:\MinGW`) and that's it.

*   A third alternative is to download and install MSYS2 from:

    URL: <https://msys2.github.io/>

    When you've followed the instructions there, you also need to install 
    these packages: autoconf, make, perl, and tar. You do so by running 
    the following in the msys console:

        pacman -S msys/autoconf msys/make msys/perl msys/tar 

    You also need a gcc. If you installed the 64 bit MSYS2 you run:

        mingw64/mingw-w64-x86_64-gcc

    And for 32 bit MSYS2:

        pacman -S mingw32/mingw-w64-i686-gcc
        pacman -S mingw-w64-i686-editrights

*   Visual Studio 2013 (Visual Studio 12.0). Download and run the web 
    installer from: 

        https://www.visualstudio.com/

*   Microsofts Windows SDK version 8.1 (corresponding to VC++ 12.0 and
    Visual Studio 2013). You'll find it here:
    
    URL: <https://msdn.microsoft.com/en-us/windows/desktop/bg162891.aspx>

*   To help setup the environment, there is a bat file, 
    `%PROGRAMFILES%\Mirosoft Visual Studio 12.0\VC\vcvarsall.bat`, 
    that set's the appropriate
    environment for a Windows command prompt. This is not appropriate
    for bash, so you'll need to convert it to bash-style environments
    by editing your `.bash_profile`. In my case, where the SDK is
    installed in the default directory and `%PROGRAMFILES%` is
    `C:\Program Files`, the commands for setting up a 32bit build
    environment (on a 64bit or 32bit machine) look like this (in Cygwin):

        # Some common paths
        C_DRV=/cygdrive/c
        PRG_FLS=$C_DRV/Program\ Files

        # nsis
        NSIS_BIN=$PRG_FLS/NSIS
        # java
        JAVA_BIN=$PROGRAMFILES/Java/jdk1.7.0_02/bin

        ##
        ## MS SDK
        ##

        CYGWIN=nowinsymlinks 

        VISUAL_STUDIO_ROOT=$PRG_FLS/Microsoft\ Visual\ Studio\ 12.0
        WIN_VISUAL_STUDIO_ROOT="C:\\Program Files\\Microsoft Visual Studio 12.0"
        SDK=$PRG_FLS/Windows\ Kits/8.1
        WIN_SDK="C:\\Program Files\\Windows Kits\\8.1"

        PATH="$NSIS_BIN:\
        $VISUAL_STUDIO_ROOT/VC/bin:\
        $VISUAL_STUDIO_ROOT/VC/vcpackages:\
        $VISUAL_STUDIO_ROOT/Common7/IDE:\
        $VISUAL_STUDIO_ROOT/Common7/Tools:\
        $SDK/bin/x86
        /usr/local/bin:/usr/bin:/bin:\
        /cygdrive/c/WINDOWS/system32:/cygdrive/c/WINDOWS:\
        /cygdrive/c/WINDOWS/system32/Wbem:\
        $JAVA_BIN"

        LIBPATH="$WIN_VISUAL_STUDIO_ROOT\\VC\\lib"

        LIB="$WIN_VISUAL_STUDIO_ROOT\\VC\\lib\\;$WIN_SDK\\lib\\winv6.3\\um\\x86"

        INCLUDE="$WIN_VISUAL_STUDIO_ROOT\\VC\\include\\;$WIN_SDK\\include\\shared\\;
        $WIN_SDK\\include\\um;$WIN_SDK\\include\\winrt\\;$WIN_SDK\\include\\um\\gl"

        export CYGWIN PATH LIBPATH LIB INCLUDE

    If you're using MinGW's MSYS instead, you need to change the `C_DRV` setting, 
    which would read:

        C_DRV=/c

    and you also need to change the PATH environment variable to: 

        MINGW_BIN=/c/MinGW/bin


        PATH="$NSIS_BIN:\
        $VISUAL_STUDIO_ROOT/VC/bin:\
        $VISUAL_STUDIO_ROOT/VC/vcpackages:\
        $VISUAL_STUDIO_ROOT/Common7/IDE:\
        $VISUAL_STUDIO_ROOT/Common7/Tools:\
        $SDK/bin/x86:/usr/local/bin:\
        $MINGW_BIN:\
        /bin:/c/Windows/system32:/c/Windows:\
        /c/Windows/System32/Wbem:\
        $JAVA_BIN"

    For MSYS2 you use the same `C_DRV` and PATH as for MSYS, only update the `MINGW_BIN`:

        MINGW_BIN=/mingw32/bin
 

    If you are building a 64 bit version of Erlang, you should set up 
    PATHs etc a little differently. We have two templates to make things 
    work in both Cygwin and MSYS but needs editing to work with MSYS2 (see the 
    comments in the script). 
    The following one is for 32 bits:
 
        make_winpath()
        { 
            P=$1
            if [ "$IN_CYGWIN" = "true" ]; then
                cygpath -d "$P"
            else
                (cd "$P" && /bin/cmd //C "for %i in (".") do @echo %~fsi")
            fi
        }

        make_upath()
        {
            P=$1
            if [ "$IN_CYGWIN" = "true" ]; then
                cygpath "$P"
            else
                echo "$P" | /bin/sed 's,^\([a-zA-Z]\):\\,/\L\1/,;s,\\,/,g'
            fi
        }

        # Some common paths
        if [ -x /usr/bin/msys-?.0.dll ]; then
          # Without this the path conversion won't work
          COMSPEC='C:\Windows\System32\cmd.exe'
          MSYSTEM=MINGW32  # Comment out this line if in MSYS2
          export MSYSTEM COMSPEC
          # For MSYS2: Change /mingw/bin to the msys bin dir on the line below
          PATH=/usr/local/bin:/mingw/bin:/bin:/c/Windows/system32:\
          /c/Windows:/c/Windows/System32/Wbem
          C_DRV=/c
          IN_CYGWIN=false
        else
          PATH=/ldisk/overrides:/usr/local/bin:/usr/bin:/bin:\
          /usr/X11R6/bin:/cygdrive/c/windows/system32:\
          /cygdrive/c/windows:/cygdrive/c/windows/system32/Wbem
          C_DRV=/cygdrive/c
          IN_CYGWIN=true
        fi

        obe_otp_gcc_vsn_map="
            .*=>default 
        "   
        obe_otp_64_gcc_vsn_map="
            .*=>default
        "
        # Program Files 
        PRG_FLS=$C_DRV/Program\ Files

        # Visual Studio
        VISUAL_STUDIO_ROOT=$PRG_FLS/Microsoft\ Visual\ Studio\ 12.0
        WIN_VISUAL_STUDIO_ROOT="C:\\Program Files\\Microsoft Visual Studio 12.0"

        # SDK
        SDK=$PRG_FLS/Windows\ Kits/8.1
        WIN_SDK="C:\\Program Files\\Windows Kits\\8.1"

        # NSIS
        NSIS_BIN=$PROGRAMFILES/NSIS

        # Java 
        JAVA_BIN=$PROGRAMFILES/Java/jdk1.7.0_02/bin

        ## The PATH variable should be Cygwin'ish
        VCPATH=
        $VISUAL_STUDIO_ROOT/VC/bin:\
        $VISUAL_STUDIO_ROOT/VC/vcpackages:\
        $VISUAL_STUDIO_ROOT/Common7/IDE:\
        $VISUAL_STUDIO_ROOT/Common7/Tools:\
        $SDK/bin/x86

        ## Microsoft SDK libs
        LIBPATH=$WIN_VISUAL_STUDIO_ROOT\\VC\\lib

        LIB=$WIN_VISUAL_STUDIO_ROOT\\VC\\lib\\;$WIN_KITS\\lib\\winv6.3\\um\\x86

        INCLUDE=$WIN_VISUAL_STUDIO_ROOT\\VC\\include\\;\
        $WIN_KITS\\include\\shared\\;$WIN_KITS\\include\\um;\
        $WIN_KITS\\include\\winrt\\;$WIN_KITS\\include\\um\\gl

        # Put nsis, c compiler and java in path
        export PATH=$VCPATH:$PATH:$JAVA_BIN:$NSIS_BIN

        # Make sure LIB and INCLUDE is available for others
        export LIBPATH LIB INCLUDE



    The first part of the 64 bit template is identical to the 32 bit one, 
    but there are some environment variable differences: 

        # Program Files
        PRG_FLS64=$C_DRV/Program\ Files
        PRG_FLS32=$C_DRV/Program\ Files\ \(x86\)

        # Visual Studio
        VISUAL_STUDIO_ROOT=$PRG_FLS32/Microsoft\ Visual\ Studio\ 12.0
        WIN_VISUAL_STUDIO_ROOT="C:\\Program Files (x86)\\Microsoft Visual Studio 12.0"

        # SDK
        SDK=$PRG_FLS32/Windows\ Kits/8.1
        WIN_SDK="C:\\Program Files (x86)\\Windows Kits\\8.1"

        # NSIS
        NSIS_BIN=$PROGRAMFILES/NSIS
        # Java 
        JAVA_BIN=$PROGRAMFILES/Java/jdk1.7.0_02/bin

        ## The PATH variable should be Cygwin'ish
        VCPATH=
        $VISUAL_STUDIO_ROOT/VC/bin/amd64:\
        $VISUAL_STUDIO_ROOT/VC/vcpackages:\
        $VISUAL_STUDIO_ROOT/Common7/IDE:\
        $VISUAL_STUDIO_ROOT/Common7/Tools:\
        $SDK/bin/x86

        ## Microsoft SDK libs
        LIBPATH=$WIN_VISUAL_STUDIO_ROOT\\VC\\lib\\amd64

        LIB=$WIN_VISUAL_STUDIO_ROOT\\VC\\lib\\amd64\\;\
        $WIN_KITS\\lib\\winv6.3\\um\\x64

        INCLUDE=$WIN_VISUAL_STUDIO_ROOT\\VC\\include\\;\
        $WIN_KITS\\include\\shared\\;$WIN_KITS\\include\\um;\
        $WIN_KITS\\include\\winrt\\;$WIN_KITS\\include\\um\\gl

        # Put nsis, c compiler and java in path
        export PATH=$VCPATH:$PATH:$JAVA_BIN:$NSIS_BIN

        # Make sure LIB and INCLUDE is available for others
        export LIBPATH LIB INCLUDE


    Make sure to set the PATH so that NSIS and Microsoft SDK is found 
    before the MSYS/Cygwin tools and that Java is last in the PATH.

    Make a simple hello world and try to compile it with the `cl` 
    command from within bash. If that does not work, your environment 
    needs fixing. Remember, there should be
    no backslashes in your path environment variable in Cygwin bash,
    but LIB and INCLUDE should contain Windows style paths with
    semicolon, drive letters and backslashes.

*   Sun's Java JDK 1.6.0 or later. Our Java code (jinterface, ic) is
    written for JDK 1.6.0. Get it for Windows and install it, the JRE is
    not enough. If you don't care about Java, you can skip this step. The
    result will be that jinterface is not built.

    URL: <http://java.sun.com>

    Add javac *LAST* to your path environment in bash, in my case this means:

        `PATH="$PATH:/cygdrive/c/Program Files/Java/jdk1.7.0_02/bin"`

    No `CLASSPATH` or anything is needed. Type `javac` in the bash prompt
    and you should get a list of available Java options. Make sure, e.g by
    typing `type java`, that you use the Java you installed. Note however that
    Cygwin's/MinGW's/MSYS2's `jar.exe` is used. That's why the JDK bin-directory should be
    added last in the `PATH`.

*   Nullsoft NSIS installer system. You need this to build the self
    installing package. It's a free open source installer that's much
    nicer to use than the commercial Wise and Install shield
    installers. This is the installer we use for commercial releases as
    well.

    URL: <http://nsis.sourceforge.net/download>

    Install the lot, especially the modern user interface components, as
    it's definitely needed. Put `makensis` in your path, in my case:

        PATH=/cygdrive/c/Program\ Files/NSIS:$PATH

    Type makensis at the bash prompt and you should get a list of options
    if everything is OK.

*   OpenSSL. This is if you want the SSL and crypto applications to
    compile (and run). There are prebuilt binaries, which you can just 
    download and install, available here:

    URL: <http://openssl.org/community/binaries.html>

    We would recommend using 1.0.2d. 

*   Building with wxWidgets. Download wxWidgets-3.0.2 or higher.

    Install or unpack it to the pgm folder:
    Cygwin: 
        `DRIVE:/PATH/cygwin/opt/local/pgm`
    MSYS:
        `DRIVE:/PATH/MinGW/msys/1.0/opt/local/pgm`
    MSYS2:
        `DRIVE:/PATH/msys<32/64>/opt/local/pgm`

    If the `wxUSE_POSTSCRIPT` isn't enabled in  `<path\to\pgm>\wxMSW-3.0.2\include\wx\msw\setup.h`,
    enable it.

    build: From a command prompt with the VC tools available (See the
    instructions for OpenSSL build above for help on starting the
    proper command prompt in RELEASE mode):
	   
        C:\...\> cd <path\to\pgm>\wxMSW-3.0.2\build\msw
        C:\...\> nmake BUILD=release SHARED=0 DIR_SUFFIX_CPU= -f makefile.vc
    
    Or - if building a 64bit version:

        C:\...\> cd <path\to\pgm>\wxMSW-3.0.2\build\msw
        C:\...\> nmake TARGET_CPU=amd64 BUILD=release SHARED=0 DIR_SUFFIX_CPU= -f makefile.vc
    	   
*   Get the Erlang source distribution (from <http://www.erlang.org/download.html>).
    The same as for Unix platforms. Preferably use tar from within Cygwin, MSYS or MSYS2 to
    unpack the source tar.gz (`tar zxf otp_src_%OTP-REL%.tar.gz`).

    Set the environment `ERL_TOP` to point to the root directory of the
    source distribution. Let's say I stood in `$HOME/src` and unpacked
    `otp_src_%OTP-REL%.tar.gz`, I then add the following to `.profile`:

        ERL_TOP=$HOME/src/otp_src_%OTP-REL%
        export $ERL_TOP


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

If you are building a 64 bit version, you supply `otp_build` with an architecture parameter:

    $ cd $ERL_TOP
    $ eval `./otp_build env_win32 x64`
     

This should do the final touch to the environment and building should
be easy after this. You could run `./otp_build env_win32` without
`eval` just to see what it does, and to see that the environment it
sets seems OK. The path is cleaned of spaces if possible (using DOS
style short names instead), the variables `OVERRIDE_TARGET`, `CC`, `CXX`,
`AR` and `RANLIB` are set to their respective wrappers and the directories
`$ERL_TOP/erts/etc/win32/<cygwin/msys>_tools/vc` and
`$ERL_TOP/erts/etc/win32/<cygwin/msys>_tool` are added first in the PATH.

Now you can check which erlc you have by writing `type erlc` in your shell. 
It should reside in `$ERL_TOP/erts/etc/win32/cygwin_tools`
or `$ERL_TOP/erts/etc/win32/msys_tools`.


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
    on a Linux platform) and the Cygwin/MSYS/MSYS2 environment over the
    years. Running autoconf in Cygwin/MSYS/MSYS2 ensures that the configure 
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
    just type `$ERL_TOP/bin/erl` and you whould have the prompt. 

4.  `$ ./otp_build release -a` - Builds a commercial release tree from the
    source tree. The default is to put it in `$ERL_TOP/release/win32`. You can
    give any directory as parameter (Cygwin style), but it doesn't really
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


Using GIT
---------

You might want to check out versions of the source code from GitHUB. That is possible directly in Cygwin, but not in MSYS. There is a project MsysGIT:

URL:<http://code.google.com/p/msysgit/>

that makes a nice Git port. The msys prompt you get from MsysGIT is
however not compatible with the full version from MinGW, so you will
need to check out files using MsysGIT's command prompt and then switch
to a common MSYS command prompt for building. Also all test suites
cannot be built as MsysGIT/MSYS does not handle symbolic links. 


Copyright and License
---------------------

%CopyrightBegin%

Copyright Ericsson AB 2003-2015. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
 
    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%


   [1]: http://www.erlang.org/static/doc/mailinglist.html
   [2]: http://bugs.erlang.org
   [3]: https://github.com/erlang/otp

   [?TOC]: true
