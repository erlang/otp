How to Build Erlang/OTP on Windows
==================================

Introduction
------------

This file describes how to build the Erlang emulator and the OTP
libraries on Windows. The instructions apply to versions of Windows
supporting the Cygwin emulated gnuish environment for Windows or the
Msys ditto. We've built on the following platforms: Windows 2003
server, Windows XP Home/Professional, Windows Vista and Windows 7 (32
and 64 bit). You can probably build on Windows 2000, but you will not
be able to install the latest Microsoft SDK, so you have to go back to
some earlier compiler. Any Windows95'ish platform will surely get you
into trouble, what I'm not sure of, but it certainly will...

The procedure described uses either Cygwin or Msys as a build
environment, you run the bash shell in Cygwin/Msys and use gnu
make/configure/autoconf etc to do the build. The emulator C-source
code is, however, mostly compiled with Microsoft Visual C++™,
producing a native Windows binary. This is the same procedure as we
use to build the pre-built binaries. The fact that we use VC++ and not
gcc is explained further in the FAQ section.

I describe the build procedure to make it possible for open source
customers to build the emulator, given that they have the needed
tools. The binary Windows releases is still a preferred alternative if
one does not have Microsoft's development tools and/or don't want to
install Cygwin or Msys.

To use Cygwin/Msys, one needs basic experience from a Unix environment, if
one does not know how to set environment variables, run programs etc
in a Unix environment, one will be quite lost in the Cygwin os Msys
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
same makefiles are used on Unix/VxWorks, so that your changes
don't break other platforms. That of course goes for C-code too, system
specific code resides in the `$ERL_TOP/erts/emulator/sys/win32` and
`$ERL_TOP/erts/etc/win32` directories mostly. The
`$ERL_TOP/erts/emulator/beam` directory is for common code.

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

Starting with R15B, our build system runs both on Cygwin and Msys
(MinGW's fork of an early cygwin version). Msys is a smaller package
to install and may on some machines run slightly faster. If Cygwin
gives you trouble, try Msys instead, and v.v. Beginning with R15B
there is also a native 64bit version of Erlang for 64bit Windows 7
(only). These instructions apply to both the 32bit VM and the 64bit
ditto.

Note that even if you build a 64bit VM, most of the directories and
files involved are still named win32. You can view the name win32 as
meaning any windows version not beeing 16bit. A few occurences of the
name Win64 are however present in the system, for example the
installation file for a 64 bit windows version of Erlang is by default
named `otp_win64_<version>.exe`.

Lets go then, I'll start with a little FAQ, based on in house questions
and misunderstandings.


Frequently Asked Questions
--------------------------

*   Q: So, now I can build Erlang using GCC on Windows?

    A: No, unfortunately not. You'll need Microsoft's Visual C++
    still, a Bourne-shell script (cc.sh) wraps the Visual C++ compiler
    and runs it from within the Cygwin environment. All other tools
    needed to build Erlang are free-ware/open source, but not the C
    compiler. The Windows SDK is however enough to build Erlang, you
    do not need to buy Visual C++, just download the SDK (SDK version
    7.1 == Visual studio 2010).

*   Q: Why haven't you got rid of VC++ then, you \*\*\*\*\*\*?

    A: Well, partly because it's a good compiler - really! Actually it's
    been possible in late R11-releases to build using mingw instead of
    visual C++ (you might see the remnants of that in some scripts and
    directories). Unfortunately the development of the SMP version for
    Windows broke the mingw build and we chose to focus on the VC++ build
    as the performance has been much better in the VC++ versions. The
    mingw build will possibly be back, but as long as VC++ gives better
    performance, the commercial build will be a VC++ one.

*   Q: OK, you need VC++, but now you've started to demand a very recent
    (and expensive) version of Visual studio, not the old and stable VC++
    6.0 that was used in earlier versions. Why?

    A: Well, it's not expensive, it's free (as in free beer). Just
    download and install the latest Windows SDK from Microsoft and all
    the tools you need are there. The included debugger (WinDbg) is
    also quite usable, it's what I used when porting Erlang to 64bit
    Windows. Another reason to use the latest Microsoft compilers is
    DLL compatibility. DLL's using a new version of the standard
    library might not load if the VM is compiled with an old VC++
    version, why we should aim to use the latest freely available SDK
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
    Cygwin binary. Ericsson does however not pay me to do a Cygwin port, so
    such a port would have to happen in spare time, which is a limited
    resource...

*   Q: Hah, I saw you, you used GCC even though you said you didn't!

    A: OK, I admit, one of the files is compiled using Cygwin's or
    MinGW's GCC and the resulting object code is then converted to MS
    VC++ compatible coff using a small C hack. It's because that
    particular file, `beam_emu.c` benefits immensely from being able
    to use the GCC labels-as-values extension, which boosts emulator
    performance by up to 50%. That does unfortunately not (yet) mean
    that all of OTP could be compiled using GCC, that particular
    source code does not do anything system specific and actually is
    adopted to the fact that GCC is used to compile it on Windows.

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

    A: Cygwin or Msys is the environment, which closely resembles the
    environments found on any Unix machine. It's almost like you had a
    virtual Unix machine inside Windows. Configure, given certain
    parameters, then creates makefiles that are used by the
    Cygwin/Msys gnu-make to built the system. Most of the actual
    compilers etc are not, however, Cygwin/Msys tools, so I've written
    a couple of wrappers (Bourne-shell scripts), which reside in
    `$ERL_TOP/etc/win32/cygwin_tools` and
    `$ERL_TOP/etc/win32/msys_tools`. They all do conversion of
    parameters and switches common in the Unix environment to fit the
    native Windows tools. Most notable is of course the paths, which
    in Cygwin/Msys are Unix-like paths with "forward slashes" (/) and
    no drive letters, the Cygwin specific command `cygpath` is used
    for most of the path conversions in a Cygwin environment, other
    tools are used (when needed) in the corresponding Msys
    environment. Luckily most compilers accept forward slashes instead
    of backslashes as path separators, but one still have to get the drive
    letters etc right, though. The wrapper scripts are not general in
    the sense that, for example, cc.sh would understand and translates
    every possible gcc option and passes correct options to
    cl.exe. The principle is that the scripts are powerful enough to
    allow building of Erlang/OTP, no more, no less. They might need
    extensions to cope with changes during the development of Erlang,
    that's one of the reasons I made them into shell-scripts and not
    Perl-scripts, I believe they are easier to understand and change
    that way. I might be wrong though, cause another reason I didn't
    write them in Perl is because I've never liked Perl and my Perl
    code is no pleasant reading...

    In `$ERL_TOP`, there is a script called `otp_build`. That script handles
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

*   Q: Which version of Cygwin/Msys and other tools do you use then?

    A: For Cygwin and Msys alike, we try to use the latest releases
    available when building. What versions you use shouldn't really
    matter, I try to include workarounds for the bugs I've found in
    different Cygwin/Msys releases, please help me add workarounds
    for new Cygwin/Msys-related bugs as soon as you encounter
    them. Also please do submit bug reports to the appropriate Cygwin
    and/or Msys developers. The GCC we used for %OTP-REL% was version
    4.7.0 (MinGW 64bit) and 4.3.4 (Cygwin 32bit). We used VC++ 10.0
    (i.e. Visual studio 2010), Sun's JDK 1.5.0\_17 (32bit) and Sun's
    JDK 1.7.0\_1 (64bit), NSIS 2.46, and Win32 OpenSSL 0.9.8r. Please
    read the next section for details on what you need.

*   Q: Can you help me setup X in Cygwin?

    A: No, unfortunately I haven't got time to help with Cygwin related
    user problems, please read Cygwin related web sites, newsgroups and
    mailing lists.

*   Q: Why is the instruction so long? Is it really that complicated?

    A: Partly it's long because I babble too much, partly because I've
    described as much as I could about the installation of the needed
    tools. Once the tools are installed, building is quite easy. I also
    have tried to make this instruction understandable for people with
    limited Unix experience. Cygwin/Msys is a whole new environment to some
    Windows users, why careful explanation of environment variables etc
    seemed to be in place. The short story, for the experienced and
    impatient is:

    *   Get and install complete Cygwin (latest) or complete MinGW with msys

    *   Install Microsofts Windows SDK 7.1 (and .Net 4)

    *   Get and install Sun's JDK 1.5.0 or higher

    *   Get and install NSIS 2.01 or higher (up to 2.46 tried and working)

    * Get, build and install OpenSSL 0.9.8r or higher (up to 1.0.0a
        tried & working) with static libs.

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
notably you'll need Cygwin or Msys and Microsofts Windows SDK, but
you also might want a Java compiler, the NSIS install system and
OpenSSL. Well' here's the list:

*   Cygwin, the very latest is usually best. Get all the development
    tools and of course all the basic ditto. In fact getting the complete
    package might be a good idea, as you'll start to love Cygwin after a
    while if you're accustomed to Unix. Make sure to get jar and also make
    sure *not* to install a Cygwin'ish Java... The Cygwin jar command is
    used but Sun's Java compiler and virtual machine...

    If you are going to build a 64bit Windows version, you should make
    sure to get MinGW's 64bit gcc installed with cygwin. It's in one of
    the development packages.

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

*   Alternatively you download MinGW and Msys. You'll find the latest
    installer at:

    URL: <http://sourceforge.net/projects/mingw/files/Installer/mingw-get-inst/>

    Make sure to install everything they've got. 

    To be able to build the 64bit VM, you will also need the 64bit
    MinGW compiler from:

    URL: <http://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win64/Automated%20Builds/>

    The latest version should do it. Make sure you download the
    `mingw-w64-bin_i686-mingw_<something>.zip`, not a linux
    version. You unzip the package on top of your MinGW installation
    (`c:\MinGW`) and that's it.

    Setting up your environment in Msys is similar to setting it up in
    Cygwin.

* Microsofts Windows SDK version 7.1 (corresponding to VC++ 10.0 and
    Visual Studio 2010). You'll find it here:
    
    URL: <http://www.microsoft.com/download/en/details.aspx?id=8279>

    but before you install that, you need to have .Net 4 installed,
    you'll find that here:

    URL: <http://www.microsoft.com/download/en/details.aspx?id=17851>

    Use the web installer for the SDK, at least when I tried
    downloading the whole package as an image, I got SDK 7.0 instead,
    which is not what you want...

    There will be a Windows command file in `%PROGRAMFILES%\Mirosoft
    SDKs\Windows\v7.1\Bin\SetEnv.cmd` that set's the appropriate
    environment for a Windows command prompt. This is not appropriate
    for bash, so you'll need to convert it to bash-style environments
    by editing your `.bash_profile`. In my case, where the SDK is
    installed in the default directory and `%PROGRAMFILES%` is
    `C:\Program Files`, the commands for setting up a 32bit build
    environment (on a 64bit or 32bit machine) look like this (in cygwin):

        # Some common paths
        C_DRV=/cygdrive/c
        PRG_FLS=$C_DRV/Program\ Files

        # nsis
        NSIS_BIN=$PRG_FLS/NSIS
        # java
        JAVA_BIN=$PRG_FLS/Java/jdk1.6.0_16/bin

        ##
        ## MS SDK
        ##

        CYGWIN=nowinsymlinks
        MVS10="$PRG_FILES/Microsoft Visual Studio 10.0"
        WIN_MVS10="C:\\Program Files\\Microsoft Visual Studio 10.0"
        SDK10="$PRG_FILES/Microsoft SDKs/Windows/v7.1"
        WIN_SDK10="C:\\Program Files\\Microsoft SDKs\\Windows\\v7.1"

        PATH="$NSIS_BIN:\
        $MVS10/Common7/IDE:\
        $MVS10/Common7/Tools:\
        $MVS10/VC/Bin:\
        $MVS10/VC/Bin/VCPackages:\
        $SDK10/Bin/NETFX 4.0 Tools:\
        $SDK10/Bin:\
        /usr/local/bin:/usr/bin:/bin:\
        /cygdrive/c/WINDOWS/system32:/cygdrive/c/WINDOWS:\
        /cygdrive/c/WINDOWS/system32/Wbem:\
        $JAVA_BIN"

        LIBPATH="$WIN_MVS10\\VC\\LIB"

        LIB="$WIN_MVS10\\VC\\LIB;$WIN_SDK10\\LIB"

        INCLUDE="$WIN_MVS10\\VC\\INCLUDE;$WIN_SDK10\\INCLUDE;$WIN_SDK10\\INCLUDE\\gl"

        export CYGWIN PATH LIBPATH LIB INCLUDE 

    If you're using Msys instead, the only thing you need to change is
    the `C_DRV` setting, which would read:

        C_DRV=/c

    And of course you might need to change `C:\Program Files` etc if
    you're using a non-english version of Windows (XP). Note that in
    later versions of Windows, the national adoptions of the program
    files directories etc are not on the file system but only in the
    explorer, so even if explorer says that your programs reside in
    e.g. `C:\Program`, they might still reside in `C:\Program Files`
    in reality...

    If you are building a 64 bit version of Erlang, you should set up
    PATHs etc a little differently. I use the following script to
    make things work in both Cygwin and Msys:

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
         if [ -x /usr/bin/msysinfo ]; then
            # Without this the path conversion won't work
            COMSPEC='C:\Windows\SysWOW64\cmd.exe'
            MSYSTEM=MINGW32
            export MSYSTEM COMSPEC
            IN_CYGWIN=false
         else
            CYGWIN=nowinsymlinks
            export CYGWIN
            IN_CYGWIN=true
         fi

         if [ "$IN_CYGWIN" = "true" ]; then 
            PATH=/usr/local/bin:/usr/bin:/bin:/usr/X11R6/bin:\
            /cygdrive/c/windows/system32:/cygdrive/c/windows:/cygdrive/c/windows/system32/Wbem
         else
            PATH=/usr/local/bin:/mingw/bin:/bin:/c/Windows/system32:/c/Windows:\
            /c/Windows/System32/Wbem
         fi

         if [ "$IN_CYGWIN" = "true" ]; then 
            C_DRV=/cygdrive/c
         else
            C_DRV=/c
         fi

         PRG_FLS64=$C_DRV/Program\ Files
         PRG_FLS32=$C_DRV/Program\ Files\ \(x86\)
         VISUAL_STUDIO_ROOT32=$PRG_FLS32/Microsoft\ Visual\ Studio\ 10.0
         MS_SDK_ROOT64=$PRG_FLS64/Microsoft\ SDKs/Windows/v7.1

         # Okay, now mangle the paths and get rid of spaces by using short names
         WIN_VCROOT32=`make_winpath "$VISUAL_STUDIO_ROOT32"`
         VCROOT32=`make_upath $WIN_VCROOT32`
         WIN_SDKROOT64=`make_winpath "$MS_SDK_ROOT64"`
         SDKROOT64=`make_upath $WIN_SDKROOT64`
         WIN_PROGRAMFILES32=`make_winpath "$PRG_FLS32"`
         PROGRAMFILES32=`make_upath $WIN_PROGRAMFILES32`
    
         WIN_PROGRAMFILES64=`make_winpath "$PRG_FLS64"`
         PROGRAMFILES64=`make_upath $WIN_PROGRAMFILES64`

         # nsis
         NSIS_BIN=$PROGRAMFILES32/NSIS
         # java
         JAVA_BIN=$PROGRAMFILES64/Java/jdk1.7.0_01/bin

         ## The PATH variable should be Unix'ish
         VCPATH=$VCROOT32/Common7/IDE:$VCROOT32/VC/BIN/amd64:$VCROOT32/Common7/Tools:\
         $VCROOT32/VC/VCPackages:$SDKROOT64/bin/NETFX4~1.0TO/x64:$SDKROOT64/bin/x64:\
         $SDKROOT64/bin

         ## Microsoft SDK libs

         LIBPATH=$WIN_VCROOT32\\VC\\LIB\\amd64
         LIB=$WIN_VCROOT32\\VC\\LIB\\amd64\;$WIN_SDKROOT64\\LIB\\X64
         INCLUDE=$WIN_VCROOT32\\VC\\INCLUDE\;$WIN_SDKROOT64\\include\;\
         $WIN_SDKROOT64\\include\\gl

         # Put nsis, c compiler and java in path
         PATH=$NSIS_BIN:$VCPATH:$PATH:$JAVA_BIN

         # Make sure LIB and INCLUDE is available for others
         export PATH LIBPATH LIB INCLUDE

    All this is derived from the SetEnv.cmd command file mentioned
    earlier. The bottom line is to set the PATH so that NSIS and
    Microsoft SDK is found before the Msys/Cygwin tools and that Java
    is last in the PATH.

    Make a simple hello world (maybe one that prints out 
    `sizeof(void *)`) and try to compile it with the `cl` command from within
    bash. If that does not work, your environment needs fixing. Also
    remember to fix up the PATH environment, especially old Erlang
    installations might have inserted quoted paths that Cygwin/Msys
    does not understand. Remove or correct such paths. There should be
    no backslashes in your path environment variable in Cygwin bash,
    but LIB and INCLUDE should contain Windows style paths with
    semicolon, drive letters and backslashes.

*   Sun's Java JDK 1.5.0 or higher. Our Java code (jinterface, ic) is
    written for JDK 1.5.0. Get it for Windows and install it, the JRE is
    not enough. If you don't care about Java, you can skip this step, the
    result will be that jinterface is not built.

    URL: <http://java.sun.com>

    Add javac *LAST* to your path environment in bash, in my case this means:

        `PATH="$PATH:/cygdrive/c/Program Files/Java/jdk1.5.0_17/bin"`

    No `CLASSPATH` or anything is needed. Type `javac` at the bash prompt
    and you should get a list of available Java options. Make sure by
    typing `type java` that you use the Java you installed. Note however that
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

* OpenSSL. This is if you want the SSL and crypto applications to
    compile (and run). There are prebuilt binaries available, but I
    strongly recommend building this yourself. It's quite easy.

    First get the source from 

    URL: <http://openssl.org/source/>

    I would recommend using 0.9.8r. 

    Download the tar file and unpack it (using your bash prompt) into
    a directory of your choise.

    You will need a Windowish Perl for the build. ActiveState has one:

    URL: <http://www.activestate.com/activeperl/downloads>

    Download and install that. Disable options to associate it with
    the .pl suffix and/or adding things to PATH, they are not needed. 

    Now fire up the Microsoft Windows SDK command prompt in RELEASE
    mode for the architecture you are going to build. The easiest is
    to copy the shortcut from the SDKs start menu item and edit the
    command line in the shortcut (Right click->Properties) to end with
    `/Release`. Make sure the banner when you double click your
    shortcut (the text in the resulting command window) says
    `Targeting Windows XP x64 Release` if you are going to do a 64 bit
    build and `Targeting Windows XP x86 Release` if you are building a
    32 bit version.

    Now cd to where you unpacked the OpenSSL source using your Release
    Windows command prompt (it should be on the same drive as where
    you are going to install it if everything is to work smothly).

        C:\> cd <some dir>

    Add ActiveState (or some other windows perl, not cygwins) to your PATH:

        C:\...\> set PATH=C:\Perl\bin;%PATH%

    Or if you installed the 64bit perl:
    
        C:\...\> set PATH=C:\Perl64\bin;%PATH%

    Configure OpenSSL for 32 bit:

        C:\...\> perl Configure VC-WIN32 --prefix=/OpenSSL

    Or for 64 bit:

        C:\...\> perl Configure VC-WIN64A --prefix=/OpenSSL-Win64

    Do some setup (for 32 bit):

        C:\...\> ms\do_ms

    The same for 64 bit:

        C:\...\> ms\do_win64a

    Then build static libraries and install:

        C:\...\> nmake -f ms\nt.mak
        C:\...\> nmake -f ms\nt.mak install

    That's it - you now have your perfectly consistent static build of
    openssl. If you want to get rid of any possibly patented
    algorithms in the lib, just read up on the OpenSSL FAQ and follow
    the instructions.

    The installation locations chosen are where configure will look
    for OpenSSL, so try to keep them as is.
       
*   Building with wxWidgets. Download wxWidgets-2.8.9 or higher patch
    release (2.9.\*  is a developer release which currently does not work
    with wxErlang).

    Install or unpack it to `DRIVE:/PATH/cygwin/opt/local/pgm`.

    edit:  `C:\cygwin\opt\local\pgm\wxMSW-2.8.11\include\wx\msw\setup.h`
    enable `wxUSE_GLCANVAS`, `wxUSE_POSTSCRIPT` and `wxUSE_GRAPHICS_CONTEXT`

    build: From a command prompt with the VC tools available (See the
    instructions for OpenSSL build above for help on starting the
    proper command prompt in RELEASE mode):
	   
        C:\...\> cd C:\cygwin\opt\local\pgm\wxMSW-2.8.11\build\msw
        C:\...\> nmake BUILD=release SHARED=0 UNICODE=1 USE_OPENGL=1 USE_GDIPLUS=1 DIR_SUFFIX_CPU= -f makefile.vc
        C:\...\> cd C:\cygwin\opt\local\pgm\wxMSW-2.8.11\contrib\build\stc
        C:\...\> nmake BUILD=release SHARED=0 UNICODE=1 USE_OPENGL=1 USE_GDIPLUS=1 DIR_SUFFIX_CPU= -f makefile.vc
    
    Or - if building a 64bit version:

        C:\...\> cd C:\cygwin\opt\local\pgm\wxMSW-2.8.11\build\msw
        C:\...\> nmake TARGET_CPU=amd64 BUILD=release SHARED=0 UNICODE=1 USE_OPENGL=1 USE_GDIPLUS=1 DIR_SUFFIX_CPU= -f makefile.vc
        C:\...\> cd C:\cygwin\opt\local\pgm\wxMSW-2.8.11\contrib\build\stc
        C:\...\> nmake TARGET_CPU=amd64 BUILD=release SHARED=0 UNICODE=1 USE_OPENGL=1 USE_GDIPLUS=1 DIR_SUFFIX_CPU= -f makefile.vc
    	   
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

    Note that there is no special 64bit version of TCL/TK needed, you
    can use the 32bit program even for a 64bit build.

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
`$ERL_TOP/erts/etc/win32/cygwin_tools/vc` and
`$ERL_TOP/erts/etc/win32/cygwin_tool` are added first in the PATH.

Try now a `type erlc`. That should result in the erlc wrapper script
(which does not have the .sh extension, for reasons best kept
untold...). It should reside in `$ERL_TOP/erts/etc/win32/cygwin_tools`
or `$ERL_TOP/erts/etc/win32/msys_tools`.  You could also try `which
cc.sh`, which `ar.sh` etc.

Now you're ready to build...


Building and Installing
-----------------------

Now it's assumed that you have executed `` eval `./otp_build env_win32` `` or 
`` eval `./otp_build env_win32 x64` `` for this particular shell...

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

    or

        $ cd $ERL_TOP
        $ release/win32/otp_win64_%OTP-REL% /S
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

Using GIT
---------

You might want to check out versions of the source code from GitHUB. That is possible directly in cygwin, but not in Msys. There is a project MsysGIT:

URL:<http://code.google.com/p/msysgit/>

that makes a nice Git port. The msys prompt you get from MsysGIT is
however not compatible with the full version from MinGW, so you will
need to check out files using MsysGIT's command prompt and then switch
to a common Msys command prompt for building. Also all test suites
cannot be built as MsysGIT/Msys does not handle symbolic links. To
build test suites on Windows, you will need Cygwin for now. Hopefully
all symbolic links will disappear from our repository soon and this
issue will disappear.

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
excellent Cygwin. The guys at Cygnus solutions and
Redhat deserve a huge THANKS! as well as all the other people in the
free software community who have helped in creating the magnificent
software that constitutes Cygwin.

Also the people developing the alternative command prompt Msys and
the MinGW compiler are worth huge THANKS! The 64bit port would have
been impossible without the 64bit MinGW compiler.

Good luck and Happy Hacking,
Patrik, OTP

Copyright and License
---------------------

%CopyrightBegin%

Copyright Ericsson AB 2003-2012. All Rights Reserved.

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
[$ERL_TOP/HOWTO/MARKDOWN.md][] document.



   [1]: http://www.erlang.org/faq.html "mailing lists"
   [$ERL_TOP/HOWTO/MARKDOWN.md]: MARKDOWN.md

   [?TOC]: true
