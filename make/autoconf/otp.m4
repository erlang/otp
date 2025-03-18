dnl -*-Autoconf-*-
dnl %CopyrightBegin%
dnl
dnl Copyright Ericsson AB 1998-2025. All Rights Reserved.
dnl
dnl Licensed under the Apache License, Version 2.0 (the "License");
dnl you may not use this file except in compliance with the License.
dnl You may obtain a copy of the License at
dnl
dnl     http://www.apache.org/licenses/LICENSE-2.0
dnl
dnl Unless required by applicable law or agreed to in writing, software
dnl distributed under the License is distributed on an "AS IS" BASIS,
dnl WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
dnl See the License for the specific language governing permissions and
dnl limitations under the License.
dnl
dnl %CopyrightEnd%
dnl

dnl
dnl otp.m4
dnl
dnl Local macros that can be used in `configure.ac` files in OTP. To use
dnl any of the macros your `configure.ac` file should include this file
dnl using `m4_include([otp.m4])` after AC_INIT.
dnl
dnl The Local Macros which could be part of autoconf are prefixed LM_,
dnl macros specific dnl to the Erlang system are prefixed ERL_ (this is
dnl not always consistently made...).
dnl
dnl To make it easier to debug, sprinkle some calls to LM_LOG in the macros

dnl Log a message to config.log
AC_DEFUN(LM_LOG,
  [
    printf "configure:$LINENO: %s\n" $1 >&AS_MESSAGE_LOG_FD
  ])


dnl We check if -Werror was given on command line and if so
dnl we disable it for the configure and only use it when
dnl actually building erts
AC_DEFUN([ERL_PUSH_WERROR],
[
no_werror_CFLAGS=$(echo " $CFLAGS " | sed 's/ -Werror / /g')
if test "X $CFLAGS " != "X$no_werror_CFLAGS"; then
   CFLAGS="$no_werror_CFLAGS"
   WERRORFLAGS=-Werror
fi])

AC_DEFUN([ERL_POP_WERROR],
[
if test "x$GCC" = xyes; then
    CFLAGS="$WERRORFLAGS $CFLAGS"
fi])

AC_DEFUN([ERL_CANONICAL_SYSTEM_TYPE],
[
    AC_CANONICAL_HOST
    # Adjust for local legacy windows hack...
    AS_CASE([$host],
            [local-aarch64-*-windows],
            [
                host=win32
                host_os=win32
                host_vendor=
                host_cpu=aarch64
            ],
            [local-*-windows],
            [
                host=win32
                host_os=win32
                host_vendor=
                host_cpu=
            ])

    AC_CANONICAL_BUILD
    # Adjust for local legacy windows hack...
    AS_CASE([$build],
            [local-aarch64-*-windows],
            [
                build=win32
                build_os=win32
                build_vendor=
                build_cpu=aarch64
            ],
            [local-*-windows],
            [
                build=win32
                build_os=win32
                build_vendor=
                build_cpu=
            ])

    AC_CANONICAL_TARGET
    # Adjust for local legacy windows hack...
    AS_CASE([$target],
            [local-aarch64-*-windows],
            [
                target=win32
                target_os=win32
                target_vendor=
                target_cpu=aarch64
            ],
            [local-*-windows],
            [
                target=win32
                target_os=win32
                target_vendor=
                target_cpu=
            ])

    AS_IF([test "$cross_compiling" = "yes" -a "$build" = "$host"  -a "$build_cpu" = "$host_cpu"],
          [AC_MSG_ERROR([
           Cross compiling with the same canonicalized 'host' and 'host_cpu'
           values as the canonicalized 'build' and 'build_cpu' values

           We are cross compiling since the '--host=$host_alias'
           and the '--build=$build_alias' arguments differ. When
           cross compiling Erlang/OTP, also the canonicalized values of
           the '--build' and the '--host' arguments *must* differ. The
           canonicalized values of these arguments however both equals:
           host = build = $host,
           host_cpu = build_cpu = $host_cpu

           You can check the canonical value by passing a value as
           argument to the 'make/autoconf/config.sub' script.
          ])])
])

AC_DEFUN(LM_PRECIOUS_VARS,
[

dnl ERL_TOP
AC_ARG_VAR(ERL_TOP, [Erlang/OTP top source directory])

dnl Tools
AC_ARG_VAR(CC, [C compiler])
AC_ARG_VAR(CFLAGS, [C compiler flags])
AC_ARG_VAR(STATIC_CFLAGS, [C compiler static flags])
AC_ARG_VAR(CFLAG_RUNTIME_LIBRARY_PATH, [runtime library path linker flag passed via C compiler])
AC_ARG_VAR(CPP, [C/C++ preprocessor])
AC_ARG_VAR(CPPFLAGS, [C/C++ preprocessor flags])
AC_ARG_VAR(CXX, [C++ compiler])
AC_ARG_VAR(CXXFLAGS, [C++ compiler flags])
AC_ARG_VAR(LD, [linker (is often overridden by configure)])
AC_ARG_VAR(LDFLAGS, [linker flags (can be risky to set since LD may be overridden by configure)])
AC_ARG_VAR(LIBS, [libraries])
AC_ARG_VAR(DED_LD, [linker for Dynamic Erlang Drivers (set all DED_LD* variables or none)])
AC_ARG_VAR(DED_LDFLAGS, [linker flags for Dynamic Erlang Drivers (set all DED_LD* variables or none)])
AC_ARG_VAR(DED_LD_FLAG_RUNTIME_LIBRARY_PATH, [runtime library path linker flag for Dynamic Erlang Drivers (set all DED_LD* variables or none)])
AC_ARG_VAR(RANLIB, [ranlib])
AC_ARG_VAR(AR, [ar])
AC_ARG_VAR(GETCONF, [getconf])
AC_ARG_VAR(EX_DOC, [Path to ex_doc executable])

dnl Cross system root
AC_ARG_VAR(erl_xcomp_sysroot, [Absolute cross system root path (only used when cross compiling)])
AC_ARG_VAR(erl_xcomp_isysroot, [Absolute cross system root include path (only used when cross compiling)])

dnl Cross compilation variables
AC_ARG_VAR(erl_xcomp_bigendian, [big endian system: yes|no (only used when cross compiling)])
AC_ARG_VAR(erl_xcomp_double_middle_endian, [double-middle-endian system: yes|no (only used when cross compiling)])
AC_ARG_VAR(erl_xcomp_linux_nptl, [have Native POSIX Thread Library: yes|no (only used when cross compiling)])
AC_ARG_VAR(erl_xcomp_linux_usable_sigusrx, [SIGUSR1 and SIGUSR2 can be used: yes|no (only used when cross compiling)])
AC_ARG_VAR(erl_xcomp_linux_usable_sigaltstack, [have working sigaltstack(): yes|no (only used when cross compiling)])
AC_ARG_VAR(erl_xcomp_poll, [have working poll(): yes|no (only used when cross compiling)])
AC_ARG_VAR(erl_xcomp_kqueue, [have working kqueue(): yes|no (only used when cross compiling)])
AC_ARG_VAR(erl_xcomp_putenv_copy, [putenv() stores key-value copy: yes|no (only used when cross compiling)])
AC_ARG_VAR(erl_xcomp_reliable_fpe, [have reliable floating point exceptions: yes|no (only used when cross compiling)])
AC_ARG_VAR(erl_xcomp_getaddrinfo, [have working getaddrinfo() for both IPv4 and IPv6: yes|no (only used when cross compiling)])
AC_ARG_VAR(erl_xcomp_gethrvtime_procfs_ioctl, [have working gethrvtime() which can be used with procfs ioctl(): yes|no (only used when cross compiling)])
AC_ARG_VAR(erl_xcomp_clock_gettime_cpu_time, [clock_gettime() can be used for retrieving process CPU time: yes|no (only used when cross compiling)])
AC_ARG_VAR(erl_xcomp_after_morecore_hook, [__after_morecore_hook can track malloc()s core memory usage: yes|no (only used when cross compiling)])
AC_ARG_VAR(erl_xcomp_dlsym_brk_wrappers, [dlsym(RTLD_NEXT, _) brk wrappers can track malloc()s core memory usage: yes|no (only used when cross compiling)])

])

AC_DEFUN(ERL_XCOMP_SYSROOT_INIT,
[
erl_xcomp_without_sysroot=no
if test "$cross_compiling" = "yes"; then
    test "$erl_xcomp_sysroot" != "" || erl_xcomp_without_sysroot=yes
    test "$erl_xcomp_isysroot" != "" || erl_xcomp_isysroot="$erl_xcomp_sysroot"
else
    erl_xcomp_sysroot=
    erl_xcomp_isysroot=
fi
])

AC_DEFUN(LM_CHECK_GETCONF,
[
AS_IF(
  [test "$cross_compiling" != "yes"],
  [
    AC_CHECK_PROG([GETCONF], [getconf], [getconf], [false])
  ],
  [
    dnl First check if we got a `<HOST>-getconf' in $PATH
    host_getconf="$host_alias-getconf"
    AC_CHECK_PROG([GETCONF], [$host_getconf], [$host_getconf], [false])
    AS_IF(
      [test "$GETCONF" = "false" && test "$erl_xcomp_sysroot" != ""],
      [
	dnl We should perhaps give up if we haven't found it by now, but at
	dnl least in one Tilera MDE `getconf' under sysroot is a bourne
	dnl shell script which we can use. We try to find `<HOST>-getconf'
    	dnl or `getconf' under sysconf, but only under sysconf since
	dnl `getconf' in $PATH is almost guaranteed to be for the build
	dnl machine.
	GETCONF=
	prfx="$erl_xcomp_sysroot"
        AC_PATH_TOOL([GETCONF], [getconf], [false],
	             ["$prfx/usr/bin:$prfx/bin:$prfx/usr/local/bin"])
      ])
   ])
])

dnl ----------------------------------------------------------------------
dnl
dnl LM_WINDOWS_ENVIRONMENT
dnl
dnl
dnl Tries to determine the windows build environment, i.e.
dnl MIXED_VC or MIXED_MINGW
dnl

AC_DEFUN(LM_WINDOWS_ENVIRONMENT,
[

if test "X$windows_environment_" != "Xchecked"; then
windows_environment_=checked
MIXED_CYGWIN=no
MIXED_MSYS=no
MIXED_VSL=no

dnl MIXED_VC is Microsoft Visual C++ used as standard compiler
MIXED_VC=no
dnl MIXED_MINGW is mingw(32|64) used as standard compiler
MIXED_MINGW=no

AC_MSG_CHECKING(for mixed mingw-gcc and native VC++ environment)
if test "X$host" = "Xwin32" -a "x$GCC" != "xyes"; then
	if test -x /usr/bin/msys-?.0.dll; then
	        CFLAGS="$CFLAGS -O2"
		MIXED_MSYS=yes
		AC_MSG_RESULT([MSYS and VC])
		MIXED_VC=yes
		CPPFLAGS="$CPPFLAGS -DERTS_MIXED_VC"
	elif test -x /usr/bin/cygpath; then
		CFLAGS="$CFLAGS -O2"
		MIXED_CYGWIN=yes
		AC_MSG_RESULT([Cygwin and VC])
		MIXED_VC=yes
		CPPFLAGS="$CPPFLAGS -DERTS_MIXED_VC"
        elif test -x /bin/wslpath; then
		CFLAGS="$CFLAGS -O2"
		MIXED_WSL=yes
		AC_MSG_RESULT([WSL and VC])
		MIXED_VC=yes
		CPPFLAGS="$CPPFLAGS -DERTS_MIXED_VC"
	else
		AC_MSG_RESULT([undeterminable])
		AC_MSG_ERROR(Seems to be mixed windows but not within any known env, cannot handle this!)
	fi
else
	AC_MSG_RESULT([no])
fi

AC_SUBST(MIXED_VC)

if test "x$MIXED_MSYS" != "xyes"; then
   AC_MSG_CHECKING(for mixed cygwin and native MinGW environment)
   if test "X$host" = "Xwin32" -a "x$GCC" = x"yes"; then
	if test -x /usr/bin/cygpath; then
		CFLAGS="$CFLAGS -O2"
		AC_MSG_RESULT([yes])
		MIXED_MINGW=yes
		CPPFLAGS="$CPPFLAGS -DERTS_MIXED_MINGW"
	else
		AC_MSG_RESULT([undeterminable])
		AC_MSG_ERROR(Seems to be mixed windows but not with cygwin, cannot handle this!)
	fi
    else
	AC_MSG_RESULT([no])
    fi
else
   AC_MSG_CHECKING(for mixed MSYS and native MinGW environment)
   if test "x$GCC" = x"yes"; then
    	if test -x /usr/bin/msys-=.0.dll; then
		CFLAGS="$CFLAGS -O2"
		AC_MSG_RESULT([yes])
		MIXED_MINGW=yes
		CPPFLAGS="$CPPFLAGS -DERTS_MIXED_MINGW"
	else
		AC_MSG_RESULT([undeterminable])
		AC_MSG_ERROR(Seems to be mixed windows but not with msys, cannot handle this!)
	fi
    else
	AC_MSG_RESULT([no])
    fi
fi
AC_SUBST(MIXED_MINGW)

AC_MSG_CHECKING(if we mix cygwin with any native compiler)
if test "X$MIXED_CYGWIN" = "Xyes"; then
	AC_MSG_RESULT([yes])
else
	AC_MSG_RESULT([no])
fi

AC_MSG_CHECKING(if we mix msys with another native compiler)
if test "X$MIXED_MSYS" = "Xyes" ; then
	AC_MSG_RESULT([yes])
else
	AC_MSG_RESULT([no])
fi

AC_MSG_CHECKING(if we mix WSL with another native compiler)
if test "X$MIXED_WSL" = "Xyes" ; then
	AC_MSG_RESULT([yes])
else
	AC_MSG_RESULT([no])
fi

fi
])


dnl ----------------------------------------------------------------------
dnl
dnl LM_PROG_LD
dnl
dnl
dnl Sets LD to the either ld.sh or '$(CC)'. We force LD to be $CC so that
dnl we know that LDFLAGS will have to be in the form acceped by $CC and not
dnl the form used to ld.
dnl
dnl Windows is a bit of a special case as we control ld.sh ourselves, so there
dnl we use ld.sh instead of cc.sh.

AC_DEFUN(LM_PROG_LD,
  [AC_CHECK_PROGS(LD, ld.sh)
   AC_CHECK_TOOL(LD, ld, [:])
   AS_IF([test "$LD" = ":"],
     [AC_MSG_ERROR([No linker found])],
     [LM_LOG('setting LD to ${CC}')
      LD=${CC}])
])

dnl ----------------------------------------------------------------------
dnl
dnl LM_FIND_EMU_CC
dnl
dnl
dnl Tries fairly hard to find a C compiler that can handle jump tables.
dnl Defines the @EMU_CC@ variable for the makefiles and 
dnl inserts NO_JUMP_TABLE in the header if one cannot be found...
dnl

AC_DEFUN(LM_FIND_EMU_CC,
	[AC_CACHE_CHECK(for a compiler that handles jumptables,
			ac_cv_prog_emu_cc,
			[
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
#if defined(__clang_major__) && __clang_major__ >= 3
    /* clang 3.x or later is fine */
#elif defined(__llvm__)
#error "this version of llvm is unable to correctly compile beam_emu.c"
#endif
    __label__ lbl1;
    __label__ lbl2;
    extern int magic(void);
    int x = magic();
    static void *jtab[2];

    jtab[0] = &&lbl1;
    jtab[1] = &&lbl2;
    goto *jtab[x];
lbl1:
    return 1;
lbl2:
    return 2;
]])],[ac_cv_prog_emu_cc="$CC"],[ac_cv_prog_emu_cc=no])

if test "$ac_cv_prog_emu_cc" = no; then
	for ac_progname in emu_cc.sh gcc-4.2 gcc; do
  		IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS=":"
  		ac_dummy="$PATH"
  		for ac_dir in $ac_dummy; do
    			test -z "$ac_dir" && ac_dir=.
    			if test -f "$ac_dir/$ac_progname"; then
      				ac_cv_prog_emu_cc="$ac_dir/$ac_progname"
      				break
    			fi
  		done
  		IFS="$ac_save_ifs"
		if test "$ac_cv_prog_emu_cc" != no; then
			break
		fi
	done
fi

AS_IF([test "$ac_cv_prog_emu_cc" != no],
      [
	save_CC="$CC"
	save_CFLAGS=$CFLAGS
	save_CPPFLAGS=$CPPFLAGS
	CC="$ac_cv_prog_emu_cc"
	CFLAGS=""
	CPPFLAGS=""
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
#if defined(__clang_major__) && __clang_major__ >= 3
    /* clang 3.x or later is fine */
#elif defined(__llvm__)
#error "this version of llvm is unable to correctly compile beam_emu.c"
#endif
    	__label__ lbl1;
    	__label__ lbl2;
	extern int magic(void);
    	int x = magic();
    	static void *jtab[2];

    	jtab[0] = &&lbl1;
    	jtab[1] = &&lbl2;
    	goto *jtab[x];
	lbl1:
    	return 1;
	lbl2:
    	return 2;
	]])],[ac_cv_prog_emu_cc="$CC"],[ac_cv_prog_emu_cc=no])
	CC=$save_CC
	CFLAGS=$save_CFLAGS
	CPPFLAGS=$save_CPPFLAGS
     ])
])
if test "$ac_cv_prog_emu_cc" = no; then
	AC_DEFINE(NO_JUMP_TABLE,[],[Defined if no found C compiler can handle jump tables])
	EMU_CC="$CC"
else
	EMU_CC="$ac_cv_prog_emu_cc"
fi
AC_SUBST(EMU_CC)
])		
			


dnl ----------------------------------------------------------------------
dnl
dnl LM_PROG_INSTALL_DIR
dnl
dnl This macro may be used by any OTP application.
dnl
dnl Figure out how to create directories with parents.
dnl (In my opinion INSTALL_DIR is a bad name, MKSUBDIRS or something is better)
dnl
dnl We prefer 'install -d', but use 'mkdir -p' if it exists.
dnl If none of these methods works, we give up.
dnl


AC_DEFUN(LM_PROG_INSTALL_DIR,
[AC_CACHE_CHECK(how to create a directory including parents,
ac_cv_prog_mkdir_p,
[
temp_name_base=config.$$
temp_name=$temp_name_base/x/y/z
$INSTALL -d $temp_name >/dev/null 2>&1
ac_cv_prog_mkdir_p=none
if test -d $temp_name; then
        ac_cv_prog_mkdir_p="$INSTALL -d"
else
        mkdir -p $temp_name >/dev/null 2>&1
        if test -d $temp_name; then
                ac_cv_prog_mkdir_p="mkdir -p"
        fi
fi
rm -fr $temp_name_base           
])

case "${ac_cv_prog_mkdir_p}" in
  none) AC_MSG_ERROR(don't know how create directories with parents) ;;
  *)    INSTALL_DIR="$ac_cv_prog_mkdir_p" AC_SUBST(INSTALL_DIR)     ;;
esac
])


dnl ----------------------------------------------------------------------
dnl
dnl LM_PROG_PERL5
dnl
dnl Try to find perl version 5. If found set PERL to the absolute path
dnl of the program, if not found set PERL to false.
dnl
AC_DEFUN(LM_PROG_PERL5,
[AC_PATH_PROGS(PERL, perl5 perl, false,
   /usr/local/bin:/opt/local/bin:/usr/local/gnu/bin:${PATH})
if test "$PERL" = "false"; then
  ac_cv_path_PERL=false
  PERL=false
dnl  AC_MSG_WARN(perl version 5 not found)
fi
])dnl


dnl ----------------------------------------------------------------------
dnl
dnl LM_DECL_SO_BSDCOMPAT
dnl
dnl Check if the system has the SO_BSDCOMPAT flag on sockets (linux) 
dnl
AC_DEFUN(LM_DECL_SO_BSDCOMPAT,
[AC_CACHE_CHECK([for SO_BSDCOMPAT declaration], ac_cv_decl_so_bsdcompat,
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/socket.h>]], [[int i = SO_BSDCOMPAT;]])],[ac_cv_decl_so_bsdcompat=yes],[ac_cv_decl_so_bsdcompat=no]))

case "${ac_cv_decl_so_bsdcompat}" in
  "yes" ) AC_DEFINE(HAVE_SO_BSDCOMPAT,[],
		[Define if you have SO_BSDCOMPAT flag on sockets]) ;;
  * ) ;;
esac
])


dnl ----------------------------------------------------------------------
dnl
dnl LM_DECL_INADDR_LOOPBACK
dnl
dnl Try to find declaration of INADDR_LOOPBACK, if nowhere provide a default
dnl

AC_DEFUN(LM_DECL_INADDR_LOOPBACK,
[AC_CACHE_CHECK([for INADDR_LOOPBACK in netinet/in.h],
 ac_cv_decl_inaddr_loopback,
[AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/types.h>
#include <netinet/in.h>]], [[int i = INADDR_LOOPBACK;]])],[ac_cv_decl_inaddr_loopback=yes],[ac_cv_decl_inaddr_loopback=no])
])

AS_IF(
 [test ${ac_cv_decl_inaddr_loopback} = no],
 [
  AC_CACHE_CHECK([for INADDR_LOOPBACK in rpc/types.h],
                   ac_cv_decl_inaddr_loopback_rpc,
                   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <rpc/types.h>]], [[int i = INADDR_LOOPBACK;]])],[ac_cv_decl_inaddr_loopback_rpc=yes],[ac_cv_decl_inaddr_loopback_rpc=no]))

   AS_IF(
      [test "${ac_cv_decl_inaddr_loopback_rpc}" = "yes"],
      [
        AC_DEFINE(DEF_INADDR_LOOPBACK_IN_RPC_TYPES_H,[],
		[Define if you need to include rpc/types.h to get INADDR_LOOPBACK defined])
      ],
      [
  	AC_CACHE_CHECK([for INADDR_LOOPBACK in winsock2.h],
                   ac_cv_decl_inaddr_loopback_winsock2,
                   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#define WIN32_LEAN_AND_MEAN
				   #include <winsock2.h>]], [[int i = INADDR_LOOPBACK;]])],[ac_cv_decl_inaddr_loopback_winsock2=yes],[ac_cv_decl_inaddr_loopback_winsock2=no]))
	case "${ac_cv_decl_inaddr_loopback_winsock2}" in
     		"yes" )
			AC_DEFINE(DEF_INADDR_LOOPBACK_IN_WINSOCK2_H,[],
				[Define if you need to include winsock2.h to get INADDR_LOOPBACK defined]) ;;
		* )
			# couldn't find it anywhere
        		AC_DEFINE(HAVE_NO_INADDR_LOOPBACK,[],
				[Define if you don't have a definition of INADDR_LOOPBACK]) ;;
	esac
      ])
 ])
])


dnl ----------------------------------------------------------------------
dnl
dnl LM_STRUCT_SOCKADDR_SA_LEN
dnl
dnl Check if the sockaddr structure has the field sa_len
dnl

AC_DEFUN(LM_STRUCT_SOCKADDR_SA_LEN,
[AC_CACHE_CHECK([whether struct sockaddr has sa_len field],
                ac_cv_struct_sockaddr_sa_len,
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/types.h>
#include <sys/socket.h>]], [[struct sockaddr s; s.sa_len = 10;]])],[ac_cv_struct_sockaddr_sa_len=yes],[ac_cv_struct_sockaddr_sa_len=no]))

dnl FIXME convbreak
case ${ac_cv_struct_sockaddr_sa_len} in
  "no" ) AC_DEFINE(NO_SA_LEN,[1],[Define if you dont have salen]) ;;
  *) ;;
esac
])

dnl ----------------------------------------------------------------------
dnl
dnl LM_SYS_IPV6
dnl
dnl Check for ipv6 support and what the in6_addr structure is called.
dnl (early linux used in_addr6 instead of in6_addr)
dnl

AC_DEFUN(LM_SYS_IPV6,
[AC_MSG_CHECKING(for IP version 6 support)
AC_CACHE_VAL(ac_cv_sys_ipv6_support,
[ok_so_far=yes
 AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/types.h>
#ifdef __WIN32__
#include <winsock2.h>
#include <ws2tcpip.h>
#else
#include <netinet/in.h>
#endif]], [[struct in6_addr a6; struct sockaddr_in6 s6;]])],[ok_so_far=yes],[ok_so_far=no])

AS_IF(
 [test $ok_so_far = yes],
 [
  ac_cv_sys_ipv6_support=yes
 ],
 [
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/types.h>
#ifdef __WIN32__
#include <winsock2.h>
#include <ws2tcpip.h>
#else
#include <netinet/in.h>
#endif]], [[struct in_addr6 a6; struct sockaddr_in6 s6;]])],[ac_cv_sys_ipv6_support=in_addr6],[ac_cv_sys_ipv6_support=no])
 ])
])dnl

dnl
dnl Have to use old style AC_DEFINE due to BC with old autoconf.
dnl

case ${ac_cv_sys_ipv6_support} in
  yes)
    AC_MSG_RESULT(yes)
    AC_DEFINE(HAVE_IN6,[1],[Define if ipv6 is present])
    ;;
  in_addr6)
    AC_MSG_RESULT([yes (but I am redefining in_addr6 to in6_addr)])
    AC_DEFINE(HAVE_IN6,[1],[Define if ipv6 is present])
    AC_DEFINE(HAVE_IN_ADDR6_STRUCT,[],[Early linux used in_addr6 instead of in6_addr, define if you have this])
    ;;
  *)
    AC_MSG_RESULT(no)
    ;;
esac
])


dnl ----------------------------------------------------------------------
dnl
dnl LM_SYS_MULTICAST
dnl
dnl Check for multicast support. Only checks for multicast options in
dnl setsockopt(), no check is performed that multicasting actually works.
dnl If options are found defines HAVE_MULTICAST_SUPPORT
dnl

AC_DEFUN(LM_SYS_MULTICAST,
[AC_CACHE_CHECK([for multicast support], ac_cv_sys_multicast_support,
[
AC_EGREP_CPP(^yes$,
[#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#if defined(IP_MULTICAST_TTL) && defined(IP_MULTICAST_LOOP) && defined(IP_MULTICAST_IF) && defined(IP_ADD_MEMBERSHIP) && defined(IP_DROP_MEMBERSHIP)
yes
#endif
], ac_cv_sys_multicast_support=yes, ac_cv_sys_multicast_support=no)])
if test $ac_cv_sys_multicast_support = yes; then
  AC_DEFINE(HAVE_MULTICAST_SUPPORT,[1],
	[Define if setsockopt() accepts multicast options])
fi
])dnl


dnl ----------------------------------------------------------------------
dnl
dnl LM_DECL_SYS_ERRLIST
dnl
dnl Define SYS_ERRLIST_DECLARED if the variable sys_errlist is declared
dnl in a system header file, stdio.h or errno.h.
dnl

AC_DEFUN(LM_DECL_SYS_ERRLIST,
[AC_CACHE_CHECK([for sys_errlist declaration in stdio.h or errno.h],
  ac_cv_decl_sys_errlist,
[AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>
#include <errno.h>]], [[char *msg = *(sys_errlist + 1);]])],[ac_cv_decl_sys_errlist=yes],[ac_cv_decl_sys_errlist=no])])
if test $ac_cv_decl_sys_errlist = yes; then
  AC_DEFINE(SYS_ERRLIST_DECLARED,[],
	[define if the variable sys_errlist is declared in a system header file])
fi
])


dnl ----------------------------------------------------------------------
dnl
dnl LM_CHECK_FUNC_DECL( funname, declaration [, extra includes 
dnl                     [, action-if-found [, action-if-not-found]]] )
dnl
dnl Checks if the declaration "declaration" of "funname" conflicts
dnl with the header files idea of how the function should be
dnl declared. It is useful on systems which lack prototypes and you
dnl need to provide your own (e.g. when you want to take the address
dnl of a function). The 4'th argument is expanded if conflicting, 
dnl the 5'th argument otherwise
dnl
dnl

AC_DEFUN(LM_CHECK_FUNC_DECL,
[AC_MSG_CHECKING([for conflicting declaration of $1])
AC_CACHE_VAL(ac_cv_func_decl_$1,
[AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>
$3]], [[$2
char *c = (char *)$1;
]])],[eval "ac_cv_func_decl_$1=no"],[eval "ac_cv_func_decl_$1=yes"])])
if eval "test \"`echo '$ac_cv_func_decl_'$1`\" = yes"; then
  AC_MSG_RESULT(yes)
  ifelse([$4], , :, [$4])
else
  AC_MSG_RESULT(no)
ifelse([$5], , , [$5
])dnl
fi
])

dnl ----------------------------------------------------------------------
dnl
dnl AC_DOUBLE_MIDDLE_ENDIAN
dnl
dnl Checks whether doubles are represented in "middle-endian" format.
dnl Sets ac_cv_double_middle_endian={no,yes,unknown} accordingly,
dnl as well as DOUBLE_MIDDLE_ENDIAN.
dnl
dnl

AC_DEFUN([AC_C_DOUBLE_MIDDLE_ENDIAN],
[AC_CACHE_CHECK(whether double word ordering is middle-endian, ac_cv_c_double_middle_endian,
[# It does not; compile a test program.
AC_RUN_IFELSE(
[AC_LANG_SOURCE([[#include <stdlib.h>

int
main(void)
{
  int i = 0;
  int zero = 0;
  int bigendian;
  int zero_index = 0;

  union
  {
    long int l;
    char c[sizeof (long int)];
  } u;

  /* we'll use the one with 32-bit words */
  union
  {
    double d;
    unsigned int c[2];
  } vint;

  union
  {
    double d;
    unsigned long c[2];
  } vlong;

  union
  {
    double d;
    unsigned short c[2];
  } vshort;


  /* Are we little or big endian?  From Harbison&Steele.  */
  u.l = 1;
  bigendian = (u.c[sizeof (long int) - 1] == 1);

  zero_index = bigendian ? 1 : 0;

  vint.d = 1.0;
  vlong.d = 1.0;
  vshort.d = 1.0;

  if (sizeof(unsigned int) == 4)
    {
      if (vint.c[zero_index] != 0)
	zero = 1;
    }
  else if (sizeof(unsigned long) == 4)
    {
      if (vlong.c[zero_index] != 0)
	zero = 1;
    }
  else if (sizeof(unsigned short) == 4)
    {
      if (vshort.c[zero_index] != 0)
	zero = 1;
    }

  exit (zero);
}
]])],
	      [ac_cv_c_double_middle_endian=no],
	      [ac_cv_c_double_middle_endian=yes],
	      [ac_cv_c_double_middle=unknown])])
case $ac_cv_c_double_middle_endian in
  yes)
    m4_default([$1],
      [AC_DEFINE([DOUBLE_MIDDLE_ENDIAN], 1,
	[Define to 1 if your processor stores the words in a double in
	 middle-endian format (like some ARMs).])]) ;;
  no)
    $2 ;;
  *)
    m4_default([$3],
      [AC_MSG_WARN([unknown double endianness
presetting ac_cv_c_double_middle_endian=no (or yes) will help])]) ;;
esac
])dnl # AC_C_DOUBLE_MIDDLE_ENDIAN


AC_DEFUN(ERL_MONOTONIC_CLOCK,
[
  # CLOCK_MONOTONIC is buggy on MacOS (darwin), or at least on Big Sur
  # and Monterey, since it may step backwards.
  if test "$3" = "yes"; then
     case $host_os in
          darwin*)
                default_resolution_clock_gettime_monotonic="CLOCK_MONOTONIC_RAW"
                low_resolution_clock_gettime_monotonic="CLOCK_MONOTONIC_RAW_APPROX"
                high_resolution_clock_gettime_monotonic="CLOCK_MONOTONIC_RAW";;
          *)
                default_resolution_clock_gettime_monotonic="CLOCK_HIGHRES CLOCK_BOOTTIME CLOCK_MONOTONIC"
                low_resolution_clock_gettime_monotonic="CLOCK_MONOTONIC_COARSE CLOCK_MONOTONIC_FAST"
                high_resolution_clock_gettime_monotonic="CLOCK_MONOTONIC_PRECISE";;
     esac
  else
     case $host_os in
          darwin*)
                default_resolution_clock_gettime_monotonic="CLOCK_UPTIME_RAW"
                low_resolution_clock_gettime_monotonic="CLOCK_UPTIME_RAW_APPROX"
                high_resolution_clock_gettime_monotonic="CLOCK_UPTIME_RAW";;
                *)
                default_resolution_clock_gettime_monotonic="CLOCK_HIGHRES CLOCK_UPTIME CLOCK_MONOTONIC"
                low_resolution_clock_gettime_monotonic="CLOCK_MONOTONIC_COARSE CLOCK_UPTIME_FAST"
                high_resolution_clock_gettime_monotonic="CLOCK_UPTIME_PRECISE";;
     esac
  fi

  case "$1" in
    high_resolution)
	check_msg="high resolution "
	prefer_resolution_clock_gettime_monotonic="$high_resolution_clock_gettime_monotonic"
	;;
    low_resolution)
	check_msg="low resolution "
	prefer_resolution_clock_gettime_monotonic="$low_resolution_clock_gettime_monotonic"
	;;
    custom_resolution)
	check_msg="custom resolution "
	prefer_resolution_clock_gettime_monotonic="$2"
	;;
    *)
	check_msg="custom "
	prefer_resolution_clock_gettime_monotonic="$2"
	;;
  esac

  clock_gettime_lib=""
  AC_CHECK_LIB(rt, clock_gettime, [clock_gettime_lib="-lrt"])

  save_LIBS="$LIBS"
  LIBS="$LIBS $clock_gettime_lib"

  if test "$LD_MAY_BE_WEAK" != "no"; then
     trust_test="#error May not be there due to weak linking"
  else
     trust_test=""
  fi

  AC_CACHE_CHECK([for clock_gettime(CLOCK_MONOTONIC_RAW, _)], erl_cv_clock_gettime_monotonic_raw,
  [
       AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <time.h>
$trust_test
		      ]], [[
    struct timespec ts;
    long long result;
    clock_gettime(CLOCK_MONOTONIC_RAW, &ts);
    result = ((long long) ts.tv_sec) * 1000000000LL + 
    ((long long) ts.tv_nsec);
		      ]])],[erl_cv_clock_gettime_monotonic_raw=yes],[erl_cv_clock_gettime_monotonic_raw=no])
  ])

  AC_CACHE_CHECK([for clock_gettime() with ${check_msg}monotonic clock type], erl_cv_clock_gettime_monotonic_$1,
  [
     for clock_type in $prefer_resolution_clock_gettime_monotonic $default_resolution_clock_gettime_monotonic $high_resolution_clock_gettime_monotonic $low_resolution_clock_gettime_monotonic; do
       AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <time.h>
$trust_test
		      ]], [[
    struct timespec ts;
    long long result;
    clock_gettime($clock_type,&ts);
    result = ((long long) ts.tv_sec) * 1000000000LL + 
    ((long long) ts.tv_nsec);
		      ]])],[erl_cv_clock_gettime_monotonic_$1=$clock_type],[erl_cv_clock_gettime_monotonic_$1=no])
       test $erl_cv_clock_gettime_monotonic_$1 = no || break
     done
  ])

  LIBS="$save_LIBS"

  AS_IF(
   [test "$LD_MAY_BE_WEAK" != "no"],
   [
    AC_CHECK_FUNCS([clock_get_attributes gethrtime])
   ],
   [
    AC_CHECK_FUNCS([clock_getres clock_get_attributes gethrtime])
   ])
  
  AC_CACHE_CHECK([for mach clock_get_time() with monotonic clock type], erl_cv_mach_clock_get_time_monotonic,
  [
     AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <mach/clock.h>
#include <mach/mach.h>
			]], [[
    kern_return_t res;
    clock_serv_t clk_srv;
    mach_timespec_t time_spec;

    host_get_clock_service(mach_host_self(), SYSTEM_CLOCK, &clk_srv);
    res = clock_get_time(clk_srv, &time_spec);
    mach_port_deallocate(mach_task_self(), clk_srv);
    			]])],[erl_cv_mach_clock_get_time_monotonic=yes],[erl_cv_mach_clock_get_time_monotonic=no])
  ])
  
  erl_corrected_monotonic_clock=no
  case $erl_cv_clock_gettime_monotonic_$1-$ac_cv_func_gethrtime-$erl_cv_mach_clock_get_time_monotonic-$host_os in
    *-*-*-win32)
      erl_monotonic_clock_func=WindowsAPI
      ;;
    CLOCK_*-*-*-linux*)
      case $erl_cv_clock_gettime_monotonic_$1-$erl_cv_clock_gettime_monotonic_raw in
        CLOCK_BOOTTIME-yes|CLOCK_MONOTONIC-yes)
	  erl_corrected_monotonic_clock=yes
	  ;;
	*)
	  # We don't trust CLOCK_MONOTONIC to be NTP
	  # adjusted on linux systems that do not have
	  # CLOCK_MONOTONIC_RAW (although it seems to
	  # be...)
	  ;;
      esac
      erl_monotonic_clock_func=clock_gettime
      ;;
    no-no-no-linux*)
      erl_monotonic_clock_func=times
      ;;
    CLOCK_*-*-*-*)
      erl_monotonic_clock_func=clock_gettime
      ;;
    no-yes-*-*)
      erl_monotonic_clock_func=gethrtime
      ;;
    no-no-yes-*)
      erl_monotonic_clock_func=mach_clock_get_time
      ;;
    no-no-no-*)
      erl_monotonic_clock_func=none
      ;;
  esac

  erl_monotonic_clock_low_resolution=no
  erl_monotonic_clock_lib=
  erl_monotonic_clock_id=
  case $erl_monotonic_clock_func in
    clock_gettime)
      erl_monotonic_clock_id=$erl_cv_clock_gettime_monotonic_$1
      for low_res_id in $low_resolution_clock_gettime_monotonic; do
      	  if test $erl_monotonic_clock_id = $low_res_id; then
	    erl_monotonic_clock_low_resolution=yes
	    break
	  fi
      done
      erl_monotonic_clock_lib=$clock_gettime_lib
      ;;
    mach_clock_get_time)
      erl_monotonic_clock_id=SYSTEM_CLOCK
      ;;
    times)
      erl_monotonic_clock_low_resolution=yes
      ;;
    *)
      ;;
  esac
 
])

AC_DEFUN(ERL_WALL_CLOCK,
[
  default_resolution_clock_gettime_wall="CLOCK_REALTIME"
  low_resolution_clock_gettime_wall="CLOCK_REALTIME_COARSE CLOCK_REALTIME_FAST"
  high_resolution_clock_gettime_wall="CLOCK_REALTIME_PRECISE"

  case "$1" in
    high_resolution)
	check_msg="high resolution "
	prefer_resolution_clock_gettime_wall="$high_resolution_clock_gettime_wall"
	;;
    low_resolution)
	check_msg="low resolution "
	prefer_resolution_clock_gettime_wall="$low_resolution_clock_gettime_wall"
	;;
    custom_resolution)
	check_msg="custom resolution "
	prefer_resolution_clock_gettime_wall="$2"
	;;
    *)
	check_msg=""
	prefer_resolution_clock_gettime_wall=
	;;
  esac

  clock_gettime_lib=""
  AC_CHECK_LIB(rt, clock_gettime, [clock_gettime_lib="-lrt"])

  save_LIBS="$LIBS"
  LIBS="$LIBS $clock_gettime_lib"

  if test "$LD_MAY_BE_WEAK" != "no"; then
     trust_test="#error May not be there due to weak linking"
  else
     trust_test=""
  fi

  AC_CACHE_CHECK([for clock_gettime() with ${check_msg}wall clock type], erl_cv_clock_gettime_wall_$1,
  [
     for clock_type in $prefer_resolution_clock_gettime_wall $default_resolution_clock_gettime_wall $high_resolution_clock_gettime_wall $low_resolution_clock_gettime_wall; do
       AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <time.h>
$trust_test
		      ]], [[
    struct timespec ts;
    long long result;
    clock_gettime($clock_type,&ts);
    result = ((long long) ts.tv_sec) * 1000000000LL + 
    ((long long) ts.tv_nsec);
		      ]])],[erl_cv_clock_gettime_wall_$1=$clock_type],[erl_cv_clock_gettime_wall_$1=no])
       test $erl_cv_clock_gettime_wall_$1 = no || break
     done
  ])

  LIBS="$save_LIBS"

  AS_IF([test "$LD_MAY_BE_WEAK" = "no"],
        [
            AC_CHECK_FUNCS([clock_getres])
        ])

  AC_CHECK_FUNCS([clock_get_attributes gettimeofday])
  
  AC_CACHE_CHECK([for mach clock_get_time() with wall clock type], erl_cv_mach_clock_get_time_wall,
  [
     AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <mach/clock.h>
#include <mach/mach.h>
			]], [[
    kern_return_t res;
    clock_serv_t clk_srv;
    mach_timespec_t time_spec;

    host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &clk_srv);
    res = clock_get_time(clk_srv, &time_spec);
    mach_port_deallocate(mach_task_self(), clk_srv);
    			]])],[erl_cv_mach_clock_get_time_wall=yes],[erl_cv_mach_clock_get_time_wall=no])
  ])

  erl_wall_clock_lib=
  erl_wall_clock_low_resolution=no
  erl_wall_clock_id=
  case $1-$erl_cv_clock_gettime_wall_$1-$erl_cv_mach_clock_get_time_wall-$ac_cv_func_gettimeofday-$host_os in
    *-*-*-*-win32)
      erl_wall_clock_func=WindowsAPI
      erl_wall_clock_low_resolution=yes
      ;;
    high_resolution-no-yes-*-*)
      erl_wall_clock_func=mach_clock_get_time
      erl_wall_clock_id=CALENDAR_CLOCK
      ;;
    *-CLOCK_*-*-*-*)
      erl_wall_clock_func=clock_gettime
      erl_wall_clock_lib=$clock_gettime_lib
      erl_wall_clock_id=$erl_cv_clock_gettime_wall_$1
      for low_res_id in $low_resolution_clock_gettime_wall; do
      	  if test $erl_wall_clock_id = $low_res_id; then
	    erl_wall_clock_low_resolution=yes
	    break
	  fi
      done
      ;;
    *-no-*-yes-*)
      erl_wall_clock_func=gettimeofday
      ;;
    *)
      erl_wall_clock_func=none
      ;;
  esac
])

dnl ----------------------------------------------------------------------
dnl
dnl LM_CHECK_THR_LIB
dnl
dnl This macro may be used by any OTP application.
dnl
dnl LM_CHECK_THR_LIB sets THR_LIBS, THR_DEFS, and THR_LIB_NAME. It also
dnl checks for some pthread headers which will appear in DEFS or config.h.
dnl

AC_DEFUN(LM_CHECK_THR_LIB,
[

NEED_NPTL_PTHREAD_H=no

dnl win32?
AC_MSG_CHECKING([for native win32 threads])
AS_IF(
  [test "X$host_os" = "Xwin32"],
  [
    AC_MSG_RESULT(yes)
    THR_DEFS="-DWIN32_THREADS"
    THR_LIBS=
    THR_LIB_NAME=win32_threads
    THR_LIB_TYPE=win32_threads
  ],
  [
    AC_MSG_RESULT(no)
    THR_DEFS=
    THR_LIBS=
    THR_LIB_NAME=
    THR_LIB_TYPE=posix_unknown

dnl Try to find POSIX threads

dnl The usual pthread lib...
    AC_CHECK_LIB(pthread, pthread_create, THR_LIBS="-lpthread")

dnl Very old versions of FreeBSD have pthreads in special c library, c_r...
    AS_IF([test "x$THR_LIBS" = "x"],
          [
              AC_CHECK_LIB([c_r], [pthread_create], [THR_LIBS="-lc_r"])
          ])

dnl QNX has pthreads in standard C library
    AS_IF([test "x$THR_LIBS" = "x"],
          [
              AC_CHECK_FUNC([pthread_create], [THR_LIBS="none_needed"])
          ])

dnl On ofs1 the '-pthread' switch should be used
    AS_IF(
      [test "x$THR_LIBS" = "x"],
      [
	AC_MSG_CHECKING([if the '-pthread' switch can be used])
	saved_cflags=$CFLAGS
	CFLAGS="$CFLAGS -pthread"
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <pthread.h>]], [[pthread_create((void*)0,(void*)0,(void*)0,(void*)0);]])],[THR_DEFS="-pthread"
		     THR_LIBS="-pthread"],[])
	CFLAGS=$saved_cflags
	if test "x$THR_LIBS" != "x"; then
	    AC_MSG_RESULT(yes)
	else
	    AC_MSG_RESULT(no)
	fi
      ])

    AS_IF(
      [test "x$THR_LIBS" != "x"],
      [
	THR_DEFS="$THR_DEFS -D_THREAD_SAFE -D_REENTRANT -DPOSIX_THREADS"
	THR_LIB_NAME=pthread
	if test "x$THR_LIBS" = "xnone_needed"; then
	    THR_LIBS=
	fi
	AS_CASE(
            [$host_os],
	    [solaris*],
            [
		THR_DEFS="$THR_DEFS -D_POSIX_PTHREAD_SEMANTICS"
            ],
            [linux*],
            [
		THR_DEFS="$THR_DEFS -D_POSIX_THREAD_SAFE_FUNCTIONS"

		LM_CHECK_GETCONF
		AC_MSG_CHECKING(for Native POSIX Thread Library)
		libpthr_vsn=`$GETCONF GNU_LIBPTHREAD_VERSION 2>/dev/null`
		if test $? -eq 0; then
		    case "$libpthr_vsn" in
			*nptl*|*NPTL*) nptl=yes;;
			*) nptl=no;;
		    esac
		elif test "$cross_compiling" = "yes"; then
		    case "$erl_xcomp_linux_nptl" in
			"") nptl=cross;;
			yes|no) nptl=$erl_xcomp_linux_nptl;;
			*) AC_MSG_ERROR([Bad erl_xcomp_linux_nptl value: $erl_xcomp_linux_nptl]);;
		    esac
		else
		    nptl=no
		fi
		AC_MSG_RESULT($nptl)
		if test $nptl = cross; then
		    nptl=yes
		    AC_MSG_WARN([result yes guessed because of cross compilation])
		fi
		AS_IF(
                  [test $nptl = yes],
                  [
		    THR_LIB_TYPE=posix_nptl
		    need_nptl_incldir=no
		    AC_CHECK_HEADER([nptl/pthread.h],
				    [need_nptl_incldir=yes
				     NEED_NPTL_PTHREAD_H=yes])
		    AS_IF(
                      [test $need_nptl_incldir = yes],
                      [
			# Ahh...
			nptl_path="$C_INCLUDE_PATH:$CPATH"
			if test X$cross_compiling != Xyes; then
			    nptl_path="$nptl_path:/usr/local/include:/usr/include"
			else
			    IROOT="$erl_xcomp_isysroot"
			    test "$IROOT" != "" || IROOT="$erl_xcomp_sysroot"
			    test "$IROOT" != "" || AC_MSG_ERROR([Don't know where to search for includes! Please set erl_xcomp_isysroot])
			    nptl_path="$nptl_path:$IROOT/usr/local/include:$IROOT/usr/include"
			fi
			nptl_ws_path=
			save_ifs="$IFS"; IFS=":"
			for dir in $nptl_path; do
			    if test "x$dir" != "x"; then
				nptl_ws_path="$nptl_ws_path $dir"
			    fi
			done
			IFS=$save_ifs
			nptl_incldir=
			for dir in $nptl_ws_path; do
		            AC_CHECK_HEADER([$dir/nptl/pthread.h],
					    [nptl_incldir=$dir/nptl])
			    if test "x$nptl_incldir" != "x"; then
				THR_DEFS="$THR_DEFS -isystem $nptl_incldir"
				break
			    fi
			done
			if test "x$nptl_incldir" = "x"; then
			    AC_MSG_ERROR(Failed to locate nptl system include directory)
			fi
                      ])
                  ])
            ])

	dnl We sometimes need THR_DEFS in order to find certain headers
	dnl (at least for pthread.h on osf1).
	saved_cppflags=$CPPFLAGS
	CPPFLAGS="$CPPFLAGS $THR_DEFS"

	dnl
	dnl Check for headers
	dnl

	AC_CHECK_HEADER(pthread.h,
			AC_DEFINE(HAVE_PTHREAD_H, 1, \
[Define if you have the <pthread.h> header file.]))

	dnl Some Linuxes have <pthread/mit/pthread.h> instead of <pthread.h>
	AC_CHECK_HEADER(pthread/mit/pthread.h, \
			AC_DEFINE(HAVE_MIT_PTHREAD_H, 1, \
[Define if the pthread.h header file is in pthread/mit directory.]))

	dnl restore CPPFLAGS
	CPPFLAGS=$saved_cppflags

      ])
  ])

])

AC_DEFUN(ERL_INTERNAL_LIBS,
[

ERTS_INTERNAL_X_LIBS=

AC_CHECK_LIB(kstat, kstat_open,
[AC_DEFINE(HAVE_KSTAT, 1, [Define if you have kstat])
ERTS_INTERNAL_X_LIBS="$ERTS_INTERNAL_X_LIBS -lkstat"])

AC_SUBST(ERTS_INTERNAL_X_LIBS)

])

AC_DEFUN(ETHR_CHK_GCC_ATOMIC_OP__,
[
    # $1 - atomic_op

    for atomic_bit_size in 32 64 128; do
	case $atomic_bit_size in
	    32) gcc_atomic_type="$gcc_atomic_type32";;
	    64) gcc_atomic_type="$gcc_atomic_type64";;
	    128) gcc_atomic_type="$gcc_atomic_type128";;
	esac
	gcc_atomic_lockfree="int x[[(2*__atomic_always_lock_free(sizeof($gcc_atomic_type), 0))-1]]"
	case $1 in
	    __sync_add_and_fetch | __sync_fetch_and_and | __sync_fetch_and_or)
		atomic_call="volatile $gcc_atomic_type var; $gcc_atomic_type res = $1(&var, ($gcc_atomic_type) 0);"
		;;
	    __sync_val_compare_and_swap)
		atomic_call="volatile $gcc_atomic_type var; $gcc_atomic_type res = $1(&var, ($gcc_atomic_type) 0, ($gcc_atomic_type) 0);"
		;;
	    __atomic_store_n)
		atomic_call="$gcc_atomic_lockfree; volatile $gcc_atomic_type var; $1(&var, ($gcc_atomic_type) 0, __ATOMIC_RELAXED); $1(&var, ($gcc_atomic_type) 0, __ATOMIC_RELEASE);"
		;;
	    __atomic_load_n)
		atomic_call="$gcc_atomic_lockfree; volatile $gcc_atomic_type var; $gcc_atomic_type res = $1(&var, __ATOMIC_RELAXED); res = $1(&var, __ATOMIC_ACQUIRE);"
		;;
	    __atomic_add_fetch| __atomic_fetch_and | __atomic_fetch_or)
		atomic_call="$gcc_atomic_lockfree; volatile $gcc_atomic_type var; $gcc_atomic_type res = $1(&var, ($gcc_atomic_type) 0, __ATOMIC_RELAXED); res = $1(&var, ($gcc_atomic_type) 0, __ATOMIC_ACQUIRE); res = $1(&var, ($gcc_atomic_type) 0, __ATOMIC_RELEASE);"
		;;
	    __atomic_compare_exchange_n)
		atomic_call="$gcc_atomic_lockfree; volatile $gcc_atomic_type var; $gcc_atomic_type val; int res = $1(&var, &val, ($gcc_atomic_type) 0, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED); res = $1(&var, &val, ($gcc_atomic_type) 0, 0, __ATOMIC_ACQUIRE, __ATOMIC_ACQUIRE);"
		;;
	    *)
		AC_MSG_ERROR([Internal error: missing implementation for $1])
		;;
	esac
	eval atomic${atomic_bit_size}_call=\"$atomic_call\"
    done
    
    AC_CACHE_CHECK([for 32-bit $1()], ethr_cv_32bit_$1,
		   [
		       ethr_cv_32bit_$1=no
		       AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[$atomic32_call]])],[ethr_cv_32bit_$1=yes],[])
		   ])
    AC_CACHE_CHECK([for 64-bit $1()], ethr_cv_64bit_$1,
		   [
		       ethr_cv_64bit_$1=no
		       AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[$atomic64_call]])],[ethr_cv_64bit_$1=yes],[])
		   ])
    AC_CACHE_CHECK([for 128-bit $1()], ethr_cv_128bit_$1,
		   [
		       ethr_cv_128bit_$1=no
		       AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[$atomic128_call]])],[ethr_cv_128bit_$1=yes],[])
		   ])

	case $ethr_cv_128bit_$1-$ethr_cv_64bit_$1-$ethr_cv_32bit_$1 in
	    no-no-no)
		have_atomic_ops=0;;
	    no-no-yes)
		have_atomic_ops=4;;
	    no-yes-no)
		have_atomic_ops=8;;
	    no-yes-yes)
		have_atomic_ops=12;;
	    yes-no-no)
		have_atomic_ops=16;;
	    yes-no-yes)
		have_atomic_ops=20;;
	    yes-yes-no)
		have_atomic_ops=24;;
	    yes-yes-yes)
		have_atomic_ops=28;;
	esac
	AC_DEFINE_UNQUOTED([ETHR_HAVE_$1], [$have_atomic_ops], [Define as a bitmask corresponding to the word sizes that $1() can handle on your system])
])

AC_DEFUN(ETHR_CHK_IF_NOOP,
[
   ethr_test_filename="chk_if_$1$3_noop_config1test.$$"
   cat > "${ethr_test_filename}.c" <<EOF
int
my_test(void)
{
    $1$2;
    return 0;
}
EOF
   $CC -O3 $ETHR_DEFS -c "${ethr_test_filename}.c" -o "${ethr_test_filename}1.o"
   cat > "${ethr_test_filename}.c" <<EOF
int
my_test(void)
{
    ;
    return 0;
}
EOF
   $CC -O3 $ETHR_DEFS -c "${ethr_test_filename}.c" -o "${ethr_test_filename}2.o"
   if diff "${ethr_test_filename}1.o" "${ethr_test_filename}2.o" >/dev/null 2>&1; then
      ethr_$1$3_noop=yes
   else
      ethr_$1$3_noop=no
   fi
   rm -f "${ethr_test_filename}.c" "${ethr_test_filename}1.o"  "${ethr_test_filename}2.o" 
])

AC_DEFUN(ETHR_CHK_GCC_ATOMIC_OPS,
[
    AC_CHECK_SIZEOF(short)
    AC_CHECK_SIZEOF(int)
    AC_CHECK_SIZEOF(long)
    AC_CHECK_SIZEOF(long long)
    AC_CHECK_SIZEOF(__int128_t)

    if test "$ac_cv_sizeof_short" = "4"; then
	gcc_atomic_type32="short"
    elif test "$ac_cv_sizeof_int" = "4"; then
	gcc_atomic_type32="int"
    elif test "$ac_cv_sizeof_long" = "4"; then
	gcc_atomic_type32="long"
    else
	AC_MSG_ERROR([No 32-bit type found])
    fi

    if test "$ac_cv_sizeof_int" = "8"; then
	gcc_atomic_type64="int"
    elif test "$ac_cv_sizeof_long" = "8"; then
	gcc_atomic_type64="long"
    elif test "$ac_cv_sizeof_long_long" = "8"; then
	gcc_atomic_type64="long long"
    else
	AC_MSG_ERROR([No 64-bit type found])
    fi

    if test "$ac_cv_sizeof___int128_t" = "16"; then
	gcc_atomic_type128="__int128_t"
    else
	gcc_atomic_type128="#error "	
    fi
    AC_CACHE_CHECK([for a working __sync_synchronize()], ethr_cv___sync_synchronize,
		   [
		       ethr_cv___sync_synchronize=no
		       AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[ __sync_synchronize(); ]])],[ethr_cv___sync_synchronize=yes],[])
		       if test $ethr_cv___sync_synchronize = yes; then
			   #
			   # Old gcc versions on at least x86 have a buggy
			   # __sync_synchronize() which does not emit a
			   # memory barrier. We try to detect this by
			   # compiling to assembly with and without
			   # __sync_synchronize() and compare the results.
			   #
			   ETHR_CHK_IF_NOOP(__sync_synchronize, [()], [])
			   if test $ethr___sync_synchronize_noop = yes; then
			      # Got a buggy implementation of
			      # __sync_synchronize...
			      ethr_cv___sync_synchronize="no; buggy implementation"
			   fi
		       fi
		   ])

    if test "$ethr_cv___sync_synchronize" = "yes"; then
	have_sync_synchronize_value="~0"
    else
	have_sync_synchronize_value="0"
    fi
    AC_DEFINE_UNQUOTED([ETHR_HAVE___sync_synchronize], [$have_sync_synchronize_value], [Define as a bitmask corresponding to the word sizes that __sync_synchronize() can handle on your system])

    ETHR_CHK_GCC_ATOMIC_OP__(__sync_add_and_fetch)
    ETHR_CHK_GCC_ATOMIC_OP__(__sync_fetch_and_and)
    ETHR_CHK_GCC_ATOMIC_OP__(__sync_fetch_and_or)
    ETHR_CHK_GCC_ATOMIC_OP__(__sync_val_compare_and_swap)

    ETHR_CHK_GCC_ATOMIC_OP__(__atomic_store_n)
    ETHR_CHK_GCC_ATOMIC_OP__(__atomic_load_n)
    ETHR_CHK_GCC_ATOMIC_OP__(__atomic_add_fetch)
    ETHR_CHK_GCC_ATOMIC_OP__(__atomic_fetch_and)
    ETHR_CHK_GCC_ATOMIC_OP__(__atomic_fetch_or)
    ETHR_CHK_GCC_ATOMIC_OP__(__atomic_compare_exchange_n)

    ethr_have_gcc_native_atomics=no
    ethr_arm_dbm_sy_instr_val=0
    ethr_arm_dbm_st_instr_val=0
    ethr_arm_dbm_ld_instr_val=0
    ethr_arm_isb_sy_instr_val=0
    ethr_arm_dc_cvau_instr_val=0
    ethr_arm_ic_ivau_instr_val=0
    AS_CASE(
      ["$GCC-$host_cpu"],
      [yes-arm*|yes-aarch*],
      [
	    AC_CACHE_CHECK([for ARM 'dmb sy' instruction], ethr_cv_arm_dbm_sy_instr,
			   [
				ethr_cv_arm_dbm_sy_instr=no
				AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[
						__asm__ __volatile__("dmb sy" : : : "memory");
					    ]])],[ethr_cv_arm_dbm_sy_instr=yes],[])
			   ])
	    if test $ethr_cv_arm_dbm_sy_instr = yes; then
		ethr_arm_dbm_sy_instr_val=1
		test $ethr_cv_64bit___atomic_compare_exchange_n = yes &&
		    ethr_have_gcc_native_atomics=yes
	    fi
	    AC_CACHE_CHECK([for ARM 'dmb st' instruction], ethr_cv_arm_dbm_st_instr,
			   [
				ethr_cv_arm_dbm_st_instr=no
				AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[
						__asm__ __volatile__("dmb st" : : : "memory");
					    ]])],[ethr_cv_arm_dbm_st_instr=yes],[])
			   ])
	    if test $ethr_cv_arm_dbm_st_instr = yes; then
		ethr_arm_dbm_st_instr_val=1
	    fi
	    AC_CACHE_CHECK([for ARM 'dmb ld' instruction], ethr_cv_arm_dbm_ld_instr,
			   [
				ethr_cv_arm_dbm_ld_instr=no
				AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[
						__asm__ __volatile__("dmb ld" : : : "memory");
					    ]])],[ethr_cv_arm_dbm_ld_instr=yes],[])
			   ])
	    if test $ethr_cv_arm_dbm_ld_instr = yes; then
		ethr_arm_dbm_ld_instr_val=1
	    fi
	    AC_CACHE_CHECK([for ARM 'isb sy' instruction], ethr_cv_arm_isb_sy_instr,
			   [
				ethr_cv_arm_isb_sy_instr=no
				AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[
						__asm__ __volatile__("isb sy\n" : : : "memory");
					    ]])],[ethr_cv_arm_isb_sy_instr=yes],[])
			   ])
	    if test $ethr_cv_arm_isb_sy_instr = yes; then
		ethr_arm_isb_sy_instr_val=1
	    fi
	    AC_CACHE_CHECK([for ARM 'dc cvau' instruction], ethr_cv_arm_dc_cvau_instr,
			   [
				ethr_cv_arm_dc_cvau_instr=no
				AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[
						char data[512]; __asm__ __volatile__("dc cvau, %0\n" :: "r" (data) : "memory");
					    ]])],[ethr_cv_arm_dc_cvau_instr=yes],[])
			   ])
	    if test $ethr_cv_arm_dc_cvau_instr = yes; then
		ethr_arm_dc_cvau_instr_val=1
	    fi
	    AC_CACHE_CHECK([for ARM 'ic ivau' instruction], ethr_cv_arm_ic_ivau_instr,
			   [
				ethr_cv_arm_ic_ivau_instr=no
				AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[
						char data[512]; __asm__ __volatile__("ic ivau, %0\n" :: "r" (data) : "memory");
					    ]])],[ethr_cv_arm_ic_ivau_instr=yes],[])
			   ])
	    if test $ethr_cv_arm_ic_ivau_instr = yes; then
		ethr_arm_ic_ivau_instr_val=1
	    fi
      ])

    AC_DEFINE_UNQUOTED([ETHR_HAVE_GCC_ASM_ARM_DMB_INSTRUCTION], [$ethr_arm_dbm_sy_instr_val], [Define as a boolean indicating whether you have a gcc compatible compiler capable of generating the ARM 'dmb sy' instruction, and are compiling for an ARM processor with ARM DMB instruction support, or not])
    AC_DEFINE_UNQUOTED([ETHR_HAVE_GCC_ASM_ARM_DMB_ST_INSTRUCTION], [$ethr_arm_dbm_st_instr_val], [Define as a boolean indicating whether you have a gcc compatible compiler capable of generating the ARM 'dmb st' instruction, and are compiling for an ARM processor with ARM DMB instruction support, or not])
    AC_DEFINE_UNQUOTED([ETHR_HAVE_GCC_ASM_ARM_DMB_LD_INSTRUCTION], [$ethr_arm_dbm_ld_instr_val], [Define as a boolean indicating whether you have a gcc compatible compiler capable of generating the ARM 'dmb ld' instruction, and are compiling for an ARM processor with ARM DMB instruction support, or not])
    AC_DEFINE_UNQUOTED([ETHR_HAVE_GCC_ASM_ARM_ISB_SY_INSTRUCTION], [$ethr_arm_isb_sy_instr_val], [Define as a boolean indicating whether you have a gcc compatible compiler capable of generating the ARM 'isb sy' instruction, and are compiling for an ARM processor with ARM ISB instruction support, or not])
    AC_DEFINE_UNQUOTED([ETHR_HAVE_GCC_ASM_ARM_DC_CVAU_INSTRUCTION], [$ethr_arm_dc_cvau_instr_val], [Define as a boolean indicating whether you have a gcc compatible compiler capable of generating the ARM 'dc cvau' instruction, and are compiling for an ARM processor with ARM DC instruction support, or not])
    AC_DEFINE_UNQUOTED([ETHR_HAVE_GCC_ASM_ARM_IC_IVAU_INSTRUCTION], [$ethr_arm_ic_ivau_instr_val], [Define as a boolean indicating whether you have a gcc compatible compiler capable of generating the ARM 'ic ivau' instruction, and are compiling for an ARM processor with ARM IC instruction support, or not])

    test $ethr_cv_32bit___sync_val_compare_and_swap = yes &&
    	ethr_have_gcc_native_atomics=yes
    test $ethr_cv_64bit___sync_val_compare_and_swap = yes &&
    	ethr_have_gcc_native_atomics=yes
    if test "$ethr_cv___sync_synchronize" = "yes"; then
    	test $ethr_cv_64bit___atomic_compare_exchange_n = yes &&
    	    ethr_have_gcc_native_atomics=yes
    	test $ethr_cv_32bit___atomic_compare_exchange_n = yes &&
    	    ethr_have_gcc_native_atomics=yes
    fi
    ethr_have_gcc_atomic_builtins=0
    if test $ethr_have_gcc_native_atomics = yes; then
       ethr_native_atomic_implementation=gcc_sync
       test $ethr_cv_32bit___atomic_compare_exchange_n = yes && ethr_have_gcc_atomic_builtins=1
       test $ethr_cv_64bit___atomic_compare_exchange_n = yes && ethr_have_gcc_atomic_builtins=1
       test $ethr_have_gcc_atomic_builtins = 1 && ethr_native_atomic_implementation=gcc_atomic_sync
    fi
    AC_DEFINE_UNQUOTED([ETHR_HAVE_GCC___ATOMIC_BUILTINS], [$ethr_have_gcc_atomic_builtins], [Define as a boolean indicating whether you have a gcc __atomic builtins or not])
    test $ethr_have_gcc_native_atomics = yes && ethr_have_native_atomics=yes
])

AC_DEFUN(ETHR_CHK_INTERLOCKED,
[
    ilckd="$1"
    case "$2" in
	"1") ilckd_call="${ilckd}(var);";;
	"2") ilckd_call="${ilckd}(var, ($3) 0);";;
	"3") ilckd_call="${ilckd}(var, ($3) 0, ($3) 0);";;
	"4") ilckd_call="${ilckd}(var, ($3) 0, ($3) 0, arr);";;
    esac
    AC_CACHE_CHECK([for ${ilckd}()],ethr_cv_have_$1,
        [ethr_cv_have_$1=no
         AC_LINK_IFELSE([AC_LANG_PROGRAM([[
	     #define WIN32_LEAN_AND_MEAN
	     #include <windows.h>
	     #include <intrin.h>
	   ]], [[
	     volatile $3 *var;
	     volatile $3 arr[2];

	      $ilckd_call
	      return 0;
	   ]])],[ethr_cv_have_$1=yes],[])])
    if [[ "${ethr_cv_have_$1}" = "yes" ]]; then
      $4
    else
      m4_default([$5], [:])
    fi
])

dnl ----------------------------------------------------------------------
dnl
dnl ERL_FIND_ETHR_LIB
dnl
dnl NOTE! This macro may be changed at any time! Should *only* be used by
dnl       ERTS!
dnl
dnl Find a thread library to use. Sets ETHR_LIBS to libraries to link
dnl with, ETHR_X_LIBS to extra libraries to link with (same as ETHR_LIBS
dnl except that the ethread lib itself is not included), ETHR_DEFS to
dnl defines to compile with, ETHR_THR_LIB_BASE to the name of the
dnl thread library which the ethread library is based on, and ETHR_LIB_NAME
dnl to the name of the library where the ethread implementation is located.
dnl  ERL_FIND_ETHR_LIB currently searches for 'pthreads', and
dnl 'win32_threads'. If no thread library was found ETHR_LIBS, ETHR_X_LIBS,
dnl ETHR_DEFS, ETHR_THR_LIB_BASE, and ETHR_LIB_NAME are all set to the
dnl empty string.
dnl

AC_DEFUN(ERL_FIND_ETHR_LIB,
[

AC_ARG_ENABLE(native-ethr-impls,
	      AS_HELP_STRING([--disable-native-ethr-impls],
                             [disable native ethread implementations]),
[ case "$enableval" in
    no) disable_native_ethr_impls=yes ;;
    *)  disable_native_ethr_impls=no ;;
  esac ], disable_native_ethr_impls=no)

test "X$disable_native_ethr_impls" = "Xyes" &&
  AC_DEFINE(ETHR_DISABLE_NATIVE_IMPLS, 1, [Define if you want to disable native ethread implementations])

AC_ARG_ENABLE(x86-out-of-order,
	      AS_HELP_STRING([--enable-x86-out-of-order],
                             [enable x86/x84_64 out of order support (default disabled)]))

AC_ARG_ENABLE(prefer-gcc-native-ethr-impls,
	      AS_HELP_STRING([--enable-prefer-gcc-native-ethr-impls],
			     [prefer gcc native ethread implementations]),
[ case "$enableval" in
    yes) enable_prefer_gcc_native_ethr_impls=yes ;;
    *)  enable_prefer_gcc_native_ethr_impls=no ;;
  esac ], enable_prefer_gcc_native_ethr_impls=no)

test $enable_prefer_gcc_native_ethr_impls = yes &&
  AC_DEFINE(ETHR_PREFER_GCC_NATIVE_IMPLS, 1, [Define if you prefer gcc native ethread implementations])

AC_ARG_ENABLE(trust-gcc-atomic-builtins-memory-barriers,
	      AS_HELP_STRING([--enable-trust-gcc-atomic-builtins-memory-barriers],
			     [trust gcc atomic builtins memory barriers]),
[ case "$enableval" in
    yes) trust_gcc_atomic_builtins_mbs=1 ;;
    *) trust_gcc_atomic_builtins_mbs=0 ;;
  esac ], trust_gcc_atomic_builtins_mbs=0)

AC_DEFINE_UNQUOTED(ETHR_TRUST_GCC_ATOMIC_BUILTINS_MEMORY_BARRIERS, [$trust_gcc_atomic_builtins_mbs], [Define as a boolean indicating whether you trust gcc's __atomic_* builtins memory barrier implementations, or not])

AC_ARG_WITH(libatomic_ops,
	    AS_HELP_STRING([--with-libatomic_ops=PATH],
			   [specify and prefer usage of libatomic_ops in the ethread library]))

AC_ARG_WITH(with_sparc_memory_order,
	    AS_HELP_STRING([--with-sparc-memory-order=TSO|PSO|RMO],
			   [specify sparc memory order (defaults to RMO)]))

AC_ARG_ENABLE(ppc-lwsync-instruction,
AS_HELP_STRING([--enable-ppc-lwsync-instruction], [enable use of powerpc lwsync instruction])
AS_HELP_STRING([--disable-ppc-lwsync-instruction], [disable use of powerpc lwsync instruction]),
[ case "$enableval" in
    no) enable_lwsync=no ;;
    *)  enable_lwsync=yes ;;
  esac ],
[
  AC_CHECK_SIZEOF(void *)
  case $host_cpu-$ac_cv_sizeof_void_p in
       macppc-8|powerpc-8|ppc-8|powerpc64-8|ppc64-8|powerpc64le-8|ppc64le-8|"Power Macintosh"-8)
           enable_lwsync=yes;;
       *)
           enable_lwsync=undefined;;
  esac ])

case $enable_lwsync in
     no)
       AC_DEFINE(ETHR_PPC_HAVE_NO_LWSYNC, [1], [Define if you do not have the powerpc lwsync instruction])
       ;;
     yes)
       AC_DEFINE(ETHR_PPC_HAVE_LWSYNC, [1], [Define if you have the powerpc lwsync instruction])
       ;;
     *)
       ;;
esac

LM_CHECK_THR_LIB
ERL_INTERNAL_LIBS

ERL_MONOTONIC_CLOCK(try_find_pthread_compatible, CLOCK_HIGHRES CLOCK_UPTIME_RAW CLOCK_MONOTONIC, no)

case $erl_monotonic_clock_func in
  clock_gettime)
    AC_DEFINE(ETHR_HAVE_CLOCK_GETTIME_MONOTONIC, [1], [Define if you have a clock_gettime() with a monotonic clock])
    ;;
  mach_clock_get_time)
    AC_DEFINE(ETHR_HAVE_MACH_CLOCK_GET_TIME, [1], [Define if you have a mach clock_get_time() with a monotonic clock])
    ;;
  gethrtime)
    AC_DEFINE(ETHR_HAVE_GETHRTIME, [1], [Define if you have a monotonic gethrtime()])
    ;;
  *)
    ;;
esac

if test "x$erl_monotonic_clock_id" != "x"; then
    AC_DEFINE_UNQUOTED(ETHR_MONOTONIC_CLOCK_ID, [$erl_monotonic_clock_id], [Define to the monotonic clock id to use])
fi

ethr_native_atomic_implementation=none
ethr_have_native_atomics=no
ethr_have_native_spinlock=no
ETHR_THR_LIB_BASE="$THR_LIB_NAME"
ETHR_THR_LIB_BASE_TYPE="$THR_LIB_TYPE"
ETHR_DEFS="$THR_DEFS"
ETHR_X_LIBS="$THR_LIBS $ERTS_INTERNAL_X_LIBS $erl_monotonic_clock_lib"
ETHR_LIBS=
ETHR_LIB_NAME=

ethr_modified_default_stack_size=

AC_ARG_WITH(threadnames,
AS_HELP_STRING([--with-threadnames], [use pthread_setname to set the thread names (default)])
AS_HELP_STRING([--without-threadnames],
               [do not set any thread names]),
[],
[with_threadnames=yes])

dnl Name of lib where ethread implementation is located
ethr_lib_name=ethread

AS_CASE(
    ["$THR_LIB_NAME"],
    [win32_threads],
    [
	ETHR_THR_LIB_BASE_DIR=win
	# * _WIN32_WINNT >= 0x0400 is needed for
	#   TryEnterCriticalSection
	# * _WIN32_WINNT >= 0x0403 is needed for
	#   InitializeCriticalSectionAndSpinCount
	# The ethread lib will refuse to build if _WIN32_WINNT < 0x0403.
	#
	# -D_WIN32_WINNT should have been defined in $CPPFLAGS; fetch it
	# and save it in ETHR_DEFS.
	found_win32_winnt=no
	for cppflag in $CPPFLAGS; do
	    case $cppflag in
		-DWINVER*)
		    ETHR_DEFS="$ETHR_DEFS $cppflag"
		    ;;
		-D_WIN32_WINNT*)
		    ETHR_DEFS="$ETHR_DEFS $cppflag"
		    found_win32_winnt=yes
		    ;;
		*)
		    ;;
	    esac
        done
        if test $found_win32_winnt = no; then
	    AC_MSG_ERROR([-D_WIN32_WINNT missing in CPPFLAGS])
        fi

	AC_DEFINE(ETHR_WIN32_THREADS, 1, [Define if you have win32 threads])

	if test "X$disable_native_ethr_impls" = "Xyes"; then
	    have_interlocked_op=no
	    ethr_have_native_atomics=no
	else
	    ETHR_CHK_INTERLOCKED([_InterlockedDecrement], [1], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDDECREMENT, 1, [Define if you have _InterlockedDecrement()]))
	    ETHR_CHK_INTERLOCKED([_InterlockedDecrement_rel], [1], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDDECREMENT_REL, 1, [Define if you have _InterlockedDecrement_rel()]))
	    ETHR_CHK_INTERLOCKED([_InterlockedIncrement], [1], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDINCREMENT, 1, [Define if you have _InterlockedIncrement()]))
	    ETHR_CHK_INTERLOCKED([_InterlockedIncrement_acq], [1], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDINCREMENT_ACQ, 1, [Define if you have _InterlockedIncrement_acq()]))
	    ETHR_CHK_INTERLOCKED([_InterlockedExchangeAdd], [2], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDEXCHANGEADD, 1, [Define if you have _InterlockedExchangeAdd()]))
	    ETHR_CHK_INTERLOCKED([_InterlockedExchangeAdd_acq], [2], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDEXCHANGEADD_ACQ, 1, [Define if you have _InterlockedExchangeAdd_acq()]))
	    ETHR_CHK_INTERLOCKED([_InterlockedAnd], [2], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDAND, 1, [Define if you have _InterlockedAnd()]))
	    ETHR_CHK_INTERLOCKED([_InterlockedOr], [2], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDOR, 1, [Define if you have _InterlockedOr()]))
	    ETHR_CHK_INTERLOCKED([_InterlockedExchange], [2], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDEXCHANGE, 1, [Define if you have _InterlockedExchange()]))
	    ETHR_CHK_INTERLOCKED([_InterlockedCompareExchange], [3], [long],
              [AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE, 1, [Define if you have _InterlockedCompareExchange()])
               ethr_have_native_atomics=yes])
	    ETHR_CHK_INTERLOCKED([_InterlockedCompareExchange_acq], [3], [long],
              [AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE_ACQ, 1, [Define if you have _InterlockedCompareExchange_acq()])
               ethr_have_native_atomics=yes])
	    ETHR_CHK_INTERLOCKED([_InterlockedCompareExchange_rel], [3], [long],
              [AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE_REL, 1, [Define if you have _InterlockedCompareExchange_rel()])
              ethr_have_native_atomics=yes])
	    ETHR_CHK_INTERLOCKED([_InterlockedDecrement64], [1], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDDECREMENT64, 1, [Define if you have _InterlockedDecrement64()]))
	    ETHR_CHK_INTERLOCKED([_InterlockedDecrement64_rel], [1], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDDECREMENT64_REL, 1, [Define if you have _InterlockedDecrement64_rel()]))
	    ETHR_CHK_INTERLOCKED([_InterlockedIncrement64], [1], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDINCREMENT64, 1, [Define if you have _InterlockedIncrement64()]))
	    ETHR_CHK_INTERLOCKED([_InterlockedIncrement64_acq], [1], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDINCREMENT64_ACQ, 1, [Define if you have _InterlockedIncrement64_acq()]))
	    ETHR_CHK_INTERLOCKED([_InterlockedExchangeAdd64], [2], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDEXCHANGEADD64, 1, [Define if you have _InterlockedExchangeAdd64()]))
	    ETHR_CHK_INTERLOCKED([_InterlockedExchangeAdd64_acq], [2], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDEXCHANGEADD64_ACQ, 1, [Define if you have _InterlockedExchangeAdd64_acq()]))
	    ETHR_CHK_INTERLOCKED([_InterlockedAnd64], [2], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDAND64, 1, [Define if you have _InterlockedAnd64()]))
	    ETHR_CHK_INTERLOCKED([_InterlockedOr64], [2], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDOR64, 1, [Define if you have _InterlockedOr64()]))
	    ETHR_CHK_INTERLOCKED([_InterlockedExchange64], [2], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDEXCHANGE64, 1, [Define if you have _InterlockedExchange64()]))
	    ETHR_CHK_INTERLOCKED([_InterlockedCompareExchange64], [3], [__int64],
              [AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE64, 1, [Define if you have _InterlockedCompareExchange64()])
               ethr_have_native_atomics=yes])
	    ETHR_CHK_INTERLOCKED([_InterlockedCompareExchange64_acq], [3], [__int64],
              [AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE64_ACQ, 1, [Define if you have _InterlockedCompareExchange64_acq()])
               ethr_have_native_atomics=yes])
	    ETHR_CHK_INTERLOCKED([_InterlockedCompareExchange64_rel], [3], [__int64],
              [AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE64_REL, 1, [Define if you have _InterlockedCompareExchange64_rel()])
               ethr_have_native_atomics=yes])
	    ETHR_CHK_INTERLOCKED([_InterlockedCompareExchange128], [4], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE128, 1, [Define if you have _InterlockedCompareExchange128()]))
	fi
	if test "$ethr_have_native_atomics" = "yes"; then
	   ethr_native_atomic_implementation=windows
	   ethr_have_native_spinlock=yes
	fi
    ],
    [pthread],
    [
	ETHR_THR_LIB_BASE_DIR=pthread
	AC_DEFINE(ETHR_PTHREADS, 1, [Define if you have pthreads])
	case $host_os in
	    openbsd*)
		# The default stack size is insufficient for our needs
		# on OpenBSD. We increase it to 256 kilo words.
		ethr_modified_default_stack_size=256;;
	    linux*)
		ETHR_DEFS="$ETHR_DEFS -D_GNU_SOURCE"

		if test	X$cross_compiling = Xyes; then
		    case X$erl_xcomp_linux_usable_sigusrx in
			X) usable_sigusrx=cross;;
			Xyes|Xno) usable_sigusrx=$erl_xcomp_linux_usable_sigusrx;;
			*) AC_MSG_ERROR([Bad erl_xcomp_linux_usable_sigusrx value: $erl_xcomp_linux_usable_sigusrx]);;
		    esac
		    case X$erl_xcomp_linux_usable_sigaltstack in
			X) usable_sigaltstack=cross;;
			Xyes|Xno) usable_sigaltstack=$erl_xcomp_linux_usable_sigaltstack;;
			*) AC_MSG_ERROR([Bad erl_xcomp_linux_usable_sigaltstack value: $erl_xcomp_linux_usable_sigaltstack]);;
		    esac
		else
		    # FIXME: Test for actual problems instead of kernel versions
		    linux_kernel_vsn_=`uname -r`
		    case $linux_kernel_vsn_ in
			[[0-1]].*|2.[[0-1]]|2.[[0-1]].*)
			    usable_sigusrx=no
			    usable_sigaltstack=no;;
			2.[[2-3]]|2.[[2-3]].*)
			    usable_sigusrx=yes
			    usable_sigaltstack=no;;
		    	*)
			    usable_sigusrx=yes
			    usable_sigaltstack=yes;;
		    esac
		fi

		AC_MSG_CHECKING(if SIGUSR1 and SIGUSR2 can be used)
		AC_MSG_RESULT($usable_sigusrx)
		if test $usable_sigusrx = cross; then
		    usable_sigusrx=yes
		    AC_MSG_WARN([result yes guessed because of cross compilation])
		fi
		if test $usable_sigusrx = no; then
		    ETHR_DEFS="$ETHR_DEFS -DETHR_UNUSABLE_SIGUSRX"
		fi

		AC_MSG_CHECKING(if sigaltstack can be used)
		AC_MSG_RESULT($usable_sigaltstack)
		if test $usable_sigaltstack = cross; then
		    usable_sigaltstack=yes
		    AC_MSG_WARN([result yes guessed because of cross compilation])
		fi
		if test $usable_sigaltstack = no; then
		    ETHR_DEFS="$ETHR_DEFS -DETHR_UNUSABLE_SIGALTSTACK"
		fi
		;;
	    *) ;;
	esac

	dnl We sometimes need ETHR_DEFS in order to find certain headers
	dnl (at least for pthread.h on osf1).
	saved_cppflags="$CPPFLAGS"
	CPPFLAGS="$CPPFLAGS $ETHR_DEFS"

	dnl We need the thread library in order to find some functions
	saved_libs="$LIBS"
	LIBS="$LIBS $ETHR_X_LIBS"

	dnl
	dnl Check for headers
	dnl
	AC_CHECK_HEADER(pthread.h, \
			AC_DEFINE(ETHR_HAVE_PTHREAD_H, 1, \
[Define if you have the <pthread.h> header file.]))

	dnl Some Linuxes have <pthread/mit/pthread.h> instead of <pthread.h>
	AC_CHECK_HEADER(pthread/mit/pthread.h, \
			AC_DEFINE(ETHR_HAVE_MIT_PTHREAD_H, 1, \
[Define if the pthread.h header file is in pthread/mit directory.]))

	if test $NEED_NPTL_PTHREAD_H = yes; then
	    AC_DEFINE(ETHR_NEED_NPTL_PTHREAD_H, 1, \
[Define if you need the <nptl/pthread.h> header file.])
	fi

	AC_CHECK_HEADER(sched.h, \
			AC_DEFINE(ETHR_HAVE_SCHED_H, 1, \
[Define if you have the <sched.h> header file.]))

	AC_CHECK_HEADER(sys/time.h, \
			AC_DEFINE(ETHR_HAVE_SYS_TIME_H, 1, \
[Define if you have the <sys/time.h> header file.]))

	AC_MSG_CHECKING([for usable PTHREAD_STACK_MIN])
	pthread_stack_min=no
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <limits.h>
#if defined(ETHR_NEED_NPTL_PTHREAD_H)
#include <nptl/pthread.h>
#elif defined(ETHR_HAVE_MIT_PTHREAD_H)
#include <pthread/mit/pthread.h>
#elif defined(ETHR_HAVE_PTHREAD_H)
#include <pthread.h>
#endif
			]], [[return PTHREAD_STACK_MIN;]])],[pthread_stack_min=yes],[])

	AC_MSG_RESULT([$pthread_stack_min])
	test $pthread_stack_min != yes || {
	     AC_DEFINE(ETHR_HAVE_USABLE_PTHREAD_STACK_MIN, 1, [Define if you can use PTHREAD_STACK_MIN])
	}

	dnl
	dnl Check for functions
	dnl
	AC_CHECK_FUNC([pthread_spin_lock], \
			[ethr_have_native_spinlock=yes \
			 AC_DEFINE(ETHR_HAVE_PTHREAD_SPIN_LOCK, 1, \
[Define if you have the pthread_spin_lock function.])])

	have_sched_yield=no
	have_librt_sched_yield=no
	AC_CHECK_FUNC([sched_yield], [have_sched_yield=yes])
	AS_IF(
          [test $have_sched_yield = no],
          [
	    AC_CHECK_LIB([rt], [sched_yield],
			 [have_librt_sched_yield=yes
			  ETHR_X_LIBS="$ETHR_X_LIBS -lrt"])
          ])
	AS_IF(
          [test $have_sched_yield = yes || test $have_librt_sched_yield = yes],
          [
	    AC_DEFINE(ETHR_HAVE_SCHED_YIELD, 1, [Define if you have the sched_yield() function.])
	    AC_MSG_CHECKING([whether sched_yield() returns an int])
	    sched_yield_ret_int=no
	    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
				#ifdef ETHR_HAVE_SCHED_H
				#include <sched.h>
				#endif
			   ]], [[int sched_yield();]])],[sched_yield_ret_int=yes],[])
	    AC_MSG_RESULT([$sched_yield_ret_int])
	    if test $sched_yield_ret_int = yes; then
		AC_DEFINE(ETHR_SCHED_YIELD_RET_INT, 1, [Define if sched_yield() returns an int.])
	    fi
          ])

	have_pthread_yield=no
	AC_CHECK_FUNC([pthread_yield], [have_pthread_yield=yes])
	AS_IF(
          [test $have_pthread_yield = yes],
          [
	    AC_DEFINE(ETHR_HAVE_PTHREAD_YIELD, 1, [Define if you have the pthread_yield() function.])
	    AC_MSG_CHECKING([whether pthread_yield() returns an int])
	    pthread_yield_ret_int=no
	    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
				#if defined(ETHR_NEED_NPTL_PTHREAD_H)
				#include <nptl/pthread.h>
				#elif defined(ETHR_HAVE_MIT_PTHREAD_H)
				#include <pthread/mit/pthread.h>
				#elif defined(ETHR_HAVE_PTHREAD_H)
				#include <pthread.h>
				#endif
			   ]], [[int pthread_yield();]])],[pthread_yield_ret_int=yes],[])
	    AC_MSG_RESULT([$pthread_yield_ret_int])
	    if test $pthread_yield_ret_int = yes; then
		AC_DEFINE(ETHR_PTHREAD_YIELD_RET_INT, 1, [Define if pthread_yield() returns an int.])
	    fi
          ])

	have_pthread_rwlock_init=no
	AC_CHECK_FUNC(pthread_rwlock_init, [have_pthread_rwlock_init=yes])
	AS_IF(
          [test $have_pthread_rwlock_init = yes],
          [
	    ethr_have_pthread_rwlockattr_setkind_np=no
	    AC_CHECK_FUNC(pthread_rwlockattr_setkind_np,
			  [ethr_have_pthread_rwlockattr_setkind_np=yes])

	    AS_IF(
              [test $ethr_have_pthread_rwlockattr_setkind_np = yes],
              [
		AC_DEFINE(ETHR_HAVE_PTHREAD_RWLOCKATTR_SETKIND_NP, 1, \
[Define if you have the pthread_rwlockattr_setkind_np() function.])

		AC_MSG_CHECKING([for PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP])
		ethr_pthread_rwlock_writer_nonrecursive_initializer_np=no
		AC_LINK_IFELSE([AC_LANG_PROGRAM([[
				#if defined(ETHR_NEED_NPTL_PTHREAD_H)
				#include <nptl/pthread.h>
				#elif defined(ETHR_HAVE_MIT_PTHREAD_H)
				#include <pthread/mit/pthread.h>
				#elif defined(ETHR_HAVE_PTHREAD_H)
				#include <pthread.h>
				#endif
			    ]], [[
				pthread_rwlockattr_t *attr;
				return pthread_rwlockattr_setkind_np(attr,
				    PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP);
			    ]])],[ethr_pthread_rwlock_writer_nonrecursive_initializer_np=yes],[])
		AC_MSG_RESULT([$ethr_pthread_rwlock_writer_nonrecursive_initializer_np])
		if test $ethr_pthread_rwlock_writer_nonrecursive_initializer_np = yes; then
		    AC_DEFINE(ETHR_HAVE_PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP, 1, \
[Define if you have the PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP rwlock attribute.])
		fi
              ])
          ])

	if test "$force_pthread_rwlocks" = "yes"; then

	    AC_DEFINE(ETHR_FORCE_PTHREAD_RWLOCK, 1, \
[Define if you want to force usage of pthread rwlocks])

	    if test $have_pthread_rwlock_init = yes; then
		AC_MSG_WARN([Forced usage of pthread rwlocks. Note that this implementation may suffer from starvation issues.])
	    else
		AC_MSG_ERROR([User forced usage of pthread rwlock, but no such implementation was found])
	    fi
	fi

	AC_CHECK_FUNC(pthread_attr_setguardsize, \
			AC_DEFINE(ETHR_HAVE_PTHREAD_ATTR_SETGUARDSIZE, 1, \
[Define if you have the pthread_attr_setguardsize function.]))

	AS_IF(
         [test "x$erl_monotonic_clock_id" != "x"],
         [
	  AC_MSG_CHECKING(whether pthread_cond_timedwait() can use the monotonic clock $erl_monotonic_clock_id for timeout)
	  pthread_cond_timedwait_monotonic=no
	  AC_LINK_IFELSE([AC_LANG_PROGRAM([[
			#if defined(ETHR_NEED_NPTL_PTHREAD_H)
			#  include <nptl/pthread.h>
			#elif defined(ETHR_HAVE_MIT_PTHREAD_H)
			#  include <pthread/mit/pthread.h>
			#elif defined(ETHR_HAVE_PTHREAD_H)
			#  include <pthread.h>
			#endif
			#include <time.h>
			#ifdef ETHR_HAVE_SYS_TIME_H
			#  include <sys/time.h>
			#endif
			#if defined(ETHR_HAVE_MACH_CLOCK_GET_TIME)
			#  include <mach/clock.h>
			#  include <mach/mach.h>
			#endif
			]], [[
			int res;
			pthread_condattr_t attr;
			pthread_cond_t cond;
			struct timespec cond_timeout;
			pthread_mutex_t mutex;
			res = pthread_condattr_init(&attr);
			res = pthread_condattr_setclock(&attr, ETHR_MONOTONIC_CLOCK_ID);
			res = pthread_cond_init(&cond, &attr);
			res = pthread_cond_timedwait(&cond, &mutex, &cond_timeout);
			]])],[pthread_cond_timedwait_monotonic=yes],[])
	  AC_MSG_RESULT([$pthread_cond_timedwait_monotonic])
	  if test $pthread_cond_timedwait_monotonic = yes; then
	    AC_DEFINE(ETHR_HAVE_PTHREAD_COND_TIMEDWAIT_MONOTONIC, [1], [Define if pthread_cond_timedwait() can be used with a monotonic clock])
	  fi
	 ])

	linux_futex=no
	AC_MSG_CHECKING([for Linux futexes])
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[
			#include <sys/syscall.h>
			#include <unistd.h>
			#include <linux/futex.h>
			#include <sys/time.h>
		    ]], [[
			int i = 1;
			syscall(__NR_futex, (void *) &i, FUTEX_WAKE, 1,
				(void*)0,(void*)0, 0);
			syscall(__NR_futex, (void *) &i, FUTEX_WAIT, 0,
				(void*)0,(void*)0, 0);
			return 0;
		    ]])],[linux_futex=yes],[])
	AC_MSG_RESULT([$linux_futex])
	test $linux_futex = yes && AC_DEFINE(ETHR_HAVE_LINUX_FUTEX, 1, [Define if you have a linux futex implementation.])

	pthread_setname=no
	AC_MSG_CHECKING([for pthread_setname_np])
	old_CFLAGS=$CFLAGS
	CFLAGS="$CFLAGS -Werror"
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[#define __USE_GNU
                     #include <pthread.h>]], [[pthread_setname_np(pthread_self(), "name");]])],[pthread_setname=linux],[])
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[#define __USE_GNU
                     #include <pthread.h>]], [[pthread_set_name_np(pthread_self(), "name");]])],[pthread_setname=bsd],[])
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[#define _DARWIN_C_SOURCE
                     #include <pthread.h>]], [[pthread_setname_np("name");]])],[pthread_setname=darwin],[])
        AC_MSG_RESULT([$pthread_setname])
        case $with_threadnames-$pthread_setname in
             yes-linux) AC_DEFINE(ETHR_HAVE_PTHREAD_SETNAME_NP_2, 1,
                          [Define if you have linux style pthread_setname_np]);;
             yes-bsd) AC_DEFINE(ETHR_HAVE_PTHREAD_SET_NAME_NP_2, 1,
                          [Define if you have bsd style pthread_set_name_np]);;
             yes-darwin) AC_DEFINE(ETHR_HAVE_PTHREAD_SETNAME_NP_1, 1,
                          [Define if you have darwin style pthread_setname_np]);;
             *) ;;
	esac

	pthread_getname=no
	AC_MSG_CHECKING([for pthread_getname_np])
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[#define __USE_GNU
                     #define _DARWIN_C_SOURCE
                     #include <pthread.h>]], [[char buff[256]; pthread_getname_np(pthread_self(), buff, 256);]])],[pthread_getname=linux],[])
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[#define __USE_GNU
                     #define _DARWIN_C_SOURCE
                     #include <pthread.h>]], [[char buff[256]; pthread_getname_np(pthread_self(), buff);]])],[pthread_getname=ibm],[])
        AC_MSG_RESULT([$pthread_getname])
        case $pthread_getname in
             linux) AC_DEFINE(ETHR_HAVE_PTHREAD_GETNAME_NP_3, 1,
                          [Define if you have linux style pthread_getname_np]);;
             ibm) AC_DEFINE(ETHR_HAVE_PTHREAD_GETNAME_NP_2, 1,
                          [Define if you have ibm style pthread_getname_np]);;
             *) ;;
	esac
	CFLAGS=$old_CFLAGS

	AS_IF(
          [test "X$disable_native_ethr_impls" = "Xyes"],
          [
	    ethr_have_native_atomics=no
          ],
          [
	    ETHR_CHK_GCC_ATOMIC_OPS([])

	    AC_MSG_CHECKING([for a usable libatomic_ops implementation])
	    case "x$with_libatomic_ops" in
	        xno | xyes | x)
	    	    libatomic_ops_include=
	    	    ;;
	        *)
	    	    if test -d "${with_libatomic_ops}/include"; then
	    	        libatomic_ops_include="-I$with_libatomic_ops/include"
	    	        CPPFLAGS="$CPPFLAGS $libatomic_ops_include"
	    	    else
	    	        AC_MSG_ERROR([libatomic_ops include directory $with_libatomic_ops/include not found])
	    	    fi;;
	    esac
	    ethr_have_libatomic_ops=no
	    AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include "atomic_ops.h"]], [[
	    	    	    volatile AO_t x;
	    	    	    AO_t y;
	    	    	    int z;

	    	    	    AO_nop_full();
#if defined(AO_HAVE_store)
	    	    	    AO_store(&x, (AO_t) 0);
#elif defined(AO_HAVE_store_release)
	    	    	    AO_store_release(&x, (AO_t) 0);
#else
#error No store
#endif
#if defined(AO_HAVE_load)
	    	    	    z = AO_load(&x);
#elif defined(AO_HAVE_load_acquire)
	    	    	    z = AO_load_acquire(&x);
#else
#error No load
#endif
#if defined(AO_HAVE_compare_and_swap_full)
	    	    	    z = AO_compare_and_swap_full(&x, (AO_t) 0, (AO_t) 1);
#elif defined(AO_HAVE_compare_and_swap_release)
	    	    	    z = AO_compare_and_swap_release(&x, (AO_t) 0, (AO_t) 1);
#elif defined(AO_HAVE_compare_and_swap_acquire)
	    	    	    z = AO_compare_and_swap_acquire(&x, (AO_t) 0, (AO_t) 1);
#elif defined(AO_HAVE_compare_and_swap)
	    	    	    z = AO_compare_and_swap(&x, (AO_t) 0, (AO_t) 1);
#else
#error No compare_and_swap
#endif
	    	        ]])],[ethr_have_native_atomics=yes
			 ethr_native_atomic_implementation=libatomic_ops
	    	         ethr_have_libatomic_ops=yes],[])
	    AC_MSG_RESULT([$ethr_have_libatomic_ops])
	    AS_IF(
              [test $ethr_have_libatomic_ops = yes],
              [
	        AC_CHECK_SIZEOF(AO_t, ,
	    	    	        [
	    	    	    	    #include <stdio.h>
	    	    	    	    #include "atomic_ops.h"
	    	    	        ])
	        AC_DEFINE_UNQUOTED(ETHR_SIZEOF_AO_T, $ac_cv_sizeof_AO_t, [Define to the size of AO_t if libatomic_ops is used])

	        AC_DEFINE(ETHR_HAVE_LIBATOMIC_OPS, 1, [Define if you have libatomic_ops atomic operations])
	        if test "x$with_libatomic_ops" != "xno" && test "x$with_libatomic_ops" != "x"; then
	    	    AC_DEFINE(ETHR_PREFER_LIBATOMIC_OPS_NATIVE_IMPLS, 1, [Define if you prefer libatomic_ops native ethread implementations])
	        fi
	        ETHR_DEFS="$ETHR_DEFS $libatomic_ops_include"
              ],
              [test "x$with_libatomic_ops" != "xno" && test "x$with_libatomic_ops" != "x"],
              [
	        AC_MSG_ERROR([No usable libatomic_ops implementation found])
              ])

	    case "$host_cpu" in
	      sparc | sun4u | sparc64 | sun4v)
	    	    case "$with_sparc_memory_order" in
	    	        "TSO")
	    	    	    AC_DEFINE(ETHR_SPARC_TSO, 1, [Define if only run in Sparc TSO mode]);;
	    	        "PSO")
	    	    	    AC_DEFINE(ETHR_SPARC_PSO, 1, [Define if only run in Sparc PSO, or TSO mode]);;
	    	        "RMO"|"")
	    	    	    AC_DEFINE(ETHR_SPARC_RMO, 1, [Define if run in Sparc RMO, PSO, or TSO mode]);;
	    	        *)
	    	    	    AC_MSG_ERROR([Unsupported Sparc memory order: $with_sparc_memory_order]);;
	    	    esac
		    ethr_native_atomic_implementation=ethread
	    	    ethr_have_native_atomics=yes;; 
	      i86pc | i*86 | x86_64 | amd64)
	    	    if test "$enable_x86_out_of_order" = "yes"; then
	    	    	    AC_DEFINE(ETHR_X86_OUT_OF_ORDER, 1, [Define if x86/x86_64 out of order instructions should be synchronized])
	    	    fi
		    ethr_native_atomic_implementation=ethread
	    	    ethr_have_native_atomics=yes;;
	      macppc | ppc | powerpc | "Power Macintosh")
	      	    ethr_native_atomic_implementation=ethread
	    	    ethr_have_native_atomics=yes;;
	      tile)
	            ethr_native_atomic_implementation=ethread
	    	    ethr_have_native_atomics=yes;;
	      *)
	    	    ;;
	    esac

          ])

	test ethr_have_native_atomics = "yes" && ethr_have_native_spinlock=yes

	dnl Restore LIBS
	LIBS=$saved_libs
	dnl restore CPPFLAGS
	CPPFLAGS=$saved_cppflags

    ])

AC_MSG_CHECKING([whether default stack size should be modified])
if test "x$ethr_modified_default_stack_size" != "x"; then
	AC_DEFINE_UNQUOTED(ETHR_MODIFIED_DEFAULT_STACK_SIZE, $ethr_modified_default_stack_size, [Define if you want to modify the default stack size])
	AC_MSG_RESULT([yes; to $ethr_modified_default_stack_size kilo words])
else
	AC_MSG_RESULT([no])
fi

if test "x$ETHR_THR_LIB_BASE" != "x"; then
	ETHR_DEFS="-DUSE_THREADS $ETHR_DEFS"
	ETHR_LIBS="-l$ethr_lib_name -lerts_internal_r $ETHR_X_LIBS"
	ETHR_LIB_NAME=$ethr_lib_name
fi

AC_CHECK_SIZEOF(void *)
AC_DEFINE_UNQUOTED(ETHR_SIZEOF_PTR, $ac_cv_sizeof_void_p, [Define to the size of pointers])

AC_CHECK_SIZEOF(int)
AC_DEFINE_UNQUOTED(ETHR_SIZEOF_INT, $ac_cv_sizeof_int, [Define to the size of int])
AC_CHECK_SIZEOF(long)
AC_DEFINE_UNQUOTED(ETHR_SIZEOF_LONG, $ac_cv_sizeof_long, [Define to the size of long])
AC_CHECK_SIZEOF(long long)
AC_DEFINE_UNQUOTED(ETHR_SIZEOF_LONG_LONG, $ac_cv_sizeof_long_long, [Define to the size of long long])
AC_CHECK_SIZEOF(__int64)
AC_DEFINE_UNQUOTED(ETHR_SIZEOF___INT64, $ac_cv_sizeof___int64, [Define to the size of __int64])
AC_CHECK_SIZEOF(__int128_t)
AC_DEFINE_UNQUOTED(ETHR_SIZEOF___INT128_T, $ac_cv_sizeof___int128_t, [Define to the size of __int128_t])


case X$erl_xcomp_bigendian in
    X) ;;
    Xyes|Xno) ac_cv_c_bigendian=$erl_xcomp_bigendian;;
    *) AC_MSG_ERROR([Bad erl_xcomp_bigendian value: $erl_xcomp_bigendian]);;
esac

AC_C_BIGENDIAN

if test "$ac_cv_c_bigendian" = "yes"; then
    AC_DEFINE(ETHR_BIGENDIAN, 1, [Define if bigendian])
fi

case X$erl_xcomp_double_middle_endian in
    X) ;;
    Xyes|Xno|Xunknown) ac_cv_c_double_middle_endian=$erl_xcomp_double_middle_endian;;
    *) AC_MSG_ERROR([Bad erl_xcomp_double_middle_endian value: $erl_xcomp_double_middle_endian]);;
esac

AC_C_DOUBLE_MIDDLE_ENDIAN

ETHR_X86_SSE2_ASM=no
AS_CASE(
 ["$GCC-$ac_cv_sizeof_void_p-$host_cpu"],
 [yes-4-i86pc | yes-4-i*86 | yes-4-x86_64 | yes-4-amd64],
 [
    AC_MSG_CHECKING([for gcc sse2 asm support])
    save_CFLAGS="$CFLAGS"
    CFLAGS="$CFLAGS -msse2"
    gcc_sse2_asm=no
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
		long long x, *y;
		__asm__ __volatile__("movq %1, %0\n\t" : "=x"(x) : "m"(*y) : "memory");
	]])],[gcc_sse2_asm=yes],[])
    CFLAGS="$save_CFLAGS"
    AC_MSG_RESULT([$gcc_sse2_asm])
    if test "$gcc_sse2_asm" = "yes"; then
      AC_DEFINE(ETHR_GCC_HAVE_SSE2_ASM_SUPPORT, 1, [Define if you use a gcc that supports -msse2 and understand sse2 specific asm statements])
      ETHR_X86_SSE2_ASM=yes
    fi
 ])

AS_CASE(
  ["$GCC-$host_cpu"],
  [yes-i86pc | yes-i*86 | yes-x86_64 | yes-amd64],
  [
    if test $ac_cv_sizeof_void_p = 4; then
       dw_cmpxchg="cmpxchg8b"
    else
       dw_cmpxchg="cmpxchg16b"
    fi

    gcc_dw_cmpxchg_asm=no
    gcc_pic_dw_cmpxchg_asm=no
    gcc_cflags_pic=no
    gcc_cmpxchg8b_pic_no_clobber_ebx=no
    gcc_cmpxchg8b_pic_no_clobber_ebx_register_shortage=no

    save_CFLAGS="$CFLAGS"

    # Check if it works out of the box using passed CFLAGS
    # and with -fPIC added to CFLAGS if the passed CFLAGS
    # doesn't trigger position independent code
    pic_cmpxchg=unknown
    while true; do

        case $pic_cmpxchg in
	  yes) pic_text="pic ";;
	  *) pic_text="";;
	esac

	AC_MSG_CHECKING([for gcc $pic_text$dw_cmpxchg plain asm support])    

	plain_cmpxchg=no
    	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
    char xchgd;
    long new[2], xchg[2], *p;		  
    __asm__ __volatile__(
#if ETHR_SIZEOF_PTR == 4
	"lock; cmpxchg8b %0\n\t"
#else
	"lock; cmpxchg16b %0\n\t"
#endif
	"setz %3\n\t"
	: "=m"(*p), "=d"(xchg[1]), "=a"(xchg[0]), "=q"(xchgd)
	: "m"(*p), "1"(xchg[1]), "2"(xchg[0]), "c"(new[1]), "b"(new[0])
	: "cc", "memory");
	]])],[plain_cmpxchg=yes],[])

	AC_MSG_RESULT([$plain_cmpxchg])

	if test $pic_cmpxchg = yes; then
	   gcc_pic_dw_cmpxchg_asm=$plain_cmpxchg
	   break
	fi

	gcc_dw_cmpxchg_asm=$plain_cmpxchg

    	# If not already compiling to position independent
	# code add -fPIC to CFLAGS and do it again. This
	# since we want also want to know how to compile
	# to position independent code since this might
	# cause problems with the use of the EBX register
	# as input to the asm on 32-bit x86 and old gcc
	# compilers (gcc vsn < 5).

    	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
#if !defined(__PIC__) || !__PIC__
#  error no pic
#endif
	]])],[pic_cmpxchg=yes
	 gcc_cflags_pic=yes],[pic_cmpxchg=no])

	if test $pic_cmpxchg = yes; then
	   gcc_pic_dw_cmpxchg_asm=$gcc_dw_cmpxchg_asm
	   break
	fi

	CFLAGS="$save_CFLAGS -fPIC"
	pic_cmpxchg=yes

    done

    AS_IF(
     [test $gcc_pic_dw_cmpxchg_asm = no && test $ac_cv_sizeof_void_p = 4],
     [ 

      AC_MSG_CHECKING([for gcc pic cmpxchg8b asm support with EBX workaround])

      # Check if we can work around it by managing the ebx
      # register explicitly in the asm...

      AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
    char xchgd;
    long new[2], xchg[2], *p;		  
    __asm__ __volatile__(
	"pushl %%ebx\n\t"
	"movl %8, %%ebx\n\t"
	"lock; cmpxchg8b %0\n\t"
	"setz %3\n\t"
	"popl %%ebx\n\t"
	: "=m"(*p), "=d"(xchg[1]), "=a"(xchg[0]), "=q"(xchgd)
	: "m"(*p), "1"(xchg[1]), "2"(xchg[0]), "c"(new[1]), "r"(new[0])
	: "cc", "memory");
	]])],[gcc_pic_dw_cmpxchg_asm=yes
	 gcc_cmpxchg8b_pic_no_clobber_ebx=yes],[])     

      AC_MSG_RESULT([$gcc_pic_dw_cmpxchg_asm])

      AS_IF(
       [test $gcc_pic_dw_cmpxchg_asm = no],
       [

      	AC_MSG_CHECKING([for gcc pic cmpxchg8b asm support with EBX and register shortage workarounds])
        # If no optimization is enabled we sometimes get a
	# register shortage. Check if we can work around
	# this...

      	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
      char xchgd;
      long new[2], xchg[2], *p;
      __asm__ __volatile__(
	"pushl %%ebx\n\t"
	"movl (%7), %%ebx\n\t"
	"movl 4(%7), %%ecx\n\t"
	"lock; cmpxchg8b %0\n\t"
	"setz %3\n\t"
	"popl %%ebx\n\t"
	: "=m"(*p), "=d"(xchg[1]), "=a"(xchg[0]), "=c"(xchgd)
	: "m"(*p), "1"(xchg[1]), "2"(xchg[0]), "r"(new)
	: "cc", "memory");

	]])],[gcc_pic_dw_cmpxchg_asm=yes
	 gcc_cmpxchg8b_pic_no_clobber_ebx=yes
	 gcc_cmpxchg8b_pic_no_clobber_ebx_register_shortage=yes],[])

        AC_MSG_RESULT([$gcc_pic_dw_cmpxchg_asm])
       ])

      if test $gcc_cflags_pic = yes; then
        gcc_dw_cmpxchg_asm=$gcc_pic_dw_cmpxchg_asm
      fi
     ])

    CFLAGS="$save_CFLAGS"

    if test "$gcc_cmpxchg8b_pic_no_clobber_ebx" = "yes"; then
      AC_DEFINE(ETHR_CMPXCHG8B_PIC_NO_CLOBBER_EBX, 1, [Define if gcc won't let you clobber ebx with cmpxchg8b and position independent code])
    fi
    if test "$gcc_cmpxchg8b_pic_no_clobber_ebx_register_shortage" = "yes"; then
      AC_DEFINE(ETHR_CMPXCHG8B_REGISTER_SHORTAGE, 1, [Define if you get a register shortage with cmpxchg8b and position independent code])
    fi
    if test "$gcc_dw_cmpxchg_asm" = "yes"; then
      AC_DEFINE(ETHR_GCC_HAVE_DW_CMPXCHG_ASM_SUPPORT, 1, [Define if you use a gcc that supports the double word cmpxchg instruction])
    fi
 ])

AC_DEFINE(ETHR_HAVE_ETHREAD_DEFINES, 1, \
[Define if you have all ethread defines])

AC_SUBST(ETHR_X_LIBS)
AC_SUBST(ETHR_LIBS)
AC_SUBST(ETHR_LIB_NAME)
AC_SUBST(ETHR_DEFS)
AC_SUBST(ETHR_THR_LIB_BASE)
AC_SUBST(ETHR_THR_LIB_BASE_DIR)
AC_SUBST(ETHR_X86_SSE2_ASM)

])


dnl ----------------------------------------------------------------------
dnl
dnl ERL_TIME_CORRECTION
dnl
dnl Check for primitives that can be used for implementing
dnl erts_os_monotonic_time() and erts_os_system_time()
dnl

AC_DEFUN(ERL_TIME_CORRECTION,
[

AC_ARG_WITH(clock-resolution,
AS_HELP_STRING([--with-clock-resolution=high|low|default],
               [specify wanted clock resolution]))

AC_ARG_WITH(clock-gettime-realtime-id,
AS_HELP_STRING([--with-clock-gettime-realtime-id=CLOCKID],
               [specify clock id to use with clock_gettime() for realtime time)]))

AC_ARG_WITH(clock-gettime-monotonic-id,
AS_HELP_STRING([--with-clock-gettime-monotonic-id=CLOCKID],
               [specify clock id to use with clock_gettime() for monotonic time)]))

AC_ARG_ENABLE(prefer-elapsed-monotonic-time-during-suspend,
AS_HELP_STRING([--enable-prefer-elapsed-monotonic-time-during-suspend],
               [Prefer an OS monotonic time source with elapsed time during suspend])
AS_HELP_STRING([--disable-prefer-elapsed-monotonic-time-during-suspend],
               [Do not prefer an OS monotonic time source with elapsed time during suspend]),
[ case "$enableval" in
    yes) prefer_elapsed_monotonic_time_during_suspend=yes ;;
    *)  prefer_elapsed_monotonic_time_during_suspend=no ;;
  esac ], prefer_elapsed_monotonic_time_during_suspend=no)

AC_ARG_ENABLE(gettimeofday-as-os-system-time,
	      AS_HELP_STRING([--enable-gettimeofday-as-os-system-time],
                             [Force usage of gettimeofday() for OS system time]),
[ case "$enableval" in
    yes) force_gettimeofday_os_system_time=yes ;;
    *)  force_gettimeofday_os_system_time=no ;;
  esac ], force_gettimeofday_os_system_time=no)

case "$with_clock_resolution" in
   ""|no|yes)
     with_clock_resolution=default;;
   high|low|default)
     ;;
   *)
     AC_MSG_ERROR([Invalid wanted clock resolution: $with_clock_resolution])
     ;;
esac

AS_IF(
 [test "$force_gettimeofday_os_system_time" = "yes"],
 [

  AC_CHECK_FUNCS([gettimeofday])
  if test "$ac_cv_func_gettimeofday" = "yes"; then
    AC_DEFINE(OS_SYSTEM_TIME_GETTIMEOFDAY,  [1], [Define if you want to implement erts_os_system_time() using gettimeofday()])
  else
    AC_MSG_ERROR([No gettimeofday() available])
  fi
 ],
 [
  # $force_gettimeofday_os_system_time != yes

case "$with_clock_gettime_realtime_id" in
   ""|no)
     with_clock_gettime_realtime_id=no
     ;;
   CLOCK_*CPUTIME*)
     AC_MSG_ERROR([Invalid clock_gettime() realtime clock id: Refusing to use the cputime clock id $with_clock_gettime_realtime_id as realtime clock id])
     ;;
   CLOCK_MONOTONIC*|CLOCK_BOOTTIME*|CLOCK_UPTIME*|CLOCK_HIGHRES*)
     AC_MSG_ERROR([Invalid clock_gettime() realtime clock id: Refusing to use the monotonic clock id $with_clock_gettime_realtime_id as realtime clock id])
     ;;
   CLOCK_*)
     ;;
   *)
     AC_MSG_ERROR([Invalid clock_gettime() clock id: $with_clock_gettime_realtime_id])
     ;;
esac

AS_CASE(["$with_clock_resolution-$with_clock_gettime_realtime_id"],
        [high-no],
        [
            ERL_WALL_CLOCK([high_resolution])
        ],
        [low-no],
        [
            ERL_WALL_CLOCK([low_resolution])
        ],
        [default-no],
        [
            ERL_WALL_CLOCK([default_resolution])
        ],
        [
            ERL_WALL_CLOCK([custom_resolution], [$with_clock_gettime_realtime_id])
        ])

case "$erl_wall_clock_func-$erl_wall_clock_id-$with_clock_gettime_realtime_id" in
  *-*-no)
    ;;
  clock_gettime-$with_clock_gettime_realtime_id-$with_clock_gettime_realtime_id)
    ;;
  *)
    AC_MSG_ERROR([$with_clock_gettime_realtime_id as clock id to clock_gettime() doesn't compile])
    ;;
esac

case $erl_wall_clock_func in
  none)
    AC_MSG_ERROR([No wall clock source found])
    ;;
  mach_clock_get_time)
    AC_DEFINE(OS_SYSTEM_TIME_USING_MACH_CLOCK_GET_TIME, [1], [Define if you want to implement erts_os_system_time() using mach clock_get_time()])
    ;;
  clock_gettime)
    AC_DEFINE(OS_SYSTEM_TIME_USING_CLOCK_GETTIME, [1], [Define if you want to implement erts_os_system_time() using clock_gettime()])
    ;;
  gettimeofday)
    AC_DEFINE(OS_SYSTEM_TIME_GETTIMEOFDAY,  [1], [Define if you want to implement erts_os_system_time() using gettimeofday()])
    ;;
  *)
    ;;
esac

if test "x$erl_wall_clock_id" != "x"; then
    AC_DEFINE_UNQUOTED(WALL_CLOCK_ID_STR, ["$erl_wall_clock_id"], [Define as a string of wall clock id to use])
    AC_DEFINE_UNQUOTED(WALL_CLOCK_ID, [$erl_wall_clock_id], [Define to wall clock id to use])
fi

 ]) # $force_gettimeofday_os_system_time != yes

case "$with_clock_gettime_monotonic_id" in
   ""|no)
     with_clock_gettime_monotonic_id=no
     ;;
   CLOCK_*CPUTIME*)
     AC_MSG_ERROR([Invalid clock_gettime() monotonic clock id: Refusing to use the cputime clock id $with_clock_gettime_monotonic_id as monotonic clock id])
     ;;
   CLOCK_REALTIME*|CLOCK_TAI*)
     AC_MSG_ERROR([Invalid clock_gettime() monotonic clock id: Refusing to use the realtime clock id $with_clock_gettime_monotonic_id as monotonic clock id])
     ;;
   CLOCK_MONOTONIC)
     case $host_os in
         darwin*)
           # CLOCK_MONOTONIC is buggy on MacOS (darwin), or at least on Big Sur
           # and Monterey, since it may step backwards.
           AC_MSG_ERROR([Invalid clock_gettime() monotonic clock id: Refusing to use $with_clock_gettime_monotonic_id as monotonic clock id since it is buggy on MacOS]);;
         *)
           ;;
     esac;;
   CLOCK_*)
     ;;
   *)
     AC_MSG_ERROR([Invalid clock_gettime() clock id: $with_clock_gettime_monotonic_id])
     ;;
esac

AS_CASE(["$with_clock_resolution-$with_clock_gettime_monotonic_id"],
        [high-no],
        [
            ERL_MONOTONIC_CLOCK([high_resolution], [undefined], [$prefer_elapsed_monotonic_time_during_suspend])
        ],
        [low-no],
        [
            ERL_MONOTONIC_CLOCK([low_resolution], [undefined], [$prefer_elapsed_monotonic_time_during_suspend])
        ],
        [default-no],
        [
            ERL_MONOTONIC_CLOCK([default_resolution], [undefined], [$prefer_elapsed_monotonic_time_during_suspend])
        ],
        [
            ERL_MONOTONIC_CLOCK([custom_resolution], [$with_clock_gettime_monotonic_id], [$prefer_elapsed_monotonic_time_during_suspend])
        ])

case "$erl_monotonic_clock_func-$erl_monotonic_clock_id-$with_clock_gettime_monotonic_id" in
  *-*-no)
    ;;
  clock_gettime-$with_clock_gettime_monotonic_id-$with_clock_gettime_monotonic_id)
    ;;
  *)
    AC_MSG_ERROR([$with_clock_gettime_monotonic_id as clock id to clock_gettime() doesn't compile])
    ;;
esac

case $erl_monotonic_clock_func in
  times)
    AC_DEFINE(OS_MONOTONIC_TIME_USING_TIMES, [1], [Define if you want to implement erts_os_monotonic_time() using times()])
    ;;
  mach_clock_get_time)
    AC_DEFINE(OS_MONOTONIC_TIME_USING_MACH_CLOCK_GET_TIME, [1], [Define if you want to implement erts_os_monotonic_time() using mach clock_get_time()])
    ;;
  clock_gettime)
    AC_DEFINE(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME, [1], [Define if you want to implement erts_os_monotonic_time() using clock_gettime()])
    ;;
  gethrtime)
    AC_DEFINE(OS_MONOTONIC_TIME_USING_GETHRTIME,  [1], [Define if you want to implement erts_os_monotonic_time() using gethrtime()])
    ;;
  *)
    ;;
esac

if test $erl_corrected_monotonic_clock = yes; then
  AC_DEFINE(ERTS_HAVE_CORRECTED_OS_MONOTONIC_TIME, [1], [Define if OS monotonic clock is corrected])
fi

if test $erl_monotonic_clock_low_resolution = yes; then
  AC_DEFINE(ERTS_HAVE_LOW_RESOLUTION_OS_MONOTONIC_LOW, [1], [Define if you have a low resolution OS monotonic clock])
fi

xrtlib=
if test "$erl_monotonic_clock_lib" != ""; then
   xrtlib="$erl_monotonic_clock_lib"
fi
if test "$erl_wall_clock_lib" != ""; then
   xrtlib="$erl_wall_clock_lib"
fi
if test "x$erl_monotonic_clock_id" != "x"; then
    AC_DEFINE_UNQUOTED(MONOTONIC_CLOCK_ID_STR, ["$erl_monotonic_clock_id"], [Define as a string of monotonic clock id to use])
    AC_DEFINE_UNQUOTED(MONOTONIC_CLOCK_ID, [$erl_monotonic_clock_id], [Define to monotonic clock id to use])
fi

if test $erl_cv_clock_gettime_monotonic_raw = yes; then
  AC_DEFINE(HAVE_CLOCK_GETTIME_MONOTONIC_RAW, [1], [Define if you have clock_gettime(CLOCK_MONOTONIC_RAW, _)])
fi

ERL_MONOTONIC_CLOCK([high_resolution], [undefined], [no])

case $erl_monotonic_clock_low_resolution-$erl_monotonic_clock_func in
  no-mach_clock_get_time)
    monotonic_hrtime=yes    
    AC_DEFINE(SYS_HRTIME_USING_MACH_CLOCK_GET_TIME, [1], [Define if you want to implement erts_os_hrtime() using mach clock_get_time()])
    ;;
  no-clock_gettime)
    monotonic_hrtime=yes
    AC_DEFINE(SYS_HRTIME_USING_CLOCK_GETTIME, [1], [Define if you want to implement erts_os_hrtime() using clock_gettime()])
    ;;
  no-gethrtime)
    monotonic_hrtime=yes
    AC_DEFINE(SYS_HRTIME_USING_GETHRTIME,  [1], [Define if you want to implement erts_os_hrtime() using gethrtime()])
    ;;
  *)
    monotonic_hrtime=no
    ;;
esac

if test $monotonic_hrtime = yes; then
    AC_DEFINE(HAVE_MONOTONIC_ERTS_SYS_HRTIME, [1], [Define if you have a monotonic erts_os_hrtime() implementation])
fi

if test "x$erl_monotonic_clock_id" != "x"; then
    AC_DEFINE_UNQUOTED(HRTIME_CLOCK_ID_STR, ["$erl_monotonic_clock_id"], [Define as a string of monotonic clock id to use])
    AC_DEFINE_UNQUOTED(HRTIME_CLOCK_ID, [$erl_monotonic_clock_id], [Define to monotonic clock id to use])
fi


dnl
dnl Check if gethrvtime is working, and if to use procfs ioctl
dnl or (yet to be written) write to the procfs ctl file.
dnl

AC_MSG_CHECKING([if gethrvtime works and how to use it])
AC_RUN_IFELSE([AC_LANG_SOURCE([[
/* gethrvtime procfs ioctl test */
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/signal.h>
#include <sys/fault.h>
#include <sys/syscall.h>
#include <sys/procfs.h>
#include <fcntl.h>

int main() {
    long msacct = PR_MSACCT;
    int fd;
    long long start, stop;
    int i;
    pid_t pid = getpid();
    char proc_self[30] = "/proc/";

    sprintf(proc_self+strlen(proc_self), "%lu", (unsigned long) pid);
    if ( (fd = open(proc_self, O_WRONLY)) == -1)
	exit(1);
    if (ioctl(fd, PIOCSET, &msacct) < 0)
	exit(2);
    if (close(fd) < 0)
	exit(3);
    start = gethrvtime();
    for (i = 0; i < 100; i++)
	stop = gethrvtime();
    if (start == 0)
	exit(4);
    if (start == stop)
	exit(5);
    exit(0); return 0;
}
]])],[erl_gethrvtime=procfs_ioctl],[erl_gethrvtime=false],[
case X$erl_xcomp_gethrvtime_procfs_ioctl in
    X)
	erl_gethrvtime=cross;;
    Xyes|Xno)
	if test $erl_xcomp_gethrvtime_procfs_ioctl = yes; then
	    erl_gethrvtime=procfs_ioctl
	else
	    erl_gethrvtime=false
	fi;;
    *)
	AC_MSG_ERROR([Bad erl_xcomp_gethrvtime_procfs_ioctl value: $erl_xcomp_gethrvtime_procfs_ioctl]);;
esac
])

LIBRT=$xrtlib
AS_IF([test "$erl_gethrvtime" = "procfs_ioctl"],
      [
	AC_DEFINE(HAVE_GETHRVTIME_PROCFS_IOCTL,[1],
		[define if gethrvtime() works and uses ioctl() to /proc/self])
	AC_MSG_RESULT(uses ioctl to procfs)
      ],
      [
	if test $erl_gethrvtime = cross; then
	    erl_gethrvtime=false
	    AC_MSG_RESULT(cross)
	    AC_MSG_WARN([result 'not working' guessed because of cross compilation])
	else
	    AC_MSG_RESULT(not working)
	fi

	dnl
	dnl Check if clock_gettime (linux) is working
	dnl

	AC_MSG_CHECKING([if clock_gettime can be used to get thread CPU time])
	save_libs=$LIBS
	LIBS="-lrt"
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
	#include <stdlib.h>
	#include <unistd.h>
	#include <string.h>
	#include <stdio.h>
	#include <time.h>
	int main() {
	    long long start, stop;
	    int i;
	    struct timespec tp;

	    if (clock_gettime(CLOCK_THREAD_CPUTIME_ID, &tp) < 0)
	      exit(1);
	    start = ((long long)tp.tv_sec * 1000000000LL) + (long long)tp.tv_nsec;
	    for (i = 0; i < 100; i++)
	      clock_gettime(CLOCK_THREAD_CPUTIME_ID, &tp);
	    stop = ((long long)tp.tv_sec * 1000000000LL) + (long long)tp.tv_nsec;
	    if (start == 0)
	      exit(4);
	    if (start == stop)
	      exit(5);
	    exit(0); return 0;
	  }
	]])],[erl_clock_gettime_cpu_time=yes],[erl_clock_gettime_cpu_time=no],[
	case X$erl_xcomp_clock_gettime_cpu_time in
	    X) erl_clock_gettime_cpu_time=cross;;
	    Xyes|Xno) erl_clock_gettime_cpu_time=$erl_xcomp_clock_gettime_cpu_time;;
	    *) AC_MSG_ERROR([Bad erl_xcomp_clock_gettime_cpu_time value: $erl_xcomp_clock_gettime_cpu_time]);;
	esac
	])
	LIBS=$save_libs
	AC_MSG_RESULT($erl_clock_gettime_cpu_time)
	case $erl_clock_gettime_cpu_time in
		yes)
			AC_DEFINE(HAVE_CLOCK_GETTIME_CPU_TIME,[],
				  [define if clock_gettime() works for getting thread time])
			LIBRT=-lrt
			;;
		cross)
			erl_clock_gettime_cpu_time=no
			AC_MSG_WARN([result no guessed because of cross compilation])
			;;
		*)
			;;
	esac
      ])
AC_SUBST(LIBRT)
])dnl

dnl ----------------------------------------------------------------------
dnl
dnl LM_TRY_ENABLE_CFLAG
dnl
dnl
dnl Tries a CFLAG and sees if it can be enabled without compiler errors
dnl $1: textual cflag to add
dnl $2: variable to store the modified CFLAG in
dnl $3: prologue
dnl Usage example LM_TRY_ENABLE_CFLAG([-Werror=return-type], [CFLAGS])
dnl
dnl
AC_DEFUN([LM_TRY_ENABLE_CFLAG], [
    AC_MSG_CHECKING([if we can add $1 to $2 (via CFLAGS)])
    saved_CFLAGS=$CFLAGS;
    CFLAGS="-Werror $1 $$2";
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([$3], [[return 0;]])],
        [can_enable_flag=true],[can_enable_flag=false])
    CFLAGS=$saved_CFLAGS;
    AS_IF(
      [test "X$can_enable_flag" = "Xtrue"],
      [
        AS_VAR_SET($2, "$1 $$2")
        AC_MSG_RESULT([yes])
      ],
      [
        AC_MSG_RESULT([no])
      ])
])

AC_DEFUN([LM_CHECK_ENABLE_CFLAG], [
    AC_MSG_CHECKING([whether $CC accepts $1...])
    saved_CFLAGS=$CFLAGS;
    CFLAGS="-Werror $1 $CFLAGS";
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[return 0;]])],[can_enable_flag=true],[can_enable_flag=false])
    CFLAGS=$saved_CFLAGS;
    AS_IF(
      [test "X$can_enable_flag" = "Xtrue"],
      [
        AS_VAR_SET($2, true)
        AC_MSG_RESULT([yes])
      ],
      [
        AS_VAR_SET($2, false)
        AC_MSG_RESULT([no])
      ])
])

dnl
dnl LM_CHECK_RUN_CFLAG
dnl
dnl As LM_CHECK_ENABLE_CFLAG but also runs the command. Required for testing
dnl profile-guided optimization, for which the "use" step may require that the
dnl binary created in the "generate" step runs.
dnl
AC_DEFUN([LM_CHECK_RUN_CFLAG], [
    AC_MSG_CHECKING([whether $CC accepts $1...])
    saved_CFLAGS=$CFLAGS;
    CFLAGS="-Werror $1 $CFLAGS";
    AC_RUN_IFELSE([AC_LANG_SOURCE([[]])],[return 0;],[can_enable_flag=true],[can_enable_flag=false])
    CFLAGS=$saved_CFLAGS;
    AS_IF(
      [test "X$can_enable_flag" = "Xtrue"],
      [
        AS_VAR_SET($2, true)
        AC_MSG_RESULT([yes])
      ],
      [
        AS_VAR_SET($2, false)
        AC_MSG_RESULT([no])
      ])
])

dnl ----------------------------------------------------------------------
dnl
dnl LM_TRY_ENABLE_LDFLAG
dnl
dnl
dnl Tries a LDFLAG and sees if it can be enabled without compiler errors
dnl $1: textual ldflag to add
dnl $2: variable to store the modified LDFLAG in
dnl Usage example LM_TRY_ENABLE_LDFLAG([-Wl,-z,noexecstack], [LDFLAGS])
dnl
AC_DEFUN([LM_TRY_ENABLE_LDFLAG], [
    AC_MSG_CHECKING([if we can add $1 to $2 (via LDFLAGS)])
    saved_LDFLAGS=$LDFLAGS;
    saved_LDFLAG="$1";
    LDFLAGS="$saved_LDFLAG $$2";
    AC_LINK_IFELSE(
        [AC_LANG_PROGRAM([[]], [[return 0;]])],
        [can_enable_flag=true],
        [can_enable_flag=false]
    )
    LDFLAGS=$saved_LDFLAGS;
    AS_IF(
      [test "X$can_enable_flag" = "Xtrue"],
      [
        AS_VAR_SET($2, "$saved_LDFLAG $$2")
        AC_MSG_RESULT([yes])
      ],
      [
        AC_MSG_RESULT([no])
      ])
])

dnl ERL_TRY_LINK_JAVA(CLASSES, FUNCTION-BODY
dnl                   [ACTION_IF_FOUND [, ACTION-IF-NOT-FOUND]])
dnl Freely inspired by AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[]])],[],[]). (Maybe better to create a 
dnl AC_LANG_JAVA instead...)
AC_DEFUN(ERL_TRY_LINK_JAVA,
[java_link='$JAVAC conftest.java 1>&AS_MESSAGE_LOG_FD'
changequote(, )dnl
cat > conftest.java <<EOF
$1
class conftest { public static void main(String[] args) {
   $2
   ; return; }}
EOF
changequote([, ])dnl
AS_IF(
  [AC_TRY_EVAL(java_link) && test -s conftest.class],
  [
   ifelse([$3], , :, [rm -rf conftest*
   $3])

  ],
  [
   echo "configure: failed program was:" 1>&AS_MESSAGE_LOG_FD
   cat conftest.java 1>&AS_MESSAGE_LOG_FD
   echo "configure: PATH was $PATH" 1>&AS_MESSAGE_LOG_FD
ifelse([$4], , , [  rm -rf conftest*
  $4
])dnl

  ])
rm -f conftest*])
dnl #define UNSAFE_MASK  0xc0000000 /* Mask for bits that must be constant */

dnl ----------------------------------------------------------------------
dnl
dnl LM_HARDWARE_ARCH
dnl
dnl Determine target hardware in ARCH
dnl
AC_DEFUN([LM_HARDWARE_ARCH], [
    AC_MSG_CHECKING([target hardware architecture])
    if test "x$host_alias" != "x" -a "x$host_cpu" != "x"; then
        chk_arch_=$host_cpu
    else
        chk_arch_=`uname -m`
    fi

    case $chk_arch_ in
    sun4u)	ARCH=ultrasparc;;
    sparc64)	ARCH=sparc64;;
    sun4v)	ARCH=ultrasparc;;
    i86pc)	ARCH=x86;;
    i386)	ARCH=x86;;
    i486)	ARCH=x86;;
    i586)	ARCH=x86;;
    i686)	ARCH=x86;;
    x86_64)	ARCH=amd64;;
    amd64)	ARCH=amd64;;
    macppc)	ARCH=ppc;;
    powerpc)	ARCH=ppc;;
    ppc)	ARCH=ppc;;
    ppc64)	ARCH=ppc64;;
    ppc64le)	ARCH=ppc64le;;
    powerpc64)	ARCH=ppc64;;
    powerpc64le) ARCH=ppc64le;;
    "Power Macintosh")	ARCH=ppc;;
    arm64)	ARCH=arm64;;
    armv5b)	ARCH=arm;;
    armv5teb)	ARCH=arm;;
    armv5tel)	ARCH=arm;;
    armv5tejl)	ARCH=arm;;
    armv6l)	ARCH=arm;;
    armv6hl)	ARCH=arm;;
    armv7l)	ARCH=arm;;
    armv7hl)	ARCH=arm;;
    armv8*)	ARCH=arm;;
    aarch64)	ARCH=arm64;;
    aarch*)	ARCH=arm;;
    tile)	ARCH=tile;;
    e2k)        ARCH=e2k;;
    *)	 	ARCH=noarch;;
    esac
    AC_MSG_RESULT($ARCH)

    dnl
    dnl Convert between x86 and amd64 based on the compiler's mode.
    dnl Ditto between ultrasparc and sparc64.
    dnl
    AC_MSG_CHECKING(whether compilation mode forces ARCH adjustment)
    case "$ARCH-$ac_cv_sizeof_void_p" in
    x86-8)
	AC_MSG_RESULT(yes: adjusting ARCH=x86 to ARCH=amd64)
	ARCH=amd64
	;;
    amd64-4)
	AC_MSG_RESULT(yes: adjusting ARCH=amd64 to ARCH=x86)
	ARCH=x86
	;;
    ultrasparc-8)
	AC_MSG_RESULT(yes: adjusting ARCH=ultrasparc to ARCH=sparc64)
	ARCH=sparc64
	;;
    sparc64-4)
	AC_MSG_RESULT(yes: adjusting ARCH=sparc64 to ARCH=ultrasparc)
	ARCH=ultrasparc
	;;
    ppc64-4)
	AC_MSG_RESULT(yes: adjusting ARCH=ppc64 to ARCH=ppc)
	ARCH=ppc
	;;
    ppc-8)
	AC_MSG_RESULT(yes: adjusting ARCH=ppc to ARCH=ppc64)
	ARCH=ppc64
	;;
    arm-8)
	AC_MSG_RESULT(yes: adjusting ARCH=arm to ARCH=arm64)
	ARCH=arm64
	;;
    *)
	AC_MSG_RESULT(no: ARCH is $ARCH)
	;;
    esac

    AC_SUBST(ARCH)
])

dnl
dnl--------------------------------------------------------------------
dnl Open Source Security Foundation FLAGS
dnl
dnl Enable the flags recomended by the OSSF that we think are good for
dnl Erlang/OTP. See https://github.com/ossf/wg-best-practices-os-developers/blob/main/docs/Compiler-Hardening-Guides/Compiler-Options-Hardening-Guide-for-C-and-C++.md
dnl for details.
dnl
dnl--------------------------------------------------------------------

AC_DEFUN([ERL_OSSF],
[
  AS_IF([test "X$host" = "Xwin32"],
    [ ossf_security_hardening_default=no],
    [ ossf_security_hardening_default=yes])
  AC_ARG_ENABLE(security-hardening-flags,
    AS_HELP_STRING([--disable-security-hardening-flags],
      [disable Open Source Security Foundation security hardening flags]),
    [case "$enableval" in
       no) ossf_security_hardening=no ;;
       *)  ossf_security_hardening=yes ;;
     esac], ossf_security_hardening=$ossf_security_hardening_default)
])

AC_DEFUN([ERL_OSSF_CFLAGS],
[
    AS_IF([test "$ossf_security_hardening" = "yes"],
      [
        LM_TRY_ENABLE_CFLAG([-U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=3], [$1], [#include <string.h>])
        AS_IF([test "X$can_enable_flag" = "Xfalse"],
          [
            LM_TRY_ENABLE_CFLAG([-U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=2], [$1], [#include <string.h>])
          ])
        dnl Flags that enable stack protection mechanism
        LM_TRY_ENABLE_CFLAG([-fstack-clash-protection], [$1])
        LM_TRY_ENABLE_CFLAG([-fstack-protector-strong], [$1])
        AS_IF([test "X$can_enable_flag" = "Xfalse"],
          [
            LM_TRY_ENABLE_CFLAG([-fstack-protector], [$1], [])
          ],
          [
            # Some systems (solaris) also require this to be part of LDFLAGS
            LM_TRY_ENABLE_LDFLAG([-fstack-protector-strong], [$2])
          ]
        )
        LM_TRY_ENABLE_CFLAG([-fcf-protection=full], [$1])
        LM_TRY_ENABLE_CFLAG([-mbranch-protection=standard], [$1])
        LM_TRY_ENABLE_CFLAG([-fexceptions],[$1])

        dnl Flags that protect against various UB things
        LM_TRY_ENABLE_CFLAG([-fno-strict-overflow],[$1])
        LM_TRY_ENABLE_CFLAG([-fno-delete-null-pointer-checks],[$1])
        LM_TRY_ENABLE_CFLAG([-fno-strict-aliasing],[$1])
        LM_TRY_ENABLE_CFLAG([-ftrivial-auto-var-init=zero],[$1])
        LM_TRY_ENABLE_CFLAG([-fstrict-flex-arrays=3],[$1])
        AS_IF([test "X$can_enable_flag" = "Xfalse"],
          [
            LM_TRY_ENABLE_CFLAG([-fstrict-flex-arrays=2], [$1])
          ], [AS_IF([test "X$can_enable_flag" != "Xtrue"], [exit 1])]
        )
      ])
])

AC_DEFUN([ERL_OSSF_CXXFLAGS],
[
    AS_IF([test "$ossf_security_hardening" = "yes"],
      [
        LM_TRY_ENABLE_CFLAG([-D_GLIBCXX_ASSERTIONS],[$1])
      ])
])

AC_DEFUN([ERL_OSSF_LDFLAGS],
[
    AS_IF([test "$ossf_security_hardening" = "yes"],
      [
        dnl Stack protection
        LM_TRY_ENABLE_LDFLAG([-Wl,-z,noexecstack],[$1])
        dnl Disables lazy loading of dynamic libraries
        dnl and make sthe relocation area read only
        LM_TRY_ENABLE_LDFLAG([-Wl,-z,relro],[$1])
        LM_TRY_ENABLE_LDFLAG([-Wl,-z,now],[$1])
        dnl Only link against dynamic libraries that are used
        LM_TRY_ENABLE_LDFLAG([-Wl,--as-needed],[$1])
        dnl Don't load symbols of transative dynamic libraries
        dnl that is beam.smp should not load libcrypto.so symbols
        dnl when it is loading crypto.so. Only crypto.so should.
        LM_TRY_ENABLE_LDFLAG([-Wl,--no-copy-dt-needed-entries],[$1])
      ])
])

AC_DEFUN([ERL_OSSF_FLAGS],
[
    ERL_OSSF
    ERL_OSSF_CFLAGS([CFLAGS], [LDFLAGS])
    ERL_OSSF_CXXFLAGS([CXXFLAGS])
    ERL_OSSF_LDFLAGS([LDFLAGS])
])

dnl
dnl--------------------------------------------------------------------
dnl Dynamic Erlang Drivers
dnl
dnl Linking to produce dynamic Erlang drivers to be loaded by Erlang's
dnl Dynamic Driver Loader and Linker (DDLL). Below the prefix DED is an
dnl abbreviation for `Dynamic Erlang Driver'.
dnl
dnl For DED we need something quite sloppy, which allows undefined references 
dnl (notably driver functions) in the resulting shared library. 
dnl Example of Makefile rule (and settings of macros):
dnl
dnl LIBS = @LIBS@
dnl LD = @DED_LD@
dnl LDFLAGS = @DED_LDFLAGS@
dnl soname = @ldsoname@
dnl
dnl my_drv.so:   my_drv.o my_utils.o
dnl              $(LD) $(LDFLAGS) $(soname) $@ -o $@ $^ -lc $(LIBS)
dnl
dnl--------------------------------------------------------------------
dnl

AC_DEFUN(ERL_DED,
	[

LM_CHECK_THR_LIB

if test "$THR_DEFS" = ""; then
    DED_THR_DEFS="-D_THREAD_SAFE -D_REENTRANT"
else
    DED_THR_DEFS="$THR_DEFS"
fi

AC_SUBST(DED_THR_DEFS)

ERL_DED_FLAGS

])

AC_DEFUN(ERL_DED_FLAGS,
         [

# Large file support and 8-byte time_t by default
AC_SYS_YEAR2038_RECOMMENDED

USER_LD=$LD
AS_IF([ test "$USER_LD" = '$(CC)' ], [USER_LD='$(DED_CC)'])
USER_LDFLAGS="$LDFLAGS"

DED_CC=$CC
DED_GCC=$GCC

DED_CFLAGS=
DED_OSTYPE=unix

case $host_os in
     linux*)
	DED_CFLAGS="-D_GNU_SOURCE" ;;
     win32)
	DED_CFLAGS="-D_WIN32_WINNT=0x0600 -DWINVER=0x0600"
        DED_OSTYPE=win32 ;;
     *)
        ;;
esac

DED_WARN_FLAGS="-Wall -Wstrict-prototypes"
case "$host_cpu" in
  tile*)
    # tile-gcc is a bit stricter with -Wmissing-prototypes than other gccs,
    # and too strict for our taste.
    ;;
  *)
    DED_WARN_FLAGS="$DED_WARN_FLAGS -Wmissing-prototypes";;
esac

LM_TRY_ENABLE_CFLAG([-Wdeclaration-after-statement], [DED_WARN_FLAGS])

LM_TRY_ENABLE_CFLAG([-Werror=return-type], [DED_WERRORFLAGS])
LM_TRY_ENABLE_CFLAG([-Werror=implicit], [DED_WERRORFLAGS])
LM_TRY_ENABLE_CFLAG([-Werror=undef], [DED_WERRORFLAGS])

DED_SYS_INCLUDE="-I${ERL_TOP}/erts/emulator/beam -I${ERL_TOP}/erts/include -I${ERL_TOP}/erts/include/$host -I${ERL_TOP}/erts/include/internal -I${ERL_TOP}/erts/include/internal/$host -I${ERL_TOP}/erts/emulator/sys/$DED_OSTYPE -I${ERL_TOP}/erts/emulator/sys/common"
DED_INCLUDE=$DED_SYS_INCLUDE

DED_CFLAGS="$CFLAGS $CPPFLAGS $DED_CFLAGS"
AS_IF(
  [test "x$GCC" = xyes],
  [
    # Use -fno-common for gcc, that is link error if multiple definitions of
    # global variables are encountered. This is ISO C compliant.
    # Until version 10, gcc has had -fcommon as default, which allows and merges
    # such dubious duplicates.
    LM_TRY_ENABLE_CFLAG([-fno-common], [DED_CFLAGS])

    DED_STATIC_CFLAGS="$DED_CFLAGS"
    DED_CFLAGS="$DED_CFLAGS -fPIC"
    # Remove -fPIE and -fno-PIE
    DED_CFLAGS=`echo $DED_CFLAGS | sed 's/-f\(no-\)\?PIE//g'`
  ])

DED_EXT=so
case $host_os in
    win32) DED_EXT=dll;;
    darwin*)
	DED_CFLAGS="$DED_CFLAGS -fno-common"
	DED_STATIC_CFLAGS="$DED_STATIC_CFLAGS -fno-common";;
    *)
	;;
esac

DED_STATIC_CFLAGS="$DED_STATIC_CFLAGS -DSTATIC_ERLANG_NIF -DSTATIC_ERLANG_DRIVER"

if test "$CFLAG_RUNTIME_LIBRARY_PATH" = ""; then

  CFLAG_RUNTIME_LIBRARY_PATH="-Wl,-R"
  case $host_os in
    darwin*)
	CFLAG_RUNTIME_LIBRARY_PATH=
	;;
    win32)
	CFLAG_RUNTIME_LIBRARY_PATH=
	;;
    osf*)
	CFLAG_RUNTIME_LIBRARY_PATH="-Wl,-rpath,"
	;;
    *)
	;;
  esac

fi

# If DED_LD is set in environment, we expect all DED_LD* variables
# to be specified (cross compiling)
if test "x$DED_LD" = "x"; then

  DED_LDFLAGS_CONFTEST=

  DED_LD_FLAG_RUNTIME_LIBRARY_PATH="-R"
  case $host_os in
	win32)
		DED_LDFLAGS="-dll"
		DED_LD_FLAG_RUNTIME_LIBRARY_PATH=
	;;
	solaris2*|sysv4*)
		DED_LDFLAGS="-G"
		if test X${enable_m64_build} = Xyes; then
			DED_LDFLAGS="-64 $DED_LDFLAGS"
		fi
	;;
	aix*|os400*)
		DED_LDFLAGS="-G -bnoentry -bexpall"
	;;
	freebsd2*)
		# Non-ELF GNU linker
		DED_LDFLAGS="-Bshareable"
	;;
	darwin*)
		# Mach-O linker: a shared lib and a loadable object file is not the same thing.

                if test "X${ERL_DED_FLAT_BUNDLE}" = "Xtrue"; then
                  # EI sets this variable when building its .so file as beam.smp
                  # has not been built yet and any ei lib will not
                  # link to beam.smp anyways
		  DED_LDFLAGS="-bundle -flat_namespace -undefined suppress"
                else
                  # Cannot use flat namespaces for drivers/nifs as that may cause
                  # symbols to collide during loading
		  DED_LDFLAGS="-bundle -bundle_loader ${ERL_TOP}/bin/$host/beam.smp"
                fi
		# DED_LDFLAGS_CONFTEST is for use in configure tests only. We
		# cannot use DED_LDFLAGS in configure tests since beam.smp has not
		# been built yet...
		DED_LDFLAGS_CONFTEST="-bundle"
		if test X${enable_m64_build} = Xyes; then
		  DED_LDFLAGS="-m64 $DED_LDFLAGS"
		else
		  if test X${enable_m32_build} = Xyes; then
		    DED_LDFLAGS="-m32 $DED_LDFLAGS"
		  else
		    AC_CHECK_SIZEOF(void *)
		    case "$ac_cv_sizeof_void_p" in
		      8)
			DED_LDFLAGS="-m64 $DED_LDFLAGS";;
		      *)
		        ;;
		    esac
		  fi
		fi
		DED_LD_FLAG_RUNTIME_LIBRARY_PATH="$CFLAG_RUNTIME_LIBRARY_PATH"
	;;
	linux*)
		DED_LD_FLAG_RUNTIME_LIBRARY_PATH="$CFLAG_RUNTIME_LIBRARY_PATH"
		DED_LDFLAGS="-shared -Wl,-Bsymbolic"
		if test X${enable_m64_build} = Xyes; then
			DED_LDFLAGS="-m64 $DED_LDFLAGS"
		fi;
		if test X${enable_m32_build} = Xyes; then
			DED_LDFLAGS="-m32 $DED_LDFLAGS"
		fi
	;;	
	freebsd*)
		DED_LD_FLAG_RUNTIME_LIBRARY_PATH="$CFLAG_RUNTIME_LIBRARY_PATH"
		DED_LDFLAGS="-shared"
		if test X${enable_m64_build} = Xyes; then
			DED_LDFLAGS="-m64 $DED_LDFLAGS"
		fi;
		if test X${enable_m32_build} = Xyes; then
			DED_LDFLAGS="-m32 $DED_LDFLAGS"
		fi
	;;	
	openbsd*)
		DED_LD_FLAG_RUNTIME_LIBRARY_PATH="$CFLAG_RUNTIME_LIBRARY_PATH"
		DED_LDFLAGS="-shared"
	;;
	osf*)
		# NOTE! Whitespace after -rpath is important.
		DED_LD_FLAG_RUNTIME_LIBRARY_PATH="-rpath "
		DED_LDFLAGS="-shared -expect_unresolved '*'"
	;;
	*)
		# assume GNU linker and ELF
		DED_LDFLAGS="-shared"
		# GNU linker has no option for 64bit build, should not propagate -m64
	;;
  esac

  if test "x$DED_LD" = "x" && test "$USER_LD" != ""; then
        LM_LOG("setting DED_LD to $USER_LD")
        DED_LD="$USER_LD"
        DED_LDFLAGS="$USER_LDFLAGS $DED_LDFLAGS"
  fi
  DED_LIBS=$LIBS

fi # "x$DED_LD" = "x"

test "$DED_LDFLAGS_CONFTEST" != "" || DED_LDFLAGS_CONFTEST="$DED_LDFLAGS"

AC_CHECK_TOOL(DED_LD, ld, false)
test "$DED_LD" != "false" || AC_MSG_ERROR([No linker found])

ERL_OSSF
ERL_OSSF_CFLAGS([DED_CFLAGS], [DED_LDFLAGS])
ERL_OSSF_LDFLAGS([DED_LDFLAGS])

AC_MSG_CHECKING(for static compiler flags)
DED_STATIC_CFLAGS="$DED_WERRORFLAGS $DED_WARN_FLAGS $DED_THR_DEFS $DED_STATIC_CFLAGS"
AC_MSG_RESULT([$DED_STATIC_CFLAGS])
AC_MSG_CHECKING(for basic compiler flags for loadable drivers)
DED_BASIC_CFLAGS=$DED_CFLAGS
AC_MSG_RESULT([$DED_CFLAGS])
AC_MSG_CHECKING(for compiler flags for loadable drivers)
DED_CFLAGS="$DED_WERRORFLAGS $DED_WARN_FLAGS $DED_THR_DEFS $DED_CFLAGS"
AC_MSG_RESULT([$DED_CFLAGS])
AC_MSG_CHECKING(for linker for loadable drivers)
AC_MSG_RESULT([$DED_LD])
AC_MSG_CHECKING(for linker flags for loadable drivers)
AC_MSG_RESULT([$DED_LDFLAGS])
AC_MSG_CHECKING(for 'runtime library path' linker flag)
if test "x$DED_LD_FLAG_RUNTIME_LIBRARY_PATH" != "x"; then
	AC_MSG_RESULT([$DED_LD_FLAG_RUNTIME_LIBRARY_PATH])
else
	AC_MSG_RESULT([not found])
fi

AC_SUBST(DED_CC)
AC_SUBST(DED_GCC)
AC_SUBST(DED_EXT)
AC_SUBST(DED_SYS_INCLUDE)
AC_SUBST(DED_INCLUDE)
AC_SUBST(DED_BASIC_CFLAGS)
AC_SUBST(DED_CFLAGS)
AC_SUBST(DED_STATIC_CFLAGS)
AC_SUBST(DED_WARN_FLAGS)
AC_SUBST(DED_WERRORFLAGS)
AC_SUBST(DED_LD)
AC_SUBST(DED_LDFLAGS)
AC_SUBST(DED_LD_FLAG_RUNTIME_LIBRARY_PATH)
AC_SUBST(DED_LIBS)
AC_SUBST(DED_OSTYPE)

])
