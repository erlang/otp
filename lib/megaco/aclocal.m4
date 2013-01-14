dnl
dnl %CopyrightBegin%
dnl
dnl Copyright Ericsson AB 1998-2012. All Rights Reserved.
dnl
dnl The contents of this file are subject to the Erlang Public License,
dnl Version 1.1, (the "License"); you may not use this file except in
dnl compliance with the License. You should have received a copy of the
dnl Erlang Public License along with this software. If not, it can be
dnl retrieved online at http://www.erlang.org/.
dnl
dnl Software distributed under the License is distributed on an "AS IS"
dnl basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
dnl the License for the specific language governing rights and limitations
dnl under the License.
dnl
dnl %CopyrightEnd%
dnl

dnl
dnl aclocal.m4
dnl
dnl Local macros used in configure.in. The Local Macros which
dnl could/should be part of autoconf are prefixed LM_, macros specific
dnl to the Erlang system are prefixed ERL_.
dnl

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
AC_ARG_VAR(LDFLAGS, [linker flags (can be risky to set since LD may be overriden by configure)])
AC_ARG_VAR(LIBS, [libraries])
AC_ARG_VAR(DED_LD, [linker for Dynamic Erlang Drivers (set all DED_LD* variables or none)])
AC_ARG_VAR(DED_LDFLAGS, [linker flags for Dynamic Erlang Drivers (set all DED_LD* variables or none)])
AC_ARG_VAR(DED_LD_FLAG_RUNTIME_LIBRARY_PATH, [runtime library path linker flag for Dynamic Erlang Drivers (set all DED_LD* variables or none)])
AC_ARG_VAR(LFS_CFLAGS, [large file support C compiler flags (set all LFS_* variables or none)])
AC_ARG_VAR(LFS_LDFLAGS, [large file support linker flags (set all LFS_* variables or none)])
AC_ARG_VAR(LFS_LIBS, [large file support libraries (set all LFS_* variables or none)])
AC_ARG_VAR(RANLIB, [ranlib])
AC_ARG_VAR(AR, [ar])
AC_ARG_VAR(GETCONF, [getconf])

dnl Cross system root
AC_ARG_VAR(erl_xcomp_sysroot, [Absolute cross system root path (only used when cross compiling)])
AC_ARG_VAR(erl_xcomp_isysroot, [Absolute cross system root include path (only used when cross compiling)])

dnl Cross compilation variables
AC_ARG_VAR(erl_xcomp_bigendian, [big endian system: yes|no (only used when cross compiling)])
AC_ARG_VAR(erl_xcomp_double_middle_endian, [double-middle-endian system: yes|no (only used when cross compiling)])
AC_ARG_VAR(erl_xcomp_linux_clock_gettime_correction, [clock_gettime() can be used for time correction: yes|no (only used when cross compiling)])
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
if test "$cross_compiling" != "yes"; then
    AC_CHECK_PROG([GETCONF], [getconf], [getconf], [false])
else
    dnl First check if we got a `<HOST>-getconf' in $PATH
    host_getconf="$host_alias-getconf"
    AC_CHECK_PROG([GETCONF], [$host_getconf], [$host_getconf], [false])
    if test "$GETCONF" = "false" && test "$erl_xcomp_sysroot" != ""; then
	dnl We should perhaps give up if we have'nt found it by now, but at
	dnl least in one Tilera MDE `getconf' under sysroot is a bourne
	dnl shell script which we can use. We try to find `<HOST>-getconf'
    	dnl or `getconf' under sysconf, but only under sysconf since
	dnl `getconf' in $PATH is almost guaranteed to be for the build
	dnl machine.
	GETCONF=
	prfx="$erl_xcomp_sysroot"
        AC_PATH_TOOL([GETCONF], [getconf], [false],
	             ["$prfx/usr/bin:$prfx/bin:$prfx/usr/local/bin"])
    fi
fi
])

dnl ----------------------------------------------------------------------
dnl
dnl LM_WINDOWS_ENVIRONMENT
dnl
dnl
dnl Tries to determine thw windows build environment, i.e. 
dnl MIXED_CYGWIN_VC or MIXED_MSYS_VC 
dnl

AC_DEFUN(LM_WINDOWS_ENVIRONMENT,
[
MIXED_CYGWIN=no
MIXED_MSYS=no

AC_MSG_CHECKING(for mixed cygwin or msys and native VC++ environment)
if test "X$host" = "Xwin32" -a "x$GCC" != "xyes"; then
	if test -x /usr/bin/cygpath; then
		CFLAGS="-O2"
		MIXED_CYGWIN=yes
		AC_MSG_RESULT([Cygwin and VC])
		MIXED_CYGWIN_VC=yes
		CPPFLAGS="$CPPFLAGS -DERTS_MIXED_CYGWIN_VC"
	elif test -x /usr/bin/msysinfo; then
	        CFLAGS="-O2"
		MIXED_MSYS=yes
		AC_MSG_RESULT([MSYS and VC])
		MIXED_MSYS_VC=yes
		CPPFLAGS="$CPPFLAGS -DERTS_MIXED_MSYS_VC"
	else		    
		AC_MSG_RESULT([undeterminable])
		AC_MSG_ERROR(Seems to be mixed windows but not with cygwin, cannot handle this!)
	fi
else
	AC_MSG_RESULT([no])
	MIXED_CYGWIN_VC=no
	MIXED_MSYS_VC=no
fi
AC_SUBST(MIXED_CYGWIN_VC)
AC_SUBST(MIXED_MSYS_VC)

MIXED_VC=no
if test "x$MIXED_MSYS_VC" = "xyes" -o  "x$MIXED_CYGWIN_VC" = "xyes" ; then
   MIXED_VC=yes
fi

AC_SUBST(MIXED_VC)

if test "x$MIXED_MSYS" != "xyes"; then
   AC_MSG_CHECKING(for mixed cygwin and native MinGW environment)
   if test "X$host" = "Xwin32" -a "x$GCC" = x"yes"; then
	if test -x /usr/bin/cygpath; then
		CFLAGS="-O2"
		MIXED_CYGWIN=yes
		AC_MSG_RESULT([yes])
		MIXED_CYGWIN_MINGW=yes
		CPPFLAGS="$CPPFLAGS -DERTS_MIXED_CYGWIN_MINGW"
	else
		AC_MSG_RESULT([undeterminable])
		AC_MSG_ERROR(Seems to be mixed windows but not with cygwin, cannot handle this!)
	fi
    else
	AC_MSG_RESULT([no])
	MIXED_CYGWIN_MINGW=no
    fi
else
	MIXED_CYGWIN_MINGW=no
fi	
AC_SUBST(MIXED_CYGWIN_MINGW)

AC_MSG_CHECKING(if we mix cygwin with any native compiler)
if test "X$MIXED_CYGWIN" = "Xyes"; then
	AC_MSG_RESULT([yes])	
else
	AC_MSG_RESULT([no])
fi

AC_SUBST(MIXED_CYGWIN)
	
AC_MSG_CHECKING(if we mix msys with another native compiler)
if test "X$MIXED_MSYS" = "Xyes" ; then
	AC_MSG_RESULT([yes])	
else
	AC_MSG_RESULT([no])
fi

AC_SUBST(MIXED_MSYS)
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
AC_TRY_COMPILE([],[
#if defined(__clang_major__) && __clang_major__ >= 3
    /* clang 3.x or later is fine */
#elif defined(__llvm__)
#error "this version of llvm is unable to correctly compile beam_emu.c"
#endif
    __label__ lbl1;
    __label__ lbl2;
    int x = magic();
    static void *jtab[2];

    jtab[0] = &&lbl1;
    jtab[1] = &&lbl2;
    goto *jtab[x];
lbl1:
    return 1;
lbl2:
    return 2;
],ac_cv_prog_emu_cc=$CC,ac_cv_prog_emu_cc=no)

if test $ac_cv_prog_emu_cc = no; then
	for ac_progname in emu_cc.sh gcc-4.2 gcc; do
  		IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS=":"
  		ac_dummy="$PATH"
  		for ac_dir in $ac_dummy; do
    			test -z "$ac_dir" && ac_dir=.
    			if test -f $ac_dir/$ac_progname; then
      				ac_cv_prog_emu_cc=$ac_dir/$ac_progname
      				break
    			fi
  		done
  		IFS="$ac_save_ifs"
		if test $ac_cv_prog_emu_cc != no; then
			break
		fi
	done
fi

if test $ac_cv_prog_emu_cc != no; then
	save_CC=$CC
	save_CFLAGS=$CFLAGS
	save_CPPFLAGS=$CPPFLAGS
	CC=$ac_cv_prog_emu_cc
	CFLAGS=""
	CPPFLAGS=""
	AC_TRY_COMPILE([],[
#if defined(__clang_major__) && __clang_major__ >= 3
    /* clang 3.x or later is fine */
#elif defined(__llvm__)
#error "this version of llvm is unable to correctly compile beam_emu.c"
#endif
    	__label__ lbl1;
    	__label__ lbl2;
    	int x = magic();
    	static void *jtab[2];

    	jtab[0] = &&lbl1;
    	jtab[1] = &&lbl2;
    	goto *jtab[x];
	lbl1:
    	return 1;
	lbl2:
    	return 2;
	],ac_cv_prog_emu_cc=$CC,ac_cv_prog_emu_cc=no)
	CC=$save_CC
	CFLAGS=$save_CFLAGS
	CPPFLAGS=$save_CPPFLAGS
fi
])
if test $ac_cv_prog_emu_cc = no; then
	AC_DEFINE(NO_JUMP_TABLE,[],[Defined if no found C compiler can handle jump tables])
	EMU_CC=$CC
else
	EMU_CC=$ac_cv_prog_emu_cc
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
dnl On some systems /usr/bin/perl is perl 4 and e.g.
dnl /usr/local/bin/perl is perl 5. We try to handle this case by
dnl putting a couple of 
dnl Tries to handle the case that there are two programs called perl
dnl in the path and one of them is perl 5 and the other isn't. 
dnl
AC_DEFUN(LM_PROG_PERL5,
[AC_PATH_PROGS(PERL, perl5 perl, false,
   /usr/local/bin:/opt/local/bin:/usr/local/gnu/bin:${PATH})
changequote(, )dnl
dnl[ That bracket is needed to balance the right bracket below
if test "$PERL" = "false" || $PERL -e 'exit ($] >= 5)'; then
changequote([, ])dnl
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
AC_TRY_COMPILE([#include <sys/socket.h>], [int i = SO_BSDCOMPAT;],
               ac_cv_decl_so_bsdcompat=yes,
               ac_cv_decl_so_bsdcompat=no))

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
[AC_TRY_COMPILE([#include <sys/types.h>
#include <netinet/in.h>], [int i = INADDR_LOOPBACK;],
ac_cv_decl_inaddr_loopback=yes, ac_cv_decl_inaddr_loopback=no)
])

if test ${ac_cv_decl_inaddr_loopback} = no; then
  AC_CACHE_CHECK([for INADDR_LOOPBACK in rpc/types.h],
                   ac_cv_decl_inaddr_loopback_rpc,
                   AC_TRY_COMPILE([#include <rpc/types.h>],
                                   [int i = INADDR_LOOPBACK;],
                                   ac_cv_decl_inaddr_loopback_rpc=yes,
                                   ac_cv_decl_inaddr_loopback_rpc=no))

   case "${ac_cv_decl_inaddr_loopback_rpc}" in
     "yes" )
        AC_DEFINE(DEF_INADDR_LOOPBACK_IN_RPC_TYPES_H,[],
		[Define if you need to include rpc/types.h to get INADDR_LOOPBACK defined]) ;;
      * )
  	AC_CACHE_CHECK([for INADDR_LOOPBACK in winsock2.h],
                   ac_cv_decl_inaddr_loopback_winsock2,
                   AC_TRY_COMPILE([#define WIN32_LEAN_AND_MEAN
				   #include <winsock2.h>],
                                   [int i = INADDR_LOOPBACK;],
                                   ac_cv_decl_inaddr_loopback_winsock2=yes,
                                   ac_cv_decl_inaddr_loopback_winsock2=no))
	case "${ac_cv_decl_inaddr_loopback_winsock2}" in
     		"yes" )
			AC_DEFINE(DEF_INADDR_LOOPBACK_IN_WINSOCK2_H,[],
				[Define if you need to include winsock2.h to get INADDR_LOOPBACK defined]) ;;
		* )
			# couldn't find it anywhere
        		AC_DEFINE(HAVE_NO_INADDR_LOOPBACK,[],
				[Define if you don't have a definition of INADDR_LOOPBACK]) ;;
	esac;;
   esac
fi
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
AC_TRY_COMPILE([#include <sys/types.h>
#include <sys/socket.h>], [struct sockaddr s; s.sa_len = 10;],
  ac_cv_struct_sockaddr_sa_len=yes, ac_cv_struct_sockaddr_sa_len=no))

dnl FIXME convbreak
case ${ac_cv_struct_sockaddr_sa_len} in
  "no" ) AC_DEFINE(NO_SA_LEN,[1],[Define if you dont have salen]) ;;
  *) ;;
esac
])

dnl ----------------------------------------------------------------------
dnl
dnl LM_STRUCT_EXCEPTION
dnl
dnl Check to see whether the system supports the matherr function
dnl and its associated type "struct exception".
dnl

AC_DEFUN(LM_STRUCT_EXCEPTION,
[AC_CACHE_CHECK([for struct exception (and matherr function)],
 ac_cv_struct_exception,
AC_TRY_COMPILE([#include <math.h>],
  [struct exception x; x.type = DOMAIN; x.type = SING;],
  ac_cv_struct_exception=yes, ac_cv_struct_exception=no))

case "${ac_cv_struct_exception}" in
  "yes" ) AC_DEFINE(USE_MATHERR,[1],[Define if you have matherr() function and struct exception type]) ;;
  *  ) ;;
esac
])


dnl ----------------------------------------------------------------------
dnl
dnl LM_SYS_IPV6
dnl
dnl Check for ipv6 support and what the in6_addr structure is called.
dnl (early linux used in_addr6 insted of in6_addr)
dnl

AC_DEFUN(LM_SYS_IPV6,
[AC_MSG_CHECKING(for IP version 6 support)
AC_CACHE_VAL(ac_cv_sys_ipv6_support,
[ok_so_far=yes
 AC_TRY_COMPILE([#include <sys/types.h>
#ifdef __WIN32__
#include <winsock2.h>
#include <ws2tcpip.h>
#else
#include <netinet/in.h>
#endif],
   [struct in6_addr a6; struct sockaddr_in6 s6;], ok_so_far=yes, ok_so_far=no)

if test $ok_so_far = yes; then
  ac_cv_sys_ipv6_support=yes
else
  AC_TRY_COMPILE([#include <sys/types.h>
#ifdef __WIN32__
#include <winsock2.h>
#include <ws2tcpip.h>
#else
#include <netinet/in.h>
#endif],
    [struct in_addr6 a6; struct sockaddr_in6 s6;],
    ac_cv_sys_ipv6_support=in_addr6, ac_cv_sys_ipv6_support=no)
fi
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
[AC_EGREP_CPP(yes,
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
[AC_TRY_COMPILE([#include <stdio.h>
#include <errno.h>], [char *msg = *(sys_errlist + 1);],
  ac_cv_decl_sys_errlist=yes, ac_cv_decl_sys_errlist=no)])
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
[AC_TRY_COMPILE([#include <stdio.h>
$3],[$2
char *c = (char *)$1;
], eval "ac_cv_func_decl_$1=no", eval "ac_cv_func_decl_$1=yes")])
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
])# AC_C_DOUBLE_MIDDLE_ENDIAN


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
if test "X$host_os" = "Xwin32"; then
    AC_MSG_RESULT(yes)
    THR_DEFS="-DWIN32_THREADS"
    THR_LIBS=
    THR_LIB_NAME=win32_threads
    THR_LIB_TYPE=win32_threads
else
    AC_MSG_RESULT(no)
    THR_DEFS=
    THR_LIBS=
    THR_LIB_NAME=
    THR_LIB_TYPE=posix_unknown

dnl Try to find POSIX threads

dnl The usual pthread lib...
    AC_CHECK_LIB(pthread, pthread_create, THR_LIBS="-lpthread")

dnl Very old versions of FreeBSD have pthreads in special c library, c_r...
    if test "x$THR_LIBS" = "x"; then
	AC_CHECK_LIB(c_r, pthread_create, THR_LIBS="-lc_r")
    fi

dnl QNX has pthreads in standard C library
    if test "x$THR_LIBS" = "x"; then
	AC_CHECK_FUNC(pthread_create, THR_LIBS="none_needed")
    fi

dnl On ofs1 the '-pthread' switch should be used
    if test "x$THR_LIBS" = "x"; then
	AC_MSG_CHECKING([if the '-pthread' switch can be used])
	saved_cflags=$CFLAGS
	CFLAGS="$CFLAGS -pthread"
	AC_TRY_LINK([#include <pthread.h>],
		    pthread_create((void*)0,(void*)0,(void*)0,(void*)0);,
		    [THR_DEFS="-pthread"
		     THR_LIBS="-pthread"])
	CFLAGS=$saved_cflags
	if test "x$THR_LIBS" != "x"; then
	    AC_MSG_RESULT(yes)
	else
	    AC_MSG_RESULT(no)
	fi
    fi

    if test "x$THR_LIBS" != "x"; then
	THR_DEFS="$THR_DEFS -D_THREAD_SAFE -D_REENTRANT -DPOSIX_THREADS"
	THR_LIB_NAME=pthread
	if test "x$THR_LIBS" = "xnone_needed"; then
	    THR_LIBS=
	fi
	case $host_os in
	    solaris*)
		THR_DEFS="$THR_DEFS -D_POSIX_PTHREAD_SEMANTICS" ;;
	    linux*)
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
		if test $nptl = yes; then
		    THR_LIB_TYPE=posix_nptl
		    need_nptl_incldir=no
		    AC_CHECK_HEADER(nptl/pthread.h,
				    [need_nptl_incldir=yes
				     NEED_NPTL_PTHREAD_H=yes])
		    if test $need_nptl_incldir = yes; then
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
		            AC_CHECK_HEADER($dir/nptl/pthread.h,
					    nptl_incldir=$dir/nptl)
			    if test "x$nptl_incldir" != "x"; then
				THR_DEFS="$THR_DEFS -isystem $nptl_incldir"
				break
			    fi
			done
			if test "x$nptl_incldir" = "x"; then
			    AC_MSG_ERROR(Failed to locate nptl system include directory)
			fi
		    fi
		fi
		;;
	    *) ;;
	esac

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

    fi
fi

])

AC_DEFUN(ERL_INTERNAL_LIBS,
[

ERTS_INTERNAL_X_LIBS=

AC_CHECK_LIB(kstat, kstat_open,
[AC_DEFINE(HAVE_KSTAT, 1, [Define if you have kstat])
ERTS_INTERNAL_X_LIBS="$ERTS_INTERNAL_X_LIBS -lkstat"])

AC_SUBST(ERTS_INTERNAL_X_LIBS)

])

AC_DEFUN(ETHR_CHK_SYNC_OP,
[
    AC_MSG_CHECKING([for $3-bit $1()])
    case "$2" in
	"1") sync_call="$1(&var);";;
	"2") sync_call="$1(&var, ($4) 0);";;
	"3") sync_call="$1(&var, ($4) 0, ($4) 0);";;
    esac
    have_sync_op=no
    AC_TRY_LINK([],
	[
	    $4 res;
	    volatile $4 var;
	    res = $sync_call
	],
	[have_sync_op=yes])
    test $have_sync_op = yes && $5
    AC_MSG_RESULT([$have_sync_op])
])

AC_DEFUN(ETHR_CHK_INTERLOCKED,
[
    ilckd="$1"
    AC_MSG_CHECKING([for ${ilckd}()])
    case "$2" in
	"1") ilckd_call="${ilckd}(var);";;
	"2") ilckd_call="${ilckd}(var, ($3) 0);";;
	"3") ilckd_call="${ilckd}(var, ($3) 0, ($3) 0);";;
	"4") ilckd_call="${ilckd}(var, ($3) 0, ($3) 0, arr);";;
    esac
    have_interlocked_op=no
    AC_TRY_LINK(
	[
	#define WIN32_LEAN_AND_MEAN
	#include <windows.h>
	#include <intrin.h>
	],
	[
	    volatile $3 *var;
	    volatile $3 arr[2];

	    $ilckd_call
	    return 0;
	],
	[have_interlocked_op=yes])
    test $have_interlocked_op = yes && $4
    AC_MSG_RESULT([$have_interlocked_op])
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

LM_CHECK_THR_LIB
ERL_INTERNAL_LIBS

ethr_have_native_atomics=no
ethr_have_native_spinlock=no
ETHR_THR_LIB_BASE="$THR_LIB_NAME"
ETHR_THR_LIB_BASE_TYPE="$THR_LIB_TYPE"
ETHR_DEFS="$THR_DEFS"
ETHR_X_LIBS="$THR_LIBS $ERTS_INTERNAL_X_LIBS"
ETHR_LIBS=
ETHR_LIB_NAME=

ethr_modified_default_stack_size=

dnl Name of lib where ethread implementation is located
ethr_lib_name=ethread

case "$THR_LIB_NAME" in

    win32_threads)
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

	ETHR_CHK_INTERLOCKED([_InterlockedDecrement], [1], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDDECREMENT, 1, [Define if you have _InterlockedDecrement()]))
	ETHR_CHK_INTERLOCKED([_InterlockedDecrement_rel], [1], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDDECREMENT_REL, 1, [Define if you have _InterlockedDecrement_rel()]))
	ETHR_CHK_INTERLOCKED([_InterlockedIncrement], [1], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDINCREMENT, 1, [Define if you have _InterlockedIncrement()]))
	ETHR_CHK_INTERLOCKED([_InterlockedIncrement_acq], [1], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDINCREMENT_ACQ, 1, [Define if you have _InterlockedIncrement_acq()]))
	ETHR_CHK_INTERLOCKED([_InterlockedExchangeAdd], [2], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDEXCHANGEADD, 1, [Define if you have _InterlockedExchangeAdd()]))
	ETHR_CHK_INTERLOCKED([_InterlockedExchangeAdd_acq], [2], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDEXCHANGEADD_ACQ, 1, [Define if you have _InterlockedExchangeAdd_acq()]))
	ETHR_CHK_INTERLOCKED([_InterlockedAnd], [2], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDAND, 1, [Define if you have _InterlockedAnd()]))
	ETHR_CHK_INTERLOCKED([_InterlockedOr], [2], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDOR, 1, [Define if you have _InterlockedOr()]))
	ETHR_CHK_INTERLOCKED([_InterlockedExchange], [2], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDEXCHANGE, 1, [Define if you have _InterlockedExchange()]))
	ETHR_CHK_INTERLOCKED([_InterlockedCompareExchange], [3], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE, 1, [Define if you have _InterlockedCompareExchange()]))
	test "$have_interlocked_op" = "yes" && ethr_have_native_atomics=yes
	ETHR_CHK_INTERLOCKED([_InterlockedCompareExchange_acq], [3], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE_ACQ, 1, [Define if you have _InterlockedCompareExchange_acq()]))
	test "$have_interlocked_op" = "yes" && ethr_have_native_atomics=yes
	ETHR_CHK_INTERLOCKED([_InterlockedCompareExchange_rel], [3], [long], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE_REL, 1, [Define if you have _InterlockedCompareExchange_rel()]))
	test "$have_interlocked_op" = "yes" && ethr_have_native_atomics=yes

	ETHR_CHK_INTERLOCKED([_InterlockedDecrement64], [1], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDDECREMENT64, 1, [Define if you have _InterlockedDecrement64()]))
	ETHR_CHK_INTERLOCKED([_InterlockedDecrement64_rel], [1], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDDECREMENT64_REL, 1, [Define if you have _InterlockedDecrement64_rel()]))
	ETHR_CHK_INTERLOCKED([_InterlockedIncrement64], [1], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDINCREMENT64, 1, [Define if you have _InterlockedIncrement64()]))
	ETHR_CHK_INTERLOCKED([_InterlockedIncrement64_acq], [1], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDINCREMENT64_ACQ, 1, [Define if you have _InterlockedIncrement64_acq()]))
	ETHR_CHK_INTERLOCKED([_InterlockedExchangeAdd64], [2], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDEXCHANGEADD64, 1, [Define if you have _InterlockedExchangeAdd64()]))
	ETHR_CHK_INTERLOCKED([_InterlockedExchangeAdd64_acq], [2], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDEXCHANGEADD64_ACQ, 1, [Define if you have _InterlockedExchangeAdd64_acq()]))
	ETHR_CHK_INTERLOCKED([_InterlockedAnd64], [2], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDAND64, 1, [Define if you have _InterlockedAnd64()]))
	ETHR_CHK_INTERLOCKED([_InterlockedOr64], [2], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDOR64, 1, [Define if you have _InterlockedOr64()]))
	ETHR_CHK_INTERLOCKED([_InterlockedExchange64], [2], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDEXCHANGE64, 1, [Define if you have _InterlockedExchange64()]))
	ETHR_CHK_INTERLOCKED([_InterlockedCompareExchange64], [3], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE64, 1, [Define if you have _InterlockedCompareExchange64()]))
	test "$have_interlocked_op" = "yes" && ethr_have_native_atomics=yes
	ETHR_CHK_INTERLOCKED([_InterlockedCompareExchange64_acq], [3], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE64_ACQ, 1, [Define if you have _InterlockedCompareExchange64_acq()]))
	test "$have_interlocked_op" = "yes" && ethr_have_native_atomics=yes
	ETHR_CHK_INTERLOCKED([_InterlockedCompareExchange64_rel], [3], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE64_REL, 1, [Define if you have _InterlockedCompareExchange64_rel()]))
	test "$have_interlocked_op" = "yes" && ethr_have_native_atomics=yes

	ETHR_CHK_INTERLOCKED([_InterlockedCompareExchange128], [4], [__int64], AC_DEFINE_UNQUOTED(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE128, 1, [Define if you have _InterlockedCompareExchange128()]))

	test "$ethr_have_native_atomics" = "yes" && ethr_have_native_spinlock=yes
	;;

    pthread)
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

	AC_TRY_COMPILE([#include <time.h>
			#include <sys/time.h>], 
			[struct timeval *tv; return 0;],
			AC_DEFINE(ETHR_TIME_WITH_SYS_TIME, 1, \
[Define if you can safely include both <sys/time.h> and <time.h>.]))


	dnl
	dnl Check for functions
	dnl

	AC_CHECK_FUNC(pthread_spin_lock, \
			[ethr_have_native_spinlock=yes \
			 AC_DEFINE(ETHR_HAVE_PTHREAD_SPIN_LOCK, 1, \
[Define if you have the pthread_spin_lock function.])])

	have_sched_yield=no
	have_librt_sched_yield=no
	AC_CHECK_FUNC(sched_yield, [have_sched_yield=yes])
	if test $have_sched_yield = no; then
	    AC_CHECK_LIB(rt, sched_yield,
			 [have_librt_sched_yield=yes
			  ETHR_X_LIBS="$ETHR_X_LIBS -lrt"])
	fi
	if test $have_sched_yield = yes || test $have_librt_sched_yield = yes; then
	    AC_DEFINE(ETHR_HAVE_SCHED_YIELD, 1, [Define if you have the sched_yield() function.])
	    AC_MSG_CHECKING([whether sched_yield() returns an int])
	    sched_yield_ret_int=no
	    AC_TRY_COMPILE([
				#ifdef ETHR_HAVE_SCHED_H
				#include <sched.h>
				#endif
			   ],
			   [int sched_yield();],
			   [sched_yield_ret_int=yes])
	    AC_MSG_RESULT([$sched_yield_ret_int])
	    if test $sched_yield_ret_int = yes; then
		AC_DEFINE(ETHR_SCHED_YIELD_RET_INT, 1, [Define if sched_yield() returns an int.])
	    fi
	fi

	have_pthread_yield=no
	AC_CHECK_FUNC(pthread_yield, [have_pthread_yield=yes])
	if test $have_pthread_yield = yes; then
	    AC_DEFINE(ETHR_HAVE_PTHREAD_YIELD, 1, [Define if you have the pthread_yield() function.])
	    AC_MSG_CHECKING([whether pthread_yield() returns an int])
	    pthread_yield_ret_int=no
	    AC_TRY_COMPILE([
				#if defined(ETHR_NEED_NPTL_PTHREAD_H)
				#include <nptl/pthread.h>
				#elif defined(ETHR_HAVE_MIT_PTHREAD_H)
				#include <pthread/mit/pthread.h>
				#elif defined(ETHR_HAVE_PTHREAD_H)
				#include <pthread.h>
				#endif
			   ],
			   [int pthread_yield();],
			   [pthread_yield_ret_int=yes])
	    AC_MSG_RESULT([$pthread_yield_ret_int])
	    if test $pthread_yield_ret_int = yes; then
		AC_DEFINE(ETHR_PTHREAD_YIELD_RET_INT, 1, [Define if pthread_yield() returns an int.])
	    fi
	fi

	have_pthread_rwlock_init=no
	AC_CHECK_FUNC(pthread_rwlock_init, [have_pthread_rwlock_init=yes])
	if test $have_pthread_rwlock_init = yes; then

	    ethr_have_pthread_rwlockattr_setkind_np=no
	    AC_CHECK_FUNC(pthread_rwlockattr_setkind_np,
			  [ethr_have_pthread_rwlockattr_setkind_np=yes])

	    if test $ethr_have_pthread_rwlockattr_setkind_np = yes; then
		AC_DEFINE(ETHR_HAVE_PTHREAD_RWLOCKATTR_SETKIND_NP, 1, \
[Define if you have the pthread_rwlockattr_setkind_np() function.])

		AC_MSG_CHECKING([for PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP])
		ethr_pthread_rwlock_writer_nonrecursive_initializer_np=no
		AC_TRY_LINK([
				#if defined(ETHR_NEED_NPTL_PTHREAD_H)
				#include <nptl/pthread.h>
				#elif defined(ETHR_HAVE_MIT_PTHREAD_H)
				#include <pthread/mit/pthread.h>
				#elif defined(ETHR_HAVE_PTHREAD_H)
				#include <pthread.h>
				#endif
			    ],
			    [
				pthread_rwlockattr_t *attr;
				return pthread_rwlockattr_setkind_np(attr,
				    PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP);
			    ],
			    [ethr_pthread_rwlock_writer_nonrecursive_initializer_np=yes])
		AC_MSG_RESULT([$ethr_pthread_rwlock_writer_nonrecursive_initializer_np])
		if test $ethr_pthread_rwlock_writer_nonrecursive_initializer_np = yes; then
		    AC_DEFINE(ETHR_HAVE_PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP, 1, \
[Define if you have the PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP rwlock attribute.])
		fi
	    fi
	fi

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

	linux_futex=no
	AC_MSG_CHECKING([for Linux futexes])
	AC_TRY_LINK([
			#include <sys/syscall.h>
			#include <unistd.h>
			#include <linux/futex.h>
			#include <sys/time.h>
		    ],
		    [
			int i = 1;
			syscall(__NR_futex, (void *) &i, FUTEX_WAKE, 1,
				(void*)0,(void*)0, 0);
			syscall(__NR_futex, (void *) &i, FUTEX_WAIT, 0,
				(void*)0,(void*)0, 0);
			return 0;
		    ],
		    linux_futex=yes)
	AC_MSG_RESULT([$linux_futex])
	test $linux_futex = yes && AC_DEFINE(ETHR_HAVE_LINUX_FUTEX, 1, [Define if you have a linux futex implementation.])

	AC_CHECK_SIZEOF(int)
	AC_CHECK_SIZEOF(long)
	AC_CHECK_SIZEOF(long long)
	AC_CHECK_SIZEOF(__int128_t)

	if test "$ac_cv_sizeof_int" = "4"; then
	    int32="int"
	elif test "$ac_cv_sizeof_long" = "4"; then
	    int32="long"
	elif test "$ac_cv_sizeof_long_long" = "4"; then
	    int32="long long"
	else
	    AC_MSG_ERROR([No 32-bit type found])
	fi

	if test "$ac_cv_sizeof_int" = "8"; then
	    int64="int"
	elif test "$ac_cv_sizeof_long" = "8"; then
	    int64="long"
	elif test "$ac_cv_sizeof_long_long" = "8"; then
	    int64="long long"
	else
	    AC_MSG_ERROR([No 64-bit type found])
	fi

	int128=no
	if test "$ac_cv_sizeof___int128_t" = "16"; then
	    int128="__int128_t"
	fi

	ETHR_CHK_SYNC_OP([__sync_val_compare_and_swap], [3], [32], [$int32], AC_DEFINE(ETHR_HAVE___SYNC_VAL_COMPARE_AND_SWAP32, 1, [Define if you have __sync_val_compare_and_swap() for 32-bit integers]))
	test "$have_sync_op" = "yes" && ethr_have_native_atomics=yes
	ETHR_CHK_SYNC_OP([__sync_add_and_fetch], [2], [32], [$int32], AC_DEFINE(ETHR_HAVE___SYNC_ADD_AND_FETCH32, 1, [Define if you have __sync_add_and_fetch() for 32-bit integers]))
	ETHR_CHK_SYNC_OP([__sync_fetch_and_and], [2], [32], [$int32], AC_DEFINE(ETHR_HAVE___SYNC_FETCH_AND_AND32, 1, [Define if you have __sync_fetch_and_and() for 32-bit integers]))
	ETHR_CHK_SYNC_OP([__sync_fetch_and_or], [2], [32], [$int32], AC_DEFINE(ETHR_HAVE___SYNC_FETCH_AND_OR32, 1, [Define if you have __sync_fetch_and_or() for 32-bit integers]))

	ETHR_CHK_SYNC_OP([__sync_val_compare_and_swap], [3], [64], [$int64], AC_DEFINE(ETHR_HAVE___SYNC_VAL_COMPARE_AND_SWAP64, 1, [Define if you have __sync_val_compare_and_swap() for 64-bit integers]))
	test "$have_sync_op" = "yes" && ethr_have_native_atomics=yes
	ETHR_CHK_SYNC_OP([__sync_add_and_fetch], [2], [64], [$int64], AC_DEFINE(ETHR_HAVE___SYNC_ADD_AND_FETCH64, 1, [Define if you have __sync_add_and_fetch() for 64-bit integers]))
	ETHR_CHK_SYNC_OP([__sync_fetch_and_and], [2], [64], [$int64], AC_DEFINE(ETHR_HAVE___SYNC_FETCH_AND_AND64, 1, [Define if you have __sync_fetch_and_and() for 64-bit integers]))
	ETHR_CHK_SYNC_OP([__sync_fetch_and_or], [2], [64], [$int64], AC_DEFINE(ETHR_HAVE___SYNC_FETCH_AND_OR64, 1, [Define if you have __sync_fetch_and_or() for 64-bit integers]))

	if test $int128 != no; then
	    ETHR_CHK_SYNC_OP([__sync_val_compare_and_swap], [3], [128], [$int128], AC_DEFINE(ETHR_HAVE___SYNC_VAL_COMPARE_AND_SWAP128, 1, [Define if you have __sync_val_compare_and_swap() for 128-bit integers]))
	fi

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
	AC_TRY_LINK([#include "atomic_ops.h"],
		    [
			volatile AO_t x;
			AO_t y;
			int z;

			AO_nop_full();
			AO_store(&x, (AO_t) 0);
			z = AO_load(&x);
			z = AO_compare_and_swap_full(&x, (AO_t) 0, (AO_t) 1);
		    ],
		    [ethr_have_native_atomics=yes
		     ethr_have_libatomic_ops=yes])
	AC_MSG_RESULT([$ethr_have_libatomic_ops])
	if test $ethr_have_libatomic_ops = yes; then
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
	elif test "x$with_libatomic_ops" != "xno" && test "x$with_libatomic_ops" != "x"; then
	    AC_MSG_ERROR([No usable libatomic_ops implementation found])
	fi

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
		ethr_have_native_atomics=yes;; 
	  i86pc | i*86 | x86_64 | amd64)
		if test "$enable_x86_out_of_order" = "yes"; then
			AC_DEFINE(ETHR_X86_OUT_OF_ORDER, 1, [Define if x86/x86_64 out of order instructions should be synchronized])
		fi
		ethr_have_native_atomics=yes;;
	  macppc | ppc | "Power Macintosh")
		ethr_have_native_atomics=yes;;
	  tile)
		ethr_have_native_atomics=yes;;
	  *)
		;;
	esac

	test ethr_have_native_atomics = "yes" && ethr_have_native_spinlock=yes

	dnl Restore LIBS
	LIBS=$saved_libs
	dnl restore CPPFLAGS
	CPPFLAGS=$saved_cppflags

	;;
    *)
	;;
esac

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

AC_ARG_ENABLE(native-ethr-impls,
	      AS_HELP_STRING([--disable-native-ethr-impls],
                             [disable native ethread implementations]),
[ case "$enableval" in
    no) disable_native_ethr_impls=yes ;;
    *)  disable_native_ethr_impls=no ;;
  esac ], disable_native_ethr_impls=no)

AC_ARG_ENABLE(x86-out-of-order,
	      AS_HELP_STRING([--enable-x86-out-of-order],
                             [enable x86/x84_64 out of order support (default disabled)]))

test "X$disable_native_ethr_impls" = "Xyes" &&
  AC_DEFINE(ETHR_DISABLE_NATIVE_IMPLS, 1, [Define if you want to disable native ethread implementations])

AC_ARG_ENABLE(prefer-gcc-native-ethr-impls,
	      AS_HELP_STRING([--enable-prefer-gcc-native-ethr-impls],
			     [prefer gcc native ethread implementations]),
[ case "$enableval" in
    yes) enable_prefer_gcc_native_ethr_impls=yes ;;
    *)  enable_prefer_gcc_native_ethr_impls=no ;;
  esac ], enable_prefer_gcc_native_ethr_impls=no)

test $enable_prefer_gcc_native_ethr_impls = yes &&
  AC_DEFINE(ETHR_PREFER_GCC_NATIVE_IMPLS, 1, [Define if you prefer gcc native ethread implementations])

AC_ARG_WITH(libatomic_ops,
	    AS_HELP_STRING([--with-libatomic_ops=PATH],
			   [specify and prefer usage of libatomic_ops in the ethread library]))

AC_ARG_WITH(with_sparc_memory_order,
	    AS_HELP_STRING([--with-sparc-memory-order=TSO|PSO|RMO],
			   [specify sparc memory order (defaults to RMO)]))

ETHR_X86_SSE2_ASM=no
case "$GCC-$ac_cv_sizeof_void_p-$host_cpu" in
  yes-4-i86pc | yes-4-i*86 | yes-4-x86_64 | yes-4-amd64)
    AC_MSG_CHECKING([for gcc sse2 asm support])
    save_CFLAGS="$CFLAGS"
    CFLAGS="$CFLAGS -msse2"
    gcc_sse2_asm=no
    AC_TRY_COMPILE([],
	[
		long long x, *y;
		__asm__ __volatile__("movq %1, %0\n\t" : "=x"(x) : "m"(*y) : "memory");
	],
	[gcc_sse2_asm=yes])
    CFLAGS="$save_CFLAGS"
    AC_MSG_RESULT([$gcc_sse2_asm])
    if test "$gcc_sse2_asm" = "yes"; then
      AC_DEFINE(ETHR_GCC_HAVE_SSE2_ASM_SUPPORT, 1, [Define if you use a gcc that supports -msse2 and understand sse2 specific asm statements])
      ETHR_X86_SSE2_ASM=yes
    fi
    ;;
  *)
    ;;
esac

case "$GCC-$host_cpu" in
  yes-i86pc | yes-i*86 | yes-x86_64 | yes-amd64)
    gcc_dw_cmpxchg_asm=no
    AC_MSG_CHECKING([for gcc double word cmpxchg asm support])    
    AC_TRY_COMPILE([],
	[
    char xchgd;
    long new[2], xchg[2], *p;		  
    __asm__ __volatile__(
#if ETHR_SIZEOF_PTR == 4 && defined(__PIC__) && __PIC__
	"pushl %%ebx\n\t"
	"movl %8, %%ebx\n\t"
#endif
#if ETHR_SIZEOF_PTR == 4
	"lock; cmpxchg8b %0\n\t"
#else
	"lock; cmpxchg16b %0\n\t"
#endif
	"setz %3\n\t"
#if ETHR_SIZEOF_PTR == 4 && defined(__PIC__) && __PIC__
	"popl %%ebx\n\t"
#endif
	: "=m"(*p), "=d"(xchg[1]), "=a"(xchg[0]), "=c"(xchgd)
	: "m"(*p), "1"(xchg[1]), "2"(xchg[0]), "3"(new[1]),
#if ETHR_SIZEOF_PTR == 4 && defined(__PIC__) && __PIC__
	  "r"(new[0])
#else
	  "b"(new[0])
#endif
	: "cc", "memory");

	],
	[gcc_dw_cmpxchg_asm=yes])
    if test $gcc_dw_cmpxchg_asm = no && test $ac_cv_sizeof_void_p = 4; then
      AC_TRY_COMPILE([],
	  [
      char xchgd;
      long new[2], xchg[2], *p;
#if !defined(__PIC__) || !__PIC__
#  error nope
#endif
      __asm__ __volatile__(
    	  "pushl %%ebx\n\t"
	  "movl (%7), %%ebx\n\t"
	  "movl 4(%7), %%ecx\n\t"
	  "lock; cmpxchg8b %0\n\t"
  	  "setz %3\n\t"
	  "popl %%ebx\n\t"
	  : "=m"(*p), "=d"(xchg[1]), "=a"(xchg[0]), "=c"(xchgd)
	  : "m"(*p), "1"(xchg[1]), "2"(xchg[0]), "3"(new)
	: "cc", "memory");

	],
	[gcc_dw_cmpxchg_asm=yes])
      if test "$gcc_dw_cmpxchg_asm" = "yes"; then
        AC_DEFINE(ETHR_CMPXCHG8B_REGISTER_SHORTAGE, 1, [Define if you get a register shortage with cmpxchg8b and position independent code])
      fi
    fi
    AC_MSG_RESULT([$gcc_dw_cmpxchg_asm])
    if test "$gcc_dw_cmpxchg_asm" = "yes"; then
      AC_DEFINE(ETHR_GCC_HAVE_DW_CMPXCHG_ASM_SUPPORT, 1, [Define if you use a gcc that supports the double word cmpxchg instruction])
    fi;;
  *)
    ;;
esac

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
dnl In the presence of a high resolution realtime timer Erlang can adapt
dnl its view of time relative to this timer. On solaris such a timer is
dnl available with the syscall gethrtime(). On other OS's a fallback
dnl solution using times() is implemented. (However on e.g. FreeBSD times()
dnl is implemented using gettimeofday so it doesn't make much sense to
dnl use it there...) On second thought, it seems to be safer to do it the
dnl other way around. I.e. only use times() on OS's where we know it will
dnl work...
dnl

AC_DEFUN(ERL_TIME_CORRECTION,
[if test x$ac_cv_func_gethrtime = x; then
  AC_CHECK_FUNC(gethrtime)
fi
if test x$clock_gettime_correction = xunknown; then
	AC_TRY_COMPILE([#include <time.h>],
			[struct timespec ts;
     			 long long result;
			 clock_gettime(CLOCK_MONOTONIC,&ts);
                         result = ((long long) ts.tv_sec) * 1000000000LL + 
			 ((long long) ts.tv_nsec);],
			clock_gettime_compiles=yes,
			clock_gettime_compiles=no)
else
	clock_gettime_compiles=no
fi
			

AC_CACHE_CHECK([how to correct for time adjustments], erl_cv_time_correction,
[
case $clock_gettime_correction in
    yes)
	erl_cv_time_correction=clock_gettime;;	
    no|unknown)
	case $ac_cv_func_gethrtime in
  	    yes)
    		erl_cv_time_correction=hrtime ;;
  	    no)
    		case $host_os in
        	    linux*)
			case $clock_gettime_correction in
			    unknown)
				if test x$clock_gettime_compiles = xyes; then
				    if test X$cross_compiling != Xyes; then
				    	linux_kernel_vsn_=`uname -r`
				    	case $linux_kernel_vsn_ in
					    [[0-1]].*|2.[[0-5]]|2.[[0-5]].*)
					    	erl_cv_time_correction=times ;;
					    *)
					    	erl_cv_time_correction=clock_gettime;;
				    	esac
				    else
					case X$erl_xcomp_linux_clock_gettime_correction in
					    X)
						erl_cv_time_correction=cross;;
					    Xyes|Xno)
						if test $erl_xcomp_linux_clock_gettime_correction = yes; then
						    erl_cv_time_correction=clock_gettime
						else
					    	    erl_cv_time_correction=times
						fi;;
					    *)
						AC_MSG_ERROR([Bad erl_xcomp_linux_clock_gettime_correction value: $erl_xcomp_linux_clock_gettime_correction]);;
					esac
				    fi
				else
				    erl_cv_time_correction=times
				fi
				;;
			     *)				
        			erl_cv_time_correction=times ;;
			esac
			;;
            	    *)
        		erl_cv_time_correction=none ;;
    		esac
    		;;
	esac
	;;
esac
])

xrtlib=""
case $erl_cv_time_correction in
  times)
    AC_DEFINE(CORRECT_USING_TIMES,[],
	[Define if you do not have a high-res. timer & want to use times() instead])
    ;;
  clock_gettime|cross)
    if test $erl_cv_time_correction = cross; then
	erl_cv_time_correction=clock_gettime
	AC_MSG_WARN([result clock_gettime guessed because of cross compilation])
    fi
    xrtlib="-lrt"
    AC_DEFINE(GETHRTIME_WITH_CLOCK_GETTIME,[1],
	[Define if you want to use clock_gettime to simulate gethrtime])
    ;;
esac
dnl
dnl Check if gethrvtime is working, and if to use procfs ioctl
dnl or (yet to be written) write to the procfs ctl file.
dnl

AC_MSG_CHECKING([if gethrvtime works and how to use it])
AC_TRY_RUN([
/* gethrvtime procfs ioctl test */
/* These need to be undef:ed to not break activation of
 * micro level process accounting on /proc/self 
 */
#ifdef _LARGEFILE_SOURCE
#  undef _LARGEFILE_SOURCE
#endif
#ifdef _FILE_OFFSET_BITS
#  undef _FILE_OFFSET_BITS
#endif
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
],
erl_gethrvtime=procfs_ioctl,
erl_gethrvtime=false,
[
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

case $erl_gethrvtime in
  procfs_ioctl)
	AC_DEFINE(HAVE_GETHRVTIME_PROCFS_IOCTL,[1],
		[define if gethrvtime() works and uses ioctl() to /proc/self])
	AC_MSG_RESULT(uses ioctl to procfs)
	;;
  *)
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

	AC_MSG_CHECKING([if clock_gettime can be used to get process CPU time])
	save_libs=$LIBS
	LIBS="-lrt"
	AC_TRY_RUN([
	#include <stdlib.h>
	#include <unistd.h>
	#include <string.h>
	#include <stdio.h>
	#include <time.h>
	int main() {
	    long long start, stop;
	    int i;
	    struct timespec tp;

	    if (clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &tp) < 0)
	      exit(1);
	    start = ((long long)tp.tv_sec * 1000000000LL) + (long long)tp.tv_nsec;
	    for (i = 0; i < 100; i++)
	      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &tp);
	    stop = ((long long)tp.tv_sec * 1000000000LL) + (long long)tp.tv_nsec;
	    if (start == 0)
	      exit(4);
	    if (start == stop)
	      exit(5);
	    exit(0); return 0;
	  }
	],
	erl_clock_gettime=yes,
	erl_clock_gettime=no,
	[
	case X$erl_xcomp_clock_gettime_cpu_time in
	    X) erl_clock_gettime=cross;;
	    Xyes|Xno) erl_clock_gettime=$erl_xcomp_clock_gettime_cpu_time;;
	    *) AC_MSG_ERROR([Bad erl_xcomp_clock_gettime_cpu_time value: $erl_xcomp_clock_gettime_cpu_time]);;
	esac
	])
	LIBS=$save_libs
	case $host_os in
		linux*)
			AC_MSG_RESULT([no; not stable])
			LIBRT=$xrtlib
			;;
		*)
			AC_MSG_RESULT($erl_clock_gettime)
			case $erl_clock_gettime in
	  			yes)
					AC_DEFINE(HAVE_CLOCK_GETTIME,[],
						  [define if clock_gettime() works for getting process time])
					LIBRT=-lrt
					;;
	  			cross)
					erl_clock_gettime=no
					AC_MSG_WARN([result no guessed because of cross compilation])
					LIBRT=$xrtlib
					;;
	  			*)
					LIBRT=$xrtlib
					;;
			esac
			;;
	esac
	AC_SUBST(LIBRT)
	;;
esac
])dnl

dnl ----------------------------------------------------------------------
dnl
dnl LM_TRY_ENABLE_CFLAG
dnl
dnl
dnl Tries a CFLAG and sees if it can be enabled without compiler errors
dnl $1: textual cflag to add
dnl $2: variable to store the modified CFLAG in
dnl Usage example LM_TRY_ENABLE_CFLAG([-Werror=return-type], [CFLAGS])
dnl
dnl
AC_DEFUN([LM_TRY_ENABLE_CFLAG], [
    AC_MSG_CHECKING([if we can add $1 to CFLAGS])
    saved_CFLAGS=$CFLAGS;
    CFLAGS="$1 $CFLAGS";
    AC_TRY_COMPILE([],[return 0;],can_enable_flag=true,can_enable_flag=false)
    CFLAGS=$saved_CFLAGS;
    if test "X$can_enable_flag" = "Xtrue"; then
        AC_MSG_RESULT([yes])
        AS_VAR_SET($2, "$1 $CFLAGS")
    else
        AC_MSG_RESULT([no])
        AS_VAR_SET($2, "$CFLAGS")
    fi
])

dnl ERL_TRY_LINK_JAVA(CLASSES, FUNCTION-BODY
dnl                   [ACTION_IF_FOUND [, ACTION-IF-NOT-FOUND]])
dnl Freely inspired by AC_TRY_LINK. (Maybe better to create a 
dnl AC_LANG_JAVA instead...)
AC_DEFUN(ERL_TRY_LINK_JAVA,
[java_link='$JAVAC conftest.java 1>&AC_FD_CC'
changequote(, )dnl
cat > conftest.java <<EOF
$1
class conftest { public static void main(String[] args) {
   $2
   ; return; }}
EOF
changequote([, ])dnl
if AC_TRY_EVAL(java_link) && test -s conftest.class; then
   ifelse([$3], , :, [rm -rf conftest*
   $3])
else
   echo "configure: failed program was:" 1>&AC_FD_CC
   cat conftest.java 1>&AC_FD_CC
   echo "configure: PATH was $PATH" 1>&AC_FD_CC
ifelse([$4], , , [  rm -rf conftest*
  $4
])dnl
fi
rm -f conftest*])
#define UNSAFE_MASK  0xc0000000 /* Mask for bits that must be constant */


