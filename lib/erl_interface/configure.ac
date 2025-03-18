#                                               -*- Autoconf -*-
# %CopyrightBegin%
#
# Copyright Ericsson AB 2000-2025. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# %CopyrightEnd%
#
# Process this file with autoconf to produce a configure script.
# The starting point for this file was the output from 'autoscan'.

# Find the erl_interface version number and set m4 macro to it.
# We do this because AC_INIT can't handle shell variables. Still broken.
dnl m4_define(EI_VERSION,`grep EI_VSN ../vsn.mk | sed 's/^.*=[ ]*//'`)
dnl m4_define(EI_VERSION,regexp(m4_include(VERSION),[version \([-.0-9A-Za-z]+\)],[\1]))

AC_INIT
AC_CONFIG_SRCDIR([include/ei.h])
AC_PREREQ([2.72])

m4_include([otp.m4])

ERL_PUSH_WERROR

dnl How to set srcdir absolute is taken from the GNU Emacs distribution
#### Make srcdir absolute, if it isn't already.  It's important to
#### avoid running the path through pwd unnecessary, since pwd can
#### give you automounter prefixes, which can go away.
case "${srcdir}" in
  /* ) ;;
  . )
    ## We may be able to use the $PWD environment variable to make this
    ## absolute.  But sometimes PWD is inaccurate.
    ## Make sure CDPATH doesn't affect cd (in case PWD is relative).
    CDPATH=
    if test "${PWD}" != "" && test "`(cd ${PWD} ; sh -c pwd)`" = "`pwd`"  ;
    then
      srcdir="$PWD"
    else
      srcdir="`(cd ${srcdir}; pwd)`"
    fi
  ;;
  *  ) srcdir="`(cd ${srcdir}; pwd)`" ;;
esac

AC_CONFIG_AUX_DIR([${ERL_TOP}/make/autoconf])

ERL_CANONICAL_SYSTEM_TYPE

TARGET=$host
AC_SUBST(TARGET)

AC_CONFIG_HEADERS([src/$host/config.h:config.h.in])

dnl ----------------------------------------------------------------------
dnl Optional features
dnl ----------------------------------------------------------------------

# Use --disable-threads to force building single threaded libs even 
# if pthreads exists (for test purposes).
AC_ARG_ENABLE(threads,
[  --disable-threads       use to only build single threaded libs],
[ case "$enableval" in
    no) threads_disabled=yes ;;
    *)  threads_disabled=no ;;
  esac ],
[ threads_disabled=maybe ])

AC_ARG_ENABLE(mask-real-errno,
[  --disable-mask-real-errno do not mask real 'errno'],
[ case "$enableval" in
    no) mask_real_errno=no ;;
    *)  mask_real_errno=yes ;;
  esac ],
[ mask_real_errno=yes ])

AC_ARG_ENABLE(ei-dynamic-lib,
[  --enable-ei-dynamic-lib  build ei as a dynamic library],
[ case "$enableval" in
    yes) DYNAMIC_LIB=yes ;;
    *)   DYNAMIC_LIB=no ;;
  esac ],
[ DYNAMIC_LIB=no ])
AC_SUBST(DYNAMIC_LIB)

dnl ----------------------------------------------------------------------
dnl Checks for programs
dnl ----------------------------------------------------------------------

AC_PROG_CC
AC_PROG_CPP
AC_PROG_EGREP
dnl AC_PROG_LIBTOOL
AC_PROG_RANLIB
LM_PROG_LD
AC_SUBST(LD)

ERL_DED_FLAT_BUNDLE=true
ERL_DED_FLAGS

# Subst flags for libs

AC_SUBST(LIB_CFLAGS)
AC_SUBST(LIB_LD)
AC_SUBST(LIB_LDFLAGS)
AC_SUBST(LIB_LD_FLAG_RUNTIME_LIBRARY_PATH)
AC_SUBST(LIB_EXT)

LIB_CFLAGS="$DED_BASIC_CFLAGS $DED_THR_DEFS"
LIB_LD="$DED_LD"
LIB_LDFLAGS="$DED_LDFLAGS"
LIB_LD_FLAG_RUNTIME_LIBRARY_PATH="$DED_LD_FLAG_RUNTIME_LIBRARY_PATH"
LIB_EXT="$DED_EXT"

AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(void *)
AC_CHECK_SIZEOF(long long)

if test $mask_real_errno = yes; then
   AC_DEFINE(EI_HIDE_REAL_ERRNO, 1, [Define if 'errno' should not be exposed as is in 'erl_errno'])
fi

dnl We set EI_64BIT mode when long is 8 bytes, this makes things
dnl work on windows and unix correctly
if test $ac_cv_sizeof_long = 8; then
  CFLAGS="$CFLAGS -DEI_64BIT"
  LIB_CFLAGS="$LIB_CFLAGS -DEI_64BIT"
fi

LM_HARDWARE_ARCH

AC_MSG_CHECKING(for unaligned word access)
case "$ARCH" in
    x86|amd64)
	AC_MSG_RESULT(yes: x86 or amd64)
	AC_DEFINE(HAVE_UNALIGNED_WORD_ACCESS, 1, [Define if hw supports unaligned word access])
	;;
    *)
	AC_MSG_RESULT(no)
	;;
esac

AC_CHECK_TOOL(AR, ar, false)
if test "$AR" = false; then
  AC_MSG_ERROR([No 'ar' command found in PATH])
fi

dnl
dnl We can live with Solaris /usr/ucb/install
dnl
case $host in
  *-*-solaris*|free_source)
    if test -x /usr/ucb/install; then
      INSTALL="/usr/ucb/install -c"
    fi
    ;;
  *)
    ;;
esac

AC_PROG_INSTALL
LM_PROG_INSTALL_DIR
AC_SUBST(INSTALL_DIR)

case $host_os in
	darwin*)
		dnl Need to preserve modification time on archives;
		dnl otherwise, ranlib has to be run on archives
		dnl again after installation.
		INSTALL_DATA="$INSTALL_DATA -p";;
	*)
		;;
esac

# Checks for libraries.
AC_SEARCH_LIBS([gethostbyname], [nsl])
AC_SEARCH_LIBS([getpeername], [socket])

# Checks for header files.
AC_CHECK_INCLUDES_DEFAULT

AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([arpa/inet.h fcntl.h limits.h malloc.h netdb.h netinet/in.h stddef.h stdlib.h string.h sys/param.h sys/socket.h sys/select.h sys/time.h unistd.h sys/types.h sys/uio.h])

# Checks for typedefs, structures, and compiler characteristics.
# fixme AC_C_CONST & AC_C_VOLATILE needed for Windows?
dnl AC_C_CONST
dnl AC_C_VOLATILE
AC_TYPE_UID_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_CHECK_HEADERS_ONCE([sys/time.h])

AC_MSG_CHECKING([for socklen_t usability])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/types.h>
#include <sys/socket.h>]], [[socklen_t mylen;]])],[AC_MSG_RESULT(yes) 
 AC_DEFINE(HAVE_SOCKLEN_T, [], [Define if you have the `socklen_t' type])],[AC_MSG_RESULT(no)])

# Checks for library functions.
AC_FUNC_ALLOCA
dnl AC_FUNC_FORK
# Check if malloc(0) is ok 
dnl AC_FUNC_MALLOC
dnl AC_FUNC_REALLOC
AC_FUNC_MEMCMP
dnl AC_FUNC_SELECT_ARGTYPES

dnl AC_FUNC_STRERROR_R
dnl AC_FUNC_VPRINTF
AC_CHECK_FUNCS([dup2 gethostbyaddr gethostbyname \
	gethostbyaddr_r \
	gethostbyname_r gethostname writev \
	gethrtime gettimeofday inet_ntoa memchr memmove memset select \
	socket strchr strerror strrchr strstr uname sysconf])
AC_CHECK_FUNC(res_gethostbyname, [],
    AC_CHECK_LIB(resolv, res_gethostbyname)
)
AC_CHECK_FUNC(clock_gettime, [],
    AC_CHECK_LIB(rt, clock_gettime)
)

# ---------------------------------------------------------------------------
# We don't link against libgmp except for "make check"
# but linking will also tell us that it is >= 4.1
# ---------------------------------------------------------------------------

AC_ARG_WITH(gmp,
[  --with-gmp=PATH         specify location of GNU MP include and lib
  --with-gmp              use GNU MP (will search for it)])

# We don't just want any GNU MP version, we want 4.1 or later
# that contain the import/export functions we need.

AS_IF([test "x$with_gmp" = "xyes"],
  [
    for dir in /usr /usr/pkg /usr/local /usr/local/gmp /usr/lib/gmp /usr/gmp; do
	AC_CHECK_HEADER($dir/include/gmp.h, ac_cv_gmp=yes, ac_cv_gmp=no)
	if test $ac_cv_gmp = yes ; then
	    CFLAGS="$CFLAGS -I$dir/include -L$dir/lib"
	    LIB_CFLAGS="$LIB_CFLAGS -I$dir/include -L$dir/lib"
	    AC_DEFINE(HAVE_GMP_H, [], [Define if you have "gmp.h"])
	    break
	fi
    done
    if test $ac_cv_gmp = no ; then
	AC_MSG_ERROR([No GNU MP installation found])
    fi
    AC_CHECK_LIB(gmp, __gmpz_export)
    # FIXME return ERROR if no lib
elif test "x$with_gmp" != "xno" -a -n "$with_gmp" ;then
    # Option given with PATH to package
    AC_MSG_CHECKING(for GNU MP)
    if test ! -d "$with_gmp" ; then
	AC_MSG_ERROR(Invalid path to option --with-gmp=PATH)
    fi
    AC_MSG_RESULT(yes)
    CFLAGS="$CFLAGS -I$with_gmp/include -L$with_gmp/lib"
    LIB_CFLAGS="$LIB_CFLAGS -I$with_gmp/include -L$with_gmp/lib"
    AC_DEFINE(HAVE_GMP_H, [], [Define if you have "gmp.h"])
    AC_CHECK_LIB(gmp, __gmpz_export)
    # FIXME return ERROR if no lib
  ])

LM_WINDOWS_ENVIRONMENT
	
	
dnl
dnl Threads
dnl
THR_LIBS=
THR_DEFS=
AC_SUBST(THR_LIBS)
AC_SUBST(THR_DEFS)
AC_SUBST(EI_THREADS)

AS_CASE(["$threads_disabled"],

    [no|maybe],
    [
	LM_CHECK_THR_LIB
        ETHR_CHK_GCC_ATOMIC_OPS([])

	case "$THR_LIB_NAME" in
	    "")
	        EI_THREADS="false"
		# Fail if --enable-threads given and no threads found
		if test "x$threads_disabled" = "xno"; then
		    AC_MSG_ERROR([No threads support found])
		fi
	        ;;
	    win32_threads)
		EI_THREADS="true"
		THR_DEFS="$THR_DEFS -D_REENTRANT -D_WIN32_WINNT=0x0600 -DWINVER=0x0600"
		;;
	    pthread)
		EI_THREADS="true"
		;;
	    *)
		EI_THREADS="true"
	    	AC_MSG_WARN([Unexpected thread library: $THR_LIB_NAME])
		;;
	esac
    ],

    [yes],
    [
	# Threads disabled
	EI_THREADS="false"
    ])

# ---------------------------------------------------------------------------
# Warning flags to the C compiler
# ---------------------------------------------------------------------------
AC_SUBST(WFLAGS)

WFLAGS="$WERRORFLAGS $DED_WERRORFLAGS $DED_WARN_FLAGS"
AS_IF([test "x$GCC" = xyes],
      [WFLAGS="$WFLAGS -Wmissing-declarations -Wnested-externs -Winline"])

ERL_OSSF_FLAGS

# Use -fno-common for gcc, that is link error if multiple definitions of
# global variables are encountered. This is ISO C compliant.
# Until version 10, gcc has had -fcommon as default, which allows and merges
# such dubious duplicates.
LM_TRY_ENABLE_CFLAG([-fno-common], [CFLAGS])

ERL_POP_WERROR

# ---------------------------------------------------------------------------
# XXX
# ---------------------------------------------------------------------------


AC_CONFIG_FILES([src/$host/Makefile:src/Makefile.in
	src/$host/eidefs.mk:src/eidefs.mk.in
	])
AC_OUTPUT
