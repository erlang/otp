dnl Process this file with autoconf to produce a configure script.

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

AC_PREREQ([2.72])dnl

AC_INIT

m4_include([otp.m4])

LM_PRECIOUS_VARS

ERL_PUSH_WERROR

default_cache_file=./config.cache

if test "x$no_recursion" != "xyes" -a "x$OVERRIDE_CONFIG_CACHE" = "x"; then
    # The no_recursion variable is not documented, but the only
    # action we take on it is disabling caching which is safe!
    if test "x$cache_file" != "x$default_cache_file"; then
	echo "Ignoring the --cache-file argument since it can cause the system to be erroneously configured"
    fi
    echo "Disabling caching"
    if test -f $cache_file; then
	echo "Removing cache file $cache_file"
	rm -f $cache_file
    fi
    cache_file=/dev/null
fi

case "X$ERL_TOP" in
    X)
	;;
    X/*)
	test -f "$ERL_TOP/erts/emulator/beam/erl_init.c" || {
	     AC_MSG_ERROR([Invalid \$ERL_TOP])
	}
	srcdir="$ERL_TOP";;
    *)
	AC_MSG_ERROR([\$ERL_TOP needs to be absolute]);;
esac

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

#
# Now srcdir is absolute and also the top of Erlang distribution, ERL_TOP.
#
test "X$ERL_TOP" != "X" || AC_MSG_ERROR([ERL_TOP not set])
AC_SUBST(ERL_TOP)

dnl
dnl Aux programs are found in erts/autoconf
dnl
AC_CONFIG_AUX_DIRS([${ERL_TOP}/make/autoconf])

ERL_CANONICAL_SYSTEM_TYPE

TARGET=$host
AC_SUBST(TARGET)

if test "$cross_compiling" = "yes"; then
    CROSS_COMPILING=yes
else
    CROSS_COMPILING=no
fi
AC_SUBST(CROSS_COMPILING)

AC_ARG_ENABLE(bootstrap-only,
AS_HELP_STRING([--enable-bootstrap-only],
               [enable bootstrap only configuration]),
[ if test "X$enableval" = "Xyes"; then
     BOOTSTRAP_ONLY=yes
  else
     BOOTSTRAP_ONLY=no  
  fi	
],
BOOTSTRAP_ONLY=no)

AC_SUBST(BOOTSTRAP_ONLY)

if test $CROSS_COMPILING = yes -a $BOOTSTRAP_ONLY = yes; then
    AC_MSG_ERROR([Cannot both cross compile and build a bootstrap system])
fi

dnl Checks for programs.

AC_PROG_CC
AC_PROG_CXX
LM_PROG_LD

LM_WINDOWS_ENVIRONMENT

_search_path=/bin:/usr/bin:/usr/local/bin:$PATH

AC_PATH_PROG(ENV, [env], false, $_search_path)
if test "$ac_cv_path_ENV" = false; then
  AC_MSG_ERROR([No 'env' command found])
fi

#
# We need GNU make, complain if we can't find it
#
AC_MSG_CHECKING(for GNU make)
# If there is a Makefile created we don't want make to start making, run
# in a subdirectory and -f /dev/null
MAKE_PROG=x
if test X"$CLEARCASE_MAKE_COMPAT" = X"gnu" -a X"$CLEARCASE_ROOT" != X"" ; then
  eval clearmake -version 2>&1 | grep clearmake > /dev/null 2>&1
  case $? in
	0) MAKE_PROG="clearmake -V";;
	*);;
  esac
fi
if test X"$MAKE_PROG" = X"x"; then
  mkdir conftestmake
  if test -d conftestmake; then
    cd conftestmake
    for m in make gmake ggmake; do
      eval $m --version -f /dev/null 2>&1 | grep GNU > /dev/null 2>&1
      case $? in
        0) MAKE_PROG=$m ; break ;;
        *) ;;
      esac
    done
    cd ..
  else
    AC_MSG_ERROR(could not create subdirectory)
  fi
fi
rm -rf conftestmake
case $MAKE_PROG in
    x) AC_MSG_RESULT(no)
       AC_MSG_ERROR(GNU make is required!)
       ;;
    *) AC_MSG_RESULT(yes ($MAKE_PROG))
       AC_SUBST(MAKE_PROG)
       ;;
esac

AC_PROG_INSTALL
if test X"${INSTALL}" = "X${ac_aux_dir}/install-sh -c" && test -f /usr/ucb/install ; then
   case $host_os in
      osf*) ;;
      *) INSTALL="/usr/ucb/install -c" ;;
   esac
fi

AC_PROG_LN_S
AC_PROG_RANLIB
LM_PROG_PERL5
if test "$ac_cv_path_PERL" = false; then
  AC_MSG_ERROR([Perl version 5 is required!])
fi

#
# Get erts version from erts/vsn.mk
#
AC_MSG_CHECKING([ERTS version])
[ERTS_VSN=`sed -n "s/^VSN[	 ]*=[	 ]*\(.*\)/\1/p" < $ERL_TOP/erts/vsn.mk`]
AC_MSG_RESULT([$ERTS_VSN])
AC_SUBST(ERTS_VSN)

#
# Get OTP release and OTP version from $ERL_TOP/OTP_VERSION
#
AC_MSG_CHECKING([OTP release])
[OTP_REL=`cat $ERL_TOP/OTP_VERSION | sed "s|\([0-9]*\).*|\1|"`]
AC_MSG_RESULT([$OTP_REL])
AC_SUBST(OTP_REL)

AC_MSG_CHECKING([OTP version])
[OTP_VSN=`cat $ERL_TOP/OTP_VERSION`]
AC_MSG_RESULT([$OTP_VSN])
AC_SUBST(OTP_VSN)

AC_ARG_ENABLE(parallel-configure,
AS_HELP_STRING([--disable-parallel-configure], [disable parallel execution of configure scripts]))

AC_ARG_ENABLE(dirty-schedulers,
AS_HELP_STRING([--enable-dirty-schedulers], [enable dirty scheduler support]))

AC_ARG_WITH(termcap,
AS_HELP_STRING([--with-termcap], [use termcap (default)])
AS_HELP_STRING([--without-termcap],
               [do not use any termcap libraries (ncurses,curses,termcap,termlib)]))

AC_ARG_ENABLE(kernel-poll,
AS_HELP_STRING([--enable-kernel-poll], [enable kernel poll support])
AS_HELP_STRING([--disable-kernel-poll], [disable kernel poll support]))

AC_ARG_ENABLE(sctp,
AS_HELP_STRING([--enable-sctp], [enable sctp support (default)
to on demand load the SCTP library in runtime])
AS_HELP_STRING([--enable-sctp=lib], [enable sctp support
to link against the SCTP library])
AS_HELP_STRING([--disable-sctp], [disable sctp support]))

AC_ARG_WITH(dynamic-trace,
AS_HELP_STRING([--with-dynamic-trace={dtrace|lttng|systemtap}],
	       [specify use of dynamic trace framework, dtrace, lttng or systemtap])
AS_HELP_STRING([--without-dynamic-trace], 
               [don't enable any dynamic tracing (default)]))
AC_ARG_ENABLE(vm-probes,
AS_HELP_STRING([--enable-vm-probes],
               [add dynamic trace probes to the Beam VM (only possible if --with-dynamic-trace is enabled, and then default)]))
AC_ARG_WITH(javac,
AS_HELP_STRING([--with-javac=JAVAC], [specify Java compiler to use])
AS_HELP_STRING([--with-javac], [use a Java compiler if found (default)])
AS_HELP_STRING([--without-javac], [don't use any Java compiler]))

AC_ARG_ENABLE(megaco_flex_scanner_lineno,
AS_HELP_STRING([--disable-megaco-flex-scanner-lineno],
               [disable megaco flex scanner lineno]))

AC_ARG_ENABLE(megaco_reentrant_flex_scanner,
AS_HELP_STRING([--disable-megaco-reentrant-flex-scanner],
               [disable reentrant megaco flex scanner]))

AC_ARG_WITH(ssl,
AS_HELP_STRING([--with-ssl=PATH], [base location of OpenSSL include and lib directories])
AS_HELP_STRING([--with-ssl], [use SSL (default)])
AS_HELP_STRING([--without-ssl], [don't use SSL]))

AC_ARG_WITH(ssl-incl,
AS_HELP_STRING([--with-ssl-incl=PATH],
	       [base location of OpenSSL include directory (if different than base location specified by --with-ssl=PATH)]))

AC_ARG_WITH(ssl-zlib,
AS_HELP_STRING([--with-ssl-zlib=PATH], [Path to static zlib library to link the
    				       crypto NIF with. This zlib library is most
				       often not necessary but might be needed in
				       order to link the NIF in some cases.]))

AC_ARG_WITH(ssl-lib-subdir,
AS_HELP_STRING([--with-ssl-lib-subdir=RELATIVE_PATH],
	       [specify extra OpenSSL lib sub-directory to search in (relative to base directory)]))

AC_ARG_WITH(ssl-rpath,
AS_HELP_STRING([--with-ssl-rpath=yes|no|PATHS],
               [runtime library path for OpenSSL. Default is 'yes', which equates to a
	       number of standard locations. If 'no', then no runtime
	       library paths will be used. Anything else should be a
	       comma or colon separated list of paths.]))

AC_ARG_ENABLE(dynamic-ssl-lib,
AS_HELP_STRING([--enable-dynamic-ssl-lib],
               [enable using dynamic openssl libraries when linking the crypto NIF])
AS_HELP_STRING([--disable-dynamic-ssl-lib],
               [disable using dynamic openssl libraries when linking the crypto NIF]))

AC_ARG_ENABLE(fips,
AS_HELP_STRING([--enable-fips], [enable OpenSSL FIPS mode support])
AS_HELP_STRING([--disable-fips], [disable OpenSSL FIPS mode support (default)]))

AC_ARG_ENABLE(builtin-zlib,
AS_HELP_STRING([--enable-builtin-zlib],
               [force use of our own built-in zlib]))

AC_ARG_ENABLE(esock,
AS_HELP_STRING([--enable-esock], [enable builtin experimental socket (as a nif) support (default)])
AS_HELP_STRING([--disable-esock], [disable builtin experimental socket (as a nif) support]))

AC_ARG_ENABLE(sharing-preserving,
AS_HELP_STRING([--enable-sharing-preserving],
               [enable copying of terms without destroying sharing]))

dnl This functionality has been lost along the way... :(
dnl It could perhaps be nice to reintroduce some day; therefore,
dnl it is not removed just commented out.
dnl
dnl #
dnl # Set Erlang man page directory
dnl #
dnl AC_ARG_ENABLE(erlang-mandir,
dnl [  --disable-erlang-mandir do not install Erlang man pages in a private directory],
dnl [ case "$enableval" in
dnl     no) erl_mandir=$mandir ;;
dnl     *)  erl_mandir='$(erlang_libdir)/man' ;;
dnl   esac ], erl_mandir='$(erlang_libdir)/man')
dnl AC_SUBST(erl_mandir)

AC_ARG_ENABLE(m64-build,
AS_HELP_STRING([--enable-m64-build],
               [build 64bit binaries using the -m64 flag to (g)cc]),
[ case "$enableval" in
    no) enable_m64_build=no ;;
    *)  enable_m64_build=yes ;;
  esac
],enable_m64_build=no)

AC_ARG_ENABLE(m32-build,
AS_HELP_STRING([--enable-m32-build],
               [build 32bit binaries using the -m32 flag to (g)cc]),
[ case "$enableval" in
    no) enable_m32_build=no ;;
    *)	enable_m32_build=yes ;;
  esac
],enable_m32_build=no)

AC_ARG_ENABLE(pie,
AS_HELP_STRING([--enable-pie], [build position independent executables])
AS_HELP_STRING([--disable-pie], [do no build position independent executables]))

AC_ARG_WITH(libatomic_ops,
	    AS_HELP_STRING([--with-libatomic_ops=PATH],
			   [specify and prefer usage of libatomic_ops in the ethread library]))

AC_ARG_ENABLE(ensure-os-monotonic-time,
AS_HELP_STRING([--enable-ensure-os-monotonic-time],
               [enable functionality ensuring the monotonicity of monotonic timestamps delivered by the OS. When a non-monotonic timestamp is detected, it will be replaced by the last delivered monotonic timestamp before being used by Erlang's time functionality. Note that you do *not* want to enable this unless the OS monotonic time source on the system fails to produce monotonic timestamps. This since ensuring the monotonicity of OS monotonic timestamps will hurt scalability and performance of the system.]))

AC_ARG_ENABLE([silent-rules], [dnl
AS_HELP_STRING(
  [--enable-silent-rules],
  [less verbose build output (undo: "make V=1")])
AS_HELP_STRING(
  [--disable-silent-rules],
  [verbose build output (undo: "make V=0")])dnl
])

DEFAULT_VERBOSITY=0
if test X${enable_silent_rules} = Xno; then
  DEFAULT_VERBOSITY=1
fi
AC_SUBST(DEFAULT_VERBOSITY)

if test X${enable_m64_build} = Xyes; then
	CFLAGS="-m64 $CFLAGS"
	export CFLAGS
	LDFLAGS="-m64 $LDFLAGS"
	export LDFLAGS
fi
if test X${enable_m32_build} = Xyes; then
	CFLAGS="-m32 $CFLAGS"
	export CFLAGS
	LDFLAGS="-m32 $LDFLAGS"
	export LDFLAGS
fi

ERL_DED

ERL_POP_WERROR

AC_CONFIG_FILES([../Makefile output.mk ../make/$host/otp_ded.mk:../make/otp_ded.mk.in])

AC_OUTPUT
