
define([AC_CACHE_LOAD], )dnl
define([AC_CACHE_SAVE], )dnl

AC_INIT
AC_CONFIG_SRCDIR([src/app/snmp.app.src])
AC_PREREQ([2.71])

m4_include([otp.m4])

AC_CONFIG_AUX_DIRS([${ERL_TOP}/make/autoconf])

dnl ----------------------------------------------------------------------
dnl Checks for programs.
dnl ----------------------------------------------------------------------

ERL_CANONICAL_SYSTEM_TYPE

AC_CHECK_PROG(PERL, perl, perl, no_perl)
if test "$PERL" = no_perl; then
  AC_MSG_ERROR([Perl is required to generate v2 to v1 mib converter script])
fi


AC_CONFIG_FILES([mibs/Makefile:mibs/Makefile.in])
AC_OUTPUT

