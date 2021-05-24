
define([AC_CACHE_LOAD], )dnl
define([AC_CACHE_SAVE], )dnl

AC_INIT
AC_CONFIG_SRCDIR([src/app/snmp.app.src])
AC_PREREQ([2.71])

AC_CONFIG_AUX_DIRS(${ERL_TOP}/erts/autoconf)

dnl ----------------------------------------------------------------------
dnl Checks for programs.
dnl ----------------------------------------------------------------------

if test "$host" = "free_source" -o "$host" = "win32"; then
   host_os="$host"
else   
   AC_CANONICAL_HOST
fi

AC_CHECK_PROG(PERL, perl, perl, no_perl)
if test "$PERL" = no_perl; then
  AC_MSG_ERROR([Perl is required to generate v2 to v1 mib converter script])
fi


AC_CONFIG_FILES([mibs/Makefile:mibs/Makefile.in])
AC_OUTPUT

