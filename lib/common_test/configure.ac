AC_INIT
AC_PREREQ([2.71])

AC_CONFIG_AUX_DIR(priv/auxdir)

if test "$host" = "free_source" -o "$host" = "win32"; then
   host_os="$host"
else   
   AC_CANONICAL_HOST
fi

TARGET=$host
AC_SUBST(TARGET)

AC_CONFIG_FILES([priv/$host/Makefile:priv/Makefile.in])
AC_OUTPUT
