AC_INIT
AC_PREREQ([2.71])

m4_include([otp.m4])

AC_CONFIG_AUX_DIR([${ERL_TOP}/make/autoconf])

ERL_CANONICAL_SYSTEM_TYPE

TARGET=$host
AC_SUBST(TARGET)

AC_CONFIG_FILES([priv/$host/Makefile:priv/Makefile.in])
AC_OUTPUT
