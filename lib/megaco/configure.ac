dnl Process this file with autoconf to produce a configure script. -*-m4-*-
dnl
dnl %CopyrightBegin%
dnl
dnl Copyright Ericsson AB 2001-2025. All Rights Reserved.
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

dnl define([AC_CACHE_LOAD], )dnl
dnl define([AC_CACHE_SAVE], )dnl


AC_INIT
AC_CONFIG_SRCDIR([vsn.mk])
AC_PREREQ([2.72])

m4_include([otp.m4])

AC_CONFIG_AUX_DIR([${ERL_TOP}/make/autoconf])

ERL_CANONICAL_SYSTEM_TYPE

ERL_PUSH_WERROR

dnl ----------------------------------------------------------------------
dnl Checks for programs.
dnl ----------------------------------------------------------------------

AC_PROG_CC
LM_PROG_LD

LM_WINDOWS_ENVIRONMENT

AC_DEFUN(ERL_REENTRANT_FLEX,
[flex_compile='$LEX -R -Pconftest -oconftest.c conftest.flex 1>&AS_MESSAGE_LOG_FD'
changequote(253, 273)dnl
cat > conftest.flex <<EOF
/*
 * This (reentrant) example code comes from the flex manual
 */

%option reentrant stack noyywrap
%x COMMENT

%%

"//"                 yy_push_state( COMMENT, yyscanner);
.|\n

<COMMENT>\n          yy_pop_state( yyscanner );
<COMMENT>[^\n]+      fprintf( yyout, "%s\n", yytext);

%%

int main ( int argc, char * argv[] )
{
  yyscan_t scanner;

  yylex_init ( &scanner );
  yylex ( scanner );
  yylex_destroy ( scanner );
  return 0;
}
EOF
changequote([, ])dnl
AC_MSG_CHECKING(for reentrant capable flex)
if AC_TRY_EVAL(flex_compile) && test -s conftest.c; then
   ifelse([$1], , :, [
   $1])
   AC_MSG_RESULT([yes])
else
   echo "configure: failed program was:" 1>&AS_MESSAGE_LOG_FD
   cat conftest.flex 1>&AS_MESSAGE_LOG_FD
   echo "configure: PATH was $PATH" 1>&AS_MESSAGE_LOG_FD
ifelse([$2], , , [
  $2
])dnl
   AC_MSG_RESULT([no])
fi
])


dnl
dnl Shall we attempt to use reentrant flex scanner or not
dnl
AC_ARG_ENABLE(megaco_reentrant_flex_scanner,
[  --enable-megaco-reentrant-flex-scanner  enable reentrant megaco flex scanner
  --disable-megaco-reentrant-flex-scanner  disable reentrant megaco flex scanner],
  if test x${enable_megaco_reentrant_flex_scanner} = xno ; then
    ENABLE_REENTRANT_MEGACO_FLEX_SCANNER=false
  else
    ENABLE_REENTRANT_MEGACO_FLEX_SCANNER=true
  fi, 
  ENABLE_REENTRANT_MEGACO_FLEX_SCANNER=true)


dnl
dnl flex is needed by megaco. lex wont do!
dnl

AC_PROG_LEX([noyywrap])
if test "$LEX" != flex; then
  ENABLE_MEGACO_FLEX_SCANNER=false
else
  ENABLE_MEGACO_FLEX_SCANNER=true
  dnl Check if we can generate a reentrant scanner
  dnl ENABLE_REENTRANT_MEGACO_FLEX_SCANNER=true
  if ${ENABLE_REENTRANT_MEGACO_FLEX_SCANNER} = true ; then
    ERL_REENTRANT_FLEX(ENABLE_REENTRANT_MEGACO_FLEX_SCANNER=true,
                       ENABLE_REENTRANT_MEGACO_FLEX_SCANNER=false)
  fi
fi
AC_SUBST(ENABLE_MEGACO_FLEX_SCANNER)
AC_SUBST(ENABLE_REENTRANT_MEGACO_FLEX_SCANNER)



dnl
dnl For increased performance it is possible to disable lineno
dnl
AC_ARG_ENABLE(megaco_flex_scanner_lineno,
[  --enable-megaco-flex-scanner-lineno  enable megaco flex scanner lineno 
  --disable-megaco-flex-scanner-lineno  disable megaco flex scanner lineno],
  if test x${enable_megaco_flex_scanner_lineno} = xno ; then
    ENABLE_MEGACO_FLEX_SCANNER_LINENO=false
  else
    ENABLE_MEGACO_FLEX_SCANNER_LINENO=true
  fi, 
  ENABLE_MEGACO_FLEX_SCANNER_LINENO=true)
AC_SUBST(ENABLE_MEGACO_FLEX_SCANNER_LINENO)


dnl This is the os flavour, should be unix or win32
if test "X$host" = "Xwin32"; then
	ERLANG_OSTYPE=win32
else
	ERLANG_OSTYPE=unix
fi

AC_SUBST(ERLANG_OSTYPE)

dnl Magic test for clearcase.
if test -d ../../system; then
	OTP_EXTRA_FLAGS=-DOTP_RELEASE
else
	OTP_EXTRA_FLAGS=
fi
AC_SUBST(OTP_EXTRA_FLAGS)

if test "x$GCC" = xyes; then
  # Treat certain GCC warnings as errors
  LM_TRY_ENABLE_CFLAG([-Werror=return-type], [CFLAGS])
fi

ERL_DED

AC_CHECK_PROG(PERL, perl, perl, no_perl)
if test "$PERL" = no_perl; then
  AC_MSG_ERROR([Perl is required to build the flex scanner!])
fi

ERL_POP_WERROR

AC_CONFIG_FILES([examples/meas/Makefile:examples/meas/Makefile.in])
AC_OUTPUT
AC_CONFIG_FILES([src/flex/$host/Makefile:src/flex/Makefile.in])
AC_OUTPUT

