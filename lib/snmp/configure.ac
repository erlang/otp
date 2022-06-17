dnl Process this file with autoconf to produce a configure script. -*-m4-*-
dnl
dnl %CopyrightBegin%
dnl
dnl Copyright Ericsson AB 2021-2022. All Rights Reserved.
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


dnl ----------------------------------------------------------------------

dnl *** SNMP_EMPTY_PDU_SIZE ***

AC_ARG_WITH(snmp-empty-pdu-size, 
AS_HELP_STRING([--with-snmp-empty-pdu-size=SZ], 
               [Empty pdu size, in number of bytes >= 21; default is 21]), 
[], 
[with_snmp_empty_pdu_size=21])

if test $with_snmp_empty_pdu_size -lt 21; then
  SNMP_EMPTY_PDU_SIZE_DEFAULT=21
  AC_MSG_ERROR([Minimum size of 'empty pdu size' is 21])
else
  SNMP_EMPTY_PDU_SIZE_DEFAULT=$with_snmp_empty_pdu_size
  AC_MSG_NOTICE([Default (snmp) empty pdu size set to $with_snmp_empty_pdu_size])
fi


dnl ----------------------------------------------------------------------

AC_SUBST(SNMP_EMPTY_PDU_SIZE_DEFAULT)
AC_CONFIG_FILES([mibs/Makefile:mibs/Makefile.in])
AC_CONFIG_FILES([src/agent/Makefile:src/agent/Makefile.in])
AC_OUTPUT

