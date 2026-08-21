dnl Process this file with autoconf to produce a configure script. -*-Autoconf-*-
dnl
dnl %CopyrightBegin%
dnl
dnl SPDX-License-Identifier: Apache-2.0
dnl
dnl Copyright Ericsson AB 2018-2026. All Rights Reserved.
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

ERL_PUSH_WERROR

ERL_CANONICAL_SYSTEM_TYPE

AC_LANG(C)

LM_PRECIOUS_VARS

## Delete previous failed configure results
if test -f ./CONF_INFO; then
   rm ./CONF_INFO
fi

if test "$cross_compiling" = "yes"; then
    CROSS_COMPILING=yes
else
    CROSS_COMPILING=no
fi
AC_SUBST(CROSS_COMPILING)

ERL_XCOMP_SYSROOT_INIT

AC_PROG_CC
LM_PROG_LD
LM_WINDOWS_ENVIRONMENT

m4_include([../../erts/crypto.ac])

ERL_POP_WERROR

AC_CONFIG_FILES([c_src/$host/Makefile:c_src/Makefile.in])
AC_OUTPUT

