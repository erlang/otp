dnl %CopyrightBegin%
dnl
dnl SPDX-License-Identifier: Apache-2.0
dnl
dnl Copyright Ericsson AB 2021-2025. All Rights Reserved.
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

AC_INIT
AC_PREREQ([2.71])

m4_include([otp.m4])

AC_CONFIG_AUX_DIR([${ERL_TOP}/make/autoconf])

ERL_PUSH_WERROR

ERL_CANONICAL_SYSTEM_TYPE

TARGET=$host
AC_SUBST(TARGET)

ERL_POP_WERROR

AC_CONFIG_FILES([priv/$host/Makefile:priv/Makefile.in])
AC_OUTPUT
