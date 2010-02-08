#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2010. All Rights Reserved.
#
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
#
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
#
# %CopyrightEnd%
#

#
# erl_build_tool_vars - Build tool configuration variables currently
#                       reqognized by `configure' scripts in OTP.
#
# NOTE:
#    When updating, also update $ERL_TOP/xcomp/{README,erl-xcomp.conf.template},
#    and precious variables in $ERL_TOP/erts/aclocal.m4.
#
erl_build_tool_vars="CC CFLAGS STATIC_CFLAGS CFLAG_RUNTIME_LIBRARY_PATH CPP CPPFLAGS CXX CXXFLAGS LD LDFLAGS LIBS DED_LD DED_LDFLAGS DED_LD_FLAG_RUNTIME_LIBRARY_PATH LFS_CFLAGS LFS_LDFLAGS LFS_LIBS RANLIB AR GETCONF"
