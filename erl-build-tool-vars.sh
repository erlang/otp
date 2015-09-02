#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2010. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
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
