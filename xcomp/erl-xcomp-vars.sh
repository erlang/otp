#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2010-2012. All Rights Reserved.
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
# erl_xcomp_vars - Cross configuration variables currently reqognized by
#                  `configure' scripts in OTP.
#
# NOTE:
#    When updating, also update $ERL_TOP/xcomp/{README,erl-xcomp.conf.template},
#    and precious variables in $ERL_TOP/erts/aclocal.m4.
#

erl_xcomp_vars="erl_xcomp_sysroot erl_xcomp_isysroot erl_xcomp_bigendian erl_xcomp_double_middle_endian erl_xcomp_linux_clock_gettime_correction erl_xcomp_linux_nptl erl_xcomp_linux_usable_sigusrx erl_xcomp_linux_usable_sigaltstack erl_xcomp_poll erl_xcomp_kqueue erl_xcomp_putenv_copy erl_xcomp_reliable_fpe erl_xcomp_getaddrinfo erl_xcomp_gethrvtime_procfs_ioctl erl_xcomp_clock_gettime_cpu_time erl_xcomp_after_morecore_hook erl_xcomp_dlsym_brk_wrappers erl_xcomp_posix_memalign"
