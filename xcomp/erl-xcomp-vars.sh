#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2010-2012. All Rights Reserved.
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
# erl_xcomp_vars - Cross configuration variables currently reqognized by
#                  `configure' scripts in OTP.
#
# NOTE:
#    When updating, also update $ERL_TOP/xcomp/{README,erl-xcomp.conf.template},
#    and precious variables in $ERL_TOP/erts/aclocal.m4.
#

erl_xcomp_vars="erl_xcomp_sysroot erl_xcomp_isysroot erl_xcomp_bigendian erl_xcomp_double_middle_endian erl_xcomp_linux_clock_gettime_correction erl_xcomp_linux_nptl erl_xcomp_linux_usable_sigusrx erl_xcomp_linux_usable_sigaltstack erl_xcomp_poll erl_xcomp_kqueue erl_xcomp_putenv_copy erl_xcomp_reliable_fpe erl_xcomp_getaddrinfo erl_xcomp_gethrvtime_procfs_ioctl erl_xcomp_clock_gettime_cpu_time erl_xcomp_after_morecore_hook erl_xcomp_dlsym_brk_wrappers erl_xcomp_posix_memalign erl_xcomp_ose_ldflags_pass1 erl_xcomp_ose_ldflags_pass2 erl_xcomp_ose_OSEROOT erl_xcomp_ose_STRIP erl_xcomp_ose_LM_POST_LINK erl_xcomp_ose_LM_SET_CONF erl_xcomp_ose_LM_GET_CONF erl_xcomp_ose_LM_ELF_SIZE erl_xcomp_ose_LM_LCF erl_xcomp_ose_BEAM_LM_CONF erl_xcomp_ose_EPMD_LM_CONF erl_xcomp_ose_RUN_ERL_LM_CONF erl_xcomp_ose_CONFD erl_xcomp_ose_CRT0_LM"
