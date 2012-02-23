#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2004-2012. All Rights Reserved.
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

BEHAVIOUR_MODULES = \
	snmpm_user \
	snmpm_user_old \
	snmpm_network_interface \
	snmpm_network_interface_filter

MODULES = \
	$(BEHAVIOUR_MODULES) \
	snmpm \
	snmpm_conf \
	snmpm_config \
	snmpm_mpd \
	snmpm_misc_sup \
	snmpm_net_if \
	snmpm_net_if_mt \
	snmpm_net_if_filter \
	snmpm_server \
	snmpm_server_sup \
	snmpm_supervisor \
	snmpm_user_default \
	snmpm_usm

INTERNAL_HRL_FILES = \
	snmpm_usm \
	snmpm_atl \
	snmpm_internal

