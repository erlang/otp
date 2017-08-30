#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

