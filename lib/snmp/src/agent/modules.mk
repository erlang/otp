#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2004-2019. All Rights Reserved.
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
	snmpa_authentication_service \
	snmpa_discovery_handler \
	snmpa_error_report \
	snmpa_get_mechanism \
	snmpa_mib_storage \
	snmpa_mib_data \
	snmpa_network_interface \
	snmpa_network_interface_filter \
	snmpa_notification_delivery_info_receiver \
	snmpa_notification_filter \
	snmpa_set_mechanism

MIB_MODULES = \
	snmp_community_mib \
	snmp_framework_mib \
	snmp_notification_mib \
	snmp_standard_mib \
	snmp_target_mib \
	snmp_user_based_sm_mib \
	snmp_view_based_acm_mib

# snmpa is "plain" interface module but also defines some agent specific types
# and therefor must be compiled before the modules that use them, including
# the behaviour modules...
# Some of the MIB modules also define types used elsewhere and therefor
# has to be built before the other mods.
# snmpa_mib_data_ttln
MODULES = \
	snmpa \
	$(MIB_MODULES) \
	snmpa_acm \
	snmpa_agent \
	snmpa_agent_sup \
	snmpa_app \
	snmpa_conf \
	snmpa_discovery_handler_default \
	snmpa_error \
	snmpa_error_io \
	snmpa_error_logger \
	snmpa_get \
	snmpa_get_lib \
	snmpa_local_db \
	snmpa_mib_storage_ets \
	snmpa_mib_storage_dets \
	snmpa_mib_storage_mnesia \
	snmpa_mib \
	snmpa_mib_data_tttn \
	snmpa_mib_lib \
	snmpa_misc_sup \
	snmpa_mpd \
	snmpa_net_if \
	snmpa_net_if_filter \
	snmpa_set \
	snmpa_set_lib \
	snmpa_supervisor \
	snmpa_svbl \
	snmpa_symbolic_store \
	snmpa_target_cache \
	snmpa_trap \
	snmpa_usm \
	snmpa_vacm \
	snmp_generic \
	snmp_generic_mnesia \
	snmp_index \
	snmp_shadow_table


INTERNAL_HRL_FILES = \
	snmpa_vacm \
	snmpa_atl  \
	snmpa_internal

EXT_HRL_FILES = 
