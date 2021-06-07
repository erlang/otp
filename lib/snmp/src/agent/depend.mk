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

$(EBIN)/snmpa_authentication_service.$(EMULATOR): \
	snmpa_authentication_service.erl

$(EBIN)/snmpa_error_report.$(EMULATOR): \
	snmpa_error_report.erl

$(EBIN)/snmpa_get_mechanism.$(EMULATOR): \
	snmpa_get_mechanism.erl

$(EBIN)/snmpa_network_interface.$(EMULATOR): \
	snmpa_network_interface.erl

$(EBIN)/snmpa_network_interface_filter.$(EMULATOR): \
	snmpa_network_interface_filter.erl

$(EBIN)/snmpa_notification_filter.$(EMULATOR): \
	snmpa_notification_filter.erl

$(EBIN)/snmpa_notification_delivery_info_receiver.$(EMULATOR): \
	snmpa_notification_delivery_info_receiver.erl

$(EBIN)/snmpa_set_mechanism.$(EMULATOR): \
	snmpa_set_mechanism.erl

$(EBIN)/snmpa.$(EMULATOR): \
	snmpa.erl

$(EBIN)/snmpa_acm.$(EMULATOR): \
	snmpa_authentication_service.erl \
	snmpa_acm.erl \
	../misc/snmp_verbosity.hrl \
	../../include/snmp_types.hrl \
	../../include/STANDARD-MIB.hrl \
	../../include/SNMP-FRAMEWORK-MIB.hrl \
	../../include/SNMPv2-TM.hrl

$(EBIN)/snmpa_agent.$(EMULATOR): \
	snmpa_agent.erl \
	snmpa_internal.hrl \
	../misc/snmp_debug.hrl \
	../misc/snmp_verbosity.hrl \
	../../include/snmp_types.hrl

$(EBIN)/snmpa_agent_sup.$(EMULATOR): \
	snmpa_agent_sup.erl \
	../misc/snmp_debug.hrl

$(EBIN)/snmpa_app.$(EMULATOR): \
	snmpa_app.erl \
	../misc/snmp_debug.hrl

$(EBIN)/snmpa_error.$(EMULATOR): \
	snmpa_error_report.erl \
	snmpa_error.erl

$(EBIN)/snmpa_error_io.$(EMULATOR): \
	snmpa_error_report.erl \
	snmpa_error_io.erl

$(EBIN)/snmpa_error_logger.$(EMULATOR): \
	snmpa_error_report.erl \
	snmpa_error_logger.erl

$(EBIN)/snmpa_set.$(EMULATOR): \
	snmpa_set_mechanism.erl \
	snmpa_set.erl \
	../misc/snmp_verbosity.hrl

$(EBIN)/snmpa_get.$(EMULATOR): \
	snmpa_get_mechanism.erl \
	snmpa_get.erl \
	../misc/snmp_verbosity.hrl

$(EBIN)/snmpa_get_lib.$(EMULATOR): \
	snmpa_get_lib.erl \
	../misc/snmp_verbosity.hrl

$(EBIN)/snmpa_local_db.$(EMULATOR): \
	snmpa_local_db.erl \
	../misc/snmp_debug.hrl \
	../misc/snmp_verbosity.hrl \
	../../include/snmp_types.hrl \
	../../include/STANDARD-MIB.hrl

$(EBIN)/snmpa_mib_storage.$(EMULATOR): \
	snmpa_mib_storage.erl

$(EBIN)/snmpa_mib_storage_ets.$(EMULATOR): \
	snmpa_mib_storage_ets.erl

$(EBIN)/snmpa_mib_storage_dets.$(EMULATOR): \
	snmpa_mib_storage_dets.erl

$(EBIN)/snmpa_mib_storage_mnesia.$(EMULATOR): \
	snmpa_mib_storage_mnesia.erl

$(EBIN)/snmpa_mib.$(EMULATOR): \
	snmpa_mib.erl \
	../misc/snmp_debug.hrl \
	../misc/snmp_verbosity.hrl \
	../../include/snmp_types.hrl

$(EBIN)/snmpa_mib_data.$(EMULATOR): \
	snmpa_mib_data.erl \
	../../include/snmp_types.hrl

$(EBIN)/snmpa_mib_data_tttn.$(EMULATOR): \
	snmpa_mib_data_tttn.erl \
	../misc/snmp_debug.hrl \
	../misc/snmp_verbosity.hrl \
	../../include/snmp_types.hrl

$(EBIN)/snmpa_mib_lib.$(EMULATOR): \
	snmpa_mib_lib.erl \
	../misc/snmp_verbosity.hrl \
	../../include/snmp_types.hrl

$(EBIN)/snmpa_misc_sup.$(EMULATOR): \
	snmpa_misc_sup.erl \
	../misc/snmp_debug.hrl

$(EBIN)/snmpa_mpd.$(EMULATOR): \
	snmpa_mpd.erl \
	../misc/snmp_verbosity.hrl \
	../../include/snmp_types.hrl \
	../../include/SNMP-MPD-MIB.hrl \
	../../include/SNMPv2-TM.hrl

$(EBIN)/snmpa_net_if.$(EMULATOR): \
	snmpa_net_if.erl \
	../misc/snmp_debug.hrl \
	../misc/snmp_verbosity.hrl \
	../../include/snmp_types.hrl

$(EBIN)/snmpa_net_if_filter.$(EMULATOR): \
	snmpa_net_if_filter.erl

$(EBIN)/snmpa_set.$(EMULATOR): \
	snmpa_set_mechanism.erl \
	snmpa_set.erl \
	../misc/snmp_verbosity.hrl

$(EBIN)/snmpa_set_lib.$(EMULATOR): \
	snmpa_set_lib.erl \
	../misc/snmp_verbosity.hrl \
	../../include/snmp_types.hrl

$(EBIN)/snmpa_supervisor.$(EMULATOR): \
	snmpa_supervisor.erl \
	snmpa_internal.hrl \
	../misc/snmp_debug.hrl \
	../misc/snmp_verbosity.hrl

$(EBIN)/snmpa_svbl.$(EMULATOR): \
	snmpa_svbl.erl \
	../../include/snmp_types.hrl

$(EBIN)/snmpa_symbolic_store.$(EMULATOR): \
	snmpa_symbolic_store.erl \
	../misc/snmp_debug.hrl \
	../misc/snmp_verbosity.hrl \
	../../include/snmp_types.hrl

$(EBIN)/snmpa_target_cache.$(EMULATOR): \
	snmpa_target_cache.erl \
	../misc/snmp_verbosity.hrl \
	../misc/snmp_debug.hrl \
	snmpa_internal.hrl \
	../app/snmp_internal.hrl

$(EBIN)/snmpa_trap.$(EMULATOR): \
	snmpa_trap.erl \
	../misc/snmp_verbosity.hrl \
	../../include/snmp_types.hrl \
	../../include/SNMPv2-MIB.hrl \
	../../include/SNMP-FRAMEWORK-MIB.hrl

$(EBIN)/snmpa_usm.$(EMULATOR): \
	snmpa_usm.erl \
	../misc/snmp_verbosity.hrl \
	../../include/snmp_types.hrl \
	../../include/SNMP-USER-BASED-SM-MIB.hrl \
	../../include/SNMPv2-TC.hrl

$(EBIN)/snmpa_vacm.$(EMULATOR): \
	snmpa_vacm.erl \
	snmpa_vacm.hrl \
	../misc/snmp_verbosity.hrl \
	../../include/snmp_types.hrl \
	../../include/SNMP-VIEW-BASED-ACM-MIB.hrl \
	../../include/SNMPv2-TC.hrl \
	../../include/SNMP-FRAMEWORK-MIB.hrl

$(EBIN)/snmp_community_mib.$(EMULATOR): \
	snmp_community_mib.erl \
	../misc/snmp_verbosity.hrl \
	../../include/SNMP-COMMUNITY-MIB.hrl \
	../../include/SNMP-TARGET-MIB.hrl \
	../../include/SNMPv2-TC.hrl

$(EBIN)/snmp_framework_mib.$(EMULATOR): \
	snmp_framework_mib.erl \
	../misc/snmp_verbosity.hrl \
	../../include/snmp_types.hrl \
	../../include/STANDARD-MIB.hrl

$(EBIN)/snmp_generic.$(EMULATOR): \
	snmp_generic.erl \
	../misc/snmp_verbosity.hrl \
	../../include/snmp_types.hrl \
	../../include/STANDARD-MIB.hrl

$(EBIN)/snmp_generic_mnesia.$(EMULATOR): \
	snmp_generic_mnesia.erl \
	../../include/snmp_types.hrl \
	../../include/STANDARD-MIB.hrl

$(EBIN)/snmp_index.$(EMULATOR): \
	snmp_index.erl

$(EBIN)/snmp_notification_mib.$(EMULATOR): \
	snmp_notification_mib.erl \
	../misc/snmp_verbosity.hrl \
	../../include/snmp_tables.hrl \
	../../include/SNMP-NOTIFICATION-MIB.hrl \
	../../include/SNMPv2-TC.hrl

$(EBIN)/snmp_shadow_table.$(EMULATOR): \
	snmp_shadow_table.erl

$(EBIN)/snmp_standard_mib.$(EMULATOR): \
	snmp_standard_mib.erl \
	../misc/snmp_verbosity.hrl \
	../../include/snmp_types.hrl \
	../../include/STANDARD-MIB.hrl

$(EBIN)/snmp_target_mib.$(EMULATOR): \
	snmp_target_mib.erl \
	../misc/snmp_verbosity.hrl \
	../../include/snmp_types.hrl \
	../../include/snmp_tables.hrl \
	../../include/SNMP-TARGET-MIB.hrl \
	../../include/SNMPv2-TC.hrl \
	../../include/SNMPv2-TM.hrl

$(EBIN)/snmp_user_based_sm_mib.$(EMULATOR): \
	snmp_user_based_sm_mib.erl \
	../misc/snmp_verbosity.hrl \
	../../include/snmp_types.hrl \
	../../include/SNMP-USER-BASED-SM-MIB.hrl \
	../../include/SNMPv2-TC.hrl

$(EBIN)/snmp_view_based_acm_mib.$(EMULATOR): \
	snmp_view_based_acm_mib.erl \
	snmpa_vacm.hrl \
	../misc/snmp_verbosity.hrl \
	../../include/snmp_types.hrl \
	../../include/SNMP-VIEW-BASED-ACM-MIB.hrl \
	../../include/SNMPv2-TC.hrl


