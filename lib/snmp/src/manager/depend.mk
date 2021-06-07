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

$(EBIN)/snmpm_user.$(EMULATOR): \
	snmpm_user.erl

$(EBIN)/snmpm_network_interface.$(EMULATOR): \
	snmpm_network_interface.erl

$(EBIN)/snmpm.$(EMULATOR): \
	snmpm.erl

$(EBIN)/snmpm_config.$(EMULATOR): \
	snmpm_config.erl \
	../../include/snmp_types.hrl \
	../misc/snmp_verbosity.hrl

$(EBIN)/snmpm_mpd.$(EMULATOR): \
	snmpm_mpd.erl \
	../../include/snmp_types.hrl \
	../../include/SNMP-MPD-MIB.hrl \
	../../include/SNMPv2-TM.hrl \
	../misc/snmp_verbosity.hrl

$(EBIN)/snmpm_misc_sup.$(EMULATOR): \
	snmpm_misc_sup.erl \
	../misc/snmp_debug.hrl

$(EBIN)/snmpm_net_if.$(EMULATOR): \
	../../include/snmp_types.hrl \
	../misc/snmp_debug.hrl \
	../misc/snmp_verbosity.hrl \
	snmpm_net_if.erl \
	snmpm_network_interface.erl

$(EBIN)/snmpm_net_if_mt.$(EMULATOR): \
	snmpm_net_if_mt.erl \
	../../include/snmp_types.hrl \
	../misc/snmp_debug.hrl \
	../misc/snmp_verbosity.hrl \
	snmpm_net_if.erl \
	snmpm_network_interface.erl

$(EBIN)/snmpm_server.$(EMULATOR): \
	../../include/snmp_types.hrl \
	../../include/STANDARD-MIB.hrl \
	../misc/snmp_verbosity.hrl \
	snmpm_server.erl

$(EBIN)/snmpm_server_sup.$(EMULATOR): \
	snmpm_server_sup.erl

$(EBIN)/snmpm_supervisor.$(EMULATOR): \
	snmpm_supervisor.erl

$(EBIN)/snmpm_user_default.$(EMULATOR): \
	snmpm_user.erl \
	snmpm_user_default.erl

$(EBIN)/snmpm_usm.$(EMULATOR): \
	snmpm_usm.erl \
	snmpm_usm.hrl \
	../../include/snmp_types.hrl \
	../../include/SNMP-USER-BASED-SM-MIB.hrl \
	../misc/snmp_verbosity.hrl 


