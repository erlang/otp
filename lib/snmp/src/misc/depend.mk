#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2004-2025. All Rights Reserved.
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

$(EBIN)/snmp_conf.$(EMULATOR): \
	snmp_conf.erl \
	snmp_verbosity.hrl \
	../../include/snmp_types.hrl

$(EBIN)/snmp_config.$(EMULATOR): \
	snmp_config.erl \
	snmp_usm.hrl \
	../../include/snmp_types.hrl

$(EBIN)/snmp_log.$(EMULATOR): \
	snmp_log.erl \
	snmp_verbosity.hrl \
	../../include/snmp_types.hrl

$(EBIN)/snmp_misc.$(EMULATOR): \
	snmp_misc.erl \
	snmp_verbosity.hrl \
	../compile/snmpc_misc.hrl \
	../../include/snmp_types.hrl

$(EBIN)/snmp_note_store.$(EMULATOR): \
	snmp_note_store.erl \
	../misc/snmp_debug.hrl \
	../misc/snmp_verbosity.hrl

$(EBIN)/snmp_pdu.$(EMULATOR): \
	snmp_pdu.erl \
	snmp_verbosity.hrl \
	../../include/snmp_types.hrl

$(EBIN)/snmp_usm.$(EMULATOR): \
	snmp_usm.erl \
	snmp_usm.hrl \
	snmp_verbosity.hrl \
	../../include/snmp_types.hrl

$(EBIN)/snmp_verbosity.$(EMULATOR): \
	snmp_verbosity.erl

# Rules for generated "stuff"

$(EBIN)/snmp_pdus_basic.$(EMULATOR): \
	snmp_pdus_basic.erl

# ASN1_OPTS += +abs
snmp_pdus_basic.erl: \
	snmp_pdus_basic.set.asn \
	snmp_pdus_basic.asn
	$(V_colon)@echo "snmp_pdus_basec:"
	$(asn_verbose)$(ERLC) -bber +noobj $(ASN1_OPTS) snmp_pdus_basic.set.asn

