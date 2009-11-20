#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2004-2009. All Rights Reserved.
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

$(EBIN)/snmp_conf.$(EMULATOR): \
	snmp_conf.erl \
	snmp_verbosity.hrl \
	../../include/snmp_types.hrl

$(EBIN)/snmp_config.$(EMULATOR): \
	snmp_config.erl \
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
	snmp_verbosity.hrl \
	../../include/snmp_types.hrl

$(EBIN)/snmp_verbosity.$(EMULATOR): \
	snmp_verbosity.erl


