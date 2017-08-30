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

snmpc_mib_gram.erl: snmpc_mib_gram.yrl

$(EBIN)/snmpc.$(EMULATOR): \
	../../include/snmp_types.hrl \
	snmpc.erl \
	snmpc.hrl

$(EBIN)/snmpc_lib.$(EMULATOR): \
	../../include/snmp_types.hrl \
	snmpc_lib.erl \
	snmpc.hrl

$(EBIN)/snmpc_tok.$(EMULATOR): \
	snmpc_tok.erl

$(EBIN)/snmpc_misc.$(EMULATOR): \
	../../include/snmp_types.hrl \
	snmpc_misc.erl

$(EBIN)/snmpc_mib_to_hrl.$(EMULATOR): \
	../../include/snmp_types.hrl \
	snmpc_mib_to_hrl.erl

$(EBIN)/snmpc_mib_gram.$(EMULATOR): \
	../../include/snmp_types.hrl \
	snmpc_mib_gram.erl

$(BIN)/snmpc: snmpc.src ../../vsn.mk
	$(vsn_verbose)$(PERL) -p -e 's?%VSN%?$(VSN)? ' < $< > $@
	$(V_at)chmod 755 $@
