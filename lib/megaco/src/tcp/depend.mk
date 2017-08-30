#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

$(EBIN)/megaco_tcp.$(EMULATOR): megaco_tcp.erl \
	megaco_tcp.hrl \
	../app/megaco_internal.hrl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl

$(EBIN)/megaco_tcp_accept.$(EMULATOR): megaco_tcp_accept.erl \
	../app/megaco_internal.hrl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl

$(EBIN)/megaco_tcp_connection.$(EMULATOR): megaco_tcp_connection.erl \
	megaco_tcp.hrl \
	../app/megaco_internal.hrl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl

$(EBIN)/megaco_tcp_sup.$(EMULATOR): megaco_tcp_sup.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl

$(EBIN)/megaco_tcp_connection_sup.$(EMULATOR): megaco_tcp_connection_sup.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl

$(EBIN)/megaco_tcp_accept_sup.$(EMULATOR): megaco_tcp_accept_sup.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl

