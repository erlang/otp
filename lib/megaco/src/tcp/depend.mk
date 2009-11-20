#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2007-2009. All Rights Reserved.
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

