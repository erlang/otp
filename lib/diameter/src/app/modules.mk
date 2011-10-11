#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2010-2011. All Rights Reserved.
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

SPEC_FILES = \
	diameter_gen_base_rfc3588.dia \
	diameter_gen_base_accounting.dia \
	diameter_gen_relay.dia

RUNTIME_MODULES = \
	diameter \
	diameter_app \
	diameter_capx \
	diameter_config \
	diameter_codec \
	diameter_dict \
	diameter_lib \
	diameter_misc_sup \
	diameter_peer \
	diameter_peer_fsm \
	diameter_peer_fsm_sup \
	diameter_reg \
	diameter_service \
	diameter_service_sup \
	diameter_session \
	diameter_stats \
	diameter_sup \
	diameter_sync \
	diameter_types \
	diameter_watchdog \
	diameter_watchdog_sup

HELP_MODULES = \
	diameter_callback \
	diameter_exprecs \
	diameter_dbg \
	diameter_info

INTERNAL_HRL_FILES = \
	diameter_internal.hrl \
	diameter_types.hrl

EXTERNAL_HRL_FILES = \
	../../include/diameter.hrl \
	../../include/diameter_gen.hrl

EXAMPLE_FILES = \
	../../examples/GNUmakefile \
	../../examples/peer.erl \
	../../examples/client.erl \
	../../examples/client_cb.erl \
	../../examples/server.erl \
	../../examples/server_cb.erl \
	../../examples/relay.erl \
	../../examples/relay_cb.erl
