
# %CopyrightBegin%
#
# Copyright Ericsson AB 2010-2019. All Rights Reserved.
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

# Runtime dictionary files in ./dict. Modules generated from these are
# included in the app file.
DICTS = \
	base_rfc3588 \
	base_rfc6733 \
	base_accounting \
	acct_rfc6733 \
	doic_rfc7683 \
	relay

# The yecc grammar for the dictionary parser.
DICT_YRL = \
	diameter_dict_parser

# Handwritten (runtime) modules included in the app file.
RT_MODULES = \
	base/diameter \
	base/diameter_app \
	base/diameter_callback \
	base/diameter_capx \
	base/diameter_config \
	base/diameter_config_sup \
	base/diameter_codec \
	base/diameter_dist \
	base/diameter_gen \
	base/diameter_lib \
	base/diameter_misc_sup \
	base/diameter_peer \
	base/diameter_peer_fsm \
	base/diameter_peer_fsm_sup \
	base/diameter_reg \
	base/diameter_service \
	base/diameter_service_sup \
	base/diameter_session \
	base/diameter_stats \
	base/diameter_sup \
	base/diameter_sync \
	base/diameter_traffic \
	base/diameter_types \
	base/diameter_watchdog \
	base/diameter_watchdog_sup \
	transport/diameter_etcp \
	transport/diameter_etcp_sup \
	transport/diameter_tcp \
	transport/diameter_tcp_sup \
	transport/diameter_sctp \
	transport/diameter_sctp_sup \
	transport/diameter_transport \
	transport/diameter_transport_sup

# Handwritten compiler modules not included in the app file.
CT_MODULES = \
	compiler/diameter_codegen \
	compiler/diameter_exprecs \
	compiler/diameter_dict_scanner \
	compiler/diameter_dict_util \
	compiler/diameter_make

# Info/debug modules, also not included in the app file.
INFO_MODULES = \
	info/diameter_dbg \
	info/diameter_info

# Released hrl files in ../include intended for public consumption.
EXTERNAL_HRLS = \
	diameter.hrl \
	diameter_gen.hrl

# Released hrl files intended for private use.
INTERNAL_HRLS = \
	base/diameter_internal.hrl \
	compiler/diameter_forms.hrl \
	compiler/diameter_vsn.hrl

# Released files relative to ../bin.
BINS = \
	diameterc

# Released files relative to ../examples.
EXAMPLES = \
	code/GNUmakefile \
	code/node.erl \
	code/client.erl \
	code/client_cb.erl \
	code/server.erl \
	code/server_cb.erl \
	code/relay.erl \
	code/relay_cb.erl \
	dict/rfc4004_mip.dia \
	dict/rfc4005_nas.dia \
	dict/rfc4006_cc.dia \
	dict/rfc4072_eap.dia \
	dict/rfc4590_digest.dia \
	dict/rfc4740_sip.dia

# Registered server names.

REGISTERED = \
	diameter_config \
	diameter_peer \
	diameter_reg \
	diameter_stats \
	diameter_sync
