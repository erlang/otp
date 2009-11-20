#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2001-2009. All Rights Reserved.
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

megaco_text_parser_v1.erl: \
	megaco_text_parser_v1.yrl \
	megaco_text_parser_v1.hrl
megaco_text_parser_v2.erl: \
	megaco_text_parser_v2.yrl \
	megaco_text_parser_v2.hrl
megaco_text_parser_v3.erl: \
	megaco_text_parser_v3.yrl \
	megaco_text_parser_v3.hrl
megaco_text_parser_prev3a.erl: \
	megaco_text_parser_prev3a.yrl \
	megaco_text_parser_prev3a.hrl
megaco_text_parser_prev3b.erl: \
	megaco_text_parser_prev3b.yrl \
	megaco_text_parser_prev3b.hrl
megaco_text_parser_prev3c.erl: \
	megaco_text_parser_prev3c.yrl \
	megaco_text_parser_prev3c.hrl

megaco_text_mini_parser.erl: \
	megaco_text_mini_parser.yrl \
	megaco_text_mini_parser.hrl

$(EBIN)/megaco_compact_text_encoder.$(EMULATOR): \
	megaco_compact_text_encoder.erl

$(EBIN)/megaco_compact_text_encoder.$(EMULATOR): \
	megaco_compact_text_encoder.erl

$(EBIN)/megaco_compact_text_encoder_v1.$(EMULATOR): \
	megaco_compact_text_encoder_v1.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl \
	megaco_text_tokens.hrl \
	megaco_text_gen_v1.hrl

$(EBIN)/megaco_compact_text_encoder_v2.$(EMULATOR): \
	megaco_compact_text_encoder_v2.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_v2.hrl \
	megaco_text_tokens.hrl \
	megaco_text_gen_v2.hrl

$(EBIN)/megaco_compact_text_encoder_v3.$(EMULATOR): \
	megaco_compact_text_encoder_v3.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_v3.hrl \
	megaco_text_tokens.hrl \
	megaco_text_gen_v3.hrl

$(EBIN)/megaco_compact_text_encoder_prev3a.$(EMULATOR): \
	megaco_compact_text_encoder_prev3a.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_prev3a.hrl \
	megaco_text_tokens.hrl \
	megaco_text_gen_prev3a.hrl

$(EBIN)/megaco_compact_text_encoder_prev3b.$(EMULATOR): \
	megaco_compact_text_encoder_prev3b.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_prev3b.hrl \
	megaco_text_tokens.hrl \
	megaco_text_gen_prev3b.hrl

$(EBIN)/megaco_compact_text_encoder_prev3c.$(EMULATOR): \
	megaco_compact_text_encoder_prev3c.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_prev3c.hrl \
	megaco_text_tokens.hrl \
	megaco_text_gen_prev3c.hrl

$(EBIN)/megaco_pretty_text_encoder.$(EMULATOR): \
	megaco_pretty_text_encoder.erl

$(EBIN)/megaco_pretty_text_encoder_v1.$(EMULATOR): \
	megaco_pretty_text_encoder_v1.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl \
	megaco_text_tokens.hrl \
	megaco_text_gen_v1.hrl

$(EBIN)/megaco_pretty_text_encoder_v2.$(EMULATOR): \
	megaco_pretty_text_encoder_v2.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_v2.hrl \
	megaco_text_tokens.hrl \
	megaco_text_gen_v2.hrl

$(EBIN)/megaco_pretty_text_encoder_v3.$(EMULATOR): \
	megaco_pretty_text_encoder_v3.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_v3.hrl \
	megaco_text_tokens.hrl \
	megaco_text_gen_v3.hrl

$(EBIN)/megaco_pretty_text_encoder_prev3a.$(EMULATOR): \
	megaco_pretty_text_encoder_prev3a.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_prev3a.hrl \
	megaco_text_tokens.hrl \
	megaco_text_gen_prev3a.hrl

$(EBIN)/megaco_pretty_text_encoder_prev3b.$(EMULATOR): \
	megaco_pretty_text_encoder_prev3b.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_prev3b.hrl \
	megaco_text_tokens.hrl \
	megaco_text_gen_prev3b.hrl

$(EBIN)/megaco_pretty_text_encoder_prev3c.$(EMULATOR): \
	megaco_pretty_text_encoder_prev3c.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_prev3c.hrl \
	megaco_text_tokens.hrl \
	megaco_text_gen_prev3c.hrl

$(EBIN)/megaco_text_parser_v1.$(EMULATOR): \
	megaco_text_parser_v1.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl \
	megaco_text_tokens.hrl \
	megaco_text_parser_v1.hrl

$(EBIN)/megaco_text_parser_v2.$(EMULATOR): \
	megaco_text_parser_v2.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_v2.hrl \
	megaco_text_tokens.hrl \
	megaco_text_parser_v2.hrl

$(EBIN)/megaco_text_parser_v3.$(EMULATOR): \
	megaco_text_parser_v3.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_v3.hrl \
	megaco_text_tokens.hrl \
	megaco_text_parser_v3.hrl

$(EBIN)/megaco_text_parser_prev3a.$(EMULATOR): \
	megaco_text_parser_prev3a.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_prev3a.hrl \
	megaco_text_tokens.hrl \
	megaco_text_parser_prev3a.hrl

$(EBIN)/megaco_text_parser_prev3b.$(EMULATOR): \
	megaco_text_parser_prev3b.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_prev3b.hrl \
	megaco_text_tokens.hrl \
	megaco_text_parser_prev3b.hrl

$(EBIN)/megaco_text_parser_prev3c.$(EMULATOR): \
	megaco_text_parser_prev3c.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_prev3c.hrl \
	megaco_text_tokens.hrl \
	megaco_text_parser_prev3c.hrl

$(EBIN)/megaco_text_scanner.$(EMULATOR): megaco_text_scanner.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	../engine/megaco_message_internal.hrl \
	megaco_text_tokens.hrl

$(EBIN)/megaco_text_mini_decoder.$(EMULATOR): \
	megaco_text_mini_decoder.erl \
	../engine/megaco_message_internal.hrl \
	megaco_text_tokens.hrl

