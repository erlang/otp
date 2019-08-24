#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

