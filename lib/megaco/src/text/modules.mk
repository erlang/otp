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

MODULES = \
	megaco_compact_text_encoder \
	megaco_compact_text_encoder_v1 \
	megaco_compact_text_encoder_v2 \
	megaco_compact_text_encoder_v3 \
	megaco_compact_text_encoder_prev3a \
	megaco_compact_text_encoder_prev3b \
	megaco_compact_text_encoder_prev3c \
	megaco_pretty_text_encoder \
	megaco_pretty_text_encoder_v1 \
	megaco_pretty_text_encoder_v2 \
	megaco_pretty_text_encoder_v3 \
	megaco_pretty_text_encoder_prev3a \
	megaco_pretty_text_encoder_prev3b \
	megaco_pretty_text_encoder_prev3c \
	megaco_text_mini_decoder \
	megaco_text_scanner 


INTERNAL_HRL_FILES = \
	megaco_text_gen_v1.hrl \
	megaco_text_gen_v2.hrl \
	megaco_text_gen_v3.hrl \
	megaco_text_gen_prev3a.hrl \
	megaco_text_gen_prev3b.hrl \
	megaco_text_gen_prev3c.hrl \
	megaco_text_parser_v1.hrl \
	megaco_text_parser_v2.hrl \
	megaco_text_parser_v3.hrl \
	megaco_text_parser_prev3a.hrl \
	megaco_text_parser_prev3b.hrl \
	megaco_text_parser_prev3c.hrl \
	megaco_text_mini_parser.hrl \
	megaco_text_tokens.hrl 


INTERNAL_YRL_FILES = \
	megaco_text_parser_v1.yrl \
	megaco_text_parser_v2.yrl \
	megaco_text_parser_v3.yrl \
	megaco_text_parser_prev3a.yrl \
	megaco_text_parser_prev3b.yrl \
	megaco_text_parser_prev3c.yrl \
	megaco_text_mini_parser.yrl


