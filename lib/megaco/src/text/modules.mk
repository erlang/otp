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


