#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2001-2011. All Rights Reserved.
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

BEHAVIOUR_MODULES = \
	megaco_edist_compress \
	megaco_encoder \
	megaco_transport

MODULES = \
	$(BEHAVIOUR_MODULES) \
	megaco_config_misc \
	megaco_config \
	megaco_digit_map \
	megaco_erl_dist_encoder \
	megaco_erl_dist_encoder_mc \
	megaco_filter \
	megaco_messenger \
	megaco_messenger_misc \
	megaco_misc_sup \
	megaco_monitor \
	megaco_sdp \
	megaco_sup \
	megaco_stats \
	megaco_timer \
	megaco_trans_sender \
	megaco_trans_sup \
	megaco_user_default 

INTERNAL_HRL_FILES = \
	megaco_message_internal.hrl


