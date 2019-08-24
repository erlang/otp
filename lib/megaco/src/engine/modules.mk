#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2001-2019. All Rights Reserved.
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

BEHAVIOUR_MODULES = \
	megaco_edist_compress \
	megaco_encoder \
	megaco_user \
	megaco_transport

MODULES = \
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


