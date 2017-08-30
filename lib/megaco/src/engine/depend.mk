#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

$(EBIN)/megaco_config_misc.$(EMULATOR): megaco_config_misc.erl

$(EBIN)/megaco_config.$(EMULATOR): megaco_config.erl \
	../../include/megaco.hrl \
	../app/megaco_internal.hrl

$(EBIN)/megaco_digit_map.$(EMULATOR): megaco_digit_map.erl \
	megaco_message_internal.hrl \
	../text/megaco_text_tokens.hrl

$(EBIN)/megaco_encoder.$(EMULATOR): megaco_encoder.erl

$(EBIN)/megaco_edist_compress.$(EMULATOR): megaco_edist_compress.erl

$(EBIN)/megaco_erl_dist_encoder.$(EMULATOR): megaco_erl_dist_encoder.erl \
	megaco_message_internal.hrl 

$(EBIN)/megaco_erl_dist_encoder_mc.$(EMULATOR): megaco_erl_dist_encoder_mc.erl \
	megaco_message_internal.hrl \
	../app/megaco_internal.hrl 

$(EBIN)/megaco_filter.$(EMULATOR): megaco_filter.erl \
	../../include/megaco.hrl \
	../../include/megaco_message_v1.hrl \
	../app/megaco_internal.hrl 

$(EBIN)/megaco_messenger.$(EMULATOR): megaco_messenger.erl \
	../../include/megaco.hrl \
	../app/megaco_internal.hrl \
	megaco_message_internal.hrl 

$(EBIN)/megaco_messenger_misc.$(EMULATOR): megaco_messenger_misc.erl \
	../../include/megaco.hrl \
	../app/megaco_internal.hrl \
	megaco_message_internal.hrl 

$(EBIN)/megaco_misc_sup.$(EMULATOR): megaco_misc_sup.erl 

$(EBIN)/megaco_monitor.$(EMULATOR): megaco_monitor.erl 

$(EBIN)/megaco_sdp.$(EMULATOR): \
	megaco_sdp.erl \
	../../include/megaco_sdp.hrl

$(EBIN)/megaco_stats.$(EMULATOR): megaco_stats.erl 

$(EBIN)/megaco_sup.$(EMULATOR): megaco_sup.erl

$(EBIN)/megaco_timer.$(EMULATOR): \
	megaco_timer.erl \
	../../include/megaco.hrl

$(EBIN)/megaco_trans_sender.$(EMULATOR): megaco_trans_sender.erl 

$(EBIN)/megaco_trans_sup.$(EMULATOR): megaco_trans_sup.erl 

$(EBIN)/megaco_transport.$(EMULATOR): megaco_transport.erl 

$(EBIN)/megaco_user.$(EMULATOR): megaco_user.erl

$(EBIN)/megaco_user_default.$(EMULATOR): megaco_user_default.erl \
	../../include/megaco.hrl \
	../../include/megaco_message_v1.hrl

