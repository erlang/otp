#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
#
# Copyright Ericsson AB 2001-2020. All Rights Reserved.
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

TEST_SPEC_FILE = megaco.spec

COVER_SPEC_FILE = megaco.cover

BEHAVIOUR_MODULES = \
	megaco_test_generator

ifeq ($(INCLUDE_PREV3_MODULES),true)
TEST_UTIL_PREV3_MODULES = \
	megaco_test_msg_prev3a_lib \
	megaco_test_msg_prev3b_lib \
	megaco_test_msg_prev3c_lib
else
TEST_UTIL_PREV3_MODULES =
endif

TEST_UTIL_MODULES = \
	$(BEHAVIOUR_MODULES) \
	megaco_codec_test_lib \
	megaco_codec_flex_lib \
	megaco_mess_user_test \
	megaco_mess_otp8212_test \
	megaco_profile \
	megaco_tc_controller \
	megaco_test_command_handler \
	megaco_test_global_sys_monitor \
	megaco_test_sys_monitor \
	megaco_test_generator_lib \
	megaco_test_megaco_generator \
	megaco_test_tcp_generator \
	megaco_test_deliver \
	megaco_test_generic_transport \
	megaco_test_mgc \
	megaco_test_mg \
	megaco_test_msg_v1_lib \
	megaco_test_msg_v2_lib \
	$(TEST_UTIL_PREV3_MODULES) \
	megaco_test_msg_v3_lib \
	megaco_test_lib


ifeq ($(INCLUDE_PREV3_MODULES),true)
SUITE_PREV3_MODULES = \
	megaco_codec_prev3a_SUITE \
	megaco_codec_prev3b_SUITE \
	megaco_codec_prev3c_SUITE
else
SUITE_PREV3_MODULES =
endif

SUITE_MODULES = \
	megaco_actions_SUITE \
	megaco_app_SUITE \
	megaco_binary_term_id_SUITE \
	megaco_call_flow_SUITE \
	megaco_codec_mini_SUITE \
	megaco_codec_v1_SUITE \
	megaco_codec_v2_SUITE \
	$(SUITE_PREV3_MODULES) \
	megaco_codec_v3_SUITE \
	megaco_config_SUITE \
	megaco_digit_map_SUITE \
	megaco_examples_SUITE \
	megaco_flex_SUITE \
	megaco_load_SUITE \
	megaco_mess_SUITE \
	megaco_mib_SUITE \
	megaco_mreq_SUITE \
	megaco_pending_limit_SUITE \
	megaco_sdp_SUITE \
	megaco_segment_SUITE \
	megaco_tcp_SUITE \
	megaco_timer_SUITE \
	megaco_trans_SUITE \
	megaco_udp_SUITE

MODULES = \
	$(TEST_UTIL_MODULES) \
	$(SUITE_MODULES)

INTERNAL_HRL_FILES = \
	megaco_test_lib.hrl



