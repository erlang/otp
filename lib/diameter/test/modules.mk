
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

TEST_SPEC_FILE  = diameter.spec
COVER_SPEC_FILE = diameter.cover

MODULES = \
	diameter_ct \
	diameter_enum \
	diameter_util \
	diameter_3xxx_SUITE \
	diameter_app_SUITE \
	diameter_capx_SUITE \
	diameter_codec_SUITE \
	diameter_codec_test \
	diameter_config_SUITE \
	diameter_compiler_SUITE \
	diameter_dist_SUITE \
	diameter_distribution_SUITE \
	diameter_dpr_SUITE \
	diameter_event_SUITE \
	diameter_examples_SUITE \
	diameter_failover_SUITE \
	diameter_gen_sctp_SUITE \
	diameter_gen_tcp_SUITE \
	diameter_length_SUITE \
	diameter_pool_SUITE \
	diameter_reg_SUITE \
	diameter_relay_SUITE \
	diameter_stats_SUITE \
	diameter_sync_SUITE \
	diameter_tls_SUITE \
	diameter_traffic_SUITE \
	diameter_transport_SUITE \
	diameter_watchdog_SUITE

HRL_FILES = \
	diameter_ct.hrl

DATA = \
	diameter_codec_SUITE_data/avps.dia \
	diameter_codec_SUITE_data/send.dia \
	diameter_codec_SUITE_data/recv.dia \
	diameter_codec_SUITE_data/diameter_test_unknown.erl
