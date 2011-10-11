#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
#
# Copyright Ericsson AB 2010-2011. All Rights Reserved.
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

TEST_SPEC_FILE  = diameter.spec
COVER_SPEC_FILE = diameter.cover

MODULES = \
	diameter_ct \
	diameter_util \
	diameter_enum \
	diameter_codec_SUITE \
	diameter_codec_test \
	diameter_app_SUITE \
	diameter_dict_SUITE \
	diameter_reg_SUITE \
	diameter_sync_SUITE \
	diameter_stats_SUITE \
	diameter_watchdog_SUITE \
	diameter_transport_SUITE \
	diameter_traffic_SUITE \
	diameter_relay_SUITE \
	diameter_tls_SUITE

INTERNAL_HRL_FILES = \
	diameter_ct.hrl
