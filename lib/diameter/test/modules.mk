#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
#
# Copyright Ericsson AB 2010. All Rights Reserved.
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

TEST_SPEC_FILE = diameter.spec

COVER_SPEC_FILE = diameter.cover

BEHAVIOUR_MODULES = 

MODULES = \
	$(BEHAVIOUR_MODULES) \
	diameter_SUITE \
	diameter_app_test \
	diameter_appup_test \
	diameter_compiler_test \
	diameter_config_test \
	diameter_peer_test \
	diameter_reg_test \
	diameter_session_test \
	diameter_stats_test \
	diameter_sync_test \
	diameter_tcp_test \
	diameter_test_lib \
	diameter_test_server


INTERNAL_HRL_FILES = \
	diameter_test_lib.hrl



