#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
#

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	ct_test_support \
	ct_test_support_eh \
	ct_userconfig_callback \
	telnet_server \
	ct_smoke_test_SUITE \
	ct_gen_conn_SUITE \
	ct_priv_dir_SUITE \
	ct_event_handler_SUITE \
	ct_config_info_SUITE \
	ct_groups_test_1_SUITE \
	ct_groups_test_2_SUITE \
	ct_group_info_SUITE \
	ct_groups_spec_SUITE \
	ct_sequence_1_SUITE \
	ct_repeat_1_SUITE \
	ct_repeat_testrun_SUITE \
	ct_testspec_1_SUITE \
	ct_testspec_2_SUITE \
	ct_testspec_3_SUITE \
	ct_skip_SUITE \
	ct_error_SUITE \
	ct_test_server_if_1_SUITE \
	ct_config_SUITE \
	ct_master_SUITE \
	ct_misc_1_SUITE \
	ct_hooks_SUITE \
	ct_pre_post_test_io_SUITE \
	ct_netconfc_SUITE \
	ct_basic_html_SUITE \
	ct_auto_compile_SUITE \
	ct_verbosity_SUITE \
	ct_shell_SUITE \
	ct_system_error_SUITE \
	ct_snmp_SUITE \
	ct_group_leader_SUITE \
	ct_cover_SUITE \
	ct_cover_nomerge_SUITE \
	ct_groups_search_SUITE \
	ct_surefire_SUITE \
	ct_telnet_SUITE \
	erl2html2_SUITE \
	test_server_SUITE \
	test_server_test_lib \
	ct_release_test_SUITE \
	ct_log_SUITE \
        ct_SUITE \
	ct_keep_logs_SUITE

ERL_FILES= $(MODULES:%=%.erl)
HRL_FILES= test_server_test_lib.hrl

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
INSTALL_PROGS= $(TARGET_FILES)

EMAKEFILE=Emakefile
COVERFILE=

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/common_test_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ERL_MAKE_FLAGS +=
ERL_COMPILE_FLAGS +=

EBIN = .

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

#.PHONY: make_emakefile

#make_emakefile:
#	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES)\
#	> $(EMAKEFILE)

tests debug opt:
	erl $(ERL_MAKE_FLAGS) -make

clean:
	rm -f $(TARGET_FILES)
#	rm -f $(EMAKEFILE)
	rm -f core

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_tests_spec:
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(COVERFILE) "$(RELSYSDIR)"
	$(INSTALL_DATA) common_test.spec common_test.cover "$(RELSYSDIR)"
	chmod -R u+w "$(RELSYSDIR)"
	@tar cf - *_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)

release_docs_spec:
