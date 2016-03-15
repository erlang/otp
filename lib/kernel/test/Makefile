#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
	rpc_SUITE \
	pdict_SUITE \
	bif_SUITE \
	kernel_SUITE \
	application_SUITE \
	myApp \
	topApp \
	topApp2 \
	topApp3 \
	ch \
	ch_sup \
	appinc1 \
	appinc1x \
	appinc2 \
	appinc2top \
	appinc2A \
	appinc2B \
	code_SUITE \
	code_b_test \
	disk_log_SUITE \
	erl_boot_server_SUITE \
	erl_distribution_SUITE \
	erl_distribution_wb_SUITE \
	erl_prim_loader_SUITE \
	error_handler_SUITE \
	error_logger_SUITE \
	error_logger_warn_SUITE \
	file_SUITE \
	file_name_SUITE \
	prim_file_SUITE \
	ram_file_SUITE \
	gen_tcp_api_SUITE \
	gen_tcp_echo_SUITE \
	gen_tcp_misc_SUITE \
	gen_udp_SUITE \
	gen_sctp_SUITE \
	global_SUITE \
	global_group_SUITE \
	heart_SUITE \
	inet_SUITE \
	inet_sockopt_SUITE \
	inet_res_SUITE \
	interactive_shell_SUITE \
	init_SUITE \
	kernel_config_SUITE \
	os_SUITE \
	pg2_SUITE \
	seq_trace_SUITE \
	wrap_log_reader_SUITE \
	cleanup \
	ignore_cores \
	zlib_SUITE \
	loose_node \
	sendfile_SUITE \
	standard_error_SUITE \
	multi_load_SUITE

APP_FILES = \
	appinc.app \
	appinc1.app \
	appinc1x.app \
	appinc2.app \
	appinc2top.app \
	appinc2A.app \
	appinc2B.app \
	myApp.app \
	topApp.app \
	topApp2.app \
	topApp3.app

ERL_FILES= $(MODULES:%=%.erl) code_a_test.erl

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
INSTALL_PROGS= $(TARGET_FILES)

EMAKEFILE=Emakefile
COVERFILE=kernel.cover

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/kernel_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ERL_MAKE_FLAGS +=
ERL_COMPILE_FLAGS +=

EBIN = .

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

make_emakefile:
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) '*_SUITE_make' \
	> $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES) \
	>> $(EMAKEFILE)

tests debug opt: make_emakefile
	erl $(ERL_MAKE_FLAGS) -make

clean:
	rm -f $(EMAKEFILE)
	rm -f $(TARGET_FILES) $(GEN_FILES)
	rm -f core

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_tests_spec: make_emakefile
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(ERL_FILES) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(APP_FILES) "$(RELSYSDIR)"
	$(INSTALL_DATA) kernel.spec kernel_smoke.spec  $(EMAKEFILE)\
		$(COVERFILE) "$(RELSYSDIR)"
	chmod -R u+w "$(RELSYSDIR)"
	@tar cf - *_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)

release_docs_spec:
