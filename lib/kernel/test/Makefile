#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1997-2024. All Rights Reserved.
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

SOCKET_MODULES = \
	gen_tcp_socket_SUITE \
	socket_test_lib \
	socket_test_logger \
	socket_test_evaluator \
	socket_test_profile \
	socket_test_ttest_lib \
	socket_test_ttest_tcp_gen \
	socket_test_ttest_tcp_gs \
	socket_test_ttest_tcp_socket \
	socket_test_ttest_tcp_client \
	socket_test_ttest_tcp_client_gen \
	socket_test_ttest_tcp_client_gs \
	socket_test_ttest_tcp_client_socket \
	socket_test_ttest_tcp_server \
	socket_test_ttest_tcp_server_gen \
	socket_test_ttest_tcp_server_gs \
	socket_test_ttest_tcp_server_socket \
	socket_SUITE

MODULES= \
	kernel_test_lib \
	kernel_test_global_sys_monitor \
	kernel_test_sys_monitor \
	erpc_SUITE \
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
	code_coverage_SUITE \
	disk_log_SUITE \
	erl_boot_server_SUITE \
	erl_distribution_SUITE \
	erl_distribution_wb_SUITE \
	erl_prim_loader_SUITE \
	erl_uds_dist \
	error_handler_SUITE \
	error_logger_SUITE \
	error_logger_warn_SUITE \
	file_SUITE \
	file_name_SUITE \
	prim_file_SUITE \
	ram_file_SUITE \
	gen_tcp_api_SUITE \
	gen_tcp_dist \
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
	logger_SUITE \
	logger_disk_log_h_SUITE \
	logger_env_var_SUITE \
	logger_filters_SUITE \
	logger_formatter_SUITE \
	logger_legacy_SUITE \
	logger_olp_SUITE \
	logger_proxy_SUITE \
	logger_simple_h_SUITE \
	logger_std_h_SUITE \
	logger_stress_SUITE \
	logger_test_lib \
	net_SUITE \
	os_SUITE \
	pg_SUITE \
	rtnode \
	seq_trace_SUITE \
	$(SOCKET_MODULES) \
	wrap_log_reader_SUITE \
	cleanup \
	ignore_cores \
	zlib_SUITE \
	sendfile_SUITE \
	standard_error_SUITE \
	multi_load_SUITE \
	zzz_SUITE

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

SASL_MODULES= otp_vsns
STDLIB_MODULES= error_info_lib

ERL_FILES= $(MODULES:%=%.erl) code_a_test.erl \
	$(SASL_MODULES:%=$(ERL_TOP)/lib/sasl/test/%.erl) \
	$(STDLIB_MODULES:%=$(ERL_TOP)/lib/stdlib/test/%.erl)

HRL_FILES= \
	kernel_test_lib.hrl \
	socket_test_evaluator.hrl \
	socket_test_ttest.hrl \
	socket_test_ttest_client.hrl

EXTRA_FILES= $(ERL_TOP)/otp_versions.table

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
INSTALL_PROGS= $(TARGET_FILES)

EMAKEFILE=Emakefile
COVERFILE=kernel.cover logger.cover tty.cover

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/kernel_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ERL_MAKE_FLAGS +=
ERL_COMPILE_FLAGS +=
ERL_COMPILE_FLAGS := $(filter-out +deterministic,$(ERL_COMPILE_FLAGS))

EBIN = .

TARGETS = $(MODULES:%=$(EBIN)/%.$(EMULATOR))
SOCKET_TARGETS = $(SOCKET_MODULES:%=$(EBIN)/%.$(EMULATOR))


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

gen_tcp_dist.erl: ../examples/gen_tcp_dist/src/gen_tcp_dist.erl
	cp $< $@

erl_uds_dist.erl: ../examples/erl_uds_dist/src/erl_uds_dist.erl
	cp $< $@

make_emakefile: $(ERL_FILES)
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) '*_SUITE_make' \
	> $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) \
	  $(MODULES) $(SASL_MODULES) $(STDLIB_MODULES) \
	>> $(EMAKEFILE)

tests $(TYPES): make_emakefile
	erl $(ERL_MAKE_FLAGS) -make

clean:
	rm -f $(EMAKEFILE)
	rm -f $(TARGET_FILES) $(GEN_FILES)
	rm -f core

docs:

targets: $(TARGETS)
socket_targets: $(SOCKET_TARGETS)


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_tests_spec: make_emakefile
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(APP_FILES) $(EXTRA_FILES) "$(RELSYSDIR)"
	$(INSTALL_DATA) \
		kernel.spec kernel_smoke.spec kernel_bench.spec kernel_gh.spec logger.spec \
		$(EMAKEFILE) $(COVERFILE) "$(RELSYSDIR)"
	chmod -R u+w "$(RELSYSDIR)"
	@tar cf - *_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)
	$(INSTALL_DIR) "$(RELSYSDIR)/kernel_SUITE_data"
	$(INSTALL_DATA) $(ERL_TOP)/make/otp_version_tickets "$(RELSYSDIR)/kernel_SUITE_data"

release_docs_spec:
