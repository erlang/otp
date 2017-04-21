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

EBIN = .

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	a_SUITE \
	after_SUITE \
	alloc_SUITE \
	async_ports_SUITE \
	beam_SUITE \
	beam_literals_SUITE \
	bif_SUITE \
	big_SUITE \
	binary_SUITE \
	bs_bincomp_SUITE \
	bs_bit_binaries_SUITE \
	bs_construct_SUITE \
	bs_match_bin_SUITE \
	bs_match_int_SUITE \
	bs_match_tail_SUITE \
	bs_match_misc_SUITE \
	bs_utf_SUITE \
	busy_port_SUITE \
	call_trace_SUITE \
	code_SUITE \
	code_parallel_load_SUITE \
	crypto_SUITE \
	ddll_SUITE \
	decode_packet_SUITE \
	dirty_bif_SUITE \
	dirty_nif_SUITE \
	distribution_SUITE \
	driver_SUITE \
	efile_SUITE \
	erts_debug_SUITE \
	estone_SUITE \
	erl_link_SUITE \
	erl_drv_thread_SUITE \
	evil_SUITE \
	exception_SUITE \
	float_SUITE \
	fun_SUITE \
	gc_SUITE \
	guard_SUITE \
	hash_SUITE \
	hibernate_SUITE \
	hipe_SUITE \
	list_bif_SUITE \
	lttng_SUITE \
	map_SUITE \
	match_spec_SUITE \
	module_info_SUITE \
	monitor_SUITE \
	multi_load_SUITE \
	nested_SUITE \
	nif_SUITE \
	node_container_SUITE \
	nofrag_SUITE \
	num_bif_SUITE \
	message_queue_data_SUITE \
	op_SUITE \
	os_signal_SUITE \
	port_SUITE \
	port_bif_SUITE \
	prim_eval_SUITE \
	process_SUITE \
	pseudoknot_SUITE \
	receive_SUITE \
	ref_SUITE \
	register_SUITE \
	mtx_SUITE \
	save_calls_SUITE \
	send_term_SUITE \
	sensitive_SUITE \
	signal_SUITE \
	smoke_test_SUITE \
	statistics_SUITE \
	system_info_SUITE \
	system_profile_SUITE \
	time_SUITE \
	timer_bif_SUITE \
	trace_SUITE \
	trace_bif_SUITE \
	trace_nif_SUITE \
	trace_port_SUITE \
	tuple_SUITE \
	trace_local_SUITE \
	trace_meta_SUITE \
	trace_call_count_SUITE \
	trace_call_time_SUITE \
	tracer_SUITE \
	tracer_test \
	scheduler_SUITE \
	old_scheduler_SUITE \
	port_trace_SUITE \
	unique_SUITE \
	z_SUITE \
	long_timers_test \
	ignore_cores \
	dgawd_handler \
	random_iolist \
	crypto_reference

NO_OPT= bs_bincomp \
	bs_bit_binaries \
	bs_construct \
	bs_match_bin \
	bs_match_int \
	bs_match_tail \
	bs_match_misc \
	bs_utf \
	guard \
	map

NATIVE= hibernate

NO_OPT_MODULES= $(NO_OPT:%=%_no_opt_SUITE)
NO_OPT_ERL_FILES= $(NO_OPT_MODULES:%=%.erl)

NATIVE_MODULES= $(NATIVE:%=%_native_SUITE)
NATIVE_ERL_FILES= $(NATIVE_MODULES:%=%.erl)

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR))

EMAKEFILE=Emakefile

TEST_SPEC_FILES= emulator.spec \
		 emulator.spec.win \
		 emulator_bench.spec \
		 emulator_smoke.spec \
                 emulator_node_container_SUITE.spec

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/emulator_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_MAKE_FLAGS += 
ERL_COMPILE_FLAGS +=

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

make_emakefile: $(NO_OPT_ERL_FILES) $(NATIVE_ERL_FILES)
	# This special rule can be removed when communication with R7B nodes 
	# is no longer supported.
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) +compressed -o$(EBIN) \
	'*_SUITE_make' > $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) +compressed -o$(EBIN) \
	$(MODULES) >> $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile +no_copt +no_postopt $(ERL_COMPILE_FLAGS) \
	-o$(EBIN) $(NO_OPT_MODULES) >> $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile +native $(ERL_COMPILE_FLAGS) \
	-o$(EBIN) $(NATIVE_MODULES) >> $(EMAKEFILE)

tests debug opt: make_emakefile
	erl $(ERL_MAKE_FLAGS) -make

clean:
	rm -f $(EMAKEFILE)
	rm -f $(TARGET_FILES)
	rm -f core *~

docs:

# ----------------------------------------------------
# Special targets
# ----------------------------------------------------

%_no_opt_SUITE.erl: %_SUITE.erl
	sed -e 's;-module($(basename $<));-module($(basename $@));' $< > $@

%_native_SUITE.erl: %_SUITE.erl
	sed -e 's;-module($(basename $<));-module($(basename $@));' $< > $@

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec:

release_tests_spec: make_emakefile
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(EMAKEFILE) $(TEST_SPEC_FILES) \
		$(ERL_FILES) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(NO_OPT_ERL_FILES) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(NATIVE_ERL_FILES) "$(RELSYSDIR)"
	chmod -R u+w "$(RELSYSDIR)"
	tar cf - *_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)

release_docs_spec:

