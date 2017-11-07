include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	array_SUITE \
	base64_SUITE \
	beam_lib_SUITE \
	binary_module_SUITE \
	binref \
	c_SUITE \
	calendar_SUITE \
	dets_SUITE \
	dict_SUITE \
	dict_test_lib \
	digraph_SUITE \
	digraph_utils_SUITE \
	dummy1_h \
	dummy_h \
	dummy_via \
	edlin_expand_SUITE \
	epp_SUITE \
	erl_anno_SUITE \
	erl_eval_SUITE \
	erl_expand_records_SUITE \
	erl_internal_SUITE \
	erl_lint_SUITE \
	erl_pp_SUITE \
	erl_scan_SUITE \
	error_logger_h_SUITE \
	escript_SUITE \
	ets_SUITE \
	ets_tough_SUITE \
	expand_test \
	expand_test1 \
	unicode_expand \
	ExpandTestCaps \
	ExpandTestCaps1 \
	filelib_SUITE \
	file_sorter_SUITE \
	filename_SUITE \
	fixtable_SUITE \
	format_SUITE \
	gen_event_SUITE \
	gen_fsm_SUITE \
	gen_server_SUITE \
	gen_statem_SUITE \
	id_transform_SUITE \
	io_SUITE \
	io_proto_SUITE \
	lists_SUITE \
	log_mf_h_SUITE \
	math_SUITE \
	ms_transform_SUITE \
	proc_lib_SUITE \
	qlc_SUITE \
	queue_SUITE \
	rand_SUITE \
	random_SUITE \
	re_SUITE \
	run_pcre_tests \
	re_testoutput1_replacement_test \
	re_testoutput1_split_test \
	slave_SUITE \
	sets_SUITE \
	sets_test_lib \
	sofs_SUITE \
	stdlib_SUITE \
	stdlib_bench_SUITE \
	string_SUITE \
	supervisor_1 \
	supervisor_2 \
	supervisor_3 \
	supervisor_deadlock \
	naughty_child \
	shell_SUITE \
	supervisor_SUITE \
	supervisor_bridge_SUITE \
	sys_SUITE \
	sys_sp1 \
	sys_sp2 \
	tar_SUITE \
	timer_SUITE \
	timer_simple_SUITE \
	unicode_SUITE \
	unicode_util_SUITE \
	uri_string_SUITE \
	win32reg_SUITE \
	y2k_SUITE \
	select_SUITE \
	zip_SUITE \
	random_unicode_list \
	random_iolist \
	error_logger_forwarder \
	maps_SUITE

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))

INSTALL_PROGS= $(TARGET_FILES)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/stdlib_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ERL_MAKE_FLAGS +=
ERL_COMPILE_FLAGS += -I$(ERL_TOP)/lib/kernel/include \
		-I$(ERL_TOP)/lib/stdlib/include

EBIN = .

EMAKEFILE=Emakefile
COVERFILE=stdlib.cover

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

make_emakefile:
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES) \
	> $(EMAKEFILE)

tests debug opt: make_emakefile
	erl $(ERL_MAKE_FLAGS) -make

clean:
	rm -f $(EMAKEFILE)
	rm -f $(TARGET_FILES)
	rm -f core

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_tests_spec: make_emakefile
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA) stdlib.spec stdlib_bench.spec $(EMAKEFILE) \
		$(ERL_FILES) $(COVERFILE) "$(RELSYSDIR)"
	chmod -R u+w "$(RELSYSDIR)"
	@tar cf - *_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)

release_docs_spec:
