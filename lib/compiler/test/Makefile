include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	andor_SUITE \
	apply_SUITE \
	beam_block_SUITE \
	beam_validator_SUITE \
	beam_disasm_SUITE \
	beam_except_SUITE \
	beam_jump_SUITE \
	beam_reorder_SUITE \
	beam_ssa_SUITE \
	beam_type_SUITE \
	beam_utils_SUITE \
	bif_SUITE \
	bs_bincomp_SUITE \
	bs_bit_binaries_SUITE \
	bs_construct_SUITE \
	bs_match_SUITE \
	bs_utf_SUITE \
	core_alias_SUITE \
	core_fold_SUITE \
	compile_SUITE \
	compilation_SUITE \
	core_SUITE \
	error_SUITE \
	float_SUITE \
	fun_SUITE \
	guard_SUITE \
	inline_SUITE \
	lc_SUITE \
	map_SUITE \
	match_SUITE \
	misc_SUITE \
	overridden_bif_SUITE \
	receive_SUITE \
	record_SUITE \
	regressions_SUITE \
	trycatch_SUITE \
	warnings_SUITE \
	z_SUITE \
	test_lib

NO_OPT= \
	andor \
	apply \
	beam_block \
	beam_except \
	beam_jump \
	beam_reorder \
	beam_ssa \
	beam_type \
	beam_utils \
	bif \
	bs_construct \
        bs_match \
	bs_utf \
	core_fold \
	float \
	fun \
	guard \
	lc \
	map \
	match \
	misc \
	overridden_bif \
	receive \
	record \
	trycatch

INLINE= \
	andor \
	apply \
	beam_block \
	beam_ssa \
	beam_utils \
	bif \
	bs_bincomp \
	bs_bit_binaries \
	bs_construct \
        bs_match \
	bs_utf \
	core_fold \
	float \
	fun \
	guard \
	lc \
	map \
	match \
	misc \
	overridden_bif \
	receive \
	record

R21= \
        bs_construct \
        bs_match

CORE_MODULES = \
	lfe_andor_SUITE \
	lfe_guard_SUITE

NO_MOD_OPT = $(NO_OPT)

NO_SSA_OPT = $(NO_OPT)

NO_TYPE_OPT = $(NO_OPT)

NO_OPT_MODULES= $(NO_OPT:%=%_no_opt_SUITE)
NO_OPT_ERL_FILES= $(NO_OPT_MODULES:%=%.erl)
POST_OPT_MODULES= $(NO_OPT:%=%_post_opt_SUITE)
POST_OPT_ERL_FILES= $(POST_OPT_MODULES:%=%.erl)
INLINE_MODULES= $(INLINE:%=%_inline_SUITE)
INLINE_ERL_FILES= $(INLINE_MODULES:%=%.erl)
R21_MODULES= $(R21:%=%_r21_SUITE)
R21_ERL_FILES= $(R21_MODULES:%=%.erl)
NO_MOD_OPT_MODULES= $(NO_MOD_OPT:%=%_no_module_opt_SUITE)
NO_MOD_OPT_ERL_FILES= $(NO_MOD_OPT_MODULES:%=%.erl)
NO_SSA_OPT_MODULES= $(NO_SSA_OPT:%=%_no_ssa_opt_SUITE)
NO_SSA_OPT_ERL_FILES= $(NO_SSA_OPT_MODULES:%=%.erl)
NO_TYPE_OPT_MODULES= $(NO_TYPE_OPT:%=%_no_type_opt_SUITE)
NO_TYPE_OPT_ERL_FILES= $(NO_TYPE_OPT_MODULES:%=%.erl)

ERL_FILES= $(MODULES:%=%.erl)
CORE_FILES= $(CORE_MODULES:%=%.core)
ERL_DUMMY_FILES= $(CORE_MODULES:%=%.erl)

##TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
##INSTALL_PROGS= $(TARGET_FILES)

EMAKEFILE=Emakefile

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/compiler_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ERL_MAKE_FLAGS +=
ERL_COMPILE_FLAGS += +clint +clint0 +ssalint

EBIN = .

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

make_emakefile: $(NO_OPT_ERL_FILES) $(POST_OPT_ERL_FILES) $(NO_SSA_OPT_ERL_FILES) \
  $(INLINE_ERL_FILES) $(R21_ERL_FILES) $(NO_MOD_OPT_ERL_FILES) $(NO_TYPE_OPT_ERL_FILES)
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES) \
	> $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile +no_copt +no_postopt \
	+no_ssa_opt +no_recv_opt $(ERL_COMPILE_FLAGS) \
	-o$(EBIN) $(NO_OPT_MODULES) >> $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile +no_share_opt +no_bsm_opt +no_fun_opt \
	+no_ssa_opt +no_recv_opt $(ERL_COMPILE_FLAGS) \
	-o$(EBIN) $(NO_SSA_OPT_MODULES) >> $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile +no_copt $(ERL_COMPILE_FLAGS) \
	-o$(EBIN) $(POST_OPT_MODULES) >> $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile +inline $(ERL_COMPILE_FLAGS) \
	-o$(EBIN) $(INLINE_MODULES) >> $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile +r21 $(ERL_COMPILE_FLAGS) \
	-o$(EBIN) $(R21_MODULES) >> $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile +no_module_opt $(ERL_COMPILE_FLAGS) \
	-o$(EBIN) $(NO_MOD_OPT_MODULES) >> $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile +from_core $(ERL_COMPILE_FLAGS) \
	-o$(EBIN) $(CORE_MODULES) >> $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile +no_type_opt $(ERL_COMPILE_FLAGS) \
	-o$(EBIN) $(NO_TYPE_OPT_MODULES) >> $(EMAKEFILE)

tests debug opt: make_emakefile
	erl $(ERL_MAKE_FLAGS) -make

clean:
	rm -f $(EMAKEFILE)
	rm -f $(TARGET_FILES)
	rm -f core

docs:

# ----------------------------------------------------
# Special targets
# ----------------------------------------------------

%_no_opt_SUITE.erl: %_SUITE.erl
	sed -e 's;-module($(basename $<));-module($(basename $@));' $< > $@

%_no_ssa_opt_SUITE.erl: %_SUITE.erl
	sed -e 's;-module($(basename $<));-module($(basename $@));' $< > $@

%_post_opt_SUITE.erl: %_SUITE.erl
	sed -e 's;-module($(basename $<));-module($(basename $@));' $< > $@

%_inline_SUITE.erl: %_SUITE.erl
	sed -e 's;-module($(basename $<));-module($(basename $@));' $< > $@

%_r21_SUITE.erl: %_SUITE.erl
	sed -e 's;-module($(basename $<));-module($(basename $@));' $< > $@

%_no_module_opt_SUITE.erl: %_SUITE.erl
	sed -e 's;-module($(basename $<));-module($(basename $@));' $< > $@

%_no_type_opt_SUITE.erl: %_SUITE.erl
	sed -e 's;-module($(basename $<));-module($(basename $@));' $< > $@


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_tests_spec: make_emakefile
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA) compiler.spec compiler.cover \
		$(EMAKEFILE) $(ERL_FILES) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(NO_OPT_ERL_FILES) $(POST_OPT_ERL_FILES) \
		$(INLINE_ERL_FILES) $(R21_ERL_FILES) \
		$(NO_MOD_OPT_ERL_FILES) \
		$(NO_SSA_OPT_ERL_FILES) \
	        $(NO_TYPE_OPT_ERL_FILES) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(CORE_FILES) "$(RELSYSDIR)"
	for file in $(ERL_DUMMY_FILES); do \
	    module=`basename $$file .erl`; \
	    echo "-module($$module). %% dummy .erl file" >$$file; \
        done
	$(INSTALL_DATA) $(ERL_DUMMY_FILES) "$(RELSYSDIR)"
	rm $(ERL_DUMMY_FILES)
	chmod -R u+w "$(RELSYSDIR)"
	@tar cf - *_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)

release_docs_spec:
