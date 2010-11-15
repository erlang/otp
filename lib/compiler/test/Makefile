include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	andor_SUITE \
	apply_SUITE \
	beam_validator_SUITE \
	bs_bincomp_SUITE \
	bs_bit_binaries_SUITE \
	bs_construct_SUITE \
	bs_match_SUITE \
	bs_utf_SUITE \
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
	match_SUITE \
	misc_SUITE \
	num_bif_SUITE \
	pmod_SUITE \
	parteval_SUITE \
	receive_SUITE \
	record_SUITE \
	trycatch_SUITE \
	warnings_SUITE \
	test_lib

NO_OPT= \
	andor \
	apply \
	bs_construct \
        bs_match \
	bs_utf \
	core_fold \
	float \
	fun \
	guard \
	lc \
	match \
	misc \
	num_bif \
	receive \
	record \
	trycatch

INLINE= \
	andor \
	apply \
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
	match \
	misc \
	num_bif \
	receive \
	record

CORE_MODULES = \
	bs_shadowed_size_var \
	nested_call_in_case


NO_OPT_MODULES= $(NO_OPT:%=%_no_opt_SUITE)
NO_OPT_ERL_FILES= $(NO_OPT_MODULES:%=%.erl)
POST_OPT_MODULES= $(NO_OPT:%=%_post_opt_SUITE)
POST_OPT_ERL_FILES= $(POST_OPT_MODULES:%=%.erl)
INLINE_MODULES= $(INLINE:%=%_inline_SUITE)
INLINE_ERL_FILES= $(INLINE_MODULES:%=%.erl)

ERL_FILES= $(MODULES:%=%.erl)

CORE_FILES= $(CORE_MODULES:%=%.core)

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
ERL_COMPILE_FLAGS += -I$(ERL_TOP)/lib/test_server/include +clint

EBIN = .

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

make_emakefile: $(NO_OPT_ERL_FILES) $(POST_OPT_ERL_FILES) $(INLINE_ERL_FILES)
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES) \
	> $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile +no_copt +no_postopt $(ERL_COMPILE_FLAGS) \
	-o$(EBIN) $(NO_OPT_MODULES) >> $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile +no_copt $(ERL_COMPILE_FLAGS) \
	-o$(EBIN) $(POST_OPT_MODULES) >> $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile +inline $(ERL_COMPILE_FLAGS) \
	-o$(EBIN) $(INLINE_MODULES) >> $(EMAKEFILE)

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

%_post_opt_SUITE.erl: %_SUITE.erl
	sed -e 's;-module($(basename $<));-module($(basename $@));' $< > $@

%_inline_SUITE.erl: %_SUITE.erl
	sed -e 's;-module($(basename $<));-module($(basename $@));' $< > $@

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_tests_spec: make_emakefile
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DATA) compiler.dynspec compiler.cover \
		$(EMAKEFILE) $(ERL_FILES) $(CORE_FILES) $(RELSYSDIR)
	$(INSTALL_DATA) $(NO_OPT_ERL_FILES) $(POST_OPT_ERL_FILES) \
		$(INLINE_ERL_FILES) $(RELSYSDIR)
	chmod -R u+w $(RELSYSDIR)
	@tar cf - *_SUITE_data | (cd $(RELSYSDIR); tar xf -)

release_docs_spec:
