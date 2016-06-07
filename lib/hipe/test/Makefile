include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	hipe_SUITE \
	opt_verify_SUITE

# .erl files for these modules are automatically generated
GEN_MODULES= \
	basic_SUITE \
	bs_SUITE \
	maps_SUITE \
	sanity_SUITE

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
INSTALL_PROGS= $(TARGET_FILES)

# ----------------------------------------------------
# Files
# ----------------------------------------------------
EMAKEFILE = Emakefile
AUXILIARY_FILES = hipe.spec hipe_testsuite_driver.erl $(EMAKEFILE)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/hipe_test

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
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES) \
	> $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) $(GEN_MODULES) \
	>> $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) '*_SUITE_make' \
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

release_docs_spec:

release_tests_spec: make_emakefile
	$(INSTALL_DIR) "$(RELSYSDIR)"
	chmod -R u+w "$(RELSYSDIR)"
	$(INSTALL_DATA) $(AUXILIARY_FILES) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(ERL_FILES) "$(RELSYSDIR)"
	@tar cf - *_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)
	cd "$(RELSYSDIR)";\
	erlc hipe_testsuite_driver.erl;\
	erl -noshell -run hipe_testsuite_driver create_all_suites $(GEN_MODULES) -s erlang halt
