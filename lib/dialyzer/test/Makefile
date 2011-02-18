include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	callgraph_tests_SUITE \
	opaque_tests_SUITE \
	options1_tests_SUITE \
	options2_tests_SUITE \
	plt_tests_SUITE \
	r9c_tests_SUITE \
	race_tests_SUITE \
	small_tests_SUITE \
	user_tests_SUITE \
	dialyzer_common\
	file_utils

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))

INSTALL_PROGS= $(TARGET_FILES)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/dialyzer_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ERL_MAKE_FLAGS +=
ERL_COMPILE_FLAGS += -I$(ERL_TOP)/lib/test_server/include

EBIN = .

EMAKEFILE=Emakefile

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
	rm -f $(TARGET_FILES) $(GEN_FILES)
	rm -f core

docs:

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_tests_spec: make_emakefile
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DATA) $(EMAKEFILE) $(ERL_FILES) $(RELSYSDIR)
	$(INSTALL_DATA) dialyzer.spec dialyzer_test_constants.hrl $(RELSYSDIR)
	chmod -f -R u+w $(RELSYSDIR)
	@tar cf - *_SUITE_data | (cd $(RELSYSDIR); tar xf -)

release_docs_spec:
