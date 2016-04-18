#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

MODULES =  \
	dyntrace_SUITE \
	dyntrace_lttng_SUITE \
	runtime_tools_SUITE \
	system_information_SUITE \
	dbg_SUITE \
	erts_alloc_config_SUITE \
	msacc_SUITE

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
INSTALL_PROGS= $(TARGET_FILES)

EMAKEFILE=Emakefile

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/runtime_tools_test

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
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES)\
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
	$(INSTALL_DATA) runtime_tools.spec runtime_tools.cover $(ERL_FILES) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(EMAKEFILE) runtime_tools.cover "$(RELSYSDIR)"
	chmod -R u+w "$(RELSYSDIR)"
	@tar cf - *_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)

release_docs_spec:
