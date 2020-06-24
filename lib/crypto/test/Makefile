include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	crypto_bench_SUITE \
	crypto_SUITE \
	crypto_property_test_SUITE \
	engine_SUITE

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))

SPEC_FILES = \
	crypto.spec \
	crypto_bench.spec

COVER_FILE = crypto.cover

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/crypto_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

EBIN = .

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
tests debug opt: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_tests_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(SPEC_FILES) $(COVER_FILE) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(ERL_FILES)    "$(RELSYSDIR)"
#	$(INSTALL_DATA) $(HRL_FILES)    "$(RELSYSDIR)"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)"
	chmod -R u+w "$(RELSYSDIR)"
	@tar cf - *_SUITE_data property_test | (cd "$(RELSYSDIR)"; tar xf -)

release_docs_spec:

