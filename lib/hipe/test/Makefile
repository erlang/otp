include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Files
# ----------------------------------------------------

AUXILIARY_FILES = hipe.spec hipe_testsuite_driver.erl

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------

RELSYSDIR = $(RELEASE_PATH)/hipe_test

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------

include $(ERL_TOP)/make/otp_release_targets.mk

release_tests_spec:
	$(INSTALL_DIR) "$(RELSYSDIR)"
	chmod -R u+w "$(RELSYSDIR)"
	$(INSTALL_DATA) $(AUXILIARY_FILES) "$(RELSYSDIR)"
	@tar cf - *_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)
	cd "$(RELSYSDIR)";\
	erlc hipe_testsuite_driver.erl;\
	erl -noshell -run hipe_testsuite_driver create_all_suites -s erlang halt
