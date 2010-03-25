#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2008-2010. All Rights Reserved.
#
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
#
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
#
# %CopyrightEnd%
#

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	ct_test_support \
	ct_test_support_eh \
	ct_smoke_test_SUITE \
	ct_event_handler_SUITE \
	ct_groups_test_1_SUITE \
	ct_groups_test_2_SUITE \
	ct_skip_SUITE \
	ct_error_SUITE \
	ct_test_server_if_1_SUITE \
	ct_config_SUITE \
	ct_master_SUITE

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
INSTALL_PROGS= $(TARGET_FILES)

EMAKEFILE=Emakefile
COVERFILE=

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/common_test_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ERL_MAKE_FLAGS += -pa $(ERL_TOP)/lib/test_server/ebin
ERL_COMPILE_FLAGS += -I$(ERL_TOP)/lib/test_server/include

EBIN = .

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

#.PHONY: make_emakefile

#make_emakefile:
#	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) \
#	'*_SUITE_make' > $(EMAKEFILE)
#	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES)\
#	>> $(EMAKEFILE)

tests debug opt:

clean:
	rm -f $(TARGET_FILES)
	rm -f core

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_tests_spec:
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DATA) $(ERL_FILES) $(COVERFILE) $(RELSYSDIR)
	$(INSTALL_PROGRAM) common_test.spec $(RELSYSDIR)
	chmod -f -R u+w $(RELSYSDIR)
	@tar cf - *_SUITE_data | (cd $(RELSYSDIR); tar xf -)

release_docs_spec:
