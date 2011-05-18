#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1996-2011. All Rights Reserved.
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
	mt \
	mnesia_SUITE \
	mnesia_test_lib \
	mnesia_install_test \
	mnesia_registry_test \
	mnesia_config_test \
	mnesia_frag_test \
	mnesia_inconsistent_database_test \
	mnesia_config_backup \
	mnesia_config_event \
	mnesia_examples_test \
	mnesia_nice_coverage_test \
	mnesia_evil_coverage_test \
	mnesia_evil_backup \
	mnesia_trans_access_test \
	mnesia_dirty_access_test \
	mnesia_atomicity_test \
	mnesia_consistency_test \
	mnesia_majority_test \
	mnesia_isolation_test \
	mnesia_durability_test \
	mnesia_recovery_test \
	mnesia_qlc_test \
	mnesia_schema_recovery_test \
	mnesia_measure_test \
	mnesia_cost \
	mnesia_dbn_meters 

MnesiaExamplesDir := ../examples

ExampleModules =  \
	company \
	company_o \
	bup \
	mnesia_meter \
	mnesia_tpcb       
ExamplesHrl = \
	company.hrl \
	company_o.hrl

ERL_FILES= $(MODULES:%=%.erl) $(ExampleModules:%=$(MnesiaExamplesDir)/%.erl)

HRL_FILES= mnesia_test_lib.hrl $(ExamplesHrl:%=$(MnesiaExamplesDir)/%)

TARGET_FILES= \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(ExampleModules:%=$(EBIN)/%.$(EMULATOR)) 

INSTALL_PROGS= $(TARGET_FILES)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/mnesia_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
#ERL_COMPILE_FLAGS +=

EBIN = .

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt: $(TARGET_FILES)

$(EBIN)/%.beam: $(MnesiaExamplesDir)/%.erl
	$(ERLC) -bbeam $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

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
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DATA) mnesia.spec mnesia.cover $(ERL_FILES) $(HRL_FILES) $(RELSYSDIR)
	$(INSTALL_SCRIPT) mt $(INSTALL_PROGS) $(RELSYSDIR)
#	chmod -R u+w $(RELSYSDIR)
#	@tar cf - *_SUITE_data | (cd $(RELSYSDIR); tar xf -)

release_docs_spec:


