#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1996-2016. All Rights Reserved.
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
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
	mnesia_bench_SUITE \
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
	mnesia_dbn_meters \
	ext_test

DocExamplesDir := ../doc/src/

DocExampleModules =  \
	company \
	company_o \
	bup

DocExamplesHrl = \
	company.hrl \
	company_o.hrl

ExamplesDir := ../examples/

ExampleModules = \
	mnesia_meter \
	mnesia_tpcb

ERL_FILES= $(MODULES:%=%.erl) $(DocExampleModules:%=$(DocExamplesDir)/%.erl) $(ExampleModules:%=$(ExamplesDir)/%.erl)

HRL_FILES= mnesia_test_lib.hrl $(DocExamplesHrl:%=$(DocExamplesDir)/%)

TARGET_FILES= \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(DocExampleModules:%=$(EBIN)/%.$(EMULATOR)) $(ExampleModules:%=$(EBIN)/%.$(EMULATOR))

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

$(EBIN)/%.beam: $(DocExamplesDir)/%.erl
	$(ERLC) -bbeam $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

$(EBIN)/%.beam: $(ExamplesDir)/%.erl
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
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA) mnesia.spec mnesia_bench.spec mnesia.cover $(ERL_FILES) $(HRL_FILES) "$(RELSYSDIR)"
	$(INSTALL_SCRIPT) mt $(INSTALL_PROGS) "$(RELSYSDIR)"
#	chmod -R u+w "$(RELSYSDIR)"
#	@tar cf - *_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)

release_docs_spec:


