#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1997-2017. All Rights Reserved.
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

MODULES =  \
	cover_SUITE \
	eprof_SUITE \
	emacs_SUITE \
	emem_SUITE \
	fprof_SUITE \
	cprof_SUITE \
	instrument_SUITE \
	lcnt_SUITE \
	make_SUITE \
	tools_SUITE \
	xref_SUITE \
	prof_bench_SUITE \
	ignore_cores

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
INSTALL_PROGS= $(TARGET_FILES)

EMAKEFILE=Emakefile

SPEC_FILES= tools.spec tools_bench.spec
COVER_FILE = tools.cover

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/tools_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_MAKE_FLAGS +=
ERL_COMPILE_FLAGS +=

EBIN = .

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
.PHONY: make_emakefile

make_emakefile:
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES)\
	> $(EMAKEFILE)

tests debug opt: make_emakefile
	erl $(ERL_MAKE_FLAGS) -make

clean:
	rm -f $(EMAKEFILE)
	rm -f $(TARGET_FILES)
	rm -f core *~

docs:


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_tests_spec: make_emakefile
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(SPEC_FILES) $(COVER_FILE) $(EMAKEFILE) \
		$(ERL_FILES) "$(RELSYSDIR)"
	chmod -R u+w "$(RELSYSDIR)"
	@tar cf - *_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)

release_docs_spec:


