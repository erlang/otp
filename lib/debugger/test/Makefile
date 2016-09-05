#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
	andor_SUITE \
	bs_bincomp_SUITE \
	bs_construct_SUITE \
	bs_match_bin_SUITE \
	bs_match_int_SUITE \
	bs_match_misc_SUITE \
	bs_match_tail_SUITE \
	bs_utf_SUITE \
	bug_SUITE \
	erl_eval_SUITE \
	dbg_ui_SUITE \
	debugger_SUITE \
	int_SUITE \
	int_break_SUITE \
	int_eval_SUITE \
	guard_SUITE \
	exception_SUITE \
	fun_SUITE \
	lc_SUITE \
	line_number_SUITE \
	map_SUITE \
	overridden_bif_SUITE \
	record_SUITE \
	trycatch_SUITE \
	test_lib \
	cleanup

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
INSTALL_PROGS= $(TARGET_FILES)

EMAKEFILE=Emakefile

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/debugger_test

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

release_tests_spec: make_emakefile
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(EMAKEFILE) $(ERL_FILES) "$(RELSYSDIR)"
	$(INSTALL_DATA) debugger.spec debugger.cover "$(RELSYSDIR)"
	chmod -R u+w "$(RELSYSDIR)"
	@tar cf - *_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)

release_docs_spec:
