# 
# %CopyrightBegin%
#
# Copyright Ericsson AB 2010-2011. All Rights Reserved.
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

ifeq ($(ERL_TOP),)
TOP = $(DIAMETER_TOP)
else
TOP = $(ERL_TOP)
DIAMETER_TOP = $(TOP)/lib/diameter
endif

include $(TOP)/make/target.mk
include $(TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------

include ../vsn.mk

VSN = $(DIAMETER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------

RELSYSDIR = $(RELEASE_PATH)/diameter_test

ifeq ($(findstring win32,$(TARGET)),win32)
MAKEFILE_SRC = Makefile.win32.src
else
MAKEFILE_SRC = Makefile.src
endif

ifeq ($(TT_DIR),)
TT_DIR = /tmp
endif

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

include modules.mk

EBIN = .

HRL_FILES = $(INTERNAL_HRL_FILES)
ERL_FILES = $(MODULES:%=%.erl)

SOURCE = $(HRL_FILES) $(ERL_FILES) 
TARGET_FILES = $(MODULES:%=%.$(EMULATOR))

SUITE_MODULES = $(filter diameter_%_SUITE, $(MODULES))
SUITES = $(SUITE_MODULES:diameter_%_SUITE=%)

EMAKEFILE  = Emakefile
MAKE_EMAKE = $(wildcard $(TOP)/make/make_emakefile)

ifeq ($(MAKE_EMAKE),)
BUILDTARGET   = $(TARGET_FILES)
RELTEST_FILES = $(TEST_SPEC_FILE) $(COVER_SPEC_FILE) $(SOURCE)
else
BUILDTARGET   = emakebuild
RELTEST_FILES = $(EMAKEFILE) $(TEST_SPEC_FILE) $(COVER_SPEC_FILE) $(SOURCE)
endif

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

include ../src/app/diameter.mk 

ifeq ($(USE_DIAMETER_TEST_CODE),true)
ERL_COMPILE_FLAGS += -DDIAMETER_TEST_CODE=mona_lisa_spelar_doom
endif

ifeq ($(USE_DIAMETER_HIPE),true)
ERL_COMPILE_FLAGS += +native -DDIAMETER_hipe_special=true
endif

ERL_COMPILE_FLAGS += $(DIAMETER_ERL_COMPILE_FLAGS)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

all test: $(SUITES)

tests debug opt: $(BUILDTARGET)

beam targets: $(TARGET_FILES)

emakebuild: $(EMAKEFILE)

$(EMAKEFILE): 
	$(MAKE_EMAKE) $(ERL_COMPILE_FLAGS) -o $(EBIN) '*_SUITE_make' $(MODULES) \
	| grep -v Warning \
	> $(EMAKEFILE)

clean:
	rm -f $(EMAKEFILE)
	rm -f $(TARGET_FILES) 
	rm -f depend.mk

realclean: clean
	rm -f errs core *~

.PHONY: all emakebuild test tests debug opt beam targets clean realclean

docs:

info:
	@echo "MAKE_EMAKE  = $(MAKE_EMAKE)"
	@echo "EMAKEFILE   = $(EMAKEFILE)"
	@echo "BUILDTARGET = $(BUILDTARGET)"
	@echo
	@echo "ERL_COMPILE_FLAGS = $(ERL_COMPILE_FLAGS)"
	@echo "ERL  = $(ERL)"
	@echo "ERLC = $(ERLC)"
	@echo
	@echo "HRL_FILES    = $(HRL_FILES)"
	@echo "ERL_FILES    = $(ERL_FILES)"
	@echo "TARGET_FILES = $(TARGET_FILES)"
	@echo
	@echo "SUITE_MODULES = $(SUITE_MODULES)"
	@echo "SUITES        = $(SUITES)"
	@echo

help:
	@echo
	@echo "This Makefile controls the test of the $(APPLICATION) application."
	@echo
	@echo "There are two separate ways to perform the test of $(APPLICATION)."
	@echo
	@echo "  a) Run the official OTP test-server (which we do not describe here)"
	@echo
	@echo "  b) Run individual suites using targets in this makefile, target"
	@echo "     xxx running the testcases contained in $(APPLICATION)_xxx_SUITE."
	@echo "     Note that this requires GNU sed."
	@echo
	@echo "Targets:"
	@echo
	@echo "    all"
	@echo "        Run all test suites."
	@echo
	@echo "    $(SUITES)"
	@echo "        Run a specific test suite."
	@echo
	@echo "    beam"
	@echo "        Compile all test-code."
	@echo
	@echo "    clean"
	@echo "        Remove generated files."
	@echo
	@echo "    info"
	@echo "        Prints various environment variables."
	@echo "        May be useful when debugging this Makefile."
	@echo
	@echo "    help"
	@echo "        Print this info."
	@echo

.PHONY: docs info help

# ----------------------------------------------------
# Special Targets
# ----------------------------------------------------

# This assumes GNU sed to exit 1 if the output looks to indicate failure.
# diameter_ct:run/1 itself can't tell (it seems).
$(SUITES): log targets
	$(ERL) -noshell \
	        -pa ../ebin \
	        -sname diameter_test_$@ \
	        -s diameter_ct run $@ \
	        -s init stop \
	| sed '/ FAILED /h; p; $$!d; x; /./!d; Q 1'

log:
	mkdir $@

.PHONY: $(SUITES)

# ----------------------------------------------------
# Release Targets
# ---------------------------------------------------- 

include $(TOP)/make/otp_release_targets.mk

release_spec: 

release_docs_spec:

release_tests_spec: tests
	$(INSTALL_DIR)  $(RELSYSDIR)
	$(INSTALL_DATA) $(RELTEST_FILES) $(RELSYSDIR)

.PHONY: release_spec release_docs_spec release_test_specs

# ----------------------------------------------------

depend: depend.mk

# Generate dependencies makefile.
depend.mk: depend.sed $(MODULES:%=%.erl) Makefile
	(for f in $(MODULES); do \
	     sed -f $< $$f.erl | sed "s@/@/$$f@"; \
	 done) \
	> $@

-include depend.mk

.PHONY: depend
