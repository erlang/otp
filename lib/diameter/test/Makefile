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

RELTEST_FILES = $(TEST_SPEC_FILE) $(COVER_SPEC_FILE) $(SOURCE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

include ../src/app/diameter.mk 

# This is only used to compile suite locally when running with a
# target like 'all' below. Target release_tests only installs source.
ERL_COMPILE_FLAGS += $(DIAMETER_ERL_COMPILE_FLAGS) \
                     -DDIAMETER_CT=true \
                     -I $(DIAMETER_TOP)/src/app

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

all: $(SUITES)

tests debug opt: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES) 
	rm -f depend.mk

realclean: clean
	rm -rf log
	rm -f errs core *~

.PHONY: all tests debug opt clean realclean

docs:

info:
	@echo "TARGET_FILES = $(TARGET_FILES)"
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
	@echo "Targets:"
	@echo
	@echo "    all"
	@echo "        Run all test suites."
	@echo
	@echo "    $(SUITES)"
	@echo "        Run a specific test suite."
	@echo
	@echo "    tests"
	@echo "        Compile all test-code."
	@echo
	@echo "    clean | realclean"
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

# Exit with a non-zero status if the output looks to indicate failure.
# diameter_ct:run/1 itself can't tell (it seems).
$(SUITES): log tests
	$(ERL) -noshell \
	        -pa $(DIAMETER_TOP)/ebin \
	        -sname diameter_test_$@ \
	        -s diameter_ct run diameter_$@_SUITE \
	        -s init stop \
	| awk '1{rc=0} {print} / FAILED /{rc=1} END{exit rc}'
# Shorter in sed but requires a GNU extension (ie. Q).

log:
	mkdir $@

.PHONY: $(SUITES)

# ----------------------------------------------------
# Release Targets
# ---------------------------------------------------- 

include $(TOP)/make/otp_release_targets.mk

release_spec: 

release_docs_spec:

release_tests_spec:
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
