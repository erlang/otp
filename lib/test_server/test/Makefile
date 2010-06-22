#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
	test_server_SUITE \
	test_server_line_SUITE \
	test_server_skip_SUITE \
	test_server_conf01_SUITE \
	test_server_conf02_SUITE \
	test_server_parallel01_SUITE \
	test_server_shuffle01_SUITE

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
INSTALL_PROGS= $(TARGET_FILES)

EMAKEFILE=Emakefile
COVERFILE=test_server.cover

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/test_server_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ERL_MAKE_FLAGS += -pa $(ERL_TOP)/lib/test_server/ebin
ERL_COMPILE_FLAGS += -I$(ERL_TOP)/lib/test_server/include

EBIN = .

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

.PHONY: make_emakefile

make_emakefile:
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) \
	'*_SUITE_make' > $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES)\
	>> $(EMAKEFILE)

tests debug opt: make_emakefile
	cd ../src && $(MAKE) ../ebin/test_server_line.beam
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
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DATA) $(EMAKEFILE) $(ERL_FILES) $(COVERFILE) $(RELSYSDIR)
	$(INSTALL_DATA) test_server.spec $(RELSYSDIR)
	chmod -f -R u+w $(RELSYSDIR)
	@tar cf - *_SUITE_data | (cd $(RELSYSDIR); tar xf -)

release_docs_spec:
