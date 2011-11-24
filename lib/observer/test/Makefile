# ``The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved via the world wide web at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
# AB. All Rights Reserved.''
# 
#     $Id$
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

MODULES =  \
	observer_SUITE \
	crashdump_viewer_SUITE \
	etop_SUITE \
	ttb_helper \
	ttb_SUITE \
	client \
	server \
	crashdump_helper

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
INSTALL_PROGS= $(TARGET_FILES)

EMAKEFILE=Emakefile
COVERFILE=observer.cover

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/observer_test

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
		$(MODULES) > $(EMAKEFILE)

tests debug opt: make_emakefile
	cd $(ERL_TOP)/lib/test_server/src && \
		$(MAKE) ../ebin/test_server_line.beam
	erl $(ERL_MAKE_FLAGS) -make

clean:
	rm -f $(EMAKEFILE)
	rm -f $(TARGET_FILES)
	rm -f core

docs:

# ----------------------------------------------------
# Special targets
# ----------------------------------------------------

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_tests_spec: make_emakefile
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DATA) observer.spec $(EMAKEFILE) \
		$(COVERFILE) $(ERL_FILES) \
		$(RELSYSDIR)
	@tar cf - *_SUITE_data | (cd $(RELSYSDIR); tar xf -)

release_docs_spec:


