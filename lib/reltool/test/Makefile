#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2009-2012. All Rights Reserved.
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

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	rtt \
	reltool_app_SUITE \
	reltool_wx_SUITE \
	reltool_server_SUITE \
	reltool_escript_SUITE \
	reltool_manual_gui_SUITE \
	reltool_test_lib


ERL_FILES= $(MODULES:%=%.erl)

HRL_FILES= reltool_test_lib.hrl

TARGET_FILES= \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR))

INSTALL_PROGS= $(TARGET_FILES)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/reltool_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -pa $(ERL_TOP)/lib/reltool/ebin/

EBIN = .

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt: $(TARGET_FILES)

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
	$(INSTALL_DATA) reltool.spec reltool.cover $(ERL_FILES) $(HRL_FILES) "$(RELSYSDIR)"
	$(INSTALL_SCRIPT) rtt $(INSTALL_PROGS) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(INSTALL_PROGS) "$(RELSYSDIR)"
	chmod -R u+w "$(RELSYSDIR)"
	@tar cf - *_SUITE_data | (cd "$(RELSYSDIR)"; tar xf -)

release_docs_spec:


