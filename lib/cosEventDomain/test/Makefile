#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2001-2012. All Rights Reserved.
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
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(COSEVENTDOMAIN_VSN)
# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/cosEventDomain_test

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
TEST_SPEC_FILE = cosEventDomain.spec
COVER_FILE = cosEventDomain.cover


MODULES =  \
	event_domain_SUITE \
	generated_SUITE

ERL_FILES = $(MODULES:%=%.erl)

HRL_FILES = 

SUITE_TARGET_FILES = $(MODULES:%=%.$(EMULATOR))

TARGET_FILES = \
	$(SUITE_TARGET_FILES)

# ----------------------------------------------------
# PROGRAMS
# ----------------------------------------------------
# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_IDL_FLAGS += -pa $(ERL_TOP)/lib/orber/ebin -pa $(ERL_TOP)/lib/ic/ebin

ERL_COMPILE_FLAGS += \
	$(ERL_IDL_FLAGS) \
	-pa $(ERL_TOP)/lib/test_server/ebin \
	-pa $(ERL_TOP)/lib/cosEventDomain/ebin \
	-pa $(ERL_TOP)/lib/cosEventDomain/include \
	-pa $(ERL_TOP)/lib/cosNotification/ebin \
	-pa $(ERL_TOP)/lib/cosNotification/include \
	-I$(ERL_TOP)/lib/cosEventDomain/include \
	-I$(ERL_TOP)/lib/cosNotification/include \
	-I$(ERL_TOP)/lib/cosNotification/ebin \
	-I$(ERL_TOP)/lib/test_server/include

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
tests debug opt: $(TARGET_FILES) 

clean:
	rm -f $(TARGET_FILES) 
	rm -f errs core *~


docs:

# ----------------------------------------------------
# Special Targets
# ----------------------------------------------------

# ----------------------------------------------------
# Release Targets
# ---------------------------------------------------- 
# We don't copy generated intermediate erlang and hrl files

include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: 

release_docs_spec:

release_tests_spec: tests
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(TEST_SPEC_FILE) \
		$(COVER_FILE) $(ERL_FILES) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(SUITE_TARGET_FILES) "$(RELSYSDIR)"

