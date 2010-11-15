#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2000-2010. All Rights Reserved.
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
VSN=$(COSFILETRANSFER_VSN)
# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/cosFileTransfer_test

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
TEST_SPEC_FILE = cosFileTransfer.spec


IDL_FILES = 

IDLOUTDIR = idl_output

MODULES =  \
	fileTransfer_SUITE \

GEN_MODULES = \

GEN_HRL_FILES = \

ERL_FILES = $(MODULES:%=%.erl)

HRL_FILES =

GEN_FILES = \
	$(GEN_HRL_FILES:%=$(IDLOUTDIR)/%) \
	$(GEN_MODULES:%=$(IDLOUTDIR)/%.erl) 

GEN_TARGET_FILES = $(GEN_MODULES:%=$(IDLOUTDIR)/%.$(EMULATOR))

SUITE_TARGET_FILES = $(MODULES:%=%.$(EMULATOR))

TARGET_FILES = \
	$(GEN_TARGET_FILES) \
	$(SUITE_TARGET_FILES)


# ----------------------------------------------------
# PROGRAMS
# ----------------------------------------------------
LOCAL_CLASSPATH = $(ERL_TOP)lib/cosFileTransfer/priv:$(ERL_TOP)lib/cosFileTransfer/test
# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_IDL_FLAGS += -pa $(ERL_TOP)/lib/cosFileTransfer/ebin \
	-pa $(ERL_TOP)/lib/cosFileTransfer/src \
	-pa $(ERL_TOP)/lib/cosFileTransfer/include \
	-pa $(ERL_TOP)/lib/cosProperty/ebin \
	-pa $(ERL_TOP)/lib/cosProperty/include \
	-pa $(ERL_TOP)/lib/orber/ebin \
	-pa $(ERL_TOP)/lib/ic/ebin

ERL_COMPILE_FLAGS +=  \
	$(ERL_IDL_FLAGS) \
	-pa $(ERL_TOP)/lib/orber/include \
	-pa $(ERL_TOP)/lib/cosProperty/include \
	-pa $(ERL_TOP)/internal_tools/test_server/ebin \
	-pa $(ERL_TOP)/lib/cosFileTransfer/ebin \
	-pa $(ERL_TOP)/lib/cosFileTransfer/include \
	-pa $(ERL_TOP)/lib/cosFileTransfer/test/idl_output \
	-I$(ERL_TOP)/lib/orber/include \
	-I$(ERL_TOP)/lib/cosProperty/include \
	-I$(ERL_TOP)/lib/cosFileTransfer/src \
	-I$(ERL_TOP)/lib/cosFileTransfer/include \
	-I$(ERL_TOP)/lib/cosFileTransfer \
	-I$(ERL_TOP)/lib/cosFileTransfer/test/$(IDLOUTDIR) \
	-I$(ERL_TOP)/lib/test_server/include

# ----------------------------------------------------
# Targets
# ----------------------------------------------------


tests debug opt: $(TARGET_FILES)

clean:
	rm -f idl_output/*
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
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DATA) $(IDL_FILES)  $(TEST_SPEC_FILE) \
		$(ERL_FILES) $(RELSYSDIR)
	$(INSTALL_DATA) $(SUITE_TARGET_FILES) $(RELSYSDIR)
	chmod -R u+w $(RELSYSDIR)
