#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

#ifeq ($(TYPE),debug)
#ERL_COMPILE_FLAGS += -Ddebug -W
#endif

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(COSTRANSACTIONS_VSN)
# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/cosTransactions_test

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
TEST_SPEC_FILE = cosTransactions.spec
COVER_FILE = cosTransactions.cover


IDL_FILES = \
	etrap_test.idl

IDLOUTDIR = idl_output

MODULES =  \
	transactions_SUITE \
	etrap_test_server_impl \
	etrap_test_lib \
	generated_SUITE

GEN_MODULES = \
	oe_etrap_test \
	etrap_test_server

GEN_HRL_FILES = \
	oe_etrap_test.hrl \
	etrap_test_server.hrl

ERL_FILES = $(MODULES:%=%.erl)

HRL_FILES = \
	etrap_test_lib.hrl

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
LOCAL_CLASSPATH = $(ERL_TOP)/lib/cosTransactions/priv:$(ERL_TOP)/lib/cosTransactions/test
# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_IDL_FLAGS += -pa $(ERL_TOP)/lib/cosTransactions/ebin\
	-pa $(ERL_TOP)/lib/orber/ebin \
	-pa $(ERL_TOP)/lib/ic/ebin

ERL_COMPILE_FLAGS +=  \
	$(ERL_IDL_FLAGS) \
	-pa $(ERL_TOP)/lib/orber/include \
	-pa $(ERL_TOP)/lib/cosTransactions/ebin \
	-pa $(ERL_TOP)/lib/cosTransactions/test/idl_output \
	-I$(ERL_TOP)/lib/orber/include \
	-I$(ERL_TOP)/lib/cosTransactions \
	-I$(ERL_TOP)/lib/cosTransactions/test/$(IDLOUTDIR)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------


tests debug opt: $(TARGET_FILES)

clean:
	rm -f idl_output/*
	rm -f $(TARGET_FILES) 
	rm -f errs core *~

#debug:
#	@${MAKE} TYPE=debug

docs:

# ----------------------------------------------------
# Special Targets
# ----------------------------------------------------

IDL-GENERATED: etrap_test.idl
	erlc $(ERL_IDL_FLAGS) -o$(IDLOUTDIR) \
                +'{cfgfile,"etrap_test.cfg"}' etrap_test.idl
	>IDL-GENERATED

$(GEN_FILES): IDL-GENERATED

$(TARGET_FILES): IDL-GENERATED

# ----------------------------------------------------
# Release Targets
# ---------------------------------------------------- 
# We don't copy generated intermediate erlang and hrl files

include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: 

release_docs_spec:

release_tests_spec: tests
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(IDL_FILES)  $(TEST_SPEC_FILE) \
		$(COVER_FILE) $(ERL_FILES) $(HRL_FILES) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(SUITE_TARGET_FILES) "$(RELSYSDIR)"
	$(INSTALL_DIR) "$(RELSYSDIR)/$(IDLOUTDIR)"
	$(INSTALL_DATA) $(GEN_TARGET_FILES) $(GEN_FILES) \
		"$(RELSYSDIR)/$(IDLOUTDIR)"

