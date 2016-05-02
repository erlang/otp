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

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(COSNOTIFICATION_VSN)
# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/cosNotification_test

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
TEST_SPEC_FILE = cosNotification.spec
COVER_FILE = cosNotification.cover


IDL_FILES = 

IDLOUTDIR = idl_output

MODULES =  \
	notification_SUITE \
	grammar_SUITE \
	eventDB_SUITE \
	generated_SUITE \
	notify_test_impl

GEN_MODULES = \
	oe_notify_test_server \
	notify_test_data \
	notify_test_computer \
	notify_test_studies \
	notify_test_ShortArray \
	notify_test_uni1 \
	notify_test_uni2 \
	notify_test_X \
	notify_test_K \
	notify_test_SeqPushC \
	notify_test_StrPushC \
	notify_test_AnyPushC \
	notify_test_SeqPullC \
	notify_test_StrPullC \
	notify_test_AnyPullC \
	notify_test_SeqPushS \
	notify_test_StrPushS \
	notify_test_AnyPushS \
	notify_test_SeqPullS \
	notify_test_StrPullS \
	notify_test_AnyPullS \
	notify_test_funcs

GEN_HRL_FILES = \
	oe_notify_test_server.hrl \
	notify_test_SeqPushC.hrl \
	notify_test_StrPushC.hrl \
	notify_test_AnyPushC.hrl \
	notify_test_SeqPullC.hrl \
	notify_test_StrPullC.hrl \
	notify_test_AnyPullC.hrl \
	notify_test_SeqPushS.hrl \
	notify_test_StrPushS.hrl \
	notify_test_AnyPushS.hrl \
	notify_test_SeqPullS.hrl \
	notify_test_StrPullS.hrl \
	notify_test_AnyPullS.hrl \
	notify_test.hrl \
	notify_test_funcs.hrl

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
LOCAL_CLASSPATH = $(ERL_TOP)lib/cosNotification/priv:$(ERL_TOP)lib/cosNotification/test
# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_IDL_FLAGS += \
	-pa $(ERL_TOP)/lib/cosNotification/ebin \
	-pa $(ERL_TOP)/lib/cosNotification/src \
	-pa $(ERL_TOP)/lib/cosTime/ebin \
	-pa $(ERL_TOP)/lib/cosTime/include \
	-pa $(ERL_TOP)/lib/orber/ebin \
	-pa $(ERL_TOP)/lib/ic/ebin \
	-pa $(ERL_TOP)/lib/cosNotification/include \
	-I$(ERL_TOP)/lib/cosEvent/src \
	-I$(ERL_TOP)/lib/cosNotification/include \

ERL_COMPILE_FLAGS +=  \
	$(ERL_IDL_FLAGS) \
	-pa $(ERL_TOP)/lib/orber/include \
	-pa $(ERL_TOP)/lib/cosEvent/ebin \
	-pa $(ERL_TOP)/lib/cosNotification/ebin \
	-pa $(ERL_TOP)/lib/cosNotification/test/idl_output \
	-pa $(ERL_TOP)/lib/cosTime/ebin \
	-pa $(ERL_TOP)/lib/cosTime/include \
	-pa $(ERL_TOP)/lib/cosNotification/include \
	-pa $(ERL_TOP)/lib/ic/ebin \
	-I$(ERL_TOP)/lib/cosTime/ebin \
	-I$(ERL_TOP)/lib/cosTime/include \
	-I$(ERL_TOP)/lib/orber/include \
	-I$(ERL_TOP)/lib/cosNotification/src \
	-I$(ERL_TOP)/lib/cosNotification/include \
	-I$(ERL_TOP)/lib/cosNotification \
	-I$(ERL_TOP)/lib/cosNotification/test/$(IDLOUTDIR)

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

IDL-GENERATED: notify_test_server.idl
	erlc $(ERL_COMPILE_FLAGS) -o$(IDLOUTDIR) \
                +'{cfgfile,"notify_test_server.cfg"}' notify_test_server.idl
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
		$(COVER_FILE) $(ERL_FILES) "$(RELSYSDIR)"
	$(INSTALL_DIR) "$(RELSYSDIR)/$(IDLOUTDIR)"
	$(INSTALL_DATA) $(GEN_TARGET_FILES) $(GEN_FILES) \
		"$(RELSYSDIR)/$(IDLOUTDIR)"
	$(INSTALL_DATA) $(SUITE_TARGET_FILES) "$(RELSYSDIR)"

