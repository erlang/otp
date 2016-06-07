#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug -W
endif
EBIN=../ebin
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(COSEVENTDOMAIN_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/cosEventDomain-$(VSN)
# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES = \
	CosEventDomainAdmin_EventDomainFactory_impl \
	CosEventDomainAdmin_EventDomain_impl \
	cosEventDomainApp

ERL_FILES = $(MODULES:%=%.erl) 
HRL_FILES = cosEventDomainApp.hrl

EXTERNAL_INC_PATH = ../include

GEN_ERL_FILES = \
	oe_CosEventDomainAdmin.erl \
	CosEventDomainAdmin.erl \
	CosEventDomainAdmin_DiamondSeq.erl \
	CosEventDomainAdmin_AlreadyExists.erl \
	CosEventDomainAdmin_DomainIDSeq.erl \
	CosEventDomainAdmin_Connection.erl \
	CosEventDomainAdmin_ConnectionIDSeq.erl \
	CosEventDomainAdmin_ConnectionNotFound.erl \
	CosEventDomainAdmin_CycleCreationForbidden.erl \
	CosEventDomainAdmin_CycleSeq.erl \
	CosEventDomainAdmin_DiamondCreationForbidden.erl \
	CosEventDomainAdmin_DomainNotFound.erl \
	CosEventDomainAdmin_EventDomain.erl \
	CosEventDomainAdmin_EventDomainFactory.erl \
	CosEventDomainAdmin_MemberIDSeq.erl \
	CosEventDomainAdmin_RouteSeq.erl



GEN_HRL_FILES = \
	oe_CosEventDomainAdmin.hrl \
	CosEventDomainAdmin.hrl \
	CosEventDomainAdmin_EventDomainFactory.hrl \
	CosEventDomainAdmin_EventDomain.hrl

EXTERNAL_GEN_HRL_FILES = \
	$(GEN_HRL_FILES:%=$(EXTERNAL_INC_PATH)/%)


TARGET_FILES = \
	$(GEN_ERL_FILES:%.erl=$(EBIN)/%.$(EMULATOR)) \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR)) 

GEN_FILES = $(GEN_HRL_FILES) $(GEN_ERL_FILES)

IDL_FILES = \
	CosEventDomainAdmin.idl

APPUP_FILE = cosEventDomain.appup
APPUP_SRC = $(APPUP_FILE).src
APPUP_TARGET = $(EBIN)/$(APPUP_FILE)

APP_FILE = cosEventDomain.app
APP_SRC = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_IDL_FLAGS += \
	-pa $(ERL_TOP)/lib/cosEventDomain/ebin \
	-pa $(ERL_TOP)/lib/cosEventDomain/include \
	-pa $(ERL_TOP)/lib/cosNotification/include \
	-pa $(ERL_TOP)/lib/cosNotification/ebin \
	-pa $(ERL_TOP)/lib/ic/ebin \
	-I$(ERL_TOP)/lib/cosNotification/include \
	-I$(ERL_TOP)/lib/cosNotification/ebin \
	-I$(ERL_TOP)/lib/cosNotification/src \
	-I$(ERL_TOP)/lib/cosEvent/src \
	-I$(ERL_TOP)/lib/cosEventDomain/include
# The -pa option is just used temporary until erlc can handle 
# includes from other directories than ../include .
ERL_COMPILE_FLAGS += \
	$(ERL_IDL_FLAGS) \
	-pa $(ERL_TOP)/lib/cosEventDomain/include \
	-pa $(ERL_TOP)/lib/cosNotification/ebin \
	-pa $(ERL_TOP)/lib/cosNotification/include \
	-pa $(ERL_TOP)/lib/cosNotification/src \
	-I$(ERL_TOP)/lib/cosEventDomain/include \
	-I$(ERL_TOP)/lib/cosNotification/ebin \
	-I$(ERL_TOP)/lib/cosNotification/include \
	-I$(ERL_TOP)/lib/cosNotification/src \
	-I$(ERL_TOP)/lib/cosEvent/src \
	-I$(ERL_TOP)/lib/cosEvent/include \
	+'{parse_transform,sys_pre_attributes}' \
	+'{attribute,insert,app_vsn,"cosEventDomain_$(COSEVENTDOMAIN_VSN)"}' 

YRL_FLAGS = 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
opt: $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)

debug:
	@${MAKE} TYPE=debug opt

clean:
	rm -f $(TARGET_FILES) $(GEN_FILES) $(APP_TARGET) $(APPUP_TARGET) IDL-GENERATED
	rm -f errs core *~

$(APP_TARGET): $(APP_SRC)
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $(APP_SRC) > $(APP_TARGET)
$(APPUP_TARGET): $(APPUP_SRC)
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $(APPUP_SRC) > $(APPUP_TARGET)

docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------
IDL-GENERATED: CosEventDomainAdmin.idl
	$(gen_verbose)erlc $(ERL_IDL_FLAGS) +'{cfgfile,"CosEventDomainAdmin.cfg"}' CosEventDomainAdmin.idl
	$(V_at)mv $(GEN_HRL_FILES) $(EXTERNAL_INC_PATH)
	$(V_at)>IDL-GENERATED

$(GEN_FILES): IDL-GENERATED

$(TARGET_FILES): IDL-GENERATED

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DATA) ../info "$(RELSYSDIR)"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(GEN_ERL_FILES) $(IDL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(EXTERNAL_GEN_HRL_FILES) "$(RELSYSDIR)/include"


release_docs_spec:





