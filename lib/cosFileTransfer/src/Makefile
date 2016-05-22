#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2000-2016. All Rights Reserved.
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

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(COSFILETRANSFER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/cosFileTransfer-$(VSN)

EXTERNAL_INC_PATH = ../include

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES = \
	cosFileTransferApp \
	CosFileTransfer_Directory_impl \
	CosFileTransfer_File_impl \
	CosFileTransfer_VirtualFileSystem_impl \
	CosFileTransfer_FileTransferSession_impl \
	CosFileTransfer_FileIterator_impl \
	cosFileTransferNATIVE_file \

ERL_FILES = $(MODULES:%=%.erl) 
HRL_FILES = \
	cosFileTransferApp.hrl \

GEN_ERL_FILES =  \
	oe_CosFileTransfer.erl \
	CosFileTransfer.erl \
	CosFileTransfer_AccessLevel.erl \
	CosFileTransfer_CommandNotImplementedException.erl \
	CosFileTransfer_Directory.erl \
	CosFileTransfer_File.erl \
	CosFileTransfer_FileIterator.erl \
	CosFileTransfer_FileList.erl \
	CosFileTransfer_FileNameList.erl \
	CosFileTransfer_FileNotFoundException.erl \
	CosFileTransfer_FileTransferSession.erl \
	CosFileTransfer_FileWrapper.erl \
	CosFileTransfer_IllegalOperationException.erl \
	CosFileTransfer_ProtocolAddressList.erl \
	CosFileTransfer_ProtocolSupport.erl \
	CosFileTransfer_RequestFailureException.erl \
	CosFileTransfer_SessionException.erl \
	CosFileTransfer_SupportedProtocolAddresses.erl \
	CosFileTransfer_TransferException.erl \
	CosFileTransfer_VirtualFileSystem.erl \
	CosFileTransfer_VirtualFileSystem_ContentList.erl \

LOCAL_HRL_FILES =  \
	oe_CosFileTransfer.hrl \
	CosFileTransfer.hrl \
	CosFileTransfer_Directory.hrl \
	CosFileTransfer_File.hrl \
	CosFileTransfer_FileIterator.hrl \
	CosFileTransfer_FileTransferSession.hrl \
	CosFileTransfer_VirtualFileSystem.hrl \

GEN_HRL_FILES = $(LOCAL_HRL_FILES:%=$(EXTERNAL_INC_PATH)/%)

GEN_FILES =  \
	$(GEN_HRL_FILES) \
	$(GEN_ERL_FILES)

TARGET_FILES = \
	$(GEN_ERL_FILES:%.erl=$(EBIN)/%.$(EMULATOR)) \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR)) 

IDL_FILES = \
	CosFileTransfer.idl

APPUP_FILE = cosFileTransfer.appup
APPUP_SRC = $(APPUP_FILE).src
APPUP_TARGET = $(EBIN)/$(APPUP_FILE)

APP_FILE = cosFileTransfer.app
APP_SRC = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_IDL_FLAGS += -pa $(ERL_TOP)/lib/cosFileTransfer/ebin \
	-pa $(ERL_TOP)/lib/cosProperty/ebin \
	-pa $(ERL_TOP)/lib/cosProperty/src \
	-I$(ERL_TOP)/lib/cosProperty/src \
	-pa $(ERL_TOP)/lib/ic/ebin \
	-pa $(ERL_TOP)/lib/orber/ebin

# The -pa option is just used temporary until erlc can handle 
# includes from other directories than ../include .
ERL_COMPILE_FLAGS += \
	$(ERL_IDL_FLAGS) \
	-pa $(ERL_TOP)/lib/orber/include \
	-pa $(ERL_TOP)/lib/cosFileTransfer/include \
	-pa $(ERL_TOP)/lib/cosProperty/include \
	-pa $(ERL_TOP)/lib/cosProperty/src \
	-I$(ERL_TOP)/lib/orber/include \
	-I$(ERL_TOP)/lib/cosFileTransfer/include \
	-I$(ERL_TOP)/lib/cosProperty/include \
	-I$(ERL_TOP)/lib/cosProperty/src \
	+'{parse_transform,sys_pre_attributes}' \
	+'{attribute,insert,app_vsn,"cosFileTransfer_$(COSFILETRANSFER_VSN)"}' 


# ----------------------------------------------------
# Targets
# ----------------------------------------------------
opt: $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)

debug:
	@${MAKE} TYPE=debug opt

cleanb:
	rm -f $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)
	rm -f errs core *~

clean:
	rm -f $(TARGET_FILES) $(GEN_FILES) $(APP_TARGET) $(APPUP_TARGET) IDL-GENERATED
	rm -f errs core *~

$(APP_TARGET): $(APP_SRC)
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------
IDL-GENERATED: CosFileTransfer.idl
	$(gen_verbose)erlc $(ERL_IDL_FLAGS) +'{cfgfile,"CosFileTransfer.cfg"}' CosFileTransfer.idl
	$(V_at)mv $(LOCAL_HRL_FILES) $(EXTERNAL_INC_PATH)
	$(V_at)>IDL-GENERATED

$(GEN_FILES): IDL-GENERATED

$(TARGET_FILES): IDL-GENERATED

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(GEN_FILES) $(IDL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(GEN_ERL_FILES) $(IDL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(GEN_HRL_FILES) "$(RELSYSDIR)/include"

release_docs_spec:
