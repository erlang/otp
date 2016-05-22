#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
EBIN=../../ebin

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(ORBER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/orber-$(VSN)
# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES = \
	CosNaming_NamingContextExt_impl \
        CosNaming_BindingIterator_impl \
	lname \
	lname_component \
	orber_cosnaming_utils

ERL_FILES = $(MODULES:%=%.erl) 
HRL_FILES = lname.hrl \
	orber_cosnaming.hrl
GEN_ERL_FILES =  \
	oe_cos_naming.erl \
	CosNaming_Name.erl \
	CosNaming_NamingContext.erl \
	CosNaming_BindingIterator.erl \
	CosNaming_NameComponent.erl \
	CosNaming_Binding.erl \
	CosNaming_BindingList.erl \
	CosNaming_NamingContext_NotFound.erl \
	CosNaming_NamingContext_AlreadyBound.erl \
	CosNaming_NamingContext_CannotProceed.erl \
	CosNaming_NamingContext_InvalidName.erl \
	CosNaming_NamingContext_NotEmpty.erl

GEN_EXT_ERL_FILES =  \
	oe_cos_naming_ext.erl \
	CosNaming_NamingContextExt.erl \
	CosNaming_NamingContextExt_InvalidAddress.erl

GEN_HRL_FILES = \
	oe_cos_naming.hrl \
	CosNaming.hrl \
	CosNaming_NamingContext.hrl \
	CosNaming_BindingIterator.hrl

GEN_EXT_HRL_FILES = \
	oe_cos_naming_ext.hrl \
	CosNaming_NamingContextExt.hrl

GEN_FILES = $(GEN_ERL_FILES) $(GEN_HRL_FILES) \
	$(GEN_EXT_ERL_FILES) $(GEN_EXT_HRL_FILES)

TARGET_FILES = \
	$(GEN_EXT_ERL_FILES:%.erl=$(EBIN)/%.$(EMULATOR)) \
	$(GEN_ERL_FILES:%.erl=$(EBIN)/%.$(EMULATOR)) \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR)) 

IDL_FILE = cos_naming.idl \
	cos_naming_ext.idl

APP_FILE = 
#APP_SRC = $(APP_FILE).src
#APP_TARGET = $(EBIN)/$(APP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_IDL_FLAGS += -pa $(ERL_TOP)/lib/orber/ebin
# The -pa option is just used temporary until erlc can handle 
# includes from other directories than ../include .
ERL_COMPILE_FLAGS += \
	$(ERL_IDL_FLAGS) \
	-I$(ERL_TOP)/lib/orber/include \
	+'{parse_transform,sys_pre_attributes}' \
	+'{attribute,insert,app_vsn,"orber_$(ORBER_VSN)"}' 

YRL_FLAGS = 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
opt: $(TARGET_FILES) $(APP_TARGET)

debug:
	@${MAKE} TYPE=debug

clean:
	rm -f $(TARGET_FILES) $(GEN_FILES)  $(APP_TARGET) IDL-GENERATED
	rm -f errs core *~

$(APP_TARGET): $(APP_SRC)
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $(APP_SRC) > $(APP_TARGET)

docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------
IDL-GENERATED: cos_naming_ext.idl cos_naming.idl
	$(gen_verbose)erlc $(ERL_IDL_FLAGS) +'{this,"CosNaming::NamingContext"}' \
	+'{this,"CosNaming::NamingContextExt"}' cos_naming_ext.idl
	$(V_at)erlc $(ERL_IDL_FLAGS) +'{this,"CosNaming::NamingContext"}' cos_naming.idl
	$(V_at)>IDL-GENERATED

$(GEN_FILES): IDL-GENERATED

$(TARGET_FILES): IDL-GENERATED

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) $(APP_TARGET) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR) "$(RELSYSDIR)/COSS/CosNaming"
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(IDL_FILE) "$(RELSYSDIR)/COSS/CosNaming"
	$(INSTALL_DATA) $(GEN_FILES) "$(RELSYSDIR)/COSS/CosNaming"


release_docs_spec:

