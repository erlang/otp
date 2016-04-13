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

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/orber-$(ORBER_VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	orber \
	corba \
	corba_boa \
	corba_object \
	any \
	iop_ior \
	orber_tc \
	orber_typedefs \
	orber_request_number \
	orber_objectkeys \
	orber_initial_references \
	cdrlib \
	cdr_encode \
	cdr_decode \
	orber_iiop \
	orber_iiop_net \
	orber_iiop_net_accept \
	orber_iiop_insup \
	orber_iiop_inproxy \
	orber_iiop_inrequest \
	orber_iiop_pm \
	orber_iiop_outsup \
	orber_iiop_outproxy \
	orber_iiop_socketsup \
	orber_socket \
	orber_ifr \
	orber_ifr_aliasdef \
	orber_ifr_arraydef \
	orber_ifr_attributedef \
	orber_ifr_constantdef \
	orber_ifr_contained \
	orber_ifr_container \
	orber_ifr_enumdef \
	orber_ifr_exceptiondef \
	orber_ifr_idltype \
	orber_ifr_interfacedef \
	orber_ifr_irobject \
	orber_ifr_moduledef \
	orber_ifr_operationdef \
	orber_ifr_orb \
	orber_ifr_primitivedef \
	orber_ifr_repository \
	orber_ifr_sequencedef \
	orber_ifr_stringdef \
	orber_ifr_wstringdef \
	orber_ifr_structdef \
	orber_ifr_typecode \
	orber_ifr_typedef \
	orber_ifr_uniondef \
	orber_ifr_fixeddef \
	orber_ifr_utils \
	OrberApp_IFR_impl \
	orber_pi \
	orber_web \
	orber_web_server \
	orber_iiop_tracer \
	orber_iiop_tracer_silent \
	orber_iiop_tracer_stealth \
	fixed \
	orber_exceptions \
	orber_diagnostics \
	orber_acl \
	orber_env \
	orber_tb

ASN_MODULES = OrberCSIv2
ASN_SET = $(ASN_MODULES:%=%.set.asn)
ASN_ASNS = $(ASN_MODULES:%=%.asn1)
GEN_ASN_ERL = $(ASN_MODULES:%=%.erl)
GEN_ASN_HRL = $(ASN_MODULES:%=%.hrl)
GEN_ASN_DBS = $(ASN_MODULES:%=%.asn1db)
GEN_ASN_TABLES = $(ASN_MODULES:%=%.table)

PKIX_FILES = \
	OrberCSIv2.asn1 \
	PKIXAttributeCertificate.asn1 \
	PKIX1Explicit88.asn1 \
	PKIX1Algorithms88.asn1 \
	PKIX1Implicit88.asn1 \
	OrberCSIv2.set.asn

EXTERNAL_HRL_FILES= ../include/corba.hrl \
	../include/ifr_types.hrl \
	../include/orber_pi.hrl

INTERNAL_HRL_FILES = \
	orber_iiop.hrl \
	ifr_objects.hrl \
	orber_ifr.hrl

ERL_FILES= $(MODULES:%=%.erl)	

GEN_ERL_FILES1 =  \
	oe_erlang.erl \
	erlang_pid.erl \
	erlang_port.erl \
	erlang_ref.erl \
	erlang_binary.erl 

GEN_ERL_FILES2 =  \
	oe_CORBA.erl

GEN_ERL_FILES3 =  \
	oe_OrberIFR.erl \
	OrberApp_IFR.erl

GEN_ERL_FILES =  $(GEN_ERL_FILES1) $(GEN_ERL_FILES2) \
	$(GEN_ERL_FILES3) \
#	$(GEN_ASN_ERL)

GEN_HRL_FILES1 = \
	oe_erlang.hrl \
	erlang.hrl

GEN_HRL_FILES2 = \
	CORBA.hrl \
	oe_CORBA.hrl

GEN_HRL_FILES3 = \
	OrberApp_IFR.hrl \
	oe_OrberIFR.hrl \
	OrberApp.hrl

GEN_HRL_FILES_EXT = $(GEN_HRL_FILES1)

GEN_HRL_FILES_LOC = $(GEN_HRL_FILES2) $(GEN_HRL_FILES3) \
#	$(GEN_ASN_HRL)


GEN_FILES = \
	$(GEN_ERL_FILES) \
	$(GEN_HRL_FILES_LOC) \
	$(GEN_HRL_FILES_EXT) \
#	$(GEN_ASN_DBS) \
#	$(GEN_ASN_TABLES)

TARGET_FILES =  \
	$(GEN_ERL_FILES:%.erl=$(EBIN)/%.$(EMULATOR)) \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR))

APPUP_FILE = orber.appup
APPUP_SRC = $(APPUP_FILE).src
APPUP_TARGET = $(EBIN)/$(APPUP_FILE)

APP_FILE = orber.app
APP_SRC = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_IDL_FLAGS += -pa $(ERL_TOP)/lib/orber/ebin
# The -pa option is just used temporary until erlc can handle 
# includes from other directories than ../include .
ERL_COMPILE_FLAGS += $(ERL_IDL_FLAGS) \
	-I$(ERL_TOP)/lib/orber/include \
	+'{parse_transform,sys_pre_attributes}' \
	+'{attribute,insert,app_vsn,"orber_$(ORBER_VSN)"}'

ASN_FLAGS = -bber +der +compact_bit_string +nowarn_unused_record

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
debug:
	@${MAKE} TYPE=debug opt

opt: $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)

clean:
	rm -f $(TARGET_FILES) $(GEN_FILES) $(APP_TARGET) $(APPUP_TARGET) IDL-GENERATED
	rm -f errs core *~

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(ORBER_VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(ORBER_VSN);' $< > $@

docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

IDL-GENERATED:  $(ERL_TOP)/lib/ic/include/erlang.idl CORBA.idl OrberIFR.idl
	$(gen_verbose)erlc $(ERL_IDL_FLAGS) $(ERL_TOP)/lib/ic/include/erlang.idl
	$(V_at)erlc $(ERL_IDL_FLAGS) CORBA.idl
	$(V_at)erlc $(ERL_IDL_FLAGS) +'{this,"Orber::IFR"}' OrberIFR.idl
	$(V_at)>IDL-GENERATED

$(GEN_ERL_FILES): IDL-GENERATED
$(TARGET_FILES): IDL-GENERATED

$(GEN_ASN_ERL) $(GEN_ASN_HRL): OrberCSIv2.asn1 OrberCSIv2.set.asn
	$(asn_verbose)erlc $(ERL_COMPILE_FLAGS) $(ASN_FLAGS) +'{inline,"OrberCSIv2"}' OrberCSIv2.set.asn
	$(V_at)rm -f $(GEN_ASN_ERL:%.erl=%.beam)

#	erlc $(ERL_COMPILE_FLAGS) $(ASN_FLAGS) OrberCSIv2.asn1 ;\
#	erlc $(GEN_ASN_ERL) 
# Use the following when we safely can inline the ASN1 runtime code.
# Requires igor (part of syntax_tools (introduced in R10B
#	erlc $(ERL_COMPILE_FLAGS) $(ASN_FLAGS) +'{inline,"OrberCSIv2"}' OrberCSIv2.set.asn ; \


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ERL_FILES) $(YRL_FILE) $(GEN_HRL_FILES_LOC) $(INTERNAL_HRL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(EXTERNAL_HRL_FILES) $(GEN_HRL_FILES_EXT) "$(RELSYSDIR)/include"


release_docs_spec:


