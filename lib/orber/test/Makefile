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
VSN=$(ORBER_VSN)
# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/orber_test

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
TEST_SPEC_FILE = orber.spec
COVER_FILE = orber.cover


IDL_FILES = \
	orber_test.idl \
	iiop_test.idl \
	orber_test_server.idl

IDLOUTDIR = idl_output

MODULES =  \
	cdrcoding_11_SUITE \
	cdrcoding_10_SUITE \
	cdrcoding_12_SUITE \
	cdrlib_SUITE \
	corba_SUITE \
	iop_ior_11_SUITE \
	iop_ior_10_SUITE \
	iop_ior_12_SUITE \
	iiop_module_do_test_impl \
	iiop_module_test_impl \
	lname_SUITE \
	naming_context_SUITE \
	orber_SUITE \
	orber_test_server_impl \
	orber_test_timeout_server_impl \
	orber_test_lib \
	csiv2_SUITE \
	multi_ORB_SUITE \
	data_types_SUITE \
	tc_SUITE \
	generated_SUITE \
	orber_web_SUITE \
	interceptors_SUITE \
	orber_acl_SUITE \
	orber_firewall_ipv4_in_SUITE \
	orber_firewall_ipv6_in_SUITE \
	orber_firewall_ipv4_out_SUITE \
	orber_firewall_ipv6_out_SUITE \
	orber_nat_SUITE \
	ip_v4v6_interop_SUITE

GEN_MOD_ORBER = \
	oe_orber_test \
	Module_Except1 \
	Module_Except2 \
	Module_Except3 \
	Module_Except4 \
	Module_HEADER \
	Module_I1 \
	Module_I2 \
	Module_Struct0 \
	Module_Struct1 \
	Module_Struct2 \
	Module_Union \
	Module_Union1 \
	Module_Union2

GEN_HRL_ORBER = \
	oe_orber_test.hrl \
	Module.hrl \
	Module_I1.hrl \
	Module_I2.hrl

GEN_MOD_IIOP = \
	oe_iiop_test \
	iiop_module_Except1 \
	iiop_module_Struct1 \
	iiop_module_Union1 \
	iiop_module_do_test \
	iiop_module_test \
	iiop_module_test_retval

GEN_HRL_IIOP = \
	oe_iiop_test.hrl \
	iiop_module.hrl \
	iiop_module_do_test.hrl \
	iiop_module_test.hrl

GEN_MOD_TEST_SERVER = \
	oe_orber_test_server \
	orber_test_server \
	orber_test_server_ComplexUserDefinedException \
	orber_test_server_UserDefinedException \
	orber_test_server_struc \
	orber_test_server_uni \
	orber_test_server_uni_d \
	orber_test_timeout_server \
	orber_parent_inherrit \
	orber_test_server_rec_struct \
	orber_test_server_rec_struct_seq \
	orber_test_server_rec_union \
	orber_test_server_rec_union_seq

GEN_HRL_TEST_SERVER = \
	oe_orber_test_server.hrl \
	orber_test_server.hrl \
	orber_test_timeout_server.hrl

GEN_MODULES = $(GEN_MOD_ORBER) $(GEN_MOD_IIOP) \
	$(GEN_MOD_TEST_SERVER)

ERL_FILES = $(MODULES:%=%.erl)

HRL_FILES = 

GEN_HRL_FILES = $(GEN_HRL_ORBER) $(GEN_HRL_IIOP) \
	$(GEN_HRL_TEST_SERVER)

GEN_FILES = \
	$(GEN_HRL_FILES:%=$(IDLOUTDIR)/%) \
	$(GEN_MODULES:%=$(IDLOUTDIR)/%.erl) 

GEN_TARGET_FILES = $(GEN_MODULES:%=$(IDLOUTDIR)/%.$(EMULATOR))

SUITE_TARGET_FILES = $(MODULES:%=%.$(EMULATOR))

TARGET_FILES = \
	$(GEN_TARGET_FILES) \
	$(SUITE_TARGET_FILES)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_IDL_FLAGS += -pa $(ERL_TOP)/lib/orber/ebin -pa $(ERL_TOP)/lib/ic/ebin

ERL_COMPILE_FLAGS += $(ERL_IDL_FLAGS) \
	-pa $(ERL_TOP)/lib/ic/ebin \
	-pa $(ERL_TOP)/lib/orber/ebin \
	-I$(ERL_TOP)/lib/orber \
	-I$(ERL_TOP)/lib/orber/test/$(IDLOUTDIR)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
tests debug opt: $(TARGET_FILES)

clean:
	rm -f idl_output/*
	rm -f $(TARGET_FILES) 
	rm -f errs core *~
	rm IDL-GENERATED


docs:

# ----------------------------------------------------
# Special Targets
# ----------------------------------------------------

IDL-GENERATED: orber_test.idl iiop_test.idl orber_test_server.idl
	erlc $(ERL_IDL_FLAGS) -o$(IDLOUTDIR) orber_test.idl
	erlc $(ERL_IDL_FLAGS) -o$(IDLOUTDIR) \
		+'{preproc_flags,"-I../COSS/CosNaming"}' iiop_test.idl
	erlc $(ERL_IDL_FLAGS) -o$(IDLOUTDIR) \
	+'{cfgfile,"orber_test_server.cfg"}' orber_test_server.idl
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
	$(INSTALL_DATA) $(IDL_FILES)  $(TEST_SPEC_FILE) $(COVER_FILE) \
		$(ERL_FILES) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(SUITE_TARGET_FILES) "$(RELSYSDIR)"
	chmod -R u+w "$(RELSYSDIR)"
	$(INSTALL_DIR) "$(RELSYSDIR)/$(IDLOUTDIR)"
	$(INSTALL_DATA) $(GEN_TARGET_FILES) $(GEN_FILES) \
		"$(RELSYSDIR)/$(IDLOUTDIR)"

