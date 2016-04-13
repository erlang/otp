#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
VSN=$(IC_VSN)
# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/ic_test

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
TEST_SPEC_FILE = ic.spec ic_smoke.spec


IDL_FILES = 

COMPILER_TEST_FILES = \
	ic_SUITE_data/Corba.idl \
	ic_SUITE_data/Coss.idl \
	ic_SUITE_data/attr.idl \
	ic_SUITE_data/c_err1.idl \
	ic_SUITE_data/c_err2.idl \
	ic_SUITE_data/c_err3.idl \
	ic_SUITE_data/c_norm.idl \
	ic_SUITE_data/enum.idl \
	ic_SUITE_data/forward.idl \
	ic_SUITE_data/include.idl \
	ic_SUITE_data/include2.idl \
	ic_SUITE_data/include3.idl \
	ic_SUITE_data/inherit.idl \
	ic_SUITE_data/inherit_err.idl \
	ic_SUITE_data/inherit_warn.idl \
	ic_SUITE_data/mult_ids.idl \
	ic_SUITE_data/nasty.idl \
	ic_SUITE_data/one.idl \
	ic_SUITE_data/one_out.idl \
	ic_SUITE_data/one_raises.idl \
	ic_SUITE_data/one_followed.idl \
	ic_SUITE_data/one_void.idl \
        ic_SUITE_data/raises_reg.idl \
	ic_SUITE_data/struct.idl \
	ic_SUITE_data/syntax1.idl \
	ic_SUITE_data/syntax2.idl \
	ic_SUITE_data/syntax3.idl \
	ic_SUITE_data/syntax4.idl \
	ic_SUITE_data/syntax5.idl \
	ic_SUITE_data/syntax6.idl \
	ic_SUITE_data/type.idl \
	ic_SUITE_data/typeid.idl \
	ic_SUITE_data/u_case_mult.idl \
	ic_SUITE_data/u_mult.idl \
	ic_SUITE_data/u_norm.idl \
	ic_SUITE_data/u_type.idl \
	ic_SUITE_data/u_default.idl \
	ic_SUITE_data/undef_id.idl


COMPILER_TEST_FILES2 = \
	ic_register_SUITE_data/reg_m8.idl \
	ic_register_SUITE_data/reg_m9.idl \
	ic_register_SUITE_data/reg_m10.idl \
	ic_register_SUITE_data/reg_m11.idl \
	ic_register_SUITE_data/reg_m12.idl


COMPILER_TEST_FILES3 = \
	ic_pragma_SUITE_data/reg_m0.idl \
	ic_pragma_SUITE_data/reg_m1.idl \
	ic_pragma_SUITE_data/reg_m2.idl \
	ic_pragma_SUITE_data/reg_m3.idl \
	ic_pragma_SUITE_data/reg_m4.idl \
	ic_pragma_SUITE_data/reg_m5.idl \
	ic_pragma_SUITE_data/reg_m6.idl \
	ic_pragma_SUITE_data/reg_m7.idl \
        ic_pragma_SUITE_data/uggly.idl	


COMPILER_TEST_FILES4 = \
	ic_be_SUITE_data/plain.idl


PREPROCESSOR_TEST_FILES = \
	ic_pp_SUITE_data/arg.idl \
	ic_pp_SUITE_data/cascade.idl \
	ic_pp_SUITE_data/comment.idl \
	ic_pp_SUITE_data/concat.idl \
	ic_pp_SUITE_data/define.idl \
	ic_pp_SUITE_data/if.idl \
	ic_pp_SUITE_data/if_zero.idl \
	ic_pp_SUITE_data/improp_nest_constr.idl \
	ic_pp_SUITE_data/inc.idl \
	ic_pp_SUITE_data/line.idl \
	ic_pp_SUITE_data/misc.idl \
	ic_pp_SUITE_data/nopara.idl \
	ic_pp_SUITE_data/predef.idl \
	ic_pp_SUITE_data/predef_time.idl \
	ic_pp_SUITE_data/self_ref.idl \
	ic_pp_SUITE_data/separate.idl \
	ic_pp_SUITE_data/swallow_sc.idl \
	ic_pp_SUITE_data/unintended_grp.idl

C_CLIENT_ERL_SERVER_TEST_FILES = \
	c_client_erl_server_SUITE_data/Makefile.src \
	c_client_erl_server_SUITE_data/c_erl_test.idl \
	c_client_erl_server_SUITE_data/c_client.c \
	c_client_erl_server_SUITE_data/m_i_impl.erl
  
C_CLIENT_ERL_SERVER_PROTO_TEST_FILES = \
	c_client_erl_server_proto_SUITE_data/Makefile.src \
	c_client_erl_server_proto_SUITE_data/c_erl_test.idl \
	c_client_erl_server_proto_SUITE_data/c_client.c \
	c_client_erl_server_proto_SUITE_data/my.c \
	c_client_erl_server_proto_SUITE_data/m_i_impl.erl
  
C_CLIENT_ERL_SERVER_PROTO_TMO_TEST_FILES = \
	c_client_erl_server_proto_tmo_SUITE_data/Makefile.src \
	c_client_erl_server_proto_tmo_SUITE_data/c_erl_test.idl \
	c_client_erl_server_proto_tmo_SUITE_data/c_client.c \
	c_client_erl_server_proto_tmo_SUITE_data/my.c \
	c_client_erl_server_proto_tmo_SUITE_data/m_i_impl.erl
  
ERL_CLIENT_C_SERVER_TEST_FILES = \
	erl_client_c_server_SUITE_data/Makefile.src \
	erl_client_c_server_SUITE_data/erl_c_test.idl \
	erl_client_c_server_SUITE_data/erl_client.erl \
	erl_client_c_server_SUITE_data/c_server.c \
	erl_client_c_server_SUITE_data/callbacks.c

ERL_CLIENT_C_SERVER_PROTO_TEST_FILES = \
	erl_client_c_server_proto_SUITE_data/Makefile.src \
	erl_client_c_server_proto_SUITE_data/erl_c_test.idl \
	erl_client_c_server_proto_SUITE_data/erl_client.erl \
	erl_client_c_server_proto_SUITE_data/c_server.c \
	erl_client_c_server_proto_SUITE_data/callbacks.c

JAVA_CLIENT_ERL_SERVER_TEST_FILES = \
	java_client_erl_server_SUITE_data/Makefile.src \
	java_client_erl_server_SUITE_data/java_erl_test.idl \
	java_client_erl_server_SUITE_data/JavaClient.java \
	java_client_erl_server_SUITE_data/m_i_impl.erl
  
MODULES = \
	ic_SUITE  \
	ic_register_SUITE \
        ic_pragma_SUITE \
	ic_pp_SUITE \
	ic_be_SUITE \
	c_client_erl_server_SUITE \
	c_client_erl_server_proto_SUITE \
	c_client_erl_server_proto_tmo_SUITE \
	erl_client_c_server_SUITE \
	erl_client_c_server_proto_SUITE \
	java_client_erl_server_SUITE

GEN_MODULES = 

ERL_FILES = $(MODULES:%=%.erl)

HRL_FILES = 

GEN_HRL_FILES =  


GEN_FILES = \
	$(GEN_HRL_FILES:%=$(IDLOUTDIR)/%) \
	$(GEN_MODULES=:%=$(IDLOUTDIR)/%.erl) 

GEN_TARGET_FILES = $(GEN_MODULES:%=$(IDLOUTDIR)/%.$(EMULATOR))

SUITE_TARGET_FILES = $(MODULES:%=%.$(EMULATOR))

TARGET_FILES = \
	$(GEN_TARGET_FILES) \
	$(SUITE_TARGET_FILES)

# ----------------------------------------------------
# PROGRAMS
# ----------------------------------------------------

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_LOCAL_FLAGS += -pa $(ERL_TOP)/lib/orber/ebin -pa $(ERL_TOP)/lib/ic/ebin

ERL_COMPILE_FLAGS += \
	$(ERL_LOCAL_FLAGS) \
	-pa $(ERL_TOP)/lib/orber/ebin \
	-I$(ERL_TOP)/lib/orber

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
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: 

release_docs_spec:

release_tests_spec: tests
	$(INSTALL_DIR) "$(RELSYSDIR)"
	$(INSTALL_DIR) "$(RELSYSDIR)/ic_SUITE_data"
	$(INSTALL_DIR) "$(RELSYSDIR)/ic_register_SUITE_data"
	$(INSTALL_DIR) "$(RELSYSDIR)/ic_pragma_SUITE_data"
	$(INSTALL_DIR) "$(RELSYSDIR)/ic_pp_SUITE_data"
	$(INSTALL_DIR) "$(RELSYSDIR)/ic_be_SUITE_data"
	$(INSTALL_DIR) "$(RELSYSDIR)/c_client_erl_server_SUITE_data"
	$(INSTALL_DIR) "$(RELSYSDIR)/c_client_erl_server_proto_SUITE_data"
	$(INSTALL_DIR) "$(RELSYSDIR)/c_client_erl_server_proto_tmo_SUITE_data"
	$(INSTALL_DIR) "$(RELSYSDIR)/erl_client_c_server_SUITE_data"
	$(INSTALL_DIR) "$(RELSYSDIR)/erl_client_c_server_proto_SUITE_data"
	$(INSTALL_DIR) "$(RELSYSDIR)/java_client_erl_server_SUITE_data"
	$(INSTALL_DATA) $(IDL_FILES)  ic.cover $(TEST_SPEC_FILE) $(ERL_FILES) \
		"$(RELSYSDIR)"
	$(INSTALL_DATA) $(COMPILER_TEST_FILES) "$(RELSYSDIR)/ic_SUITE_data"
	$(INSTALL_DATA) $(COMPILER_TEST_FILES2) \
		"$(RELSYSDIR)/ic_register_SUITE_data"
	$(INSTALL_DATA) $(COMPILER_TEST_FILES3) \
		"$(RELSYSDIR)/ic_pragma_SUITE_data"
	$(INSTALL_DATA) $(COMPILER_TEST_FILES4) \
		"$(RELSYSDIR)/ic_be_SUITE_data"
	$(INSTALL_DATA) $(PREPROCESSOR_TEST_FILES) \
		"$(RELSYSDIR)/ic_pp_SUITE_data"
	$(INSTALL_DATA) $(C_CLIENT_ERL_SERVER_TEST_FILES) \
		"$(RELSYSDIR)/c_client_erl_server_SUITE_data"
	$(INSTALL_DATA) $(C_CLIENT_ERL_SERVER_PROTO_TEST_FILES) \
		"$(RELSYSDIR)/c_client_erl_server_proto_SUITE_data"
	$(INSTALL_DATA) $(C_CLIENT_ERL_SERVER_PROTO_TMO_TEST_FILES) \
		"$(RELSYSDIR)/c_client_erl_server_proto_tmo_SUITE_data"
	$(INSTALL_DATA) $(ERL_CLIENT_C_SERVER_TEST_FILES) \
		"$(RELSYSDIR)/erl_client_c_server_SUITE_data"
	$(INSTALL_DATA) $(ERL_CLIENT_C_SERVER_PROTO_TEST_FILES) \
		"$(RELSYSDIR)/erl_client_c_server_proto_SUITE_data"
	$(INSTALL_DATA) $(SUITE_TARGET_FILES) "$(RELSYSDIR)"
	$(INSTALL_DATA) $(JAVA_CLIENT_ERL_SERVER_TEST_FILES) \
		"$(RELSYSDIR)/java_client_erl_server_SUITE_data"
