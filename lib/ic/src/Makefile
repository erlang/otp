#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1998-2012. All Rights Reserved.
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

ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug -W
endif

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(IC_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/ic-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	ic \
	ic_erlbe \
	ic_cbe \
	icscan \
	icparse \
	iceval \
	ictype \
	ictk \
	icstruct \
	icenum \
	icpreproc \
	icunion \
	ic_pp \
	ic_pragma \
	ic_noc \
	ic_plainbe \
	ic_cclient \
	ic_cserver \
	ic_fetch \
	ic_code \
	ic_codegen \
	ic_error \
	ic_file \
	ic_forms \
	ic_genobj \
	ic_options \
	ic_symtab \
	ic_util \
	ic_jbe \
	ic_struct_java \
	ic_union_java  \
	ic_enum_java \
	ic_constant_java \
	ic_sequence_java \
	ic_array_java \
	ic_attribute_java \
	ic_java_type \
	ic_erl_template


CCL_EX_FILES = \
	../examples/c-client/ReadMe \
	../examples/c-client/Makefile \
	../examples/c-client/client.c \
	../examples/c-client/random.idl \
	../examples/c-client/rmod_random_impl.erl \
        ../examples/c-client/test.erl

CSRV_EX_FILES = \
	../examples/c-server/ReadMe \
	../examples/c-server/Makefile \
	../examples/c-server/client.c \
	../examples/c-server/client.erl \
	../examples/c-server/server.c \
	../examples/c-server/callbacks.c \
	../examples/c-server/random.idl        

EPL_EX_FILES = \
	../examples/erl-plain/ReadMe \
	../examples/erl-plain/rmod_random_impl.erl \
	../examples/erl-plain/random.idl


ESRV_EX_FILES = \
	../examples/erl-genserv/ReadMe \
	../examples/erl-genserv/rmod_random_impl.erl \
	../examples/erl-genserv/random.idl

JAVA_EX_FILES = \
	../examples/java-client-server/ReadMe \
	../examples/java-client-server/client.java \
	../examples/java-client-server/server.java \
	../examples/java-client-server/serverImpl.java \
	../examples/java-client-server/random.idl 

MIXED_EX_FILES = \
	../examples/all-against-all/ReadMe \
	../examples/all-against-all/Makefile \
	../examples/all-against-all/client.erl \
	../examples/all-against-all/server.erl \
	../examples/all-against-all/client.c \
	../examples/all-against-all/server.c \
	../examples/all-against-all/callbacks.c \
	../examples/all-against-all/client.java \
	../examples/all-against-all/server.java \
	../examples/all-against-all/serverImpl.java \
	../examples/all-against-all/random.idl 


EXTERNAL_HRL_FILES= 

INTERNAL_HRL_FILES = \
	ic.hrl \
	ic_debug.hrl \
	icforms.hrl

ERL_FILES= $(MODULES:%=%.erl)	

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR))

YRL_FILE = icparse.yrl

GEN_FILES = icparse.erl

APP_FILE = ic.app
APP_SRC = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_LOCAL_FLAGS += -pa ../../ic/ebin 
# The -pa option is just used temporary until erlc can handle 
# includes from other directories than ../include .
ERL_COMPILE_FLAGS += \
	$(ERL_LOCAL_FLAGS) \
	+'{parse_transform,sys_pre_attributes}' \
	+'{attribute,insert,app_vsn,"ic_$(VSN)"}' \
	-D'COMPILERVSN="$(VSN)"'
YRL_FLAGS = -Iicyeccpre.hrl

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
debug:
	@${MAKE} TYPE=debug opt

opt: $(TARGET_FILES) $(APP_TARGET)

clean:
	rm -f $(TARGET_FILES) $(GEN_FILES) $(APP_TARGET)
	rm -f errs core *~

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------
../ebin/icparse.beam: icparse.erl
	$(V_ERLC) $(ERL_COMPILE_FLAGS) +nowarn_unused_vars +nowarn_unused_function -o$(EBIN) +pj $<

icparse.erl: icparse.yrl icyeccpre.hrl

###	$(ERLC) $(YRL_FLAGS) $<

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) $(APP_TARGET) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ERL_FILES) $(YRL_FILE) $(INTERNAL_HRL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/examples"
	$(INSTALL_DIR) "$(RELSYSDIR)/examples/c-client"
	$(INSTALL_DATA) $(CCL_EX_FILES) "$(RELSYSDIR)/examples/c-client"
	$(INSTALL_DIR) "$(RELSYSDIR)/examples/c-server"
	$(INSTALL_DATA) $(CSRV_EX_FILES) "$(RELSYSDIR)/examples/c-server"
	$(INSTALL_DIR) "$(RELSYSDIR)/examples/erl-plain"
	$(INSTALL_DATA) $(EPL_EX_FILES) "$(RELSYSDIR)/examples/erl-plain"
	$(INSTALL_DIR) "$(RELSYSDIR)/examples/erl-genserv"
	$(INSTALL_DATA) $(ESRV_EX_FILES) "$(RELSYSDIR)/examples/erl-genserv"
	$(INSTALL_DIR) "$(RELSYSDIR)/examples/java-client-server"
	$(INSTALL_DATA) $(JAVA_EX_FILES) "$(RELSYSDIR)/examples/java-client-server"
	$(INSTALL_DIR) "$(RELSYSDIR)/examples/all-against-all"
	$(INSTALL_DATA) $(MIXED_EX_FILES) "$(RELSYSDIR)/examples/all-against-all"

release_docs_spec:

