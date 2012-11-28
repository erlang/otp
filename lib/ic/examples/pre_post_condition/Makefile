#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1999-2012. All Rights Reserved.
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
# ``The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved via the world wide web at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
# AB. All Rights Reserved.''
# 
#     $Id$
#
include $(ERL_TOP)/make/target.mk

EBIN= ./

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(IC_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/ic-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

IDL_FILES = \
	ex.idl 

GEN_ERL_MODULES =  \
	oe_ex \
	m_i \
	m_NotAnInteger

MODULES= \
	m_i_impl \
	tracer

GEN_HRL_FILES =  \
	oe_ex.hrl \
	m.hrl \
	m_i.hrl

HRL_FILES = 
TXT_FILES = ReadMe.txt

ERL_FILES= $(MODULES:%=%.erl)


TARGET_FILES = \
	$(GEN_ERL_MODULES:%=$(EBIN)/%.$(EMULATOR)) \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR))

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_LOCAL_FLAGS += -pa $(ERL_TOP)/lib/orber/ebin -pa $(ERL_TOP)/lib/ic/ebin
# The -pa option is just used temporary until erlc can handle 
# includes from other directories than ../include .
ERL_COMPILE_FLAGS += \
	$(ERL_LOCAL_FLAGS) \
	-pa $(ERL_TOP)/lib/orber -I$(ERL_TOP)/lib/orber
YRL_FLAGS = 


# ----------------------------------------------------
# Targets
# ----------------------------------------------------
debug opt: $(TARGET_FILES) 

clean:
	rm -f $(TARGET_FILES) $(GEN_ERL_MODULES:%=%.erl) $(GEN_HRL_FILES) $(CLASS_FILES) IDL-GENERATED
	rm -f errs core *~

docs:

test: $(TEST_TARGET_FILES)


IDL-GENERATED: ex.idl
	$(gen_verbose)erlc $(ERL_LOCAL_FLAGS) +'{precond,{tracer,pre}}' \
		+'{{postcond,"m::i::f"},{tracer,post}}' ex.idl
	$(V_at)>IDL-GENERATED

$(GEN_ERL_MODULES:%=%.erl) $(GEN_HRL_FILES): IDL-GENERATED

$(TARGET_FILES): IDL-GENERATED

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/examples/pre_post_condition"
	$(INSTALL_DATA) $(ERL_FILES) $(IDL_FILES) $(TXT_FILES) "$(RELSYSDIR)/examples/pre_post_condition"


release_docs_spec:


