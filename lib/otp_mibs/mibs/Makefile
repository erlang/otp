#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1997-2012. All Rights Reserved.
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

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# Release directory specification
# ----------------------------------------------------
include ../vsn.mk
ifdef SASL_VSN
VSN=$(SASL_VSN)
RELSYSDIR = $(RELEASE_PATH)/lib/sasl-$(VSN)
else
VSN=$(OTP_MIBS_VSN)
RELSYSDIR = $(RELEASE_PATH)/lib/otp_mibs-$(VSN)
endif

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MIB_FILES= OTP-REG.mib OTP-TC.mib OTP-MIB.mib
FUNCS_FILES = OTP-MIB.funcs

BIN_TARGETS= $(MIB_FILES:%.mib=$(SNMP_BIN_TARGET_DIR)/%.bin)
HRL_TARGETS= $(MIB_FILES:%.mib=$(SNMP_HRL_TARGET_DIR)/%.hrl)
V1_MIB_FILES= $(MIB_FILES:%.mib=v1/%.mib.v1)

TARGET_FILES= $(BIN_TARGETS) $(HRL_TARGETS) $(V1_MIB_FILES)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
SNMP_FLAGS = -I ../priv/mibs

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

v1/%.mib.v1: %.mib
	$(gen_verbose)$(ERL_TOP)/lib/snmp/bin/snmp-v2tov1 -o $@ $<

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/mibs"
	$(INSTALL_DIR) "$(RELSYSDIR)/mibs/v1"
	$(INSTALL_DATA) $(MIB_FILES) $(FUNCS_FILES) "$(RELSYSDIR)/mibs"
	$(INSTALL_DATA) $(V1_MIB_FILES) "$(RELSYSDIR)/mibs/v1"
	$(INSTALL_DIR) "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(HRL_TARGETS) "$(RELSYSDIR)/include"
	$(INSTALL_DIR) "$(RELSYSDIR)/priv/mibs"
	$(INSTALL_DATA) $(BIN_TARGETS) "$(RELSYSDIR)/priv/mibs"

release_docs_spec:
