#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 1997-2009. All Rights Reserved.
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

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk


# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk

VSN = $(SNMP_VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

include modules.mk

SYS_CONFIG_SRCS     = $(SYS_CONFIG_FILES:%=%.src)
AGENT_CONFIG_SRCS   = $(AGENT_CONFIG_FILES:%=%.src)
MANAGER_CONFIG_SRCS = $(MANAGER_CONFIG_FILES:%=%.src)


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)


# ----------------------------------------------------
# SNMP FLAGS
# ----------------------------------------------------


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt: $(SYS_CONFIG_FILES) $(AGENT_CONFIG_FILES) $(MANAGER_CONFIG_FILES)

clean:
	rm -f $(SYS_CONFIG_FILES)
	rm -f $(AGENT_CONFIG_FILES)
	rm -f $(MANAGER_CONFIG_FILES)
	rm -f core

docs:


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec:

release_tests_spec: opt
	$(INSTALL_DIR)  $(RELSYSDIR)
	chmod -f -R u+w $(RELSYSDIR)
	$(INSTALL_DIR)  $(RELSYSDIR)/agent
	chmod -f -R u+w $(RELSYSDIR)/agent
	$(INSTALL_DIR)  $(RELSYSDIR)/agent/conf
	chmod -f -R u+w $(RELSYSDIR)/agent/conf
	$(INSTALL_DIR)  $(RELSYSDIR)/agent/db
	chmod -f -R u+w $(RELSYSDIR)/agent/db
	$(INSTALL_DIR)  $(RELSYSDIR)/agent/log
	chmod -f -R u+w $(RELSYSDIR)/agent/log
	$(INSTALL_DIR)  $(RELSYSDIR)/manager
	chmod -f -R u+w $(RELSYSDIR)/manager
	$(INSTALL_DIR)  $(RELSYSDIR)/manager/conf
	chmod -f -R u+w $(RELSYSDIR)/manager/conf
	$(INSTALL_DIR)  $(RELSYSDIR)/manager/db
	chmod -f -R u+w $(RELSYSDIR)/manager/db
	$(INSTALL_DIR)  $(RELSYSDIR)/manager/log
	chmod -f -R u+w $(RELSYSDIR)/manager/log
	$(INSTALL_DATA) $(SYS_CONFIG_FILES)     $(RELSYSDIR)
	$(INSTALL_DATA) $(AGENT_CONFIG_FILES)   $(RELSYSDIR)/agent/conf
	$(INSTALL_DATA) $(MANAGER_CONFIG_FILES) $(RELSYSDIR)/manager/conf

release_docs_spec:


info:
	@echo "SNMP_DEBUG        = $(SNMP_DEBUG)"
	@echo "SNMP_FLAGS        = $(SNMP_FLAGS)"
	@echo ""
	@echo "SNMP_MIB_DIR      = $(SNMP_MIB_DIR)"
	@echo "MIB_SOURCE        = $(MIB_SOURCE)"
	@echo "MIB_TARGETS       = $(MIB_TARGETS)"
	@echo "SNMP_MIB_FLAGS    = $(SNMP_MIB_FLAGS)"
	@echo ""
	@echo "ERL_COMPILE_FLAGS = $(ERL_COMPILE_FLAGS)"
	@echo ""
	@echo "RELSYSDIR         = $(RELSYSDIR)"
	@echo ""
	@echo "SOURCE            = $(SOURCE)"
	@echo ""
	@echo "TARGET_FILES      = $(TARGET_FILES)"
	@echo ""
	@echo "EMAKEFILE         = $(EMAKEFILE)"
	@echo "MAKE_EMAKE        = $(MAKE_EMAKE)"
	@echo "BUILDTARGET       = $(BUILDTARGET)"
	@echo "RELTEST_FILES     = $(RELTEST_FILES)"
	@echo ""


