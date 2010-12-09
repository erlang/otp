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
# Configured variables
# ----------------------------------------------------
# PERL = @PERL@
PERL = perl


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

include modules.mk

ERL_TARGETS = $(MODULES:%=$(EBIN)/%.$(EMULATOR))

SYS_CONFIG_SRCS     = $(SYS_CONFIG_FILES:%=%.src)
AGENT_CONFIG_SRCS   = $(AGENT_CONFIG_FILES:%=%.src)
MANAGER_CONFIG_SRCS = $(MANAGER_CONFIG_FILES:%=%.src)

CONFIG_FILES = \
	$(SYS_CONFIG_FILES) \
	$(AGENT_CONFIG_FILES) \
	$(MANAGER_CONFIG_FILES)

TARGETS = \
	$(ERL_TARGETS) \
	$(CONFIG_FILES)


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
ifeq ($(TESTROOT),)
TESTROOT=/tmp
endif
RELSYSDIR = $(TESTROOT)


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

EBIN = .

ERL_COMPILE_FLAGS += +'{parse_transform,sys_pre_attributes}' \
                     +'{attribute,insert,app_vsn,$(APP_VSN)}'

ifeq ($(ADDR),)
ADDR = $(shell erl -noshell -s test_config ip_address -s init stop)
endif


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt: $(TARGETS) 

clean:
	rm -f $(CONFIG_FILES)
	rm -f $(ERL_TARGETS)
	rm -f core

docs:

$(SYS_CONFIG_FILES): $(SYS_CONFIG_SRCS)
	@echo "$< -> $@"
	$(PERL) -p -e 's?%DIR%?$(RELSYSDIR)? ' < $< > $@

$(AGENT_CONFIG_FILES): $(AGENT_CONFIG_SRCS)
	@echo "$< -> $@"
	$(PERL) -p -e 's?%ADDR%?$(ADDR)? ' < $< > $@

$(MANAGER_CONFIG_FILES): $(MANAGER_CONFIG_SRCS)
	@echo "$< -> $@"
	$(PERL) -p -e 's?%ADDR%?$(ADDR)? ' < $< > $@


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
	@echo ""
	@echo "RELSYSDIR            = $(RELSYSDIR)"
	@echo ""
	@echo "SYS_CONFIG_SRCS      = $(SYS_CONFIG_SRCS)"
	@echo "SYS_CONFIG_FILES     = $(SYS_CONFIG_FILES)"
	@echo ""
	@echo "AGENT_CONFIG_SRCS    = $(AGENT_CONFIG_SRCS)"
	@echo "AGENT_CONFIG_FILES   = $(AGENT_CONFIG_FILES)"
	@echo ""
	@echo "MANAGER_CONFIG_SRCS  = $(MANAGER_CONFIG_SRCS)"
	@echo "MANAGER_CONFIG_FILES = $(MANAGER_CONFIG_FILES)"
	@echo ""
	@echo "ADDR  = $(ADDR)"
	@echo ""


