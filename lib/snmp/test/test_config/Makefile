#-*-makefile-*-   ; force emacs to enter makefile-mode

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
# FLAGS AND VARIABLES
# ----------------------------------------------------

EBIN = .

ERL_COMPILE_FLAGS += +'{parse_transform,sys_pre_attributes}' \
                     +'{attribute,insert,app_vsn,$(APP_VSN)}'

ifeq ($(ADDR),)
ADDR = $(shell erl -noshell -s snmp_test_config ip_address -s init stop)
endif

ifeq ($(TARGET_NAME_PRE),)
TARGET_NAME_PRE = $(shell erl -noshell -s snmp_test_config ip_address2 -s init stop)
endif

ifeq ($(SYS_CONTACT),)
SYS_CONTACT = foo@bar.ericsson.se
endif

ifeq ($(SYS_LOCATION),)
SYS_LOCATION = Erlang/OTP
endif

ifeq ($(SYS_NAME),)
SYS_NAME = FOO
endif

ifeq ($(AGENT_ENGINE_ID),)
AGENT_ENGINE_ID = Agent engine of $(USER)
endif

ifeq ($(AGENT_USM_ENGINE_ID),)
AGENT_USM_ENGINE_ID = $(AGENT_ENGINE_ID)
endif

ifeq ($(MANAGER_ENGINE_ID),)
MANAGER_ENGINE_ID = Manager engine of $(USER)
endif

ifeq ($(MANAGER_USM_ENGINE_ID),)
MANAGER_USM_ENGINE_ID = $(MANAGER_ENGINE_ID)
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

%.config: %.config.src
	@echo "$< -> $@"
	$(PERL) -p -e 's?%DIR%?$(RELSYSDIR)? ' < $< > $@

agent/%.conf: agent/%.conf.src
	@echo "$< -> $@"
	sed -e 's?%ADDR%?$(ADDR)? ' \
            -e 's?%SYS_CONTACT%?$(SYS_CONTACT)? ' \
            -e 's?%SYS_LOCATION%?$(SYS_LOCATION)? ' \
            -e 's?%SYS_NAME%?$(SYS_NAME)? ' \
            -e 's?%TARGET_NAME_PRE%?$(TARGET_NAME_PRE)? ' \
            -e 's?%ENGINE_ID%?\"$(AGENT_ENGINE_ID)\"? ' \
            -e 's?%USM_ENGINE_ID%?\"$(AGENT_USM_ENGINE_ID)\"? ' < $< > $@

manager/%.conf: manager/%.conf.src
	@echo "$< -> $@"
	sed -e 's?%ADDR%?$(ADDR)? ' \
            -e 's?%ENGINE_ID%?\"$(MANAGER_ENGINE_ID)\"? ' \
            -e 's?%USM_ENGINE_ID%?\"$(MANAGER_USM_ENGINE_ID)\"? ' < $< > $@


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec:

release_tests_spec: clean opt
	$(INSTALL_DIR)  $(RELSYSDIR)
	chmod -R u+w $(RELSYSDIR)
	$(INSTALL_DIR)  $(RELSYSDIR)/agent
	chmod -R u+w $(RELSYSDIR)/agent
	$(INSTALL_DIR)  $(RELSYSDIR)/agent/conf
	chmod -R u+w $(RELSYSDIR)/agent/conf
	$(INSTALL_DIR)  $(RELSYSDIR)/agent/db
	chmod -R u+w $(RELSYSDIR)/agent/db
	$(INSTALL_DIR)  $(RELSYSDIR)/agent/log
	chmod -R u+w $(RELSYSDIR)/agent/log
	$(INSTALL_DIR)  $(RELSYSDIR)/manager
	chmod -R u+w $(RELSYSDIR)/manager
	$(INSTALL_DIR)  $(RELSYSDIR)/manager/conf
	chmod -R u+w $(RELSYSDIR)/manager/conf
	$(INSTALL_DIR)  $(RELSYSDIR)/manager/db
	chmod -R u+w $(RELSYSDIR)/manager/db
	$(INSTALL_DIR)  $(RELSYSDIR)/manager/log
	chmod -R u+w $(RELSYSDIR)/manager/log
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
	@echo "ADDR            = $(ADDR)"
	@echo "TARGET_NAME_PRE = $(TARGET_NAME_PRE)"
	@echo ""


