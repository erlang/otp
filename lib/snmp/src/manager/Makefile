#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2004-2019. All Rights Reserved.
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

EBIN = ../../ebin

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk

VSN = $(SNMP_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/snmp-$(VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

include modules.mk

ERL_FILES = $(BEHAVIOUR_MODULES:%=%.erl) $(MODULES:%=%.erl)

HRL_FILES = $(INTERNAL_HRL_FILES:%=%.hrl)

BEHAVIOUR_TARGET_FILES = $(BEHAVIOUR_MODULES:%=$(EBIN)/%.$(EMULATOR))
TARGET_FILES =           $(MODULES:%=$(EBIN)/%.$(EMULATOR))


# ----------------------------------------------------
# SNMP FLAGS
# ----------------------------------------------------
ifeq ($(SNMP_DEFAULT_VERBOSITY),)
  SNMP_FLAGS = -Ddefault_verbosity=silence
else
  SNMP_FLAGS = -Ddefault_verbosity=$(SNMP_DEFAULT_VERBOSITY)
endif

ifeq ($(SNMP_DEBUG),d)
  SNMP_FLAGS += -Dsnmp_debug
endif


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ERL_COMPILE_FLAGS += -pa $(ERL_TOP)/lib/snmp/ebin

ifeq ($(WARN_UNUSED_VARS),true)
ERL_COMPILE_FLAGS += +warn_unused_vars
endif

ERL_COMPILE_FLAGS += -I../../include \
                     -I../misc \
                     -Dversion=\"$(VSN)$(PRE_VSN)\" \
                     +'{parse_transform,sys_pre_attributes}' \
                     +'{attribute,insert,app_vsn,$(APP_VSN)}' \
                     -I$(ERL_TOP)/lib/stdlib \
                     $(SNMP_FLAGS)


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug:
	@$(MAKE) TYPE=debug opt

opt:
	@$(MAKE) behaviour_targets
	@$(MAKE) targets

behaviour_targets: $(BEHAVIOUR_TARGET_FILES)

targets: $(TARGET_FILES)


clean:
	rm -f $(BEHAVIOUR_TARGET_FILES) 
	rm -f $(TARGET_FILES) 
	rm -f core *~

docs:

info:
	@echo "ERL_FILES:              $(ERL_FILES)"
	@echo "HRL_FILES:              $(HRL_FILES)"
	@echo "BEHAVIOUR_TARGET_FILES: $(BEHAVIOUR_TARGET_FILES)"
	@echo "TARGET_FILES:           $(TARGET_FILES)"
	@echo ""


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/src/manager"
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) "$(RELSYSDIR)/src/manager"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(BEHAVIOUR_TARGET_FILES) $(TARGET_FILES) "$(RELSYSDIR)/ebin"
# 	$(INSTALL_DIR) "$(RELSYSDIR)/include"
# 	$(INSTALL_DATA) $(EXT_HRL_FILES) "$(RELSYSDIR)/include"

release_docs_spec:

include depend.mk
