#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
# FLAGS
# ----------------------------------------------------

ifneq ($(MIBS_VERBOSITY),) 
SNMP_FLAGS += +'{verbosity,$(MIBS_VERBOSITY)}'
endif

ERL_COMPILE_FLAGS += -I../include \
                     -Dversion=\"$(VSN)$(PRE_VSN)\" \
                     +'{parse_transform,sys_pre_attributes}' \
                     +'{attribute,insert,app_vsn,$(APP_VSN)}' \
                     -I$(ERL_TOP)/lib/stdlib \
                     $(SNMP_FLAGS)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR         = $(RELEASE_PATH)/lib/snmp-$(VSN)
EXAMPLE_RELSYSDIR = "$(RELSYSDIR)/examples"
EX1_RELSYSDIR     = $(EXAMPLE_RELSYSDIR)/ex1

# ----------------------------------------------------
# Common macros
# ----------------------------------------------------

ERL_FILES  = ex1.erl
MIB_FILES  = EX1-MIB.funcs EX1-MIB.mib
BUILD_MIBS = EX1-MIB

TARGET_FILES= \
	$(BUILD_MIBS:%=%.bin) \
	$(ERL_FILES:%.erl=%.$(EMULATOR))


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: build

clean:
	rm -f $(TARGET_FILES)
	rm -f core *~

docs:

build: $(TARGET_FILES)


info: 
	@echo "TARGET_FILES:  $(TARGET_FILES)"
	@echo "EXAMPLE_FILES: $(EXAMPLE_FILES)"


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(EXAMPLE_RELSYSDIR)
	$(INSTALL_DIR) $(EX1_RELSYSDIR)
	$(INSTALL_DATA) $(MIB_FILES) $(EX1_RELSYSDIR)
	$(INSTALL_DATA) $(ERL_FILES) $(EX1_RELSYSDIR)
	$(INSTALL_DATA) $(TARGET_FILES) $(EX1_RELSYSDIR)

release_docs_spec:
