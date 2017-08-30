#
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
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(OS_MON_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/os_mon-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES= disksup memsup cpu_sup os_mon os_mon_mib os_sup os_mon_sysinfo \
	nteventlog

INCLUDE=../include
CSRC=../c_src

MEMSUP_HRL=$(INCLUDE)/memsup.hrl

HRL_FILES= $(MEMSUP_HRL)
ERL_FILES= $(MODULES:%=%.erl)

APP_FILE= os_mon.app
APP_SRC= $(APP_FILE).src
APP_TARGET=$(EBIN)/$(APP_FILE)

APPUP_FILE= os_mon.appup

APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= ../ebin/$(APPUP_FILE)


TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += +warn_obsolete_guard -I$(INCLUDE) -Werror

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core *~

docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

#-------------------------------------------------------
# Special dependencies
#-------------------------------------------------------
$(EBIN)/memsup.$(EMULATOR): $(MEMSUP_HRL)

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ERL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(HRL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"

release_docs_spec:







