#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2000-2016. All Rights Reserved.
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

ERL_COMPILE_FLAGS += -W +warn_unused_vars

ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug
endif

# This is temporary until the et app is actually moved...
EBIN = ../ebin

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(ET_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/et-$(VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

include modules.mk

ERL_FILES = $(MODULES:%=%.erl)

TARGET_FILES =  \
	$(ERL_FILES:%.erl=$(EBIN)/%.$(EMULATOR))

APP_FILE   = et.app
APP_SRC    = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

APPUP_FILE   = et.appup
APPUP_SRC    = $(APPUP_FILE).src
APPUP_TARGET = $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -pa $(ERL_TOP)/lib/et/ebin -I../include -Werror

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug:
	@${MAKE} TYPE=debug opt

opt: $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)

clean:
	rm -f $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)
	rm -f errs core *~

docs:


# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 

include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR)  "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(APP_TARGET) $(APPUP_TARGET) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR)  "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ERL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(INTERNAL_HRL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR)  "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(HRL_FILES) "$(RELSYSDIR)/include"


release_docs_spec:


# ----------------------------------------------------
# Include dependencies
# ----------------------------------------------------

$(EBIN)/et.$(EMULATOR): et.erl

$(EBIN)/et_selector.$(EMULATOR): et_selector.erl ../include/et.hrl

$(EBIN)/et_contents_viewer.$(EMULATOR): et_contents_viewer.erl ../include/et.hrl et_internal.hrl

$(EBIN)/et_wx_contents_viewer.$(EMULATOR): et_wx_contents_viewer.erl ../include/et.hrl et_internal.hrl

$(EBIN)/et_collector.$(EMULATOR): et_collector.erl ../include/et.hrl et_internal.hrl

$(EBIN)/et_viewer.$(EMULATOR): et_viewer.erl ../include/et.hrl et_internal.hrl

$(EBIN)/et_wx_viewer.$(EMULATOR): et_wx_viewer.erl ../include/et.hrl et_internal.hrl



