#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
EBIN = ../../ebin
include $(ERL_TOP)/make/$(TARGET)/otp.mk


# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk

VSN = $(INETS_VSN)


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPLICATION)-$(VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES = \
	inets \
	inets_service \
	inets_app \
	inets_sup \
	inets_trace \
	inets_lib

INTERNAL_HRL_FILES = inets_internal.hrl
EXTERNAL_HRL_FILES = ../../include/httpd.hrl \
	../../include/mod_auth.hrl

ERL_FILES = $(MODULES:%=%.erl)

TARGET_FILES= \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR)) \
	$(APP_TARGET) \
	$(APPUP_TARGET)

APP_FILE   = inets.app
APPUP_FILE = inets.appup

APP_SRC    = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

APPUP_SRC    = $(APPUP_FILE).src
APPUP_TARGET = $(EBIN)/$(APPUP_FILE)


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

include inets.mk

ERL_COMPILE_FLAGS += \
	$(INETS_FLAGS) \
	$(INETS_ERL_COMPILE_FLAGS)


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

$(APP_TARGET): $(APP_SRC) ../../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR)  "$(RELSYSDIR)/src"
	$(INSTALL_DIR)  "$(RELSYSDIR)/src/inets_app"
	$(INSTALL_DATA) $(INTERNAL_HRL_FILES) $(ERL_FILES) "$(RELSYSDIR)/src/inets_app"
	$(INSTALL_DIR)  "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(EXTERNAL_HRL_FILES) "$(RELSYSDIR)/include"
	$(INSTALL_DIR)  "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"

release_docs_spec:

info:
	@echo "APPLICATION       = $(APPLICATION)"
	@echo "INETS_DEBUG       = $(INETS_DEBUG)"
	@echo "INETS_FLAGS       = $(INETS_FLAGS)"
	@echo "ERL_COMPILE_FLAGS = $(ERL_COMPILE_FLAGS)"
