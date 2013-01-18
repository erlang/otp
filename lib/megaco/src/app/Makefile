# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2007-2012. All Rights Reserved.
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

EBIN = ../../ebin
MEGACO_INCLUDEDIR = ../../include

include $(ERL_TOP)/make/$(TARGET)/otp.mk


# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(MEGACO_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/megaco-$(VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

include modules.mk

ERL_FILES = $(MODULES:%=%.erl)	

TARGET_FILES =  \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR))

APP_FILE   = megaco.app
APP_SRC    = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

APPUP_FILE   = megaco.appup
APPUP_SRC    = $(APPUP_FILE).src
APPUP_TARGET = $(EBIN)/$(APPUP_FILE)


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug
endif

include megaco.mk

ERL_COMPILE_FLAGS += \
	$(MEGACO_ERL_COMPILE_FLAGS) \
	-I../../include


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

info:
	@echo "MODULES = $(MODULES)"
	@echo ""


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
	$(INSTALL_DIR)  "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(APP_TARGET) $(APPUP_TARGET) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR)  "$(RELSYSDIR)/src"
	$(INSTALL_DIR)  "$(RELSYSDIR)/src/app"
	$(INSTALL_DATA) $(ERL_FILES) $(INTERNAL_HRL_FILES) "$(RELSYSDIR)/src/app"
	$(INSTALL_DIR)  "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(EXTERNAL_HRL_FILES) "$(RELSYSDIR)/include"


release_docs_spec:


# ----------------------------------------------------
# Include dependencies
# ----------------------------------------------------

include depend.mk

